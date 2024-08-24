use std::convert::TryFrom;

use quote::{ToTokens, TokenStreamExt};
use proc_macro2::{Span, TokenStream, TokenTree};
use proc_macro2_diagnostics::{Diagnostic, SpanDiagnosticExt};
use syn::{self, punctuated::Punctuated, spanned::Spanned, parse::{Parse, Parser}};

use generator::Result;

#[derive(Debug, Clone)]
pub enum MetaItem {
    Path(syn::Path),
    Tokens(TokenStream),
    KeyValue {
        path: syn::Path,
        eq: syn::Token![=],
        tokens: TokenStream,
    },
    List {
        path: syn::Path,
        delimiter: syn::MacroDelimiter,
        items: Punctuated<MetaItem, syn::token::Comma>
    }
}

fn parse_delimited_tokens(input: syn::parse::ParseStream) -> syn::Result<TokenStream> {
    input.step(|cursor| {
        let mut stream = TokenStream::new();
        let mut rest = *cursor;
        while let Some((tt, next)) = rest.token_tree() {
            if matches!(&tt, TokenTree::Punct(p) if p.as_char() == ',') {
                return Ok((stream, rest));
            }

            rest = next;
            stream.append(tt);
        }

        Ok((stream, rest))
    })
}

macro_rules! macro_delimited {
    ($a:ident in $input:ident) => {
        if $input.peek(syn::token::Brace) {
            syn::MacroDelimiter::Brace(syn::braced!($a in $input))
        } else if $input.peek(syn::token::Bracket) {
            syn::MacroDelimiter::Bracket(syn::bracketed!($a in $input))
        } else {
            syn::MacroDelimiter::Paren(syn::parenthesized!($a in $input))
        }
    };
}

impl syn::parse::Parse for MetaItem {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let item = if let Ok(path) = input.parse::<syn::Path>() {
            if input.peek(syn::token::Paren) {
                let list;
                MetaItem::List {
                    path,
                    delimiter: macro_delimited!(list in input),
                    items: list.parse_terminated(Self::parse, syn::Token![,])?
                }
            } else if input.peek(syn::Token![=]) {
                MetaItem::KeyValue {
                    path,
                    eq: input.parse()?,
                    tokens: parse_delimited_tokens(input)?,
                }
            } else {
                MetaItem::Path(path)
            }
        } else {
            MetaItem::Tokens(parse_delimited_tokens(input)?)
        };

        Ok(item)
    }
}

impl TryFrom<syn::Meta> for MetaItem {
    type Error = syn::Error;

    fn try_from(value: syn::Meta) -> std::result::Result<Self, Self::Error> {
        let item = match value {
            syn::Meta::Path(path) => MetaItem::Path(path),
            syn::Meta::List(list) => MetaItem::List {
                path: list.path,
                delimiter: list.delimiter,
                items: <Punctuated<Self, syn::Token![,]>>::parse_terminated.parse2(list.tokens)?,
            },
            syn::Meta::NameValue(nv) => MetaItem::KeyValue {
                path: nv.path,
                eq: nv.eq_token,
                tokens: parse_delimited_tokens.parse2(nv.value.to_token_stream())?,
            }
        };

        Ok(item)
    }
}

impl MetaItem {
    pub fn attr_path(&self) -> Option<&syn::Path> {
        use MetaItem::*;

        match self {
            Path(p) => Some(p),
            KeyValue { path, .. } => Some(path),
            List { path, .. } => Some(path),
            _ => None
        }
    }

    pub fn name(&self) -> Option<&syn::Ident> {
        let path = self.attr_path()?;
        path.segments.last().map(|l| &l.ident)
    }

    pub fn tokens(&self) -> Option<&TokenStream> {
        match self {
            MetaItem::Tokens(tokens) | MetaItem::KeyValue { tokens, .. } => Some(tokens),
            _ => None
        }
    }

    pub fn parse_value<T: Parse>(&self, expected: &str) -> Result<T> {
        let tokens = self.tokens().ok_or_else(|| self.expected(expected))?;
        syn::parse2(tokens.clone())
            .map_err(|e| e.span().error(format!("failed to parse {}: {}", expected, e)))
    }

    pub fn parse_value_with<P: Parser>(&self, parser: P, expected: &str) -> Result<P::Output> {
        match self {
            MetaItem::Tokens(tokens) | MetaItem::KeyValue { tokens, .. } => {
                parser.parse2(tokens.clone()).map_err(|e| {
                    e.span().error(format!("failed to parse {}: {}", expected, e))
                })
            },
            _ => Err(self.expected(expected))
        }
    }

    pub fn expected(&self, k: &str) -> Diagnostic {
        let bare = self.is_bare().then_some("bare ").unwrap_or("");
        let msg = match self.name().map(|i| i.to_string()) {
            Some(n) => format!("expected {}, found {}{} {:?}", k, bare, self.description(), n),
            None => format!("expected {}, found {}{}", k, bare, self.description()),
        };

        self.span().error(msg)
    }

    pub fn description(&self) -> &'static str {
        let expr = self.tokens().and_then(|t| syn::parse2::<syn::Expr>(t.clone()).ok());
        if let Some(syn::Expr::Lit(e)) = expr {
            match e.lit {
                syn::Lit::Str(..) => "string literal",
                syn::Lit::ByteStr(..) => "byte string literal",
                syn::Lit::Byte(..) => "byte literal",
                syn::Lit::Char(..) => "character literal",
                syn::Lit::Int(..) => "integer literal",
                syn::Lit::Float(..) => "float literal",
                syn::Lit::Bool(..) => "boolean literal",
                syn::Lit::Verbatim(..) => "literal",
                _ => "unknown literal"
            }
        } else if expr.is_some() {
            "non-literal expression"
        } else {
            match self {
                MetaItem::Tokens(..) => "tokens",
                MetaItem::KeyValue { .. } => "key/value pair",
                MetaItem::List { .. } => "list",
                MetaItem::Path(_) => "path",
            }
        }
    }

    pub fn is_bare(&self) -> bool {
        match self {
            MetaItem::Path(..) | MetaItem::Tokens(..) => true,
            MetaItem::KeyValue { .. } | MetaItem::List { .. } => false,
        }
    }

    pub fn expr(&self) -> Result<syn::Expr> {
        self.parse_value("expression")
    }

    pub fn path(&self) -> Result<syn::Path> {
        match self {
            MetaItem::Path(p) => Ok(p.clone()),
            _ => self.parse_value("path")
        }
    }

    pub fn lit(&self) -> Result<syn::Lit> {
        fn from_expr(meta: &MetaItem, expr: syn::Expr) -> Result<syn::Lit> {
            match expr {
                syn::Expr::Lit(e) => Ok(e.lit),
                syn::Expr::Group(g) => from_expr(meta, *g.expr),
                _ => Err(meta.expected("literal")),
            }
        }

        self.parse_value("literal").and_then(|e| from_expr(self, e))
    }

    pub fn list(&self) -> Result<impl Iterator<Item = &MetaItem> + Clone> {
        match self {
            MetaItem::List { items, .. } => Ok(items.iter()),
            _ => {
                let n = self.name().map(|i| i.to_string()).unwrap_or_else(|| "attr".into());
                Err(self.expected(&format!("list `#[{}(..)]`", n)))
            }
        }
    }

    pub fn value_span(&self) -> Span {
        match self {
            MetaItem::KeyValue { tokens, .. } => tokens.span(),
            _ => self.span(),
        }
    }
}

impl ToTokens for MetaItem {
    fn to_tokens(&self, stream: &mut TokenStream) {
        match self {
            MetaItem::Path(p) => p.to_tokens(stream),
            MetaItem::Tokens(tokens) => stream.append_all(tokens.clone()),
            MetaItem::KeyValue { path, eq, tokens } => {
                path.to_tokens(stream);
                eq.to_tokens(stream);
                stream.append_all(tokens.clone());
            }
            MetaItem::List { path, delimiter, items } => {
                use syn::MacroDelimiter::*;

                path.to_tokens(stream);
                match delimiter {
                    Paren(p) => p.surround(stream, |t| items.to_tokens(t)),
                    Brace(b) => b.surround(stream, |t| items.to_tokens(t)),
                    Bracket(b) => b.surround(stream, |t| items.to_tokens(t)),
                }
            }
        }
    }
}
