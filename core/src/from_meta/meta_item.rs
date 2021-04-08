use quote::ToTokens;
use proc_macro2::{Span, TokenStream};
use proc_macro2_diagnostics::{Diagnostic, SpanDiagnosticExt};
use syn::{self, punctuated::Punctuated, spanned::Spanned};

use generator::Result;

#[derive(Debug, Clone)]
pub enum MetaItem {
    Path(syn::Path),
    Expr(syn::Expr),
    KeyValue {
        path: syn::Path,
        eq: syn::Token![=],
        expr: syn::Expr,
    },
    List {
        path: syn::Path,
        paren: syn::token::Paren,
        items: Punctuated<MetaItem, syn::token::Comma>
    }
}

impl syn::parse::Parse for MetaItem {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let item = if let Ok(path) = input.parse::<syn::Path>() {
            if input.peek(syn::token::Paren) {
                let list;
                MetaItem::List {
                    path,
                    paren: syn::parenthesized!(list in input),
                    items: list.parse_terminated(Self::parse)?
                }
            } else if input.peek(syn::Token![=]) {
                MetaItem::KeyValue {
                    path,
                    eq: input.parse()?,
                    expr: input.parse()?,
                }
            } else {
                MetaItem::Path(path)
            }
        } else {
            MetaItem::Expr(input.parse()?)
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

    pub fn description(&self) -> &'static str {
        match self {
            MetaItem::Path(..) => "path",
            MetaItem::Expr(syn::Expr::Lit(e)) => match e.lit {
                syn::Lit::Str(..) => "string literal",
                syn::Lit::ByteStr(..) => "byte string literal",
                syn::Lit::Byte(..) => "byte literal",
                syn::Lit::Char(..) => "character literal",
                syn::Lit::Int(..) => "integer literal",
                syn::Lit::Float(..) => "float literal",
                syn::Lit::Bool(..) => "boolean literal",
                syn::Lit::Verbatim(..) => "literal",
            },
            MetaItem::Expr(..) => "expression",
            MetaItem::KeyValue { .. } => "key/value pair",
            MetaItem::List { .. } => "list",
        }
    }

    pub fn expected(&self, k: &str) -> Diagnostic {
        let desc = self.description();
        let msg = match self.name().map(|i| i.to_string()) {
            Some(n) if self.is_bare() => format!("expected {}, found bare {} {:?}", k, desc, n),
            Some(n) => format!("expected {}, found {} {:?}", k, desc, n),
            None if self.is_bare() => format!("expected {}, found bare {}", k, self.description()),
            None => format!("expected {}, found {}", k, self.description()),
        };

        self.span().error(msg)
    }

    pub fn is_bare(&self) -> bool {
        match self {
            MetaItem::Path(..) | MetaItem::Expr(..) => true,
            MetaItem::KeyValue { .. } | MetaItem::List { .. } => false,
        }
    }

    pub fn expr(&self) -> Result<&syn::Expr> {
        match self {
            MetaItem::Expr(e) => Ok(e),
            MetaItem::KeyValue { expr: e, .. } => Ok(e),
            _ => Err(self.expected("expression")),
        }
    }

    pub fn path(&self) -> Result<&syn::Path> {
        match self {
            MetaItem::Path(p) => Ok(p),
            MetaItem::KeyValue { expr: syn::Expr::Path(e), .. } => Ok(&e.path),
            _ => Err(self.expected("path")),
        }
    }

    pub fn lit(&self) -> Result<&syn::Lit> {
        fn from_expr<'a>(meta: &MetaItem, expr: &'a syn::Expr) -> Result<&'a syn::Lit> {
            match expr {
                syn::Expr::Lit(e) => Ok(&e.lit),
                syn::Expr::Group(g) => from_expr(meta, &g.expr),
                _ => Err(meta.expected("literal")),
            }
        }

        match self {
            MetaItem::Expr(e) => from_expr(self, &e),
            MetaItem::KeyValue { expr: syn::Expr::Lit(e), .. } => Ok(&e.lit),
            _ => Err(self.expected("literal")),
        }
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
            MetaItem::KeyValue { expr, .. } => expr.span(),
            _ => self.span(),
        }
    }
}

impl ToTokens for MetaItem {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            MetaItem::Path(p) => p.to_tokens(tokens),
            MetaItem::Expr(e) => e.to_tokens(tokens),
            MetaItem::KeyValue { path, eq, expr } => {
                path.to_tokens(tokens);
                eq.to_tokens(tokens);
                expr.to_tokens(tokens);
            }
            MetaItem::List { path, paren, items } => {
                path.to_tokens(tokens);
                paren.surround(tokens, |t| items.to_tokens(t));
            }
        }
    }
}
