use std::ops::Deref;

use quote::ToTokens;
use proc_macro2::{Span, TokenStream};
use syn::{self, Member, Index, punctuated::Punctuated, spanned::Spanned};

use derived::Derived;

#[derive(Debug, Copy, Clone)]
pub enum FieldParent<'p> {
    Variant(Derived<'p, syn::Variant>),
    Struct(Derived<'p, syn::DataStruct>),
    Union(Derived<'p, syn::DataUnion>),
}

impl<'p> FieldParent<'p> {
    pub fn input(self) -> &'p syn::DeriveInput {
        match self {
            FieldParent::Variant(v) => v.derive_input,
            FieldParent::Struct(s) => s.derive_input,
            FieldParent::Union(u) => u.derive_input,
        }
    }

    pub fn fields(self) -> Fields<'p> {
        let (mut span, kind) = match self {
            FieldParent::Variant(v) => (v.fields.span(), (&v.value.fields).into()),
            FieldParent::Struct(s) => (s.fields.span(), (&s.value.fields).into()),
            FieldParent::Union(u) => (u.fields.span(), FieldsKind::Named(&u.value.fields.named)),
        };

        if let FieldsKind::Unit = kind {
            span = match self {
                FieldParent::Variant(v) => v.span(),
                _ => self.input().span(),
            };
        }

        Fields { parent: self, kind, span }
    }

    pub fn attrs(self) -> &'p [syn::Attribute] {
        match self {
            FieldParent::Variant(v) => &v.value.attrs,
            FieldParent::Struct(_) | FieldParent::Union(_) => &self.input().attrs,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum FieldsKind<'p> {
    Named(&'p Punctuated<syn::Field, syn::token::Comma>),
    Unnamed(&'p Punctuated<syn::Field, syn::token::Comma>),
    Unit
}

impl<'a> From<&'a syn::Fields> for FieldsKind<'a> {
    fn from(syn_fields: &'a syn::Fields) -> Self {
        match syn_fields {
            syn::Fields::Named(ref fs) => FieldsKind::Named(&fs.named),
            syn::Fields::Unnamed(ref fs) => FieldsKind::Unnamed(&fs.unnamed),
            syn::Fields::Unit => FieldsKind::Unit,
        }
    }
}

impl<'p> FieldsKind<'p> {
    fn fields(&self) -> Option<&'p Punctuated<syn::Field, syn::token::Comma>> {
        match self {
            FieldsKind::Named(inner) | FieldsKind::Unnamed(inner) => Some(inner),
            FieldsKind::Unit => None
        }
    }

    fn iter(self) -> impl Iterator<Item = &'p syn::Field> {
        self.fields().into_iter().flat_map(|fields| fields.iter())
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Fields<'p> {
    parent: FieldParent<'p>,
    kind: FieldsKind<'p>,
    span: Span,
}

impl<'f> Fields<'f> {
    pub fn iter(self) -> impl Iterator<Item = Field<'f>> {
        self.kind.iter().enumerate().map(move |(index, field)| Field {
            index,
            parent: self.parent,
            field: Derived::from(self.parent.input(), field),
        })
    }

    pub fn is_empty(self) -> bool {
        self.count() == 0
    }

    pub fn count(self) -> usize {
        match self.kind {
            FieldsKind::Named(fields) => fields.len(),
            FieldsKind::Unnamed(fields) => fields.len(),
            FieldsKind::Unit => 0
        }
    }

    pub fn parent_attrs(self) -> &'f [syn::Attribute] {
        self.parent.attrs()
    }

    pub fn are_named(self) -> bool {
        match self.kind {
            FieldsKind::Named(..) => true,
            _ => false
        }
    }

    pub fn are_unnamed(self) -> bool {
        match self.kind {
            FieldsKind::Unnamed(..) => true,
            _ => false
        }
    }

    pub fn are_unit(self) -> bool {
        match self.kind {
            FieldsKind::Unit => true,
            _ => false
        }
    }

    fn surround(self, tokens: TokenStream) -> TokenStream {
        match self.kind {
            FieldsKind::Named(..) => quote!({ #tokens }),
            FieldsKind::Unnamed(..) => quote!(( #tokens )),
            FieldsKind::Unit => quote!()
        }
    }

    pub fn match_tokens(self) -> TokenStream {
        // This relies on match ergonomics to work in either case.
        let idents = self.iter().map(|field| {
            let match_ident = field.match_ident();
            match field.ident {
                Some(ref id) => quote!(#id: #match_ident),
                None => quote!(#match_ident)
            }

        });

        self.surround(quote!(#(#idents),*))
    }
}

impl<'a> ToTokens for Fields<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self.kind.fields() {
            Some(fields) => fields.to_tokens(tokens),
            None => tokens.extend(quote_spanned!(self.span => (A,)))
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Field<'f> {
    pub parent: FieldParent<'f>,
    pub field: Derived<'f, syn::Field>,
    pub index: usize,
}

impl<'f> Field<'f> {
    pub fn match_ident(self) -> syn::Ident {
        let name = match self.ident {
            Some(ref id) => format!("__{}", id),
            None => format!("__{}", self.index)
        };

        syn::Ident::new(&name, self.span().into())
    }

    pub fn accessor(&self) -> TokenStream {
        if let FieldParent::Variant(_) = self.parent {
            let ident = self.match_ident();
            quote!(#ident)
        } else {
            let span = self.field.span().into();
            let member = match self.ident {
                Some(ref ident) => Member::Named(ident.clone()),
                None => Member::Unnamed(Index { index: self.index as u32, span })
            };

            quote_spanned!(span => self.#member)
        }
    }
}

impl<'f> Deref for Field<'f> {
    type Target = Derived<'f, syn::Field>;

    fn deref(&self) -> &Self::Target {
        &self.field
    }
}
