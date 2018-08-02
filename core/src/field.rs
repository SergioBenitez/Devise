use std::ops::Deref;

use syn::{self, Member, Index};
use proc_macro::Span;
use proc_macro2::TokenStream as TokenStream2;

use Derived;
use spanned::Spanned;

#[derive(Debug, Copy, Clone)]
pub enum FieldParent<'p> {
    Variant(Derived<'p, syn::Variant>),
    Struct(Derived<'p, syn::DataStruct>),
}

impl<'p> FieldParent<'p> {
    pub fn parent(self) -> &'p syn::DeriveInput {
        match self {
            FieldParent::Variant(v) => v.derive_input,
            FieldParent::Struct(s) => s.derive_input,
        }
    }

    pub fn syn_fields(self) -> &'p syn::Fields {
        match self {
            FieldParent::Variant(v) => &v.value.fields,
            FieldParent::Struct(s) => &s.value.fields,
        }
    }

    pub fn fields(self) -> Fields<'p> {
        Fields(self)
    }

    pub fn attrs(self) -> &'p [syn::Attribute] {
        match self {
            FieldParent::Variant(v) => &v.value.attrs,
            FieldParent::Struct(s) => &s.derive_input.attrs,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Fields<'p>(crate FieldParent<'p>);

impl<'f> Fields<'f> {
    pub fn iter(self) -> impl Iterator<Item = Field<'f>> {
        self.0.syn_fields().iter().enumerate().map(move |(index, field)| Field {
            parent: self.0,
            index: index,
            field: Derived::from(self.0.parent(), field),
        })
    }

    pub fn count(self) -> usize {
        match self.0.syn_fields() {
            syn::Fields::Named(fields) => fields.named.len(),
            syn::Fields::Unnamed(fields) => fields.unnamed.len(),
            syn::Fields::Unit => 0
        }
    }

    pub fn parent_attrs(self) -> &'f [syn::Attribute] {
        self.0.attrs()
    }

    pub fn are_named(self) -> bool {
        match self.0.syn_fields() {
            syn::Fields::Named(..) => true,
            _ => false
        }
    }

    pub fn are_unnamed(self) -> bool {
        match self.0.syn_fields() {
            syn::Fields::Unnamed(..) => true,
            _ => false
        }
    }

    pub fn are_unit(self) -> bool {
        match self.0.syn_fields() {
            syn::Fields::Unit => true,
            _ => false
        }
    }

    crate fn surround(self, tokens: TokenStream2) -> TokenStream2 {
        match self.0.syn_fields() {
            syn::Fields::Named(..) => quote!({ #tokens }),
            syn::Fields::Unnamed(..) => quote!(( #tokens )),
            syn::Fields::Unit => quote!()
        }
    }

    pub fn match_tokens(self) -> TokenStream2 {
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

impl<'f> Spanned for Fields<'f> {
    fn span(&self) -> Span {
        self.0.syn_fields().span()
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

    pub fn accessor(&self) -> TokenStream2 {
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
