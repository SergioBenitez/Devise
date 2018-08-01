use std::ops::Deref;

use syn::{self, Member, Index};
use proc_macro2::TokenStream as TokenStream2;

use ext::field_to_ident;
use spanned::Spanned;

#[derive(Debug)]
pub struct Field<'f> {
    pub matched: bool,
    pub index: usize,
    pub field: &'f syn::Field
}

impl<'f> Field<'f> {
    pub fn accessor(&self) -> TokenStream2 {
        if self.matched {
            let ident = field_to_ident(self.index, self.field);
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

    pub fn parent_attrs(&self) -> Option<&[syn::Attribute]> {
        None
    }
}

impl<'f> Deref for Field<'f> {
    type Target = syn::Field;

    fn deref(&self) -> &syn::Field {
        self.field
    }
}
