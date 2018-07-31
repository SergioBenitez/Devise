use std::ops::Deref;

use proc_macro2::TokenStream as TokenStream2;
use syn;

use ext::FieldsExt;

#[derive(Debug)]
pub struct Variant<'v> {
    pub parent: syn::Ident,
    pub variant: &'v syn::Variant
}

impl<'v> Deref for Variant<'v> {
    type Target = syn::Variant;

    fn deref(&self) -> &syn::Variant {
        self.variant
    }
}

impl<'f> Variant<'f> {
    pub fn builder<F: Fn(&syn::Field) -> TokenStream2>(&self, f: F) -> TokenStream2 {
        let variant = &self.ident;
        let expression = self.fields.iter().map(f);
        let enum_name = &self.parent;
        if self.fields.is_named() {
            let field_name = self.fields.iter().map(|f| f.ident.as_ref().unwrap());
            quote! {
                #enum_name::#variant { #(#field_name: #expression),* }
            }
        } else if self.fields.is_unnamed() {
            quote! {
                #enum_name::#variant(#(#expression),*)
            }
        } else {
            quote! {
                #enum_name::#variant
            }
        }
    }
}
