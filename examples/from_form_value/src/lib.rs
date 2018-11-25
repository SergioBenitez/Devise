#![feature(proc_macro_diagnostic)]

#[macro_use] extern crate quote;
extern crate devise;
extern crate proc_macro;

use proc_macro::TokenStream;
use devise::*;

#[proc_macro_derive(FromFormValue)]
pub fn derive_from_form_value(input: TokenStream) -> TokenStream {
    DeriveGenerator::build_for(input, quote!(impl<'__v> ::rocket::request::FromFormValue<'__v>))
        .data_support(DataSupport::Enum)
        .validate_enum(|generator, data| {
            // This derive only works for variants that are nullary.
            for variant in data.variants() {
                if !variant.fields().count() == 0 {
                    return Err(variant.span().error("variants cannot have fields"));
                }
            }

            // Emit a warning if the enum is empty.
            if data.variants.is_empty() {
                generator.input.span().warning("deriving for empty enum").emit();
            }

            Ok(())
        })
        .function(|_, inner| quote! {
            type Error = &'__v ::rocket::http::RawStr;

            fn from_form_value(
                value: &'__v ::rocket::http::RawStr
            ) -> ::std::result::Result<Self, Self::Error> {
                let uncased = value.as_uncased_str();
                #inner
                ::std::result::Result::Err(value)
            }
        })
        .try_map_enum(null_enum_mapper)
        .map_variant(|_, variant| {
            let variant_str = variant.ident.to_string();
            let builder = variant.builder(|_| unreachable!());
            quote! {
                if uncased == #variant_str {
                    return ::std::result::Result::Ok(#builder);
                }
            }
        })
        .to_tokens()
}
