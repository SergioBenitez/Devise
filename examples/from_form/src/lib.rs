#![feature(proc_macro_diagnostic)]

#[macro_use] extern crate quote;
#[macro_use] extern crate derive_utils;
extern crate proc_macro;

use proc_macro::TokenStream;
use derive_utils::{*, ext::*};

// FIXME! Make this happen.

#[derive(FromAttr)]
struct FormField {
    field: String
}

// impl FormField { .. }

#[proc_macro_derive(FromForm)]
pub fn derive_from_form(input: TokenStream) -> TokenStream {
    DeriveGenerator::build_for(input, "FromForm<'__f>")
        .generic_support(GenericSupport::Lifetime | GenericSupport::Type)
        .data_support(DataSupport::NamedStruct) // FIXME
        .map_type_generic(|_, ident, _| quote!(#ident : FromFormValue<'__f>))
        .replace_generic(0, 0) // FIXME
        .validate_fields(|_, fields| match fields.is_empty() {
            true => Err(gen.input.span().error("at least one field is required")),
            false => Ok(())
        })
        .function(|_, inner| quote! {
            fn from_form(
                __items: &mut ::rocket::request::FormItems<'__f>,
                __strict: bool,
            ) -> ::std::result::Result<Self, ()>
                #inner
            }
        })
        .map_fields(|gen, fields| {
            let (constructors, matchers, builders) = fields.iter().map(|field| {
                let config = Config::from_attr(field.parent_attributes())?;
                let (name, ident, ty) = (config.name(&field), &field.ident, &field.ty);
                let span = field.span().into();

                let constructor = quote_spanned!(span => let #ident: Option<#ty> = None;);

                let matcher = quote_spanned! { span =>
                    #name => #ident = FromFormValue::from_form_value(__v.into())
                    .map_err(|_| FormError::BadValue(__k, __v))?,
                };

                let builder = quote_spanned! { span =>
                    #ident.or_else(|| <#ty as FromFormValue>::default())
                    .ok_or_else(|| FormError::MissingField(#name))?,
                };

                (constructor, matcher, builder)
            }).split3();

            quote! {
                #(#constructors)*

                for (__k, __v) in __items {
                    match __k.as_str() {
                        #(#matchers)*
                        _ if __strict && __k != "_method" => {
                            return Err(FormError::Unknown(__k, __v));
                        }
                        _ => { /* lenient or "method"; let it pass */ }
                    }
                }

                Self { #(#builders)* }
            }
        })
        .to_tokens()
}
