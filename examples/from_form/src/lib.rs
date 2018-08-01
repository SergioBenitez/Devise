#![feature(proc_macro_diagnostic)]

#[macro_use] extern crate quote;
#[macro_use] extern crate derive_utils;
extern crate proc_macro;

use proc_macro::TokenStream;
use derive_utils::{*, ext::*};

// #[derive(FromAttr)]
// struct Form {
//     field: Option<String>
// }

// impl Form {
//     fn name(self, field: &Field) -> String {
//         self.field.unwrap_or(field.ident.to_string())
//     }
// }

#[proc_macro_derive(FromForm)]
pub fn derive_from_form(input: TokenStream) -> TokenStream {
    DeriveGenerator::build_for(input, "::rocket::request::FromForm<'__f>")
        .generic_support(GenericSupport::Lifetime | GenericSupport::Type)
        .replace_generic(0, 0)
        .data_support(DataSupport::NamedStruct)
        .map_type_generic(|_, ident, _| quote! {
            #ident : ::rocket::request::FromFormValue<'__f>
        })
        // .validate_attributes(Form::validate_attrs)
        .validate_generics(|_, generics| match generics.lifetimes().count() > 1 {
            true => Err(generics.span().error("only one lifetime is supported")),
            false => Ok(())
        })
        .validate_struct(|gen, data| match data.fields.is_empty() {
            true => Err(gen.input.span().error("at least one field is required")),
            false => Ok(())
        })
        .function(|_, inner| quote! {
            type Error = FormError<'__f>;

            fn from_form(
                __items: &mut ::rocket::request::FormItems<'__f>,
                __strict: bool,
            ) -> ::std::result::Result<Self, Self::Error> {
                #inner
                unimplemented!()
            }
        })
        // .map_fields(|gen, fields| {
        //     let (constructors, matchers, builders) = fields.iter().map(|field| {
        //         let attr = Form::from_attr(field.parent_attr())?;
        //         let (name, ident, ty) = (attr.into_name(&field), &field.ident, &field.ty);
        //         let span = field.span().into();

        //         let constructor = quote_spanned!(span => let #ident: Option<#ty> = None;);

        //         let matcher = quote_spanned! { span =>
        //             #name => #ident = FromFormValue::from_form_value(__v.into())
        //             .map_err(|_| FormError::BadValue(__k, __v))?,
        //         };

        //         let builder = quote_spanned! { span =>
        //             #ident.or_else(|| <#ty as FromFormValue>::default())
        //             .ok_or_else(|| FormError::MissingField(#name))?,
        //         };

        //         (constructor, matcher, builder)
        //     }).split3();

        //     quote! {
        //         #(#constructors)*

        //         for (__k, __v) in __items {
        //             match __k.as_str() {
        //                 #(#matchers)*
        //                 _ if __strict && __k != "_method" => {
        //                     return Err(FormError::Unknown(__k, __v));
        //                 }
        //                 _ => { /* lenient or "method"; let it pass */ }
        //             }
        //         }

        //         Self { #(#builders)* }
        //     }
        // })
        .to_tokens()
}
