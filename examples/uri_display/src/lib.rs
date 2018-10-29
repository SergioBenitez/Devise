#![feature(proc_macro_diagnostic)]

#[macro_use] extern crate quote;
extern crate devise;
extern crate proc_macro;

use proc_macro::{TokenStream, Span};
use devise::*;

const NO_EMPTY_FIELDS: &str = "fieldless structs or variants are not allowed";
const NO_NULLARY: &str = "nullary items are not allowed";
const NO_EMPTY_ENUMS: &str = "empty enums are not allowed";
const ONLY_ONE_UNNAMED: &str = "tuple structs or variants must have exactly one field";

fn validate_fields(fields: Fields, parent_span: Span) -> Result<()> {
    if fields.count() == 0 {
        return Err(parent_span.error(NO_EMPTY_FIELDS))
    } else if fields.are_unnamed() && fields.count() > 1 {
        return Err(fields.span().error(ONLY_ONE_UNNAMED));
    } else if fields.are_unit() {
        return Err(parent_span.error(NO_NULLARY));
    }

    Ok(())
}

fn validate_struct(gen: &DeriveGenerator, data: Struct) -> Result<()> {
    validate_fields(data.fields(), gen.input.span())
}

fn validate_enum(gen: &DeriveGenerator, data: Enum) -> Result<()> {
    if data.variants().count() == 0 {
        return Err(gen.input.span().error(NO_EMPTY_ENUMS));
    }

    for variant in data.variants() {
        validate_fields(variant.fields(), variant.span())?;
    }

    Ok(())
}

#[proc_macro_derive(UriDisplay)]
pub fn derive_uri_display(input: TokenStream) -> TokenStream {
    // DeriveGenerator::build_for(input, "::rocket::uri::UriDisplay")
    DeriveGenerator::build_for(input, "UriDisplay")
        .generic_support(GenericSupport::Type | GenericSupport::Lifetime)
        .data_support(DataSupport::Struct | DataSupport::Enum)
        .validate_enum(validate_enum)
        .validate_struct(validate_struct)
        // .map_type_generic(|_, ident, _| quote!(#ident : ::rocket::uri::UriDisplay))
        .map_type_generic(|_, ident, _| quote!(#ident : UriDisplay))
        .function(|_, inner| quote! {
            // fn fmt(&self, f: &mut ::rocket::uri::Formatter) -> ::std::fmt::Result {
            fn fmt(&self, f: &mut Formatter) -> ::std::fmt::Result {
                #inner
                Ok(())
            }
        })
        .map_field(|_, field| {
            let span = field.span().into();
            let accessor = field.accessor();
            if let Some(ref ident) = field.ident {
                let name = ident.to_string();
                quote_spanned!(span => f.write_named_value(#name, &#accessor)?;)
            } else {
                quote_spanned!(span => f.write_value(&#accessor)?;)
            }
        })
        .to_tokens()
}
