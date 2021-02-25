#[macro_use] extern crate quote;
extern crate devise;
extern crate proc_macro;

use proc_macro::TokenStream;
use devise::proc_macro2::Span;
use devise::{*, ext::SpanDiagnosticExt, syn::spanned::Spanned};

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

fn validate_struct(data: Struct) -> Result<()> {
    validate_fields(data.fields(), data.span())
}

fn validate_enum(data: Enum) -> Result<()> {
    if data.variants().count() == 0 {
        return Err(data.span().error(NO_EMPTY_ENUMS));
    }

    for variant in data.variants() {
        validate_fields(variant.fields(), variant.span())?;
    }

    Ok(())
}

#[proc_macro_derive(UriDisplay)]
pub fn derive_uri_display(input: TokenStream) -> TokenStream {
    DeriveGenerator::build_for(input, quote!(impl UriDisplay))
        .support(Support::Struct | Support::Enum | Support::AllGeneric)
        .type_bound(quote!(UriDisplay))
        .validator(ValidatorBuild::new()
            .enum_validate(|_, e| validate_enum(e))
            .struct_validate(|_, e| validate_struct(e))
        )
        .inner_mapper(MapperBuild::new()
            .with_output(|_, output| quote! {
                fn fmt(&self, f: &mut Formatter) -> ::std::fmt::Result {
                    #output
                    Ok(())
                }
            })
            .field_map(|_, field| {
                let span = field.span().into();
                let accessor = field.accessor();
                if let Some(ref ident) = field.ident {
                    let name = ident.to_string();
                    quote_spanned!(span => f.write_named_value(#name, &#accessor)?;)
                } else {
                    quote_spanned!(span => f.write_value(&#accessor)?;)
                }
            })
        )
        .to_tokens()
}
