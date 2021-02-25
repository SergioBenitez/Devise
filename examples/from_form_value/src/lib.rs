#[macro_use] extern crate quote;
extern crate devise;
extern crate proc_macro;

use proc_macro::TokenStream;
use devise::{*, ext::SpanDiagnosticExt};

#[proc_macro_derive(FromFormValue)]
pub fn derive_from_form_value(input: TokenStream) -> TokenStream {
    DeriveGenerator::build_for(input, quote!(impl<'__v> ::rocket::request::FromFormValue<'__v>))
        .support(Support::Enum)
        .validator(ValidatorBuild::new()
            .enum_validate(|_, data| {
                // This derive only works for variants that are nullary.
                for variant in data.variants() {
                    if !variant.fields().is_empty() {
                        return Err(variant.span().error("variants cannot have fields"));
                    }
                }

                // Emit a warning if the enum is empty.
                if data.variants.is_empty() {
                    return Err(data.span().warning("deriving for empty enum"));
                }

                Ok(())
            })
        )
        .inner_mapper(MapperBuild::new()
            .with_output(|_, output| quote! {
                type Error = &'__v ::rocket::http::RawStr;

                fn from_form_value(
                    __value: &'__v ::rocket::http::RawStr
                ) -> ::std::result::Result<Self, Self::Error> {
                    let __uncased = __value.as_uncased_str();
                    #output
                    ::std::result::Result::Err(__value)
                }
            })
            .try_enum_map(|m, e| mapper::enum_null(m, e))
            .variant_map(|_, variant| {
                let variant_str = variant.ident.to_string();
                let builder = variant.builder(|_| unreachable!());
                quote! {
                    if __uncased == #variant_str {
                        return ::std::result::Result::Ok(#builder);
                    }
                }
            })
        )
        .to_tokens()
}
