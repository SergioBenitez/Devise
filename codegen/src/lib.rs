#![feature(proc_macro_diagnostic, proc_macro_span)]
#![recursion_limit="256"]

extern crate proc_macro;
#[macro_use] extern crate quote;
extern crate derive_utils_core;

use proc_macro::TokenStream;
use derive_utils_core::{*, ext::Split3};

#[proc_macro_derive(FromMeta)]
pub fn derive_from_meta(input: TokenStream) -> TokenStream {
    DeriveGenerator::build_for(input, "::derive_utils::FromMeta")
        .data_support(DataSupport::NamedStruct)
        .function(|_, inner| quote! {
            fn from_meta(
                __meta: &::derive_utils::syn::Meta
            ) -> ::derive_utils::Result<Self> {
                #inner
            }
        })
        .map_struct(|_, data| {
            let (constructors, matchers, builders) = data.fields().iter().map(|f| {
                let (ident, span) = (f.ident.as_ref().unwrap(), f.span().into());
                let (name, ty) = (ident.to_string(), &f.ty);

                let constructor = quote_spanned!(span => let mut #ident = None;);

                let matcher = quote_spanned! { span =>
                    if __meta.name() == #name {
                        if #ident.is_some() {
                            return Err(__meta.span().error(format!(
                                        "duplicate attribute parameter: {}", #name)));
                        }

                        #ident = Some(<#ty>::from_meta(&__meta)?);
                        continue;
                    }
                };

                let builder = quote_spanned! { span =>
                    #ident: #ident.or_else(::derive_utils::FromMeta::default)
                        .ok_or_else(|| __meta.span().error(format!(
                                    "missing required attribute parameter: `{}`", #name)))?,
                };

                (constructor, matcher, builder)
            }).split3();

            quote! {
                let __list = match __meta {
                    ::derive_utils::syn::Meta::List(ref __l) => __l,
                    _ => return Err(__meta.span()
                                    .error("malformed attribute")
                                    .help("expected syntax: #[attr(key = value, ..)]"))
                };

                #(#constructors)*

                for __nested in &__list.nested {
                    let __meta = match __nested {
                        ::derive_utils::syn::NestedMeta::Meta(__m) => __m,
                        _ => return Err(__nested.span().error("unexpected literal"))
                    };

                    #(#matchers)*

                    let __msg = format!("unexpected attribute parameter: {}", __meta.name());
                    return Err(__meta.span().error(__msg));
                }

                Ok(Self { #(#builders)* })
            }
        })
        .to_tokens()
}
