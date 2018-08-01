#![feature(proc_macro_diagnostic)]

#[macro_use] extern crate quote;
#[macro_use] extern crate derive_utils;
extern crate proc_macro;
extern crate syn;

use proc_macro::TokenStream;
use derive_utils::{*, ext::*, meta::{FromMeta, SpanWrapped}};

struct FormField(String);

// #[derive(FromMeta)] // FIXME: Use this.
struct Form {
    field: FormField
}

fn is_valid_field_name(s: &str) -> bool {
    // The HTML5 spec (4.10.18.1) says 'isindex' is not allowed.
    if s == "isindex" || s.is_empty() {
        return false
    }

    // We allow all visible ASCII characters except '&', '=', and '?' since we
    // use those as control characters for parsing.
    s.chars().all(|c| (c >= ' ' && c <= '~') && c != '&' && c != '=' && c != '?')
}

impl FromMeta for FormField {
    fn from_meta(meta: &syn::Meta) -> Result<Self> {
        let string = <SpanWrapped<String>>::from_meta(meta)?;
        if !is_valid_field_name(&string.value) {
            return Err(string.value_span.error("invalid form field name"));
        }

        Ok(FormField(string.value))
    }
}

// FIXME: Code generate this.
impl FromMeta for Form {
    fn from_meta(meta: &syn::Meta) -> Result<Self> {
        let list = match meta {
            syn::Meta::List(ref list) => list,
            _ => return Err(meta.span()
                            .error("malformed attribute")
                            .help("expected syntax: (key = value, ..)"))
        };

        let mut field = None;
        for nested_meta in &list.nested {
            let meta = match nested_meta {
                syn::NestedMeta::Meta(meta) => meta,
                _ => return Err(nested_meta.span().error("unexpected literal"))
            };

            if meta.name() == "field" {
                if field.is_some() {
                    return Err(meta.span().error("duplicate parameter: field"));
                }

                field = Some(FormField::from_meta(&meta)?);
                continue;
            }

            let msg = format!("unexpected attribute parameter: {}", meta.name());
            return Err(meta.span().error(msg));
        }

        Ok(Form {
            field: field.or_else(<FormField as FromMeta>::default)
                .ok_or_else(|| meta.span().error("missing field: `field`"))?,
        })
    }
}

#[proc_macro_derive(FromForm, attributes(form))]
pub fn derive_from_form(input: TokenStream) -> TokenStream {
    // FIXME: Allow non-fn coercing closures to be passed in. Then, move
    // ::rocket::request::FromFormValue out and capture in closures.
    // FIXME: Allow the mappers and stuff to fail. Perhaps have to different
    // kinds of mappers, fallible and non-fallible to differentiate? Internally
    // store only as fallable by doing `non_fallible(); Ok(())` in non-fallible.
    // Call the methods `try_map_struct` and so on.
    DeriveGenerator::build_for(input, "::rocket::request::FromForm<'__f>")
        .generic_support(GenericSupport::Lifetime | GenericSupport::Type)
        .replace_generic(0, 0)
        .data_support(DataSupport::NamedStruct)
        .map_type_generic(|_, ident, _| quote! {
            #ident : ::rocket::request::FromFormValue<'__f>
        })
        .validate_generics(|_, generics| match generics.lifetimes().count() > 1 {
            true => Err(generics.span().error("only one lifetime is supported")),
            false => Ok(())
        })
        // FIXME: Check that no two field names (including renames) are the same.
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
            }
        })
        .map_struct(|_, data| {
            let (constructors, matchers, builders) = data.fields().map(|field| {
                let (ident, span) = (&field.ident, field.span().into());
                let ty = field.ty.with_stripped_lifetimes();
                let name = match Form::from_attrs("form", &field.attrs) {
                    Some(Ok(f)) => f.field.0,
                    Some(Err(e)) => { e.emit(); panic!("boo") },
                    _ => field.ident.clone().unwrap().to_string()
                };

                let constructor = quote_spanned!(span => let mut #ident = None;);

                let matcher = quote_spanned! { span =>
                    #name => { #ident = ::rocket::request::FromFormValue::from_form_value(__v.into())
                        .map_err(|_| FormError::BadValue(__k, __v))? },
                };

                let builder = quote_spanned! { span =>
                    #ident: #ident.or_else(<#ty as ::rocket::request::FromFormValue>::default)
                    .ok_or_else(|| FormError::Missing(#name.into()))?,
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

                Ok(Self { #(#builders)* })
            }
        })
        .to_tokens()
}
