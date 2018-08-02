#![feature(proc_macro_diagnostic)]

#[macro_use] extern crate quote;
#[macro_use] extern crate derive_utils;
extern crate proc_macro;

use proc_macro::{Span, TokenStream};
use derive_utils::{*, syn, ext::{TypeExt, Split3}};

#[derive(FromMeta)]
struct Form {
    field: FormField,
}

struct FormField {
    span: Span,
    name: String
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

        Ok(FormField { span: string.value_span, name: string.value })
    }
}

fn validate_struct(gen: &DeriveGenerator, data: Struct) -> Result<()> {
    if data.fields().count() == 0 {
        return Err(gen.input.span().error("at least one field is required"));
    }

    let mut names = ::std::collections::HashMap::new();
    for field in data.fields().iter() {
        let id = field.ident.as_ref().expect("named field");
        let field = match Form::from_attrs("form", &field.attrs) {
            Some(result) => result?.field,
            None => FormField { span: Spanned::span(&id), name: id.to_string() }
        };

        if let Some(span) = names.get(&field.name) {
            return Err(field.span.error("duplicate field name")
                       .span_note(*span, "previous definition here"));
        }

        names.insert(field.name, field.span);
    }

    Ok(())
}

#[proc_macro_derive(FromForm, attributes(form))]
pub fn derive_from_form(input: TokenStream) -> TokenStream {
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
        .validate_struct(validate_struct)
        .function(|_, inner| quote! {
            type Error = FormError<'__f>;

            fn from_form(
                __items: &mut ::rocket::request::FormItems<'__f>,
                __strict: bool,
            ) -> ::std::result::Result<Self, Self::Error> {
                #inner
            }
        })
        .try_map_fields(|_, fields| {
            let (constructors, matchers, builders) = fields.iter().map(|field| {
                let (ident, span) = (&field.ident, field.span().into());
                let default_name = ident.as_ref().expect("named").to_string();
                let name = Form::from_attrs("form", &field.attrs)
                    .map(|result| result.map(|form| form.field.name))
                    .unwrap_or_else(|| Ok(default_name))?;

                let ty = field.ty.with_stripped_lifetimes();
                let ty = quote!(<#ty as ::rocket::request::FromFormValue>);

                let constructor = quote_spanned!(span => let mut #ident = None;);

                let matcher = quote_spanned! { span =>
                    #name => { #ident = Some(#ty::from_form_value(__v)
                                .map_err(|_| FormError::BadValue(__k, __v))?); },
                };

                let builder = quote_spanned! { span =>
                    #ident: #ident.or_else(#ty::default)
                        .ok_or_else(|| FormError::Missing(#name.into()))?,
                };

                Ok((constructor, matcher, builder))
            }).collect::<Result<Vec<_>>>()?.into_iter().split3();

            Ok(quote! {
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
            })
        })
        .to_tokens()
}
