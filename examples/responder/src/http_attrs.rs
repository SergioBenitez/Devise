use syn;
use quote::ToTokens;
use proc_macro2::TokenStream as TokenStream2;
use derive_utils::{SpanWrapped, FromMeta, Result, ext::Split2};
use rocket_http as http;

pub struct ContentType(http::ContentType);

pub struct Status(http::Status);

struct MediaType(http::MediaType);

impl FromMeta for Status {
    fn from_meta(meta: &syn::Meta) -> Result<Self> {
        let num = <SpanWrapped<usize>>::from_meta(meta)?;
        if num.value < 100 || num.value >= 600 {
            return Err(num.value_span.error("status must be in range [100, 600)"));
        }

        Ok(Status(http::Status::raw(num.value as u16)))
    }
}

impl ToTokens for Status {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let (code, reason) = (self.0.code, self.0.reason);
        tokens.extend(quote!(rocket::http::Status::new(#code, #reason)));
    }
}

impl FromMeta for ContentType {
    fn from_meta(meta: &syn::Meta) -> Result<Self> {
        let s = <SpanWrapped<String>>::from_meta(meta)?;
        let parsed = http::ContentType::parse_flexible(&s.value)
            .ok_or_else(|| s.value_span.error("invalid or unknown content-type"))?;

        Ok(ContentType(parsed))
    }
}

impl ToTokens for ContentType {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        // Yeah, yeah. (((((i))).kn0w()))
        let media_type = MediaType((self.0).clone().0);
        tokens.extend(quote!(::rocket::http::ContentType(#media_type)));
    }
}

impl ToTokens for MediaType {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        use std::iter::repeat;
        let (top, sub) = (self.0.top().as_str(), self.0.sub().as_str());
        let (keys, values) = self.0.params().split2();

        let (http, cow) = (quote!(::rocket::http), quote!(::std::borrow::Cow));
        let (http_, http__) = (repeat(&http), repeat(&http));
        let (cow_, cow__) = (repeat(&cow), repeat(&cow));

        // TODO: Produce less code when possible (for known media types).
        tokens.extend(quote!(#http::MediaType {
            source: #http::Source::None,
            top: #http::Indexed::Concrete(#cow::Borrowed(#top)),
            sub: #http::Indexed::Concrete(#cow::Borrowed(#sub)),
            params: #http::MediaParams::Static(&[
                #((
                    #http_::Indexed::Concrete(#cow_::Borrowed(#keys)),
                    #http__::Indexed::Concrete(#cow__::Borrowed(#values))
                )),*
            ])
        }))
    }
}
