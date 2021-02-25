use quote::ToTokens;
use proc_macro2::TokenStream;

use devise::{*, ext::SpanDiagnosticExt};

pub struct ContentType(String, String);

pub struct Status(u16);

impl FromMeta for Status {
    fn from_meta(meta: &MetaItem) -> Result<Self> {
        let num = usize::from_meta(meta)?;
        if num < 100 || num >= 600 {
            return Err(meta.value_span().error("status must be in range [100, 600)"));
        }

        Ok(Status(num as u16))
    }
}

impl ToTokens for Status {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let code = self.0;
        tokens.extend(quote!(rocket::http::Status(#code)));
    }
}

impl FromMeta for ContentType {
    fn from_meta(meta: &MetaItem) -> Result<Self> {
        let string = String::from_meta(meta)?;
        let splits = string.split('/').collect::<Vec<_>>();
        if splits.len() != 2 {
            return Err(meta.value_span().error("invalid or unknown content-type"));
        }

        Ok(ContentType(splits[0].into(), splits[1].into()))
    }
}

impl ToTokens for ContentType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let (top, bottom) = (&self.0, &self.1);
        tokens.extend(quote!(::rocket::http::ContentType(#top, #bottom)));
    }
}
