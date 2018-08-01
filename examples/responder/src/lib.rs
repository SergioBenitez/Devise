#![feature(proc_macro_diagnostic)]

#[macro_use] extern crate quote;
#[macro_use] extern crate derive_utils;
extern crate proc_macro;
extern crate proc_macro2;

use quote::ToTokens;
use proc_macro::{Span, TokenStream};
use derive_utils::{*, ext::*};

// #[derive(FromAttr)]
// struct Response {
//     content_type: ContentType,
//     status: Status,
//     responder: Member,
//     ignore: Vec<Member>,
// }

// #[proc_macro_derive(Responder)]
// pub fn derive_responder(input: TokenStream) -> TokenStream {
//     DeriveGenerator::build_for(input, "::rocket::Responder<'__r>")
//         .generic_support(GenericSupport::None)
//         .data_support(DataSupport::Struct | DataSupport::Enum)
//         .replace_generic(0, 0)
//         .validate_struct(validate_struct)
//         .validate_enum(validate_enum)
//         .function(|_, inner| quote! {
//             fn respond_to(self, _req: &Request) -> ::rocket::response::Result<'__r> {
//                 #inner
//             }
//         })
//         .map_fields(|gen, fields| {
//             fn set_header_tokens<T: ToTokens>(span: Span, item: T) -> TokenStream2 {
//                 quote_spanned!(span => _res.set_header(#item);)
//             }

//             let fields = fields.iter().enumerate().map(|(index, field)| {
//                 let config = Response::from_attr(field.parent_attributes())?;
//                 let (span, accessor) = (field.span().into(), field.accessor());

//                 let tokens = if config.is_responder(&field) {
//                     quote_spanned!(span => let mut _res = #accessor.respond_to(_req);)
//                 } else {
//                     set_header_tokens(span, accessor)
//                 };

//                 (config.is_responder(&field), tokens)
//             });

//             let responder = fields.iter().filter(|f| f.0).map(|f| f.1).next();
//             let headers = fields.iter().filter(|f| !f.0).map(|f| f.1);

//             let config = Response::from_attr(fields.parent_attributes())?;
//             let content_type = config.content_type().map(set_header_tokens);
//             let status = config.status().map(set_header_tokens);

//             quote! {
//                 #responder
//                 #(#headers)*
//                 #content_type
//                 #status
//                 Ok(_res)
//             }
//         })
//         .to_tokens()
// }
