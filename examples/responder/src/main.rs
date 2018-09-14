#[macro_use] extern crate responder;
extern crate rocket;

#[derive(Responder)]
pub enum Foo<'r> {
    First(String),
    #[response(status = 500)]
    Second(Vec<u8>),
    #[response(status = 404, content_type = "html")]
    Third {
        responder: &'r str,
        ct: ::rocket::http::ContentType,
    },
}

#[derive(Responder)]
#[response(content_type = "plain")]
pub struct Bar<'r> {
    responder: Foo<'r>,
    other: ::rocket::http::ContentType,
    #[response(ignore)]
    _yet_another: String,
}

pub fn main() { }
