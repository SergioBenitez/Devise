#[macro_use] extern crate responder;

pub mod rocket {
    pub struct Request<'r>(&'r str);

    pub mod http {
        pub struct ContentType(pub &'static str, pub &'static str);

        pub struct Status(pub u16);
    }

    pub mod response {
        pub type Result<'o> = std::result::Result<Response<'o>, ()>;

        pub struct Response<'r>(&'r str);

        impl Response<'_> {
            pub fn set_header<T>(&mut self, _header: T) {}
            pub fn set_status(&mut self, _status: super::http::Status) {}
        }

        pub trait Responder<'r, 'o: 'r> {
            fn respond_to(self, req: &'r super::Request<'_>) -> Result<'o>;
        }

        impl<'r> Responder<'r, 'static> for String {
            fn respond_to(self, _: &'r super::Request<'_>) -> Result<'static> {
                todo!()
            }
        }

        impl<'r, T> Responder<'r, 'static> for Vec<T> {
            fn respond_to(self, _: &'r super::Request<'_>) -> Result<'static> {
                todo!()
            }
        }

        impl<'r, 'o: 'r> Responder<'r, 'o> for &'o str {
            fn respond_to(self, _: &'r super::Request<'_>) -> Result<'o> {
                Ok(Response(self))
            }
        }
    }
}

#[derive(Responder)]
pub enum Foo<'r> {
    First(String),
    #[response(status = 500)]
    Second(Vec<u8>),
    #[response(status = 404, content_type = "text/html")]
    Third {
        responder: &'r str,
        ct: ::rocket::http::ContentType,
    },
}

#[derive(Responder)]
#[response(content_type = "text/plain")]
pub struct Bar<'r> {
    responder: Foo<'r>,
    other: ::rocket::http::ContentType,
    #[response(ignore)]
    _yet_another: String,
}

#[derive(Responder)]
pub enum Baz {
    First(String),
    #[response(status = 500)]
    Second(Vec<u8>),
}

pub fn main() { }
