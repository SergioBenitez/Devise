#[macro_use] extern crate from_form_value;

// Mock what the derive uses from `rocket`.
pub mod rocket {
    pub mod http {
        pub struct RawStr;

        impl RawStr {
            pub fn as_uncased_str(&self) -> &str { unimplemented!() }
        }
    }

    pub mod request {
        pub trait FromFormValue<'a>: Sized {
            type Error;

            fn from_form_value(
                value: &'a ::rocket::http::RawStr
            ) -> Result<Self, Self::Error>;
        }
    }
}

#[derive(FromFormValue)]
enum Foo {
    OptionA,
    ThenB,
    Other
}

// #[derive(FromFormValue)]
// enum Bar<T> {
//     OptionA(T),
// }

// #[derive(FromFormValue)]
// enum Bar2 {
//     OptionA(usize),
// }

pub fn main() { }
