#[macro_use] extern crate from_form_value;
extern crate rocket;

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

pub fn main() { }
