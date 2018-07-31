#[macro_use] extern crate from_form_value;
extern crate rocket;

#[derive(FromFormValue)]
enum Foo {
    OptionA,
    ThenB,
    Other
}

pub fn main() { }
