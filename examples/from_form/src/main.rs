#![allow(dead_code)]

#[macro_use] extern crate from_form;
extern crate rocket;

use rocket::http::RawStr;

enum FormError<'f> {
    Missing(&'f RawStr),
    BadValue(&'f RawStr, &'f RawStr),
    Unknown(&'f RawStr, &'f RawStr),
}

#[derive(FromForm)]
struct TodoTask<'f> {
    description: String,
    raw_description: &'f RawStr,
    completed: bool,
}

#[derive(FromForm)]
struct Other {
    description: String,
    completed: bool,
}

// #[derive(FromForm)]
// struct NextTodoTask<'f, 'a> {
//     description: String,
//     raw_description: &'f RawStr,
//     other: &'a RawStr,
//     completed: bool,
// }

#[derive(FromForm)]
struct Another {
    #[form(field = "type")]
    field: String,
    #[form(field = "a b")]
    other: usize,
}

pub fn main() { }
