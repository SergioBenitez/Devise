#![allow(dead_code)]

#[macro_use] extern crate from_form;
extern crate rocket;

use rocket::http::RawStr;

enum FormError<'f> {
    MissingField(&'f RawStr)
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

#[derive(FromForm)]
struct TodoTask<'f, 'a> {
    description: String,
    raw_description: &'f RawStr,
    other: &'a RawStr,
    completed: bool,
}

pub fn main() { }
