#[macro_use] extern crate from_form;
extern crate rocket;

#[derive(FromForm)]
struct TodoTask {
    description: String,
    completed: bool
}

pub fn main() { }
