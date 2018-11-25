#![allow(dead_code)]

#[macro_use] extern crate generics;

trait Example0<T> {
    fn example(self) -> T;
}

#[derive(Example0)]
struct Foo;

#[derive(Example0)]
struct Bar<'a, A, B>(&'a (A, B));

#[derive(Example0)]
struct Baz<'a>(&'a usize);

#[derive(Example0usize)]
struct Foo1;

#[derive(Example0usize)]
struct Bar1<'a, A, B>(&'a (A, B));

#[derive(Example0usize)]
struct Baz1<'a>(&'a usize);

pub fn main() { }
