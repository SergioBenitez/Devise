#[macro_use] extern crate uri_display;
extern crate rocket;
extern crate smallvec;

use std::fmt;
use smallvec::SmallVec;
use rocket::http::uri::Uri;

pub struct Formatter<'i, 'f: 'i> {
    prefixes: SmallVec<[&'static str; 3]>,
    inner: &'i mut fmt::Formatter<'f>,
    previous: bool,
    fresh: bool
}

impl<'i, 'f: 'i> Formatter<'i, 'f> {
    pub fn write_raw<S: AsRef<str>>(&mut self, s: S) -> fmt::Result {
        let s = s.as_ref();
        if self.fresh && !self.prefixes.is_empty() {
            if self.previous {
                self.inner.write_str("&")?;
            }

            self.fresh = false;
            self.previous = true;

            for (i, prefix) in self.prefixes.iter().enumerate() {
                self.inner.write_str(prefix)?;
                if i < self.prefixes.len() - 1 {
                    self.inner.write_str(".")?;
                }
            }

            self.inner.write_str("=")?;
        }

        self.inner.write_str(s)
    }

    fn with_prefix<F>(&mut self, prefix: &str, f: F) -> fmt::Result
        where F: FnOnce(&mut Self) -> fmt::Result
    {
        self.fresh = true;

        // TODO: PROOF OF CORRECTNESS.
        let prefix: &'static str = unsafe { ::std::mem::transmute(prefix) };
        self.prefixes.push(prefix);

        let result = f(self);

        self.prefixes.pop();
        result
    }

    pub fn write_seq_value<T: UriDisplay>(&mut self, value: T) -> fmt::Result {
        self.fresh = true;
        self.write_value(value)
    }

    pub fn write_named_seq_value<T: UriDisplay>(&mut self, name: &str, value: T) -> fmt::Result {
        self.write_named_value(name, value)
    }

    #[inline]
    pub fn write_named_value<T: UriDisplay>(&mut self, name: &str, value: T) -> fmt::Result {
        self.with_prefix(name, |f| f.write_value(value))
    }

    #[inline]
    pub fn write_value<T: UriDisplay>(&mut self, value: T) -> fmt::Result {
        UriDisplay::fmt(&value, self)
    }
}

impl<'f, 'i: 'f> fmt::Write for Formatter<'f, 'i> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.write_raw(s)
    }
}

pub trait UriDisplay {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result;
}

impl UriDisplay for str {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        f.write_raw(&Uri::percent_encode(self))
    }
}

impl<'a> UriDisplay for &'a str {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        UriDisplay::fmt(*self, f)
    }
}

impl UriDisplay for u8 {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use fmt::Write;
        write!(f, "{}", self)
    }
}

impl<'a, T: UriDisplay> UriDisplay for &'a T {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        UriDisplay::fmt(*self, f)
    }
}
impl<'a> fmt::Display for &'a UriDisplay {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut formatter = Formatter {
            prefixes: SmallVec::new(),
            inner: f,
            previous: false,
            fresh: true,
        };

        UriDisplay::fmt(*self, &mut formatter)
    }
}

#[derive(UriDisplay)]
pub enum Or {
    A {
        foo: &'static str,
        bar: u8,
    },
    B(u8),
}

#[derive(UriDisplay)]
pub struct Other<T> {
    field: &'static str,
    next: T,
    one_more: Or,
}

#[derive(UriDisplay)]
pub struct WrappedThing<T> {
    inner: Other<u8>,
    other: T,
}

// #[derive(UriDisplay)]
// pub struct Bad {
//     inner: usize,
// }

// #[derive(UriDisplay)]
// pub enum Worse {
//     Some,
// }

pub fn main() { }
