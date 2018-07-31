#![feature(proc_macro_diagnostic, proc_macro_span)]
#![feature(crate_visibility_modifier)]
#![recursion_limit="256"]

extern crate syn;
extern crate proc_macro;
extern crate proc_macro2;
#[macro_use] extern crate quote;
#[macro_use] extern crate bitflags;

mod spanned;
mod field;
mod variant;
mod generator;
mod support;

pub mod ext;
pub mod parser;

pub use field::Field;
pub use variant::Variant;
pub use support::{GenericSupport, DataSupport};
pub use generator::*;
pub use spanned::*;
