#![recursion_limit="256"]

pub extern crate syn;
pub extern crate proc_macro2;
pub extern crate proc_macro2_diagnostics;
#[macro_use] pub extern crate quote;

extern crate proc_macro;
#[macro_use] extern crate bitflags;

mod field;
mod generator;
mod support;
mod derived;
mod from_meta;

pub mod ext;

pub use field::*;
pub use support::{GenericSupport, DataSupport};
pub use generator::*;
pub use from_meta::*;
pub use derived::*;
pub use proc_macro2_diagnostics::*;
