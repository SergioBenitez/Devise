extern crate derive_utils_core;

// Magic incantantion to reexport proc-macros from codegen crate.
#[allow(unused_imports)] #[macro_use] extern crate derive_utils_codegen;
#[doc(hidden)] pub use derive_utils_codegen::*;

pub use derive_utils_core::*;
