#[macro_use] extern crate quote;
extern crate devise;
extern crate proc_macro;

use proc_macro::TokenStream;
use devise::*;

#[proc_macro_derive(Example0)]
pub fn derive_generic_example(input: TokenStream) -> TokenStream {
    DeriveGenerator::build_for(input, quote!(impl<T> Example0<T>))
        .generic_support(GenericSupport::All)
        .data_support(DataSupport::All)
        .replace_generic(0, 0)
        .function(|_, _| quote! {
            fn example(self) -> T { unimplemented!() }
        })
        .to_tokens()
}

#[proc_macro_derive(Example0usize)]
pub fn derive_concrete_generic(input: TokenStream) -> TokenStream {
    DeriveGenerator::build_for(input, quote!(impl Example0<usize>))
        .generic_support(GenericSupport::All)
        .data_support(DataSupport::All)
        .function(|_, _| quote! {
            fn example(self) -> usize { unimplemented!() }
        })
        .to_tokens()
}
