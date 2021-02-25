#[macro_use] extern crate quote;
extern crate devise;
extern crate proc_macro;

use devise::*;
use proc_macro::TokenStream;

#[proc_macro_derive(Example0)]
pub fn derive_generic_example(input: TokenStream) -> TokenStream {
    DeriveGenerator::build_for(input, quote!(impl<T> Example0<T>))
        .support(Support::All)
        .replace_generic(0, 0)
        .inner_mapper(MapperBuild::new()
            .with_output(|_, _| quote! {
                fn example(self) -> T { unimplemented!() }
            })
        )
        .to_tokens()
}

#[proc_macro_derive(Example0usize)]
pub fn derive_concrete_generic(input: TokenStream) -> TokenStream {
    DeriveGenerator::build_for(input, quote!(impl Example0<usize>))
        .support(Support::All)
        .inner_mapper(MapperBuild::new()
            .with_output(|_, _| quote! {
                fn example(self) -> usize { unimplemented!() }
            })
        )
        .to_tokens()
}

#[proc_macro_derive(GenExample)]
pub fn derive_gen_example(input: TokenStream) -> TokenStream {
    DeriveGenerator::build_for(input, quote!(impl<'__a, '__b: '__a> GenExample<'__a, '__b>))
        .support(Support::All)
        .replace_generic(1, 0)
        .inner_mapper(MapperBuild::new()
            .with_output(|_, _| quote! {
                fn gen(&self, s: &'__a str) -> &'__b str { self.0 }
            })
        )
        .to_tokens()
}
