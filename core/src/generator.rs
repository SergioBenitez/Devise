use syn;
use proc_macro::{TokenStream, Diagnostic};
use proc_macro2::TokenStream as TokenStream2;

use spanned::Spanned;
use ext::{PathExt, GenericExt};

use field::{Field, Fields};
use support::{GenericSupport, DataSupport};
use derived::{Derived, Variant, Struct, Enum};

pub type Result<T> = ::std::result::Result<T, Diagnostic>;
pub type MapResult = Result<TokenStream2>;

pub type EnumValidatorFn = fn(&DeriveGenerator, Enum) -> Result<()>;
pub type StructValidatorFn = fn(&DeriveGenerator, Struct) -> Result<()>;
pub type GenericsValidatorFn = fn(&DeriveGenerator, &::syn::Generics) -> Result<()>;
pub type FieldsValidatorFn = fn(&DeriveGenerator, Fields) -> Result<()>;

pub type FunctionFn = fn(&DeriveGenerator, TokenStream2) -> TokenStream2;
pub type TypeGenericMapFn = fn(&DeriveGenerator, &syn::Ident, &syn::TypeParam) -> TokenStream2;

macro_rules! validator {
    ($fn_name:ident: $validate_fn_type:ty, $field:ident) => {
        pub fn $fn_name(&mut self, f: $validate_fn_type) -> &mut Self {
            self.$field = f;
            self
        }
    }
}

macro_rules! mappers {
    ($(($map_f:ident, $try_f:ident, $get_f:ident): $type:ty, $vec:ident),*) => (
        crate fn push_default_mappers(&mut self) {
            $(self.$vec.push(Box::new(concat_idents!(default_, $get_f)));)*
        }

        $(
            pub fn $map_f<F>(&mut self, f: F) -> &mut Self
                where F: Fn(&DeriveGenerator, $type) -> TokenStream2 + 'static
            {
                if !self.$vec.is_empty() {
                    let last = self.$vec.len() - 1;
                    self.$vec[last] = Box::new(move |g, v| Ok(f(g, v)));
                }

                self
            }

            pub fn $try_f<F>(&mut self, f: F) -> &mut Self
                where F: Fn(&DeriveGenerator, $type) -> MapResult + 'static
            {
                if !self.$vec.is_empty() {
                    let last = self.$vec.len() - 1;
                    self.$vec[last] = Box::new(f);
                }

                self
            }

            pub fn $get_f(&self) -> &Box<Fn(&DeriveGenerator, $type) -> MapResult> {
                assert!(!self.$vec.is_empty());
                let last = self.$vec.len() - 1;
                &self.$vec[last]
            }
        )*
    )
}

pub struct DeriveGenerator {
    pub input: syn::DeriveInput,
    pub trait_path: syn::Path,
    crate generic_support: GenericSupport,
    crate data_support: DataSupport,
    crate enum_validator: EnumValidatorFn,
    crate struct_validator: StructValidatorFn,
    crate generics_validator: GenericsValidatorFn,
    crate fields_validator: FieldsValidatorFn,
    crate type_generic_mapper: Option<TypeGenericMapFn>,
    crate generic_replacements: Vec<(usize, usize)>,
    crate functions: Vec<FunctionFn>,
    crate enum_mappers: Vec<Box<Fn(&DeriveGenerator, Enum) -> MapResult>>,
    crate struct_mappers: Vec<Box<Fn(&DeriveGenerator, Struct) -> MapResult>>,
    crate variant_mappers: Vec<Box<Fn(&DeriveGenerator, Variant) -> MapResult>>,
    crate fields_mappers: Vec<Box<Fn(&DeriveGenerator, Fields) -> MapResult>>,
    crate field_mappers: Vec<Box<Fn(&DeriveGenerator, Field) -> MapResult>>,
}

pub fn default_enum_mapper(gen: &DeriveGenerator, data: Enum) -> MapResult {
    let variant = data.variants().map(|v| &v.value.ident);
    let fields = data.variants().map(|v| v.fields().match_tokens());
    let enum_name = ::std::iter::repeat(&data.derive_input.ident);
    let expression = data.variants()
        .map(|v| gen.variant_mapper()(gen, v))
        .collect::<Result<Vec<_>>>()?;

    Ok(quote! {
        // FIXME: Check if we can also use id_match_tokens due to match
        // ergonomics. I don't think so, though. If we can't, then ask (in
        // `function`) whether receiver is `&self`, `&mut self` or `self` and
        // bind match accordingly.
        match self {
            #(#enum_name::#variant #fields => { #expression }),*
        }
    })
}

pub fn null_enum_mapper(gen: &DeriveGenerator, data: Enum) -> MapResult {
    let expression = data.variants()
        .map(|v| gen.variant_mapper()(gen, v))
        .collect::<Result<Vec<_>>>()?;

    Ok(quote!(#(#expression)*))
}

pub fn default_struct_mapper(gen: &DeriveGenerator, data: Struct) -> MapResult {
    gen.fields_mapper()(gen, data.fields())
}

pub fn default_variant_mapper(gen: &DeriveGenerator, data: Variant) -> MapResult {
    gen.fields_mapper()(gen, data.fields())
}

pub fn default_field_mapper(_gen: &DeriveGenerator, _data: Field) -> MapResult {
    Ok(TokenStream2::new())
}

pub fn default_fields_mapper(g: &DeriveGenerator, fields: Fields) -> MapResult {
    let field = fields.iter()
        .map(|field| g.field_mapper()(g, field))
        .collect::<Result<Vec<_>>>()?;

    Ok(quote!({ #(#field)* }))
}

impl DeriveGenerator {
    pub fn build_for(input: TokenStream, trait_path: &str) -> DeriveGenerator {
        let trait_path_ts = trait_path.parse().expect("invalid tokens for path");
        let trait_path = syn::parse(trait_path_ts).expect("invalid trait path");
        let input = syn::parse(input).expect("invalid derive input");

        DeriveGenerator {
            input, trait_path,
            generic_support: GenericSupport::None,
            data_support: DataSupport::None,
            type_generic_mapper: None,
            generic_replacements: vec![],
            enum_validator: |_, _| Ok(()),
            struct_validator: |_, _| Ok(()),
            generics_validator: |_, _| Ok(()),
            fields_validator: |_, _| Ok(()),
            functions: vec![],
            enum_mappers: vec![],
            struct_mappers: vec![],
            variant_mappers: vec![],
            field_mappers: vec![],
            fields_mappers: vec![],
        }
    }

    pub fn generic_support(&mut self, support: GenericSupport) -> &mut Self {
        self.generic_support = support;
        self
    }

    pub fn data_support(&mut self, support: DataSupport) -> &mut Self {
        self.data_support = support;
        self
    }

    pub fn map_type_generic(&mut self, f: TypeGenericMapFn) -> &mut Self {
        self.type_generic_mapper = Some(f);
        self
    }

    pub fn replace_generic(&mut self, trait_gen: usize, impl_gen: usize) -> &mut Self {
        self.generic_replacements.push((trait_gen, impl_gen));
        self
    }

    validator!(validate_enum: EnumValidatorFn, enum_validator);
    validator!(validate_struct: StructValidatorFn, struct_validator);
    validator!(validate_generics: GenericsValidatorFn, generics_validator);
    validator!(validate_fields: FieldsValidatorFn, fields_validator);

    pub fn function(&mut self, f: FunctionFn) -> &mut Self {
        self.functions.push(f);
        self.push_default_mappers();
        self
    }

    mappers! {
        (map_struct, try_map_struct, struct_mapper): Struct, struct_mappers,
        (map_enum, try_map_enum, enum_mapper): Enum, enum_mappers,
        (map_variant, try_map_variant, variant_mapper): Variant, variant_mappers,
        (map_fields, try_map_fields, fields_mapper): Fields, fields_mappers,
        (map_field, try_map_field, field_mapper): Field, field_mappers
    }

    fn _to_tokens(&mut self) -> Result<TokenStream> {
        use syn::*;

        // Step 1: Run all validators.
        // Step 1a: First, check for data support.
        let (span, support) = (self.input.span(), self.data_support);
        match self.input.data {
            Data::Struct(ref data) => {
                let named = Struct::from(&self.input, data).fields().are_named();
                if named && !support.contains(DataSupport::NamedStruct) {
                    return Err(span.error("named structs are not supported"));
                }

                if !named && !support.contains(DataSupport::TupleStruct) {
                    return Err(span.error("tuple structs are not supported"));
                }
            }
            Data::Enum(..) if !support.contains(DataSupport::Enum) => {
                return Err(span.error("enums are not supported"));
            }
            Data::Union(..) if !support.contains(DataSupport::Union) => {
                return Err(span.error("unions are not supported"));
            }
            _ => { /* we're okay! */ }
        }

        // Step 1b: Second, check for generics support.
        for generic in &self.input.generics.params {
            use syn::GenericParam::*;

            let (span, support) = (generic.span(), self.generic_support);
            match generic {
                Type(..) if !support.contains(GenericSupport::Type) => {
                    return Err(span.error("type generics are not supported"));
                }
                Lifetime(..) if !support.contains(GenericSupport::Lifetime) => {
                    return Err(span.error("lifetime generics are not supported"));
                }
                Const(..) if !support.contains(GenericSupport::Const) => {
                    return Err(span.error("const generics are not supported"));
                }
                _ => { /* we're okay! */ }
            }
        }

        // Step 1c: Third, run the custom validators.
        (self.generics_validator)(self, &self.input.generics)?;
        match self.input.data {
            Data::Struct(ref data) => {
                let derived = Derived::from(&self.input, data);
                (self.struct_validator)(self, derived)?;
                (self.fields_validator)(self, derived.fields())?;
            }
            Data::Enum(ref data) => {
                let derived = Derived::from(&self.input, data);
                (self.enum_validator)(self, derived)?;
                for variant in derived.variants() {
                    (self.fields_validator)(self, variant.fields())?;
                }
            }
            Data::Union(ref _data) => unimplemented!("union custom validation"),
        }

        // Step 2: Generate the code!
        // Step 2a: Generate the code for each function.
        let mut function_code = vec![];
        for i in 0..self.functions.len() {
            let function = self.functions[i];
            let inner = match self.input.data {
                Data::Struct(ref data) => {
                    let derived = Derived::from(&self.input, data);
                    self.struct_mappers[i](self, derived)?
                }
                Data::Enum(ref data) => {
                    let derived = Derived::from(&self.input, data);
                    self.enum_mappers[i](self, derived)?
                }
                Data::Union(ref _data) => unimplemented!("can't gen unions yet"),
            };

            function_code.push(function(self, inner));
        }

        // Step 2b: Create a couple of generics to mutate with user's input.
        let mut generics = self.input.generics.clone();

        // Step 2c: Add additional where bounds if the user asked for it.
        if let Some(type_mapper) = self.type_generic_mapper {
            for ty in self.input.generics.type_params() {
                let new_ty = type_mapper(self, &ty.ident, ty);
                let clause = syn::parse2(new_ty).expect("yo");
                generics.make_where_clause().predicates.push(clause);
            }
        }

        // Step 2d: Add any generics the user supplied.
        let mut generics_for_impl_generics = generics.clone();
        if let Some(trait_generics) = self.trait_path.generics() {
            for (i, generic) in trait_generics.iter().enumerate() {
                use ::syn::GenericArgument::Lifetime;

                let param: GenericParam = if let Lifetime(lifetime) = generic {
                    LifetimeDef::new(lifetime.clone()).into()
                } else {
                    unimplemented!("can only handle lifetime generics in traits")
                };

                // Step 2d.0: Perform a generic replacement if requested. Here,
                // we determine if this generic in the trait is going to replace
                // a generic in the user's type.
                let replacement = self.generic_replacements.iter()
                    .filter(|r| r.0 == i)
                    .next();

                if let Some((_, j)) = replacement {
                    use syn::{punctuated::Punctuated, token::Comma};

                    // Step 2d.1: Actually perform the replacement.
                    let replace_in = |ps: &mut Punctuated<GenericParam, Comma>| -> bool {
                        ps.iter_mut()
                            .filter(|param| param.kind() == generic.kind())
                            .nth(*j)
                            .map(|impl_param| *impl_param = param.clone())
                            .is_some()
                    };

                    // Step 2d.2: If it fails, insert a new impl generic.
                    // NOTE: It's critical that `generics` is attempted first!
                    // Otherwise, we might replace generics that don't exist in
                    // the user's type.
                    if !replace_in(&mut generics.params)
                         || !replace_in(&mut generics_for_impl_generics.params) {
                        generics_for_impl_generics.params.insert(0, param.clone());
                    }
                } else {
                    // Step 2d.2: Otherwise, insert a new impl generic.
                    generics_for_impl_generics.params.insert(0, param);
                }
            }
        }

        // Step 2e: Split the generics, but use the `impl_generics` from above.
        let (impl_gen, _, _) = generics_for_impl_generics.split_for_impl();
        let (_, ty_gen, where_gen) = generics.split_for_impl();

        // Step 2b: Generate the complete implementation.
        let target = &self.input.ident;
        let trait_name = &self.trait_path;
        Ok(quote! {
            impl #impl_gen #trait_name for #target #ty_gen #where_gen {
                #(#function_code)*
            }
        }.into())
    }

    pub fn debug(&mut self) -> &mut Self {
        match self._to_tokens() {
            Ok(tokens) => println!("Tokens produced: {}", tokens.to_string()),
            Err(e) => println!("Error produced: {:?}", e)
        }

        self
    }

    pub fn to_tokens(&mut self) -> TokenStream {
        // FIXME: Emit something like: Trait: msg.
        self._to_tokens()
            .unwrap_or_else(|diag| {
                if let Some(last) = self.trait_path.segments.last() {
                    use proc_macro::Span;
                    use proc_macro::Level::*;

                    let id = &last.value().ident;
                    let msg = match diag.level() {
                        Error => format!("error occurred while deriving `{}`", id),
                        Warning => format!("warning issued by `{}` derive", id),
                        Note => format!("note issued by `{}` derive", id),
                        Help => format!("help provided by `{}` derive", id),
                        _ => format!("while deriving `{}`", id)
                    };

                    diag.span_note(Span::call_site(), msg).emit();
                }

                TokenStream::new().into()
            })
    }
}
