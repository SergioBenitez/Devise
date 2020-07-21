use syn::{self, spanned::Spanned, visit_mut::VisitMut};
use proc_macro2::TokenStream;
use proc_macro2_diagnostics::{SpanDiagnosticExt, Diagnostic};

use ext::{GenericExt, GenericParamExt, IdentReplacer};

use field::{Field, Fields};
use support::{GenericSupport, DataSupport};
use derived::{Derived, Variant, Struct, Enum};

pub type Result<T> = ::std::result::Result<T, Diagnostic>;
pub type MapResult = Result<TokenStream>;

macro_rules! validator {
    ($fn_name:ident: $validate_fn_type:ty, $field:ident) => {
        pub fn $fn_name<F: 'static>(&mut self, f: F) -> &mut Self
            where F: Fn(&DeriveGenerator, $validate_fn_type) -> Result<()>
        {
            self.$field = Box::new(f);
            self
        }
    }
}

macro_rules! mappers {
    ($(($map_f:ident, $try_f:ident, $get_f:ident, $default_f:ident): $type:ty, $vec:ident),*) => (
        fn push_default_mappers(&mut self) {
            $(self.$vec.push(Box::new($default_f));)*
        }

        $(
            pub fn $map_f<F: 'static>(&mut self, f: F) -> &mut Self
                where F: Fn(&DeriveGenerator, $type) -> TokenStream
            {
                if !self.$vec.is_empty() {
                    let last = self.$vec.len() - 1;
                    self.$vec[last] = Box::new(move |g, v| Ok(f(g, v)));
                }

                self
            }

            pub fn $try_f<F: 'static>(&mut self, f: F) -> &mut Self
                where F: Fn(&DeriveGenerator, $type) -> MapResult
            {
                if !self.$vec.is_empty() {
                    let last = self.$vec.len() - 1;
                    self.$vec[last] = Box::new(f);
                }

                self
            }

            pub fn $get_f(&self) -> &Box<dyn Fn(&DeriveGenerator, $type) -> MapResult> {
                assert!(!self.$vec.is_empty());
                let last = self.$vec.len() - 1;
                &self.$vec[last]
            }
        )*
    )
}

// FIXME: Take a `Box<Fn>` everywhere so we can capture args!
pub struct DeriveGenerator {
    pub input: syn::DeriveInput,
    pub trait_impl: syn::ItemImpl,
    pub trait_path: syn::Path,
    generic_support: GenericSupport,
    data_support: DataSupport,
    enum_validator: Box<dyn Fn(&DeriveGenerator, Enum) -> Result<()>>,
    struct_validator: Box<dyn Fn(&DeriveGenerator, Struct) -> Result<()>>,
    generics_validator: Box<dyn Fn(&DeriveGenerator, &::syn::Generics) -> Result<()>>,
    fields_validator: Box<dyn Fn(&DeriveGenerator, Fields) -> Result<()>>,
    type_generic_mapper: Option<Box<dyn Fn(&DeriveGenerator, &syn::Ident, &syn::TypeParam) -> TokenStream>>,
    generic_replacements: Vec<(usize, usize)>,
    functions: Vec<Box<dyn Fn(&DeriveGenerator, TokenStream) -> TokenStream>>,
    enum_mappers: Vec<Box<dyn Fn(&DeriveGenerator, Enum) -> MapResult>>,
    struct_mappers: Vec<Box<dyn Fn(&DeriveGenerator, Struct) -> MapResult>>,
    variant_mappers: Vec<Box<dyn Fn(&DeriveGenerator, Variant) -> MapResult>>,
    fields_mappers: Vec<Box<dyn Fn(&DeriveGenerator, Fields) -> MapResult>>,
    field_mappers: Vec<Box<dyn Fn(&DeriveGenerator, Field) -> MapResult>>,
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
    Ok(TokenStream::new())
}

pub fn default_fields_mapper(g: &DeriveGenerator, fields: Fields) -> MapResult {
    let field = fields.iter()
        .map(|field| g.field_mapper()(g, field))
        .collect::<Result<Vec<_>>>()?;

    Ok(quote!({ #(#field)* }))
}

impl DeriveGenerator {
    pub fn build_for2(input: TokenStream, trait_impl: TokenStream) -> DeriveGenerator {
        let trait_impl: syn::ItemImpl = syn::parse2(quote!(#trait_impl for Foo {}))
            .expect("invalid impl");
        let trait_path = trait_impl.trait_.clone().expect("impl does not have trait").1;
        let input = syn::parse2(input).expect("invalid derive input");

        DeriveGenerator {
            input, trait_impl, trait_path,
            generic_support: GenericSupport::None,
            data_support: DataSupport::None,
            type_generic_mapper: None,
            generic_replacements: vec![],
            enum_validator: Box::new(|_, _| Ok(())),
            struct_validator: Box::new(|_, _| Ok(())),
            generics_validator: Box::new(|_, _| Ok(())),
            fields_validator: Box::new(|_, _| Ok(())),
            functions: vec![],
            enum_mappers: vec![],
            struct_mappers: vec![],
            variant_mappers: vec![],
            field_mappers: vec![],
            fields_mappers: vec![],
        }
    }

    #[inline]
    pub fn build_for(
        input: proc_macro::TokenStream,
        trait_impl: TokenStream
    ) -> DeriveGenerator {
        Self::build_for2(input.into(), trait_impl)
    }

    pub fn generic_support(&mut self, support: GenericSupport) -> &mut Self {
        self.generic_support = support;
        self
    }

    pub fn data_support(&mut self, support: DataSupport) -> &mut Self {
        self.data_support = support;
        self
    }

    pub fn map_type_generic<F: 'static>(&mut self, f: F) -> &mut Self
        where F: Fn(&DeriveGenerator, &syn::Ident, &syn::TypeParam) -> TokenStream
    {
        self.type_generic_mapper = Some(Box::new(f));
        self
    }

    pub fn replace_generic(
        &mut self,
        use_trait_gen: usize,
        in_place_of_type_gen: usize
    ) -> &mut Self {
        self.generic_replacements.push((use_trait_gen, in_place_of_type_gen));
        self
    }

    validator!(validate_enum: Enum, enum_validator);
    validator!(validate_struct: Struct, struct_validator);
    validator!(validate_generics: &syn::Generics, generics_validator);
    validator!(validate_fields: Fields, fields_validator);

    pub fn function<F: 'static>(&mut self, f: F) -> &mut Self
        where F: Fn(&DeriveGenerator, TokenStream) -> TokenStream
    {
        self.functions.push(Box::new(f));
        self.push_default_mappers();
        self
    }

    mappers! {
        (map_struct, try_map_struct, struct_mapper, default_struct_mapper):
            Struct, struct_mappers,

        (map_enum, try_map_enum, enum_mapper, default_enum_mapper):
            Enum, enum_mappers,

        (map_variant, try_map_variant, variant_mapper, default_variant_mapper):
            Variant, variant_mappers,

        (map_fields, try_map_fields, fields_mapper, default_fields_mapper):
            Fields, fields_mappers,

        (map_field, try_map_field, field_mapper, default_field_mapper):
            Field, field_mappers
    }

    fn _to_tokens(&self) -> Result<TokenStream> {
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
            let function = &self.functions[i];
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

        // Step 2b: Copy user's generics to mutate with bounds + replacements.
        let mut type_generics = self.input.generics.clone();

        // Step 2c: Add an additional where bounds for each type parameter using
        // the: `type_mapper(T) -> quote!(T: Foo)` adds `where T: Foo`.
        if let Some(ref type_mapper) = self.type_generic_mapper {
            for ty in self.input.generics.type_params() {
                let new_ty = type_mapper(self, &ty.ident, ty);
                let clause = syn::parse2(new_ty).expect("invalid type generic mapping");
                type_generics.make_where_clause().predicates.push(clause);
            }
        }

        // Step 2d: Perform generic replacememnt: replace generics in the input
        // type with generics from the trait definition: 1) determine the
        // identifer of the generic to be replaced in the type. 2) replace every
        // identifer in the type with the same name with the identifer of the
        // replacement trait generic. For example:
        //   * replace: trait_i = 1, type_i = 0
        //   * trait: impl<'_a, '_b: '_a> GenExample<'_a, '_b>
        //   * type: GenFooAB<'x, 'y: 'x>
        //   * new type: GenFooAB<'_b, 'y: 'b>
        for (trait_i, type_i) in &self.generic_replacements {
            let idents = self.trait_impl.generics.params.iter()
                .nth(*trait_i)
                .and_then(|trait_gen| type_generics.params.iter()
                    .filter(|gen| gen.kind() == trait_gen.kind())
                    .nth(*type_i)
                    .map(|type_gen| (trait_gen.ident(), type_gen.ident().clone())));

            if let Some((with, ref to_replace)) = idents {
                let mut replacer = IdentReplacer { to_replace, with, replaced: false };
                replacer.visit_generics_mut(&mut type_generics);
            }
        }

        // Step 2e: Determine which generics from the type need to be added to
        // the trait's `impl<>` generics. These are all of the generics in the
        // type that aren't in the trait's `impl<>` already.
        let mut type_generics_for_impl = self.trait_impl.generics.clone();
        for type_gen in &type_generics.params {
            let type_gen_in_trait_gens = type_generics_for_impl.params.iter()
                .map(|gen| gen.ident())
                .find(|g| g == &type_gen.ident())
                .is_some();

            if !type_gen_in_trait_gens {
                type_generics_for_impl.params.push(type_gen.clone())
            }
        }

        // Step 2f: Split the generics, but use the `impl_generics` from above.
        let (impl_gen, _, _) = type_generics_for_impl.split_for_impl();
        let (_, ty_gen, where_gen) = type_generics.split_for_impl();

        // Step 2g: Generate the complete implementation.
        let target = &self.input.ident;
        let trait_name = &self.trait_path;
        Ok(quote! {
            impl #impl_gen #trait_name for #target #ty_gen #where_gen {
                #(#function_code)*
            }
        })
    }

    pub fn debug(&mut self) -> &mut Self {
        match self._to_tokens() {
            Ok(tokens) => println!("Tokens produced: {}", tokens.to_string()),
            Err(e) => println!("Error produced: {:?}", e)
        }

        self
    }

    pub fn to_tokens(&mut self) -> proc_macro::TokenStream {
        self.to_tokens2().into()
    }

    pub fn try_to_tokens(&mut self) -> Result<proc_macro::TokenStream> {
        self.try_to_tokens2().map(|t| t.into())
    }

    pub fn to_tokens2(&mut self) -> TokenStream {
        self.try_to_tokens2().unwrap_or_else(|diag| diag.emit_as_tokens())
    }

    pub fn try_to_tokens2(&mut self) -> Result<TokenStream> {
        // FIXME: Emit something like: Trait: msg.
        self._to_tokens()
            .map_err(|diag| {
                if let Some(last) = self.trait_path.segments.last() {
                    use proc_macro2::Span;
                    use proc_macro2_diagnostics::Level::*;

                    let id = &last.ident;
                    let msg = match diag.level() {
                        Error => format!("error occurred while deriving `{}`", id),
                        Warning => format!("warning issued by `{}` derive", id),
                        Note => format!("note issued by `{}` derive", id),
                        Help => format!("help provided by `{}` derive", id),
                        _ => format!("while deriving `{}`", id)
                    };

                    diag.span_note(Span::call_site(), msg)
                } else {
                    diag
                }
            })
    }
}
