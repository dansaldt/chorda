use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse::Parse, punctuated::Punctuated, Attribute, Data, DeriveInput, Fields, Generics, Ident,
    Meta, MetaList, Token, Variant,
};

#[derive(Debug)]
pub struct EnumRange {
    name: Ident,
    generics: Generics,
    attrs: Attributes,
    variants: Variants,
}

impl Parse for EnumRange {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let input = input.parse::<DeriveInput>()?;
        let name = input.ident.clone();
        let generics = input.generics.clone();
        let attrs = parse_attributes(input.attrs)?;
        let variants = parse_variants(input.data)?;
        Ok(Self {
            name,
            generics,
            attrs,
            variants,
        })
    }
}

impl EnumRange {
    pub fn expand(self) -> TokenStream {
        self.expand_item().into()
    }

    pub fn expand_item(self) -> proc_macro2::TokenStream {
        let repr_int = self.attrs.repr_int;

        let first = self.variants.first.ident;
        let last = self.variants.last.ident;

        let first_fn = quote! {
            /// Return the first variant in the enum.
            ///
            /// Automatically derived from `EnumRange`.
            #[inline]
            pub const fn first() -> Self {
                Self::#first
            }
        };
        let last_fn = quote! {
            /// Return the last variant in the enum.
            ///
            /// Automatically derived from `EnumRange`.
            #[inline]
            pub const fn last() -> Self {
                Self::#last
            }
        };

        let range_fn = quote! {
            /// Return range of the enum variants as #range_ty
            ///
            /// Automatically derived from `EnumRange`
            #[inline]
            pub const fn range() -> std::ops::RangeInclusive< #repr_int > {
                let __first = (Self::#first) as #repr_int;
                let __last = (Self::#last) as #repr_int;
                __first..=__last
            }
        };

        let as_repr_int_fn_name = {
            let name_str = format!("as_{}", repr_int.to_string());
            Ident::new(&name_str, repr_int.span())
        };
        let as_repr_int_fn = quote! {
            /// Return self as u8
            ///
            /// Automatically derived from `EnumRange`
            #[inline]
            pub const fn #as_repr_int_fn_name(&self) -> #repr_int {
                *self as #repr_int
            }
        };

        let name = self.name;
        let generics = self.generics;

        quote! {
            #[automatically_derived]
            impl #name #generics {
                #first_fn
                #last_fn
                #range_fn
                #as_repr_int_fn
            }
        }
    }
}

#[derive(Debug)]
pub struct Attributes {
    repr_int: Ident,
}

fn parse_attributes(attrs: Vec<Attribute>) -> syn::Result<Attributes> {
    let repr_meta = attrs
        .iter()
        .find_map(|attr| {
            if let Meta::List(meta_list @ MetaList { path, .. }) = &attr.meta {
                if path.is_ident("repr") {
                    return Some(meta_list);
                }
            }
            None
        })
        .unwrap_or_else(|| {
            // Currently [`EnumRange`] requires the enum to impl `#[repr({integer-type})]`
            // simply because the fn `range` return type, `RangeInclusive<Idx>`, has generics
            // `Idx` that bound to unstable trait `Step`.
            // Because of it we cannot return it as `RangeInclusive<Self>` which is quite unfortunate
            // (see [#42168](https://github.com/rust-lang/rust/issues/42168) for more info on *step_trait*).
            //
            // Thus now we requires it to have integer `repr` and the fn `range` can return the range
            // based on it.
            panic!("EnumRange expects the enum impl `#[repr({{integer-type}})]`")
        });

    let mut repr_int = None;

    repr_meta
        .parse_args_with(Punctuated::<Meta, Token![,]>::parse_terminated)?
        .into_iter()
        .for_each(|meta| {
            if let Some(ident) = meta.path().get_ident() {
                if ident_is_integer_type(ident) {
                    repr_int = Some(ident.clone());
                }
            }
        });

    let repr_int = repr_int
        .unwrap_or_else(|| panic!("EnumRange expects the enum impl `#[repr({{integer-type}})]`"));
    Ok(Attributes { repr_int })
}

#[derive(Debug)]
pub struct Variants {
    first: Variant,
    last: Variant,
    all: Vec<Variant>,
    ty: Option<()>,
}

fn parse_variants(data: Data) -> syn::Result<Variants> {
    let Data::Enum(data) = data else {
        panic!("EnumRange must be used only on enum");
    };

    let variants_len = data.variants.len();
    let mut first = None;
    let mut last = None;
    let mut vec = vec![];

    for (idx, variant) in data.variants.into_iter().enumerate() {
        if idx == 0 {
            first = Some(variant.clone());
        } else if idx == variants_len - 1 {
            last = Some(variant.clone());
        }

        if !matches!(variant.fields, Fields::Unit) {
            panic!("EnumRange can only be applied on enum with only unit variants");
        };

        vec.push(variant);
    }

    Ok(Variants {
        first: first.unwrap_or_else(|| unreachable!("first variant must exist")),
        last: last.unwrap_or_else(|| unreachable!("last variant must exist")),
        all: vec,
        ty: None,
    })
}

fn ident_is_integer_type(ident: &Ident) -> bool {
    matches!(
        ident.to_string().as_str(),
        "u8" | "u16"
            | "u32"
            | "u64"
            | "u128"
            | "i8"
            | "i16"
            | "i32"
            | "i64"
            | "i128"
            | "usize"
            | "isize"
    )
}
