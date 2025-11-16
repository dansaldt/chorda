use proc_macro::TokenStream;
use proc_macro2::Literal;
use quote::{format_ident, quote};
use syn::{
    parse::Parse, punctuated::Punctuated, Attribute, Data, DeriveInput, Expr, ExprLit, Fields,
    FieldsUnnamed, Generics, Ident, Lit, Meta, MetaList, Token, Variant,
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
        let variants = parse_variants(input.data, &attrs.repr_int)?;
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

        let as_method = format_ident!("as_{}", repr_int);
        let range_fn = quote! {
            /// Return range of the enum variants as #range_ty
            ///
            /// Automatically derived from `EnumRange`
            #[inline]
            pub const fn range() -> std::ops::RangeInclusive< #repr_int > {
                let __first = Self::#first.#as_method();
                let __last = Self::#last.#as_method();
                __first..=__last
            }
        };

        let mut variants_as_repr = Vec::new();
        let mut variants_from_repr = Vec::new();
        let mut discriminants = Vec::new();
        self.variants.all.iter().for_each(|(variant, expr)| {
            let ident = &variant.ident;
            match &variant.fields {
                Fields::Named(fields) => {
                    variants_as_repr.push(quote! { #ident { .. } });

                    let fields_ident: Vec<_> =
                        fields.named.iter().map(|f| f.ident.as_ref()).collect();
                    variants_from_repr
                        .push(quote! { #ident { #( #fields_ident: Default::default(), )* } });
                }
                Fields::Unnamed(fields @ FieldsUnnamed { .. }) => {
                    variants_as_repr.push(quote! { #ident( .. ) });

                    let default: Vec<_> = fields
                        .unnamed
                        .iter()
                        .map(|_| quote! { Default::default() })
                        .collect();
                    variants_from_repr.push(quote! { #ident ( #( #default , )* ) });
                }
                Fields::Unit => {
                    variants_as_repr.push(quote! { #ident });
                    variants_from_repr.push(quote! { #ident });
                }
            };
            discriminants.push(expr);
        });

        let as_repr_int_fn_name = {
            let name_str = format!("as_{}", repr_int.to_string());
            Ident::new(&name_str, repr_int.span())
        };
        let as_repr_int_fn = quote! {
            /// Return self as its integer representation defined by `#[repr(..)]`
            ///
            /// Automatically derived from `EnumRange`
            #[inline]
            pub const fn #as_repr_int_fn_name(&self) -> #repr_int {
                match self {
                    #( Self::#variants_as_repr => #discriminants, )*
                }
            }
        };

        let const_kw = if self.variants.only_unit_variants {
            quote! { const }
        } else {
            quote! {}
        };

        let from_repr_int_fn_name = {
            let name_str = format!("from_{}", repr_int.to_string());
            Ident::new(&name_str, repr_int.span())
        };
        let from_repr_int_fn = quote! {
            /// Make self from its integer representation defined by `#[repr(..)]`.
            ///
            /// Automatically derived from `EnumRange`
            #[inline]
            pub #const_kw fn #from_repr_int_fn_name(value: #repr_int) -> Option<Self> {
                match value {
                    #( #discriminants => Some(Self::#variants_from_repr), )*
                    _ => None
                }
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
                #from_repr_int_fn
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
    all: Vec<(Variant, Expr)>,
    only_unit_variants: bool,
    ty: Option<()>,
}

fn parse_variants(data: Data, repr_int: &Ident) -> syn::Result<Variants> {
    let Data::Enum(data) = data else {
        panic!("EnumRange must be used only on enum");
    };

    let variants_len = data.variants.len();
    let mut first = None;
    let mut last = None;
    let mut all = vec![];
    let mut last_discriminant = None;
    let mut only_unit_variants = true;

    for (idx, variant) in data.variants.into_iter().enumerate() {
        if idx == 0 {
            first = Some(variant.clone());
        }
        if idx == variants_len - 1 {
            last = Some(variant.clone());
        }

        match &variant.fields {
            Fields::Named(_) | Fields::Unnamed(_) => only_unit_variants = false,
            Fields::Unit => (),
        };

        let discriminant = if let Some((_, expr)) = &variant.discriminant {
            if let Expr::Lit(ExprLit {
                lit: Lit::Int(val), ..
            }) = expr
            {
                last_discriminant = Some(val.base10_parse::<i64>()?);
                expr.clone()
            } else {
                unreachable!("expected discriminant is of integer type")
            }
        } else {
            last_discriminant = last_discriminant.map_or(Some(0), |x| Some(x + 1));
            new_syn_int_literal(repr_int, last_discriminant.unwrap())
        };

        all.push((variant, discriminant));
    }

    Ok(Variants {
        first: first.unwrap_or_else(|| unreachable!("first variant must exist")),
        last: last.unwrap_or_else(|| unreachable!("last variant must exist")),
        all,
        only_unit_variants,
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

fn new_syn_int_literal(int_ident: &Ident, val: i64) -> Expr {
    assert!(ident_is_integer_type(int_ident));
    let token = match int_ident.to_string().as_str() {
        "u8" => Literal::u8_unsuffixed(val as u8),
        "u16" => Literal::u16_unsuffixed(val as u16),
        "u32" => Literal::u32_unsuffixed(val as u32),
        "u64" => Literal::u64_unsuffixed(val as u64),
        "u128" => Literal::u128_unsuffixed(val as u128),
        "i8" => Literal::i8_unsuffixed(val as i8),
        "i16" => Literal::i16_unsuffixed(val as i16),
        "i32" => Literal::i32_unsuffixed(val as i32),
        "i64" => Literal::i64_unsuffixed(val as i64),
        "i128" => Literal::i128_unsuffixed(val as i128),
        "usize" => Literal::usize_unsuffixed(val as usize),
        "isize" => Literal::isize_unsuffixed(val as isize),
        _ => unreachable!("not an integer type"),
    };
    Expr::Lit(ExprLit {
        attrs: vec![],
        lit: Lit::new(token),
    })
}
