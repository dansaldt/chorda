#![allow(dead_code)]

use proc_macro::TokenStream;
use syn::parse_macro_input;

mod enum_macros;
use enum_macros::EnumRange;

#[proc_macro_derive(EnumRange)]
pub fn enum_range(item: TokenStream) -> TokenStream {
    let item = parse_macro_input!(item as EnumRange);
    item.expand()
}
