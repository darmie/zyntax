//! ZRTL Macros for Rust Plugin Development
//!
//! This crate provides procedural macros for creating ZRTL plugins in Rust.
//! Use this alongside the `zrtl` crate which provides the runtime types.
//!
//! # Example
//!
//! ```rust,ignore
//! use zrtl::prelude::*;
//! use zrtl_macros::{zrtl_plugin, zrtl_export};
//!
//! // Define the plugin
//! zrtl_plugin!("my_runtime");
//!
//! // Export functions
//! #[zrtl_export("$MyRuntime$add")]
//! pub extern "C" fn add(a: i32, b: i32) -> i32 {
//!     a + b
//! }
//!
//! #[zrtl_export("$MyRuntime$multiply")]
//! pub extern "C" fn multiply(a: i32, b: i32) -> i32 {
//!     a * b
//! }
//! ```
//!
//! Build as cdylib:
//! ```toml
//! [lib]
//! crate-type = ["cdylib"]
//!
//! [dependencies]
//! zrtl = "0.1"
//! zrtl_macros = "0.1"
//! ```

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, LitStr, ItemFn, DeriveInput, parse::Parse, parse::ParseStream};

/// Defines the ZRTL plugin metadata and symbol table infrastructure
///
/// This macro creates:
/// - `_zrtl_info`: Plugin metadata export
/// - `_zrtl_symbols`: Symbol table export (populated by `#[zrtl_export]`)
/// - Constructor to populate symbols at load time
///
/// # Example
/// ```rust,ignore
/// zrtl_plugin!("my_runtime");
/// ```
#[proc_macro]
pub fn zrtl_plugin(input: TokenStream) -> TokenStream {
    let name = parse_macro_input!(input as LitStr);
    let name_value = name.value();

    let expanded = quote! {
        // Plugin info export
        #[no_mangle]
        pub static _zrtl_info: ::zrtl::ZrtlInfo = ::zrtl::ZrtlInfo {
            version: ::zrtl::ZRTL_VERSION,
            name: concat!(#name_value, "\0").as_ptr() as *const ::std::ffi::c_char,
        };

        // Symbol table will be populated by #[zrtl_export] attributes
        // using the inventory crate for collection
        ::inventory::collect!(::zrtl::ZrtlSymbolEntry);

        #[no_mangle]
        pub static mut _zrtl_symbols: [::zrtl::ZrtlSymbol; 256] = [::zrtl::ZrtlSymbol {
            name: ::std::ptr::null(),
            ptr: ::std::ptr::null(),
        }; 256];

        // Constructor to populate the symbol table
        #[used]
        #[cfg_attr(target_os = "linux", link_section = ".init_array")]
        #[cfg_attr(target_os = "macos", link_section = "__DATA,__mod_init_func")]
        #[cfg_attr(target_os = "windows", link_section = ".CRT$XCU")]
        static INIT_SYMBOLS: extern "C" fn() = {
            extern "C" fn init() {
                unsafe {
                    let mut i = 0;
                    for entry in ::inventory::iter::<::zrtl::ZrtlSymbolEntry> {
                        if i < 255 {
                            _zrtl_symbols[i] = ::zrtl::ZrtlSymbol {
                                name: entry.name.as_ptr() as *const ::std::ffi::c_char,
                                ptr: entry.ptr,
                            };
                            i += 1;
                        }
                    }
                    // Sentinel
                    _zrtl_symbols[i] = ::zrtl::ZrtlSymbol {
                        name: ::std::ptr::null(),
                        ptr: ::std::ptr::null(),
                    };
                }
            }
            init
        };
    };

    TokenStream::from(expanded)
}

struct ZrtlExportArgs {
    symbol_name: LitStr,
}

impl Parse for ZrtlExportArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let symbol_name = input.parse()?;
        Ok(ZrtlExportArgs { symbol_name })
    }
}

/// Exports a function as a ZRTL symbol
///
/// The function must be `extern "C"` for proper ABI compatibility.
///
/// # Example
/// ```rust,ignore
/// #[zrtl_export("$Array$push")]
/// pub extern "C" fn array_push(arr: *mut ZrtlArray, value: i32) {
///     // ...
/// }
/// ```
#[proc_macro_attribute]
pub fn zrtl_export(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr as ZrtlExportArgs);
    let func = parse_macro_input!(item as ItemFn);

    let symbol_name = args.symbol_name.value();
    let func_name = &func.sig.ident;

    // Ensure the function is extern "C"
    let has_extern_c = func.sig.abi.as_ref()
        .map(|abi| abi.name.as_ref().map(|n| n.value() == "C").unwrap_or(false))
        .unwrap_or(false);

    if !has_extern_c {
        return syn::Error::new_spanned(
            &func.sig,
            "ZRTL exported functions must be extern \"C\""
        ).to_compile_error().into();
    }

    let symbol_name_with_null = format!("{}\0", symbol_name);

    let expanded = quote! {
        #[no_mangle]
        #func

        // Register the symbol using inventory
        ::inventory::submit! {
            ::zrtl::ZrtlSymbolEntry {
                name: #symbol_name_with_null,
                ptr: #func_name as *const u8,
            }
        }
    };

    TokenStream::from(expanded)
}

/// Derive macro for implementing ZrtlTyped trait
///
/// This allows custom structs to be used with the ZRTL type system.
///
/// # Attributes
///
/// - `#[zrtl(name = "TypeName")]` - Override the type name (default: struct name)
/// - `#[zrtl(category = "Struct")]` - Set the type category (default: Struct)
///
/// # Example
/// ```rust,ignore
/// #[derive(ZrtlType)]
/// #[zrtl(name = "Point2D")]
/// pub struct Point {
///     pub x: f64,
///     pub y: f64,
/// }
/// ```
#[proc_macro_derive(ZrtlType, attributes(zrtl))]
pub fn derive_zrtl_type(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = &input.ident;

    // Parse attributes
    let mut type_name = name.to_string();
    let mut category = quote! { ::zrtl::TypeCategory::Struct };

    for attr in &input.attrs {
        if attr.path().is_ident("zrtl") {
            let _ = attr.parse_nested_meta(|meta| {
                if meta.path.is_ident("name") {
                    let value: LitStr = meta.value()?.parse()?;
                    type_name = value.value();
                } else if meta.path.is_ident("category") {
                    let value: LitStr = meta.value()?.parse()?;
                    let cat_str = value.value();
                    category = match cat_str.as_str() {
                        "Void" => quote! { ::zrtl::TypeCategory::Void },
                        "Bool" => quote! { ::zrtl::TypeCategory::Bool },
                        "Int" => quote! { ::zrtl::TypeCategory::Int },
                        "UInt" => quote! { ::zrtl::TypeCategory::UInt },
                        "Float" => quote! { ::zrtl::TypeCategory::Float },
                        "String" => quote! { ::zrtl::TypeCategory::String },
                        "Array" => quote! { ::zrtl::TypeCategory::Array },
                        "Map" => quote! { ::zrtl::TypeCategory::Map },
                        "Struct" => quote! { ::zrtl::TypeCategory::Struct },
                        "Class" => quote! { ::zrtl::TypeCategory::Class },
                        "Enum" => quote! { ::zrtl::TypeCategory::Enum },
                        "Union" => quote! { ::zrtl::TypeCategory::Union },
                        "Function" => quote! { ::zrtl::TypeCategory::Function },
                        "Pointer" => quote! { ::zrtl::TypeCategory::Pointer },
                        "Optional" => quote! { ::zrtl::TypeCategory::Optional },
                        "Result" => quote! { ::zrtl::TypeCategory::Result },
                        "Tuple" => quote! { ::zrtl::TypeCategory::Tuple },
                        "TraitObject" => quote! { ::zrtl::TypeCategory::TraitObject },
                        "Opaque" => quote! { ::zrtl::TypeCategory::Opaque },
                        _ => quote! { ::zrtl::TypeCategory::Custom },
                    };
                }
                Ok(())
            });
        }
    }

    let expanded = quote! {
        impl ::zrtl::ZrtlTyped for #name {
            fn type_name() -> &'static str {
                #type_name
            }

            fn type_category() -> ::zrtl::TypeCategory {
                #category
            }
        }
    };

    TokenStream::from(expanded)
}
