//! ZRTL Macros for Rust Plugin Development
//!
//! This crate provides procedural macros for creating ZRTL plugins in Rust.
//!
//! # Example
//!
//! ```rust
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
//! ```

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, LitStr, ItemFn, parse::Parse, parse::ParseStream};

/// Defines the ZRTL plugin metadata
///
/// # Example
/// ```rust
/// zrtl_plugin!("my_runtime");
/// ```
#[proc_macro]
pub fn zrtl_plugin(input: TokenStream) -> TokenStream {
    let name = parse_macro_input!(input as LitStr);
    let name_value = name.value();

    let expanded = quote! {
        // Plugin info export
        #[no_mangle]
        pub static _zrtl_info: ::zrtl_macros::ZrtlInfo = ::zrtl_macros::ZrtlInfo {
            version: ::zrtl_macros::ZRTL_VERSION,
            name: concat!(#name_value, "\0").as_ptr() as *const ::std::ffi::c_char,
        };

        // Symbol table will be populated by #[zrtl_export] attributes
        // using the inventory crate for collection
        ::inventory::collect!(::zrtl_macros::ZrtlSymbolEntry);

        #[no_mangle]
        pub static mut _zrtl_symbols: [::zrtl_macros::ZrtlSymbol; 256] = [::zrtl_macros::ZrtlSymbol {
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
                    for entry in ::inventory::iter::<::zrtl_macros::ZrtlSymbolEntry> {
                        if i < 255 {
                            _zrtl_symbols[i] = ::zrtl_macros::ZrtlSymbol {
                                name: entry.name.as_ptr() as *const ::std::ffi::c_char,
                                ptr: entry.ptr,
                            };
                            i += 1;
                        }
                    }
                    // Sentinel
                    _zrtl_symbols[i] = ::zrtl_macros::ZrtlSymbol {
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
/// # Example
/// ```rust
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
            ::zrtl_macros::ZrtlSymbolEntry {
                name: #symbol_name_with_null,
                ptr: #func_name as *const u8,
            }
        }
    };

    TokenStream::from(expanded)
}
