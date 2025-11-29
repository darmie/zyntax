//! # ZRTL - Zyntax Runtime Library
//!
//! This crate provides types and utilities for creating ZRTL plugins that can be
//! loaded by the Zyntax compiler at runtime. It is the Rust equivalent of `zrtl.h`.
//!
//! ## Features
//!
//! - **Type System**: Type tags, categories, and flags for dynamic typing
//! - **DynamicBox**: Runtime boxed values with type information
//! - **GenericBox**: Support for generic types like `Array<T>`, `Map<K,V>`
//! - **String/Array**: Inline format helpers matching Zyntax's memory layout
//! - **Plugin Support**: Macros for defining ZRTL plugins
//! - **Async Support**: Promise-based async runtime for Zyntax async functions
//!
//! ## Quick Start
//!
//! ```rust,ignore
//! use zrtl::{zrtl_plugin, TypeTag, DynamicBox};
//!
//! // Define exported functions
//! extern "C" fn add(a: i32, b: i32) -> i32 {
//!     a + b
//! }
//!
//! extern "C" fn multiply(a: i32, b: i32) -> i32 {
//!     a * b
//! }
//!
//! // Create the plugin
//! zrtl_plugin! {
//!     name: "math_runtime",
//!     symbols: [
//!         ("$Math$add", add),
//!         ("$Math$multiply", multiply),
//!     ]
//! }
//! ```
//!
//! ## Native Async Functions (for Language Frontends)
//!
//! When building a custom language with Zyntax that supports async/await,
//! you can expose native async functions that guest code can await:
//!
//! ```rust,ignore
//! use zrtl::prelude::*;
//! use zrtl_macros::zrtl_async;
//!
//! // This function can be awaited from any Zyntax-based language
//! #[zrtl_async("$IO$read_file")]
//! pub async fn read_file(path_ptr: *const u8) -> i64 {
//!     // Real async I/O - can use tokio, async-std, etc.
//!     let contents = async_read(path_ptr).await;
//!     contents.len() as i64
//! }
//! ```
//!
//! In your custom language:
//! ```text
//! // MyLang source code
//! async fn main() {
//!     let size = await read_file("data.txt");  // Calls native async!
//!     print(size);
//! }
//! ```
//!
//! The async support includes:
//! - `ZrtlPromise`: C ABI promise type for native async functions
//! - `PollResult`: Poll result enum (Pending/Ready/Failed) with i64 ABI
//! - `StateMachineHeader`: Base type for manual state machines
//! - `PromiseAll`, `PromiseRace`, `PromiseAllSettled`: Promise combinators
//!
//! ## Memory Formats
//!
//! Zyntax uses specific inline memory formats:
//!
//! - **Strings**: `[i32 length][utf8_bytes...]`
//! - **Arrays**: `[i32 capacity][i32 length][elements...]`
//!
//! Use the `string` and `array` modules to work with these formats.
//!
//! ## Type Tags
//!
//! Type tags are 32-bit packed identifiers:
//! ```text
//! [flags:8][type_id:16][category:8]
//! ```
//!
//! Use the `zrtl_tag!` macro for compile-time tag creation:
//!
//! ```rust
//! use zrtl::zrtl_tag;
//!
//! let int_tag = zrtl_tag!(i32);
//! let custom_tag = zrtl_tag!(Struct, 42);
//! ```

pub mod type_system;
pub mod dynamic_box;
pub mod generic_box;
pub mod string;
pub mod array;
pub mod plugin;
pub mod async_support;

// Re-export main types at crate root
pub use type_system::{TypeCategory, TypeFlags, TypeTag, PrimitiveSize};
pub use dynamic_box::{DynamicBox, DropFn};
pub use generic_box::{GenericBox, GenericTypeArgs, MAX_TYPE_ARGS};
pub use string::{OwnedString, StringPtr, StringConstPtr, StringView};
pub use array::{OwnedArray, ArrayPtr, ArrayConstPtr, ArrayIterator};
pub use plugin::{ZrtlSymbol, ZrtlInfo, ZrtlSymbolEntry, ZrtlTyped, TypeInfo, ZRTL_VERSION};

// Re-export async types
pub use async_support::{
    ZrtlPromise, PollResult, PromiseError,
    AsyncState, StateMachineHeader,
    PromiseAll, PromiseRace, PromiseAllSettled, SettledResult,
    FutureAdapter, YieldOnce, Timer,
    noop_waker, noop_context, yield_once, sleep, next_task_id,
};

// Re-export string functions
pub use string::{
    string_new, string_empty, string_free, string_copy,
    string_length, string_data, string_equals, string_as_str, string_as_bytes,
    string_alloc_size, STRING_HEADER_SIZE,
};

// Re-export array functions
pub use array::{
    array_new, array_free, array_push, array_get, array_set,
    array_capacity, array_length, array_data, array_as_slice,
    array_alloc_size, ARRAY_HEADER_SIZE, ARRAY_HEADER_BYTES,
};

/// Prelude module for convenient imports
pub mod prelude {
    pub use crate::type_system::{TypeCategory, TypeFlags, TypeTag};
    pub use crate::dynamic_box::DynamicBox;
    pub use crate::generic_box::{GenericBox, GenericTypeArgs};
    pub use crate::string::OwnedString;
    pub use crate::array::OwnedArray;
    pub use crate::plugin::{ZrtlTyped, TypeInfo};
    pub use crate::async_support::{
        ZrtlPromise, PollResult, PromiseError,
        AsyncState, StateMachineHeader,
        PromiseAll, PromiseRace, PromiseAllSettled, SettledResult,
        yield_once, sleep,
    };
    pub use crate::zrtl_tag;
    pub use crate::zrtl_plugin;
    pub use crate::zrtl_symbol;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_type_tag_roundtrip() {
        let tag = TypeTag::new(TypeCategory::Struct, 123, TypeFlags::NULLABLE | TypeFlags::BOXED);

        assert_eq!(tag.category(), TypeCategory::Struct);
        assert_eq!(tag.type_id(), 123);
        assert!(tag.flags().is_nullable());
        assert!(tag.flags().is_boxed());
    }

    #[test]
    fn test_dynamic_box_primitives() {
        let b = DynamicBox::owned_i32(42);
        assert_eq!(b.as_i32(), Some(42));

        let b = DynamicBox::owned_f64(3.14);
        assert_eq!(b.as_f64(), Some(3.14));
    }

    #[test]
    fn test_owned_string() {
        let s = OwnedString::from("Hello, ZRTL!");
        assert_eq!(s.len(), 12);
        assert_eq!(s.as_str(), Some("Hello, ZRTL!"));
    }

    #[test]
    fn test_owned_array() {
        let mut arr: OwnedArray<i32> = OwnedArray::new().unwrap();
        arr.push(1);
        arr.push(2);
        arr.push(3);

        assert_eq!(arr.len(), 3);
        assert_eq!(arr.as_slice(), &[1, 2, 3]);
    }

    #[test]
    fn test_generic_box() {
        let args = GenericTypeArgs::array(TypeTag::I32);
        assert_eq!(args.get(0), Some(TypeTag::I32));
    }
}
