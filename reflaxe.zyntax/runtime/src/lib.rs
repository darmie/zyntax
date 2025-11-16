//! Haxe Runtime Library for Zyntax
//!
//! This library provides comprehensive Haxe-specific runtime implementations for the Zyntax target.
//! All runtime functions are compiled to native code and linked into the JIT runtime via the plugin system.
//!
//! # Architecture
//!
//! The runtime is organized into modules:
//! - `array`: Dynamic arrays with push/pop/shift/unshift operations
//! - `string`: Immutable UTF-8 strings with manipulation functions
//!
//! # Plugin System
//!
//! This runtime uses Zyntax's plugin architecture for automatic symbol registration.
//! The `runtime_plugin!` macro generates boilerplate, and `#[runtime_export]` registers functions.
//!
//! # Memory Management
//!
//! All heap-allocated structures use libc malloc/free. The frontend is responsible for
//! calling appropriate `free` functions when objects are no longer needed.

extern crate libc;

use zyntax_plugin_macros::runtime_plugin;

// Declare this as a Zyntax runtime plugin
runtime_plugin! {
    name: "haxe",
}

// Export runtime modules
pub mod array;
pub mod string;
