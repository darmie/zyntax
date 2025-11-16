//! # Whirlwind to TypedAST Adapter
//!
//! This crate provides translation from Whirlwind's Standpoint IR to Zyntax's TypedAST.
//!
//! ## Architecture
//!
//! ```text
//! Whirlwind Source
//!       ↓
//! Whirlwind Parser/Analyzer
//!       ↓
//! Standpoint (Whirlwind IR)
//!       ↓
//! [THIS ADAPTER] ← You are here
//!       ↓
//! TypedAST (Zyntax IR)
//!       ↓
//! HIR (Zyntax HIR)
//!       ↓
//! LLVM/Cranelift Backend
//! ```
//!
//! ## Usage
//!
//! ```rust,ignore
//! use whirlwind_adapter::WhirlwindAdapter;
//!
//! let adapter = WhirlwindAdapter::new();
//! let typed_program = adapter.convert_standpoint(standpoint)?;
//! ```

mod error;
mod type_converter;
mod expression_converter;
mod statement_converter;
mod typed_expression_converter;
mod symbol_extractor;
mod adapter;

pub use error::{AdapterError, AdapterResult};
pub use adapter::WhirlwindAdapter;

// Re-export key types for convenience
pub use zyntax_typed_ast::{TypedProgram, Type, TypedExpression, TypedStatement};
