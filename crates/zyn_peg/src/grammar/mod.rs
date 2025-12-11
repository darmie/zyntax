//! Grammar IR and Parser for ZynPEG 2.0
//!
//! This module provides:
//! - `ir.rs`: Intermediate representation for grammar rules with named bindings
//! - `parser.rs`: Parser for the new .zyn grammar syntax
//!
//! The new grammar syntax uses named bindings instead of positional `$N` captures:
//!
//! ```zyn
//! fn_def = { "fn" ~ name:identifier ~ "(" ~ params:fn_params? ~ ")" ~ ret:type_annotation? ~ body:block }
//!   -> TypedDeclaration::Function {
//!       name: name.text,
//!       params: params.unwrap_or_default(),
//!       return_type: ret,
//!       body: body,
//!   }
//! ```

pub mod ir;
pub mod parser;

pub use ir::*;
pub use parser::*;
