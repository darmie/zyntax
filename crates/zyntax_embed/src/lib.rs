//! # Zyntax Embed - Rust SDK for Embedding Zyntax JIT
//!
//! This crate provides ergonomic Rust APIs for embedding Zyntax as a JIT runtime,
//! enabling bidirectional conversion between Zyntax runtime values and native Rust types.
//!
//! ## Key Features
//!
//! - **Compiler Integration**: Compile and execute Zyntax code directly from Rust
//! - **Type-safe conversions**: `FromZyntax` and `IntoZyntax` traits for seamless value conversion
//! - **Async Support**: `ZyntaxPromise` for handling async operations with `.then()` and `.catch()`
//! - **Runtime value handling**: `ZyntaxValue` enum for working with dynamically-typed Zyntax values
//! - **String/Array interop**: Zero-copy wrappers for Zyntax's native formats
//! - **Hot Reloading**: Update functions at runtime without restarting
//!
//! ## Quick Start
//!
//! ```rust,ignore
//! use zyntax_embed::{ZyntaxRuntime, ZyntaxValue, FromZyntax};
//!
//! // Create a runtime and compile code
//! let mut runtime = ZyntaxRuntime::new()?;
//! runtime.compile_module(&hir_module)?;
//!
//! // Call functions with automatic type conversion
//! let result: i32 = runtime.call("add", &[10.into(), 20.into()])?;
//! assert_eq!(result, 30);
//!
//! // Async functions return Promises
//! let promise = runtime.call_async("fetch_data", &[url.into()])?;
//! let data: String = promise.await_result()?;
//! ```
//!
//! ## Memory Management
//!
//! Zyntax uses a specific memory format for its runtime values:
//! - **Strings**: Length-prefixed format `[i32 length][utf8_bytes...]`
//! - **Arrays**: Header format `[i32 capacity][i32 length][elements...]`
//!
//! This crate handles all memory conversion automatically, ensuring proper allocation
//! and deallocation when values cross the Rust/Zyntax boundary.
//!
//! ## Language Grammar Support
//!
//! Use `LanguageGrammar` to parse source code using ZynPEG grammars:
//!
//! ```rust,ignore
//! use zyntax_embed::LanguageGrammar;
//!
//! // Compile from .zyn grammar source
//! let grammar = LanguageGrammar::compile_zyn(include_str!("my_lang.zyn"))?;
//!
//! // Parse source code
//! let program = grammar.parse("fn main() { 42 }")?;
//! ```

mod convert;
mod error;
mod string;
mod array;
mod value;
mod runtime;
mod grammar;
pub mod iterator;

pub use convert::{FromZyntax, IntoZyntax, TryFromZyntax, TryIntoZyntax};
pub use error::{ConversionError, ZyntaxError};
pub use string::ZyntaxString;
pub use array::ZyntaxArray;
pub use value::ZyntaxValue;
pub use runtime::{
    ZyntaxRuntime, ZyntaxPromise, PromiseState, RuntimeError, RuntimeResult,
    TieredRuntime, ImportResolverCallback,
    // Re-export import resolver types for advanced use cases
    ImportResolverTrait, ImportContext, ImportManager, ImportError,
    ResolvedImport, ExportedSymbol, SymbolKind, ModuleArchitecture,
    ChainedResolver, BuiltinResolver,
    // Native calling interface
    NativeType, NativeSignature,
    // Async ABI types
    AsyncPollResult,
    // Promise combinators (Promise.all, Promise.race, etc.)
    PromiseAll, PromiseAllState,
    PromiseRace, PromiseRaceState,
    PromiseAllSettled, SettledResult,
};
pub use grammar::{LanguageGrammar, GrammarError, GrammarResult};
pub use iterator::{
    ZrtlIterable, ZrtlIterator,
    ZyntaxArrayIterator, ZyntaxStringCharsIterator, ZyntaxStringBytesIterator,
    ZyntaxValueIterator, ZrtlRangeIterator,
    StdIteratorAdapter, ZrtlIteratorAdapter,
    IntoZrtlIterator, ZrtlIteratorExt,
};

// Re-export zyn_peg types for custom AST builders and advanced grammar use
pub use zyn_peg::runtime::{
    AstHostFunctions, CommandInterpreter, RuntimeValue, TypedAstBuilder,
    ZpegModule, ZpegMetadata, RuleCommands, AstCommand, NodeHandle,
};

// Re-export TypedProgram for users who parse to TypedAST
pub use zyntax_typed_ast::TypedProgram;

// Re-export tiered compilation types
pub use zyntax_compiler::tiered_backend::{TieredConfig, OptimizationTier, TieredStatistics};

// Re-export core types from zyntax_compiler for convenience
pub use zyntax_compiler::zrtl::{
    DynamicValue, TypeId, TypeCategory, TypeFlags, TypeTag, TypeMeta,
    GenericTypeArgs, GenericValue, TypeInfo, TypeRegistry,
    // ZRTL plugin loading
    ZrtlPlugin, ZrtlRegistry, ZrtlError, ZrtlSymbol, ZrtlInfo, ZRTL_VERSION,
};

// Re-export compiler types needed for module compilation
pub use zyntax_compiler::{
    CompilationConfig, HirModule, HirFunction, CompilerError, CompilerResult,
    compile_to_hir, compile_to_jit,
};
