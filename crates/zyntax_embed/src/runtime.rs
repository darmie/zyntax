//! ZyntaxRuntime - Compiler execution API for embedding Zyntax
//!
//! This module provides a high-level API for compiling and executing Zyntax code
//! from Rust, with automatic value conversion and async/await support.

use std::collections::HashMap;
use std::path::Path;
use std::sync::{Arc, Mutex};
use crate::error::{ConversionError, ZyntaxError};
use crate::grammar::{GrammarError, LanguageGrammar};
use crate::value::ZyntaxValue;
use crate::convert::FromZyntax;
use zyntax_compiler::{
    CompilationConfig, CompilerError,
    cranelift_backend::CraneliftBackend,
    hir::{HirId, HirModule},
    zrtl::DynamicValue,
    tiered_backend::{TieredBackend, TieredConfig, TieredStatistics, OptimizationTier},
    lowering::AstLowering, // For lower_program trait method
    runtime::{Executor, Waker as RuntimeWaker},
};

/// Result type for runtime operations
pub type RuntimeResult<T> = Result<T, RuntimeError>;

/// Errors that can occur during runtime operations
#[derive(Debug, thiserror::Error)]
pub enum RuntimeError {
    #[error("Compilation error: {0}")]
    Compilation(#[from] CompilerError),

    #[error("Function not found: {0}")]
    FunctionNotFound(String),

    #[error("Type conversion error: {0}")]
    Conversion(#[from] ConversionError),

    #[error("Execution error: {0}")]
    Execution(String),

    #[error("Promise error: {0}")]
    Promise(String),

    #[error("Invalid argument count: expected {expected}, got {got}")]
    ArgumentCount { expected: usize, got: usize },
}

impl From<ZyntaxError> for RuntimeError {
    fn from(err: ZyntaxError) -> Self {
        RuntimeError::Execution(err.to_string())
    }
}

// ============================================================================
// Native Calling Convention Types
// ============================================================================

/// Native type for function signatures
///
/// Represents the primitive types that can be passed to/from JIT-compiled functions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NativeType {
    /// 32-bit signed integer
    I32,
    /// 64-bit signed integer
    I64,
    /// 32-bit floating point
    F32,
    /// 64-bit floating point
    F64,
    /// Boolean (passed as i8)
    Bool,
    /// Void (no return value)
    Void,
    /// Pointer (passed as usize)
    Ptr,
}

impl NativeType {
    /// Convert from HIR type to native type
    pub fn from_hir_type(ty: &zyntax_compiler::hir::HirType) -> Self {
        use zyntax_compiler::hir::HirType;
        match ty {
            HirType::I8 | HirType::I16 | HirType::I32 => NativeType::I32,
            HirType::I64 | HirType::I128 => NativeType::I64,
            HirType::U8 | HirType::U16 | HirType::U32 => NativeType::I32,
            HirType::U64 | HirType::U128 => NativeType::I64,
            HirType::F32 => NativeType::F32,
            HirType::F64 => NativeType::F64,
            HirType::Bool => NativeType::Bool,
            HirType::Void => NativeType::Void,
            HirType::Ptr(_) | HirType::Ref { .. } | HirType::Function(_) => NativeType::Ptr,
            HirType::Promise(_) => NativeType::Ptr,
            _ => NativeType::I64, // Default to i64 for unknown types
        }
    }
}

/// Function signature for native calling convention
///
/// Describes the parameter types and return type for a JIT-compiled function.
#[derive(Debug, Clone)]
pub struct NativeSignature {
    /// Parameter types
    pub params: Vec<NativeType>,
    /// Return type
    pub ret: NativeType,
}

impl NativeSignature {
    /// Create a new signature
    pub fn new(params: &[NativeType], ret: NativeType) -> Self {
        Self {
            params: params.to_vec(),
            ret,
        }
    }

    /// Create a signature from an HIR function signature
    pub fn from_hir_signature(sig: &zyntax_compiler::hir::HirFunctionSignature) -> Self {
        let params: Vec<NativeType> = sig.params.iter()
            .map(|p| NativeType::from_hir_type(&p.ty))
            .collect();

        let ret = sig.returns.first()
            .map(|ty| NativeType::from_hir_type(ty))
            .unwrap_or(NativeType::Void);

        Self { params, ret }
    }

    /// Create a signature from a string like "(i32, i32) -> i32"
    pub fn parse(s: &str) -> Option<Self> {
        // Simple parser for signature strings
        let s = s.trim();

        // Find the arrow
        let arrow_pos = s.find("->")?;
        let params_str = s[..arrow_pos].trim();
        let ret_str = s[arrow_pos + 2..].trim();

        // Parse return type
        let ret = Self::parse_type(ret_str)?;

        // Parse parameters
        let params_str = params_str.strip_prefix('(')?.strip_suffix(')')?;
        let params: Option<Vec<_>> = if params_str.is_empty() {
            Some(vec![])
        } else {
            params_str.split(',')
                .map(|p| Self::parse_type(p.trim()))
                .collect()
        };

        Some(Self { params: params?, ret })
    }

    fn parse_type(s: &str) -> Option<NativeType> {
        match s {
            "i32" => Some(NativeType::I32),
            "i64" => Some(NativeType::I64),
            "f32" => Some(NativeType::F32),
            "f64" => Some(NativeType::F64),
            "bool" => Some(NativeType::Bool),
            "void" | "()" => Some(NativeType::Void),
            "ptr" | "*" => Some(NativeType::Ptr),
            _ => None,
        }
    }
}

/// Call a native function with the given signature
///
/// # Safety
/// The caller must ensure the function pointer has the correct signature.
unsafe fn call_native_with_signature(
    ptr: *const u8,
    args: &[ZyntaxValue],
    signature: &NativeSignature,
) -> RuntimeResult<ZyntaxValue> {
    // Convert arguments to native values on the stack
    // We use a union-like approach with i64 as the largest type
    let native_args: Vec<i64> = args.iter()
        .zip(&signature.params)
        .map(|(arg, ty)| value_to_native(arg, *ty))
        .collect::<Result<Vec<_>, _>>()?;

    // Dispatch based on argument count and return type
    // This generates the actual function call with proper ABI
    // Supports up to 8 arguments
    let result_i64 = match native_args.len() {
        0 => call_0(ptr, signature.ret),
        1 => call_1(ptr, native_args[0], signature.ret),
        2 => call_2(ptr, native_args[0], native_args[1], signature.ret),
        3 => call_3(ptr, native_args[0], native_args[1], native_args[2], signature.ret),
        4 => call_4(ptr, native_args[0], native_args[1], native_args[2], native_args[3], signature.ret),
        5 => call_5(ptr, native_args[0], native_args[1], native_args[2], native_args[3], native_args[4], signature.ret),
        6 => call_6(ptr, native_args[0], native_args[1], native_args[2], native_args[3], native_args[4], native_args[5], signature.ret),
        7 => call_7(ptr, native_args[0], native_args[1], native_args[2], native_args[3], native_args[4], native_args[5], native_args[6], signature.ret),
        8 => call_8(ptr, native_args[0], native_args[1], native_args[2], native_args[3], native_args[4], native_args[5], native_args[6], native_args[7], signature.ret),
        n => return Err(RuntimeError::Execution(format!(
            "Unsupported argument count: {}. Maximum is 8.", n
        ))),
    };

    // Convert result back to ZyntaxValue
    native_to_value(result_i64, signature.ret)
}

/// Convert a ZyntaxValue to a native i64 representation
fn value_to_native(value: &ZyntaxValue, ty: NativeType) -> RuntimeResult<i64> {
    match (value, ty) {
        (ZyntaxValue::Int(n), NativeType::I32) => Ok(*n as i64),
        (ZyntaxValue::Int(n), NativeType::I64) => Ok(*n),
        (ZyntaxValue::Float(f), NativeType::F32) => Ok((*f as f32).to_bits() as i64),
        (ZyntaxValue::Float(f), NativeType::F64) => Ok(f.to_bits() as i64),
        (ZyntaxValue::Bool(b), NativeType::Bool) => Ok(if *b { 1 } else { 0 }),
        _ => Err(RuntimeError::Execution(format!(
            "Cannot convert {:?} to {:?}", value, ty
        ))),
    }
}

/// Convert a native i64 result back to ZyntaxValue
fn native_to_value(raw: i64, ty: NativeType) -> RuntimeResult<ZyntaxValue> {
    Ok(match ty {
        NativeType::I32 => ZyntaxValue::Int(raw as i32 as i64),
        NativeType::I64 => ZyntaxValue::Int(raw),
        NativeType::F32 => ZyntaxValue::Float(f32::from_bits(raw as u32) as f64),
        NativeType::F64 => ZyntaxValue::Float(f64::from_bits(raw as u64)),
        NativeType::Bool => ZyntaxValue::Bool(raw != 0),
        NativeType::Void => ZyntaxValue::Null,
        NativeType::Ptr => ZyntaxValue::Int(raw),
    })
}

// Native call dispatch functions
// These use i64 as a universal container and reinterpret based on return type

unsafe fn call_0(ptr: *const u8, ret: NativeType) -> i64 {
    match ret {
        NativeType::I32 => {
            let f: extern "C" fn() -> i32 = std::mem::transmute(ptr);
            f() as i64
        }
        NativeType::I64 => {
            let f: extern "C" fn() -> i64 = std::mem::transmute(ptr);
            f()
        }
        NativeType::F32 => {
            let f: extern "C" fn() -> f32 = std::mem::transmute(ptr);
            f().to_bits() as i64
        }
        NativeType::F64 => {
            let f: extern "C" fn() -> f64 = std::mem::transmute(ptr);
            f().to_bits() as i64
        }
        NativeType::Bool => {
            let f: extern "C" fn() -> i8 = std::mem::transmute(ptr);
            f() as i64
        }
        NativeType::Void | NativeType::Ptr => {
            let f: extern "C" fn() = std::mem::transmute(ptr);
            f();
            0
        }
    }
}

unsafe fn call_1(ptr: *const u8, a0: i64, ret: NativeType) -> i64 {
    match ret {
        NativeType::I32 => {
            let f: extern "C" fn(i64) -> i32 = std::mem::transmute(ptr);
            f(a0) as i64
        }
        NativeType::I64 => {
            let f: extern "C" fn(i64) -> i64 = std::mem::transmute(ptr);
            f(a0)
        }
        NativeType::F32 => {
            let f: extern "C" fn(i64) -> f32 = std::mem::transmute(ptr);
            f(a0).to_bits() as i64
        }
        NativeType::F64 => {
            let f: extern "C" fn(i64) -> f64 = std::mem::transmute(ptr);
            f(a0).to_bits() as i64
        }
        NativeType::Bool => {
            let f: extern "C" fn(i64) -> i8 = std::mem::transmute(ptr);
            f(a0) as i64
        }
        NativeType::Void | NativeType::Ptr => {
            let f: extern "C" fn(i64) = std::mem::transmute(ptr);
            f(a0);
            0
        }
    }
}

unsafe fn call_2(ptr: *const u8, a0: i64, a1: i64, ret: NativeType) -> i64 {
    match ret {
        NativeType::I32 => {
            let f: extern "C" fn(i64, i64) -> i32 = std::mem::transmute(ptr);
            f(a0, a1) as i64
        }
        NativeType::I64 => {
            let f: extern "C" fn(i64, i64) -> i64 = std::mem::transmute(ptr);
            f(a0, a1)
        }
        NativeType::F32 => {
            let f: extern "C" fn(i64, i64) -> f32 = std::mem::transmute(ptr);
            f(a0, a1).to_bits() as i64
        }
        NativeType::F64 => {
            let f: extern "C" fn(i64, i64) -> f64 = std::mem::transmute(ptr);
            f(a0, a1).to_bits() as i64
        }
        NativeType::Bool => {
            let f: extern "C" fn(i64, i64) -> i8 = std::mem::transmute(ptr);
            f(a0, a1) as i64
        }
        NativeType::Void | NativeType::Ptr => {
            let f: extern "C" fn(i64, i64) = std::mem::transmute(ptr);
            f(a0, a1);
            0
        }
    }
}

unsafe fn call_3(ptr: *const u8, a0: i64, a1: i64, a2: i64, ret: NativeType) -> i64 {
    match ret {
        NativeType::I32 => {
            let f: extern "C" fn(i64, i64, i64) -> i32 = std::mem::transmute(ptr);
            f(a0, a1, a2) as i64
        }
        NativeType::I64 => {
            let f: extern "C" fn(i64, i64, i64) -> i64 = std::mem::transmute(ptr);
            f(a0, a1, a2)
        }
        NativeType::F32 => {
            let f: extern "C" fn(i64, i64, i64) -> f32 = std::mem::transmute(ptr);
            f(a0, a1, a2).to_bits() as i64
        }
        NativeType::F64 => {
            let f: extern "C" fn(i64, i64, i64) -> f64 = std::mem::transmute(ptr);
            f(a0, a1, a2).to_bits() as i64
        }
        NativeType::Bool => {
            let f: extern "C" fn(i64, i64, i64) -> i8 = std::mem::transmute(ptr);
            f(a0, a1, a2) as i64
        }
        NativeType::Void | NativeType::Ptr => {
            let f: extern "C" fn(i64, i64, i64) = std::mem::transmute(ptr);
            f(a0, a1, a2);
            0
        }
    }
}

unsafe fn call_4(ptr: *const u8, a0: i64, a1: i64, a2: i64, a3: i64, ret: NativeType) -> i64 {
    match ret {
        NativeType::I32 => {
            let f: extern "C" fn(i64, i64, i64, i64) -> i32 = std::mem::transmute(ptr);
            f(a0, a1, a2, a3) as i64
        }
        NativeType::I64 => {
            let f: extern "C" fn(i64, i64, i64, i64) -> i64 = std::mem::transmute(ptr);
            f(a0, a1, a2, a3)
        }
        NativeType::F32 => {
            let f: extern "C" fn(i64, i64, i64, i64) -> f32 = std::mem::transmute(ptr);
            f(a0, a1, a2, a3).to_bits() as i64
        }
        NativeType::F64 => {
            let f: extern "C" fn(i64, i64, i64, i64) -> f64 = std::mem::transmute(ptr);
            f(a0, a1, a2, a3).to_bits() as i64
        }
        NativeType::Bool => {
            let f: extern "C" fn(i64, i64, i64, i64) -> i8 = std::mem::transmute(ptr);
            f(a0, a1, a2, a3) as i64
        }
        NativeType::Void | NativeType::Ptr => {
            let f: extern "C" fn(i64, i64, i64, i64) = std::mem::transmute(ptr);
            f(a0, a1, a2, a3);
            0
        }
    }
}

unsafe fn call_5(ptr: *const u8, a0: i64, a1: i64, a2: i64, a3: i64, a4: i64, ret: NativeType) -> i64 {
    match ret {
        NativeType::I32 => {
            let f: extern "C" fn(i64, i64, i64, i64, i64) -> i32 = std::mem::transmute(ptr);
            f(a0, a1, a2, a3, a4) as i64
        }
        NativeType::I64 => {
            let f: extern "C" fn(i64, i64, i64, i64, i64) -> i64 = std::mem::transmute(ptr);
            f(a0, a1, a2, a3, a4)
        }
        NativeType::F32 => {
            let f: extern "C" fn(i64, i64, i64, i64, i64) -> f32 = std::mem::transmute(ptr);
            f(a0, a1, a2, a3, a4).to_bits() as i64
        }
        NativeType::F64 => {
            let f: extern "C" fn(i64, i64, i64, i64, i64) -> f64 = std::mem::transmute(ptr);
            f(a0, a1, a2, a3, a4).to_bits() as i64
        }
        NativeType::Bool => {
            let f: extern "C" fn(i64, i64, i64, i64, i64) -> i8 = std::mem::transmute(ptr);
            f(a0, a1, a2, a3, a4) as i64
        }
        NativeType::Void | NativeType::Ptr => {
            let f: extern "C" fn(i64, i64, i64, i64, i64) = std::mem::transmute(ptr);
            f(a0, a1, a2, a3, a4);
            0
        }
    }
}

unsafe fn call_6(ptr: *const u8, a0: i64, a1: i64, a2: i64, a3: i64, a4: i64, a5: i64, ret: NativeType) -> i64 {
    match ret {
        NativeType::I32 => {
            let f: extern "C" fn(i64, i64, i64, i64, i64, i64) -> i32 = std::mem::transmute(ptr);
            f(a0, a1, a2, a3, a4, a5) as i64
        }
        NativeType::I64 => {
            let f: extern "C" fn(i64, i64, i64, i64, i64, i64) -> i64 = std::mem::transmute(ptr);
            f(a0, a1, a2, a3, a4, a5)
        }
        NativeType::F32 => {
            let f: extern "C" fn(i64, i64, i64, i64, i64, i64) -> f32 = std::mem::transmute(ptr);
            f(a0, a1, a2, a3, a4, a5).to_bits() as i64
        }
        NativeType::F64 => {
            let f: extern "C" fn(i64, i64, i64, i64, i64, i64) -> f64 = std::mem::transmute(ptr);
            f(a0, a1, a2, a3, a4, a5).to_bits() as i64
        }
        NativeType::Bool => {
            let f: extern "C" fn(i64, i64, i64, i64, i64, i64) -> i8 = std::mem::transmute(ptr);
            f(a0, a1, a2, a3, a4, a5) as i64
        }
        NativeType::Void | NativeType::Ptr => {
            let f: extern "C" fn(i64, i64, i64, i64, i64, i64) = std::mem::transmute(ptr);
            f(a0, a1, a2, a3, a4, a5);
            0
        }
    }
}

unsafe fn call_7(ptr: *const u8, a0: i64, a1: i64, a2: i64, a3: i64, a4: i64, a5: i64, a6: i64, ret: NativeType) -> i64 {
    match ret {
        NativeType::I32 => {
            let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64) -> i32 = std::mem::transmute(ptr);
            f(a0, a1, a2, a3, a4, a5, a6) as i64
        }
        NativeType::I64 => {
            let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64) -> i64 = std::mem::transmute(ptr);
            f(a0, a1, a2, a3, a4, a5, a6)
        }
        NativeType::F32 => {
            let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64) -> f32 = std::mem::transmute(ptr);
            f(a0, a1, a2, a3, a4, a5, a6).to_bits() as i64
        }
        NativeType::F64 => {
            let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64) -> f64 = std::mem::transmute(ptr);
            f(a0, a1, a2, a3, a4, a5, a6).to_bits() as i64
        }
        NativeType::Bool => {
            let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64) -> i8 = std::mem::transmute(ptr);
            f(a0, a1, a2, a3, a4, a5, a6) as i64
        }
        NativeType::Void | NativeType::Ptr => {
            let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64) = std::mem::transmute(ptr);
            f(a0, a1, a2, a3, a4, a5, a6);
            0
        }
    }
}

unsafe fn call_8(ptr: *const u8, a0: i64, a1: i64, a2: i64, a3: i64, a4: i64, a5: i64, a6: i64, a7: i64, ret: NativeType) -> i64 {
    match ret {
        NativeType::I32 => {
            let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64, i64) -> i32 = std::mem::transmute(ptr);
            f(a0, a1, a2, a3, a4, a5, a6, a7) as i64
        }
        NativeType::I64 => {
            let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64, i64) -> i64 = std::mem::transmute(ptr);
            f(a0, a1, a2, a3, a4, a5, a6, a7)
        }
        NativeType::F32 => {
            let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64, i64) -> f32 = std::mem::transmute(ptr);
            f(a0, a1, a2, a3, a4, a5, a6, a7).to_bits() as i64
        }
        NativeType::F64 => {
            let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64, i64) -> f64 = std::mem::transmute(ptr);
            f(a0, a1, a2, a3, a4, a5, a6, a7).to_bits() as i64
        }
        NativeType::Bool => {
            let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64, i64) -> i8 = std::mem::transmute(ptr);
            f(a0, a1, a2, a3, a4, a5, a6, a7) as i64
        }
        NativeType::Void | NativeType::Ptr => {
            let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64, i64) = std::mem::transmute(ptr);
            f(a0, a1, a2, a3, a4, a5, a6, a7);
            0
        }
    }
}

/// Call a function with dynamic values using a signature
///
/// This is the signature-based dispatch for async function calls.
/// Returns the raw pointer result (for Promise-returning async functions).
///
/// # Safety
/// The caller must ensure the function pointer has the correct signature.
unsafe fn call_with_signature(ptr: *const u8, args: &[DynamicValue], signature: &NativeSignature) -> *const u8 {
    // Convert DynamicValue to i64 for the native call
    let native_args: Vec<i64> = args.iter()
        .zip(&signature.params)
        .map(|(arg, _ty)| dynamic_to_i64(arg))
        .collect();

    // For async functions, the return type is always a pointer (*Promise<T>)
    // We dispatch based on argument count
    match native_args.len() {
        0 => {
            let f: extern "C" fn() -> *const u8 = std::mem::transmute(ptr);
            f()
        }
        1 => {
            let f: extern "C" fn(i64) -> *const u8 = std::mem::transmute(ptr);
            f(native_args[0])
        }
        2 => {
            let f: extern "C" fn(i64, i64) -> *const u8 = std::mem::transmute(ptr);
            f(native_args[0], native_args[1])
        }
        3 => {
            let f: extern "C" fn(i64, i64, i64) -> *const u8 = std::mem::transmute(ptr);
            f(native_args[0], native_args[1], native_args[2])
        }
        4 => {
            let f: extern "C" fn(i64, i64, i64, i64) -> *const u8 = std::mem::transmute(ptr);
            f(native_args[0], native_args[1], native_args[2], native_args[3])
        }
        5 => {
            let f: extern "C" fn(i64, i64, i64, i64, i64) -> *const u8 = std::mem::transmute(ptr);
            f(native_args[0], native_args[1], native_args[2], native_args[3], native_args[4])
        }
        6 => {
            let f: extern "C" fn(i64, i64, i64, i64, i64, i64) -> *const u8 = std::mem::transmute(ptr);
            f(native_args[0], native_args[1], native_args[2], native_args[3], native_args[4], native_args[5])
        }
        7 => {
            let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64) -> *const u8 = std::mem::transmute(ptr);
            f(native_args[0], native_args[1], native_args[2], native_args[3], native_args[4], native_args[5], native_args[6])
        }
        8 => {
            let f: extern "C" fn(i64, i64, i64, i64, i64, i64, i64, i64) -> *const u8 = std::mem::transmute(ptr);
            f(native_args[0], native_args[1], native_args[2], native_args[3], native_args[4], native_args[5], native_args[6], native_args[7])
        }
        _ => {
            log::error!("Unsupported argument count: {}. Maximum is 8.", native_args.len());
            std::ptr::null()
        }
    }
}

/// Convert a DynamicValue to i64 for native calls
fn dynamic_to_i64(value: &DynamicValue) -> i64 {
    // Try each primitive type accessor
    if let Some(i) = value.get_i32() {
        return i as i64;
    }
    if let Some(i) = value.get_i64() {
        return i;
    }
    if let Some(f) = value.get_f32() {
        return f.to_bits() as i64;
    }
    if let Some(f) = value.get_f64() {
        return f.to_bits() as i64;
    }
    if let Some(b) = value.get_bool() {
        return if b { 1 } else { 0 };
    }
    // For pointer types, just use the raw pointer value
    if !value.value_ptr.is_null() {
        return value.value_ptr as i64;
    }
    0
}



/// Simple callback type for resolving imports
///
/// Called during compilation when an import statement is encountered.
/// Returns the resolved module content (source code) or an error message.
///
/// # Arguments
/// * `module_path` - The import path as a dot-separated string (e.g., "std.io", "my_module")
///
/// # Returns
/// * `Ok(Some(source))` - The resolved module source code
/// * `Ok(None)` - Module not found by this resolver (try next resolver)
/// * `Err(message)` - Error resolving the module
pub type ImportResolverCallback = Box<dyn Fn(&str) -> Result<Option<String>, String> + Send + Sync>;

// Re-export the full ImportResolver trait from the compiler for advanced use cases
pub use zyntax_compiler::{
    ImportResolver as ImportResolverTrait, ImportContext, ImportManager,
    ImportError, ResolvedImport, ExportedSymbol, SymbolKind, ModuleArchitecture,
    ChainedResolver, BuiltinResolver,
};


/// A compiled Zyntax runtime ready for execution
///
/// `ZyntaxRuntime` provides a safe interface for:
/// - Compiling Zyntax source code or TypedAST
/// - Calling functions with automatic value conversion
/// - Managing async operations via promises
///
/// # Example
///
/// ```ignore
/// use zyntax_embed::{ZyntaxRuntime, ZyntaxValue};
///
/// let mut runtime = ZyntaxRuntime::new()?;
/// runtime.compile_source("fn add(a: i32, b: i32) -> i32 { a + b }")?;
///
/// let result: i32 = runtime.call("add", &[42.into(), 8.into()])?;
/// assert_eq!(result, 50);
/// ```
pub struct ZyntaxRuntime {
    /// The Cranelift JIT backend
    backend: CraneliftBackend,
    /// Mapping from function names to HIR IDs
    function_ids: HashMap<String, HirId>,
    /// Mapping from function names to their native signatures
    function_signatures: HashMap<String, NativeSignature>,
    /// Compilation configuration
    config: CompilationConfig,
    /// Registered external functions
    external_functions: HashMap<String, ExternalFunction>,
    /// Import resolver callbacks (tried in order)
    import_resolvers: Vec<ImportResolverCallback>,
    /// Registered language grammars (language name -> grammar)
    grammars: HashMap<String, Arc<LanguageGrammar>>,
    /// File extension to language mapping (e.g., ".zig" -> "zig")
    extension_map: HashMap<String, String>,
    /// Names of async functions (original name, not _new suffix)
    async_functions: std::collections::HashSet<String>,
    /// Plugin signatures (symbol name -> ZRTL signature)
    /// Collected from loaded plugins for proper extern function type checking
    plugin_signatures: HashMap<String, zyntax_compiler::zrtl::ZrtlSymbolSig>,
}

/// An external function that can be called from Zyntax code
#[derive(Clone)]
pub struct ExternalFunction {
    /// Function name
    pub name: String,
    /// Function pointer
    pub ptr: *const u8,
    /// Expected argument count
    pub arg_count: usize,
}

// SAFETY: Function pointers are inherently thread-unsafe, but we manage
// access through the runtime's mutex-protected state
unsafe impl Send for ExternalFunction {}
unsafe impl Sync for ExternalFunction {}

impl ZyntaxRuntime {
    /// Create a new runtime with default configuration
    pub fn new() -> RuntimeResult<Self> {
        Self::with_config(CompilationConfig::default())
    }

    /// Create a new runtime with custom configuration
    pub fn with_config(config: CompilationConfig) -> RuntimeResult<Self> {
        let backend = CraneliftBackend::new()?;

        Ok(Self {
            backend,
            function_ids: HashMap::new(),
            function_signatures: HashMap::new(),
            config,
            external_functions: HashMap::new(),
            import_resolvers: Vec::new(),
            grammars: HashMap::new(),
            extension_map: HashMap::new(),
            async_functions: std::collections::HashSet::new(),
            plugin_signatures: HashMap::new(),
        })
    }

    /// Create a new runtime with additional runtime symbols for FFI
    ///
    /// This allows linking external C functions or Rust functions into the JIT.
    pub fn with_symbols(symbols: &[(&str, *const u8)]) -> RuntimeResult<Self> {
        let backend = CraneliftBackend::with_runtime_symbols(symbols)?;

        Ok(Self {
            backend,
            function_ids: HashMap::new(),
            function_signatures: HashMap::new(),
            config: CompilationConfig::default(),
            external_functions: HashMap::new(),
            import_resolvers: Vec::new(),
            grammars: HashMap::new(),
            extension_map: HashMap::new(),
            async_functions: std::collections::HashSet::new(),
            plugin_signatures: HashMap::new(),
        })
    }

    /// Compile a HIR module into the runtime
    ///
    /// After compilation, functions can be called via `call()` or `call_async()`.
    ///
    /// If the module has extern declarations that match previously compiled functions,
    /// the backend will be rebuilt to include those symbols before compilation.
    pub fn compile_module(&mut self, module: &zyntax_compiler::HirModule) -> RuntimeResult<()> {
        // Check if we need to rebuild the backend for cross-module linking
        if self.backend.needs_rebuild_for_module(module) {
            log::debug!("[Runtime] Rebuilding JIT for cross-module symbol resolution");
            self.backend.rebuild_with_accumulated_symbols()?;
        }

        // Store function name -> ID mapping (resolve InternedString to actual string)
        // Also track which functions are async and store their signatures
        for (id, func) in &module.functions {
            if let Some(name) = func.name.resolve_global() {
                self.function_ids.insert(name.clone(), *id);

                // Store the function signature for later use in call/call_async
                let native_sig = NativeSignature::from_hir_signature(&func.signature);
                self.function_signatures.insert(name.clone(), native_sig);

                // Track async functions by their original name
                // Async functions are transformed into {name}_new (constructor) and {name}_poll (poll)
                // We track the original name for call_async lookup
                //
                // Detection strategy:
                // 1. If func.signature.is_async, use the function name directly
                // 2. If function name ends with "_new", extract original name (async constructor)
                if func.signature.is_async {
                    self.async_functions.insert(name.clone());
                } else if name.ends_with("_new") {
                    // This is an async constructor - extract the original function name
                    let orig_name = name[..name.len() - 4].to_string();
                    self.async_functions.insert(orig_name);
                }
            }
        }

        // Compile the module
        self.backend.compile_module(module)?;

        // Finalize definitions to get function pointers
        self.backend.finalize_definitions()?;

        Ok(())
    }

    /// Compile source code using a language grammar
    ///
    /// This method parses the source code using the provided grammar, lowers the
    /// TypedAST to HIR, and compiles it into the runtime.
    ///
    /// # Arguments
    /// * `grammar` - The language grammar to use for parsing
    /// * `source` - The source code to compile
    ///
    /// # Example
    ///
    /// ```ignore
    /// use zyntax_embed::{ZyntaxRuntime, LanguageGrammar};
    ///
    /// let grammar = LanguageGrammar::compile_zyn(include_str!("zig.zyn"))?;
    /// let mut runtime = ZyntaxRuntime::new()?;
    /// runtime.compile_with_grammar(&grammar, "fn main() -> i32 { 42 }")?;
    ///
    /// let result: i32 = runtime.call("main", &[])?;
    /// assert_eq!(result, 42);
    /// ```
    pub fn compile_with_grammar(
        &mut self,
        grammar: &crate::grammar::LanguageGrammar,
        source: &str,
    ) -> RuntimeResult<()> {
        // Parse source to TypedAST with plugin signatures for proper extern declarations
        let typed_program = grammar.parse_with_signatures(source, "source.zynml", &self.plugin_signatures)
            .map_err(|e| RuntimeError::Execution(e.to_string()))?;

        // Lower to HIR
        let hir_module = self.lower_typed_program(typed_program)?;

        // Compile the module
        self.compile_module(&hir_module)
    }

    /// Lower a TypedProgram to HirModule
    ///
    /// This performs the lowering pass to convert the TypedAST to HIR,
    /// which can then be compiled to machine code.
    fn lower_typed_program(&self, mut program: zyntax_typed_ast::TypedProgram) -> RuntimeResult<HirModule> {
        use zyntax_compiler::lowering::{LoweringContext, LoweringConfig};
        use zyntax_typed_ast::{AstArena, InternedString, TypeRegistry, TypedDeclaration, type_registry::*};

        // Rebuild type registry from declarations (TypeRegistry is not serializable)
        // Scan for struct definitions (TypedDeclaration::Class) and register them
        // IMPORTANT: Only register types that don't already exist (abstract types are pre-registered by parser)
        for decl_node in &program.declarations {
            let decl_kind = match &decl_node.node {
                TypedDeclaration::Function(f) => format!("Function({})", f.name.resolve_global().unwrap_or_default()),
                TypedDeclaration::Class(c) => format!("Class({})", c.name.resolve_global().unwrap_or_default()),
                TypedDeclaration::Impl(_) => "Impl".to_string(),
                TypedDeclaration::Variable(_) => "Variable".to_string(),
                TypedDeclaration::Import(_) => "Import".to_string(),
                TypedDeclaration::Enum(_) => "Enum".to_string(),
                _ => "Other".to_string(),
            };
            eprintln!("[DEBUG] Declaration {} ty: {:?}", decl_kind, decl_node.ty);
            if let TypedDeclaration::Class(class) = &decl_node.node {
                eprintln!("[DEBUG] Found Class '{}' with type: {:?}", class.name, decl_node.ty);

                // Check if type is already registered (e.g., abstract types from parser)
                if let Some(existing_type) = program.type_registry.get_type_by_name(class.name) {
                    eprintln!("[DEBUG] Type '{}' already registered with kind: {:?}, skipping re-registration",
                        class.name.resolve_global().unwrap_or("Unknown".to_string()),
                        std::mem::discriminant(&existing_type.kind));
                    continue;
                }

                // Check if this is a struct (no methods, just fields)
                // Create TypeDefinition and register it
                if let zyntax_typed_ast::Type::Named { id, .. } = &decl_node.ty {
                    let field_defs: Vec<FieldDef> = class.fields.iter().map(|f| FieldDef {
                        name: f.name,
                        ty: f.ty.clone(),
                        visibility: f.visibility,
                        mutability: f.mutability,
                        is_static: f.is_static,
                        span: f.span,
                        getter: None,
                        setter: None,
                        is_synthetic: false,
                    }).collect();

                    let type_def = TypeDefinition {
                        id: *id,
                        name: class.name,
                        kind: TypeKind::Struct {
                            fields: field_defs.clone(),
                            is_tuple: false,
                        },
                        type_params: vec![],
                        constraints: vec![],
                        fields: field_defs,
                        methods: vec![],
                        constructors: vec![],
                        metadata: Default::default(),
                        span: class.span,
                    };
                    program.type_registry.register_type(type_def);
                    eprintln!("[DEBUG] Reconstructed struct type from main program: {} with TypeId: {:?}", class.name, *id);
                }
            }
        }

        let arena = AstArena::new();
        let module_name = InternedString::new_global("main");
        let mut type_registry = program.type_registry.clone();

        // Process imports FIRST to load stdlib traits and impls
        // This merges declarations from imported modules into the program
        // and registers their opaque types in the type registry
        self.process_imports_for_traits(&mut program, &mut type_registry)?;

        // Now process extern declarations from the merged program (main + imports)
        // to ensure all opaque types are registered (needs &mut)
        self.process_extern_declarations_mut(&program, &mut type_registry)?;

        // IMPORTANT: Resolve all Type::Unresolved in the TypedAST before lowering
        // This mutates the program to replace Unresolved types with actual types from TypeRegistry
        // The compiler's type checker and SSA builder need resolved types
        self.resolve_unresolved_types(&mut program, &type_registry);

        // IMPORTANT: Sync the program's type_registry with our local copy that has merged imports
        // This is needed because process_imports_for_traits merges into `type_registry` not `program.type_registry`
        program.type_registry = type_registry;

        // Register impl blocks before lowering
        zyntax_compiler::register_impl_blocks(&mut program)
            .map_err(|e| RuntimeError::Execution(format!("Failed to register impl blocks: {:?}", e)))?;

        // Generate automatic trait implementations for abstract types
        zyntax_compiler::generate_abstract_trait_impls(&mut program)
            .map_err(|e| RuntimeError::Execution(format!("Failed to generate abstract trait impls: {:?}", e)))?;

        // Debug: Check what declarations exist after generation
        eprintln!("[DEBUG AFTER GENERATION] Total declarations: {}", program.declarations.len());
        for decl_node in &program.declarations {
            if let TypedDeclaration::Function(f) = &decl_node.node {
                let name = f.name.resolve_global().unwrap_or_default();
                if name.contains("Duration") || name.contains("$add") || name.contains("$sub") {
                    eprintln!("[DEBUG AFTER GENERATION] Found Duration-related function: {}", name);
                }
            }
        }

        // Register the generated impl blocks
        zyntax_compiler::register_impl_blocks(&mut program)
            .map_err(|e| RuntimeError::Execution(format!("Failed to register generated impl blocks: {:?}", e)))?;

        // Wrap program's type registry in Arc for sharing (it now includes registered traits and impls)
        let type_registry_arc = std::sync::Arc::new(program.type_registry.clone());

        let mut lowering_ctx = LoweringContext::new(
            module_name,
            type_registry_arc.clone(),
            std::sync::Arc::new(std::sync::Mutex::new(arena)),
            LoweringConfig::default(),
        );

        // Don't skip type checking - enable type inference for trait resolution
        // The parser produces TypedAST with Type::Any and placeholder TypeIds that need inference

        // Debug: Print all declaration types before lowering
        eprintln!("[DEBUG BEFORE LOWERING] Total declarations: {}", program.declarations.len());
        for (idx, decl) in program.declarations.iter().enumerate() {
            if idx < 20 || matches!(&decl.node, TypedDeclaration::Impl(_)) {
                eprintln!("[DEBUG BEFORE LOWERING] Decl {}: {:?}", idx, std::mem::discriminant(&decl.node));
            }
        }

        let mut hir_module = lowering_ctx
            .lower_program(&mut program)
            .map_err(|e| RuntimeError::Execution(format!("Lowering error: {:?}", e)))?;

        // Monomorphization
        zyntax_compiler::monomorphize_module(&mut hir_module)
            .map_err(|e| RuntimeError::Execution(format!("Monomorphization error: {:?}", e)))?;

        Ok(hir_module)
    }

    /// Process import declarations to load stdlib traits and implementations
    ///
    /// This scans the TypedProgram for Import declarations, resolves them using
    /// the registered import resolvers, parses the imported modules, and registers
    /// their trait definitions and implementations in the TypeRegistry.
    fn process_imports_for_traits(
        &self,
        program: &mut zyntax_typed_ast::TypedProgram,
        type_registry: &mut zyntax_typed_ast::TypeRegistry,
    ) -> RuntimeResult<()> {
        use zyntax_typed_ast::typed_ast::TypedDeclaration;

        // Collect imports to process (can't mutate while iterating)
        let mut imports_to_process = Vec::new();
        for decl in &program.declarations {
            if let TypedDeclaration::Import(import) = &decl.node {
                let module_name = import.module_path
                    .first()
                    .and_then(|s| s.resolve_global())
                    .unwrap_or_else(|| "unknown".to_string());
                imports_to_process.push(module_name);
            }
        }

        // Process each import
        for module_name in imports_to_process {
            log::debug!("Processing import: {}", module_name);

            // Try to resolve the import using our import resolvers
            if let Ok(Some(source)) = self.resolve_import(&module_name) {
                log::debug!("Resolved import '{}', parsing module...", module_name);

                // Find a grammar to parse the imported module
                // Try each registered grammar until one succeeds
                // Pass plugin signatures to ensure proper extern function declarations
                let mut parsed_program = None;
                for (_lang_name, grammar) in &self.grammars {
                    match grammar.parse_with_signatures(&source, &module_name, &self.plugin_signatures) {
                        Ok(imported_program) => {
                            parsed_program = Some(imported_program);
                            break;
                        }
                        Err(_) => continue,
                    }
                }

                if let Some(mut imported_program) = parsed_program {
                    log::info!("Parsed stdlib module '{}': {} declarations",
                        module_name, imported_program.declarations.len());

                    // First, merge the TypeRegistry from the imported module
                    // This includes struct definitions, trait definitions, etc.
                    eprintln!("[DEBUG] Merging TypeRegistry from imported module '{}'", module_name);
                    eprintln!("[DEBUG] Imported module has {} types before merge", imported_program.type_registry.get_all_types().count());
                    type_registry.merge_from(&imported_program.type_registry);
                    eprintln!("[DEBUG] Main registry now has {} types after merge", type_registry.get_all_types().count());

                    // First, process extern declarations from the imported module
                    // to register opaque types in the type registry
                    if let Err(e) = self.process_extern_declarations_mut(&imported_program, type_registry) {
                        log::warn!("Failed to process extern declarations from '{}': {}", module_name, e);
                    }

                    // Merge declarations from imported module into main program
                    // Filter out the import declarations themselves to avoid circular imports
                    for imported_decl in imported_program.declarations.drain(..) {
                        if !matches!(imported_decl.node, TypedDeclaration::Import(_)) {
                            program.declarations.push(imported_decl);
                        }
                    }

                    log::debug!("Merged declarations from '{}'", module_name);
                } else {
                    log::warn!("Failed to parse imported module '{}' with any registered grammar", module_name);
                }
            } else {
                log::warn!("Could not resolve import: {}", module_name);
            }
        }

        Ok(())
    }

    /// Resolve all Type::Unresolved in the TypedProgram
    ///
    /// This walks the TypedAST and replaces Type::Unresolved(name) with the actual
    /// type from TypeRegistry (e.g., Type::Extern for opaque types from imports).
    /// This must be called AFTER process_imports_for_traits and process_extern_declarations_mut.
    fn resolve_unresolved_types(
        &self,
        program: &mut zyntax_typed_ast::TypedProgram,
        type_registry: &zyntax_typed_ast::TypeRegistry,
    ) {
        use zyntax_typed_ast::typed_ast::{TypedDeclaration, TypedNode};
        use zyntax_typed_ast::type_registry::Type;

        log::debug!("Resolving unresolved types in program with {} declarations", program.declarations.len());

        // Walk all declarations and resolve types
        for decl in &mut program.declarations {
            Self::resolve_in_declaration(&mut decl.node, type_registry);
            Self::resolve_in_type(&mut decl.ty, type_registry);
        }
    }

    fn resolve_in_declaration(
        decl: &mut zyntax_typed_ast::typed_ast::TypedDeclaration,
        type_registry: &zyntax_typed_ast::TypeRegistry,
    ) {
        use zyntax_typed_ast::typed_ast::TypedDeclaration;

        match decl {
            TypedDeclaration::Function(func) => {
                // Resolve parameter types
                for param in &mut func.params {
                    Self::resolve_in_type(&mut param.ty, type_registry);
                }
                // Resolve return type
                Self::resolve_in_type(&mut func.return_type, type_registry);
                // Resolve body expression types
                if let Some(body) = &mut func.body {
                    Self::resolve_in_block(body, type_registry);
                }
            }
            TypedDeclaration::Impl(impl_block) => {
                // Resolve trait type arguments
                for trait_ty in &mut impl_block.trait_type_args {
                    Self::resolve_in_type(trait_ty, type_registry);
                }
                // Resolve target type
                Self::resolve_in_type(&mut impl_block.for_type, type_registry);
                // Resolve method types
                for method in &mut impl_block.methods {
                    for param in &mut method.params {
                        Self::resolve_in_type(&mut param.ty, type_registry);
                    }
                    Self::resolve_in_type(&mut method.return_type, type_registry);
                    if let Some(body) = &mut method.body {
                        Self::resolve_in_block(body, type_registry);
                    }
                }
            }
            _ => {
                // Other declarations don't have types to resolve
            }
        }
    }

    fn resolve_in_block(
        block: &mut zyntax_typed_ast::typed_ast::TypedBlock,
        type_registry: &zyntax_typed_ast::TypeRegistry,
    ) {
        use zyntax_typed_ast::typed_ast::TypedStatement;

        for stmt in &mut block.statements {
            Self::resolve_in_stmt(stmt, type_registry);
        }
    }

    fn resolve_in_stmt(
        stmt: &mut zyntax_typed_ast::typed_ast::TypedNode<zyntax_typed_ast::typed_ast::TypedStatement>,
        type_registry: &zyntax_typed_ast::TypeRegistry,
    ) {
        use zyntax_typed_ast::typed_ast::TypedStatement;

        // Resolve the statement's type annotation
        Self::resolve_in_type(&mut stmt.ty, type_registry);

        // Resolve types in the statement node
        match &mut stmt.node {
            TypedStatement::Expression(expr) => {
                Self::resolve_in_expr(expr, type_registry);
            }
            TypedStatement::Let(let_stmt) => {
                Self::resolve_in_type(&mut let_stmt.ty, type_registry);
                if let Some(init) = &mut let_stmt.initializer {
                    Self::resolve_in_expr(init, type_registry);
                }
            }
            _ => {
                // Other statement types
            }
        }
    }

    fn resolve_in_expr(
        expr: &mut zyntax_typed_ast::typed_ast::TypedNode<zyntax_typed_ast::typed_ast::TypedExpression>,
        type_registry: &zyntax_typed_ast::TypeRegistry,
    ) {
        use zyntax_typed_ast::typed_ast::TypedExpression;

        // Resolve the expression's type annotation
        Self::resolve_in_type(&mut expr.ty, type_registry);

        // Recursively resolve types in sub-expressions
        match &mut expr.node {
            TypedExpression::Binary(bin) => {
                Self::resolve_in_expr(&mut bin.left, type_registry);
                Self::resolve_in_expr(&mut bin.right, type_registry);
            }
            TypedExpression::Unary(un) => {
                Self::resolve_in_expr(&mut un.operand, type_registry);
            }
            TypedExpression::Call(call) => {
                Self::resolve_in_expr(&mut call.callee, type_registry);
                for arg in &mut call.positional_args {
                    Self::resolve_in_expr(arg, type_registry);
                }
                for named_arg in &mut call.named_args {
                    Self::resolve_in_expr(&mut named_arg.value, type_registry);
                }
            }
            TypedExpression::Block(block) => {
                Self::resolve_in_block(block, type_registry);
            }
            _ => {
                // Other expression types don't have nested expressions that need resolution
                // TODO: Handle other expression types as needed
            }
        }
    }

    fn resolve_in_type(
        ty: &mut zyntax_typed_ast::type_registry::Type,
        type_registry: &zyntax_typed_ast::TypeRegistry,
    ) {
        use zyntax_typed_ast::type_registry::Type;

        match ty {
            Type::Unresolved(name) => {
                // First try type aliases
                if let Some(resolved) = type_registry.resolve_alias(*name) {
                    log::debug!("Resolved type '{}' from alias to {:?}",
                        name.resolve_global().unwrap_or_default(), resolved);
                    *ty = resolved.clone();
                }
                // Then try looking up by name in the type definitions (for structs, enums, etc.)
                else if let Some(type_def) = type_registry.get_type_by_name(*name) {
                    let resolved = Type::Named {
                        id: type_def.id,
                        type_args: vec![],
                        const_args: vec![],
                        variance: vec![],
                        nullability: zyntax_typed_ast::type_registry::NullabilityKind::NonNull,
                    };
                    log::debug!("Resolved type '{}' from Unresolved to Named({:?})",
                        name.resolve_global().unwrap_or_default(), type_def.id);
                    *ty = resolved;
                } else {
                    log::warn!("Could not resolve type '{}'",
                        name.resolve_global().unwrap_or_default());
                }
            }
            // Recursively resolve nested types
            Type::Reference { ty: inner, .. } => {
                Self::resolve_in_type(inner, type_registry);
            }
            Type::Array { element_type, .. } => {
                Self::resolve_in_type(element_type, type_registry);
            }
            Type::Function { params, return_type, .. } => {
                for param in params {
                    Self::resolve_in_type(&mut param.ty, type_registry);
                }
                Self::resolve_in_type(return_type, type_registry);
            }
            Type::Tuple(types) => {
                for t in types {
                    Self::resolve_in_type(t, type_registry);
                }
            }
            _ => {
                // Other types don't need resolution
            }
        }
    }

    /// Process extern declarations to register opaque types in the TypeRegistry
    ///
    /// This scans the TypedProgram for Extern::Struct declarations (created by @opaque)
    /// and registers them in the TypeRegistry so they can be resolved during type checking.
    fn process_extern_declarations_mut(
        &self,
        program: &zyntax_typed_ast::TypedProgram,
        type_registry: &mut zyntax_typed_ast::TypeRegistry,
    ) -> RuntimeResult<()> {
        use zyntax_typed_ast::typed_ast::{TypedDeclaration, TypedExtern};
        use zyntax_typed_ast::type_registry::{Type, TypeDefinition, TypeKind, TypeMetadata};
        use zyntax_typed_ast::TypeId;

        eprintln!("[DEBUG] process_extern_declarations_mut called with {} declarations", program.declarations.len());

        // Collect all extern struct declarations
        for decl in &program.declarations {
            eprintln!("[DEBUG] Processing declaration: {:?}", std::mem::discriminant(&decl.node));
            if let TypedDeclaration::Extern(extern_decl) = &decl.node {
                eprintln!("[DEBUG] Found Extern declaration!");
                if let TypedExtern::Struct(extern_struct) = extern_decl {
                    eprintln!("[DEBUG] Found Extern::Struct!");

                    // Register the extern type as a proper type in the type registry
                    // This creates a Named type with a real TypeId
                    eprintln!("[DEBUG] Registering extern struct: name='{}', runtime_prefix='{}'",
                        extern_struct.name.resolve_global().unwrap_or_default(),
                        extern_struct.runtime_prefix.resolve_global().unwrap_or_default()
                    );

                    // Create TypeDefinition for the extern/opaque type
                    let type_id = TypeId::next();
                    let type_def = TypeDefinition {
                        id: type_id,
                        name: extern_struct.name,
                        kind: TypeKind::Atomic,  // Extern types are atomic/opaque
                        type_params: vec![],
                        constraints: vec![],
                        fields: vec![],
                        methods: vec![],
                        constructors: vec![],
                        metadata: TypeMetadata::default(),
                        span: decl.span,
                    };

                    // Register the type definition
                    type_registry.register_type(type_def);

                    // ALSO register an alias mapping the name to Type::Extern
                    // This allows the resolution pass to find the runtime name
                    let extern_type = Type::Extern {
                        name: extern_struct.runtime_prefix,
                        layout: None,
                    };
                    type_registry.register_alias(extern_struct.name, extern_type);

                    eprintln!("[DEBUG] Registered extern type with TypeId {:?}", type_id);
                } else {
                    eprintln!("[DEBUG] Extern but not Struct: {:?}", std::mem::discriminant(extern_decl));
                }
            }
        }

        Ok(())
    }

    /// Get a function pointer by name
    pub fn get_function_ptr(&self, name: &str) -> Option<*const u8> {
        self.function_ids.get(name)
            .and_then(|id| self.backend.get_function_ptr(*id))
    }

    /// Call a function by name with the given arguments
    ///
    /// Arguments are automatically converted from `ZyntaxValue` and the result
    /// is converted back to the requested Rust type.
    ///
    /// # Example
    ///
    /// ```ignore
    /// let result: i32 = runtime.call("add", &[10.into(), 20.into()])?;
    /// ```
    pub fn call<T: FromZyntax>(&self, name: &str, args: &[ZyntaxValue]) -> RuntimeResult<T> {
        let result = self.call_raw(name, args)?;
        T::from_zyntax(result).map_err(RuntimeError::from)
    }

    /// Call a function and get the raw ZyntaxValue result
    ///
    /// Note: This uses the stored function signature to determine the calling convention.
    /// For void functions, it uses a void-returning call. For native (i32, i64) functions,
    /// use `call_native` with a signature instead.
    pub fn call_raw(&self, name: &str, args: &[ZyntaxValue]) -> RuntimeResult<ZyntaxValue> {
        let ptr = self.get_function_ptr(name)
            .ok_or_else(|| RuntimeError::FunctionNotFound(name.to_string()))?;

        // Check if this is a void function using the stored signature
        if let Some(sig) = self.function_signatures.get(name) {
            if sig.ret == NativeType::Void {
                // For void functions, use the native calling convention
                // SAFETY: We trust the function signature is correct
                unsafe {
                    call_native_with_signature(ptr, args, sig)?;
                }
                return Ok(ZyntaxValue::Void);
            }
        }

        // Convert arguments to DynamicValues
        let dynamic_args: Vec<DynamicValue> = args.iter()
            .cloned()
            .map(|v| v.into_dynamic())
            .collect();

        // Call the function using the variadic caller helper
        // SAFETY: We trust the caller has provided the correct function pointer
        // and matching arguments. from_dynamic is safe because call_dynamic_function
        // returns a valid DynamicValue.
        unsafe {
            let result = call_dynamic_function(ptr, &dynamic_args)?;
            ZyntaxValue::from_dynamic(result).map_err(RuntimeError::from)
        }
    }

    /// Call a JIT-compiled function with the specified signature
    ///
    /// This method dynamically constructs the function call based on the provided
    /// signature, converting ZyntaxValue arguments to the appropriate types.
    ///
    /// # Arguments
    /// * `name` - The function name
    /// * `args` - The arguments as ZyntaxValues
    /// * `signature` - The function signature describing parameter and return types
    ///
    /// # Example
    ///
    /// ```ignore
    /// use zyntax_embed::{ZyntaxRuntime, NativeSignature, NativeType};
    ///
    /// // fn add(a: i32, b: i32) -> i32
    /// let sig = NativeSignature::new(&[NativeType::I32, NativeType::I32], NativeType::I32);
    /// let result = runtime.call_function("add", &[10.into(), 32.into()], &sig)?;
    /// assert_eq!(result, ZyntaxValue::Int(42));
    /// ```
    pub fn call_function(
        &self,
        name: &str,
        args: &[ZyntaxValue],
        signature: &NativeSignature,
    ) -> RuntimeResult<ZyntaxValue> {
        let ptr = self.get_function_ptr(name)
            .ok_or_else(|| RuntimeError::FunctionNotFound(name.to_string()))?;

        // Validate argument count
        if args.len() != signature.params.len() {
            return Err(RuntimeError::Execution(format!(
                "Function '{}' expects {} arguments, got {}",
                name, signature.params.len(), args.len()
            )));
        }

        // SAFETY: We trust the caller has provided the correct signature
        unsafe {
            call_native_with_signature(ptr, args, signature)
        }
    }

    /// Call an async function, returning a Promise
    ///
    /// The promise can be awaited to get the result, or polled manually.
    ///
    /// For async functions compiled from Zyntax source, this automatically uses
    /// the state machine ABI:
    /// - `{fn}_new(params...) -> *mut StateMachine` - constructor
    /// - `{fn}_poll(state_machine, context) -> AsyncPollResult` - poll function
    ///
    /// # Example
    ///
    /// ```ignore
    /// let promise = runtime.call_async("fetch_data", &[url.into()])?;
    /// let result: String = promise.await_result()?;
    /// ```
    pub fn call_async(&self, name: &str, args: &[ZyntaxValue]) -> RuntimeResult<ZyntaxPromise> {
        // First, try the new Promise-based ABI:
        // The async function directly returns *Promise<T>
        if let Some(func_ptr) = self.get_function_ptr(name) {
            // Look up the stored signature for this function
            let signature = self.function_signatures.get(name)
                .cloned()
                .unwrap_or_else(|| {
                    // Fallback: infer signature from args (legacy behavior)
                    let params: Vec<NativeType> = args.iter()
                        .map(|arg| match arg {
                            ZyntaxValue::Int(_) => NativeType::I64,
                            ZyntaxValue::Float(_) => NativeType::F64,
                            ZyntaxValue::Bool(_) => NativeType::Bool,
                            ZyntaxValue::String(_) => NativeType::Ptr,
                            ZyntaxValue::Null => NativeType::Ptr,
                            ZyntaxValue::Pointer(_) => NativeType::Ptr,
                            _ => NativeType::Ptr, // All other types (Array, Struct, Map, etc.)
                        })
                        .collect();
                    NativeSignature { params, ret: NativeType::Ptr }
                });

            let dynamic_args: Vec<DynamicValue> = args.iter()
                .cloned()
                .map(|v| v.into_dynamic())
                .collect();

            return Ok(ZyntaxPromise::from_async_call(func_ptr, dynamic_args, &signature));
        }

        // Fall back to legacy _new/_poll naming convention
        if self.async_functions.contains(name) {
            let constructor_name = format!("{}_new", name);
            let poll_name = format!("{}_poll", name);

            let init_ptr = self.get_function_ptr(&constructor_name)
                .ok_or_else(|| RuntimeError::FunctionNotFound(format!(
                    "Async constructor '{}' not found (for async function '{}')",
                    constructor_name, name
                )))?;

            let poll_ptr = self.get_function_ptr(&poll_name)
                .ok_or_else(|| RuntimeError::FunctionNotFound(format!(
                    "Async poll function '{}' not found (for async function '{}')",
                    poll_name, name
                )))?;

            let dynamic_args: Vec<DynamicValue> = args.iter()
                .cloned()
                .map(|v| v.into_dynamic())
                .collect();

            return Ok(ZyntaxPromise::with_poll_fn(init_ptr, poll_ptr, dynamic_args));
        }

        Err(RuntimeError::FunctionNotFound(format!(
            "Async function '{}' not found (tried both new Promise ABI and legacy _new/_poll)", name
        )))
    }

    /// Register an external function that can be called from Zyntax code
    pub fn register_function(&mut self, name: &str, ptr: *const u8, arg_count: usize) {
        self.external_functions.insert(name.to_string(), ExternalFunction {
            name: name.to_string(),
            ptr,
            arg_count,
        });
        // Also register with the backend so Cranelift can resolve the symbol during JIT linking
        self.backend.register_runtime_symbol(name, ptr);
    }

    /// Hot-reload a function with new code
    pub fn hot_reload(&mut self, name: &str, function: &zyntax_compiler::HirFunction) -> RuntimeResult<()> {
        let id = self.function_ids.get(name)
            .ok_or_else(|| RuntimeError::FunctionNotFound(name.to_string()))?;

        self.backend.hot_reload_function(*id, function)?;
        Ok(())
    }

    /// Get the compilation configuration
    pub fn config(&self) -> &CompilationConfig {
        &self.config
    }

    /// Get a mutable reference to the compilation configuration
    pub fn config_mut(&mut self) -> &mut CompilationConfig {
        &mut self.config
    }

    /// Load a ZRTL plugin from a file path
    ///
    /// This loads a native dynamic library (.zrtl, .so, .dylib, .dll) and
    /// registers all its exported symbols as external functions.
    ///
    /// # Example
    ///
    /// ```ignore
    /// runtime.load_plugin("./my_runtime.zrtl")?;
    /// ```
    pub fn load_plugin<P: AsRef<std::path::Path>>(&mut self, path: P) -> RuntimeResult<()> {
        use zyntax_compiler::zrtl::{ZrtlPlugin, ZrtlError};

        let plugin = ZrtlPlugin::load(path).map_err(|e| RuntimeError::Execution(e.to_string()))?;

        // Register all symbols from the plugin as runtime symbols
        // AND collect their signatures for type checking
        for symbol_info in plugin.symbols_with_signatures() {
            self.register_function(symbol_info.name, symbol_info.ptr, 0); // Arity unknown without type info

            // Store signature if available
            if let Some(sig) = symbol_info.sig {
                self.plugin_signatures.insert(symbol_info.name.to_string(), sig);
            }
        }

        // Register symbol signatures for auto-boxing support in backend
        self.backend.register_symbol_signatures(plugin.symbols_with_signatures());

        // Rebuild the JIT module to include the new symbols
        self.backend.rebuild_with_accumulated_symbols()?;

        Ok(())
    }

    /// Load all ZRTL plugins from a directory
    ///
    /// Loads all `.zrtl` files from the specified directory.
    ///
    /// # Returns
    ///
    /// The number of plugins loaded successfully.
    pub fn load_plugins_from_directory<P: AsRef<std::path::Path>>(&mut self, dir: P) -> RuntimeResult<usize> {
        use zyntax_compiler::zrtl::ZrtlRegistry;

        let mut registry = ZrtlRegistry::new();
        let count = registry.load_directory(&dir)
            .map_err(|e| RuntimeError::Execution(e.to_string()))?;

        // Register all collected symbols AND their signatures
        for symbol_info in registry.collect_symbols_with_signatures() {
            self.register_function(symbol_info.name, symbol_info.ptr, 0);

            // Store signature if available
            if let Some(sig) = symbol_info.sig {
                self.plugin_signatures.insert(symbol_info.name.to_string(), sig);
            }
        }

        // Register symbol signatures for auto-boxing support in backend
        let symbols_with_sigs = registry.collect_symbols_with_signatures();
        self.backend.register_symbol_signatures(&symbols_with_sigs);

        // Rebuild the JIT module to include all the new symbols
        self.backend.rebuild_with_accumulated_symbols()?;

        Ok(count)
    }

    /// Register an import resolver callback
    ///
    /// Import resolvers are called in order when resolving import statements.
    /// The first resolver to return `Ok(Some(source))` wins.
    ///
    /// # Example
    ///
    /// ```ignore
    /// runtime.add_import_resolver(Box::new(|path| {
    ///     if path == "my_module" {
    ///         Ok(Some("pub fn hello() -> i32 { 42 }".to_string()))
    ///     } else {
    ///         Ok(None) // Not found by this resolver
    ///     }
    /// }));
    /// ```
    pub fn add_import_resolver(&mut self, resolver: ImportResolverCallback) {
        self.import_resolvers.push(resolver);
    }

    /// Add a file-system based import resolver
    ///
    /// This resolver looks for modules in the specified directory using the given file extension.
    /// For import path "foo.bar" with extension "zig", it looks for:
    /// - `{base_path}/foo/bar.zig`
    /// - `{base_path}/foo.bar.zig` (dot-style path)
    ///
    /// # Arguments
    /// * `base_path` - The base directory to search for modules
    /// * `extension` - The file extension (without the dot), e.g., "zig", "hx", "py"
    ///
    /// # Example
    /// ```ignore
    /// // For Zig source files
    /// runtime.add_filesystem_resolver("./src", "zig");
    ///
    /// // For Haxe source files
    /// runtime.add_filesystem_resolver("./src", "hx");
    /// ```
    pub fn add_filesystem_resolver<P: AsRef<std::path::Path> + Send + Sync + 'static>(
        &mut self,
        base_path: P,
        extension: &str,
    ) {
        let base = base_path.as_ref().to_path_buf();
        let ext = extension.to_string();

        self.add_import_resolver(Box::new(move |module_path| {
            // Try slash-separated path (e.g., "foo.bar" -> "foo/bar.zig")
            let slash_path = module_path.replace('.', "/");
            let file_path = base.join(format!("{}.{}", slash_path, ext));
            if file_path.exists() {
                return std::fs::read_to_string(&file_path)
                    .map(Some)
                    .map_err(|e| format!("Failed to read {}: {}", file_path.display(), e));
            }

            // Try dot-style path directly (e.g., "foo.bar" -> "foo.bar.zig")
            let dot_path = base.join(format!("{}.{}", module_path, ext));
            if dot_path.exists() {
                return std::fs::read_to_string(&dot_path)
                    .map(Some)
                    .map_err(|e| format!("Failed to read {}: {}", dot_path.display(), e));
            }

            Ok(None) // Not found
        }));
    }

    /// Resolve an import path using registered resolvers
    ///
    /// Returns the source code for the module if found.
    pub fn resolve_import(&self, module_path: &str) -> Result<Option<String>, String> {
        for resolver in &self.import_resolvers {
            match resolver(module_path) {
                Ok(Some(source)) => return Ok(Some(source)),
                Ok(None) => continue, // Try next resolver
                Err(e) => return Err(e),
            }
        }
        Ok(None) // Not found by any resolver
    }

    /// Get the number of registered import resolvers
    pub fn import_resolver_count(&self) -> usize {
        self.import_resolvers.len()
    }

    // ========================================================================
    // Multi-Language Grammar Registry
    // ========================================================================

    /// Register a language grammar with the runtime
    ///
    /// The language identifier is used to select the grammar when loading modules.
    /// File extensions from the grammar's metadata are automatically registered
    /// for extension-based language detection.
    ///
    /// # Arguments
    /// * `language` - The language identifier (e.g., "zig", "python", "haxe")
    /// * `grammar` - The compiled language grammar
    ///
    /// # Example
    ///
    /// ```ignore
    /// use zyntax_embed::{ZyntaxRuntime, LanguageGrammar};
    ///
    /// let mut runtime = ZyntaxRuntime::new()?;
    /// runtime.register_grammar("zig", LanguageGrammar::compile_zyn_file("zig.zyn")?);
    /// runtime.register_grammar("python", LanguageGrammar::compile_zyn_file("python.zyn")?);
    ///
    /// // Now load modules by language
    /// runtime.load_module("zig", "pub fn add(a: i32, b: i32) i32 { return a + b; }")?;
    /// ```
    pub fn register_grammar(&mut self, language: &str, grammar: LanguageGrammar) {
        // Register builtin aliases from the grammar
        // This maps DSL-friendly names (e.g., "image_load") to plugin symbols (e.g., "$Image$load")
        for (alias, target) in &grammar.builtins().functions {
            // If the target symbol is registered, create an alias for it
            if let Some(ext_func) = self.external_functions.get(target).cloned() {
                self.external_functions.insert(alias.clone(), ext_func);
            } else {
                log::debug!(
                    "Grammar builtin '{}' -> '{}' not found in external functions (yet)",
                    alias, target
                );
            }
        }

        let grammar = Arc::new(grammar);

        // Register file extensions from grammar metadata
        for ext in grammar.file_extensions() {
            let ext_key = if ext.starts_with('.') {
                ext.clone()
            } else {
                format!(".{}", ext)
            };
            self.extension_map.insert(ext_key, language.to_string());
        }

        self.grammars.insert(language.to_string(), grammar);
    }

    /// Register a grammar from a .zyn file
    ///
    /// Convenience method that compiles the grammar and registers it.
    ///
    /// # Example
    ///
    /// ```ignore
    /// runtime.register_grammar_file("zig", "grammars/zig.zyn")?;
    /// ```
    pub fn register_grammar_file<P: AsRef<Path>>(
        &mut self,
        language: &str,
        zyn_path: P,
    ) -> Result<(), GrammarError> {
        let grammar = LanguageGrammar::compile_zyn_file(zyn_path)?;
        self.register_grammar(language, grammar);
        Ok(())
    }

    /// Register a grammar from a pre-compiled .zpeg file
    ///
    /// # Example
    ///
    /// ```ignore
    /// runtime.register_grammar_zpeg("zig", "grammars/zig.zpeg")?;
    /// ```
    pub fn register_grammar_zpeg<P: AsRef<Path>>(
        &mut self,
        language: &str,
        zpeg_path: P,
    ) -> Result<(), GrammarError> {
        let grammar = LanguageGrammar::load(zpeg_path)?;
        self.register_grammar(language, grammar);
        Ok(())
    }

    /// Get a registered grammar by language name
    pub fn get_grammar(&self, language: &str) -> Option<&Arc<LanguageGrammar>> {
        self.grammars.get(language)
    }

    /// Get the language name for a file extension
    ///
    /// # Arguments
    /// * `extension` - The file extension (with or without leading dot)
    pub fn language_for_extension(&self, extension: &str) -> Option<&str> {
        let ext_key = if extension.starts_with('.') {
            extension.to_string()
        } else {
            format!(".{}", extension)
        };
        self.extension_map.get(&ext_key).map(|s| s.as_str())
    }

    /// List all registered language names
    pub fn languages(&self) -> Vec<&str> {
        self.grammars.keys().map(|s| s.as_str()).collect()
    }

    /// Check if a language grammar is registered
    pub fn has_language(&self, language: &str) -> bool {
        self.grammars.contains_key(language)
    }

    /// Load a module from source code using a registered language grammar
    ///
    /// This parses the source code using the registered grammar for the language,
    /// lowers it to HIR, and compiles it into the runtime.
    ///
    /// Note: Functions are NOT automatically exported for cross-module linking.
    /// Use `load_module_with_exports` to specify which functions to export.
    ///
    /// # Arguments
    /// * `language` - The language identifier (must be previously registered)
    /// * `source` - The source code to compile
    ///
    /// # Returns
    /// The names of functions defined in the module
    ///
    /// # Example
    ///
    /// ```ignore
    /// use zyntax_embed::{ZyntaxRuntime, LanguageGrammar};
    ///
    /// let mut runtime = ZyntaxRuntime::new()?;
    /// runtime.register_grammar("zig", LanguageGrammar::compile_zyn_file("zig.zyn")?);
    ///
    /// let functions = runtime.load_module("zig", r#"
    ///     pub fn add(a: i32, b: i32) i32 { return a + b; }
    ///     pub fn mul(a: i32, b: i32) i32 { return a * b; }
    /// "#)?;
    ///
    /// assert!(functions.contains(&"add".to_string()));
    /// let result: i32 = runtime.call("add", &[10.into(), 32.into()])?;
    /// ```
    pub fn load_module(&mut self, language: &str, source: &str) -> RuntimeResult<Vec<String>> {
        self.load_module_with_exports_and_filename(language, source, &[], None)
    }

    /// Load a module and export specified functions for cross-module linking
    ///
    /// Functions listed in `exports` will be made available as extern symbols
    /// for subsequent modules to call. A warning is printed if there's a name conflict.
    ///
    /// # Arguments
    /// * `language` - The language identifier (must be previously registered)
    /// * `source` - The source code to compile
    /// * `exports` - Names of functions to export for cross-module linking
    ///
    /// # Returns
    /// The names of functions defined in the module
    ///
    /// # Example
    ///
    /// ```ignore
    /// // Module A exports 'add'
    /// runtime.load_module_with_exports("zig", r#"
    ///     pub fn add(a: i32, b: i32) i32 { return a + b; }
    /// "#, &["add"])?;
    ///
    /// // Module B can call 'add' via extern
    /// runtime.load_module("zig", r#"
    ///     extern fn add(a: i32, b: i32) i32;
    ///     pub fn double_add(a: i32, b: i32) i32 { return add(a, b) + add(a, b); }
    /// "#)?;
    /// ```
    pub fn load_module_with_exports(
        &mut self,
        language: &str,
        source: &str,
        exports: &[&str],
    ) -> RuntimeResult<Vec<String>> {
        self.load_module_with_exports_and_filename(language, source, exports, None)
    }

    /// Load a module with exports and a specific filename for diagnostics
    pub fn load_module_with_exports_and_filename(
        &mut self,
        language: &str,
        source: &str,
        exports: &[&str],
        filename: Option<&str>,
    ) -> RuntimeResult<Vec<String>> {
        let grammar = self
            .grammars
            .get(language)
            .cloned()
            .ok_or_else(|| RuntimeError::Execution(format!(
                "Unknown language '{}'. Registered languages: {:?}",
                language,
                self.languages()
            )))?;

        // Parse source to TypedAST with plugin signatures for proper extern declarations
        let typed_program = if let Some(fname) = filename {
            grammar
                .parse_with_signatures(source, fname, &self.plugin_signatures)
                .map_err(|e| RuntimeError::Execution(e.to_string()))?
        } else {
            grammar
                .parse_with_signatures(source, "module.zynml", &self.plugin_signatures)
                .map_err(|e| RuntimeError::Execution(e.to_string()))?
        };

        // Lower to HIR
        let hir_module = self.lower_typed_program(typed_program)?;

        // Collect function names before compilation
        // Use resolve_global() to get the actual string from InternedString
        let function_names: Vec<String> = hir_module
            .functions
            .values()
            .filter(|f| !f.is_external)
            .filter_map(|f| f.name.resolve_global())
            .collect();

        // Compile the module
        self.compile_module(&hir_module)?;

        // Export specified functions
        for export_name in exports {
            self.export_function(export_name)?;
        }

        Ok(function_names)
    }

    /// Export a compiled function for cross-module linking
    ///
    /// Makes the function available as an extern symbol for subsequent modules.
    /// Returns an error if the function doesn't exist or if there's a symbol conflict.
    ///
    /// # Arguments
    /// * `name` - The function name to export
    pub fn export_function(&mut self, name: &str) -> RuntimeResult<()> {
        // Get the function pointer from our function_ids map
        let ptr = self.get_function_ptr(name)
            .ok_or_else(|| RuntimeError::FunctionNotFound(name.to_string()))?;

        // Check for conflict and warn
        if let Some(existing) = self.backend.check_export_conflict(name) {
            log::warn!(
                "Symbol conflict: '{}' is already exported at {:?}. Overwriting.",
                name, existing
            );
            // Use overwrite method to replace
            self.backend.export_function_ptr_overwrite(name, ptr);
        } else {
            // No conflict, use regular export
            self.backend.export_function_ptr(name, ptr)
                .map_err(|e| RuntimeError::Execution(e.to_string()))?;
        }

        Ok(())
    }

    /// Check if exporting a function would cause a symbol conflict
    ///
    /// Returns Some with the existing pointer if a conflict exists.
    pub fn check_export_conflict(&self, name: &str) -> Option<*const u8> {
        self.backend.check_export_conflict(name)
    }

    /// Get all currently exported symbols
    pub fn exported_symbols(&self) -> Vec<(&str, *const u8)> {
        self.backend.exported_symbols()
    }

    /// Load a module from a file, auto-detecting the language from extension
    ///
    /// The file extension is used to look up the registered grammar.
    ///
    /// # Example
    ///
    /// ```ignore
    /// // Automatically uses "zig" grammar based on .zig extension
    /// runtime.load_module_file("./src/math.zig")?;
    /// ```
    pub fn load_module_file<P: AsRef<Path>>(&mut self, path: P) -> RuntimeResult<Vec<String>> {
        let path = path.as_ref();

        // Get the file extension
        let extension = path
            .extension()
            .and_then(|e| e.to_str())
            .ok_or_else(|| RuntimeError::Execution(format!(
                "File '{}' has no extension",
                path.display()
            )))?;

        // Look up the language for this extension
        let language = self
            .language_for_extension(extension)
            .ok_or_else(|| RuntimeError::Execution(format!(
                "No grammar registered for extension '.{}'. Registered extensions: {:?}",
                extension,
                self.extension_map.keys().collect::<Vec<_>>()
            )))?
            .to_string();

        // Read the source file
        let source = std::fs::read_to_string(path)
            .map_err(|e| RuntimeError::Execution(format!(
                "Failed to read '{}': {}",
                path.display(),
                e
            )))?;

        // Use the file path as the filename for diagnostics
        let filename = path.to_string_lossy();
        self.load_module_with_exports_and_filename(&language, &source, &[], Some(&filename))
    }

    /// List all loaded function names
    pub fn functions(&self) -> Vec<&str> {
        self.function_ids.keys().map(|s| s.as_str()).collect()
    }

    /// Check if a function is defined
    pub fn has_function(&self, name: &str) -> bool {
        self.function_ids.contains_key(name)
    }

    /// Get a reference to the plugin signatures
    ///
    /// This is useful for Grammar2 parsers that need to inject extern declarations
    /// for builtin functions with proper type signatures.
    pub fn plugin_signatures(&self) -> &HashMap<String, zyntax_compiler::zrtl::ZrtlSymbolSig> {
        &self.plugin_signatures
    }

    /// Compile a TypedProgram directly (without parsing)
    ///
    /// This is useful when using Grammar2 to parse source code directly to TypedAST,
    /// bypassing the traditional grammar.parse() path.
    ///
    /// # Returns
    ///
    /// The names of functions defined in the module.
    pub fn compile_typed_program(&mut self, program: zyntax_typed_ast::TypedProgram) -> RuntimeResult<Vec<String>> {
        // Lower to HIR
        let hir_module = self.lower_typed_program(program)?;

        // Collect function names before compilation
        let function_names: Vec<String> = hir_module
            .functions
            .values()
            .filter(|f| !f.is_external)
            .filter_map(|f| f.name.resolve_global())
            .collect();

        // Compile the module
        self.compile_module(&hir_module)?;

        Ok(function_names)
    }
}

// ============================================================================
// Tiered JIT Runtime
// ============================================================================

/// A multi-tier JIT runtime with automatic optimization
///
/// `TieredRuntime` provides adaptive compilation where frequently-called
/// functions are automatically optimized to higher tiers:
///
/// - **Tier 0 (Baseline)**: Fast compilation, minimal optimization (cold code)
/// - **Tier 1 (Standard)**: Moderate optimization (warm code)
/// - **Tier 2 (Optimized)**: Aggressive optimization (hot code)
///
/// ## How It Works
///
/// 1. All functions start at Tier 0 (baseline JIT with Cranelift)
/// 2. Execution counters track how often functions are called
/// 3. When a function crosses the "warm" threshold, it's recompiled at Tier 1
/// 4. When it crosses the "hot" threshold, it's recompiled at Tier 2
/// 5. Function pointers are atomically swapped after recompilation
///
/// ## Example
///
/// ```ignore
/// use zyntax_embed::{TieredRuntime, TieredConfig};
///
/// // Development: Fast startup, no background optimization
/// let mut runtime = TieredRuntime::development()?;
///
/// // Production: Full tiered optimization with background worker
/// let mut runtime = TieredRuntime::production()?;
///
/// // Production with LLVM for Tier 2 (requires llvm-backend feature)
/// let mut runtime = TieredRuntime::production_llvm()?;
/// ```
pub struct TieredRuntime {
    /// The tiered JIT backend
    backend: TieredBackend,
    /// Mapping from function names to HIR IDs
    function_ids: HashMap<String, HirId>,
    /// Function signatures for native calling
    function_signatures: HashMap<String, NativeSignature>,
    /// Tiered configuration
    config: TieredConfig,
    /// Registered language grammars (language name -> grammar)
    grammars: HashMap<String, Arc<LanguageGrammar>>,
    /// File extension to language mapping (e.g., ".zig" -> "zig")
    extension_map: HashMap<String, String>,
    /// Plugin signatures (symbol name -> ZRTL signature)
    /// Collected from loaded plugins for proper extern function type checking
    plugin_signatures: HashMap<String, zyntax_compiler::zrtl::ZrtlSymbolSig>,
}

impl TieredRuntime {
    /// Create a tiered runtime with the given configuration
    pub fn new(config: TieredConfig) -> RuntimeResult<Self> {
        let backend = TieredBackend::new(config.clone())?;

        Ok(Self {
            backend,
            function_ids: HashMap::new(),
            function_signatures: HashMap::new(),
            config,
            grammars: HashMap::new(),
            extension_map: HashMap::new(),
            plugin_signatures: HashMap::new(),
        })
    }

    /// Create a runtime optimized for development
    ///
    /// - Fast compilation with minimal optimization
    /// - No background optimization worker
    /// - Good for rapid iteration and debugging
    pub fn development() -> RuntimeResult<Self> {
        Self::new(TieredConfig::development())
    }

    /// Create a runtime optimized for production
    ///
    /// - Full tiered optimization with Cranelift
    /// - Background optimization worker enabled
    /// - Automatic promotion of hot functions
    pub fn production() -> RuntimeResult<Self> {
        Self::new(TieredConfig::production())
    }

    /// Create a runtime with LLVM for maximum Tier 2 optimization
    ///
    /// - Uses LLVM MCJIT for hot-path optimization
    /// - Best performance for compute-intensive workloads
    /// - Requires the `llvm-backend` feature
    #[cfg(feature = "llvm-backend")]
    pub fn production_llvm() -> RuntimeResult<Self> {
        Self::new(TieredConfig::production_llvm())
    }

    /// Compile a HIR module into the tiered runtime
    pub fn compile_module(&mut self, module: HirModule) -> RuntimeResult<()> {
        // Store function name -> ID mapping and signatures (resolve InternedString to actual string)
        for (id, func) in &module.functions {
            if let Some(name) = func.name.resolve_global() {
                self.function_ids.insert(name.clone(), *id);

                // Store the function signature for later use in call/call_async
                let native_sig = NativeSignature::from_hir_signature(&func.signature);
                self.function_signatures.insert(name, native_sig);
            }
        }

        // Compile the module (consumes it)
        self.backend.compile_module(module)?;

        Ok(())
    }

    /// Get a function pointer by name
    pub fn get_function_ptr(&self, name: &str) -> Option<*const u8> {
        self.function_ids.get(name)
            .and_then(|id| self.backend.get_function_pointer(*id))
    }

    /// Call a function by name with automatic type conversion
    ///
    /// This also records the call for profiling, which may trigger
    /// automatic optimization if the function becomes hot.
    pub fn call<T: FromZyntax>(&self, name: &str, args: &[ZyntaxValue]) -> RuntimeResult<T> {
        let result = self.call_raw(name, args)?;
        T::from_zyntax(result).map_err(RuntimeError::from)
    }

    /// Call a function and get the raw ZyntaxValue result
    pub fn call_raw(&self, name: &str, args: &[ZyntaxValue]) -> RuntimeResult<ZyntaxValue> {
        let func_id = self.function_ids.get(name)
            .ok_or_else(|| RuntimeError::FunctionNotFound(name.to_string()))?;

        // Record the call for profiling
        self.backend.record_call(*func_id);

        let ptr = self.backend.get_function_pointer(*func_id)
            .ok_or_else(|| RuntimeError::FunctionNotFound(name.to_string()))?;

        // Check if this is a void function using the stored signature
        if let Some(sig) = self.function_signatures.get(name) {
            if sig.ret == NativeType::Void {
                // For void functions, use the native calling convention
                // SAFETY: We trust the function signature is correct
                unsafe {
                    call_native_with_signature(ptr, args, sig)?;
                }
                return Ok(ZyntaxValue::Void);
            }
        }

        // Convert arguments to DynamicValues
        let dynamic_args: Vec<DynamicValue> = args.iter()
            .cloned()
            .map(|v| v.into_dynamic())
            .collect();

        // Call the function using the variadic caller helper
        // SAFETY: We trust the caller has provided the correct function pointer
        // and matching arguments. from_dynamic is safe because call_dynamic_function
        // returns a valid DynamicValue.
        unsafe {
            let result = call_dynamic_function(ptr, &dynamic_args)?;
            ZyntaxValue::from_dynamic(result).map_err(RuntimeError::from)
        }
    }

    /// Call an async function, returning a Promise
    ///
    /// With the new Promise-based async ABI:
    /// - Calling `double(21)` returns a Promise struct `{state_machine_ptr, poll_fn_ptr}`
    /// - The Promise contains everything needed to poll for completion
    /// - No `_new`/`_poll` naming convention needed
    pub fn call_async(&self, name: &str, args: &[ZyntaxValue]) -> RuntimeResult<ZyntaxPromise> {
        // First, try the new Promise-returning API
        // The async function directly returns Promise<T> = {state_machine_ptr, poll_fn_ptr}
        if let Some(func_id) = self.function_ids.get(name) {
            self.backend.record_call(*func_id);
            let func_ptr = self.backend.get_function_pointer(*func_id)
                .ok_or_else(|| RuntimeError::FunctionNotFound(name.to_string()))?;

            // Look up the stored signature for this function
            let signature = self.function_signatures.get(name)
                .cloned()
                .unwrap_or_else(|| {
                    // Fallback: infer signature from args (legacy behavior)
                    let params: Vec<NativeType> = args.iter()
                        .map(|arg| match arg {
                            ZyntaxValue::Int(_) => NativeType::I64,
                            ZyntaxValue::Float(_) => NativeType::F64,
                            ZyntaxValue::Bool(_) => NativeType::Bool,
                            ZyntaxValue::String(_) => NativeType::Ptr,
                            ZyntaxValue::Null => NativeType::Ptr,
                            ZyntaxValue::Pointer(_) => NativeType::Ptr,
                            _ => NativeType::Ptr, // All other types (Array, Struct, Map, etc.)
                        })
                        .collect();
                    NativeSignature { params, ret: NativeType::Ptr }
                });

            let dynamic_args: Vec<DynamicValue> = args.iter()
                .cloned()
                .map(|v| v.into_dynamic())
                .collect();

            // Call the function - it returns a Promise struct
            return Ok(ZyntaxPromise::from_async_call(func_ptr, dynamic_args, &signature));
        }

        // Fall back to legacy _new/_poll naming convention for backwards compatibility
        let new_name = format!("{}_new", name);
        let poll_name = format!("{}_poll", name);

        if let (Some(new_id), Some(poll_id)) =
            (self.function_ids.get(&new_name), self.function_ids.get(&poll_name))
        {
            self.backend.record_call(*new_id);
            self.backend.record_call(*poll_id);

            let new_ptr = self.backend.get_function_pointer(*new_id)
                .ok_or_else(|| RuntimeError::FunctionNotFound(new_name.clone()))?;
            let poll_ptr = self.backend.get_function_pointer(*poll_id)
                .ok_or_else(|| RuntimeError::FunctionNotFound(poll_name.clone()))?;

            let dynamic_args: Vec<DynamicValue> = args.iter()
                .cloned()
                .map(|v| v.into_dynamic())
                .collect();

            return Ok(ZyntaxPromise::with_poll_fn(new_ptr, poll_ptr, dynamic_args));
        }

        Err(RuntimeError::FunctionNotFound(format!(
            "Async function '{}' not found (tried both Promise-returning and legacy _new/_poll APIs)", name
        )))
    }

    /// Manually optimize a function to a specific tier
    ///
    /// Useful for pre-warming hot paths or testing.
    pub fn optimize_function(&mut self, name: &str, tier: OptimizationTier) -> RuntimeResult<()> {
        let func_id = self.function_ids.get(name)
            .ok_or_else(|| RuntimeError::FunctionNotFound(name.to_string()))?;

        self.backend.optimize_function(*func_id, tier)?;
        Ok(())
    }

    /// Get optimization statistics
    pub fn statistics(&self) -> TieredStatistics {
        self.backend.get_statistics()
    }

    /// Get the current optimization tier for a function
    pub fn function_tier(&self, name: &str) -> Option<OptimizationTier> {
        // Implementation would query the backend's function_tiers map
        // For now, return None as this requires backend API access
        let _ = name;
        None
    }

    /// Get the tiered configuration
    pub fn config(&self) -> &TieredConfig {
        &self.config
    }

    /// Shutdown the runtime (stops background optimization)
    pub fn shutdown(&mut self) {
        self.backend.shutdown();
    }

    /// Load a ZRTL plugin from a file path
    ///
    /// This loads a native dynamic library (.zrtl, .so, .dylib, .dll) and
    /// registers all its exported symbols as external functions.
    ///
    /// # Example
    ///
    /// ```ignore
    /// runtime.load_plugin("./my_runtime.zrtl")?;
    /// ```
    pub fn load_plugin<P: AsRef<std::path::Path>>(&mut self, path: P) -> RuntimeResult<()> {
        use zyntax_compiler::zrtl::ZrtlPlugin;

        let plugin = ZrtlPlugin::load(path).map_err(|e| RuntimeError::Execution(e.to_string()))?;

        // Register all symbols from the plugin as runtime symbols
        // AND collect their signatures for type checking
        for symbol_info in plugin.symbols_with_signatures() {
            self.backend.register_runtime_symbol(symbol_info.name, symbol_info.ptr);

            // Store signature if available
            if let Some(sig) = symbol_info.sig {
                self.plugin_signatures.insert(symbol_info.name.to_string(), sig);
            }
        }

        Ok(())
    }

    /// Load all ZRTL plugins from a directory
    ///
    /// Loads all `.zrtl` files from the specified directory.
    ///
    /// # Returns
    ///
    /// The number of plugins loaded successfully.
    pub fn load_plugins_from_directory<P: AsRef<std::path::Path>>(&mut self, dir: P) -> RuntimeResult<usize> {
        use zyntax_compiler::zrtl::ZrtlRegistry;

        let mut registry = ZrtlRegistry::new();
        let count = registry.load_directory(&dir)
            .map_err(|e| RuntimeError::Execution(e.to_string()))?;

        // Register all collected symbols AND their signatures
        for symbol_info in registry.collect_symbols_with_signatures() {
            self.backend.register_runtime_symbol(symbol_info.name, symbol_info.ptr);

            // Store signature if available
            if let Some(sig) = symbol_info.sig {
                self.plugin_signatures.insert(symbol_info.name.to_string(), sig);
            }
        }

        Ok(count)
    }

    /// Get plugin signatures for all loaded plugins
    ///
    /// Returns a reference to the mapping of symbol names to ZRTL signatures.
    /// This can be used during parsing to create properly typed extern function declarations.
    ///
    /// # Returns
    ///
    /// A HashMap mapping symbol names (e.g., "$IO$println_dynamic") to their ZRTL signatures.
    pub fn plugin_signatures(&self) -> &HashMap<String, zyntax_compiler::zrtl::ZrtlSymbolSig> {
        &self.plugin_signatures
    }

    // ========================================================================
    // Multi-Language Grammar Registry
    // ========================================================================

    /// Register a language grammar with the runtime
    ///
    /// See `ZyntaxRuntime::register_grammar` for full documentation.
    pub fn register_grammar(&mut self, language: &str, grammar: LanguageGrammar) {
        // Note: Builtin aliases are resolved during parsing via ZynPEG's builtin resolution.
        // The @builtin section in the grammar maps DSL names (e.g., "image_load") to
        // runtime symbols (e.g., "$Image$load"). This resolution happens in the parser,
        // not here in the runtime.
        let grammar = Arc::new(grammar);

        // Register file extensions from grammar metadata
        for ext in grammar.file_extensions() {
            let ext_key = if ext.starts_with('.') {
                ext.clone()
            } else {
                format!(".{}", ext)
            };
            self.extension_map.insert(ext_key, language.to_string());
        }

        self.grammars.insert(language.to_string(), grammar);
    }

    /// Register a grammar from a .zyn file
    pub fn register_grammar_file<P: AsRef<Path>>(
        &mut self,
        language: &str,
        zyn_path: P,
    ) -> Result<(), GrammarError> {
        let grammar = LanguageGrammar::compile_zyn_file(zyn_path)?;
        self.register_grammar(language, grammar);
        Ok(())
    }

    /// Register a grammar from a pre-compiled .zpeg file
    pub fn register_grammar_zpeg<P: AsRef<Path>>(
        &mut self,
        language: &str,
        zpeg_path: P,
    ) -> Result<(), GrammarError> {
        let grammar = LanguageGrammar::load(zpeg_path)?;
        self.register_grammar(language, grammar);
        Ok(())
    }

    /// Get a registered grammar by language name
    pub fn get_grammar(&self, language: &str) -> Option<&Arc<LanguageGrammar>> {
        self.grammars.get(language)
    }

    /// Get the language name for a file extension
    pub fn language_for_extension(&self, extension: &str) -> Option<&str> {
        let ext_key = if extension.starts_with('.') {
            extension.to_string()
        } else {
            format!(".{}", extension)
        };
        self.extension_map.get(&ext_key).map(|s| s.as_str())
    }

    /// List all registered language names
    pub fn languages(&self) -> Vec<&str> {
        self.grammars.keys().map(|s| s.as_str()).collect()
    }

    /// Check if a language grammar is registered
    pub fn has_language(&self, language: &str) -> bool {
        self.grammars.contains_key(language)
    }

    /// Load a module from source code using a registered language grammar
    ///
    /// See `ZyntaxRuntime::load_module` for full documentation.
    pub fn load_module(&mut self, language: &str, source: &str) -> RuntimeResult<Vec<String>> {
        let grammar = self
            .grammars
            .get(language)
            .cloned()
            .ok_or_else(|| RuntimeError::Execution(format!(
                "Unknown language '{}'. Registered languages: {:?}",
                language,
                self.languages()
            )))?;

        // Parse source to TypedAST with plugin signatures for proper extern declarations
        let typed_program = grammar
            .parse_with_signatures(source, "module.zynml", &self.plugin_signatures)
            .map_err(|e| RuntimeError::Execution(e.to_string()))?;

        // Lower to HIR
        let hir_module = self.lower_typed_program(typed_program)?;

        // Collect function names before compilation
        let function_names: Vec<String> = hir_module
            .functions
            .values()
            .map(|f| f.name.to_string())
            .collect();

        // Compile the module
        self.compile_module(hir_module)?;

        Ok(function_names)
    }

    /// Load a module from a file, auto-detecting the language from extension
    pub fn load_module_file<P: AsRef<Path>>(&mut self, path: P) -> RuntimeResult<Vec<String>> {
        let path = path.as_ref();

        let extension = path
            .extension()
            .and_then(|e| e.to_str())
            .ok_or_else(|| RuntimeError::Execution(format!(
                "File '{}' has no extension",
                path.display()
            )))?;

        let language = self
            .language_for_extension(extension)
            .ok_or_else(|| RuntimeError::Execution(format!(
                "No grammar registered for extension '.{}'",
                extension
            )))?
            .to_string();

        let source = std::fs::read_to_string(path)
            .map_err(|e| RuntimeError::Execution(format!(
                "Failed to read '{}': {}",
                path.display(),
                e
            )))?;

        self.load_module(&language, &source)
    }

    /// Lower a TypedProgram to HirModule
    fn lower_typed_program(&self, mut program: zyntax_typed_ast::TypedProgram) -> RuntimeResult<HirModule> {
        use zyntax_compiler::lowering::{LoweringContext, LoweringConfig};
        use zyntax_typed_ast::{AstArena, InternedString, TypeRegistry, TypedDeclaration, type_registry::*};

        // Rebuild type registry from declarations (TypeRegistry is not serializable)
        // Scan for struct definitions (TypedDeclaration::Class) and register them
        for decl_node in &program.declarations {
            eprintln!("[DEBUG] Declaration node ty: {:?}", decl_node.ty);
            if let TypedDeclaration::Class(class) = &decl_node.node {
                eprintln!("[DEBUG] Found Class '{}' with type: {:?}", class.name, decl_node.ty);
                // Check if this is a struct (no methods, just fields)
                // Create TypeDefinition and register it
                if let zyntax_typed_ast::Type::Named { id, .. } = &decl_node.ty {
                    let field_defs: Vec<FieldDef> = class.fields.iter().map(|f| FieldDef {
                        name: f.name,
                        ty: f.ty.clone(),
                        visibility: f.visibility,
                        mutability: f.mutability,
                        is_static: f.is_static,
                        span: f.span,
                        getter: None,
                        setter: None,
                        is_synthetic: false,
                    }).collect();

                    let type_def = TypeDefinition {
                        id: *id,
                        name: class.name,
                        kind: TypeKind::Struct {
                            fields: field_defs.clone(),
                            is_tuple: false,
                        },
                        type_params: vec![],
                        constraints: vec![],
                        fields: field_defs,
                        methods: vec![],
                        constructors: vec![],
                        metadata: Default::default(),
                        span: class.span,
                    };
                    program.type_registry.register_type(type_def);
                }
            }
        }

        // Register impl blocks before lowering
        zyntax_compiler::register_impl_blocks(&mut program)
            .map_err(|e| RuntimeError::Execution(format!("Failed to register impl blocks: {:?}", e)))?;

        // Generate automatic trait implementations for abstract types
        zyntax_compiler::generate_abstract_trait_impls(&mut program)
            .map_err(|e| RuntimeError::Execution(format!("Failed to generate abstract trait impls: {:?}", e)))?;

        // Register the generated impl blocks
        zyntax_compiler::register_impl_blocks(&mut program)
            .map_err(|e| RuntimeError::Execution(format!("Failed to register generated impl blocks: {:?}", e)))?;

        let arena = AstArena::new();
        let module_name = InternedString::new_global("main");
        // Use the type registry from the parsed program (now contains registered structs)
        let type_registry = std::sync::Arc::new(program.type_registry.clone());

        let mut lowering_ctx = LoweringContext::new(
            module_name,
            type_registry.clone(),
            std::sync::Arc::new(std::sync::Mutex::new(arena)),
            LoweringConfig::default(),
        );

        let mut hir_module = lowering_ctx
            .lower_program(&mut program)
            .map_err(|e| RuntimeError::Execution(format!("Lowering error: {:?}", e)))?;

        zyntax_compiler::monomorphize_module(&mut hir_module)
            .map_err(|e| RuntimeError::Execution(format!("Monomorphization error: {:?}", e)))?;

        Ok(hir_module)
    }

    /// List all loaded function names
    pub fn functions(&self) -> Vec<&str> {
        self.function_ids.keys().map(|s| s.as_str()).collect()
    }

    /// Check if a function is defined
    pub fn has_function(&self, name: &str) -> bool {
        self.function_ids.contains_key(name)
    }
}

impl Drop for TieredRuntime {
    fn drop(&mut self) {
        self.shutdown();
    }
}

/// A promise representing an async operation
///
/// `ZyntaxPromise` wraps a Zyntax async function call and provides methods
/// to await or poll the result.
///
/// # States
///
/// - `Pending`: The operation is still in progress
/// - `Ready`: The operation completed successfully with a value
/// - `Failed`: The operation failed with an error
///
/// # Example
///
/// ```ignore
/// let promise = runtime.call_async("fetch", &[url.into()])?;
///
/// // Block until complete
/// let result: String = promise.await_result()?;
///
/// // Or poll manually
/// loop {
///     match promise.poll() {
///         PromiseState::Ready(value) => break,
///         PromiseState::Pending => std::thread::yield_now(),
///         PromiseState::Failed(err) => return Err(err),
///     }
/// }
/// ```
pub struct ZyntaxPromise {
    state: Arc<Mutex<PromiseInner>>,
}

/// Poll result from async state machine
///
/// This matches the Zyntax async ABI where poll functions return a discriminated union.
#[repr(C, u8)]
#[derive(Clone, Debug)]
pub enum AsyncPollResult {
    /// Still pending, needs more polls
    Pending = 0,
    /// Completed with a value (the DynamicValue)
    Ready(DynamicValue) = 1,
    /// Failed with an error message
    Failed(*const u8, usize) = 2, // (ptr, len) for error string
}

struct PromiseInner {
    /// Function pointer for creating the state machine
    init_fn: *const u8,
    /// Poll function pointer (once state machine is created)
    poll_fn: Option<*const u8>,
    /// Arguments to pass
    args: Vec<DynamicValue>,
    /// Current state
    state: PromiseState,
    /// State machine pointer (for Zyntax async functions)
    state_machine: Option<*mut u8>,
    /// Ready queue for waker integration
    ready_queue: Arc<Mutex<std::collections::VecDeque<usize>>>,
    /// Task ID for waker
    task_id: usize,
    /// Poll count for timeout detection
    poll_count: usize,
    /// Waker for Rust Future integration
    waker: Option<std::task::Waker>,
}

// SAFETY: Promise state is protected by mutex
unsafe impl Send for PromiseInner {}
unsafe impl Sync for PromiseInner {}

/// The state of a promise
#[derive(Debug, Clone)]
pub enum PromiseState {
    /// The operation is still in progress
    Pending,
    /// The operation completed with a value
    Ready(ZyntaxValue),
    /// The operation failed with an error
    Failed(String),
    /// The operation was cancelled
    Cancelled,
}

/// Global task ID counter for promise wakers
static NEXT_TASK_ID: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

impl ZyntaxPromise {
    /// Create a new promise for an async function call
    fn new(func_ptr: *const u8, args: Vec<DynamicValue>) -> Self {
        let task_id = NEXT_TASK_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        Self {
            state: Arc::new(Mutex::new(PromiseInner {
                init_fn: func_ptr,
                poll_fn: None,
                args,
                state: PromiseState::Pending,
                state_machine: None,
                ready_queue: Arc::new(Mutex::new(std::collections::VecDeque::new())),
                task_id,
                poll_count: 0,
                waker: None,
            })),
        }
    }

    /// Create a promise from a function that returns *Promise<T>
    ///
    /// The new Promise-based async ABI:
    /// - `async fn foo(x: i32) -> i32` compiles to `fn foo(x: i32) -> *Promise<i32>`
    /// - Promise is a struct on the stack: `{state_machine: *mut u8, poll_fn: fn(*mut u8) -> i64}`
    /// - Calling the function allocates state machine and returns pointer to Promise
    ///
    /// Uses the provided signature to properly invoke the function with the correct
    /// number and types of arguments.
    pub fn from_async_call(func_ptr: *const u8, args: Vec<DynamicValue>, signature: &NativeSignature) -> Self {
        let task_id = NEXT_TASK_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        eprintln!("[DEBUG] from_async_call: func_ptr={:?}, args.len()={}, signature.params={:?}, signature.ret={:?}",
            func_ptr, args.len(), signature.params, signature.ret);

        // Call the function to get the Promise pointer using signature-based dynamic dispatch
        // Promise layout at pointer: offset 0 = state_machine (8 bytes), offset 8 = poll_fn (8 bytes)
        let (state_machine, poll_fn) = unsafe {
            eprintln!("[DEBUG] from_async_call: about to call_with_signature");
            let promise_ptr: *const u8 = call_with_signature(func_ptr, &args, signature);
            eprintln!("[DEBUG] from_async_call: promise_ptr={:?}", promise_ptr);

            if promise_ptr.is_null() {
                (std::ptr::null_mut(), std::ptr::null())
            } else {
                // Read the Promise struct from the pointer
                // Promise layout: {state_machine: *mut u8, poll_fn: fn(*mut u8) -> i64}
                eprintln!("[DEBUG] from_async_call: reading state_machine at offset 0");
                let state_machine = *(promise_ptr as *const *mut u8);
                eprintln!("[DEBUG] from_async_call: state_machine={:?}", state_machine);
                eprintln!("[DEBUG] from_async_call: reading poll_fn at offset 8");
                let poll_fn = *((promise_ptr as *const u8).offset(8) as *const *const u8);
                eprintln!("[DEBUG] from_async_call: poll_fn={:?}", poll_fn);
                (state_machine, poll_fn)
            }
        };

        Self {
            state: Arc::new(Mutex::new(PromiseInner {
                init_fn: func_ptr, // Keep for reference
                poll_fn: if poll_fn.is_null() { None } else { Some(poll_fn) },
                args,
                state: PromiseState::Pending,
                state_machine: if state_machine.is_null() { None } else { Some(state_machine) },
                ready_queue: Arc::new(Mutex::new(std::collections::VecDeque::new())),
                task_id,
                poll_count: 0,
                waker: None,
            })),
        }
    }

    /// Create a new promise with separate constructor and poll functions
    ///
    /// This follows the legacy Zyntax async ABI where:
    /// - `init_fn`: `{fn}_new(params...) -> *mut StateMachine` - constructor
    /// - `poll_fn`: `async_wrapper(self: *StateMachine, cx: *Context) -> Poll` - poll function
    ///
    /// See `crates/compiler/src/async_support.rs` for the full async ABI.
    pub fn with_poll_fn(init_fn: *const u8, poll_fn: *const u8, args: Vec<DynamicValue>) -> Self {
        let task_id = NEXT_TASK_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        Self {
            state: Arc::new(Mutex::new(PromiseInner {
                init_fn,
                poll_fn: Some(poll_fn),
                args,
                state: PromiseState::Pending,
                state_machine: None,
                ready_queue: Arc::new(Mutex::new(std::collections::VecDeque::new())),
                task_id,
                poll_count: 0,
                waker: None,
            })),
        }
    }

    /// Set the poll function for this promise
    ///
    /// Call this before polling if the promise was created with just an init function.
    pub fn set_poll_fn(&self, poll_fn: *const u8) {
        let mut inner = self.state.lock().unwrap();
        inner.poll_fn = Some(poll_fn);
    }

    /// Poll the promise for completion
    ///
    /// Returns the current state without blocking.
    ///
    /// # Async ABI
    ///
    /// Zyntax async functions follow this ABI:
    /// 1. `init_fn(args...) -> *mut StateMachine` - Creates the state machine
    /// 2. `poll_fn(state_machine: *mut u8, waker_data: *const u8) -> AsyncPollResult`
    ///
    /// The poll function advances the state machine until it yields or completes.
    pub fn poll(&self) -> PromiseState {
        let mut inner = self.state.lock().unwrap();

        // If already complete or cancelled, return the state
        match &inner.state {
            PromiseState::Ready(_) | PromiseState::Failed(_) | PromiseState::Cancelled => {
                return inner.state.clone();
            }
            PromiseState::Pending => {}
        }

        inner.poll_count += 1;

        // Try to advance the state machine
        if let Some(state_machine) = inner.state_machine {
            if let Some(poll_fn) = inner.poll_fn {
                unsafe {
                    // Call the poll function on the state machine
                    // New ABI: poll(state_machine: *mut u8) -> i64
                    // Return value: 0 = Pending, positive = Ready(value), negative = Failed

                    // Debug: read state field (u32 at offset 0)
                    let state_field = *(state_machine as *const u32);
                    // Also read a few more fields to understand state machine layout
                    let field_4 = *((state_machine as *const u8).add(4) as *const i32); // n
                    let field_8 = *((state_machine as *const u8).add(8) as *const i32); // i
                    let field_12 = *((state_machine as *const u8).add(12) as *const i32); // total
                    let field_16 = *((state_machine as *const u8).add(16) as *const u64); // future ptr
                    eprintln!("[DEBUG] poll: state={}, n={}, i={}, total={}, future_ptr=0x{:x}",
                        state_field, field_4, field_8, field_12, field_16);

                    let f: extern "C" fn(*mut u8) -> i64 = std::mem::transmute(poll_fn);
                    let result = f(state_machine);

                    // Debug: read state field again after poll
                    let state_field_after = *(state_machine as *const u32);
                    eprintln!("[DEBUG] poll: result={}, state_field_after={}", result, state_field_after);

                    if result == 0 {
                        // Pending - state remains unchanged
                    } else if result < 0 {
                        // Negative value indicates failure
                        inner.state = PromiseState::Failed(format!("Async operation failed with code {}", result));
                    } else {
                        // Ready with value
                        // For i64/i32 returns, the value is in the result directly
                        inner.state = PromiseState::Ready(ZyntaxValue::Int(result));
                    }
                }
            } else {
                // No poll function available, mark as complete with void
                // This handles sync functions wrapped as async
                inner.state = PromiseState::Ready(ZyntaxValue::Void);
            }
        } else {
            // Initialize the state machine on first poll
            // Initialize state machine on first poll (legacy path)
            unsafe {
                // The init function creates the state machine and returns a pointer to it.
                // ABI: init_fn(args...) -> *mut StateMachine
                //
                // For Zyntax async functions, the constructor takes the same parameters
                // as the original async function and returns a state machine struct.

                if inner.init_fn.is_null() {
                    inner.state = PromiseState::Failed("Null async function pointer".to_string());
                    return inner.state.clone();
                }

                // Call the init function with the provided arguments
                // The state machine struct is returned by value (as a struct), not as a pointer
                // For Cranelift, structs are returned via pointer in the first hidden argument
                // We'll allocate space and pass the pointer
                let state_machine: *mut u8 = match inner.args.len() {
                    0 => {
                        // No arguments - allocate state machine on stack and call init()
                        // Allocate a fixed-size buffer for the state machine (state: u32 + local_x: i32 = 8 bytes)
                        let buffer = Box::into_raw(Box::new([0u8; 64])) as *mut u8;
                        let f: extern "C" fn(*mut u8) = std::mem::transmute(inner.init_fn);
                        f(buffer);
                        buffer
                    }
                    1 => {
                        // Single argument (common case: async fn foo(x: i32) ...)
                        // Allocate space for state machine, pass it as first arg (sret), original arg as second
                        let arg0 = inner.args[0].get_i32()
                            .map(|i| i as i64)
                            .or_else(|| inner.args[0].get_i64())
                            .unwrap_or(0i64);
                        let buffer = Box::into_raw(Box::new([0u8; 64])) as *mut u8;
                        let f: extern "C" fn(*mut u8, i64) = std::mem::transmute(inner.init_fn);
                        f(buffer, arg0);
                        buffer
                    }
                    _ => {
                        // Multiple arguments not yet supported
                        inner.state = PromiseState::Failed(format!(
                            "Async functions with {} arguments not yet supported",
                            inner.args.len()
                        ));
                        return inner.state.clone();
                    }
                };

                if state_machine.is_null() {
                    inner.state = PromiseState::Failed("Failed to create async state machine".to_string());
                    return inner.state.clone();
                }

                inner.state_machine = Some(state_machine);

                // The async ABI in Zyntax generates two functions:
                // 1. Constructor: `{fn}_new(params...) -> StateMachine` (init_fn)
                // 2. Poll: `{fn}_poll(self: *StateMachine, cx: *Context) -> i64`
                //    where 0 = Pending, non-zero = Ready(value)
            }
        }

        inner.state.clone()
    }

    /// Poll with a maximum number of iterations
    ///
    /// This is useful for avoiding infinite loops when the async function
    /// might be stuck or taking too long.
    pub fn poll_with_limit(&self, max_polls: usize) -> PromiseState {
        let inner = self.state.lock().unwrap();
        if inner.poll_count >= max_polls {
            drop(inner);
            let mut inner = self.state.lock().unwrap();
            inner.state = PromiseState::Failed(format!(
                "Async operation timed out after {} polls", max_polls
            ));
            return inner.state.clone();
        }
        drop(inner);
        self.poll()
    }

    /// Block until the promise completes and return the result
    pub fn await_result<T: FromZyntax>(&self) -> RuntimeResult<T> {
        loop {
            match self.poll() {
                PromiseState::Pending => {
                    // Yield to allow other work
                    std::thread::yield_now();
                }
                PromiseState::Ready(value) => {
                    return T::from_zyntax(value).map_err(RuntimeError::from);
                }
                PromiseState::Failed(err) => {
                    return Err(RuntimeError::Promise(err));
                }
                PromiseState::Cancelled => {
                    return Err(RuntimeError::Promise("Promise was cancelled".to_string()));
                }
            }
        }
    }

    /// Block until the promise completes and return the raw value
    pub fn await_raw(&self) -> RuntimeResult<ZyntaxValue> {
        loop {
            match self.poll() {
                PromiseState::Pending => {
                    std::thread::yield_now();
                }
                PromiseState::Ready(value) => {
                    return Ok(value);
                }
                PromiseState::Failed(err) => {
                    return Err(RuntimeError::Promise(err));
                }
                PromiseState::Cancelled => {
                    return Err(RuntimeError::Promise("Promise was cancelled".to_string()));
                }
            }
        }
    }

    /// Check if the promise is complete
    pub fn is_complete(&self) -> bool {
        let inner = self.state.lock().unwrap();
        !matches!(inner.state, PromiseState::Pending)
    }

    /// Check if the promise is pending
    pub fn is_pending(&self) -> bool {
        let inner = self.state.lock().unwrap();
        matches!(inner.state, PromiseState::Pending)
    }

    /// Check if the promise was cancelled
    pub fn is_cancelled(&self) -> bool {
        let inner = self.state.lock().unwrap();
        matches!(inner.state, PromiseState::Cancelled)
    }

    /// Cancel the promise
    ///
    /// Returns `true` if the promise was successfully cancelled (was pending),
    /// `false` if the promise was already complete or cancelled.
    ///
    /// Once cancelled, any subsequent polls will return `PromiseState::Cancelled`.
    /// Any code waiting on this promise will receive a cancellation error.
    ///
    /// # Example
    ///
    /// ```rust,ignore
    /// let promise = runtime.call_async("slow_task", &[])?;
    ///
    /// // Cancel if taking too long
    /// std::thread::sleep(std::time::Duration::from_secs(1));
    /// if promise.is_pending() {
    ///     promise.cancel();
    /// }
    /// ```
    pub fn cancel(&self) -> bool {
        let mut inner = self.state.lock().unwrap();
        if matches!(inner.state, PromiseState::Pending) {
            inner.state = PromiseState::Cancelled;
            // Wake any waiting futures
            if let Some(waker) = inner.waker.take() {
                waker.wake();
            }
            true
        } else {
            false
        }
    }

    /// Get the current state without polling
    pub fn state(&self) -> PromiseState {
        self.state.lock().unwrap().state.clone()
    }

    /// Block until the promise completes with a timeout
    ///
    /// Returns `Err` if the timeout is exceeded.
    pub fn await_with_timeout(&self, timeout: std::time::Duration) -> RuntimeResult<ZyntaxValue> {
        let start = std::time::Instant::now();
        loop {
            if start.elapsed() > timeout {
                return Err(RuntimeError::Promise(format!(
                    "Async operation timed out after {:?}", timeout
                )));
            }
            match self.poll() {
                PromiseState::Pending => {
                    std::thread::yield_now();
                }
                PromiseState::Ready(value) => {
                    return Ok(value);
                }
                PromiseState::Failed(err) => {
                    return Err(RuntimeError::Promise(err));
                }
                PromiseState::Cancelled => {
                    return Err(RuntimeError::Promise("Promise was cancelled".to_string()));
                }
            }
        }
    }

    /// Get the number of times this promise has been polled
    pub fn poll_count(&self) -> usize {
        self.state.lock().unwrap().poll_count
    }

    /// Chain another operation to run when this promise completes
    pub fn then<F>(&self, f: F) -> ZyntaxPromise
    where
        F: FnOnce(ZyntaxValue) -> ZyntaxValue + Send + 'static,
    {
        let source = self.state.clone();
        let task_id = NEXT_TASK_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        // Create a new promise that depends on this one
        let new_promise = ZyntaxPromise {
            state: Arc::new(Mutex::new(PromiseInner {
                init_fn: std::ptr::null(),
                poll_fn: None,
                args: vec![],
                state: PromiseState::Pending,
                state_machine: None,
                ready_queue: Arc::new(Mutex::new(std::collections::VecDeque::new())),
                task_id,
                poll_count: 0,
                waker: None,
            })),
        };

        let target = new_promise.state.clone();

        // Spawn a thread to wait for completion and run the callback
        std::thread::spawn(move || {
            loop {
                let source_state = source.lock().unwrap().state.clone();
                match source_state {
                    PromiseState::Ready(value) => {
                        let result = f(value);
                        target.lock().unwrap().state = PromiseState::Ready(result);
                        break;
                    }
                    PromiseState::Failed(err) => {
                        target.lock().unwrap().state = PromiseState::Failed(err);
                        break;
                    }
                    PromiseState::Cancelled => {
                        target.lock().unwrap().state = PromiseState::Cancelled;
                        break;
                    }
                    PromiseState::Pending => {
                        std::thread::yield_now();
                    }
                }
            }
        });

        new_promise
    }

    /// Handle errors from this promise
    pub fn catch<F>(&self, f: F) -> ZyntaxPromise
    where
        F: FnOnce(String) -> ZyntaxValue + Send + 'static,
    {
        let source = self.state.clone();
        let task_id = NEXT_TASK_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst);

        let new_promise = ZyntaxPromise {
            state: Arc::new(Mutex::new(PromiseInner {
                init_fn: std::ptr::null(),
                poll_fn: None,
                args: vec![],
                state: PromiseState::Pending,
                state_machine: None,
                ready_queue: Arc::new(Mutex::new(std::collections::VecDeque::new())),
                task_id,
                poll_count: 0,
                waker: None,
            })),
        };

        let target = new_promise.state.clone();

        std::thread::spawn(move || {
            loop {
                let source_state = source.lock().unwrap().state.clone();
                match source_state {
                    PromiseState::Ready(value) => {
                        target.lock().unwrap().state = PromiseState::Ready(value);
                        break;
                    }
                    PromiseState::Failed(err) => {
                        let result = f(err);
                        target.lock().unwrap().state = PromiseState::Ready(result);
                        break;
                    }
                    PromiseState::Cancelled => {
                        // For catch, treat cancellation as an error to recover from
                        let result = f("Promise was cancelled".to_string());
                        target.lock().unwrap().state = PromiseState::Ready(result);
                        break;
                    }
                    PromiseState::Pending => {
                        std::thread::yield_now();
                    }
                }
            }
        });

        new_promise
    }
}

impl Clone for ZyntaxPromise {
    fn clone(&self) -> Self {
        Self {
            state: self.state.clone(),
        }
    }
}

// ============================================================================
// Promise Combinators (Promise.all, Promise.race, etc.)
// ============================================================================

/// Result of awaiting multiple promises in parallel
///
/// Similar to JavaScript's `Promise.all()`, this collects the results of
/// multiple async operations that run concurrently.
#[derive(Debug, Clone)]
pub enum PromiseAllState {
    /// All promises are still pending or some are in progress
    Pending,
    /// All promises completed successfully with their values (in order)
    AllReady(Vec<ZyntaxValue>),
    /// At least one promise failed (first failure encountered)
    Failed(String),
}

/// Await multiple promises in parallel, similar to JavaScript's `Promise.all()`
///
/// This polls all promises concurrently and resolves when ALL promises complete.
/// If any promise fails, the entire operation fails with the first error.
///
/// # Example
///
/// ```rust,ignore
/// use zyntax_embed::{ZyntaxRuntime, ZyntaxValue, PromiseAll};
///
/// // Create multiple async calls
/// let promises = vec![
///     runtime.call_async("compute", &[ZyntaxValue::Int(1)])?,
///     runtime.call_async("compute", &[ZyntaxValue::Int(2)])?,
///     runtime.call_async("compute", &[ZyntaxValue::Int(3)])?,
/// ];
///
/// // Wait for all to complete
/// let mut all = PromiseAll::new(promises);
/// let results = all.await_all()?;
/// // results = [result1, result2, result3]
/// ```
pub struct PromiseAll {
    promises: Vec<ZyntaxPromise>,
    poll_count: usize,
}

impl PromiseAll {
    /// Create a new PromiseAll from a vector of promises
    pub fn new(promises: Vec<ZyntaxPromise>) -> Self {
        Self {
            promises,
            poll_count: 0,
        }
    }

    /// Create a PromiseAll from an iterator of promises
    pub fn from_iter<I: IntoIterator<Item = ZyntaxPromise>>(iter: I) -> Self {
        Self::new(iter.into_iter().collect())
    }

    /// Poll all promises once, advancing each state machine
    ///
    /// Returns the combined state of all promises.
    pub fn poll(&mut self) -> PromiseAllState {
        self.poll_count += 1;

        let mut all_ready = true;
        let mut results = Vec::with_capacity(self.promises.len());

        for promise in &self.promises {
            match promise.poll() {
                PromiseState::Pending => {
                    all_ready = false;
                    results.push(ZyntaxValue::Void); // Placeholder
                }
                PromiseState::Ready(value) => {
                    results.push(value);
                }
                PromiseState::Failed(err) => {
                    // Fast-fail on first error
                    return PromiseAllState::Failed(err);
                }
                PromiseState::Cancelled => {
                    // Fast-fail on cancellation
                    return PromiseAllState::Failed("Promise was cancelled".to_string());
                }
            }
        }

        if all_ready {
            PromiseAllState::AllReady(results)
        } else {
            PromiseAllState::Pending
        }
    }

    /// Poll with a maximum number of iterations per promise
    pub fn poll_with_limit(&mut self, max_polls: usize) -> PromiseAllState {
        if self.poll_count >= max_polls {
            return PromiseAllState::Failed(format!(
                "PromiseAll timed out after {} polls", max_polls
            ));
        }
        self.poll()
    }

    /// Block until all promises complete
    ///
    /// Returns all results in order, or the first error encountered.
    pub fn await_all(&mut self) -> RuntimeResult<Vec<ZyntaxValue>> {
        loop {
            match self.poll() {
                PromiseAllState::Pending => {
                    std::thread::yield_now();
                }
                PromiseAllState::AllReady(values) => {
                    return Ok(values);
                }
                PromiseAllState::Failed(err) => {
                    return Err(RuntimeError::Promise(err));
                }
            }
        }
    }

    /// Block until all promises complete with a timeout
    pub fn await_all_with_timeout(
        &mut self,
        timeout: std::time::Duration,
    ) -> RuntimeResult<Vec<ZyntaxValue>> {
        let start = std::time::Instant::now();
        loop {
            if start.elapsed() > timeout {
                return Err(RuntimeError::Promise(format!(
                    "PromiseAll timed out after {:?}", timeout
                )));
            }
            match self.poll() {
                PromiseAllState::Pending => {
                    std::thread::yield_now();
                }
                PromiseAllState::AllReady(values) => {
                    return Ok(values);
                }
                PromiseAllState::Failed(err) => {
                    return Err(RuntimeError::Promise(err));
                }
            }
        }
    }

    /// Get the number of promises in this group
    pub fn len(&self) -> usize {
        self.promises.len()
    }

    /// Check if this group is empty
    pub fn is_empty(&self) -> bool {
        self.promises.is_empty()
    }

    /// Get the total poll count
    pub fn poll_count(&self) -> usize {
        self.poll_count
    }

    /// Check if all promises are complete (without polling)
    pub fn is_complete(&self) -> bool {
        self.promises.iter().all(|p| p.is_complete())
    }
}

/// Await the first promise to complete, similar to JavaScript's `Promise.race()`
///
/// This polls all promises concurrently and resolves as soon as ANY promise completes
/// (either successfully or with an error).
///
/// # Example
///
/// ```rust,ignore
/// use zyntax_embed::{ZyntaxRuntime, ZyntaxValue, PromiseRace};
///
/// let promises = vec![
///     runtime.call_async("slow_task", &[])?,
///     runtime.call_async("fast_task", &[])?,
/// ];
///
/// let mut race = PromiseRace::new(promises);
/// let (index, result) = race.await_first()?;
/// // index = index of the first promise to complete
/// // result = the value from that promise
/// ```
pub struct PromiseRace {
    promises: Vec<ZyntaxPromise>,
    poll_count: usize,
}

/// Result of a promise race
#[derive(Debug, Clone)]
pub enum PromiseRaceState {
    /// No promise has completed yet
    Pending,
    /// A promise completed successfully (index, value)
    Winner(usize, ZyntaxValue),
    /// A promise failed (index, error)
    Failed(usize, String),
}

impl PromiseRace {
    /// Create a new PromiseRace from a vector of promises
    pub fn new(promises: Vec<ZyntaxPromise>) -> Self {
        Self {
            promises,
            poll_count: 0,
        }
    }

    /// Poll all promises once, checking for the first completion
    pub fn poll(&mut self) -> PromiseRaceState {
        self.poll_count += 1;

        for (index, promise) in self.promises.iter().enumerate() {
            match promise.poll() {
                PromiseState::Ready(value) => {
                    return PromiseRaceState::Winner(index, value);
                }
                PromiseState::Failed(err) => {
                    return PromiseRaceState::Failed(index, err);
                }
                PromiseState::Cancelled => {
                    return PromiseRaceState::Failed(index, "Promise was cancelled".to_string());
                }
                PromiseState::Pending => {
                    // Continue checking other promises
                }
            }
        }

        PromiseRaceState::Pending
    }

    /// Block until any promise completes
    ///
    /// Returns the index and value of the first promise to complete.
    pub fn await_first(&mut self) -> RuntimeResult<(usize, ZyntaxValue)> {
        loop {
            match self.poll() {
                PromiseRaceState::Pending => {
                    std::thread::yield_now();
                }
                PromiseRaceState::Winner(index, value) => {
                    return Ok((index, value));
                }
                PromiseRaceState::Failed(index, err) => {
                    return Err(RuntimeError::Promise(format!(
                        "Promise {} failed: {}", index, err
                    )));
                }
            }
        }
    }

    /// Block until any promise completes with a timeout
    pub fn await_first_with_timeout(
        &mut self,
        timeout: std::time::Duration,
    ) -> RuntimeResult<(usize, ZyntaxValue)> {
        let start = std::time::Instant::now();
        loop {
            if start.elapsed() > timeout {
                return Err(RuntimeError::Promise(format!(
                    "PromiseRace timed out after {:?}", timeout
                )));
            }
            match self.poll() {
                PromiseRaceState::Pending => {
                    std::thread::yield_now();
                }
                PromiseRaceState::Winner(index, value) => {
                    return Ok((index, value));
                }
                PromiseRaceState::Failed(index, err) => {
                    return Err(RuntimeError::Promise(format!(
                        "Promise {} failed: {}", index, err
                    )));
                }
            }
        }
    }

    /// Get the number of promises in the race
    pub fn len(&self) -> usize {
        self.promises.len()
    }

    /// Check if the race is empty
    pub fn is_empty(&self) -> bool {
        self.promises.is_empty()
    }

    /// Get the total poll count
    pub fn poll_count(&self) -> usize {
        self.poll_count
    }
}

/// Await all promises, collecting both successes and failures
///
/// Similar to JavaScript's `Promise.allSettled()`, this waits for ALL promises
/// to complete regardless of success or failure.
///
/// # Example
///
/// ```rust,ignore
/// use zyntax_embed::{ZyntaxRuntime, ZyntaxValue, PromiseAllSettled, SettledResult};
///
/// let promises = vec![
///     runtime.call_async("might_fail", &[ZyntaxValue::Int(1)])?,
///     runtime.call_async("might_fail", &[ZyntaxValue::Int(2)])?,
/// ];
///
/// let mut settled = PromiseAllSettled::new(promises);
/// let results = settled.await_all();
/// for result in results {
///     match result {
///         SettledResult::Fulfilled(value) => println!("Success: {:?}", value),
///         SettledResult::Rejected(err) => println!("Failed: {}", err),
///     }
/// }
/// ```
pub struct PromiseAllSettled {
    promises: Vec<ZyntaxPromise>,
    poll_count: usize,
}

/// Result for a single promise in allSettled
#[derive(Debug, Clone)]
pub enum SettledResult {
    /// Promise completed successfully
    Fulfilled(ZyntaxValue),
    /// Promise failed with an error
    Rejected(String),
}

impl PromiseAllSettled {
    /// Create a new PromiseAllSettled from a vector of promises
    pub fn new(promises: Vec<ZyntaxPromise>) -> Self {
        Self {
            promises,
            poll_count: 0,
        }
    }

    /// Poll all promises once
    ///
    /// Returns None if any promise is still pending, or Some with all results.
    pub fn poll(&mut self) -> Option<Vec<SettledResult>> {
        self.poll_count += 1;

        let mut all_settled = true;
        let mut results = Vec::with_capacity(self.promises.len());

        for promise in &self.promises {
            match promise.poll() {
                PromiseState::Pending => {
                    all_settled = false;
                    results.push(SettledResult::Rejected("pending".to_string())); // Placeholder
                }
                PromiseState::Ready(value) => {
                    results.push(SettledResult::Fulfilled(value));
                }
                PromiseState::Failed(err) => {
                    results.push(SettledResult::Rejected(err));
                }
                PromiseState::Cancelled => {
                    results.push(SettledResult::Rejected("Promise was cancelled".to_string()));
                }
            }
        }

        if all_settled {
            Some(results)
        } else {
            None
        }
    }

    /// Block until all promises settle (complete or fail)
    pub fn await_all(&mut self) -> Vec<SettledResult> {
        loop {
            if let Some(results) = self.poll() {
                return results;
            }
            std::thread::yield_now();
        }
    }

    /// Block until all promises settle with a timeout
    pub fn await_all_with_timeout(
        &mut self,
        timeout: std::time::Duration,
    ) -> RuntimeResult<Vec<SettledResult>> {
        let start = std::time::Instant::now();
        loop {
            if start.elapsed() > timeout {
                return Err(RuntimeError::Promise(format!(
                    "PromiseAllSettled timed out after {:?}", timeout
                )));
            }
            if let Some(results) = self.poll() {
                return Ok(results);
            }
            std::thread::yield_now();
        }
    }

    /// Get the number of promises
    pub fn len(&self) -> usize {
        self.promises.len()
    }

    /// Check if empty
    pub fn is_empty(&self) -> bool {
        self.promises.is_empty()
    }

    /// Get the total poll count
    pub fn poll_count(&self) -> usize {
        self.poll_count
    }
}

// ============================================================================
// Variadic Function Calling Support
// ============================================================================

/// Call a function pointer with dynamic arguments
///
/// Supports up to 8 arguments. For more arguments, consider using
/// a struct-based calling convention or libffi.
///
/// # Safety
///
/// The caller must ensure:
/// - `ptr` is a valid function pointer with the correct signature
/// - `args` contains the correct number and types of arguments
unsafe fn call_dynamic_function(
    ptr: *const u8,
    args: &[DynamicValue],
) -> RuntimeResult<DynamicValue> {
    let result = match args.len() {
        0 => {
            let f: extern "C" fn() -> DynamicValue = std::mem::transmute(ptr);
            f()
        }
        1 => {
            let f: extern "C" fn(DynamicValue) -> DynamicValue = std::mem::transmute(ptr);
            f(args[0].clone())
        }
        2 => {
            let f: extern "C" fn(DynamicValue, DynamicValue) -> DynamicValue =
                std::mem::transmute(ptr);
            f(args[0].clone(), args[1].clone())
        }
        3 => {
            let f: extern "C" fn(DynamicValue, DynamicValue, DynamicValue) -> DynamicValue =
                std::mem::transmute(ptr);
            f(args[0].clone(), args[1].clone(), args[2].clone())
        }
        4 => {
            let f: extern "C" fn(DynamicValue, DynamicValue, DynamicValue, DynamicValue) -> DynamicValue =
                std::mem::transmute(ptr);
            f(args[0].clone(), args[1].clone(), args[2].clone(), args[3].clone())
        }
        5 => {
            let f: extern "C" fn(DynamicValue, DynamicValue, DynamicValue, DynamicValue, DynamicValue) -> DynamicValue =
                std::mem::transmute(ptr);
            f(args[0].clone(), args[1].clone(), args[2].clone(), args[3].clone(), args[4].clone())
        }
        6 => {
            let f: extern "C" fn(DynamicValue, DynamicValue, DynamicValue, DynamicValue, DynamicValue, DynamicValue) -> DynamicValue =
                std::mem::transmute(ptr);
            f(args[0].clone(), args[1].clone(), args[2].clone(), args[3].clone(), args[4].clone(), args[5].clone())
        }
        7 => {
            let f: extern "C" fn(DynamicValue, DynamicValue, DynamicValue, DynamicValue, DynamicValue, DynamicValue, DynamicValue) -> DynamicValue =
                std::mem::transmute(ptr);
            f(args[0].clone(), args[1].clone(), args[2].clone(), args[3].clone(), args[4].clone(), args[5].clone(), args[6].clone())
        }
        8 => {
            let f: extern "C" fn(DynamicValue, DynamicValue, DynamicValue, DynamicValue, DynamicValue, DynamicValue, DynamicValue, DynamicValue) -> DynamicValue =
                std::mem::transmute(ptr);
            f(args[0].clone(), args[1].clone(), args[2].clone(), args[3].clone(), args[4].clone(), args[5].clone(), args[6].clone(), args[7].clone())
        }
        n => {
            return Err(RuntimeError::Execution(
                format!("Functions with {} arguments not supported (max 8). Consider using a struct-based calling convention.", n)
            ));
        }
    };
    Ok(result)
}

/// Implement Rust's Future trait for ZyntaxPromise
impl std::future::Future for ZyntaxPromise {
    type Output = RuntimeResult<ZyntaxValue>;

    fn poll(self: std::pin::Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> std::task::Poll<Self::Output> {
        // Store the waker for later notification
        {
            let mut inner = self.state.lock().unwrap();
            inner.waker = Some(cx.waker().clone());
        }

        // Poll the promise
        match ZyntaxPromise::poll(&self) {
            PromiseState::Ready(value) => std::task::Poll::Ready(Ok(value)),
            PromiseState::Failed(err) => std::task::Poll::Ready(Err(RuntimeError::Promise(err))),
            PromiseState::Cancelled => std::task::Poll::Ready(Err(RuntimeError::Promise("Promise was cancelled".to_string()))),
            PromiseState::Pending => std::task::Poll::Pending,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_promise_state() {
        let promise = ZyntaxPromise {
            state: Arc::new(Mutex::new(PromiseInner {
                init_fn: std::ptr::null(),
                poll_fn: None,
                args: vec![],
                state: PromiseState::Ready(ZyntaxValue::Int(42)),
                state_machine: None,
                ready_queue: Arc::new(Mutex::new(std::collections::VecDeque::new())),
                task_id: 0,
                poll_count: 0,
                waker: None,
            })),
        };

        assert!(promise.is_complete());
        assert!(!promise.is_pending());

        match promise.state() {
            PromiseState::Ready(ZyntaxValue::Int(42)) => {}
            _ => panic!("Expected Ready(42)"),
        }
    }

    #[test]
    fn test_promise_then() {
        let promise = ZyntaxPromise {
            state: Arc::new(Mutex::new(PromiseInner {
                init_fn: std::ptr::null(),
                poll_fn: None,
                args: vec![],
                state: PromiseState::Ready(ZyntaxValue::Int(10)),
                state_machine: None,
                ready_queue: Arc::new(Mutex::new(std::collections::VecDeque::new())),
                task_id: 0,
                poll_count: 0,
                waker: None,
            })),
        };

        let chained = promise.then(|v| {
            if let ZyntaxValue::Int(n) = v {
                ZyntaxValue::Int(n * 2)
            } else {
                v
            }
        });

        // Wait for the chain to complete
        std::thread::sleep(std::time::Duration::from_millis(50));

        match chained.state() {
            PromiseState::Ready(ZyntaxValue::Int(20)) => {}
            state => panic!("Expected Ready(20), got {:?}", state),
        }
    }
}
