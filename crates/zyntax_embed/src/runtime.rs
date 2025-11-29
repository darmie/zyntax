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
    let result_i64 = match native_args.len() {
        0 => call_0(ptr, signature.ret),
        1 => call_1(ptr, native_args[0], signature.ret),
        2 => call_2(ptr, native_args[0], native_args[1], signature.ret),
        3 => call_3(ptr, native_args[0], native_args[1], native_args[2], signature.ret),
        4 => call_4(ptr, native_args[0], native_args[1], native_args[2], native_args[3], signature.ret),
        n => return Err(RuntimeError::Execution(format!(
            "Unsupported argument count: {}. Maximum is 4.", n
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
}

/// An external function that can be called from Zyntax code
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
            config,
            external_functions: HashMap::new(),
            import_resolvers: Vec::new(),
            grammars: HashMap::new(),
            extension_map: HashMap::new(),
            async_functions: std::collections::HashSet::new(),
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
            config: CompilationConfig::default(),
            external_functions: HashMap::new(),
            import_resolvers: Vec::new(),
            grammars: HashMap::new(),
            extension_map: HashMap::new(),
            async_functions: std::collections::HashSet::new(),
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
        // Also track which functions are async
        for (id, func) in &module.functions {
            if let Some(name) = func.name.resolve_global() {
                self.function_ids.insert(name.clone(), *id);

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
        // Parse source to TypedAST
        let typed_program = grammar.parse(source)
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
    fn lower_typed_program(&self, program: zyntax_typed_ast::TypedProgram) -> RuntimeResult<HirModule> {
        use zyntax_compiler::lowering::{LoweringContext, LoweringConfig};
        use zyntax_typed_ast::{AstArena, InternedString, TypeRegistry};

        let arena = AstArena::new();
        let module_name = InternedString::new_global("main");
        let type_registry = std::sync::Arc::new(TypeRegistry::new());

        let mut lowering_ctx = LoweringContext::new(
            module_name,
            type_registry.clone(),
            std::sync::Arc::new(std::sync::Mutex::new(arena)),
            LoweringConfig::default(),
        );

        // Skip type checking (parser already produced typed AST)
        std::env::set_var("SKIP_TYPE_CHECK", "1");

        let mut hir_module = lowering_ctx
            .lower_program(&program)
            .map_err(|e| RuntimeError::Execution(format!("Lowering error: {:?}", e)))?;

        // Monomorphization
        zyntax_compiler::monomorphize_module(&mut hir_module)
            .map_err(|e| RuntimeError::Execution(format!("Monomorphization error: {:?}", e)))?;

        Ok(hir_module)
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
    /// Note: This uses dynamic calling convention. For native (i32, i64) functions,
    /// use `call_native` with a signature instead.
    pub fn call_raw(&self, name: &str, args: &[ZyntaxValue]) -> RuntimeResult<ZyntaxValue> {
        let ptr = self.get_function_ptr(name)
            .ok_or_else(|| RuntimeError::FunctionNotFound(name.to_string()))?;

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
        // Check if this is an async function compiled with state machine ABI
        if self.async_functions.contains(name) {
            // Use the state machine pattern:
            // 1. Look up {fn}_new as the constructor
            // 2. Look up {fn}_poll as the poll function
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

            // Convert arguments
            let dynamic_args: Vec<DynamicValue> = args.iter()
                .cloned()
                .map(|v| v.into_dynamic())
                .collect();

            Ok(ZyntaxPromise::with_poll_fn(init_ptr, poll_ptr, dynamic_args))
        } else {
            // Fall back to simple function pointer for non-async or external functions
            let ptr = self.get_function_ptr(name)
                .ok_or_else(|| RuntimeError::FunctionNotFound(name.to_string()))?;

            // Convert arguments
            let dynamic_args: Vec<DynamicValue> = args.iter()
                .cloned()
                .map(|v| v.into_dynamic())
                .collect();

            Ok(ZyntaxPromise::new(ptr, dynamic_args))
        }
    }

    /// Register an external function that can be called from Zyntax code
    pub fn register_function(&mut self, name: &str, ptr: *const u8, arg_count: usize) {
        self.external_functions.insert(name.to_string(), ExternalFunction {
            name: name.to_string(),
            ptr,
            arg_count,
        });
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

        // Register all symbols from the plugin
        for (name, ptr) in plugin.symbols() {
            self.register_function(name, *ptr, 0); // Arity unknown without type info
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

        // Register all collected symbols
        for (name, ptr) in registry.collect_symbols() {
            self.register_function(name, ptr, 0);
        }

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
        self.load_module_with_exports(language, source, &[])
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
        let grammar = self
            .grammars
            .get(language)
            .cloned()
            .ok_or_else(|| RuntimeError::Execution(format!(
                "Unknown language '{}'. Registered languages: {:?}",
                language,
                self.languages()
            )))?;

        // Parse source to TypedAST
        let typed_program = grammar
            .parse(source)
            .map_err(|e| RuntimeError::Execution(e.to_string()))?;

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

        self.load_module(&language, &source)
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
    /// Tiered configuration
    config: TieredConfig,
    /// Registered language grammars (language name -> grammar)
    grammars: HashMap<String, Arc<LanguageGrammar>>,
    /// File extension to language mapping (e.g., ".zig" -> "zig")
    extension_map: HashMap<String, String>,
}

impl TieredRuntime {
    /// Create a tiered runtime with the given configuration
    pub fn new(config: TieredConfig) -> RuntimeResult<Self> {
        let backend = TieredBackend::new(config.clone())?;

        Ok(Self {
            backend,
            function_ids: HashMap::new(),
            config,
            grammars: HashMap::new(),
            extension_map: HashMap::new(),
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
        // Store function name -> ID mapping (resolve InternedString to actual string)
        for (id, func) in &module.functions {
            if let Some(name) = func.name.resolve_global() {
                self.function_ids.insert(name, *id);
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
    pub fn call_async(&self, name: &str, args: &[ZyntaxValue]) -> RuntimeResult<ZyntaxPromise> {
        // Async functions in Zyntax generate two functions:
        // - {name}_new: constructor that returns the state machine
        // - {name}_poll: poll function that advances the state machine
        //
        // Try to find both. If the user passed "double", look for "double_new" and "double_poll"
        let new_name = format!("{}_new", name);
        let poll_name = format!("{}_poll", name);

        // Try the async naming convention first
        let (init_ptr, poll_ptr) = if let (Some(new_id), Some(poll_id)) =
            (self.function_ids.get(&new_name), self.function_ids.get(&poll_name))
        {
            // Record calls
            self.backend.record_call(*new_id);
            self.backend.record_call(*poll_id);

            let new_ptr = self.backend.get_function_pointer(*new_id)
                .ok_or_else(|| RuntimeError::FunctionNotFound(new_name.clone()))?;
            let poll_ptr = self.backend.get_function_pointer(*poll_id)
                .ok_or_else(|| RuntimeError::FunctionNotFound(poll_name.clone()))?;

            (new_ptr, Some(poll_ptr))
        } else if let Some(func_id) = self.function_ids.get(name) {
            // Fall back to direct function call (for sync functions used as async)
            self.backend.record_call(*func_id);
            let ptr = self.backend.get_function_pointer(*func_id)
                .ok_or_else(|| RuntimeError::FunctionNotFound(name.to_string()))?;
            (ptr, None)
        } else {
            return Err(RuntimeError::FunctionNotFound(format!(
                "Neither '{}' nor '{}_new'/'{}_poll' found", name, name, name
            )));
        };

        let dynamic_args: Vec<DynamicValue> = args.iter()
            .cloned()
            .map(|v| v.into_dynamic())
            .collect();

        if let Some(poll_ptr) = poll_ptr {
            Ok(ZyntaxPromise::with_poll_fn(init_ptr, poll_ptr, dynamic_args))
        } else {
            Ok(ZyntaxPromise::new(init_ptr, dynamic_args))
        }
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
        for (name, ptr) in plugin.symbols() {
            self.backend.register_runtime_symbol(name, *ptr);
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

        // Register all collected symbols
        for (name, ptr) in registry.collect_symbols() {
            self.backend.register_runtime_symbol(name, ptr);
        }

        Ok(count)
    }

    // ========================================================================
    // Multi-Language Grammar Registry
    // ========================================================================

    /// Register a language grammar with the runtime
    ///
    /// See `ZyntaxRuntime::register_grammar` for full documentation.
    pub fn register_grammar(&mut self, language: &str, grammar: LanguageGrammar) {
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

        // Parse source to TypedAST
        let typed_program = grammar
            .parse(source)
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
    fn lower_typed_program(&self, program: zyntax_typed_ast::TypedProgram) -> RuntimeResult<HirModule> {
        use zyntax_compiler::lowering::{LoweringContext, LoweringConfig};
        use zyntax_typed_ast::{AstArena, InternedString, TypeRegistry};

        let arena = AstArena::new();
        let module_name = InternedString::new_global("main");
        let type_registry = std::sync::Arc::new(TypeRegistry::new());

        let mut lowering_ctx = LoweringContext::new(
            module_name,
            type_registry.clone(),
            std::sync::Arc::new(std::sync::Mutex::new(arena)),
            LoweringConfig::default(),
        );

        std::env::set_var("SKIP_TYPE_CHECK", "1");

        let mut hir_module = lowering_ctx
            .lower_program(&program)
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

    /// Create a new promise with separate constructor and poll functions
    ///
    /// This follows the Zyntax async ABI where:
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

        // If already complete, return the state
        match &inner.state {
            PromiseState::Ready(_) | PromiseState::Failed(_) => {
                return inner.state.clone();
            }
            PromiseState::Pending => {}
        }

        inner.poll_count += 1;

        // Try to advance the state machine
        if let Some(state_machine) = inner.state_machine {
            if let Some(poll_fn) = inner.poll_fn {
                unsafe {
                    // Create a waker for this task (used for the Context parameter)
                    let waker = RuntimeWaker::new(inner.task_id, inner.ready_queue.clone());
                    let std_waker = waker.into_std_waker();
                    let waker_ptr = &std_waker as *const std::task::Waker as *const u8;

                    // Call the poll function on the state machine
                    // Simplified ABI: poll(state_machine: *mut u8, context: *const u8) -> i64
                    // Return value: 0 = Pending, positive = Ready(value), negative = Failed
                    eprintln!("[Promise Poll] Calling poll_fn={:?} with state_machine={:?}", poll_fn, state_machine);
                    let f: extern "C" fn(*mut u8, *const u8) -> i64 =
                        std::mem::transmute(poll_fn);

                    let result = f(state_machine, waker_ptr);
                    eprintln!("[Promise Poll] Result = {}", result);

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
            eprintln!("[Promise Poll] First poll - initializing state machine with init_fn={:?}", inner.init_fn);
            eprintln!("[Promise Poll] poll_fn={:?}", inner.poll_fn);
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
