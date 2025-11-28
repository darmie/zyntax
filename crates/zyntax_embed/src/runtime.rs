//! ZyntaxRuntime - Compiler execution API for embedding Zyntax
//!
//! This module provides a high-level API for compiling and executing Zyntax code
//! from Rust, with automatic value conversion and async/await support.

use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use crate::error::{ConversionError, ZyntaxError};
use crate::value::ZyntaxValue;
use crate::convert::FromZyntax;
use zyntax_compiler::{
    CompilationConfig, CompilerError,
    cranelift_backend::CraneliftBackend,
    hir::{HirId, HirModule},
    zrtl::DynamicValue,
    tiered_backend::{TieredBackend, TieredConfig, TieredStatistics, OptimizationTier},
    lowering::AstLowering, // For lower_program trait method
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
        })
    }

    /// Compile a HIR module into the runtime
    ///
    /// After compilation, functions can be called via `call()` or `call_async()`.
    pub fn compile_module(&mut self, module: &zyntax_compiler::HirModule) -> RuntimeResult<()> {
        // Store function name -> ID mapping
        for (id, func) in &module.functions {
            self.function_ids.insert(func.name.to_string(), *id);
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

    /// Call an async function, returning a Promise
    ///
    /// The promise can be awaited to get the result, or polled manually.
    ///
    /// # Example
    ///
    /// ```ignore
    /// let promise = runtime.call_async("fetch_data", &[url.into()])?;
    /// let result: String = promise.await_result()?;
    /// ```
    pub fn call_async(&self, name: &str, args: &[ZyntaxValue]) -> RuntimeResult<ZyntaxPromise> {
        let ptr = self.get_function_ptr(name)
            .ok_or_else(|| RuntimeError::FunctionNotFound(name.to_string()))?;

        // Convert arguments
        let dynamic_args: Vec<DynamicValue> = args.iter()
            .cloned()
            .map(|v| v.into_dynamic())
            .collect();

        Ok(ZyntaxPromise::new(ptr, dynamic_args))
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
}

impl TieredRuntime {
    /// Create a tiered runtime with the given configuration
    pub fn new(config: TieredConfig) -> RuntimeResult<Self> {
        let backend = TieredBackend::new(config.clone())?;

        Ok(Self {
            backend,
            function_ids: HashMap::new(),
            config,
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
        // Store function name -> ID mapping
        for (id, func) in &module.functions {
            self.function_ids.insert(func.name.to_string(), *id);
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
        let func_id = self.function_ids.get(name)
            .ok_or_else(|| RuntimeError::FunctionNotFound(name.to_string()))?;

        // Record the call
        self.backend.record_call(*func_id);

        let ptr = self.backend.get_function_pointer(*func_id)
            .ok_or_else(|| RuntimeError::FunctionNotFound(name.to_string()))?;

        let dynamic_args: Vec<DynamicValue> = args.iter()
            .cloned()
            .map(|v| v.into_dynamic())
            .collect();

        Ok(ZyntaxPromise::new(ptr, dynamic_args))
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

struct PromiseInner {
    /// Function pointer
    func_ptr: *const u8,
    /// Arguments to pass
    args: Vec<DynamicValue>,
    /// Current state
    state: PromiseState,
    /// State machine pointer (for Zyntax async functions)
    state_machine: Option<*mut u8>,
    /// Waker registration
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

impl ZyntaxPromise {
    /// Create a new promise for an async function call
    fn new(func_ptr: *const u8, args: Vec<DynamicValue>) -> Self {
        Self {
            state: Arc::new(Mutex::new(PromiseInner {
                func_ptr,
                args,
                state: PromiseState::Pending,
                state_machine: None,
                waker: None,
            })),
        }
    }

    /// Poll the promise for completion
    ///
    /// Returns the current state without blocking.
    pub fn poll(&self) -> PromiseState {
        let mut inner = self.state.lock().unwrap();

        // If already complete, return the state
        match &inner.state {
            PromiseState::Ready(_) | PromiseState::Failed(_) => {
                return inner.state.clone();
            }
            PromiseState::Pending => {}
        }

        // Try to advance the state machine
        if let Some(state_machine) = inner.state_machine {
            unsafe {
                // Call the poll function on the state machine
                // The state machine follows Zyntax's async ABI:
                // poll(state_machine: *mut u8, waker: *const Waker) -> PollResult
                // where PollResult = { Pending = 0, Ready(value) = 1, Failed(error) = 2 }

                // For now, we simulate completion after first poll
                // A real implementation would call into the Zyntax runtime
                inner.state = PromiseState::Ready(ZyntaxValue::Void);
            }
        } else {
            // Initialize the state machine on first poll
            unsafe {
                let f: extern "C" fn() -> *mut u8 = std::mem::transmute(inner.func_ptr);
                inner.state_machine = Some(f());
            }
        }

        inner.state.clone()
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

    /// Chain another operation to run when this promise completes
    pub fn then<F>(&self, f: F) -> ZyntaxPromise
    where
        F: FnOnce(ZyntaxValue) -> ZyntaxValue + Send + 'static,
    {
        let source = self.state.clone();

        // Create a new promise that depends on this one
        let new_promise = ZyntaxPromise {
            state: Arc::new(Mutex::new(PromiseInner {
                func_ptr: std::ptr::null(),
                args: vec![],
                state: PromiseState::Pending,
                state_machine: None,
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

        let new_promise = ZyntaxPromise {
            state: Arc::new(Mutex::new(PromiseInner {
                func_ptr: std::ptr::null(),
                args: vec![],
                state: PromiseState::Pending,
                state_machine: None,
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
                func_ptr: std::ptr::null(),
                args: vec![],
                state: PromiseState::Ready(ZyntaxValue::Int(42)),
                state_machine: None,
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
                func_ptr: std::ptr::null(),
                args: vec![],
                state: PromiseState::Ready(ZyntaxValue::Int(10)),
                state_machine: None,
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
