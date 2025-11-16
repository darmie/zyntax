//! # LLVM JIT Backend using MCJIT
//!
//! This backend uses LLVM's Modern JIT (MCJIT) for dynamic compilation with maximum optimization.
//! It's designed to be used as Tier 2 in the tiered compilation system for hot functions.
//!
//! ## Features
//! - On-demand compilation with full LLVM optimizations
//! - Function pointer retrieval for direct calls
//! - Memory-efficient: only compiles functions that are actually hot
//! - Wraps LLVMBackend for HIR → LLVM IR translation (composition pattern)
//!
//! ## vs AOT LLVM Backend
//! - AOT (`llvm_backend.rs`): Compiles entire program to object files/executables
//! - JIT (this file): Compiles individual functions to memory at runtime
//!
//! Both use the same HIR → LLVM IR translation logic.

use std::collections::HashMap;
use inkwell::{
    OptimizationLevel,
    context::Context,
    execution_engine::ExecutionEngine,
    targets::{InitializationConfig, Target},
};

use crate::{CompilerError, CompilerResult};
use crate::hir::{HirModule, HirFunction, HirId};
use crate::llvm_backend::LLVMBackend;

/// LLVM JIT backend using MCJIT
///
/// Wraps the AOT LLVMBackend to reuse HIR → LLVM IR translation,
/// then adds JIT execution capabilities via MCJIT.
pub struct LLVMJitBackend<'ctx> {
    /// AOT backend for HIR → LLVM IR translation
    backend: LLVMBackend<'ctx>,

    /// Execution engine (MCJIT)
    execution_engine: ExecutionEngine<'ctx>,

    /// Function pointers cache (stored as usize for thread safety)
    function_pointers: HashMap<HirId, usize>,

    /// Optimization level
    opt_level: OptimizationLevel,
}

impl<'ctx> LLVMJitBackend<'ctx> {
    /// Create a new LLVM JIT backend with aggressive optimization
    pub fn new(context: &'ctx Context) -> CompilerResult<Self> {
        Self::with_opt_level(context, OptimizationLevel::Aggressive)
    }

    /// Create with custom optimization level
    pub fn with_opt_level(context: &'ctx Context, opt_level: OptimizationLevel) -> CompilerResult<Self> {
        // Initialize LLVM targets
        Target::initialize_native(&InitializationConfig::default())
            .map_err(|e| CompilerError::Backend(format!("Failed to initialize LLVM target: {}", e)))?;

        // Link in MCJIT
        ExecutionEngine::link_in_mc_jit();

        // Create backend for HIR → LLVM IR translation
        let backend = LLVMBackend::new(context, "zyntax_jit");

        // Create JIT execution engine from the backend's module
        let execution_engine = backend.module()
            .create_jit_execution_engine(opt_level)
            .map_err(|e| CompilerError::Backend(format!("Failed to create JIT execution engine: {}", e)))?;

        Ok(Self {
            backend,
            execution_engine,
            function_pointers: HashMap::new(),
            opt_level,
        })
    }

    /// Compile a full HIR module
    ///
    /// Translates all functions to LLVM IR and JIT compiles them.
    /// Function pointers become available via get_function_pointer().
    pub fn compile_module(&mut self, hir_module: &HirModule) -> CompilerResult<()> {
        // Use the wrapped backend to translate HIR → LLVM IR
        self.backend.compile_module(hir_module)?;

        // Extract function pointers from the execution engine
        for (id, function) in &hir_module.functions {
            let fn_name = if function.is_external {
                // External functions use their actual name for linking
                function.name.to_string()
            } else {
                // Regular functions use mangled name matching llvm_backend.rs
                format!("func_{:?}", id)
            };

            // Get function address from JIT execution engine
            let fn_ptr = self.execution_engine
                .get_function_address(&fn_name)
                .map_err(|e| CompilerError::Backend(
                    format!("Failed to get function address for '{}': {:?}", fn_name, e)
                ))?;

            // Cache the pointer (stored as usize for thread safety)
            self.function_pointers.insert(*id, fn_ptr as usize);
        }

        Ok(())
    }

    /// Compile a single function
    ///
    /// Note: For JIT use, prefer compile_module() which handles forward references correctly.
    /// Single-function compilation may fail if the function references other functions.
    pub fn compile_function(&mut self, id: HirId, function: &HirFunction) -> CompilerResult<()> {
        // Create a temporary module with just this function
        use std::collections::HashSet;
        use zyntax_typed_ast::TypeId;

        let temp_module = HirModule {
            id: HirId::new(),
            name: function.name,
            functions: [(id, function.clone())].iter().cloned().collect(),
            globals: HashMap::new(),
            types: HashMap::new(),
            imports: Vec::new(),
            exports: Vec::new(),
            version: 0,
            dependencies: HashSet::new(),
        };

        // Compile it
        self.compile_module(&temp_module)?;

        Ok(())
    }

    /// Get a function pointer
    pub fn get_function_pointer(&self, func_id: HirId) -> Option<*const u8> {
        self.function_pointers.get(&func_id).map(|&addr| addr as *const u8)
    }

    /// Get optimization level
    pub fn optimization_level(&self) -> OptimizationLevel {
        self.opt_level
    }
}

/// Function signature type for JIT-compiled functions
/// This is a type alias for function pointers returned by the JIT
pub type JitFunctionPointer = unsafe extern "C" fn() -> ();

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_llvm_jit_backend_creation() {
        let context = Context::create();
        let backend = LLVMJitBackend::new(&context);
        assert!(backend.is_ok());
    }

    #[test]
    fn test_llvm_jit_backend_opt_levels() {
        let context = Context::create();

        // Test all optimization levels
        for opt_level in &[
            OptimizationLevel::None,
            OptimizationLevel::Less,
            OptimizationLevel::Default,
            OptimizationLevel::Aggressive,
        ] {
            let backend = LLVMJitBackend::with_opt_level(&context, *opt_level);
            assert!(backend.is_ok());
            assert_eq!(backend.unwrap().optimization_level(), *opt_level);
        }
    }
}
