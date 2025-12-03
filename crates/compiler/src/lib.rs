//! # Zyntax Compiler Infrastructure
//! 
//! This crate provides the compilation pipeline from TypedAST to executable code,
//! supporting both JIT (via Cranelift) and AOT (via LLVM) compilation targets.
//!
//! ## Architecture
//! 
//! The compiler uses a multi-tier approach:
//! - **HIR (High-level IR)**: Platform-agnostic representation with CFG and SSA
//! - **Cranelift Backend**: Fast JIT compilation with hot-reloading support
//! - **LLVM Backend**: Optimized AOT compilation to native targets
//!
//! ## Design Principles
//! 
//! - Shared HIR ensures consistent semantics across backends
//! - CFG and SSA form enable standard compiler optimizations
//! - Type information preserved for optimization opportunities
//! - Memory safety validated before code generation

pub mod hir;
pub mod hir_builder;  // HIR Builder API for direct HIR construction
pub mod bytecode;  // HIR bytecode serialization/deserialization
pub mod stdlib;  // Standard library implementation using HIR Builder
pub mod cfg;
pub mod typed_cfg;  // New: TypedAST-aware CFG builder
pub mod ssa;
pub mod lowering;
pub mod trait_lowering;  // Trait/interface lowering to HIR
pub mod vtable_registry;  // Vtable management and caching
pub mod associated_type_resolver;  // Associated type resolution for trait dispatch
pub mod analysis;
pub mod optimization;
pub mod memory_optimization;  // Memory-aware optimizations
pub mod const_eval;
pub mod monomorphize;
pub mod pattern_matching;
pub mod memory_management;
pub mod memory_pass;
pub mod async_support;
pub mod runtime;  // Async runtime (executor, task, waker)

pub mod cranelift_backend;

#[cfg(feature = "llvm-backend")]
pub mod llvm_backend;

#[cfg(feature = "llvm-backend")]
pub mod llvm_jit_backend;  // LLVM MCJIT for hot-path optimization

pub mod profiling;  // Runtime profiling for tiered compilation
pub mod tiered_backend;  // Tiered JIT/AOT compilation system
pub mod plugin;  // Plugin system for frontend runtime registration
pub mod zrtl;    // ZRTL (Zyntax Runtime Library) dynamic plugin format
pub mod zpack;   // ZPack package format for distributing modules + runtimes

// Re-export key types
pub use hir::{HirModule, HirFunction, HirBlock, HirInstruction, HirValue, HirId};
pub use hir_builder::HirBuilder;  // HIR Builder API
pub use cfg::{ControlFlowGraph, BasicBlock, CfgEdge};
pub use typed_cfg::{TypedCfgBuilder, TypedControlFlowGraph, TypedBasicBlock, TypedTerminator};
pub use ssa::{SsaBuilder, SsaForm, PhiNode};
pub use lowering::{
    LoweringContext, LoweringPipeline, AstLowering, LoweringConfig,
    // Re-export import resolver types for convenience
    ImportResolver, ImportContext, ImportManager, ImportError,
    ResolvedImport, ExportedSymbol, SymbolKind, ModuleArchitecture,
    ChainedResolver, BuiltinResolver,
};
pub use const_eval::{ConstEvaluator, ConstEvalContext};
pub use monomorphize::{MonomorphizationContext, monomorphize_module};
pub use pattern_matching::{PatternMatchCompiler, DecisionNode, check_exhaustiveness};
pub use memory_management::{
    MemoryStrategy, MemoryContext, ARCManager, DropManager, EscapeAnalysis,
    RefCountInfo, EscapeInfo, AllocationInfo
};
pub use memory_pass::MemoryManagementPass;
pub use memory_optimization::MemoryOptimizationPass;
pub use async_support::{
    AsyncCompiler, AsyncStateMachine, AsyncState, AsyncRuntime, AsyncRuntimeType,
    AsyncCapture, AsyncCaptureMode, AsyncTerminator
};

use thiserror::Error;
use zyntax_typed_ast::TypedProgram;
use std::sync::{Arc, Mutex};
use optimization::OptimizationPass;

#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("Lowering error: {0}")]
    Lowering(String),

    #[error("Analysis error: {0}")]
    Analysis(String),

    #[error("Optimization error: {0}")]
    Optimization(String),

    #[error("Code generation error: {0}")]
    CodeGen(String),

    #[error("Backend error: {0}")]
    Backend(String),
}

// Implement From for inkwell BuilderError (only when llvm-backend feature is enabled)
#[cfg(feature = "llvm-backend")]
impl From<inkwell::builder::BuilderError> for CompilerError {
    fn from(err: inkwell::builder::BuilderError) -> Self {
        CompilerError::CodeGen(format!("LLVM builder error: {}", err))
    }
}

pub type CompilerResult<T> = Result<T, CompilerError>;

/// Compilation pipeline configuration
#[derive(Clone)]
pub struct CompilationConfig {
    /// Optimization level (0-3)
    pub opt_level: u8,
    /// Enable debug information
    pub debug_info: bool,
    /// Target triple for code generation
    pub target_triple: String,
    /// Enable hot-reloading support (Cranelift only)
    pub hot_reload: bool,
    /// Enable monomorphization of generic functions
    pub enable_monomorphization: bool,
    /// Memory management strategy
    pub memory_strategy: Option<memory_management::MemoryStrategy>,
    /// Async runtime configuration
    pub async_runtime: Option<async_support::AsyncRuntimeType>,
    /// Import resolver for resolving import statements during compilation
    pub import_resolver: Option<Arc<dyn ImportResolver>>,
}

impl std::fmt::Debug for CompilationConfig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CompilationConfig")
            .field("opt_level", &self.opt_level)
            .field("debug_info", &self.debug_info)
            .field("target_triple", &self.target_triple)
            .field("hot_reload", &self.hot_reload)
            .field("enable_monomorphization", &self.enable_monomorphization)
            .field("memory_strategy", &self.memory_strategy)
            .field("async_runtime", &self.async_runtime)
            .field("import_resolver", &self.import_resolver.as_ref().map(|r| r.resolver_name()))
            .finish()
    }
}

impl Default for CompilationConfig {
    fn default() -> Self {
        Self {
            opt_level: 2,
            debug_info: true,
            target_triple: "x86_64-unknown-linux-gnu".to_string(),
            hot_reload: false,
            enable_monomorphization: true,
            memory_strategy: Some(memory_management::MemoryStrategy::ARC),
            async_runtime: Some(async_support::AsyncRuntimeType::Tokio),
            import_resolver: None,
        }
    }
}

/// Main compilation pipeline: TypedAST → HIR → Optimized HIR
///
/// This function orchestrates the complete compilation process:
/// Register all impl blocks from a TypedProgram into its type registry
/// This must be called before lowering starts
pub fn register_impl_blocks(program: &mut zyntax_typed_ast::TypedProgram) -> Result<(), CompilerError> {
    use zyntax_typed_ast::typed_ast::TypedDeclaration;
    use zyntax_typed_ast::type_registry::{ImplDef, MethodImpl, MethodSig, ParamDef};
    use zyntax_typed_ast::Type;

    eprintln!("[REGISTER_IMPL] Checking {} declarations", program.declarations.len());

    // First pass: Register all traits
    eprintln!("[REGISTER_IMPL] First pass: registering traits...");
    let mut trait_count = 0;
    for decl in program.declarations.iter() {
        if let TypedDeclaration::Interface(trait_decl) = &decl.node {
            trait_count += 1;
            let trait_def = zyntax_typed_ast::type_registry::TraitDef {
                id: zyntax_typed_ast::type_registry::TypeId::next(),
                name: trait_decl.name,
                methods: vec![], // Simplified for now
                associated_types: vec![],
                super_traits: vec![],
                type_params: vec![],
                is_object_safe: true,
                span: trait_decl.span,
            };
            program.type_registry.register_trait(trait_def);
            eprintln!("[REGISTER_IMPL] Registered trait {:?}", trait_decl.name);
        }
    }
    eprintln!("[REGISTER_IMPL] Registered {} traits", trait_count);

    // Second pass: Register impl blocks
    eprintln!("[REGISTER_IMPL] Second pass: registering impl blocks...");
    let mut impl_count = 0;
    for (idx, decl) in program.declarations.iter().enumerate() {
        // Log all declaration types to debug
        if idx < 10 || idx >= program.declarations.len() - 10 {
            eprintln!("[REGISTER_IMPL] Decl {}: {:?}", idx, std::mem::discriminant(&decl.node));
        }

        if let TypedDeclaration::Impl(impl_block) = &decl.node {
            impl_count += 1;
            eprintln!("[REGISTER_IMPL] Found impl block #{} for trait {:?}", impl_count, impl_block.trait_name);

            // Find the trait by name
            let trait_def = program.type_registry.get_trait_by_name(impl_block.trait_name)
                .ok_or_else(|| {
                    CompilerError::Analysis(format!("Trait {:?} not found in registry", impl_block.trait_name))
                })?;
            let trait_id = trait_def.id;
            eprintln!("[REGISTER_IMPL] Trait ID: {:?}", trait_id);

            // Resolve the implementing type
            let implementing_type = match &impl_block.for_type {
                Type::Unresolved(name) => {
                    if let Some(type_def) = program.type_registry.get_type_by_name(*name) {
                        Type::Named {
                            id: type_def.id,
                            type_args: vec![],
                            const_args: vec![],
                            variance: vec![],
                            nullability: zyntax_typed_ast::type_registry::NullabilityKind::NonNull,
                        }
                    } else {
                        impl_block.for_type.clone()
                    }
                }
                _ => impl_block.for_type.clone(),
            };

            // Build MethodImpl list
            let method_impls: Vec<MethodImpl> = impl_block.methods.iter().map(|method| {
                let params: Vec<ParamDef> = method.params.iter().map(|p| {
                    ParamDef {
                        name: p.name,
                        ty: p.ty.clone(),
                        is_self: p.is_self,
                        is_varargs: false,
                        is_mut: matches!(p.mutability, zyntax_typed_ast::type_registry::Mutability::Mutable),
                    }
                }).collect();

                let type_params: Vec<zyntax_typed_ast::type_registry::TypeParam> = method.type_params.iter().map(|tp| {
                    zyntax_typed_ast::type_registry::TypeParam {
                        name: tp.name,
                        bounds: vec![],  // Simplified for now
                        variance: zyntax_typed_ast::type_registry::Variance::Invariant,
                        default: tp.default.clone(),
                        span: tp.span,
                    }
                }).collect();

                let method_sig = MethodSig {
                    name: method.name,
                    type_params,
                    params,
                    return_type: method.return_type.clone(),
                    where_clause: vec![],
                    is_static: false,
                    is_async: method.is_async,
                    visibility: zyntax_typed_ast::type_registry::Visibility::Public,
                    span: method.span,
                    is_extension: false,
                };

                MethodImpl {
                    signature: method_sig,
                    is_default: false,
                }
            }).collect();

            // Create and register ImplDef
            let impl_def = ImplDef {
                trait_id,
                for_type: implementing_type.clone(),
                type_args: vec![],
                methods: method_impls,
                associated_types: std::collections::HashMap::new(),
                where_clause: vec![],
                span: impl_block.span,
            };

            program.type_registry.register_implementation(impl_def.clone());
            eprintln!("[REGISTER_IMPL] Registered impl for type {:?} trait {:?}", implementing_type, trait_id);
        }
    }

    eprintln!("[REGISTER_IMPL] Registration complete - registered {} impl blocks", impl_count);
    Ok(())
}

/// 1. Lower TypedAST to HIR (with generic definitions intact)
/// 2. Monomorphize generic functions and types (if enabled)
/// 3. Run analysis passes (liveness, alias analysis, etc.)
/// 4. Run optimization passes
/// 5. Return optimized HIR ready for backend code generation
pub fn compile_to_hir(
    program: &mut TypedProgram,
    type_registry: Arc<zyntax_typed_ast::TypeRegistry>,
    config: CompilationConfig,
) -> CompilerResult<HirModule> {
    // Step 1: Lower TypedAST → HIR
    let lowering_config = lowering::LoweringConfig {
        debug_info: config.debug_info,
        opt_level: config.opt_level,
        target_triple: config.target_triple.clone(),
        hot_reload: config.hot_reload,
        strict_mode: false, // Default to non-strict
        import_resolver: config.import_resolver.clone(),
    };

    // Create arena for string interning (needed for async transformation)
    let mut arena = zyntax_typed_ast::arena::AstArena::new();

    // Extract module name from program or use default
    let module_name = arena.intern_string("main");

    let arena_arc = Arc::new(Mutex::new(arena));
    let mut lowering_ctx = lowering::LoweringContext::new(module_name, type_registry.clone(), arena_arc, lowering_config);
    let mut hir_module = lowering_ctx.lower_program(program)?;

    // Step 2: Async transformation (if async runtime is configured)
    if let Some(runtime_type) = config.async_runtime {
        // Transform async functions into state machines
        let async_func_ids: Vec<HirId> = hir_module.functions.values()
            .filter(|f| f.signature.is_async)
            .map(|f| f.id)
            .collect();

        if !async_func_ids.is_empty() {
            let mut async_compiler = async_support::AsyncCompiler::new();

            // Create async runtime with proper HIR IDs
            let runtime = async_support::AsyncRuntime {
                runtime_type,
                executor: HirId::new(),
                future_trait: HirId::new(),
                waker_type: HirId::new(),
            };
            async_compiler.set_runtime(runtime);

            // Transform each async function
            for func_id in async_func_ids {
                let original_func = hir_module.functions.get(&func_id)
                    .ok_or_else(|| CompilerError::Backend("Async function not found".into()))?
                    .clone();

                // Compile to state machine
                let state_machine = async_compiler.compile_async_function(&original_func)?;

                // Generate poll function (internal implementation)
                let poll_func = async_compiler.generate_poll_function(
                    &state_machine,
                    &mut *lowering_ctx.arena.lock().unwrap(),
                    &original_func,
                )?;

                // Generate async entry function that returns Promise
                let entry_func = async_compiler.generate_async_entry_function(
                    &state_machine,
                    &poll_func,
                    &original_func,
                    &mut *lowering_ctx.arena.lock().unwrap(),
                )?;

                // Add poll function to module (internal implementation detail)
                hir_module.functions.insert(poll_func.id, poll_func);

                // Replace the original async function with the entry function that returns Promise
                hir_module.functions.insert(func_id, entry_func);
            }
        }
    }

    // Step 3: Monomorphization (if enabled and generic functions exist)
    if config.enable_monomorphization {
        let has_generics = hir_module.functions.values()
            .any(|f| !f.signature.type_params.is_empty() || !f.signature.const_params.is_empty());

        if has_generics {
            monomorphize_module(&mut hir_module)?;
        }
    }

    // Step 3: Analysis passes
    let mut analysis_runner = analysis::AnalysisRunner::new(hir_module.clone());
    let analysis = analysis_runner.run_all()?;

    // Step 4: Memory management pass (if strategy is configured)
    if let Some(strategy) = config.memory_strategy {
        let mut memory_pass = memory_pass::MemoryManagementPass::new(strategy);
        memory_pass.run(&mut hir_module, &analysis)?;
    }

    // Step 5: Optimization passes (if opt_level > 0)
    if config.opt_level > 0 {
        let opt_level = match config.opt_level {
            0 => optimization::OptLevel::None,
            1 => optimization::OptLevel::Less,
            2 => optimization::OptLevel::Default,
            _ => optimization::OptLevel::More,
        };

        let mut opt_pipeline = optimization::OptimizationPipeline::new(opt_level);
        opt_pipeline.run(&mut hir_module, &analysis)?;

        // Step 5b: Memory-specific optimizations (if memory strategy is enabled)
        if config.memory_strategy.is_some() {
            let mut memory_opt = memory_optimization::MemoryOptimizationPass::new();
            memory_opt.run(&mut hir_module, &analysis)?;
        }
    }

    Ok(hir_module)
}

/// Compile a TypedProgram to JIT executable code using Cranelift backend
///
/// This is a convenience function that:
/// 1. Compiles TypedAST to HIR (via compile_to_hir)
/// 2. Generates native code using Cranelift JIT
/// 3. Returns a Cranelift backend ready to execute functions
#[cfg(feature = "cranelift")]
pub fn compile_to_jit(
    program: &mut TypedProgram,
    type_registry: Arc<zyntax_typed_ast::TypeRegistry>,
    config: CompilationConfig,
) -> CompilerResult<cranelift_backend::CraneliftBackend> {
    // Compile to HIR
    let hir_module = compile_to_hir(program, type_registry, config)?;

    // Generate native code with Cranelift
    let mut backend = cranelift_backend::CraneliftBackend::new()
        .map_err(|e| CompilerError::Backend(format!("Failed to initialize Cranelift: {}", e)))?;

    backend.compile_module(&hir_module)
        .map_err(|e| CompilerError::CodeGen(format!("Cranelift compilation failed: {}", e)))?;

    Ok(backend)
}