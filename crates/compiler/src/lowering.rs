//! # AST Lowering Pipeline
//! 
//! Orchestrates the transformation from TypedAST to HIR in SSA form,
//! ready for both Cranelift and LLVM backends.

use std::sync::{Arc, Mutex};
use std::hash::{Hash, Hasher};
use zyntax_typed_ast::{
    TypedProgram, TypedDeclaration, TypedFunction, TypedNode,
    InternedString, Type, Visibility, Mutability, AstArena, TypeId
};

/// Helper to compute a hash for generating synthetic TypeIds
fn hash_string(s: &str) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    s.hash(&mut hasher);
    hasher.finish()
}
use crate::hir::{HirModule, HirFunction, HirFunctionSignature, HirParam, HirType, ParamAttributes};
use crate::cfg::{ControlFlowGraph, CfgBuilder};
use crate::ssa::{SsaBuilder, SsaForm};
use crate::CompilerResult;

/// Target data layout information for precise size and alignment calculations
#[derive(Debug, Clone)]
pub struct TargetData {
    /// Pointer size in bytes (4 for 32-bit, 8 for 64-bit)
    pub pointer_size: usize,
    /// Pointer alignment in bytes
    pub pointer_alignment: usize,
    /// Target architecture (e.g., "x86_64", "aarch64", "wasm32")
    pub architecture: String,
    /// Endianness (true for big-endian, false for little-endian)
    pub is_big_endian: bool,
}

impl TargetData {
    /// Create target data for the host platform
    pub fn host() -> Self {
        Self {
            pointer_size: std::mem::size_of::<*const ()>(),
            pointer_alignment: std::mem::align_of::<*const ()>(),
            architecture: std::env::consts::ARCH.to_string(),
            is_big_endian: cfg!(target_endian = "big"),
        }
    }

    /// Create target data for a specific architecture
    pub fn for_architecture(arch: &str) -> Self {
        match arch {
            "x86_64" | "aarch64" => Self {
                pointer_size: 8,
                pointer_alignment: 8,
                architecture: arch.to_string(),
                is_big_endian: false,
            },
            "x86" | "arm" | "wasm32" => Self {
                pointer_size: 4,
                pointer_alignment: 4,
                architecture: arch.to_string(),
                is_big_endian: false,
            },
            "wasm64" => Self {
                pointer_size: 8,
                pointer_alignment: 8,
                architecture: arch.to_string(),
                is_big_endian: false,
            },
            _ => Self::host(), // Default to host
        }
    }

    /// Calculate the size of a type in bytes
    pub fn size_of(&self, ty: &Type) -> usize {
        match ty {
            Type::Primitive(prim) => self.primitive_size(prim),
            Type::Reference { .. } => self.pointer_size,
            Type::Array { element_type, size: Some(size_val), .. } => {
                let count = match size_val {
                    zyntax_typed_ast::ConstValue::UInt(u) => *u as usize,
                    zyntax_typed_ast::ConstValue::Int(i) => (*i).max(0) as usize,
                    _ => 1,
                };
                let elem_size = self.size_of(element_type);
                let elem_align = self.align_of(element_type);
                // Align each element
                Self::align_to(elem_size, elem_align) * count
            }
            Type::Tuple(elems) => {
                self.calculate_struct_size(elems.iter().collect())
            }
            Type::Struct { fields, .. } => {
                let field_types: Vec<&Type> = fields.iter().map(|f| &f.ty).collect();
                self.calculate_struct_size(field_types)
            }
            Type::Function { .. } => self.pointer_size, // Function pointer
            Type::Named { .. } => {
                // For named types, we'd need to look up the definition
                // For now, use a conservative estimate
                self.pointer_size
            }
            _ => self.pointer_size, // Conservative default
        }
    }

    /// Calculate the alignment of a type in bytes
    pub fn align_of(&self, ty: &Type) -> usize {
        match ty {
            Type::Primitive(prim) => self.primitive_alignment(prim),
            Type::Reference { .. } => self.pointer_alignment,
            Type::Array { element_type, .. } => self.align_of(element_type),
            Type::Tuple(elems) => {
                elems.iter()
                    .map(|e| self.align_of(e))
                    .max()
                    .unwrap_or(1)
            }
            Type::Struct { fields, .. } => {
                fields.iter()
                    .map(|f| self.align_of(&f.ty))
                    .max()
                    .unwrap_or(1)
            }
            Type::Function { .. } => self.pointer_alignment,
            _ => self.pointer_alignment, // Conservative default
        }
    }

    /// Get the size of a primitive type
    fn primitive_size(&self, prim: &zyntax_typed_ast::PrimitiveType) -> usize {
        use zyntax_typed_ast::PrimitiveType;
        match prim {
            PrimitiveType::Bool => 1,
            PrimitiveType::I8 | PrimitiveType::U8 => 1,
            PrimitiveType::I16 | PrimitiveType::U16 => 2,
            PrimitiveType::I32 | PrimitiveType::U32 => 4,
            PrimitiveType::I64 | PrimitiveType::U64 => 8,
            PrimitiveType::I128 | PrimitiveType::U128 => 16,
            PrimitiveType::F32 => 4,
            PrimitiveType::F64 => 8,
            PrimitiveType::String => self.pointer_size, // String is a pointer
            _ => self.pointer_size,
        }
    }

    /// Get the alignment of a primitive type
    fn primitive_alignment(&self, prim: &zyntax_typed_ast::PrimitiveType) -> usize {
        use zyntax_typed_ast::PrimitiveType;
        match prim {
            PrimitiveType::Bool => 1,
            PrimitiveType::I8 | PrimitiveType::U8 => 1,
            PrimitiveType::I16 | PrimitiveType::U16 => 2,
            PrimitiveType::I32 | PrimitiveType::U32 => 4,
            PrimitiveType::I64 | PrimitiveType::U64 => 8,
            PrimitiveType::I128 | PrimitiveType::U128 => {
                // i128 alignment varies by platform
                if self.architecture.contains("x86") {
                    8 // x86/x86_64 aligns i128 to 8 bytes
                } else {
                    16 // ARM, others align to 16
                }
            }
            PrimitiveType::F32 => 4,
            PrimitiveType::F64 => 8,
            PrimitiveType::String => self.pointer_alignment,
            _ => self.pointer_alignment,
        }
    }

    /// Calculate struct size with proper padding
    fn calculate_struct_size(&self, fields: Vec<&Type>) -> usize {
        if fields.is_empty() {
            return 0;
        }

        let mut offset = 0;
        let mut max_align = 1;

        for field in fields {
            let field_size = self.size_of(field);
            let field_align = self.align_of(field);
            max_align = max_align.max(field_align);

            // Align current offset
            offset = Self::align_to(offset, field_align);
            offset += field_size;
        }

        // Align total size to struct alignment
        Self::align_to(offset, max_align)
    }

    /// Round up to the next multiple of alignment
    fn align_to(value: usize, alignment: usize) -> usize {
        (value + alignment - 1) & !(alignment - 1)
    }
}

/// Lowering context for a compilation unit
pub struct LoweringContext {
    /// Current module being lowered
    pub module: HirModule,
    /// Type registry for type conversions
    pub type_registry: Arc<zyntax_typed_ast::TypeRegistry>,
    /// String arena for creating mangled names
    pub arena: Arc<Mutex<AstArena>>,
    /// Symbol table for name resolution
    pub symbols: SymbolTable,
    /// Diagnostics collector
    pub diagnostics: Vec<LoweringDiagnostic>,
    /// Configuration options
    pub config: LoweringConfig,
    /// Vtable registry for trait dispatch
    pub vtable_registry: crate::vtable_registry::VtableRegistry,
    /// Associated type resolver for trait dispatch
    pub associated_type_resolver: crate::associated_type_resolver::AssociatedTypeResolver,
    /// Target data for precise size/alignment calculations
    pub target_data: TargetData,
    /// Import metadata for debugging
    pub import_metadata: Vec<ImportMetadata>,
    /// Import context for resolving imports during lowering
    pub import_context: ImportContext,
    /// Cache of already-resolved modules (module_path -> resolved imports)
    /// This avoids re-resolving the same module multiple times
    pub resolved_module_cache: std::collections::HashMap<Vec<String>, Vec<ResolvedImport>>,
}

/// Symbol table for name resolution
#[derive(Debug, Default)]
pub struct SymbolTable {
    /// Functions by name
    pub functions: indexmap::IndexMap<InternedString, crate::hir::HirId>,
    /// Globals by name
    pub globals: indexmap::IndexMap<InternedString, crate::hir::HirId>,
    /// Types by name
    pub types: indexmap::IndexMap<InternedString, zyntax_typed_ast::TypeId>,
}

/// Import metadata for debugging and error messages
#[derive(Debug, Clone)]
pub struct ImportMetadata {
    /// Module path (e.g., ["std", "io", "println"])
    pub module_path: Vec<InternedString>,
    /// Imported items
    pub items: Vec<ImportedItem>,
    /// Source location
    pub span: zyntax_typed_ast::Span,
    /// Resolved imports (populated if import resolver is configured)
    pub resolved: Vec<ResolvedImport>,
}

/// An imported item
#[derive(Debug, Clone)]
pub struct ImportedItem {
    /// Original name in the source module
    pub name: InternedString,
    /// Local alias (if renamed)
    pub alias: Option<InternedString>,
    /// Whether this is a glob import
    pub is_glob: bool,
}

// Re-export import resolver types from typed_ast
pub use zyntax_typed_ast::import_resolver::{
    ImportResolver, ImportContext, ImportManager, ImportError,
    ResolvedImport, ExportedSymbol, SymbolKind, ModuleArchitecture,
    ChainedResolver, BuiltinResolver,
};

/// Lowering configuration
#[derive(Clone)]
pub struct LoweringConfig {
    /// Enable debug information
    pub debug_info: bool,
    /// Optimization level (0-3)
    pub opt_level: u8,
    /// Target triple for platform-specific lowering
    pub target_triple: String,
    /// Enable hot-reloading support
    pub hot_reload: bool,
    /// Enable strict mode (fail on warnings)
    pub strict_mode: bool,
    /// Optional import resolver for resolving import statements
    pub import_resolver: Option<Arc<dyn ImportResolver>>,
}

impl std::fmt::Debug for LoweringConfig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LoweringConfig")
            .field("debug_info", &self.debug_info)
            .field("opt_level", &self.opt_level)
            .field("target_triple", &self.target_triple)
            .field("hot_reload", &self.hot_reload)
            .field("strict_mode", &self.strict_mode)
            .field("import_resolver", &self.import_resolver.as_ref().map(|r| r.resolver_name()))
            .finish()
    }
}

impl Default for LoweringConfig {
    fn default() -> Self {
        Self {
            debug_info: true,
            opt_level: 0,
            target_triple: "x86_64-unknown-linux-gnu".to_string(),
            hot_reload: false,
            strict_mode: false,
            import_resolver: None,
        }
    }
}

/// Diagnostic during lowering
#[derive(Debug)]
pub struct LoweringDiagnostic {
    pub level: DiagnosticLevel,
    pub message: String,
    pub span: Option<zyntax_typed_ast::Span>,
}

#[derive(Debug, Clone, Copy)]
pub enum DiagnosticLevel {
    Error,
    Warning,
    Info,
}

/// Main AST lowering interface
pub trait AstLowering {
    /// Lower a typed program to HIR
    fn lower_program(&mut self, program: &TypedProgram) -> CompilerResult<HirModule>;
}

/// Lowering pipeline implementation
pub struct LoweringPipeline {
    passes: Vec<Box<dyn LoweringPass>>,
}

/// Individual lowering pass
pub trait LoweringPass: Send + Sync {
    /// Name of this pass
    fn name(&self) -> &'static str;
    
    /// Dependencies this pass requires
    fn dependencies(&self) -> &[&'static str];
    
    /// Run this pass
    fn run(&mut self, context: &mut LoweringContext) -> CompilerResult<()>;
}

impl LoweringContext {
    pub fn new(
        module_name: InternedString,
        type_registry: Arc<zyntax_typed_ast::TypeRegistry>,
        arena: Arc<Mutex<AstArena>>,
        config: LoweringConfig,
    ) -> Self {
        Self {
            module: HirModule::new(module_name),
            type_registry,
            arena,
            symbols: SymbolTable::default(),
            diagnostics: Vec::new(),
            config,
            vtable_registry: crate::vtable_registry::VtableRegistry::new(),
            associated_type_resolver: crate::associated_type_resolver::AssociatedTypeResolver::new(),
            target_data: TargetData::host(),
            import_metadata: Vec::new(),
            import_context: ImportContext::default(),
            resolved_module_cache: std::collections::HashMap::new(),
        }
    }
    
    /// Add a diagnostic
    pub fn diagnostic(&mut self, level: DiagnosticLevel, message: String, span: Option<zyntax_typed_ast::Span>) {
        self.diagnostics.push(LoweringDiagnostic {
            level,
            message,
            span,
        });
    }
}

impl AstLowering for LoweringContext {
    fn lower_program(&mut self, program: &TypedProgram) -> CompilerResult<HirModule> {
        // Phase 0: Run type checking and inference (Issue 0 Phase 1)
        // This validates types and performs type inference, reporting any errors
        // Skip type checking if SKIP_TYPE_CHECK env var is set (for debugging)
        if std::env::var("SKIP_TYPE_CHECK").is_err() {
            self.run_type_checking(program)?;
        }

        // First pass: collect all declarations
        self.collect_declarations(program)?;

        // Second pass: lower each declaration
        for decl in &program.declarations {
            self.lower_declaration(decl)?;
        }

        // New Phase: Lower trait implementations and generate vtables
        self.lower_implementations()?;

        // Third pass: resolve forward references
        self.resolve_references()?;

        // Increment version for hot-reloading
        if self.config.hot_reload {
            self.module.increment_version();
        }

        Ok(self.module.clone())
    }
}

impl LoweringContext {
    /// Run type checking and inference on the program
    /// This is Issue 0 Phase 1: integrate type inference into lowering
    fn run_type_checking(&mut self, program: &TypedProgram) -> CompilerResult<()> {
        use zyntax_typed_ast::type_checker::{TypeChecker, TypeCheckOptions};

        // Create type checker - needs Box<TypeRegistry>
        let registry = Box::new((*self.type_registry).clone());
        let mut type_checker = TypeChecker::with_options(registry, TypeCheckOptions {
            strict_nulls: false,  // Be lenient for now
            strict_functions: false,
            no_implicit_any: false,  // Allow Unknown types
            check_unreachable: false,
        });

        // Run type checking (validates and performs internal inference)
        type_checker.check_program(program);

        // Check for type errors and display diagnostics
        let diagnostics_collector = type_checker.diagnostics();
        let error_count = diagnostics_collector.error_count();
        let warning_count = diagnostics_collector.warning_count();

        // Display diagnostics using the built-in pretty formatter
        if error_count > 0 || warning_count > 0 {
            use zyntax_typed_ast::diagnostics::{ConsoleDiagnosticDisplay, DiagnosticDisplay};
            use zyntax_typed_ast::source::SourceMap;

            eprintln!("\n=== Type Checking Diagnostics ===");

            // Create a source map (empty for now since we don't have source files in tests)
            let source_map = SourceMap::new();
            let display = ConsoleDiagnosticDisplay::default();

            // Use the built-in pretty formatter
            let diagnostic_output = diagnostics_collector.display_all(&display, &source_map);
            eprintln!("{}", diagnostic_output);

            eprintln!("=================================\n");
        }

        // TODO (Issue 0 Phase 2): Make type checking stricter once inference properly modifies AST
        // For now, just warn about errors rather than failing compilation
        if error_count > 0 {
            eprintln!("WARNING: Type checking found {} error(s), {} warning(s) - continuing compilation",
                error_count, warning_count);
            eprintln!("NOTE: Issue 0 Phase 1 complete - type checking integrated but lenient");
            eprintln!("      Enable strict checking with ZYNTAX_STRICT_TYPES=1\n");

            if std::env::var("ZYNTAX_STRICT_TYPES").is_ok() {
                return Err(crate::CompilerError::Lowering(format!(
                    "Type checking failed with {} error(s), {} warning(s)",
                    error_count, warning_count
                )));
            }
        }

        // Warn about Unknown types (for debugging Issue 0)
        if std::env::var("ZYNTAX_DEBUG_TYPES").is_ok() {
            self.check_for_unknown_types(program);
        }

        Ok(())
    }

    /// Debug helper: check for Unknown types in the program
    fn check_for_unknown_types(&self, program: &TypedProgram) {
        use zyntax_typed_ast::Type;

        let mut unknown_count = 0;
        let mut func_count = 0;

        for decl in &program.declarations {
            if let zyntax_typed_ast::TypedDeclaration::Function(func) = &decl.node {
                func_count += 1;

                // Check return type
                if matches!(func.return_type, Type::Unknown) {
                    eprintln!("  - Function has Unknown return type");
                    unknown_count += 1;
                }

                // Check parameter types
                for param in &func.params {
                    if matches!(param.ty, Type::Unknown) {
                        eprintln!("  - Parameter has Unknown type");
                        unknown_count += 1;
                    }
                }
            }
        }

        if unknown_count > 0 {
            eprintln!("\n[ZYNTAX_DEBUG_TYPES] Found {} Unknown types in {} functions", unknown_count, func_count);
            eprintln!("[ZYNTAX_DEBUG_TYPES] Issue 0 Phase 1: Type checking integrated but inference doesn't modify AST yet");
            eprintln!("[ZYNTAX_DEBUG_TYPES] Next step: Implement AST transformation to apply inferred types\n");
        } else {
            eprintln!("\n[ZYNTAX_DEBUG_TYPES] ✓ No Unknown types found in program\n");
        }
    }

    /// Collect all declarations for forward references
    fn collect_declarations(&mut self, program: &TypedProgram) -> CompilerResult<()> {
        for decl in &program.declarations {
            match &decl.node {
                TypedDeclaration::Function(func) => {
                    let func_id = crate::hir::HirId::new();
                    self.symbols.functions.insert(func.name, func_id);
                }
                
                TypedDeclaration::Class(class_decl) => {
                    // Pre-register class methods in symbol table
                    for method in &class_decl.methods {
                        let method_id = crate::hir::HirId::new();
                        // Methods become mangled names: ClassName_methodName
                        let mangled_name = self.mangle_method_name(class_decl.name, method.name);
                        self.symbols.functions.insert(mangled_name, method_id);
                    }

                    // Pre-register constructors
                    for (i, _ctor) in class_decl.constructors.iter().enumerate() {
                        let ctor_id = crate::hir::HirId::new();
                        // Constructors: ClassName_constructor_N
                        // Use resolve_global() for portability across interner sources
                        let class_name_str = class_decl.name.resolve_global()
                            .unwrap_or_else(|| "UnknownClass".to_string());
                        let mut arena = self.arena.lock().unwrap();
                        let ctor_name = arena.intern_string(&format!("{}_constructor_{}", class_name_str, i));
                        drop(arena);
                        self.symbols.functions.insert(ctor_name, ctor_id);
                    }
                }

                TypedDeclaration::Interface(_trait_decl) => {
                    // NOTE: Trait/interface lowering not yet implemented.
                    // Requires: (1) Trait method table, (2) Vtable generation, (3) Dynamic dispatch infrastructure
                    // WORKAROUND: Skipped (works for code without traits)
                    // FUTURE (v2.0): Implement trait system
                    // Estimated effort: 40-60 hours (major feature - requires runtime support)
                }
                
                _ => {}
            }
        }
        
        Ok(())
    }
    
    /// Lower a declaration
    fn lower_declaration(&mut self, decl: &TypedNode<TypedDeclaration>) -> CompilerResult<()> {
        match &decl.node {
            TypedDeclaration::Function(func) => {
                self.lower_function(func)?;
            }
            
            TypedDeclaration::Variable(var) => {
                self.lower_global_variable(var)?;
            }
            
            TypedDeclaration::Import(import) => {
                self.lower_import(import)?;
            }

            TypedDeclaration::Class(class_decl) => {
                self.lower_class(class_decl)?;
            }

            TypedDeclaration::Enum(enum_decl) => {
                self.lower_enum(enum_decl)?;
            }

            _ => {
                // NOTE: Other declaration types not yet lowered.
                // Includes: TypeAlias, Interface (trait lowering), etc.
                // Most type declarations are handled during type conversion, not as standalone declarations.
                //
                // WORKAROUND: Skipped (types registered on-demand during usage)
                // FUTURE (v2.0): Add explicit lowering for type declarations
                // Estimated effort: Interface/trait lowering ~40-60 hours
            }
        }

        Ok(())
    }
    
    /// Lower a function
    fn lower_function(&mut self, func: &TypedFunction) -> CompilerResult<()> {
        // Convert function signature
        let signature = self.convert_function_signature(func)?;

        // Create HIR function
        let mut hir_func = HirFunction::new(func.name, signature);
        hir_func.id = *self.symbols.functions.get(&func.name).unwrap();

        // Set function attributes
        self.set_function_attributes(&mut hir_func, func);

        // Gap 11: Handle extern functions
        if func.is_external {
            // Mark as external in HIR
            hir_func.is_external = true;

            // Set calling convention
            hir_func.calling_convention = self.convert_calling_convention(func.calling_convention);

            // Extern functions have no body - clear the default entry block
            hir_func.blocks.clear();

            // Add to module and return early
            self.module.add_function(hir_func);
            return Ok(());
        }

        // Regular function: build CFG and SSA from body
        let body = func.body.as_ref().ok_or_else(|| {
            crate::CompilerError::Lowering(format!(
                "Non-extern function '{}' must have a body",
                func.name
            ))
        })?;

        // Build TypedCFG from function body (new approach - Gap #4 solution!)
        // This creates CFG structure from TypedAST without converting to HIR first
        let mut typed_cfg_builder = crate::typed_cfg::TypedCfgBuilder::new();
        let typed_cfg = typed_cfg_builder.build_from_block(body, hir_func.entry_block)?;

        // Debug: check TypedCFG
        log::trace!("[LOWERING] TypedCFG for function {:?} (is_async={}): entry={:?}, nodes={}, edges={}",
            func.name, func.is_async, hir_func.entry_block, typed_cfg.graph.node_count(), typed_cfg.graph.edge_count());

        // Convert to SSA form, processing TypedStatements to emit HIR instructions
        let ssa_builder = SsaBuilder::new(
            hir_func,
            self.type_registry.clone(),
            self.arena.clone(),
            self.symbols.functions.clone()
        ).with_return_type(func.return_type.clone());
        let ssa = ssa_builder.build_from_typed_cfg(&typed_cfg)?;

        // Debug: check SSA result
        if func.is_async {
            log::trace!("[LOWERING] After SSA build for async function {:?}: {} blocks", func.name, ssa.function.blocks.len());
        }

        // Verify SSA properties
        ssa.verify()?;

        // Debug: print phi count before optimization
        let total_phis_before: usize = ssa.function.blocks.values().map(|b| b.phis.len()).sum();
        log::debug!("[Lowering] Before optimize_trivial_phis: {} total phis", total_phis_before);
        for (block_id, block) in &ssa.function.blocks {
            if !block.phis.is_empty() {
                log::debug!("[Lowering]   Block {:?} has {} phis", block_id, block.phis.len());
            }
        }

        // Optimize trivial phis
        let mut ssa = ssa;
        ssa.optimize_trivial_phis();

        // Debug: print phi count after optimization
        let total_phis_after: usize = ssa.function.blocks.values().map(|b| b.phis.len()).sum();
        log::debug!("[Lowering] After optimize_trivial_phis: {} total phis", total_phis_after);

        hir_func = ssa.function;

        // Add string globals generated during SSA construction
        for global in ssa.string_globals {
            self.module.globals.insert(global.id, global);
        }

        // Add closure/lambda functions generated during SSA construction
        for closure_func in ssa.closure_functions {
            self.module.add_function(closure_func);
        }

        // Gap 6 Phase 2: Transform async functions to state machines
        if func.is_async {
            log::trace!("[LOWERING] Before transform_async_function: {:?} with {} values, {} blocks",
                hir_func.name, hir_func.values.len(), hir_func.blocks.len());
            hir_func = self.transform_async_function(hir_func)?;
        }

        // Add to module
        self.module.add_function(hir_func);

        Ok(())
    }

    /// Transform async function into Promise-returning function
    ///
    /// The new design:
    /// - `async fn foo(x: i32) -> i32` becomes `fn foo(x: i32) -> Promise<i32>`
    /// - Promise contains `{state_machine: *mut u8, poll_fn: fn(*mut u8) -> i64}`
    /// - The poll function is generated internally (not exposed as `_poll`)
    /// - When you call foo(x), you get a Promise that holds the state machine and poll fn
    fn transform_async_function(&mut self, original_func: HirFunction) -> CompilerResult<HirFunction> {
        // Create async compiler
        let mut async_compiler = crate::async_support::AsyncCompiler::new();

        // Compile the async function into a state machine
        let state_machine = async_compiler.compile_async_function(&original_func)?;

        // Generate state machine infrastructure
        let (poll_wrapper, async_entry_func) = {
            let mut arena = self.arena.lock().unwrap();

            // 1. Generate the state machine struct type (needed for size calculation)
            let _struct_type = async_compiler.generate_state_machine_struct(
                &state_machine,
                &mut *arena
            );

            // 2. Generate the poll() function (internal, prefixed with __)
            let poll_wrapper = async_compiler.generate_poll_function(
                &state_machine,
                &mut *arena,
                &original_func,
            )?;

            // 3. Generate the async entry function that returns Promise<T>
            // This keeps the ORIGINAL function name and returns Promise
            let async_entry_func = async_compiler.generate_async_entry_function(
                &state_machine,
                &poll_wrapper,
                &original_func,
                &mut *arena
            )?;

            (poll_wrapper, async_entry_func)
        };

        // Add the poll() function to the module (internal implementation detail)
        self.module.add_function(poll_wrapper);

        // Return the async entry function as the replacement
        // This function has the SAME NAME as the original async function
        // and returns Promise<T>
        Ok(async_entry_func)
    }

    /// Convert function signature
    fn convert_function_signature(&self, func: &TypedFunction) -> CompilerResult<HirFunctionSignature> {
        let mut params = Vec::new();
        
        for param in &func.params {
            let hir_type = self.convert_type(&param.ty);
            let attributes = self.compute_param_attributes(&param.ty, param.mutability);
            
            params.push(HirParam {
                id: crate::hir::HirId::new(),
                name: param.name,
                ty: hir_type,
                attributes,
            });
        }
        
        let returns = vec![self.convert_type(&func.return_type)];
        
        // Convert type params from TypedFunction to HirTypeParam
        let hir_type_params: Vec<crate::hir::HirTypeParam> = func.type_params.iter().map(|tp| {
            crate::hir::HirTypeParam {
                name: tp.name,
                constraints: vec![], // TODO: Convert bounds to constraints
            }
        }).collect();

        Ok(HirFunctionSignature {
            params,
            returns,
            type_params: hir_type_params,
            const_params: vec![],
            lifetime_params: vec![],
            is_variadic: false,
            is_async: func.is_async,
        })
    }
    
    /// Set function attributes
    fn set_function_attributes(&self, hir_func: &mut HirFunction, func: &TypedFunction) {
        // Set visibility-based attributes
        match func.visibility {
            Visibility::Public => {
                hir_func.attributes.hot = true; // Public functions might be hot
            }
            Visibility::Private => {
                hir_func.attributes.inline = crate::hir::InlineHint::Hint;
            }
            _ => {}
        }
        
        // Set calling convention (before async transformation)
        hir_func.calling_convention = if func.visibility == Visibility::Public {
            crate::hir::CallingConvention::C
        } else {
            crate::hir::CallingConvention::Fast
        };
    }
    
    /// Compute parameter attributes
    fn compute_param_attributes(&self, ty: &Type, mutability: Mutability) -> ParamAttributes {
        let mut attrs = ParamAttributes::default();
        
        // Large structs passed by reference
        if self.is_large_type(ty) {
            attrs.by_ref = true;
        }
        
        // Immutable references are readonly
        if matches!(ty, Type::Reference { .. }) && mutability == Mutability::Immutable {
            attrs.readonly = true;
            attrs.noalias = true;
        }
        
        // Non-null pointers
        if matches!(ty, Type::Reference { .. }) {
            attrs.nonnull = true;
        }
        
        attrs
    }
    
    /// Check if a type is considered "large" for ABI decisions
    fn is_large_type(&self, ty: &Type) -> bool {
        // Use precise TargetData size calculation
        // Types larger than 16 bytes are typically passed by reference in most ABIs
        matches!(ty, Type::Struct { .. } | Type::Tuple(_) if self.target_data.size_of(ty) > 16)
    }

    /// Get the size of a type in bytes (delegates to TargetData)
    fn estimate_type_size(&self, ty: &Type) -> usize {
        self.target_data.size_of(ty)
    }

    /// Convert a lifetime from TypedAST to HIR
    fn convert_lifetime(&self, lifetime: &zyntax_typed_ast::Lifetime) -> crate::hir::HirLifetime {
        crate::hir::HirLifetime {
            id: crate::hir::LifetimeId::new(),
            name: Some(lifetime.name),
            bounds: lifetime.bounds.iter().map(|bound| {
                match bound {
                    zyntax_typed_ast::LifetimeBound::Outlives(other) => {
                        crate::hir::LifetimeBound::Outlives(crate::hir::LifetimeId::new())
                    }
                    zyntax_typed_ast::LifetimeBound::Static => {
                        crate::hir::LifetimeBound::Static
                    }
                }
            }).collect(),
        }
    }
    
    /// Convert frontend type to HIR type
    fn convert_type(&self, ty: &Type) -> HirType {
        use zyntax_typed_ast::PrimitiveType;
        
        match ty {
            Type::Primitive(prim) => match prim {
                PrimitiveType::Bool => HirType::Bool,
                PrimitiveType::I8 => HirType::I8,
                PrimitiveType::I16 => HirType::I16,
                PrimitiveType::I32 => HirType::I32,
                PrimitiveType::I64 => HirType::I64,
                PrimitiveType::I128 => HirType::I128,
                PrimitiveType::U8 => HirType::U8,
                PrimitiveType::U16 => HirType::U16,
                PrimitiveType::U32 => HirType::U32,
                PrimitiveType::U64 => HirType::U64,
                PrimitiveType::U128 => HirType::U128,
                PrimitiveType::F32 => HirType::F32,
                PrimitiveType::F64 => HirType::F64,
                PrimitiveType::Unit => HirType::Void,
                PrimitiveType::Char => HirType::U32, // Unicode scalar
                PrimitiveType::String => HirType::Ptr(Box::new(HirType::U8)), // String as u8 pointer
                _ => HirType::I64, // Default
            },
            
            Type::Tuple(types) if types.is_empty() => HirType::Void,
            Type::Tuple(types) => HirType::Struct(crate::hir::HirStructType {
                name: None,
                fields: types.iter().map(|t| self.convert_type(t)).collect(),
                packed: false,
            }),
            
            Type::Reference { ty, mutability, lifetime, .. } => {
                // Convert lifetime if present
                let hir_lifetime = if let Some(lt) = lifetime {
                    self.convert_lifetime(lt)
                } else {
                    // Anonymous lifetime
                    crate::hir::HirLifetime {
                        id: crate::hir::LifetimeId::new(),
                        name: None,
                        bounds: vec![],
                    }
                };

                HirType::Ref {
                    lifetime: hir_lifetime,
                    pointee: Box::new(self.convert_type(ty)),
                    mutable: *mutability == Mutability::Mutable,
                }
            }
            
            Type::Array { element_type, size: Some(size_val), .. } => {
                let size = match size_val {
                    zyntax_typed_ast::ConstValue::UInt(u) => *u,
                    zyntax_typed_ast::ConstValue::Int(i) => *i as u64,
                    _ => 0,
                };
                HirType::Array(Box::new(self.convert_type(element_type)), size)
            }
            
            Type::Function { params, return_type, is_varargs, .. } => {
                // Note: Lifetime parameters are embedded in the parameter types themselves
                // (e.g., Type::Reference contains lifetime information). They are preserved
                // during convert_type() and don't need separate tracking here.
                HirType::Function(Box::new(crate::hir::HirFunctionType {
                    params: params.iter().map(|p| self.convert_type(&p.ty)).collect(),
                    returns: vec![self.convert_type(return_type)],
                    lifetime_params: vec![], // Lifetimes tracked in parameter types
                    is_variadic: *is_varargs,
                }))
            }
            
            Type::Projection { base, item } => {
                // Handle projections like T::AssocType
                // For now, we don't have full associated type information in TypedAST
                // This will be used when TypedAST is extended with associated types
                // TODO: Enhance when TypedAST gets AssociatedType variant with trait_id
                HirType::Opaque(*item)
            }

            Type::Associated { trait_name, type_name } => {
                // Associated types in traits (not yet fully integrated)
                // TODO: Resolve using trait_name and type_name when trait registry is enhanced
                HirType::Opaque(*type_name)
            }

            Type::Named { id, .. } => {
                // Look up type definition in registry
                if let Some(type_def) = self.type_registry.get_type_by_id(*id) {
                    match &type_def.kind {
                        zyntax_typed_ast::TypeKind::Struct { fields, .. } => {
                            // Convert struct fields
                            let field_types: Vec<_> = fields.iter()
                                .map(|field| self.convert_type(&field.ty))
                                .collect();

                            HirType::Struct(crate::hir::HirStructType {
                                name: Some(type_def.name),
                                fields: field_types,
                                packed: false,
                            })
                        }

                        zyntax_typed_ast::TypeKind::Enum { variants } => {
                            // Convert enum to discriminated union
                            let hir_variants: Vec<_> = variants.iter()
                                .map(|variant| {
                                    let variant_ty = match &variant.fields {
                                        zyntax_typed_ast::VariantFields::Unit => HirType::Void,
                                        zyntax_typed_ast::VariantFields::Tuple(types) => {
                                            let fields: Vec<_> = types.iter()
                                                .map(|ty| self.convert_type(ty))
                                                .collect();
                                            HirType::Struct(crate::hir::HirStructType {
                                                name: None,
                                                fields,
                                                packed: false,
                                            })
                                        }
                                        zyntax_typed_ast::VariantFields::Named(fields) => {
                                            let field_types: Vec<_> = fields.iter()
                                                .map(|field| self.convert_type(&field.ty))
                                                .collect();
                                            HirType::Struct(crate::hir::HirStructType {
                                                name: Some(variant.name),
                                                fields: field_types,
                                                packed: false,
                                            })
                                        }
                                    };

                                    crate::hir::HirUnionVariant {
                                        name: variant.name,
                                        ty: variant_ty,
                                        discriminant: variant.discriminant.unwrap_or(0) as u64,
                                    }
                                })
                                .collect();

                            HirType::Union(Box::new(crate::hir::HirUnionType {
                                name: Some(type_def.name),
                                variants: hir_variants,
                                discriminant_type: Box::new(HirType::U32),
                                is_c_union: false,
                            }))
                        }

                        zyntax_typed_ast::TypeKind::Interface { .. } => {
                            // Interfaces/Traits become opaque for now
                            // Full trait support requires vtable generation
                            HirType::Opaque(type_def.name)
                        }

                        zyntax_typed_ast::TypeKind::Alias { target } => {
                            // Recursively resolve alias
                            self.convert_type(target)
                        }

                        _ => HirType::Opaque(type_def.name),
                    }
                } else {
                    // Type not found - create opaque placeholder
                    // Use a simple string representation
                    HirType::I64 // Fallback
                }
            }
            
            _ => HirType::I64, // Default for unsupported types
        }
    }
    
    /// Lower a global variable
    fn lower_global_variable(&mut self, var: &zyntax_typed_ast::TypedVariable) -> CompilerResult<()> {
        let hir_type = self.convert_type(&var.ty);

        // Evaluate initializer expression if present
        let initializer = if let Some(init_expr) = &var.initializer {
            match self.eval_const_expression(&init_expr.node, &hir_type) {
                Ok(constant) => Some(constant),
                Err(e) => {
                    self.diagnostic(
                        DiagnosticLevel::Error,
                        format!("Global variable '{}' has non-constant initializer: {}", var.name, e),
                        Some(init_expr.span),
                    );
                    None
                }
            }
        } else {
            None
        };

        let global = crate::hir::HirGlobal {
            id: crate::hir::HirId::new(),
            name: var.name,
            ty: hir_type,
            initializer,
            is_const: var.mutability == Mutability::Immutable,
            is_thread_local: false,
            linkage: self.convert_linkage(var.visibility),
            visibility: self.convert_visibility(var.visibility),
        };

        self.symbols.globals.insert(var.name, global.id);
        self.module.add_global(global);

        Ok(())
    }

    /// Lower an enum declaration to HIR Union type
    fn lower_enum(&mut self, enum_decl: &zyntax_typed_ast::typed_ast::TypedEnum) -> CompilerResult<()> {
        use zyntax_typed_ast::typed_ast::TypedVariantFields;
        use crate::hir::{HirUnionType, HirUnionVariant, HirType};

        // Determine discriminant type based on number of variants
        let discriminant_type = if enum_decl.variants.len() <= 256 {
            HirType::U8
        } else if enum_decl.variants.len() <= 65536 {
            HirType::U16
        } else {
            HirType::U32
        };

        // Convert variants
        let mut hir_variants = Vec::new();
        for (variant_idx, variant) in enum_decl.variants.iter().enumerate() {
            // Determine discriminant value
            let discriminant = if let Some(disc_expr) = &variant.discriminant {
                // Evaluate constant expression for explicit discriminant
                match self.eval_const_expression(&disc_expr.node, &discriminant_type) {
                    Ok(crate::hir::HirConstant::U8(v)) => v as u64,
                    Ok(crate::hir::HirConstant::U16(v)) => v as u64,
                    Ok(crate::hir::HirConstant::U32(v)) => v as u64,
                    Ok(crate::hir::HirConstant::U64(v)) => v,
                    Ok(crate::hir::HirConstant::I8(v)) => v as u64,
                    Ok(crate::hir::HirConstant::I16(v)) => v as u64,
                    Ok(crate::hir::HirConstant::I32(v)) => v as u64,
                    Ok(crate::hir::HirConstant::I64(v)) => v as u64,
                    _ => {
                        self.diagnostic(
                            DiagnosticLevel::Error,
                            format!("Invalid discriminant for enum variant '{}'", variant.name),
                            Some(variant.span),
                        );
                        variant_idx as u64
                    }
                }
            } else {
                variant_idx as u64
            };

            // Convert variant fields to HIR type
            let variant_ty = match &variant.fields {
                TypedVariantFields::Unit => {
                    // Unit variant - use void type
                    HirType::Void
                }
                TypedVariantFields::Tuple(field_types) => {
                    if field_types.len() == 1 {
                        // Single field - use type directly
                        self.convert_type(&field_types[0])
                    } else {
                        // Multiple fields - create tuple struct
                        let fields: Vec<HirType> = field_types
                            .iter()
                            .map(|ty| self.convert_type(ty))
                            .collect();
                        HirType::Struct(crate::hir::HirStructType {
                            name: None, // Anonymous tuple struct
                            fields,
                            packed: false,
                        })
                    }
                }
                TypedVariantFields::Named(named_fields) => {
                    // Named fields - create named struct
                    let fields: Vec<HirType> = named_fields
                        .iter()
                        .map(|field| self.convert_type(&field.ty))
                        .collect();
                    HirType::Struct(crate::hir::HirStructType {
                        name: Some(variant.name), // Use variant name
                        fields,
                        packed: false,
                    })
                }
            };

            hir_variants.push(HirUnionVariant {
                name: variant.name,
                ty: variant_ty,
                discriminant,
            });
        }

        // Create HIR union type
        let union_type = HirUnionType {
            name: Some(enum_decl.name),
            variants: hir_variants,
            discriminant_type: Box::new(discriminant_type),
            is_c_union: false, // Tagged union with discriminant
        };

        // Register the enum type in the module
        // Look up the type ID from the type registry (should already be registered by type checker)
        if let Some(type_id) = self.type_registry.get_type_by_name(enum_decl.name).map(|def| def.id) {
            self.module.types.insert(type_id, HirType::Union(Box::new(union_type)));
        } else {
            // Type not found in registry - this shouldn't happen for well-typed programs
            self.diagnostic(
                DiagnosticLevel::Warning,
                format!("Enum type '{}' not found in type registry", enum_decl.name),
                Some(enum_decl.span),
            );
        }

        Ok(())
    }

    /// Evaluate a TypedExpression as a compile-time constant
    fn eval_const_expression(
        &self,
        expr: &zyntax_typed_ast::TypedExpression,
        expected_ty: &crate::hir::HirType,
    ) -> CompilerResult<crate::hir::HirConstant> {
        use zyntax_typed_ast::{TypedExpression, TypedLiteral};
        use crate::hir::HirConstant;

        match expr {
            // Simple literals
            TypedExpression::Literal(lit) => match lit {
                TypedLiteral::Bool(b) => Ok(HirConstant::Bool(*b)),
                TypedLiteral::Integer(i) => {
                    // Convert to appropriate integer type based on expected type
                    match expected_ty {
                        crate::hir::HirType::I8 => Ok(HirConstant::I8(*i as i8)),
                        crate::hir::HirType::I16 => Ok(HirConstant::I16(*i as i16)),
                        crate::hir::HirType::I32 => Ok(HirConstant::I32(*i as i32)),
                        crate::hir::HirType::I64 => Ok(HirConstant::I64(*i as i64)),
                        crate::hir::HirType::I128 => Ok(HirConstant::I128(*i)),
                        crate::hir::HirType::U8 => Ok(HirConstant::U8(*i as u8)),
                        crate::hir::HirType::U16 => Ok(HirConstant::U16(*i as u16)),
                        crate::hir::HirType::U32 => Ok(HirConstant::U32(*i as u32)),
                        crate::hir::HirType::U64 => Ok(HirConstant::U64(*i as u64)),
                        crate::hir::HirType::U128 => Ok(HirConstant::U128(*i as u128)),
                        _ => Ok(HirConstant::I32(*i as i32)), // Default to i32
                    }
                }
                TypedLiteral::Float(f) => {
                    match expected_ty {
                        crate::hir::HirType::F32 => Ok(HirConstant::F32(*f as f32)),
                        crate::hir::HirType::F64 => Ok(HirConstant::F64(*f)),
                        _ => Ok(HirConstant::F64(*f)), // Default to f64
                    }
                }
                TypedLiteral::String(s) => Ok(HirConstant::String(*s)),
                TypedLiteral::Char(_) => {
                    Err(crate::CompilerError::Analysis("Char literals not yet supported in global initializers".into()))
                }
                TypedLiteral::Unit => {
                    // Unit type maps to null pointer type
                    Ok(HirConstant::Null(expected_ty.clone()))
                }
                TypedLiteral::Null => {
                    // Null literal for optional types
                    Ok(HirConstant::Null(expected_ty.clone()))
                }
                TypedLiteral::Undefined => {
                    // Undefined - use zeroed memory as placeholder
                    Ok(HirConstant::I32(0))
                }
            }

            // Array literals
            TypedExpression::Array(elements) => {
                let element_ty = match expected_ty {
                    crate::hir::HirType::Array(elem_ty, _) => elem_ty.as_ref().clone(),
                    _ => return Err(crate::CompilerError::Analysis("Array initializer for non-array type".into())),
                };

                let mut const_elements = Vec::new();
                for elem in elements {
                    const_elements.push(self.eval_const_expression(&elem.node, &element_ty)?);
                }

                Ok(HirConstant::Array(const_elements))
            }

            // Struct literals
            TypedExpression::Struct(struct_lit) => {
                let field_types = match expected_ty {
                    crate::hir::HirType::Struct(s) => &s.fields,
                    _ => return Err(crate::CompilerError::Analysis("Struct initializer for non-struct type".into())),
                };

                let mut const_fields = Vec::new();
                for (field_init, field_ty) in struct_lit.fields.iter().zip(field_types.iter()) {
                    const_fields.push(self.eval_const_expression(&field_init.value.node, field_ty)?);
                }

                Ok(HirConstant::Struct(const_fields))
            }

            // Binary operations (for simple const arithmetic)
            TypedExpression::Binary(binary) => {
                let left = self.eval_const_expression(&binary.left.node, expected_ty)?;
                let right = self.eval_const_expression(&binary.right.node, expected_ty)?;
                self.eval_const_binary_op(&binary.op, &left, &right)
            }

            // Unary operations
            TypedExpression::Unary(unary) => {
                let operand = self.eval_const_expression(&unary.operand.node, expected_ty)?;
                self.eval_const_unary_op(&unary.op, &operand)
            }

            _ => Err(crate::CompilerError::Analysis(
                format!("Expression type {:?} not supported in global initializers",
                    std::mem::discriminant(expr))
            )),
        }
    }

    /// Evaluate a binary operation on constants
    fn eval_const_binary_op(
        &self,
        op: &zyntax_typed_ast::BinaryOp,
        left: &crate::hir::HirConstant,
        right: &crate::hir::HirConstant,
    ) -> CompilerResult<crate::hir::HirConstant> {
        use zyntax_typed_ast::BinaryOp;
        use crate::hir::HirConstant;

        // Helper macros for arithmetic operations
        macro_rules! int_op {
            ($left:expr, $right:expr, $op:tt) => {
                match ($left, $right) {
                    (HirConstant::I8(l), HirConstant::I8(r)) => Ok(HirConstant::I8(l $op r)),
                    (HirConstant::I16(l), HirConstant::I16(r)) => Ok(HirConstant::I16(l $op r)),
                    (HirConstant::I32(l), HirConstant::I32(r)) => Ok(HirConstant::I32(l $op r)),
                    (HirConstant::I64(l), HirConstant::I64(r)) => Ok(HirConstant::I64(l $op r)),
                    (HirConstant::U8(l), HirConstant::U8(r)) => Ok(HirConstant::U8(l $op r)),
                    (HirConstant::U16(l), HirConstant::U16(r)) => Ok(HirConstant::U16(l $op r)),
                    (HirConstant::U32(l), HirConstant::U32(r)) => Ok(HirConstant::U32(l $op r)),
                    (HirConstant::U64(l), HirConstant::U64(r)) => Ok(HirConstant::U64(l $op r)),
                    _ => Err(crate::CompilerError::Analysis("Type mismatch in const binary operation".into())),
                }
            }
        }

        match op {
            BinaryOp::Add => int_op!(left, right, +),
            BinaryOp::Sub => int_op!(left, right, -),
            BinaryOp::Mul => int_op!(left, right, *),
            BinaryOp::Div => int_op!(left, right, /),
            BinaryOp::Rem => int_op!(left, right, %),
            _ => Err(crate::CompilerError::Analysis(format!("Binary operator {:?} not supported in const context", op))),
        }
    }

    /// Evaluate a unary operation on a constant
    fn eval_const_unary_op(
        &self,
        op: &zyntax_typed_ast::UnaryOp,
        operand: &crate::hir::HirConstant,
    ) -> CompilerResult<crate::hir::HirConstant> {
        use zyntax_typed_ast::UnaryOp;
        use crate::hir::HirConstant;

        match op {
            UnaryOp::Minus => match operand {
                HirConstant::I8(v) => Ok(HirConstant::I8(-v)),
                HirConstant::I16(v) => Ok(HirConstant::I16(-v)),
                HirConstant::I32(v) => Ok(HirConstant::I32(-v)),
                HirConstant::I64(v) => Ok(HirConstant::I64(-v)),
                HirConstant::F32(v) => Ok(HirConstant::F32(-v)),
                HirConstant::F64(v) => Ok(HirConstant::F64(-v)),
                _ => Err(crate::CompilerError::Analysis("Cannot negate non-numeric constant".into())),
            }
            UnaryOp::Not => match operand {
                HirConstant::Bool(v) => Ok(HirConstant::Bool(!v)),
                _ => Err(crate::CompilerError::Analysis("Cannot apply NOT to non-boolean constant".into())),
            }
            _ => Err(crate::CompilerError::Analysis(format!("Unary operator {:?} not supported in const context", op))),
        }
    }
    
    /// Lower an import declaration
    ///
    /// If an import resolver is configured, this will attempt to resolve the import
    /// and register the resolved symbols in the HIR module. Already-resolved modules
    /// are cached to avoid redundant resolution.
    fn lower_import(&mut self, import: &zyntax_typed_ast::typed_ast::TypedImport) -> CompilerResult<()> {
        use zyntax_typed_ast::typed_ast::TypedImportItem;

        // Convert import items to our internal representation
        let items: Vec<ImportedItem> = import.items.iter().map(|item| {
            match item {
                TypedImportItem::Named { name, alias } => ImportedItem {
                    name: *name,
                    alias: *alias,
                    is_glob: false,
                },
                TypedImportItem::Glob => ImportedItem {
                    name: {
                        let mut arena = self.arena.lock().unwrap();
                        arena.intern_string("*")
                    },
                    alias: None,
                    is_glob: true,
                },
                TypedImportItem::Default(name) => ImportedItem {
                    name: *name,
                    alias: None,
                    is_glob: false,
                },
            }
        }).collect();

        // Convert module path to string vec for cache lookup
        let module_path_strings: Vec<String> = import.module_path
            .iter()
            .filter_map(|s| s.resolve_global())
            .collect();

        // Check cache first - avoid re-resolving already resolved modules
        let resolved = if let Some(cached) = self.resolved_module_cache.get(&module_path_strings) {
            log::debug!(
                "Using cached resolution for {:?}: {} symbols",
                module_path_strings,
                cached.len()
            );
            cached.clone()
        } else if let Some(ref resolver) = self.config.import_resolver {
            // Resolve the import
            match resolver.resolve_import(import, &self.import_context) {
                Ok(resolved_imports) => {
                    log::debug!(
                        "Resolved import {:?}: {} symbols",
                        module_path_strings,
                        resolved_imports.len()
                    );
                    // Cache the result
                    self.resolved_module_cache.insert(module_path_strings.clone(), resolved_imports.clone());
                    resolved_imports
                }
                Err(e) => {
                    // Log the error but don't fail - imports might be resolved externally
                    self.diagnostic(
                        DiagnosticLevel::Warning,
                        format!("Import resolution warning: {}", e),
                        Some(import.span),
                    );
                    Vec::new()
                }
            }
        } else {
            Vec::new()
        };

        // Lower resolved imports to HIR - register symbols for codegen
        for resolved_import in &resolved {
            self.register_resolved_import(resolved_import, &items)?;
        }

        // Track the module path in import context to detect cycles
        self.import_context.imported_modules.push(module_path_strings);

        // Store import metadata
        self.import_metadata.push(ImportMetadata {
            module_path: import.module_path.clone(),
            items,
            span: import.span,
            resolved,
        });

        Ok(())
    }

    /// Register a resolved import in the HIR module for codegen
    ///
    /// This makes the resolved symbols available for code generation.
    fn register_resolved_import(
        &mut self,
        resolved: &ResolvedImport,
        items: &[ImportedItem],
    ) -> CompilerResult<()> {
        use crate::hir::{HirImport, ImportKind, ImportAttributes, HirFunctionSignature, HirParam};

        match resolved {
            ResolvedImport::Function { qualified_name, params, return_type, is_extern } => {
                // Register as an external function declaration in HIR
                let func_name = qualified_name.last()
                    .map(|s| s.as_str())
                    .unwrap_or("unknown");

                let interned_name = {
                    let mut arena = self.arena.lock().unwrap();
                    arena.intern_string(func_name)
                };

                // Check if we need to apply an alias
                let local_name = items.iter()
                    .find(|item| {
                        item.name.resolve_global()
                            .map(|n| n == func_name)
                            .unwrap_or(false)
                    })
                    .and_then(|item| item.alias)
                    .unwrap_or(interned_name);

                // Convert typed_ast types to HIR types
                let hir_params: Vec<HirParam> = params.iter()
                    .enumerate()
                    .map(|(i, ty)| {
                        let hir_type = self.convert_type(ty);
                        let param_name = {
                            let mut arena = self.arena.lock().unwrap();
                            arena.intern_string(&format!("arg{}", i))
                        };
                        HirParam {
                            id: crate::hir::HirId::new(),
                            name: param_name,
                            ty: hir_type,
                            attributes: Default::default(),
                        }
                    })
                    .collect();
                let hir_return = self.convert_type(return_type);

                // Create the function signature
                let signature = HirFunctionSignature {
                    params: hir_params,
                    returns: vec![hir_return],
                    type_params: Vec::new(),
                    const_params: Vec::new(),
                    lifetime_params: Vec::new(),
                    is_variadic: false,
                    is_async: false,
                };

                // Create an import entry for the external function
                let hir_import = HirImport {
                    name: local_name,
                    kind: ImportKind::Function(signature),
                    attributes: ImportAttributes::default(),
                };

                // Add to module's imports
                self.module.imports.push(hir_import);

                log::debug!(
                    "Registered imported function '{}' (alias: {:?})",
                    func_name,
                    if local_name != interned_name { Some(local_name) } else { None }
                );
            }

            ResolvedImport::Type { qualified_name, ty, is_extern } => {
                // Register the type in our symbol table and as an import
                let type_name = qualified_name.last()
                    .map(|s| s.as_str())
                    .unwrap_or("unknown");

                let interned_name = {
                    let mut arena = self.arena.lock().unwrap();
                    arena.intern_string(type_name)
                };

                // Check if we need to apply an alias
                let local_name = items.iter()
                    .find(|item| {
                        item.name.resolve_global()
                            .map(|n| n == type_name)
                            .unwrap_or(false)
                    })
                    .and_then(|item| item.alias)
                    .unwrap_or(interned_name);

                // Convert to HIR type
                let hir_type = self.convert_type(ty);

                // Get or create a TypeId for this imported type
                // For named types, try to get the existing ID from the registry
                let type_id = match ty {
                    zyntax_typed_ast::Type::Named { id, .. } => *id,
                    _ => {
                        // For other types, create a synthetic TypeId from hash
                        zyntax_typed_ast::TypeId::new(
                            hash_string(&qualified_name.join("::")) as u32
                        )
                    }
                };

                // Register in the module's type table
                self.module.types.insert(type_id, hir_type.clone());

                // Register in symbol table for name resolution
                self.symbols.types.insert(local_name, type_id);

                // Create an import entry for the type
                let hir_import = HirImport {
                    name: local_name,
                    kind: ImportKind::Type {
                        ty: hir_type,
                        type_id,
                    },
                    attributes: ImportAttributes::default(),
                };

                self.module.imports.push(hir_import);
            }

            ResolvedImport::Module { path, exports } => {
                // For module imports, register all exported symbols
                for export in exports {
                    if !export.is_public {
                        continue;
                    }

                    let symbol_name = {
                        let mut arena = self.arena.lock().unwrap();
                        arena.intern_string(&export.name)
                    };

                    // Register based on symbol kind
                    match export.kind {
                        SymbolKind::Function => {
                            // Create a placeholder function signature
                            // The actual signature will be resolved when the function is used
                            let signature = HirFunctionSignature {
                                params: Vec::new(),
                                returns: vec![crate::hir::HirType::Void],
                                type_params: Vec::new(),
                                const_params: Vec::new(),
                                lifetime_params: Vec::new(),
                                is_variadic: false,
                                is_async: false,
                            };

                            let hir_import = HirImport {
                                name: symbol_name,
                                kind: ImportKind::Function(signature),
                                attributes: ImportAttributes::default(),
                            };

                            self.module.imports.push(hir_import);
                        }
                        SymbolKind::Type | SymbolKind::Class | SymbolKind::Enum |
                        SymbolKind::Interface | SymbolKind::Trait => {
                            // Create an opaque type for the imported type
                            let hir_type = crate::hir::HirType::Opaque(symbol_name);
                            let type_id = zyntax_typed_ast::TypeId::new(
                                hash_string(&format!("{}::{}", path.join("::"), export.name)) as u32
                            );

                            self.module.types.insert(type_id, hir_type.clone());
                            self.symbols.types.insert(symbol_name, type_id);

                            let hir_import = HirImport {
                                name: symbol_name,
                                kind: ImportKind::Type {
                                    ty: hir_type,
                                    type_id,
                                },
                                attributes: ImportAttributes::default(),
                            };

                            self.module.imports.push(hir_import);
                        }
                        SymbolKind::Constant | SymbolKind::Module => {
                            // Constants and submodules - create global import
                            let hir_import = HirImport {
                                name: symbol_name,
                                kind: ImportKind::Global(crate::hir::HirType::Void),
                                attributes: ImportAttributes::default(),
                            };

                            self.module.imports.push(hir_import);
                        }
                    }
                }
            }

            ResolvedImport::Constant { qualified_name, ty } => {
                // Register as a global import
                let const_name = qualified_name.last()
                    .map(|s| s.as_str())
                    .unwrap_or("unknown");

                let interned_name = {
                    let mut arena = self.arena.lock().unwrap();
                    arena.intern_string(const_name)
                };

                let hir_type = self.convert_type(ty);

                let hir_import = HirImport {
                    name: interned_name,
                    kind: ImportKind::Global(hir_type),
                    attributes: ImportAttributes::default(),
                };

                self.module.imports.push(hir_import);
            }

            ResolvedImport::Glob { module_path, symbols } => {
                // For glob imports, register all public symbols from the module
                for symbol in symbols {
                    if !symbol.is_public {
                        continue;
                    }

                    let symbol_name = {
                        let mut arena = self.arena.lock().unwrap();
                        arena.intern_string(&symbol.name)
                    };

                    match symbol.kind {
                        SymbolKind::Function => {
                            let signature = HirFunctionSignature {
                                params: Vec::new(),
                                returns: vec![crate::hir::HirType::Void],
                                type_params: Vec::new(),
                                const_params: Vec::new(),
                                lifetime_params: Vec::new(),
                                is_variadic: false,
                                is_async: false,
                            };

                            let hir_import = HirImport {
                                name: symbol_name,
                                kind: ImportKind::Function(signature),
                                attributes: ImportAttributes::default(),
                            };

                            self.module.imports.push(hir_import);
                        }
                        SymbolKind::Type | SymbolKind::Class | SymbolKind::Enum |
                        SymbolKind::Interface | SymbolKind::Trait => {
                            let hir_type = crate::hir::HirType::Opaque(symbol_name);
                            let type_id = zyntax_typed_ast::TypeId::new(
                                hash_string(&format!("{}::{}", module_path.join("::"), symbol.name)) as u32
                            );

                            self.module.types.insert(type_id, hir_type.clone());
                            self.symbols.types.insert(symbol_name, type_id);

                            let hir_import = HirImport {
                                name: symbol_name,
                                kind: ImportKind::Type {
                                    ty: hir_type,
                                    type_id,
                                },
                                attributes: ImportAttributes::default(),
                            };

                            self.module.imports.push(hir_import);
                        }
                        SymbolKind::Constant | SymbolKind::Module => {
                            let hir_import = HirImport {
                                name: symbol_name,
                                kind: ImportKind::Global(crate::hir::HirType::Void),
                                attributes: ImportAttributes::default(),
                            };

                            self.module.imports.push(hir_import);
                        }
                    }
                }
            }
        }

        Ok(())
    }

    /// Lower a class declaration
    fn lower_class(&mut self, class: &zyntax_typed_ast::typed_ast::TypedClass) -> CompilerResult<()> {
        // Lower each method as a free function with mangled name
        for method in &class.methods {
            self.lower_method(class.name, method)?;
        }

        // Lower constructors
        for (i, ctor) in class.constructors.iter().enumerate() {
            self.lower_constructor(class.name, i, ctor)?;
        }

        Ok(())
    }

    /// Lower a method to a free function with `self` parameter
    fn lower_method(&mut self, class_name: InternedString, method: &zyntax_typed_ast::typed_ast::TypedMethod) -> CompilerResult<()> {
        use zyntax_typed_ast::typed_ast::{TypedFunction, TypedParameter, ParameterKind, typed_node};
        use zyntax_typed_ast::type_registry::{Type, NullabilityKind, TypeId};

        // Create mangled function name
        let mangled_name = self.mangle_method_name(class_name, method.name);

        // Build parameters: self parameter + method parameters
        let mut params = Vec::new();

        // Add self parameter if not static
        if !method.is_static {
            // Look up the class TypeId from the registry
            let class_type_id = self.type_registry
                .get_type_by_name(class_name)
                .map(|def| def.id)
                .unwrap_or_else(|| TypeId::new(0)); // Fallback if not found

            let self_type = Type::Named {
                id: class_type_id,
                type_args: vec![],
                const_args: vec![],
                variance: vec![],
                nullability: NullabilityKind::NonNull,
            };

            let self_name = {
                let mut arena = self.arena.lock().unwrap();
                arena.intern_string("self")
            };

            let self_param = TypedParameter {
                name: self_name,
                ty: self_type,
                mutability: Mutability::Immutable,
                kind: ParameterKind::Regular,
                default_value: None,
                attributes: vec![],
                span: method.span,
            };
            params.push(self_param);
        }

        // Add method parameters
        for method_param in &method.params {
            let param = TypedParameter {
                name: method_param.name,
                ty: method_param.ty.clone(),
                mutability: method_param.mutability,
                kind: ParameterKind::Regular,
                default_value: method_param.default_value.clone(),
                attributes: vec![],
                span: method_param.span,
            };
            params.push(param);
        }

        // Create a function from the method
        let func = TypedFunction {
            name: mangled_name,
            type_params: method.type_params.clone(),
            params,
            return_type: method.return_type.clone(),
            body: method.body.clone().or_else(|| Some(zyntax_typed_ast::typed_ast::TypedBlock {
                statements: vec![],
                span: method.span,
            })),
            visibility: method.visibility,
            is_async: method.is_async,
            is_external: false,
            calling_convention: zyntax_typed_ast::CallingConvention::Default,
            link_name: None,
        };

        // Lower as a regular function
        self.lower_function(&func)
    }

    /// Lower a constructor
    fn lower_constructor(&mut self, class_name: InternedString, index: usize, ctor: &zyntax_typed_ast::typed_ast::TypedConstructor) -> CompilerResult<()> {
        use zyntax_typed_ast::typed_ast::{TypedFunction, TypedParameter, ParameterKind};
        use zyntax_typed_ast::type_registry::{Type, NullabilityKind, TypeId};

        // Constructor name: ClassName_constructor_N
        // Use resolve_global() for portability across interner sources
        let ctor_name = {
            let class_name_str = class_name.resolve_global()
                .unwrap_or_else(|| "UnknownClass".to_string());
            let mut arena = self.arena.lock().unwrap();
            arena.intern_string(&format!("{}_constructor_{}", class_name_str, index))
        };

        // Build parameters from constructor
        let params: Vec<TypedParameter> = ctor.params.iter().map(|p| TypedParameter {
            name: p.name,
            ty: p.ty.clone(),
            mutability: p.mutability,
            kind: ParameterKind::Regular,
            default_value: p.default_value.clone(),
            attributes: vec![],
            span: p.span,
        }).collect();

        // Constructor returns an instance of the class
        let class_type_id = self.type_registry
            .get_type_by_name(class_name)
            .map(|def| def.id)
            .unwrap_or_else(|| TypeId::new(0)); // Fallback if not found

        let return_type = Type::Named {
            id: class_type_id,
            type_args: vec![],
            const_args: vec![],
            variance: vec![],
            nullability: NullabilityKind::NonNull,
        };

        let func = TypedFunction {
            name: ctor_name,
            type_params: vec![],
            params,
            return_type,
            body: Some(ctor.body.clone()),
            visibility: ctor.visibility,
            is_async: false,
            is_external: false,
            calling_convention: zyntax_typed_ast::CallingConvention::Default,
            link_name: None,
        };

        self.lower_function(&func)
    }

    /// Mangle method name: ClassName_methodName
    fn mangle_method_name(&self, class_name: InternedString, method_name: InternedString) -> InternedString {
        // Use resolve_global() since InternedStrings may come from different sources
        // (global interner from ZynPEG runtime, local arena from JSON deserialization, etc.)
        let class_name_str = class_name.resolve_global()
            .unwrap_or_else(|| "UnknownClass".to_string());
        let method_name_str = method_name.resolve_global()
            .unwrap_or_else(|| "unknown_method".to_string());
        let mut arena = self.arena.lock().unwrap();
        arena.intern_string(&format!("{}_{}", class_name_str, method_name_str))
    }

    /// Convert visibility to linkage
    fn convert_linkage(&self, vis: Visibility) -> crate::hir::Linkage {
        match vis {
            Visibility::Public => crate::hir::Linkage::External,
            Visibility::Private => crate::hir::Linkage::Private,
            Visibility::Protected => crate::hir::Linkage::Internal,
            Visibility::Internal => crate::hir::Linkage::Internal,
        }
    }
    
    /// Convert visibility
    fn convert_visibility(&self, vis: Visibility) -> crate::hir::Visibility {
        match vis {
            Visibility::Public => crate::hir::Visibility::Default,
            Visibility::Private => crate::hir::Visibility::Hidden,
            Visibility::Protected => crate::hir::Visibility::Protected,
            Visibility::Internal => crate::hir::Visibility::Hidden,
        }
    }

    /// Convert TypedAST calling convention to HIR calling convention (Gap 11)
    fn convert_calling_convention(&self, cc: zyntax_typed_ast::CallingConvention) -> crate::hir::CallingConvention {
        use zyntax_typed_ast::CallingConvention as TypedCC;
        match cc {
            TypedCC::Default | TypedCC::Rust => crate::hir::CallingConvention::Fast,
            TypedCC::Cdecl => crate::hir::CallingConvention::C,
            TypedCC::System => crate::hir::CallingConvention::System,
            TypedCC::Stdcall | TypedCC::Fastcall | TypedCC::Thiscall | TypedCC::Vectorcall => {
                // These platform-specific conventions map to C for now
                // Backends can handle them differently if needed
                crate::hir::CallingConvention::C
            }
        }
    }

    /// Lower trait implementations and generate vtables
    ///
    /// This method iterates over all trait implementations in the TypeRegistry
    /// and lowers them to HIR, generating vtables and registering methods in
    /// the VtableRegistry.
    ///
    /// Integration status:
    /// - ✅ TypeRegistry.iter_implementations() available
    /// - ✅ TypeRegistry.get_trait_by_id() available
    /// - ⚠️ Method body lowering needed (lower_impl_method placeholder)
    /// - ⚠️ Vtable globals emission needed (backend work)
    fn lower_implementations(&mut self) -> CompilerResult<()> {
        // Collect implementations to avoid borrow checker issues
        // (iter_implementations borrows self.type_registry immutably,
        //  but lower_impl borrows self mutably)
        let implementations: Vec<_> = self.type_registry
            .iter_implementations()
            .map(|(trait_id, impl_defs)| (*trait_id, impl_defs.clone()))
            .collect();

        // Lower each implementation
        for (trait_id, impl_defs) in implementations {
            for impl_def in &impl_defs {
                self.lower_impl(trait_id, impl_def)?;
            }
        }

        Ok(())
    }

    /// Lower a single trait implementation
    ///
    /// This method:
    /// 1. Converts impl for_type to HIR type
    /// 2. Generates trait method table
    /// 3. Validates implementation satisfies trait
    /// 4. Lowers each method and registers in VtableRegistry
    /// 5. Generates vtable with real function IDs
    /// 6. Creates vtable global and adds to module
    ///
    /// NOTE: Ready to use once TypeRegistry provides access to ImplDef
    #[allow(dead_code)]  // Will be used when TypeRegistry integration is complete
    fn lower_impl(
        &mut self,
        trait_id: TypeId,
        impl_def: &zyntax_typed_ast::ImplDef,
    ) -> CompilerResult<()> {
        use crate::trait_lowering::{
            validate_trait_implementation,
            generate_vtable, create_vtable_global, convert_type,
            generate_trait_method_table
        };

        // Step 1: Convert for_type to HIR type
        let for_type_hir = convert_type(&impl_def.for_type, &self.type_registry)?;

        // Step 2: Extract type_id from for_type (nominal types only)
        let type_id = match &impl_def.for_type {
            zyntax_typed_ast::Type::Named { id, .. } => *id,
            _ => {
                return Err(crate::CompilerError::Analysis(
                    "Impl for_type must be a named type (nominal typing only)".to_string()
                ));
            }
        };

        // Step 2.5: Register implementation in AssociatedTypeResolver
        // This enables resolution of associated types like <T as Trait>::Item
        self.associated_type_resolver.register_impl(
            trait_id,
            type_id,
            Arc::new(impl_def.clone()),
        );

        // Step 3: Generate trait method table
        let trait_def = self.type_registry.get_trait_def(trait_id)
            .ok_or_else(|| crate::CompilerError::Analysis("Trait not found".to_string()))?;
        let trait_method_table = generate_trait_method_table(
            trait_def,
            trait_id,
            &self.type_registry,
        )?;

        // Step 4: Validate implementation
        validate_trait_implementation(&trait_method_table, impl_def)?;

        // Step 5: Lower each method and register in VtableRegistry
        for method_impl in &impl_def.methods {
            // Lower method body to HIR function
            let method_func_id = self.lower_impl_method(
                trait_id,
                type_id,
                method_impl,
            )?;

            // Register method in vtable registry
            self.vtable_registry.register_method(
                trait_id,
                type_id,
                method_impl.signature.name,
                method_func_id,
            );
        }

        // Step 6: Generate vtable (requires trait_method_table from step 3)
        let vtable = generate_vtable(
            &trait_method_table,
            impl_def,
            for_type_hir.clone(),
            type_id,
            &self.vtable_registry,
        )?;

        // Step 7: Create vtable global
        // Generate vtable name: vtable_TraitName_TypeName
        let vtable_name = {
            let name_str = format!("vtable_{}_{}",
                self.type_registry.get_trait_def(trait_id).map(|t| t.name.to_string()).unwrap_or("unknown".to_string()),
                format!("{:?}", impl_def.for_type)
            );
            self.arena.lock().unwrap().intern_string(name_str)
        };
        let vtable_global = create_vtable_global(vtable.clone(), vtable_name);

        // Step 8: Register vtable in registry
        let vtable_id = self.vtable_registry.register_vtable(
            trait_id,
            type_id,
            vtable.clone(),
            vtable_global.clone(),
        );

        // Step 9: Add vtable to module globals
        self.module.globals.insert(vtable_global.id, vtable_global);

        Ok(())
    }

    /// Lower an impl method to HIR function
    ///
    /// This method looks up the already-lowered class method function.
    /// Class methods are lowered during lower_class(), so we just need to
    /// find the corresponding function ID from the symbol table.
    ///
    /// Returns the function ID for vtable registration.
    #[allow(dead_code)]  // Will be used when TypeRegistry integration is complete
    fn lower_impl_method(
        &mut self,
        _trait_id: TypeId,
        type_id: TypeId,
        method_impl: &zyntax_typed_ast::MethodImpl,
    ) -> CompilerResult<crate::hir::HirId> {
        // Get type name first
        let type_def = self.type_registry.get_type_by_id(type_id)
            .ok_or_else(|| crate::CompilerError::Analysis(
                format!("Type {:?} not found in registry", type_id)
            ))?;
        let type_name = type_def.name;

        // Create mangled name matching lower_method format: ClassName::methodName
        // Note: mangle_method_name locks arena internally, so don't hold lock here
        let mangled_name = self.mangle_method_name(type_name, method_impl.signature.name);

        // Lookup the already-lowered function ID from symbol table
        let method_id = self.symbols.functions.get(&mangled_name)
            .copied()
            .ok_or_else(|| crate::CompilerError::Analysis(
                format!("Method function not found in symbol table: {:?}. \
                        This likely means the class method was not lowered yet. \
                        Ensure lower_class() runs before lower_implementations().",
                        mangled_name)
            ))?;

        Ok(method_id)
    }

    /// Verify that all references have been resolved (defensive check)
    fn resolve_references(&mut self) -> CompilerResult<()> {
        // Verification pass: ensure all functions and globals in the module
        // have entries in the symbol table. This is a defensive check since
        // the type checker should have already resolved everything.

        let mut missing_symbols = Vec::new();

        // Check all functions
        for (func_id, func) in &self.module.functions {
            if !self.symbols.functions.values().any(|id| id == func_id) {
                missing_symbols.push(format!("Function '{}' has no symbol table entry", func.name));
            }
        }

        // Check all globals
        for (global_id, global) in &self.module.globals {
            if !self.symbols.globals.values().any(|id| id == global_id) {
                missing_symbols.push(format!("Global '{}' has no symbol table entry", global.name));
            }
        }

        if !missing_symbols.is_empty() {
            // Report warnings for missing symbols (non-fatal, since type checker validated)
            for msg in &missing_symbols {
                self.diagnostic(
                    DiagnosticLevel::Warning,
                    format!("Symbol table inconsistency: {}", msg),
                    None,
                );
            }

            if self.config.strict_mode {
                return Err(crate::CompilerError::Analysis(
                    format!("Symbol table verification failed: {} unresolved references", missing_symbols.len())
                ));
            }
        }

        Ok(())
    }
}

impl LoweringPipeline {
    pub fn new() -> Self {
        Self {
            passes: Vec::new(),
        }
    }
    
    /// Add a pass to the pipeline
    pub fn add_pass(&mut self, pass: Box<dyn LoweringPass>) {
        self.passes.push(pass);
    }
    
    /// Run all passes
    pub fn run(&mut self, context: &mut LoweringContext) -> CompilerResult<()> {
        // Sort passes by dependencies
        self.sort_passes();
        
        // Run each pass
        for pass in &mut self.passes {
            let pass_name = pass.name();
            
            if context.config.debug_info {
                context.diagnostic(
                    DiagnosticLevel::Info,
                    format!("Running lowering pass: {}", pass_name),
                    None,
                );
            }
            
            pass.run(context)?;
        }
        
        Ok(())
    }
    
    /// Sort passes by dependencies using topological sort (Kahn's algorithm)
    fn sort_passes(&mut self) {
        use std::collections::{HashMap, HashSet, VecDeque};

        if self.passes.is_empty() {
            return;
        }

        // Build dependency graph
        let mut in_degree: HashMap<&'static str, usize> = HashMap::new();
        let mut adjacency: HashMap<&'static str, Vec<usize>> = HashMap::new();
        let mut name_to_idx: HashMap<&'static str, usize> = HashMap::new();

        // Initialize data structures
        for (idx, pass) in self.passes.iter().enumerate() {
            let name = pass.name();
            name_to_idx.insert(name, idx);
            in_degree.insert(name, 0);
            adjacency.insert(name, Vec::new());
        }

        // Count in-degrees and build adjacency list
        for (idx, pass) in self.passes.iter().enumerate() {
            for dep in pass.dependencies() {
                // Check if dependency exists
                if !name_to_idx.contains_key(dep) {
                    panic!("Pass '{}' depends on '{}' which is not registered", pass.name(), dep);
                }

                // Add edge: dep -> current pass
                adjacency.get_mut(dep).unwrap().push(idx);
                *in_degree.get_mut(pass.name()).unwrap() += 1;
            }
        }

        // Kahn's algorithm: start with passes that have no dependencies
        let mut queue: VecDeque<usize> = VecDeque::new();
        for (idx, pass) in self.passes.iter().enumerate() {
            if *in_degree.get(pass.name()).unwrap() == 0 {
                queue.push_back(idx);
            }
        }

        // Topological sort
        let mut sorted_indices: Vec<usize> = Vec::new();
        let mut visited: HashSet<&'static str> = HashSet::new();

        while let Some(idx) = queue.pop_front() {
            let pass_name = self.passes[idx].name();
            sorted_indices.push(idx);
            visited.insert(pass_name);

            // Reduce in-degree for dependent passes
            for &dependent_idx in adjacency.get(pass_name).unwrap() {
                let dependent_name = self.passes[dependent_idx].name();
                let degree = in_degree.get_mut(dependent_name).unwrap();
                *degree -= 1;

                if *degree == 0 {
                    queue.push_back(dependent_idx);
                }
            }
        }

        // Detect cycles
        if sorted_indices.len() != self.passes.len() {
            let mut unvisited: Vec<&'static str> = Vec::new();
            for pass in &self.passes {
                if !visited.contains(pass.name()) {
                    unvisited.push(pass.name());
                }
            }
            panic!("Circular dependency detected among passes: {:?}", unvisited);
        }

        // Reorder passes based on sorted indices
        let mut sorted_passes: Vec<Box<dyn LoweringPass>> = Vec::new();
        for idx in sorted_indices {
            // We need to temporarily take ownership - use swap_remove and rebuild
            sorted_passes.push(std::mem::replace(
                &mut self.passes[idx],
                Box::new(passes::CfgConstructionPass) // Dummy placeholder
            ));
        }

        self.passes = sorted_passes;
    }
}

/// Standard lowering passes
pub mod passes {
    use super::*;
    
    /// CFG construction pass
    pub struct CfgConstructionPass;
    
    impl LoweringPass for CfgConstructionPass {
        fn name(&self) -> &'static str {
            "cfg-construction"
        }
        
        fn dependencies(&self) -> &[&'static str] {
            &[]
        }
        
        fn run(&mut self, _context: &mut LoweringContext) -> CompilerResult<()> {
            // CFG is built during function lowering
            Ok(())
        }
    }
    
    /// SSA construction pass
    pub struct SsaConstructionPass;
    
    impl LoweringPass for SsaConstructionPass {
        fn name(&self) -> &'static str {
            "ssa-construction"
        }
        
        fn dependencies(&self) -> &[&'static str] {
            &["cfg-construction"]
        }
        
        fn run(&mut self, _context: &mut LoweringContext) -> CompilerResult<()> {
            // SSA is built during function lowering
            Ok(())
        }
    }
    
    /// Type validation pass
    pub struct TypeValidationPass;
    
    impl LoweringPass for TypeValidationPass {
        fn name(&self) -> &'static str {
            "type-validation"
        }
        
        fn dependencies(&self) -> &[&'static str] {
            &["ssa-construction"]
        }
        
        fn run(&mut self, context: &mut LoweringContext) -> CompilerResult<()> {
            // Validate all types in HIR
            let functions: Vec<_> = context.module.functions.values().cloned().collect();
            for func in &functions {
                Self::validate_function_types(func, context)?;
            }
            
            Ok(())
        }
    }
    
    impl TypeValidationPass {
        fn validate_function_types(func: &HirFunction, context: &mut LoweringContext) -> CompilerResult<()> {
            // Validate parameter types
            for param in &func.signature.params {
                if !Self::is_valid_hir_type(&param.ty) {
                    context.diagnostic(
                        DiagnosticLevel::Error,
                        format!("Invalid parameter type in function {}: {:?}", func.name, param.ty),
                        None,
                    );
                }
            }
            
            // Validate return types
            for ret_ty in &func.signature.returns {
                if !Self::is_valid_hir_type(ret_ty) {
                    context.diagnostic(
                        DiagnosticLevel::Error,
                        format!("Invalid return type in function {}: {:?}", func.name, ret_ty),
                        None,
                    );
                }
            }
            
            Ok(())
        }
        
        fn is_valid_hir_type(ty: &HirType) -> bool {
            match ty {
                HirType::Ptr(inner) => Self::is_valid_hir_type(inner),
                HirType::Array(inner, _) => Self::is_valid_hir_type(inner),
                HirType::Vector(inner, _) => Self::is_valid_hir_type(inner),
                HirType::Struct(s) => s.fields.iter().all(Self::is_valid_hir_type),
                HirType::Function(f) => {
                    f.params.iter().all(Self::is_valid_hir_type) &&
                    f.returns.iter().all(Self::is_valid_hir_type)
                }
                _ => true,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Test passes for topological sort
    struct PassA;
    impl LoweringPass for PassA {
        fn name(&self) -> &'static str { "pass-a" }
        fn dependencies(&self) -> &[&'static str] { &[] }
        fn run(&mut self, _context: &mut LoweringContext) -> CompilerResult<()> { Ok(()) }
    }

    struct PassB;
    impl LoweringPass for PassB {
        fn name(&self) -> &'static str { "pass-b" }
        fn dependencies(&self) -> &[&'static str] { &["pass-a"] }
        fn run(&mut self, _context: &mut LoweringContext) -> CompilerResult<()> { Ok(()) }
    }

    struct PassC;
    impl LoweringPass for PassC {
        fn name(&self) -> &'static str { "pass-c" }
        fn dependencies(&self) -> &[&'static str] { &["pass-a"] }
        fn run(&mut self, _context: &mut LoweringContext) -> CompilerResult<()> { Ok(()) }
    }

    struct PassD;
    impl LoweringPass for PassD {
        fn name(&self) -> &'static str { "pass-d" }
        fn dependencies(&self) -> &[&'static str] { &["pass-b", "pass-c"] }
        fn run(&mut self, _context: &mut LoweringContext) -> CompilerResult<()> { Ok(()) }
    }

    #[test]
    fn test_pass_topological_sort_simple() {
        // Test: B depends on A
        // Expected order: A, B
        let mut pipeline = LoweringPipeline { passes: Vec::new() };
        pipeline.passes.push(Box::new(PassB));
        pipeline.passes.push(Box::new(PassA));

        pipeline.sort_passes();

        assert_eq!(pipeline.passes.len(), 2);
        assert_eq!(pipeline.passes[0].name(), "pass-a");
        assert_eq!(pipeline.passes[1].name(), "pass-b");
    }

    #[test]
    fn test_pass_topological_sort_diamond() {
        // Test: Diamond dependency
        //       A
        //      / \
        //     B   C
        //      \ /
        //       D
        // Expected order: A, then B and C (either order), then D
        let mut pipeline = LoweringPipeline { passes: Vec::new() };
        pipeline.passes.push(Box::new(PassD));
        pipeline.passes.push(Box::new(PassC));
        pipeline.passes.push(Box::new(PassB));
        pipeline.passes.push(Box::new(PassA));

        pipeline.sort_passes();

        assert_eq!(pipeline.passes.len(), 4);
        assert_eq!(pipeline.passes[0].name(), "pass-a");
        // B and C can be in either order
        let middle: Vec<&str> = vec![pipeline.passes[1].name(), pipeline.passes[2].name()];
        assert!(middle.contains(&"pass-b"));
        assert!(middle.contains(&"pass-c"));
        assert_eq!(pipeline.passes[3].name(), "pass-d");
    }

    #[test]
    fn test_pass_topological_sort_independent() {
        // Test: Independent passes maintain stable order
        let mut pipeline = LoweringPipeline { passes: Vec::new() };
        pipeline.passes.push(Box::new(PassA));
        pipeline.passes.push(Box::new(PassC));

        let original_order = vec![
            pipeline.passes[0].name(),
            pipeline.passes[1].name(),
        ];

        pipeline.sort_passes();

        assert_eq!(pipeline.passes.len(), 2);
        let sorted_order = vec![
            pipeline.passes[0].name(),
            pipeline.passes[1].name(),
        ];

        // Independent passes keep their original relative order
        assert!(sorted_order == original_order);
    }

    #[test]
    #[should_panic(expected = "Circular dependency detected")]
    fn test_pass_circular_dependency() {
        // Test circular dependencies are detected
        struct PassX;
        impl LoweringPass for PassX {
            fn name(&self) -> &'static str { "pass-x" }
            fn dependencies(&self) -> &[&'static str] { &["pass-y"] }
            fn run(&mut self, _context: &mut LoweringContext) -> CompilerResult<()> { Ok(()) }
        }

        struct PassY;
        impl LoweringPass for PassY {
            fn name(&self) -> &'static str { "pass-y" }
            fn dependencies(&self) -> &[&'static str] { &["pass-x"] }
            fn run(&mut self, _context: &mut LoweringContext) -> CompilerResult<()> { Ok(()) }
        }

        let mut pipeline = LoweringPipeline { passes: Vec::new() };
        pipeline.passes.push(Box::new(PassX));
        pipeline.passes.push(Box::new(PassY));

        pipeline.sort_passes(); // Should panic
    }

    #[test]
    #[should_panic(expected = "depends on")]
    fn test_pass_missing_dependency() {
        // Test missing dependencies are detected
        struct PassMissing;
        impl LoweringPass for PassMissing {
            fn name(&self) -> &'static str { "pass-missing" }
            fn dependencies(&self) -> &[&'static str] { &["nonexistent-pass"] }
            fn run(&mut self, _context: &mut LoweringContext) -> CompilerResult<()> { Ok(()) }
        }

        let mut pipeline = LoweringPipeline { passes: Vec::new() };
        pipeline.passes.push(Box::new(PassMissing));

        pipeline.sort_passes(); // Should panic
    }
}