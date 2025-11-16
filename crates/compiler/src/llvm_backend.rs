// LLVM Backend Implementation for Zyntax
//
// This backend compiles HIR (High-level Intermediate Representation) to LLVM IR,
// enabling production-quality code generation with world-class optimizations.
//
// Architecture:
// - LLVMBackend: Main compiler struct managing LLVM context, module, and builder
// - Type Translation: Maps HIR types to LLVM types
// - Instruction Compilation: Converts HIR instructions to LLVM IR
// - Function Compilation: Handles function signatures, bodies, and calling conventions
//
// Use cases:
// 1. AOT (Ahead-of-Time): Full program optimization for production binaries
// 2. Tiered JIT: Optimize hot functions while keeping cold paths in Cranelift/VM
// 3. Profile-guided optimization: Recompile based on runtime profiling

use crate::{CompilerError, CompilerResult};
use crate::hir::{
    HirBlock, HirFunction, HirId, HirInstruction, HirModule, HirType, HirConstant,
    BinaryOp, UnaryOp, CastOp, HirCallable, HirValueKind, HirTerminator, HirPhi,
    HirGlobal, HirVTable,
};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, IntType},
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, PointerValue, PhiValue},
    basic_block::BasicBlock,
    AddressSpace, IntPredicate, FloatPredicate,
};
use std::collections::HashMap;

// Helper macro to convert inkwell errors to CompilerError
macro_rules! llvm_try {
    ($expr:expr) => {
        $expr.map_err(|e| CompilerError::CodeGen(format!("LLVM error: {}", e)))?
    };
}


/// Main LLVM backend compiler
///
/// Manages the LLVM context, module, and compilation state.
/// Lifetime 'ctx ties all LLVM objects to the context they were created in.
pub struct LLVMBackend<'ctx> {
    /// LLVM context - all types and values are tied to this
    context: &'ctx Context,

    /// LLVM module - container for all compiled functions and globals
    module: Module<'ctx>,

    /// LLVM IR builder - used to construct instructions
    builder: Builder<'ctx>,

    /// Maps HIR value IDs to compiled LLVM values
    value_map: HashMap<HirId, BasicValueEnum<'ctx>>,

    /// Maps HIR value IDs to their original HIR types (for indirect calls and other type lookups)
    type_map: HashMap<HirId, HirType>,

    /// Maps HIR function IDs to compiled LLVM functions
    functions: HashMap<HirId, FunctionValue<'ctx>>,

    /// Maps HIR basic block IDs to LLVM basic blocks
    block_map: HashMap<HirId, BasicBlock<'ctx>>,

    /// Maps HIR phi result IDs to LLVM phi nodes (for adding incoming edges later)
    phi_map: HashMap<HirId, PhiValue<'ctx>>,

    /// Current function being compiled (for accessing locals, blocks, etc.)
    current_function: Option<FunctionValue<'ctx>>,
}

impl<'ctx> LLVMBackend<'ctx> {
    /// Create a new LLVM backend
    ///
    /// # Arguments
    /// * `context` - LLVM context (must outlive the backend)
    /// * `module_name` - Name for the LLVM module
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        Self {
            context,
            module,
            builder,
            value_map: HashMap::new(),
            type_map: HashMap::new(),
            functions: HashMap::new(),
            block_map: HashMap::new(),
            phi_map: HashMap::new(),
            current_function: None,
        }
    }

    /// Compile an entire HIR module to LLVM IR
    ///
    /// This is the main entry point for compilation. It:
    /// 1. Processes global variables (including vtables)
    /// 2. Declares all functions (for forward references)
    /// 3. Compiles function bodies
    /// 4. Returns the compiled LLVM module
    pub fn compile_module(&mut self, hir_module: &HirModule) -> CompilerResult<String> {
        // Phase 1: Process globals first (including vtables)
        for (id, global) in &hir_module.globals {
            self.compile_global(*id, global)?;
        }

        // Phase 2: Declare all functions (allows forward references)
        for (id, func) in &hir_module.functions {
            self.declare_function(*id, func)?;
        }

        // Phase 3: Compile function bodies
        for (id, func) in &hir_module.functions {
            self.compile_function(*id, func)?;
        }

        // Return LLVM IR as string for inspection/debugging
        Ok(self.module.print_to_string().to_string())
    }

    /// Get a reference to the compiled LLVM module
    ///
    /// This is useful for creating an execution engine or writing to a file.
    pub fn module(&self) -> &Module<'ctx> {
        &self.module
    }

    /// Declare a function signature without compiling its body
    ///
    /// This allows other functions to call this one before it's fully compiled.
    fn declare_function(&mut self, id: HirId, func: &HirFunction) -> CompilerResult<FunctionValue<'ctx>> {
        // Translate parameter types
        let param_types: Vec<BasicMetadataTypeEnum> = func
            .signature
            .params
            .iter()
            .map(|param| self.translate_type(&param.ty).map(|t| t.into()))
            .collect::<CompilerResult<Vec<_>>>()?;

        // Translate return type
        let fn_type = if func.signature.returns.is_empty() {
            // Void function
            self.context.void_type().fn_type(&param_types, false)
        } else if func.signature.returns.len() == 1 {
            // Function returning a single value
            let return_type = self.translate_type(&func.signature.returns[0])?;
            return_type.fn_type(&param_types, false)
        } else {
            // Multiple return values - represent as struct (tuple)
            let return_types: Vec<BasicTypeEnum> = func.signature.returns
                .iter()
                .map(|ty| self.translate_type(ty))
                .collect::<CompilerResult<Vec<_>>>()?;

            let tuple_type = self.context.struct_type(&return_types, false);
            tuple_type.fn_type(&param_types, false)
        };

        // Add function to module
        // Gap 11: Use actual name for extern functions (for linking), mangled name for regular functions
        let fn_name = if func.is_external {
            // External functions use their actual name for linking
            func.name.to_string()
        } else {
            // Regular functions use mangled name with HirId
            format!("func_{:?}", id)
        };
        let fn_value = self.module.add_function(&fn_name, fn_type, None);

        // Set parameter names (helps with debugging IR)
        for (i, param) in func.signature.params.iter().enumerate() {
            let param_name = format!("param_{}", i);
            fn_value.get_nth_param(i as u32).unwrap().set_name(&param_name);
        }

        // Store for later reference
        self.functions.insert(id, fn_value);

        Ok(fn_value)
    }

    /// Compile a function body
    fn compile_function(&mut self, id: HirId, func: &HirFunction) -> CompilerResult<()> {
        let fn_value = self.functions[&id];
        self.current_function = Some(fn_value);

        // Skip external functions (declarations only)
        if func.is_external {
            self.current_function = None;
            return Ok(());
        }

        // Clear block, value, and phi maps for this function
        self.block_map.clear();
        self.phi_map.clear();
        // Note: We keep function parameters in value_map from declare_function

        // Map function parameters to HIR value IDs and store their types
        for (i, param) in func.signature.params.iter().enumerate() {
            let param_value = fn_value.get_nth_param(i as u32).unwrap();
            self.value_map.insert(param.id, param_value);
            self.type_map.insert(param.id, param.ty.clone());
        }

        // Map constant values and special instruction values to LLVM values
        for (value_id, value) in &func.values {
            match &value.kind {
                HirValueKind::Constant(constant) => {
                    let llvm_constant = self.compile_constant(constant)?;
                    self.value_map.insert(*value_id, llvm_constant);
                }
                HirValueKind::Instruction => {
                    // For instruction values that appear in func.values (like undef structs),
                    // create an undef value of the appropriate type
                    let llvm_type = self.translate_type(&value.ty)?;
                    let undef_value = llvm_type.const_zero(); // or use undef if available
                    self.value_map.insert(*value_id, undef_value);
                }
                _ => {}
            }
        }

        // Phase 1: Create LLVM basic blocks for all HIR blocks
        // IMPORTANT: Create entry block FIRST, as the first block added becomes the entry in LLVM IR
        let entry_block_name = format!("bb_{:?}", func.entry_block);
        let entry_llvm_block = self.context.append_basic_block(fn_value, &entry_block_name);
        self.block_map.insert(func.entry_block, entry_llvm_block);

        // Create remaining blocks
        for (block_id, _) in &func.blocks {
            if *block_id == func.entry_block {
                continue; // Already created
            }
            let block_name = format!("bb_{:?}", block_id);
            let llvm_block = self.context.append_basic_block(fn_value, &block_name);
            self.block_map.insert(*block_id, llvm_block);
        }

        // Phase 2: Compile all blocks in order (entry block first)
        // Start with entry block
        if let Some(entry_llvm_block) = self.block_map.get(&func.entry_block) {
            self.builder.position_at_end(*entry_llvm_block);

            if let Some(entry_hir_block) = func.blocks.get(&func.entry_block) {
                self.compile_block_with_terminator(&func.entry_block, entry_hir_block, func)?;
            }
        }

        // Compile remaining blocks
        for (block_id, hir_block) in &func.blocks {
            if *block_id == func.entry_block {
                continue; // Already compiled
            }

            if let Some(llvm_block) = self.block_map.get(block_id) {
                self.builder.position_at_end(*llvm_block);
                self.compile_block_with_terminator(block_id, hir_block, func)?;
            }
        }

        // Phase 3: Add incoming edges to phi nodes
        // Now that all blocks are compiled and all values are in value_map,
        // we can add the incoming edges to phi nodes
        for (_block_id, hir_block) in &func.blocks {
            for phi in &hir_block.phis {
                if let Some(phi_value) = self.phi_map.get(&phi.result) {
                    // Add incoming edges from each predecessor
                    for (value_id, pred_block_id) in &phi.incoming {
                        let incoming_value = self.get_value(*value_id)?;
                        let incoming_block = self.block_map.get(pred_block_id)
                            .ok_or_else(|| CompilerError::CodeGen(
                                format!("Phi node references unknown block: {:?}", pred_block_id)
                            ))?;
                        phi_value.add_incoming(&[(&incoming_value, *incoming_block)]);
                    }
                }
            }
        }

        // Phase 4: Verify the function
        // (LLVM will check that all blocks have terminators and phi nodes are valid)

        self.current_function = None;
        Ok(())
    }

    /// Compile a global variable (including vtables)
    ///
    /// This creates LLVM global variables with appropriate linkage and initializers.
    /// For vtables, the initializer contains an array of function pointers.
    fn compile_global(&mut self, id: HirId, global: &HirGlobal) -> CompilerResult<()> {
        // Translate the global's type to LLVM type
        let llvm_ty = self.translate_type(&global.ty)?;

        // Create unique name for the global
        let global_name = format!("global__{:?}", id);

        // Add global variable to module
        let global_value = self.module.add_global(llvm_ty, Some(AddressSpace::default()), &global_name);

        // Set linkage (export for now - could be internal for private globals)
        global_value.set_linkage(inkwell::module::Linkage::External);

        // Set initializer based on whether this is a vtable or regular global
        if let Some(HirConstant::VTable(vtable)) = &global.initializer {
            // Emit vtable as array of function pointers
            let ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());

            // Create array of function pointers
            let mut func_ptrs = Vec::new();
            for method_entry in &vtable.methods {
                if let Some(func_value) = self.functions.get(&method_entry.function_id) {
                    // Cast function to i8*
                    let func_ptr = func_value.as_global_value().as_pointer_value();
                    func_ptrs.push(func_ptr);
                } else {
                    eprintln!("WARNING: Vtable method function {:?} not found", method_entry.function_id);
                    // Use null pointer as fallback
                    func_ptrs.push(ptr_type.const_null());
                }
            }

            // Create constant array
            let vtable_array = ptr_type.const_array(&func_ptrs);
            global_value.set_initializer(&vtable_array);
        } else if let Some(initializer) = &global.initializer {
            // Other constants - compile them using compile_constant
            match self.compile_constant(initializer) {
                Ok(const_value) => {
                    global_value.set_initializer(&const_value);
                }
                Err(e) => {
                    eprintln!("WARNING: Failed to compile global initializer for {:?}: {}", id, e);
                    // Fall back to zero initializer
                    global_value.set_initializer(&llvm_ty.const_zero());
                }
            }
        } else {
            // Regular global without explicit initializer - use zero
            global_value.set_initializer(&llvm_ty.const_zero());
        }

        // Store the global in value_map so it can be referenced
        self.value_map.insert(id, global_value.as_pointer_value().into());

        Ok(())
    }

    /// Compile a basic block (instructions only, no terminator)
    fn compile_block(&mut self, block: &HirBlock) -> CompilerResult<()> {
        for instruction in &block.instructions {
            self.compile_instruction(instruction)?;
        }
        Ok(())
    }

    /// Compile a basic block with its terminator
    fn compile_block_with_terminator(
        &mut self,
        block_id: &HirId,
        block: &HirBlock,
        function: &HirFunction,
    ) -> CompilerResult<()> {
        // Compile phi nodes first
        for phi in &block.phis {
            self.compile_phi(phi, block_id, function)?;
        }

        // Compile instructions
        self.compile_block(block)?;

        // Compile terminator
        self.compile_terminator(&block.terminator)?;

        Ok(())
    }

    /// Compile a phi node
    fn compile_phi(
        &mut self,
        phi: &HirPhi,
        _current_block: &HirId,
        _function: &HirFunction,
    ) -> CompilerResult<()> {
        // Translate the phi node's type
        let llvm_ty = self.translate_type(&phi.ty)?;

        // Create the phi node
        let phi_value = self.builder.build_phi(llvm_ty, "phi")?;

        // Store the phi value in both maps:
        // - value_map: so other instructions can use it
        // - phi_map: so we can add incoming edges later (Phase 3)
        self.value_map.insert(phi.result, phi_value.as_basic_value());
        self.phi_map.insert(phi.result, phi_value);

        // Note: Incoming edges will be added in Phase 3 of compile_function
        // after all blocks are compiled and all values are available

        Ok(())
    }

    /// Compile a terminator instruction
    fn compile_terminator(&mut self, terminator: &HirTerminator) -> CompilerResult<()> {
        match terminator {
            HirTerminator::Return { values } => {
                if values.is_empty() {
                    self.builder.build_return(None)?;
                } else if values.len() == 1 {
                    let val = self.get_value(values[0])?;
                    self.builder.build_return(Some(&val))?;
                } else {
                    // Multiple return values - pack into a struct (tuple)
                    let return_values: Vec<BasicValueEnum> = values
                        .iter()
                        .map(|id| self.get_value(*id))
                        .collect::<CompilerResult<Vec<_>>>()?;

                    // Get the function's return type (should be a struct)
                    let fn_value = self.current_function.expect("No current function");
                    let fn_type = fn_value.get_type();
                    let return_type = fn_type.get_return_type()
                        .expect("Function should have a return type");

                    // Build the struct value
                    let mut tuple_value = return_type.into_struct_type().get_undef();
                    for (i, val) in return_values.iter().enumerate() {
                        tuple_value = self.builder.build_insert_value(
                            tuple_value,
                            *val,
                            i as u32,
                            &format!("tuple_field_{}", i)
                        )?.into_struct_value();
                    }

                    self.builder.build_return(Some(&tuple_value.as_basic_value_enum()))?;
                }
            }

            HirTerminator::Branch { target } => {
                let target_block = self.block_map.get(target)
                    .ok_or_else(|| CompilerError::CodeGen(
                        format!("Branch target block not found: {:?}", target)
                    ))?;
                self.builder.build_unconditional_branch(*target_block)?;
            }

            HirTerminator::CondBranch { condition, true_target, false_target } => {
                let cond = self.get_value(*condition)?;
                let true_block = self.block_map.get(true_target)
                    .ok_or_else(|| CompilerError::CodeGen(
                        format!("True branch target block not found: {:?}", true_target)
                    ))?;
                let false_block = self.block_map.get(false_target)
                    .ok_or_else(|| CompilerError::CodeGen(
                        format!("False branch target block not found: {:?}", false_target)
                    ))?;
                self.builder.build_conditional_branch(
                    cond.into_int_value(),
                    *true_block,
                    *false_block
                )?;
            }

            HirTerminator::Switch { value, default, cases } => {
                let switch_val = self.get_value(*value)?;
                let default_block = self.block_map.get(default)
                    .ok_or_else(|| CompilerError::CodeGen(
                        format!("Switch default block not found: {:?}", default)
                    ))?;

                // Build switch instruction with all cases at once
                let case_values: Vec<_> = cases.iter()
                    .map(|(const_val, target)| {
                        let target_block = self.block_map.get(target)
                            .ok_or_else(|| CompilerError::CodeGen(
                                format!("Switch case target block not found: {:?}", target)
                            ))?;

                        // Convert HIR constant to LLVM constant
                        let llvm_const = match const_val {
                            HirConstant::I8(v) => self.context.i8_type().const_int(*v as u64, true),
                            HirConstant::I16(v) => self.context.i16_type().const_int(*v as u64, true),
                            HirConstant::I32(v) => self.context.i32_type().const_int(*v as u64, true),
                            HirConstant::I64(v) => self.context.i64_type().const_int(*v as u64, true),
                            HirConstant::U8(v) => self.context.i8_type().const_int(*v as u64, false),
                            HirConstant::U16(v) => self.context.i16_type().const_int(*v as u64, false),
                            HirConstant::U32(v) => self.context.i32_type().const_int(*v as u64, false),
                            HirConstant::U64(v) => self.context.i64_type().const_int(*v, false),
                            _ => {
                                return Err(CompilerError::CodeGen(
                                    "Switch cases must be integer constants".to_string()
                                ));
                            }
                        };

                        Ok((llvm_const, *target_block))
                    })
                    .collect::<CompilerResult<Vec<_>>>()?;

                self.builder.build_switch(
                    switch_val.into_int_value(),
                    *default_block,
                    &case_values
                )?;
            }

            HirTerminator::Unreachable => {
                self.builder.build_unreachable()?;
            }

            HirTerminator::PatternMatch { value, patterns, default } => {
                // Pattern matching is lowered to a switch for now
                // Extract constant patterns
                let switch_val = self.get_value(*value)?;

                let default_target = default.ok_or_else(|| CompilerError::CodeGen(
                    "PatternMatch requires a default target".to_string()
                ))?;

                let default_block = self.block_map.get(&default_target)
                    .ok_or_else(|| CompilerError::CodeGen(
                        format!("Pattern match default block not found: {:?}", default_target)
                    ))?;

                // Build switch with pattern cases
                let pattern_cases: Vec<_> = patterns.iter()
                    .filter_map(|pattern| {
                        if let crate::hir::HirPatternKind::Constant(ref const_val) = pattern.kind {
                            let target_block = self.block_map.get(&pattern.target)?;

                            let llvm_const = match const_val {
                                HirConstant::I8(v) => self.context.i8_type().const_int(*v as u64, true),
                                HirConstant::I16(v) => self.context.i16_type().const_int(*v as u64, true),
                                HirConstant::I32(v) => self.context.i32_type().const_int(*v as u64, true),
                                HirConstant::I64(v) => self.context.i64_type().const_int(*v as u64, true),
                                _ => return None, // Skip non-integer patterns for now
                            };

                            Some((llvm_const, *target_block))
                        } else {
                            None
                        }
                    })
                    .collect();

                self.builder.build_switch(
                    switch_val.into_int_value(),
                    *default_block,
                    &pattern_cases
                )?;
            }

            _ => {
                return Err(CompilerError::CodeGen(
                    format!("Terminator not yet implemented: {:?}", terminator)
                ));
            }
        }

        Ok(())
    }

    /// Compile a single HIR instruction to LLVM IR
    fn compile_instruction(&mut self, instruction: &HirInstruction) -> CompilerResult<()> {
        match instruction {
            // ========== Arithmetic & Logic ==========
            HirInstruction::Binary { result, op, ty: _, left, right } => {
                let left_val = self.get_value(*left)?;
                let right_val = self.get_value(*right)?;
                let result_val = self.compile_binary_op(*op, left_val, right_val)?;
                self.value_map.insert(*result, result_val);
            }

            HirInstruction::Unary { result, op, ty: _, operand } => {
                let operand_val = self.get_value(*operand)?;
                let result_val = self.compile_unary_op(*op, operand_val)?;
                self.value_map.insert(*result, result_val);
            }

            HirInstruction::Select { result, ty, condition, true_val, false_val } => {
                let cond = self.get_value(*condition)?;
                let true_v = self.get_value(*true_val)?;
                let false_v = self.get_value(*false_val)?;
                let selected = self.builder.build_select(
                    cond.into_int_value(),
                    true_v,
                    false_v,
                    "select"
                )?;
                self.value_map.insert(*result, selected);
            }

            // ========== Type Conversions ==========
            HirInstruction::Cast { result, ty, op, operand } => {
                let operand_val = self.get_value(*operand)?;
                let target_ty = self.translate_type(ty)?;
                let casted = self.compile_cast(*op, operand_val, target_ty)?;
                self.value_map.insert(*result, casted);
            }

            // ========== Function Calls ==========
            HirInstruction::Call { result, callee, args, type_args: _, const_args: _, is_tail: _ } => {
                let result_val = self.compile_call(callee, args)?;
                if let Some(res_id) = result {
                    self.value_map.insert(*res_id, result_val);
                }
            }

            // ========== Memory Operations ==========
            HirInstruction::Load { result, ty, ptr, align: _, volatile: _ } => {
                let addr = self.get_value(*ptr)?;
                let ptr_val = addr.into_pointer_value();
                let llvm_ty = self.translate_type(ty)?;
                let loaded = self.builder.build_load(llvm_ty, ptr_val, "load")?;
                self.value_map.insert(*result, loaded);
            }

            HirInstruction::Store { value, ptr, align: _, volatile: _ } => {
                let addr = self.get_value(*ptr)?;
                let val = self.get_value(*value)?;
                let ptr_val = addr.into_pointer_value();
                self.builder.build_store(ptr_val, val)?;
            }

            HirInstruction::Alloca { result, ty, count, align: _ } => {
                let llvm_ty = self.translate_type(ty)?;
                let alloca = if let Some(count_id) = count {
                    // Array allocation
                    let count_val = self.get_value(*count_id)?;
                    self.builder.build_array_alloca(llvm_ty, count_val.into_int_value(), "array_alloca")?
                } else {
                    // Single value allocation
                    self.builder.build_alloca(llvm_ty, "alloca")?
                };
                self.value_map.insert(*result, alloca.into());
            }

            HirInstruction::GetElementPtr { result, ty, ptr, indices } => {
                let ptr_val = self.get_value(*ptr)?;
                let llvm_ty = self.translate_type(ty)?;

                // Convert all index HIR values to LLVM values
                let index_values: Vec<_> = indices
                    .iter()
                    .map(|idx| self.get_value(*idx).map(|v| v.into_int_value()))
                    .collect::<CompilerResult<Vec<_>>>()?;

                // Use GEP to compute the address
                let gep_result = unsafe {
                    self.builder.build_gep(
                        llvm_ty,
                        ptr_val.into_pointer_value(),
                        &index_values,
                        "gep"
                    )?
                };
                self.value_map.insert(*result, gep_result.into());
            }

            // ========== Aggregate Operations ==========
            HirInstruction::ExtractValue { result, ty, aggregate, indices } => {
                let mut current_value = self.get_value(*aggregate)?;

                // Extract value from struct or array using chained extraction
                // LLVM's build_extract_value only takes a single index, so we need to
                // apply it iteratively for nested access
                if indices.is_empty() {
                    return Err(CompilerError::CodeGen(
                        "ExtractValue requires at least one index".to_string()
                    ));
                }

                // Apply each index in sequence for nested extraction
                for (i, &index) in indices.iter().enumerate() {
                    let is_last = i == indices.len() - 1;
                    let name = if is_last {
                        "extract"
                    } else {
                        &format!("extract_nested_{}", i)
                    };

                    // Try to extract from struct
                    if let Ok(struct_val) = TryInto::<inkwell::values::StructValue>::try_into(current_value) {
                        let extracted = self.builder.build_extract_value(
                            struct_val,
                            index,
                            name
                        )?;
                        current_value = extracted.as_basic_value_enum();
                    } else if let Ok(array_val) = TryInto::<inkwell::values::ArrayValue>::try_into(current_value) {
                        // For arrays, we can also use extract_value
                        let extracted = self.builder.build_extract_value(
                            array_val,
                            index,
                            name
                        )?;
                        current_value = extracted.as_basic_value_enum();
                    } else {
                        return Err(CompilerError::CodeGen(
                            format!("ExtractValue can only be used on struct or array types, got: {:?}", current_value.get_type())
                        ));
                    }
                }

                self.value_map.insert(*result, current_value);
            }

            HirInstruction::InsertValue { result, ty, aggregate, value, indices } => {
                let mut current_agg = self.get_value(*aggregate)?;
                let val = self.get_value(*value)?;

                if indices.is_empty() {
                    return Err(CompilerError::CodeGen(
                        "InsertValue requires at least one index".to_string()
                    ));
                }

                // For nested insertion, we need to:
                // 1. Extract the nested aggregate at indices[0..n-1]
                // 2. Insert value at the final index
                // 3. Insert the modified nested aggregate back
                if indices.len() == 1 {
                    // Simple case: single-level insertion
                    let inserted = if let Ok(struct_val) = TryInto::<inkwell::values::StructValue>::try_into(current_agg) {
                        self.builder.build_insert_value(
                            struct_val,
                            val,
                            indices[0],
                            "insert"
                        )?
                    } else if let Ok(array_val) = TryInto::<inkwell::values::ArrayValue>::try_into(current_agg) {
                        self.builder.build_insert_value(
                            array_val,
                            val,
                            indices[0],
                            "insert"
                        )?
                    } else {
                        return Err(CompilerError::CodeGen(
                            format!("InsertValue can only be used on struct or array types, got: {:?}", current_agg.get_type())
                        ));
                    };
                    self.value_map.insert(*result, inserted.as_basic_value_enum());
                } else {
                    // Nested insertion: extract nested aggregate, insert value, then insert back
                    let mut extracted_path = Vec::new();

                    // Extract the nested aggregate at all indices except the last
                    let mut nested_agg = current_agg;
                    for &index in &indices[..indices.len() - 1] {
                        if let Ok(struct_val) = TryInto::<inkwell::values::StructValue>::try_into(nested_agg) {
                            let extracted = self.builder.build_extract_value(
                                struct_val,
                                index,
                                "nested_extract_for_insert"
                            )?;
                            extracted_path.push(index);
                            nested_agg = extracted.as_basic_value_enum();
                        } else if let Ok(array_val) = TryInto::<inkwell::values::ArrayValue>::try_into(nested_agg) {
                            let extracted = self.builder.build_extract_value(
                                array_val,
                                index,
                                "nested_extract_for_insert"
                            )?;
                            extracted_path.push(index);
                            nested_agg = extracted.as_basic_value_enum();
                        } else {
                            return Err(CompilerError::CodeGen(
                                format!("Nested InsertValue path contains non-aggregate type: {:?}", nested_agg.get_type())
                            ));
                        }
                    }

                    // Insert the value at the final index in the nested aggregate
                    let final_index = indices[indices.len() - 1];
                    let modified_nested = if let Ok(struct_val) = TryInto::<inkwell::values::StructValue>::try_into(nested_agg) {
                        self.builder.build_insert_value(
                            struct_val,
                            val,
                            final_index,
                            "nested_insert"
                        )?
                    } else if let Ok(array_val) = TryInto::<inkwell::values::ArrayValue>::try_into(nested_agg) {
                        self.builder.build_insert_value(
                            array_val,
                            val,
                            final_index,
                            "nested_insert"
                        )?
                    } else {
                        return Err(CompilerError::CodeGen(
                            format!("Cannot insert into non-aggregate type: {:?}", nested_agg.get_type())
                        ));
                    };

                    // Now insert the modified nested aggregate back into the original
                    // We need to work backwards through the path, inserting at each level
                    let mut current_value = modified_nested.as_basic_value_enum();

                    // Start with the original aggregate
                    let mut result_agg = current_agg;

                    // Re-extract and rebuild the path, inserting the modified value
                    // This is complex, so for now we'll use a simpler approach:
                    // Build the insertion from the innermost to outermost
                    for (depth, &index) in extracted_path.iter().enumerate().rev() {
                        // Extract up to this depth
                        let mut temp_agg = current_agg;
                        for &idx in &extracted_path[..depth] {
                            if let Ok(struct_val) = TryInto::<inkwell::values::StructValue>::try_into(temp_agg) {
                                let extracted = self.builder.build_extract_value(
                                    struct_val,
                                    idx,
                                    "rebuild_extract"
                                )?;
                                temp_agg = extracted.as_basic_value_enum();
                            } else if let Ok(array_val) = TryInto::<inkwell::values::ArrayValue>::try_into(temp_agg) {
                                let extracted = self.builder.build_extract_value(
                                    array_val,
                                    idx,
                                    "rebuild_extract"
                                )?;
                                temp_agg = extracted.as_basic_value_enum();
                            }
                        }

                        // Insert the current value at this index
                        current_value = if let Ok(struct_val) = TryInto::<inkwell::values::StructValue>::try_into(temp_agg) {
                            self.builder.build_insert_value(
                                struct_val,
                                current_value,
                                index,
                                &format!("rebuild_insert_{}", depth)
                            )?.as_basic_value_enum()
                        } else if let Ok(array_val) = TryInto::<inkwell::values::ArrayValue>::try_into(temp_agg) {
                            self.builder.build_insert_value(
                                array_val,
                                current_value,
                                index,
                                &format!("rebuild_insert_{}", depth)
                            )?.as_basic_value_enum()
                        } else {
                            return Err(CompilerError::CodeGen(
                                "InsertValue rebuild failed: non-aggregate type".to_string()
                            ));
                        };
                    }

                    self.value_map.insert(*result, current_value);
                }
            }

            // ========== Atomic Operations ==========
            HirInstruction::Atomic { op, result, ty, ptr, value, ordering } => {
                return Err(CompilerError::CodeGen(
                    "Atomic operations not yet implemented in LLVM backend".to_string()
                ));
            }

            HirInstruction::Fence { ordering } => {
                // Memory fence
                return Err(CompilerError::CodeGen(
                    "Fence instruction not yet implemented in LLVM backend".to_string()
                ));
            }

            // ========== Union Type Operations ==========
            HirInstruction::CreateUnion { result, union_ty, variant_index, value } => {
                return Err(CompilerError::CodeGen(
                    "Union operations not yet implemented in LLVM backend".to_string()
                ));
            }

            HirInstruction::GetUnionDiscriminant { result, union_val } => {
                return Err(CompilerError::CodeGen(
                    "Union discriminant not yet implemented in LLVM backend".to_string()
                ));
            }

            HirInstruction::ExtractUnionValue { result, ty, union_val, variant_index } => {
                return Err(CompilerError::CodeGen(
                    "Union extraction not yet implemented in LLVM backend".to_string()
                ));
            }

            // ========== Closure Operations ==========
            HirInstruction::CreateClosure { result, closure_ty, function, captures } => {
                return Err(CompilerError::CodeGen(
                    "Closure creation not yet implemented in LLVM backend".to_string()
                ));
            }

            HirInstruction::CallClosure { result, closure, args } => {
                return Err(CompilerError::CodeGen(
                    "Closure calls not yet implemented in LLVM backend".to_string()
                ));
            }

            // ========== Trait Objects ==========
            HirInstruction::CreateTraitObject { result, trait_id, data_ptr, vtable_id } => {
                // Create trait object as fat pointer: { *data, *vtable }
                // This matches Cranelift's implementation for backend parity

                let data_ptr_val = self.get_value(*data_ptr)?;
                let vtable_ptr_val = self.get_value(*vtable_id)?;

                // Create a struct type for the fat pointer (two pointers)
                let ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
                let fat_ptr_type = self.context.struct_type(&[ptr_type.into(), ptr_type.into()], false);

                // Allocate space for fat pointer on stack
                let fat_ptr_alloca = self.builder.build_alloca(fat_ptr_type, "trait_obj")?;

                // Store data pointer at field 0
                let data_field_ptr = self.builder.build_struct_gep(fat_ptr_type, fat_ptr_alloca, 0, "data_field")?;
                self.builder.build_store(data_field_ptr, data_ptr_val)?;

                // Store vtable pointer at field 1
                let vtable_field_ptr = self.builder.build_struct_gep(fat_ptr_type, fat_ptr_alloca, 1, "vtable_field")?;
                self.builder.build_store(vtable_field_ptr, vtable_ptr_val)?;

                // Return the fat pointer (as pointer to struct)
                self.value_map.insert(*result, fat_ptr_alloca.into());
            }

            HirInstruction::UpcastTraitObject { result, sub_trait_object, sub_trait_id, super_trait_id, super_vtable_id } => {
                // Upcast trait object: extract data pointer from sub-trait, combine with super-trait vtable
                // Fat pointer layout: struct { *data, *vtable }

                let ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
                let fat_ptr_type = self.context.struct_type(&[ptr_type.into(), ptr_type.into()], false);

                // Step 1: Get sub-trait fat pointer
                let sub_trait_fat_ptr = self.get_value(*sub_trait_object)?
                    .into_pointer_value();

                // Step 2: Extract data pointer from sub-trait object (field 0)
                let data_field_ptr = self.builder.build_struct_gep(fat_ptr_type, sub_trait_fat_ptr, 0, "data_field")?;
                let data_ptr = self.builder.build_load(ptr_type, data_field_ptr, "data_ptr")?;

                // Step 3: Get super-trait vtable pointer
                let super_vtable_ptr = self.get_value(*super_vtable_id)?;

                // Step 4: Allocate space for new super-trait fat pointer on stack
                let super_trait_fat_ptr_alloca = self.builder.build_alloca(fat_ptr_type, "super_trait_obj")?;

                // Step 5: Store data pointer at field 0 (same as sub-trait)
                let super_data_field_ptr = self.builder.build_struct_gep(fat_ptr_type, super_trait_fat_ptr_alloca, 0, "super_data_field")?;
                self.builder.build_store(super_data_field_ptr, data_ptr)?;

                // Step 6: Store super-trait vtable pointer at field 1
                let super_vtable_field_ptr = self.builder.build_struct_gep(fat_ptr_type, super_trait_fat_ptr_alloca, 1, "super_vtable_field")?;
                self.builder.build_store(super_vtable_field_ptr, super_vtable_ptr)?;

                // Return the new fat pointer
                self.value_map.insert(*result, super_trait_fat_ptr_alloca.into());
            }

            HirInstruction::TraitMethodCall { result, trait_object, method_index, method_sig, args, return_ty } => {
                // Dynamic dispatch: call method on trait object
                // Fat pointer layout: struct { *data, *vtable }

                let ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
                let fat_ptr_type = self.context.struct_type(&[ptr_type.into(), ptr_type.into()], false);

                // Get fat pointer (trait object)
                let fat_ptr = self.get_value(*trait_object)?
                    .into_pointer_value();

                // Step 1: Load data pointer from fat_ptr.field[0]
                let data_field_ptr = self.builder.build_struct_gep(fat_ptr_type, fat_ptr, 0, "data_field")?;
                let data_ptr = self.builder.build_load(ptr_type, data_field_ptr, "data_ptr")?
                    .into_pointer_value();

                // Step 2: Load vtable pointer from fat_ptr.field[1]
                let vtable_field_ptr = self.builder.build_struct_gep(fat_ptr_type, fat_ptr, 1, "vtable_field")?;
                let vtable_ptr = self.builder.build_load(ptr_type, vtable_field_ptr, "vtable_ptr")?
                    .into_pointer_value();

                // Step 3: Load function pointer from vtable[method_index]
                // Vtable is an array of function pointers
                let method_index_val = self.context.i32_type().const_int(*method_index as u64, false);
                let func_ptr_ptr = unsafe {
                    self.builder.build_gep(
                        ptr_type,
                        vtable_ptr,
                        &[method_index_val],
                        "func_ptr_ptr"
                    )?
                };
                let func_ptr = self.builder.build_load(ptr_type, func_ptr_ptr, "func_ptr")?
                    .into_pointer_value();

                // Step 4: Build arguments: prepend self (data_ptr) to args
                let mut call_args: Vec<inkwell::values::BasicMetadataValueEnum> = vec![data_ptr.into()];
                for arg_id in args {
                    let arg_val = self.get_value(*arg_id)?;
                    call_args.push(arg_val.into());
                }

                // Step 5: Create function type for indirect call
                // First parameter is always self (data pointer)
                let mut param_types: Vec<inkwell::types::BasicMetadataTypeEnum> = vec![ptr_type.into()]; // self

                // Translate actual parameter types from method signature
                for param_ty in &method_sig.params {
                    let llvm_ty = self.translate_type(param_ty)?;
                    param_types.push(llvm_ty.into());
                }

                // Translate return type from method signature
                let return_type = if matches!(method_sig.return_type, HirType::Void) {
                    None
                } else {
                    let ret_ty = self.translate_type(&method_sig.return_type)?;
                    Some(ret_ty)
                };

                let func_type = if let Some(ret_ty) = return_type {
                    ret_ty.fn_type(&param_types, false)
                } else {
                    self.context.void_type().fn_type(&param_types, false)
                };

                // Step 6: Cast function pointer to correct type
                let typed_func_ptr = self.builder.build_pointer_cast(
                    func_ptr,
                    func_type.ptr_type(AddressSpace::default()),
                    "typed_func_ptr"
                )?;

                // Step 7: Perform indirect call
                let call_site = self.builder.build_indirect_call(
                    func_type,
                    typed_func_ptr,
                    &call_args,
                    "trait_method_call"
                )?;

                // Step 8: Get return value if non-void
                if let Some(result_id) = result {
                    if let Some(return_val) = call_site.try_as_basic_value().left() {
                        self.value_map.insert(*result_id, return_val);
                    }
                }
            }

            _ => {
                return Err(CompilerError::CodeGen(
                    format!("Instruction not yet implemented: {:?}", instruction)
                ));
            }
        }
        Ok(())
    }

    /// Compile a binary operation
    fn compile_binary_op(
        &mut self,
        op: BinaryOp,
        left: BasicValueEnum<'ctx>,
        right: BasicValueEnum<'ctx>,
    ) -> CompilerResult<BasicValueEnum<'ctx>> {
        use BinaryOp::*;

        let result = match op {
            // Integer arithmetic
            Add => {
                if left.is_int_value() {
                    self.builder.build_int_add(
                        left.into_int_value(),
                        right.into_int_value(),
                        "add"
                    )?.into()
                } else {
                    self.builder.build_float_add(
                        left.into_float_value(),
                        right.into_float_value(),
                        "fadd"
                    )?.into()
                }
            }
            Sub => {
                if left.is_int_value() {
                    self.builder.build_int_sub(
                        left.into_int_value(),
                        right.into_int_value(),
                        "sub"
                    )?.into()
                } else {
                    self.builder.build_float_sub(
                        left.into_float_value(),
                        right.into_float_value(),
                        "fsub"
                    )?.into()
                }
            }
            Mul => {
                if left.is_int_value() {
                    self.builder.build_int_mul(
                        left.into_int_value(),
                        right.into_int_value(),
                        "mul"
                    )?.into()
                } else {
                    self.builder.build_float_mul(
                        left.into_float_value(),
                        right.into_float_value(),
                        "fmul"
                    )?.into()
                }
            }
            Div => {
                if left.is_int_value() {
                    self.builder.build_int_signed_div(
                        left.into_int_value(),
                        right.into_int_value(),
                        "div"
                    )?.into()
                } else {
                    self.builder.build_float_div(
                        left.into_float_value(),
                        right.into_float_value(),
                        "fdiv"
                    )?.into()
                }
            }
            Rem => {
                if left.is_int_value() {
                    self.builder.build_int_signed_rem(
                        left.into_int_value(),
                        right.into_int_value(),
                        "rem"
                    )?.into()
                } else {
                    self.builder.build_float_rem(
                        left.into_float_value(),
                        right.into_float_value(),
                        "frem"
                    )?.into()
                }
            }

            // Bitwise operations
            And => {
                self.builder.build_and(
                    left.into_int_value(),
                    right.into_int_value(),
                    "and"
                )?.into()
            }
            Or => {
                self.builder.build_or(
                    left.into_int_value(),
                    right.into_int_value(),
                    "or"
                )?.into()
            }
            Xor => {
                self.builder.build_xor(
                    left.into_int_value(),
                    right.into_int_value(),
                    "xor"
                )?.into()
            }
            Shl => {
                self.builder.build_left_shift(
                    left.into_int_value(),
                    right.into_int_value(),
                    "shl"
                )?.into()
            }
            Shr => {
                self.builder.build_right_shift(
                    left.into_int_value(),
                    right.into_int_value(),
                    true,  // arithmetic shift (sign-extend)
                    "shr"
                )?.into()
            }

            // Comparison operations
            Eq => {
                if left.is_int_value() {
                    self.builder.build_int_compare(
                        IntPredicate::EQ,
                        left.into_int_value(),
                        right.into_int_value(),
                        "eq"
                    )?.into()
                } else {
                    self.builder.build_float_compare(
                        FloatPredicate::OEQ,
                        left.into_float_value(),
                        right.into_float_value(),
                        "feq"
                    )?.into()
                }
            }
            Ne => {
                if left.is_int_value() {
                    self.builder.build_int_compare(
                        IntPredicate::NE,
                        left.into_int_value(),
                        right.into_int_value(),
                        "ne"
                    )?.into()
                } else {
                    self.builder.build_float_compare(
                        FloatPredicate::ONE,
                        left.into_float_value(),
                        right.into_float_value(),
                        "fne"
                    )?.into()
                }
            }
            Lt => {
                if left.is_int_value() {
                    self.builder.build_int_compare(
                        IntPredicate::SLT,
                        left.into_int_value(),
                        right.into_int_value(),
                        "lt"
                    )?.into()
                } else {
                    self.builder.build_float_compare(
                        FloatPredicate::OLT,
                        left.into_float_value(),
                        right.into_float_value(),
                        "flt"
                    )?.into()
                }
            }
            Le => {
                if left.is_int_value() {
                    self.builder.build_int_compare(
                        IntPredicate::SLE,
                        left.into_int_value(),
                        right.into_int_value(),
                        "le"
                    )?.into()
                } else {
                    self.builder.build_float_compare(
                        FloatPredicate::OLE,
                        left.into_float_value(),
                        right.into_float_value(),
                        "fle"
                    )?.into()
                }
            }
            Gt => {
                if left.is_int_value() {
                    self.builder.build_int_compare(
                        IntPredicate::SGT,
                        left.into_int_value(),
                        right.into_int_value(),
                        "gt"
                    )?.into()
                } else {
                    self.builder.build_float_compare(
                        FloatPredicate::OGT,
                        left.into_float_value(),
                        right.into_float_value(),
                        "fgt"
                    )?.into()
                }
            }
            Ge => {
                if left.is_int_value() {
                    self.builder.build_int_compare(
                        IntPredicate::SGE,
                        left.into_int_value(),
                        right.into_int_value(),
                        "ge"
                    )?.into()
                } else {
                    self.builder.build_float_compare(
                        FloatPredicate::OGE,
                        left.into_float_value(),
                        right.into_float_value(),
                        "fge"
                    )?.into()
                }
            }

            // Explicit floating-point operations (for when type is already known)
            FAdd => {
                self.builder.build_float_add(
                    left.into_float_value(),
                    right.into_float_value(),
                    "fadd"
                )?.into()
            }
            FSub => {
                self.builder.build_float_sub(
                    left.into_float_value(),
                    right.into_float_value(),
                    "fsub"
                )?.into()
            }
            FMul => {
                self.builder.build_float_mul(
                    left.into_float_value(),
                    right.into_float_value(),
                    "fmul"
                )?.into()
            }
            FDiv => {
                self.builder.build_float_div(
                    left.into_float_value(),
                    right.into_float_value(),
                    "fdiv"
                )?.into()
            }
            FRem => {
                self.builder.build_float_rem(
                    left.into_float_value(),
                    right.into_float_value(),
                    "frem"
                )?.into()
            }
            FEq => {
                self.builder.build_float_compare(
                    FloatPredicate::OEQ,
                    left.into_float_value(),
                    right.into_float_value(),
                    "feq"
                )?.into()
            }
            FNe => {
                self.builder.build_float_compare(
                    FloatPredicate::ONE,
                    left.into_float_value(),
                    right.into_float_value(),
                    "fne"
                )?.into()
            }
            FLt => {
                self.builder.build_float_compare(
                    FloatPredicate::OLT,
                    left.into_float_value(),
                    right.into_float_value(),
                    "flt"
                )?.into()
            }
            FLe => {
                self.builder.build_float_compare(
                    FloatPredicate::OLE,
                    left.into_float_value(),
                    right.into_float_value(),
                    "fle"
                )?.into()
            }
            FGt => {
                self.builder.build_float_compare(
                    FloatPredicate::OGT,
                    left.into_float_value(),
                    right.into_float_value(),
                    "fgt"
                )?.into()
            }
            FGe => {
                self.builder.build_float_compare(
                    FloatPredicate::OGE,
                    left.into_float_value(),
                    right.into_float_value(),
                    "fge"
                )?.into()
            }
        };

        Ok(result)
    }

    /// Compile a unary operation
    fn compile_unary_op(
        &mut self,
        op: UnaryOp,
        operand: BasicValueEnum<'ctx>,
    ) -> CompilerResult<BasicValueEnum<'ctx>> {
        use UnaryOp::*;

        let result = match op {
            Neg => {
                if operand.is_int_value() {
                    self.builder.build_int_neg(operand.into_int_value(), "neg")?.into()
                } else {
                    self.builder.build_float_neg(operand.into_float_value(), "fneg")?.into()
                }
            }
            Not => {
                self.builder.build_not(operand.into_int_value(), "not")?.into()
            }
            FNeg => {
                self.builder.build_float_neg(operand.into_float_value(), "fneg")?.into()
            }
        };

        Ok(result)
    }

    /// Compile a cast operation
    fn compile_cast(
        &mut self,
        op: CastOp,
        operand: BasicValueEnum<'ctx>,
        target_ty: BasicTypeEnum<'ctx>,
    ) -> CompilerResult<BasicValueEnum<'ctx>> {
        use CastOp::*;

        let result = match op {
            // Integer truncation (i64 -> i32, etc.)
            Trunc => {
                self.builder.build_int_truncate(
                    operand.into_int_value(),
                    target_ty.into_int_type(),
                    "trunc"
                )?.into()
            }

            // Zero extension (unsigned: i32 -> i64, etc.)
            ZExt => {
                self.builder.build_int_z_extend(
                    operand.into_int_value(),
                    target_ty.into_int_type(),
                    "zext"
                )?.into()
            }

            // Sign extension (signed: i32 -> i64, etc.)
            SExt => {
                self.builder.build_int_s_extend(
                    operand.into_int_value(),
                    target_ty.into_int_type(),
                    "sext"
                )?.into()
            }

            // Float truncation (f64 -> f32)
            FpTrunc => {
                self.builder.build_float_trunc(
                    operand.into_float_value(),
                    target_ty.into_float_type(),
                    "fptrunc"
                )?.into()
            }

            // Float extension (f32 -> f64)
            FpExt => {
                self.builder.build_float_ext(
                    operand.into_float_value(),
                    target_ty.into_float_type(),
                    "fpext"
                )?.into()
            }

            // Float to unsigned int
            FpToUi => {
                self.builder.build_float_to_unsigned_int(
                    operand.into_float_value(),
                    target_ty.into_int_type(),
                    "fptoui"
                )?.into()
            }

            // Float to signed int
            FpToSi => {
                self.builder.build_float_to_signed_int(
                    operand.into_float_value(),
                    target_ty.into_int_type(),
                    "fptosi"
                )?.into()
            }

            // Unsigned int to float
            UiToFp => {
                self.builder.build_unsigned_int_to_float(
                    operand.into_int_value(),
                    target_ty.into_float_type(),
                    "uitofp"
                )?.into()
            }

            // Signed int to float
            SiToFp => {
                self.builder.build_signed_int_to_float(
                    operand.into_int_value(),
                    target_ty.into_float_type(),
                    "sitofp"
                )?.into()
            }

            // Pointer to integer
            PtrToInt => {
                self.builder.build_ptr_to_int(
                    operand.into_pointer_value(),
                    target_ty.into_int_type(),
                    "ptrtoint"
                )?.into()
            }

            // Integer to pointer
            IntToPtr => {
                self.builder.build_int_to_ptr(
                    operand.into_int_value(),
                    target_ty.into_pointer_type(),
                    "inttoptr"
                )?.into()
            }

            // Bitcast (reinterpret bits as different type)
            Bitcast => {
                self.builder.build_bitcast(
                    operand,
                    target_ty,
                    "bitcast"
                )?.into()
            }
        };

        Ok(result)
    }

    /// Compile a function call
    fn compile_call(
        &mut self,
        callee: &HirCallable,
        args: &[HirId],
    ) -> CompilerResult<BasicValueEnum<'ctx>> {
        match callee {
            HirCallable::Function(func_id) => {
                // Direct function call
                let function = self.functions.get(func_id)
                    .ok_or_else(|| CompilerError::CodeGen(
                        format!("Function not found: {:?}", func_id)
                    ))?;

                // Compile arguments
                let arg_values: Vec<BasicMetadataValueEnum> = args
                    .iter()
                    .map(|arg_id| {
                        self.get_value(*arg_id)
                            .map(|v| v.into())
                    })
                    .collect::<CompilerResult<Vec<_>>>()?;

                // Build call
                let call_site = self.builder.build_call(*function, &arg_values, "call")?;

                // Return value (or void)
                call_site.try_as_basic_value()
                    .left()
                    .ok_or_else(|| CompilerError::CodeGen(
                        "Function call returned void when value expected".to_string()
                    ))
            }
            HirCallable::Indirect(func_ptr_id) => {
                // Indirect call through function pointer
                let func_ptr_val = self.get_value(*func_ptr_id)?;

                // The function pointer should be a pointer value
                let func_ptr = func_ptr_val.into_pointer_value();

                // Get the HIR type for this function pointer to extract the signature
                let hir_type = self.type_map.get(func_ptr_id)
                    .ok_or_else(|| CompilerError::CodeGen(
                        format!("Type not found for function pointer: {:?}", func_ptr_id)
                    ))?;

                // Extract the function type from the HIR type
                let func_hir_type = match hir_type {
                    HirType::Function(ft) => ft.as_ref(),
                    _ => return Err(CompilerError::CodeGen(
                        format!("Expected function type for indirect call, got: {:?}", hir_type)
                    )),
                };

                // Translate the HIR function type to LLVM function type
                let param_types: Result<Vec<BasicMetadataTypeEnum>, _> = func_hir_type.params
                    .iter()
                    .map(|param_ty| {
                        self.translate_type(param_ty)
                            .map(|t| t.into())
                    })
                    .collect();
                let param_types = param_types?;

                // Handle return type
                let fn_type = if func_hir_type.returns.is_empty() {
                    self.context.void_type().fn_type(&param_types, func_hir_type.is_variadic)
                } else if func_hir_type.returns.len() == 1 {
                    let ret_ty = self.translate_type(&func_hir_type.returns[0])?;
                    ret_ty.fn_type(&param_types, func_hir_type.is_variadic)
                } else {
                    let ret_types: Result<Vec<BasicTypeEnum>, _> = func_hir_type.returns
                        .iter()
                        .map(|ret_ty| self.translate_type(ret_ty))
                        .collect();
                    let ret_types = ret_types?;
                    let struct_ret = self.context.struct_type(&ret_types, false);
                    struct_ret.fn_type(&param_types, func_hir_type.is_variadic)
                };

                // Compile arguments
                let arg_values: Vec<BasicMetadataValueEnum> = args
                    .iter()
                    .map(|arg_id| {
                        self.get_value(*arg_id)
                            .map(|v| v.into())
                    })
                    .collect::<CompilerResult<Vec<_>>>()?;

                // Build indirect call
                let call_site = self.builder.build_indirect_call(
                    fn_type,
                    func_ptr,
                    &arg_values,
                    "indirect_call"
                )?;

                // Return value (or void)
                call_site.try_as_basic_value()
                    .left()
                    .ok_or_else(|| CompilerError::CodeGen(
                        "Function call returned void when value expected".to_string()
                    ))
            }
            HirCallable::Intrinsic(intrinsic) => {
                self.compile_intrinsic(*intrinsic, args)
            }
        }
    }

    /// Compile an intrinsic function call
    fn compile_intrinsic(
        &mut self,
        intrinsic: crate::hir::Intrinsic,
        args: &[HirId],
    ) -> CompilerResult<BasicValueEnum<'ctx>> {
        use crate::hir::Intrinsic::*;

        match intrinsic {
            // ========== Memory Management ==========
            Malloc => {
                // malloc(size: usize) -> *mut u8
                if args.len() != 1 {
                    return Err(CompilerError::CodeGen(
                        format!("malloc expects 1 argument, got {}", args.len())
                    ));
                }

                let size = self.get_value(args[0])?;

                // Declare or get malloc function: declare ptr @malloc(i64)
                let malloc_fn = self.module.get_function("malloc").unwrap_or_else(|| {
                    let i64_type = self.context.i64_type();
                    let ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
                    let fn_type = ptr_type.fn_type(&[i64_type.into()], false);
                    self.module.add_function("malloc", fn_type, None)
                });

                let call_site = self.builder.build_call(
                    malloc_fn,
                    &[size.into()],
                    "malloc"
                )?;

                call_site.try_as_basic_value()
                    .left()
                    .ok_or_else(|| CompilerError::CodeGen(
                        "malloc returned void".to_string()
                    ))
            }

            Free => {
                // free(ptr: *mut u8)
                if args.len() != 1 {
                    return Err(CompilerError::CodeGen(
                        format!("free expects 1 argument, got {}", args.len())
                    ));
                }

                let ptr = self.get_value(args[0])?;

                // Declare or get free function: declare void @free(ptr)
                let free_fn = self.module.get_function("free").unwrap_or_else(|| {
                    let ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
                    let fn_type = self.context.void_type().fn_type(&[ptr_type.into()], false);
                    self.module.add_function("free", fn_type, None)
                });

                self.builder.build_call(
                    free_fn,
                    &[ptr.into()],
                    "free"
                )?;

                // free returns void, but we need to return something
                // Return a dummy i8 value (caller should ignore it)
                Ok(self.context.i8_type().const_zero().into())
            }

            Realloc => {
                // realloc(ptr: *mut u8, new_size: usize) -> *mut u8
                if args.len() != 2 {
                    return Err(CompilerError::CodeGen(
                        format!("realloc expects 2 arguments, got {}", args.len())
                    ));
                }

                let ptr = self.get_value(args[0])?;
                let new_size = self.get_value(args[1])?;

                // Declare or get realloc function: declare ptr @realloc(ptr, i64)
                let realloc_fn = self.module.get_function("realloc").unwrap_or_else(|| {
                    let ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
                    let i64_type = self.context.i64_type();
                    let fn_type = ptr_type.fn_type(&[ptr_type.into(), i64_type.into()], false);
                    self.module.add_function("realloc", fn_type, None)
                });

                let call_site = self.builder.build_call(
                    realloc_fn,
                    &[ptr.into(), new_size.into()],
                    "realloc"
                )?;

                call_site.try_as_basic_value()
                    .left()
                    .ok_or_else(|| CompilerError::CodeGen(
                        "realloc returned void".to_string()
                    ))
            }

            // ========== Math Intrinsics ==========
            Sqrt => {
                if args.len() != 1 {
                    return Err(CompilerError::CodeGen(
                        format!("sqrt expects 1 argument, got {}", args.len())
                    ));
                }

                let value = self.get_value(args[0])?;

                // Use LLVM's sqrt intrinsic
                let intrinsic_name = if value.is_float_value() {
                    let float_val = value.into_float_value();
                    if float_val.get_type() == self.context.f32_type() {
                        "llvm.sqrt.f32"
                    } else {
                        "llvm.sqrt.f64"
                    }
                } else {
                    return Err(CompilerError::CodeGen(
                        "sqrt requires float argument".to_string()
                    ));
                };

                let sqrt_fn = self.get_or_declare_intrinsic(intrinsic_name, value.get_type())?;
                let call_site = self.builder.build_call(sqrt_fn, &[value.into()], "sqrt")?;

                call_site.try_as_basic_value()
                    .left()
                    .ok_or_else(|| CompilerError::CodeGen("sqrt returned void".to_string()))
            }

            Sin | Cos | Log | Exp => {
                if args.len() != 1 {
                    return Err(CompilerError::CodeGen(
                        format!("{:?} expects 1 argument, got {}", intrinsic, args.len())
                    ));
                }

                let value = self.get_value(args[0])?;

                let intrinsic_name = if value.is_float_value() {
                    let float_val = value.into_float_value();
                    let suffix = if float_val.get_type() == self.context.f32_type() {
                        "f32"
                    } else {
                        "f64"
                    };

                    match intrinsic {
                        Sin => format!("llvm.sin.{}", suffix),
                        Cos => format!("llvm.cos.{}", suffix),
                        Log => format!("llvm.log.{}", suffix),
                        Exp => format!("llvm.exp.{}", suffix),
                        _ => unreachable!(),
                    }
                } else {
                    return Err(CompilerError::CodeGen(
                        format!("{:?} requires float argument", intrinsic)
                    ));
                };

                let math_fn = self.get_or_declare_intrinsic(&intrinsic_name, value.get_type())?;
                let call_site = self.builder.build_call(math_fn, &[value.into()], &format!("{:?}", intrinsic).to_lowercase())?;

                call_site.try_as_basic_value()
                    .left()
                    .ok_or_else(|| CompilerError::CodeGen(format!("{:?} returned void", intrinsic)))
            }

            Pow => {
                if args.len() != 2 {
                    return Err(CompilerError::CodeGen(
                        format!("pow expects 2 arguments, got {}", args.len())
                    ));
                }

                let base = self.get_value(args[0])?;
                let exponent = self.get_value(args[1])?;

                let intrinsic_name = if base.is_float_value() {
                    let float_val = base.into_float_value();
                    if float_val.get_type() == self.context.f32_type() {
                        "llvm.pow.f32"
                    } else {
                        "llvm.pow.f64"
                    }
                } else {
                    return Err(CompilerError::CodeGen(
                        "pow requires float arguments".to_string()
                    ));
                };

                let pow_fn = self.get_or_declare_intrinsic_binary(intrinsic_name, base.get_type())?;
                let call_site = self.builder.build_call(pow_fn, &[base.into(), exponent.into()], "pow")?;

                call_site.try_as_basic_value()
                    .left()
                    .ok_or_else(|| CompilerError::CodeGen("pow returned void".to_string()))
            }

            // ========== Memory Operations ==========
            Memcpy => {
                if args.len() != 3 {
                    return Err(CompilerError::CodeGen(
                        format!("memcpy expects 3 arguments (dst, src, len), got {}", args.len())
                    ));
                }

                let dst = self.get_value(args[0])?;
                let src = self.get_value(args[1])?;
                let len = self.get_value(args[2])?;

                // Use LLVM's memcpy intrinsic: llvm.memcpy.p0.p0.i64(ptr dst, ptr src, i64 len, i1 isvolatile)
                let memcpy_fn = self.module.get_function("llvm.memcpy.p0.p0.i64").unwrap_or_else(|| {
                    let ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
                    let i64_type = self.context.i64_type();
                    let i1_type = self.context.bool_type();
                    let fn_type = self.context.void_type().fn_type(
                        &[ptr_type.into(), ptr_type.into(), i64_type.into(), i1_type.into()],
                        false
                    );
                    self.module.add_function("llvm.memcpy.p0.p0.i64", fn_type, None)
                });

                let is_volatile = self.context.bool_type().const_zero(); // not volatile
                self.builder.build_call(
                    memcpy_fn,
                    &[dst.into(), src.into(), len.into(), is_volatile.into()],
                    "memcpy"
                )?;

                // memcpy returns void, return dummy value
                Ok(self.context.i8_type().const_zero().into())
            }

            Memset => {
                if args.len() != 3 {
                    return Err(CompilerError::CodeGen(
                        format!("memset expects 3 arguments (dst, val, len), got {}", args.len())
                    ));
                }

                let dst = self.get_value(args[0])?;
                let val = self.get_value(args[1])?;
                let len = self.get_value(args[2])?;

                // Use LLVM's memset intrinsic
                let memset_fn = self.module.get_function("llvm.memset.p0.i64").unwrap_or_else(|| {
                    let ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
                    let i8_type = self.context.i8_type();
                    let i64_type = self.context.i64_type();
                    let i1_type = self.context.bool_type();
                    let fn_type = self.context.void_type().fn_type(
                        &[ptr_type.into(), i8_type.into(), i64_type.into(), i1_type.into()],
                        false
                    );
                    self.module.add_function("llvm.memset.p0.i64", fn_type, None)
                });

                let is_volatile = self.context.bool_type().const_zero();
                self.builder.build_call(
                    memset_fn,
                    &[dst.into(), val.into(), len.into(), is_volatile.into()],
                    "memset"
                )?;

                Ok(self.context.i8_type().const_zero().into())
            }

            Memmove => {
                if args.len() != 3 {
                    return Err(CompilerError::CodeGen(
                        format!("memmove expects 3 arguments (dst, src, len), got {}", args.len())
                    ));
                }

                let dst = self.get_value(args[0])?;
                let src = self.get_value(args[1])?;
                let len = self.get_value(args[2])?;

                let memmove_fn = self.module.get_function("llvm.memmove.p0.p0.i64").unwrap_or_else(|| {
                    let ptr_type = self.context.i8_type().ptr_type(AddressSpace::default());
                    let i64_type = self.context.i64_type();
                    let i1_type = self.context.bool_type();
                    let fn_type = self.context.void_type().fn_type(
                        &[ptr_type.into(), ptr_type.into(), i64_type.into(), i1_type.into()],
                        false
                    );
                    self.module.add_function("llvm.memmove.p0.p0.i64", fn_type, None)
                });

                let is_volatile = self.context.bool_type().const_zero();
                self.builder.build_call(
                    memmove_fn,
                    &[dst.into(), src.into(), len.into(), is_volatile.into()],
                    "memmove"
                )?;

                Ok(self.context.i8_type().const_zero().into())
            }

            // ========== Bit Manipulation ==========
            Ctpop | Ctlz | Cttz | Bswap => {
                if args.len() != 1 {
                    return Err(CompilerError::CodeGen(
                        format!("{:?} expects 1 argument, got {}", intrinsic, args.len())
                    ));
                }

                let value = self.get_value(args[0])?;

                if !value.is_int_value() {
                    return Err(CompilerError::CodeGen(
                        format!("{:?} requires integer argument", intrinsic)
                    ));
                }

                let int_val = value.into_int_value();
                let int_type = int_val.get_type();

                let bit_width = int_type.get_bit_width();
                let intrinsic_name = match intrinsic {
                    Ctpop => format!("llvm.ctpop.i{}", bit_width),
                    Ctlz => format!("llvm.ctlz.i{}", bit_width),
                    Cttz => format!("llvm.cttz.i{}", bit_width),
                    Bswap => format!("llvm.bswap.i{}", bit_width),
                    _ => unreachable!(),
                };

                let bit_fn = if intrinsic == Ctlz || intrinsic == Cttz {
                    // ctlz/cttz need extra i1 parameter (is_zero_undef)
                    self.get_or_declare_intrinsic_with_bool(&intrinsic_name, int_type)?
                } else {
                    self.get_or_declare_intrinsic(&intrinsic_name, int_type.into())?
                };

                let call_args = if intrinsic == Ctlz || intrinsic == Cttz {
                    vec![value.into(), self.context.bool_type().const_zero().into()]
                } else {
                    vec![value.into()]
                };

                let call_site = self.builder.build_call(bit_fn, &call_args, &format!("{:?}", intrinsic).to_lowercase())?;

                call_site.try_as_basic_value()
                    .left()
                    .ok_or_else(|| CompilerError::CodeGen(format!("{:?} returned void", intrinsic)))
            }

            // ========== Type Queries ==========
            SizeOf | AlignOf => {
                // These should be resolved at compile time, not runtime
                // For now, return error
                Err(CompilerError::CodeGen(
                    format!("{:?} should be resolved at compile time", intrinsic)
                ))
            }

            // ========== Not Yet Implemented ==========
            AddWithOverflow | SubWithOverflow | MulWithOverflow |
            // ========== Error Handling (Gap 8) ==========
            Panic => {
                // Gap 8 Phase 3: Panic with message
                // Calls abort() from libc, which terminates immediately
                // Future: Add message printing, stack unwinding

                // Declare or get abort function: declare void @abort()
                let abort_fn = self.module.get_function("abort").unwrap_or_else(|| {
                    let fn_type = self.context.void_type().fn_type(&[], false);
                    self.module.add_function("abort", fn_type, None)
                });

                // Call abort() - doesn't return
                self.builder.build_call(abort_fn, &[], "panic")?;

                // Add unreachable to satisfy control flow
                self.builder.build_unreachable()?;

                // Return dummy value (unreachable anyway)
                Ok(self.context.i8_type().const_zero().into())
            }

            Abort => {
                // Gap 8 Phase 3: Immediate abort
                // Calls abort() from libc

                let abort_fn = self.module.get_function("abort").unwrap_or_else(|| {
                    let fn_type = self.context.void_type().fn_type(&[], false);
                    self.module.add_function("abort", fn_type, None)
                });

                self.builder.build_call(abort_fn, &[], "abort")?;
                self.builder.build_unreachable()?;

                Ok(self.context.i8_type().const_zero().into())
            }

            Drop | IncRef | DecRef | Alloca | GCSafepoint | Await | Yield => {
                Err(CompilerError::CodeGen(
                    format!("Intrinsic {:?} not yet implemented in LLVM backend", intrinsic)
                ))
            }
        }
    }

    /// Helper to get or declare a unary LLVM intrinsic
    fn get_or_declare_intrinsic(
        &self,
        name: &str,
        arg_type: BasicTypeEnum<'ctx>,
    ) -> CompilerResult<FunctionValue<'ctx>> {
        Ok(self.module.get_function(name).unwrap_or_else(|| {
            let fn_type = arg_type.fn_type(&[arg_type.into()], false);
            self.module.add_function(name, fn_type, None)
        }))
    }

    /// Helper to get or declare a binary LLVM intrinsic
    fn get_or_declare_intrinsic_binary(
        &self,
        name: &str,
        arg_type: BasicTypeEnum<'ctx>,
    ) -> CompilerResult<FunctionValue<'ctx>> {
        Ok(self.module.get_function(name).unwrap_or_else(|| {
            let fn_type = arg_type.fn_type(&[arg_type.into(), arg_type.into()], false);
            self.module.add_function(name, fn_type, None)
        }))
    }

    /// Helper to get or declare an LLVM intrinsic with bool parameter
    fn get_or_declare_intrinsic_with_bool(
        &self,
        name: &str,
        arg_type: IntType<'ctx>,
    ) -> CompilerResult<FunctionValue<'ctx>> {
        Ok(self.module.get_function(name).unwrap_or_else(|| {
            let fn_type = arg_type.fn_type(&[arg_type.into(), self.context.bool_type().into()], false);
            self.module.add_function(name, fn_type, None)
        }))
    }

    /// Compile a constant value
    fn compile_constant(&self, value: &HirConstant) -> CompilerResult<BasicValueEnum<'ctx>> {
        use HirConstant::*;

        let result = match value {
            // Primitive integers (signed)
            I8(v) => self.context.i8_type().const_int(*v as u64, true).into(),
            I16(v) => self.context.i16_type().const_int(*v as u64, true).into(),
            I32(v) => self.context.i32_type().const_int(*v as u64, true).into(),
            I64(v) => self.context.i64_type().const_int(*v as u64, true).into(),
            I128(v) => {
                // Split i128 into high and low u64 parts
                let low = (*v as u128 & 0xFFFFFFFFFFFFFFFF) as u64;
                let high = ((*v as u128 >> 64) & 0xFFFFFFFFFFFFFFFF) as u64;
                self.context.i128_type().const_int_arbitrary_precision(&[low, high]).into()
            }

            // Primitive integers (unsigned)
            U8(v) => self.context.i8_type().const_int(*v as u64, false).into(),
            U16(v) => self.context.i16_type().const_int(*v as u64, false).into(),
            U32(v) => self.context.i32_type().const_int(*v as u64, false).into(),
            U64(v) => self.context.i64_type().const_int(*v as u64, false).into(),
            U128(v) => {
                // Split u128 into high and low u64 parts
                let low = (*v & 0xFFFFFFFFFFFFFFFF) as u64;
                let high = ((*v >> 64) & 0xFFFFFFFFFFFFFFFF) as u64;
                self.context.i128_type().const_int_arbitrary_precision(&[low, high]).into()
            }

            // Floating point
            F32(v) => self.context.f32_type().const_float(*v as f64).into(),
            F64(v) => self.context.f64_type().const_float(*v).into(),

            // Boolean
            Bool(v) => self.context.bool_type().const_int(*v as u64, false).into(),

            // Null pointer
            Null(ty) => {
                let llvm_ty = self.translate_type(ty)?;
                if let BasicTypeEnum::PointerType(ptr_ty) = llvm_ty {
                    ptr_ty.const_null().into()
                } else {
                    return Err(CompilerError::CodeGen(
                        format!("Null constant must have pointer type, got: {:?}", ty)
                    ));
                }
            }

            // Array constant
            Array(elements) => {
                if elements.is_empty() {
                    // Empty array - create zero-sized array of i8
                    let arr_ty = self.context.i8_type().array_type(0);
                    arr_ty.const_zero().into()
                } else {
                    // Compile each element
                    let compiled_elements: Vec<BasicValueEnum> = elements.iter()
                        .map(|elem| self.compile_constant(elem))
                        .collect::<CompilerResult<Vec<_>>>()?;

                    // Determine element type from first element
                    let elem_type = compiled_elements[0].get_type();

                    // Create constant array based on element type
                    match elem_type {
                        BasicTypeEnum::IntType(int_ty) => {
                            let int_values: Vec<_> = compiled_elements.iter()
                                .map(|v| v.into_int_value())
                                .collect();
                            int_ty.const_array(&int_values).into()
                        }
                        BasicTypeEnum::FloatType(float_ty) => {
                            let float_values: Vec<_> = compiled_elements.iter()
                                .map(|v| v.into_float_value())
                                .collect();
                            float_ty.const_array(&float_values).into()
                        }
                        BasicTypeEnum::PointerType(ptr_ty) => {
                            let ptr_values: Vec<_> = compiled_elements.iter()
                                .map(|v| v.into_pointer_value())
                                .collect();
                            ptr_ty.const_array(&ptr_values).into()
                        }
                        BasicTypeEnum::StructType(struct_ty) => {
                            let struct_values: Vec<_> = compiled_elements.iter()
                                .map(|v| v.into_struct_value())
                                .collect();
                            struct_ty.const_array(&struct_values).into()
                        }
                        BasicTypeEnum::ArrayType(arr_ty) => {
                            let arr_values: Vec<_> = compiled_elements.iter()
                                .map(|v| v.into_array_value())
                                .collect();
                            arr_ty.const_array(&arr_values).into()
                        }
                        BasicTypeEnum::VectorType(_) => {
                            return Err(CompilerError::CodeGen(
                                "Vector type arrays not yet supported in constants".to_string()
                            ));
                        }
                    }
                }
            }

            // Struct constant
            Struct(fields) => {
                if fields.is_empty() {
                    // Empty struct (unit type)
                    let struct_ty = self.context.struct_type(&[], false);
                    struct_ty.const_named_struct(&[]).into()
                } else {
                    // Compile each field
                    let compiled_fields: Vec<BasicValueEnum> = fields.iter()
                        .map(|field| self.compile_constant(field))
                        .collect::<CompilerResult<Vec<_>>>()?;

                    // Create constant struct
                    let struct_ty = self.context.const_struct(&compiled_fields, false);
                    struct_ty.into()
                }
            }

            // String constant
            String(s) => {
                let string_value = self.context.const_string(s.to_string().as_bytes(), true);
                string_value.into()
            }

            // VTable should not go through compile_constant - handled separately
            VTable(_) => {
                return Err(CompilerError::CodeGen(
                    "VTable constants should be compiled via compile_vtable, not compile_constant".to_string()
                ));
            }
        };

        Ok(result)
    }

    /// Translate HIR type to LLVM type
    fn translate_type(&self, ty: &HirType) -> CompilerResult<BasicTypeEnum<'ctx>> {
        use HirType::*;

        let result = match ty {
            I8 => self.context.i8_type().into(),
            I16 => self.context.i16_type().into(),
            I32 => self.context.i32_type().into(),
            I64 => self.context.i64_type().into(),
            I128 => self.context.i128_type().into(),
            U8 => self.context.i8_type().into(),
            U16 => self.context.i16_type().into(),
            U32 => self.context.i32_type().into(),
            U64 => self.context.i64_type().into(),
            U128 => self.context.i128_type().into(),
            F32 => self.context.f32_type().into(),
            F64 => self.context.f64_type().into(),
            Bool => self.context.bool_type().into(),
            Ptr(inner) => {
                let inner_ty = self.translate_type(inner)?;
                inner_ty.ptr_type(AddressSpace::default()).into()
            }
            Ref { pointee, .. } => {
                // References are compiled as pointers
                let inner_ty = self.translate_type(pointee)?;
                inner_ty.ptr_type(AddressSpace::default()).into()
            }
            Array(element_ty, size) => {
                let elem_ty = self.translate_type(element_ty)?;
                elem_ty.array_type(*size as u32).into()
            }
            Struct(struct_ty) => {
                // Translate struct fields to LLVM types
                let field_types: Result<Vec<BasicTypeEnum>, _> = struct_ty.fields
                    .iter()
                    .map(|field_ty| self.translate_type(field_ty))
                    .collect();

                let field_types = field_types?;

                // Create LLVM struct type
                // Use opaque struct if it has a name (for recursive types)
                if let Some(name) = struct_ty.name {
                    let name_str = format!("struct.{:?}", name);
                    let struct_type = self.context.opaque_struct_type(&name_str);
                    struct_type.set_body(&field_types, struct_ty.packed);
                    struct_type.into()
                } else {
                    // Anonymous struct
                    self.context.struct_type(&field_types, struct_ty.packed).into()
                }
            }
            Function(func_ty) => {
                // Translate function type to function pointer
                let param_types: Result<Vec<BasicMetadataTypeEnum>, _> = func_ty.params
                    .iter()
                    .map(|param_ty| {
                        self.translate_type(param_ty)
                            .map(|t| t.into())
                    })
                    .collect();

                let param_types = param_types?;

                // Handle return types
                let fn_type = if func_ty.returns.is_empty() {
                    // Void return
                    self.context.void_type().fn_type(&param_types, func_ty.is_variadic)
                } else if func_ty.returns.len() == 1 {
                    // Single return value
                    let ret_ty = self.translate_type(&func_ty.returns[0])?;
                    ret_ty.fn_type(&param_types, func_ty.is_variadic)
                } else {
                    // Multiple returns - wrap in struct
                    let ret_types: Result<Vec<BasicTypeEnum>, _> = func_ty.returns
                        .iter()
                        .map(|ret_ty| self.translate_type(ret_ty))
                        .collect();
                    let ret_types = ret_types?;
                    let struct_ret = self.context.struct_type(&ret_types, false);
                    struct_ret.fn_type(&param_types, func_ty.is_variadic)
                };

                // Return function pointer type
                fn_type.ptr_type(AddressSpace::default()).into()
            }
            _ => {
                return Err(CompilerError::CodeGen(
                    format!("Type translation not yet implemented: {:?}", ty)
                ));
            }
        };

        Ok(result)
    }

    /// Get a value from the value map
    fn get_value(&self, id: HirId) -> CompilerResult<BasicValueEnum<'ctx>> {
        self.value_map.get(&id)
            .copied()
            .ok_or_else(|| {
                CompilerError::CodeGen(
                    format!("Value not found: {:?}", id)
                )
            })
    }

    /// Create a default value for a type (used for implicit returns)
    fn default_value(&self, ty: &HirType) -> CompilerResult<BasicValueEnum<'ctx>> {
        use HirType::*;

        let result = match ty {
            I8 | I16 | I32 | I64 | U8 | U16 | U32 | U64 => {
                let llvm_ty = self.translate_type(ty)?;
                llvm_ty.into_int_type().const_zero().into()
            }
            F32 | F64 => {
                let llvm_ty = self.translate_type(ty)?;
                llvm_ty.into_float_type().const_zero().into()
            }
            Bool => self.context.bool_type().const_zero().into(),
            _ => {
                return Err(CompilerError::CodeGen(
                    format!("Cannot create default value for type: {:?}", ty)
                ));
            }
        };

        Ok(result)
    }

    /// Get the compiled LLVM module
    pub fn get_module(&self) -> &Module<'ctx> {
        &self.module
    }

    /// Verify the module (checks for LLVM IR errors)
    pub fn verify(&self) -> Result<(), String> {
        self.module.verify()
            .map_err(|e| e.to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hir::*;

    #[test]
    fn test_llvm_backend_creation() {
        let context = Context::create();
        let backend = LLVMBackend::new(&context, "test_module");
        assert_eq!(backend.module.get_name().to_str().unwrap(), "test_module");
    }

    #[test]
    fn test_basic_type_translation() {
        let context = Context::create();
        let backend = LLVMBackend::new(&context, "test");

        // Test integer types
        assert!(backend.translate_type(&HirType::I32).is_ok());
        assert!(backend.translate_type(&HirType::I64).is_ok());
        assert!(backend.translate_type(&HirType::U32).is_ok());

        // Test float types
        assert!(backend.translate_type(&HirType::F32).is_ok());
        assert!(backend.translate_type(&HirType::F64).is_ok());

        // Test bool
        assert!(backend.translate_type(&HirType::Bool).is_ok());
    }

    #[test]
    fn test_constant_compilation() {
        let context = Context::create();
        let backend = LLVMBackend::new(&context, "test");

        // Integer constants
        let i32_const = backend.compile_constant(&HirConstant::I32(42));
        assert!(i32_const.is_ok());

        // Float constants
        let f64_const = backend.compile_constant(&HirConstant::F64(3.14));
        assert!(f64_const.is_ok());

        // Bool constants
        let bool_const = backend.compile_constant(&HirConstant::Bool(true));
        assert!(bool_const.is_ok());
    }
}
