//! # Async/Coroutine Support for HIR
//! 
//! Implements async function compilation, coroutine state machines, and
//! async runtime integration for the HIR. This module provides the foundation
//! for async/await syntax and coroutine-based programming models.

use std::collections::{HashSet, VecDeque};
use indexmap::IndexMap;
use crate::hir::*;
use crate::CompilerResult;
use zyntax_typed_ast::InternedString;

/// Async function state machine representation
#[derive(Debug, Clone)]
pub struct AsyncStateMachine {
    /// Unique identifier for this state machine
    pub id: HirId,
    /// Original async function ID
    pub original_function: HirId,
    /// Original async function name (used for generating _new and _poll functions)
    pub original_name: InternedString,
    /// States in the state machine
    pub states: IndexMap<AsyncStateId, AsyncState>,
    /// Initial state
    pub initial_state: AsyncStateId,
    /// Final/completion state
    pub final_state: AsyncStateId,
    /// Captured variables (closure environment)
    pub captures: Vec<AsyncCapture>,
    /// Type of the final result
    pub result_type: HirType,
    /// Values from the original function (constants, etc.) needed by instructions
    pub values: IndexMap<HirId, HirValue>,
}

/// State identifier in an async state machine
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AsyncStateId(pub u32);

/// A state in an async state machine
#[derive(Debug, Clone)]
pub struct AsyncState {
    /// State identifier
    pub id: AsyncStateId,
    /// Local variables in this state
    pub locals: Vec<AsyncLocal>,
    /// Instructions to execute in this state
    pub instructions: Vec<HirInstruction>,
    /// Terminator for this state
    pub terminator: AsyncTerminator,
}

/// Local variable in an async state
#[derive(Debug, Clone)]
pub struct AsyncLocal {
    /// Variable identifier
    pub id: HirId,
    /// Variable name
    pub name: InternedString,
    /// Variable type
    pub ty: HirType,
    /// Whether this variable persists across await points
    pub persists: bool,
}

/// Captured variable for async closure
#[derive(Debug, Clone)]
pub struct AsyncCapture {
    /// Capture identifier
    pub id: HirId,
    /// Captured variable name
    pub name: InternedString,
    /// Type of captured variable
    pub ty: HirType,
    /// Capture mode
    pub mode: AsyncCaptureMode,
}

/// How a variable is captured in async closure
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AsyncCaptureMode {
    /// Capture by value (move)
    ByValue,
    /// Capture by immutable reference
    ByRef(HirLifetime),
    /// Capture by mutable reference
    ByMutRef(HirLifetime),
}

/// Terminator instructions for async states
#[derive(Debug, Clone)]
pub enum AsyncTerminator {
    /// Continue to next state
    Continue { next_state: AsyncStateId },
    /// Await an async operation
    Await {
        /// Future to await
        future: HirId,
        /// State to resume after await completes
        resume_state: AsyncStateId,
    },
    /// Yield a value (for generators)
    Yield {
        /// Value to yield
        value: HirId,
        /// State to resume when next() is called
        resume_state: AsyncStateId,
    },
    /// Return from async function
    Return { value: Option<HirId> },
    /// Panic/throw an error
    Panic { error: HirId },
}

/// Async runtime integration
#[derive(Debug, Clone)]
pub struct AsyncRuntime {
    /// Runtime type (e.g., Tokio, async-std, custom)
    pub runtime_type: AsyncRuntimeType,
    /// Executor function for spawning tasks
    pub executor: HirId,
    /// Future trait implementation
    pub future_trait: HirId,
    /// Wake mechanism
    pub waker_type: HirId,
}

/// Type of async runtime
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AsyncRuntimeType {
    /// Tokio runtime
    Tokio,
    /// async-std runtime
    AsyncStd,
    /// Custom runtime
    Custom(InternedString),
    /// No runtime (bare futures)
    None,
}

/// Async function compiler
pub struct AsyncCompiler {
    /// State machine counter
    next_state_id: u32,
    /// Generated state machines
    state_machines: IndexMap<HirId, AsyncStateMachine>,
    /// Async runtime configuration
    runtime: Option<AsyncRuntime>,
}

impl AsyncCompiler {
    pub fn new() -> Self {
        Self {
            next_state_id: 0,
            state_machines: IndexMap::new(),
            runtime: None,
        }
    }
    
    /// Set async runtime configuration
    pub fn set_runtime(&mut self, runtime: AsyncRuntime) {
        self.runtime = Some(runtime);
    }
    
    /// Compile an async function into a state machine
    pub fn compile_async_function(&mut self, func: &HirFunction) -> CompilerResult<AsyncStateMachine> {
        // Analyze the function for await points and control flow
        let await_points = self.find_await_points(func)?;
        
        // Create state machine structure
        let machine_id = HirId::new();
        let initial_state = self.next_state_id();
        let final_state = self.next_state_id();
        
        let mut state_machine = AsyncStateMachine {
            id: machine_id,
            original_function: func.id,
            original_name: func.name,
            states: IndexMap::new(),
            initial_state,
            final_state,
            captures: self.analyze_captures(func)?,
            result_type: func.signature.returns.first().cloned().unwrap_or(HirType::Void),
            // Copy values from the original function (constants, etc.) needed by instructions
            values: func.values.clone(),
        };
        
        // Build states based on await points
        self.build_states(func, &await_points, &mut state_machine)?;
        
        // Store the state machine
        self.state_machines.insert(func.id, state_machine.clone());
        
        Ok(state_machine)
    }
    
    /// Find all await points in a function
    fn find_await_points(&self, func: &HirFunction) -> CompilerResult<Vec<AwaitPoint>> {
        let mut await_points = Vec::new();
        
        for (block_id, block) in &func.blocks {
            for (inst_idx, inst) in block.instructions.iter().enumerate() {
                if let HirInstruction::Call { 
                    callee: HirCallable::Intrinsic(Intrinsic::Await), 
                    args,
                    result,
                    ..
                } = inst {
                    await_points.push(AwaitPoint {
                        block_id: *block_id,
                        instruction_index: inst_idx,
                        future: args.get(0).copied().unwrap_or_else(HirId::new),
                        result: *result,
                    });
                }
            }
        }
        
        Ok(await_points)
    }
    
    /// Analyze captures for async closure
    ///
    /// This performs escape analysis to determine which variables need to be
    /// captured in the state machine struct. A variable needs to be captured if:
    /// 1. It's a parameter or local defined before an await point
    /// 2. It's used after that await point
    ///
    /// Variables that are only used within a single segment don't need capturing.
    fn analyze_captures(&self, func: &HirFunction) -> CompilerResult<Vec<AsyncCapture>> {
        let mut captures = Vec::new();
        let mut captured_ids = HashSet::new();

        // Find all await point instruction indices
        let await_indices: HashSet<usize> = func.blocks.values()
            .flat_map(|block| block.instructions.iter().enumerate())
            .filter_map(|(idx, inst)| {
                if let HirInstruction::Call {
                    callee: HirCallable::Intrinsic(Intrinsic::Await),
                    ..
                } = inst {
                    Some(idx)
                } else {
                    None
                }
            })
            .collect();

        // Build a map from parameter index to SSA value ID
        // The SSA builder creates new HirIds for parameters that are different from param.id
        let param_ssa_ids: Vec<HirId> = (0..func.signature.params.len())
            .map(|idx| {
                // Find the value with HirValueKind::Parameter(idx)
                func.values.iter()
                    .find(|(_, v)| matches!(&v.kind, HirValueKind::Parameter(i) if *i as usize == idx))
                    .map(|(id, _)| *id)
                    .unwrap_or_else(|| func.signature.params[idx].id) // Fallback to param.id
            })
            .collect();

        eprintln!("[ASYNC DEBUG] analyze_captures: param_ssa_ids = {:?}", param_ssa_ids);
        for (idx, param) in func.signature.params.iter().enumerate() {
            eprintln!("[ASYNC DEBUG]   param[{}] signature id = {:?}, SSA id = {:?}", idx, param.id, param_ssa_ids.get(idx));
        }

        // If no await points, we still need to capture all parameters for the state machine
        if await_indices.is_empty() {
            for (idx, param) in func.signature.params.iter().enumerate() {
                let ssa_id = param_ssa_ids.get(idx).copied().unwrap_or(param.id);
                captures.push(AsyncCapture {
                    id: ssa_id, // Use SSA value ID, not signature param.id
                    name: param.name,
                    ty: param.ty.clone(),
                    mode: AsyncCaptureMode::ByValue,
                });
            }
            return Ok(captures);
        }

        // Collect all values defined before each await point
        let mut defined_before_await: HashSet<HirId> = HashSet::new();

        // Parameters are always defined at the start (use SSA IDs)
        for ssa_id in &param_ssa_ids {
            defined_before_await.insert(*ssa_id);
        }

        // Collect all values used after await points
        let mut used_after_await: HashSet<HirId> = HashSet::new();

        for block in func.blocks.values() {
            let mut past_await = false;

            for (idx, inst) in block.instructions.iter().enumerate() {
                // Check if this is an await point
                if await_indices.contains(&idx) {
                    past_await = true;
                    continue;
                }

                // If we're past an await, collect used values
                if past_await {
                    let used = self.collect_instruction_uses(inst);
                    used_after_await.extend(used);
                }

                // Track definitions before await
                if !past_await {
                    if let Some(result) = self.get_instruction_result(inst) {
                        defined_before_await.insert(result);
                    }
                }
            }
        }

        // A value needs to be captured if it's defined before await AND used after
        let needs_capture: HashSet<HirId> = defined_before_await
            .intersection(&used_after_await)
            .copied()
            .collect();

        // Add parameters that need capturing (use SSA IDs)
        for (idx, param) in func.signature.params.iter().enumerate() {
            let ssa_id = param_ssa_ids.get(idx).copied().unwrap_or(param.id);
            if needs_capture.contains(&ssa_id) && !captured_ids.contains(&ssa_id) {
                captured_ids.insert(ssa_id);
                captures.push(AsyncCapture {
                    id: ssa_id, // Use SSA value ID
                    name: param.name,
                    ty: param.ty.clone(),
                    mode: AsyncCaptureMode::ByValue,
                });
            }
        }

        // Add locals that need capturing
        for (local_id, local) in &func.locals {
            if needs_capture.contains(local_id) && !captured_ids.contains(local_id) {
                captured_ids.insert(*local_id);
                captures.push(AsyncCapture {
                    id: *local_id,
                    name: local.name,
                    ty: local.ty.clone(),
                    mode: if local.is_mutable {
                        AsyncCaptureMode::ByMutRef(HirLifetime::anonymous())
                    } else {
                        AsyncCaptureMode::ByValue
                    },
                });
            }
        }

        // If no captures identified but we have parameters, capture all parameters
        // (conservative fallback for safety)
        // IMPORTANT: Use the parameter VALUE id from func.values, not the param declaration id.
        // Instructions reference the value id, not the declaration id.
        if captures.is_empty() && !func.signature.params.is_empty() {
            eprintln!("[ASYNC DEBUG] Capturing parameters for {:?}", func.name);
            eprintln!("[ASYNC DEBUG]   func.values has {} entries", func.values.len());
            for (id, v) in &func.values {
                eprintln!("[ASYNC DEBUG]     {:?} -> {:?}", id, v.kind);
            }
            for (param_idx, param) in func.signature.params.iter().enumerate() {
                // Find the HirId for this parameter's VALUE (not declaration)
                // The SSA builder creates values with HirValueKind::Parameter(idx) for each parameter
                let value_id = func.values.iter()
                    .find(|(_, v)| matches!(v.kind, HirValueKind::Parameter(idx) if idx == param_idx as u32))
                    .map(|(id, _)| *id)
                    .unwrap_or(param.id); // Fallback to declaration id if not found

                eprintln!("[ASYNC DEBUG]   param[{}] {:?}: declaration id={:?}, value id={:?}",
                    param_idx, param.name, param.id, value_id);

                captures.push(AsyncCapture {
                    id: value_id,
                    name: param.name,
                    ty: param.ty.clone(),
                    mode: AsyncCaptureMode::ByValue,
                });
            }
        }

        Ok(captures)
    }

    /// Collect all HirIds used by an instruction
    fn collect_instruction_uses(&self, inst: &HirInstruction) -> Vec<HirId> {
        match inst {
            HirInstruction::Binary { left, right, .. } => vec![*left, *right],
            HirInstruction::Unary { operand, .. } => vec![*operand],
            HirInstruction::Load { ptr, .. } => vec![*ptr],
            HirInstruction::Store { ptr, value, .. } => vec![*ptr, *value],
            HirInstruction::Call { args, .. } => args.clone(),
            HirInstruction::IndirectCall { func_ptr, args, .. } => {
                let mut uses = vec![*func_ptr];
                uses.extend(args.iter().copied());
                uses
            }
            HirInstruction::GetElementPtr { ptr, indices, .. } => {
                let mut uses = vec![*ptr];
                uses.extend(indices.iter().copied());
                uses
            }
            HirInstruction::ExtractValue { aggregate, .. } => vec![*aggregate],
            HirInstruction::InsertValue { aggregate, value, .. } => vec![*aggregate, *value],
            HirInstruction::Cast { operand, .. } => vec![*operand],
            HirInstruction::Select { condition, true_val, false_val, .. } => {
                vec![*condition, *true_val, *false_val]
            }
            HirInstruction::Atomic { ptr, value, .. } => {
                let mut uses = vec![*ptr];
                if let Some(v) = value {
                    uses.push(*v);
                }
                uses
            }
            HirInstruction::CreateUnion { value, .. } => vec![*value],
            HirInstruction::GetUnionDiscriminant { union_val, .. } => vec![*union_val],
            HirInstruction::ExtractUnionValue { union_val, .. } => vec![*union_val],
            HirInstruction::CreateTraitObject { data_ptr, vtable_id, .. } => {
                vec![*data_ptr, *vtable_id]
            }
            _ => vec![],
        }
    }

    /// Get the result HirId of an instruction if it produces one
    fn get_instruction_result(&self, inst: &HirInstruction) -> Option<HirId> {
        match inst {
            HirInstruction::Binary { result, .. } |
            HirInstruction::Unary { result, .. } |
            HirInstruction::Alloca { result, .. } |
            HirInstruction::Load { result, .. } |
            HirInstruction::GetElementPtr { result, .. } |
            HirInstruction::ExtractValue { result, .. } |
            HirInstruction::InsertValue { result, .. } |
            HirInstruction::Cast { result, .. } |
            HirInstruction::Select { result, .. } |
            HirInstruction::Atomic { result, .. } |
            HirInstruction::CreateUnion { result, .. } |
            HirInstruction::GetUnionDiscriminant { result, .. } |
            HirInstruction::ExtractUnionValue { result, .. } |
            HirInstruction::CreateTraitObject { result, .. } => Some(*result),
            HirInstruction::Call { result, .. } |
            HirInstruction::IndirectCall { result, .. } => *result,
            HirInstruction::Store { .. } |
            HirInstruction::Fence { .. } => None,
            _ => None,
        }
    }
    
    /// Build state machine states
    fn build_states(
        &mut self,
        func: &HirFunction,
        await_points: &[AwaitPoint],
        state_machine: &mut AsyncStateMachine,
    ) -> CompilerResult<()> {
        // Split function at await points
        let segments = self.split_at_await_points(func, await_points)?;
        
        // Create states for each segment
        for (i, segment) in segments.iter().enumerate() {
            let state_id = if i == 0 {
                state_machine.initial_state
            } else {
                self.next_state_id()
            };
            
            let terminator = if i < segments.len() - 1 {
                // More segments to process
                if let Some(await_point) = await_points.get(i) {
                    AsyncTerminator::Await {
                        future: await_point.future,
                        resume_state: self.next_state_id(),
                    }
                } else {
                    AsyncTerminator::Continue {
                        next_state: self.next_state_id(),
                    }
                }
            } else {
                // Final segment - extract return value from segment's terminator
                match &segment.terminator {
                    HirTerminator::Return { values } => {
                        // Use the first return value if present
                        eprintln!("[ASYNC DEBUG] build_states: segment terminator has return values: {:?}", values);
                        AsyncTerminator::Return {
                            value: values.first().copied()
                        }
                    }
                    _ => {
                        // No explicit return value (void function or other terminator)
                        eprintln!("[ASYNC DEBUG] build_states: segment terminator is not Return: {:?}", segment.terminator);
                        AsyncTerminator::Return { value: None }
                    }
                }
            };
            
            let state = AsyncState {
                id: state_id,
                locals: self.extract_locals_from_segment(segment)?,
                instructions: segment.instructions.clone(),
                terminator,
            };
            
            state_machine.states.insert(state_id, state);
        }
        
        Ok(())
    }
    
    /// Split function into segments at await points
    ///
    /// This traverses all blocks in the function's CFG, collecting instructions
    /// and splitting into segments whenever an await point is encountered.
    /// Each segment represents a continuous sequence of operations that can
    /// execute without yielding.
    fn split_at_await_points(
        &self,
        func: &HirFunction,
        await_points: &[AwaitPoint],
    ) -> CompilerResult<Vec<CodeSegment>> {
        let mut segments = Vec::new();

        // Build a map of await points by (block_id, instruction_index)
        let await_point_map: HashSet<(HirId, usize)> = await_points
            .iter()
            .map(|ap| (ap.block_id, ap.instruction_index))
            .collect();

        // For simple functions with a single block (common case), use optimized path
        if func.blocks.len() == 1 {
            return self.split_single_block(func, await_points);
        }

        // Traverse blocks in order (starting from entry block)
        let mut visited = HashSet::new();
        let mut work_queue = VecDeque::new();
        work_queue.push_back(func.entry_block);

        let mut current_segment = CodeSegment {
            instructions: Vec::new(),
            terminator: HirTerminator::Unreachable, // Will be set properly
        };

        while let Some(block_id) = work_queue.pop_front() {
            if visited.contains(&block_id) {
                continue;
            }
            visited.insert(block_id);

            let block = match func.blocks.get(&block_id) {
                Some(b) => b,
                None => continue,
            };

            // Process instructions in this block
            for (inst_idx, inst) in block.instructions.iter().enumerate() {
                // Check if this is an await point
                if await_point_map.contains(&(block_id, inst_idx)) {
                    // End current segment before the await
                    if !current_segment.instructions.is_empty() {
                        // Set terminator to continue to next segment (will be patched)
                        current_segment.terminator = HirTerminator::Branch { target: HirId::new() };
                        segments.push(current_segment);
                    }

                    // Start new segment after the await
                    current_segment = CodeSegment {
                        instructions: Vec::new(),
                        terminator: HirTerminator::Unreachable,
                    };
                } else {
                    current_segment.instructions.push(inst.clone());
                }
            }

            // Set terminator from the block
            current_segment.terminator = block.terminator.clone();

            // Add successor blocks to work queue
            match &block.terminator {
                HirTerminator::Branch { target } => {
                    work_queue.push_back(*target);
                }
                HirTerminator::CondBranch { true_target, false_target, .. } => {
                    work_queue.push_back(*true_target);
                    work_queue.push_back(*false_target);
                }
                HirTerminator::Switch { default, cases, .. } => {
                    work_queue.push_back(*default);
                    for (_, target) in cases {
                        work_queue.push_back(*target);
                    }
                }
                HirTerminator::Return { .. } |
                HirTerminator::Unreachable |
                HirTerminator::Invoke { .. } |
                HirTerminator::PatternMatch { .. } => {
                    // End of control flow path - push segment if non-empty
                    if !current_segment.instructions.is_empty() || segments.is_empty() {
                        segments.push(current_segment.clone());
                    }
                    current_segment = CodeSegment {
                        instructions: Vec::new(),
                        terminator: HirTerminator::Unreachable,
                    };
                }
            }
        }

        // Push final segment if it has content
        if !current_segment.instructions.is_empty() && !matches!(current_segment.terminator, HirTerminator::Unreachable) {
            segments.push(current_segment);
        }

        // Ensure we have at least one segment
        if segments.is_empty() {
            segments.push(CodeSegment {
                instructions: Vec::new(),
                terminator: HirTerminator::Return { values: vec![] },
            });
        }

        Ok(segments)
    }

    /// Optimized path for single-block functions (most common case)
    fn split_single_block(
        &self,
        func: &HirFunction,
        await_points: &[AwaitPoint],
    ) -> CompilerResult<Vec<CodeSegment>> {
        let mut segments = Vec::new();

        if let Some(entry_block) = func.blocks.get(&func.entry_block) {
            eprintln!("[ASYNC DEBUG] split_single_block: entry block terminator = {:?}", entry_block.terminator);
            eprintln!("[ASYNC DEBUG] split_single_block: entry block has {} instructions", entry_block.instructions.len());
            let mut current_segment = CodeSegment {
                instructions: Vec::new(),
                terminator: entry_block.terminator.clone(),
            };

            for (i, inst) in entry_block.instructions.iter().enumerate() {
                // Check if this is an await point
                if await_points.iter().any(|ap| ap.instruction_index == i) {
                    // End current segment and start new one
                    if !current_segment.instructions.is_empty() {
                        segments.push(current_segment);
                        current_segment = CodeSegment {
                            instructions: Vec::new(),
                            terminator: entry_block.terminator.clone(),
                        };
                    }
                } else {
                    current_segment.instructions.push(inst.clone());
                }
            }

            // Add final segment
            if !current_segment.instructions.is_empty() || segments.is_empty() {
                segments.push(current_segment);
            }
        }

        Ok(segments)
    }
    
    /// Extract local variables from a code segment
    ///
    /// This analyzes the instructions in a segment to find:
    /// 1. Alloca instructions (stack allocations)
    /// 2. Values defined by instructions that need to be stored in the state machine
    ///
    /// Local variables are marked with `persists: true` if they need to be stored
    /// in the state machine struct to survive across await points.
    fn extract_locals_from_segment(&self, segment: &CodeSegment) -> CompilerResult<Vec<AsyncLocal>> {
        let mut locals = Vec::new();
        let mut defined_values: HashSet<HirId> = HashSet::new();

        for inst in &segment.instructions {
            // Handle Alloca instructions specially - these are explicit local variables
            if let HirInstruction::Alloca { result, ty, .. } = inst {
                locals.push(AsyncLocal {
                    id: *result,
                    name: InternedString::default(), // Will be resolved from debug info if available
                    ty: ty.clone(),
                    persists: true, // Stack allocations typically need to persist
                });
                defined_values.insert(*result);
                continue;
            }

            // Get the result of this instruction if it produces one
            if let Some(result_id) = self.get_instruction_result(inst) {
                // Don't duplicate alloca results
                if defined_values.contains(&result_id) {
                    continue;
                }

                // Determine if this local needs to persist based on whether it's used
                // in subsequent instructions within this segment
                let ty = self.get_instruction_result_type(inst);
                let uses = self.collect_instruction_uses(inst);

                // A local persists if its result is used by other instructions
                // (We'll refine this later based on await point analysis)
                let persists = !uses.is_empty();

                locals.push(AsyncLocal {
                    id: result_id,
                    name: InternedString::default(), // Anonymous local
                    ty,
                    persists,
                });
                defined_values.insert(result_id);
            }
        }

        Ok(locals)
    }

    /// Get the result type of an instruction
    fn get_instruction_result_type(&self, inst: &HirInstruction) -> HirType {
        match inst {
            HirInstruction::Binary { op, .. } => {
                // Binary operations typically preserve the operand type or return bool for comparisons
                match op {
                    BinaryOp::Eq | BinaryOp::Ne | BinaryOp::Lt | BinaryOp::Le |
                    BinaryOp::Gt | BinaryOp::Ge => HirType::Bool,
                    _ => HirType::I64, // Default to i64 for arithmetic
                }
            }
            HirInstruction::Unary { op, .. } => {
                match op {
                    UnaryOp::Not => HirType::Bool,
                    _ => HirType::I64,
                }
            }
            HirInstruction::Alloca { ty, .. } => HirType::Ptr(Box::new(ty.clone())),
            HirInstruction::Load { ty, .. } => ty.clone(),
            HirInstruction::GetElementPtr { ty, .. } => ty.clone(),
            HirInstruction::ExtractValue { ty, .. } => ty.clone(),
            HirInstruction::InsertValue { ty, .. } => ty.clone(),
            HirInstruction::Cast { ty, .. } => ty.clone(),
            HirInstruction::Select { ty, .. } => ty.clone(),
            HirInstruction::Call { .. } => HirType::Void, // Actual return type is in callee signature
            HirInstruction::IndirectCall { return_ty, .. } => return_ty.clone(),
            HirInstruction::Atomic { ty, .. } => ty.clone(),
            HirInstruction::CreateUnion { union_ty, .. } => union_ty.clone(),
            HirInstruction::GetUnionDiscriminant { .. } => HirType::U32, // Discriminant is typically u32
            HirInstruction::ExtractUnionValue { ty, .. } => ty.clone(),
            HirInstruction::CreateTraitObject { .. } => {
                // Fat pointer: {data_ptr, vtable_ptr}
                HirType::Opaque(InternedString::default()) // Placeholder
            }
            _ => HirType::Void,
        }
    }
    
    /// Generate next state ID
    fn next_state_id(&mut self) -> AsyncStateId {
        let id = AsyncStateId(self.next_state_id);
        self.next_state_id += 1;
        id
    }
    
    /// Generate async wrapper function with arena
    ///
    /// This creates the poll function for the state machine. The function is named
    /// `{original_name}_poll` to allow multiple async functions to coexist.
    ///
    /// For `async fn compute(x: i32) i32`, this generates:
    /// - `compute_poll(self: *StateMachine, cx: *Context) -> Poll`
    pub fn generate_async_wrapper_with_arena(
        &self,
        state_machine: &AsyncStateMachine,
        arena: &mut zyntax_typed_ast::arena::AstArena,
        original_func: &HirFunction,
    ) -> CompilerResult<HirFunction> {
        // Generate function-specific poll function name: {original}_poll
        let poll_fn_name = if let Some(name_str) = state_machine.original_name.resolve_global() {
            arena.intern_string(&format!("{}_poll", name_str))
        } else {
            arena.intern_string("anonymous_async_poll")
        };

        // Create wrapper function that implements the Future trait
        // Return type convention for poll:
        //   - Returns i64: 0 = Pending, otherwise Ready(value)
        //   - For void async functions, Ready is represented as any non-zero value
        let wrapper_sig = HirFunctionSignature {
            params: vec![
                // self parameter (the state machine)
                HirParam {
                    id: HirId::new(),
                    name: arena.intern_string("self"),
                    ty: HirType::Ptr(Box::new(HirType::Opaque(arena.intern_string("StateMachine")))),
                    attributes: ParamAttributes::default(),
                },
                // context parameter
                HirParam {
                    id: HirId::new(),
                    name: arena.intern_string("cx"),
                    ty: HirType::Ptr(Box::new(HirType::Opaque(arena.intern_string("Context")))),
                    attributes: ParamAttributes::default(),
                },
            ],
            // Use i64 for poll result: 0 = Pending, non-zero = Ready(value)
            returns: vec![HirType::I64],
            type_params: vec![],
            const_params: vec![],
            lifetime_params: vec![],
            is_variadic: false,
            is_async: false, // The wrapper function itself is not async (it's a poll function)
        };

        let mut wrapper = HirFunction::new(
            poll_fn_name,
            wrapper_sig
        );

        // Check if this is a simple async function (no await points)
        // For simple functions, we can preserve the original block structure
        eprintln!("[ASYNC DEBUG] generate_async_wrapper_with_arena: states.len() = {}", state_machine.states.len());
        if state_machine.states.len() == 1 {
            eprintln!("[ASYNC DEBUG] Using build_simple_async_wrapper (single state)");
            self.build_simple_async_wrapper(&mut wrapper, state_machine, original_func)?;
        } else {
            eprintln!("[ASYNC DEBUG] Using build_state_dispatch (multiple states)");
            // Build state machine dispatch logic for functions with await points
            self.build_state_dispatch(&mut wrapper, state_machine, original_func)?;
        }

        Ok(wrapper)
    }

    /// Build a simple async wrapper that preserves the original function's block structure
    ///
    /// This is used for async functions WITHOUT await points. Instead of creating
    /// a state machine dispatch, we:
    /// 1. Copy all blocks from the original function
    /// 2. Add parameter loading from state machine at the entry
    /// 3. Transform Return terminators to return Poll::Ready(value)
    fn build_simple_async_wrapper(
        &self,
        wrapper: &mut HirFunction,
        state_machine: &AsyncStateMachine,
        original_func: &HirFunction,
    ) -> CompilerResult<()> {
        // Remove the default entry block created by HirFunction::new()
        // We'll add our own param_load block as the entry
        let old_entry = wrapper.entry_block;
        wrapper.blocks.remove(&old_entry);

        // Copy all values from the original function EXCEPT Parameters
        // Parameters are loaded from the state machine, so we don't copy them.
        // If we copy them, Cranelift will create iconst instructions for them
        // instead of using the loaded values.
        eprintln!("[ASYNC DEBUG] Copying values from original func to wrapper (excluding Parameters)");
        eprintln!("[ASYNC DEBUG]   Original func has {} values", original_func.values.len());
        for (hir_id, hir_value) in &original_func.values {
            // Skip Parameter values - they will be loaded from state machine
            if matches!(hir_value.kind, HirValueKind::Parameter(_)) {
                eprintln!("[ASYNC DEBUG]   SKIPPING {:?} -> {:?} (will be loaded from state machine)", hir_id, hir_value.kind);
                continue;
            }
            eprintln!("[ASYNC DEBUG]   Copying value {:?} -> {:?}", hir_id, hir_value.kind);
            if !wrapper.values.contains_key(hir_id) {
                wrapper.values.insert(*hir_id, hir_value.clone());
            }
        }

        // Get self parameter (state machine pointer)
        let self_param = wrapper.create_value(
            HirType::Ptr(Box::new(HirType::Opaque(InternedString::new_global("StateMachine")))),
            HirValueKind::Parameter(0)
        );

        // Create a new entry block that loads parameters from state machine
        let param_load_block_id = HirId::new();
        let mut param_load_block = HirBlock::new(param_load_block_id);
        param_load_block.label = Some(InternedString::new_global("param_load"));

        // Load captured parameters from the state machine struct
        eprintln!("[ASYNC DEBUG] build_simple_async_wrapper: {} captures", state_machine.captures.len());
        let mut current_offset: i64 = 4; // Start after state field
        for capture in &state_machine.captures {
            let offset_const = wrapper.create_value(HirType::I64, HirValueKind::Constant(HirConstant::I64(current_offset)));
            let field_ptr = wrapper.create_value(HirType::Ptr(Box::new(capture.ty.clone())), HirValueKind::Instruction);

            param_load_block.add_instruction(HirInstruction::Binary {
                result: field_ptr,
                op: BinaryOp::Add,
                ty: HirType::I64,
                left: self_param,
                right: offset_const,
            });

            param_load_block.add_instruction(HirInstruction::Load {
                result: capture.id,
                ty: capture.ty.clone(),
                ptr: field_ptr,
                align: 4,
                volatile: false,
            });

            wrapper.values.insert(capture.id, HirValue {
                id: capture.id,
                ty: capture.ty.clone(),
                kind: HirValueKind::Instruction,
                uses: HashSet::new(),
                span: None,
            });

            current_offset += 4;
        }

        // Branch to original entry block
        param_load_block.set_terminator(HirTerminator::Branch {
            target: original_func.entry_block,
        });

        // Copy all blocks from original function, transforming Return terminators
        for (block_id, block) in &original_func.blocks {
            let mut new_block = HirBlock::new(*block_id);
            new_block.label = block.label;
            new_block.phis = block.phis.clone();
            new_block.instructions = block.instructions.clone();

            // Transform terminator
            let new_terminator = match &block.terminator {
                HirTerminator::Return { values } => {
                    eprintln!("[ASYNC DEBUG] Transforming return with values: {:?}", values);
                    // Transform return to Poll::Ready(value)
                    if let Some(v) = values.first() {
                        eprintln!("[ASYNC DEBUG]   Return value: {:?}", v);
                        eprintln!("[ASYNC DEBUG]   Is {:?} in wrapper.values? {}", v, wrapper.values.contains_key(v));
                        // Cast to i64
                        let cast_result = wrapper.create_value(HirType::I64, HirValueKind::Instruction);
                        new_block.add_instruction(HirInstruction::Cast {
                            op: CastOp::SExt,
                            result: cast_result,
                            ty: HirType::I64,
                            operand: *v,
                        });
                        HirTerminator::Return { values: vec![cast_result] }
                    } else {
                        // Void return -> return 1 (Ready with no value)
                        let ready_const = wrapper.create_value(HirType::I64, HirValueKind::Constant(HirConstant::I64(1)));
                        HirTerminator::Return { values: vec![ready_const] }
                    }
                }
                other => other.clone(),
            };
            new_block.set_terminator(new_terminator);

            wrapper.blocks.insert(*block_id, new_block);
        }

        // Add the param load block and set it as entry
        wrapper.blocks.insert(param_load_block_id, param_load_block);
        wrapper.entry_block = param_load_block_id;

        eprintln!("[ASYNC DEBUG] build_simple_async_wrapper complete:");
        eprintln!("[ASYNC DEBUG]   Entry block: {:?}", wrapper.entry_block);
        eprintln!("[ASYNC DEBUG]   Total blocks: {}", wrapper.blocks.len());
        for (bid, block) in &wrapper.blocks {
            eprintln!("[ASYNC DEBUG]   Block {:?}: {} instructions, terminator = {:?}", bid, block.instructions.len(), block.terminator);
        }

        Ok(())
    }

    /// Build state machine dispatch logic
    ///
    /// This creates the switch dispatch that routes to the correct state handler
    /// based on the current state field in the state machine struct.
    fn build_state_dispatch(
        &self,
        wrapper: &mut HirFunction,
        state_machine: &AsyncStateMachine,
        original_func: &HirFunction,
    ) -> CompilerResult<()> {
        // Copy all values from the original function to the wrapper.
        // The state instructions reference HirIds from the original function,
        // so we need these values to exist in the wrapper's values map.
        for (hir_id, hir_value) in &original_func.values {
            if !wrapper.values.contains_key(hir_id) {
                wrapper.values.insert(*hir_id, hir_value.clone());
            }
        }

        // Create switch on current state
        let state_field = wrapper.create_value(HirType::U32, HirValueKind::Instruction);

        // Get self parameter (state machine pointer) - first parameter
        let self_param = wrapper.create_value(
            HirType::Ptr(Box::new(HirType::Opaque(InternedString::new_global("StateMachine")))),
            HirValueKind::Parameter(0)
        );

        // Load current state from state machine's first field (state: u32)
        let load_state = HirInstruction::Load {
            result: state_field,
            ty: HirType::U32,
            ptr: self_param,
            align: 4,
            volatile: false,
        };

        // Create blocks for each state and build switch cases
        let mut cases = Vec::new();
        let mut state_blocks = Vec::new();

        for (state_id, state) in &state_machine.states {
            // Create a block for this state's handler
            let block_id = HirId::new();
            let mut state_block = HirBlock::new(block_id);
            state_block.label = Some(InternedString::new_global(&format!("state_{}", state_id.0)));

            // Load captured parameters from the state machine struct
            // Layout: { state: u32 (offset 0), param1: T1 (offset 4), param2: T2 (offset 8), ... }
            let mut current_offset: i64 = 4; // Start after state field
            for capture in &state_machine.captures {
                // Create offset constant
                let offset_const = wrapper.create_value(HirType::I64, HirValueKind::Constant(HirConstant::I64(current_offset)));

                // Calculate pointer: self_param + offset
                let field_ptr = wrapper.create_value(HirType::Ptr(Box::new(capture.ty.clone())), HirValueKind::Instruction);
                state_block.add_instruction(HirInstruction::Binary {
                    result: field_ptr,
                    op: BinaryOp::Add,
                    ty: HirType::I64,
                    left: self_param,
                    right: offset_const,
                });

                // Load the captured value - use the ORIGINAL capture HirId so instructions can reference it
                // This is the key: we're defining a value with the capture's original HirId
                state_block.add_instruction(HirInstruction::Load {
                    result: capture.id,
                    ty: capture.ty.clone(),
                    ptr: field_ptr,
                    align: 4,
                    volatile: false,
                });

                // Register this value in the wrapper's values map
                wrapper.values.insert(capture.id, HirValue {
                    id: capture.id,
                    ty: capture.ty.clone(),
                    kind: HirValueKind::Instruction,
                    uses: HashSet::new(),
                    span: None,
                });

                // Move to next field (assuming 4 bytes for i32, should match constructor)
                current_offset += 4;
            }

            // Add instructions from this state
            // Values from the original function have already been copied to the wrapper,
            // so constants and other values referenced by these instructions should be available.
            for inst in &state.instructions {
                state_block.add_instruction(inst.clone());
            }

            // Set terminator based on async terminator
            // Poll convention: 0 = Pending, non-zero = Ready(value)
            let terminator = match &state.terminator {
                AsyncTerminator::Return { value } => {
                    // Return Poll::Ready(value)
                    // For Ready, we return the value itself (or 1 if void)
                    if let Some(v) = value {
                        // Cast the return value to i64 (poll function returns i64)
                        // We need to sign-extend i32 to i64
                        let cast_result = wrapper.create_value(HirType::I64, HirValueKind::Instruction);
                        state_block.add_instruction(HirInstruction::Cast {
                            op: CastOp::SExt,
                            result: cast_result,
                            ty: HirType::I64,
                            operand: *v,
                        });
                        HirTerminator::Return { values: vec![cast_result] }
                    } else {
                        // Void return - create a "Ready with no value" constant (use 1)
                        let ready_const = wrapper.create_value(HirType::I64, HirValueKind::Constant(HirConstant::I64(1)));
                        HirTerminator::Return { values: vec![ready_const] }
                    }
                }
                AsyncTerminator::Await { future: _, resume_state } => {
                    // Update state and return Poll::Pending (0)
                    // Store new state value before returning
                    let new_state_val = wrapper.create_value(HirType::U32, HirValueKind::Instruction);
                    let resume_state_const = wrapper.create_value(HirType::U32, HirValueKind::Constant(HirConstant::U32(resume_state.0)));
                    let store_new_state = HirInstruction::Store {
                        value: new_state_val,
                        ptr: self_param,
                        align: 4,
                        volatile: false,
                    };
                    state_block.add_instruction(HirInstruction::Binary {
                        result: new_state_val,
                        op: BinaryOp::Add,
                        ty: HirType::U32,
                        left: state_field,
                        right: resume_state_const,
                    });
                    state_block.add_instruction(store_new_state);

                    // Return Pending (0)
                    let pending_const = wrapper.create_value(HirType::I64, HirValueKind::Constant(HirConstant::I64(0)));
                    HirTerminator::Return { values: vec![pending_const] }
                }
                AsyncTerminator::Continue { next_state: _ } => {
                    // Continue to next state - return Pending (0) to indicate not done yet
                    let pending_const = wrapper.create_value(HirType::I64, HirValueKind::Constant(HirConstant::I64(0)));
                    HirTerminator::Return { values: vec![pending_const] }
                }
                AsyncTerminator::Yield { value, resume_state: _ } => {
                    // Yield value and return Poll::Ready(Some(value))
                    HirTerminator::Return { values: vec![*value] }
                }
                AsyncTerminator::Panic { .. } => {
                    HirTerminator::Unreachable
                }
            };
            state_block.set_terminator(terminator);

            cases.push((HirConstant::U32(state_id.0), block_id));
            state_blocks.push((block_id, state_block));
        }

        // Create unreachable default block (for invalid state values)
        let default_block_id = HirId::new();
        let mut default_block = HirBlock::new(default_block_id);
        default_block.label = Some(InternedString::new_global("state_invalid"));
        default_block.set_terminator(HirTerminator::Unreachable);

        // Set up entry block with switch
        let entry_block = wrapper.blocks.get_mut(&wrapper.entry_block).unwrap();
        entry_block.add_instruction(load_state);
        entry_block.set_terminator(HirTerminator::Switch {
            value: state_field,
            default: default_block_id,
            cases,
        });

        // Add all state blocks to the function
        wrapper.blocks.insert(default_block_id, default_block);
        for (block_id, block) in state_blocks {
            wrapper.blocks.insert(block_id, block);
        }

        Ok(())
    }

    /// Generate the state machine struct type
    ///
    /// Creates a struct with:
    /// - state: u32 (current state)
    /// - One field per captured variable
    pub fn generate_state_machine_struct(
        &self,
        state_machine: &AsyncStateMachine,
        arena: &mut zyntax_typed_ast::arena::AstArena,
    ) -> HirType {
        let mut fields = Vec::new();

        // Field 1: state: u32
        fields.push(HirType::U32);

        // Add captured variable fields
        for capture in &state_machine.captures {
            fields.push(capture.ty.clone());
        }

        HirType::Struct(HirStructType {
            name: Some(arena.intern_string("AsyncStateMachine")),
            fields,
            packed: false,
        })
    }

    /// Generate constructor function for the state machine
    ///
    /// Creates a function that:
    /// - Takes a pointer to output buffer as first parameter (sret convention)
    /// - Takes original function parameters as remaining parameters
    /// - Initializes state = 0
    /// - Stores parameters as captures
    /// - Returns void (struct is written to the output pointer)
    ///
    /// This uses explicit sret convention for reliable FFI with Rust.
    /// The struct layout is:
    /// - Offset 0: state (u32, 4 bytes)
    /// - Offset 4: first captured param (4 bytes for i32)
    /// - etc.
    pub fn generate_state_machine_constructor(
        &self,
        state_machine: &AsyncStateMachine,
        _struct_type: HirType,
        original_func: &HirFunction,
        arena: &mut zyntax_typed_ast::arena::AstArena,
    ) -> HirFunction {
        let entry_block_id = HirId::new();
        let mut blocks = IndexMap::new();
        let mut values = IndexMap::new();
        let mut instructions = Vec::new();

        // Parameter 0: output pointer (sret) - generic pointer type
        let out_ptr_id = HirId::new();
        let out_ptr_ty = HirType::Ptr(Box::new(HirType::U8)); // Generic byte pointer
        values.insert(out_ptr_id, HirValue {
            id: out_ptr_id,
            ty: out_ptr_ty.clone(),
            kind: HirValueKind::Parameter(0),
            uses: HashSet::new(),
            span: None,
        });

        // Create constant for state = 0
        let state_const_id = HirId::new();
        values.insert(state_const_id, HirValue {
            id: state_const_id,
            ty: HirType::U32,
            kind: HirValueKind::Constant(HirConstant::U32(0)),
            uses: HashSet::new(),
            span: None,
        });

        // Store state = 0 directly at the base pointer (offset 0)
        instructions.push(HirInstruction::Store {
            ptr: out_ptr_id,
            value: state_const_id,
            align: 4,
            volatile: false,
        });

        // Store captured parameters at successive offsets
        // Each field is 4 bytes apart (assuming i32 captures)
        // Offset: 0 = state (u32), 4 = first param, 8 = second param, etc.
        let mut current_offset: i64 = 4; // Start after state field

        for (idx, _capture) in state_machine.captures.iter().enumerate() {
            // Find the corresponding parameter
            if let Some(param) = original_func.signature.params.get(idx) {
                // Create parameter value reference (shifted by 1 for sret)
                let param_value_id = HirId::new();
                values.insert(param_value_id, HirValue {
                    id: param_value_id,
                    ty: param.ty.clone(),
                    kind: HirValueKind::Parameter((idx + 1) as u32), // +1 for sret
                    uses: HashSet::new(),
                    span: None,
                });

                // Create offset constant
                let offset_const_id = HirId::new();
                values.insert(offset_const_id, HirValue {
                    id: offset_const_id,
                    ty: HirType::I64,
                    kind: HirValueKind::Constant(HirConstant::I64(current_offset)),
                    uses: HashSet::new(),
                    span: None,
                });

                // Calculate pointer: base + offset using Binary Add (as pointer arithmetic)
                let field_ptr = HirId::new();
                values.insert(field_ptr, HirValue {
                    id: field_ptr,
                    ty: HirType::Ptr(Box::new(param.ty.clone())),
                    kind: HirValueKind::Instruction,
                    uses: HashSet::new(),
                    span: None,
                });

                instructions.push(HirInstruction::Binary {
                    result: field_ptr,
                    op: BinaryOp::Add,
                    ty: HirType::I64, // Pointer is 64-bit on modern systems
                    left: out_ptr_id,
                    right: offset_const_id,
                });

                // Store parameter value at the calculated address
                instructions.push(HirInstruction::Store {
                    ptr: field_ptr,
                    value: param_value_id,
                    align: 4,
                    volatile: false,
                });

                // Move to next field (4 bytes for i32)
                current_offset += 4;
            }
        }

        // Create entry block with void return
        blocks.insert(entry_block_id, HirBlock {
            id: entry_block_id,
            label: None,
            phis: Vec::new(),
            instructions,
            terminator: HirTerminator::Return {
                values: vec![], // Void return
            },
            dominance_frontier: HashSet::new(),
            predecessors: Vec::new(),
            successors: Vec::new(),
        });

        // Create constructor function signature
        // Resolve the original function name first, then create _new suffix
        let base_name = original_func.name.resolve_global()
            .unwrap_or_else(|| format!("async_fn_{:?}", original_func.id));
        let constructor_name = arena.intern_string(&format!("{}_new", base_name));

        // Build parameter list: first is output pointer (sret), then original params
        let mut params = vec![
            HirParam {
                id: HirId::new(),
                name: arena.intern_string("__sret"),
                ty: out_ptr_ty,
                attributes: ParamAttributes::default(),
            }
        ];
        params.extend(original_func.signature.params.clone());

        HirFunction {
            id: HirId::new(),
            name: constructor_name,
            signature: HirFunctionSignature {
                params,
                returns: vec![], // Void return - struct is written via sret pointer
                type_params: Vec::new(),
                const_params: Vec::new(),
                lifetime_params: Vec::new(),
                is_variadic: false,
                is_async: false,
            },
            entry_block: entry_block_id,
            blocks,
            locals: IndexMap::new(),
            values,
            previous_version: None,
            is_external: false,
            calling_convention: CallingConvention::C,
            attributes: FunctionAttributes::default(),
            link_name: None,
        }
    }
}

/// Await point in async function
#[derive(Debug, Clone)]
struct AwaitPoint {
    /// Block containing the await
    block_id: HirId,
    /// Index of await instruction in block
    instruction_index: usize,
    /// Future being awaited
    future: HirId,
    /// Result of the await
    result: Option<HirId>,
}

/// Code segment between await points
#[derive(Debug, Clone)]
struct CodeSegment {
    /// Instructions in this segment
    instructions: Vec<HirInstruction>,
    /// Terminator of this segment
    terminator: HirTerminator,
}

// Note: Intrinsics Await and Yield are defined in hir.rs

/// Extend HIR types for async support
impl HirType {
    /// Create a future type with arena support
    pub fn future_with_arena(result_type: HirType, arena: &mut zyntax_typed_ast::arena::AstArena) -> Self {
        HirType::Generic {
            base: Box::new(HirType::Opaque(arena.intern_string("Future"))),
            type_args: vec![result_type],
            const_args: vec![],
        }
    }
    
    /// Create an async function type with arena support
    pub fn async_function_with_arena(
        params: Vec<HirType>, 
        result_type: HirType, 
        arena: &mut zyntax_typed_ast::arena::AstArena
    ) -> Self {
        HirType::Function(Box::new(HirFunctionType {
            params,
            returns: vec![Self::future_with_arena(result_type, arena)],
            lifetime_params: vec![],
            is_variadic: false,
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use zyntax_typed_ast::arena::AstArena;
    
    fn create_test_arena() -> AstArena {
        AstArena::new()
    }
    
    fn intern_str(arena: &mut AstArena, s: &str) -> InternedString {
        arena.intern_string(s)
    }
    
    #[test]
    fn test_async_compiler_creation() {
        let compiler = AsyncCompiler::new();
        assert_eq!(compiler.next_state_id, 0);
        assert!(compiler.state_machines.is_empty());
    }
    
    #[test]
    fn test_async_state_machine_creation() {
        let mut arena = create_test_arena();
        let mut compiler = AsyncCompiler::new();
        
        // Create a simple async function
        let sig = HirFunctionSignature {
            params: vec![],
            returns: vec![HirType::future_with_arena(HirType::I32, &mut arena)],
            type_params: vec![],
            const_params: vec![],
            lifetime_params: vec![],
            is_variadic: false,
            is_async: true, // Test is for async function
        };
        
        let func = HirFunction::new(intern_str(&mut arena, "async_test"), sig);
        
        let result = compiler.compile_async_function(&func);
        assert!(result.is_ok());
        
        let state_machine = result.unwrap();
        assert_eq!(state_machine.original_function, func.id);
        assert!(state_machine.states.contains_key(&state_machine.initial_state));
    }
    
    #[test]
    fn test_async_types() {
        let mut arena = create_test_arena();
        let future_i32 = HirType::future_with_arena(HirType::I32, &mut arena);
        match future_i32 {
            HirType::Generic { base, type_args, .. } => {
                match base.as_ref() {
                    HirType::Opaque(_) => {
                        assert_eq!(type_args.len(), 1);
                        assert_eq!(type_args[0], HirType::I32);
                    }
                    _ => panic!("Expected opaque base type"),
                }
            }
            _ => panic!("Expected generic future type"),
        }
        
        let async_fn = HirType::async_function_with_arena(vec![HirType::I32], HirType::Ptr(Box::new(HirType::I8)), &mut arena);
        match async_fn {
            HirType::Function(func_ty) => {
                assert_eq!(func_ty.params.len(), 1);
                assert_eq!(func_ty.returns.len(), 1);
                assert!(matches!(func_ty.returns[0], HirType::Generic { .. }));
            }
            _ => panic!("Expected function type"),
        }
    }
    
    #[test]
    fn test_async_capture_modes() {
        let by_value = AsyncCaptureMode::ByValue;
        let by_ref = AsyncCaptureMode::ByRef(HirLifetime::anonymous());
        let by_mut_ref = AsyncCaptureMode::ByMutRef(HirLifetime::static_lifetime());
        
        assert_eq!(by_value, AsyncCaptureMode::ByValue);
        assert!(matches!(by_ref, AsyncCaptureMode::ByRef(_)));
        assert!(matches!(by_mut_ref, AsyncCaptureMode::ByMutRef(_)));
    }
    
    #[test]
    fn test_async_runtime_types() {
        let mut arena = create_test_arena();
        let tokio = AsyncRuntimeType::Tokio;
        let custom = AsyncRuntimeType::Custom(intern_str(&mut arena, "MyRuntime"));
        let none = AsyncRuntimeType::None;

        assert_eq!(tokio, AsyncRuntimeType::Tokio);
        assert!(matches!(custom, AsyncRuntimeType::Custom(_)));
        assert_eq!(none, AsyncRuntimeType::None);
    }

    #[test]
    fn test_state_machine_struct_generation() {
        let mut arena = create_test_arena();
        let compiler = AsyncCompiler::new();

        // Create a state machine with captures
        let capture = AsyncCapture {
            id: HirId::new(),
            name: intern_str(&mut arena, "x"),
            ty: HirType::I32,
            mode: AsyncCaptureMode::ByValue,
        };

        let state_machine = AsyncStateMachine {
            id: HirId::new(),
            original_function: HirId::new(),
            states: IndexMap::new(),
            initial_state: AsyncStateId(0),
            final_state: AsyncStateId(1),
            captures: vec![capture],
            result_type: HirType::I32,
        };

        let struct_type = compiler.generate_state_machine_struct(&state_machine, &mut arena);

        // Verify it's a struct type
        match struct_type {
            HirType::Struct(struct_ty) => {
                // Should have state + captured variables
                assert_eq!(struct_ty.fields.len(), 2); // state + x
                assert_eq!(struct_ty.fields[0], HirType::U32); // state field
                assert_eq!(struct_ty.fields[1], HirType::I32); // x field
                assert!(!struct_ty.packed);
            }
            _ => panic!("Expected struct type"),
        }
    }

    #[test]
    fn test_state_machine_constructor_generation() {
        let mut arena = create_test_arena();
        let compiler = AsyncCompiler::new();

        // Create a simple async function
        let func_id = HirId::new();
        let param = HirParam {
            id: HirId::new(),
            name: intern_str(&mut arena, "x"),
            ty: HirType::I32,
            attributes: ParamAttributes::default(),
        };

        let sig = HirFunctionSignature {
            params: vec![param],
            returns: vec![HirType::I32],
            type_params: vec![],
            const_params: vec![],
            lifetime_params: vec![],
            is_variadic: false,
            is_async: true,
        };

        let func = HirFunction {
            id: func_id,
            name: intern_str(&mut arena, "test_async"),
            signature: sig,
            entry_block: HirId::new(),
            blocks: IndexMap::new(),
            locals: IndexMap::new(),
            values: IndexMap::new(),
            previous_version: None,
            is_external: false,
            calling_convention: CallingConvention::C,
            attributes: FunctionAttributes::default(),
            link_name: None,
        };

        // Create state machine
        let capture = AsyncCapture {
            id: HirId::new(),
            name: intern_str(&mut arena, "x"),
            ty: HirType::I32,
            mode: AsyncCaptureMode::ByValue,
        };

        let state_machine = AsyncStateMachine {
            id: HirId::new(),
            original_function: func_id,
            states: IndexMap::new(),
            initial_state: AsyncStateId(0),
            final_state: AsyncStateId(1),
            captures: vec![capture],
            result_type: HirType::I32,
        };

        let struct_type = compiler.generate_state_machine_struct(&state_machine, &mut arena);
        let constructor = compiler.generate_state_machine_constructor(
            &state_machine,
            struct_type.clone(),
            &func,
            &mut arena
        );

        // Verify constructor signature
        assert_eq!(constructor.signature.params.len(), 1); // Original param
        assert_eq!(constructor.signature.returns.len(), 1);
        assert_eq!(constructor.signature.returns[0], struct_type);
        assert!(!constructor.signature.is_async); // Constructor is not async

        // Verify it has an entry block
        assert!(constructor.blocks.contains_key(&constructor.entry_block));

        // Verify entry block has return terminator
        let entry_block = &constructor.blocks[&constructor.entry_block];
        assert!(matches!(entry_block.terminator, HirTerminator::Return { .. }));

        // Verify return values
        match &entry_block.terminator {
            HirTerminator::Return { values } => {
                assert_eq!(values.len(), 1); // Should return struct instance
            }
            _ => panic!("Expected return terminator"),
        }
    }

    #[test]
    fn test_state_machine_struct_without_captures() {
        let mut arena = create_test_arena();
        let compiler = AsyncCompiler::new();

        // Create a state machine without captures
        let state_machine = AsyncStateMachine {
            id: HirId::new(),
            original_function: HirId::new(),
            states: IndexMap::new(),
            initial_state: AsyncStateId(0),
            final_state: AsyncStateId(1),
            captures: vec![], // No captures
            result_type: HirType::Void,
        };

        let struct_type = compiler.generate_state_machine_struct(&state_machine, &mut arena);

        // Should only have state field
        match struct_type {
            HirType::Struct(struct_ty) => {
                assert_eq!(struct_ty.fields.len(), 1); // Only state field
                assert_eq!(struct_ty.fields[0], HirType::U32);
            }
            _ => panic!("Expected struct type"),
        }
    }

    #[test]
    fn test_constructor_with_multiple_parameters() {
        let mut arena = create_test_arena();
        let compiler = AsyncCompiler::new();

        // Create async function with 3 parameters
        let func_id = HirId::new();
        let params = vec![
            HirParam {
                id: HirId::new(),
                name: intern_str(&mut arena, "x"),
                ty: HirType::I32,
                attributes: ParamAttributes::default(),
            },
            HirParam {
                id: HirId::new(),
                name: intern_str(&mut arena, "y"),
                ty: HirType::I64,
                attributes: ParamAttributes::default(),
            },
            HirParam {
                id: HirId::new(),
                name: intern_str(&mut arena, "msg"),
                ty: HirType::Ptr(Box::new(HirType::I8)),
                attributes: ParamAttributes::default(),
            },
        ];

        let sig = HirFunctionSignature {
            params: params.clone(),
            returns: vec![HirType::Void],
            type_params: vec![],
            const_params: vec![],
            lifetime_params: vec![],
            is_variadic: false,
            is_async: true,
        };

        let func = HirFunction {
            id: func_id,
            name: intern_str(&mut arena, "multi_param_async"),
            signature: sig,
            entry_block: HirId::new(),
            blocks: IndexMap::new(),
            locals: IndexMap::new(),
            values: IndexMap::new(),
            previous_version: None,
            is_external: false,
            calling_convention: CallingConvention::C,
            attributes: FunctionAttributes::default(),
            link_name: None,
        };

        // Create captures for all 3 parameters
        let captures = vec![
            AsyncCapture {
                id: HirId::new(),
                name: intern_str(&mut arena, "x"),
                ty: HirType::I32,
                mode: AsyncCaptureMode::ByValue,
            },
            AsyncCapture {
                id: HirId::new(),
                name: intern_str(&mut arena, "y"),
                ty: HirType::I64,
                mode: AsyncCaptureMode::ByValue,
            },
            AsyncCapture {
                id: HirId::new(),
                name: intern_str(&mut arena, "msg"),
                ty: HirType::Ptr(Box::new(HirType::I8)),
                mode: AsyncCaptureMode::ByValue,
            },
        ];

        let state_machine = AsyncStateMachine {
            id: HirId::new(),
            original_function: func_id,
            states: IndexMap::new(),
            initial_state: AsyncStateId(0),
            final_state: AsyncStateId(1),
            captures,
            result_type: HirType::Void,
        };

        let struct_type = compiler.generate_state_machine_struct(&state_machine, &mut arena);
        let constructor = compiler.generate_state_machine_constructor(
            &state_machine,
            struct_type.clone(),
            &func,
            &mut arena
        );

        // Verify struct has 4 fields (state + 3 params)
        match &struct_type {
            HirType::Struct(struct_ty) => {
                assert_eq!(struct_ty.fields.len(), 4); // state + x + y + msg
                assert_eq!(struct_ty.fields[0], HirType::U32); // state
                assert_eq!(struct_ty.fields[1], HirType::I32); // x
                assert_eq!(struct_ty.fields[2], HirType::I64); // y
                assert_eq!(struct_ty.fields[3], HirType::Ptr(Box::new(HirType::I8))); // msg
            }
            _ => panic!("Expected struct type"),
        }

        // Verify constructor has InsertValue instructions
        let entry_block = &constructor.blocks[&constructor.entry_block];

        // Should have 4 InsertValue instructions (state + 3 params)
        let insert_count = entry_block.instructions.iter()
            .filter(|inst| matches!(inst, HirInstruction::InsertValue { .. }))
            .count();
        assert_eq!(insert_count, 4);

        // Verify the InsertValue instructions have correct indices
        for (idx, inst) in entry_block.instructions.iter().enumerate() {
            if let HirInstruction::InsertValue { indices, .. } = inst {
                assert_eq!(indices.len(), 1);
                assert_eq!(indices[0], idx as u32); // Index matches instruction order
            }
        }

        // Verify parameters are properly referenced
        let param_values: Vec<_> = constructor.values.values()
            .filter(|v| matches!(v.kind, HirValueKind::Parameter(_)))
            .collect();
        assert_eq!(param_values.len(), 3); // 3 parameters
    }
}