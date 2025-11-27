//! # Async/Coroutine Support for HIR
//! 
//! Implements async function compilation, coroutine state machines, and
//! async runtime integration for the HIR. This module provides the foundation
//! for async/await syntax and coroutine-based programming models.

use std::collections::{HashSet, VecDeque};
use indexmap::IndexMap;
use crate::hir::*;
use crate::{CompilerResult, CompilerError};
use zyntax_typed_ast::InternedString;

/// Async function state machine representation
#[derive(Debug, Clone)]
pub struct AsyncStateMachine {
    /// Unique identifier for this state machine
    pub id: HirId,
    /// Original async function
    pub original_function: HirId,
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
            states: IndexMap::new(),
            initial_state,
            final_state,
            captures: self.analyze_captures(func)?,
            result_type: func.signature.returns.first().cloned().unwrap_or(HirType::Void),
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
    fn analyze_captures(&self, func: &HirFunction) -> CompilerResult<Vec<AsyncCapture>> {
        let mut captures = Vec::new();
        
        // For now, capture all parameters by value
        // Real implementation would do escape analysis
        for param in &func.signature.params {
            captures.push(AsyncCapture {
                id: param.id,
                name: param.name,
                ty: param.ty.clone(),
                mode: AsyncCaptureMode::ByValue,
            });
        }
        
        Ok(captures)
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
                // Final segment
                AsyncTerminator::Return { value: None }
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
    fn split_at_await_points(
        &self,
        func: &HirFunction,
        await_points: &[AwaitPoint],
    ) -> CompilerResult<Vec<CodeSegment>> {
        let mut segments = Vec::new();
        
        // For simplicity, assume single basic block for now
        // Real implementation would handle complex control flow
        if let Some(entry_block) = func.blocks.get(&func.entry_block) {
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
    fn extract_locals_from_segment(&self, _segment: &CodeSegment) -> CompilerResult<Vec<AsyncLocal>> {
        // For now, return empty - real implementation would analyze instructions
        Ok(Vec::new())
    }
    
    /// Generate next state ID
    fn next_state_id(&mut self) -> AsyncStateId {
        let id = AsyncStateId(self.next_state_id);
        self.next_state_id += 1;
        id
    }
    
    /// Generate async wrapper function with arena
    pub fn generate_async_wrapper_with_arena(
        &self, 
        state_machine: &AsyncStateMachine,
        arena: &mut zyntax_typed_ast::arena::AstArena,
    ) -> CompilerResult<HirFunction> {
        // Create wrapper function that implements the Future trait
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
            returns: vec![HirType::Opaque(arena.intern_string("Poll"))],
            type_params: vec![],
            const_params: vec![],
            lifetime_params: vec![],
            is_variadic: false,
            is_async: false, // The wrapper function itself is not async (it's a poll function)
        };
        
        let mut wrapper = HirFunction::new(
            arena.intern_string("async_wrapper"),
            wrapper_sig
        );
        
        // Build state machine dispatch logic
        self.build_state_dispatch(&mut wrapper, state_machine)?;
        
        Ok(wrapper)
    }
    
    /// Build state machine dispatch logic
    fn build_state_dispatch(
        &self,
        wrapper: &mut HirFunction,
        state_machine: &AsyncStateMachine,
    ) -> CompilerResult<()> {
        // Create switch on current state
        let state_field = wrapper.create_value(HirType::U32, HirValueKind::Instruction);
        
        // Load current state from state machine  
        // Note: In real implementation, this would use proper arena-allocated strings
        let self_param = wrapper.create_value(
            HirType::Ptr(Box::new(HirType::I8)), // Use simple type for now
            HirValueKind::Parameter(0)
        );
        
        let load_state = HirInstruction::Load {
            result: state_field,
            ty: HirType::U32,
            ptr: self_param,
            align: 4,
            volatile: false,
        };
        
        // Create switch cases for each state
        let mut cases = Vec::new();
        for (state_id, _state) in &state_machine.states {
            cases.push((
                HirConstant::U32(state_id.0),
                HirId::new(), // Target block for this state
            ));
        }
        
        let entry_block = wrapper.blocks.get_mut(&wrapper.entry_block).unwrap();
        entry_block.add_instruction(load_state);
        entry_block.set_terminator(HirTerminator::Switch {
            value: state_field,
            default: HirId::new(), // Default case
            cases,
        });
        
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
    /// - Takes original function parameters
    /// - Creates state machine instance
    /// - Initializes state = 0
    /// - Stores parameters as captures
    /// - Returns state machine
    pub fn generate_state_machine_constructor(
        &self,
        state_machine: &AsyncStateMachine,
        struct_type: HirType,
        original_func: &HirFunction,
        arena: &mut zyntax_typed_ast::arena::AstArena,
    ) -> HirFunction {
        let entry_block_id = HirId::new();
        let mut blocks = IndexMap::new();
        let mut values = IndexMap::new();
        let mut instructions = Vec::new();

        // Create constant for state = 0
        let state_const_id = HirId::new();
        values.insert(state_const_id, HirValue {
            id: state_const_id,
            ty: HirType::U32,
            kind: HirValueKind::Constant(HirConstant::U32(0)),
            uses: HashSet::new(),
            span: None,
        });

        // Build struct by inserting fields one by one
        // Start with an empty/undefined struct
        let mut current_struct_id = HirId::new();
        values.insert(current_struct_id, HirValue {
            id: current_struct_id,
            ty: struct_type.clone(),
            kind: HirValueKind::Undef,
            uses: HashSet::new(),
            span: None,
        });

        // Field 0: Insert state = 0
        let struct_with_state = HirId::new();
        values.insert(struct_with_state, HirValue {
            id: struct_with_state,
            ty: struct_type.clone(),
            kind: HirValueKind::Instruction,
            uses: HashSet::new(),
            span: None,
        });

        instructions.push(HirInstruction::InsertValue {
            result: struct_with_state,
            ty: struct_type.clone(),
            aggregate: current_struct_id,
            value: state_const_id,
            indices: vec![0],
        });

        current_struct_id = struct_with_state;

        // Insert captured parameters (fields 1, 2, 3, ...)
        for (idx, capture) in state_machine.captures.iter().enumerate() {
            // Find the corresponding parameter
            if let Some(param) = original_func.signature.params.get(idx) {
                // Create parameter value reference
                let param_value_id = HirId::new();
                values.insert(param_value_id, HirValue {
                    id: param_value_id,
                    ty: param.ty.clone(),
                    kind: HirValueKind::Parameter(idx as u32),
                    uses: HashSet::new(),
                    span: None,
                });

                // Insert parameter into struct
                let new_struct_id = HirId::new();
                values.insert(new_struct_id, HirValue {
                    id: new_struct_id,
                    ty: struct_type.clone(),
                    kind: HirValueKind::Instruction,
                    uses: HashSet::new(),
                    span: None,
                });

                instructions.push(HirInstruction::InsertValue {
                    result: new_struct_id,
                    ty: struct_type.clone(),
                    aggregate: current_struct_id,
                    value: param_value_id,
                    indices: vec![(idx + 1) as u32], // +1 because field 0 is state
                });

                current_struct_id = new_struct_id;
            }
        }

        // Final result is the fully constructed struct
        let result_id = current_struct_id;

        // Create entry block with return
        blocks.insert(entry_block_id, HirBlock {
            id: entry_block_id,
            label: None,
            phis: Vec::new(),
            instructions,
            terminator: HirTerminator::Return {
                values: vec![result_id],
            },
            dominance_frontier: HashSet::new(),
            predecessors: Vec::new(),
            successors: Vec::new(),
        });

        // Create constructor function signature
        let constructor_name = arena.intern_string(&format!("{}_new",
            original_func.name));

        HirFunction {
            id: HirId::new(),
            name: constructor_name,
            signature: HirFunctionSignature {
                params: original_func.signature.params.clone(),
                returns: vec![struct_type],
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