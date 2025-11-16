# Zyntax Compiler Architecture: Complete Integration Map

## Project Structure

```
zyntax/
├── crates/
│   ├── typed_ast/          # Multi-language typed AST + type system
│   ├── compiler/           # Compilation pipeline (TypedAST → HIR → Native + Runtime)
│   └── whirlwind_adapter/  # Whirlwind language integration
└── [docs, examples, tests]
```

---

## LAYER 1: TYPED-AST CRATE (Input Representation)

### Location
`/Users/amaterasu/Vibranium/zyntax/crates/typed_ast/src/`

### Core Purpose
Language-agnostic typed abstract syntax tree serving as the common intermediate representation for multiple languages (Rust, Java, C#, TypeScript, Haxe).

### Main Types & Structure

#### 1. **Type System Foundation** (`type_registry.rs`)
```rust
pub struct TypeRegistry {
    // Manages all type definitions and lookups
}

pub enum Type {
    // Primitive types
    Primitive(PrimitiveType),
    
    // User-defined types
    Named { id: TypeId, type_args, const_args, variance, nullability },
    
    // Function types with enhanced parameter info
    Function { params, return_type, is_varargs, has_named_params, ... },
    
    // Complex types
    Tuple(Vec<Type>),
    Array { element_type, size, nullability },
    Reference { ty, mutability, lifetime, nullability },
    Optional(Box<Type>),
    Union(Vec<Type>),
    Intersection(Vec<Type>),
    Trait { id, associated_types, super_traits },
    Interface { methods, is_structural, nullability },
    Struct { fields, is_anonymous, nullability },
    
    // Advanced features
    TypeVar(TypeVar),               // Type inference variable
    ConstVar { id, name, const_type },  // Const generic variable
    ConstDependent { base_type, constraint },  // Dependent types
    Dynamic,                        // Gradual typing
    Unknown,                        // TypeScript-style unknown
    Never,                          // Bottom type (diverging)
    Any,                           // Top type
}

pub enum PrimitiveType {
    I8, I16, I32, I64, I128,
    U8, U16, U32, U64, U128,
    F32, F64,
    Bool, Char, String,
    Unit,  // void/unit
}

// Const value representation for const generics
pub enum ConstValue {
    Int(i64), UInt(u64), Bool(bool), String(InternedString),
    Array(Vec<ConstValue>), Tuple(Vec<ConstValue>),
    Struct(Vec<(InternedString, ConstValue)>),
    BinaryOp { op, left, right },
    UnaryOp { op, operand },
}
```

**Key Functions:**
- `register_type()` - Register type definitions
- `register_struct_type()` - Register struct types
- `register_trait()` - Register trait definitions
- `register_implementation()` - Register trait implementations
- `register_alias()` - Register type aliases

#### 2. **Typed AST Structure** (`typed_ast.rs`)
```rust
pub struct TypedProgram {
    pub declarations: Vec<TypedNode<TypedDeclaration>>,
}

pub enum TypedDeclaration {
    Function(TypedFunction),
    Variable(TypedVariable),
    Class(TypedClass),
    Interface(TypedInterface),
    Enum(TypedEnum),
    TypeAlias(TypedTypeAlias),
    Module(TypedModule),
    Import(TypedImport),
}

pub struct TypedFunction {
    pub name: InternedString,
    pub params: Vec<TypedParameter>,
    pub return_type: Type,
    pub body: TypedBlock,
    pub visibility: Visibility,
    pub is_async: bool,
}

pub struct TypedParameter {
    pub name: InternedString,
    pub ty: Type,
    pub mutability: Mutability,
    pub kind: ParameterKind,  // Regular, Out, Ref, InOut, Rest, Optional, etc.
    pub default_value: Option<Box<TypedNode<TypedExpression>>>,
}

pub enum TypedExpression {
    Literal(TypedLiteral),
    Variable(InternedString),
    Binary(TypedBinary),
    Unary(TypedUnary),
    Call(TypedCall),
    MethodCall(TypedMethodCall),
    Field(TypedFieldAccess),
    Index(TypedIndex),
    Array(Vec<TypedNode<TypedExpression>>),
    Tuple(Vec<TypedNode<TypedExpression>>),
    Struct(TypedStructLiteral),
    Lambda(TypedLambda),
    Match(TypedMatchExpr),
    If(TypedIfExpr),
    Cast(TypedCast),
    Await(Box<TypedNode<TypedExpression>>),
    Try(Box<TypedNode<TypedExpression>>),
    Reference(TypedReference),
    Dereference(Box<TypedNode<TypedExpression>>),
}

pub enum TypedStatement {
    Expression(Box<TypedNode<TypedExpression>>),
    Let(TypedLet),
    Return(Option<Box<TypedNode<TypedExpression>>>),
    If(TypedIf),
    While(TypedWhile),
    For(TypedFor),
    ForCStyle(TypedForCStyle),
    Loop(TypedLoop),
    Match(TypedMatch),
    Try(TypedTry),
    Block(TypedBlock),
    Coroutine(TypedCoroutine),
    Defer(TypedDefer),
    Select(TypedSelect),
}

// Every node wraps content with type and span information
pub struct TypedNode<T> {
    pub node: T,
    pub ty: Type,
    pub span: Span,
}
```

**Key Features:**
- Full type annotations on all nodes
- Support for multiple language paradigms (OOP, functional, etc.)
- Named arguments in function calls
- Parameter kinds (optional, out, ref, inout, rest, keyword-only, positional-only)
- Async/await support
- Pattern matching
- Exception handling (try/catch)
- Coroutines and generators

#### 3. **Advanced Analysis** (`advanced_analysis.rs`)
```rust
pub struct DataFlowGraph { /* CFG + data dependencies */ }
pub struct ControlFlowGraph { /* Block connectivity */ }
pub struct OwnershipAnalysis { /* Rust-style ownership */ }
pub struct LifetimeAnalysis { /* Lifetime constraints */ }
```

#### 4. **Type Checking & Inference**
- `type_checker.rs` - Sound type checking
- `type_inference.rs` - Hindley-Milner style inference with extensions
- `constraint_solver.rs` - Constraint solving for type unification
- `nominal_type_checker.rs` - Class/trait-based type checking
- `structural_type_checker.rs` - Go/TypeScript-style structural typing
- `gradual_type_checker.rs` - TypeScript-style gradual typing with runtime checks

#### 5. **Specialized Type Systems**
- `linear_types.rs` - Rust-style linear/affine types
- `dependent_types.rs` - Refinement types and dependent typing
- `effect_system.rs` - Effect tracking for purity/exceptions
- `const_evaluator.rs` - Compile-time const evaluation

**Module Entry Point:**
```rust
pub fn typed_builder() -> TypedASTBuilder
```

**Key Re-exports:**
```rust
pub use typed_ast::{
    TypedNode, TypedProgram, TypedDeclaration, TypedFunction,
    TypedStatement, TypedExpression, BinaryOp, UnaryOp,
};
pub use type_registry::{Type, TypeId, PrimitiveType, TypeRegistry, ...};
```

---

## LAYER 2: COMPILER CRATE (Lowering & Code Generation)

### Location
`/Users/amaterasu/Vibranium/zyntax/crates/compiler/src/`

### Core Pipeline
```
TypedAST → Lowering Context → HIR (SSA Form) → Analysis → Optimization → Backend
```

### Phase 1: Lowering (`lowering.rs`)

#### Purpose
Transform TypedAST to HIR with complete type information preservation.

#### Main Components

```rust
pub struct LoweringContext {
    pub module: HirModule,
    pub type_registry: Arc<TypeRegistry>,
    pub symbols: SymbolTable,
    pub diagnostics: Vec<LoweringDiagnostic>,
    pub config: LoweringConfig,
}

pub struct LoweringConfig {
    pub debug_info: bool,
    pub opt_level: u8,
    pub target_triple: String,
    pub hot_reload: bool,
}

pub trait AstLowering {
    fn lower_program(&mut self, program: &TypedProgram) -> CompilerResult<HirModule>;
}
```

#### Lowering Process

**Entry Point:**
```rust
impl AstLowering for LoweringContext {
    fn lower_program(&mut self, program: &TypedProgram) -> CompilerResult<HirModule> {
        // Phase 1: Collect all declarations
        self.collect_declarations(program)?;
        
        // Phase 2: Lower each declaration
        for decl in &program.declarations {
            self.lower_declaration(decl)?;
        }
        
        // Phase 3: Resolve forward references
        self.resolve_references()?;
        
        // Handle hot-reloading
        if self.config.hot_reload {
            self.module.increment_version();
        }
        
        Ok(self.module.clone())
    }
}
```

**Lowering Steps for Functions:**
1. Convert function signature (TypedFunction → HirFunctionSignature)
2. Compute parameter attributes (by_ref, sret, zext, sext, noalias, nonnull, readonly)
3. Build CFG from function body
4. Convert CFG to SSA form
5. Verify SSA properties
6. Optimize trivial phi nodes

**Type Conversion** (TypedAST Type → HIR Type):
```rust
fn convert_type(&self, ty: &Type) -> HirType {
    Type::Primitive(prim) → HirType::I8/I16/I32/.../F32/F64/Bool/Void
    Type::Tuple(types)    → HirType::Struct{fields: Vec<HirType>}
    Type::Reference{ty}   → HirType::Ptr(Box<HirType>)
    Type::Array{ty, size} → HirType::Array(Box<HirType>, u64)
    Type::Function{...}   → HirType::Function(Box<HirFunctionType>)
}
```

**Lowering Passes** (pluggable pipeline):
```rust
pub trait LoweringPass: Send + Sync {
    fn name(&self) -> &'static str;
    fn dependencies(&self) -> &[&'static str];
    fn run(&mut self, context: &mut LoweringContext) -> CompilerResult<()>;
}

pub struct LoweringPipeline {
    passes: Vec<Box<dyn LoweringPass>>,
}

// Standard passes:
pub mod passes {
    pub struct CfgConstructionPass;       // Build control flow graph
    pub struct SsaConstructionPass;       // Convert CFG to SSA form
    pub struct TypeValidationPass;        // Validate HIR types
}
```

### Phase 2: Control Flow Graph (`cfg.rs`)

#### Purpose
Construct CFG from TypedAST statements, preparing for SSA conversion.

#### Components

```rust
pub struct ControlFlowGraph {
    pub graph: DiGraph<BasicBlock, CfgEdge>,
    pub entry: NodeIndex,
    pub exit: NodeIndex,
    pub block_map: HashMap<HirId, NodeIndex>,
    pub dominance: Option<DominanceInfo>,
    pub loops: Vec<LoopInfo>,
}

pub struct BasicBlock {
    pub id: HirId,
    pub label: Option<InternedString>,
    pub statements: Vec<TypedNode<TypedStatement>>,
    pub terminator: Option<TypedNode<TypedStatement>>,
    pub live_in: HashSet<InternedString>,
    pub live_out: HashSet<InternedString>,
}

pub enum CfgEdge {
    Unconditional,
    True,
    False,
    Case(i64),
    Default,
    Exception,
}

pub struct DominanceInfo {
    pub idom: HashMap<NodeIndex, NodeIndex>,
    pub dom_tree: HashMap<NodeIndex, Vec<NodeIndex>>,
    pub dom_frontier: HashMap<NodeIndex, HashSet<NodeIndex>>,
}

pub struct LoopInfo {
    pub header: NodeIndex,
    pub blocks: HashSet<NodeIndex>,
    pub back_edges: Vec<(NodeIndex, NodeIndex)>,
    pub exits: HashSet<NodeIndex>,
    pub depth: u32,
}

pub struct CfgBuilder { /* builds CFG from TypedAST */ }
```

**Key Methods:**
- `build_from_block(&TypedBlock)` - Construct CFG from block
- Computes dominance tree, loop nesting, liveness information

### Phase 3: SSA Form (`ssa.rs`)

#### Purpose
Convert CFG to Static Single Assignment form using efficient algorithm.

#### Components

```rust
pub struct SsaBuilder {
    function: HirFunction,
    definitions: HashMap<HirId, HashMap<InternedString, HirId>>,
    incomplete_phis: HashMap<(HirId, InternedString), HirId>,
    var_counter: HashMap<InternedString, u32>,
    sealed_blocks: HashSet<HirId>,
}

pub struct SsaForm {
    pub function: HirFunction,
    pub def_use_chains: HashMap<HirId, HashSet<HirId>>,
    pub use_def_chains: HashMap<HirId, HirId>,
}

pub struct PhiNode {
    pub result: HirId,
    pub variable: InternedString,
    pub block: HirId,
    pub operands: Vec<(HirId, HirId)>, // (value, predecessor_block)
}
```

**Algorithm:**
1. Process blocks in dominance order
2. Create incomplete phi nodes
3. Fill incomplete phis after all blocks processed
4. Build def-use chains

### Phase 4: HIR - High-level Intermediate Representation (`hir.rs`)

#### Purpose
Platform-agnostic IR compatible with both Cranelift and LLVM.

#### Components

```rust
pub struct HirId(Uuid);  // Unique identifier for all HIR entities

pub struct HirModule {
    pub id: HirId,
    pub name: InternedString,
    pub functions: HashMap<HirId, HirFunction>,
    pub globals: HashMap<HirId, HirGlobal>,
    pub types: HashMap<TypeId, HirType>,
    pub imports: Vec<HirImport>,
    pub exports: Vec<HirExport>,
    pub version: u64,  // For hot-reloading
    pub dependencies: HashSet<HirId>,
}

impl HirModule {
    pub fn new(name: InternedString) -> Self { ... }
    pub fn add_function(&mut self, func: HirFunction) { ... }
    pub fn add_global(&mut self, global: HirGlobal) { ... }
    pub fn increment_version(&mut self) { ... }
}

pub struct HirFunction {
    pub id: HirId,
    pub name: InternedString,
    pub signature: HirFunctionSignature,
    pub entry_block: HirId,
    pub blocks: HashMap<HirId, HirBlock>,
    pub locals: HashMap<HirId, HirLocal>,
    pub values: HashMap<HirId, HirValue>,  // SSA values
    pub previous_version: Option<HirId>,   // Hot-reloading
    pub is_external: bool,
    pub calling_convention: CallingConvention,
}

pub struct HirFunctionSignature {
    pub params: Vec<HirParam>,
    pub returns: Vec<HirType>,
    pub type_params: Vec<HirTypeParam>,
    pub const_params: Vec<HirConstParam>,
    pub lifetime_params: Vec<HirLifetime>,
    pub is_variadic: bool,
}

pub struct HirBlock {
    pub id: HirId,
    pub label: Option<InternedString>,
    pub phis: Vec<HirPhi>,           // SSA phi nodes (must be first)
    pub instructions: Vec<HirInstruction>,
    pub terminator: HirTerminator,   // Must be last
    pub dominance_frontier: HashSet<HirId>,
    pub predecessors: Vec<HirId>,
    pub successors: Vec<HirId>,
}

pub struct HirPhi {
    pub result: HirId,
    pub ty: HirType,
    pub incoming: Vec<(HirId, HirId)>, // (value, block)
}

pub struct HirValue {
    pub id: HirId,
    pub ty: HirType,
    pub kind: HirValueKind,
    pub uses: HashSet<HirId>,        // Use-def chains
    pub span: Option<Span>,
}

pub enum HirValueKind {
    Parameter(u32),
    Instruction,
    Constant(HirConstant),
    Global(HirId),
    Undef,
}
```

**HIR Instructions:**
```rust
pub enum HirInstruction {
    // Arithmetic
    Binary { op, result, ty, left, right },
    Unary { op, result, ty, operand },
    
    // Memory operations
    Alloca { result, ty, count, align },
    Load { result, ty, ptr, align, volatile },
    Store { value, ptr, align, volatile },
    GetElementPtr { result, ty, ptr, indices },
    
    // Control flow & calls
    Call { result, callee, args, is_tail },
    CreateClosure { result, closure_ty, function, captures },
    CallClosure { result, closure, args },
    
    // Type operations
    Cast { op, result, ty, operand },
    Select { result, ty, condition, true_val, false_val },
    ExtractValue { result, ty, aggregate, indices },
    InsertValue { result, ty, aggregate, value, indices },
    
    // Union/variant operations
    CreateUnion { result, union_ty, variant_index, value },
    GetUnionDiscriminant { result, union_val },
    ExtractUnionValue { result, ty, union_val, variant_index },
    
    // References & ownership
    CreateRef { result, value, lifetime, mutable },
    Deref { result, ty, reference },
    Move { result, ty, source },
    Copy { result, ty, source },
    
    // Lifetime tracking
    BeginLifetime { lifetime },
    EndLifetime { lifetime },
    LifetimeConstraint { longer, shorter },
    
    // Atomic & synchronization
    Atomic { op, result, ty, ptr, value, ordering },
    Fence { ordering },
}

pub enum HirTerminator {
    Return { values },
    Branch { target },
    CondBranch { condition, true_target, false_target },
    Switch { value, default, cases },
    Invoke { callee, args, normal, unwind },
    PatternMatch { value, patterns, default },
    Unreachable,
}

pub enum HirType {
    Void,
    Bool,
    I8, I16, I32, I64, I128,
    U8, U16, U32, U64, U128,
    F32, F64,
    Ptr(Box<HirType>),
    Ref { lifetime, pointee, mutable },
    Array(Box<HirType>, u64),
    Vector(Box<HirType>, u32),      // SIMD
    Struct(HirStructType),
    Union(Box<HirUnionType>),
    Function(Box<HirFunctionType>),
    Closure(Box<HirClosureType>),
    Opaque(InternedString),
    Generic { base, type_args, const_args },
}

pub struct HirStructType {
    pub name: Option<InternedString>,
    pub fields: Vec<HirType>,
    pub packed: bool,
}

pub enum HirConstant {
    I8(i8), I16(i16), I32(i32), I64(i64), I128(i128),
    U8(u8), U16(u16), U32(u32), U64(u64), U128(u128),
    F32(f32), F64(f64),
    Bool(bool),
    String(InternedString),
}

pub enum HirCallable {
    Function(HirId),
    Intrinsic(Intrinsic),
    Indirect(Box<HirType>), // Function pointer
}

pub enum Intrinsic {
    Sqrt, Ctpop,
    Malloc, Free,
    IncRef, DecRef,
    // ... more intrinsics
}

// Lifetime tracking for memory safety
pub struct HirLifetime {
    pub id: LifetimeId,
    pub name: Option<InternedString>,
    pub bounds: Vec<LifetimeBound>,
}

pub enum LifetimeBound {
    Outlives(LifetimeId),
    Static,
}
```

### Phase 5: Analysis (`analysis.rs`)

#### Components

```rust
pub struct ModuleAnalysis {
    pub functions: HashMap<HirId, FunctionAnalysis>,
    pub call_graph: CallGraph,
    pub globals: GlobalAnalysis,
}

pub struct FunctionAnalysis {
    pub liveness: LivenessAnalysis,
    pub aliases: AliasAnalysis,
    pub memory: MemoryAnalysis,
    pub loops: LoopAnalysis,
}

pub struct LivenessAnalysis {
    pub live_in: HashMap<HirId, HashSet<HirId>>,
    pub live_out: HashMap<HirId, HashSet<HirId>>,
    pub live_ranges: HashMap<HirId, LiveRange>,
}

pub struct AliasAnalysis {
    pub alias_sets: Vec<AliasSet>,
    pub points_to: HashMap<HirId, PointsTo>,
}

pub enum PointsTo {
    Value(HirId),
    Global(HirId),
    Heap(HirId),
    Unknown,
}
```

### Phase 6: Optimization (`optimization.rs`)

#### Components

```rust
pub trait OptimizationPass: Send + Sync {
    fn name(&self) -> &'static str;
    fn required_analyses(&self) -> &[&'static str];
    fn run(&mut self, module: &mut HirModule, analysis: &ModuleAnalysis) -> CompilerResult<bool>;
}

pub struct OptimizationPipeline {
    passes: Vec<Box<dyn OptimizationPass>>,
    level: OptLevel,
}

pub enum OptLevel {
    None,      // -O0
    Less,      // -O1
    Default,   // -O2
    More,      // -O3
}

// Built-in passes:
pub mod passes {
    pub struct DeadCodeElimination { ... }
    pub struct ConstantFolding { ... }
    pub struct CommonSubexpressionElimination { ... }
    pub struct SimplifyCfg { ... }
    pub struct LoopInvariantCodeMotion { ... }
    pub struct Inlining { ... }
}
```

### Phase 7: Cranelift Backend (`cranelift_backend.rs`)

#### Purpose
JIT compilation to native code with hot-reloading support.

#### Components

```rust
pub struct CraneliftBackend {
    module: JITModule,
    builder_context: FunctionBuilderContext,
    codegen_context: codegen::Context,
    data_desc: DataDescription,
    function_map: HashMap<HirId, FuncId>,
    value_map: HashMap<HirId, Value>,           // HIR values → Cranelift values
    block_map: HashMap<HirId, Block>,           // HIR blocks → Cranelift blocks
    compiled_functions: HashMap<HirId, CompiledFunction>,
    hot_reload: HotReloadState,
}

pub struct CompiledFunction {
    function_id: FuncId,
    version: u64,
    code_ptr: *const u8,
    size: usize,
    signature: Signature,
}

struct HotReloadState {
    versions: Arc<RwLock<HashMap<HirId, u64>>>,
    previous_versions: Arc<RwLock<HashMap<HirId, Vec<CompiledFunction>>>>,
    function_pointers: Arc<RwLock<HashMap<HirId, *const u8>>>,
}
```

**Key Methods:**

```rust
impl CraneliftBackend {
    pub fn new() -> CompilerResult<Self> {
        // Initialize Cranelift with platform-specific ISA
        // Configure for JIT speed optimization
    }
    
    pub fn compile_module(&mut self, module: &HirModule) -> CompilerResult<()> {
        // Process globals first
        for (id, global) in &module.globals {
            self.compile_global(*id, global)?;
        }
        
        // Compile functions
        for (id, function) in &module.functions {
            self.compile_function(*id, function)?;
        }
        
        // Finalize and update function pointers
        self.module.finalize_definitions();
        
        for (hir_id, compiled_func) in &self.compiled_functions {
            let code_ptr = self.module.get_finalized_function(compiled_func.function_id);
            self.hot_reload.function_pointers.write().unwrap().insert(*hir_id, code_ptr);
        }
    }
    
    pub fn compile_function(&mut self, id: HirId, function: &HirFunction) -> CompilerResult<()> {
        // 1. Create Cranelift function signature
        let sig = self.translate_signature(function)?;
        
        // 2. Declare function in JIT module
        let func_id = self.module.declare_function(
            &format!("{}__{:?}", function.name, id),
            Linkage::Export,
            &sig,
        )?;
        
        // 3. Prepare parameter types
        let param_types: Vec<_> = function.signature.params.iter()
            .map(|param| self.translate_type(&param.ty))
            .collect::<Result<Vec<_>, _>>()?;
        
        // 4. Build function body
        // Create all blocks first
        // Map HIR values to Cranelift values
        // Translate all instructions
        // Handle terminators
    }
    
    fn translate_signature(&self, function: &HirFunction) -> CompilerResult<Signature> {
        // Convert HirFunctionSignature to Cranelift Signature
    }
    
    fn translate_type(&self, ty: &HirType) -> CompilerResult<types::Type> {
        // HirType → Cranelift types::Type
        HirType::Bool → types::I8
        HirType::I32 → types::I32
        HirType::I64 → types::I64
        HirType::F32 → types::F32
        HirType::F64 → types::F64
        HirType::Ptr(_) → ptr_type
        HirType::Array(inner, size) → [inner_type; size]
    }
}

// Instruction translation (HIR → Cranelift)
HirInstruction::Binary { op, left, right, ... }
    → match op {
        BinaryOp::Add → builder.ins().iadd(lhs, rhs)
        BinaryOp::Sub → builder.ins().isub(lhs, rhs)
        BinaryOp::Mul → builder.ins().imul(lhs, rhs)
        BinaryOp::Div → builder.ins().sdiv(lhs, rhs)
        BinaryOp::Eq  → builder.ins().icmp(IntCC::Equal, lhs, rhs)
        BinaryOp::Lt  → builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs)
        BinaryOp::FAdd → builder.ins().fadd(lhs, rhs)
        ...
    }

HirInstruction::Call { callee, args, ... }
    → match callee {
        HirCallable::Function(func_id) → {
            let func_ref = module.declare_func_in_func(func_id, builder.func);
            builder.ins().call(func_ref, &arg_values)
        }
        HirCallable::Intrinsic(intr) → {
            match intr {
                Intrinsic::Sqrt → builder.ins().sqrt(arg)
                Intrinsic::Ctpop → builder.ins().popcnt(arg)
                ...
            }
        }
    }

HirTerminator::Return { values }
    → builder.ins().return_(&cranelift_values)

HirTerminator::CondBranch { condition, true_target, false_target }
    → builder.ins().brif(cond, true_block, &[], false_block, &[])

HirTerminator::Switch { value, cases, default }
    → Generate chain of comparisons (Cranelift lacks native switch)
```

**Hot-Reload Support:**
- Version tracking per function
- Previous function versions stored for rollback
- Function pointers updated after finalization
- Enables live code patching during development

### Additional Compiler Modules

**Memory Management** (`memory_management.rs`, `memory_pass.rs`)
```rust
pub enum MemoryStrategy {
    Gc,          // Garbage collection
    ARC,         // Atomic reference counting
    ManualRef,   // Reference counting
    Ownership,   // Rust-style ownership
}

pub struct MemoryContext { ... }
pub struct ARCManager { ... }
pub struct DropManager { ... }
pub struct EscapeAnalysis { ... }

pub struct MemoryManagementPass { ... }  // Lowers high-level memory to low-level ops
```

**Async Support** (`async_support.rs`)
```rust
pub struct AsyncCompiler { ... }
pub struct AsyncStateMachine { ... }
pub enum AsyncState { Pending, Ready, Complete }
pub enum AsyncRuntime { Tokio, Async-Std, Custom }
pub struct AsyncCapture { ... }
```

**Pattern Matching** (`pattern_matching.rs`)
```rust
pub struct PatternMatchCompiler { ... }
pub enum DecisionNode { ... }
pub fn check_exhaustiveness(...) -> Result<...>
```

**Const Evaluation** (`const_eval.rs`)
```rust
pub struct ConstEvaluator { ... }
pub struct ConstEvalContext { ... }
// Compile-time evaluation of const expressions
```

**Monomorphization** (`monomorphize.rs`)
```rust
pub struct MonomorphizationContext { ... }
pub fn monomorphize_module(...) -> Result<HirModule>
// Instantiate generic functions for concrete types
```

---

## Integration Points & Data Flow

### Complete Compilation Pipeline

```
Source Code (Python/Java/etc.)
     ↓
[Language-specific Parser]
     ↓
typed-ast crate:
  TypedProgram (raw AST with types)
     ↓
Type Checking & Inference
  (type_checker.rs, type_inference.rs)
     ↓
Fully typed TypedAST
     ↓
compiler crate:
  
  LoweringContext (lowering.rs)
    ↓
  [Type Conversion]
  TypedProgram → HirModule
    ↓
  CFG Construction (cfg.rs)
    ↓
  SSA Conversion (ssa.rs)
    ↓
  HIR with SSA form
    ↓
  [Analysis Passes] (analysis.rs)
    - Liveness analysis
    - Alias analysis
    - Loop detection
    - Call graph
    ↓
  [Optimization Passes] (optimization.rs)
    - Dead code elimination
    - Constant folding
    - Common subexpression elimination
    - Loop invariant code motion
    - Inlining
    ↓
  Optimized HIR
    ↓
  CraneliftBackend (cranelift_backend.rs)
    [Signature Translation]
    [Value Mapping]
    [Instruction Translation]
    [Block Mapping]
    ↓
  Cranelift IR (Function definitions)
    ↓
  JIT Compilation
    ↓
  Native Machine Code
    ↓
  Hot-reload Manager
    [Version tracking]
    [Function pointers]
    ↓
  Executable Functions
```

### Key Handoff Points

#### 1. **TypedAST → HIR Lowering**

**Input:** `TypedProgram` from typed_ast crate
```rust
pub struct TypedProgram {
    pub declarations: Vec<TypedNode<TypedDeclaration>>,
}
```

**Output:** `HirModule` from compiler crate
```rust
pub struct HirModule {
    pub functions: HashMap<HirId, HirFunction>,
    pub globals: HashMap<HirId, HirGlobal>,
}
```

**Conversion Process:**
```
TypedFunction {
    name: InternedString,
    params: Vec<TypedParameter>,
    return_type: Type,
    body: TypedBlock,
    is_async: bool,
}
    ↓ [Type conversion + CFG + SSA]
HirFunction {
    id: HirId,
    name: InternedString,
    signature: HirFunctionSignature,
    entry_block: HirId,
    blocks: HashMap<HirId, HirBlock>,
    values: HashMap<HirId, HirValue>,
}
```

**Type Mapping Table:**
```
TypedAST Type          →  HIR Type
─────────────────────────────────────
Primitive(I32)         →  I32
Primitive(F64)         →  F64
Tuple([I32, Bool])     →  Struct{fields: [I32, Bool]}
Reference{ty, ...}    →  Ptr(boxed_type)
Array{elem, size}     →  Array(boxed_elem, size)
Function{params, ret}  →  Function{params, ret}
Named{...}            →  Opaque(name) or concrete lookup
```

#### 2. **CFG → SSA Conversion**

**Input:** `ControlFlowGraph` with BasicBlocks
```rust
pub struct BasicBlock {
    pub statements: Vec<TypedNode<TypedStatement>>,
    pub terminator: Option<TypedNode<TypedStatement>>,
}
```

**Output:** `HirBlock` with phi nodes and SSA values
```rust
pub struct HirBlock {
    pub phis: Vec<HirPhi>,  // Phi nodes for convergence points
    pub instructions: Vec<HirInstruction>,
    pub terminator: HirTerminator,
}

pub struct HirPhi {
    pub result: HirId,
    pub incoming: Vec<(HirId, HirId)>,  // (value, predecessor_block)
}
```

**Def-Use Chains:**
```rust
pub struct HirValue {
    pub uses: HashSet<HirId>,  // All uses of this value
}
```

#### 3. **HIR → Cranelift Translation**

**Mapping Tables:**

```
HIR Value Types        →  Cranelift Value
────────────────────────────────────────
HirValueKind::Parameter(i)  →  block_params[i]
HirValueKind::Constant(c)   →  iconst/fconst
HirValueKind::Instruction   →  inst.results()
```

**Block Mapping:**
```
for (hir_block_id, hir_block) in function.blocks {
    let cranelift_block = builder.create_block();
    block_map.insert(hir_block_id, cranelift_block);
}
```

**Instruction Dispatch:**
```
match hir_instruction {
    HirInstruction::Binary{op, left, right, ...} →
        Translate to builder.ins().OPCODE(left_val, right_val)
    
    HirInstruction::Call{callee, args, ...} →
        Translate to builder.ins().call(func_ref, arg_vals)
    
    HirInstruction::Load/Store/Alloca →
        Translate to builder.ins().load/store/stack_slot
    
    HirInstruction::CreateRef/Deref/Move/Copy →
        Translate to reference handling ops
    
    HirInstruction::Atomic → 
        Translate to atomic operations
}
```

**Terminator Translation:**
```
match hir_terminator {
    HirTerminator::Return{values} →
        builder.ins().return_(&cranelift_values)
    
    HirTerminator::CondBranch{condition, true_target, false_target} →
        builder.ins().brif(cond_val, true_block, &[], false_block, &[])
    
    HirTerminator::Branch{target} →
        builder.ins().jump(target_block, &[])
    
    HirTerminator::Switch{value, cases, default} →
        Generate if-else chain (Cranelift limitation)
    
    HirTerminator::PatternMatch{value, patterns, default} →
        Compile pattern decision tree
}
```

---

## Critical Gaps & Integration Challenges

### Current Implementation Gaps

1. **Expression Lowering Not Fully Implemented**
   - `lower_expression()` and `lower_statement()` methods missing from LoweringContext
   - Only function/global lowering skeleton exists
   - Need full expression-to-instruction translation

2. **Generic & Const Generic Support**
   - Monomorphization pass exists but not integrated into pipeline
   - Type parameter handling incomplete
   - Const generic evaluation needs integration

3. **Advanced Type Features**
   - Named type lookups in TypeRegistry not implemented in lowering
   - Associated types not translated
   - Trait bounds not enforced at lowering

4. **Async/Await Compilation**
   - AsyncCompiler module exists but not integrated
   - State machine generation incomplete
   - Runtime interaction not defined

5. **Memory Management**
   - Multiple strategies (GC, ARC, Manual) defined but not integrated
   - Reference counting ops need translation to intrinsics
   - Drop/cleanup semantics incomplete

6. **Pattern Matching**
   - Exhaustiveness checking exists
   - Decision tree compilation incomplete
   - Integration with pattern lowering needed

### Required Integration Bridges

```
TypedAST Features          →  Required HIR Features
───────────────────────────────────────────────────
Async functions            →  AsyncStateMachine instructions
Generics + Constraints     →  Type parameter constraints in HIR
Trait objects              →  VTable generation + dispatch
Linear types              →  Move/Copy/Borrow instructions
Ownership                 →  Lifetime constraints
Pattern matching          →  Decision tree terminators
Const generics            →  Const value propagation
Higher-kinded types       →  Generic type abstractions
Effect system             →  Effect annotations on functions
Dependent types           →  Refinement constraints
```

---

## Summary: Architecture Layers

| Layer | Crate | Files | Purpose |
|-------|-------|-------|---------|
| **1. Input** | typed_ast | typed_ast.rs, type_registry.rs | Language-agnostic typed AST |
| **2. Type System** | typed_ast | type_checker.rs, type_inference.rs, constraint_solver.rs | Sound type checking & inference |
| **3. Lowering** | compiler | lowering.rs | TypedAST → HIR conversion |
| **4. CFG** | compiler | cfg.rs | Control flow analysis |
| **5. SSA** | compiler | ssa.rs | Static single assignment form |
| **6. HIR** | compiler | hir.rs | Platform-agnostic intermediate rep |
| **7. Analysis** | compiler | analysis.rs | Liveness, alias, memory analysis |
| **8. Optimization** | compiler | optimization.rs | Standard optimization passes |
| **9. Backend** | compiler | cranelift_backend.rs | JIT code generation |

---

## File Paths Reference

```
/Users/amaterasu/Vibranium/zyntax/
├── crates/typed_ast/src/
│   ├── lib.rs                          # Main module exports
│   ├── typed_ast.rs                    # AST node definitions
│   ├── type_registry.rs                # Type system & registration
│   ├── type_checker.rs                 # Sound type checking
│   ├── type_inference.rs               # Hindley-Milner inference
│   ├── constraint_solver.rs            # Constraint solving
│   ├── nominal_type_checker.rs         # Class/trait checking
│   ├── structural_type_checker.rs      # Go-style typing
│   ├── gradual_type_checker.rs         # TypeScript-style gradual
│   ├── linear_types.rs                 # Affine/linear type checking
│   ├── dependent_types.rs              # Refinement types
│   ├── effect_system.rs                # Effect tracking
│   ├── const_evaluator.rs              # Const evaluation
│   └── advanced_analysis.rs            # Flow & ownership analysis
│
├── crates/compiler/src/
│   ├── lib.rs                          # Main module exports
│   ├── lowering.rs                     # TypedAST → HIR lowering
│   ├── cfg.rs                          # Control flow graph
│   ├── ssa.rs                          # SSA form construction
│   ├── hir.rs                          # HIR data structures
│   ├── analysis.rs                     # Analysis infrastructure
│   ├── optimization.rs                 # Optimization passes
│   ├── const_eval.rs                   # Const evaluation
│   ├── monomorphize.rs                 # Generic instantiation
│   ├── pattern_matching.rs             # Pattern compilation
│   ├── memory_management.rs            # Memory strategies
│   ├── memory_pass.rs                  # Memory lowering
│   ├── async_support.rs                # Async compilation
│   ├── cranelift_backend.rs            # JIT code generation
│   └── stdlib.rs                       # Standard library (Vec, String, HashMap, etc.)
│
└── crates/whirlwind_adapter/src/
    └── lib.rs                          # Whirlwind language integration
```

