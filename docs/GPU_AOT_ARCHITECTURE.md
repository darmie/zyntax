# GPU AOT Architecture: NVPTX via LLVM IR

**Status**: 📋 Planning
**Target**: Q1-Q2 2026
**Dependencies**: LLVM Backend, HIR Extensions, TypedAST Kernel Metadata
**Feature Flag**: `compute`

---

## Executive Summary

This document describes the architecture for adding GPU compute support to Zyntax via NVPTX (NVIDIA PTX) code generation through LLVM IR. The design extends the existing LLVM backend to emit GPU kernels alongside CPU code, enabling high-performance compute workloads for DSLs like QuantDSL, ZynML, and ImagePipe.

**Key Goals:**
- Compile GPU kernels from HIR to NVPTX via LLVM IR
- Zero-allocation critical CPU paths for ultra-low-latency execution
- First-class GPU primitives in TypedAST and HIR
- Seamless integration with existing tiered JIT compilation
- Support for heterogeneous CPU+GPU workloads

---

## Feature Flag: `compute`

GPU compute support is **opt-in** via the `compute` Cargo feature flag. This keeps the default build lightweight and avoids CUDA/LLVM dependencies for users who don't need GPU support.

### Building with GPU Compute Support

```bash
# Build compiler with GPU compute support
cargo build --release -p zyntax_compiler --features compute

# Build CLI with GPU compute support
cargo build --release -p zyntax_cli --features compute

# Build embed runtime with GPU compute support
cargo build --release -p zyntax_embed --features compute

# Build all crates with GPU compute support
cargo build --release --features compute

# Run tests with GPU compute support
cargo test --features compute
```

### Cargo.toml Configuration

```toml
# In crates/zyntax_compiler/Cargo.toml
[features]
default = []
compute = ["cuda-sys", "llvm-sys/nvptx"]

[dependencies]
cuda-sys = { version = "0.3", optional = true }

# In crates/zyntax_cli/Cargo.toml
[features]
default = []
compute = ["zyntax_compiler/compute"]

# In crates/zyntax_embed/Cargo.toml
[features]
default = []
compute = ["zyntax_compiler/compute"]
```

### Conditional Compilation

All GPU-related code is gated behind `#[cfg(feature = "compute")]`:

```rust
// In crates/compiler/src/lib.rs
#[cfg(feature = "compute")]
pub mod llvm_nvptx_backend;

#[cfg(feature = "compute")]
pub mod cuda_runtime;

#[cfg(feature = "compute")]
pub mod cuda_memory;

// GPU instructions are always defined in HIR (for type checking)
// but only compiled when the feature is enabled
impl HirInstruction {
    #[cfg(not(feature = "compute"))]
    pub fn is_gpu_instruction(&self) -> bool {
        false
    }

    #[cfg(feature = "compute")]
    pub fn is_gpu_instruction(&self) -> bool {
        matches!(self,
            HirInstruction::ThreadIdx { .. } |
            HirInstruction::BlockIdx { .. } |
            HirInstruction::SyncThreads |
            // ... other GPU instructions
        )
    }
}
```

### Runtime Detection

When the `compute` feature is enabled, the runtime checks for CUDA availability:

```rust
#[cfg(feature = "compute")]
pub fn gpu_available() -> bool {
    CudaRuntime::new().is_ok()
}

#[cfg(not(feature = "compute"))]
pub fn gpu_available() -> bool {
    false
}
```

### CLI Usage

```bash
# Without compute feature: GPU backends are not available
zyntax compile --backend llvm source.zyn  # CPU only

# With compute feature: GPU backends are available
zyntax compile --backend nvptx source.zyn  # Compiles to PTX
zyntax compile --backend cuda source.zyn   # Compiles and runs on GPU
```

### Why Opt-In?

1. **Binary Size**: CUDA runtime and LLVM NVPTX support add ~50MB to binary size
2. **Build Dependencies**: Requires CUDA Toolkit installed on build machine
3. **Runtime Dependencies**: Requires NVIDIA driver and CUDA runtime on target machine
4. **Build Time**: LLVM with NVPTX target significantly increases compile time
5. **Portability**: Default build works everywhere without GPU dependencies

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           Language Frontends                                 │
│                    (ZynML, QuantDSL, ImagePipe, etc.)                       │
└────────────────────────────────┬────────────────────────────────────────────┘
                                 │
                                 ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                        LAYER 1: TypedAST + Kernel Metadata                   │
│  ┌────────────────────────────────────────────────────────────────────────┐ │
│  │ Kernel Annotations                                                      │ │
│  │  • @kernel(type), @device, @workgroup, @shared                         │ │
│  │  • @critical_path, @no_allocation, @inline_always                      │ │
│  │  • @vectorizable, @gpu_compute                                          │ │
│  └────────────────────────────────────────────────────────────────────────┘ │
│  ┌────────────────────────────────────────────────────────────────────────┐ │
│  │ GPU Type Extensions                                                     │ │
│  │  • Tensor types with device placement                                   │ │
│  │  • Shared memory types                                                  │ │
│  │  • Address space annotations                                            │ │
│  └────────────────────────────────────────────────────────────────────────┘ │
└────────────────────────────────┬────────────────────────────────────────────┘
                                 │
                                 ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                    LAYER 2: HIR + GPU Primitives                            │
│  ┌────────────────────────────────────────────────────────────────────────┐ │
│  │ GPU Instructions                                                        │ │
│  │  • ThreadIdx, BlockIdx, BlockDim, GridDim                              │ │
│  │  • SyncThreads, SharedMemAlloc, AtomicOp                               │ │
│  │  • WarpShuffle, MemoryFence                                            │ │
│  └────────────────────────────────────────────────────────────────────────┘ │
│  ┌────────────────────────────────────────────────────────────────────────┐ │
│  │ Address Space Model                                                     │ │
│  │  • Generic (0), Global (1), Shared (3), Constant (4), Local (5)        │ │
│  └────────────────────────────────────────────────────────────────────────┘ │
│  ┌────────────────────────────────────────────────────────────────────────┐ │
│  │ Kernel Function Metadata                                                │ │
│  │  • Entry point markers                                                  │ │
│  │  • Launch configuration (grid/block dims)                              │ │
│  │  • Resource requirements (registers, shared memory)                    │ │
│  └────────────────────────────────────────────────────────────────────────┘ │
└────────────────────────────────┬────────────────────────────────────────────┘
                                 │
              ┌──────────────────┴──────────────────┐
              │                                     │
              ▼                                     ▼
┌──────────────────────────────────┐  ┌──────────────────────────────────┐
│  LAYER 3A: CPU Backend           │  │  LAYER 3B: GPU Backend           │
│                                  │  │                                  │
│  ┌────────────────────────────┐  │  │  ┌────────────────────────────┐  │
│  │ Cranelift JIT              │  │  │  │ LLVM NVPTX Backend         │  │
│  │  • Tier 0/1 baseline       │  │  │  │  • Target: nvptx64-nvidia  │  │
│  │  • Fast compilation        │  │  │  │  • PTX emission            │  │
│  │  • Low-latency paths       │  │  │  │  • Kernel metadata         │  │
│  └────────────────────────────┘  │  │  └────────────────────────────┘  │
│  ┌────────────────────────────┐  │  │  ┌────────────────────────────┐  │
│  │ LLVM x86/ARM Backend       │  │  │  │ CUDA Driver Runtime        │  │
│  │  • Tier 2 hot paths        │  │  │  │  • Kernel loading          │  │
│  │  • Critical path opts      │  │  │  │  • Memory management       │  │
│  │  • SIMD vectorization      │  │  │  │  • Stream synchronization  │  │
│  └────────────────────────────┘  │  │  └────────────────────────────┘  │
└──────────────────────────────────┘  └──────────────────────────────────┘
              │                                     │
              └─────────────┬───────────────────────┘
                            ▼
              ┌──────────────────────────────────┐
              │      Heterogeneous Runtime       │
              │  • Unified memory management     │
              │  • CPU/GPU task scheduling       │
              │  • Async execution streams       │
              └──────────────────────────────────┘
```

---

## Part 1: TypedAST Kernel Metadata Extensions

### 1.1 Kernel Annotation System

Extend `TypedAST` with annotations that mark functions for GPU compilation:

```rust
// In crates/typed_ast/src/typed_ast.rs

/// Kernel metadata attached to function declarations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KernelMetadata {
    /// Kernel type (affects code generation strategy)
    pub kernel_type: KernelType,

    /// Target device for execution
    pub device: DeviceTarget,

    /// Workgroup/block dimensions (None = runtime-specified)
    pub workgroup_size: Option<(u32, u32, u32)>,

    /// Required shared memory in bytes
    pub shared_memory_bytes: u32,

    /// Maximum registers per thread (0 = no limit)
    pub max_registers: u32,

    /// Whether this is a device-callable function (not entry point)
    pub is_device_function: bool,

    /// Optimization hints
    pub hints: KernelHints,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum KernelType {
    /// Element-wise operations (map)
    Elementwise,
    /// Reduction operations (sum, max, etc.)
    Reduce,
    /// Matrix multiplication
    MatMul,
    /// 2D convolution
    Conv2D,
    /// Attention mechanism (transformer)
    Attention,
    /// Flash attention (memory-efficient)
    FlashAttention,
    /// Scan/prefix sum
    Scan,
    /// Sort operations
    Sort,
    /// Custom user-defined kernel
    Custom,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DeviceTarget {
    /// CPU execution (fallback or SIMD)
    Cpu,
    /// NVIDIA GPU via CUDA/PTX
    Cuda { compute_capability: (u32, u32) },
    /// AMD GPU via ROCm/HIP
    Rocm,
    /// Apple GPU via Metal
    Metal,
    /// Vulkan compute (cross-platform)
    Vulkan,
    /// Automatic device selection
    Auto,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct KernelHints {
    /// Use tensor cores (NVIDIA Ampere+)
    pub use_tensor_cores: bool,
    /// Vectorization width hint
    pub vector_width: Option<u32>,
    /// Loop unroll factor
    pub unroll_factor: Option<u32>,
    /// Memory coalescing hint
    pub coalesced_access: bool,
    /// Async copy hint (for shared memory staging)
    pub async_copy: bool,
}
```

### 1.2 Critical Path Annotations

For ultra-low-latency CPU code (e.g., order execution in QuantDSL):

```rust
/// Execution path constraints for low-latency code
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ExecutionConstraints {
    /// No heap allocation allowed (stack-only)
    pub no_allocation: bool,

    /// No system calls allowed
    pub no_syscalls: bool,

    /// Always inline this function
    pub inline_always: bool,

    /// Disable bounds checking
    pub unsafe_unchecked: bool,

    /// Pin to specific CPU core (runtime hint)
    pub cpu_affinity: Option<u32>,

    /// Target latency in nanoseconds (for profiling)
    pub target_latency_ns: Option<u64>,

    /// Disable all runtime checks
    pub critical_path: bool,
}

/// Extended function declaration with kernel and execution metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypedFunctionDeclaration {
    pub name: InternedString,
    pub parameters: Vec<TypedParameter>,
    pub return_type: Type,
    pub body: Option<TypedBlock>,
    pub is_async: bool,
    pub visibility: Visibility,

    // New fields for GPU/low-latency support
    pub kernel_metadata: Option<KernelMetadata>,
    pub execution_constraints: Option<ExecutionConstraints>,
    pub compile_target: CompileTarget,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum CompileTarget {
    #[default]
    Cpu,
    Gpu,
    /// Generate both CPU and GPU versions
    Heterogeneous,
}
```

### 1.3 GPU Type Extensions

Extend the type system with GPU-specific types:

```rust
// In crates/typed_ast/src/type_registry.rs

/// Extended type variants for GPU computing
pub enum Type {
    // ... existing variants ...

    /// Tensor type with shape, dtype, and device placement
    Tensor {
        element_type: Box<Type>,
        shape: TensorShape,
        device: DeviceTarget,
    },

    /// Pointer with explicit address space
    DevicePointer {
        pointee: Box<Type>,
        address_space: AddressSpace,
    },

    /// Shared memory array (block-local)
    SharedArray {
        element_type: Box<Type>,
        size: u32,
    },

    /// Warp-level type (32-wide SIMD on NVIDIA)
    WarpValue {
        element_type: Box<Type>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TensorShape {
    /// Static shape known at compile time
    Static(Vec<u64>),
    /// Dynamic shape with optional bounds
    Dynamic {
        rank: usize,
        max_dims: Option<Vec<u64>>,
    },
    /// Symbolic shape (for shape polymorphism)
    Symbolic(Vec<ShapeExpr>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum AddressSpace {
    Generic = 0,   // Default address space
    Global = 1,    // Device main memory
    Shared = 3,    // Block-local shared memory
    Constant = 4,  // Read-only constant memory
    Local = 5,     // Thread-private local memory
}
```

---

## Part 2: HIR GPU Primitives

### 2.1 GPU Instruction Set

Extend `HirInstruction` with GPU-specific operations:

```rust
// In crates/compiler/src/hir.rs

pub enum HirInstruction {
    // ... existing instructions ...

    // ═══════════════════════════════════════════════════════════
    // GPU Thread/Block Indexing
    // ═══════════════════════════════════════════════════════════

    /// Get thread index within block: threadIdx.{x,y,z}
    ThreadIdx {
        dim: GpuDimension,
        result: HirId,
    },

    /// Get block index within grid: blockIdx.{x,y,z}
    BlockIdx {
        dim: GpuDimension,
        result: HirId,
    },

    /// Get block dimensions: blockDim.{x,y,z}
    BlockDim {
        dim: GpuDimension,
        result: HirId,
    },

    /// Get grid dimensions: gridDim.{x,y,z}
    GridDim {
        dim: GpuDimension,
        result: HirId,
    },

    // ═══════════════════════════════════════════════════════════
    // Synchronization
    // ═══════════════════════════════════════════════════════════

    /// Barrier synchronization within thread block
    /// All threads in block must reach this point before any proceed
    SyncThreads,

    /// Memory fence at specified scope
    GpuMemoryFence {
        scope: GpuFenceScope,
    },

    /// Warp-level barrier (no memory ordering)
    WarpSync {
        mask: Option<HirId>,  // Active thread mask (None = all)
    },

    // ═══════════════════════════════════════════════════════════
    // Shared Memory
    // ═══════════════════════════════════════════════════════════

    /// Allocate block-local shared memory
    SharedMemAlloc {
        result: HirId,
        element_type: HirType,
        num_elements: u32,
        align: u32,
    },

    /// Dynamic shared memory access (size determined at launch)
    DynamicSharedMemPtr {
        result: HirId,
        element_type: HirType,
        offset: HirId,  // Offset in bytes
    },

    // ═══════════════════════════════════════════════════════════
    // Atomic Operations (GPU-specific)
    // ═══════════════════════════════════════════════════════════

    /// GPU atomic operation with scope
    GpuAtomicOp {
        op: GpuAtomicKind,
        result: HirId,
        ptr: HirId,
        value: HirId,
        scope: GpuAtomicScope,
    },

    // ═══════════════════════════════════════════════════════════
    // Warp-Level Primitives
    // ═══════════════════════════════════════════════════════════

    /// Warp shuffle: exchange values between lanes
    WarpShuffle {
        mode: WarpShuffleMode,
        result: HirId,
        value: HirId,
        lane_or_delta: HirId,
        width: u32,  // Shuffle width (usually 32)
    },

    /// Warp vote: ballot/any/all
    WarpVote {
        op: WarpVoteOp,
        result: HirId,
        predicate: HirId,
    },

    /// Warp match: find threads with matching values
    WarpMatch {
        result: HirId,
        value: HirId,
    },

    // ═══════════════════════════════════════════════════════════
    // Tensor Core Operations (NVIDIA Ampere+)
    // ═══════════════════════════════════════════════════════════

    /// Matrix multiply-accumulate using tensor cores
    TensorCoreMMA {
        result: HirId,
        a: HirId,           // Matrix A fragment
        b: HirId,           // Matrix B fragment
        c: HirId,           // Accumulator
        layout: MmaLayout,  // Row/column major
    },

    /// Load matrix fragment from memory to registers
    TensorCoreLoad {
        result: HirId,
        ptr: HirId,
        layout: MmaLayout,
        fragment_type: MmaFragmentType,
    },

    /// Store matrix fragment from registers to memory
    TensorCoreStore {
        ptr: HirId,
        fragment: HirId,
        layout: MmaLayout,
    },

    // ═══════════════════════════════════════════════════════════
    // Async Copy (Ampere+)
    // ═══════════════════════════════════════════════════════════

    /// Async copy from global to shared memory
    AsyncCopy {
        dst: HirId,         // Shared memory destination
        src: HirId,         // Global memory source
        size_bytes: u32,
        cache_hint: CacheHint,
    },

    /// Commit async copy group
    AsyncCopyCommit {
        group: u32,
    },

    /// Wait for async copies to complete
    AsyncCopyWait {
        count: u32,  // Number of groups to wait for
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum GpuDimension {
    X,
    Y,
    Z,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum GpuFenceScope {
    /// Thread scope (compiler barrier only)
    Thread,
    /// Block scope (visible to all threads in block)
    Block,
    /// Device scope (visible to all threads on device)
    Device,
    /// System scope (visible to CPU and all GPUs)
    System,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum GpuAtomicKind {
    Add, Sub, Min, Max,
    And, Or, Xor,
    Exchange,
    CompareAndSwap,
    Inc, Dec,  // Wrap-around increment/decrement
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum GpuAtomicScope {
    Block,
    Device,
    System,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum WarpShuffleMode {
    /// Direct exchange: read from lane idx
    Idx,
    /// Up: read from (lane - delta)
    Up,
    /// Down: read from (lane + delta)
    Down,
    /// Xor: read from (lane ^ mask)
    Xor,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum WarpVoteOp {
    /// Return ballot mask of predicate across warp
    Ballot,
    /// True if any thread's predicate is true
    Any,
    /// True if all threads' predicates are true
    All,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum MmaLayout {
    RowMajor,
    ColMajor,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum MmaFragmentType {
    /// 16x16 matrix A (f16)
    MatrixA_16x16_F16,
    /// 16x16 matrix B (f16)
    MatrixB_16x16_F16,
    /// 16x16 accumulator (f32)
    Accumulator_16x16_F32,
    /// 8x8 matrix for smaller tiles
    MatrixA_8x8_F16,
    MatrixB_8x8_F16,
    Accumulator_8x8_F32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CacheHint {
    Default,
    /// Cache at L1
    CacheL1,
    /// Cache at L2 only
    CacheL2,
    /// Streaming (don't cache)
    Streaming,
}
```

### 2.2 Kernel Function Metadata in HIR

```rust
// In crates/compiler/src/hir.rs

/// Extended HirFunction with kernel metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirFunction {
    pub id: HirId,
    pub name: InternedString,
    pub signature: HirFunctionSignature,
    pub entry_block: HirId,
    pub blocks: IndexMap<HirId, HirBlock>,
    pub locals: IndexMap<HirId, HirLocal>,
    pub values: IndexMap<HirId, HirValue>,
    pub previous_version: Option<HirId>,
    pub is_external: bool,
    pub calling_convention: CallingConvention,
    pub attributes: FunctionAttributes,
    pub link_name: Option<String>,

    // New: GPU kernel metadata
    pub kernel_info: Option<HirKernelInfo>,

    // New: Critical path constraints
    pub execution_constraints: Option<HirExecutionConstraints>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirKernelInfo {
    /// This is a GPU kernel entry point
    pub is_kernel_entry: bool,

    /// Kernel type for optimization hints
    pub kernel_type: KernelType,

    /// Target GPU architecture
    pub target_arch: GpuArch,

    /// Launch bounds
    pub launch_bounds: Option<LaunchBounds>,

    /// Required shared memory
    pub shared_memory_bytes: u32,

    /// Maximum register usage
    pub max_registers: Option<u32>,

    /// Kernel uses these address spaces
    pub address_spaces_used: HashSet<AddressSpace>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum GpuArch {
    /// NVIDIA sm_XX
    Sm(u32),  // e.g., Sm(80) for Ampere
    /// AMD gfx
    Gfx(u32),
    /// Generic PTX
    Ptx(u32),
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct LaunchBounds {
    /// Maximum threads per block
    pub max_threads_per_block: u32,
    /// Minimum blocks per SM (optional)
    pub min_blocks_per_sm: Option<u32>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct HirExecutionConstraints {
    /// No dynamic memory allocation
    pub no_alloc: bool,
    /// Always inline
    pub inline_always: bool,
    /// No bounds checking
    pub no_bounds_check: bool,
    /// No null checks
    pub no_null_check: bool,
    /// This is on the critical execution path
    pub critical_path: bool,
    /// Preferred optimization level
    pub opt_level: OptimizationLevel,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum OptimizationLevel {
    /// No optimization (debug)
    O0,
    /// Basic optimization
    #[default]
    O1,
    /// Standard optimization
    O2,
    /// Aggressive optimization
    O3,
    /// Size optimization
    Os,
    /// Ultra-low-latency (may sacrifice portability)
    Olatency,
}
```

### 2.3 HIR Module Extensions

```rust
// Extended HirModule with GPU support
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HirModule {
    pub id: HirId,
    pub name: InternedString,
    pub functions: IndexMap<HirId, HirFunction>,
    pub globals: IndexMap<HirId, HirGlobal>,
    pub types: IndexMap<TypeId, HirType>,
    pub imports: Vec<HirImport>,
    pub exports: Vec<HirExport>,
    pub version: u64,
    pub dependencies: HashSet<HirId>,

    // New: GPU-specific module metadata
    pub gpu_metadata: Option<GpuModuleMetadata>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GpuModuleMetadata {
    /// Target GPU architectures
    pub target_archs: Vec<GpuArch>,

    /// PTX version to emit
    pub ptx_version: (u32, u32),  // e.g., (7, 5) for PTX 7.5

    /// CUDA compute capability
    pub compute_capability: (u32, u32),  // e.g., (8, 0) for sm_80

    /// Required CUDA features
    pub required_features: HashSet<String>,

    /// Kernel entry points in this module
    pub kernel_entries: Vec<HirId>,

    /// Device functions (callable from kernels)
    pub device_functions: Vec<HirId>,

    /// Constant memory globals
    pub constant_memory: Vec<HirId>,
}
```

---

## Part 3: LLVM NVPTX Backend

### 3.1 Target Initialization

```rust
// In crates/compiler/src/llvm_nvptx_backend.rs

use inkwell::{
    targets::{Target, TargetMachine, InitializationConfig, RelocMode, CodeModel},
    OptimizationLevel,
};

pub struct NvptxBackend<'ctx> {
    /// Base LLVM backend (reuse existing infrastructure)
    base: LLVMBackend<'ctx>,

    /// NVPTX target machine
    target_machine: TargetMachine,

    /// Compute capability (e.g., 80 for sm_80)
    compute_capability: u32,

    /// PTX version (e.g., 75 for ptx75)
    ptx_version: u32,

    /// Kernel metadata for nvvm annotations
    kernel_metadata: Vec<KernelNvvmAnnotation>,
}

#[derive(Debug, Clone)]
struct KernelNvvmAnnotation {
    function_name: String,
    is_kernel: bool,
    max_threads: Option<u32>,
    min_blocks: Option<u32>,
}

impl<'ctx> NvptxBackend<'ctx> {
    pub fn new(
        context: &'ctx Context,
        module_name: &str,
        compute_capability: u32,
    ) -> CompilerResult<Self> {
        // Initialize NVPTX target
        Target::initialize_nvptx(&InitializationConfig::default());

        // Create target triple
        let triple = TargetTriple::create("nvptx64-nvidia-cuda");
        let target = Target::from_triple(&triple)
            .map_err(|e| CompilerError::CodeGen(format!("NVPTX target error: {}", e)))?;

        // Determine PTX version from compute capability
        let ptx_version = match compute_capability {
            80..=89 => 75,  // Ampere: PTX 7.5
            90..=99 => 80,  // Hopper: PTX 8.0
            70..=79 => 63,  // Volta: PTX 6.3
            _ => 60,        // Default: PTX 6.0
        };

        // Create target machine
        let cpu = format!("sm_{}", compute_capability);
        let features = format!("+ptx{}", ptx_version);

        let target_machine = target
            .create_target_machine(
                &triple,
                &cpu,
                &features,
                OptimizationLevel::Aggressive,
                RelocMode::Default,
                CodeModel::Default,
            )
            .ok_or_else(|| CompilerError::CodeGen("Failed to create NVPTX target machine".into()))?;

        // Create base LLVM backend
        let base = LLVMBackend::new(context, module_name);

        Ok(Self {
            base,
            target_machine,
            compute_capability,
            ptx_version,
            kernel_metadata: Vec::new(),
        })
    }

    /// Compile HIR module to PTX string
    pub fn compile_to_ptx(&mut self, hir_module: &HirModule) -> CompilerResult<String> {
        // Compile HIR to LLVM IR using base backend
        self.compile_module(hir_module)?;

        // Add NVVM kernel annotations
        self.add_nvvm_annotations()?;

        // Set correct data layout and triple
        self.set_nvptx_metadata()?;

        // Emit PTX
        let ptx = self.emit_ptx()?;

        Ok(ptx)
    }

    fn compile_module(&mut self, hir_module: &HirModule) -> CompilerResult<()> {
        // Use base LLVM backend compilation
        // But intercept kernel functions for special handling

        for (id, func) in &hir_module.functions {
            if let Some(kernel_info) = &func.kernel_info {
                if kernel_info.is_kernel_entry {
                    // Compile as kernel entry point
                    self.compile_kernel_function(*id, func)?;
                } else {
                    // Compile as device function
                    self.compile_device_function(*id, func)?;
                }
            } else {
                // Regular function - use base backend
                // (These typically won't be emitted to PTX)
            }
        }

        Ok(())
    }

    fn compile_kernel_function(&mut self, id: HirId, func: &HirFunction) -> CompilerResult<()> {
        // Create function with correct calling convention for kernels
        let fn_value = self.base.declare_function(id, func)?;

        // Set NVPTX kernel calling convention
        fn_value.set_call_conventions(inkwell::llvm_sys::LLVMCallConv::LLVMPTXKernelCallConv as u32);

        // Record for nvvm.annotations
        self.kernel_metadata.push(KernelNvvmAnnotation {
            function_name: func.name.resolve_global().unwrap_or_default(),
            is_kernel: true,
            max_threads: func.kernel_info.as_ref()
                .and_then(|k| k.launch_bounds.map(|b| b.max_threads_per_block)),
            min_blocks: func.kernel_info.as_ref()
                .and_then(|k| k.launch_bounds.and_then(|b| b.min_blocks_per_sm)),
        });

        // Compile function body with GPU instruction support
        self.compile_function_body(id, func)?;

        Ok(())
    }

    fn add_nvvm_annotations(&mut self) -> CompilerResult<()> {
        let module = self.base.module();
        let context = module.get_context();

        // Create nvvm.annotations metadata
        for annotation in &self.kernel_metadata {
            // Get function
            if let Some(func) = module.get_function(&annotation.function_name) {
                // Create metadata: !{ptr @func, !"kernel", i32 1}
                let kernel_md = context.metadata_node(&[
                    func.as_global_value().as_pointer_value().into(),
                    context.metadata_string("kernel").into(),
                    context.i32_type().const_int(1, false).into(),
                ]);

                // Add to nvvm.annotations
                module.add_named_metadata("nvvm.annotations", &kernel_md);

                // Add maxntidx if specified
                if let Some(max_threads) = annotation.max_threads {
                    let maxntid_md = context.metadata_node(&[
                        func.as_global_value().as_pointer_value().into(),
                        context.metadata_string("maxntidx").into(),
                        context.i32_type().const_int(max_threads as u64, false).into(),
                    ]);
                    module.add_named_metadata("nvvm.annotations", &maxntid_md);
                }
            }
        }

        Ok(())
    }

    fn set_nvptx_metadata(&mut self) -> CompilerResult<()> {
        let module = self.base.module();

        // Set data layout for NVPTX64
        module.set_data_layout(&self.target_machine.get_target_data().get_data_layout());

        // Set target triple
        module.set_triple(&TargetTriple::create("nvptx64-nvidia-cuda"));

        Ok(())
    }

    fn emit_ptx(&self) -> CompilerResult<String> {
        let module = self.base.module();

        // Emit assembly (PTX) to buffer
        let buffer = self.target_machine
            .write_to_memory_buffer(module, inkwell::targets::FileType::Assembly)
            .map_err(|e| CompilerError::CodeGen(format!("PTX emission error: {}", e)))?;

        // Convert to string
        let ptx = String::from_utf8_lossy(buffer.as_slice()).to_string();

        Ok(ptx)
    }
}
```

### 3.2 GPU Instruction Lowering

```rust
// In crates/compiler/src/llvm_nvptx_backend.rs

impl<'ctx> NvptxBackend<'ctx> {
    /// Compile a single HIR instruction with GPU support
    fn compile_gpu_instruction(&mut self, inst: &HirInstruction) -> CompilerResult<()> {
        match inst {
            HirInstruction::ThreadIdx { dim, result } => {
                self.compile_thread_idx(*dim, *result)
            }
            HirInstruction::BlockIdx { dim, result } => {
                self.compile_block_idx(*dim, *result)
            }
            HirInstruction::BlockDim { dim, result } => {
                self.compile_block_dim(*dim, *result)
            }
            HirInstruction::GridDim { dim, result } => {
                self.compile_grid_dim(*dim, *result)
            }
            HirInstruction::SyncThreads => {
                self.compile_sync_threads()
            }
            HirInstruction::SharedMemAlloc { result, element_type, num_elements, align } => {
                self.compile_shared_mem_alloc(*result, element_type, *num_elements, *align)
            }
            HirInstruction::GpuAtomicOp { op, result, ptr, value, scope } => {
                self.compile_gpu_atomic(*op, *result, *ptr, *value, *scope)
            }
            HirInstruction::WarpShuffle { mode, result, value, lane_or_delta, width } => {
                self.compile_warp_shuffle(*mode, *result, *value, *lane_or_delta, *width)
            }
            // ... other GPU instructions
            _ => {
                // Delegate to base backend for non-GPU instructions
                self.base.compile_instruction(inst)
            }
        }
    }

    fn compile_thread_idx(&mut self, dim: GpuDimension, result: HirId) -> CompilerResult<()> {
        // Call NVVM intrinsic: llvm.nvvm.read.ptx.sreg.tid.{x,y,z}
        let intrinsic_name = match dim {
            GpuDimension::X => "llvm.nvvm.read.ptx.sreg.tid.x",
            GpuDimension::Y => "llvm.nvvm.read.ptx.sreg.tid.y",
            GpuDimension::Z => "llvm.nvvm.read.ptx.sreg.tid.z",
        };

        let context = self.base.context;
        let i32_ty = context.i32_type();

        // Declare intrinsic
        let intrinsic_ty = i32_ty.fn_type(&[], false);
        let intrinsic = self.base.module().add_function(intrinsic_name, intrinsic_ty, None);

        // Call intrinsic
        let value = self.base.builder.build_call(intrinsic, &[], "tid")?
            .try_as_basic_value()
            .left()
            .unwrap();

        self.base.value_map.insert(result, value);
        Ok(())
    }

    fn compile_block_idx(&mut self, dim: GpuDimension, result: HirId) -> CompilerResult<()> {
        let intrinsic_name = match dim {
            GpuDimension::X => "llvm.nvvm.read.ptx.sreg.ctaid.x",
            GpuDimension::Y => "llvm.nvvm.read.ptx.sreg.ctaid.y",
            GpuDimension::Z => "llvm.nvvm.read.ptx.sreg.ctaid.z",
        };

        // Similar to thread_idx...
        self.compile_nvvm_sreg_intrinsic(intrinsic_name, result)
    }

    fn compile_block_dim(&mut self, dim: GpuDimension, result: HirId) -> CompilerResult<()> {
        let intrinsic_name = match dim {
            GpuDimension::X => "llvm.nvvm.read.ptx.sreg.ntid.x",
            GpuDimension::Y => "llvm.nvvm.read.ptx.sreg.ntid.y",
            GpuDimension::Z => "llvm.nvvm.read.ptx.sreg.ntid.z",
        };

        self.compile_nvvm_sreg_intrinsic(intrinsic_name, result)
    }

    fn compile_grid_dim(&mut self, dim: GpuDimension, result: HirId) -> CompilerResult<()> {
        let intrinsic_name = match dim {
            GpuDimension::X => "llvm.nvvm.read.ptx.sreg.nctaid.x",
            GpuDimension::Y => "llvm.nvvm.read.ptx.sreg.nctaid.y",
            GpuDimension::Z => "llvm.nvvm.read.ptx.sreg.nctaid.z",
        };

        self.compile_nvvm_sreg_intrinsic(intrinsic_name, result)
    }

    fn compile_nvvm_sreg_intrinsic(&mut self, name: &str, result: HirId) -> CompilerResult<()> {
        let context = self.base.context;
        let i32_ty = context.i32_type();

        let intrinsic_ty = i32_ty.fn_type(&[], false);
        let intrinsic = self.base.module().add_function(name, intrinsic_ty, None);

        let value = self.base.builder.build_call(intrinsic, &[], "sreg")?
            .try_as_basic_value()
            .left()
            .unwrap();

        self.base.value_map.insert(result, value);
        Ok(())
    }

    fn compile_sync_threads(&mut self) -> CompilerResult<()> {
        // llvm.nvvm.barrier0
        let context = self.base.context;
        let void_ty = context.void_type();

        let intrinsic_ty = void_ty.fn_type(&[], false);
        let intrinsic = self.base.module().add_function("llvm.nvvm.barrier0", intrinsic_ty, None);

        self.base.builder.build_call(intrinsic, &[], "")?;
        Ok(())
    }

    fn compile_shared_mem_alloc(
        &mut self,
        result: HirId,
        element_type: &HirType,
        num_elements: u32,
        align: u32,
    ) -> CompilerResult<()> {
        let context = self.base.context;
        let elem_ty = self.base.translate_type(element_type)?;

        // Create array type
        let array_ty = elem_ty.array_type(num_elements);

        // Allocate in address space 3 (shared memory)
        let global = self.base.module().add_global(
            array_ty,
            Some(AddressSpace::from(3)),  // Shared memory
            &format!("shared_mem_{:?}", result),
        );

        // Mark as internal linkage (not exported)
        global.set_linkage(inkwell::module::Linkage::Internal);
        global.set_alignment(align);

        // Initialize to undef
        global.set_initializer(&array_ty.get_undef());

        // Store pointer
        let ptr = global.as_pointer_value();
        self.base.value_map.insert(result, ptr.into());

        Ok(())
    }

    fn compile_gpu_atomic(
        &mut self,
        op: GpuAtomicKind,
        result: HirId,
        ptr: HirId,
        value: HirId,
        scope: GpuAtomicScope,
    ) -> CompilerResult<()> {
        let ptr_val = self.base.get_value(ptr)?
            .into_pointer_value();
        let val = self.base.get_value(value)?;

        // Map to LLVM atomicrmw operation
        let rmw_op = match op {
            GpuAtomicKind::Add => AtomicRMWBinOp::Add,
            GpuAtomicKind::Sub => AtomicRMWBinOp::Sub,
            GpuAtomicKind::Min => AtomicRMWBinOp::Min,
            GpuAtomicKind::Max => AtomicRMWBinOp::Max,
            GpuAtomicKind::And => AtomicRMWBinOp::And,
            GpuAtomicKind::Or => AtomicRMWBinOp::Or,
            GpuAtomicKind::Xor => AtomicRMWBinOp::Xor,
            GpuAtomicKind::Exchange => AtomicRMWBinOp::Xchg,
            _ => return Err(CompilerError::CodeGen("Unsupported GPU atomic op".into())),
        };

        // Use appropriate memory ordering based on scope
        let ordering = match scope {
            GpuAtomicScope::Block => LLVMAtomicOrdering::LLVMAcquireRelease,
            GpuAtomicScope::Device => LLVMAtomicOrdering::LLVMSequentiallyConsistent,
            GpuAtomicScope::System => LLVMAtomicOrdering::LLVMSequentiallyConsistent,
        };

        let result_val = self.base.builder.build_atomicrmw(
            rmw_op,
            ptr_val,
            val.into_int_value(),
            ordering,
        )?;

        self.base.value_map.insert(result, result_val.into());
        Ok(())
    }

    fn compile_warp_shuffle(
        &mut self,
        mode: WarpShuffleMode,
        result: HirId,
        value: HirId,
        lane_or_delta: HirId,
        width: u32,
    ) -> CompilerResult<()> {
        let context = self.base.context;
        let i32_ty = context.i32_type();

        let val = self.base.get_value(value)?;
        let lane = self.base.get_value(lane_or_delta)?;

        // Determine intrinsic based on mode
        let intrinsic_name = match mode {
            WarpShuffleMode::Idx => "llvm.nvvm.shfl.sync.idx.i32",
            WarpShuffleMode::Up => "llvm.nvvm.shfl.sync.up.i32",
            WarpShuffleMode::Down => "llvm.nvvm.shfl.sync.down.i32",
            WarpShuffleMode::Xor => "llvm.nvvm.shfl.sync.bfly.i32",
        };

        // Create intrinsic type: (mask, value, lane/delta, width) -> value
        let intrinsic_ty = i32_ty.fn_type(
            &[i32_ty.into(), i32_ty.into(), i32_ty.into(), i32_ty.into()],
            false,
        );
        let intrinsic = self.base.module().add_function(intrinsic_name, intrinsic_ty, None);

        // Full warp mask
        let mask = i32_ty.const_int(0xFFFFFFFF, false);
        let width_val = i32_ty.const_int(width as u64, false);

        let result_val = self.base.builder.build_call(
            intrinsic,
            &[mask.into(), val.into(), lane.into(), width_val.into()],
            "shfl",
        )?
        .try_as_basic_value()
        .left()
        .unwrap();

        self.base.value_map.insert(result, result_val);
        Ok(())
    }
}
```

---

## Part 4: Low-Latency CPU Backend Optimizations

### 4.1 Critical Path Compiler Mode

For ultra-low-latency execution (e.g., QuantDSL order execution):

```rust
// In crates/compiler/src/critical_path.rs

use crate::hir::{HirFunction, HirExecutionConstraints, OptimizationLevel};

/// Critical path optimization pass
pub struct CriticalPathOptimizer {
    /// Target latency in nanoseconds
    target_latency_ns: u64,
    /// Allow unsafe optimizations
    allow_unsafe: bool,
}

impl CriticalPathOptimizer {
    pub fn new(target_latency_ns: u64) -> Self {
        Self {
            target_latency_ns,
            allow_unsafe: false,
        }
    }

    /// Optimize a function for minimal latency
    pub fn optimize(&mut self, func: &mut HirFunction) -> CompilerResult<()> {
        let constraints = func.execution_constraints.as_ref()
            .cloned()
            .unwrap_or_default();

        if constraints.critical_path {
            self.apply_critical_path_optimizations(func, &constraints)?;
        }

        Ok(())
    }

    fn apply_critical_path_optimizations(
        &mut self,
        func: &mut HirFunction,
        constraints: &HirExecutionConstraints,
    ) -> CompilerResult<()> {
        // 1. Remove all allocation
        if constraints.no_alloc {
            self.remove_allocations(func)?;
        }

        // 2. Inline all function calls
        if constraints.inline_always {
            self.force_inline_all(func)?;
        }

        // 3. Remove bounds checking
        if constraints.no_bounds_check {
            self.remove_bounds_checks(func)?;
        }

        // 4. Vectorize where possible
        self.vectorize_loops(func)?;

        // 5. Prefetch data
        self.insert_prefetches(func)?;

        // 6. Align hot loops
        self.align_hot_loops(func)?;

        Ok(())
    }

    fn remove_allocations(&self, func: &mut HirFunction) -> CompilerResult<()> {
        for (_, block) in func.blocks.iter_mut() {
            block.instructions.retain(|inst| {
                match inst {
                    HirInstruction::Alloca { .. } => {
                        log::warn!("Removing heap allocation in critical path");
                        false
                    }
                    HirInstruction::Call { callee: HirCallable::Intrinsic(Intrinsic::Malloc), .. } |
                    HirInstruction::Call { callee: HirCallable::Intrinsic(Intrinsic::Realloc), .. } => {
                        log::warn!("Removing heap allocation call in critical path");
                        false
                    }
                    _ => true,
                }
            });
        }
        Ok(())
    }

    fn force_inline_all(&self, func: &mut HirFunction) -> CompilerResult<()> {
        // Mark all calls for inlining
        func.attributes.inline = InlineHint::Always;
        func.attributes.always_inline = true;
        Ok(())
    }

    fn remove_bounds_checks(&self, func: &mut HirFunction) -> CompilerResult<()> {
        // Transform Load/Store with bounds checks to direct access
        // This is unsafe but explicitly requested
        for (_, block) in func.blocks.iter_mut() {
            for inst in &mut block.instructions {
                match inst {
                    // Remove conditional bounds checks before array access
                    // (This requires dataflow analysis in practice)
                    _ => {}
                }
            }
        }
        Ok(())
    }

    fn vectorize_loops(&self, func: &mut HirFunction) -> CompilerResult<()> {
        // Auto-vectorization hints for LLVM
        // Add loop metadata for vectorization
        Ok(())
    }

    fn insert_prefetches(&self, func: &mut HirFunction) -> CompilerResult<()> {
        // Analyze memory access patterns and insert prefetch instructions
        Ok(())
    }

    fn align_hot_loops(&self, func: &mut HirFunction) -> CompilerResult<()> {
        // Ensure loop headers are aligned to cache line boundaries
        Ok(())
    }
}
```

### 4.2 LLVM Backend Critical Path Extensions

```rust
// In crates/compiler/src/llvm_backend.rs - extend existing implementation

impl<'ctx> LLVMBackend<'ctx> {
    /// Compile with critical path optimizations
    pub fn compile_critical_path(
        &mut self,
        func: &HirFunction,
    ) -> CompilerResult<()> {
        let constraints = func.execution_constraints.as_ref();

        // Apply function attributes for aggressive optimization
        if let Some(fn_value) = self.functions.get(&func.id) {
            if constraints.map(|c| c.inline_always).unwrap_or(false) {
                fn_value.add_attribute(
                    inkwell::attributes::AttributeLoc::Function,
                    self.context.create_string_attribute("alwaysinline", ""),
                );
            }

            if constraints.map(|c| c.no_alloc).unwrap_or(false) {
                // Mark as not allocating
                fn_value.add_attribute(
                    inkwell::attributes::AttributeLoc::Function,
                    self.context.create_string_attribute("noalloc", ""),
                );
            }

            if constraints.map(|c| c.critical_path).unwrap_or(false) {
                // Mark as hot
                fn_value.add_attribute(
                    inkwell::attributes::AttributeLoc::Function,
                    self.context.create_string_attribute("hot", ""),
                );

                // Disable stack protection
                fn_value.add_attribute(
                    inkwell::attributes::AttributeLoc::Function,
                    self.context.create_string_attribute("nossp", ""),
                );
            }
        }

        Ok(())
    }

    /// Add LLVM optimization passes for low-latency code
    pub fn add_low_latency_passes(&self, pass_manager: &PassManager<Module<'ctx>>) {
        // Aggressive inlining
        pass_manager.add_always_inliner_pass();

        // SROA: Scalar Replacement of Aggregates (eliminate structs)
        pass_manager.add_scalar_repl_aggregates_pass();

        // GVN: Global Value Numbering (eliminate redundant loads)
        pass_manager.add_gvn_pass();

        // Loop vectorization
        pass_manager.add_loop_vectorize_pass();

        // SLP vectorization (straight-line code)
        pass_manager.add_slp_vectorize_pass();

        // Instruction combining
        pass_manager.add_instruction_combining_pass();

        // Tail call optimization
        pass_manager.add_tail_call_elimination_pass();

        // Dead code elimination
        pass_manager.add_aggressive_dce_pass();
    }
}
```

### 4.3 SIMD Vectorization Support

```rust
// In crates/compiler/src/simd.rs

/// SIMD width detection and code generation
pub struct SimdCodegen {
    /// Target vector width in bits
    vector_width: u32,
    /// Available SIMD features
    features: SimdFeatures,
}

#[derive(Debug, Clone, Default)]
pub struct SimdFeatures {
    pub sse: bool,
    pub sse2: bool,
    pub sse3: bool,
    pub ssse3: bool,
    pub sse4_1: bool,
    pub sse4_2: bool,
    pub avx: bool,
    pub avx2: bool,
    pub avx512f: bool,
    pub avx512vl: bool,
    pub neon: bool,  // ARM
}

impl SimdCodegen {
    pub fn detect() -> Self {
        #[cfg(target_arch = "x86_64")]
        {
            use std::arch::x86_64::*;

            let features = SimdFeatures {
                sse: is_x86_feature_detected!("sse"),
                sse2: is_x86_feature_detected!("sse2"),
                sse3: is_x86_feature_detected!("sse3"),
                ssse3: is_x86_feature_detected!("ssse3"),
                sse4_1: is_x86_feature_detected!("sse4.1"),
                sse4_2: is_x86_feature_detected!("sse4.2"),
                avx: is_x86_feature_detected!("avx"),
                avx2: is_x86_feature_detected!("avx2"),
                avx512f: is_x86_feature_detected!("avx512f"),
                avx512vl: is_x86_feature_detected!("avx512vl"),
                ..Default::default()
            };

            let vector_width = if features.avx512f { 512 }
                else if features.avx2 { 256 }
                else if features.sse2 { 128 }
                else { 64 };

            Self { vector_width, features }
        }

        #[cfg(target_arch = "aarch64")]
        {
            Self {
                vector_width: 128,  // NEON is 128-bit
                features: SimdFeatures { neon: true, ..Default::default() },
            }
        }

        #[cfg(not(any(target_arch = "x86_64", target_arch = "aarch64")))]
        {
            Self {
                vector_width: 64,
                features: SimdFeatures::default(),
            }
        }
    }

    /// Get recommended vectorization width for a scalar type
    pub fn recommend_width(&self, scalar_bits: u32) -> u32 {
        self.vector_width / scalar_bits
    }

    /// Generate target features string for LLVM
    pub fn llvm_features(&self) -> String {
        let mut features = Vec::new();

        if self.features.avx512f { features.push("+avx512f"); }
        if self.features.avx512vl { features.push("+avx512vl"); }
        if self.features.avx2 { features.push("+avx2"); }
        if self.features.avx { features.push("+avx"); }
        if self.features.sse4_2 { features.push("+sse4.2"); }
        if self.features.sse4_1 { features.push("+sse4.1"); }

        features.join(",")
    }
}
```

---

## Part 5: CUDA Driver Runtime Integration

### 5.1 Runtime Module Management

```rust
// In crates/compiler/src/cuda_runtime.rs

use std::ffi::CString;
use std::ptr;

/// CUDA driver API wrapper for kernel execution
pub struct CudaRuntime {
    /// CUDA context
    context: CUcontext,
    /// Loaded modules (PTX -> cuModule)
    modules: HashMap<String, CudaModule>,
    /// Active streams
    streams: Vec<CudaStream>,
    /// Device properties
    device_props: CudaDeviceProps,
}

pub struct CudaModule {
    module: CUmodule,
    kernels: HashMap<String, CUfunction>,
}

pub struct CudaStream {
    stream: CUstream,
    /// Events for timing/sync
    events: Vec<CUevent>,
}

#[derive(Debug, Clone)]
pub struct CudaDeviceProps {
    pub name: String,
    pub compute_capability: (u32, u32),
    pub total_memory: usize,
    pub multiprocessor_count: u32,
    pub max_threads_per_block: u32,
    pub max_threads_per_multiprocessor: u32,
    pub warp_size: u32,
    pub shared_memory_per_block: usize,
    pub registers_per_block: u32,
}

impl CudaRuntime {
    /// Initialize CUDA runtime
    pub fn new() -> Result<Self, CudaError> {
        // Initialize CUDA driver
        unsafe {
            let result = cuInit(0);
            if result != 0 {
                return Err(CudaError::InitFailed(result));
            }
        }

        // Get device
        let mut device: CUdevice = 0;
        unsafe {
            cuDeviceGet(&mut device, 0)?;
        }

        // Create context
        let mut context: CUcontext = ptr::null_mut();
        unsafe {
            cuCtxCreate(&mut context, 0, device)?;
        }

        // Query device properties
        let device_props = Self::query_device_props(device)?;

        Ok(Self {
            context,
            modules: HashMap::new(),
            streams: Vec::new(),
            device_props,
        })
    }

    /// Load PTX code as a module
    pub fn load_ptx(&mut self, name: &str, ptx: &str) -> Result<(), CudaError> {
        let ptx_cstring = CString::new(ptx)?;

        let mut module: CUmodule = ptr::null_mut();
        unsafe {
            cuModuleLoadData(&mut module, ptx_cstring.as_ptr() as *const _)?;
        }

        self.modules.insert(name.to_string(), CudaModule {
            module,
            kernels: HashMap::new(),
        });

        Ok(())
    }

    /// Get kernel function from module
    pub fn get_kernel(&mut self, module_name: &str, kernel_name: &str) -> Result<CUfunction, CudaError> {
        let module = self.modules.get_mut(module_name)
            .ok_or(CudaError::ModuleNotFound)?;

        if let Some(&func) = module.kernels.get(kernel_name) {
            return Ok(func);
        }

        // Load kernel
        let kernel_cstring = CString::new(kernel_name)?;
        let mut func: CUfunction = ptr::null_mut();

        unsafe {
            cuModuleGetFunction(&mut func, module.module, kernel_cstring.as_ptr())?;
        }

        module.kernels.insert(kernel_name.to_string(), func);
        Ok(func)
    }

    /// Launch a kernel
    pub fn launch_kernel(
        &self,
        kernel: CUfunction,
        grid_dim: (u32, u32, u32),
        block_dim: (u32, u32, u32),
        shared_mem_bytes: u32,
        stream: Option<&CudaStream>,
        args: &[*mut std::ffi::c_void],
    ) -> Result<(), CudaError> {
        let stream_handle = stream.map(|s| s.stream).unwrap_or(ptr::null_mut());

        unsafe {
            cuLaunchKernel(
                kernel,
                grid_dim.0, grid_dim.1, grid_dim.2,
                block_dim.0, block_dim.1, block_dim.2,
                shared_mem_bytes,
                stream_handle,
                args.as_ptr() as *mut _,
                ptr::null_mut(),
            )?;
        }

        Ok(())
    }

    /// Create a new execution stream
    pub fn create_stream(&mut self) -> Result<usize, CudaError> {
        let mut stream: CUstream = ptr::null_mut();
        unsafe {
            cuStreamCreate(&mut stream, 0)?;
        }

        let idx = self.streams.len();
        self.streams.push(CudaStream {
            stream,
            events: Vec::new(),
        });

        Ok(idx)
    }

    /// Synchronize all pending operations
    pub fn synchronize(&self) -> Result<(), CudaError> {
        unsafe {
            cuCtxSynchronize()?;
        }
        Ok(())
    }

    /// Allocate device memory
    pub fn malloc(&self, size: usize) -> Result<CUdeviceptr, CudaError> {
        let mut ptr: CUdeviceptr = 0;
        unsafe {
            cuMemAlloc(&mut ptr, size)?;
        }
        Ok(ptr)
    }

    /// Free device memory
    pub fn free(&self, ptr: CUdeviceptr) -> Result<(), CudaError> {
        unsafe {
            cuMemFree(ptr)?;
        }
        Ok(())
    }

    /// Copy data to device
    pub fn memcpy_to_device<T>(&self, dst: CUdeviceptr, src: &[T]) -> Result<(), CudaError> {
        let size = std::mem::size_of_val(src);
        unsafe {
            cuMemcpyHtoD(dst, src.as_ptr() as *const _, size)?;
        }
        Ok(())
    }

    /// Copy data from device
    pub fn memcpy_from_device<T>(&self, dst: &mut [T], src: CUdeviceptr) -> Result<(), CudaError> {
        let size = std::mem::size_of_val(dst);
        unsafe {
            cuMemcpyDtoH(dst.as_mut_ptr() as *mut _, src, size)?;
        }
        Ok(())
    }

    fn query_device_props(device: CUdevice) -> Result<CudaDeviceProps, CudaError> {
        // Query various device properties via cuDeviceGetAttribute
        // ... implementation details ...
        todo!()
    }
}

impl Drop for CudaRuntime {
    fn drop(&mut self) {
        // Cleanup streams
        for stream in &self.streams {
            unsafe { cuStreamDestroy(stream.stream); }
        }

        // Cleanup modules
        for (_, module) in &self.modules {
            unsafe { cuModuleUnload(module.module); }
        }

        // Destroy context
        unsafe { cuCtxDestroy(self.context); }
    }
}

#[derive(Debug)]
pub enum CudaError {
    InitFailed(i32),
    ModuleNotFound,
    KernelNotFound,
    LaunchFailed(i32),
    MemoryError(i32),
    Other(String),
}
```

### 5.2 Memory Management

```rust
// In crates/compiler/src/cuda_memory.rs

/// GPU memory allocator with pooling
pub struct GpuMemoryPool {
    /// Pool of pre-allocated buffers by size class
    pools: HashMap<usize, Vec<CUdeviceptr>>,
    /// Active allocations
    active: HashMap<CUdeviceptr, usize>,
    /// Runtime reference
    runtime: Arc<CudaRuntime>,
    /// Total allocated bytes
    total_allocated: usize,
    /// Memory limit
    memory_limit: usize,
}

impl GpuMemoryPool {
    pub fn new(runtime: Arc<CudaRuntime>, memory_limit: usize) -> Self {
        Self {
            pools: HashMap::new(),
            active: HashMap::new(),
            runtime,
            total_allocated: 0,
            memory_limit,
        }
    }

    /// Allocate from pool or create new allocation
    pub fn alloc(&mut self, size: usize) -> Result<CUdeviceptr, CudaError> {
        // Round up to next power of 2 (size class)
        let size_class = size.next_power_of_two();

        // Try to get from pool
        if let Some(pool) = self.pools.get_mut(&size_class) {
            if let Some(ptr) = pool.pop() {
                self.active.insert(ptr, size_class);
                return Ok(ptr);
            }
        }

        // Allocate new
        if self.total_allocated + size_class > self.memory_limit {
            return Err(CudaError::MemoryError(-1));
        }

        let ptr = self.runtime.malloc(size_class)?;
        self.active.insert(ptr, size_class);
        self.total_allocated += size_class;

        Ok(ptr)
    }

    /// Return allocation to pool
    pub fn free(&mut self, ptr: CUdeviceptr) {
        if let Some(size_class) = self.active.remove(&ptr) {
            self.pools.entry(size_class).or_default().push(ptr);
        }
    }

    /// Actually free all pooled memory
    pub fn flush(&mut self) {
        for (_, ptrs) in self.pools.drain() {
            for ptr in ptrs {
                let _ = self.runtime.free(ptr);
            }
        }
        self.total_allocated = 0;
    }
}

/// Unified memory manager for heterogeneous CPU+GPU workloads
pub struct UnifiedMemory {
    /// GPU memory pool
    gpu_pool: GpuMemoryPool,
    /// Pinned host memory for fast transfers
    pinned_allocations: Vec<(*mut u8, usize)>,
    /// Registered host memory (page-locked)
    registered_memory: Vec<(*mut u8, usize)>,
}

impl UnifiedMemory {
    /// Allocate pinned host memory (for fast CPU<->GPU transfers)
    pub fn alloc_pinned(&mut self, size: usize) -> Result<*mut u8, CudaError> {
        let mut ptr: *mut u8 = ptr::null_mut();
        unsafe {
            cuMemAllocHost(&mut ptr as *mut _ as *mut _, size)?;
        }
        self.pinned_allocations.push((ptr, size));
        Ok(ptr)
    }

    /// Register existing host memory for fast transfers
    pub fn register_host_memory(&mut self, ptr: *mut u8, size: usize) -> Result<(), CudaError> {
        unsafe {
            cuMemHostRegister(ptr as *mut _, size, 0)?;
        }
        self.registered_memory.push((ptr, size));
        Ok(())
    }
}
```

---

## Part 6: Integration with ZynML and DSLs

### 6.1 DSL Kernel Generation Pipeline

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           DSL Source (ZynML/QuantDSL)                        │
│                                                                              │
│  compute(A, B) @kernel(matmul) @device("cuda:0") {                          │
│      @workgroup(16, 16)                                                      │
│      for i in 0..M, j in 0..N:                                              │
│          var sum = 0.0                                                       │
│          for k in 0..K:                                                      │
│              sum += A[i, k] * B[k, j]                                       │
│          out[i, j] = sum                                                     │
│  }                                                                           │
└────────────────────────────────┬────────────────────────────────────────────┘
                                 │
                                 ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                        ZynPEG Grammar Actions                                │
│                                                                              │
│  Parse @kernel, @device, @workgroup annotations                             │
│  Generate TypedAST with KernelMetadata                                       │
└────────────────────────────────┬────────────────────────────────────────────┘
                                 │
                                 ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                        HIR Lowering with GPU Primitives                      │
│                                                                              │
│  - Map loop indices to ThreadIdx/BlockIdx                                    │
│  - Insert SyncThreads for shared memory access                               │
│  - Generate SharedMemAlloc for tiled algorithms                              │
└────────────────────────────────┬────────────────────────────────────────────┘
                                 │
              ┌──────────────────┴──────────────────┐
              ▼                                     ▼
┌──────────────────────────────────┐  ┌──────────────────────────────────┐
│  CPU Backend (Fallback)          │  │  NVPTX Backend                   │
│                                  │  │                                  │
│  Generate vectorized loops       │  │  Generate PTX via LLVM IR        │
│  Use SIMD intrinsics             │  │  Add nvvm.annotations            │
│  OpenMP parallelization          │  │  Emit to PTX string              │
└──────────────────────────────────┘  └────────────────┬─────────────────┘
                                                       │
                                                       ▼
                                      ┌──────────────────────────────────┐
                                      │  CUDA Runtime                    │
                                      │                                  │
                                      │  Load PTX module                 │
                                      │  Get kernel function             │
                                      │  Launch with grid/block dims     │
                                      └──────────────────────────────────┘
```

### 6.2 Kernel Fusion Optimization

```rust
// In crates/compiler/src/kernel_fusion.rs

/// Fuse multiple kernels into a single launch
pub struct KernelFusionPass {
    /// Maximum shared memory to allow fusion
    max_shared_memory: usize,
    /// Maximum register pressure to allow fusion
    max_registers: u32,
}

impl KernelFusionPass {
    /// Attempt to fuse consecutive kernel launches
    pub fn fuse_kernels(&self, kernels: &[HirFunction]) -> Vec<HirFunction> {
        let mut fused = Vec::new();
        let mut current_group: Vec<&HirFunction> = Vec::new();

        for kernel in kernels {
            if self.can_fuse_with_group(&current_group, kernel) {
                current_group.push(kernel);
            } else {
                // Emit fused kernel for current group
                if !current_group.is_empty() {
                    fused.push(self.create_fused_kernel(&current_group));
                }
                current_group = vec![kernel];
            }
        }

        // Don't forget last group
        if !current_group.is_empty() {
            fused.push(self.create_fused_kernel(&current_group));
        }

        fused
    }

    fn can_fuse_with_group(&self, group: &[&HirFunction], kernel: &HirFunction) -> bool {
        // Check data dependencies
        // Check resource usage (shared memory, registers)
        // Check launch configuration compatibility
        true // Placeholder
    }

    fn create_fused_kernel(&self, kernels: &[&HirFunction]) -> HirFunction {
        // Merge kernel bodies
        // Handle shared memory layout
        // Insert synchronization points
        todo!()
    }
}
```

---

## Part 7: Implementation Roadmap

### Phase 1: Foundation (Weeks 1-3)

| Task | Description | Effort |
|------|-------------|--------|
| TypedAST Extensions | Add kernel metadata, execution constraints | 1 week |
| HIR GPU Primitives | Add GPU instructions to HirInstruction enum | 1 week |
| NVPTX Target Setup | Initialize LLVM NVPTX target, create backend struct | 0.5 week |
| Basic PTX Emission | Compile simple kernel to PTX | 0.5 week |

**Milestone:** Compile and execute simple arithmetic kernel via CUDA driver.

### Phase 2: Core Backend (Weeks 4-6)

| Task | Description | Effort |
|------|-------------|--------|
| Thread Indexing | Implement ThreadIdx/BlockIdx/BlockDim/GridDim | 0.5 week |
| Synchronization | Implement SyncThreads, barriers | 0.5 week |
| Shared Memory | Implement SharedMemAlloc, address spaces | 1 week |
| Atomic Operations | Implement GPU atomics with scopes | 0.5 week |
| Warp Primitives | Implement shuffle, vote, match | 0.5 week |

**Milestone:** Parallel reduction kernel working correctly.

### Phase 3: Advanced Features (Weeks 7-9)

| Task | Description | Effort |
|------|-------------|--------|
| Tensor Cores | Implement MMA operations | 1 week |
| Async Copy | Implement async global→shared copy | 0.5 week |
| Critical Path Optimizer | CPU low-latency optimizations | 1 week |
| SIMD Codegen | Auto-vectorization for CPU fallback | 0.5 week |

**Milestone:** Matrix multiplication using tensor cores.

### Phase 4: Runtime Integration (Weeks 10-12)

| Task | Description | Effort |
|------|-------------|--------|
| CUDA Runtime | cuModule/cuFunction management | 1 week |
| Memory Pool | Efficient GPU memory allocation | 0.5 week |
| Unified Memory | Pinned memory, memory registration | 0.5 week |
| Stream Management | Async execution, synchronization | 0.5 week |
| DSL Integration | Connect ZynML compute() to backend | 0.5 week |

**Milestone:** End-to-end ZynML compute() working with GPU.

---

## Appendix A: LLVM NVPTX Intrinsics Reference

### Thread Indexing
```llvm
declare i32 @llvm.nvvm.read.ptx.sreg.tid.x()
declare i32 @llvm.nvvm.read.ptx.sreg.tid.y()
declare i32 @llvm.nvvm.read.ptx.sreg.tid.z()
declare i32 @llvm.nvvm.read.ptx.sreg.ctaid.x()
declare i32 @llvm.nvvm.read.ptx.sreg.ctaid.y()
declare i32 @llvm.nvvm.read.ptx.sreg.ctaid.z()
declare i32 @llvm.nvvm.read.ptx.sreg.ntid.x()
declare i32 @llvm.nvvm.read.ptx.sreg.ntid.y()
declare i32 @llvm.nvvm.read.ptx.sreg.ntid.z()
declare i32 @llvm.nvvm.read.ptx.sreg.nctaid.x()
declare i32 @llvm.nvvm.read.ptx.sreg.nctaid.y()
declare i32 @llvm.nvvm.read.ptx.sreg.nctaid.z()
```

### Synchronization
```llvm
declare void @llvm.nvvm.barrier0()
declare void @llvm.nvvm.barrier.sync(i32)
declare void @llvm.nvvm.membar.cta()
declare void @llvm.nvvm.membar.gl()
declare void @llvm.nvvm.membar.sys()
```

### Warp Operations
```llvm
declare i32 @llvm.nvvm.shfl.sync.idx.i32(i32, i32, i32, i32)
declare i32 @llvm.nvvm.shfl.sync.up.i32(i32, i32, i32, i32)
declare i32 @llvm.nvvm.shfl.sync.down.i32(i32, i32, i32, i32)
declare i32 @llvm.nvvm.shfl.sync.bfly.i32(i32, i32, i32, i32)
declare i32 @llvm.nvvm.vote.ballot.sync(i32, i1)
declare i1 @llvm.nvvm.vote.any.sync(i32, i1)
declare i1 @llvm.nvvm.vote.all.sync(i32, i1)
```

### Tensor Cores (Ampere+)
```llvm
declare {<2 x half>, <2 x half>, <2 x half>, <2 x half>}
    @llvm.nvvm.wmma.load.a.sync.row.m16n16k16.f16(ptr addrspace(1), i32)
declare void @llvm.nvvm.wmma.store.d.sync.row.m16n16k16.f32(ptr addrspace(1),
    float, float, float, float, float, float, float, float, i32)
declare {float, float, float, float, float, float, float, float}
    @llvm.nvvm.wmma.mma.sync.row.row.m16n16k16.f32.f16(
        <2 x half>, <2 x half>, <2 x half>, <2 x half>,
        <2 x half>, <2 x half>, <2 x half>, <2 x half>,
        float, float, float, float, float, float, float, float)
```

---

## Appendix B: Performance Targets

### Kernel Launch Latency
| Operation | Target |
|-----------|--------|
| PTX load (cached) | < 1μs |
| Kernel launch | < 5μs |
| Empty kernel round-trip | < 10μs |
| Grid synchronization | < 100μs |

### Memory Bandwidth
| Operation | Target |
|-----------|--------|
| H2D (pinned) | > 12 GB/s (PCIe 4.0) |
| D2H (pinned) | > 12 GB/s (PCIe 4.0) |
| Device memory | > 900 GB/s (A100) |
| Shared memory | > 19 TB/s (A100) |

### Compute Throughput
| Operation | Target (A100) |
|-----------|---------------|
| FP32 | 19.5 TFLOPS |
| FP16 | 156 TFLOPS (tensor cores) |
| INT8 | 624 TOPS (tensor cores) |

### Critical Path CPU Latency
| Operation | Target |
|-----------|--------|
| Order submission | < 500ns |
| Signal evaluation | < 1μs |
| Risk check | < 100ns |
| Position update | < 200ns |

---

## References

- [LLVM NVPTX Backend](https://llvm.org/docs/NVPTXUsage.html)
- [CUDA C++ Programming Guide](https://docs.nvidia.com/cuda/cuda-c-programming-guide/)
- [PTX ISA Reference](https://docs.nvidia.com/cuda/parallel-thread-execution/)
- [NVIDIA NVVM IR Specification](https://docs.nvidia.com/cuda/nvvm-ir-spec/)
- [Tensor Core Programming](https://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html#wmma)
- [Triton Language](https://github.com/openai/triton) - Reference for DSL design
- [Halide](https://halide-lang.org/) - Reference for scheduling DSL

---

*Last Updated: December 2025*
*Version: 1.0*
*Status: Planning*
