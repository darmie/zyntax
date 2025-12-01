# ZynML GPU Compute System

**Status**: Future Work
**Priority**: High (after core ZynML stabilization)
**Complexity**: Very High

## Overview

The `compute()` construct in ZynML provides a unified way to express parallel computations that can be dispatched to:
- **GPU** (CUDA, Metal, Vulkan Compute, WebGPU)
- **SIMD CPU** (AVX2, AVX-512, NEON)
- **Accelerators** (TPU, NPU, future hardware)

The same kernel code works across all backends with automatic optimization.

## Design Goals

1. **Single Source** - Write once, run on any device
2. **Explicit Parallelism** - Clear parallel structure, no magic
3. **Composable** - Kernels can be fused automatically
4. **Type Safe** - Catch dimension errors at compile time
5. **Debuggable** - Fall back to CPU for debugging
6. **Performant** - Match hand-written CUDA/Metal performance

## Syntax Design

### Basic Compute Expression

```zynml
// Compute expression syntax
let output = compute(input1, input2, ...) [@device(device)] [@async] {
    @kernel kernel_type [kernel_params]
    [@workgroup(x, y, z)]
    [@shared(size)]

    // Kernel body
    for index_vars in ranges:
        // Computation
        out[indices] = expression
}
```

### Kernel Types

```zynml
// === Element-wise Operations ===
// Automatically parallelized per-element

let y = compute(x) {
    @kernel elementwise
    for i in 0..len:
        out[i] = relu(x[i])
}

// With multiple inputs
let z = compute(x, y) {
    @kernel elementwise
    for i in 0..len:
        out[i] = x[i] * y[i] + 1.0
}

// Broadcasting supported
let scaled = compute(matrix, vector) {
    @kernel elementwise
    @broadcast(vector, axis=1)
    for i in 0..rows, j in 0..cols:
        out[i, j] = matrix[i, j] * vector[j]
}


// === Reduction Operations ===
// Parallel reduction with specified operator

let sum = compute(data) {
    @kernel reduce(+)
    for i in 0..len:
        yield data[i]
}

let max_val = compute(data) {
    @kernel reduce(max)
    for i in 0..len:
        yield data[i]
}

// Reduction with transformation
let norm = compute(data) {
    @kernel reduce(+)
    for i in 0..len:
        yield data[i] * data[i]
} |> sqrt()

// Reduction along axis
let row_sums = compute(matrix) {
    @kernel reduce(+, axis=1)
    for i in 0..rows, j in 0..cols:
        yield matrix[i, j]
}

// Argmax (reduction returning index)
let max_idx = compute(data) {
    @kernel reduce(argmax)
    for i in 0..len:
        yield (data[i], i)  // (value, index) pair
}


// === Matrix Operations ===

// Matrix multiply
let C = compute(A, B) {
    @kernel matmul
    @workgroup(16, 16)
    @shared(16 * 16 * 4 * 2)  // Tile A and B in shared memory
    for i in 0..M, j in 0..N:
        var sum = 0.0
        for k in 0..K:
            sum += A[i, k] * B[k, j]
        out[i, j] = sum
}

// Batched matmul
let C = compute(A, B) {
    @kernel batched_matmul
    @workgroup(16, 16, 1)
    for b in 0..batch, i in 0..M, j in 0..N:
        var sum = 0.0
        for k in 0..K:
            sum += A[b, i, k] * B[b, k, j]
        out[b, i, j] = sum
}


// === Convolution ===

let output = compute(input, kernel, bias) {
    @kernel conv2d
    @workgroup(8, 8, 4)
    for b in 0..batch, oc in 0..out_channels:
        for oh in 0..out_h, ow in 0..out_w:
            var sum = bias[oc]
            for ic in 0..in_channels:
                for kh in 0..kernel_h, kw in 0..kernel_w:
                    let ih = oh * stride + kh - padding
                    let iw = ow * stride + kw - padding
                    if ih >= 0 and ih < in_h and iw >= 0 and iw < in_w:
                        sum += input[b, ic, ih, iw] * kernel[oc, ic, kh, kw]
            out[b, oc, oh, ow] = sum
}

// Depthwise convolution
let output = compute(input, kernel) {
    @kernel conv2d_depthwise
    @workgroup(8, 8)
    for b in 0..batch, c in 0..channels:
        for oh in 0..out_h, ow in 0..out_w:
            var sum = 0.0
            for kh in 0..kernel_h, kw in 0..kernel_w:
                sum += input[b, c, oh + kh, ow + kw] * kernel[c, kh, kw]
            out[b, c, oh, ow] = sum
}


// === Attention Mechanisms ===

// Scaled dot-product attention
let attn = compute(Q, K, V, mask) {
    @kernel attention
    @workgroup(32, 4)  // seq_tile, head_tile
    @shared(32 * 64 * 4)  // K, V tiles

    for b in 0..batch, h in 0..heads, i in 0..seq_len:
        // Compute max for numerical stability
        var max_score = -inf
        for j in 0..seq_len:
            if mask[i, j]:
                let score = dot(Q[b, h, i], K[b, h, j]) / sqrt(head_dim)
                max_score = max(max_score, score)

        // Compute softmax denominator
        var sum_exp = 0.0
        for j in 0..seq_len:
            if mask[i, j]:
                let score = dot(Q[b, h, i], K[b, h, j]) / sqrt(head_dim)
                sum_exp += exp(score - max_score)

        // Weighted sum of values
        for d in 0..head_dim:
            var acc = 0.0
            for j in 0..seq_len:
                if mask[i, j]:
                    let score = dot(Q[b, h, i], K[b, h, j]) / sqrt(head_dim)
                    let weight = exp(score - max_score) / sum_exp
                    acc += weight * V[b, h, j, d]
            out[b, h, i, d] = acc
}

// Flash attention (memory-efficient)
let attn = compute(Q, K, V) {
    @kernel flash_attention
    @workgroup(64)
    @tile(block_q=64, block_kv=64)

    for b in 0..batch, h in 0..heads:
        for q_block in 0..seq_len step 64:
            // Online softmax computation
            var m = -inf  // Running max
            var l = 0.0   // Running sum of exp
            var o = zeros(64, head_dim)  // Running output

            for kv_block in 0..seq_len step 64:
                // Load K, V blocks to shared memory
                let K_block = K[b, h, kv_block:kv_block+64]
                let V_block = V[b, h, kv_block:kv_block+64]

                // Compute attention scores for this block
                let S = Q[b, h, q_block:q_block+64] @ transpose(K_block) / sqrt(head_dim)

                // Update running statistics
                let m_new = max(m, row_max(S))
                let l_new = exp(m - m_new) * l + row_sum(exp(S - m_new))

                // Update output
                o = exp(m - m_new) * o + exp(S - m_new) @ V_block
                m = m_new
                l = l_new

            // Normalize
            out[b, h, q_block:q_block+64] = o / l


// === Custom Kernels ===

// Softmax (fused)
let probs = compute(logits) {
    @kernel fused
    // First pass: find max
    let max_val = reduce(max, logits)
    // Second pass: compute exp and sum
    let shifted = logits - max_val
    let exp_vals = exp(shifted)
    let sum_exp = reduce(+, exp_vals)
    // Third pass: normalize
    out = exp_vals / sum_exp
}

// Layer norm (fused)
let normalized = compute(x, gamma, beta) {
    @kernel fused
    @workgroup(256)

    for b in 0..batch:
        // Compute mean
        var mean = 0.0
        for i in 0..hidden:
            mean += x[b, i]
        mean /= hidden

        // Compute variance
        var var = 0.0
        for i in 0..hidden:
            let diff = x[b, i] - mean
            var += diff * diff
        var /= hidden

        // Normalize and scale
        let inv_std = rsqrt(var + eps)
        for i in 0..hidden:
            out[b, i] = (x[b, i] - mean) * inv_std * gamma[i] + beta[i]
}

// GELU activation
let activated = compute(x) {
    @kernel elementwise
    for i in 0..len:
        // GELU: x * 0.5 * (1 + tanh(sqrt(2/pi) * (x + 0.044715 * x^3)))
        let x3 = x[i] * x[i] * x[i]
        out[i] = x[i] * 0.5 * (1.0 + tanh(0.7978845608 * (x[i] + 0.044715 * x3)))
}

// RoPE (Rotary Position Embedding)
let rotated = compute(x, cos_cache, sin_cache) {
    @kernel elementwise
    for b in 0..batch, h in 0..heads, s in 0..seq, d in 0..head_dim/2:
        let x0 = x[b, h, s, d * 2]
        let x1 = x[b, h, s, d * 2 + 1]
        let c = cos_cache[s, d]
        let s = sin_cache[s, d]
        out[b, h, s, d * 2] = x0 * c - x1 * s
        out[b, h, s, d * 2 + 1] = x0 * s + x1 * c
}
```

### Device Management

```zynml
// Query available devices
let devices = compute_devices()
// Returns: [
//   {id: "cuda:0", type: "gpu", name: "NVIDIA RTX 4090", memory: 24GB},
//   {id: "cuda:1", type: "gpu", name: "NVIDIA RTX 4090", memory: 24GB},
//   {id: "metal:0", type: "gpu", name: "Apple M2 Max", memory: 32GB},
//   {id: "cpu", type: "cpu", name: "AMD Ryzen 9", cores: 16},
// ]

// Set default device
config {
    compute_device: "cuda:0"  // or "metal:0", "cpu", "auto"
}

// Per-computation device selection
let gpu_result = compute(data) @device("cuda:0") {
    @kernel matmul
    ...
}

let cpu_result = compute(data) @device("cpu") {
    @kernel matmul
    ...
}

// Automatic device selection (GPU if available, else CPU)
let result = compute(data) @device("auto") {
    @kernel matmul
    ...
}

// Multi-GPU (data parallel)
let result = compute(large_data) @device(["cuda:0", "cuda:1"]) @parallel(data) {
    @kernel matmul
    ...
}

// Multi-GPU (model parallel)
let result = compute(data) @device(["cuda:0", "cuda:1"]) @parallel(model) {
    @kernel large_model_forward
    ...
}
```

### Async Execution

```zynml
// Synchronous (default)
let result = compute(data) {
    @kernel expensive
    ...
}
// Blocks until complete

// Asynchronous
let future = compute(data) @async {
    @kernel expensive
    ...
}
// Returns immediately

// Do other work while GPU computes
let cpu_work = process_metadata()

// Wait for GPU result
let result = await future

// Multiple async computations
let futures = [
    compute(batch1) @async { ... },
    compute(batch2) @async { ... },
    compute(batch3) @async { ... },
]

// Wait for all
let results = await all(futures)

// Wait for any (returns first completed)
let (result, index) = await any(futures)

// Async with timeout
let result = await future timeout 1000ms else default_value
```

### Memory Management

```zynml
// Explicit device memory allocation
let gpu_tensor = allocate(shape=[1024, 1024], dtype=float32, device="cuda:0")

// Copy to device
let gpu_data = data.to_device("cuda:0")

// Copy back to CPU
let cpu_data = gpu_data.to_device("cpu")

// Zero-copy (if supported)
let gpu_view = data.to_device("cuda:0", copy=false)  // Unified memory

// Pinned memory (for faster transfers)
let pinned = allocate(shape=[1024, 1024], dtype=float32, device="cpu", pinned=true)

// Memory pool (reduce allocation overhead)
let pool = memory_pool(device="cuda:0", size=1GB)
let tensor1 = pool.allocate(shape=[512, 512])
let tensor2 = pool.allocate(shape=[256, 256])
pool.free(tensor1)
pool.reset()  // Free all allocations
```

## Implementation Architecture

### Compilation Pipeline

```
ZynML compute() block
        │
        ▼
┌───────────────────┐
│   Parse & Type    │
│     Check         │
└────────┬──────────┘
         │
         ▼
┌───────────────────┐
│   Compute IR      │  ← Device-independent intermediate representation
│   (CIR)           │
└────────┬──────────┘
         │
         ├─────────────────┬─────────────────┬─────────────────┐
         ▼                 ▼                 ▼                 ▼
┌──────────────┐  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐
│  CUDA        │  │  Metal       │  │  Vulkan      │  │  CPU SIMD    │
│  Backend     │  │  Backend     │  │  Compute     │  │  Backend     │
│  (PTX)       │  │  (MSL)       │  │  (SPIR-V)    │  │  (Cranelift) │
└──────┬───────┘  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘
       │                 │                 │                 │
       ▼                 ▼                 ▼                 ▼
   NVIDIA GPU       Apple GPU         Any GPU            CPU
```

### Compute IR (CIR)

```rust
// Compute Intermediate Representation

pub enum CirType {
    Scalar(ScalarType),
    Tensor { shape: Vec<Dim>, dtype: ScalarType },
    Pointer(Box<CirType>),
}

pub enum ScalarType {
    F16, F32, F64,
    I8, I16, I32, I64,
    U8, U16, U32, U64,
    Bool,
}

pub enum Dim {
    Fixed(usize),
    Dynamic(String),  // Named dimension
}

pub struct ComputeKernel {
    name: String,
    kernel_type: KernelType,
    inputs: Vec<KernelInput>,
    output: KernelOutput,
    workgroup_size: Option<[u32; 3]>,
    shared_memory: Option<usize>,
    body: Vec<CirStatement>,
}

pub enum KernelType {
    Elementwise,
    Reduce(ReduceOp),
    MatMul,
    Conv2d { stride: u32, padding: u32 },
    Attention,
    Custom,
    Fused(Vec<KernelType>),
}

pub enum ReduceOp {
    Sum, Product, Min, Max, And, Or, ArgMin, ArgMax,
}

pub struct KernelInput {
    name: String,
    ty: CirType,
    access: AccessMode,
}

pub enum AccessMode {
    Read,
    Write,
    ReadWrite,
}

pub enum CirStatement {
    // Control flow
    For { var: String, range: Range, body: Vec<CirStatement> },
    If { cond: CirExpr, then_body: Vec<CirStatement>, else_body: Vec<CirStatement> },

    // Assignment
    Let { var: String, value: CirExpr },
    Assign { target: CirExpr, value: CirExpr },

    // Parallel constructs
    ParallelFor { vars: Vec<String>, ranges: Vec<Range>, body: Vec<CirStatement> },
    Barrier,  // Synchronization
    AtomicOp { op: AtomicOp, target: CirExpr, value: CirExpr },

    // Reduction
    Yield(CirExpr),

    // Memory
    SharedAlloc { name: String, size: usize },
    Load { dst: String, src: CirExpr },
    Store { dst: CirExpr, src: CirExpr },
}

pub enum CirExpr {
    // Literals
    IntLit(i64),
    FloatLit(f64),
    BoolLit(bool),

    // Variables
    Var(String),
    ThreadIdx(Axis),
    BlockIdx(Axis),
    BlockDim(Axis),

    // Indexing
    Index { base: Box<CirExpr>, indices: Vec<CirExpr> },
    Slice { base: Box<CirExpr>, ranges: Vec<Range> },

    // Arithmetic
    BinOp { op: BinOp, left: Box<CirExpr>, right: Box<CirExpr> },
    UnaryOp { op: UnaryOp, operand: Box<CirExpr> },

    // Math functions
    MathFn { fn_name: MathFn, args: Vec<CirExpr> },

    // Special
    Dot { a: Box<CirExpr>, b: Box<CirExpr> },
    Reduce { op: ReduceOp, input: Box<CirExpr>, axis: Option<usize> },
}

pub enum MathFn {
    Sin, Cos, Tan, Exp, Log, Sqrt, Rsqrt, Abs, Floor, Ceil,
    Tanh, Sinh, Cosh, Pow, Min, Max, Clamp,
}
```

### Backend Implementations

#### CUDA Backend

```rust
pub struct CudaBackend {
    context: CudaContext,
    module_cache: HashMap<KernelHash, CudaModule>,
}

impl ComputeBackend for CudaBackend {
    fn compile(&mut self, kernel: &ComputeKernel) -> Result<CompiledKernel> {
        // Generate PTX
        let ptx = self.generate_ptx(kernel)?;

        // Compile PTX to cubin
        let module = self.context.load_ptx(&ptx)?;

        Ok(CompiledKernel::Cuda(module))
    }

    fn execute(&self, kernel: &CompiledKernel, args: &[TensorArg]) -> Result<()> {
        let CompiledKernel::Cuda(module) = kernel else { bail!("Wrong backend") };

        // Set up kernel arguments
        let mut kernel_args = Vec::new();
        for arg in args {
            kernel_args.push(arg.device_ptr());
        }

        // Launch kernel
        let grid = self.compute_grid_size(kernel, args);
        let block = kernel.workgroup_size.unwrap_or([256, 1, 1]);

        module.launch(grid, block, &kernel_args)?;

        Ok(())
    }

    fn generate_ptx(&self, kernel: &ComputeKernel) -> Result<String> {
        let mut ptx = String::new();

        // Header
        writeln!(ptx, ".version 7.0")?;
        writeln!(ptx, ".target sm_80")?;
        writeln!(ptx, ".address_size 64")?;

        // Kernel function
        writeln!(ptx, ".visible .entry {}(", kernel.name)?;
        for (i, input) in kernel.inputs.iter().enumerate() {
            let param_type = self.cir_type_to_ptx(&input.ty);
            writeln!(ptx, "  .param .{} param_{},", param_type, i)?;
        }
        writeln!(ptx, ") {{")?;

        // Generate body
        self.generate_ptx_body(&mut ptx, kernel)?;

        writeln!(ptx, "}}")?;

        Ok(ptx)
    }
}
```

#### Metal Backend

```rust
pub struct MetalBackend {
    device: metal::Device,
    command_queue: metal::CommandQueue,
    library_cache: HashMap<KernelHash, metal::Library>,
}

impl ComputeBackend for MetalBackend {
    fn compile(&mut self, kernel: &ComputeKernel) -> Result<CompiledKernel> {
        // Generate MSL (Metal Shading Language)
        let msl = self.generate_msl(kernel)?;

        // Compile MSL
        let library = self.device.new_library_with_source(&msl, &Default::default())?;

        Ok(CompiledKernel::Metal(library))
    }

    fn execute(&self, kernel: &CompiledKernel, args: &[TensorArg]) -> Result<()> {
        let CompiledKernel::Metal(library) = kernel else { bail!("Wrong backend") };

        let function = library.get_function(&kernel.name)?;
        let pipeline = self.device.new_compute_pipeline_state_with_function(&function)?;

        let command_buffer = self.command_queue.new_command_buffer();
        let encoder = command_buffer.new_compute_command_encoder();

        encoder.set_compute_pipeline_state(&pipeline);

        for (i, arg) in args.iter().enumerate() {
            encoder.set_buffer(i as u64, Some(arg.metal_buffer()), 0);
        }

        let grid_size = self.compute_grid_size(kernel, args);
        let threadgroup_size = kernel.workgroup_size.unwrap_or([256, 1, 1]);

        encoder.dispatch_threads(grid_size, threadgroup_size);
        encoder.end_encoding();

        command_buffer.commit();
        command_buffer.wait_until_completed();

        Ok(())
    }

    fn generate_msl(&self, kernel: &ComputeKernel) -> Result<String> {
        let mut msl = String::new();

        writeln!(msl, "#include <metal_stdlib>")?;
        writeln!(msl, "using namespace metal;")?;

        // Kernel function
        writeln!(msl, "kernel void {}(", kernel.name)?;
        for (i, input) in kernel.inputs.iter().enumerate() {
            let metal_type = self.cir_type_to_metal(&input.ty);
            writeln!(msl, "  device {} *arg{} [[buffer({})]],", metal_type, i, i)?;
        }
        writeln!(msl, "  uint3 gid [[thread_position_in_grid]]")?;
        writeln!(msl, ") {{")?;

        // Generate body
        self.generate_msl_body(&mut msl, kernel)?;

        writeln!(msl, "}}")?;

        Ok(msl)
    }
}
```

#### CPU SIMD Backend

```rust
pub struct CpuSimdBackend {
    // Uses existing Cranelift infrastructure
    backend: CraneliftBackend,
    simd_level: SimdLevel,
}

pub enum SimdLevel {
    Scalar,
    Sse41,
    Avx2,
    Avx512,
    Neon,
}

impl ComputeBackend for CpuSimdBackend {
    fn compile(&mut self, kernel: &ComputeKernel) -> Result<CompiledKernel> {
        // Lower to Cranelift IR with SIMD instructions
        let mut builder = self.backend.create_function();

        match kernel.kernel_type {
            KernelType::Elementwise => {
                self.compile_elementwise(&mut builder, kernel)?;
            }
            KernelType::Reduce(op) => {
                self.compile_reduction(&mut builder, kernel, op)?;
            }
            KernelType::MatMul => {
                self.compile_matmul(&mut builder, kernel)?;
            }
            _ => {
                self.compile_generic(&mut builder, kernel)?;
            }
        }

        let func = builder.finalize()?;
        Ok(CompiledKernel::Cpu(func))
    }

    fn compile_elementwise(&self, builder: &mut FunctionBuilder, kernel: &ComputeKernel) -> Result<()> {
        // Vectorize loop by SIMD width
        let simd_width = match self.simd_level {
            SimdLevel::Avx512 => 16,  // 512 bits / 32 bits
            SimdLevel::Avx2 => 8,     // 256 bits / 32 bits
            SimdLevel::Sse41 | SimdLevel::Neon => 4,  // 128 bits / 32 bits
            SimdLevel::Scalar => 1,
        };

        // Generate SIMD loop + scalar remainder
        // ...

        Ok(())
    }
}
```

## Dependencies

### Rust Crates

```toml
[dependencies]
# Core
zyntax-compiler = { path = "../crates/compiler" }

# CUDA (optional)
[target.'cfg(target_os = "linux")'.dependencies]
cuda-runtime = { version = "0.3", optional = true }
cuda-driver = { version = "0.3", optional = true }

# Metal (Apple only)
[target.'cfg(target_os = "macos")'.dependencies]
metal = { version = "0.27", optional = true }

# Vulkan (cross-platform)
vulkano = { version = "0.34", optional = true }
vulkano-shaders = { version = "0.34", optional = true }

# WebGPU (for browser/wasm)
wgpu = { version = "0.19", optional = true }

[features]
default = ["cpu"]
cpu = []
cuda = ["cuda-runtime", "cuda-driver"]
metal = ["dep:metal"]
vulkan = ["vulkano", "vulkano-shaders"]
webgpu = ["wgpu"]
all-backends = ["cpu", "cuda", "metal", "vulkan"]
```

## Implementation Phases

### Phase 1: CPU SIMD Backend (Weeks 1-4)
- [x] Existing `zrtl_simd` operations
- [ ] Compute IR design
- [ ] Elementwise kernels
- [ ] Reduction kernels
- [ ] Integration with Cranelift

### Phase 2: Compute Syntax (Weeks 5-6)
- [ ] Grammar extension for `compute()`
- [ ] Type checking for kernels
- [ ] Lowering to Compute IR

### Phase 3: CUDA Backend (Weeks 7-10)
- [ ] PTX code generation
- [ ] CUDA runtime integration
- [ ] Memory management
- [ ] Async execution

### Phase 4: Metal Backend (Weeks 11-14)
- [ ] MSL code generation
- [ ] Metal runtime integration
- [ ] Shared memory support

### Phase 5: Advanced Features (Weeks 15-18)
- [ ] Kernel fusion
- [ ] Auto-tuning
- [ ] Multi-GPU support
- [ ] Flash attention kernel

### Phase 6: WebGPU Backend (Weeks 19-22)
- [ ] WGSL code generation
- [ ] Browser integration
- [ ] ZynBook GPU support

## Performance Targets

### vs PyTorch (CUDA)

| Operation | ZynML Target | PyTorch |
|-----------|--------------|---------|
| MatMul (4096x4096) | 1.1x | 1.0x |
| Softmax (seq=2048) | 1.0x | 1.0x |
| Attention (BERT-base) | 1.5x | 1.0x (with Flash) |
| Conv2d (ResNet block) | 1.0x | 1.0x |
| Custom fused kernel | 2-3x | Manual CUDA |

### Memory Efficiency

| Operation | ZynML | PyTorch |
|-----------|-------|---------|
| Attention (seq=4096) | O(n) | O(n²) |
| Fused LayerNorm | 1 pass | 2 passes |
| Activation checkpointing | Automatic | Manual |

## Example: End-to-End Transformer Block

```zynml
pipeline transformer_block(x: tensor[batch, seq, hidden], layer: int) -> tensor[batch, seq, hidden]:
    let weights = model.layers[layer]

    // Self-attention with fused softmax
    let qkv = compute(x, weights.qkv_proj) {
        @kernel matmul
        @device("auto")
        for b in 0..batch, s in 0..seq, h in 0..3*hidden:
            var sum = 0.0
            for i in 0..hidden:
                sum += x[b, s, i] * weights.qkv_proj[i, h]
            out[b, s, h] = sum
    }

    let (q, k, v) = split(qkv, 3, axis=-1)

    // Reshape for multi-head
    let q = reshape(q, [batch, seq, num_heads, head_dim])
    let k = reshape(k, [batch, seq, num_heads, head_dim])
    let v = reshape(v, [batch, seq, num_heads, head_dim])

    // Flash attention
    let attn_out = compute(q, k, v) @device("auto") {
        @kernel flash_attention
        @workgroup(64)
        // ... flash attention implementation
    }

    // Output projection
    let attn_out = reshape(attn_out, [batch, seq, hidden])
    let projected = compute(attn_out, weights.out_proj) {
        @kernel matmul
        for b in 0..batch, s in 0..seq, h in 0..hidden:
            var sum = 0.0
            for i in 0..hidden:
                sum += attn_out[b, s, i] * weights.out_proj[i, h]
            out[b, s, h] = sum
    }

    // Residual + LayerNorm (fused)
    let normed = compute(x, projected, weights.ln1_weight, weights.ln1_bias) {
        @kernel fused
        for b in 0..batch, s in 0..seq:
            // Add residual
            var sum = 0.0
            var sq_sum = 0.0
            for h in 0..hidden:
                let val = x[b, s, h] + projected[b, s, h]
                sum += val
                sq_sum += val * val

            let mean = sum / hidden
            let var = sq_sum / hidden - mean * mean
            let inv_std = rsqrt(var + 1e-6)

            for h in 0..hidden:
                let val = x[b, s, h] + projected[b, s, h]
                out[b, s, h] = (val - mean) * inv_std * weights.ln1_weight[h] + weights.ln1_bias[h]
    }

    // FFN (fused GELU)
    let ffn_out = compute(normed, weights.ffn_up, weights.ffn_down) {
        @kernel fused
        @shared(batch * seq * ffn_hidden * 4)

        for b in 0..batch, s in 0..seq:
            // Up projection + GELU
            shared var up[ffn_hidden]
            for f in 0..ffn_hidden:
                var sum = 0.0
                for h in 0..hidden:
                    sum += normed[b, s, h] * weights.ffn_up[h, f]
                // GELU
                let x = sum
                let x3 = x * x * x
                up[f] = x * 0.5 * (1.0 + tanh(0.7978845608 * (x + 0.044715 * x3)))

            barrier()

            // Down projection
            for h in 0..hidden:
                var sum = 0.0
                for f in 0..ffn_hidden:
                    sum += up[f] * weights.ffn_down[f, h]
                out[b, s, h] = sum
    }

    // Final residual + LayerNorm
    let output = compute(normed, ffn_out, weights.ln2_weight, weights.ln2_bias) {
        @kernel fused
        // ... same as above
    }

    return output
```

This provides a complete, GPU-accelerated transformer block written entirely in ZynML!
