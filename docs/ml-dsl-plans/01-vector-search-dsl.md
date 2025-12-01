# Vector Search DSL

**Priority**: High
**Complexity**: Medium
**Estimated New Code**: ~2000 LOC

## Overview

A domain-specific language for vector similarity search, powering RAG systems, semantic search, and recommendation engines. Optimized for CPU inference with SIMD acceleration.

## Use Cases

1. **RAG (Retrieval-Augmented Generation)**
   - Retrieve relevant documents for LLM context
   - Semantic chunk matching

2. **Semantic Search**
   - Natural language queries over document collections
   - Code search, FAQ matching

3. **Recommendation Systems**
   - Item-to-item similarity
   - User preference matching

4. **Deduplication**
   - Near-duplicate detection
   - Content clustering

## Syntax Design

```
// vecsearch.vs - Vector Search DSL

// Load pre-computed embeddings
load embeddings "products.npy" as catalog dim 384
load embeddings "categories.npy" as categories dim 384

// Optional: Load an encoder model for query embedding
load encoder "all-MiniLM-L6-v2.onnx" as encoder

// Define an index with specific algorithm
index catalog using hnsw {
    m: 16,
    ef_construction: 200,
    ef_search: 50
}

// Search pipeline
pipeline product_search:
    input: text

    // Encode query text to vector
    query_vec = encode encoder input

    // Normalize for cosine similarity
    normalize query_vec

    // Search with filtering
    results = search catalog
        for query_vec
        top 10
        using cosine
        where category in ["electronics", "computers"]

    // Post-process: rerank or filter
    rerank results by freshness_score * 0.3 + similarity * 0.7

    output: results

// Execute
run product_search with "wireless bluetooth headphones"
```

## Grammar Specification

```
// vecsearch.zyn

@name = "vecsearch"
@version = "1.0"
@file_extensions = [".vs", ".vecsearch"]

@builtins {
    // Tensor operations
    tensor_load_npy: "$Tensor$load_npy"
    tensor_load_safetensors: "$Tensor$load_safetensors"
    tensor_alloc: "$Tensor$alloc"
    tensor_free: "$Tensor$free"

    // Vector operations (from zrtl_simd)
    vec_dot: "$SIMD$dot_product_f32"
    vec_sum: "$SIMD$sum_f32"
    vec_scale: "$SIMD$scale_f32"
    vec_add: "$SIMD$add_f32"
    vec_sqrt: "$SIMD$sqrt_f32"

    // Search operations (new plugin: zrtl_vecsearch)
    vec_normalize: "$VecSearch$l2_normalize"
    vec_cosine_batch: "$VecSearch$cosine_similarity_batch"
    vec_euclidean_batch: "$VecSearch$euclidean_distance_batch"
    vec_topk: "$VecSearch$topk"
    vec_filter: "$VecSearch$filter_by_mask"

    // Index operations
    hnsw_build: "$VecSearch$hnsw_build"
    hnsw_search: "$VecSearch$hnsw_search"
    ivf_build: "$VecSearch$ivf_build"
    ivf_search: "$VecSearch$ivf_search"

    // Encoder operations (optional ONNX runtime)
    encoder_load: "$Encoder$load_onnx"
    encoder_run: "$Encoder$run"

    // IO
    print: "$IO$println"
    json_output: "$IO$json_serialize"
}

// Grammar rules
program = statement*

statement = load_stmt
          | index_stmt
          | pipeline_def
          | run_stmt
          | config_stmt

// Load embeddings or encoder
load_stmt = "load" load_type STRING "as" IDENT ("dim" INTEGER)?

load_type = "embeddings" | "encoder" | "metadata"

// Create search index
index_stmt = "index" IDENT "using" index_type index_config?

index_type = "hnsw" | "ivf" | "flat" | "pq"

index_config = "{" (config_pair ("," config_pair)*)? "}"

config_pair = IDENT ":" literal

// Pipeline definition
pipeline_def = "pipeline" IDENT ":" NEWLINE INDENT pipeline_body DEDENT

pipeline_body = input_decl statement* output_decl

input_decl = "input" ":" type_spec

output_decl = "output" ":" expr

// Pipeline statements
pipeline_stmt = assign_stmt | search_stmt | transform_stmt | filter_stmt

assign_stmt = IDENT "=" expr

search_stmt = "search" IDENT "for" expr "top" INTEGER
              ("using" distance_metric)?
              ("where" filter_expr)?

distance_metric = "cosine" | "euclidean" | "dot" | "manhattan"

transform_stmt = "normalize" IDENT
               | "encode" IDENT IDENT
               | "rerank" IDENT "by" expr

filter_expr = IDENT comparator literal
            | IDENT "in" "[" literal_list "]"
            | filter_expr "and" filter_expr
            | filter_expr "or" filter_expr

// Run pipeline
run_stmt = "run" IDENT ("with" expr)?

// Configuration
config_stmt = "config" "{" config_pair* "}"

// Expressions
expr = primary_expr (binary_op primary_expr)*

primary_expr = IDENT
             | literal
             | call_expr
             | "(" expr ")"

call_expr = IDENT "(" (expr ("," expr)*)? ")"

binary_op = "+" | "-" | "*" | "/" | "."

// Literals
literal = STRING | INTEGER | FLOAT | BOOL

literal_list = literal ("," literal)*

// Types
type_spec = "text" | "vector" | "tensor" dim_spec?

dim_spec = "[" INTEGER ("," INTEGER)* "]"

// Tokens
IDENT = /[a-zA-Z_][a-zA-Z0-9_]*/
STRING = /"[^"]*"/ | /'[^']*'/
INTEGER = /[0-9]+/
FLOAT = /[0-9]+\.[0-9]+/
BOOL = "true" | "false"
NEWLINE = /\n/
INDENT = <indentation increase>
DEDENT = <indentation decrease>

// Comments and whitespace
COMMENT = "//" /[^\n]*/
WS = /[ \t]+/
```

## Required New Operations

### Plugin: `zrtl_vecsearch`

```rust
// New plugin for vector search operations

/// L2 normalize a vector in-place: v = v / ||v||
#[no_mangle]
pub extern "C" fn l2_normalize(data: *mut f32, len: u64);

/// Batch cosine similarity: compute similarity of query against all vectors
/// query: [dim], vectors: [n, dim], output: [n]
#[no_mangle]
pub extern "C" fn cosine_similarity_batch(
    query: *const f32,
    vectors: *const f32,
    output: *mut f32,
    n: u64,
    dim: u64
);

/// Batch euclidean distance
#[no_mangle]
pub extern "C" fn euclidean_distance_batch(
    query: *const f32,
    vectors: *const f32,
    output: *mut f32,
    n: u64,
    dim: u64
);

/// Find top-k values and their indices
/// Returns: number of results written
#[no_mangle]
pub extern "C" fn topk(
    scores: *const f32,
    n: u64,
    k: u64,
    out_scores: *mut f32,
    out_indices: *mut u64
) -> u64;

/// Filter vectors by boolean mask
#[no_mangle]
pub extern "C" fn filter_by_mask(
    vectors: *const f32,
    mask: *const u8,
    output: *mut f32,
    n: u64,
    dim: u64
) -> u64;  // Returns count of filtered vectors

// HNSW Index operations
#[no_mangle]
pub extern "C" fn hnsw_build(
    vectors: *const f32,
    n: u64,
    dim: u64,
    m: u32,
    ef_construction: u32
) -> *mut HnswIndex;

#[no_mangle]
pub extern "C" fn hnsw_search(
    index: *const HnswIndex,
    query: *const f32,
    k: u64,
    ef_search: u32,
    out_indices: *mut u64,
    out_distances: *mut f32
) -> u64;

#[no_mangle]
pub extern "C" fn hnsw_free(index: *mut HnswIndex);
```

### Plugin: `zrtl_tensor`

```rust
/// Tensor handle (opaque pointer to tensor metadata + data)
pub struct TensorHandle {
    data: *mut u8,
    shape: Vec<usize>,
    stride: Vec<usize>,
    dtype: DType,
}

/// Load tensor from NPY file
#[no_mangle]
pub extern "C" fn tensor_load_npy(
    path: *const u8,  // ZRTL string
    path_len: u32
) -> *mut TensorHandle;

/// Load tensor from safetensors file
#[no_mangle]
pub extern "C" fn tensor_load_safetensors(
    path: *const u8,
    path_len: u32,
    tensor_name: *const u8,
    name_len: u32
) -> *mut TensorHandle;

/// Get tensor data pointer
#[no_mangle]
pub extern "C" fn tensor_data_ptr(handle: *const TensorHandle) -> *const f32;

/// Get tensor shape
#[no_mangle]
pub extern "C" fn tensor_shape(
    handle: *const TensorHandle,
    dim: u32
) -> u64;

/// Get tensor rank (number of dimensions)
#[no_mangle]
pub extern "C" fn tensor_rank(handle: *const TensorHandle) -> u32;

/// Allocate new tensor
#[no_mangle]
pub extern "C" fn tensor_alloc(
    shape: *const u64,
    rank: u32,
    dtype: u32
) -> *mut TensorHandle;

/// Free tensor
#[no_mangle]
pub extern "C" fn tensor_free(handle: *mut TensorHandle);
```

## Dependencies

### Rust Crates

```toml
[dependencies]
# Core
zrtl = { path = "../sdk/zrtl" }
wide = "0.7"

# Tensor I/O
npy = "0.4"                    # NPY file format
safetensors = "0.4"            # Safetensors format
memmap2 = "0.9"                # Memory-mapped files

# HNSW Index (optional, can implement from scratch)
hnsw = "0.11"                  # Or implement custom

# ONNX Runtime (optional, for encoder)
ort = { version = "2.0", optional = true }
```

### External Dependencies

- **ONNX Runtime** (optional): For running encoder models
  - Can be disabled for pure vector search (pre-computed embeddings)
  - ~50MB shared library

## File Structure

```
examples/vecsearch/
├── Cargo.toml
├── vecsearch.zyn              # Grammar definition
├── src/
│   └── main.rs                # CLI entry point
├── samples/
│   ├── simple_search.vs       # Basic example
│   ├── rag_pipeline.vs        # RAG use case
│   └── recommendations.vs     # Recommendation system
└── test_data/
    ├── sample_embeddings.npy  # Test vectors
    └── sample_queries.txt     # Test queries

plugins/zrtl_vecsearch/
├── Cargo.toml
├── src/
│   ├── lib.rs                 # Plugin entry
│   ├── similarity.rs          # Similarity functions
│   ├── topk.rs                # Top-k selection
│   ├── hnsw.rs                # HNSW index
│   └── filter.rs              # Filtering operations
└── benches/
    └── similarity_bench.rs    # Benchmarks

plugins/zrtl_tensor/
├── Cargo.toml
├── src/
│   ├── lib.rs                 # Plugin entry
│   ├── handle.rs              # Tensor handle
│   ├── npy.rs                 # NPY loader
│   ├── safetensors.rs         # Safetensors loader
│   └── alloc.rs               # Allocation
└── tests/
    └── load_tests.rs
```

## Implementation Plan

### Phase 1: Core Tensor Infrastructure (Week 1)
- [ ] Implement `zrtl_tensor` plugin
  - [ ] TensorHandle struct with shape/stride/dtype
  - [ ] NPY file loading (f32, f16, i8)
  - [ ] Memory-mapped loading for large files
  - [ ] Basic allocation/free

### Phase 2: Similarity Operations (Week 1-2)
- [ ] Implement `zrtl_vecsearch` similarity functions
  - [ ] `l2_normalize` with SIMD
  - [ ] `cosine_similarity_batch` with SIMD
  - [ ] `euclidean_distance_batch` with SIMD
  - [ ] Unit tests and benchmarks

### Phase 3: Top-K and Filtering (Week 2)
- [ ] Implement selection operations
  - [ ] `topk` with partial sort (heap-based)
  - [ ] `filter_by_mask` for metadata filtering
  - [ ] Integration tests

### Phase 4: Grammar and Runtime (Week 2-3)
- [ ] Write `vecsearch.zyn` grammar
- [ ] Implement semantic actions
  - [ ] Load statement handling
  - [ ] Search pipeline compilation
  - [ ] Result formatting

### Phase 5: HNSW Index (Week 3-4)
- [ ] Implement HNSW index
  - [ ] Graph construction
  - [ ] Search with ef parameter
  - [ ] Serialization/deserialization
  - [ ] OR integrate existing crate

### Phase 6: CLI and Examples (Week 4)
- [ ] Build CLI tool
- [ ] Create example pipelines
- [ ] Write documentation
- [ ] Benchmark vs FAISS/usearch

## Performance Targets

| Operation | Target Throughput | Baseline (scalar) |
|-----------|-------------------|-------------------|
| Cosine similarity (384d, 1M vectors) | 50ms | 400ms |
| Top-100 selection (1M scores) | 5ms | 20ms |
| HNSW search (1M vectors, k=10) | 1ms | N/A |
| L2 normalize (384d) | 0.5μs | 3μs |

## Example Outputs

### Simple Search

```bash
$ vecsearch run samples/simple_search.vs --query "machine learning tutorial"

Results (top 5):
  1. [0.923] "Introduction to Machine Learning" (id: doc_1842)
  2. [0.891] "Deep Learning Fundamentals" (id: doc_2103)
  3. [0.887] "ML Tutorial for Beginners" (id: doc_0521)
  4. [0.842] "Neural Network Basics" (id: doc_1204)
  5. [0.831] "Supervised Learning Guide" (id: doc_0892)

Search completed in 12.3ms (scanned 100,000 vectors)
```

### RAG Pipeline

```bash
$ vecsearch run samples/rag_pipeline.vs --query "How do I configure HTTPS?"

Retrieved contexts (top 3):
  1. [0.912] docs/security/https-setup.md:45-78
     "To configure HTTPS, first obtain an SSL certificate..."

  2. [0.887] docs/configuration/ssl.md:12-34
     "SSL configuration requires the following parameters..."

  3. [0.854] docs/tutorials/secure-deployment.md:89-120
     "For production deployments, HTTPS is mandatory..."

Total retrieval time: 8.2ms
```

## Testing Strategy

### Unit Tests
- Similarity function correctness
- Top-k selection edge cases
- Tensor loading various formats

### Integration Tests
- End-to-end pipeline execution
- Memory leak detection
- Large dataset handling

### Benchmarks
- Throughput vs vector count
- Latency distribution (p50, p95, p99)
- Memory usage profiling
- Comparison with FAISS, usearch, qdrant

## Future Enhancements

1. **Product Quantization (PQ)**
   - Compress vectors 4-8x
   - Approximate distance computation

2. **IVF Index**
   - Inverted file index for billion-scale
   - Cluster-based pruning

3. **Hybrid Search**
   - Combine vector + keyword search
   - BM25 integration

4. **Streaming Updates**
   - Add/remove vectors without rebuild
   - Incremental index updates

5. **GPU Acceleration**
   - CUDA backend for batch operations
   - Metal for Apple Silicon
