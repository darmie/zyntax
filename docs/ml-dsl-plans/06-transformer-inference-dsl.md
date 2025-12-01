# Transformer Inference DSL

**Priority**: Low (High complexity)
**Complexity**: Very High
**Estimated New Code**: ~6000 LOC

## Overview

A domain-specific language for running transformer-based models (BERT, GPT-style) on CPU. Focuses on efficient attention computation and KV-cache management for text generation.

## Use Cases

1. **Text Embedding**
   - Sentence/document embeddings
   - Semantic similarity
   - RAG encoding

2. **Text Classification**
   - Sentiment analysis
   - Intent classification
   - Content moderation

3. **Text Generation**
   - Chatbots
   - Code completion
   - Summarization

4. **Question Answering**
   - Extractive QA
   - Reading comprehension

## Syntax Design

```
// transformer.tinf - Transformer Inference DSL

// Load transformer model
load transformer "distilbert-base.safetensors" as encoder {
    architecture: bert,
    hidden_size: 768,
    num_layers: 6,
    num_heads: 12,
    vocab_size: 30522,
    max_seq_length: 512,
    tokenizer: "distilbert-tokenizer.json"
}

load transformer "gpt2-small.safetensors" as generator {
    architecture: gpt2,
    hidden_size: 768,
    num_layers: 12,
    num_heads: 12,
    vocab_size: 50257,
    max_seq_length: 1024,
    tokenizer: "gpt2-tokenizer.json"
}

// Define the encoder architecture (BERT-style)
architecture bert:
    // Input processing
    layer token_embeddings: embedding vocab_size -> hidden_size
    layer position_embeddings: embedding max_seq_length -> hidden_size
    layer segment_embeddings: embedding 2 -> hidden_size

    // Transformer blocks
    repeat num_layers:
        // Self-attention
        layer attention:
            q_proj: linear hidden_size -> hidden_size
            k_proj: linear hidden_size -> hidden_size
            v_proj: linear hidden_size -> hidden_size
            out_proj: linear hidden_size -> hidden_size
            num_heads: num_heads
            attention_type: bidirectional

        layer attention_norm: layer_norm hidden_size

        // Feed-forward
        layer ffn:
            up_proj: linear hidden_size -> 4 * hidden_size
            down_proj: linear 4 * hidden_size -> hidden_size
            activation: gelu

        layer ffn_norm: layer_norm hidden_size

    // Pooler (for classification)
    layer pooler: linear hidden_size -> hidden_size activation=tanh

// Define decoder architecture (GPT-style)
architecture gpt2:
    layer token_embeddings: embedding vocab_size -> hidden_size
    layer position_embeddings: embedding max_seq_length -> hidden_size

    repeat num_layers:
        layer attention:
            q_proj: linear hidden_size -> hidden_size
            k_proj: linear hidden_size -> hidden_size
            v_proj: linear hidden_size -> hidden_size
            out_proj: linear hidden_size -> hidden_size
            num_heads: num_heads
            attention_type: causal  // Masked self-attention

        layer attention_norm: layer_norm hidden_size

        layer ffn:
            up_proj: linear hidden_size -> 4 * hidden_size
            down_proj: linear 4 * hidden_size -> hidden_size
            activation: gelu

        layer ffn_norm: layer_norm hidden_size

    layer lm_head: linear hidden_size -> vocab_size

// Embedding pipeline
pipeline encode_text:
    input: text string
    config: {
        max_length: 512,
        truncation: true,
        padding: max_length
    }

    // Tokenize
    tokens = tokenize encoder.tokenizer input
    attention_mask = create_attention_mask tokens

    // Get embeddings
    x = embed encoder.token_embeddings tokens
    x = x + embed encoder.position_embeddings positions(len(tokens))

    // Run transformer blocks
    for layer in encoder.layers:
        // Self-attention with residual
        residual = x
        x = layer_norm layer.attention_norm x

        q = linear layer.attention.q_proj x
        k = linear layer.attention.k_proj x
        v = linear layer.attention.v_proj x

        attn_out = multi_head_attention q k v attention_mask num_heads=12
        x = linear layer.attention.out_proj attn_out
        x = x + residual

        // FFN with residual
        residual = x
        x = layer_norm layer.ffn_norm x
        x = linear layer.ffn.up_proj x
        x = gelu x
        x = linear layer.ffn.down_proj x
        x = x + residual

    // Pool: take [CLS] token embedding
    cls_embedding = x[0]
    pooled = tanh(linear encoder.pooler cls_embedding)

    output: pooled

// Text generation pipeline with KV cache
pipeline generate_text:
    input: prompt string
    config: {
        max_new_tokens: 100,
        temperature: 0.7,
        top_p: 0.9,
        top_k: 50
    }

    // Tokenize prompt
    tokens = tokenize generator.tokenizer prompt
    kv_cache = create_kv_cache generator.num_layers generator.hidden_size

    // Process prompt (prefill phase)
    x = embed generator.token_embeddings tokens
    x = x + embed generator.position_embeddings positions(len(tokens))

    for layer_idx, layer in enumerate(generator.layers):
        residual = x
        x = layer_norm layer.attention_norm x

        q = linear layer.attention.q_proj x
        k = linear layer.attention.k_proj x
        v = linear layer.attention.v_proj x

        // Store K, V in cache
        kv_cache[layer_idx].k = k
        kv_cache[layer_idx].v = v

        attn_out = causal_attention q k v num_heads=12
        x = linear layer.attention.out_proj attn_out
        x = x + residual

        residual = x
        x = layer_norm layer.ffn_norm x
        x = linear layer.ffn.up_proj x
        x = gelu x
        x = linear layer.ffn.down_proj x
        x = x + residual

    // Get logits for last token
    logits = linear generator.lm_head x[-1]

    // Generation loop (decode phase)
    generated = []
    position = len(tokens)

    while len(generated) < config.max_new_tokens:
        // Sample next token
        probs = softmax(logits / config.temperature)
        probs = top_p_filter probs config.top_p
        probs = top_k_filter probs config.top_k
        next_token = sample probs

        if next_token == generator.eos_token:
            break

        append generated next_token

        // Process single token with KV cache
        x = embed generator.token_embeddings [next_token]
        x = x + embed generator.position_embeddings [position]

        for layer_idx, layer in enumerate(generator.layers):
            residual = x
            x = layer_norm layer.attention_norm x

            q = linear layer.attention.q_proj x
            k = linear layer.attention.k_proj x
            v = linear layer.attention.v_proj x

            // Append to KV cache
            append kv_cache[layer_idx].k k
            append kv_cache[layer_idx].v v

            // Attention with full cache
            attn_out = causal_attention q kv_cache[layer_idx].k kv_cache[layer_idx].v num_heads=12
            x = linear layer.attention.out_proj attn_out
            x = x + residual

            residual = x
            x = layer_norm layer.ffn_norm x
            x = linear layer.ffn.up_proj x
            x = gelu x
            x = linear layer.ffn.down_proj x
            x = x + residual

        logits = linear generator.lm_head x[0]
        position = position + 1

    // Decode tokens to text
    output_text = detokenize generator.tokenizer generated

    output: output_text

// Batch embedding for efficiency
pipeline batch_encode:
    input: texts list[string]
    config: {
        batch_size: 32,
        max_length: 512
    }

    embeddings = []

    for batch in chunks(texts, config.batch_size):
        // Tokenize batch with padding
        batch_tokens = tokenize_batch encoder.tokenizer batch max_length=config.max_length
        batch_masks = create_attention_masks batch_tokens

        // Forward pass
        batch_embeddings = forward encoder batch_tokens batch_masks

        // Pool embeddings
        for emb in batch_embeddings:
            append embeddings mean_pool(emb)

    output: embeddings

// Run
run encode_text with "This is a test sentence."
run generate_text with "Once upon a time"
```

## Grammar Specification

```
// transformer.zyn

@name = "transformer"
@version = "1.0"
@file_extensions = [".tinf", ".transformer"]

@builtins {
    // Attention operations (new: zrtl_attention)
    multi_head_attention: "$Attention$multi_head"
    causal_attention: "$Attention$causal"
    flash_attention: "$Attention$flash"
    create_kv_cache: "$Attention$create_kv_cache"
    append_kv: "$Attention$append_kv"

    // Position encoding
    rope_encode: "$Position$rope"
    alibi_bias: "$Position$alibi"

    // Activations
    gelu: "$SIMD$gelu_f32"
    silu: "$Activation$silu"
    softmax: "$SIMD$softmax_f32"

    // Linear algebra
    gemm: "$SIMD$gemm_f32"
    layer_norm: "$SIMD$layer_norm_f32"
    rms_norm: "$Norm$rms_norm"

    // Embedding
    embedding_lookup: "$Embedding$lookup"

    // Sampling
    sample_multinomial: "$Sample$multinomial"
    top_k_filter: "$Sample$top_k"
    top_p_filter: "$Sample$top_p"

    // Tokenization
    tokenize: "$Tokenizer$encode"
    detokenize: "$Tokenizer$decode"
    tokenize_batch: "$Tokenizer$encode_batch"

    // Tensor ops
    tensor_add: "$SIMD$add_f32"
    tensor_mul: "$SIMD$mul_f32"
    transpose: "$SIMD$transpose_2d_f32"

    // IO
    print: "$IO$println"
}

// Grammar rules
program = (load_stmt | architecture_def | pipeline_def | run_stmt)*

// Load transformer model
load_stmt = "load" "transformer" STRING "as" IDENT model_config

model_config = "{" model_option* "}"

model_option = "architecture" ":" IDENT
             | "hidden_size" ":" INTEGER
             | "num_layers" ":" INTEGER
             | "num_heads" ":" INTEGER
             | "vocab_size" ":" INTEGER
             | "max_seq_length" ":" INTEGER
             | "tokenizer" ":" STRING
             | "rope_theta" ":" FLOAT
             | "attention_type" ":" attention_type

attention_type = "bidirectional" | "causal"

// Architecture definition
architecture_def = "architecture" IDENT ":" NEWLINE INDENT
                   layer_def*
                   DEDENT

layer_def = "layer" IDENT ":" layer_spec
          | "repeat" IDENT ":" NEWLINE INDENT layer_def* DEDENT

layer_spec = embedding_spec
           | linear_spec
           | attention_spec
           | ffn_spec
           | norm_spec

embedding_spec = "embedding" INTEGER "->" INTEGER

linear_spec = "linear" INTEGER "->" INTEGER linear_opts?

linear_opts = ("activation" "=" activation)?

attention_spec = NEWLINE INDENT
                 "q_proj" ":" linear_spec
                 "k_proj" ":" linear_spec
                 "v_proj" ":" linear_spec
                 "out_proj" ":" linear_spec
                 "num_heads" ":" INTEGER
                 "attention_type" ":" attention_type
                 DEDENT

ffn_spec = NEWLINE INDENT
           "up_proj" ":" linear_spec
           "down_proj" ":" linear_spec
           "activation" ":" activation
           DEDENT

norm_spec = "layer_norm" INTEGER
          | "rms_norm" INTEGER

activation = "relu" | "gelu" | "silu" | "tanh" | "none"

// Pipeline definition
pipeline_def = "pipeline" IDENT ":" NEWLINE INDENT
               pipeline_input
               pipeline_config?
               pipeline_stmt*
               output_decl
               DEDENT

pipeline_input = "input" ":" type_hint IDENT?

pipeline_config = "config" ":" "{" config_pair* "}"

pipeline_stmt = assign_stmt
              | for_stmt
              | while_stmt
              | if_stmt
              | break_stmt

assign_stmt = IDENT "=" expr
            | "append" IDENT expr

for_stmt = "for" for_vars "in" expr ":" NEWLINE INDENT pipeline_stmt* DEDENT

for_vars = IDENT
         | IDENT "," IDENT

while_stmt = "while" condition ":" NEWLINE INDENT pipeline_stmt* DEDENT

if_stmt = "if" condition ":" NEWLINE INDENT pipeline_stmt* DEDENT

break_stmt = "break"

output_decl = "output" ":" expr

// Expressions
expr = binary_expr
     | unary_expr
     | call_expr
     | index_expr
     | member_expr
     | IDENT
     | literal
     | "[" (expr ("," expr)*)? "]"
     | "(" expr ")"

binary_expr = expr binary_op expr

binary_op = "+" | "-" | "*" | "/" | "==" | "!=" | "<" | ">"

unary_expr = "-" expr

call_expr = func_name "(" (call_arg ("," call_arg)*)? ")"
          | "embed" IDENT expr
          | "linear" IDENT expr
          | "layer_norm" IDENT expr
          | "tokenize" IDENT expr
          | "detokenize" IDENT expr
          | func_name expr (IDENT "=" expr)*

func_name = IDENT

call_arg = expr | IDENT "=" expr

index_expr = expr "[" (expr | slice) "]"

slice = expr? ":" expr?

member_expr = expr "." IDENT

condition = expr comparator expr
          | expr

comparator = "==" | "!=" | "<" | ">" | "<=" | ">="

// Run statement
run_stmt = "run" IDENT "with" expr

// Types
type_hint = "text" | "string" | "tensor" shape? | "list" "[" type_hint "]"

shape = "[" INTEGER ("," INTEGER)* "]"

// Literals
literal = STRING | INTEGER | FLOAT | BOOL

config_pair = IDENT ":" literal

// Tokens
IDENT = /[a-zA-Z_][a-zA-Z0-9_]*/
STRING = /"[^"]*"/
INTEGER = /-?[0-9]+/
FLOAT = /-?[0-9]+\.[0-9]+/
BOOL = "true" | "false"
```

## Required New Operations

### Plugin: `zrtl_attention`

```rust
//! Attention mechanisms for transformers

/// Multi-head attention
/// Q, K, V: [batch, seq_len, hidden_size]
/// Output: [batch, seq_len, hidden_size]
#[no_mangle]
pub extern "C" fn multi_head_attention(
    q: *const f32,
    k: *const f32,
    v: *const f32,
    output: *mut f32,
    attention_mask: *const f32,  // [batch, 1, 1, seq_len] or null
    batch: u64,
    seq_len: u64,
    hidden_size: u64,
    num_heads: u64
);

/// Causal (masked) self-attention for autoregressive generation
#[no_mangle]
pub extern "C" fn causal_attention(
    q: *const f32,
    k: *const f32,
    v: *const f32,
    output: *mut f32,
    batch: u64,
    q_len: u64,      // Query sequence length (1 for generation)
    kv_len: u64,     // Key/Value sequence length (includes cache)
    hidden_size: u64,
    num_heads: u64
);

/// Flash Attention (memory-efficient)
/// Computes attention in tiles to reduce memory usage
#[no_mangle]
pub extern "C" fn flash_attention(
    q: *const f32,
    k: *const f32,
    v: *const f32,
    output: *mut f32,
    batch: u64,
    seq_len: u64,
    hidden_size: u64,
    num_heads: u64,
    is_causal: bool,
    block_size: u64  // Tile size (e.g., 64 or 128)
);

/// KV-Cache for autoregressive generation
pub struct KVCache {
    k: Vec<f32>,  // [num_layers, max_seq_len, hidden_size]
    v: Vec<f32>,
    seq_len: usize,
    max_seq_len: usize,
    hidden_size: usize,
    num_layers: usize,
}

/// Create KV cache
#[no_mangle]
pub extern "C" fn create_kv_cache(
    num_layers: u64,
    max_seq_len: u64,
    hidden_size: u64
) -> *mut KVCache;

/// Append K, V vectors to cache for a specific layer
#[no_mangle]
pub extern "C" fn append_kv(
    cache: *mut KVCache,
    layer: u64,
    k: *const f32,  // [1, hidden_size] for single token
    v: *const f32,
    num_tokens: u64
);

/// Get K, V from cache for attention
#[no_mangle]
pub extern "C" fn get_kv(
    cache: *const KVCache,
    layer: u64,
    k_out: *mut *const f32,
    v_out: *mut *const f32,
    seq_len_out: *mut u64
);

/// Free KV cache
#[no_mangle]
pub extern "C" fn free_kv_cache(cache: *mut KVCache);
```

### Plugin: `zrtl_position`

```rust
//! Positional encoding for transformers

/// Apply Rotary Position Embedding (RoPE)
/// Modifies Q and K in-place
#[no_mangle]
pub extern "C" fn rope_encode(
    q: *mut f32,
    k: *mut f32,
    positions: *const u32,  // Position indices
    batch: u64,
    seq_len: u64,
    head_dim: u64,
    num_heads: u64,
    theta: f32  // Base for rotation (typically 10000.0)
);

/// Compute ALiBi (Attention with Linear Biases) bias matrix
/// Returns: [num_heads, seq_len, seq_len]
#[no_mangle]
pub extern "C" fn alibi_bias(
    output: *mut f32,
    seq_len: u64,
    num_heads: u64
);

/// Sinusoidal position embedding lookup
#[no_mangle]
pub extern "C" fn sinusoidal_position_embedding(
    positions: *const u32,
    output: *mut f32,
    seq_len: u64,
    hidden_size: u64,
    max_seq_len: u64
);
```

### Plugin: `zrtl_activation` (Extended)

```rust
//! Additional activation functions

/// SiLU (Swish): x * sigmoid(x)
#[no_mangle]
pub extern "C" fn silu_f32(data: *mut f32, len: u64);

/// GELU (Gaussian Error Linear Unit)
/// Approximation: 0.5 * x * (1 + tanh(sqrt(2/pi) * (x + 0.044715 * x^3)))
#[no_mangle]
pub extern "C" fn gelu_f32(data: *mut f32, len: u64);

/// GELU exact (slower but more accurate)
#[no_mangle]
pub extern "C" fn gelu_exact_f32(data: *mut f32, len: u64);

/// GLU (Gated Linear Unit): a * sigmoid(b)
/// Input is split in half: first half is a, second half is b
#[no_mangle]
pub extern "C" fn glu_f32(
    input: *const f32,
    output: *mut f32,
    len: u64  // Input length (output is len/2)
);

/// SwiGLU: a * silu(b)
#[no_mangle]
pub extern "C" fn swiglu_f32(
    input: *const f32,
    output: *mut f32,
    len: u64
);
```

### Plugin: `zrtl_norm`

```rust
//! Normalization layers

/// RMS Normalization (used in LLaMA, etc.)
/// out = x / sqrt(mean(x^2) + eps) * weight
#[no_mangle]
pub extern "C" fn rms_norm(
    data: *mut f32,
    weight: *const f32,
    batch_size: u64,
    hidden_size: u64,
    eps: f32
);
```

### Plugin: `zrtl_sample`

```rust
//! Sampling for text generation

/// Multinomial sampling from probability distribution
#[no_mangle]
pub extern "C" fn sample_multinomial(
    probs: *const f32,
    vocab_size: u64
) -> u32;  // Returns sampled token index

/// Top-k filtering: keep only top k probabilities
#[no_mangle]
pub extern "C" fn top_k_filter(
    logits: *mut f32,
    vocab_size: u64,
    k: u64
);

/// Top-p (nucleus) filtering: keep tokens with cumulative prob < p
#[no_mangle]
pub extern "C" fn top_p_filter(
    logits: *mut f32,
    vocab_size: u64,
    p: f32
);

/// Repetition penalty
#[no_mangle]
pub extern "C" fn repetition_penalty(
    logits: *mut f32,
    vocab_size: u64,
    generated_tokens: *const u32,
    num_generated: u64,
    penalty: f32
);

/// Temperature scaling
#[no_mangle]
pub extern "C" fn temperature_scale(
    logits: *mut f32,
    vocab_size: u64,
    temperature: f32
);
```

### Plugin: `zrtl_tokenizer`

```rust
//! Tokenization for transformer models

/// Tokenizer handle (wraps tokenizers library or custom impl)
pub struct TokenizerHandle;

/// Load tokenizer from JSON file
#[no_mangle]
pub extern "C" fn tokenizer_load(
    path: *const u8,
    path_len: u32
) -> *mut TokenizerHandle;

/// Encode text to token IDs
/// Returns number of tokens
#[no_mangle]
pub extern "C" fn tokenizer_encode(
    tokenizer: *const TokenizerHandle,
    text: *const u8,
    text_len: u32,
    output: *mut u32,
    max_tokens: u64,
    add_special_tokens: bool
) -> u64;

/// Decode token IDs to text
/// Returns length of decoded string
#[no_mangle]
pub extern "C" fn tokenizer_decode(
    tokenizer: *const TokenizerHandle,
    tokens: *const u32,
    num_tokens: u64,
    output: *mut u8,
    max_len: u64,
    skip_special_tokens: bool
) -> u64;

/// Batch encode with padding
#[no_mangle]
pub extern "C" fn tokenizer_encode_batch(
    tokenizer: *const TokenizerHandle,
    texts: *const *const u8,
    text_lens: *const u32,
    num_texts: u64,
    output: *mut u32,          // [num_texts, max_len]
    attention_mask: *mut u8,   // [num_texts, max_len]
    max_len: u64,
    padding: bool,
    truncation: bool
) -> u64;  // Returns actual max length used

/// Get special token IDs
#[no_mangle]
pub extern "C" fn tokenizer_bos_id(tokenizer: *const TokenizerHandle) -> u32;

#[no_mangle]
pub extern "C" fn tokenizer_eos_id(tokenizer: *const TokenizerHandle) -> u32;

#[no_mangle]
pub extern "C" fn tokenizer_pad_id(tokenizer: *const TokenizerHandle) -> u32;

/// Free tokenizer
#[no_mangle]
pub extern "C" fn tokenizer_free(tokenizer: *mut TokenizerHandle);
```

## Dependencies

### Rust Crates

```toml
[dependencies]
# Core
zrtl = { path = "../sdk/zrtl" }
wide = "0.7"

# Model loading
safetensors = "0.4"
memmap2 = "0.9"

# Tokenization
tokenizers = "0.19"  # Hugging Face tokenizers

# Random sampling
rand = "0.8"
rand_distr = "0.4"

# Serialization
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"

# Optional: faster attention
[features]
flash-attention = []  # Enable flash attention implementation

[dev-dependencies]
criterion = "0.5"
approx = "0.5"
```

## File Structure

```
examples/transformer/
├── Cargo.toml
├── transformer.zyn                 # Grammar definition
├── src/
│   ├── main.rs                     # CLI entry point
│   ├── model.rs                    # Model loading and structure
│   ├── executor.rs                 # Inference executor
│   ├── kv_cache.rs                 # KV cache management
│   └── generation.rs               # Text generation logic
├── models/
│   ├── distilbert/
│   │   ├── model.safetensors
│   │   └── tokenizer.json
│   └── gpt2-small/
│       ├── model.safetensors
│       └── tokenizer.json
├── samples/
│   ├── encode_text.tinf            # Text embedding
│   ├── classify_text.tinf          # Classification
│   └── generate_text.tinf          # Text generation
└── test_data/
    └── test_texts.txt

plugins/zrtl_attention/
├── Cargo.toml
├── src/
│   ├── lib.rs
│   ├── multi_head.rs
│   ├── causal.rs
│   ├── flash.rs
│   └── kv_cache.rs
├── benches/
│   └── attention_bench.rs
└── tests/
    └── attention_tests.rs

plugins/zrtl_position/
├── Cargo.toml
├── src/
│   ├── lib.rs
│   ├── rope.rs
│   ├── alibi.rs
│   └── sinusoidal.rs

plugins/zrtl_sample/
├── Cargo.toml
├── src/
│   ├── lib.rs
│   ├── multinomial.rs
│   └── filtering.rs

plugins/zrtl_tokenizer/
├── Cargo.toml
├── src/
│   ├── lib.rs
│   └── wrapper.rs              # Wraps tokenizers crate
```

## Implementation Plan

### Phase 1: Attention Mechanisms (Week 1-2)
- [ ] Implement basic multi-head attention
- [ ] Implement causal attention
- [ ] KV cache data structure
- [ ] SIMD optimization for attention
- [ ] Unit tests

### Phase 2: Position Encodings (Week 2)
- [ ] Sinusoidal embeddings
- [ ] RoPE implementation
- [ ] ALiBi bias computation

### Phase 3: Activations and Norms (Week 2-3)
- [ ] GELU implementation
- [ ] SiLU/SwiGLU
- [ ] RMSNorm
- [ ] Layer norm (already exists, verify)

### Phase 4: Tokenization (Week 3)
- [ ] Integrate tokenizers crate
- [ ] Encode/decode wrappers
- [ ] Batch encoding with padding

### Phase 5: Sampling (Week 3-4)
- [ ] Temperature scaling
- [ ] Top-k filtering
- [ ] Top-p filtering
- [ ] Multinomial sampling
- [ ] Repetition penalty

### Phase 6: Grammar and Runtime (Week 4-5)
- [ ] Write grammar
- [ ] Implement semantic actions
- [ ] Model loading
- [ ] Forward pass orchestration

### Phase 7: Optimization (Week 5-6)
- [ ] Flash attention (optional)
- [ ] Batch inference
- [ ] Memory optimization
- [ ] Benchmarking

## Performance Targets

### BERT-base Encoding (seq_len=128)

| Metric | Target | PyTorch CPU |
|--------|--------|-------------|
| Latency | 25ms | 35ms |
| Throughput | 40 seq/s | 28 seq/s |
| Memory | 500MB | 600MB |

### GPT-2 Small Generation

| Metric | Target | Notes |
|--------|--------|-------|
| Prefill (128 tokens) | 30ms | Initial prompt processing |
| Decode (per token) | 15ms | With KV cache |
| Tokens/sec | 66 | Generation speed |
| Memory (1K context) | 600MB | Including KV cache |

## Example Outputs

### Text Embedding

```bash
$ transformer run encode_text --input "This is a test sentence."

Loading model: distilbert-base.safetensors
  - Architecture: BERT
  - Hidden size: 768
  - Layers: 6
  - Heads: 12

Tokenizing: "This is a test sentence."
  Tokens: [101, 2023, 2003, 1037, 3231, 6251, 1012, 102]
  Length: 8

Forward pass...
  - Embedding: 0.3ms
  - Attention layers: 18.2ms
  - Pooling: 0.1ms

Output embedding: [768] float32
  Sample values: [0.234, -0.567, 0.891, ...]

Total time: 18.6ms
```

### Text Generation

```bash
$ transformer run generate_text --prompt "Once upon a time" \
    --max-tokens 50 --temperature 0.8

Loading model: gpt2-small.safetensors
  - Architecture: GPT-2
  - Hidden size: 768
  - Layers: 12
  - Heads: 12

Prompt: "Once upon a time"
  Tokens: [7454, 2402, 257, 640]

Generating (max 50 tokens, temp=0.8)...

Prefill: 28.3ms
Generation:
  [1] " there" (12.1ms)
  [2] " was" (11.8ms)
  [3] " a" (11.9ms)
  ...
  [47] "." (12.0ms)

Output:
"Once upon a time there was a young princess who lived in a castle
high up in the mountains. She spent her days reading books and
dreaming of adventure beyond the castle walls."

Stats:
  - Tokens generated: 47
  - Total time: 592ms
  - Tokens/sec: 79.4
  - Memory peak: 512MB
```

## Testing Strategy

### Unit Tests
- Attention output correctness
- Position encoding values
- Sampling distribution

### Accuracy Tests
- Compare with PyTorch/Transformers outputs
- Perplexity measurement
- Task accuracy (classification, QA)

### Performance Tests
- Latency benchmarks
- Throughput measurement
- Memory profiling

### Generation Tests
- Coherence evaluation
- Repetition detection
- Edge cases (empty input, max length)

## Future Enhancements

1. **Flash Attention**
   - Memory-efficient attention
   - Longer context support

2. **Quantization**
   - INT8 attention
   - Mixed precision

3. **Batched Generation**
   - Multiple sequences in parallel
   - Dynamic batching

4. **More Architectures**
   - LLaMA/Mistral
   - T5 (encoder-decoder)
   - Vision transformers

5. **Speculative Decoding**
   - Draft model acceleration
   - Parallel verification
