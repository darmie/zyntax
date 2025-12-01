# ML Inference DSL Plans

This directory contains comprehensive planning documents for domain-specific languages targeting ML inference workloads on CPU with SIMD acceleration.

## Overview

Each DSL leverages the ZRTL plugin system and ZynPEG grammar framework to provide:
- **Declarative syntax** for common ML patterns
- **SIMD-accelerated operations** via `zrtl_simd` plugin
- **JIT compilation** through Cranelift backend
- **Plugin extensibility** for domain-specific operations

## DSL Plans

| DSL | Primary Use Case | Complexity | Priority |
|-----|------------------|------------|----------|
| [Vector Search](01-vector-search-dsl.md) | RAG, Semantic Search, Recommendations | Medium | **High** |
| [Classification Pipeline](02-classification-dsl.md) | Image/Text Classification on Edge | High | High |
| [Anomaly Detection](03-anomaly-detection-dsl.md) | IoT, Industrial Monitoring | Medium | Medium |
| [Quantized Inference](04-quantized-inference-dsl.md) | INT8 Optimized Models | High | Medium |
| [Feature Engineering](05-feature-engineering-dsl.md) | Real-time Feature Computation | Medium | Medium |
| [Transformer Inference](06-transformer-inference-dsl.md) | BERT, GPT-style Models | Very High | Low |
| [Audio Processing](07-audio-processing-dsl.md) | Speech Recognition, Audio ML | High | Low |

## Implementation Order

Recommended implementation sequence based on:
1. Value delivered vs effort required
2. Reusability of components across DSLs
3. Existing SIMD operation coverage

```
Phase 1: Vector Search DSL
    └── Establishes: tensor loading, similarity ops, result ranking

Phase 2: Classification Pipeline DSL
    └── Adds: CNN ops, pooling, activation functions
    └── Reuses: tensor loading, softmax, argmax

Phase 3: Anomaly Detection DSL
    └── Adds: streaming, windowing, statistical ops
    └── Reuses: normalization, distance metrics

Phase 4: Feature Engineering DSL
    └── Adds: aggregations, time windows, materialization
    └── Reuses: vector ops, normalization

Phase 5: Quantized Inference DSL
    └── Adds: INT8 ops, calibration, mixed precision
    └── Reuses: all inference ops

Phase 6: Transformer Inference DSL
    └── Adds: attention, positional encoding, KV cache
    └── Reuses: GEMM, layer norm, softmax

Phase 7: Audio Processing DSL
    └── Adds: FFT, mel filterbank, spectrogram
    └── Reuses: conv1d, transformer blocks
```

## Shared Infrastructure

Components that benefit multiple DSLs:

### Tensor Runtime (`zrtl_tensor`)
- Memory management (aligned allocations)
- Tensor metadata (shape, stride, dtype)
- View/slice operations
- Serialization (NPY, safetensors)

### Model Loading (`zrtl_model`)
- ONNX parser (subset)
- Safetensors loader
- Weight quantization/dequantization
- Model optimization passes

### IO Operations (`zrtl_io`)
- File streaming
- Memory-mapped files
- Network sources (HTTP, MQTT)
- Result serialization

## SIMD Operation Coverage

Current `zrtl_simd` operations and their DSL usage:

| Operation | VecSearch | Classify | Anomaly | Quantized | Features | Transformer | Audio |
|-----------|:---------:|:--------:|:-------:|:---------:|:--------:|:-----------:|:-----:|
| dot_product_f32 | ✓ | ✓ | ✓ | | ✓ | ✓ | |
| gemm_f32 | | ✓ | | ✓ | | ✓ | ✓ |
| conv2d_f32 | | ✓ | | ✓ | | | ✓ |
| softmax_f32 | ✓ | ✓ | | ✓ | | ✓ | |
| layer_norm_f32 | | | ✓ | ✓ | | ✓ | ✓ |
| relu_f32 | | ✓ | | ✓ | | ✓ | |
| argmax_f32 | ✓ | ✓ | | ✓ | | ✓ | |
| max_pool2d_f32 | | ✓ | | ✓ | | | |
| sum_f32 | ✓ | | ✓ | | ✓ | | |
| scale_f32 | ✓ | | ✓ | | ✓ | | |
| transpose_2d_f32 | | | | | | ✓ | |

## New Operations Needed

Operations to be added to `zrtl_simd` or new plugins:

### High Priority (Phase 1-2)
- `cosine_similarity_f32` - Normalized dot product
- `topk_f32` - Find top-k values and indices
- `l2_normalize_f32` - L2 normalization
- `euclidean_distance_f32` - L2 distance

### Medium Priority (Phase 3-4)
- `running_mean_f32` - Streaming mean
- `running_var_f32` - Streaming variance
- `quantize_f32_i8` - Float to INT8
- `dequantize_i8_f32` - INT8 to float
- `gemm_i8` - INT8 matrix multiply

### Lower Priority (Phase 5-7)
- `gelu_f32` - GELU activation
- `rope_f32` - Rotary position embedding
- `fft_f32` - Fast Fourier Transform
- `mel_filterbank` - Mel spectrogram
- `attention_f32` - Fused attention kernel
