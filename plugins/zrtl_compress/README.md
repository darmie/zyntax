# zrtl_compress

Compression and decompression for Zyntax-based languages.

## Overview

Provides gzip, zlib, and deflate compression/decompression. Works directly with byte arrays for maximum efficiency.

## Exported Symbols

### Gzip (Most Common)

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Compress$gzip` | `(ArrayPtr) -> ArrayPtr` | Compress with gzip (default level 6) |
| `$Compress$gunzip` | `(ArrayPtr) -> ArrayPtr` | Decompress gzip |
| `$Compress$gzip_level` | `(ArrayPtr, u32) -> ArrayPtr` | Compress with level (0-9) |

### Zlib

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Compress$zlib` | `(ArrayPtr) -> ArrayPtr` | Compress with zlib |
| `$Compress$zlib_decompress` | `(ArrayPtr) -> ArrayPtr` | Decompress zlib |
| `$Compress$zlib_level` | `(ArrayPtr, u32) -> ArrayPtr` | Compress with level (0-9) |

### Deflate (Raw)

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Compress$deflate` | `(ArrayPtr) -> ArrayPtr` | Compress with deflate |
| `$Compress$inflate` | `(ArrayPtr) -> ArrayPtr` | Decompress deflate |
| `$Compress$deflate_level` | `(ArrayPtr, u32) -> ArrayPtr` | Compress with level (0-9) |

### Utilities

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Compress$is_gzip` | `(ArrayPtr) -> i32` | Check if data is gzip (magic bytes) |
| `$Compress$is_zlib` | `(ArrayPtr) -> i32` | Check if data is zlib format |
| `$Compress$gzip_size` | `(ArrayPtr) -> i64` | Get uncompressed size from gzip trailer |

## Usage Example

```zig
// Create some data
const data = "Hello, World! ".repeat(100);
const bytes = string_to_bytes(data);  // Convert to byte array

// Compress with gzip
const compressed = $Compress$gzip(bytes);
// compressed is much smaller than original

// Decompress
const decompressed = $Compress$gunzip(compressed);
// decompressed == original bytes

// Use higher compression
const max_compressed = $Compress$gzip_level(bytes, 9);

// Use faster compression
const fast_compressed = $Compress$gzip_level(bytes, 1);

// Check format before decompressing
if ($Compress$is_gzip(unknown_data) == 1) {
    const result = $Compress$gunzip(unknown_data);
} else if ($Compress$is_zlib(unknown_data) == 1) {
    const result = $Compress$zlib_decompress(unknown_data);
}

// Get expected size before decompressing
const expected_size = $Compress$gzip_size(compressed);

// Cleanup
array_free(bytes);
array_free(compressed);
array_free(decompressed);
```

## Compression Levels

| Level | Description | Use Case |
|-------|-------------|----------|
| 0 | No compression | Testing, already compressed data |
| 1 | Fastest | Real-time streaming |
| 6 | Default | Best balance |
| 9 | Best ratio | Archives, storage |

## Format Comparison

| Format | Header | Use Case |
|--------|--------|----------|
| **Gzip** | Yes (10+ bytes) | Files, HTTP `Content-Encoding: gzip` |
| **Zlib** | Yes (2 bytes) | Network protocols, PNG |
| **Deflate** | No | Raw compression, custom protocols |

## Detection

- **Gzip**: Magic bytes `1f 8b` at start
- **Zlib**: First byte typically `78` (CMF byte with deflate method)

## Return Values

- All compression functions return null on error
- Decompression of invalid data returns null

## Memory Management

All functions returning `ArrayPtr` allocate memory. Caller must free with `array_free`.

## Dependencies

- `zrtl` - Core ZRTL SDK
- `flate2` - Compression algorithms
