//! ZRTL Compress Plugin
//!
//! Provides compression and decompression functionality using gzip, zlib, and deflate.
//! Works directly with byte arrays for maximum efficiency - no base64 encoding overhead.
//!
//! ## Gzip (most common for files/HTTP)
//! - `$Compress$gzip` - Compress bytes with gzip
//! - `$Compress$gunzip` - Decompress gzip bytes
//!
//! ## Zlib (common for network protocols)
//! - `$Compress$zlib` - Compress bytes with zlib
//! - `$Compress$zlib_decompress` - Decompress zlib bytes
//!
//! ## Deflate (raw compression)
//! - `$Compress$deflate` - Compress bytes with deflate
//! - `$Compress$inflate` - Decompress deflate bytes

use std::io::{Read, Write};
use flate2::read::{GzDecoder, ZlibDecoder, DeflateDecoder};
use flate2::write::{GzEncoder, ZlibEncoder, DeflateEncoder};
use flate2::Compression;
use zrtl::{zrtl_plugin, ArrayPtr, array_new, array_as_slice, array_push, array_length};

// ============================================================================
// Helper: Array creation from Vec<u8>
// ============================================================================

/// Creates an ArrayPtr from compressed output
/// Uses array_push for safety with the zrtl API, but with pre-allocated capacity
#[inline]
fn vec_to_array(data: Vec<u8>) -> ArrayPtr {
    if data.is_empty() {
        return array_new::<u8>(0);
    }

    // Pre-allocate with exact capacity to avoid reallocations
    let mut arr = array_new::<u8>(data.len());
    if arr.is_null() {
        return std::ptr::null_mut();
    }

    // Push each byte - with pre-allocated capacity this won't reallocate
    for &byte in &data {
        arr = unsafe { array_push(arr, byte) };
        if arr.is_null() {
            return std::ptr::null_mut();
        }
    }
    arr
}

/// Gets a slice view of array data - zero-copy
#[inline]
fn array_as_bytes(arr: ArrayPtr) -> &'static [u8] {
    if arr.is_null() {
        return &[];
    }
    unsafe { array_as_slice::<u8>(arr) }
}

// ============================================================================
// Gzip Compression (most common)
// ============================================================================

/// Compress bytes with gzip (default compression level 6)
#[no_mangle]
pub extern "C" fn compress_gzip(data: ArrayPtr) -> ArrayPtr {
    let bytes = array_as_bytes(data);

    let mut encoder = GzEncoder::new(Vec::with_capacity(bytes.len()), Compression::default());
    if encoder.write_all(bytes).is_err() {
        return std::ptr::null_mut();
    }

    match encoder.finish() {
        Ok(compressed) => vec_to_array(compressed),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Decompress gzip bytes
#[no_mangle]
pub extern "C" fn compress_gunzip(data: ArrayPtr) -> ArrayPtr {
    let bytes = array_as_bytes(data);
    if bytes.is_empty() {
        return array_new::<u8>(0);
    }

    let mut decoder = GzDecoder::new(bytes);
    let mut decompressed = Vec::with_capacity(bytes.len() * 2); // Estimate 2x expansion

    match decoder.read_to_end(&mut decompressed) {
        Ok(_) => vec_to_array(decompressed),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Compress with gzip at specified level (0=none, 1=fast, 6=default, 9=best)
#[no_mangle]
pub extern "C" fn compress_gzip_level(data: ArrayPtr, level: u32) -> ArrayPtr {
    let bytes = array_as_bytes(data);

    let compression = Compression::new(level.min(9));
    let mut encoder = GzEncoder::new(Vec::with_capacity(bytes.len()), compression);
    if encoder.write_all(bytes).is_err() {
        return std::ptr::null_mut();
    }

    match encoder.finish() {
        Ok(compressed) => vec_to_array(compressed),
        Err(_) => std::ptr::null_mut(),
    }
}

// ============================================================================
// Zlib Compression
// ============================================================================

/// Compress bytes with zlib
#[no_mangle]
pub extern "C" fn compress_zlib(data: ArrayPtr) -> ArrayPtr {
    let bytes = array_as_bytes(data);

    let mut encoder = ZlibEncoder::new(Vec::with_capacity(bytes.len()), Compression::default());
    if encoder.write_all(bytes).is_err() {
        return std::ptr::null_mut();
    }

    match encoder.finish() {
        Ok(compressed) => vec_to_array(compressed),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Decompress zlib bytes
#[no_mangle]
pub extern "C" fn compress_zlib_decompress(data: ArrayPtr) -> ArrayPtr {
    let bytes = array_as_bytes(data);
    if bytes.is_empty() {
        return array_new::<u8>(0);
    }

    let mut decoder = ZlibDecoder::new(bytes);
    let mut decompressed = Vec::with_capacity(bytes.len() * 2);

    match decoder.read_to_end(&mut decompressed) {
        Ok(_) => vec_to_array(decompressed),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Compress with zlib at specified level (0-9)
#[no_mangle]
pub extern "C" fn compress_zlib_level(data: ArrayPtr, level: u32) -> ArrayPtr {
    let bytes = array_as_bytes(data);

    let compression = Compression::new(level.min(9));
    let mut encoder = ZlibEncoder::new(Vec::with_capacity(bytes.len()), compression);
    if encoder.write_all(bytes).is_err() {
        return std::ptr::null_mut();
    }

    match encoder.finish() {
        Ok(compressed) => vec_to_array(compressed),
        Err(_) => std::ptr::null_mut(),
    }
}

// ============================================================================
// Deflate (raw) Compression
// ============================================================================

/// Compress bytes with deflate
#[no_mangle]
pub extern "C" fn compress_deflate(data: ArrayPtr) -> ArrayPtr {
    let bytes = array_as_bytes(data);

    let mut encoder = DeflateEncoder::new(Vec::with_capacity(bytes.len()), Compression::default());
    if encoder.write_all(bytes).is_err() {
        return std::ptr::null_mut();
    }

    match encoder.finish() {
        Ok(compressed) => vec_to_array(compressed),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Decompress deflate bytes
#[no_mangle]
pub extern "C" fn compress_inflate(data: ArrayPtr) -> ArrayPtr {
    let bytes = array_as_bytes(data);
    if bytes.is_empty() {
        return array_new::<u8>(0);
    }

    let mut decoder = DeflateDecoder::new(bytes);
    let mut decompressed = Vec::with_capacity(bytes.len() * 2);

    match decoder.read_to_end(&mut decompressed) {
        Ok(_) => vec_to_array(decompressed),
        Err(_) => std::ptr::null_mut(),
    }
}

/// Compress with deflate at specified level (0-9)
#[no_mangle]
pub extern "C" fn compress_deflate_level(data: ArrayPtr, level: u32) -> ArrayPtr {
    let bytes = array_as_bytes(data);

    let compression = Compression::new(level.min(9));
    let mut encoder = DeflateEncoder::new(Vec::with_capacity(bytes.len()), compression);
    if encoder.write_all(bytes).is_err() {
        return std::ptr::null_mut();
    }

    match encoder.finish() {
        Ok(compressed) => vec_to_array(compressed),
        Err(_) => std::ptr::null_mut(),
    }
}

// ============================================================================
// Utility Functions
// ============================================================================

/// Check if data appears to be gzip compressed (checks magic bytes)
#[no_mangle]
pub extern "C" fn compress_is_gzip(data: ArrayPtr) -> i32 {
    let bytes = array_as_bytes(data);
    // Gzip magic number: 1f 8b
    (bytes.len() >= 2 && bytes[0] == 0x1f && bytes[1] == 0x8b) as i32
}

/// Check if data appears to be zlib compressed
#[no_mangle]
pub extern "C" fn compress_is_zlib(data: ArrayPtr) -> i32 {
    let bytes = array_as_bytes(data);
    // Zlib header: CMF byte where (CMF & 0x0F) == 8 (deflate) and typically CMF == 0x78
    (bytes.len() >= 2 && bytes[0] == 0x78) as i32
}

/// Get uncompressed size estimate for gzip data (reads trailer)
#[no_mangle]
pub extern "C" fn compress_gzip_size(data: ArrayPtr) -> i64 {
    let bytes = array_as_bytes(data);
    if bytes.len() < 4 {
        return -1;
    }

    // Last 4 bytes of gzip contain uncompressed size (mod 2^32)
    let size_bytes = &bytes[bytes.len() - 4..];
    i64::from(u32::from_le_bytes([size_bytes[0], size_bytes[1], size_bytes[2], size_bytes[3]]))
}

// ============================================================================
// Plugin Export
// ============================================================================

zrtl_plugin! {
    name: "zrtl_compress",
    symbols: [
        // Gzip
        ("$Compress$gzip", compress_gzip),
        ("$Compress$gunzip", compress_gunzip),
        ("$Compress$gzip_level", compress_gzip_level),

        // Zlib
        ("$Compress$zlib", compress_zlib),
        ("$Compress$zlib_decompress", compress_zlib_decompress),
        ("$Compress$zlib_level", compress_zlib_level),

        // Deflate
        ("$Compress$deflate", compress_deflate),
        ("$Compress$inflate", compress_inflate),
        ("$Compress$deflate_level", compress_deflate_level),

        // Utilities
        ("$Compress$is_gzip", compress_is_gzip),
        ("$Compress$is_zlib", compress_is_zlib),
        ("$Compress$gzip_size", compress_gzip_size),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_test_array(data: &[u8]) -> ArrayPtr {
        vec_to_array(data.to_vec())
    }

    #[test]
    fn test_gzip_roundtrip() {
        let original = b"Hello, World! This is a test of gzip compression.";
        let input = make_test_array(original);

        let compressed = compress_gzip(input);
        assert!(!compressed.is_null());

        let decompressed = compress_gunzip(compressed);
        assert!(!decompressed.is_null());

        let result = array_as_bytes(decompressed);
        assert_eq!(result, original);
    }

    #[test]
    fn test_zlib_roundtrip() {
        let original = b"Zlib compression test data here.";
        let input = make_test_array(original);

        let compressed = compress_zlib(input);
        assert!(!compressed.is_null());

        let decompressed = compress_zlib_decompress(compressed);
        assert!(!decompressed.is_null());

        let result = array_as_bytes(decompressed);
        assert_eq!(result, original);
    }

    #[test]
    fn test_deflate_roundtrip() {
        let original = b"Deflate raw compression test.";
        let input = make_test_array(original);

        let compressed = compress_deflate(input);
        assert!(!compressed.is_null());

        let decompressed = compress_inflate(compressed);
        assert!(!decompressed.is_null());

        let result = array_as_bytes(decompressed);
        assert_eq!(result, original);
    }

    #[test]
    fn test_gzip_levels() {
        let original = b"Test compression at different levels - repeated data AAAAAAAAAA";

        // Level 1 (fastest)
        let input1 = make_test_array(original);
        let fast = compress_gzip_level(input1, 1);
        assert!(!fast.is_null());
        let fast_result = compress_gunzip(fast);
        assert_eq!(array_as_bytes(fast_result), original);

        // Level 9 (best compression)
        let input2 = make_test_array(original);
        let best = compress_gzip_level(input2, 9);
        assert!(!best.is_null());
        let best_result = compress_gunzip(best);
        assert_eq!(array_as_bytes(best_result), original);
    }

    #[test]
    fn test_empty_data() {
        let input = make_test_array(b"");
        let compressed = compress_gzip(input);
        let decompressed = compress_gunzip(compressed);
        assert_eq!(array_as_bytes(decompressed), b"");
    }

    #[test]
    fn test_is_gzip() {
        let original = b"Test data for gzip detection";
        let input = make_test_array(original);
        let compressed = compress_gzip(input);
        assert_eq!(compress_is_gzip(compressed), 1);

        let not_gzip = make_test_array(b"Hello World - not compressed");
        assert_eq!(compress_is_gzip(not_gzip), 0);
    }

    #[test]
    fn test_is_zlib() {
        let original = b"Test data for zlib detection";
        let input = make_test_array(original);
        let compressed = compress_zlib(input);
        assert_eq!(compress_is_zlib(compressed), 1);
    }

    #[test]
    fn test_binary_data() {
        // Test with all possible byte values
        let original: Vec<u8> = (0u8..=255).collect();
        let input = make_test_array(&original);

        let compressed = compress_gzip(input);
        assert!(!compressed.is_null());

        let decompressed = compress_gunzip(compressed);
        assert!(!decompressed.is_null());

        let result = array_as_bytes(decompressed);
        assert_eq!(result, original.as_slice());
    }

    #[test]
    fn test_large_data() {
        // Test with 1MB of data
        let original: Vec<u8> = (0..1024 * 1024).map(|i| (i % 256) as u8).collect();
        let input = make_test_array(&original);

        let compressed = compress_gzip(input);
        assert!(!compressed.is_null());

        // Should actually compress well since it's repetitive
        let compressed_len = unsafe { array_length(compressed) } as usize;
        assert!(compressed_len < original.len());

        let decompressed = compress_gunzip(compressed);
        assert!(!decompressed.is_null());

        let result = array_as_bytes(decompressed);
        assert_eq!(result, original.as_slice());
    }

    #[test]
    fn test_gzip_size() {
        let original = b"Test data for size estimation - make it long enough to test properly";
        let input = make_test_array(original);
        let compressed = compress_gzip(input);

        let size = compress_gzip_size(compressed);
        assert_eq!(size as usize, original.len());
    }
}
