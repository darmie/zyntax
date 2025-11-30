//! ZRTL Crypto Plugin
//!
//! Provides cryptographic hashing, encoding, and UUID generation.
//!
//! ## Hashing
//! - `$Crypto$sha256`, `$Crypto$sha512` - SHA-2 family
//! - `$Crypto$md5` - MD5 (not for security, use for checksums)
//!
//! ## Encoding
//! - `$Crypto$base64_encode`, `$Crypto$base64_decode` - Base64
//! - `$Crypto$hex_encode`, `$Crypto$hex_decode` - Hexadecimal
//!
//! ## UUID
//! - `$Crypto$uuid_v4` - Random UUID
//! - `$Crypto$uuid_v7` - Time-ordered UUID

use sha2::{Sha256, Sha512, Digest};
use md5::Md5;
use zrtl::{zrtl_plugin, StringPtr, ArrayPtr, string_new, string_as_str, array_new, array_push, array_length};

// ============================================================================
// Hashing - SHA-2
// ============================================================================

/// SHA-256 hash of string, returns hex-encoded digest
#[no_mangle]
pub extern "C" fn crypto_sha256(data: StringPtr) -> StringPtr {
    let data_str = match unsafe { string_as_str(data) } {
        Some(s) => s,
        None => return string_new(""),
    };

    let mut hasher = Sha256::new();
    hasher.update(data_str.as_bytes());
    let result = hasher.finalize();
    string_new(&hex::encode(result))
}

/// SHA-256 hash of raw bytes, returns hex-encoded digest
#[no_mangle]
pub extern "C" fn crypto_sha256_bytes(data: ArrayPtr) -> StringPtr {
    if data.is_null() {
        return string_new("");
    }

    let header = data as *const i32;
    let length = unsafe { *header.add(1) } as usize;
    let bytes = unsafe { header.add(2) } as *const u8;
    let slice = unsafe { std::slice::from_raw_parts(bytes, length) };

    let mut hasher = Sha256::new();
    hasher.update(slice);
    let result = hasher.finalize();
    string_new(&hex::encode(result))
}

/// SHA-512 hash of string, returns hex-encoded digest
#[no_mangle]
pub extern "C" fn crypto_sha512(data: StringPtr) -> StringPtr {
    let data_str = match unsafe { string_as_str(data) } {
        Some(s) => s,
        None => return string_new(""),
    };

    let mut hasher = Sha512::new();
    hasher.update(data_str.as_bytes());
    let result = hasher.finalize();
    string_new(&hex::encode(result))
}

// ============================================================================
// Hashing - MD5 (not for security!)
// ============================================================================

/// MD5 hash of string, returns hex-encoded digest
/// WARNING: MD5 is cryptographically broken. Use only for checksums.
#[no_mangle]
pub extern "C" fn crypto_md5(data: StringPtr) -> StringPtr {
    let data_str = match unsafe { string_as_str(data) } {
        Some(s) => s,
        None => return string_new(""),
    };

    let mut hasher = Md5::new();
    hasher.update(data_str.as_bytes());
    let result = hasher.finalize();
    string_new(&hex::encode(result))
}

// ============================================================================
// Base64 Encoding
// ============================================================================

/// Base64 encode a string
#[no_mangle]
pub extern "C" fn crypto_base64_encode(data: StringPtr) -> StringPtr {
    let data_str = match unsafe { string_as_str(data) } {
        Some(s) => s,
        None => return string_new(""),
    };

    use base64::Engine;
    let encoded = base64::engine::general_purpose::STANDARD.encode(data_str.as_bytes());
    string_new(&encoded)
}

/// Base64 encode raw bytes
#[no_mangle]
pub extern "C" fn crypto_base64_encode_bytes(data: ArrayPtr) -> StringPtr {
    if data.is_null() {
        return string_new("");
    }

    let header = data as *const i32;
    let length = unsafe { *header.add(1) } as usize;
    let bytes = unsafe { header.add(2) } as *const u8;
    let slice = unsafe { std::slice::from_raw_parts(bytes, length) };

    use base64::Engine;
    let encoded = base64::engine::general_purpose::STANDARD.encode(slice);
    string_new(&encoded)
}

/// Base64 decode to string (returns empty on invalid input)
#[no_mangle]
pub extern "C" fn crypto_base64_decode(encoded: StringPtr) -> StringPtr {
    let encoded_str = match unsafe { string_as_str(encoded) } {
        Some(s) => s,
        None => return string_new(""),
    };

    use base64::Engine;
    match base64::engine::general_purpose::STANDARD.decode(encoded_str) {
        Ok(bytes) => {
            match String::from_utf8(bytes) {
                Ok(s) => string_new(&s),
                Err(_) => string_new(""),
            }
        }
        Err(_) => string_new(""),
    }
}

/// Base64 decode to bytes
#[no_mangle]
pub extern "C" fn crypto_base64_decode_bytes(encoded: StringPtr) -> ArrayPtr {
    let encoded_str = match unsafe { string_as_str(encoded) } {
        Some(s) => s,
        None => return array_new::<u8>(0),
    };

    use base64::Engine;
    match base64::engine::general_purpose::STANDARD.decode(encoded_str) {
        Ok(bytes) => {
            let arr = array_new::<u8>(bytes.len());
            for byte in bytes {
                unsafe { array_push(arr, byte); }
            }
            arr
        }
        Err(_) => array_new::<u8>(0),
    }
}

/// URL-safe Base64 encode
#[no_mangle]
pub extern "C" fn crypto_base64_url_encode(data: StringPtr) -> StringPtr {
    let data_str = match unsafe { string_as_str(data) } {
        Some(s) => s,
        None => return string_new(""),
    };

    use base64::Engine;
    let encoded = base64::engine::general_purpose::URL_SAFE_NO_PAD.encode(data_str.as_bytes());
    string_new(&encoded)
}

/// URL-safe Base64 decode
#[no_mangle]
pub extern "C" fn crypto_base64_url_decode(encoded: StringPtr) -> StringPtr {
    let encoded_str = match unsafe { string_as_str(encoded) } {
        Some(s) => s,
        None => return string_new(""),
    };

    use base64::Engine;
    match base64::engine::general_purpose::URL_SAFE_NO_PAD.decode(encoded_str) {
        Ok(bytes) => {
            match String::from_utf8(bytes) {
                Ok(s) => string_new(&s),
                Err(_) => string_new(""),
            }
        }
        Err(_) => string_new(""),
    }
}

// ============================================================================
// Hex Encoding
// ============================================================================

/// Hex encode a string
#[no_mangle]
pub extern "C" fn crypto_hex_encode(data: StringPtr) -> StringPtr {
    let data_str = match unsafe { string_as_str(data) } {
        Some(s) => s,
        None => return string_new(""),
    };

    string_new(&hex::encode(data_str.as_bytes()))
}

/// Hex encode raw bytes
#[no_mangle]
pub extern "C" fn crypto_hex_encode_bytes(data: ArrayPtr) -> StringPtr {
    if data.is_null() {
        return string_new("");
    }

    let header = data as *const i32;
    let length = unsafe { *header.add(1) } as usize;
    let bytes = unsafe { header.add(2) } as *const u8;
    let slice = unsafe { std::slice::from_raw_parts(bytes, length) };

    string_new(&hex::encode(slice))
}

/// Hex decode to string
#[no_mangle]
pub extern "C" fn crypto_hex_decode(encoded: StringPtr) -> StringPtr {
    let encoded_str = match unsafe { string_as_str(encoded) } {
        Some(s) => s,
        None => return string_new(""),
    };

    match hex::decode(encoded_str) {
        Ok(bytes) => {
            match String::from_utf8(bytes) {
                Ok(s) => string_new(&s),
                Err(_) => string_new(""),
            }
        }
        Err(_) => string_new(""),
    }
}

/// Hex decode to bytes
#[no_mangle]
pub extern "C" fn crypto_hex_decode_bytes(encoded: StringPtr) -> ArrayPtr {
    let encoded_str = match unsafe { string_as_str(encoded) } {
        Some(s) => s,
        None => return array_new::<u8>(0),
    };

    match hex::decode(encoded_str) {
        Ok(bytes) => {
            let arr = array_new::<u8>(bytes.len());
            for byte in bytes {
                unsafe { array_push(arr, byte); }
            }
            arr
        }
        Err(_) => array_new::<u8>(0),
    }
}

// ============================================================================
// UUID Generation
// ============================================================================

/// Generate a random UUID v4
#[no_mangle]
pub extern "C" fn crypto_uuid_v4() -> StringPtr {
    let id = uuid::Uuid::new_v4();
    string_new(&id.to_string())
}

/// Generate a time-ordered UUID v7
#[no_mangle]
pub extern "C" fn crypto_uuid_v7() -> StringPtr {
    let id = uuid::Uuid::now_v7();
    string_new(&id.to_string())
}

/// Validate a UUID string
#[no_mangle]
pub extern "C" fn crypto_uuid_is_valid(s: StringPtr) -> i32 {
    let s_str = match unsafe { string_as_str(s) } {
        Some(s) => s,
        None => return 0,
    };

    uuid::Uuid::parse_str(s_str).is_ok() as i32
}

/// Get UUID version (returns 0 if invalid)
#[no_mangle]
pub extern "C" fn crypto_uuid_version(s: StringPtr) -> i32 {
    let s_str = match unsafe { string_as_str(s) } {
        Some(s) => s,
        None => return 0,
    };

    match uuid::Uuid::parse_str(s_str) {
        Ok(id) => id.get_version_num() as i32,
        Err(_) => 0,
    }
}

// ============================================================================
// Random Bytes
// ============================================================================

/// Generate cryptographically secure random bytes using OS entropy source
#[no_mangle]
pub extern "C" fn crypto_random_bytes(count: i32) -> ArrayPtr {
    if count <= 0 {
        return array_new::<u8>(0);
    }

    let mut bytes = vec![0u8; count as usize];
    if getrandom::getrandom(&mut bytes).is_err() {
        return array_new::<u8>(0);
    }

    let arr = array_new::<u8>(count as usize);
    for byte in bytes {
        unsafe { array_push(arr, byte); }
    }
    arr
}

/// Generate random hex string of given length using cryptographic randomness
#[no_mangle]
pub extern "C" fn crypto_random_hex(length: i32) -> StringPtr {
    if length <= 0 {
        return string_new("");
    }

    let bytes_needed = (length as usize + 1) / 2;
    let mut bytes = vec![0u8; bytes_needed];

    if getrandom::getrandom(&mut bytes).is_err() {
        return string_new("");
    }

    let hex_str = hex::encode(&bytes);
    string_new(&hex_str[..length as usize])
}

// ============================================================================
// Plugin Export
// ============================================================================

zrtl_plugin! {
    name: "zrtl_crypto",
    symbols: [
        // Hashing
        ("$Crypto$sha256", crypto_sha256),
        ("$Crypto$sha256_bytes", crypto_sha256_bytes),
        ("$Crypto$sha512", crypto_sha512),
        ("$Crypto$md5", crypto_md5),

        // Base64
        ("$Crypto$base64_encode", crypto_base64_encode),
        ("$Crypto$base64_encode_bytes", crypto_base64_encode_bytes),
        ("$Crypto$base64_decode", crypto_base64_decode),
        ("$Crypto$base64_decode_bytes", crypto_base64_decode_bytes),
        ("$Crypto$base64_url_encode", crypto_base64_url_encode),
        ("$Crypto$base64_url_decode", crypto_base64_url_decode),

        // Hex
        ("$Crypto$hex_encode", crypto_hex_encode),
        ("$Crypto$hex_encode_bytes", crypto_hex_encode_bytes),
        ("$Crypto$hex_decode", crypto_hex_decode),
        ("$Crypto$hex_decode_bytes", crypto_hex_decode_bytes),

        // UUID
        ("$Crypto$uuid_v4", crypto_uuid_v4),
        ("$Crypto$uuid_v7", crypto_uuid_v7),
        ("$Crypto$uuid_is_valid", crypto_uuid_is_valid),
        ("$Crypto$uuid_version", crypto_uuid_version),

        // Random
        ("$Crypto$random_bytes", crypto_random_bytes),
        ("$Crypto$random_hex", crypto_random_hex),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sha256() {
        let input = string_new("hello");
        let hash = crypto_sha256(input);
        let hash_str = unsafe { string_as_str(hash) }.unwrap();
        // Known SHA-256 of "hello"
        assert_eq!(hash_str, "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824");
    }

    #[test]
    fn test_md5() {
        let input = string_new("hello");
        let hash = crypto_md5(input);
        let hash_str = unsafe { string_as_str(hash) }.unwrap();
        // Known MD5 of "hello"
        assert_eq!(hash_str, "5d41402abc4b2a76b9719d911017c592");
    }

    #[test]
    fn test_base64() {
        let input = string_new("hello world");
        let encoded = crypto_base64_encode(input);
        let encoded_str = unsafe { string_as_str(encoded) }.unwrap();
        assert_eq!(encoded_str, "aGVsbG8gd29ybGQ=");

        let decoded = crypto_base64_decode(encoded);
        let decoded_str = unsafe { string_as_str(decoded) }.unwrap();
        assert_eq!(decoded_str, "hello world");
    }

    #[test]
    fn test_hex() {
        let input = string_new("hello");
        let encoded = crypto_hex_encode(input);
        let encoded_str = unsafe { string_as_str(encoded) }.unwrap();
        assert_eq!(encoded_str, "68656c6c6f");

        let decoded = crypto_hex_decode(encoded);
        let decoded_str = unsafe { string_as_str(decoded) }.unwrap();
        assert_eq!(decoded_str, "hello");
    }

    #[test]
    fn test_uuid_v4() {
        let uuid1 = crypto_uuid_v4();
        let uuid2 = crypto_uuid_v4();

        let uuid1_str = unsafe { string_as_str(uuid1) }.unwrap();
        let uuid2_str = unsafe { string_as_str(uuid2) }.unwrap();

        // Should be valid UUIDs
        assert_eq!(crypto_uuid_is_valid(uuid1), 1);
        assert_eq!(crypto_uuid_is_valid(uuid2), 1);

        // Should be different
        assert_ne!(uuid1_str, uuid2_str);

        // Should be version 4
        assert_eq!(crypto_uuid_version(uuid1), 4);
    }

    #[test]
    fn test_uuid_v7() {
        let uuid = crypto_uuid_v7();
        assert_eq!(crypto_uuid_is_valid(uuid), 1);
        assert_eq!(crypto_uuid_version(uuid), 7);
    }

    #[test]
    fn test_random_hex() {
        let hex1 = crypto_random_hex(16);
        let hex2 = crypto_random_hex(16);

        let hex1_str = unsafe { string_as_str(hex1) }.unwrap();
        let hex2_str = unsafe { string_as_str(hex2) }.unwrap();

        assert_eq!(hex1_str.len(), 16);
        assert_eq!(hex2_str.len(), 16);
        // Very unlikely to be equal
        assert_ne!(hex1_str, hex2_str);
    }
}
