# zrtl_crypto

Cryptographic hashing, encoding, and UUID generation for Zyntax-based languages.

## Overview

Provides cryptographic hash functions (SHA-256, SHA-512, MD5), encoding utilities (Base64, Hex), UUID generation, and cryptographically secure random bytes.

## Exported Symbols

### Hashing

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Crypto$sha256` | `(StringPtr) -> StringPtr` | SHA-256 hash, returns hex digest |
| `$Crypto$sha256_bytes` | `(ArrayPtr) -> StringPtr` | SHA-256 of raw bytes |
| `$Crypto$sha512` | `(StringPtr) -> StringPtr` | SHA-512 hash, returns hex digest |
| `$Crypto$md5` | `(StringPtr) -> StringPtr` | MD5 hash (not for security!) |

### Base64 Encoding

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Crypto$base64_encode` | `(StringPtr) -> StringPtr` | Encode string to Base64 |
| `$Crypto$base64_encode_bytes` | `(ArrayPtr) -> StringPtr` | Encode bytes to Base64 |
| `$Crypto$base64_decode` | `(StringPtr) -> StringPtr` | Decode Base64 to string |
| `$Crypto$base64_decode_bytes` | `(StringPtr) -> ArrayPtr` | Decode Base64 to bytes |
| `$Crypto$base64_url_encode` | `(StringPtr) -> StringPtr` | URL-safe Base64 encode |
| `$Crypto$base64_url_decode` | `(StringPtr) -> StringPtr` | URL-safe Base64 decode |

### Hex Encoding

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Crypto$hex_encode` | `(StringPtr) -> StringPtr` | Encode string to hex |
| `$Crypto$hex_encode_bytes` | `(ArrayPtr) -> StringPtr` | Encode bytes to hex |
| `$Crypto$hex_decode` | `(StringPtr) -> StringPtr` | Decode hex to string |
| `$Crypto$hex_decode_bytes` | `(StringPtr) -> ArrayPtr` | Decode hex to bytes |

### UUID Generation

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Crypto$uuid_v4` | `() -> StringPtr` | Generate random UUID (v4) |
| `$Crypto$uuid_v7` | `() -> StringPtr` | Generate time-ordered UUID (v7) |
| `$Crypto$uuid_is_valid` | `(StringPtr) -> i32` | Validate UUID string |
| `$Crypto$uuid_version` | `(StringPtr) -> i32` | Get UUID version (0 if invalid) |

### Random Bytes

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Crypto$random_bytes` | `(i32) -> ArrayPtr` | Generate cryptographically secure bytes |
| `$Crypto$random_hex` | `(i32) -> StringPtr` | Generate random hex string |

## Usage Example

```zig
// Hashing
const hash = $Crypto$sha256("hello");
// "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"

const md5_hash = $Crypto$md5("hello");
// "5d41402abc4b2a76b9719d911017c592"

// Base64 encoding
const encoded = $Crypto$base64_encode("Hello, World!");
// "SGVsbG8sIFdvcmxkIQ=="

const decoded = $Crypto$base64_decode(encoded);
// "Hello, World!"

// URL-safe Base64 (no padding, uses -_ instead of +/)
const url_safe = $Crypto$base64_url_encode("Hello?World!");

// Hex encoding
const hex = $Crypto$hex_encode("hello");
// "68656c6c6f"

const unhexed = $Crypto$hex_decode(hex);
// "hello"

// UUID generation
const uuid = $Crypto$uuid_v4();
// "550e8400-e29b-41d4-a716-446655440000" (random)

const uuid7 = $Crypto$uuid_v7();
// Time-ordered UUID (sortable by creation time)

// Validate UUID
if ($Crypto$uuid_is_valid(uuid) == 1) {
    const version = $Crypto$uuid_version(uuid);  // 4
}

// Cryptographically secure random
const random_bytes = $Crypto$random_bytes(32);  // 32 random bytes
const random_hex = $Crypto$random_hex(16);      // 16-char hex string

// Hash raw bytes
const data = array_new<u8>(5);
array_push(data, 'h');
array_push(data, 'e');
array_push(data, 'l');
array_push(data, 'l');
array_push(data, 'o');
const bytes_hash = $Crypto$sha256_bytes(data);
```

## Security Notes

### MD5
**WARNING**: MD5 is cryptographically broken. Use only for:
- Checksums (verifying file integrity, not security)
- Legacy compatibility
- Non-security purposes

For security, use SHA-256 or SHA-512.

### Random Numbers
`$Crypto$random_bytes` and `$Crypto$random_hex` use OS entropy sources (`getrandom`) and are suitable for:
- Cryptographic keys
- Session tokens
- Nonces
- Secure identifiers

For non-cryptographic random numbers (games, simulations), use `$Math$random` instead.

## UUID Versions

### v4 (Random)
- Completely random
- Best for general-purpose unique IDs
- No ordering guarantee

### v7 (Time-ordered)
- Contains timestamp
- Sortable by creation time
- Good for database primary keys
- Reveals approximate creation time

## Memory Management

All functions returning `StringPtr` or `ArrayPtr` allocate memory. Caller must free.

## Dependencies

- `zrtl` - Core ZRTL SDK
- `sha2` - SHA-256/512 implementation
- `md5` - MD5 implementation
- `base64` - Base64 encoding
- `hex` - Hex encoding
- `uuid` - UUID generation
- `getrandom` - Cryptographic random bytes
