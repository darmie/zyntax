//! Haxe String implementation
//!
//! Memory layout: [length: i32, char0, char1, ..., charN, '\0']
//! - Strings are immutable in Haxe
//! - UTF-8 encoded (for now, simplified to ASCII/byte strings)
//! - Null-terminated for C interop

use zyntax_plugin_macros::{runtime_export, runtime_method};

/// String header
#[repr(C)]
pub struct StringHeader {
    pub length: i32,
}

const HEADER_SIZE: isize = 1;

// ============================================================================
// String Creation
// ============================================================================

/// Create string from C string (null-terminated)
#[runtime_export("$String$fromCString")]
pub extern "C" fn String_fromCString(cstr: *const u8) -> *mut i32 {
    if cstr.is_null() {
        return String_empty();
    }

    unsafe {
        let len = libc::strlen(cstr as *const i8) as i32;
        String_fromBytes(cstr, len)
    }
}

/// Create string from bytes with known length
#[runtime_export("$String$fromBytes")]
pub extern "C" fn String_fromBytes(bytes: *const u8, length: i32) -> *mut i32 {
    if length < 0 {
        return core::ptr::null_mut();
    }

    unsafe {
        // Allocate: [length: i32, bytes..., '\0']
        let total_size = std::mem::size_of::<i32>() + (length as usize) + 1;
        let ptr = libc::malloc(total_size) as *mut i32;

        if ptr.is_null() {
            return core::ptr::null_mut();
        }

        *ptr = length;

        let str_ptr = ptr.offset(HEADER_SIZE) as *mut u8;

        if !bytes.is_null() && length > 0 {
            libc::memcpy(
                str_ptr as *mut libc::c_void,
                bytes as *const libc::c_void,
                length as usize,
            );
        }

        // Null terminate
        *str_ptr.offset(length as isize) = 0;

        ptr
    }
}

/// Create empty string
#[runtime_export("$String$empty")]
pub extern "C" fn String_empty() -> *mut i32 {
    String_fromBytes(core::ptr::null(), 0)
}

/// Create string from single character
#[runtime_export("$String$fromChar")]
pub extern "C" fn String_fromChar(ch: u8) -> *mut i32 {
    String_fromBytes(&ch as *const u8, 1)
}

// ============================================================================
// String Access
// ============================================================================

/// Get string length
#[runtime_method(
    symbol = "$String$length",
    haxe_type = "String",
    haxe_property = "length"
)]
pub extern "C" fn String_length(str_ptr: *const i32) -> i32 {
    if str_ptr.is_null() {
        return 0;
    }

    unsafe { *str_ptr }
}

/// Get character at index (returns 0 if out of bounds)
#[runtime_method(
    symbol = "$String$charAt",
    haxe_type = "String",
    haxe_method = "charAt"
)]
pub extern "C" fn String_charAt(str_ptr: *const i32, index: i32) -> u8 {
    if str_ptr.is_null() || index < 0 {
        return 0;
    }

    unsafe {
        let length = *str_ptr;
        if index >= length {
            return 0;
        }

        let chars = str_ptr.offset(HEADER_SIZE) as *const u8;
        *chars.offset(index as isize)
    }
}

/// Get character code at index (same as charAt for now)
#[runtime_method(
    symbol = "$String$charCodeAt",
    haxe_type = "String",
    haxe_method = "charCodeAt"
)]
pub extern "C" fn String_charCodeAt(str_ptr: *const i32, index: i32) -> i32 {
    String_charAt(str_ptr, index) as i32
}

/// Get pointer to null-terminated C string (for interop)
#[runtime_export("$String$toCString")]
pub extern "C" fn String_toCString(str_ptr: *const i32) -> *const u8 {
    if str_ptr.is_null() {
        return core::ptr::null();
    }

    unsafe { str_ptr.offset(HEADER_SIZE) as *const u8 }
}

// ============================================================================
// String Manipulation
// ============================================================================

/// Concatenate two strings (creates new string)
#[runtime_method(
    symbol = "$String$concat",
    haxe_type = "String",
    haxe_method = "+"
)]
pub extern "C" fn String_concat(str1: *const i32, str2: *const i32) -> *mut i32 {
    if str1.is_null() && str2.is_null() {
        return String_empty();
    }
    if str1.is_null() {
        return String_copy(str2);
    }
    if str2.is_null() {
        return String_copy(str1);
    }

    unsafe {
        let len1 = *str1;
        let len2 = *str2;
        let new_len = len1 + len2;

        let total_size = std::mem::size_of::<i32>() + (new_len as usize) + 1;
        let new_ptr = libc::malloc(total_size) as *mut i32;

        if new_ptr.is_null() {
            return core::ptr::null_mut();
        }

        *new_ptr = new_len;

        let new_chars = new_ptr.offset(HEADER_SIZE) as *mut u8;
        let chars1 = str1.offset(HEADER_SIZE) as *const u8;
        let chars2 = str2.offset(HEADER_SIZE) as *const u8;

        // Copy first string
        if len1 > 0 {
            libc::memcpy(
                new_chars as *mut libc::c_void,
                chars1 as *const libc::c_void,
                len1 as usize,
            );
        }

        // Copy second string
        if len2 > 0 {
            libc::memcpy(
                new_chars.offset(len1 as isize) as *mut libc::c_void,
                chars2 as *const libc::c_void,
                len2 as usize,
            );
        }

        // Null terminate
        *new_chars.offset(new_len as isize) = 0;

        new_ptr
    }
}

/// Get substring from start to end (exclusive)
#[runtime_method(
    symbol = "$String$substring",
    haxe_type = "String",
    haxe_method = "substring"
)]
pub extern "C" fn String_substring(str_ptr: *const i32, start: i32, end: i32) -> *mut i32 {
    if str_ptr.is_null() {
        return String_empty();
    }

    unsafe {
        let length = *str_ptr;

        // Normalize indices
        let mut start_idx = start.max(0).min(length);
        let mut end_idx = end.max(0).min(length);

        // Swap if start > end (Haxe behavior)
        if start_idx > end_idx {
            std::mem::swap(&mut start_idx, &mut end_idx);
        }

        let sub_len = end_idx - start_idx;

        if sub_len == 0 {
            return String_empty();
        }

        let chars = str_ptr.offset(HEADER_SIZE) as *const u8;
        String_fromBytes(chars.offset(start_idx as isize), sub_len)
    }
}

/// Get substring from start index to end of string
#[runtime_method(
    symbol = "$String$substr",
    haxe_type = "String",
    haxe_method = "substr"
)]
pub extern "C" fn String_substr(str_ptr: *const i32, start: i32, len: i32) -> *mut i32 {
    if str_ptr.is_null() {
        return String_empty();
    }

    unsafe {
        let length = *str_ptr;
        let start_idx = start.max(0).min(length);
        let sub_len = len.max(0).min(length - start_idx);

        if sub_len == 0 {
            return String_empty();
        }

        let chars = str_ptr.offset(HEADER_SIZE) as *const u8;
        String_fromBytes(chars.offset(start_idx as isize), sub_len)
    }
}

/// Convert string to lowercase (creates new string)
#[runtime_method(
    symbol = "$String$toLowerCase",
    haxe_type = "String",
    haxe_method = "toLowerCase"
)]
pub extern "C" fn String_toLowerCase(str_ptr: *const i32) -> *mut i32 {
    if str_ptr.is_null() {
        return String_empty();
    }

    unsafe {
        let length = *str_ptr;
        let chars = str_ptr.offset(HEADER_SIZE) as *const u8;

        // Allocate new string
        let total_size = std::mem::size_of::<i32>() + (length as usize) + 1;
        let new_ptr = libc::malloc(total_size) as *mut i32;

        if new_ptr.is_null() {
            return core::ptr::null_mut();
        }

        *new_ptr = length;
        let new_chars = new_ptr.offset(HEADER_SIZE) as *mut u8;

        // Convert to lowercase
        for i in 0..length {
            let ch = *chars.offset(i as isize);
            *new_chars.offset(i as isize) = if ch >= b'A' && ch <= b'Z' {
                ch + 32 // Convert to lowercase
            } else {
                ch
            };
        }

        // Null terminate
        *new_chars.offset(length as isize) = 0;

        new_ptr
    }
}

/// Convert string to uppercase (creates new string)
#[runtime_method(
    symbol = "$String$toUpperCase",
    haxe_type = "String",
    haxe_method = "toUpperCase"
)]
pub extern "C" fn String_toUpperCase(str_ptr: *const i32) -> *mut i32 {
    if str_ptr.is_null() {
        return String_empty();
    }

    unsafe {
        let length = *str_ptr;
        let chars = str_ptr.offset(HEADER_SIZE) as *const u8;

        let total_size = std::mem::size_of::<i32>() + (length as usize) + 1;
        let new_ptr = libc::malloc(total_size) as *mut i32;

        if new_ptr.is_null() {
            return core::ptr::null_mut();
        }

        *new_ptr = length;
        let new_chars = new_ptr.offset(HEADER_SIZE) as *mut u8;

        // Convert to uppercase
        for i in 0..length {
            let ch = *chars.offset(i as isize);
            *new_chars.offset(i as isize) = if ch >= b'a' && ch <= b'z' {
                ch - 32 // Convert to uppercase
            } else {
                ch
            };
        }

        *new_chars.offset(length as isize) = 0;

        new_ptr
    }
}

// ============================================================================
// String Searching
// ============================================================================

/// Find index of substring (returns -1 if not found)
#[runtime_method(
    symbol = "$String$indexOf",
    haxe_type = "String",
    haxe_method = "indexOf"
)]
pub extern "C" fn String_indexOf(str_ptr: *const i32, search: *const i32, start: i32) -> i32 {
    if str_ptr.is_null() || search.is_null() {
        return -1;
    }

    unsafe {
        let str_len = *str_ptr;
        let search_len = *search;

        if search_len == 0 {
            return start.max(0).min(str_len);
        }

        if search_len > str_len {
            return -1;
        }

        let str_chars = str_ptr.offset(HEADER_SIZE) as *const u8;
        let search_chars = search.offset(HEADER_SIZE) as *const u8;

        let start_idx = start.max(0);

        for i in start_idx..=(str_len - search_len) {
            let mut found = true;
            for j in 0..search_len {
                if *str_chars.offset((i + j) as isize) != *search_chars.offset(j as isize) {
                    found = false;
                    break;
                }
            }
            if found {
                return i;
            }
        }

        -1
    }
}

/// Find last index of substring (returns -1 if not found)
#[runtime_method(
    symbol = "$String$lastIndexOf",
    haxe_type = "String",
    haxe_method = "lastIndexOf"
)]
pub extern "C" fn String_lastIndexOf(str_ptr: *const i32, search: *const i32, start: i32) -> i32 {
    if str_ptr.is_null() || search.is_null() {
        return -1;
    }

    unsafe {
        let str_len = *str_ptr;
        let search_len = *search;

        if search_len == 0 {
            return str_len;
        }

        if search_len > str_len {
            return -1;
        }

        let str_chars = str_ptr.offset(HEADER_SIZE) as *const u8;
        let search_chars = search.offset(HEADER_SIZE) as *const u8;

        let start_idx = if start < 0 {
            str_len - search_len
        } else {
            start.min(str_len - search_len)
        };

        for i in (0..=start_idx).rev() {
            let mut found = true;
            for j in 0..search_len {
                if *str_chars.offset((i + j) as isize) != *search_chars.offset(j as isize) {
                    found = false;
                    break;
                }
            }
            if found {
                return i;
            }
        }

        -1
    }
}

// ============================================================================
// String Utilities
// ============================================================================

/// Check if two strings are equal
#[runtime_method(
    symbol = "$String$equals",
    haxe_type = "String",
    haxe_method = "=="
)]
pub extern "C" fn String_equals(str1: *const i32, str2: *const i32) -> i32 {
    if str1 == str2 {
        return 1;
    }
    if str1.is_null() || str2.is_null() {
        return 0;
    }

    unsafe {
        let len1 = *str1;
        let len2 = *str2;

        if len1 != len2 {
            return 0;
        }

        let chars1 = str1.offset(HEADER_SIZE) as *const u8;
        let chars2 = str2.offset(HEADER_SIZE) as *const u8;

        let cmp_result = libc::memcmp(
            chars1 as *const libc::c_void,
            chars2 as *const libc::c_void,
            len1 as usize,
        );

        if cmp_result == 0 { 1 } else { 0 }
    }
}

/// Create a copy of string
#[runtime_export("$String$copy")]
pub extern "C" fn String_copy(str_ptr: *const i32) -> *mut i32 {
    if str_ptr.is_null() {
        return String_empty();
    }

    unsafe {
        let length = *str_ptr;
        let total_size = std::mem::size_of::<i32>() + (length as usize) + 1;

        let new_ptr = libc::malloc(total_size) as *mut i32;
        if new_ptr.is_null() {
            return core::ptr::null_mut();
        }

        libc::memcpy(
            new_ptr as *mut libc::c_void,
            str_ptr as *const libc::c_void,
            total_size,
        );

        new_ptr
    }
}

/// Free string memory
#[runtime_export("$String$free")]
pub extern "C" fn String_free(str_ptr: *mut i32) {
    if !str_ptr.is_null() {
        unsafe {
            libc::free(str_ptr as *mut libc::c_void);
        }
    }
}

/// Print a Haxe string to stdout
#[runtime_export("$String$print")]
pub extern "C" fn String_print(str_ptr: *const i32) {
    if str_ptr.is_null() {
        return;
    }

    unsafe {
        let length = *str_ptr;
        let chars = str_ptr.offset(HEADER_SIZE) as *const u8;

        // Print characters one by one
        for i in 0..length {
            let ch = *chars.offset(i as isize);
            libc::putchar(ch as i32);
        }
    }
}

/// Print a Haxe string to stdout with newline
#[runtime_export("$String$println")]
pub extern "C" fn String_println(str_ptr: *const i32) {
    String_print(str_ptr);
    unsafe {
        libc::putchar(b'\n' as i32);
    }
}

/// Print an integer to stdout with newline
#[runtime_export("$Int$println")]
pub extern "C" fn Int_println(value: i32) {
    unsafe {
        libc::printf(b"%d\n\0".as_ptr() as *const i8, value);
    }
}

/// Print a float to stdout with newline
#[runtime_export("$Float$println")]
pub extern "C" fn Float_println(value: f64) {
    unsafe {
        libc::printf(b"%g\n\0".as_ptr() as *const i8, value);
    }
}

/// Print a boolean to stdout with newline
#[runtime_export("$Bool$println")]
pub extern "C" fn Bool_println(value: i32) {
    unsafe {
        if value != 0 {
            libc::printf(b"true\n\0".as_ptr() as *const i8);
        } else {
            libc::printf(b"false\n\0".as_ptr() as *const i8);
        }
    }
}

// ============================================================================
// ToString Functions (aliases and implementations)
// ============================================================================

/// Convert integer to string (alias for String_fromInt)
#[runtime_export("$Int$toString")]
pub extern "C" fn Int_toString(value: i32) -> *mut i32 {
    String_fromInt(value)
}

/// Convert float to string (alias for String_fromFloat)
#[runtime_export("$Float$toString")]
pub extern "C" fn Float_toString(value: f64) -> *mut i32 {
    String_fromFloat(value)
}

/// Convert boolean to string
#[runtime_export("$Bool$toString")]
pub extern "C" fn Bool_toString(value: i32) -> *mut i32 {
    unsafe {
        if value != 0 {
            String_fromBytes(b"true".as_ptr(), 4)
        } else {
            String_fromBytes(b"false".as_ptr(), 5)
        }
    }
}

// ============================================================================
// Type Conversions
// ============================================================================

/// Convert integer to string
#[runtime_export("$String$fromInt")]
pub extern "C" fn String_fromInt(value: i32) -> *mut i32 {
    unsafe {
        // Allocate buffer for int to string conversion
        let mut buffer: [u8; 32] = [0; 32];
        let len = libc::snprintf(
            buffer.as_mut_ptr() as *mut i8,
            32,
            b"%d\0".as_ptr() as *const i8,
            value,
        );

        String_fromBytes(buffer.as_ptr(), len)
    }
}

/// Convert float to string
#[runtime_export("$String$fromFloat")]
pub extern "C" fn String_fromFloat(value: f64) -> *mut i32 {
    unsafe {
        let mut buffer: [u8; 64] = [0; 64];
        let len = libc::snprintf(
            buffer.as_mut_ptr() as *mut i8,
            64,
            b"%g\0".as_ptr() as *const i8,
            value,
        );

        String_fromBytes(buffer.as_ptr(), len)
    }
}
