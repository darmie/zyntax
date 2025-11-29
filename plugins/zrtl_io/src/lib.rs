//! ZRTL I/O Plugin
//!
//! Provides standard I/O operations for Zyntax-based languages using
//! the ZRTL SDK string format `[i32 length][utf8_bytes...]`.
//!
//! ## Exported Symbols
//!
//! ### String Output
//! - `$IO$print` - Print ZRTL string to stdout (no newline)
//! - `$IO$println` - Print ZRTL string with newline to stdout
//! - `$IO$eprint` - Print ZRTL string to stderr (no newline)
//! - `$IO$eprintln` - Print ZRTL string with newline to stderr
//!
//! ### Primitive Output
//! - `$IO$print_i64`, `$IO$println_i64` - Print integers
//! - `$IO$print_f64`, `$IO$println_f64` - Print floats
//! - `$IO$print_bool`, `$IO$println_bool` - Print booleans
//!
//! ### Input
//! - `$IO$read_line` - Read line from stdin, returns ZRTL string
//! - `$IO$input` - Read line with prompt, returns ZRTL string
//!
//! ### Formatting
//! - `$IO$format_i64` - Format integer as ZRTL string
//! - `$IO$format_f64` - Format float as ZRTL string
//! - `$IO$format_bool` - Format boolean as ZRTL string

use std::io::{self, BufRead, Write};
use zrtl::{
    zrtl_plugin,
    StringConstPtr, StringPtr,
    string_length, string_data, string_new, string_free,
};

// ============================================================================
// String I/O Functions (using ZRTL string format)
// ============================================================================

/// Print a ZRTL string to stdout (no newline)
///
/// ZRTL string format: `[i32 length][utf8_bytes...]`
///
/// # Safety
/// The pointer must be a valid ZRTL string pointer.
#[no_mangle]
pub unsafe extern "C" fn io_print(s: StringConstPtr) {
    if s.is_null() {
        return;
    }
    let len = string_length(s) as usize;
    let data = string_data(s);
    if len > 0 && !data.is_null() {
        let bytes = std::slice::from_raw_parts(data, len);
        if let Ok(str_slice) = std::str::from_utf8(bytes) {
            print!("{}", str_slice);
        }
    }
}

/// Print a ZRTL string to stdout with newline
///
/// # Safety
/// The pointer must be a valid ZRTL string pointer.
#[no_mangle]
pub unsafe extern "C" fn io_println(s: StringConstPtr) {
    if s.is_null() {
        println!();
        return;
    }
    let len = string_length(s) as usize;
    let data = string_data(s);
    if len > 0 && !data.is_null() {
        let bytes = std::slice::from_raw_parts(data, len);
        if let Ok(str_slice) = std::str::from_utf8(bytes) {
            println!("{}", str_slice);
        } else {
            println!();
        }
    } else {
        println!();
    }
}

/// Print a ZRTL string to stderr (no newline)
///
/// # Safety
/// The pointer must be a valid ZRTL string pointer.
#[no_mangle]
pub unsafe extern "C" fn io_eprint(s: StringConstPtr) {
    if s.is_null() {
        return;
    }
    let len = string_length(s) as usize;
    let data = string_data(s);
    if len > 0 && !data.is_null() {
        let bytes = std::slice::from_raw_parts(data, len);
        if let Ok(str_slice) = std::str::from_utf8(bytes) {
            eprint!("{}", str_slice);
        }
    }
}

/// Print a ZRTL string to stderr with newline
///
/// # Safety
/// The pointer must be a valid ZRTL string pointer.
#[no_mangle]
pub unsafe extern "C" fn io_eprintln(s: StringConstPtr) {
    if s.is_null() {
        eprintln!();
        return;
    }
    let len = string_length(s) as usize;
    let data = string_data(s);
    if len > 0 && !data.is_null() {
        let bytes = std::slice::from_raw_parts(data, len);
        if let Ok(str_slice) = std::str::from_utf8(bytes) {
            eprintln!("{}", str_slice);
        } else {
            eprintln!();
        }
    } else {
        eprintln!();
    }
}

// ============================================================================
// Primitive I/O Functions
// ============================================================================

/// Print an i64 to stdout
#[no_mangle]
pub extern "C" fn io_print_i64(value: i64) {
    print!("{}", value);
}

/// Print an i64 to stdout with newline
#[no_mangle]
pub extern "C" fn io_println_i64(value: i64) {
    println!("{}", value);
}

/// Print a u64 to stdout
#[no_mangle]
pub extern "C" fn io_print_u64(value: u64) {
    print!("{}", value);
}

/// Print a u64 to stdout with newline
#[no_mangle]
pub extern "C" fn io_println_u64(value: u64) {
    println!("{}", value);
}

/// Print an f64 to stdout
#[no_mangle]
pub extern "C" fn io_print_f64(value: f64) {
    print!("{}", value);
}

/// Print an f64 to stdout with newline
#[no_mangle]
pub extern "C" fn io_println_f64(value: f64) {
    println!("{}", value);
}

/// Print a boolean to stdout
#[no_mangle]
pub extern "C" fn io_print_bool(value: bool) {
    print!("{}", value);
}

/// Print a boolean to stdout with newline
#[no_mangle]
pub extern "C" fn io_println_bool(value: bool) {
    println!("{}", value);
}

/// Print a single character (Unicode codepoint) to stdout
#[no_mangle]
pub extern "C" fn io_print_char(codepoint: u32) {
    if let Some(c) = char::from_u32(codepoint) {
        print!("{}", c);
    }
}

/// Print a single character with newline
#[no_mangle]
pub extern "C" fn io_println_char(codepoint: u32) {
    if let Some(c) = char::from_u32(codepoint) {
        println!("{}", c);
    } else {
        println!();
    }
}

// ============================================================================
// Flush Functions
// ============================================================================

/// Flush stdout
/// Returns 0 on success, -1 on error
#[no_mangle]
pub extern "C" fn io_flush() -> i32 {
    match io::stdout().flush() {
        Ok(()) => 0,
        Err(_) => -1,
    }
}

/// Flush stderr
/// Returns 0 on success, -1 on error
#[no_mangle]
pub extern "C" fn io_flush_stderr() -> i32 {
    match io::stderr().flush() {
        Ok(()) => 0,
        Err(_) => -1,
    }
}

// ============================================================================
// Input Functions (returning ZRTL strings)
// ============================================================================

/// Read a line from stdin
///
/// Returns a ZRTL string pointer `[i32 length][utf8_bytes...]`
/// The caller must free this memory using `string_free` from the SDK.
/// Returns null on EOF or error.
#[no_mangle]
pub extern "C" fn io_read_line() -> StringPtr {
    let stdin = io::stdin();
    let mut handle = stdin.lock();
    let mut buffer = String::new();

    match handle.read_line(&mut buffer) {
        Ok(0) => std::ptr::null_mut(), // EOF
        Ok(_) => {
            // Remove trailing newline
            if buffer.ends_with('\n') {
                buffer.pop();
                if buffer.ends_with('\r') {
                    buffer.pop();
                }
            }
            // Return as ZRTL string
            string_new(&buffer)
        }
        Err(_) => std::ptr::null_mut(),
    }
}

/// Read a line from stdin with a prompt
///
/// The prompt is a ZRTL string that will be printed before reading.
/// Returns a ZRTL string pointer, caller must free with `string_free`.
///
/// # Safety
/// The prompt pointer must be a valid ZRTL string pointer or null.
#[no_mangle]
pub unsafe extern "C" fn io_input(prompt: StringConstPtr) -> StringPtr {
    // Print prompt if provided
    if !prompt.is_null() {
        io_print(prompt);
        let _ = io::stdout().flush();
    }
    io_read_line()
}

// ============================================================================
// Formatting Functions (returning ZRTL strings)
// ============================================================================

/// Format an i64 as a ZRTL string
///
/// Returns a ZRTL string pointer, caller must free with `string_free`.
#[no_mangle]
pub extern "C" fn io_format_i64(value: i64) -> StringPtr {
    let formatted = format!("{}", value);
    string_new(&formatted)
}

/// Format a u64 as a ZRTL string
#[no_mangle]
pub extern "C" fn io_format_u64(value: u64) -> StringPtr {
    let formatted = format!("{}", value);
    string_new(&formatted)
}

/// Format an f64 as a ZRTL string
#[no_mangle]
pub extern "C" fn io_format_f64(value: f64) -> StringPtr {
    let formatted = format!("{}", value);
    string_new(&formatted)
}

/// Format an f64 with specified decimal precision
#[no_mangle]
pub extern "C" fn io_format_f64_precision(value: f64, precision: u32) -> StringPtr {
    let formatted = format!("{:.prec$}", value, prec = precision as usize);
    string_new(&formatted)
}

/// Format a boolean as a ZRTL string ("true" or "false")
#[no_mangle]
pub extern "C" fn io_format_bool(value: bool) -> StringPtr {
    string_new(if value { "true" } else { "false" })
}

/// Format an i64 in hexadecimal (lowercase)
#[no_mangle]
pub extern "C" fn io_format_hex(value: i64) -> StringPtr {
    let formatted = format!("{:x}", value);
    string_new(&formatted)
}

/// Format an i64 in hexadecimal (uppercase)
#[no_mangle]
pub extern "C" fn io_format_hex_upper(value: i64) -> StringPtr {
    let formatted = format!("{:X}", value);
    string_new(&formatted)
}

/// Format an i64 in binary
#[no_mangle]
pub extern "C" fn io_format_binary(value: i64) -> StringPtr {
    let formatted = format!("{:b}", value);
    string_new(&formatted)
}

/// Format an i64 in octal
#[no_mangle]
pub extern "C" fn io_format_octal(value: i64) -> StringPtr {
    let formatted = format!("{:o}", value);
    string_new(&formatted)
}

/// Format a Unicode codepoint as a ZRTL string
#[no_mangle]
pub extern "C" fn io_format_char(codepoint: u32) -> StringPtr {
    if let Some(c) = char::from_u32(codepoint) {
        let formatted = format!("{}", c);
        string_new(&formatted)
    } else {
        string_new("")
    }
}

// ============================================================================
// String Memory Management (re-exported from SDK)
// ============================================================================

/// Free a ZRTL string allocated by this module
///
/// # Safety
/// The pointer must have been allocated by io_read_line, io_input, or io_format_*
#[no_mangle]
pub unsafe extern "C" fn io_string_free(s: StringPtr) {
    string_free(s);
}

// ============================================================================
// Plugin Export
// ============================================================================

zrtl_plugin! {
    name: "zrtl_io",
    symbols: [
        // String output (ZRTL string format)
        ("$IO$print", io_print),
        ("$IO$println", io_println),
        ("$IO$eprint", io_eprint),
        ("$IO$eprintln", io_eprintln),

        // Primitive output
        ("$IO$print_i64", io_print_i64),
        ("$IO$println_i64", io_println_i64),
        ("$IO$print_u64", io_print_u64),
        ("$IO$println_u64", io_println_u64),
        ("$IO$print_f64", io_print_f64),
        ("$IO$println_f64", io_println_f64),
        ("$IO$print_bool", io_print_bool),
        ("$IO$println_bool", io_println_bool),
        ("$IO$print_char", io_print_char),
        ("$IO$println_char", io_println_char),

        // Flush
        ("$IO$flush", io_flush),
        ("$IO$flush_stderr", io_flush_stderr),

        // Input (returns ZRTL strings)
        ("$IO$read_line", io_read_line),
        ("$IO$input", io_input),

        // Formatting (returns ZRTL strings)
        ("$IO$format_i64", io_format_i64),
        ("$IO$format_u64", io_format_u64),
        ("$IO$format_f64", io_format_f64),
        ("$IO$format_f64_precision", io_format_f64_precision),
        ("$IO$format_bool", io_format_bool),
        ("$IO$format_hex", io_format_hex),
        ("$IO$format_hex_upper", io_format_hex_upper),
        ("$IO$format_binary", io_format_binary),
        ("$IO$format_octal", io_format_octal),
        ("$IO$format_char", io_format_char),

        // Memory management
        ("$IO$string_free", io_string_free),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;
    use zrtl::string_as_str;

    #[test]
    fn test_format_i64() {
        let result = io_format_i64(42);
        assert!(!result.is_null());
        unsafe {
            assert_eq!(string_as_str(result), Some("42"));
            string_free(result);
        }
    }

    #[test]
    fn test_format_f64() {
        let result = io_format_f64(3.14);
        assert!(!result.is_null());
        unsafe {
            let s = string_as_str(result);
            assert!(s.is_some());
            assert!(s.unwrap().starts_with("3.14"));
            string_free(result);
        }
    }

    #[test]
    fn test_format_f64_precision() {
        let result = io_format_f64_precision(3.14159, 2);
        assert!(!result.is_null());
        unsafe {
            assert_eq!(string_as_str(result), Some("3.14"));
            string_free(result);
        }
    }

    #[test]
    fn test_format_hex() {
        let result = io_format_hex(255);
        assert!(!result.is_null());
        unsafe {
            assert_eq!(string_as_str(result), Some("ff"));
            string_free(result);
        }
    }

    #[test]
    fn test_format_binary() {
        let result = io_format_binary(5);
        assert!(!result.is_null());
        unsafe {
            assert_eq!(string_as_str(result), Some("101"));
            string_free(result);
        }
    }

    #[test]
    fn test_format_bool() {
        let result_true = io_format_bool(true);
        let result_false = io_format_bool(false);
        assert!(!result_true.is_null());
        assert!(!result_false.is_null());
        unsafe {
            assert_eq!(string_as_str(result_true), Some("true"));
            assert_eq!(string_as_str(result_false), Some("false"));
            string_free(result_true);
            string_free(result_false);
        }
    }

    #[test]
    fn test_print_zrtl_string() {
        let s = string_new("Hello, ZRTL!");
        assert!(!s.is_null());
        // Just verify it doesn't crash
        unsafe {
            io_print(s);
            io_println(s);
            string_free(s);
        }
    }
}
