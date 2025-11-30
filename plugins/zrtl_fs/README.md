# zrtl_fs

File system operations for Zyntax-based languages.

## Overview

Provides file reading, writing, directory operations, and path manipulation using ZRTL SDK types.

## Exported Symbols

### File Read Operations

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$FS$read_file` | `(StringPtr) -> StringPtr` | Read entire file as string (null on error) |
| `$FS$read_bytes` | `(StringPtr) -> ArrayPtr` | Read file as byte array (null on error) |

### File Write Operations

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$FS$write_file` | `(StringPtr, StringPtr) -> i32` | Write string to file (0=success, -1=error) |
| `$FS$write_bytes` | `(StringPtr, ArrayPtr) -> i32` | Write bytes to file (0=success, -1=error) |
| `$FS$append_file` | `(StringPtr, StringPtr) -> i32` | Append string to file (0=success, -1=error) |

### File Information

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$FS$exists` | `(StringPtr) -> i32` | Check if path exists (1=yes, 0=no) |
| `$FS$is_file` | `(StringPtr) -> i32` | Check if path is a file (1=yes, 0=no) |
| `$FS$is_dir` | `(StringPtr) -> i32` | Check if path is a directory (1=yes, 0=no) |
| `$FS$is_symlink` | `(StringPtr) -> i32` | Check if path is a symbolic link |
| `$FS$file_size` | `(StringPtr) -> i64` | Get file size in bytes (-1 on error) |

### Directory Operations

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$FS$create_dir` | `(StringPtr) -> i32` | Create a directory (0=success) |
| `$FS$create_dir_all` | `(StringPtr) -> i32` | Create directory and all parents (0=success) |
| `$FS$remove_file` | `(StringPtr) -> i32` | Delete a file (0=success) |
| `$FS$remove_dir` | `(StringPtr) -> i32` | Delete an empty directory (0=success) |
| `$FS$remove_dir_all` | `(StringPtr) -> i32` | Delete directory recursively (0=success) |
| `$FS$rename` | `(StringPtr, StringPtr) -> i32` | Rename/move file or directory |
| `$FS$copy` | `(StringPtr, StringPtr) -> i64` | Copy file (returns bytes copied, -1 on error) |

### Path Operations

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$FS$current_dir` | `() -> StringPtr` | Get current working directory |
| `$FS$set_current_dir` | `(StringPtr) -> i32` | Change current working directory |
| `$FS$canonicalize` | `(StringPtr) -> StringPtr` | Get absolute/canonical path |
| `$FS$extension` | `(StringPtr) -> StringPtr` | Get file extension |
| `$FS$file_name` | `(StringPtr) -> StringPtr` | Get file name (last path component) |
| `$FS$parent` | `(StringPtr) -> StringPtr` | Get parent directory |

## Usage Example

```zig
// Read a file
const contents = $FS$read_file("config.json");
if (contents != null) {
    process(contents);
    string_free(contents);
}

// Write a file
const data = "Hello, World!";
if ($FS$write_file("output.txt", data) == 0) {
    // Success
}

// Create directories
$FS$create_dir_all("path/to/nested/dir");

// Get file info
const size = $FS$file_size("large_file.bin");
if ($FS$is_file("test.txt") == 1) {
    // It's a file
}

// Path manipulation
const ext = $FS$extension("image.png");  // "png"
const name = $FS$file_name("/path/to/file.txt");  // "file.txt"
const parent = $FS$parent("/path/to/file.txt");  // "/path/to"
```

## Return Values

- Functions returning `i32` use 0 for success, -1 for error
- Functions returning `StringPtr` return null on error
- Functions returning `ArrayPtr` return null on error

## Memory Management

All functions returning `StringPtr` or `ArrayPtr` allocate new memory. The caller is responsible for freeing using the appropriate SDK functions.

## Dependencies

- `zrtl` - Core ZRTL SDK
