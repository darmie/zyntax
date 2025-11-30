# zrtl_regex

Regular expression matching and replacement for Zyntax-based languages.

## Overview

Provides regex compilation, matching, replacement, and capture group extraction. Supports both compiled patterns (for reuse) and quick one-off operations.

## Exported Symbols

### Pattern Compilation

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Regex$compile` | `(StringPtr) -> u64` | Compile regex pattern (0 on error) |
| `$Regex$compile_ignorecase` | `(StringPtr) -> u64` | Compile case-insensitive pattern |
| `$Regex$free` | `(u64) -> ()` | Free compiled pattern |

### Matching (Compiled Pattern)

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Regex$matches` | `(u64, StringPtr) -> i32` | Check if pattern matches (1=yes) |
| `$Regex$find_start` | `(u64, StringPtr) -> i64` | Find first match position (-1 if not found) |
| `$Regex$find_match` | `(u64, StringPtr) -> StringPtr` | Get first match as string |
| `$Regex$find_all` | `(u64, StringPtr) -> ArrayPtr` | Get all matches as string array |
| `$Regex$count` | `(u64, StringPtr) -> i64` | Count number of matches |

### Replacement (Compiled Pattern)

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Regex$replace_first` | `(u64, StringPtr, StringPtr) -> StringPtr` | Replace first match |
| `$Regex$replace_all_compiled` | `(u64, StringPtr, StringPtr) -> StringPtr` | Replace all matches |

### Capture Groups

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Regex$capture` | `(u64, StringPtr, i32) -> StringPtr` | Get capture group by index |
| `$Regex$captures` | `(u64, StringPtr) -> ArrayPtr` | Get all capture groups as array |

### Split

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Regex$split` | `(u64, StringPtr) -> ArrayPtr` | Split string by pattern |
| `$Regex$splitn` | `(u64, StringPtr, i32) -> ArrayPtr` | Split with limit |

### Quick Functions (No Compilation)

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Regex$is_match` | `(StringPtr, StringPtr) -> i32` | Quick pattern match check |
| `$Regex$find` | `(StringPtr, StringPtr) -> StringPtr` | Quick find first match |
| `$Regex$replace` | `(StringPtr, StringPtr, StringPtr) -> StringPtr` | Quick replace first |
| `$Regex$replace_all` | `(StringPtr, StringPtr, StringPtr) -> StringPtr` | Quick replace all |

### Utility

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Regex$escape` | `(StringPtr) -> StringPtr` | Escape special regex characters |

## Usage Example

```zig
// Compile pattern for reuse
const pattern = $Regex$compile("\\d+");  // Match digits

// Check if matches
if ($Regex$matches(pattern, "hello 123") == 1) {
    // Found digits
}

// Find matches
const first = $Regex$find_match(pattern, "a1b2c3");  // "1"
const all = $Regex$find_all(pattern, "a1b2c3");      // ["1", "2", "3"]
const count = $Regex$count(pattern, "a1b2c3");       // 3

// Replace
const result = $Regex$replace_all_compiled(pattern, "a1b2c3", "X");
// result = "aXbXcX"

// Capture groups
const email_pattern = $Regex$compile("(\\w+)@(\\w+)\\.(\\w+)");
const text = "Contact: user@example.com";

const username = $Regex$capture(email_pattern, text, 1);  // "user"
const domain = $Regex$capture(email_pattern, text, 2);    // "example"
const tld = $Regex$capture(email_pattern, text, 3);       // "com"

// Get all captures at once
const groups = $Regex$captures(email_pattern, text);
// groups[0] = "user@example.com" (full match)
// groups[1] = "user"
// groups[2] = "example"
// groups[3] = "com"

// Split by pattern
const ws_pattern = $Regex$compile("\\s+");
const words = $Regex$split(ws_pattern, "hello   world  foo");
// words = ["hello", "world", "foo"]

// Quick one-off operations (no need to compile)
if ($Regex$is_match("\\d+", "test123") == 1) {
    // Contains digits
}
const replaced = $Regex$replace_all("\\s+", "a  b   c", " ");
// replaced = "a b c"

// Escape special characters for literal matching
const escaped = $Regex$escape("hello.world*");  // "hello\\.world\\*"

// Cleanup
$Regex$free(pattern);
$Regex$free(email_pattern);
$Regex$free(ws_pattern);
```

## Regex Syntax

Uses Rust's `regex` crate syntax (similar to PCRE):
- `\d` - digit
- `\w` - word character
- `\s` - whitespace
- `.` - any character
- `*`, `+`, `?` - quantifiers
- `^`, `$` - anchors
- `[abc]` - character class
- `(...)` - capture group
- `(?:...)` - non-capturing group
- `(?i)` - case insensitive flag

## Performance Tips

- For patterns used multiple times, compile once with `$Regex$compile` and reuse
- Quick functions (`$Regex$is_match`, `$Regex$replace_all`) compile the pattern each call
- Free compiled patterns with `$Regex$free` when done

## Capture Groups

- Group 0 is always the full match
- Groups 1+ are the parenthesized captures from left to right
- `$Regex$capture` returns empty string if group doesn't exist

## Memory Management

All functions returning `StringPtr` or `ArrayPtr` allocate memory. Caller must free.

## Dependencies

- `zrtl` - Core ZRTL SDK
- `regex` - Regular expression engine
