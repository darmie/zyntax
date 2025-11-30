# zrtl_json

JSON parsing and manipulation for Zyntax-based languages.

## Overview

Provides JSON parsing, serialization, and manipulation using an opaque handle-based API. JSON values are represented as handles (u64) that can be queried and modified.

## Type Constants

| Constant | Value | Description |
|----------|-------|-------------|
| `JSON_TYPE_NULL` | 0 | Null value |
| `JSON_TYPE_BOOL` | 1 | Boolean value |
| `JSON_TYPE_NUMBER` | 2 | Numeric value |
| `JSON_TYPE_STRING` | 3 | String value |
| `JSON_TYPE_ARRAY` | 4 | Array value |
| `JSON_TYPE_OBJECT` | 5 | Object value |

## Exported Symbols

### Parse & Stringify

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Json$parse` | `(StringPtr) -> u64` | Parse JSON string (0 on error) |
| `$Json$stringify` | `(u64) -> StringPtr` | Convert to compact JSON string |
| `$Json$stringify_pretty` | `(u64) -> StringPtr` | Convert to pretty-printed JSON |
| `$Json$free` | `(u64) -> ()` | Free a JSON handle |

### Type Checking

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Json$get_type` | `(u64) -> i32` | Get value type (0-5) |
| `$Json$is_null` | `(u64) -> i32` | Check if null |
| `$Json$is_bool` | `(u64) -> i32` | Check if boolean |
| `$Json$is_number` | `(u64) -> i32` | Check if number |
| `$Json$is_string` | `(u64) -> i32` | Check if string |
| `$Json$is_array` | `(u64) -> i32` | Check if array |
| `$Json$is_object` | `(u64) -> i32` | Check if object |

### Value Getters

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Json$get_bool` | `(u64) -> i32` | Get boolean value (0 if not bool) |
| `$Json$get_int` | `(u64) -> i64` | Get integer value (0 if not number) |
| `$Json$get_float` | `(u64) -> f64` | Get float value (0.0 if not number) |
| `$Json$get_string` | `(u64) -> StringPtr` | Get string value (empty if not string) |

### Object Access

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Json$get` | `(u64, StringPtr) -> u64` | Get value by key (0 if not found) |
| `$Json$has` | `(u64, StringPtr) -> i32` | Check if key exists |
| `$Json$keys` | `(u64) -> ArrayPtr` | Get all keys as string array |
| `$Json$set` | `(u64, StringPtr, u64) -> ()` | Set value by key |
| `$Json$remove` | `(u64, StringPtr) -> ()` | Remove key from object |

### Array Access

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Json$array_length` | `(u64) -> i64` | Get array length |
| `$Json$array_get` | `(u64, i64) -> u64` | Get value at index (0 if out of bounds) |
| `$Json$array_set` | `(u64, i64, u64) -> ()` | Set value at index |
| `$Json$array_push` | `(u64, u64) -> ()` | Append value to array |
| `$Json$array_pop` | `(u64) -> u64` | Remove and return last element |

### Value Constructors

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Json$null` | `() -> u64` | Create null value |
| `$Json$bool` | `(i32) -> u64` | Create boolean value |
| `$Json$int` | `(i64) -> u64` | Create integer value |
| `$Json$float` | `(f64) -> u64` | Create float value |
| `$Json$string` | `(StringPtr) -> u64` | Create string value |
| `$Json$array` | `() -> u64` | Create empty array |
| `$Json$object` | `() -> u64` | Create empty object |

### Path Access

| Symbol | Signature | Description |
|--------|-----------|-------------|
| `$Json$path_get` | `(u64, StringPtr) -> u64` | Get value by dot-separated path |

## Usage Example

```zig
// Parse JSON
const json_str = "{\"name\": \"Alice\", \"age\": 30}";
const doc = $Json$parse(json_str);

// Access values
const name_h = $Json$get(doc, "name");
const name = $Json$get_string(name_h);  // "Alice"
const age = $Json$get_int($Json$get(doc, "age"));  // 30

// Type checking
if ($Json$is_object(doc) == 1) {
    // It's an object
}

// Build JSON
const obj = $Json$object();
$Json$set(obj, "greeting", $Json$string("Hello"));
$Json$set(obj, "count", $Json$int(42));

const arr = $Json$array();
$Json$array_push(arr, $Json$int(1));
$Json$array_push(arr, $Json$int(2));
$Json$array_push(arr, $Json$int(3));
$Json$set(obj, "numbers", arr);

// Stringify
const output = $Json$stringify_pretty(obj);
// {
//   "greeting": "Hello",
//   "count": 42,
//   "numbers": [1, 2, 3]
// }

// Path access for nested data
const data = $Json$parse("{\"users\": [{\"name\": \"Bob\"}, {\"name\": \"Carol\"}]}");
const second_name = $Json$path_get(data, "users.1.name");
const name_str = $Json$get_string(second_name);  // "Carol"

// Cleanup
$Json$free(doc);
$Json$free(obj);
$Json$free(data);
```

## Path Access Syntax

The `$Json$path_get` function accepts dot-separated paths:
- `"foo.bar"` - Access nested object keys
- `"arr.0"` - Access array by index
- `"users.1.name"` - Combined access

## Memory Management

- Each parsed JSON document or constructed value returns a handle
- Use `$Json$free` to release handles when done
- Getting values from objects/arrays creates new handles that must also be freed
- String values returned by `$Json$get_string` must be freed with `string_free`

## Dependencies

- `zrtl` - Core ZRTL SDK
- `serde_json` - JSON parsing/serialization
