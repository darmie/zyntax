# Haxe Runtime for Zyntax

Comprehensive native runtime implementation for Haxe targeting Zyntax. This runtime provides efficient implementations of Haxe's core types and operations compiled to native code.

## Architecture

The runtime is implemented in Rust and compiled to native code that is statically linked into the Zyntax CLI. All symbols are automatically registered via Zyntax's plugin system.

### Modules

- **array**: Dynamic arrays with automatic growth and shrinking
- **string**: Immutable UTF-8 strings with manipulation operations

### Memory Layout

#### Array Layout
```
[capacity: i32][length: i32][elem0][elem1][elem2]...
```
- `capacity`: Total allocated slots (including 2-word header)
- `length`: Number of elements currently stored
- Elements stored contiguously after header

#### String Layout
```
[length: i32][char0][char1]...[charN]['\0']
```
- `length`: Number of characters
- Characters stored as UTF-8 bytes
- Null-terminated for C interop

## API Reference

### Array Operations

#### Creation

| Function | Signature | Description |
|----------|-----------|-------------|
| `$Array$new` | `() -> *mut i32` | Create empty array with default capacity |
| `$Array$create` | `(i32, i32) -> *mut i32` | Create array from two elements |
| `$Array$fromElements` | `(*const i32, i32) -> *mut i32` | Create array from element list |

#### Access

| Function | Signature | Description |
|----------|-----------|-------------|
| `$Array$get` | `(*const i32, i32) -> i32` | Get element at index (0 if out of bounds) |
| `$Array$set` | `(*mut i32, i32, i32) -> i32` | Set element at index (returns 1 on success) |
| `$Array$length` | `(*const i32) -> i32` | Get array length |

#### Modification

| Function | Signature | Description |
|----------|-----------|-------------|
| `$Array$push` | `(*mut i32, i32) -> *mut i32` | Append element (may reallocate) |
| `$Array$pop` | `(*mut i32) -> i32` | Remove and return last element |
| `$Array$shift` | `(*mut i32) -> i32` | Remove and return first element |
| `$Array$unshift` | `(*mut i32, i32) -> *mut i32` | Prepend element (may reallocate) |
| `$Array$insert` | `(*mut i32, i32, i32) -> *mut i32` | Insert element at index |
| `$Array$remove` | `(*mut i32, i32) -> i32` | Remove element at index |

#### Queries

| Function | Signature | Description |
|----------|-----------|-------------|
| `$Array$indexOf` | `(*const i32, i32) -> i32` | Find index of element (-1 if not found) |
| `$Array$contains` | `(*const i32, i32) -> i32` | Check if contains element (1/0) |

#### Utilities

| Function | Signature | Description |
|----------|-----------|-------------|
| `$Array$reverse` | `(*mut i32)` | Reverse array in place |
| `$Array$copy` | `(*const i32) -> *mut i32` | Create shallow copy |
| `$Array$free` | `(*mut i32)` | Free array memory |

### String Operations

#### Creation

| Function | Signature | Description |
|----------|-----------|-------------|
| `$String$fromCString` | `(*const u8) -> *mut i32` | Create from C string |
| `$String$fromBytes` | `(*const u8, i32) -> *mut i32` | Create from bytes with length |
| `$String$empty` | `() -> *mut i32` | Create empty string |
| `$String$fromChar` | `(u8) -> *mut i32` | Create from single character |
| `$String$fromInt` | `(i32) -> *mut i32` | Convert integer to string |
| `$String$fromFloat` | `(f64) -> *mut i32` | Convert float to string |

#### Access

| Function | Signature | Description |
|----------|-----------|-------------|
| `$String$length` | `(*const i32) -> i32` | Get string length |
| `$String$charAt` | `(*const i32, i32) -> u8` | Get character at index |
| `$String$charCodeAt` | `(*const i32, i32) -> i32` | Get character code at index |
| `$String$toCString` | `(*const i32) -> *const u8` | Get C string pointer |

#### Manipulation

| Function | Signature | Description |
|----------|-----------|-------------|
| `$String$concat` | `(*const i32, *const i32) -> *mut i32` | Concatenate two strings |
| `$String$substring` | `(*const i32, i32, i32) -> *mut i32` | Extract substring (start, end) |
| `$String$substr` | `(*const i32, i32, i32) -> *mut i32` | Extract substring (start, length) |
| `$String$toLowerCase` | `(*const i32) -> *mut i32` | Convert to lowercase |
| `$String$toUpperCase` | `(*const i32) -> *mut i32` | Convert to uppercase |

#### Searching

| Function | Signature | Description |
|----------|-----------|-------------|
| `$String$indexOf` | `(*const i32, *const i32, i32) -> i32` | Find substring from start |
| `$String$lastIndexOf` | `(*const i32, *const i32, i32) -> i32` | Find substring from end |

#### Utilities

| Function | Signature | Description |
|----------|-----------|-------------|
| `$String$equals` | `(*const i32, *const i32) -> i32` | Compare strings (1/0) |
| `$String$copy` | `(*const i32) -> *mut i32` | Create copy |
| `$String$free` | `(*mut i32)` | Free string memory |

## Usage Examples

### Array Operations

```haxe
// Haxe source
var arr = [10, 20];
arr.push(30);
trace(arr[0]); // 10
trace(arr.length); // 3
```

Generated HIR calls:
```
arr = call $Array$create(10, 20)
arr = call $Array$push(arr, 30)
value = call $Array$get(arr, 0)
len = call $Array$length(arr)
```

### String Operations

```haxe
// Haxe source
var str = "Hello";
var str2 = str + " World";
trace(str2.length); // 11
trace(str2.substring(0, 5)); // "Hello"
```

Generated HIR calls:
```
str = call $String$fromCString("Hello")
world = call $String$fromCString(" World")
str2 = call $String$concat(str, world)
len = call $String$length(str2)
sub = call $String$substring(str2, 0, 5)
```

## Memory Management

### Ownership Model

- Arrays and strings are heap-allocated via `libc::malloc`
- Functions that may reallocate (push, unshift, insert) return new pointers
- Caller must update references when reallocation occurs
- Memory must be explicitly freed with `$Array$free` or `$String$free`

### Reallocation

Functions that return `*mut i32` may reallocate:

```rust
// CORRECT - Update reference
arr = Array_push(arr, value);

// WRONG - Using stale pointer after potential reallocation
Array_push(arr, value); // arr may now be invalid!
```

### Memory Leaks

The frontend is responsible for calling `free` functions:

```haxe
// Haxe manages this automatically via GC
// But for manual management:
var arr = [1, 2, 3];
// ... use arr
// arr.free(); // When done
```

## Performance Characteristics

### Array

| Operation | Complexity | Notes |
|-----------|-----------|--------|
| get/set | O(1) | Direct indexing |
| push | O(1) amortized | Doubles capacity when full |
| pop | O(1) | No reallocation |
| shift/unshift | O(n) | Requires shifting elements |
| insert/remove | O(n) | Requires shifting elements |
| indexOf | O(n) | Linear search |
| reverse | O(n) | In-place reversal |
| copy | O(n) | Full memory copy |

### String

| Operation | Complexity | Notes |
|-----------|-----------|--------|
| length | O(1) | Stored in header |
| charAt | O(1) | Direct indexing |
| concat | O(n+m) | Allocates new string |
| substring | O(k) | Allocates k-length string |
| indexOf | O(n*m) | Naive search |
| toLowerCase/Upper | O(n) | Allocates new string |

## Implementation Details

### Array Growth Strategy

Arrays use exponential growth (doubling) when capacity is exceeded:

```rust
if length + HEADER_SIZE >= capacity {
    new_capacity = capacity * 2;
    realloc(new_capacity);
}
```

Default initial capacity: 8 elements

### String Immutability

All string operations that modify content create new strings:

```rust
// concat always allocates
let result = String_concat(str1, str2); // New allocation

// toLowerCase allocates
let lower = String_toLowerCase(str); // New allocation
```

### Bounds Checking

All access operations include bounds checking:

```rust
// Array get
if index < 0 || index >= length {
    return 0; // Safe default
}

// String charAt
if index < 0 || index >= length {
    return 0; // Safe default
}
```

## Thread Safety

**⚠️ Not thread-safe**: The runtime does not use any synchronization primitives. Concurrent access to the same array/string from multiple threads will cause data races.

For thread-safe operations, wrap accesses in appropriate synchronization (mutexes, etc.) at the Haxe level.

## Future Enhancements

Planned additions:

1. **Map/Dictionary** - Hash table implementation
2. **Reference Counting** - Automatic memory management
3. **StringBuf** - Mutable string buffer
4. **Regex** - Regular expression support
5. **Date/Time** - Temporal operations
6. **Math** - Transcendental functions
7. **Bytes** - Binary data manipulation

## Plugin Registration

All symbols are automatically registered via the plugin system:

```rust
runtime_plugin! {
    name: "haxe",
}

#[runtime_export("$Array$push")]
pub extern "C" fn Array_push(...) { ... }
```

No manual registration required - the inventory crate collects all `#[runtime_export]` symbols at compile time.

## Building

```bash
cd reflaxe.zyntax/runtime
cargo build --release
```

The runtime is automatically linked into the Zyntax CLI.

## Testing

Run the test suite:

```bash
# Compile Haxe test
cd reflaxe.zyntax/test
haxe build.hxml

# Run with Zyntax
cd ../..
./target/release/zyntax compile reflaxe.zyntax/test/output/HelloWorld.json --run
```

## License

Same as Zyntax project.
