# ZynML Examples

This directory contains example ZynML programs demonstrating various language features.

## Abstract Types (`abstract_types.zynml`)

Demonstrates **zero-cost abstract types** with method dispatch and suffix literals.

### Features Showcased:

1. **Abstract Type Definitions**
   ```zynml
   abstract Duration(i64) with Suffixes("ms, s, m"):
       value: i64
   ```
   - Wraps a primitive type (i64) with type safety
   - Zero runtime overhead - compiles to raw i64 operations
   - Suffix literals for ergonomic usage

2. **Suffix Literals**
   ```zynml
   let delay = 1000ms  // Creates Duration from literal
   let time = 5s       // Different suffix, same type
   ```

3. **Method Dispatch**
   ```zynml
   impl Duration(i64) {
       fn to_ms(self) -> i64:
           self.value
   }

   delay.to_ms()  // Resolves to Duration$to_ms at compile time
   ```

4. **Inherent Methods**
   - Constructor methods: `from_ms`, `from_s`, `from_m`
   - Conversion methods: `to_ms`, `to_s`, `to_m`
   - All methods are statically dispatched with zero overhead

### How It Works:

- **Compile-time**: Method calls resolve to mangled function names (`Duration$to_ms`)
- **Runtime**: Abstract types are their underlying type (i64), accessed directly
- **Result**: Type safety + zero cost = best of both worlds

### Running the Example:

```bash
cargo build --release
./target/release/zynml run examples/zynml/abstract_types.zynml
```

### Expected Output:

```
1000
5000
5
120000
120
2
500
10000
10
```

## Implementation Details

### Type Safety
Abstract types prevent mixing incompatible units:
```zynml
let time: Duration = 1000ms  // OK
let number: i64 = 1000       // Different type
// time + number  // Would be type error (if operators implemented)
```

### Zero-Cost Abstraction
At the IR level, `Duration` is just `i64`:
- No wrapper struct in memory
- No vtables or runtime dispatch
- Direct integer operations
- Method calls inline to simple function calls

### Method Resolution
Method dispatch uses nominal typing:
1. Parse: `delay.to_ms()` creates MethodCall node with receiver type `Duration`
2. Resolve: Lookup finds `to_ms` in `Duration`'s inherent methods
3. Mangle: Generate name `Duration$to_ms`
4. Codegen: Direct function call, receiver passes as i64

This provides the ergonomics of OOP with the performance of procedural code.
