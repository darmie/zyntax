# Zyntax Compiler Roadmap

> **Status**: Early Development - Core features working, expanding language coverage
>
> **Last Updated**: 2025-11-15

---

## 🎯 Current Status

### ✅ What Works Today

**Core Language Features:**
- ✅ Primitive types (Int, Float, Bool, String, Void)
- ✅ Variables and constants (`var x = 10`)
- ✅ Arithmetic operators (`+`, `-`, `*`, `/`, `%`)
- ✅ Comparison operators (`==`, `!=`, `<`, `<=`, `>`, `>=`)
- ✅ Logical operators (`&&`, `||`)
- ✅ Static functions with parameters and return values
- ✅ Function calls (direct invocation)
- ✅ Expression blocks
- ✅ Return statements
- ✅ External functions via `@:native` FFI
- ✅ Type-aware `trace()` debugging (Int, Float, Bool, String)

**Data Structures:**
- ✅ Arrays - comprehensive runtime (create, get, set, push, pop, shift, unshift, indexOf, contains, reverse, copy)
- ✅ Strings - strong support (charAt, charCodeAt, substring, substr, toUpperCase, toLowerCase, indexOf, lastIndexOf, concatenation, equality)

**Runtime Library:**
- ✅ Memory allocation (malloc/free)
- ✅ String operations (12+ functions)
- ✅ Array operations (12+ functions)
- ✅ Type conversions (toString for Int/Float/Bool)
- ✅ Debug output (type-aware println)

**Compilation Pipeline:**
- ✅ Haxe → Reflaxe → Zyntax AST → HIR → Cranelift IR → JIT execution
- ✅ ZBC (Zyntax Bytecode) format with 44-byte header
- ✅ Expression preprocessing (TraceTransformer, StringOperatorTransformer)
- ✅ Variable scope tracking
- ✅ External function registry

---

## 🚧 Priority 1: Control Flow (Critical Path)

**Goal**: Enable if/else and while loops to unlock real-world programs

### P1.1 - PHI Nodes for SSA Form
- **Status**: ❌ Not Started
- **Blocker**: HIR builder returns `null` for if/else (line 281-282 in HirBuilder.hx)
- **Description**: Implement Static Single Assignment form with PHI nodes for merging control flow paths
- **Impact**: 🔥 HIGH - Unlocks ~60% more real programs
- **Tasks**:
  - [ ] Design PHI node representation in HIR
  - [ ] Implement basic block branching in HIR
  - [ ] Add CFG (Control Flow Graph) construction
  - [ ] Handle variable merging at join points
  - [ ] Test with simple if-else cases
  - [ ] Test with nested if-else
  - [ ] Add ternary operator support (`? :`)

**Test Cases to Pass**:
```haxe
// Simple if-else
if (x > 0) {
    trace("positive");
} else {
    trace("negative");
}

// Nested conditions
if (x > 0) {
    if (y > 0) {
        trace("both positive");
    }
}

// If with variable assignment
var result;
if (condition) {
    result = 10;
} else {
    result = 20;
}
```

### P1.2 - While Loop Implementation
- **Status**: ❌ Not Started
- **Blocker**: HIR builder returns `null` for while (line 293-294 in HirBuilder.hx)
- **Description**: Implement loop control flow with back edges
- **Impact**: 🔥 HIGH - Enables iteration
- **Tasks**:
  - [ ] Design loop header and backedge blocks
  - [ ] Implement loop PHI nodes for induction variables
  - [ ] Add break/continue support
  - [ ] Handle loop-carried dependencies
  - [ ] Test with simple counter loops
  - [ ] Test with nested loops
  - [ ] Add do-while support

**Test Cases to Pass**:
```haxe
// Simple while loop
var i = 0;
while (i < 10) {
    trace(i);
    i++;
}

// Nested loops
var i = 0;
while (i < 3) {
    var j = 0;
    while (j < 3) {
        trace(i * 3 + j);
        j++;
    }
    i++;
}

// While with break/continue
while (true) {
    if (condition) break;
    if (other) continue;
    doWork();
}
```

### P1.3 - For Loop Support
- **Status**: ❌ Not Started
- **Blocker**: `TFor` not implemented in ZyntaxCompiler
- **Description**: Add for-in loops over ranges and iterables
- **Impact**: 🔥 HIGH - Standard iteration pattern
- **Tasks**:
  - [ ] Implement `TFor` → AST conversion
  - [ ] Support integer ranges (`for (i in 0...10)`)
  - [ ] Support array iteration (`for (x in arr)`)
  - [ ] Desugar for loops to while loops
  - [ ] Test with range iterations
  - [ ] Test with array iterations

**Test Cases to Pass**:
```haxe
// Range iteration
for (i in 0...10) {
    trace(i);
}

// Array iteration
var arr = [1, 2, 3];
for (x in arr) {
    trace(x);
}
```

---

## 🎨 Priority 2: Object-Oriented Programming

**Goal**: Support instance-based classes, fields, and methods

### P2.1 - Instance Fields
- **Status**: ❌ Not Started
- **Description**: Support class instance variables
- **Impact**: 🔥 HIGH - Core OOP feature
- **Tasks**:
  - [ ] Add struct types to HIR
  - [ ] Implement field access codegen
  - [ ] Support field initialization
  - [ ] Handle `this` context
  - [ ] Test with simple classes

**Test Cases to Pass**:
```haxe
class Point {
    var x: Int;
    var y: Int;

    function new(x: Int, y: Int) {
        this.x = x;
        this.y = y;
    }
}
```

### P2.2 - Instance Methods
- **Status**: ❌ Not Started
- **Description**: Support methods that operate on instance data
- **Impact**: 🔥 HIGH - Core OOP feature
- **Tasks**:
  - [ ] Pass `this` pointer as first argument
  - [ ] Implement method dispatch
  - [ ] Support method calls on instances
  - [ ] Test with simple methods

**Test Cases to Pass**:
```haxe
class Counter {
    var count: Int;

    function increment() {
        count++;
    }

    function getValue(): Int {
        return count;
    }
}
```

### P2.3 - Constructors
- **Status**: ❌ Not Started
- **Description**: Support `new()` for object instantiation
- **Impact**: 🔥 HIGH - Object creation
- **Tasks**:
  - [ ] Implement `TNew` → AST conversion
  - [ ] Allocate memory for instances
  - [ ] Call constructor with parameters
  - [ ] Initialize fields
  - [ ] Return object pointer

### P2.4 - Inheritance
- **Status**: ❌ Not Started
- **Description**: Support class inheritance and method overriding
- **Impact**: 🟡 MEDIUM - Advanced OOP
- **Tasks**:
  - [ ] Implement vtable for dynamic dispatch
  - [ ] Support `extends` keyword
  - [ ] Handle `super` calls
  - [ ] Support method overriding
  - [ ] Test inheritance hierarchies

---

## 🔧 Priority 3: Type System Improvements

### P3.1 - Type Inference
- **Status**: ⚠️ Partial - TODO at line 223, 253 in HirBuilder.hx
- **Description**: Proper type inference for expressions
- **Impact**: 🟡 MEDIUM - Better developer experience
- **Tasks**:
  - [ ] Implement type inference for binary operations
  - [ ] Infer function call return types
  - [ ] Support generic type inference
  - [ ] Handle type constraints
  - [ ] Test with complex expressions

### P3.2 - Null Safety
- **Status**: ❌ Not Started
- **Current**: Null represented as `0` pointer (line 129-131 in ZyntaxCompiler.hx)
- **Description**: Implement Option types and null checking
- **Impact**: 🟡 MEDIUM - Safety and correctness
- **Tasks**:
  - [ ] Design Option<T> representation
  - [ ] Implement null coalescing operator (`??`)
  - [ ] Add nullable type checking
  - [ ] Support safe navigation (`?.`)
  - [ ] Test null safety scenarios

### P3.3 - Generics
- **Status**: ❌ Not Started
- **Description**: Support generic types and functions
- **Impact**: 🟢 LOW - Advanced feature
- **Tasks**:
  - [ ] Implement monomorphization
  - [ ] Support generic type parameters
  - [ ] Handle type constraints
  - [ ] Support generic functions
  - [ ] Test with generic collections

---

## 🔨 Priority 4: Missing Operators & Expressions

### P4.1 - Unary Operators
- **Status**: ❌ Not Started
- **Description**: Implement negation, increment, decrement, boolean not
- **Impact**: 🟡 MEDIUM - Common operations
- **Tasks**:
  - [ ] Implement negation (`-x`)
  - [ ] Implement boolean not (`!x`)
  - [ ] Implement pre-increment (`++x`)
  - [ ] Implement post-increment (`x++`)
  - [ ] Implement pre-decrement (`--x`)
  - [ ] Implement post-decrement (`x--`)

**Test Cases to Pass**:
```haxe
var x = 10;
var neg = -x;      // -10
var notFlag = !true;  // false
x++;               // 11
++x;               // 12
```

### P4.2 - Bitwise Operators
- **Status**: ❌ Not Started (HIR has ops, not mapped from Haxe)
- **Description**: Implement bitwise operations
- **Impact**: 🟢 LOW - Specialized use cases
- **Tasks**:
  - [ ] Map `OpAnd` → HIR `And`
  - [ ] Map `OpOr` → HIR `Or`
  - [ ] Map `OpXor` → HIR `Xor`
  - [ ] Map `OpShl` → HIR `Shl`
  - [ ] Map `OpShr` → HIR `Shr`
  - [ ] Test bitwise operations

### P4.3 - Assignment Operators
- **Status**: ❌ Not Started
- **Description**: Compound assignment operators
- **Impact**: 🟢 LOW - Convenience
- **Tasks**:
  - [ ] Implement `+=`, `-=`, `*=`, `/=`, `%=`
  - [ ] Implement `&=`, `|=`, `^=`
  - [ ] Implement `<<=`, `>>=`
  - [ ] Test all assignment operators

---

## 🗃️ Priority 5: Data Structures & Collections

### P5.1 - Maps/Dictionaries
- **Status**: ❌ Not Started
- **Description**: Hash map implementation
- **Impact**: 🔥 HIGH - Essential data structure
- **Tasks**:
  - [ ] Design Map<K, V> type in HIR
  - [ ] Implement hash function
  - [ ] Implement get/set operations
  - [ ] Support iteration
  - [ ] Test with various key types

### P5.2 - Sets
- **Status**: ❌ Not Started
- **Description**: Hash set implementation
- **Impact**: 🟡 MEDIUM - Common data structure
- **Tasks**:
  - [ ] Design Set<T> type
  - [ ] Implement add/remove/contains
  - [ ] Support set operations (union, intersection)
  - [ ] Test with various types

### P5.3 - Enums
- **Status**: ❌ Not Started
- **Blocker**: `compileEnumImpl` returns `null` (line 107-109 in ZyntaxCompiler.hx)
- **Description**: Algebraic data types / tagged unions
- **Impact**: 🔥 HIGH - Powerful type system feature
- **Tasks**:
  - [ ] Implement enum type compilation
  - [ ] Support enum constructors
  - [ ] Implement pattern matching on enums
  - [ ] Support enum with associated data
  - [ ] Test with option types

**Test Cases to Pass**:
```haxe
enum Color {
    Red;
    Green;
    Blue;
    RGB(r: Int, g: Int, b: Int);
}

enum Option<T> {
    None;
    Some(value: T);
}
```

---

## 🎭 Priority 6: Pattern Matching & Advanced Control Flow

### P6.1 - Switch/Match Statements
- **Status**: ❌ Not Started
- **Description**: Pattern matching with switch
- **Impact**: 🔥 HIGH - Powerful control flow
- **Tasks**:
  - [ ] Implement `TSwitch` → AST conversion
  - [ ] Support literal patterns
  - [ ] Support enum patterns
  - [ ] Support guards
  - [ ] Implement exhaustiveness checking
  - [ ] Test with various patterns

**Test Cases to Pass**:
```haxe
switch (value) {
    case 0: trace("zero");
    case 1: trace("one");
    case n if (n > 0): trace("positive");
    default: trace("other");
}

switch (color) {
    case Red: trace("red");
    case RGB(r, g, b): trace('RGB: $r, $g, $b');
}
```

### P6.2 - Try-Catch-Finally
- **Status**: ❌ Not Started
- **Description**: Exception handling
- **Impact**: 🟡 MEDIUM - Error handling
- **Tasks**:
  - [ ] Design exception representation
  - [ ] Implement try-catch blocks
  - [ ] Support finally blocks
  - [ ] Implement throw statements
  - [ ] Add stack unwinding
  - [ ] Test error propagation

---

## 🧩 Priority 7: Functional Programming Features

### P7.1 - Closures & Lambdas
- **Status**: ❌ Not Started
- **Description**: Anonymous functions and closures
- **Impact**: 🔥 HIGH - Functional programming
- **Tasks**:
  - [ ] Implement lambda syntax support
  - [ ] Capture environment variables
  - [ ] Generate closure structs
  - [ ] Support higher-order functions
  - [ ] Test with callbacks

**Test Cases to Pass**:
```haxe
var add = (a, b) -> a + b;
var result = add(5, 3);

var numbers = [1, 2, 3, 4, 5];
var doubled = numbers.map(x -> x * 2);
```

### P7.2 - First-Class Functions
- **Status**: ❌ Not Started
- **Description**: Functions as values
- **Impact**: 🟡 MEDIUM - Functional patterns
- **Tasks**:
  - [ ] Support function types in HIR
  - [ ] Implement function pointers
  - [ ] Support function passing
  - [ ] Test with callbacks

### P7.3 - Array Higher-Order Functions
- **Status**: ❌ Not Started
- **Description**: map, filter, reduce, etc.
- **Impact**: 🟡 MEDIUM - Functional data processing
- **Tasks**:
  - [ ] Implement Array.map()
  - [ ] Implement Array.filter()
  - [ ] Implement Array.reduce()
  - [ ] Implement Array.forEach()
  - [ ] Test with lambdas

---

## 🌐 Priority 8: String & Unicode Support

### P8.1 - Full UTF-8 Support
- **Status**: ⚠️ Partial - Runtime comment says "simplified to ASCII" (line 5 in string.rs)
- **Description**: Proper Unicode string handling
- **Impact**: 🟡 MEDIUM - Internationalization
- **Tasks**:
  - [ ] Verify UTF-8 encoding/decoding
  - [ ] Handle multi-byte characters correctly
  - [ ] Test with emoji and international text
  - [ ] Add Unicode normalization

### P8.2 - String Interpolation
- **Status**: ⚠️ Partial - String concatenation with type conversion needs work
- **Description**: Template strings with embedded expressions
- **Impact**: 🟢 LOW - Convenience
- **Tasks**:
  - [ ] Fix automatic toString() conversion in string concat
  - [ ] Support `'text ${expr}'` syntax
  - [ ] Handle complex expressions in interpolation
  - [ ] Test with various types

**Test Cases to Pass**:
```haxe
var name = "World";
var count = 42;
trace('Hello, ${name}! Count: ${count}');
```

---

## 🧠 Priority 9: Memory Management

### P9.1 - Garbage Collection
- **Status**: ❌ Not Started
- **Current**: Manual malloc/free
- **Description**: Automatic memory management
- **Impact**: 🔥 HIGH - Safety and ergonomics
- **Options**:
  - Reference counting (simple, predictable)
  - Mark-and-sweep GC (more general)
  - Generational GC (high performance)
- **Tasks**:
  - [ ] Design GC strategy
  - [ ] Implement allocator integration
  - [ ] Add root scanning
  - [ ] Handle cycles (if using refcounting)
  - [ ] Test memory safety

### P9.2 - Reference Counting
- **Status**: ❌ Not Started
- **Description**: Simple automatic memory management
- **Impact**: 🟡 MEDIUM - Deterministic cleanup
- **Tasks**:
  - [ ] Add ref count to heap allocations
  - [ ] Implement retain/release operations
  - [ ] Handle strong/weak references
  - [ ] Detect and break cycles
  - [ ] Test memory leaks

---

## 🚀 Priority 10: Advanced Features

### P10.1 - Async/Await
- **Status**: ❌ Not Started
- **Note**: `is_async` flag exists in HIR but not implemented
- **Description**: Asynchronous programming support
- **Impact**: 🔥 HIGH - Modern programming patterns
- **Tasks**:
  - [ ] Design async runtime
  - [ ] Implement Future/Promise types
  - [ ] Transform async functions to state machines
  - [ ] Add await support
  - [ ] Test with concurrent operations

### P10.2 - Iterators
- **Status**: ❌ Not Started
- **Description**: Custom iteration support
- **Impact**: 🟡 MEDIUM - Data structure abstraction
- **Tasks**:
  - [ ] Design Iterator interface
  - [ ] Support `hasNext()` / `next()` protocol
  - [ ] Implement for collections
  - [ ] Test custom iterators

### P10.3 - Abstract Types
- **Status**: ⚠️ Partial - Converted to primitives only
- **Description**: Type abstractions with operator overloading
- **Impact**: 🟢 LOW - Advanced type system
- **Tasks**:
  - [ ] Preserve abstract type information
  - [ ] Support @:op operator overloading
  - [ ] Handle implicit casts
  - [ ] Test with numeric abstracts

---

## 🔬 Research & Optimization

### R1 - LLVM Backend (Alternative to Cranelift)
- **Status**: ❌ Not Started
- **Description**: LLVM backend for better optimization
- **Impact**: Performance improvement
- **Tasks**:
  - [ ] Design LLVM IR generation
  - [ ] Implement HIR → LLVM translation
  - [ ] Support optimization passes
  - [ ] Benchmark vs Cranelift
  - [ ] Add AOT compilation support

### R2 - Ahead-of-Time Compilation
- **Status**: ❌ Not Started
- **Current**: JIT-only
- **Description**: Generate standalone executables
- **Impact**: Deployment flexibility
- **Tasks**:
  - [ ] Implement object file generation
  - [ ] Add linker integration
  - [ ] Support executable output
  - [ ] Test standalone binaries

### R3 - Optimization Passes
- **Status**: ❌ Not Started
- **Description**: HIR-level optimizations
- **Impact**: Performance improvement
- **Tasks**:
  - [ ] Constant folding
  - [ ] Dead code elimination
  - [ ] Common subexpression elimination
  - [ ] Loop optimization
  - [ ] Inlining

### R4 - Debugging Support
- **Status**: ⚠️ Partial - trace() works
- **Description**: Full debugging with breakpoints
- **Impact**: Developer experience
- **Tasks**:
  - [ ] Generate DWARF debug info
  - [ ] Support source line mapping
  - [ ] Enable debugger integration (lldb/gdb)
  - [ ] Add stack traces on errors

---

## 📊 Feature Coverage Targets

### Short-term (Next 3-6 months)
- ✅ **60% coverage** - Control flow (if/else, while, for)
- ✅ **40% coverage** - Basic OOP (instance methods/fields)
- ✅ **20% coverage** - Pattern matching (switch/case)

### Mid-term (6-12 months)
- ✅ **80% coverage** - Full OOP (inheritance, interfaces)
- ✅ **60% coverage** - Functional features (closures, lambdas)
- ✅ **40% coverage** - Advanced types (generics, null safety)

### Long-term (12+ months)
- ✅ **95% coverage** - Full Haxe language support
- ✅ **80% coverage** - Advanced features (async/await, GC)
- ✅ **60% coverage** - Production-ready tooling

---

## 🐛 Known Issues

### Critical
1. **Control flow doesn't execute** - PHI nodes not implemented (P1.1)
2. **String concat with type conversion** - Haxe implicit conversions not handled correctly
3. **No instance-based OOP** - Only static functions work (P2.1-P2.3)

### High Priority
1. **Type inference incomplete** - Defaults to I32 (P3.1)
2. **No exception handling** - Try/catch not supported (P6.2)
3. **Manual memory management** - Risk of leaks (P9.1)

### Medium Priority
1. **UTF-8 support unclear** - May be ASCII-only (P8.1)
2. **No generics** - Limits code reuse (P3.3)
3. **Missing unary operators** - No negation, increment, etc. (P4.1)

---

## 📚 Documentation Needs

- [ ] API documentation for runtime library
- [ ] HIR specification document
- [ ] ZBC bytecode format specification
- [ ] Developer guide for contributing
- [ ] Examples and tutorials
- [ ] Language feature compatibility matrix
- [ ] Performance benchmarking guide

---

## 🎯 Success Metrics

### Phase 1: Foundation (Current → +3 months)
- ✅ All basic tests pass
- ✅ Control flow (if/while/for) working
- ✅ Can write simple algorithms
- ✅ 60% Haxe feature coverage

### Phase 2: Practical (3-6 months)
- ✅ OOP features working
- ✅ Can build utility libraries
- ✅ Pattern matching supported
- ✅ 75% Haxe feature coverage

### Phase 3: Production (6-12 months)
- ✅ Full language support
- ✅ Garbage collection
- ✅ Can build real applications
- ✅ 95% Haxe feature coverage

---

## 🤝 Contributing

This roadmap is a living document. Priority ordering may change based on:
- Community feedback
- Real-world use cases
- Technical dependencies
- Performance considerations

See `CONTRIBUTING.md` for guidelines on implementing roadmap items.

---

## 📝 Notes

- **JIT vs AOT**: Currently JIT-only via Cranelift. AOT support planned for R2.
- **Memory Model**: Currently manual malloc/free. GC planned for P9.1.
- **Type System**: Basic types working. Advanced features (generics, null safety) in P3.x.
- **Standard Library**: Arrays and strings have good coverage. More collections needed in P5.x.

**Last Test Results**: TypeAwareTrace test passes ✅ (demonstrates type-aware runtime operations work correctly)
