# Zyntax Compiler - Development Backlog

> **Quick reference for active development tasks**
>
> For detailed roadmap see: [ROADMAP.md](ROADMAP.md)

---

## 🔥 Immediate Next Tasks (Ready to Start)

### 1. String Concatenation with Type Conversion (Bug Fix)
**Priority**: HIGH | **Effort**: Medium | **Impact**: Type-aware features

**Problem**: `"x = " + x` where `x` is Int/Float/Bool doesn't work correctly
- String concat calls getting only 1 argument instead of 2
- Haxe's implicit toString() creating expressions HirBuilder doesn't handle

**Investigation Needed**:
- [ ] Identify what TypedExpr Haxe generates for implicit toString()
- [ ] Add handling in HirBuilder for implicit conversion expressions
- [ ] Test with `trace("value: " + intVar)`

**Files to Modify**:
- `src/zyntax/StringOperatorTransformer.hx` - Already has toString detection
- `src/zyntax/HirBuilder.hx` - Needs to handle conversion expressions

---

### 2. Implement PHI Nodes for If-Else (Feature)
**Priority**: CRITICAL | **Effort**: High | **Impact**: Unlocks control flow

**Goal**: Make if-else statements executable

**Current State**:
- `TIf` compiles to AST ✅
- HIR builder returns null (line 281-282) ❌

**Tasks**:
- [ ] Research SSA form and PHI nodes
- [ ] Design PHI node representation in HIR
- [ ] Implement basic block creation for if/else branches
- [ ] Add terminator instructions (branch, jump)
- [ ] Implement PHI node insertion at merge points
- [ ] Test with: `if (x > 0) { trace("yes"); } else { trace("no"); }`
- [ ] Test with variable assignment in branches

**Files to Create/Modify**:
- `src/zyntax/HIR.hx` - Add PHI node types
- `src/zyntax/HirBuilder.hx` - Implement if-else to HIR
- `crates/compiler/src/hir.rs` - Ensure Rust HIR matches
- `test/IfElseTest.hx` - Test cases

**References**:
- SSA form: https://en.wikipedia.org/wiki/Static_single-assignment_form
- LLVM PHI nodes: https://llvm.org/docs/LangRef.html#phi-instruction

---

### 3. Implement While Loops (Feature)
**Priority**: CRITICAL | **Effort**: High | **Impact**: Enables iteration

**Goal**: Make while loops executable

**Current State**:
- `TWhile` compiles to AST ✅
- HIR builder returns null (line 293-294) ❌

**Dependencies**: PHI nodes (task #2)

**Tasks**:
- [ ] Design loop header and backedge blocks
- [ ] Implement loop PHI nodes for loop variables
- [ ] Add loop terminator instructions
- [ ] Handle break/continue (if supported)
- [ ] Test with: `var i = 0; while (i < 10) { trace(i); i++; }`
- [ ] Test with nested loops

**Files to Modify**:
- `src/zyntax/HirBuilder.hx` - Implement while to HIR
- `test/WhileLoopTest.hx` - Test cases

---

## 🎯 Short-term Tasks (Next 2-4 weeks)

### 4. Implement Unary Operators
**Priority**: HIGH | **Effort**: Low | **Impact**: Common operations

**Missing Operators**:
- Negation: `-x`
- Boolean NOT: `!flag`
- Increment: `++x`, `x++`
- Decrement: `--x`, `x--`

**Tasks**:
- [ ] Add `TUnop` case to ZyntaxCompiler
- [ ] Map unary ops to HIR operations
- [ ] Distinguish pre/post increment
- [ ] Test all unary operators

**Files to Modify**:
- `src/zyntax/ZyntaxCompiler.hx` - Add TUnop handling
- `src/zyntax/HirBuilder.hx` - Add unary op cases

---

### 5. Implement For Loops
**Priority**: HIGH | **Effort**: Medium | **Impact**: Standard iteration

**Dependencies**: While loops (task #3)

**Tasks**:
- [ ] Add `TFor` case to ZyntaxCompiler
- [ ] Desugar `for (i in 0...10)` to while loop
- [ ] Support array iteration `for (x in arr)`
- [ ] Test range iteration
- [ ] Test array iteration

**Test Cases**:
```haxe
for (i in 0...10) { trace(i); }
for (x in [1,2,3]) { trace(x); }
```

---

### 6. Add Bitwise Operators
**Priority**: MEDIUM | **Effort**: Low | **Impact**: Specialized use

**Note**: HIR already has bitwise ops (And, Or, Xor, Shl, Shr), just need mapping

**Tasks**:
- [ ] Map `OpAnd` → HIR `And`
- [ ] Map `OpOr` → HIR `Or`
- [ ] Map `OpXor` → HIR `Xor`
- [ ] Map `OpShl` → HIR `Shl`
- [ ] Map `OpShr` → HIR `Shr`
- [ ] Map `OpUShr` → unsigned shift
- [ ] Test all bitwise operations

**Files to Modify**:
- `src/zyntax/ZyntaxCompiler.hx` - Add bitwise op mappings

---

## 🏗️ Medium-term Tasks (1-2 months)

### 7. Instance Fields and Methods
**Priority**: HIGH | **Effort**: High | **Impact**: Enables OOP

**Goal**: Support classes with instance data and methods

**Tasks**:
- [ ] Design struct types in HIR
- [ ] Implement field access (get/set)
- [ ] Pass `this` pointer to instance methods
- [ ] Support constructors (`new()`)
- [ ] Allocate memory for instances
- [ ] Test simple classes

**Test Case**:
```haxe
class Point {
    var x: Int;
    var y: Int;

    function new(x: Int, y: Int) {
        this.x = x;
        this.y = y;
    }

    function distance(): Float {
        return Math.sqrt(x*x + y*y);
    }
}
```

---

### 8. Type Inference Improvements
**Priority**: MEDIUM | **Effort**: Medium | **Impact**: Better DX

**Current Issues**:
- Binary ops default to I32 (TODO: line 223)
- Call results default to I32 (TODO: line 253)

**Tasks**:
- [ ] Implement proper type inference for binary operations
- [ ] Infer function return types from signature
- [ ] Propagate types through expressions
- [ ] Test with complex expressions

---

### 9. Switch/Case Pattern Matching
**Priority**: HIGH | **Effort**: High | **Impact**: Powerful control flow

**Dependencies**: If-else (task #2)

**Tasks**:
- [ ] Add `TSwitch` case to ZyntaxCompiler
- [ ] Implement literal pattern matching
- [ ] Support enum patterns (when enums work)
- [ ] Add guard support (`case x if (x > 0)`)
- [ ] Desugar to if-else chain or jump table
- [ ] Test various patterns

---

### 10. Maps and Dictionaries
**Priority**: HIGH | **Effort**: High | **Impact**: Essential data structure

**Tasks**:
- [ ] Design Map<K,V> type in HIR
- [ ] Implement hash function
- [ ] Implement get/set/has operations
- [ ] Support iteration over keys/values
- [ ] Test with Int and String keys
- [ ] Add to runtime library

---

## 🔬 Research Tasks (Exploratory)

### R1. Investigate Haxe Implicit Conversions
**Goal**: Understand what Haxe generates for `"str" + intVar`

**Tasks**:
- [ ] Write test that dumps TypedExpr structure
- [ ] Identify what expression type Haxe uses for implicit toString
- [ ] Document findings
- [ ] Design solution

---

### R2. Memory Management Strategy
**Goal**: Decide on GC approach

**Options**:
1. Reference counting (simple, predictable)
2. Mark-and-sweep (more general)
3. Generational GC (high performance)
4. Manual with better API (current + improvements)

**Tasks**:
- [ ] Research GC strategies for native languages
- [ ] Evaluate performance tradeoffs
- [ ] Prototype reference counting
- [ ] Make decision and document

---

### R3. LLVM Backend Investigation
**Goal**: Evaluate LLVM as alternative to Cranelift

**Pros**: Better optimization, wider platform support
**Cons**: Slower compile times, larger dependency

**Tasks**:
- [ ] Set up LLVM-sys Rust bindings
- [ ] Prototype HIR → LLVM IR translation
- [ ] Benchmark compile time vs Cranelift
- [ ] Benchmark runtime performance
- [ ] Document findings

---

## 🐛 Bug Fixes

### B1. String Concatenation Type Conversion
**Status**: In Progress
**See**: Task #1 above

### B2. UTF-8 String Support Verification
**Priority**: MEDIUM

**Issue**: Runtime comment says "simplified to ASCII" but unclear if true

**Tasks**:
- [ ] Write test with emoji and international characters
- [ ] Verify byte-level handling
- [ ] Fix if broken
- [ ] Document UTF-8 support status

---

### B3. Variable Scope Edge Cases
**Priority**: LOW

**Potential Issues**:
- Shadowing in nested blocks
- Closure captures (when implemented)
- Loop variable scoping

**Tasks**:
- [ ] Write comprehensive scope tests
- [ ] Verify variable shadowing works
- [ ] Test edge cases

---

## 📝 Documentation Tasks

### D1. HIR Specification
**Priority**: HIGH | **Effort**: Medium

**Goal**: Document HIR format completely

**Tasks**:
- [ ] Document all HIR types
- [ ] Document all instructions
- [ ] Document type system
- [ ] Add examples
- [ ] Publish as markdown

---

### D2. ZBC Format Specification
**Priority**: HIGH | **Effort**: Low

**Goal**: Document bytecode format

**Tasks**:
- [ ] Document 44-byte header format
- [ ] Document JSON payload schema
- [ ] Document versioning strategy
- [ ] Add examples

---

### D3. Developer Contribution Guide
**Priority**: MEDIUM | **Effort**: Low

**Tasks**:
- [ ] Write CONTRIBUTING.md
- [ ] Document build process
- [ ] Explain codebase structure
- [ ] List good first issues
- [ ] Add code style guide

---

## 🎯 Quick Wins (Low effort, good impact)

1. **Add more array tests** - Effort: Very Low | Impact: Medium
   - Test all array methods in runtime
   - Verify type safety

2. **Add more string tests** - Effort: Very Low | Impact: Medium
   - Test all string methods
   - Test edge cases

3. **Better error messages** - Effort: Low | Impact: High
   - Add source positions to errors
   - Make compiler errors clearer

4. **Add benchmarks** - Effort: Low | Impact: Medium
   - Create performance test suite
   - Track compile time
   - Track runtime performance

5. **Assignment operators** - Effort: Low | Impact: Medium
   - Implement `+=`, `-=`, `*=`, `/=`, `%=`
   - Desugar to `x = x + y`

---

## 📊 Progress Tracking

### Completed ✅
- Basic types (Int, Float, Bool, String)
- Arithmetic operators
- Comparison operators
- Static functions
- Variable scope tracking
- Type-aware trace()
- Array runtime library
- String runtime library
- ZBC bytecode format
- External function registry

### In Progress 🚧
- String concatenation with type conversion (debugging)

### Blocked ⛔
- Instance methods (blocked on struct types)
- For loops (blocked on while loops)
- Switch/case (blocked on if-else)
- Closures (blocked on instance methods)

---

## 🗓️ Sprint Planning Template

### Sprint N: [Date Range]

**Goal**: [Main objective]

**Tasks**:
- [ ] Task 1
- [ ] Task 2
- [ ] Task 3

**Tests to Pass**:
- [ ] Test 1
- [ ] Test 2

**Stretch Goals**:
- [ ] Nice-to-have 1

---

## 💡 Ideas / Future Considerations

- **Compile-time evaluation** - Constant folding at HIR level
- **SIMD support** - Vector operations in HIR
- **Parallel compilation** - Multi-threaded HIR building
- **Incremental compilation** - Cache compiled functions
- **Hot reload** - JIT function replacement
- **REPL** - Interactive shell for Zyntax
- **Language server** - IDE integration
- **Package manager** - Dependency management
- **Cross-compilation** - Target multiple platforms

---

**Last Updated**: 2025-11-15
**Next Review**: When starting new sprint or completing major milestone
