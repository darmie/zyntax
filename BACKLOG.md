# Zyntax Compiler - Development Backlog

**Last Updated**: November 14, 2025
**Current Status**: Production-Ready Core (98.6% tests passing, 280/284)

---

## Active Development

### 1. Reflaxe/Haxe Integration (HIGH PRIORITY)
**Goal**: Create `reflaxe.Zyntax` backend to tap into Haxe's mature ecosystem

**Tasks**:
- [ ] Set up Reflaxe project structure (`haxelib run reflaxe new zyntax`)
- [ ] Implement Haxe AST → Zyntax HIR translator
  - [ ] `compileClassImpl()` - Map classes to structs + trait impls
  - [ ] `compileEnumImpl()` - Map enums to Zyntax enums
  - [ ] `compileExpressionImpl()` - Map expressions to HIR
- [ ] Type system mapping (Haxe → Zyntax)
- [ ] Runtime bridge (GC semantics, stdlib calls, async interop)
- [ ] Test with Haxe standard library
- [ ] Performance benchmarking vs existing Haxe targets

**Benefits**:
- Instant access to thousands of production-tested libraries
- Validation at scale with real-world code
- Positioning as high-performance backend for Haxe

---

## Compiler Infrastructure

### 2. LLVM AOT Backend (MEDIUM PRIORITY)
**Status**: Framework exists, needs completion
**Location**: `crates/compiler/src/llvm_backend.rs`

**Remaining Work**:
- [ ] Complete instruction translation
- [ ] Implement optimization passes
- [ ] Generate executable binaries
- [ ] Cross-platform testing

### 3. Bytecode Interpreter (LOW PRIORITY)
**Status**: Specification complete
**Location**: `docs/BYTECODE_FORMAT_SPEC.md`

**Remaining Work**:
- [ ] Implement bytecode VM
- [ ] Serialization/deserialization
- [ ] JIT fallback integration

---

## Language Features

### 4. Exception Handling
**Status**: Planned, not started
**Design**: Try-catch-finally with panic/Result interop

**Tasks**:
- [ ] Design exception handling model (panic vs Result)
- [ ] Implement try/catch/finally in parser
- [ ] Add exception types to type system
- [ ] Generate exception handling HIR
- [ ] Cranelift backend support

### 5. Advanced Pattern Matching
**Status**: Basic patterns work, advanced features pending

**Pending Features**:
- [ ] Or patterns (`Some(x) | None`)
- [ ] Pattern guards (`if` conditions)
- [ ] Range patterns (`1..=10`)
- [ ] Slice patterns (`[first, .., last]`)

---

## Standard Library

### 6. Extended Collections
**Status**: Core collections complete (Vec, String, HashMap)

**Additions Needed**:
- [ ] HashSet<T>
- [ ] BTreeMap<K,V> / BTreeSet<T>
- [ ] LinkedList<T>
- [ ] VecDeque<T>

### 7. I/O and File System
**Status**: Not started

**Tasks**:
- [ ] File operations (read, write, open, close)
- [ ] Directory operations
- [ ] Path manipulation
- [ ] Stdio (stdin, stdout, stderr)

### 8. Networking
**Status**: Not started

**Tasks**:
- [ ] TCP sockets
- [ ] UDP sockets
- [ ] HTTP client/server (basic)
- [ ] Async networking integration

---

## Testing & Quality

### 9. Integration Testing
**Status**: 280/284 tests passing

**Remaining Work**:
- [ ] Fix 4 failing tests
- [ ] Add more real-world test cases
- [ ] Performance regression testing
- [ ] Continuous integration improvements

### 10. Fuzzing & Property Testing
**Status**: Not started

**Tasks**:
- [ ] Set up fuzzing infrastructure
- [ ] Property-based testing framework
- [ ] Continuous fuzzing in CI

---

## Performance

### 11. Tiered JIT Optimization
**Status**: Framework complete, needs tuning

**Tasks**:
- [ ] Profile-guided optimization (PGO)
- [ ] Better hot-path detection
- [ ] Deoptimization support
- [ ] Inline caching

### 12. Memory Management
**Status**: Basic ownership works

**Enhancements**:
- [ ] Escape analysis optimization
- [ ] Stack allocation promotion
- [ ] Better lifetime elision
- [ ] Reference counting optimization

---

## Tooling

### 13. Language Server Protocol (LSP)
**Status**: Not started

**Tasks**:
- [ ] Implement LSP server
- [ ] Autocomplete
- [ ] Go-to-definition
- [ ] Refactoring support
- [ ] IDE integration (VSCode, IntelliJ)

### 14. Package Manager
**Status**: Not started

**Design Considerations**:
- [ ] Package registry
- [ ] Dependency resolution
- [ ] Build system integration
- [ ] Version management

---

## Documentation

### 15. Language Reference
**Status**: Needs comprehensive docs

**Required Documentation**:
- [ ] Language syntax reference
- [ ] Type system guide
- [ ] Async/await tutorial
- [ ] FFI guide
- [ ] Standard library API docs

### 16. Tutorials & Examples
**Status**: Limited examples

**Needed**:
- [ ] "Getting Started" guide
- [ ] Common patterns cookbook
- [ ] Real-world application examples
- [ ] Performance tuning guide

---

## Research & Exploration

### 17. Effect System
**Status**: Research phase

**Exploration**:
- [ ] Effect type design
- [ ] Effect handlers
- [ ] Integration with async/await
- [ ] Algebraic effects

### 18. Dependent Types (Advanced)
**Status**: Basic framework exists

**Enhancements**:
- [ ] Refinement types
- [ ] Indexed families
- [ ] Full dependent function types
- [ ] Proof obligations

---

## Completed (Archive)

### ✅ Core Compiler Pipeline
- Parser → TypeChecker → HIR → Cranelift JIT
- 98.6% test coverage (280/284 tests)
- Full end-to-end compilation

### ✅ Type System
- Generics, traits, lifetimes, dependent types
- Structural and nominal typing
- Multi-paradigm support (OOP, FP, procedural)
- Advanced type features (associated types, GADTs)

### ✅ Async/Await Runtime
- Complete executor, task, waker infrastructure
- Parameter capture in async state machines
- Integration with tiered JIT
- Zero-cost futures

### ✅ Standard Library (Core)
- Vec<T>, String, HashMap<K,V>
- Iterator trait with 50+ adapters
- Basic memory management
- 93/100 stdlib functions compile

### ✅ Tiered JIT Compilation
- 3-tier optimization (Baseline → Standard → Optimized)
- Runtime profiling with atomic counters
- Hot-path detection and recompilation
- Cranelift (fast) + LLVM (optimized) backends

### ✅ HIR Builder API
- Fluent interface for HIR construction
- Type-safe SSA value management
- Zero-cost abstractions

---

## Priority Matrix

| Priority | Category | Item | Effort | Impact |
|----------|----------|------|--------|--------|
| **HIGH** | Ecosystem | Reflaxe/Haxe Integration | 3-4 weeks | Massive |
| **HIGH** | Testing | Fix remaining 4 test failures | 1-2 days | High |
| **MEDIUM** | Compiler | LLVM AOT Backend | 2 weeks | High |
| **MEDIUM** | Language | Exception Handling | 1 week | Medium |
| **MEDIUM** | Stdlib | I/O and File System | 1-2 weeks | High |
| **LOW** | Tooling | LSP Support | 2-3 weeks | Medium |
| **LOW** | Compiler | Bytecode Interpreter | 2 weeks | Low |

---

## Next Milestones

### Milestone 1: Reflaxe Integration (Q1 2025)
- Complete Haxe backend
- Run 100+ Haxe projects through Zyntax
- Performance benchmarks vs other targets

### Milestone 2: Production Stability (Q2 2025)
- 100% test pass rate
- Complete I/O and networking stdlib
- LLVM AOT backend complete

### Milestone 3: Developer Experience (Q3 2025)
- LSP implementation
- Package manager
- Comprehensive documentation

---

## Contributing

See individual task items for details. Most tasks have clear acceptance criteria and can be tackled independently.

**Key Resources**:
- [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) - System architecture
- [docs/HIR_BUILDER_EXAMPLE.md](docs/HIR_BUILDER_EXAMPLE.md) - HIR construction guide
- [docs/ASYNC_RUNTIME_DESIGN.md](docs/ASYNC_RUNTIME_DESIGN.md) - Async runtime internals

**Getting Started**:
1. Pick a task from the backlog
2. Check related documentation
3. Run existing tests to understand the system
4. Implement incrementally with tests
5. Submit PR with clear description

---

## Notes

- Keep this document updated as work progresses
- Move completed items to the Archive section
- Update priority matrix quarterly
- Track time estimates vs actuals for planning
