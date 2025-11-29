# Zyntax Embed Integration Plan

## Overview

This document outlines the integration gaps between:
1. **zyntax_embed** (Rust SDK for embedding Zyntax JIT)
2. **zrtl_macros** (Rust procedural macros for ZRTL plugins)
3. **zrtl.h** (C SDK header - comprehensive reference implementation)

The C SDK (`zrtl.h`) is significantly more robust than the Rust macro crate. This plan addresses feature parity and integration improvements.

---

## Gap Analysis: zrtl.h vs zrtl_macros

### Features in zrtl.h (1350 lines) Missing from zrtl_macros (153 lines)

| Feature | zrtl.h | zrtl_macros | Priority |
|---------|--------|-------------|----------|
| Type Tags (ZrtlTypeTag) | ✅ Full support | ❌ None | High |
| Type Categories enum | ✅ ZRTL_CAT_* | ❌ None | High |
| Type Flags (nullable, boxed, etc.) | ✅ ZRTL_FLAG_* | ❌ None | High |
| DynamicBox struct | ✅ 24-byte boxed value | ❌ None | High |
| Box accessors (zrtl_box_as_i32, etc.) | ✅ 10+ inline functions | ❌ None | High |
| Box creation helpers | ✅ zrtl_box_i32, etc. | ❌ None | High |
| GenericBox for generics | ✅ Array<T>, Map<K,V> | ❌ None | High |
| GenericTypeArgs | ✅ Nested generics | ❌ None | Medium |
| TypeDescriptor | ✅ Full type info | ❌ None | Medium |
| ZrtlString (inline format) | ✅ Length-prefixed | ❌ None | High |
| String helpers | ✅ new, copy, print, equals | ❌ None | High |
| ZrtlStringView | ✅ Non-owning view | ❌ None | Low |
| ZrtlArray (inline format) | ✅ Capacity+length header | ❌ None | High |
| Array helpers | ✅ new, push, get | ❌ None | High |
| Iterator protocol | ✅ ZrtlIterator | ❌ None | Medium |
| ArrayIterator | ✅ Typed iteration | ❌ None | Medium |
| StringIterator | ✅ UTF-8 codepoint iteration | ❌ None | Low |
| IterableVtable | ✅ Custom iterables | ❌ None | Low |
| Type registry | ✅ Custom type registration | ❌ None | Medium |
| Test harness | ✅ ZRTL_TEST macros | ❌ None | Low |

### What zrtl_macros Currently Provides

1. `zrtl_plugin!("name")` - Creates `_zrtl_info` and `_zrtl_symbols` exports
2. `#[zrtl_export("$Type$method")]` - Registers function in symbol table

### What's Missing from zrtl_macros

1. **Type system macros** - No Rust equivalents for type tags, categories, flags
2. **DynamicBox/GenericBox** - No proc macros for generating box wrappers
3. **String/Array format** - No helper macros for inline memory formats
4. **Iterator traits** - No derive macros for making types iterable
5. **Test framework** - No Rust test macros equivalent to ZRTL_TEST

---

## Gap Analysis: zyntax_embed

### Current State (Updated)

| Component | Status | Notes |
|-----------|--------|-------|
| ZyntaxValue enum | ✅ Complete | All categories represented |
| ZyntaxString | ✅ Complete | Length-prefixed format |
| ZyntaxArray | ✅ Complete | Header format |
| FromZyntax/IntoZyntax traits | ✅ Complete | Bidirectional conversion |
| ZyntaxRuntime | ✅ Complete | Single-tier JIT with native calling |
| TieredRuntime | ✅ Complete | Multi-tier with stats |
| ZyntaxPromise | ✅ Complete | Full async ABI with combinators |
| Hot reload | ⚠️ Partial | API exists, needs testing |
| DynamicValue interop | ✅ Complete | Full conversion support |
| GenericValue support | ✅ Complete | GenericTypeArgs exported |
| TypeMeta preservation | ✅ Complete | TypeMeta, TypeRegistry exported |
| TypeRegistry | ✅ Complete | Re-exported from zyntax_compiler |
| Error value support | ⚠️ Partial | Result<T,E> conversion incomplete |
| ZrtlPlugin loading | ✅ Complete | load_plugin, load_plugins_from_directory |
| Promise combinators | ✅ Complete | PromiseAll, PromiseRace, PromiseAllSettled |
| Native async (ZRTL) | ✅ Complete | #[zrtl_async] macro in sdk/zrtl_macros |

### Remaining Integration Gaps

1. ~~**Limited argument handling** - Only supports 0-4 args via manual transmute~~ ✅ Supports 0-8 args
2. ~~**No varargs support** - Can't call functions with >4 parameters~~ ✅ Supports up to 8 parameters
3. **No struct field access from compiled code** - Can build structs but not read
4. ~~**No cancellation** - Promises can't be cancelled~~ ✅ `cancel()`, `is_cancelled()`, `PromiseState::Cancelled`
5. ~~**Iterator traits** - No ZrtlIterable/ZrtlIterator in embed crate~~ ✅ `ZrtlIterable`, `ZrtlIterator` traits added

---

## Proposed Roadmap

### Phase 1: Type System Alignment (High Priority)

#### 1.1 Add type system to zrtl_macros

```rust
// New derive macro for ZRTL types
#[derive(ZrtlType)]
#[zrtl(name = "Point", category = "struct")]
pub struct Point {
    pub x: f64,
    pub y: f64,
}

// Generates:
// - static TYPE_INFO: ZrtlTypeInfo
// - impl ZrtlTyped for Point
// - Drop handling with dropper
```

#### 1.2 Add DynamicBox to zrtl_macros

```rust
// Proc macro for generating box wrappers
zrtl_box_types! {
    i8, i16, i32, i64,
    u8, u16, u32, u64,
    f32, f64, bool,
}

// Generates all zrtl_box_as_* and zrtl_box_* functions as Rust FFI
```

#### 1.3 Add GenericBox support

```rust
// Derive for generic containers
#[derive(ZrtlGeneric)]
pub struct ZrtlArray<T> { /* ... */ }

// Generates GenericTypeArgs handling
```

### Phase 2: zyntax_embed Enhancements (High Priority)

#### 2.1 Add TypeRegistry to embed crate

```rust
// Re-export from compiler
pub use zyntax_compiler::zrtl::TypeRegistry;

// Add to runtime
impl ZyntaxRuntime {
    pub fn type_registry(&self) -> &TypeRegistry { /* ... */ }
    pub fn register_type<T: ZrtlTyped>(&mut self) -> TypeId { /* ... */ }
}
```

#### 2.2 Improve function calling

```rust
// Add variadic calling support
impl ZyntaxRuntime {
    pub fn call_variadic(&self, name: &str, args: &[ZyntaxValue]) -> RuntimeResult<ZyntaxValue> {
        // Use assembly trampoline for arbitrary arg counts
    }
}
```

#### 2.3 Add ZRTL plugin loading

```rust
// Load native plugins
impl ZyntaxRuntime {
    pub fn load_plugin<P: AsRef<Path>>(&mut self, path: P) -> RuntimeResult<()> {
        let plugin = ZrtlPlugin::load(path)?;
        for (name, ptr) in plugin.symbols() {
            self.register_function(name, ptr, 0); // TODO: get arity
        }
        Ok(())
    }
}
```

### Phase 3: Iterator and Collection Support (Medium Priority)

#### 3.1 Iterator traits for Rust

```rust
// Trait for ZRTL-compatible iteration
pub trait ZrtlIterable {
    type Item;
    type Iterator: ZrtlIterator<Item = Self::Item>;
    fn zrtl_iter(&self) -> Self::Iterator;
}

pub trait ZrtlIterator {
    type Item;
    fn has_next(&self) -> bool;
    fn next(&mut self) -> Option<Self::Item>;
}

// Implement for ZyntaxArray
impl<T: Clone> ZrtlIterable for ZyntaxArray<T> {
    // ...
}
```

#### 3.2 Collection interop

```rust
// Convert between Rust Vec and ZyntaxArray
impl<T: IntoZyntax> From<Vec<T>> for ZyntaxArray<ZyntaxValue> { /* ... */ }
impl<T: FromZyntax> TryFrom<ZyntaxArray<ZyntaxValue>> for Vec<T> { /* ... */ }

// HashMap interop
impl<K, V> From<HashMap<K, V>> for ZyntaxValue
where K: Into<String>, V: IntoZyntax { /* ... */ }
```

### Phase 4: Async/Await Improvements (Medium Priority)

#### 4.1 Proper state machine integration

```rust
// Connect to Zyntax async ABI
impl ZyntaxPromise {
    fn poll_internal(&self) -> PromiseState {
        // Call actual Zyntax poll function
        // Handle generator/coroutine state
    }
}
```

#### 4.2 Cancellation support

```rust
impl ZyntaxPromise {
    pub fn cancel(&self) -> bool {
        // Set cancelled flag
        // Clean up state machine
    }
}
```

#### 4.3 Promise combinators

```rust
impl ZyntaxPromise {
    pub fn all(promises: Vec<ZyntaxPromise>) -> ZyntaxPromise { /* ... */ }
    pub fn race(promises: Vec<ZyntaxPromise>) -> ZyntaxPromise { /* ... */ }
    pub fn any(promises: Vec<ZyntaxPromise>) -> ZyntaxPromise { /* ... */ }
}
```

### Phase 5: Test Framework (Low Priority)

#### 5.1 Rust test macros

```rust
// Test harness for ZRTL plugins
#[zrtl_test]
fn test_array_push() {
    let mut arr = ZyntaxArray::new();
    arr.push(42);
    zrtl_assert_eq!(arr.len(), 1);
    zrtl_assert_eq!(arr[0], 42);
}
```

---

## Priority Matrix

| Task | Impact | Effort | Priority | Status |
|------|--------|--------|----------|--------|
| Type tags in zrtl_macros | High | Medium | P0 | ✅ Done (in zrtl crate) |
| DynamicBox in zrtl_macros | High | Medium | P0 | ✅ Done (in zrtl crate) |
| TypeRegistry in embed | High | Low | P0 | ✅ Done (re-exported) |
| ZRTL plugin loading | High | Medium | P1 | ✅ Done |
| Variadic function calls | High | High | P1 | ✅ Done (0-8 args) |
| GenericBox support | Medium | Medium | P1 | ✅ Done (in zrtl crate) |
| Iterator traits | Medium | Medium | P2 | ✅ Done |
| Collection interop | Medium | Low | P2 | ✅ Done |
| Async state machine | Medium | High | P2 | ✅ Done |
| Cancellation | Low | Medium | P3 | ✅ Done |
| Promise combinators | Low | Low | P3 | ✅ Done |
| Test framework | Low | Medium | P3 | ✅ Done |
| Native async functions (ZRTL) | High | Medium | P1 | ✅ Done |

---

## Implementation Order

### Sprint 1: Type System Foundation ✅ COMPLETE

1. ✅ Add `TypeCategory`, `TypeFlags`, `TypeTag` to `zrtl` crate
2. ✅ Add `DynamicBox` struct and accessors to `zrtl` crate
3. ✅ Add `#[derive(ZrtlType)]` macro to `zrtl_macros`
4. ✅ Re-export `TypeRegistry` in `zyntax_embed`

### Sprint 2: Runtime Integration ✅ COMPLETE

1. ✅ Add ZRTL plugin loading to `ZyntaxRuntime` (load_plugin, load_plugins_from_directory)
2. ✅ Improve function calling with variadic support (0-8 arguments)
3. ✅ Add `GenericTypeArgs` handling to `ZyntaxValue` (re-exported from zrtl)

### Sprint 3: Collections & Iteration ✅ COMPLETE

1. ✅ Add `ZrtlIterable` and `ZrtlIterator` traits
2. ✅ Implement for `ZyntaxArray` and `ZyntaxString`
3. ✅ Add collection conversion helpers (Extend, FromIterator, From, Into)

### Sprint 4: Async & Polish ✅ COMPLETE

1. ✅ Connect `ZyntaxPromise` to actual Zyntax async ABI
2. ✅ Add cancellation support (`cancel()`, `is_cancelled()`, `PromiseState::Cancelled`)
3. ✅ Add promise combinators (`PromiseAll`, `PromiseRace`, `PromiseAllSettled`)
4. ✅ Add native async functions to ZRTL SDK (`#[zrtl_async]` macro)
5. ✅ Add test framework macros (`#[zrtl_test]`, `zrtl_assert_eq!`, etc.)

---

## Files to Modify/Create

### New Files
- `sdk/zrtl_macros/src/type_system.rs` - Type tags, categories, flags
- `sdk/zrtl_macros/src/dynamic_box.rs` - DynamicBox generation
- `sdk/zrtl_macros/src/derive_type.rs` - #[derive(ZrtlType)]
- `sdk/zrtl_macros/src/iterators.rs` - Iterator traits and derives
- `crates/zyntax_embed/src/plugin.rs` - ZRTL plugin loading
- `crates/zyntax_embed/src/iterator.rs` - Iterator implementations
- `crates/zyntax_embed/src/generic.rs` - Generic type handling

### Files to Modify
- `sdk/zrtl_macros/src/lib.rs` - Export new macros
- `sdk/zrtl_macros/Cargo.toml` - Add dependencies (inventory, etc.)
- `crates/zyntax_embed/src/lib.rs` - Export new types
- `crates/zyntax_embed/src/runtime.rs` - Add plugin loading, variadic calls
- `crates/zyntax_embed/src/value.rs` - Add generic type args
- `crates/zyntax_embed/Cargo.toml` - Feature flags for llvm-backend

---

## Success Criteria

1. **Feature Parity**: All major features from `zrtl.h` available in Rust
2. **Type Safety**: Rust SDK provides compile-time type checking where possible
3. **Ergonomics**: Rust API is idiomatic and easy to use
4. **Performance**: Zero-overhead abstraction where possible
5. **Interop**: Can load C ZRTL plugins from Rust and vice versa
6. **Testing**: Comprehensive test coverage for all features
