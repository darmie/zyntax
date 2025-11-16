# Whirlwind Adapter Testing Guide

## Testing Strategy

This document outlines the comprehensive testing approach for the Whirlwind → TypedAST adapter.

## Test Structure

```
tests/
├── integration_tests.rs       # Main integration tests
├── fixtures/                  # Real Whirlwind source files
│   ├── simple_function.wrl    # Basic function
│   ├── union_types.wrl        # Union types (A | B)
│   ├── optional_types.wrl     # Optional types (?T)
│   ├── generic_function.wrl   # Generics <T>
│   ├── control_flow.wrl       # If/while/for
│   ├── model_interface.wrl    # Classes & interfaces
│   └── enum_record.wrl        # Enums & structs
└── snapshots/                 # (Future) Expected output snapshots
```

## Test Levels

### 1. Unit Tests (Per Module)

**TypeConverter Tests** (`src/type_converter.rs`)
- ✅ Primitive type mapping (i32, bool, etc.)
- ⏳ Union type conversion (A | B → `Type::Union`)
- ⏳ Optional type conversion (?T → `Type::Optional`)
- ⏳ Function type conversion (fn(A) -> B)
- ⏳ Generic type conversion (List<T>)
- ⏳ Array type conversion ([T])
- ⏳ Member type conversion (core.io.Error)

**ExpressionConverter Tests** (`src/expression_converter.rs`)
- ⏳ Literal conversion (42, "hello", true)
- ⏳ Binary operations (+, -, *, /, ==, !=)
- ⏳ Unary operations (!, -)
- ⏳ Function calls
- ⏳ Lambda expressions
- ⏳ Member access (a.b)
- ⏳ Array indexing (a[b])
- ⏳ If expressions
- ⏳ Block expressions

**StatementConverter Tests** (`src/statement_converter.rs`)
- ⏳ Function declarations
- ⏳ Variable declarations
- ⏳ Model (class) declarations
- ⏳ Interface declarations
- ⏳ Enum declarations
- ⏳ Record (struct) declarations
- ⏳ Type aliases
- ⏳ While loops
- ⏳ For loops
- ⏳ Return statements
- ⏳ Break/continue

### 2. Integration Tests (Full Pipeline)

**Current Tests** (`tests/integration_tests.rs`)
- ✅ Fixture loading
- ✅ Adapter initialization
- ✅ Error message validation

**Planned Tests** (Once Whirlwind dependency added)
- ⏳ Simple function conversion
- ⏳ Union type program
- ⏳ Optional type program
- ⏳ Generic function program
- ⏳ Control flow program
- ⏳ Model/interface program
- ⏳ Enum/record program
- ⏳ Complete program roundtrip

### 3. Regression Tests (Snapshot Testing)

**Strategy:**
1. Convert known Whirlwind programs to TypedAST
2. Save the TypedAST output as "snapshots"
3. On future changes, compare new output with snapshots
4. Alert if conversion behavior changes unexpectedly

**Benefits:**
- Catch subtle conversion regressions
- Document expected behavior
- Quick visual diff of changes

## Test Fixtures

### simple_function.wrl
```whirlwind
function add(a: i32, b: i32) -> i32 {
    return a + b
}
```
**Tests:** Basic function, primitive types, return statement

### union_types.wrl
```whirlwind
type Result = Success | Error | Pending
function handleResult(r: Result) -> i32 { ... }
```
**Tests:** Union type declarations, union type parameters

### optional_types.wrl
```whirlwind
function findItem(items: [i32], target: i32) -> ?i32 { ... }
```
**Tests:** Optional return types, null handling, array parameters

### generic_function.wrl
```whirlwind
function identity<T>(value: T) -> T { ... }
```
**Tests:** Generic functions, type parameters, generic instantiation

### control_flow.wrl
```whirlwind
function factorial(n: i32) -> i32 {
    if n <= 1 { return 1 }
    return n * factorial(n - 1)
}
```
**Tests:** If statements, recursion, arithmetic operations

### model_interface.wrl
```whirlwind
interface Drawable { function draw() -> void }
model Circle implements Drawable { ... }
```
**Tests:** Interface declarations, class declarations, inheritance

### enum_record.wrl
```whirlwind
enum Color { Red, Green, Blue, RGB(u8, u8, u8) }
record Point { x: f64, y: f64 }
```
**Tests:** Enum variants, variant payloads, struct fields

## Running Tests

### Current State (Before Whirlwind Dependency)
```bash
# Run basic tests (fixture loading, adapter init)
cargo test -p whirlwind_adapter

# Should show:
# - test_fixtures_exist ... ok
# - test_adapter_initialization ... ok
# - test_error_messages ... ok
```

### After Whirlwind Integration
```bash
# Run all tests
cargo test -p whirlwind_adapter

# Run specific test
cargo test -p whirlwind_adapter test_simple_function_conversion

# Run with output
cargo test -p whirlwind_adapter -- --nocapture

# Run integration tests only
cargo test -p whirlwind_adapter --test integration_tests
```

## Validation Checklist

For each fixture, we validate:

### ✅ Type Conversion
- [ ] All types map correctly to TypedAST
- [ ] Union types preserve all variants
- [ ] Optional types wrap correctly
- [ ] Generic type parameters preserved
- [ ] Function types have correct signatures

### ✅ Expression Conversion
- [ ] Literals have correct values and types
- [ ] Binary operations preserve operands and operator
- [ ] Function calls reference correct callees
- [ ] Member access resolves correctly
- [ ] Array indexing preserved

### ✅ Statement Conversion
- [ ] Function declarations have correct names
- [ ] Parameters map with correct types
- [ ] Return types match
- [ ] Function bodies contain all statements
- [ ] Variable declarations preserve initialization

### ✅ Structure Preservation
- [ ] Module structure maintained
- [ ] Import/export relationships preserved
- [ ] Symbol table information retained
- [ ] Span information for error reporting

### ✅ Error Handling
- [ ] Unsupported features report clearly
- [ ] Type mismatches identified
- [ ] Conversion failures are descriptive
- [ ] Errors include source location

## Test Output Examples

### Successful Conversion
```
✓ Fixture loaded: simple_function (73 bytes)
✓ Adapter initialized successfully
✓ Simple function converted successfully
  - Found function 'add' with 2 parameters
  - Return type: i32
  - Body: 1 statement
✓ All assertions passed
```

### Error Reporting
```
✗ Conversion failed for union_types
Error: Type conversion error: Union type with 3 variants
  Expected: Type::Union(...)
  Got: Type::Named(...)
  Location: union_types.wrl:1:6
```

## Extending Tests

### Adding New Fixtures

1. Create `.wrl` file in `tests/fixtures/`
2. Add fixture name to `test_fixtures_exist()`
3. Create dedicated test function
4. Add to `test_complete_program_roundtrip()`

Example:
```rust
#[test]
fn test_my_new_feature() {
    let source = load_fixture("my_new_feature");

    let mut standpoint = Standpoint::new();
    standpoint.add_module_from_text(&source).unwrap();

    let mut adapter = WhirlwindAdapter::new();
    let typed_program = adapter.convert_standpoint(&standpoint).unwrap();

    // Your assertions here
    assert!(!typed_program.modules.is_empty());
}
```

### Adding Snapshot Tests

```rust
#[test]
fn test_snapshot_simple_function() {
    let typed_program = /* ... conversion ... */;

    let snapshot = serialize_typed_program(&typed_program);

    // First run: save snapshot
    // save_snapshot("simple_function", &snapshot);

    // Subsequent runs: compare
    compare_snapshot("simple_function", &snapshot)
        .expect("Snapshot mismatch");
}
```

## CI/CD Integration

### GitHub Actions Workflow
```yaml
name: Whirlwind Adapter Tests

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - uses: actions-rs/cargo@v1
        with:
          command: test
          args: -p whirlwind_adapter
```

## Coverage Goals

- **Unit Tests:** 80%+ coverage of converter methods
- **Integration Tests:** All fixtures convert successfully
- **Regression Tests:** Snapshot tests for critical conversions
- **Edge Cases:** Error paths tested

## Performance Testing

Future: Add benchmarks for large programs

```rust
#[bench]
fn bench_large_program_conversion(b: &mut Bencher) {
    let source = load_fixture("large_program");
    let standpoint = parse_whirlwind(&source);

    b.iter(|| {
        let mut adapter = WhirlwindAdapter::new();
        adapter.convert_standpoint(&standpoint).unwrap()
    });
}
```

## Next Steps

1. ✅ Create test fixtures with real Whirlwind syntax
2. ✅ Create integration test structure
3. ⏳ Add Whirlwind dependency
4. ⏳ Uncomment and run integration tests
5. ⏳ Implement converters to make tests pass
6. ⏳ Add snapshot testing
7. ⏳ Set up CI/CD

---

**Status:** Ready for implementation once Whirlwind dependency is added!
