# Whirlwind Adapter

Adapter for converting Whirlwind language AST to Zyntax TypedAST (HIR).

## Status

✅ **Core Features Working:**
- Function declarations with typed parameters and return types
- Model (class) declarations with fields, methods, and constructors
- Type aliases
- Enums with variants (including tagged variants)
- Interfaces
- Type resolution through TypeRegistry
- Primitive type mapping (int→I32, float→F64, string→String, etc.)
- IntermediateType → Type conversion for all major type constructs

## Known Limitations

### Whirlwind Analyzer Diagnostics

When converting Whirlwind code, you may see `UnknownValue` diagnostics for primitive types:

```
ProgramDiagnostic { _type: Error(Context(ContextError { _type: UnknownValue { name: "int" } })) }
```

**This is expected and does not affect conversion.** These diagnostics occur because:

1. **Whirlwind's core library is not yet implemented** - Built-in types aren't defined
2. **Our adapter works around this** - We map type names to primitives during conversion
3. **Conversion succeeds** - Types resolve correctly as `Primitive(I32)`, etc.

The diagnostics are Whirlwind analyzer warnings, not adapter errors.

## Testing

Run tests with:
```bash
cargo test --package whirlwind_adapter
```

- **22 tests passing**: Integration, real Whirlwind code, and systematic fixture tests
- **10 fixture tests** covering simple to complex language features
- All types resolve correctly (no `Type::Unknown` for proper code)

## Example Usage

```rust
use whirlwind_adapter::WhirlwindAdapter;
use whirlwind_analyzer::{Module, Standpoint};

let source = r#"
module Test;

model User {
    var id: int,
    var name: string,

    function greet() -> string {
        return "Hello"
    }
}
"#;

let mut module = Module::from_text(source);
module.module_path = Some(PathBuf::from("test.wrl"));
let standpoint = Standpoint::build_from_module(module, false).unwrap();

let mut adapter = WhirlwindAdapter::new();
let typed_program = adapter.convert_standpoint(&standpoint).unwrap();

// Result: TypedProgram with User class containing 2 fields, 1 method
```

## License

Part of the Zyntax compiler project.
