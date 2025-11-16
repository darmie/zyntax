# Frontend Lowering Strategy for Haxe String Operations

## Problem Statement

**Goal**: Map Haxe string concatenation (`str1 + str2`) to Zyntax runtime calls (`$String$concat`).

**Initial Approach** (Rejected): Add type-specific checks in core compiler (ssa.rs)
```rust
// ❌ BAD: Core infrastructure checking for specific types
if self.is_string_type(&left.ty) {
    return self.translate_string_concat(...);
}
```

**Problem**: Violates architecture principle that **core compiler must be language-agnostic**.

## Correct Approach: Frontend-Level Transformation

String concatenation handling should happen in the **reflaxe.zyntax frontend**, not the core compiler.

### Option 1: Haxe Macro Transformation (Recommended)

Transform string operations in Haxe before TypedAST generation:

```haxe
// In reflaxe.zyntax Haxe macro
class StringTransformer {
    macro static function init() {
        Context.onTypeExpr(function(e:TypedExprDef) {
            return switch(e) {
                // Detect binary + on strings
                case TBinop(OpAdd, e1, e2) if (isString(e1.t) && isString(e2.t)):
                    // Transform to explicit runtime call
                    {
                        expr: TCall(
                            {expr: TIdent("$String$concat"), t: ...},
                            [e1, e2]
                        ),
                        t: Context.getType("String")
                    }
                default: e;
            }
        });
    }
}
```

This way:
- ✅ Transformation happens at Haxe level
- ✅ Core compiler remains language-agnostic
- ✅ TypedAST already contains the runtime call
- ✅ Works for ALL Haxe-specific operations (not just strings)

### Option 2: TypedAST Post-Processing

Add a transformation pass in reflaxe.zyntax BEFORE sending to core compiler:

```rust
// In reflaxe.zyntax/src/typed_ast_transformer.rs

fn transform_typed_expression(expr: &mut TypedExpression) {
    match expr {
        TypedExpression::Binary(bin) if bin.op == OpAdd => {
            if is_string_type(&bin.left.ty) {
                // Replace with Call to $String$concat
                *expr = TypedExpression::Call(TypedCall {
                    callee: create_runtime_ref("$String$concat"),
                    args: vec![bin.left.clone(), bin.right.clone()],
                    ...
                });
            }
        }
        _ => {}
    }
}
```

Then in typed_ast_json.rs loader:
```rust
pub fn load_and_transform(path: &Path) -> Result<TypedProgram> {
    let mut program = load_typed_ast_json(path)?;

    // Frontend-specific transformations
    transform_haxe_operators(&mut program);

    Ok(program)
}
```

### Option 3: Extended TypedAST Format

Add metadata to TypedAST to mark operations that need runtime lowering:

```json
{
  "expr": "Binary",
  "op": "Add",
  "left": {...},
  "right": {...},
  "metadata": {
    "runtime_call": "$String$concat"  // Added by reflaxe.zyntax
  }
}
```

Core compiler checks for `runtime_call` metadata (generic, not string-specific):
```rust
match expr {
    TypedExpression::Binary(bin) => {
        if let Some(runtime_call) = bin.metadata.get("runtime_call") {
            // Lower to runtime call (generic mechanism)
            return self.translate_runtime_call(runtime_call, &[left, right]);
        }
        // Regular binary operation
        ...
    }
}
```

## Recommended Implementation

**Use Option 1 (Haxe Macro)** for the MVP:

1. In `reflaxe.zyntax/src/Main.hx`, add initialization macro:
   ```haxe
   class Main {
       macro static function init() {
           Compiler.addGlobalMetadata("", "@:build(zyntax.StringOperatorTransformer.build())");
       }
   }
   ```

2. Create `zyntax/StringOperatorTransformer.hx`:
   ```haxe
   class StringOperatorTransformer {
       #if macro
       public static function build():Array<Field> {
           // Transform string operations in this class
           Context.onGenerate(transformStringOps);
           return Context.getBuildFields();
       }

       static function transformStringOps(types:Array<Type>) {
           for (type in types) {
               // Walk TypedExpressions and transform OpAdd on strings
           }
       }
       #end
   }
   ```

3. Compile Haxe with this transformation active

4. Result: TypedAST JSON already contains `$String$concat` calls

5. Core compiler just sees regular function calls (stays generic!)

## Benefits of Frontend Approach

- ✅ **Core compiler remains language-agnostic**
- ✅ **Works for all Haxe-specific operations**:
  - String concat (`+`)
  - Array operations
  - Property access (`.length`)
  - Method calls
  - Custom operators
- ✅ **Leverages Haxe's type system** - transformation is type-safe
- ✅ **Easier to maintain** - frontend concerns in frontend code
- ✅ **Scalable** - easy to add more transformations

## Migration Path

Current state:
- ✅ Runtime functions implemented (Rust)
- ✅ Method mapping manifest generated
- ✅ Runtime functions declared in typed_ast_json.rs
- ❌ String concat detection in core compiler (needs removal)

Next steps:
1. Remove string-specific code from core compiler
2. Implement Haxe macro transformation in reflaxe.zyntax
3. Test: Haxe `"a" + "b"` → TypedAST with `$String$concat` call → HIR → Cranelift → Native
4. Extend to other operations (`.length`, `.charAt`, etc.)

## References

- [reflaxe.zyntax/runtime/haxe/FINDINGS.md](runtime/haxe/FINDINGS.md) - Why build-time type injection doesn't work
- [reflaxe.zyntax/runtime/method_mappings.json](runtime/method_mappings.json) - Runtime function manifest
- Haxe macro documentation: https://haxe.org/manual/macro.html
