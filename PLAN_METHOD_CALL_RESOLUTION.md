# Implementation Plan: Method Call and Trait Dispatch Resolution

## Problem Statement

Currently, `p1.clone()` is parsed as a field access followed by a call, but the compiler doesn't resolve that `clone` is a trait method from an impl block. This results in a field lookup error. Similarly, operator overloading via traits (e.g., `a + b` for custom types) doesn't work because there's no trait dispatch mechanism.

## Current State Analysis

### What Works
1. ✅ Struct field access via `ExtractValue` instruction
2. ✅ Impl blocks are parsed and lowered to functions
3. ✅ Self parameter type resolution in impl methods
4. ✅ Extern function calls via @builtin name mapping
5. ✅ Direct function calls by name

### What Doesn't Work
1. ❌ Method calls via dot syntax (`p1.clone()`)
2. ❌ Operator overloading via traits (`a + b` for custom types)
3. ❌ Trait method dispatch (looking up which impl provides a method)

### Architecture Overview

**Parser (ZynPEG)**:
- Creates TypedAST with `TypedExpression::Field` for dot access
- Creates `TypedExpression::Call` for function calls
- Parses `p1.clone()` as `Call(Field(p1, "clone"), [])`

**Type Registry**:
- Stores `ImplDef` with trait methods via `register_implementation()`
- Has `impl_cache: HashMap<(TypeId, Type), ImplDef>` for fast lookup
- Tracks `type_implementations: HashMap<TypeId, HashSet<TypeId>>`

**Lowering**:
- Converts TypedAST → HIR
- Lowers impl block methods as regular functions
- No trait dispatch resolution currently

**SSA Generation**:
- `TypedExpression::Field` → `HirInstruction::ExtractValue` (struct field access)
- `TypedExpression::Call` → `HirInstruction::Call`
- No logic to check if field is actually a method

## Implementation Approaches

### Approach 1: Parser-Level Desugaring (Not Recommended)
**Idea**: Modify parser to distinguish `x.method()` from `x.field`.

**Pros**:
- Early resolution
- Cleaner AST

**Cons**:
- ❌ Parser is language-agnostic (ZynPEG)
- ❌ Parser doesn't have type information
- ❌ Would require grammar changes that couple it to ZynML semantics

**Verdict**: Rejected - violates parser/language separation

### Approach 2: Type Checking Pass (Ideal Long-term)
**Idea**: Add a dedicated type checking/method resolution pass between parsing and lowering.

**Pros**:
- Proper separation of concerns
- Can do comprehensive trait resolution
- Better error messages

**Cons**:
- ❌ Major architectural change
- ❌ Would take significant time (8-12 hours)
- ❌ Overkill for current needs

**Verdict**: Future work, not for this iteration

### Approach 3: Lowering-Time Resolution (RECOMMENDED)
**Idea**: During lowering, when encountering `Call(Field(obj, name), args)`, check if `name` is a trait method and transform it into a direct method call.

**Pros**:
- ✅ Minimal changes to existing architecture
- ✅ Type registry already available in lowering
- ✅ Can implement incrementally
- ✅ No parser changes needed

**Cons**:
- Less clean than dedicated pass
- Resolution happens late

**Verdict**: Best pragmatic solution for current architecture

### Approach 4: SSA-Time Resolution (Alternative)
**Idea**: During SSA generation, detect method calls and dispatch.

**Pros**:
- Very late binding
- Could support runtime dispatch

**Cons**:
- ❌ Too late for good error messages
- ❌ HIR should already be resolved
- ❌ More complex to implement

**Verdict**: Not recommended

## Selected Approach: Lowering-Time Resolution

### Design Overview

We'll add method resolution during the lowering phase by:

1. **Detecting method calls**: Recognize `Call(Field(obj, method_name), args)` pattern
2. **Type-based lookup**: Use object's type to find trait implementations
3. **Method resolution**: Look up method in impl blocks via type registry
4. **Transformation**: Convert to direct function call with proper name mangling

### Detailed Design

#### Phase 1: Method Call Detection

In `lowering.rs`, add a method resolution step:

```rust
fn resolve_method_call(
    &self,
    object_expr: &TypedNode<TypedExpression>,
    method_name: InternedString,
    args: &[TypedNode<TypedExpression>],
) -> Option<(InternedString, Vec<TypedNode<TypedExpression>>)>
```

This checks:
1. Is object's type a Named type?
2. Does the type have any trait implementations?
3. Do any of those impls provide a method with `method_name`?
4. If yes, return the mangled function name and args with `object_expr` prepended

#### Phase 2: Operator Overloading

Similarly, for `TypedExpression::Binary`:

```rust
fn resolve_operator_trait(
    &self,
    op: BinaryOp,
    left_type: &Type,
    right_type: &Type,
) -> Option<InternedString>
```

Maps operators to trait names:
- `+` → `Add::add`
- `-` → `Sub::sub`
- `*` → `Mul::mul`
- etc.

Then looks up the trait impl and returns the method name.

#### Phase 3: Function Name Mangling

Methods need unique names to avoid conflicts:

```rust
fn mangle_trait_method_name(
    trait_name: InternedString,
    type_name: InternedString,
    method_name: InternedString,
) -> InternedString
```

Naming scheme: `{TypeName}${TraitName}${method_name}`
Example: `Point$Clone$clone`, `Tensor$Add$add`

#### Phase 4: Integration Points

**In `lower_expression` for `TypedExpression::Call`**:
```rust
TypedExpression::Call(call) => {
    // Check if callee is a field access (potential method call)
    if let TypedExpression::Field(field_access) = &call.callee.node {
        if let Some((mangled_name, new_args)) =
            self.resolve_method_call(&field_access.object, field_access.field, &call.positional_args)
        {
            // Transform into direct function call
            let direct_call = /* create call with mangled_name and new_args */;
            return self.lower_call(direct_call);
        }
    }

    // Fall through to normal call handling
    self.lower_call(call)
}
```

**In `lower_expression` for `TypedExpression::Binary`**:
```rust
TypedExpression::Binary(binary) => {
    // Check for trait-based operator overloading
    if let Some(method_name) = self.resolve_operator_trait(&binary.op, &binary.left.ty, &binary.right.ty) {
        // Transform into method call: left.method(right)
        return self.lower_trait_operator_call(method_name, &binary.left, &binary.right);
    }

    // Fall through to primitive operator
    self.lower_binary_op(binary)
}
```

**In `lower_impl_block`**:
```rust
// When lowering impl methods, use mangled names
let mangled_name = self.mangle_trait_method_name(
    impl_block.trait_id,
    implementing_type_name,
    method.name,
);

let func = TypedFunction {
    name: mangled_name,  // Use mangled name instead of bare method name
    // ... rest of function
};
```

### Implementation Steps

#### Step 1: Add Method Resolution Infrastructure
- [ ] Add `resolve_method_call()` to lowering
- [ ] Add `mangle_trait_method_name()` helper
- [ ] Add type registry lookups for impl methods

#### Step 2: Implement Method Call Detection
- [ ] Modify `TypedExpression::Call` lowering
- [ ] Detect `Call(Field(...))` pattern
- [ ] Transform to direct call with mangled name

#### Step 3: Update Impl Block Lowering
- [ ] Use mangled names when lowering impl methods
- [ ] Ensure consistency with resolution logic

#### Step 4: Add Operator Overloading
- [ ] Add `resolve_operator_trait()`
- [ ] Map operators to trait method names
- [ ] Modify `TypedExpression::Binary` lowering

#### Step 5: Testing
- [ ] Test method calls: `p1.clone()`
- [ ] Test operator overloading: `a + b`
- [ ] Test with multiple traits per type
- [ ] Test error cases (method not found, ambiguous)

### Data Structures Needed

```rust
// In Lowering struct
trait_method_cache: HashMap<(TypeId, InternedString), InternedString>
```

This caches method name lookups to avoid repeated type registry queries.

### Error Handling

When method resolution fails:
1. Check if it's actually a field (fall back to field access)
2. If not a field and not a method, emit clear error: "Method 'clone' not found for type 'Point'"
3. Consider suggesting available methods

### Performance Considerations

- Cache method resolutions in `trait_method_cache`
- Type registry lookups are `O(1)` via HashMap
- Method name interning avoids string comparisons
- No runtime overhead - all resolved at compile time

### Testing Strategy

**Test Cases**:
1. Simple method call: `p1.clone()`
2. Method with arguments: `p1.distance(p2)`
3. Chained calls: `p1.clone().clone()`
4. Operator overloading: `p1 + p2`
5. Mixed field and method: `p1.x + p2.x`
6. Multiple impls: type with both Clone and Display
7. Ambiguous calls (should error)
8. Method not found (should error gracefully)

**Integration Tests**:
- Run existing `/tmp/test_impl_field.zynml`
- Run tensor operator tests
- Ensure no regressions in struct field access

## Alternative Considered: Uniform Call Syntax

We could also support calling methods as regular functions:
```
clone(p1)  // equivalent to p1.clone()
```

This would require:
- Parser support for both syntaxes
- Function name resolution considering trait methods

**Decision**: Defer this to future work. Focus on dot syntax first.

## Future Enhancements

1. **Generic trait methods**: `impl<T> Trait for Type<T>`
2. **Trait bounds**: `where T: Clone`
3. **UFCS (Uniform Function Call Syntax)**: `Clone::clone(p1)`
4. **Default trait methods**: Methods with default implementations
5. **Associated types resolution**: `<T as Trait>::AssocType`
6. **Multiple trait bounds**: `T: Clone + Display`
7. **Trait objects**: `dyn Trait` for runtime polymorphism

## Open Questions

1. **Name mangling scheme**: Is `Type$Trait$method` sufficient or do we need more info (generics, etc.)?
   - **Answer**: Start simple, extend if needed for generics later

2. **Trait method priorities**: If type implements multiple traits with same method name, which wins?
   - **Answer**: Should be a compile error (ambiguous). User must use UFCS to disambiguate.

3. **Inherent methods vs trait methods**: Should inherent impl methods (impl without trait) work differently?
   - **Answer**: Yes, inherent methods should have priority over trait methods. Use simpler mangling: `Type$method`

4. **Extension methods**: Can we allow impl blocks for types from other modules?
   - **Answer**: Defer to later - requires orphan rule checking

## Success Criteria

After implementation, these should work:

```zynml
// Method calls
struct Point { x: i32, y: i32 }

impl Clone for Point {
    fn clone(self) -> Point {
        Point { x: self.x, y: self.y }
    }
}

fn main() {
    let p1 = Point { x: 5, y: 10 }
    let p2 = p1.clone()  // ✅ Should work
    println(p2.x)        // ✅ Should print 5
}

// Operator overloading
impl Add<Point> for Point {
    type Output = Point
    fn add(self, other: Point) -> Point {
        Point { x: self.x + other.x, y: self.y + other.y }
    }
}

fn main() {
    let p1 = Point { x: 1, y: 2 }
    let p2 = Point { x: 3, y: 4 }
    let p3 = p1 + p2     // ✅ Should work
    println(p3.x)        // ✅ Should print 4
}
```

## Timeline Estimate

- **Step 1-2** (Method resolution infrastructure + detection): 2-3 hours
- **Step 3** (Impl block name mangling): 1 hour
- **Step 4** (Operator overloading): 2 hours
- **Step 5** (Testing + debugging): 2-3 hours

**Total**: 7-9 hours for complete implementation

## Risk Mitigation

1. **Risk**: Existing code breaks due to name mangling changes
   - **Mitigation**: Only mangle trait methods, keep other functions as-is

2. **Risk**: Complex generic cases not handled
   - **Mitigation**: Start with non-generic case, add TODO comments for generics

3. **Risk**: Performance degradation from type registry lookups
   - **Mitigation**: Add caching layer, measure impact

4. **Risk**: Ambiguous cases cause confusing errors
   - **Mitigation**: Add clear error messages with suggestions
