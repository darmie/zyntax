# Continue Statement Bug Investigation

## Status: ROOT CAUSE IDENTIFIED, FIX IN PROGRESS

## Problem Statement
Continue statements in while loops cause infinite loops during JIT execution. The compiled code hangs indefinitely instead of properly jumping back to the loop header.

## Test Case
```zig
fn sum_odd_numbers(n: i32) i32 {
    var sum = 0;
    var i = 1;
    while (i <= n) {
        if (i % 2 == 0) {
            i = i + 1;
            continue;  // ← BUG: Causes infinite loop
        }
        sum = sum + i;
        i = i + 1;
    }
    return sum;
}
```

Expected: `sum_odd_numbers(10) = 25` (1+3+5+7+9)
Actual: Hangs indefinitely

## Root Cause

### CFG Block Reuse Issue
The block containing variable initializations (`var sum = 0; var i = 1;`) is being reused as the loop body's merge block (where `sum += i; i++` happens after the if statement).

### Evidence from HIR Dump

**Block f2445c86** serves DUAL purposes:
1. **Entry path**: Variables are initialized
2. **Loop-back path**: Variables are updated

**Phi node in loop header (Block 94a0a878)**:
```rust
sum_phi = phi([
  (result_of_sum+i, Block_f2445c86),  // From "initialization" block
  (sum_phi, Continue_block),           // Self-referencing (correct!)
  (some_value, Block_e0f631ba)
])
```

The phi expects incoming values from f2445c86 for BOTH:
- Initial entry (should provide initial value 0)
- Loop iteration (should provide sum+i result)

This creates a circular dependency that causes the infinite loop.

### Comparison with Working HIR Test

**Working test** (`continue_debug_test.rs`):
- Separate blocks for initialization and updates
- Clear phi incoming values from distinct blocks
- Test PASSES and returns correct result (8)

**Failing Zig test**:
- Shared block for initialization and updates
- Ambiguous phi incoming values
- Test HANGS indefinitely

## Investigation Steps Taken

### 1. SSA Phi Structure Analysis
- Added debug output to trace phi node filling
- Confirmed: SSA generates **correct** self-referencing phis
- Output shows: `sum_phi` correctly references itself from continue block
- **Conclusion**: SSA construction is NOT the bug

### 2. Cranelift Backend Verification
- Created pure HIR test with manually constructed continue loop
- Test uses identical self-referencing phi structure
- Test **PASSES** and executes correctly
- **Conclusion**: Cranelift backend correctly handles continues

### 3. TypedAST→HIR Lowering Analysis
- Examined TypedCfgBuilder.split_at_control_flow()
- While loop CFG construction appears correct:
  - Creates header, body, exit blocks
  - Closes initialization block before loop
  - Processes body recursively
- If statement handling creates merge blocks correctly
- **BUT**: Block IDs may be reused incorrectly

### 4. Variable Initialization Handling
- `var sum = 0` becomes `TypedStatement::Let`
- Let statements fall through to default case
- Added to `current_statements` without special handling
- May not create separate initialization block

## Potential Fix Locations

### Option 1: TypedCFG Builder (`typed_cfg.rs`)
**File**: `crates/compiler/src/typed_cfg.rs`
**Function**: `split_at_control_flow()`, lines 283-330 (while loop handling)

**Issue**: When processing while loop body with continue, the merge block after the if statement may have the same ID as a previous block.

**Potential Fix**:
```rust
// After line 313: let (body_blocks, _, body_exit) = self.split_at_control_flow(&while_stmt.body, body_id)?;

// Force creation of new block if body_exit wasn't properly closed
if all_blocks.iter().any(|b| b.id == body_exit &&
   !matches!(b.terminator, TypedTerminator::Unreachable)) {
    // Body exit already has terminator, create new merge block
    let new_exit = self.new_block_id();
    // ... handle transition
}
```

### Option 2: SSA Builder (`ssa.rs`)
**File**: `crates/compiler/src/ssa.rs`
**Function**: `build_from_typed_cfg()`

**Issue**: When converting TypedCFG to SSA, variable initializations may not create a separate logical block before the loop.

**Potential Fix**: Ensure Let statements before loops create proper initial definitions in a distinct block context.

### Option 3: Block ID Management
**Issue**: The TypedCFG builder may be reusing block IDs when it shouldn't.

**Investigation Needed**:
- Add logging to track block ID creation and reuse
- Verify `new_block_id()` always returns unique IDs
- Check if `current_block_id` is being reset incorrectly

## Recommended Next Steps

1. **Add TypedCFG dumping** to failing test
   - Print all TypedBasicBlocks before SSA conversion
   - Check if block reuse is visible in TypedCFG or only in HIR

2. **Compare TypedCFG structures**
   - Working HIR test (manually constructed)
   - Failing Zig test (from TypedAST)
   - Identify exact structural difference

3. **Trace block ID lifecycle**
   - Add debug output showing when blocks are created
   - Track which block IDs are used for what purpose
   - Find where reuse occurs

4. **Targeted Fix**
   - Once exact location identified, ensure:
     - Initialization block != merge block
     - Each block has single, clear purpose
     - Phi incoming values map to correct blocks

## Files Modified During Investigation

- `crates/compiler/src/cranelift_backend.rs` - Added `get_ir_string()` method
- `crates/compiler/tests/continue_debug_test.rs` - Working HIR test (PASSES)
- `crates/zyn_parser/tests/zig_e2e_jit.rs` - Added HIR structure dumping
- `crates/compiler/src/ssa.rs` - Temporary debug output (removed)

## Key Insights

1. **Bug is NOT in**:
   - SSA phi construction (verified correct)
   - Cranelift backend (verified with working test)

2. **Bug IS in**:
   - TypedAST→HIR lowering phase
   - Specifically: block creation/reuse during CFG construction

3. **Fix requires**:
   - Ensuring initialization and loop update blocks are distinct
   - Proper block ID management in TypedCfgBuilder
   - Possibly special handling for Let statements before loops

## Success Criteria

- [ ] `test_zig_jit_continue` test passes
- [ ] Returns correct value (25 for input 10)
- [ ] No infinite loop during execution
- [ ] HIR structure shows distinct initialization and update blocks
- [ ] Phi nodes have clear, non-ambiguous incoming values

## Related Issues

- Short-circuit evaluation (completed, using bitwise ops)
- Logical operators (completed, tests passing)
- Array support (pending)
- String support (pending)
