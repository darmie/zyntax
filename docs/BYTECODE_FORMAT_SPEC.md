# Zyntax Bytecode Format Specification v1.0

**Version**: 1.0.0
**Date**: November 13, 2025
**Status**: Draft Specification

---

## Executive Summary

This document specifies the **Zyntax Bytecode Format**, a platform-independent binary serialization format for HIR (High-level Intermediate Representation). This enables language implementers to target Zyntax without using Rust, by emitting bytecode that can be deserialized and compiled.

**Key Benefits**:
- Language-agnostic compiler frontend implementation
- Cross-platform HIR distribution
- Hot-reloading and dynamic code loading
- Compilation caching and incremental compilation
- Remote compilation and code distribution

---

## Design Goals

1. **Language Independence**: Any language can emit Zyntax bytecode
2. **Compact Representation**: Efficient binary encoding
3. **Fast Deserialization**: Minimal parsing overhead
4. **Version Compatibility**: Forward and backward compatibility via versioning
5. **Complete Fidelity**: Preserve all HIR semantics
6. **Debuggability**: Support for debug info and source maps
7. **Security**: Validation and sandboxing support

---

## File Format Overview

### File Extension
- **Binary**: `.zbc` (Zyntax ByteCode)
- **Text/Debug**: `.zbct` (Zyntax ByteCode Text) - JSON representation

### File Structure

```
┌─────────────────────────────────────┐
│ File Header (32 bytes)              │
├─────────────────────────────────────┤
│ String Table Section                │
├─────────────────────────────────────┤
│ Type Table Section                  │
├─────────────────────────────────────┤
│ Constant Pool Section               │
├─────────────────────────────────────┤
│ Global Definitions Section          │
├─────────────────────────────────────┤
│ Function Definitions Section        │
├─────────────────────────────────────┤
│ Import/Export Section               │
├─────────────────────────────────────┤
│ Debug Info Section (optional)       │
├─────────────────────────────────────┤
│ Metadata Section (optional)         │
└─────────────────────────────────────┘
```

---

## 1. File Header (32 bytes)

### Layout

| Offset | Size | Field | Description |
|--------|------|-------|-------------|
| 0x00 | 4 | magic | Magic number: `0x5A424300` ("ZBC\0") |
| 0x04 | 2 | major_version | Major version (1) |
| 0x06 | 2 | minor_version | Minor version (0) |
| 0x08 | 4 | flags | Compilation flags (bitfield) |
| 0x0C | 8 | module_id | UUID of module (16 bytes in v2) |
| 0x14 | 4 | string_table_offset | Offset to string table |
| 0x18 | 4 | string_table_size | Size of string table |
| 0x1C | 4 | checksum | CRC32 checksum of entire file (excluding this field) |

### Flags Bitfield

| Bit | Name | Description |
|-----|------|-------------|
| 0 | DEBUG_INFO | Debug information included |
| 1 | OPTIMIZED | Code has been optimized |
| 2 | POSITION_INDEPENDENT | Position-independent code |
| 3 | HOT_RELOAD | Supports hot-reloading |
| 4-7 | RESERVED | Reserved for future use |

### Example

```rust
struct BytecodeHeader {
    magic: u32,              // 0x5A424300
    major_version: u16,      // 1
    minor_version: u16,      // 0
    flags: u32,              // Bitfield
    module_id: [u8; 16],     // UUID bytes
    string_table_offset: u32,
    string_table_size: u32,
    checksum: u32,
}
```

---

## 2. String Table Section

Stores all string literals and identifiers used in the module.

### Format

```
┌────────────────────────┐
│ string_count: u32      │ Number of strings
├────────────────────────┤
│ String 0               │
│   length: u32          │ UTF-8 byte length
│   data: [u8; length]   │ UTF-8 bytes
├────────────────────────┤
│ String 1               │
│   ...                  │
├────────────────────────┤
│ String N               │
└────────────────────────┘
```

### String References

Throughout the bytecode, strings are referenced by **index** into the string table (0-based).

**Example**: String index `5` refers to the 6th string in the table.

---

## 3. Type Table Section

Defines all types used in the module.

### Format

```
┌────────────────────────┐
│ type_count: u32        │ Number of type definitions
├────────────────────────┤
│ Type 0                 │
├────────────────────────┤
│ Type 1                 │
├────────────────────────┤
│ Type N                 │
└────────────────────────┘
```

### Type Encoding

Each type is encoded as:

```
┌────────────────────────┐
│ type_tag: u8           │ Type discriminator
├────────────────────────┤
│ type_data: varies      │ Type-specific data
└────────────────────────┘
```

### Type Tags

| Tag | Type | Payload |
|-----|------|---------|
| 0x00 | Void | None |
| 0x01 | Bool | None |
| 0x02 | I8 | None |
| 0x03 | I16 | None |
| 0x04 | I32 | None |
| 0x05 | I64 | None |
| 0x06 | I128 | None |
| 0x07 | U8 | None |
| 0x08 | U16 | None |
| 0x09 | U32 | None |
| 0x0A | U64 | None |
| 0x0B | U128 | None |
| 0x0C | F32 | None |
| 0x0D | F64 | None |
| 0x10 | Ptr | type_index: u32 |
| 0x11 | Ref | lifetime_index: u32, type_index: u32, mutable: u8 |
| 0x12 | Array | element_type: u32, length: u64 |
| 0x13 | Vector | element_type: u32, length: u32 |
| 0x20 | Struct | name_index: u32, field_count: u32, fields: [u32; count], packed: u8 |
| 0x21 | Union | name_index: u32, variant_count: u32, variants: [...], discriminant_type: u32, is_c_union: u8 |
| 0x30 | Function | param_count: u32, params: [u32], return_count: u32, returns: [u32], is_variadic: u8 |
| 0x31 | Closure | function_type: u32, capture_count: u32, captures: [...], call_mode: u8 |
| 0x40 | TraitObject | trait_id: UUID (16 bytes), vtable_index: u32 |
| 0x41 | Interface | method_count: u32, methods: [...], is_structural: u8 |
| 0x42 | AssociatedType | trait_id: UUID, self_type: u32, name_index: u32 |
| 0x50 | Opaque | name_index: u32 |
| 0x51 | ConstGeneric | name_index: u32 |
| 0x52 | Generic | base_type: u32, type_arg_count: u32, type_args: [u32], const_arg_count: u32, const_args: [u32] |

---

## 4. Constant Pool Section

Stores constant values referenced by instructions.

### Format

```
┌────────────────────────┐
│ constant_count: u32    │
├────────────────────────┤
│ Constant 0             │
├────────────────────────┤
│ Constant 1             │
├────────────────────────┤
│ Constant N             │
└────────────────────────┘
```

### Constant Encoding

```
┌────────────────────────┐
│ const_tag: u8          │ Constant type discriminator
├────────────────────────┤
│ const_data: varies     │ Constant-specific data
└────────────────────────┘
```

### Constant Tags

| Tag | Type | Payload |
|-----|------|---------|
| 0x00 | Bool | value: u8 (0 or 1) |
| 0x01 | I8 | value: i8 |
| 0x02 | I16 | value: i16 (little-endian) |
| 0x03 | I32 | value: i32 (little-endian) |
| 0x04 | I64 | value: i64 (little-endian) |
| 0x05 | I128 | value: i128 (little-endian, 16 bytes) |
| 0x06 | U8 | value: u8 |
| 0x07 | U16 | value: u16 (little-endian) |
| 0x08 | U32 | value: u32 (little-endian) |
| 0x09 | U64 | value: u64 (little-endian) |
| 0x0A | U128 | value: u128 (little-endian, 16 bytes) |
| 0x0B | F32 | value: f32 (IEEE 754, little-endian) |
| 0x0C | F64 | value: f64 (IEEE 754, little-endian) |
| 0x10 | Null | type_index: u32 |
| 0x20 | Array | element_count: u32, elements: [const_index; count] |
| 0x21 | Struct | field_count: u32, fields: [const_index; count] |
| 0x30 | String | string_index: u32 |
| 0x40 | VTable | vtable_id: UUID, method_count: u32, methods: [function_index; count] |

---

## 5. Global Definitions Section

Defines global variables and constants.

### Format

```
┌────────────────────────┐
│ global_count: u32      │
├────────────────────────┤
│ Global 0               │
│   id: UUID (16 bytes)  │
│   name_index: u32      │
│   type_index: u32      │
│   is_mutable: u8       │
│   is_external: u8      │
│   initializer: u32     │ Constant index (0xFFFFFFFF if none)
│   linkage: u8          │ 0=private, 1=public, 2=external
├────────────────────────┤
│ Global 1               │
│   ...                  │
└────────────────────────┘
```

---

## 6. Function Definitions Section

The core section containing all function definitions.

### Format

```
┌────────────────────────┐
│ function_count: u32    │
├────────────────────────┤
│ Function 0             │
├────────────────────────┤
│ Function 1             │
├────────────────────────┤
│ Function N             │
└────────────────────────┘
```

### Function Encoding

```
┌───────────────────────────────────┐
│ Function Header                   │
│   id: UUID (16 bytes)             │
│   name_index: u32                 │
│   is_external: u8                 │
│   calling_convention: u8          │
│   signature_offset: u32           │ Offset to signature data
│   body_offset: u32                │ Offset to body (0 if external)
├───────────────────────────────────┤
│ Function Signature                │
│   param_count: u32                │
│   params: [Param; count]          │
│   return_count: u32               │
│   returns: [u32; count]           │ Type indices
│   type_param_count: u32           │
│   type_params: [TypeParam; count] │
│   const_param_count: u32          │
│   const_params: [ConstParam; count]│
│   lifetime_param_count: u32       │
│   lifetime_params: [Lifetime; count]│
│   is_variadic: u8                 │
│   is_async: u8                    │
├───────────────────────────────────┤
│ Function Body (if not external)   │
│   entry_block: UUID               │
│   block_count: u32                │
│   blocks: [Block; count]          │
│   local_count: u32                │
│   locals: [Local; count]          │
│   value_count: u32                │
│   values: [Value; count]          │
└───────────────────────────────────┘
```

### Parameter Encoding

```
struct Param {
    id: UUID,                // 16 bytes
    name_index: u32,         // String table index
    type_index: u32,         // Type table index
    attributes: u16,         // Bitfield: by_ref, sret, zext, sext, noalias, nonnull, readonly
}
```

### Basic Block Encoding

```
┌───────────────────────────────────┐
│ Block Header                      │
│   id: UUID (16 bytes)             │
│   label_index: u32                │ String table index (0xFFFFFFFF if none)
│   phi_count: u32                  │
│   instruction_count: u32          │
│   predecessor_count: u32          │
│   predecessors: [UUID; count]     │
│   successor_count: u32            │
│   successors: [UUID; count]       │
├───────────────────────────────────┤
│ Phi Nodes                         │
│   phi_0: PhiNode                  │
│   phi_1: PhiNode                  │
│   ...                             │
├───────────────────────────────────┤
│ Instructions                      │
│   instr_0: Instruction            │
│   instr_1: Instruction            │
│   ...                             │
├───────────────────────────────────┤
│ Terminator                        │
│   terminator: Terminator          │
└───────────────────────────────────┘
```

### Phi Node Encoding

```
struct PhiNode {
    result_id: UUID,             // 16 bytes
    type_index: u32,
    incoming_count: u32,
    incoming: [(UUID, UUID); count],  // (value_id, block_id) pairs
}
```

---

## 7. Instruction Encoding

Each instruction is encoded with an opcode followed by operands.

### Instruction Format

```
┌────────────────────────┐
│ opcode: u8             │ Instruction opcode
├────────────────────────┤
│ operands: varies       │ Opcode-specific operands
└────────────────────────┘
```

### Instruction Opcodes

| Opcode | Instruction | Operands |
|--------|-------------|----------|
| 0x00 | Binary | op: u8, result: UUID, type: u32, left: UUID, right: UUID |
| 0x01 | Unary | op: u8, result: UUID, type: u32, operand: UUID |
| 0x02 | Alloca | result: UUID, type: u32, count: UUID (or NULL), align: u32 |
| 0x03 | Load | result: UUID, type: u32, ptr: UUID, align: u32, volatile: u8 |
| 0x04 | Store | value: UUID, ptr: UUID, align: u32, volatile: u8 |
| 0x05 | GetElementPtr | result: UUID, type: u32, ptr: UUID, index_count: u32, indices: [UUID] |
| 0x06 | Call | result: UUID (or NULL), callee_type: u8, callee: varies, arg_count: u32, args: [UUID], type_arg_count: u32, type_args: [u32], const_arg_count: u32, const_args: [u32], is_tail: u8 |
| 0x07 | IndirectCall | result: UUID (or NULL), func_ptr: UUID, arg_count: u32, args: [UUID], return_type: u32 |
| 0x08 | Cast | op: u8, result: UUID, type: u32, operand: UUID |
| 0x09 | Select | result: UUID, type: u32, condition: UUID, true_val: UUID, false_val: UUID |
| 0x0A | ExtractValue | result: UUID, type: u32, aggregate: UUID, index_count: u32, indices: [u32] |
| 0x0B | InsertValue | result: UUID, type: u32, aggregate: UUID, value: UUID, index_count: u32, indices: [u32] |
| 0x0C | Atomic | op: u8, result: UUID, type: u32, ptr: UUID, value: UUID (or NULL), ordering: u8 |
| 0x0D | Fence | ordering: u8 |
| 0x10 | CreateUnion | result: UUID, union_type: u32, variant_index: u32, value: UUID |
| 0x11 | GetUnionDiscriminant | result: UUID, union_val: UUID |
| 0x12 | ExtractUnionValue | result: UUID, type: u32, union_val: UUID, variant_index: u32 |
| 0x20 | CreateTraitObject | result: UUID, trait_id: UUID, data_ptr: UUID, vtable_id: UUID |
| 0x21 | UpcastTraitObject | result: UUID, sub_trait_object: UUID, sub_trait_id: UUID, super_trait_id: UUID, super_vtable_id: UUID |
| 0x22 | TraitMethodCall | result: UUID (or NULL), trait_object: UUID, method_index: u32, method_sig: MethodSignature, arg_count: u32, args: [UUID], return_type: u32 |
| 0x30 | CreateClosure | result: UUID, closure_type: u32, function: UUID, capture_count: u32, captures: [UUID] |
| 0x31 | CallClosure | result: UUID (or NULL), closure: UUID, arg_count: u32, args: [UUID] |
| 0x40 | CreateRef | result: UUID, value: UUID, lifetime_index: u32, mutable: u8 |
| 0x41 | Deref | result: UUID, type: u32, reference: UUID |
| 0x42 | Move | result: UUID, type: u32, source: UUID |
| 0x43 | Copy | result: UUID, type: u32, source: UUID |
| 0x50 | BeginLifetime | lifetime_index: u32 |
| 0x51 | EndLifetime | lifetime_index: u32 |
| 0x52 | LifetimeConstraint | longer: u32, shorter: u32 |

### Binary/Unary Operation Tags

**BinaryOp** (for opcode 0x00):
| Tag | Operation |
|-----|-----------|
| 0x00 | Add |
| 0x01 | Sub |
| 0x02 | Mul |
| 0x03 | Div |
| 0x04 | Rem |
| 0x05 | And |
| 0x06 | Or |
| 0x07 | Xor |
| 0x08 | Shl |
| 0x09 | Shr |
| 0x0A | Eq |
| 0x0B | Ne |
| 0x0C | Lt |
| 0x0D | Le |
| 0x0E | Gt |
| 0x0F | Ge |
| 0x10 | FAdd |
| 0x11 | FSub |
| 0x12 | FMul |
| 0x13 | FDiv |
| 0x14 | FRem |
| 0x15 | FEq |
| 0x16 | FNe |
| 0x17 | FLt |
| 0x18 | FLe |
| 0x19 | FGt |
| 0x1A | FGe |

**UnaryOp** (for opcode 0x01):
| Tag | Operation |
|-----|-----------|
| 0x00 | Neg |
| 0x01 | Not |
| 0x02 | FNeg |

**CastOp** (for opcode 0x08):
| Tag | Operation |
|-----|-----------|
| 0x00 | BitCast |
| 0x01 | ZExt |
| 0x02 | SExt |
| 0x03 | Trunc |
| 0x04 | FPTrunc |
| 0x05 | FPExt |
| 0x06 | FPToUI |
| 0x07 | FPToSI |
| 0x08 | UIToFP |
| 0x09 | SIToFP |
| 0x0A | PtrToInt |
| 0x0B | IntToPtr |

---

## 8. Terminator Encoding

Block terminators control control flow.

### Terminator Format

```
┌────────────────────────┐
│ terminator_tag: u8     │ Terminator type
├────────────────────────┤
│ terminator_data: varies│ Terminator-specific data
└────────────────────────┘
```

### Terminator Tags

| Tag | Terminator | Payload |
|-----|------------|---------|
| 0x00 | Return | value_count: u32, values: [UUID; count] |
| 0x01 | Branch | target: UUID |
| 0x02 | CondBranch | condition: UUID, true_target: UUID, false_target: UUID |
| 0x03 | Switch | value: UUID, default: UUID, case_count: u32, cases: [(const_index, block_id); count] |
| 0x04 | Unreachable | None |
| 0x05 | Invoke | callee_type: u8, callee: varies, arg_count: u32, args: [UUID], normal: UUID, unwind: UUID |
| 0x06 | PatternMatch | value: UUID, pattern_count: u32, patterns: [Pattern; count], default: UUID (or NULL) |

---

## 9. Import/Export Section

Defines module imports and exports.

### Format

```
┌────────────────────────┐
│ import_count: u32      │
├────────────────────────┤
│ Import 0               │
│   name_index: u32      │
│   module_name: u32     │
│   kind: u8             │ 0=function, 1=global, 2=type
│   id: UUID             │
├────────────────────────┤
│ ...                    │
├────────────────────────┤
│ export_count: u32      │
├────────────────────────┤
│ Export 0               │
│   name_index: u32      │
│   kind: u8             │ 0=function, 1=global, 2=type
│   id: UUID             │
├────────────────────────┤
│ ...                    │
└────────────────────────┘
```

---

## 10. Debug Info Section (Optional)

Provides source location mapping and debug symbols.

### Format

```
┌────────────────────────┐
│ debug_magic: u32       │ 0x44424700 ("DBG\0")
├────────────────────────┤
│ source_file_count: u32 │
├────────────────────────┤
│ Source Files           │
│   file_0:              │
│     path_index: u32    │
│     hash: [u8; 32]     │ SHA-256 of file contents
├────────────────────────┤
│ location_count: u32    │
├────────────────────────┤
│ Locations              │
│   loc_0:               │
│     instruction_id: UUID│
│     file_index: u32    │
│     line: u32          │
│     column: u32        │
│     length: u32        │
└────────────────────────┘
```

---

## 11. Metadata Section (Optional)

Custom metadata for tooling, profiling, etc.

### Format

```
┌────────────────────────┐
│ metadata_magic: u32    │ 0x4D455400 ("MET\0")
├────────────────────────┤
│ entry_count: u32       │
├────────────────────────┤
│ Metadata Entry 0       │
│   key_index: u32       │ String table index
│   value_size: u32      │
│   value_data: [u8]     │
├────────────────────────┤
│ ...                    │
└────────────────────────┘
```

---

## Usage Examples

### Example 1: Simple Function

**Zyntax Source**:
```rust
fn add(a: i32, b: i32) -> i32 {
    a + b
}
```

**Bytecode (Conceptual)**:
```
Header: [magic=ZBC, version=1.0, ...]

StringTable:
  0: "add"
  1: "a"
  2: "b"

TypeTable:
  0: I32

Functions:
  Function {
    id: <uuid>,
    name: 0,  // "add"
    signature: {
      params: [
        Param { id: <uuid-a>, name: 1, type: 0 },  // a: i32
        Param { id: <uuid-b>, name: 2, type: 0 },  // b: i32
      ],
      returns: [0],  // i32
    },
    blocks: [
      Block {
        id: <uuid-entry>,
        instructions: [
          Binary {
            op: Add,
            result: <uuid-result>,
            type: 0,  // i32
            left: <uuid-a>,
            right: <uuid-b>,
          }
        ],
        terminator: Return { values: [<uuid-result>] }
      }
    ]
  }
```

### Example 2: Generic Function with Trait

**Zyntax Source**:
```rust
fn identity<T>(x: T) -> T {
    x
}
```

**Bytecode** would include:
- Type parameter `T` in signature
- Move or Copy instruction depending on trait bounds
- Monomorphization handled at link time

---

## Implementation Guidelines

### For Bytecode Producers (Language Frontends)

1. **Build String Table First**: Collect all identifiers and strings
2. **Build Type Table**: Encode all types with proper indices
3. **Build Constant Pool**: Encode all constant values
4. **Build Functions**: Encode function signatures and bodies
5. **Generate UUIDs**: Use UUID v4 for all HirIds
6. **Validate**: Ensure all references are valid
7. **Calculate Checksum**: CRC32 of entire file
8. **Write Binary**: Write sections in order

### For Bytecode Consumers (Zyntax Compiler)

1. **Validate Header**: Check magic number and version
2. **Verify Checksum**: Ensure file integrity
3. **Load String Table**: Deserialize all strings
4. **Load Type Table**: Reconstruct type definitions
5. **Load Constant Pool**: Deserialize constants
6. **Load Functions**: Reconstruct HIR functions
7. **Validate References**: Ensure all UUIDs and indices are valid
8. **Build HIR Module**: Construct complete HirModule

---

## Versioning and Compatibility

### Version Number Format

**Major.Minor** (e.g., 1.0)

- **Major**: Incompatible changes (breaking)
- **Minor**: Backward-compatible additions

### Compatibility Rules

1. **Forward Compatibility**: Older compilers can ignore unknown minor features
2. **Backward Compatibility**: Newer compilers must support older bytecode
3. **Deprecation**: Features deprecated for 2 major versions before removal

### Version History

| Version | Date | Changes |
|---------|------|---------|
| 1.0 | 2025-11-13 | Initial specification |

---

## Security Considerations

### Validation

All bytecode must be validated before execution:

1. **Bounds Checking**: All indices must be within valid ranges
2. **Type Safety**: All operations must be type-correct
3. **CFG Validation**: Control flow graph must be well-formed
4. **SSA Validation**: SSA form must be valid
5. **Lifetime Validation**: Lifetime constraints must be satisfiable

### Sandboxing

Bytecode can be sandboxed:

1. **Memory Limits**: Restrict allocations
2. **Capability-based Security**: Limit system calls
3. **Resource Quotas**: CPU time, memory, I/O

---

## Tooling

### Recommended Tools

1. **zbcdump**: Disassembler for `.zbc` files
2. **zbcvalid**: Validator for bytecode
3. **zbc2json**: Convert binary to JSON representation
4. **json2zbc**: Convert JSON to binary bytecode

---

## References

1. **WebAssembly Binary Format**: Inspiration for compact encoding
2. **LLVM Bitcode**: Similar IR serialization approach
3. **JVM Class File Format**: Constant pool and string table design
4. **Protocol Buffers**: Binary serialization best practices

---

## Appendix: Complete Type Tag Reference

See Section 3 for complete type tag listing.

---

## Appendix: Complete Opcode Reference

See Section 7 for complete instruction opcode listing.

---

**Last Updated**: November 13, 2025
**Specification Version**: 1.0.0
**Status**: Draft - Ready for Implementation
