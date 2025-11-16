package zyntax;

#if (macro || zyntax_runtime)

import haxe.io.Bytes;
import haxe.io.BytesOutput;

/**
 * HIR (High-level Intermediate Representation) types for Zyntax
 * This mirrors the Rust HIR structures for bytecode generation
 */

typedef HirId = String; // UUID as hex string

enum HirType {
    Void;
    Bool;
    I8;
    I16;
    I32;
    I64;
    I128;
    U8;
    U16;
    U32;
    U64;
    U128;
    F32;
    F64;
    Ptr(inner: HirType);
}

enum CallingConvention {
    Cdecl;    // C calling convention
    Rust;     // Rust calling convention
    Fast;     // Fast calling convention
    System;   // System calling convention
}

enum Linkage {
    Private;
    Public;
    External;
}

enum Visibility {
    Default;
    Hidden;
    Protected;
}

typedef HirParameter = {
    id: HirId,
    name: String,
    ty: HirType,
    attributes: ParamAttributes
}

typedef ParamAttributes = {
    by_ref: Bool,
    sret: Bool,
    zext: Bool,
    sext: Bool,
    noalias: Bool,
    nonnull: Bool,
    readonly: Bool
}

typedef HirSignature = {
    params: Array<HirParameter>,
    returns: Array<HirType>,
    is_variadic: Bool,
    is_async: Bool
}

enum BinaryOp {
    Add;
    Sub;
    Mul;
    Div;
    Rem;
    And;
    Or;
    Xor;
    Shl;
    Shr;
    Eq;
    Ne;
    Lt;
    Le;
    Gt;
    Ge;
}

enum HirValueKind {
    Instruction;
    Parameter(index: Int);
    Constant(value: HirConstant);
    Global(id: HirId);
    Undef;
}

enum HirConstant {
    Bool(value: Bool);
    I8(value: Int);
    I16(value: Int);
    I32(value: Int);
    I64(value: Int);
    I128(value: Int);
    U8(value: Int);
    U16(value: Int);
    U32(value: Int);
    U64(value: Int);
    U128(value: Int);
    F32(value: Float);
    F64(value: Float);
    String(value: String);
}

typedef HirValue = {
    id: HirId,
    ty: HirType,
    kind: HirValueKind,
    uses: Array<HirId>,  // HashSet in Rust
    span: Null<{start: Int, end: Int}>  // Option<Span> in Rust
}

enum HirCallable {
    Function(id: HirId);
    Indirect(value: HirId);
}

enum HirInstruction {
    Binary(op: BinaryOp, result: HirId, ty: HirType, left: HirId, right: HirId);
    Call(result: Null<HirId>, callee: HirCallable, args: Array<HirId>, type_args: Array<HirType>, is_tail: Bool);
    Alloca(result: HirId, ty: HirType, count: Null<HirId>, align: Int);
    Load(result: HirId, ty: HirType, ptr: HirId, align: Int, is_volatile: Bool);
    Store(value: HirId, ptr: HirId, align: Int, is_volatile: Bool);
}

typedef HirPhi = {
    result: HirId,
    ty: HirType,
    incoming: Array<{value: HirId, block: HirId}>
}

enum HirTerminator {
    Return(values: Array<HirId>);
    Branch(target: HirId);
    CondBranch(condition: HirId, true_target: HirId, false_target: HirId);
    Unreachable;
}

typedef HirBasicBlock = {
    id: HirId,
    label: Null<String>,
    phis: Array<HirPhi>,
    instructions: Array<HirInstruction>,
    terminator: HirTerminator
}

typedef HirFunction = {
    id: HirId,
    name: String,
    signature: HirSignature,
    calling_convention: CallingConvention,
    is_external: Bool,
    entry_block: HirId,
    blocks: Map<HirId, HirBasicBlock>,
    values: Map<HirId, HirValue>
}

typedef HirGlobal = {
    id: HirId,
    name: String,
    ty: HirType,
    initializer: Null<HirConstant>,
    is_const: Bool,
    is_thread_local: Bool,
    linkage: Linkage,
    visibility: Visibility
}

typedef HirModule = {
    id: HirId,
    name: String,
    functions: Map<HirId, HirFunction>,
    globals: Map<HirId, HirGlobal>,
    types: Map<HirId, HirType>,
    imports: Array<String>,
    exports: Array<String>,
    version: Int
}

#end
