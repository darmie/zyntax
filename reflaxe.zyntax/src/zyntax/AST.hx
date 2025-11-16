package zyntax;

#if (macro || zyntax_runtime)

import haxe.macro.Expr.Position;

/**
 * Source span information
 */
typedef Span = {
    start: Int,
    end: Int
}

/**
 * Intermediate representation for a Zyntax module
 */
typedef Module = {
    name: String,
    functions: Array<Function>
}

/**
 * Zyntax function
 */
typedef Function = {
    name: String,
    params: Array<Param>,
    return_type: ZType,
    body: Null<ExprWithPos>,
    is_external: Bool,
    link_name: Null<String>,
    pos: Position
}

/**
 * Function parameter
 */
typedef Param = {
    name: String,
    ty: ZType,
    pos: Position
}

/**
 * Zyntax type
 */
enum ZType {
    Primitive(name: String);
    Function(params: Array<ZType>, returnType: ZType);
}

/**
 * Placeholder for enum (not implemented yet)
 */
class Enum {
}

/**
 * Expression with position and type information
 */
typedef ExprWithPos = {
    expr: Expr,
    ty: ZType,
    pos: Position
}

/**
 * Zyntax expression AST
 */
enum Expr {
    IntLiteral(value: Int);
    FloatLiteral(value: Float);
    StringLiteral(value: String);
    BoolLiteral(value: Bool);
    Variable(name: String);
    BinaryOp(op: String, left: ExprWithPos, right: ExprWithPos);
    Call(callee: ExprWithPos, args: Array<ExprWithPos>);
    Block(exprs: Array<ExprWithPos>);
    Return(value: Null<ExprWithPos>);
    Let(name: String, ty: ZType, init: Null<ExprWithPos>);
    If(cond: ExprWithPos, thenExpr: ExprWithPos, elseExpr: Null<ExprWithPos>);
    While(cond: ExprWithPos, body: ExprWithPos);
}

#end
