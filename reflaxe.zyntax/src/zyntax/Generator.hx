package zyntax;

#if (macro || zyntax_runtime)

import haxe.Json;
import haxe.macro.Expr.Position;

/**
 * Convert Haxe Position to span {start, end}
 */
function posToSpan(pos: Position): Dynamic {
    // Use PositionTools to get the range
    #if macro
    var range = haxe.macro.PositionTools.getInfos(pos);
    return {
        start: range.min,
        end: range.max
    };
    #else
    return {start: 0, end: 0};
    #end
}

/**
 * Generate Zyntax TypedAST JSON from Module
 */
function generateModule(m: AST.Module): Null<String> {
    var declarations = [];

    for (func in m.functions) {
        var funcDecl = {
            node: {
                Function: {
                    name: func.name,
                    params: func.params.map(p -> {
                        name: p.name,
                        ty: generateType(p.ty),
                        mutability: "Immutable",
                        kind: "Regular",
                        default_value: null,
                        attributes: [],
                        span: posToSpan(p.pos)
                    }),
                    return_type: generateType(func.return_type),
                    body: func.body != null ? generateBlock(func.body) : null,
                    visibility: "Public",
                    is_async: false,
                    is_external: func.is_external,
                    calling_convention: func.is_external ? "C" : "Rust",
                    link_name: func.link_name
                }
            },
            ty: {Primitive: "Unit"}, // Declarations themselves have Unit type
            span: posToSpan(func.pos)
        };
        declarations.push(funcDecl);
    }

    var program = {
        declarations: declarations,
        span: {start: 0, end: 0}
    };

    return Json.stringify(program, null, "  ");
}

/**
 * Generate type JSON
 */
function generateType(t: AST.ZType): Dynamic {
    return switch(t) {
        case Primitive(name): {Primitive: name};
        case Function(params, returnType): {
            Function: {
                params: params.map(p -> {
                    name: null,
                    ty: generateType(p),
                    is_optional: false,
                    is_varargs: false,
                    is_keyword_only: false,
                    is_positional_only: false,
                    is_out: false,
                    is_ref: false,
                    is_inout: false
                }),
                return_type: generateType(returnType),
                is_varargs: false,
                has_named_params: false,
                has_default_params: false,
                async_kind: "Sync",
                calling_convention: "Rust",
                nullability: "NonNull"
            }
        };
    }
}

/**
 * Generate block from expression with position
 */
function generateBlock(exprWithPos: AST.ExprWithPos): Dynamic {
    var statements = switch(exprWithPos.expr) {
        case Block(exprs):
            exprs.map(e -> generateStatement(e));
        default:
            [generateStatement(exprWithPos)];
    };

    return {
        statements: statements,
        span: posToSpan(exprWithPos.pos)
    };
}

/**
 * Generate statement JSON
 */
function generateStatement(exprWithPos: AST.ExprWithPos): Dynamic {
    var pos = exprWithPos.pos;
    var stmtTy = {Primitive: "Unit"}; // Statements always have Unit type

    return switch(exprWithPos.expr) {
        case Return(value):
            {
                node: {
                    Return: value != null ? generateExpression(value) : null
                },
                ty: stmtTy,
                span: posToSpan(pos)
            };

        case Let(name, varType, init):
            {
                node: {
                    Let: {
                        name: name,
                        ty: generateType(varType),
                        mutability: "Mutable",
                        initializer: init != null ? generateExpression(init) : null,
                        visibility: "Private",
                        span: posToSpan(pos)
                    }
                },
                ty: stmtTy,
                span: posToSpan(pos)
            };

        default:
            {
                node: {
                    Expression: generateExpression(exprWithPos)
                },
                ty: stmtTy,
                span: posToSpan(pos)
            };
    }
}

/**
 * Generate expression JSON
 */
function generateExpression(exprWithPos: AST.ExprWithPos): Dynamic {
    var pos = exprWithPos.pos;
    var ty = generateType(exprWithPos.ty);

    return switch(exprWithPos.expr) {
        case IntLiteral(value):
            {
                node: {
                    Literal: {Integer: value}
                },
                ty: ty,
                span: posToSpan(pos)
            };

        case FloatLiteral(value):
            {
                node: {
                    Literal: {Float: value}
                },
                ty: ty,
                span: posToSpan(pos)
            };

        case StringLiteral(value):
            {
                node: {
                    Literal: {String: value}
                },
                ty: ty,
                span: posToSpan(pos)
            };

        case BoolLiteral(value):
            {
                node: {
                    Literal: {Bool: value}
                },
                ty: ty,
                span: posToSpan(pos)
            };

        case Variable(name):
            {
                node: {Variable: name},
                ty: ty,
                span: posToSpan(pos)
            };

        case BinaryOp(op, left, right):
            {
                node: {
                    Binary: {
                        op: op,
                        left: generateExpression(left),
                        right: generateExpression(right)
                    }
                },
                ty: ty,
                span: posToSpan(pos)
            };

        case Call(callee, args):
            {
                node: {
                    Call: {
                        callee: generateExpression(callee),
                        positional_args: args.map(generateExpression),
                        named_args: [],
                        type_args: []
                    }
                },
                ty: ty,
                span: posToSpan(pos)
            };

        case If(cond, thenExpr, elseExpr):
            {
                node: {
                    If: {
                        condition: generateExpression(cond),
                        then_branch: generateExpression(thenExpr),
                        else_branch: elseExpr != null ? generateExpression(elseExpr) : null
                    }
                },
                ty: ty,
                span: posToSpan(pos)
            };

        case While(cond, body):
            {
                node: {
                    While: {
                        condition: generateExpression(cond),
                        body: generateExpression(body)
                    }
                },
                ty: ty,
                span: posToSpan(pos)
            };

        case Block(exprs):
            {
                node: {
                    Block: {
                        statements: exprs.map(e -> generateStatement(e)),
                        span: posToSpan(pos)
                    }
                },
                ty: ty,
                span: posToSpan(pos)
            };

        case Return(_) | Let(_):
            // These should be handled as statements, not expressions
            {node: {Literal: {Integer: 0}}, ty: ty, span: posToSpan(pos)};
    }
}

#end
