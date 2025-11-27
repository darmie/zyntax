package zyntax;

#if (macro || zyntax_runtime)

import haxe.Json;
import haxe.macro.Expr.Position;
using Lambda;
using StringTools;

// Track runtime functions encountered during generation
// Maps function name -> {params: [...], return_type: ...}
private var runtimeFunctions: Map<String, {params: Array<AST.ZType>, returnType: AST.ZType}> = new Map();

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
    // Reset runtime functions tracking for each module
    runtimeFunctions = new Map();

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
                    calling_convention: func.is_external ? "Cdecl" : "Rust",
                    link_name: func.link_name
                }
            },
            ty: {Primitive: "Unit"}, // Declarations themselves have Unit type
            span: posToSpan(func.pos)
        };
        declarations.push(funcDecl);
    }

    // Emit external declarations for all runtime functions encountered
    for (funcName => funcInfo in runtimeFunctions) {
        var params = funcInfo.params.mapi((i, p) -> {
            name: "arg" + i,
            ty: generateType(p),
            mutability: "Immutable",
            kind: "Regular",
            default_value: null,
            attributes: [],
            span: {start: 0, end: 0}
        });

        var externDecl = {
            node: {
                Function: {
                    name: funcName,
                    params: params,
                    return_type: generateType(funcInfo.returnType),
                    body: null,  // External functions have no body
                    visibility: "Public",
                    is_async: false,
                    is_external: true,
                    calling_convention: "Cdecl",
                    link_name: funcName  // Use the $ prefixed name as link name
                }
            },
            ty: {Primitive: "Unit"},
            span: {start: 0, end: 0}
        };
        declarations.push(externDecl);
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
        case Generic(name, typeParams): {
            Generic: {
                name: name,
                type_params: typeParams.map(p -> generateType(p))
            }
        };
        case Pointer(inner): {
            Pointer: generateType(inner)
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

        case For(varName, varType, iterator, body):
            // For-in loop as statement
            {
                node: {
                    For: {
                        pattern: {
                            node: {
                                Identifier: {
                                    name: varName,
                                    mutability: "Mutable"
                                }
                            },
                            ty: generateType(varType),
                            span: posToSpan(pos)
                        },
                        iterator: generateExpression(iterator),
                        body: {
                            statements: [generateStatement({expr: body.expr, ty: body.ty, pos: body.pos})],
                            span: posToSpan(body.pos)
                        }
                    }
                },
                ty: stmtTy,
                span: posToSpan(pos)
            };

        case While(cond, body):
            // While loop as statement
            {
                node: {
                    While: {
                        condition: generateExpression(cond),
                        body: {
                            statements: [generateStatement({expr: body.expr, ty: body.ty, pos: body.pos})],
                            span: posToSpan(body.pos)
                        },
                        span: posToSpan(pos)
                    }
                },
                ty: stmtTy,
                span: posToSpan(pos)
            };

        case Break:
            {
                node: {
                    Break: null
                },
                ty: stmtTy,
                span: posToSpan(pos)
            };

        case Continue:
            {
                node: "Continue",
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
            // Track runtime functions (those starting with $)
            switch(callee.expr) {
                case Variable(name) if (StringTools.startsWith(name, "$")):
                    // Extract function type from callee
                    switch(callee.ty) {
                        case Function(paramTypes, returnType):
                            if (!runtimeFunctions.exists(name)) {
                                runtimeFunctions.set(name, {params: paramTypes, returnType: returnType});
                            }
                        default:
                    }
                default:
            }
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

        case For(varName, varType, iterator, body):
            // For-in loop: generates TypedFor with pattern and iterator
            {
                node: {
                    For: {
                        pattern: {
                            node: {
                                Identifier: {
                                    name: varName,
                                    mutability: "Mutable"
                                }
                            },
                            ty: generateType(varType),
                            span: posToSpan(pos)
                        },
                        iterator: generateExpression(iterator),
                        body: {
                            statements: [generateStatement({expr: body.expr, ty: body.ty, pos: body.pos})],
                            span: posToSpan(body.pos)
                        }
                    }
                },
                ty: ty,
                span: posToSpan(pos)
            };

        case Break:
            {
                node: {
                    Break: null
                },
                ty: ty,
                span: posToSpan(pos)
            };

        case Continue:
            {
                node: "Continue",
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
