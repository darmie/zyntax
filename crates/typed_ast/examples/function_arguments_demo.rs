//! # Function Arguments Support Demonstration
//!
//! This example showcases the comprehensive function argument support in the Zyntax TypedAST,
//! demonstrating how it handles various parameter passing conventions from different languages.

use zyntax_typed_ast::{
    typed_ast::*,
    Type, TypeId, PrimitiveType, Mutability, Visibility, ParamInfo,
    NullabilityKind, AsyncKind, CallingConvention,
    AstArena, Span,
};

/// Demonstrates basic positional arguments (C, Go, Java style)
fn positional_arguments_demo() {
    let mut arena = AstArena::new();
    
    println!("🔥 Basic Positional Arguments:\n");
    
    // function add(x: int, y: int) -> int
    let add_function = TypedFunction {
        name: arena.intern_string("add"),
        params: vec![
            TypedParameter::regular(
                arena.intern_string("x"),
                Type::Primitive(PrimitiveType::I32),
                Mutability::Immutable,
                Span::new(10, 16)
            ),
            TypedParameter::regular(
                arena.intern_string("y"),
                Type::Primitive(PrimitiveType::I32),
                Mutability::Immutable,
                Span::new(18, 24)
            ),
        ],
        return_type: Type::Primitive(PrimitiveType::I32),
        body: TypedBlock {
            statements: vec![],
            span: Span::new(35, 50),
        },
        visibility: Visibility::Public,
        is_async: false,
    };
    
    // Function call: add(10, 20)
    let add_call = TypedCall::positional(
        typed_node(
            TypedExpression::Variable(arena.intern_string("add")),
            Type::Function {
                params: vec![
                    ParamInfo {
                        name: Some(arena.intern_string("x")),
                        ty: Type::Primitive(PrimitiveType::I32),
                        is_optional: false,
                        is_varargs: false,
                        is_keyword_only: false,
                        is_positional_only: false,
                        is_out: false,
                        is_ref: false,
                        is_inout: false,
                    },
                    ParamInfo {
                        name: Some(arena.intern_string("y")),
                        ty: Type::Primitive(PrimitiveType::I32),
                        is_optional: false,
                        is_varargs: false,
                        is_keyword_only: false,
                        is_positional_only: false,
                        is_out: false,
                        is_ref: false,
                        is_inout: false,
                    },
                ],
                return_type: Box::new(Type::Primitive(PrimitiveType::I32)),
                is_varargs: false,
                has_named_params: false,
                has_default_params: false,
                async_kind: AsyncKind::Sync,
                calling_convention: CallingConvention::Default,
                nullability: NullabilityKind::NonNull,
            },
            Span::new(0, 3)
        ),
        vec![
            typed_node(
                TypedExpression::Literal(TypedLiteral::Integer(10)),
                Type::Primitive(PrimitiveType::I32),
                Span::new(4, 6)
            ),
            typed_node(
                TypedExpression::Literal(TypedLiteral::Integer(20)),
                Type::Primitive(PrimitiveType::I32),
                Span::new(8, 10)
            ),
        ]
    );
    
    println!("✅ C/Go/Java style: add(10, 20)");
    assert_eq!(add_call.positional_args.len(), 2);
    assert_eq!(add_call.named_args.len(), 0);
}

fn main() {
    println!("🚀 Zyntax TypedAST Function Arguments Support Demo\n");
    
    positional_arguments_demo();
    
    println!("\n✨ Basic function arguments are working!");
}