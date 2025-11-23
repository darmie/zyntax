//! ZynPEG: Parser Generator for Multi-Language Zyntax Frontends
//!
//! ZynPEG extends pest PEG syntax with TypedAST action blocks, enabling
//! automatic generation of type-safe AST builders from grammar files.
//!
//! # Example .zyn Grammar
//!
//! ```zyn
//! @language {
//!     name: "Calculator",
//!     version: "1.0",
//! }
//!
//! @imports {
//!     use zyntax_typed_ast::*;
//! }
//!
//! @context {
//!     arena: &mut AstArena,
//! }
//!
//! expr = { number | binary_op }
//!
//! binary_op = { expr ~ "+" ~ expr }
//!   -> TypedExpression {
//!       expr: BinaryOp(Add, $1, $3),
//!       ty: infer_type($1.ty, Add, $3.ty),
//!       span: span($1, $3),
//!   }
//!
//! number = @{ ASCII_DIGIT+ }
//!   -> TypedExpression {
//!       expr: IntLiteral($1.parse()),
//!       ty: Type::I32,
//!       span: $1.span,
//!   }
//! ```

use pest_derive::Parser;

pub mod ast;
pub mod generator;
pub mod error;
pub mod runtime;

// Parser for .zyn grammar files
#[derive(Parser)]
#[grammar = "zyn_grammar.pest"]
pub struct ZynGrammarParser;

/// Language metadata from @language directive
#[derive(Debug, Clone, Default)]
pub struct LanguageInfo {
    pub name: String,
    pub version: String,
    pub file_extensions: Vec<String>,
    /// Entry point function name (declared by the grammar for --run flag)
    pub entry_point: Option<String>,
}

/// Context variable from @context directive
#[derive(Debug, Clone)]
pub struct ContextVar {
    pub name: String,
    pub ty: String,
}

/// Imports from @imports directive
#[derive(Debug, Clone, Default)]
pub struct Imports {
    pub code: String,
}

/// Type helper functions from @type_helpers directive
#[derive(Debug, Clone, Default)]
pub struct TypeHelpers {
    pub code: String,
}

/// A parsed .zyn grammar file
#[derive(Debug, Clone, Default)]
pub struct ZynGrammar {
    pub language: LanguageInfo,
    pub imports: Imports,
    pub context: Vec<ContextVar>,
    pub type_helpers: TypeHelpers,
    pub rules: Vec<RuleDef>,
}

/// A grammar rule with optional action block
#[derive(Debug, Clone)]
pub struct RuleDef {
    pub name: String,
    pub modifier: Option<RuleModifier>,
    pub pattern: String,
    pub action: Option<ActionBlock>,
}

/// Rule modifiers (@ for atomic, ! for silent, etc.)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuleModifier {
    Atomic,   // @
    Silent,   // _
    Compound, // $
    NonAtomic, // !
}

/// Action block for TypedAST generation
#[derive(Debug, Clone)]
pub struct ActionBlock {
    pub return_type: String,
    pub fields: Vec<ActionField>,
    /// Raw Rust code if not using structured fields (legacy)
    pub raw_code: Option<String>,
    /// JSON commands for zpeg runtime interpreter
    pub json_commands: Option<String>,
}

/// A field in an action block
#[derive(Debug, Clone)]
pub struct ActionField {
    pub name: String,
    pub value: String,
}

#[cfg(test)]
mod tests {
    use super::*;
    use pest::Parser;

    #[test]
    fn test_comment_in_string_literal() {
        // Test that "//" inside a string literal doesn't start a comment
        let input = r#"COMMENT = _{ "//" ~ ANY* }"#;
        let result = ZynGrammarParser::parse(Rule::rule_def, input);
        assert!(result.is_ok(), "Failed to parse rule with // in string: {:?}", result.err());
    }

    #[test]
    fn test_parse_simple_rule() {
        let input = r#"
            number = @{ ASCII_DIGIT+ }
        "#;

        let result = ZynGrammarParser::parse(Rule::rule_def, input.trim());
        assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
    }

    #[test]
    fn test_parse_rule_with_action() {
        let input = r#"
            number = @{ ASCII_DIGIT+ }
              -> TypedExpression {
                  expr: IntLiteral(parse_int($1)),
                  ty: Type::I32,
                  span: $1.span,
              }
        "#;

        let result = ZynGrammarParser::parse(Rule::rule_def, input.trim());
        assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
    }

    #[test]
    fn test_parse_imports_directive() {
        let input = r#"
            @imports {
                use zyntax_typed_ast::*;
                use zyntax_typed_ast::typed_ast::TypedExpression;
            }
        "#;

        let result = ZynGrammarParser::parse(Rule::imports_directive, input.trim());
        assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
    }

    #[test]
    fn test_parse_context_directive() {
        let input = r#"
            @context {
                arena: &mut AstArena,
                type_registry: &mut TypeRegistry,
            }
        "#;

        let result = ZynGrammarParser::parse(Rule::context_directive, input.trim());
        assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
    }

    #[test]
    fn test_parse_language_directive() {
        let input = r#"
            @language {
                name: "Zig",
                version: "0.11",
                file_extensions: [".zig", ".zyn"],
            }
        "#;

        let result = ZynGrammarParser::parse(Rule::language_directive, input.trim());
        assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
    }
}
