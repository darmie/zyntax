// ZynParser - PEG-based Parser Generator for Zyntax
//
// Phase 1: Calculator POC (✅ Complete)
// Phase 2: Zig Subset (🚧 In Progress - Week 1)

use pest_derive::Parser;

// Phase 1: Calculator (Reference implementation)
pub mod typed_ast_builder;

// Phase 2: Zig Subset
pub mod zig_builder;

#[derive(Parser)]
#[grammar = "calculator.pest"]
pub struct CalculatorParser;

pub use typed_ast_builder::TypedAstBuilder;
pub use zig_builder::{ZigParser, ZigBuilder, zig_parser};

#[cfg(test)]
mod tests {
    use super::*;
    use pest::Parser;

    #[test]
    fn test_parse_number() {
        let result = CalculatorParser::parse(Rule::number, "42");
        assert!(result.is_ok());

        let result = CalculatorParser::parse(Rule::number, "3.14");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_binary_op() {
        let result = CalculatorParser::parse(Rule::program, "2 + 3");
        assert!(result.is_ok());

        let result = CalculatorParser::parse(Rule::program, "10 * 5");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_complex_expr() {
        let result = CalculatorParser::parse(Rule::program, "(2 + 3) * 4");
        assert!(result.is_ok());

        let result = CalculatorParser::parse(Rule::program, "1 + 2 * 3 - 4");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_unary() {
        let result = CalculatorParser::parse(Rule::program, "-42");
        assert!(result.is_ok());

        let result = CalculatorParser::parse(Rule::program, "-(5 + 3)");
        assert!(result.is_ok());
    }
}
