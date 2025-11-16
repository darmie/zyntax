// ZynParser - Phase 1 POC: Calculator Parser
//
// Demonstrates feasibility of using pest to generate Zyntax TypedAST.
// Future phases will extend this with .zyn grammar format and action blocks.

use pest_derive::Parser;

pub mod typed_ast_builder;

#[derive(Parser)]
#[grammar = "calculator.pest"]
pub struct CalculatorParser;

pub use typed_ast_builder::TypedAstBuilder;

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
