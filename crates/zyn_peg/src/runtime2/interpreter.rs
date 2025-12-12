//! Runtime Interpreter for ZynPEG 2.0
//!
//! Executes GrammarIR patterns directly without code generation.
//! This is useful for:
//! - Testing grammars
//! - Embedded DSL use cases
//! - Rapid prototyping
//!
//! For production use, the code generator (`codegen::ParserGenerator`) should
//! be used instead as it produces more efficient compiled code.

use crate::grammar::{GrammarIR, RuleIR, PatternIR, ActionIR, CharClass, RuleModifier};
use super::state::{ParserState, ParseResult, ParsedValue, ParseFailure};
use std::collections::HashMap;

/// Runtime interpreter for GrammarIR
pub struct GrammarInterpreter<'g> {
    grammar: &'g GrammarIR,
    /// Rule ID counter for memoization
    rule_id_map: HashMap<String, usize>,
}

impl<'g> GrammarInterpreter<'g> {
    /// Create a new interpreter for the given grammar
    pub fn new(grammar: &'g GrammarIR) -> Self {
        let mut rule_id_map = HashMap::new();
        for (i, name) in grammar.rules.keys().enumerate() {
            rule_id_map.insert(name.clone(), i);
        }
        GrammarInterpreter { grammar, rule_id_map }
    }

    /// Parse input using a specific rule
    pub fn parse_rule<'a>(
        &self,
        rule_name: &str,
        state: &mut ParserState<'a>,
    ) -> ParseResult<ParsedValue> {
        let rule = match self.grammar.get_rule(rule_name) {
            Some(r) => r,
            None => return state.fail(&format!("unknown rule: {}", rule_name)),
        };

        self.execute_rule(rule, state)
    }

    /// Parse input from the entry rule
    pub fn parse<'a>(&self, state: &mut ParserState<'a>) -> ParseResult<ParsedValue> {
        let entry = &self.grammar.entry_rule;
        self.parse_rule(entry, state)
    }

    /// Execute a rule
    fn execute_rule<'a>(
        &self,
        rule: &RuleIR,
        state: &mut ParserState<'a>,
    ) -> ParseResult<ParsedValue> {
        let start_pos = state.pos();

        // Note: We don't skip whitespace here at the start of a rule.
        // Whitespace skipping happens between sequence elements (see execute_pattern).
        // This preserves the position for SOI matching.

        // Execute pattern
        let result = self.execute_pattern(&rule.pattern, state, rule.modifier == Some(RuleModifier::Atomic));

        match result {
            ParseResult::Success(value, pos) => {
                // For atomic rules, capture the text
                if rule.modifier == Some(RuleModifier::Atomic) {
                    let text = state.slice(start_pos, pos).to_string();
                    ParseResult::Success(ParsedValue::Text(text), pos)
                } else {
                    ParseResult::Success(value, pos)
                }
            }
            ParseResult::Failure(e) => {
                state.set_pos(start_pos);
                ParseResult::Failure(e)
            }
        }
    }

    /// Execute a pattern
    fn execute_pattern<'a>(
        &self,
        pattern: &PatternIR,
        state: &mut ParserState<'a>,
        atomic: bool,
    ) -> ParseResult<ParsedValue> {
        match pattern {
            PatternIR::Literal(s) => {
                match state.match_literal(s) {
                    ParseResult::Success(_, pos) => ParseResult::Success(ParsedValue::None, pos),
                    ParseResult::Failure(e) => ParseResult::Failure(e),
                }
            }

            PatternIR::CharClass(class) => {
                self.execute_char_class(class, state)
            }

            PatternIR::RuleRef { rule_name, binding } => {
                // Check for built-in patterns first
                let result = match rule_name.as_str() {
                    "ASCII_DIGIT" => {
                        state.match_char(|c| c.is_ascii_digit(), "digit")
                            .map(|c| ParsedValue::Text(c.to_string()))
                    }
                    "ASCII_ALPHA" => {
                        state.match_char(|c| c.is_ascii_alphabetic(), "letter")
                            .map(|c| ParsedValue::Text(c.to_string()))
                    }
                    "ASCII_ALPHANUMERIC" => {
                        state.match_char(|c| c.is_ascii_alphanumeric(), "alphanumeric")
                            .map(|c| ParsedValue::Text(c.to_string()))
                    }
                    "ASCII_HEX_DIGIT" => {
                        state.match_char(|c| c.is_ascii_hexdigit(), "hex digit")
                            .map(|c| ParsedValue::Text(c.to_string()))
                    }
                    "ANY" => {
                        match state.peek_char() {
                            Some(c) => {
                                state.advance();
                                ParseResult::Success(ParsedValue::Text(c.to_string()), state.pos())
                            }
                            None => state.fail("any character"),
                        }
                    }
                    "SOI" => {
                        if state.pos() == 0 {
                            ParseResult::Success(ParsedValue::None, 0)
                        } else {
                            state.fail("start of input")
                        }
                    }
                    "EOI" => {
                        if state.is_eof() {
                            ParseResult::Success(ParsedValue::None, state.pos())
                        } else {
                            state.fail("end of input")
                        }
                    }
                    _ => {
                        // Look up rule in grammar
                        match self.grammar.get_rule(rule_name) {
                            Some(rule) => self.execute_rule(rule, state),
                            None => state.fail(&format!("unknown rule: {}", rule_name)),
                        }
                    }
                };

                // Store binding if specified
                if let Some(bind_name) = binding {
                    if let ParseResult::Success(ref value, _) = result {
                        state.set_binding(bind_name, value.clone());
                    }
                }

                result
            }

            PatternIR::Sequence(patterns) => {
                let start_pos = state.pos();
                let mut last_value = ParsedValue::None;

                for (i, p) in patterns.iter().enumerate() {
                    // Skip whitespace between elements (unless atomic)
                    if !atomic && i > 0 {
                        state.skip_ws();
                    }

                    match self.execute_pattern(p, state, atomic) {
                        ParseResult::Success(v, _) => {
                            last_value = v;
                        }
                        ParseResult::Failure(e) => {
                            state.set_pos(start_pos);
                            return ParseResult::Failure(e);
                        }
                    }
                }

                ParseResult::Success(last_value, state.pos())
            }

            PatternIR::Choice(choices) => {
                let start_pos = state.pos();
                let saved_bindings = state.save_bindings();
                let mut last_error: Option<ParseFailure> = None;

                for choice in choices {
                    state.set_pos(start_pos);
                    state.restore_bindings(saved_bindings.clone());

                    match self.execute_pattern(choice, state, atomic) {
                        ParseResult::Success(v, pos) => {
                            return ParseResult::Success(v, pos);
                        }
                        ParseResult::Failure(e) => {
                            last_error = Some(match last_error {
                                Some(prev) => prev.merge(e),
                                None => e,
                            });
                        }
                    }
                }

                state.set_pos(start_pos);
                state.restore_bindings(saved_bindings);
                ParseResult::Failure(last_error.unwrap_or_else(|| {
                    ParseFailure::new("choice", start_pos, state.line(), state.column())
                }))
            }

            PatternIR::Optional(inner) => {
                let start_pos = state.pos();
                let saved_bindings = state.save_bindings();

                match self.execute_pattern(inner, state, atomic) {
                    ParseResult::Success(v, pos) => {
                        ParseResult::Success(ParsedValue::Optional(Some(Box::new(v))), pos)
                    }
                    ParseResult::Failure(_) => {
                        state.set_pos(start_pos);
                        state.restore_bindings(saved_bindings);
                        ParseResult::Success(ParsedValue::Optional(None), start_pos)
                    }
                }
            }

            PatternIR::Repeat { pattern, min, max, separator } => {
                let mut items = Vec::new();

                loop {
                    // Skip whitespace before each item (including the first)
                    if !atomic {
                        state.skip_ws();
                    }

                    let item_start = state.pos();
                    let saved_bindings = state.save_bindings();

                    // Handle separator (after first item)
                    if !items.is_empty() {
                        if let Some(sep) = separator {
                            match self.execute_pattern(sep, state, atomic) {
                                ParseResult::Success(_, _) => {
                                    if !atomic {
                                        state.skip_ws();
                                    }
                                }
                                ParseResult::Failure(_) => {
                                    state.set_pos(item_start);
                                    state.restore_bindings(saved_bindings);
                                    break;
                                }
                            }
                        }
                    }

                    // Try to match item
                    match self.execute_pattern(pattern, state, atomic) {
                        ParseResult::Success(v, _) => {
                            items.push(v);
                        }
                        ParseResult::Failure(_) => {
                            state.set_pos(item_start);
                            state.restore_bindings(saved_bindings);
                            break;
                        }
                    }

                    // Check max
                    if let Some(max_count) = max {
                        if items.len() >= *max_count {
                            break;
                        }
                    }
                }

                // Check min
                if items.len() < *min {
                    return state.fail(&format!("expected at least {} items, got {}", min, items.len()));
                }

                ParseResult::Success(ParsedValue::List(items), state.pos())
            }

            PatternIR::PositiveLookahead(inner) => {
                let start_pos = state.pos();
                let saved_bindings = state.save_bindings();

                let result = self.execute_pattern(inner, state, atomic);

                state.set_pos(start_pos);
                state.restore_bindings(saved_bindings);

                match result {
                    ParseResult::Success(_, _) => ParseResult::Success(ParsedValue::None, start_pos),
                    ParseResult::Failure(e) => ParseResult::Failure(e),
                }
            }

            PatternIR::NegativeLookahead(inner) => {
                let start_pos = state.pos();
                let saved_bindings = state.save_bindings();

                let result = self.execute_pattern(inner, state, atomic);

                state.set_pos(start_pos);
                state.restore_bindings(saved_bindings);

                match result {
                    ParseResult::Success(_, _) => state.fail("negative lookahead matched"),
                    ParseResult::Failure(_) => ParseResult::Success(ParsedValue::None, start_pos),
                }
            }

            PatternIR::Any => {
                match state.peek_char() {
                    Some(c) => {
                        state.advance();
                        ParseResult::Success(ParsedValue::Text(c.to_string()), state.pos())
                    }
                    None => state.fail("any character"),
                }
            }

            PatternIR::StartOfInput => {
                if state.pos() == 0 {
                    ParseResult::Success(ParsedValue::None, 0)
                } else {
                    state.fail("start of input")
                }
            }

            PatternIR::EndOfInput => {
                if state.is_eof() {
                    ParseResult::Success(ParsedValue::None, state.pos())
                } else {
                    state.fail("end of input")
                }
            }

            PatternIR::Whitespace => {
                state.skip_ws();
                ParseResult::Success(ParsedValue::None, state.pos())
            }
        }
    }

    /// Execute a character class pattern
    fn execute_char_class<'a>(
        &self,
        class: &CharClass,
        state: &mut ParserState<'a>,
    ) -> ParseResult<ParsedValue> {
        match class {
            CharClass::Single(expected) => {
                match state.peek_char() {
                    Some(c) if c == *expected => {
                        state.advance();
                        ParseResult::Success(ParsedValue::Text(c.to_string()), state.pos())
                    }
                    _ => state.fail(&format!("'{}'", expected)),
                }
            }

            CharClass::Range(start, end) => {
                match state.peek_char() {
                    Some(c) if c >= *start && c <= *end => {
                        state.advance();
                        ParseResult::Success(ParsedValue::Text(c.to_string()), state.pos())
                    }
                    _ => state.fail(&format!("'{}'..'{}'", start, end)),
                }
            }

            CharClass::Builtin(name) => {
                let pred: Box<dyn Fn(char) -> bool> = match name.as_str() {
                    "ASCII_DIGIT" => Box::new(|c: char| c.is_ascii_digit()),
                    "ASCII_ALPHA" => Box::new(|c: char| c.is_ascii_alphabetic()),
                    "ASCII_ALPHANUMERIC" => Box::new(|c: char| c.is_ascii_alphanumeric()),
                    "ASCII_HEX_DIGIT" => Box::new(|c: char| c.is_ascii_hexdigit()),
                    "NEWLINE" => Box::new(|c: char| c == '\n' || c == '\r'),
                    _ => return state.fail(&format!("unknown char class: {}", name)),
                };

                match state.peek_char() {
                    Some(c) if pred(c) => {
                        state.advance();
                        ParseResult::Success(ParsedValue::Text(c.to_string()), state.pos())
                    }
                    _ => state.fail(name),
                }
            }

            CharClass::Union(classes) => {
                let start_pos = state.pos();

                for class in classes {
                    state.set_pos(start_pos);
                    match self.execute_char_class(class, state) {
                        ParseResult::Success(v, pos) => {
                            return ParseResult::Success(v, pos);
                        }
                        ParseResult::Failure(_) => continue,
                    }
                }

                state.set_pos(start_pos);
                state.fail("character class union")
            }

            CharClass::Negation(inner) => {
                let start_pos = state.pos();

                match self.execute_char_class(inner, state) {
                    ParseResult::Success(_, _) => {
                        state.set_pos(start_pos);
                        state.fail("negated character class matched")
                    }
                    ParseResult::Failure(_) => {
                        state.set_pos(start_pos);
                        // Match any character instead
                        match state.peek_char() {
                            Some(c) => {
                                state.advance();
                                ParseResult::Success(ParsedValue::Text(c.to_string()), state.pos())
                            }
                            None => state.fail("any character (negation)"),
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::grammar::parser::parse_grammar;
    use zyntax_typed_ast::TypedASTBuilder;
    use zyntax_typed_ast::type_registry::TypeRegistry;

    #[test]
    fn test_interpret_literal() {
        let grammar_src = r#"
            @language { name: "Test", version: "1.0" }
            hello = { "hello" }
        "#;
        let grammar = parse_grammar(grammar_src).unwrap();
        let interp = GrammarInterpreter::new(&grammar);

        let mut builder = TypedASTBuilder::new();
        let mut registry = TypeRegistry::new();
        let mut state = ParserState::new("hello world", &mut builder, &mut registry);

        let result = interp.parse_rule("hello", &mut state);
        assert!(result.is_success());
        assert_eq!(state.pos(), 5);
    }

    #[test]
    fn test_interpret_sequence() {
        let grammar_src = r#"
            @language { name: "Test", version: "1.0" }
            greeting = { "hello" ~ "world" }
        "#;
        let grammar = parse_grammar(grammar_src).unwrap();
        let interp = GrammarInterpreter::new(&grammar);

        let mut builder = TypedASTBuilder::new();
        let mut registry = TypeRegistry::new();
        let mut state = ParserState::new("hello world", &mut builder, &mut registry);

        let result = interp.parse_rule("greeting", &mut state);
        assert!(result.is_success());
    }

    #[test]
    fn test_interpret_choice() {
        let grammar_src = r#"
            @language { name: "Test", version: "1.0" }
            word = { "hello" | "world" }
        "#;
        let grammar = parse_grammar(grammar_src).unwrap();
        let interp = GrammarInterpreter::new(&grammar);

        let mut builder = TypedASTBuilder::new();
        let mut registry = TypeRegistry::new();

        let mut state1 = ParserState::new("hello", &mut builder, &mut registry);
        assert!(interp.parse_rule("word", &mut state1).is_success());

        let mut state2 = ParserState::new("world", &mut builder, &mut registry);
        assert!(interp.parse_rule("word", &mut state2).is_success());
    }

    #[test]
    fn test_interpret_repeat() {
        let grammar_src = r#"
            @language { name: "Test", version: "1.0" }
            digits = @{ ASCII_DIGIT+ }
        "#;
        let grammar = parse_grammar(grammar_src).unwrap();
        let interp = GrammarInterpreter::new(&grammar);

        let mut builder = TypedASTBuilder::new();
        let mut registry = TypeRegistry::new();
        let mut state = ParserState::new("12345abc", &mut builder, &mut registry);

        let result = interp.parse_rule("digits", &mut state);
        match result {
            ParseResult::Success(ParsedValue::Text(s), _) => {
                assert_eq!(s, "12345");
            }
            _ => panic!("Expected text result"),
        }
    }

    #[test]
    fn test_interpret_optional() {
        let grammar_src = r#"
            @language { name: "Test", version: "1.0" }
            maybe_hello = { "hello"? ~ "world" }
        "#;
        let grammar = parse_grammar(grammar_src).unwrap();
        let interp = GrammarInterpreter::new(&grammar);

        let mut builder = TypedASTBuilder::new();
        let mut registry = TypeRegistry::new();

        // With optional
        let mut state1 = ParserState::new("hello world", &mut builder, &mut registry);
        assert!(interp.parse_rule("maybe_hello", &mut state1).is_success());

        // Without optional
        let mut state2 = ParserState::new("world", &mut builder, &mut registry);
        assert!(interp.parse_rule("maybe_hello", &mut state2).is_success());
    }

    #[test]
    fn test_interpret_identifier() {
        let grammar_src = r#"
            @language { name: "Test", version: "1.0" }
            identifier = @{ ASCII_ALPHA ~ (ASCII_ALPHANUMERIC | "_")* }
        "#;
        let grammar = parse_grammar(grammar_src).unwrap();
        let interp = GrammarInterpreter::new(&grammar);

        let mut builder = TypedASTBuilder::new();
        let mut registry = TypeRegistry::new();
        let mut state = ParserState::new("my_var_123 rest", &mut builder, &mut registry);

        let result = interp.parse_rule("identifier", &mut state);
        match result {
            ParseResult::Success(ParsedValue::Text(s), _) => {
                assert_eq!(s, "my_var_123");
            }
            _ => panic!("Expected text result"),
        }
    }

    #[test]
    fn test_interpret_rule_reference() {
        let grammar_src = r#"
            @language { name: "Test", version: "1.0" }
            greeting = { "hello" ~ name }
            name = @{ ASCII_ALPHA+ }
        "#;
        let grammar = parse_grammar(grammar_src).unwrap();
        let interp = GrammarInterpreter::new(&grammar);

        let mut builder = TypedASTBuilder::new();
        let mut registry = TypeRegistry::new();
        let mut state = ParserState::new("hello world", &mut builder, &mut registry);

        let result = interp.parse_rule("greeting", &mut state);
        assert!(result.is_success());
    }
}
