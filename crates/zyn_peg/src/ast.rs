//! AST types for parsed .zyn grammar files

use pest::iterators::Pair;
use crate::{Rule, ZynGrammar, LanguageInfo, Imports, ContextVar, TypeHelpers, RuleDef, RuleModifier, ActionBlock, ActionField};

/// Build a ZynGrammar from parsed pest pairs
pub fn build_grammar(pairs: pest::iterators::Pairs<Rule>) -> Result<ZynGrammar, String> {
    let mut grammar = ZynGrammar::default();

    for pair in pairs {
        match pair.as_rule() {
            Rule::program => {
                for inner in pair.into_inner() {
                    process_top_level(&mut grammar, inner)?;
                }
            }
            Rule::directive => {
                process_directive(&mut grammar, pair)?;
            }
            Rule::rule_def => {
                grammar.rules.push(build_rule_def(pair)?);
            }
            Rule::EOI => {}
            _ => {
                process_top_level(&mut grammar, pair)?;
            }
        }
    }

    Ok(grammar)
}

fn process_top_level(grammar: &mut ZynGrammar, pair: Pair<Rule>) -> Result<(), String> {
    match pair.as_rule() {
        Rule::directive => process_directive(grammar, pair)?,
        Rule::rule_def => grammar.rules.push(build_rule_def(pair)?),
        Rule::language_directive => grammar.language = build_language_info(pair)?,
        Rule::imports_directive => grammar.imports = build_imports(pair)?,
        Rule::context_directive => grammar.context = build_context(pair)?,
        Rule::type_helpers_directive => grammar.type_helpers = build_type_helpers(pair)?,
        Rule::EOI => {}
        _ => {}
    }
    Ok(())
}

fn process_directive(grammar: &mut ZynGrammar, pair: Pair<Rule>) -> Result<(), String> {
    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::language_directive => grammar.language = build_language_info(inner)?,
            Rule::imports_directive => grammar.imports = build_imports(inner)?,
            Rule::context_directive => grammar.context = build_context(inner)?,
            Rule::type_helpers_directive => grammar.type_helpers = build_type_helpers(inner)?,
            Rule::error_messages_directive => {
                // TODO: Parse error messages
            }
            _ => {}
        }
    }
    Ok(())
}

fn build_language_info(pair: Pair<Rule>) -> Result<LanguageInfo, String> {
    let mut info = LanguageInfo::default();

    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::language_field {
            // Get the raw text of the field to determine type
            let field_text = inner.as_str();
            let field_content: Vec<_> = inner.into_inner().collect();

            if field_text.trim().starts_with("name") {
                // Find the string literal
                for item in &field_content {
                    if item.as_rule() == Rule::string_literal {
                        info.name = extract_string_value(item);
                        break;
                    }
                }
            } else if field_text.trim().starts_with("version") {
                for item in &field_content {
                    if item.as_rule() == Rule::string_literal {
                        info.version = extract_string_value(item);
                        break;
                    }
                }
            } else if field_text.trim().starts_with("file_extensions") {
                for item in &field_content {
                    if item.as_rule() == Rule::string_literal {
                        info.file_extensions.push(extract_string_value(item));
                    }
                }
            }
        }
    }

    Ok(info)
}

fn extract_string_value(pair: &Pair<Rule>) -> String {
    let s = pair.as_str();
    // Remove quotes
    if s.starts_with('"') && s.ends_with('"') {
        s[1..s.len()-1].to_string()
    } else {
        s.to_string()
    }
}

fn build_imports(pair: Pair<Rule>) -> Result<Imports, String> {
    let mut imports = Imports::default();

    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::rust_code {
            imports.code = inner.as_str().trim().to_string();
        }
    }

    Ok(imports)
}

fn build_context(pair: Pair<Rule>) -> Result<Vec<ContextVar>, String> {
    let mut context = Vec::new();

    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::context_field {
            let mut parts = inner.into_inner();
            if let (Some(name), Some(ty)) = (parts.next(), parts.next()) {
                context.push(ContextVar {
                    name: name.as_str().to_string(),
                    ty: ty.as_str().to_string(),
                });
            }
        }
    }

    Ok(context)
}

fn build_type_helpers(pair: Pair<Rule>) -> Result<TypeHelpers, String> {
    let mut helpers = TypeHelpers::default();

    for inner in pair.into_inner() {
        if inner.as_rule() == Rule::rust_code {
            helpers.code = inner.as_str().trim().to_string();
        }
    }

    Ok(helpers)
}

fn build_rule_def(pair: Pair<Rule>) -> Result<RuleDef, String> {
    let mut name = String::new();
    let mut modifier = None;
    let mut pattern = String::new();
    let mut action = None;

    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::identifier => {
                if name.is_empty() {
                    name = inner.as_str().to_string();
                }
            }
            Rule::rule_modifier => {
                modifier = Some(match inner.as_str() {
                    "@" => RuleModifier::Atomic,
                    "_" => RuleModifier::Silent,
                    "$" => RuleModifier::Compound,
                    "!" => RuleModifier::NonAtomic,
                    _ => RuleModifier::Atomic,
                });
            }
            Rule::pattern => {
                pattern = inner.as_str().to_string();
            }
            Rule::action_block => {
                action = Some(build_action_block(inner)?);
            }
            _ => {}
        }
    }

    Ok(RuleDef {
        name,
        modifier,
        pattern,
        action,
    })
}

fn build_action_block(pair: Pair<Rule>) -> Result<ActionBlock, String> {
    let mut return_type = String::new();
    let mut fields = Vec::new();
    let mut raw_code = None;

    for inner in pair.into_inner() {
        match inner.as_rule() {
            Rule::rust_type => {
                return_type = inner.as_str().trim().to_string();
            }
            Rule::action_body => {
                for field_pair in inner.into_inner() {
                    match field_pair.as_rule() {
                        Rule::action_field => {
                            fields.push(build_action_field(field_pair)?);
                        }
                        Rule::action_code => {
                            raw_code = Some(field_pair.as_str().trim().to_string());
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }

    Ok(ActionBlock {
        return_type,
        fields,
        raw_code,
    })
}

fn build_action_field(pair: Pair<Rule>) -> Result<ActionField, String> {
    let mut parts = pair.into_inner();
    let name = parts.next()
        .ok_or("Missing field name")?
        .as_str()
        .to_string();
    let value = parts.next()
        .ok_or("Missing field value")?
        .as_str()
        .to_string();

    Ok(ActionField { name, value })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ZynGrammarParser;
    use pest::Parser;

    #[test]
    fn test_build_language_info() {
        let input = r#"@language {
            name: "Calculator",
            version: "1.0",
        }"#;

        let pairs = ZynGrammarParser::parse(Rule::language_directive, input).unwrap();
        let info = build_language_info(pairs.into_iter().next().unwrap()).unwrap();

        assert_eq!(info.name, "Calculator");
        assert_eq!(info.version, "1.0");
    }

    #[test]
    fn test_build_rule_def() {
        let input = r#"number = @{ ASCII_DIGIT+ }"#;

        let pairs = ZynGrammarParser::parse(Rule::rule_def, input).unwrap();
        let rule = build_rule_def(pairs.into_iter().next().unwrap()).unwrap();

        assert_eq!(rule.name, "number");
        assert_eq!(rule.modifier, Some(RuleModifier::Atomic));
        assert!(rule.pattern.contains("ASCII_DIGIT"));
    }

    #[test]
    fn test_build_rule_with_action() {
        let input = r#"number = @{ ASCII_DIGIT+ }
          -> TypedExpression {
              expr: IntLiteral(parse_int($1)),
              ty: Type::I32,
          }"#;

        let pairs = ZynGrammarParser::parse(Rule::rule_def, input).unwrap();
        let rule = build_rule_def(pairs.into_iter().next().unwrap()).unwrap();

        assert_eq!(rule.name, "number");
        assert!(rule.action.is_some());

        let action = rule.action.unwrap();
        assert_eq!(action.return_type, "TypedExpression");
        // Grammar now parses action body as raw_code (more general) rather than structured fields
        assert!(action.raw_code.is_some());
        let raw = action.raw_code.unwrap();
        assert!(raw.contains("expr:"));
        assert!(raw.contains("ty:"));
    }

    #[test]
    fn test_nested_braces_in_action() {
        let input = r#"test = { "test" }
  -> TestType {
      decl: ConstDecl {
          name: intern($2),
          ty: Type::I32,
      },
      visibility: Visibility::Private,
  }"#;

        let result = ZynGrammarParser::parse(Rule::rule_def, input);
        match result {
            Ok(pairs) => {
                let rule = build_rule_def(pairs.into_iter().next().unwrap()).unwrap();
                assert_eq!(rule.name, "test");
                assert!(rule.action.is_some());
                let action = rule.action.unwrap();
                assert_eq!(action.return_type, "TestType");
                // Grammar now parses action body as raw_code (more general) rather than structured fields
                assert!(action.raw_code.is_some());
                let raw = action.raw_code.unwrap();
                assert!(raw.contains("decl:"));
                assert!(raw.contains("visibility:"));
                assert!(raw.contains("ConstDecl"));
            }
            Err(e) => {
                panic!("Failed to parse: {}", e);
            }
        }
    }
}
