//! Comprehensive ImagePipe Grammar Tests
//!
//! Tests the ZynPEG 2.0 parser against the ImagePipe DSL grammar.
//! ImagePipe was chosen for its detailed grammar with:
//! - Multiple statement types (load, save, resize, blur, etc.)
//! - Terminal rules (identifier, integer, string_literal)
//! - Legacy JSON action syntax
//! - Builtin function mappings
//!
//! These tests validate:
//! 1. Grammar parsing into GrammarIR
//! 2. Pattern structure validation for each rule
//! 3. Using GrammarInterpreter to parse actual ImagePipe source files

use zyn_peg::grammar::{
    parse_grammar, PatternIR, ActionIR, RuleModifier, CharClass,
};
use zyn_peg::runtime2::{ParserState, ParseResult, ParsedValue, GrammarInterpreter};
use zyntax_typed_ast::TypedASTBuilder;
use zyntax_typed_ast::type_registry::TypeRegistry;

const IMAGEPIPE_GRAMMAR: &str = include_str!("../../../examples/imagepipe/imagepipe.zyn");
const VINTAGE_IMGPIPE: &str = include_str!("../../../examples/imagepipe/samples/vintage.imgpipe");

// =============================================================================
// Grammar Parsing Tests
// =============================================================================

#[test]
fn test_imagepipe_grammar_parses_successfully() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR)
        .expect("ImagePipe grammar should parse successfully");

    // Validate metadata
    assert_eq!(grammar.metadata.name, "ImagePipe", "Language name should be ImagePipe");
    assert_eq!(grammar.metadata.version, "1.0", "Version should be 1.0");
    assert_eq!(
        grammar.metadata.file_extensions,
        vec![".imgpipe", ".ip"],
        "File extensions should include .imgpipe and .ip"
    );
    assert_eq!(
        grammar.metadata.entry_point,
        Some("run_pipeline".to_string()),
        "Entry point should be run_pipeline"
    );
}

#[test]
fn test_imagepipe_builtin_mappings() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();

    // Test image I/O builtins
    assert_eq!(
        grammar.builtins.functions.get("image_load"),
        Some(&"$Image$load".to_string()),
        "image_load should map to $Image$load"
    );
    assert_eq!(
        grammar.builtins.functions.get("image_save"),
        Some(&"$Image$save".to_string()),
        "image_save should map to $Image$save"
    );

    // Test transformation builtins
    assert_eq!(
        grammar.builtins.functions.get("image_resize"),
        Some(&"$Image$resize".to_string()),
        "image_resize should map to $Image$resize"
    );
    assert_eq!(
        grammar.builtins.functions.get("image_crop"),
        Some(&"$Image$crop".to_string()),
        "image_crop should map to $Image$crop"
    );
    assert_eq!(
        grammar.builtins.functions.get("image_rotate90"),
        Some(&"$Image$rotate90".to_string()),
        "image_rotate90 should map to $Image$rotate90"
    );
    assert_eq!(
        grammar.builtins.functions.get("image_flip_h"),
        Some(&"$Image$flip_horizontal".to_string()),
        "image_flip_h should map to $Image$flip_horizontal"
    );

    // Test filter builtins
    assert_eq!(
        grammar.builtins.functions.get("image_blur"),
        Some(&"$Image$blur".to_string()),
        "image_blur should map to $Image$blur"
    );
    assert_eq!(
        grammar.builtins.functions.get("image_brighten"),
        Some(&"$Image$brighten".to_string()),
        "image_brighten should map to $Image$brighten"
    );
    assert_eq!(
        grammar.builtins.functions.get("image_contrast"),
        Some(&"$Image$contrast".to_string()),
        "image_contrast should map to $Image$contrast"
    );
    assert_eq!(
        grammar.builtins.functions.get("image_grayscale"),
        Some(&"$Image$grayscale".to_string()),
        "image_grayscale should map to $Image$grayscale"
    );
    assert_eq!(
        grammar.builtins.functions.get("image_invert"),
        Some(&"$Image$invert".to_string()),
        "image_invert should map to $Image$invert"
    );

    // Test I/O builtin
    assert_eq!(
        grammar.builtins.functions.get("println"),
        Some(&"$IO$println".to_string()),
        "println should map to $IO$println"
    );

    // Verify total count (14 image operations + println = 15)
    assert_eq!(
        grammar.builtins.functions.len(),
        15,
        "Should have 15 builtin function mappings"
    );
}

#[test]
fn test_imagepipe_rule_count() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();

    // ImagePipe has 23 rules
    let expected_rules = vec![
        "program", "statements", "statement",
        "load_stmt", "save_stmt", "print_stmt",
        "resize_stmt", "crop_stmt", "rotate_stmt", "flip_stmt",
        "blur_stmt", "brightness_stmt", "contrast_stmt", "grayscale_stmt", "invert_stmt",
        "string_literal", "string_inner", "integer", "signed_integer", "number", "identifier",
        "WHITESPACE", "COMMENT"
    ];

    for rule_name in &expected_rules {
        assert!(
            grammar.rules.contains_key(*rule_name),
            "Grammar should contain rule '{}'",
            rule_name
        );
    }

    assert_eq!(
        grammar.rules.len(),
        expected_rules.len(),
        "Grammar should have exactly {} rules",
        expected_rules.len()
    );
}

// =============================================================================
// Program Structure Rules
// =============================================================================

#[test]
fn test_program_rule_structure() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let rule = grammar.get_rule("program").expect("program rule should exist");

    // program = { SOI ~ statements ~ EOI }
    assert!(rule.modifier.is_none(), "program should have no modifier");

    match &rule.pattern {
        PatternIR::Sequence(items) => {
            assert_eq!(items.len(), 3, "program pattern should have 3 elements");
            assert!(matches!(items[0], PatternIR::StartOfInput), "First element should be SOI");
            assert!(matches!(items[1], PatternIR::RuleRef { ref rule_name, .. } if rule_name == "statements"),
                "Second element should be statements rule reference");
            assert!(matches!(items[2], PatternIR::EndOfInput), "Third element should be EOI");
        }
        _ => panic!("program pattern should be a Sequence"),
    }

    // Check action is LegacyJson
    match &rule.action {
        Some(ActionIR::LegacyJson { return_type, json_content }) => {
            assert_eq!(return_type, "TypedProgram");
            assert!(json_content.contains("get_child"), "JSON should contain get_child");
        }
        _ => panic!("program action should be LegacyJson"),
    }
}

#[test]
fn test_statements_rule_structure() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let rule = grammar.get_rule("statements").expect("statements rule should exist");

    // statements = { statement* }
    match &rule.pattern {
        PatternIR::Repeat { pattern, min, max, separator } => {
            assert_eq!(*min, 0, "statements should match zero or more");
            assert!(max.is_none(), "statements should have no max");
            assert!(separator.is_none(), "statements should have no separator");
            match pattern.as_ref() {
                PatternIR::RuleRef { rule_name, .. } => {
                    assert_eq!(rule_name, "statement", "Should repeat statement rule");
                }
                _ => panic!("statements should repeat statement rule reference"),
            }
        }
        _ => panic!("statements pattern should be Repeat"),
    }

    // Action creates program with run_pipeline function
    match &rule.action {
        Some(ActionIR::LegacyJson { return_type, json_content }) => {
            assert_eq!(return_type, "TypedProgram");
            assert!(json_content.contains("get_all_children"), "Should get all children");
            assert!(json_content.contains("run_pipeline"), "Should define run_pipeline function");
            assert!(json_content.contains("\"define\": \"function\""), "Should define a function");
        }
        _ => panic!("statements action should be LegacyJson"),
    }
}

#[test]
fn test_statement_rule_is_choice() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let rule = grammar.get_rule("statement").expect("statement rule should exist");

    // statement = { load_stmt | save_stmt | resize_stmt | ... }
    match &rule.pattern {
        PatternIR::Choice(alternatives) => {
            let expected_stmts = vec![
                "load_stmt", "save_stmt", "resize_stmt", "crop_stmt",
                "rotate_stmt", "flip_stmt", "blur_stmt", "brightness_stmt",
                "contrast_stmt", "grayscale_stmt", "invert_stmt", "print_stmt"
            ];

            assert_eq!(
                alternatives.len(),
                expected_stmts.len(),
                "statement should have {} alternatives",
                expected_stmts.len()
            );

            for (i, alt) in alternatives.iter().enumerate() {
                match alt {
                    PatternIR::RuleRef { rule_name, .. } => {
                        assert_eq!(
                            rule_name, expected_stmts[i],
                            "Alternative {} should be {}",
                            i, expected_stmts[i]
                        );
                    }
                    _ => panic!("All statement alternatives should be rule references"),
                }
            }
        }
        _ => panic!("statement pattern should be Choice"),
    }
}

// =============================================================================
// I/O Statement Rules
// =============================================================================

#[test]
fn test_load_stmt_rule_structure() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let rule = grammar.get_rule("load_stmt").expect("load_stmt rule should exist");

    // load_stmt = { "load" ~ string_literal ~ "as" ~ identifier }
    match &rule.pattern {
        PatternIR::Sequence(items) => {
            assert_eq!(items.len(), 4, "load_stmt should have 4 elements");

            // "load"
            match &items[0] {
                PatternIR::Literal(s) => assert_eq!(s, "load"),
                _ => panic!("First element should be literal 'load'"),
            }

            // string_literal
            match &items[1] {
                PatternIR::RuleRef { rule_name, .. } => {
                    assert_eq!(rule_name, "string_literal");
                }
                _ => panic!("Second element should be string_literal"),
            }

            // "as"
            match &items[2] {
                PatternIR::Literal(s) => assert_eq!(s, "as"),
                _ => panic!("Third element should be literal 'as'"),
            }

            // identifier
            match &items[3] {
                PatternIR::RuleRef { rule_name, .. } => {
                    assert_eq!(rule_name, "identifier");
                }
                _ => panic!("Fourth element should be identifier"),
            }
        }
        _ => panic!("load_stmt pattern should be Sequence"),
    }

    // Action creates let statement with image_load call
    match &rule.action {
        Some(ActionIR::LegacyJson { json_content, .. }) => {
            assert!(json_content.contains("let_stmt"), "Should define let_stmt");
            assert!(json_content.contains("image_load"), "Should call image_load");
            assert!(json_content.contains("\"$2\""), "Should use $2 for variable name");
            assert!(json_content.contains("\"$1\""), "Should use $1 for file path");
        }
        _ => panic!("load_stmt action should be LegacyJson"),
    }
}

#[test]
fn test_save_stmt_rule_structure() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let rule = grammar.get_rule("save_stmt").expect("save_stmt rule should exist");

    // save_stmt = { "save" ~ identifier ~ "as" ~ string_literal }
    match &rule.pattern {
        PatternIR::Sequence(items) => {
            assert_eq!(items.len(), 4, "save_stmt should have 4 elements");
            match &items[0] {
                PatternIR::Literal(s) => assert_eq!(s, "save"),
                _ => panic!("First element should be literal 'save'"),
            }
        }
        _ => panic!("save_stmt pattern should be Sequence"),
    }

    // Action creates expression statement with image_save call
    match &rule.action {
        Some(ActionIR::LegacyJson { json_content, .. }) => {
            assert!(json_content.contains("expression_stmt"), "Should define expression_stmt");
            assert!(json_content.contains("image_save"), "Should call image_save");
        }
        _ => panic!("save_stmt action should be LegacyJson"),
    }
}

#[test]
fn test_print_stmt_rule_structure() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let rule = grammar.get_rule("print_stmt").expect("print_stmt rule should exist");

    // print_stmt = { "print" ~ string_literal }
    match &rule.pattern {
        PatternIR::Sequence(items) => {
            assert_eq!(items.len(), 2, "print_stmt should have 2 elements");
            match &items[0] {
                PatternIR::Literal(s) => assert_eq!(s, "print"),
                _ => panic!("First element should be literal 'print'"),
            }
            match &items[1] {
                PatternIR::RuleRef { rule_name, .. } => {
                    assert_eq!(rule_name, "string_literal");
                }
                _ => panic!("Second element should be string_literal"),
            }
        }
        _ => panic!("print_stmt pattern should be Sequence"),
    }

    // Action creates expression statement with println call
    match &rule.action {
        Some(ActionIR::LegacyJson { json_content, .. }) => {
            assert!(json_content.contains("println"), "Should call println");
        }
        _ => panic!("print_stmt action should be LegacyJson"),
    }
}

// =============================================================================
// Transformation Statement Rules
// =============================================================================

#[test]
fn test_resize_stmt_rule_structure() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let rule = grammar.get_rule("resize_stmt").expect("resize_stmt rule should exist");

    // resize_stmt = { "resize" ~ identifier ~ "to" ~ integer ~ "x" ~ integer }
    match &rule.pattern {
        PatternIR::Sequence(items) => {
            assert_eq!(items.len(), 6, "resize_stmt should have 6 elements");

            match &items[0] {
                PatternIR::Literal(s) => assert_eq!(s, "resize"),
                _ => panic!("Element 0 should be 'resize'"),
            }
            match &items[1] {
                PatternIR::RuleRef { rule_name, .. } => assert_eq!(rule_name, "identifier"),
                _ => panic!("Element 1 should be identifier"),
            }
            match &items[2] {
                PatternIR::Literal(s) => assert_eq!(s, "to"),
                _ => panic!("Element 2 should be 'to'"),
            }
            match &items[3] {
                PatternIR::RuleRef { rule_name, .. } => assert_eq!(rule_name, "integer"),
                _ => panic!("Element 3 should be integer"),
            }
            match &items[4] {
                PatternIR::Literal(s) => assert_eq!(s, "x"),
                _ => panic!("Element 4 should be 'x'"),
            }
            match &items[5] {
                PatternIR::RuleRef { rule_name, .. } => assert_eq!(rule_name, "integer"),
                _ => panic!("Element 5 should be integer"),
            }
        }
        _ => panic!("resize_stmt pattern should be Sequence"),
    }

    // Action creates assignment with image_resize call
    match &rule.action {
        Some(ActionIR::LegacyJson { json_content, .. }) => {
            assert!(json_content.contains("assignment"), "Should define assignment");
            assert!(json_content.contains("image_resize"), "Should call image_resize");
            assert!(json_content.contains("\"$1\""), "Should use $1 for image name");
            assert!(json_content.contains("\"$2\""), "Should use $2 for width");
            assert!(json_content.contains("\"$3\""), "Should use $3 for height");
        }
        _ => panic!("resize_stmt action should be LegacyJson"),
    }
}

#[test]
fn test_crop_stmt_rule_structure() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let rule = grammar.get_rule("crop_stmt").expect("crop_stmt rule should exist");

    // crop_stmt = { "crop" ~ identifier ~ "from" ~ integer ~ "," ~ integer ~ "to" ~ integer ~ "," ~ integer }
    match &rule.pattern {
        PatternIR::Sequence(items) => {
            assert_eq!(items.len(), 10, "crop_stmt should have 10 elements");

            // Verify key elements
            match &items[0] {
                PatternIR::Literal(s) => assert_eq!(s, "crop"),
                _ => panic!("Element 0 should be 'crop'"),
            }
            match &items[2] {
                PatternIR::Literal(s) => assert_eq!(s, "from"),
                _ => panic!("Element 2 should be 'from'"),
            }
            match &items[4] {
                PatternIR::Literal(s) => assert_eq!(s, ","),
                _ => panic!("Element 4 should be ','"),
            }
            match &items[6] {
                PatternIR::Literal(s) => assert_eq!(s, "to"),
                _ => panic!("Element 6 should be 'to'"),
            }
        }
        _ => panic!("crop_stmt pattern should be Sequence"),
    }

    // Action uses $1-$5 for all coordinates
    match &rule.action {
        Some(ActionIR::LegacyJson { json_content, .. }) => {
            assert!(json_content.contains("image_crop"), "Should call image_crop");
            assert!(json_content.contains("\"$4\""), "Should use $4 for end_x");
            assert!(json_content.contains("\"$5\""), "Should use $5 for end_y");
        }
        _ => panic!("crop_stmt action should be LegacyJson"),
    }
}

#[test]
fn test_rotate_stmt_has_choice_for_angles() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let rule = grammar.get_rule("rotate_stmt").expect("rotate_stmt rule should exist");

    // rotate_stmt = { "rotate" ~ identifier ~ ("90" | "180" | "270") }
    match &rule.pattern {
        PatternIR::Sequence(items) => {
            assert_eq!(items.len(), 3, "rotate_stmt should have 3 elements");

            // Third element should be a choice of angles
            match &items[2] {
                PatternIR::Choice(angles) => {
                    assert_eq!(angles.len(), 3, "Should have 3 angle choices");
                    let angle_values: Vec<&str> = angles.iter().filter_map(|p| {
                        if let PatternIR::Literal(s) = p {
                            Some(s.as_str())
                        } else {
                            None
                        }
                    }).collect();
                    assert_eq!(angle_values, vec!["90", "180", "270"]);
                }
                _ => panic!("Third element should be Choice of angles"),
            }
        }
        _ => panic!("rotate_stmt pattern should be Sequence"),
    }
}

#[test]
fn test_flip_stmt_has_direction_choice() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let rule = grammar.get_rule("flip_stmt").expect("flip_stmt rule should exist");

    // flip_stmt = { "flip" ~ identifier ~ ("horizontal" | "vertical") }
    match &rule.pattern {
        PatternIR::Sequence(items) => {
            assert_eq!(items.len(), 3, "flip_stmt should have 3 elements");

            match &items[2] {
                PatternIR::Choice(directions) => {
                    assert_eq!(directions.len(), 2, "Should have 2 direction choices");
                    let dir_values: Vec<&str> = directions.iter().filter_map(|p| {
                        if let PatternIR::Literal(s) = p {
                            Some(s.as_str())
                        } else {
                            None
                        }
                    }).collect();
                    assert_eq!(dir_values, vec!["horizontal", "vertical"]);
                }
                _ => panic!("Third element should be Choice of directions"),
            }
        }
        _ => panic!("flip_stmt pattern should be Sequence"),
    }
}

// =============================================================================
// Filter Statement Rules
// =============================================================================

#[test]
fn test_blur_stmt_uses_number() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let rule = grammar.get_rule("blur_stmt").expect("blur_stmt rule should exist");

    // blur_stmt = { "blur" ~ identifier ~ "by" ~ number }
    match &rule.pattern {
        PatternIR::Sequence(items) => {
            assert_eq!(items.len(), 4, "blur_stmt should have 4 elements");

            match &items[0] {
                PatternIR::Literal(s) => assert_eq!(s, "blur"),
                _ => panic!("Element 0 should be 'blur'"),
            }
            match &items[2] {
                PatternIR::Literal(s) => assert_eq!(s, "by"),
                _ => panic!("Element 2 should be 'by'"),
            }
            match &items[3] {
                PatternIR::RuleRef { rule_name, .. } => {
                    assert_eq!(rule_name, "number", "Should use 'number' for sigma");
                }
                _ => panic!("Element 3 should be number"),
            }
        }
        _ => panic!("blur_stmt pattern should be Sequence"),
    }

    match &rule.action {
        Some(ActionIR::LegacyJson { json_content, .. }) => {
            assert!(json_content.contains("image_blur"), "Should call image_blur");
        }
        _ => panic!("blur_stmt action should be LegacyJson"),
    }
}

#[test]
fn test_brightness_stmt_uses_signed_integer() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let rule = grammar.get_rule("brightness_stmt").expect("brightness_stmt rule should exist");

    // brightness_stmt = { "brighten" ~ identifier ~ "by" ~ signed_integer }
    match &rule.pattern {
        PatternIR::Sequence(items) => {
            assert_eq!(items.len(), 4, "brightness_stmt should have 4 elements");

            match &items[0] {
                PatternIR::Literal(s) => assert_eq!(s, "brighten"),
                _ => panic!("Element 0 should be 'brighten'"),
            }
            match &items[3] {
                PatternIR::RuleRef { rule_name, .. } => {
                    assert_eq!(rule_name, "signed_integer", "Should use 'signed_integer' for amount");
                }
                _ => panic!("Element 3 should be signed_integer"),
            }
        }
        _ => panic!("brightness_stmt pattern should be Sequence"),
    }
}

#[test]
fn test_contrast_stmt_uses_number() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let rule = grammar.get_rule("contrast_stmt").expect("contrast_stmt rule should exist");

    // contrast_stmt = { "contrast" ~ identifier ~ "by" ~ number }
    match &rule.pattern {
        PatternIR::Sequence(items) => {
            assert_eq!(items.len(), 4);
            match &items[3] {
                PatternIR::RuleRef { rule_name, .. } => {
                    assert_eq!(rule_name, "number", "Should use 'number' for factor");
                }
                _ => panic!("Element 3 should be number"),
            }
        }
        _ => panic!("contrast_stmt pattern should be Sequence"),
    }
}

#[test]
fn test_grayscale_stmt_unary() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let rule = grammar.get_rule("grayscale_stmt").expect("grayscale_stmt rule should exist");

    // grayscale_stmt = { "grayscale" ~ identifier }
    match &rule.pattern {
        PatternIR::Sequence(items) => {
            assert_eq!(items.len(), 2, "grayscale_stmt should have 2 elements");
            match &items[0] {
                PatternIR::Literal(s) => assert_eq!(s, "grayscale"),
                _ => panic!("Element 0 should be 'grayscale'"),
            }
            match &items[1] {
                PatternIR::RuleRef { rule_name, .. } => {
                    assert_eq!(rule_name, "identifier");
                }
                _ => panic!("Element 1 should be identifier"),
            }
        }
        _ => panic!("grayscale_stmt pattern should be Sequence"),
    }

    match &rule.action {
        Some(ActionIR::LegacyJson { json_content, .. }) => {
            assert!(json_content.contains("image_grayscale"), "Should call image_grayscale");
            // Only uses $1 (identifier), no extra arguments
            assert!(!json_content.contains("\"$2\""), "Should not have $2 argument");
        }
        _ => panic!("grayscale_stmt action should be LegacyJson"),
    }
}

#[test]
fn test_invert_stmt_unary() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let rule = grammar.get_rule("invert_stmt").expect("invert_stmt rule should exist");

    // invert_stmt = { "invert" ~ identifier }
    match &rule.pattern {
        PatternIR::Sequence(items) => {
            assert_eq!(items.len(), 2, "invert_stmt should have 2 elements");
        }
        _ => panic!("invert_stmt pattern should be Sequence"),
    }

    match &rule.action {
        Some(ActionIR::LegacyJson { json_content, .. }) => {
            assert!(json_content.contains("image_invert"), "Should call image_invert");
        }
        _ => panic!("invert_stmt action should be LegacyJson"),
    }
}

// =============================================================================
// Terminal Rules
// =============================================================================

#[test]
fn test_string_literal_is_atomic() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let rule = grammar.get_rule("string_literal").expect("string_literal rule should exist");

    assert_eq!(
        rule.modifier,
        Some(RuleModifier::Atomic),
        "string_literal should be atomic (@)"
    );

    // string_literal = @{ "\"" ~ string_inner* ~ "\"" }
    match &rule.pattern {
        PatternIR::Sequence(items) => {
            assert_eq!(items.len(), 3, "string_literal should have 3 elements");

            match &items[0] {
                PatternIR::Literal(s) => assert_eq!(s, "\""),
                _ => panic!("Element 0 should be opening quote"),
            }
            match &items[1] {
                PatternIR::Repeat { pattern, min, .. } => {
                    assert_eq!(*min, 0, "string_inner should be zero or more");
                    match pattern.as_ref() {
                        PatternIR::RuleRef { rule_name, .. } => {
                            assert_eq!(rule_name, "string_inner");
                        }
                        _ => panic!("Should repeat string_inner"),
                    }
                }
                _ => panic!("Element 1 should be Repeat"),
            }
            match &items[2] {
                PatternIR::Literal(s) => assert_eq!(s, "\""),
                _ => panic!("Element 2 should be closing quote"),
            }
        }
        _ => panic!("string_literal pattern should be Sequence"),
    }

    // Check action generates string_literal TypedExpression
    match &rule.action {
        Some(ActionIR::LegacyJson { return_type, json_content }) => {
            assert_eq!(return_type, "TypedExpression");
            assert!(json_content.contains("\"get_text\": true"), "Should get text");
            assert!(json_content.contains("string_literal"), "Should define string_literal");
        }
        _ => panic!("string_literal action should be LegacyJson"),
    }
}

#[test]
fn test_string_inner_complex_pattern() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let rule = grammar.get_rule("string_inner").expect("string_inner rule should exist");

    // string_inner = { !("\"" | "\\") ~ ANY | "\\" ~ ANY }
    // This should be a choice between escaped and non-escaped characters
    match &rule.pattern {
        PatternIR::Choice(alts) => {
            assert_eq!(alts.len(), 2, "string_inner should have 2 alternatives");
        }
        _ => panic!("string_inner pattern should be Choice"),
    }
}

#[test]
fn test_integer_is_atomic() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let rule = grammar.get_rule("integer").expect("integer rule should exist");

    assert_eq!(
        rule.modifier,
        Some(RuleModifier::Atomic),
        "integer should be atomic (@)"
    );

    // integer = @{ ASCII_DIGIT+ }
    match &rule.pattern {
        PatternIR::Repeat { pattern, min, max, .. } => {
            assert_eq!(*min, 1, "integer should be one or more digits");
            assert!(max.is_none(), "integer should have no max");
            match pattern.as_ref() {
                PatternIR::CharClass(CharClass::Builtin(name)) => {
                    assert_eq!(name, "ASCII_DIGIT");
                }
                _ => panic!("Should repeat ASCII_DIGIT"),
            }
        }
        _ => panic!("integer pattern should be Repeat"),
    }

    // Check action
    match &rule.action {
        Some(ActionIR::LegacyJson { return_type, json_content }) => {
            assert_eq!(return_type, "TypedExpression");
            assert!(json_content.contains("\"parse_int\": true"), "Should parse as int");
            assert!(json_content.contains("int_literal"), "Should define int_literal");
        }
        _ => panic!("integer action should be LegacyJson"),
    }
}

#[test]
fn test_signed_integer_allows_sign() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let rule = grammar.get_rule("signed_integer").expect("signed_integer rule should exist");

    assert_eq!(rule.modifier, Some(RuleModifier::Atomic));

    // signed_integer = @{ ("+" | "-")? ~ ASCII_DIGIT+ }
    match &rule.pattern {
        PatternIR::Sequence(items) => {
            assert_eq!(items.len(), 2, "signed_integer should have 2 elements");

            // First element: optional sign
            match &items[0] {
                PatternIR::Optional(inner) => {
                    match inner.as_ref() {
                        PatternIR::Choice(signs) => {
                            assert_eq!(signs.len(), 2, "Should have + and - choices");
                        }
                        _ => panic!("Optional should contain Choice"),
                    }
                }
                _ => panic!("Element 0 should be Optional sign"),
            }

            // Second element: digits
            match &items[1] {
                PatternIR::Repeat { min: 1, .. } => {}
                _ => panic!("Element 1 should be one or more digits"),
            }
        }
        _ => panic!("signed_integer pattern should be Sequence"),
    }
}

#[test]
fn test_number_supports_decimals() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let rule = grammar.get_rule("number").expect("number rule should exist");

    assert_eq!(rule.modifier, Some(RuleModifier::Atomic));

    // number = @{ "-"? ~ ASCII_DIGIT+ ~ ("." ~ ASCII_DIGIT+)? }
    match &rule.pattern {
        PatternIR::Sequence(items) => {
            // Should have: optional minus, digits, optional decimal part
            assert!(items.len() >= 2, "number should have at least 2 elements");
        }
        _ => panic!("number pattern should be Sequence"),
    }

    // Check action parses as float
    match &rule.action {
        Some(ActionIR::LegacyJson { json_content, .. }) => {
            assert!(json_content.contains("\"parse_float\": true"), "Should parse as float");
            assert!(json_content.contains("float_literal"), "Should define float_literal");
        }
        _ => panic!("number action should be LegacyJson"),
    }
}

#[test]
fn test_identifier_is_atomic() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let rule = grammar.get_rule("identifier").expect("identifier rule should exist");

    assert_eq!(
        rule.modifier,
        Some(RuleModifier::Atomic),
        "identifier should be atomic (@)"
    );

    // identifier = @{ ASCII_ALPHA ~ (ASCII_ALPHANUMERIC | "_")* }
    match &rule.pattern {
        PatternIR::Sequence(items) => {
            assert_eq!(items.len(), 2, "identifier should have 2 elements");

            // First char must be ASCII_ALPHA
            match &items[0] {
                PatternIR::CharClass(CharClass::Builtin(name)) => {
                    assert_eq!(name, "ASCII_ALPHA");
                }
                _ => panic!("Element 0 should be ASCII_ALPHA"),
            }

            // Rest can be alphanumeric or underscore
            match &items[1] {
                PatternIR::Repeat { min: 0, .. } => {}
                _ => panic!("Element 1 should be zero or more"),
            }
        }
        _ => panic!("identifier pattern should be Sequence"),
    }

    // Identifier returns String (text only)
    match &rule.action {
        Some(ActionIR::LegacyJson { return_type, json_content }) => {
            assert_eq!(return_type, "String");
            assert!(json_content.contains("\"get_text\": true"), "Should get text");
        }
        _ => panic!("identifier action should be LegacyJson"),
    }
}

#[test]
fn test_whitespace_is_silent() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let rule = grammar.get_rule("WHITESPACE").expect("WHITESPACE rule should exist");

    assert_eq!(
        rule.modifier,
        Some(RuleModifier::Silent),
        "WHITESPACE should be silent (_)"
    );

    // WHITESPACE = _{ " " | "\t" | "\n" | "\r" }
    match &rule.pattern {
        PatternIR::Choice(items) => {
            assert_eq!(items.len(), 4, "WHITESPACE should match 4 character types");
        }
        _ => panic!("WHITESPACE pattern should be Choice"),
    }
}

#[test]
fn test_comment_is_silent() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let rule = grammar.get_rule("COMMENT").expect("COMMENT rule should exist");

    assert_eq!(
        rule.modifier,
        Some(RuleModifier::Silent),
        "COMMENT should be silent (_)"
    );
}

// =============================================================================
// GrammarInterpreter Tests - Parse Using the Actual Grammar
// =============================================================================

#[test]
fn test_interpreter_parse_identifier() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let interp = GrammarInterpreter::new(&grammar);

    let mut builder = TypedASTBuilder::new();
    let mut registry = TypeRegistry::new();
    let mut state = ParserState::new("my_image_123", &mut builder, &mut registry);

    let result = interp.parse_rule("identifier", &mut state);

    match result {
        ParseResult::Success(ParsedValue::Text(text), pos) => {
            assert_eq!(text, "my_image_123");
            assert_eq!(pos, 12);
        }
        other => panic!("Expected text result, got {:?}", other),
    }
}

#[test]
fn test_interpreter_parse_integer() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let interp = GrammarInterpreter::new(&grammar);

    let mut builder = TypedASTBuilder::new();
    let mut registry = TypeRegistry::new();
    let mut state = ParserState::new("12345 rest", &mut builder, &mut registry);

    let result = interp.parse_rule("integer", &mut state);

    match result {
        ParseResult::Success(ParsedValue::Text(text), _) => {
            assert_eq!(text, "12345");
        }
        other => panic!("Expected text result, got {:?}", other),
    }
}

#[test]
fn test_interpreter_parse_signed_integer_positive() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let interp = GrammarInterpreter::new(&grammar);

    let mut builder = TypedASTBuilder::new();
    let mut registry = TypeRegistry::new();
    let mut state = ParserState::new("+30", &mut builder, &mut registry);

    let result = interp.parse_rule("signed_integer", &mut state);

    match result {
        ParseResult::Success(ParsedValue::Text(text), _) => {
            assert_eq!(text, "+30");
        }
        other => panic!("Expected text result, got {:?}", other),
    }
}

#[test]
fn test_interpreter_parse_signed_integer_negative() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let interp = GrammarInterpreter::new(&grammar);

    let mut builder = TypedASTBuilder::new();
    let mut registry = TypeRegistry::new();
    let mut state = ParserState::new("-20", &mut builder, &mut registry);

    let result = interp.parse_rule("signed_integer", &mut state);

    match result {
        ParseResult::Success(ParsedValue::Text(text), _) => {
            assert_eq!(text, "-20");
        }
        other => panic!("Expected text result, got {:?}", other),
    }
}

#[test]
fn test_interpreter_parse_number_integer() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let interp = GrammarInterpreter::new(&grammar);

    let mut builder = TypedASTBuilder::new();
    let mut registry = TypeRegistry::new();
    let mut state = ParserState::new("42", &mut builder, &mut registry);

    let result = interp.parse_rule("number", &mut state);

    match result {
        ParseResult::Success(ParsedValue::Text(text), _) => {
            assert_eq!(text, "42");
        }
        other => panic!("Expected text result, got {:?}", other),
    }
}

#[test]
fn test_interpreter_parse_number_decimal() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let interp = GrammarInterpreter::new(&grammar);

    let mut builder = TypedASTBuilder::new();
    let mut registry = TypeRegistry::new();
    let mut state = ParserState::new("3.14", &mut builder, &mut registry);

    let result = interp.parse_rule("number", &mut state);

    match result {
        ParseResult::Success(ParsedValue::Text(text), _) => {
            assert_eq!(text, "3.14");
        }
        other => panic!("Expected text result, got {:?}", other),
    }
}

#[test]
fn test_interpreter_parse_number_negative() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let interp = GrammarInterpreter::new(&grammar);

    let mut builder = TypedASTBuilder::new();
    let mut registry = TypeRegistry::new();
    let mut state = ParserState::new("-2.5", &mut builder, &mut registry);

    let result = interp.parse_rule("number", &mut state);

    match result {
        ParseResult::Success(ParsedValue::Text(text), _) => {
            assert_eq!(text, "-2.5");
        }
        other => panic!("Expected text result, got {:?}", other),
    }
}

#[test]
fn test_interpreter_parse_string_literal() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let interp = GrammarInterpreter::new(&grammar);

    let mut builder = TypedASTBuilder::new();
    let mut registry = TypeRegistry::new();
    let mut state = ParserState::new("\"hello world\"", &mut builder, &mut registry);

    let result = interp.parse_rule("string_literal", &mut state);

    match result {
        ParseResult::Success(ParsedValue::Text(text), pos) => {
            assert_eq!(text, "\"hello world\"");
            assert_eq!(pos, 13);
        }
        other => panic!("Expected text result, got {:?}", other),
    }
}

#[test]
fn test_interpreter_parse_load_stmt() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let interp = GrammarInterpreter::new(&grammar);

    let mut builder = TypedASTBuilder::new();
    let mut registry = TypeRegistry::new();
    let mut state = ParserState::new("load \"input.jpg\" as photo", &mut builder, &mut registry);

    let result = interp.parse_rule("load_stmt", &mut state);
    assert!(result.is_success(), "load_stmt should parse successfully");
    assert_eq!(state.pos(), 25, "Should consume entire input");
}

#[test]
fn test_interpreter_parse_save_stmt() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let interp = GrammarInterpreter::new(&grammar);

    let mut builder = TypedASTBuilder::new();
    let mut registry = TypeRegistry::new();
    let mut state = ParserState::new("save photo as \"output.png\"", &mut builder, &mut registry);

    let result = interp.parse_rule("save_stmt", &mut state);
    assert!(result.is_success(), "save_stmt should parse successfully");
}

#[test]
fn test_interpreter_parse_resize_stmt() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let interp = GrammarInterpreter::new(&grammar);

    let mut builder = TypedASTBuilder::new();
    let mut registry = TypeRegistry::new();
    let mut state = ParserState::new("resize photo to 800x600", &mut builder, &mut registry);

    let result = interp.parse_rule("resize_stmt", &mut state);
    assert!(result.is_success(), "resize_stmt should parse successfully");
}

#[test]
fn test_interpreter_parse_blur_stmt() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let interp = GrammarInterpreter::new(&grammar);

    let mut builder = TypedASTBuilder::new();
    let mut registry = TypeRegistry::new();
    let mut state = ParserState::new("blur photo by 2.5", &mut builder, &mut registry);

    let result = interp.parse_rule("blur_stmt", &mut state);
    assert!(result.is_success(), "blur_stmt should parse successfully");
}

#[test]
fn test_interpreter_parse_brightness_stmt() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let interp = GrammarInterpreter::new(&grammar);

    let mut builder = TypedASTBuilder::new();
    let mut registry = TypeRegistry::new();
    let mut state = ParserState::new("brighten photo by -30", &mut builder, &mut registry);

    let result = interp.parse_rule("brightness_stmt", &mut state);
    assert!(result.is_success(), "brightness_stmt should parse successfully");
}

#[test]
fn test_interpreter_parse_contrast_stmt() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let interp = GrammarInterpreter::new(&grammar);

    let mut builder = TypedASTBuilder::new();
    let mut registry = TypeRegistry::new();
    let mut state = ParserState::new("contrast photo by 1.5", &mut builder, &mut registry);

    let result = interp.parse_rule("contrast_stmt", &mut state);
    assert!(result.is_success(), "contrast_stmt should parse successfully");
}

#[test]
fn test_interpreter_parse_grayscale_stmt() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let interp = GrammarInterpreter::new(&grammar);

    let mut builder = TypedASTBuilder::new();
    let mut registry = TypeRegistry::new();
    let mut state = ParserState::new("grayscale photo", &mut builder, &mut registry);

    let result = interp.parse_rule("grayscale_stmt", &mut state);
    assert!(result.is_success(), "grayscale_stmt should parse successfully");
}

#[test]
fn test_interpreter_parse_print_stmt() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let interp = GrammarInterpreter::new(&grammar);

    let mut builder = TypedASTBuilder::new();
    let mut registry = TypeRegistry::new();
    let mut state = ParserState::new("print \"Vintage effect applied!\"", &mut builder, &mut registry);

    let result = interp.parse_rule("print_stmt", &mut state);
    assert!(result.is_success(), "print_stmt should parse successfully");
}

// =============================================================================
// End-to-End: Parse Actual vintage.imgpipe Using GrammarInterpreter
// =============================================================================

#[test]
fn test_interpreter_parse_vintage_imgpipe_statements() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let interp = GrammarInterpreter::new(&grammar);

    let mut builder = TypedASTBuilder::new();
    let mut registry = TypeRegistry::new();
    let mut state = ParserState::new(VINTAGE_IMGPIPE, &mut builder, &mut registry);

    // Parse using the 'statements' rule which matches statement*
    let result = interp.parse_rule("statements", &mut state);

    match result {
        ParseResult::Success(ParsedValue::List(statements), _) => {
            // vintage.imgpipe has 7 statements:
            // 1. load, 2. resize, 3. contrast, 4. brighten, 5. blur, 6. save, 7. print
            assert_eq!(
                statements.len(),
                7,
                "vintage.imgpipe should have 7 statements, got {}",
                statements.len()
            );
        }
        ParseResult::Success(other, _) => {
            panic!("Expected list of statements, got {:?}", other);
        }
        ParseResult::Failure(e) => {
            panic!("Failed to parse vintage.imgpipe: line {} col {}: {:?}", e.line, e.column, e.expected);
        }
    }
}

#[test]
fn test_interpreter_parse_vintage_imgpipe_program() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let interp = GrammarInterpreter::new(&grammar);

    let mut builder = TypedASTBuilder::new();
    let mut registry = TypeRegistry::new();
    let mut state = ParserState::new(VINTAGE_IMGPIPE, &mut builder, &mut registry);

    // Parse using the 'program' rule (SOI ~ statements ~ EOI)
    let result = interp.parse_rule("program", &mut state);

    match &result {
        ParseResult::Success(_, _) => {},
        ParseResult::Failure(e) => {
            // Show what's at the failure position
            let remaining = if state.pos() < VINTAGE_IMGPIPE.len() {
                &VINTAGE_IMGPIPE[state.pos()..std::cmp::min(state.pos() + 50, VINTAGE_IMGPIPE.len())]
            } else {
                ""
            };
            panic!(
                "Failed to parse vintage.imgpipe as program at line {} col {} (pos {}): expected {:?}\nRemaining input: {:?}",
                e.line, e.column, state.pos(), e.expected, remaining
            );
        }
    }

    assert!(
        result.is_success(),
        "Should successfully parse entire vintage.imgpipe as program"
    );

    // Should have consumed entire input
    assert!(
        state.is_eof(),
        "Should be at end of file after parsing program"
    );
}

#[test]
fn test_interpreter_parse_individual_vintage_statements() {
    let grammar = parse_grammar(IMAGEPIPE_GRAMMAR).unwrap();
    let interp = GrammarInterpreter::new(&grammar);

    // Test each statement from vintage.imgpipe individually
    let statements = vec![
        ("load_stmt", "load \"PXL_20251101_145444778.jpg\" as photo"),
        ("resize_stmt", "resize photo to 800x1200"),
        ("contrast_stmt", "contrast photo by -50"),
        ("brightness_stmt", "brighten photo by -30"),
        ("blur_stmt", "blur photo by 3"),
        ("save_stmt", "save photo as \"vintage_rotunda.png\""),
        ("print_stmt", "print \"Vintage effect applied!\""),
    ];

    for (rule_name, input) in statements {
        let mut builder = TypedASTBuilder::new();
        let mut registry = TypeRegistry::new();
        let mut state = ParserState::new(input, &mut builder, &mut registry);

        let result = interp.parse_rule(rule_name, &mut state);

        assert!(
            result.is_success(),
            "Failed to parse '{}' with rule '{}': input was '{}'",
            rule_name,
            rule_name,
            input
        );
    }
}
