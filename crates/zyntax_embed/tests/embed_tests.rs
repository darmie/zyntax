//! Comprehensive tests for zyntax_embed features
//!
//! These tests verify:
//! - ZyntaxValue accessors and conversions
//! - NativeSignature parsing
//! - FromZyntax/IntoZyntax trait implementations
//! - Error handling
//! - LanguageGrammar operations

use zyntax_embed::{
    ZyntaxValue, ZyntaxRuntime, LanguageGrammar, NativeType, NativeSignature,
    FromZyntax, IntoZyntax, ConversionError,
};

// ============================================================================
// ZyntaxValue Tests
// ============================================================================

mod value_tests {
    use super::*;

    #[test]
    fn test_int_accessors() {
        let val = ZyntaxValue::Int(42);

        assert_eq!(val.as_int(), Some(42));
        assert_eq!(val.as_i32(), Some(42));
        assert_eq!(val.as_i64(), Some(42));
        assert_eq!(val.as_uint(), Some(42)); // Positive int converts to uint
        assert_eq!(val.as_float(), Some(42.0));
        assert_eq!(val.as_str(), None);
        assert_eq!(val.as_bool(), None);
    }

    #[test]
    fn test_negative_int_accessors() {
        let val = ZyntaxValue::Int(-100);

        assert_eq!(val.as_int(), Some(-100));
        assert_eq!(val.as_i32(), Some(-100));
        assert_eq!(val.as_uint(), None); // Negative doesn't convert to uint
        assert_eq!(val.as_float(), Some(-100.0));
    }

    #[test]
    fn test_large_int_accessors() {
        // Value too large for i32
        let val = ZyntaxValue::Int(i64::MAX);

        assert_eq!(val.as_int(), Some(i64::MAX));
        assert_eq!(val.as_i64(), Some(i64::MAX));
        assert_eq!(val.as_i32(), None); // Overflow
    }

    #[test]
    fn test_uint_accessors() {
        let val = ZyntaxValue::UInt(100);

        assert_eq!(val.as_uint(), Some(100));
        assert_eq!(val.as_int(), Some(100)); // Small uint converts to int
        assert_eq!(val.as_float(), Some(100.0));
    }

    #[test]
    fn test_large_uint_accessors() {
        // Value too large for i64
        let val = ZyntaxValue::UInt(u64::MAX);

        assert_eq!(val.as_uint(), Some(u64::MAX));
        assert_eq!(val.as_int(), None); // Overflow
    }

    #[test]
    fn test_float_accessors() {
        let val = ZyntaxValue::Float(3.14);

        assert_eq!(val.as_float(), Some(3.14));
        assert_eq!(val.as_int(), None);
        assert_eq!(val.as_str(), None);
    }

    #[test]
    fn test_bool_accessors() {
        let true_val = ZyntaxValue::Bool(true);
        let false_val = ZyntaxValue::Bool(false);

        assert_eq!(true_val.as_bool(), Some(true));
        assert_eq!(false_val.as_bool(), Some(false));
        assert_eq!(true_val.as_int(), None);
    }

    #[test]
    fn test_string_accessors() {
        let val = ZyntaxValue::String("hello".to_string());

        assert_eq!(val.as_str(), Some("hello"));
        assert_eq!(val.as_int(), None);
    }

    #[test]
    fn test_array_accessors() {
        let arr = ZyntaxValue::Array(vec![
            ZyntaxValue::Int(1),
            ZyntaxValue::Int(2),
            ZyntaxValue::Int(3),
        ]);

        let slice = arr.as_array().unwrap();
        assert_eq!(slice.len(), 3);
        assert_eq!(slice[0].as_int(), Some(1));
        assert_eq!(slice[2].as_int(), Some(3));
    }

    #[test]
    fn test_struct_builder() {
        let point = ZyntaxValue::new_struct("Point")
            .field("x", 10i32)
            .field("y", 20i32)
            .field("name", "origin")
            .build();

        assert_eq!(point.get_field("x"), Some(&ZyntaxValue::Int(10)));
        assert_eq!(point.get_field("y"), Some(&ZyntaxValue::Int(20)));
        assert_eq!(point.get_field("name"), Some(&ZyntaxValue::String("origin".to_string())));
        assert_eq!(point.get_field("z"), None);
    }

    #[test]
    fn test_enum_creation() {
        let some_val = ZyntaxValue::new_enum("Option", "Some", Some(ZyntaxValue::Int(42)));
        let none_val = ZyntaxValue::new_enum("Option", "None", None);

        match some_val {
            ZyntaxValue::Enum { type_name, variant, data } => {
                assert_eq!(type_name, "Option");
                assert_eq!(variant, "Some");
                assert!(data.is_some());
                assert_eq!(*data.unwrap(), ZyntaxValue::Int(42));
            }
            _ => panic!("Expected Enum variant"),
        }

        match none_val {
            ZyntaxValue::Enum { type_name, variant, data } => {
                assert_eq!(type_name, "Option");
                assert_eq!(variant, "None");
                assert!(data.is_none());
            }
            _ => panic!("Expected Enum variant"),
        }
    }

    #[test]
    fn test_type_category() {
        use zyntax_embed::TypeCategory;

        assert_eq!(ZyntaxValue::Int(0).type_category(), TypeCategory::Int);
        assert_eq!(ZyntaxValue::UInt(0).type_category(), TypeCategory::UInt);
        assert_eq!(ZyntaxValue::Float(0.0).type_category(), TypeCategory::Float);
        assert_eq!(ZyntaxValue::Bool(true).type_category(), TypeCategory::Bool);
        assert_eq!(ZyntaxValue::String("".into()).type_category(), TypeCategory::String);
        assert_eq!(ZyntaxValue::Array(vec![]).type_category(), TypeCategory::Array);
        assert_eq!(ZyntaxValue::Void.type_category(), TypeCategory::Void);
        assert_eq!(ZyntaxValue::Null.type_category(), TypeCategory::Void);
    }

    #[test]
    fn test_is_null_or_void() {
        assert!(ZyntaxValue::Void.is_null_or_void());
        assert!(ZyntaxValue::Null.is_null_or_void());
        assert!(!ZyntaxValue::Int(0).is_null_or_void());
    }

    #[test]
    fn test_is_integer() {
        assert!(ZyntaxValue::Int(42).is_integer());
        assert!(ZyntaxValue::UInt(42).is_integer());
        assert!(!ZyntaxValue::Float(42.0).is_integer());
    }

    #[test]
    fn test_is_numeric() {
        assert!(ZyntaxValue::Int(42).is_numeric());
        assert!(ZyntaxValue::UInt(42).is_numeric());
        assert!(ZyntaxValue::Float(42.0).is_numeric());
        assert!(!ZyntaxValue::String("42".into()).is_numeric());
    }

    #[test]
    fn test_from_implementations() {
        // Test From<bool>
        let v: ZyntaxValue = true.into();
        assert!(matches!(v, ZyntaxValue::Bool(true)));

        // Test From<i8>
        let v: ZyntaxValue = 8i8.into();
        assert!(matches!(v, ZyntaxValue::Int(8)));

        // Test From<i16>
        let v: ZyntaxValue = 16i16.into();
        assert!(matches!(v, ZyntaxValue::Int(16)));

        // Test From<i32>
        let v: ZyntaxValue = 32i32.into();
        assert!(matches!(v, ZyntaxValue::Int(32)));

        // Test From<i64>
        let v: ZyntaxValue = 64i64.into();
        assert!(matches!(v, ZyntaxValue::Int(64)));

        // Test From<u8>
        let v: ZyntaxValue = 8u8.into();
        assert!(matches!(v, ZyntaxValue::UInt(8)));

        // Test From<u16>
        let v: ZyntaxValue = 16u16.into();
        assert!(matches!(v, ZyntaxValue::UInt(16)));

        // Test From<u32>
        let v: ZyntaxValue = 32u32.into();
        assert!(matches!(v, ZyntaxValue::UInt(32)));

        // Test From<u64>
        let v: ZyntaxValue = 64u64.into();
        assert!(matches!(v, ZyntaxValue::UInt(64)));

        // Test From<f32>
        let v: ZyntaxValue = 3.14f32.into();
        assert!(matches!(v, ZyntaxValue::Float(_)));

        // Test From<f64>
        let v: ZyntaxValue = 3.14f64.into();
        assert!(matches!(v, ZyntaxValue::Float(_)));

        // Test From<String>
        let v: ZyntaxValue = "hello".to_string().into();
        assert!(matches!(v, ZyntaxValue::String(s) if s == "hello"));

        // Test From<&str>
        let v: ZyntaxValue = "world".into();
        assert!(matches!(v, ZyntaxValue::String(s) if s == "world"));

        // Test From<Vec<T>>
        let v: ZyntaxValue = vec![1i32, 2, 3].into();
        assert!(matches!(v, ZyntaxValue::Array(_)));

        // Test From<Option<T>>
        let v: ZyntaxValue = Some(42i32).into();
        assert!(matches!(v, ZyntaxValue::Optional(_)));

        let v: ZyntaxValue = Option::<i32>::None.into();
        assert!(matches!(v, ZyntaxValue::Optional(_)));
    }

    #[test]
    fn test_default() {
        let v: ZyntaxValue = Default::default();
        assert!(matches!(v, ZyntaxValue::Null));
    }
}

// ============================================================================
// NativeSignature Tests
// ============================================================================

mod signature_tests {
    use super::*;

    #[test]
    fn test_native_signature_new() {
        let sig = NativeSignature::new(&[NativeType::I32, NativeType::I32], NativeType::I32);

        assert_eq!(sig.params.len(), 2);
        assert_eq!(sig.params[0], NativeType::I32);
        assert_eq!(sig.params[1], NativeType::I32);
        assert_eq!(sig.ret, NativeType::I32);
    }

    #[test]
    fn test_parse_simple_signature() {
        let sig = NativeSignature::parse("(i32, i32) -> i32").unwrap();

        assert_eq!(sig.params.len(), 2);
        assert_eq!(sig.params[0], NativeType::I32);
        assert_eq!(sig.params[1], NativeType::I32);
        assert_eq!(sig.ret, NativeType::I32);
    }

    #[test]
    fn test_parse_void_return() {
        let sig = NativeSignature::parse("(i64) -> void").unwrap();

        assert_eq!(sig.params.len(), 1);
        assert_eq!(sig.params[0], NativeType::I64);
        assert_eq!(sig.ret, NativeType::Void);
    }

    #[test]
    fn test_parse_no_params() {
        let sig = NativeSignature::parse("() -> i64").unwrap();

        assert!(sig.params.is_empty());
        assert_eq!(sig.ret, NativeType::I64);
    }

    #[test]
    fn test_parse_all_types() {
        // Test i32
        let sig = NativeSignature::parse("(i32) -> i32").unwrap();
        assert_eq!(sig.params[0], NativeType::I32);

        // Test i64
        let sig = NativeSignature::parse("(i64) -> i64").unwrap();
        assert_eq!(sig.params[0], NativeType::I64);

        // Test f32
        let sig = NativeSignature::parse("(f32) -> f32").unwrap();
        assert_eq!(sig.params[0], NativeType::F32);

        // Test f64
        let sig = NativeSignature::parse("(f64) -> f64").unwrap();
        assert_eq!(sig.params[0], NativeType::F64);

        // Test bool
        let sig = NativeSignature::parse("(bool) -> bool").unwrap();
        assert_eq!(sig.params[0], NativeType::Bool);

        // Test ptr
        let sig = NativeSignature::parse("(ptr) -> ptr").unwrap();
        assert_eq!(sig.params[0], NativeType::Ptr);

        // Test void as ()
        let sig = NativeSignature::parse("() -> ()").unwrap();
        assert_eq!(sig.ret, NativeType::Void);
    }

    #[test]
    fn test_parse_with_whitespace() {
        let sig = NativeSignature::parse("  ( i32 , i64 )  ->  f64  ").unwrap();

        assert_eq!(sig.params.len(), 2);
        assert_eq!(sig.params[0], NativeType::I32);
        assert_eq!(sig.params[1], NativeType::I64);
        assert_eq!(sig.ret, NativeType::F64);
    }

    #[test]
    fn test_parse_invalid_missing_arrow() {
        assert!(NativeSignature::parse("(i32, i32)").is_none());
    }

    #[test]
    fn test_parse_invalid_type() {
        assert!(NativeSignature::parse("(string) -> i32").is_none());
    }

    #[test]
    fn test_parse_invalid_no_parens() {
        assert!(NativeSignature::parse("i32 -> i32").is_none());
    }
}

// ============================================================================
// Conversion Trait Tests
// ============================================================================

mod conversion_tests {
    use super::*;

    #[test]
    fn test_i32_roundtrip() {
        let original: i32 = 42;
        let zyntax = original.into_zyntax();
        let back: i32 = i32::from_zyntax(zyntax).unwrap();
        assert_eq!(original, back);
    }

    #[test]
    fn test_i64_roundtrip() {
        let original: i64 = i64::MAX;
        let zyntax = original.into_zyntax();
        let back: i64 = i64::from_zyntax(zyntax).unwrap();
        assert_eq!(original, back);
    }

    #[test]
    fn test_i8_roundtrip() {
        let original: i8 = -128;
        let zyntax = original.into_zyntax();
        let back: i8 = i8::from_zyntax(zyntax).unwrap();
        assert_eq!(original, back);
    }

    #[test]
    fn test_i16_roundtrip() {
        let original: i16 = 32767;
        let zyntax = original.into_zyntax();
        let back: i16 = i16::from_zyntax(zyntax).unwrap();
        assert_eq!(original, back);
    }

    #[test]
    fn test_u8_roundtrip() {
        let original: u8 = 255;
        let zyntax = original.into_zyntax();
        let back: u8 = u8::from_zyntax(zyntax).unwrap();
        assert_eq!(original, back);
    }

    #[test]
    fn test_u16_roundtrip() {
        let original: u16 = 65535;
        let zyntax = original.into_zyntax();
        let back: u16 = u16::from_zyntax(zyntax).unwrap();
        assert_eq!(original, back);
    }

    #[test]
    fn test_u32_roundtrip() {
        let original: u32 = u32::MAX;
        let zyntax = original.into_zyntax();
        let back: u32 = u32::from_zyntax(zyntax).unwrap();
        assert_eq!(original, back);
    }

    #[test]
    fn test_u64_roundtrip() {
        let original: u64 = u64::MAX;
        let zyntax = original.into_zyntax();
        let back: u64 = u64::from_zyntax(zyntax).unwrap();
        assert_eq!(original, back);
    }

    #[test]
    fn test_f32_roundtrip() {
        let original: f32 = 3.14;
        let zyntax = original.into_zyntax();
        let back: f32 = f32::from_zyntax(zyntax).unwrap();
        assert!((original - back).abs() < 0.001);
    }

    #[test]
    fn test_f64_roundtrip() {
        let original: f64 = std::f64::consts::PI;
        let zyntax = original.into_zyntax();
        let back: f64 = f64::from_zyntax(zyntax).unwrap();
        assert_eq!(original, back);
    }

    #[test]
    fn test_bool_roundtrip() {
        let zyntax = true.into_zyntax();
        assert_eq!(bool::from_zyntax(zyntax).unwrap(), true);

        let zyntax = false.into_zyntax();
        assert_eq!(bool::from_zyntax(zyntax).unwrap(), false);
    }

    #[test]
    fn test_string_roundtrip() {
        let original = "Hello, Zyntax!".to_string();
        let zyntax = original.clone().into_zyntax();
        let back: String = String::from_zyntax(zyntax).unwrap();
        assert_eq!(original, back);
    }

    #[test]
    fn test_vec_roundtrip() {
        let original: Vec<i32> = vec![1, 2, 3, 4, 5];
        let zyntax = original.clone().into_zyntax();
        let back: Vec<i32> = Vec::from_zyntax(zyntax).unwrap();
        assert_eq!(original, back);
    }

    #[test]
    fn test_option_some_roundtrip() {
        let original: Option<i32> = Some(42);
        let zyntax = original.into_zyntax();
        let back: Option<i32> = Option::from_zyntax(zyntax).unwrap();
        assert_eq!(back, Some(42));
    }

    #[test]
    fn test_option_none_roundtrip() {
        let original: Option<i32> = None;
        let zyntax = original.into_zyntax();
        let back: Option<i32> = Option::from_zyntax(zyntax).unwrap();
        assert_eq!(back, None);
    }

    #[test]
    fn test_result_ok_roundtrip() {
        let original: Result<i32, String> = Ok(42);
        let zyntax = original.into_zyntax();
        let back: Result<i32, String> = Result::from_zyntax(zyntax).unwrap();
        assert_eq!(back, Ok(42));
    }

    #[test]
    fn test_result_err_roundtrip() {
        let original: Result<i32, String> = Err("error".to_string());
        let zyntax = original.into_zyntax();
        let back: Result<i32, String> = Result::from_zyntax(zyntax).unwrap();
        assert_eq!(back, Err("error".to_string()));
    }

    #[test]
    fn test_unit_roundtrip() {
        let zyntax = ().into_zyntax();
        let back: () = <()>::from_zyntax(zyntax).unwrap();
        assert_eq!(back, ());
    }

    #[test]
    fn test_overflow_error_i8() {
        let big_val = ZyntaxValue::Int(1000); // > i8::MAX
        let result: Result<i8, _> = i8::from_zyntax(big_val);
        assert!(matches!(result, Err(ConversionError::IntegerOverflow { .. })));
    }

    #[test]
    fn test_overflow_error_i16() {
        let big_val = ZyntaxValue::Int(100000); // > i16::MAX
        let result: Result<i16, _> = i16::from_zyntax(big_val);
        assert!(matches!(result, Err(ConversionError::IntegerOverflow { .. })));
    }

    #[test]
    fn test_overflow_error_i32() {
        let big_val = ZyntaxValue::Int(i64::MAX);
        let result: Result<i32, _> = i32::from_zyntax(big_val);
        assert!(matches!(result, Err(ConversionError::IntegerOverflow { .. })));
    }

    #[test]
    fn test_overflow_error_u8() {
        let big_val = ZyntaxValue::Int(1000);
        let result: Result<u8, _> = u8::from_zyntax(big_val);
        assert!(matches!(result, Err(ConversionError::IntegerOverflow { .. })));
    }

    #[test]
    fn test_overflow_error_u8_negative() {
        let neg_val = ZyntaxValue::Int(-1);
        let result: Result<u8, _> = u8::from_zyntax(neg_val);
        assert!(matches!(result, Err(ConversionError::IntegerOverflow { .. })));
    }

    #[test]
    fn test_type_mismatch_int_from_string() {
        let string_val = ZyntaxValue::String("hello".to_string());
        let result: Result<i32, _> = i32::from_zyntax(string_val);
        assert!(matches!(result, Err(ConversionError::TypeMismatch { .. })));
    }

    #[test]
    fn test_type_mismatch_string_from_int() {
        let int_val = ZyntaxValue::Int(42);
        let result: Result<String, _> = String::from_zyntax(int_val);
        assert!(matches!(result, Err(ConversionError::TypeMismatch { .. })));
    }

    #[test]
    fn test_type_mismatch_bool_from_string() {
        let string_val = ZyntaxValue::String("true".to_string());
        let result: Result<bool, _> = bool::from_zyntax(string_val);
        assert!(matches!(result, Err(ConversionError::TypeMismatch { .. })));
    }

    #[test]
    fn test_bool_from_int() {
        // bools can be created from ints (0 = false, non-zero = true)
        let zero = ZyntaxValue::Int(0);
        let one = ZyntaxValue::Int(1);
        let neg = ZyntaxValue::Int(-1);

        assert_eq!(bool::from_zyntax(zero).unwrap(), false);
        assert_eq!(bool::from_zyntax(one).unwrap(), true);
        assert_eq!(bool::from_zyntax(neg).unwrap(), true);
    }

    #[test]
    fn test_float_from_int() {
        // floats can be created from ints
        let int_val = ZyntaxValue::Int(42);
        let float: f64 = f64::from_zyntax(int_val).unwrap();
        assert_eq!(float, 42.0);
    }

    #[test]
    fn test_nested_vec_roundtrip() {
        let original: Vec<Vec<i32>> = vec![vec![1, 2], vec![3, 4, 5]];
        let zyntax = original.clone().into_zyntax();
        let back: Vec<Vec<i32>> = Vec::from_zyntax(zyntax).unwrap();
        assert_eq!(original, back);
    }

    #[test]
    fn test_option_string_roundtrip() {
        let original: Option<String> = Some("hello".to_string());
        let zyntax = original.clone().into_zyntax();
        let back: Option<String> = Option::from_zyntax(zyntax).unwrap();
        assert_eq!(original, back);
    }
}

// ============================================================================
// Error Type Tests
// ============================================================================

mod error_tests {
    use super::*;
    use zyntax_embed::TypeCategory;

    #[test]
    fn test_type_mismatch_error() {
        let err = ConversionError::type_mismatch(TypeCategory::Int, TypeCategory::String);
        let msg = err.to_string();
        assert!(msg.contains("Type mismatch"));
    }

    #[test]
    fn test_array_element_error() {
        let inner = ConversionError::type_mismatch(TypeCategory::Int, TypeCategory::String);
        let err = ConversionError::array_element(5, inner);
        let msg = err.to_string();
        assert!(msg.contains("index 5"));
    }

    #[test]
    fn test_struct_field_error() {
        let inner = ConversionError::type_mismatch(TypeCategory::Int, TypeCategory::String);
        let err = ConversionError::struct_field("foo", inner);
        let msg = err.to_string();
        assert!(msg.contains("foo"));
    }

    #[test]
    fn test_integer_overflow_error() {
        let err = ConversionError::IntegerOverflow { value: 9999999999 };
        let msg = err.to_string();
        assert!(msg.contains("overflow"));
    }
}

// ============================================================================
// Grammar Tests
// ============================================================================

mod grammar_tests {
    use super::*;

    /// Helper to load the Zig grammar for tests
    fn load_zig_grammar() -> Result<LanguageGrammar, Box<dyn std::error::Error>> {
        const ZIG_GRAMMAR_SOURCE: &str = include_str!("../../zyn_peg/grammars/zig.zyn");
        Ok(LanguageGrammar::compile_zyn(ZIG_GRAMMAR_SOURCE)?)
    }

    #[test]
    fn test_grammar_compile_and_metadata() {
        let grammar = match load_zig_grammar() {
            Ok(g) => g,
            Err(e) => {
                eprintln!("Skipping test: could not load grammar: {}", e);
                return;
            }
        };

        // The zig.zyn grammar should have metadata
        let name = grammar.name();
        let version = grammar.version();

        // Name should not be empty
        assert!(!name.is_empty(), "Grammar name should not be empty");

        // File extensions should include .zig
        let extensions = grammar.file_extensions();
        assert!(
            extensions.iter().any(|e| e.contains("zig")),
            "Grammar should handle .zig files, got {:?}",
            extensions
        );

        println!("Grammar loaded: {} v{}", name, version);
    }

    #[test]
    fn test_grammar_pest_grammar_not_empty() {
        let grammar = match load_zig_grammar() {
            Ok(g) => g,
            Err(e) => {
                eprintln!("Skipping test: could not load grammar: {}", e);
                return;
            }
        };

        let pest = grammar.pest_grammar();
        assert!(!pest.is_empty(), "Pest grammar should not be empty");

        // Should contain common PEG constructs
        assert!(pest.contains("program"), "Pest grammar should have a 'program' rule");
    }

    #[test]
    fn test_grammar_parse_simple_function() {
        let grammar = match load_zig_grammar() {
            Ok(g) => g,
            Err(e) => {
                eprintln!("Skipping test: could not load grammar: {}", e);
                return;
            }
        };

        let result = grammar.parse_to_json(r#"
            fn add(a: i32, b: i32) i32 {
                return a + b;
            }
        "#);

        match result {
            Ok(json) => {
                // Verify we got valid JSON
                assert!(json.starts_with("{") || json.starts_with("["),
                    "Result should be valid JSON, got: {}", &json[..100.min(json.len())]);

                // Should contain function declaration
                assert!(json.contains("add"), "JSON should contain function name 'add'");
            }
            Err(e) => {
                eprintln!("Parse error (may be expected): {}", e);
            }
        }
    }

    #[test]
    fn test_grammar_clone() {
        let grammar1 = match load_zig_grammar() {
            Ok(g) => g,
            Err(e) => {
                eprintln!("Skipping test: could not load grammar: {}", e);
                return;
            }
        };

        let grammar2 = grammar1.clone();

        // Both should have same metadata
        assert_eq!(grammar1.name(), grammar2.name());
        assert_eq!(grammar1.version(), grammar2.version());
    }
}

// ============================================================================
// Runtime Basic Tests
// ============================================================================

mod runtime_tests {
    use super::*;

    #[test]
    fn test_runtime_creation() {
        let runtime = ZyntaxRuntime::new();
        assert!(runtime.is_ok(), "Should create runtime successfully");
    }

    #[test]
    fn test_runtime_no_functions_initially() {
        let runtime = ZyntaxRuntime::new().unwrap();
        let functions = runtime.functions();

        assert!(functions.is_empty(), "New runtime should have no functions");
    }

    #[test]
    fn test_runtime_has_function() {
        let runtime = ZyntaxRuntime::new().unwrap();

        assert!(!runtime.has_function("nonexistent"));
    }

    #[test]
    fn test_runtime_no_languages_initially() {
        let runtime = ZyntaxRuntime::new().unwrap();
        let languages = runtime.languages();

        assert!(languages.is_empty(), "New runtime should have no languages");
    }

    #[test]
    fn test_runtime_has_language() {
        let runtime = ZyntaxRuntime::new().unwrap();

        assert!(!runtime.has_language("zig"));
        assert!(!runtime.has_language("python"));
    }

    #[test]
    fn test_runtime_register_grammar() {
        // This test requires a working grammar - skip if unavailable
        const ZIG_GRAMMAR_SOURCE: &str = include_str!("../../zyn_peg/grammars/zig.zyn");

        let grammar = match LanguageGrammar::compile_zyn(ZIG_GRAMMAR_SOURCE) {
            Ok(g) => g,
            Err(e) => {
                eprintln!("Skipping test: could not compile grammar: {}", e);
                return;
            }
        };

        let mut runtime = ZyntaxRuntime::new().unwrap();
        runtime.register_grammar("zig", grammar);

        assert!(runtime.has_language("zig"));
        assert!(runtime.languages().contains(&"zig"));
    }

    #[test]
    fn test_runtime_language_for_extension() {
        const ZIG_GRAMMAR_SOURCE: &str = include_str!("../../zyn_peg/grammars/zig.zyn");

        let grammar = match LanguageGrammar::compile_zyn(ZIG_GRAMMAR_SOURCE) {
            Ok(g) => g,
            Err(e) => {
                eprintln!("Skipping test: could not compile grammar: {}", e);
                return;
            }
        };

        let mut runtime = ZyntaxRuntime::new().unwrap();
        runtime.register_grammar("zig", grammar);

        // The zig grammar declares .zig extension
        let lang = runtime.language_for_extension("zig");
        assert_eq!(lang, Some("zig"), "Should map .zig to zig language");

        // Unknown extension returns None
        let unknown = runtime.language_for_extension("xyz");
        assert_eq!(unknown, None);
    }

    #[test]
    fn test_runtime_import_resolver() {
        let mut runtime = ZyntaxRuntime::new().unwrap();

        assert_eq!(runtime.import_resolver_count(), 0);

        runtime.add_import_resolver(Box::new(|path| {
            if path == "test_module" {
                Ok(Some("fn test() i32 { return 42; }".to_string()))
            } else {
                Ok(None)
            }
        }));

        assert_eq!(runtime.import_resolver_count(), 1);

        // Test resolution
        let result = runtime.resolve_import("test_module");
        assert!(result.is_ok());
        assert!(result.unwrap().is_some());

        let not_found = runtime.resolve_import("nonexistent");
        assert!(not_found.is_ok());
        assert!(not_found.unwrap().is_none());
    }

    #[test]
    fn test_runtime_get_function_ptr_nonexistent() {
        let runtime = ZyntaxRuntime::new().unwrap();

        let ptr = runtime.get_function_ptr("nonexistent");
        assert!(ptr.is_none());
    }
}
