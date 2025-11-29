//! Tests for the ZRTL test framework macros

use zrtl::prelude::*;
use zrtl_macros::zrtl_test;

// Test that the #[zrtl_test] macro works
#[zrtl_test]
fn test_basic_assertion() {
    zrtl_assert!(true);
    zrtl_assert!(1 + 1 == 2);
}

#[zrtl_test]
fn test_array_operations() {
    let mut arr: OwnedArray<i32> = OwnedArray::new().unwrap();
    arr.push(42);
    arr.push(100);

    zrtl_assert_eq!(arr.len(), 2);
    zrtl_assert_eq!(arr.get(0).unwrap(), 42);
    zrtl_assert_eq!(arr.get(1).unwrap(), 100);
    zrtl_assert_ne!(arr.get(0), arr.get(1));
}

#[zrtl_test]
fn test_string_operations() {
    let s = OwnedString::from("hello");
    zrtl_assert_eq!(s.len(), 5);
    zrtl_assert_eq!(s.as_str(), Some("hello"));
}

#[zrtl_test]
fn test_option_assertions() {
    let some_value: Option<i32> = Some(42);
    let none_value: Option<i32> = None;

    let value = zrtl_assert_some!(some_value);
    zrtl_assert_eq!(value, 42);

    zrtl_assert_none!(none_value);
}

#[zrtl_test]
fn test_result_assertions() {
    let ok_result: Result<i32, &str> = Ok(42);
    let err_result: Result<i32, &str> = Err("error");

    let value = zrtl_assert_ok!(ok_result);
    zrtl_assert_eq!(value, 42);

    let error = zrtl_assert_err!(err_result);
    zrtl_assert_eq!(error, "error");
}

#[zrtl_test]
fn test_with_custom_message() {
    let x = 5;
    zrtl_assert!(x > 0, "x should be positive, got {}", x);
    zrtl_assert_eq!(x, 5, "x should equal 5");
    zrtl_assert_ne!(x, 10, "x should not equal 10");
}

#[zrtl_test(ignore)]
fn test_ignored() {
    // This test should be skipped
    panic!("This should not run");
}

#[zrtl_test]
fn test_dynamic_box() {
    let boxed = DynamicBox::owned_i32(123);
    let value = zrtl_assert_some!(boxed.as_i32());
    zrtl_assert_eq!(value, 123);
}

#[zrtl_test]
fn test_type_tags() {
    let int_tag = TypeTag::I32;
    zrtl_assert_eq!(int_tag.category(), TypeCategory::Int);

    let struct_tag = TypeTag::new(TypeCategory::Struct, 42, TypeFlags::NONE);
    zrtl_assert_eq!(struct_tag.type_id(), 42);
    zrtl_assert!(!struct_tag.flags().is_nullable());
}
