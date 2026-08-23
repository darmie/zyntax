//! Ownership stated on a parameter reaches the IR.
//!
//! `own` is the only modifier that ends the caller's claim on what it
//! passes. Stating nothing leaves the type to decide: a pointer is
//! borrowed, because the caller still holds it and is still the one that
//! has to release it, and anything held by value is copied. So adding
//! this changes the meaning of no program that was already written.

use zynml::{Grammar2, ZYNML_GRAMMAR, ZYNML_STDLIB_PRELUDE, ZYNML_STDLIB_SIMD};
use zyntax_compiler::hir::ParamOwnership;
use zyntax_embed::ZyntaxRuntime;

/// Ownership modes of one function's parameters, in order.
fn modes(src: &str, func: &str) -> Vec<ParamOwnership> {
    let grammar = Grammar2::from_source(ZYNML_GRAMMAR).expect("grammar");
    let program = grammar.parse_with_filename(src, "<own>").expect("parse");
    let mut rt = ZyntaxRuntime::new().expect("runtime");
    rt.add_import_resolver(Box::new(|m| match m {
        "prelude" => Ok(Some(ZYNML_STDLIB_PRELUDE.to_string())),
        "simd" => Ok(Some(ZYNML_STDLIB_SIMD.to_string())),
        _ => Ok(None),
    }));
    let builtins = rt
        .config()
        .builtins
        .iter()
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect();
    let module = rt.lower_typed_program(program, builtins).expect("lower");
    module
        .functions
        .values()
        .find(|f| f.name.resolve_global().as_deref() == Some(func))
        .unwrap_or_else(|| panic!("{func} should be lowered"))
        .signature
        .params
        .iter()
        .map(|p| p.ownership)
        .collect()
}

/// A pointer parameter with nothing stated is borrowed, not owned.
#[test]
fn a_pointer_is_borrowed_by_default() {
    let m = modes(
        r#"
import prelude
import simd
def touch(p: Ptr<f32>, n: i64): i64 { return n }
"#,
        "touch",
    );
    assert_eq!(m, vec![ParamOwnership::Borrowed, ParamOwnership::Copied]);
}

/// `own` is what states consumption, and nothing else produces it.
#[test]
fn own_marks_a_parameter_as_consumed() {
    let m = modes(
        r#"
import prelude
import simd
def release(own p: Ptr<f32>, keep: Ptr<f32>): i64 { return 0 }
"#,
        "release",
    );
    assert_eq!(
        m,
        vec![ParamOwnership::Owned, ParamOwnership::Borrowed],
        "only the parameter written `own` should consume"
    );
}

/// `mut` states that the callee writes through the parameter while the
/// caller keeps it, which is a borrow and not a move.
#[test]
fn mut_is_a_borrow_not_a_move() {
    let m = modes(
        r#"
import prelude
import simd
def fill(mut p: Ptr<f32>, v: f32): i64 { return 0 }
"#,
        "fill",
    );
    assert_eq!(m, vec![ParamOwnership::BorrowedMut, ParamOwnership::Copied]);
    assert!(
        !m[0].consumes(),
        "a mutable borrow must not end the caller's claim"
    );
}

/// Scalars are copied, so ownership never enters into them.
#[test]
fn scalars_are_copied() {
    let m = modes(
        r#"
def add(a: i64, b: f64): i64 { return a }
"#,
        "add",
    );
    assert_eq!(m, vec![ParamOwnership::Copied, ParamOwnership::Copied]);
}

/// `own` on a method's receiver reaches the IR the way it does on a
/// free function's parameter.
///
/// A method is lowered through a `TypedFunction` built from it, and
/// what that construction carries decides whether an annotation on the
/// receiver survives. `own self: Node` parses, so a program can write
/// it; this is whether writing it means anything.
#[test]
fn own_on_a_receiver_reaches_the_ir() {
    let m = modes(
        r#"
import prelude

@reference
struct Node { left: Node, right: Node, item: i64 }

impl Node {
    def dispose(own self: Node) { free(self as Ptr<i8>) }
    def peek(self): i64 { return self.item }
}
"#,
        "Node$dispose",
    );
    assert_eq!(
        m,
        vec![ParamOwnership::Owned],
        "a receiver written `own` should consume, or the only way to \
         say a method releases its receiver is accepted and ignored"
    );
}

/// And a receiver with nothing stated is still a borrow, so the check
/// above is about `own` rather than about receivers in general.
#[test]
fn a_plain_receiver_is_borrowed() {
    let m = modes(
        r#"
import prelude

@reference
struct Node { left: Node, right: Node, item: i64 }

impl Node {
    def peek(self): i64 { return self.item }
}
"#,
        "Node$peek",
    );
    assert_eq!(m, vec![ParamOwnership::Borrowed]);
}
