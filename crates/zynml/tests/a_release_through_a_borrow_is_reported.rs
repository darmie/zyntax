//! Releasing a parameter the function only borrows is an error.
//!
//! A borrowing function's caller keeps its claim: it may read the
//! storage afterwards, and something else is still going to release it.
//! A function that releases such a parameter breaks both, and the
//! symptom is a read of freed memory or a second release. Neither says
//! where it came from, and on a tree the second release walks memory
//! that has been handed back and recurses on whatever is now in it,
//! which arrives as a stack overflow nowhere near the cause.
//!
//! Declaring the parameter owned is how a function says it takes that
//! responsibility. This is what makes the difference between the two a
//! message rather than a crash.

use std::path::Path;
use zynml::{ZynML, ZynMLConfig};

fn load(src: &str) -> Result<(), String> {
    let plugins = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../plugins/target/zrtl");
    let cfg = ZynMLConfig {
        plugins_dir: plugins.to_string_lossy().to_string(),
        // The profile that enforces ownership. The tiered profiles do
        // not, so asking them would prove nothing either way.
        runtime_profile: zynml::ZynMLRuntimeProfile::Classic,
        ..ZynMLConfig::default()
    };
    let mut z = ZynML::with_config(cfg).map_err(|e| format!("{e:?}"))?;
    z.load_source(src).map_err(|e| format!("{e:?}"))?;
    Ok(())
}

const NODE: &str = r#"
import prelude

@reference
struct Node { left: Node, right: Node, item: i64 }
"#;

/// A method that frees its receiver without saying it consumes it.
#[test]
fn a_method_that_frees_a_borrowed_receiver_is_reported() {
    let err = load(&format!(
        "{NODE}
impl Node {{
    def make(item: i64): Node {{ return Node {{ left: null, right: null, item: item }} }}
    def dispose(self) {{ free(self as Ptr<i8>) }}
}}

def main(): i64 {{
    let n: Node = Node::make(7)
    n.dispose()
    return 0
}}"
    ))
    .expect_err("freeing a borrowed receiver should be refused");

    assert!(
        err.contains("dispose"),
        "the report should name the function it is in, got: {err}"
    );
    assert!(
        err.contains("only borrows") || err.contains("declared owned"),
        "the report should say what is wrong and what would be right, got: {err}"
    );
}

/// The same method, declared to consume its receiver, is accepted.
///
/// This is the half that makes the test above about `own` rather than
/// about freeing.
#[test]
fn declaring_the_receiver_owned_is_accepted() {
    load(&format!(
        "{NODE}
impl Node {{
    def make(item: i64): Node {{ return Node {{ left: null, right: null, item: item }} }}
    def dispose(own self: Node) {{ free(self as Ptr<i8>) }}
}}

def main(): i64 {{
    let n: Node = Node::make(7)
    n.dispose()
    return 0
}}"
    ))
    .expect("a receiver declared owned may be released");
}

/// A free function releasing a borrowed pointer is the same mistake.
#[test]
fn a_free_function_that_frees_a_borrowed_pointer_is_reported() {
    let err = load(
        r#"
import prelude
import simd

def dispose(p: Ptr<i8>) { free(p) }

def main(): i64 {
    let p: Ptr<i8> = alloc_i8(16)
    dispose(p)
    return 0
}
"#,
    )
    .expect_err("freeing a borrowed pointer should be refused");
    assert!(err.contains("dispose"), "should name the function: {err}");
}

/// And a function that merely reads through a borrowed pointer is not
/// reported, so the rule is about releasing rather than about pointers.
#[test]
fn reading_through_a_borrow_is_not_a_release() {
    load(
        r#"
import prelude
import simd

def total(p: Ptr<i8>, n: i64): i64 {
    let q: Ptr<i64> = p as Ptr<i64>
    let mut sum: i64 = 0
    let mut i: i64 = 0
    while i < n {
        sum = sum + q[i]
        i = i + 1
    }
    return sum
}

def main(): i64 {
    let p: Ptr<i8> = alloc_i8(64)
    let s: i64 = total(p, 4)
    free(p)
    return s
}
"#,
    )
    .expect("reading through a borrow is not releasing it");
}
