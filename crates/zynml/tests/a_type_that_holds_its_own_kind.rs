//! A struct whose fields are of its own type.
//!
//! A tree node holds two nodes. Expanding that type expands it again,
//! and the second expansion reaches the same two fields, so the two
//! `convert_type` implementations ran the compiler out of stack rather
//! than compiling it. Nothing reported it: the process aborted before
//! any diagnostic could be produced.
//!
//! The absence cost more than the crash. Every linked structure in the
//! language was unwritable, so the benchmark for allocation was written
//! with raw pointers and hand-computed offsets instead, and read as a
//! statement about `alloc` rather than about `struct`.
//!
//! Three further faults sat behind the first, each only reachable once
//! the one in front of it was fixed, and each covered here: `null` in a
//! field, reading a field through one, and a method that returns
//! nothing.

use std::path::Path;
use zynml::{ZynML, ZynMLConfig};

fn run(src: &str) -> i64 {
    let plugins = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../plugins/target/zrtl");
    let cfg = ZynMLConfig {
        plugins_dir: plugins.to_string_lossy().to_string(),
        ..ZynMLConfig::default()
    };
    let mut z = ZynML::with_config(cfg).expect("runtime");
    z.load_source(src).expect("should compile");
    z.call_with_result::<i64>("main").expect("should run")
}

const NODE: &str = r#"
import prelude

@reference
struct Node {
    left: Node,
    right: Node,
    item: i64
}
"#;

/// The type compiles at all, which is the whole of the first fault.
#[test]
fn a_field_of_its_own_type_compiles() {
    assert_eq!(
        run(&format!(
            "{NODE}
def main(): i64 {{
    let leaf = Node {{ left: null, right: null, item: 7 }}
    return leaf.item
}}"
        )),
        7
    );
}

/// `null` in a field is that field's absent pointer.
///
/// It used to lower as an optional's `None`, so a tagged union was
/// written into a slot holding an address and every leaf read back as
/// though it had children.
#[test]
fn an_absent_child_reads_as_absent() {
    assert_eq!(
        run(&format!(
            "{NODE}
def main(): i64 {{
    let leaf = Node {{ left: null, right: null, item: 7 }}
    let root = Node {{ left: leaf, right: null, item: 3 }}
    // 1 for each answer that is right, so a pass cannot come from one
    // of them compensating for the other.
    let mut ok: i64 = 0
    if leaf.left == null {{ ok = ok + 1 }}
    if root.left != null {{ ok = ok + 10 }}
    return ok
}}"
        )),
        11
    );
}

/// A field read through a field of the same type.
///
/// Stopping the expansion leaves the inner mention of the type with no
/// fields, which is the right width for the pointer holding it and
/// nothing to compute an offset from. Reading through one has to expand
/// the name again; before it did, the read fell through to an
/// `extractvalue` on an address.
#[test]
fn a_field_is_readable_through_a_field_of_the_same_type() {
    assert_eq!(
        run(&format!(
            "{NODE}
def main(): i64 {{
    let leaf = Node {{ left: null, right: null, item: 7 }}
    let root = Node {{ left: leaf, right: null, item: 3 }}
    return root.item + root.left.item
}}"
        )),
        10
    );
}

/// A method on the type, recursing through its own fields, including
/// one that returns nothing.
///
/// A method's return type was not recorded for its call sites to read,
/// so a call fell through to guessing and the guess was `I64`. That
/// bound a result to a call returning nothing, which Cranelift absorbs
/// and LLVM refuses.
///
/// The release is written `own self: Node` because it releases what it
/// is given. Without that the receiver is a borrow, and a borrow that
/// frees is a release the caller does not know happened.
#[test]
fn a_method_recurses_through_the_type_and_releases_it() {
    assert_eq!(
        run(&format!(
            "{NODE}
impl Node {{
    def build(item: i64, depth: i64): Node {{
        if depth > 0 {{
            return Node {{
                left: Node::build(2 * item - 1, depth - 1),
                right: Node::build(2 * item, depth - 1),
                item: item
            }}
        }}
        return Node {{ left: null, right: null, item: item }}
    }}

    def check(self): i64 {{
        if self.left == null {{ return self.item }}
        return self.item + self.left.check() - self.right.check()
    }}

    // `own` states that the call ends the caller's claim, so a
    // compiler that releases this type on its own does not release it
    // a second time behind this one.
    def release(own self: Node) {{
        if self.left != null {{
            self.left.release()
            self.right.release()
        }}
        free(self as Ptr<i8>)
    }}
}}

def main(): i64 {{
    let t: Node = Node::build(1, 6)
    let c: i64 = t.check()
    t.release()
    // Built and walked and released; 0 is what this tree checks to.
    return c
}}"
        )),
        0
    );
}
