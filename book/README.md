# The Zyn Book

A comprehensive guide to building language frontends with ZynPEG.

## Table of Contents

1. [Introduction](./01-introduction.md) - What is Zyn and why use it?
2. [Getting Started](./02-getting-started.md) - Your first Zyn grammar
3. [Using the CLI](./03-using-the-cli.md) - Compilation, execution, and REPL
4. [Grammar Syntax](./04-grammar-syntax.md) - PEG-based grammar rules
5. [Semantic Actions](./05-semantic-actions.md) - JSON command blocks
6. [The TypedAST](./06-typed-ast.md) - Understanding the target representation
7. [TypedAST Builder](./07-typed-ast-builder.md) - Building AST nodes programmatically
8. [Complete Example: Zig](./08-zig-example.md) - A real-world grammar walkthrough
9. [Reference](./09-reference.md) - Command reference and API
10. [Packaging & Distribution](./10-packaging-distribution.md) - ZPack format, AOT linking, and distribution
11. [HIR Builder](./11-hir-builder.md) - Building HIR directly for custom backends
12. [Embedding SDK](./12-embedding-sdk.md) - Embedding Zyntax in Rust applications with native calling
13. [Async Runtime](./13-async-runtime.md) - Promise-based async native runtime
14. [Runtime Plugins](./14-runtime-plugins.md) - ZRTL standard library plugins (I/O, FS, Net, Thread, etc.)
15. [Building DSLs](./15-building-dsls.md) - Creating domain-specific languages with Zyntax

## Quick Start

```bash
# Build zyntax
cargo build --release

# Compile and run a Zig file using the zig.zyn grammar
./target/release/zyntax compile \
    --grammar crates/zyn_peg/grammars/zig.zyn \
    --source examples/hello.zig \
    --run

# Start an interactive REPL
./target/release/zyntax repl --grammar crates/zyn_peg/grammars/zig.zyn
```

## What is Zyn?

Zyn is a Parser Expression Grammar (PEG) system that combines:

1. **Pest-compatible grammar syntax** - Familiar PEG rules for parsing
2. **JSON semantic actions** - Declarative AST construction
3. **TypedAST target** - A universal, typed intermediate representation

Instead of writing imperative code to build AST nodes, you declare *what* to build using JSON command blocks attached to grammar rules.

## Example

```zyn
// Grammar rule for integer literals
integer_literal = @{ "-"? ~ ASCII_DIGIT+ }
  -> TypedExpression {
      "get_text": true,
      "parse_int": true,
      "define": "int_literal",
      "args": { "value": "$result" }
  }
```

This single rule:

- Parses signed integers
- Extracts the matched text
- Converts it to an integer
- Creates a TypedAST literal node
