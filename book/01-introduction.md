# Chapter 1: Introduction

## What is Zyn?

Zyn (ZynPEG) is a grammar-driven language frontend system that transforms source code into a typed abstract syntax tree (TypedAST). It combines three powerful concepts:

1. **PEG Parsing** - Uses Pest-compatible Parser Expression Grammars for syntax definition
2. **Declarative Semantics** - JSON command blocks describe how to build AST nodes
3. **Universal TypedAST** - A target representation that supports multiple programming paradigms

## The Problem Zyn Solves

Building a language frontend traditionally requires:

```
Source Code → Lexer → Parser → AST Builder → Type Checker → IR
```

Each stage requires significant code:
- Lexer rules and token definitions
- Parser with precedence handling
- AST node types and construction logic
- Visitor patterns for tree traversal

Zyn collapses the parser and AST builder into a single declarative specification:

```
Source Code → Zyn Grammar → TypedAST → Compiler Backend
```

## How It Works

A Zyn grammar file (`.zyn`) contains:

1. **Grammar rules** - PEG syntax defining what to parse
2. **Semantic actions** - JSON blocks defining what AST nodes to create

```zyn
// Grammar rule
integer_literal = @{ "-"? ~ ASCII_DIGIT+ }
  -> TypedExpression {
      "get_text": true,
      "parse_int": true,
      "define": "int_literal",
      "args": { "value": "$result" }
  }
```

When this rule matches, Zyn:
1. Extracts the matched text (`get_text`)
2. Parses it as an integer (`parse_int`)
3. Calls `create_int_literal(value)` on the AST builder

## The TypedAST

The TypedAST is a universal intermediate representation that can express:

- **Multiple paradigms**: OOP, functional, procedural
- **Rich type system**: Generics, traits, enums, structs
- **Language features**: Pattern matching, async/await, error handling

Every node carries:
- **Type information** - Full type annotation
- **Source spans** - Location for error reporting
- **Semantic data** - Names, operators, modifiers

```rust
pub struct TypedNode<T> {
    pub node: T,      // The actual AST node
    pub ty: Type,     // Type annotation
    pub span: Span,   // Source location
}
```

## Why Use Zyn?

### 1. Rapid Prototyping
Define a new language syntax in hours, not weeks. The grammar and semantics live in one file.

### 2. Correctness
Declarative specifications are easier to verify than imperative AST construction code.

### 3. Reusability
The TypedAST can target multiple backends: JIT compilation, LLVM, interpreters.

### 4. Maintainability
Grammar changes automatically propagate to AST construction - no separate files to update.

## Architecture Overview

```
┌─────────────────┐
│   Source File   │
│    (*.zig)      │
└────────┬────────┘
         │
         ▼
┌─────────────────┐     ┌─────────────────┐
│   Zyn Grammar   │────▶│   Pest Parser   │
│    (*.zyn)      │     │   (generated)   │
└─────────────────┘     └────────┬────────┘
                                 │
                                 ▼
                        ┌─────────────────┐
                        │ Command         │
                        │ Interpreter     │
                        └────────┬────────┘
                                 │
                                 ▼
                        ┌─────────────────┐
                        │  TypedAST       │
                        │  Builder        │
                        └────────┬────────┘
                                 │
                                 ▼
                        ┌─────────────────┐
                        │  TypedProgram   │
                        │  (JSON/Binary)  │
                        └────────┬────────┘
                                 │
                                 ▼
                        ┌─────────────────┐
                        │  Compiler       │
                        │  Backend        │
                        └─────────────────┘
```

## Next Steps

In the following chapters, you'll learn:

- [Chapter 2](./02-getting-started.md): Set up your environment and write your first grammar
- [Chapter 3](./03-using-the-cli.md): Use the CLI for compilation and REPL testing
- [Chapter 4](./04-grammar-syntax.md): Master PEG syntax for parsing
- [Chapter 5](./05-semantic-actions.md): Use JSON commands to build AST nodes
- [Chapter 6](./06-typed-ast.md): Understand the TypedAST structure
- [Chapter 7](./07-typed-ast-builder.md): Use the builder API directly
- [Chapter 8](./08-zig-example.md): Walk through a complete Zig implementation
