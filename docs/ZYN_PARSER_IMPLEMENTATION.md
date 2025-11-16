# ZynPEG: Parser Generator Implementation Plan

> **PEG-based parser generator for multi-language Zyntax frontends**
>
> **Status**: Design Phase
> **Target**: Q1 2025 POC, Q2 2025 Production

---

## 🎯 Executive Summary

### What is ZynPEG?

**ZynPEG** is a parser generator that enables rapid development of language frontends targeting Zyntax's TypedAST intermediate representation. It extends the proven **pest** PEG parser with native TypedAST generation capabilities.

### Vision

```
Language Source Code (.zig, .swift, .kt, .go, etc.)
    ↓
.zyn Grammar File (Zig.zyn, Swift.zyn, Kotlin.zyn)
    ↓
ZynPEG Parser Generator
    ↓
Generated Rust Parser
    ↓
Zyntax TypedAST (with rich type system: generics, traits, lifetimes)
    ↓
[Rest of Zyntax Pipeline: HIR → Cranelift/LLVM → Native Code]
```

### Goals

1. **Multi-language support** - Enable Zyntax to compile ANY language
2. **Rapid frontend development** - Add new language in weeks, not months
3. **Consistent TypedAST** - All frontends produce well-formed, type-correct AST
4. **Excellent diagnostics** - Rich error messages with span tracking
5. **Type system integration** - Built-in type inference and checking

---

## 📊 Technology Choice: pest

### Why pest?

After comprehensive analysis of Rust parser generators:

| Feature | pest | peg | nom | lalrpop | winnow |
|---------|------|-----|-----|---------|--------|
| External grammar files | ✅ | ❌ | ❌ | ✅ | ❌ |
| PEG-based | ✅ | ✅ | ❌ | ❌ | ❌ |
| Production ready | ✅ | ✅ | ✅ | ✅ | ✅ |
| Easy to fork | ✅ | ⚠️ | ⚠️ | ✅ | ⚠️ |
| Downloads/month | 3M | 100K | 2M | 400K | 800K |
| Active development | ✅ | ✅ | ✅ | ✅ | ✅ |

**Winner: pest**

- ✅ **External `.pest` files** - Easy to rename to `.zyn`
- ✅ **40M+ total downloads** - Battle-tested
- ✅ **Used by**: Gleam, Deno, Dhall compilers
- ✅ **MIT/Apache-2.0** - Fork-friendly license
- ✅ **Self-hosted** - pest parses itself!
- ✅ **Clear separation** - Parse tree → AST building

**Fork target**: https://github.com/pest-parser/pest

---

## 🗺️ Implementation Roadmap

### Phase 1: POC with Stock pest (2-4 weeks)

**Goal**: Prove the concept with minimal changes

**Deliverables**:
- [ ] Create `crates/zyn_parser` workspace crate
- [ ] Integrate pest + pest_derive dependencies
- [ ] Write calculator.pest grammar (future .zyn format)
- [ ] Implement TypedAST builder for calculator
- [ ] End-to-end test: source → TypedAST → HIR → JIT execution
- [ ] Document integration patterns

**Example Grammar** (calculator.pest):
```pest
program = { SOI ~ expr ~ EOI }

expr = {
    number
  | binary_op
  | "(" ~ expr ~ ")"
}

binary_op = { expr ~ operator ~ expr }
operator = { "+" | "-" | "*" | "/" }
number = @{ ASCII_DIGIT+ ("." ~ ASCII_DIGIT+)? }

WHITESPACE = _{ " " | "\t" | "\n" }
```

**TypedAST Builder**:
```rust
pub fn build_typed_ast(
    pairs: Pairs<Rule>,
    arena: &mut AstArena,
    type_registry: &mut TypeRegistry,
) -> Result<TypedProgram, ParseError> {
    for pair in pairs {
        match pair.as_rule() {
            Rule::binary_op => {
                // Extract left, op, right
                let mut inner = pair.into_inner();
                let left = build_expr(inner.next().unwrap(), arena, type_registry)?;
                let op_str = inner.next().unwrap().as_str();
                let right = build_expr(inner.next().unwrap(), arena, type_registry)?;

                // Build TypedExpression
                TypedExpression {
                    expr: Expression::BinaryOp(
                        map_operator(op_str),
                        Box::new(left),
                        Box::new(right),
                    ),
                    ty: infer_binary_type(&left.ty, op_str, &right.ty)?,
                    span: Span::from_pest(pair.as_span()),
                }
            }
            // ... other rules
        }
    }
}
```

**Success Metrics**:
- ✅ Parse arithmetic expressions
- ✅ Generate valid TypedAST
- ✅ Compile through Zyntax HIR
- ✅ Execute with Cranelift JIT
- ✅ Correct results for 100+ test cases

**Timeline**: 2-4 weeks
**Risk**: Low - Using battle-tested library

---

### Phase 2: Full Language Grammar (4-8 weeks)

**Goal**: Support a complete statically-typed language

**Target Language Options**:
1. **Zig subset** - Modern systems language with explicit types, comptime, no hidden control flow
2. **Swift subset** - Modern language with strong type system, generics, protocols
3. **Kotlin subset** - Nullable types, data classes, when expressions
4. **Go subset** - Interfaces, goroutines (map to async), explicit error handling

**Recommended: Zig subset**

**Why Zig?**
- Explicit, static type system (aligns with Zyntax's TypedAST)
- No hidden memory allocations (clear ownership semantics)
- Comptime evaluation (maps to Zyntax's type-level computation)
- Error unions (explicit error handling)
- Simple, consistent syntax
- Growing community

**Deliverables**:
- [ ] Complete Zig.pest grammar covering:
  - [ ] Functions with explicit types and return values
  - [ ] Struct definitions with fields
  - [ ] Control flow (if, while, for, switch)
  - [ ] Primitive types (i32, f64, bool, etc.)
  - [ ] Pointers and slices
  - [ ] Error unions (Result types)
  - [ ] Basic generics
- [ ] Comprehensive TypedAST builder
- [ ] Type checking for Zig semantics
- [ ] Error recovery and diagnostics
- [ ] 1000+ test cases
- [ ] Documentation and examples

**Grammar Preview** (zig.pest):
```pest
program = { SOI ~ top_level_decl* ~ EOI }

top_level_decl = {
    function_def
  | struct_def
  | const_decl
}

function_def = {
    "fn" ~ ident ~ "(" ~ params? ~ ")" ~ type_expr ~ "{" ~ statement* ~ "}"
}

struct_def = {
    "struct" ~ "{" ~ field_def* ~ "}"
}

type_expr = {
    "i32" | "i64" | "f32" | "f64" | "bool" | "void"
  | "*" ~ type_expr  // pointer
  | "[" ~ "]" ~ type_expr  // slice
  | ident  // named type
}

// ... rest of Zig grammar
```

**Success Metrics**:
- ✅ Parse realistic Zig programs (100+ LOC)
- ✅ Correct TypedAST for all language features
- ✅ Type errors caught during parsing
- ✅ Compile and execute Zig programs via Zyntax
- ✅ Performance: <100ms parse time for 1000 LOC
- ✅ Handle generics and type parameters
- ✅ Error union types map to Result<T, E>
- ✅ Comptime expressions properly lowered

**Timeline**: 4-8 weeks
**Risk**: Medium - Complexity in type system mapping

---

### Phase 3: Fork pest → ZynPEG (8-12 weeks)

**Goal**: Native `.zyn` format with TypedAST actions

**Fork Strategy**:
```bash
git clone https://github.com/pest-parser/pest.git zyntax-pest
cd zyntax-pest
git checkout -b zyntax-integration
```

**Key Modifications**:

#### 3.1 Extend Grammar Syntax

**Before** (pest):
```pest
expr = { number | binary_op }
binary_op = { expr ~ "+" ~ expr }
```

**After** (ZynPEG):
```zyn
@imports {
    use zyntax_typed_ast::*;
}

@context {
    arena: &mut AstArena,
    type_registry: &mut TypeRegistry,
}

expr = { number | binary_op }

binary_op = { expr ~ "+" ~ expr }
  -> TypedExpression {
      expr: BinaryOp(Add, $1, $3),
      ty: infer_type($1.ty, Add, $3.ty),
      span: span($1, $3)
  }

number = @{ ASCII_DIGIT+ }
  -> TypedExpression {
      expr: IntLiteral($1.parse()),
      ty: Type::I32,
      span: $1.span
  }
```

**New Syntax**:
- `@imports { }` - Rust imports for actions
- `@context { }` - Available context during parsing
- `-> TypedExpression { }` - AST construction actions
- `$1, $2, $3` - Capture references
- `span($1, $3)` - Span helper functions

#### 3.2 Modify pest_generator

**File**: `generator/src/lib.rs`

**Changes**:
```rust
// Add action block parsing
struct ActionBlock {
    rule_name: String,
    imports: Vec<String>,
    context_vars: HashMap<String, String>,
    action_code: String,
}

// Extend code generation
fn generate_parser_with_actions(grammar: &Grammar) -> TokenStream {
    // Generate pest parser
    let pest_parser = generate_pest_parser(grammar);

    // Generate TypedAST builder
    let ast_builder = generate_ast_builder(grammar);

    quote! {
        #pest_parser
        #ast_builder

        impl ZynParser {
            pub fn parse_to_typed_ast(
                input: &str,
                arena: &mut AstArena,
                type_registry: &mut TypeRegistry,
            ) -> Result<TypedProgram, ParseError> {
                let pairs = Self::parse(Rule::program, input)?;
                build_typed_ast(pairs, arena, type_registry)
            }
        }
    }
}
```

#### 3.3 Type Inference Integration

**Add built-in type helpers**:
```zyn
@type_helpers {
    fn infer_type(left_ty: Type, op: BinaryOperator, right_ty: Type) -> Type {
        match (left_ty, op, right_ty) {
            (Type::I32, Add, Type::I32) => Type::I32,
            (Type::String, Add, Type::String) => Type::String,
            (Type::F64, _, Type::F64) => Type::F64,
            _ => Type::Error
        }
    }

    fn check_assignable(target: &Type, value: &Type) -> bool {
        // Type compatibility checking
    }
}
```

#### 3.4 Error Diagnostics

**Enhanced error messages**:
```rust
pub struct ZynParseError {
    pub message: String,
    pub span: Span,
    pub severity: Severity,
    pub hints: Vec<String>,
    pub related: Vec<(Span, String)>,
}

impl ZynParseError {
    pub fn type_mismatch(
        span: Span,
        expected: &Type,
        found: &Type,
    ) -> Self {
        ZynParseError {
            message: format!(
                "Type mismatch: expected {}, found {}",
                expected, found
            ),
            span,
            severity: Severity::Error,
            hints: vec![
                format!("Try converting {} to {}", found, expected),
            ],
            related: vec![],
        }
    }
}
```

**Deliverables**:
- [ ] Fork pest repository
- [ ] Extend grammar syntax with action blocks
- [ ] Modify pest_generator for TypedAST emission
- [ ] Add type inference helpers
- [ ] Implement rich error diagnostics
- [ ] Update documentation
- [ ] Create migration guide from .pest to .zyn
- [ ] Comprehensive test suite

**Success Metrics**:
- ✅ `.zyn` grammars compile to TypedAST-generating parsers
- ✅ No manual AST building required
- ✅ Type errors detected during parsing
- ✅ Excellent error messages
- ✅ 100% backward compatible with .pest files

**Timeline**: 8-12 weeks
**Risk**: High - Complex code generation

---

### Phase 4: Multi-Language Ecosystem (Ongoing)

**Goal**: Support multiple production languages

**Target Languages** (statically-typed systems languages):
1. **Zig** - Modern systems language with explicit types and comptime
2. **Swift** - Protocol-oriented programming with strong type inference
3. **Kotlin** - Null safety, data classes, sealed types
4. **Go** - Interfaces, goroutines (map to async), simplicity
5. **Rust subset** - Ownership, borrowing, traits (similar to Zyntax's own type system)
6. **Custom DSLs** - Type-safe domain-specific languages

**Grammar Library Structure**:
```
grammars/
├── zig.zyn             # Zig language
├── swift.zyn           # Swift language
├── kotlin.zyn          # Kotlin language
├── go.zyn              # Go language
├── rust.zyn            # Rust subset
├── examples/
│   ├── calc.zyn        # Calculator DSL
│   ├── shader.zyn      # GPU shader language (explicit types)
│   └── protocol.zyn    # Network protocol DSL
├── stdlib/
│   ├── common.zyn      # Common patterns
│   ├── types.zyn       # Type system helpers
│   ├── generics.zyn    # Generic type patterns
│   └── operators.zyn   # Operator precedence
└── tests/
    └── conformance/    # Language conformance tests
```

**For Each Language**:
- [ ] Complete grammar specification
- [ ] TypedAST mapping
- [ ] Type system integration
- [ ] Standard library mapping
- [ ] Conformance test suite (1000+ tests)
- [ ] Documentation and examples
- [ ] Performance benchmarks

**Success Metrics**:
- ✅ 95%+ language conformance
- ✅ <500ms compile time for 10K LOC
- ✅ Correct execution on test suites
- ✅ Community adoption

**Timeline**: Ongoing (6+ months)
**Risk**: Medium - Language-specific complexities

---

## 🏗️ Technical Architecture

### Component Overview

```
┌─────────────────────────────────────────────────┐
│              ZynPEG System                      │
├─────────────────────────────────────────────────┤
│                                                 │
│  ┌───────────────────────────────────────────┐ │
│  │  .zyn Grammar Files                       │ │
│  │  • Language syntax definitions            │ │
│  │  • TypedAST action blocks                 │ │
│  │  • Type inference rules                   │ │
│  └────────────────┬──────────────────────────┘ │
│                   │                             │
│                   ▼                             │
│  ┌───────────────────────────────────────────┐ │
│  │  ZynPEG Parser Generator                  │ │
│  │  (Forked from pest)                       │ │
│  │  • Grammar parsing and validation         │ │
│  │  • Code generation (Rust)                 │ │
│  │  • TypedAST builder synthesis             │ │
│  └────────────────┬──────────────────────────┘ │
│                   │                             │
│                   ▼                             │
│  ┌───────────────────────────────────────────┐ │
│  │  Generated Rust Parser                    │ │
│  │  • PEG parser (from pest)                 │ │
│  │  • TypedAST builder (from actions)        │ │
│  │  • Type checking logic                    │ │
│  └────────────────┬──────────────────────────┘ │
│                   │                             │
└───────────────────┼─────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────────────┐
│         Zyntax TypedAST                         │
│  • Multi-paradigm type system                   │
│  • Generics, traits, lifetimes                  │
│  • Rich diagnostics                             │
└────────────────┬────────────────────────────────┘
                 │
                 ▼
         [HIR → Backends → Native Code]
```

### File Organization

```
zyntax/
├── crates/
│   ├── zyn_parser/              # NEW: Parser generator
│   │   ├── Cargo.toml
│   │   ├── src/
│   │   │   ├── lib.rs
│   │   │   ├── grammar.rs       # Grammar parsing
│   │   │   ├── generator.rs     # Code generation
│   │   │   ├── actions.rs       # Action block handling
│   │   │   ├── type_helpers.rs  # Type inference
│   │   │   └── diagnostics.rs   # Error messages
│   │   ├── grammars/            # NEW: .zyn grammar files
│   │   │   ├── python.zyn
│   │   │   ├── javascript.zyn
│   │   │   └── examples/
│   │   │       └── calculator.zyn
│   │   └── tests/
│   │       ├── calculator_tests.rs
│   │       └── python_tests.rs
│   │
│   ├── typed_ast/               # EXISTING
│   ├── compiler/                # EXISTING
│   └── whirlwind_adapter/       # EXISTING
│
└── docs/
    ├── ZYN_PARSER_IMPLEMENTATION.md  # This file
    ├── ZYN_GRAMMAR_GUIDE.md          # NEW: Grammar writing
    └── ZYN_TYPE_SYSTEM.md            # NEW: Type integration
```

### Integration with Zyntax

**Current Zyntax Pipeline**:
```
Source → [Language Parser] → TypedAST → HIR → Cranelift/LLVM → Native
```

**With ZynPEG**:
```
Source → [ZynPEG-generated Parser] → TypedAST → HIR → Cranelift/LLVM → Native
```

**Key Interfaces**:

```rust
// crates/zyn_parser/src/lib.rs

/// Main entry point for ZynPEG-generated parsers
pub trait ZynLanguageParser {
    /// Parse source code to TypedAST
    fn parse(
        source: &str,
        arena: &mut AstArena,
        type_registry: &mut TypeRegistry,
    ) -> Result<TypedProgram, ParseError>;

    /// Get language metadata
    fn language_info() -> LanguageInfo;
}

/// Generated by ZynPEG for each .zyn grammar
pub struct GeneratedParser {
    pest_parser: PestParser,
    ast_builder: AstBuilder,
}

impl ZynLanguageParser for GeneratedParser {
    fn parse(
        source: &str,
        arena: &mut AstArena,
        type_registry: &mut TypeRegistry,
    ) -> Result<TypedProgram, ParseError> {
        // 1. Parse with pest
        let pairs = self.pest_parser.parse(Rule::program, source)?;

        // 2. Build TypedAST with generated builder
        let program = self.ast_builder.build(pairs, arena, type_registry)?;

        // 3. Validate types
        type_registry.validate(&program)?;

        Ok(program)
    }
}
```

**Usage Example**:
```rust
use zyntax_typed_ast::arena::AstArena;
use zyntax_typed_ast::type_registry::TypeRegistry;
use zyn_parser::python::PythonParser;

fn compile_python(source: &str) -> Result<(), CompileError> {
    let mut arena = AstArena::new();
    let mut type_registry = TypeRegistry::new();

    // Parse Python source to TypedAST
    let program = PythonParser::parse(source, &mut arena, &mut type_registry)?;

    // Continue with existing Zyntax pipeline
    let hir = lower_to_hir(&program, &type_registry)?;
    let mut backend = CraneliftBackend::new()?;
    backend.compile_module(&hir)?;
    backend.execute_main()?;

    Ok(())
}
```

---

## 📚 Grammar Specification (.zyn format)

### Basic Structure

```zyn
// python.zyn - Python language grammar

@language {
    name: "Python",
    version: "3.12",
    file_extensions: [".py"],
}

@imports {
    use zyntax_typed_ast::*;
    use zyntax_typed_ast::typed_ast::*;
}

@context {
    arena: &mut AstArena,
    type_registry: &mut TypeRegistry,
}

// Entry point
program = { SOI ~ statement* ~ EOI }
  -> TypedProgram {
      statements: $1.collect(),
      module_path: vec![],
  }

// ... grammar rules with actions
```

### Grammar Rule Syntax

**Pattern**: `rule_name = { pattern } [-> Type { action }]`

**Components**:
- `rule_name` - Identifier for the rule
- `{ pattern }` - PEG pattern (pest syntax)
- `-> Type { }` - Optional action block for TypedAST

**Patterns** (pest PEG syntax):
- `"text"` - Literal string
- `rule_name` - Call another rule
- `pattern*` - Zero or more
- `pattern+` - One or more
- `pattern?` - Optional
- `pattern1 | pattern2` - Ordered choice
- `pattern1 ~ pattern2` - Sequence
- `!pattern` - Negative lookahead
- `&pattern` - Positive lookahead

**Example**:
```zyn
// Function definition
function_def = {
    "def" ~ ident ~ "(" ~ params? ~ ")" ~ type_hint? ~ ":" ~ block
}
  -> TypedDeclaration {
      decl: FunctionDecl {
          name: $2.intern(arena),
          params: $4.unwrap_or_default(),
          return_type: $6.unwrap_or(Type::Void),
          body: $8,
      },
      visibility: Visibility::Public,
      span: span($1, $8),
  }
```

### Action Block Syntax

**Capture References**:
- `$1, $2, $3, ...` - Positional captures (1-indexed)
- `$rule_name` - Named captures

**Helper Functions**:
- `span($start, $end)` - Create span from captures
- `intern(arena, str)` - Intern string in arena
- `parse_int(str)` - Parse integer literal
- `parse_float(str)` - Parse float literal
- `infer_type(...)` - Type inference helpers

**Example with Helpers**:
```zyn
binary_op = { expr ~ operator ~ expr }
  -> TypedExpression {
      let left_ty = $1.ty;
      let right_ty = $3.ty;
      let op = $2;

      expr: BinaryOp(op, Box::new($1), Box::new($3)),
      ty: infer_binary_type(left_ty, op, right_ty)?,
      span: span($1, $3),
  }
```

### Type Inference Blocks

```zyn
@type_helpers {
    /// Infer type of binary operation
    fn infer_binary_type(
        left: Type,
        op: BinaryOperator,
        right: Type,
    ) -> Result<Type, TypeError> {
        match (left, op, right) {
            // Int arithmetic
            (Type::I32, Add | Sub | Mul | Div | Mod, Type::I32) => Ok(Type::I32),

            // Float arithmetic
            (Type::F64, Add | Sub | Mul | Div, Type::F64) => Ok(Type::F64),

            // String concatenation
            (Type::String, Add, Type::String) => Ok(Type::String),

            // Comparisons
            (Type::I32, Eq | Ne | Lt | Le | Gt | Ge, Type::I32) => Ok(Type::Bool),

            // Error
            _ => Err(TypeError::IncompatibleTypes {
                left,
                op,
                right,
            }),
        }
    }
}
```

### Error Handling

```zyn
@error_messages {
    type_mismatch(expected: Type, found: Type, span: Span) {
        message: "Type mismatch",
        primary: (span, format!("expected {}, found {}", expected, found)),
        hints: vec![
            format!("Try converting {} to {}", found, expected),
        ],
    }

    undefined_variable(name: String, span: Span) {
        message: format!("Undefined variable '{}'", name),
        primary: (span, "not found in this scope"),
        hints: vec![
            "Check if the variable is declared before use",
        ],
    }
}
```

---

## 🧪 Testing Strategy

### Test Pyramid

```
        ┌──────────────────┐
        │  E2E Tests       │  ← Full pipeline: source → native
        │  (100 tests)     │
        └──────────────────┘
            ┌──────────────────────┐
            │  Integration Tests   │  ← Parse → TypedAST → HIR
            │  (500 tests)         │
            └──────────────────────┘
                ┌──────────────────────────┐
                │  Unit Tests              │  ← Individual grammar rules
                │  (2000 tests)            │
                └──────────────────────────┘
```

### Unit Tests (Grammar Rules)

```rust
#[test]
fn test_parse_number() {
    let mut arena = AstArena::new();
    let mut type_registry = TypeRegistry::new();

    let result = CalculatorParser::parse("42", &mut arena, &mut type_registry);

    assert!(result.is_ok());
    let expr = result.unwrap();
    assert_eq!(expr.ty, Type::I32);
    match expr.expr {
        Expression::IntLiteral(42) => {}
        _ => panic!("Expected IntLiteral(42)"),
    }
}

#[test]
fn test_parse_binary_op() {
    let mut arena = AstArena::new();
    let mut type_registry = TypeRegistry::new();

    let result = CalculatorParser::parse("2 + 3", &mut arena, &mut type_registry);

    assert!(result.is_ok());
    let expr = result.unwrap();
    assert_eq!(expr.ty, Type::I32);
    // Verify BinaryOp structure
}
```

### Integration Tests (TypedAST Validation)

```rust
#[test]
fn test_python_function_to_typed_ast() {
    let source = r#"
def add(x: int, y: int) -> int:
    return x + y
"#;

    let mut arena = AstArena::new();
    let mut type_registry = TypeRegistry::new();

    let program = PythonParser::parse(source, &mut arena, &mut type_registry).unwrap();

    // Validate TypedAST structure
    assert_eq!(program.declarations.len(), 1);
    match &program.declarations[0].decl {
        Declaration::FunctionDecl(func) => {
            assert_eq!(func.name.as_str(), "add");
            assert_eq!(func.params.len(), 2);
            assert_eq!(func.return_type, Type::I32);
        }
        _ => panic!("Expected FunctionDecl"),
    }
}
```

### End-to-End Tests (Full Pipeline)

```rust
#[test]
fn test_compile_and_execute_python() {
    let source = r#"
def factorial(n: int) -> int:
    if n <= 1:
        return 1
    return n * factorial(n - 1)

print(factorial(5))
"#;

    let mut arena = AstArena::new();
    let mut type_registry = TypeRegistry::new();

    // Parse to TypedAST
    let program = PythonParser::parse(source, &mut arena, &mut type_registry).unwrap();

    // Lower to HIR
    let hir = lower_to_hir(&program, &type_registry).unwrap();

    // Compile with Cranelift
    let mut backend = CraneliftBackend::new().unwrap();
    backend.compile_module(&hir).unwrap();

    // Execute
    let output = capture_stdout(|| {
        backend.execute_main().unwrap();
    });

    assert_eq!(output, "120\n");
}
```

### Conformance Tests (Language Spec)

```rust
// tests/conformance/python/
// - literals.py
// - operators.py
// - control_flow.py
// - functions.py
// - classes.py
// - etc.

#[test]
fn test_python_conformance() {
    let test_dir = "tests/conformance/python/";

    for entry in fs::read_dir(test_dir).unwrap() {
        let path = entry.unwrap().path();
        if path.extension() == Some("py") {
            println!("Testing: {}", path.display());

            let source = fs::read_to_string(&path).unwrap();
            let expected = read_expected_output(&path);

            let output = compile_and_run_python(&source).unwrap();
            assert_eq!(output, expected, "Failed: {}", path.display());
        }
    }
}
```

---

## 📈 Performance Targets

### Parse Speed

| Source Size | Target Parse Time | Target Memory |
|-------------|-------------------|---------------|
| 100 LOC | <10ms | <1 MB |
| 1K LOC | <50ms | <10 MB |
| 10K LOC | <500ms | <100 MB |
| 100K LOC | <5s | <1 GB |

### Compilation Pipeline

```
Source Code (10K LOC Python)
    ↓ <500ms
TypedAST
    ↓ <1s
HIR
    ↓ <2s (Tier 1 Cranelift)
Native Code
    ↓ <10ms
Execution
```

**Total**: <4s for 10K LOC (cold start)

### Optimization Goals

- **Incremental parsing** - Only reparse changed functions
- **Parallel parsing** - Parse multiple files concurrently
- **Lazy TypedAST** - Generate AST on-demand
- **Caching** - Cache parsed ASTs between compilations

---

## 🚧 Known Challenges & Mitigations

### Challenge 1: Type Inference Complexity

**Problem**: Languages like Python have complex type inference (duck typing, union types, etc.)

**Mitigation**:
- Start with explicit type hints only
- Gradually add inference passes
- Use Zyntax's gradual type checker
- Document unsupported features

### Challenge 2: Error Recovery

**Problem**: PEG parsers traditionally don't have good error recovery

**Mitigation**:
- Add recovery rules in grammar
- Use pest's error reporting
- Implement custom error messages
- Add "did you mean?" suggestions

### Challenge 3: Performance at Scale

**Problem**: Large files may parse slowly

**Mitigation**:
- Profile and optimize hot paths
- Implement incremental parsing
- Add parallel parsing for multiple files
- Cache parse results

### Challenge 4: Grammar Maintenance

**Problem**: Language specs evolve, grammars need updates

**Mitigation**:
- Comprehensive test suites
- Conformance testing against language specs
- Version grammar files
- Document language coverage

---

## 📚 Documentation Plan

### User Documentation

1. **ZynPEG Quick Start**
   - Installing ZynPEG
   - Writing your first grammar
   - Generating a parser
   - Compiling through Zyntax

2. **Grammar Writing Guide**
   - PEG syntax reference
   - Action block syntax
   - Type helpers
   - Error handling
   - Best practices

3. **Language Implementation Guide**
   - Mapping language semantics to TypedAST
   - Type system integration
   - Standard library bindings
   - Optimization tips

4. **API Reference**
   - Generated parser API
   - TypedAST builder API
   - Type helpers API
   - Error types

### Developer Documentation

1. **ZynPEG Architecture**
   - Code organization
   - Parser generation pipeline
   - Action compilation
   - Type checking integration

2. **Contributing Guide**
   - Setting up development environment
   - Running tests
   - Adding new features
   - Submitting PRs

3. **Language Porting Guide**
   - Analyzing target language
   - Writing grammar incrementally
   - Testing strategy
   - Common pitfalls

---

## 🎯 Success Criteria

### Phase 1 (POC)
- ✅ Calculator language parses and executes
- ✅ TypedAST correctly generated
- ✅ 100% test pass rate
- ✅ Documentation complete

### Phase 2 (Full Language)
- ✅ Python subset >90% coverage
- ✅ 1000+ test cases passing
- ✅ Real programs compile and run
- ✅ Performance targets met

### Phase 3 (ZynPEG)
- ✅ .zyn grammars generate correct parsers
- ✅ No manual AST building needed
- ✅ Type errors in grammar compilation
- ✅ Excellent error messages

### Phase 4 (Ecosystem)
- ✅ 3+ languages supported
- ✅ Community contributions
- ✅ Production use cases
- ✅ Published documentation

---

## 📅 Timeline Summary

```
Month 1     [========] POC with pest
Month 2-3   [================] Python grammar
Month 4-6   [========================] Fork pest → ZynPEG
Month 7+    [================================] Multi-language ecosystem
```

**Estimated Total**: 6-9 months for production-ready ZynPEG

---

## 🤝 Next Steps

### Immediate Actions (This Week)

1. [ ] Create `crates/zyn_parser` directory
2. [ ] Add pest dependencies to Cargo.toml
3. [ ] Write calculator.pest grammar
4. [ ] Implement basic TypedAST builder
5. [ ] Add first end-to-end test
6. [ ] Document integration pattern

### Short-term (This Month)

1. [ ] Complete calculator implementation
2. [ ] Write 100+ calculator tests
3. [ ] Document pest integration
4. [ ] Design .zyn action syntax
5. [ ] Create grammar writing guide
6. [ ] Present POC demo

### Medium-term (Next Quarter)

1. [ ] Python grammar implementation
2. [ ] Fork pest repository
3. [ ] Prototype .zyn syntax
4. [ ] Type system integration
5. [ ] Performance optimization
6. [ ] Community feedback

---

## 📞 Questions & Feedback

**Open Questions**:
1. Which language should we target first for Phase 2?
2. Should .zyn support multiple output formats (not just TypedAST)?
3. How should we handle language-specific standard libraries?
4. What's the strategy for versioning .zyn grammars?

**Feedback Welcome**:
- Grammar syntax proposals
- Use case requirements
- Performance considerations
- Ecosystem integration ideas

---

**Document Status**: Draft
**Last Updated**: 2025-11-15
**Owner**: Zyntax Core Team
**Review Date**: TBD

---

**Next**: Start Phase 1 POC implementation →
