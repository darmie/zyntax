//! ZynPEG Runtime - JSON Command Pattern for TypedAST Construction
//!
//! This module provides a runtime interpreter for ZynPEG grammars that doesn't
//! require Rust compilation. Instead of generating Rust code, it:
//!
//! 1. Compiles .zyn grammar to .zpeg format (pest grammar + JSON command mappings)
//! 2. At runtime, parses source with pest and executes JSON commands via host functions
//! 3. Uses the TypedASTBuilder fluent API from zyntax_typed_ast to construct TypedProgram
//!
//! ## Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────┐
//! │                    Compile Time                             │
//! ├─────────────────────────────────────────────────────────────┤
//! │  .zyn grammar  →  ZynPEG Compiler  →  .zpeg bytecode        │
//! └─────────────────────────────────────────────────────────────┘
//!
//! ┌─────────────────────────────────────────────────────────────┐
//! │                    Runtime                                  │
//! ├─────────────────────────────────────────────────────────────┤
//! │  source.lang + .zpeg  →  Runtime Interpreter  →  TypedAST  │
//! │                                                             │
//! │  Uses TypedASTBuilder fluent API to construct TypedProgram  │
//! └─────────────────────────────────────────────────────────────┘
//! ```
//!
//! ## Usage
//!
//! ```ignore
//! // Compile grammar to zpeg format
//! let zpeg = ZpegCompiler::compile(&zyn_grammar)?;
//! zpeg.save("my_lang.zpeg")?;
//!
//! // At runtime, load and execute
//! let zpeg = ZpegModule::load("my_lang.zpeg")?;
//! let typed_ast = zpeg.parse_source(&source_code)?;
//! ```

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;

use crate::error::{Result, ZynPegError};
use crate::ZynGrammar;

// Re-export types from typed_ast for host function implementations
pub use zyntax_typed_ast::{
    TypedASTBuilder, TypedProgram, TypedNode, TypedDeclaration, TypedExpression,
    TypedStatement, TypedBlock, BinaryOp, UnaryOp, Span,
    type_registry::{Type, PrimitiveType, Mutability, Visibility},
};

// ============================================================================
// ZPEG Module Format
// ============================================================================

/// Compiled ZynPEG module - contains pest grammar and AST builder commands
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ZpegModule {
    /// Module metadata
    pub metadata: ZpegMetadata,
    /// The pest grammar string (embedded)
    pub pest_grammar: String,
    /// Rule definitions with their AST builder commands
    pub rules: HashMap<String, RuleCommands>,
}

/// Module metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ZpegMetadata {
    /// Language name from @language directive
    pub name: String,
    /// Language version
    pub version: String,
    /// File extensions this grammar handles
    pub file_extensions: Vec<String>,
    /// Entry point function name (declared by the grammar)
    #[serde(default)]
    pub entry_point: Option<String>,
    /// ZynPEG version used to compile
    pub zpeg_version: String,
}

/// Commands for a single grammar rule
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RuleCommands {
    /// The TypedAST type this rule produces (e.g., "TypedExpression", "TypedStatement")
    pub return_type: Option<String>,
    /// Sequence of commands to build the AST node
    pub commands: Vec<AstCommand>,
}

/// Named arguments for define commands (object-based)
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct NamedArgs(pub HashMap<String, CommandArg>);

/// A single AST construction command
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum AstCommand {
    /// Define an AST node using a host function (preferred term over "call")
    /// e.g., {"type": "define", "node": "int_literal", "args": {"value": "$result"}}
    Define { node: String, args: NamedArgs },
    /// Legacy: Call a host function to create an AST node (deprecated, use Define)
    /// e.g., {"type": "call", "func": "binary_op", "args": ["$op", "$1", "$3"]}
    Call { func: String, args: Vec<CommandArg> },
    /// Get a child node by index (0-based) or name
    /// e.g., {"type": "get_child", "index": 0}
    GetChild {
        #[serde(default)]
        index: Option<usize>,
        #[serde(default)]
        name: Option<String>,
    },
    /// Get all children as a list (for rule* patterns)
    /// e.g., {"type": "get_all_children"}
    GetAllChildren,
    /// Get the text content of current node
    /// e.g., {"type": "get_text"}
    GetText,
    /// Parse text as integer
    /// e.g., {"type": "parse_int"}
    ParseInt,
    /// Parse text as float
    /// e.g., {"type": "parse_float"}
    ParseFloat,
    /// Create a span from current node
    /// e.g., {"type": "span"}
    Span,
    /// Fold binary operations (for left-associative parsing)
    /// e.g., {"type": "fold_binary", "operand_rule": "term", "operator_rule": "addop"}
    FoldBinary {
        operand_rule: String,
        operator_rule: String,
    },
    /// Iterate over all children matching a rule
    /// e.g., {"type": "map_children", "rule": "statement", "commands": [...]}
    MapChildren {
        rule: String,
        commands: Vec<AstCommand>,
    },
    /// Conditional command based on rule match
    /// e.g., {"type": "match_rule", "cases": {"number": [...], "ident": [...]}}
    MatchRule {
        cases: HashMap<String, Vec<AstCommand>>,
    },
    /// Store result in a named variable
    /// e.g., {"type": "store", "name": "left"}
    Store { name: String },
    /// Load from a named variable
    /// e.g., {"type": "load", "name": "left"}
    Load { name: String },
    /// Return the current value as the rule's result
    /// e.g., {"type": "return"}
    Return,
}

/// Argument to a command - can be a literal, reference, list, or nested command
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum CommandArg {
    /// Reference to a child by index: "$1", "$2", etc. (1-based for compatibility)
    ChildRef(String),
    /// String literal
    StringLit(String),
    /// Integer literal
    IntLit(i64),
    /// Boolean literal
    BoolLit(bool),
    /// List of arguments
    List(Vec<CommandArg>),
    /// Nested command that produces a value
    Nested(Box<AstCommand>),
}

// ============================================================================
// Host Functions Interface
// ============================================================================

/// Handle to an AST node being constructed
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct NodeHandle(pub u32);

/// Value that can be passed between commands
#[derive(Debug, Clone)]
pub enum RuntimeValue {
    /// Handle to an AST node
    Node(NodeHandle),
    /// String value
    String(String),
    /// Integer value
    Int(i64),
    /// Float value
    Float(f64),
    /// Boolean value
    Bool(bool),
    /// Span value
    Span { start: usize, end: usize },
    /// List of values
    List(Vec<RuntimeValue>),
    /// Null/None value
    Null,
}

/// Host functions that the runtime calls to construct TypedAST
pub trait AstHostFunctions {
    // ========== Program Structure ==========

    /// Create a new program node
    fn create_program(&mut self) -> NodeHandle;

    /// Add a declaration to a program
    fn program_add_decl(&mut self, program: NodeHandle, decl: NodeHandle);

    /// Finalize program and return serialized TypedAST JSON
    fn finalize_program(&mut self, program: NodeHandle) -> String;

    // ========== Functions ==========

    /// Create a function declaration
    fn create_function(
        &mut self,
        name: &str,
        params: Vec<NodeHandle>,
        return_type: NodeHandle,
        body: NodeHandle,
    ) -> NodeHandle;

    /// Create a function parameter
    fn create_param(&mut self, name: &str, ty: NodeHandle) -> NodeHandle;

    // ========== Expressions ==========

    /// Create a binary operation expression
    fn create_binary_op(&mut self, op: &str, left: NodeHandle, right: NodeHandle) -> NodeHandle;

    /// Create a unary operation expression
    fn create_unary_op(&mut self, op: &str, operand: NodeHandle) -> NodeHandle;

    /// Create an integer literal
    fn create_int_literal(&mut self, value: i64) -> NodeHandle;

    /// Create a float literal
    fn create_float_literal(&mut self, value: f64) -> NodeHandle;

    /// Create a string literal
    fn create_string_literal(&mut self, value: &str) -> NodeHandle;

    /// Create a boolean literal
    fn create_bool_literal(&mut self, value: bool) -> NodeHandle;

    /// Create an identifier expression
    fn create_identifier(&mut self, name: &str) -> NodeHandle;

    /// Create a character literal
    fn create_char_literal(&mut self, value: char) -> NodeHandle;

    /// Create a variable reference expression
    fn create_variable(&mut self, name: &str) -> NodeHandle;

    /// Create a function call expression
    fn create_call(&mut self, callee: NodeHandle, args: Vec<NodeHandle>) -> NodeHandle;

    /// Create a method call expression
    fn create_method_call(&mut self, receiver: NodeHandle, method: &str, args: Vec<NodeHandle>) -> NodeHandle;

    /// Create an array/index access expression
    fn create_index(&mut self, array: NodeHandle, index: NodeHandle) -> NodeHandle;

    /// Create a field access expression
    fn create_field_access(&mut self, object: NodeHandle, field: &str) -> NodeHandle;

    /// Create an array literal expression
    fn create_array(&mut self, elements: Vec<NodeHandle>) -> NodeHandle;

    /// Create a struct literal expression
    fn create_struct_literal(&mut self, name: &str, fields: Vec<(String, NodeHandle)>) -> NodeHandle;

    /// Create a cast expression
    fn create_cast(&mut self, expr: NodeHandle, target_type: NodeHandle) -> NodeHandle;

    /// Create a lambda expression
    fn create_lambda(&mut self, params: Vec<NodeHandle>, body: NodeHandle) -> NodeHandle;

    // ========== Statements ==========

    /// Create a variable declaration statement
    fn create_var_decl(
        &mut self,
        name: &str,
        ty: Option<NodeHandle>,
        init: Option<NodeHandle>,
        is_const: bool,
    ) -> NodeHandle;

    /// Create an assignment statement
    fn create_assignment(&mut self, target: NodeHandle, value: NodeHandle) -> NodeHandle;

    /// Create a return statement
    fn create_return(&mut self, value: Option<NodeHandle>) -> NodeHandle;

    /// Create an if statement
    fn create_if(
        &mut self,
        condition: NodeHandle,
        then_branch: NodeHandle,
        else_branch: Option<NodeHandle>,
    ) -> NodeHandle;

    /// Create a while loop
    fn create_while(&mut self, condition: NodeHandle, body: NodeHandle) -> NodeHandle;

    /// Create a for loop
    fn create_for(&mut self, iterator: &str, iterable: NodeHandle, body: NodeHandle) -> NodeHandle;

    /// Create a block statement
    fn create_block(&mut self, statements: Vec<NodeHandle>) -> NodeHandle;

    /// Create an expression statement
    fn create_expr_stmt(&mut self, expr: NodeHandle) -> NodeHandle;

    /// Create a let/variable declaration (alias for create_var_decl)
    fn create_let(
        &mut self,
        name: &str,
        ty: Option<NodeHandle>,
        init: Option<NodeHandle>,
        is_const: bool,
    ) -> NodeHandle;

    /// Create a break statement
    fn create_break(&mut self, value: Option<NodeHandle>) -> NodeHandle;

    /// Create a continue statement
    fn create_continue(&mut self) -> NodeHandle;

    /// Create an expression statement (alias)
    fn create_expression_stmt(&mut self, expr: NodeHandle) -> NodeHandle;

    // ========== Types ==========

    /// Create a primitive type (i32, i64, f32, f64, bool, etc.)
    fn create_primitive_type(&mut self, name: &str) -> NodeHandle;

    /// Create a pointer type
    fn create_pointer_type(&mut self, pointee: NodeHandle) -> NodeHandle;

    /// Create an array type
    fn create_array_type(&mut self, element: NodeHandle, size: Option<usize>) -> NodeHandle;

    /// Create a function type
    fn create_function_type(
        &mut self,
        params: Vec<NodeHandle>,
        return_type: NodeHandle,
    ) -> NodeHandle;

    /// Create a named/user type
    fn create_named_type(&mut self, name: &str) -> NodeHandle;

    // ========== Struct/Enum Declarations ==========

    /// Create a struct declaration
    fn create_struct(&mut self, name: &str, fields: Vec<NodeHandle>) -> NodeHandle;

    /// Create an enum declaration
    fn create_enum(&mut self, name: &str, variants: Vec<NodeHandle>) -> NodeHandle;

    /// Create a struct field
    fn create_field(&mut self, name: &str, ty: NodeHandle) -> NodeHandle;

    /// Create an enum variant
    fn create_variant(&mut self, name: &str) -> NodeHandle;

    // ========== Span/Location ==========

    /// Set span on a node
    fn set_span(&mut self, node: NodeHandle, start: usize, end: usize);
}

// ============================================================================
// ZPEG Compiler - Converts ZynGrammar to ZpegModule
// ============================================================================

/// Compiler that converts ZynGrammar to ZpegModule
pub struct ZpegCompiler;

impl ZpegCompiler {
    /// Compile a ZynGrammar to a ZpegModule
    pub fn compile(grammar: &ZynGrammar) -> Result<ZpegModule> {
        let metadata = ZpegMetadata {
            name: grammar.language.name.clone(),
            version: grammar.language.version.clone(),
            file_extensions: grammar.language.file_extensions.clone(),
            entry_point: grammar.language.entry_point.clone(),
            zpeg_version: env!("CARGO_PKG_VERSION").to_string(),
        };

        // Generate pest grammar
        let pest_grammar = crate::generator::generate_pest_grammar_string(grammar)?;

        // Convert rules to commands
        let mut rules = HashMap::new();
        for rule in &grammar.rules {
            let commands = Self::compile_rule(rule)?;
            rules.insert(rule.name.clone(), commands);
        }

        Ok(ZpegModule {
            metadata,
            pest_grammar,
            rules,
        })
    }

    /// Compile a single rule's action block to commands
    fn compile_rule(rule: &crate::RuleDef) -> Result<RuleCommands> {
        let return_type = rule.action.as_ref().map(|a| a.return_type.clone());

        let commands = if let Some(action) = &rule.action {
            Self::compile_action(action)?
        } else {
            // No action block - just pass through child
            vec![AstCommand::GetChild {
                index: Some(0),
                name: None,
            }]
        };

        Ok(RuleCommands {
            return_type,
            commands,
        })
    }

    /// Compile an action block to commands
    fn compile_action(action: &crate::ActionBlock) -> Result<Vec<AstCommand>> {
        let mut commands = Vec::new();

        // Prefer JSON commands if available (new format)
        if let Some(json_str) = &action.json_commands {
            commands.extend(Self::parse_json_commands(json_str)?);
        }
        // Otherwise, if there's raw code, parse it into commands (legacy)
        else if let Some(raw) = &action.raw_code {
            commands.extend(Self::parse_raw_action(raw)?);
        } else {
            // Structured fields - generate commands for each
            for field in &action.fields {
                commands.push(Self::compile_field_value(&field.name, &field.value)?);
            }
        }

        // Add return at the end
        commands.push(AstCommand::Return);

        Ok(commands)
    }

    /// Parse JSON commands from action block
    fn parse_json_commands(json_str: &str) -> Result<Vec<AstCommand>> {
        // Parse the JSON - can be an array of commands or a single object
        let value: serde_json::Value = serde_json::from_str(json_str)
            .map_err(|e| ZynPegError::ParseError(format!("Invalid JSON in action block: {}", e)))?;

        let mut commands = Vec::new();

        // Support array of command objects
        if let serde_json::Value::Array(arr) = value {
            for item in arr {
                if let serde_json::Value::Object(map) = item {
                    commands.extend(Self::parse_json_object_commands(&map)?);
                }
            }
            return Ok(commands);
        }

        // Single object (original format or "commands" wrapper)
        if let serde_json::Value::Object(map) = value {
            // Check for "commands": [...] wrapper format
            if let Some(serde_json::Value::Array(arr)) = map.get("commands") {
                for item in arr {
                    if let serde_json::Value::Object(cmd_map) = item {
                        commands.extend(Self::parse_json_object_commands(cmd_map)?);
                    }
                }
            } else {
                commands.extend(Self::parse_json_object_commands(&map)?);
            }
        }

        Ok(commands)
    }

    /// Parse commands from a single JSON object
    fn parse_json_object_commands(map: &serde_json::Map<String, serde_json::Value>) -> Result<Vec<AstCommand>> {
        let mut commands = Vec::new();

        if true {  // Scope for borrowing map
            // Check for different command types

            // "get_child": { "index": 0 }
            if let Some(val) = map.get("get_child") {
                let index = val.get("index").and_then(|v| v.as_u64()).map(|n| n as usize);
                let name = val.get("name").and_then(|v| v.as_str()).map(|s| s.to_string());
                commands.push(AstCommand::GetChild { index, name });
            }

            // "get_all_children": true - collect all children as a list
            // Can also include "store": "var_name" to store the result
            if map.get("get_all_children").is_some() {
                commands.push(AstCommand::GetAllChildren);
                // If "store" field exists, add a Store command after get_all_children
                if let Some(store_name) = map.get("store").and_then(|v| v.as_str()) {
                    commands.push(AstCommand::Store { name: store_name.to_string() });
                }
            }

            // "get_text": true
            if map.get("get_text").is_some() {
                commands.push(AstCommand::GetText);
            }

            // "parse_int": true
            if map.get("parse_int").is_some() {
                commands.push(AstCommand::ParseInt);
            }

            // "parse_float": true
            if map.get("parse_float").is_some() {
                commands.push(AstCommand::ParseFloat);
            }

            // "define": "node_type", "args": {...}, "store": "var_name" (optional)
            // This is the preferred new format with named arguments
            if let Some(node_type) = map.get("define").and_then(|v| v.as_str()) {
                let args = if let Some(serde_json::Value::Object(obj)) = map.get("args") {
                    let mut named_args = HashMap::new();
                    for (key, val) in obj {
                        named_args.insert(key.clone(), Self::json_to_command_arg(val));
                    }
                    NamedArgs(named_args)
                } else {
                    NamedArgs::default()
                };
                commands.push(AstCommand::Define {
                    node: node_type.to_string(),
                    args,
                });
                // If "store" field exists, add a Store command after the define
                if let Some(store_name) = map.get("store").and_then(|v| v.as_str()) {
                    commands.push(AstCommand::Store { name: store_name.to_string() });
                }
            }

            // "call": "func_name", "args": [...], "store": "var_name" (optional)
            // Legacy format with positional arguments (deprecated but still supported)
            if let Some(func) = map.get("call").and_then(|v| v.as_str()) {
                let args = if let Some(serde_json::Value::Array(arr)) = map.get("args") {
                    arr.iter().map(|v| Self::json_to_command_arg(v)).collect()
                } else {
                    vec![]
                };
                commands.push(AstCommand::Call {
                    func: func.to_string(),
                    args,
                });
                // If "store" field exists, add a Store command after the call
                if let Some(store_name) = map.get("store").and_then(|v| v.as_str()) {
                    commands.push(AstCommand::Store { name: store_name.to_string() });
                }
            }

            // "fold_binary": { "operand": "term", "operator_map": {...} }
            if let Some(val) = map.get("fold_binary") {
                let operand_rule = val.get("operand")
                    .or(val.get("operand_rule"))
                    .and_then(|v| v.as_str())
                    .unwrap_or("operand")
                    .to_string();
                let operator_rule = val.get("operator")
                    .or(val.get("operator_rule"))
                    .and_then(|v| v.as_str())
                    .unwrap_or("operator")
                    .to_string();
                commands.push(AstCommand::FoldBinary { operand_rule, operator_rule });
            }

            // "match_rule": { "rule_name": [...], ... }
            if let Some(serde_json::Value::Object(cases_map)) = map.get("match_rule") {
                let mut cases = std::collections::HashMap::new();
                for (rule_name, case_cmds) in cases_map {
                    if let serde_json::Value::Array(arr) = case_cmds {
                        let cmds: Vec<AstCommand> = arr.iter()
                            .filter_map(|v| Self::json_value_to_command(v).ok())
                            .collect();
                        cases.insert(rule_name.clone(), cmds);
                    }
                }
                commands.push(AstCommand::MatchRule { cases });
            }

            // "store": { "name": "var_name" }
            if let Some(val) = map.get("store") {
                if let Some(name) = val.get("name").and_then(|v| v.as_str()) {
                    commands.push(AstCommand::Store { name: name.to_string() });
                }
            }

            // "load": { "name": "var_name" }
            if let Some(val) = map.get("load") {
                if let Some(name) = val.get("name").and_then(|v| v.as_str()) {
                    commands.push(AstCommand::Load { name: name.to_string() });
                }
            }
        }

        Ok(commands)
    }

    /// Convert a JSON value to a command argument
    fn json_to_command_arg(value: &serde_json::Value) -> CommandArg {
        match value {
            serde_json::Value::String(s) => {
                if s.starts_with('$') {
                    CommandArg::ChildRef(s.clone())
                } else {
                    CommandArg::StringLit(s.clone())
                }
            }
            serde_json::Value::Number(n) => {
                if let Some(i) = n.as_i64() {
                    CommandArg::IntLit(i)
                } else {
                    CommandArg::StringLit(n.to_string())
                }
            }
            serde_json::Value::Bool(b) => CommandArg::BoolLit(*b),
            serde_json::Value::Array(arr) => {
                // Convert array to a list of command args
                CommandArg::List(arr.iter().map(Self::json_to_command_arg).collect())
            }
            _ => CommandArg::StringLit(value.to_string()),
        }
    }

    /// Convert a JSON value to an AstCommand
    fn json_value_to_command(value: &serde_json::Value) -> Result<AstCommand> {
        if let serde_json::Value::Object(map) = value {
            if let Some(val) = map.get("get_child") {
                let index = val.get("index").and_then(|v| v.as_u64()).map(|n| n as usize);
                let name = val.get("name").and_then(|v| v.as_str()).map(|s| s.to_string());
                return Ok(AstCommand::GetChild { index, name });
            }
            if map.get("get_text").is_some() {
                return Ok(AstCommand::GetText);
            }
            if map.get("parse_int").is_some() {
                return Ok(AstCommand::ParseInt);
            }
            if let Some(func) = map.get("call").and_then(|v| v.as_str()) {
                let args = if let Some(serde_json::Value::Array(arr)) = map.get("args") {
                    arr.iter().map(|v| Self::json_to_command_arg(v)).collect()
                } else {
                    vec![]
                };
                return Ok(AstCommand::Call { func: func.to_string(), args });
            }
        }
        Err(ZynPegError::ParseError("Invalid JSON command".into()))
    }

    /// Parse raw action code into commands
    fn parse_raw_action(code: &str) -> Result<Vec<AstCommand>> {
        // For now, simple pattern matching on common patterns
        // This can be expanded to handle more complex expressions

        let code = code.trim();
        let mut commands = Vec::new();

        // Pattern: IntLiteral($1.parse())
        if code.contains("IntLiteral") && code.contains(".parse()") {
            commands.push(AstCommand::GetChild {
                index: Some(0),
                name: None,
            });
            commands.push(AstCommand::GetText);
            commands.push(AstCommand::ParseInt);
            commands.push(AstCommand::Call {
                func: "int_literal".to_string(),
                args: vec![CommandArg::ChildRef("$result".to_string())],
            });
        }
        // Pattern: BinaryOp($op, $1, $3) or similar
        else if code.contains("BinaryOp") {
            commands.push(AstCommand::FoldBinary {
                operand_rule: "term".to_string(),
                operator_rule: "operator".to_string(),
            });
        }
        // Default: pass through
        else {
            commands.push(AstCommand::GetChild {
                index: Some(0),
                name: None,
            });
        }

        Ok(commands)
    }

    /// Compile a field value expression to a command
    fn compile_field_value(field_name: &str, value: &str) -> Result<AstCommand> {
        // Parse the value expression and generate appropriate command
        let value = value.trim();

        // $N reference
        if value.starts_with('$') && value.len() > 1 {
            if let Ok(idx) = value[1..].parse::<usize>() {
                return Ok(AstCommand::GetChild {
                    index: Some(idx - 1),
                    name: None,
                });
            }
        }

        // Function call pattern: func(args)
        if let Some(paren_pos) = value.find('(') {
            let func_name = &value[..paren_pos];
            let args_str = &value[paren_pos + 1..value.len() - 1];
            let args = Self::parse_args(args_str)?;

            return Ok(AstCommand::Call {
                func: func_name.to_string(),
                args,
            });
        }

        // Default: treat as identifier/constant
        Ok(AstCommand::Call {
            func: "constant".to_string(),
            args: vec![CommandArg::StringLit(value.to_string())],
        })
    }

    /// Parse function arguments
    fn parse_args(args_str: &str) -> Result<Vec<CommandArg>> {
        let mut args = Vec::new();

        for arg in args_str.split(',') {
            let arg = arg.trim();
            if arg.is_empty() {
                continue;
            }

            // $N reference
            if arg.starts_with('$') {
                args.push(CommandArg::ChildRef(arg.to_string()));
            }
            // Quoted string
            else if arg.starts_with('"') && arg.ends_with('"') {
                args.push(CommandArg::StringLit(arg[1..arg.len() - 1].to_string()));
            }
            // Number
            else if let Ok(n) = arg.parse::<i64>() {
                args.push(CommandArg::IntLit(n));
            }
            // Boolean
            else if arg == "true" {
                args.push(CommandArg::BoolLit(true));
            } else if arg == "false" {
                args.push(CommandArg::BoolLit(false));
            }
            // Identifier/other
            else {
                args.push(CommandArg::StringLit(arg.to_string()));
            }
        }

        Ok(args)
    }
}

impl ZpegModule {
    /// Save module to a file
    pub fn save(&self, path: impl AsRef<Path>) -> Result<()> {
        let json = serde_json::to_string_pretty(self)
            .map_err(|e| ZynPegError::CodeGenError(format!("Failed to serialize zpeg: {}", e)))?;
        std::fs::write(path, json)?;
        Ok(())
    }

    /// Load module from a file
    pub fn load(path: impl AsRef<Path>) -> Result<Self> {
        let json = std::fs::read_to_string(path)?;
        let module: ZpegModule = serde_json::from_str(&json)
            .map_err(|e| ZynPegError::ParseError(format!("Failed to parse zpeg: {}", e)))?;
        Ok(module)
    }

    /// Get the pest grammar for runtime parsing
    pub fn pest_grammar(&self) -> &str {
        &self.pest_grammar
    }

    /// Get commands for a rule
    pub fn rule_commands(&self, rule_name: &str) -> Option<&RuleCommands> {
        self.rules.get(rule_name)
    }
}

// ============================================================================
// Default TypedAST Host Functions Implementation
// ============================================================================

/// Default implementation of AstHostFunctions that uses the TypedASTBuilder
/// fluent API from zyntax_typed_ast to build TypedProgram directly.
///
/// This approach:
/// 1. Uses NodeHandle as a reference to stored TypedNode<TypedExpression> etc.
/// 2. Builds actual typed AST nodes using the fluent API
/// 3. Serializes to JSON for pipeline integration (can also return TypedProgram directly)
pub struct TypedAstBuilder {
    /// The underlying fluent builder from zyntax_typed_ast
    inner: TypedASTBuilder,
    /// Next handle ID
    next_id: u32,
    /// Stored expression nodes by handle
    expressions: HashMap<NodeHandle, TypedNode<TypedExpression>>,
    /// Stored statement nodes by handle
    statements: HashMap<NodeHandle, TypedNode<TypedStatement>>,
    /// Stored block nodes by handle
    blocks: HashMap<NodeHandle, TypedBlock>,
    /// Stored declaration nodes by handle
    declarations: HashMap<NodeHandle, TypedNode<TypedDeclaration>>,
    /// Program declaration handles (in order)
    program_decls: Vec<NodeHandle>,
}

impl Default for TypedAstBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl TypedAstBuilder {
    pub fn new() -> Self {
        Self {
            inner: TypedASTBuilder::new(),
            next_id: 0,
            expressions: HashMap::new(),
            statements: HashMap::new(),
            blocks: HashMap::new(),
            declarations: HashMap::new(),
            program_decls: Vec::new(),
        }
    }

    /// Allocate a new handle ID
    fn alloc_handle(&mut self) -> NodeHandle {
        let handle = NodeHandle(self.next_id);
        self.next_id += 1;
        handle
    }

    /// Store an expression and return its handle
    fn store_expr(&mut self, expr: TypedNode<TypedExpression>) -> NodeHandle {
        let handle = self.alloc_handle();
        self.expressions.insert(handle, expr);
        handle
    }

    /// Store a statement and return its handle
    fn store_stmt(&mut self, stmt: TypedNode<TypedStatement>) -> NodeHandle {
        let handle = self.alloc_handle();
        self.statements.insert(handle, stmt);
        handle
    }

    /// Store a declaration and return its handle
    fn store_decl(&mut self, decl: TypedNode<TypedDeclaration>) -> NodeHandle {
        let handle = self.alloc_handle();
        self.declarations.insert(handle, decl);
        handle
    }

    /// Store a block and return its handle
    fn store_block(&mut self, block: TypedBlock) -> NodeHandle {
        let handle = self.alloc_handle();
        self.blocks.insert(handle, block);
        handle
    }

    /// Get an expression by handle (cloning it)
    fn get_expr(&self, handle: NodeHandle) -> Option<TypedNode<TypedExpression>> {
        self.expressions.get(&handle).cloned()
    }

    /// Get a statement by handle (cloning it)
    fn get_stmt(&self, handle: NodeHandle) -> Option<TypedNode<TypedStatement>> {
        self.statements.get(&handle).cloned()
    }

    /// Get a block by handle (cloning it)
    fn get_block(&self, handle: NodeHandle) -> Option<TypedBlock> {
        self.blocks.get(&handle).cloned()
    }

    /// Get the default span for nodes
    fn default_span(&self) -> Span {
        self.inner.dummy_span()
    }

    /// Build and return the final TypedProgram
    pub fn build_program(&self) -> TypedProgram {
        let decls: Vec<TypedNode<TypedDeclaration>> = self.program_decls.iter()
            .filter_map(|h| self.declarations.get(h).cloned())
            .collect();

        TypedProgram {
            declarations: decls,
            span: self.default_span(),
        }
    }

    /// Convert string operator to BinaryOp enum
    fn string_to_binary_op(op: &str) -> BinaryOp {
        match op.to_lowercase().as_str() {
            "add" | "+" => BinaryOp::Add,
            "sub" | "-" => BinaryOp::Sub,
            "mul" | "*" => BinaryOp::Mul,
            "div" | "/" => BinaryOp::Div,
            "mod" | "%" | "rem" => BinaryOp::Rem,
            "eq" | "==" => BinaryOp::Eq,
            "ne" | "!=" => BinaryOp::Ne,
            "lt" | "<" => BinaryOp::Lt,
            "le" | "<=" => BinaryOp::Le,
            "gt" | ">" => BinaryOp::Gt,
            "ge" | ">=" => BinaryOp::Ge,
            "and" | "&&" => BinaryOp::And,
            "or" | "||" => BinaryOp::Or,
            "bitand" | "&" => BinaryOp::BitAnd,
            "bitor" | "|" => BinaryOp::BitOr,
            "bitxor" | "^" => BinaryOp::BitXor,
            "shl" | "<<" => BinaryOp::Shl,
            "shr" | ">>" => BinaryOp::Shr,
            _ => BinaryOp::Add, // Default fallback
        }
    }

    /// Convert string operator to UnaryOp enum
    fn string_to_unary_op(op: &str) -> UnaryOp {
        match op.to_lowercase().as_str() {
            "neg" | "-" | "minus" => UnaryOp::Minus,
            "pos" | "+" | "plus" => UnaryOp::Plus,
            "not" | "!" => UnaryOp::Not,
            "bitnot" | "~" => UnaryOp::BitNot,
            _ => UnaryOp::Minus, // Default fallback
        }
    }
}

impl AstHostFunctions for TypedAstBuilder {
    fn create_program(&mut self) -> NodeHandle {
        // Just return a handle - program tracks declarations separately
        self.alloc_handle()
    }

    fn program_add_decl(&mut self, _program: NodeHandle, decl: NodeHandle) {
        // Only add actual declarations - grammar is responsible for defining entry points
        if self.declarations.contains_key(&decl) {
            self.program_decls.push(decl);
        }
        // Note: If the grammar passes an expression or statement handle here,
        // it's a grammar error - the grammar should use wrap_main or similar
        // to create proper function declarations with entry points
    }

    fn finalize_program(&mut self, _program: NodeHandle) -> String {
        // Build the TypedProgram and serialize to JSON
        let typed_program = self.build_program();
        serde_json::to_string(&typed_program)
            .unwrap_or_else(|e| format!(r#"{{"declarations": [], "error": "{}"}}"#, e))
    }

    fn create_function(
        &mut self,
        name: &str,
        params: Vec<NodeHandle>,
        _return_type: NodeHandle,
        body: NodeHandle,
    ) -> NodeHandle {
        let span = self.default_span();

        // Convert param handles to TypedParameter
        let typed_params: Vec<_> = params.iter()
            .map(|_h| {
                // For now, create simple parameters
                self.inner.parameter("arg", Type::Primitive(PrimitiveType::I32), Mutability::Immutable, span)
            })
            .collect();

        // Get body block - first try as a block, then as a single statement
        let body_block = if let Some(block) = self.get_block(body) {
            block
        } else if let Some(stmt) = self.get_stmt(body) {
            TypedBlock { statements: vec![stmt], span }
        } else if let Some(expr) = self.get_expr(body) {
            TypedBlock { statements: vec![self.inner.expression_statement(expr, span)], span }
        } else {
            TypedBlock { statements: vec![], span }
        };

        let func = self.inner.function(
            name,
            typed_params,
            Type::Primitive(PrimitiveType::I32),
            body_block,
            Visibility::Public,
            false,
            span,
        );

        self.store_decl(func)
    }

    fn create_param(&mut self, name: &str, _ty: NodeHandle) -> NodeHandle {
        // Parameters are stored differently - just return a placeholder handle
        // The actual TypedParameter is created in create_function
        let _ = name;
        self.alloc_handle()
    }

    fn create_binary_op(&mut self, op: &str, left: NodeHandle, right: NodeHandle) -> NodeHandle {
        let span = self.default_span();

        let left_expr = self.get_expr(left).unwrap_or_else(|| self.inner.int_literal(0, span));
        let right_expr = self.get_expr(right).unwrap_or_else(|| self.inner.int_literal(0, span));

        let binary_op = Self::string_to_binary_op(op);
        let result_type = Type::Primitive(PrimitiveType::I32);

        let expr = self.inner.binary(binary_op, left_expr, right_expr, result_type, span);
        self.store_expr(expr)
    }

    fn create_unary_op(&mut self, op: &str, operand: NodeHandle) -> NodeHandle {
        let span = self.default_span();

        let operand_expr = self.get_expr(operand).unwrap_or_else(|| self.inner.int_literal(0, span));

        let unary_op = Self::string_to_unary_op(op);
        let result_type = Type::Primitive(PrimitiveType::I32);

        let expr = self.inner.unary(unary_op, operand_expr, result_type, span);
        self.store_expr(expr)
    }

    fn create_int_literal(&mut self, value: i64) -> NodeHandle {
        let span = self.default_span();
        let expr = self.inner.int_literal(value as i128, span);
        self.store_expr(expr)
    }

    fn create_float_literal(&mut self, value: f64) -> NodeHandle {
        let span = self.default_span();
        // TypedASTBuilder doesn't have float_literal, use string_literal workaround for now
        // or we could add a float literal type
        let expr = self.inner.int_literal(value as i128, span);
        self.store_expr(expr)
    }

    fn create_string_literal(&mut self, value: &str) -> NodeHandle {
        let span = self.default_span();
        let expr = self.inner.string_literal(value, span);
        self.store_expr(expr)
    }

    fn create_bool_literal(&mut self, value: bool) -> NodeHandle {
        let span = self.default_span();
        let expr = self.inner.bool_literal(value, span);
        self.store_expr(expr)
    }

    fn create_identifier(&mut self, name: &str) -> NodeHandle {
        let span = self.default_span();
        let expr = self.inner.variable(name, Type::Primitive(PrimitiveType::I32), span);
        self.store_expr(expr)
    }

    fn create_call(&mut self, callee: NodeHandle, args: Vec<NodeHandle>) -> NodeHandle {
        let span = self.default_span();

        let callee_expr = self.get_expr(callee)
            .unwrap_or_else(|| self.inner.variable("unknown", Type::Primitive(PrimitiveType::I32), span));

        let arg_exprs: Vec<_> = args.iter()
            .filter_map(|h| self.get_expr(*h))
            .collect();

        let expr = self.inner.call_positional(callee_expr, arg_exprs, Type::Primitive(PrimitiveType::I32), span);
        self.store_expr(expr)
    }

    fn create_index(&mut self, array: NodeHandle, index: NodeHandle) -> NodeHandle {
        let span = self.default_span();

        let array_expr = self.get_expr(array)
            .unwrap_or_else(|| self.inner.variable("array", Type::Primitive(PrimitiveType::I32), span));
        let index_expr = self.get_expr(index)
            .unwrap_or_else(|| self.inner.int_literal(0, span));

        let expr = self.inner.index(array_expr, index_expr, Type::Primitive(PrimitiveType::I32), span);
        self.store_expr(expr)
    }

    fn create_field_access(&mut self, object: NodeHandle, field: &str) -> NodeHandle {
        let span = self.default_span();

        let object_expr = self.get_expr(object)
            .unwrap_or_else(|| self.inner.variable("object", Type::Primitive(PrimitiveType::I32), span));

        let expr = self.inner.field_access(object_expr, field, Type::Primitive(PrimitiveType::I32), span);
        self.store_expr(expr)
    }

    fn create_var_decl(
        &mut self,
        name: &str,
        _ty: Option<NodeHandle>,
        init: Option<NodeHandle>,
        is_const: bool,
    ) -> NodeHandle {
        let span = self.default_span();

        let init_expr = init.and_then(|h| self.get_expr(h));
        let mutability = if is_const { Mutability::Immutable } else { Mutability::Mutable };

        let stmt = self.inner.let_statement(
            name,
            Type::Primitive(PrimitiveType::I32),
            mutability,
            init_expr,
            span,
        );
        self.store_stmt(stmt)
    }

    fn create_assignment(&mut self, target: NodeHandle, value: NodeHandle) -> NodeHandle {
        let span = self.default_span();

        // Get target and value expressions
        let target_expr = self.get_expr(target)
            .unwrap_or_else(|| self.inner.variable("target", Type::Primitive(PrimitiveType::I32), span));
        let value_expr = self.get_expr(value)
            .unwrap_or_else(|| self.inner.int_literal(0, span));

        // Create assignment as a binary expression: target = value
        let assign_expr = self.inner.binary(
            BinaryOp::Assign,
            target_expr,
            value_expr,
            Type::Primitive(PrimitiveType::Unit),
            span,
        );

        // Wrap in expression statement
        let stmt = self.inner.expression_statement(assign_expr, span);
        self.store_stmt(stmt)
    }

    fn create_return(&mut self, value: Option<NodeHandle>) -> NodeHandle {
        let span = self.default_span();

        if let Some(h) = value {
            if let Some(expr) = self.get_expr(h) {
                let stmt = self.inner.return_stmt(expr, span);
                return self.store_stmt(stmt);
            }
        }

        let stmt = self.inner.return_void(span);
        self.store_stmt(stmt)
    }

    fn create_if(
        &mut self,
        condition: NodeHandle,
        then_branch: NodeHandle,
        else_branch: Option<NodeHandle>,
    ) -> NodeHandle {
        let span = self.default_span();

        let cond_expr = self.get_expr(condition)
            .unwrap_or_else(|| self.inner.bool_literal(true, span));

        // Get the then block - check for block first, then statement, then expression
        let then_block = if let Some(block) = self.get_block(then_branch) {
            block
        } else if let Some(stmt) = self.get_stmt(then_branch) {
            TypedBlock { statements: vec![stmt], span }
        } else if let Some(expr) = self.get_expr(then_branch) {
            TypedBlock { statements: vec![self.inner.expression_statement(expr, span)], span }
        } else {
            TypedBlock { statements: vec![], span }
        };

        // Get the else block if present
        let else_block = else_branch.map(|h| {
            if let Some(block) = self.get_block(h) {
                block
            } else if let Some(stmt) = self.get_stmt(h) {
                TypedBlock { statements: vec![stmt], span }
            } else if let Some(expr) = self.get_expr(h) {
                TypedBlock { statements: vec![self.inner.expression_statement(expr, span)], span }
            } else {
                TypedBlock { statements: vec![], span }
            }
        });

        let stmt = self.inner.if_statement(cond_expr, then_block, else_block, span);
        self.store_stmt(stmt)
    }

    fn create_while(&mut self, condition: NodeHandle, body: NodeHandle) -> NodeHandle {
        let span = self.default_span();

        let cond_expr = self.get_expr(condition)
            .unwrap_or_else(|| self.inner.bool_literal(true, span));

        // Get body block - check for block first, then statement, then expression
        let body_block = if let Some(block) = self.get_block(body) {
            block
        } else if let Some(stmt) = self.get_stmt(body) {
            TypedBlock { statements: vec![stmt], span }
        } else if let Some(expr) = self.get_expr(body) {
            TypedBlock { statements: vec![self.inner.expression_statement(expr, span)], span }
        } else {
            TypedBlock { statements: vec![], span }
        };

        let stmt = self.inner.while_loop(cond_expr, body_block, span);
        self.store_stmt(stmt)
    }

    fn create_for(&mut self, iterator: &str, iterable: NodeHandle, body: NodeHandle) -> NodeHandle {
        let span = self.default_span();

        let iter_expr = self.get_expr(iterable)
            .unwrap_or_else(|| self.inner.variable("iter", Type::Primitive(PrimitiveType::I32), span));

        // Get body block - check for block first, then statement, then expression
        let body_block = if let Some(block) = self.get_block(body) {
            block
        } else if let Some(stmt) = self.get_stmt(body) {
            TypedBlock { statements: vec![stmt], span }
        } else if let Some(expr) = self.get_expr(body) {
            TypedBlock { statements: vec![self.inner.expression_statement(expr, span)], span }
        } else {
            TypedBlock { statements: vec![], span }
        };

        let stmt = self.inner.for_loop(iterator, iter_expr, body_block, span);
        self.store_stmt(stmt)
    }

    fn create_block(&mut self, statements: Vec<NodeHandle>) -> NodeHandle {
        let span = self.default_span();

        // First collect all the statements we can find
        let mut stmts: Vec<TypedNode<TypedStatement>> = Vec::new();
        for h in &statements {
            if let Some(stmt) = self.get_stmt(*h) {
                stmts.push(stmt);
            } else if let Some(expr) = self.get_expr(*h) {
                let expr_stmt = self.inner.expression_statement(expr, span);
                stmts.push(expr_stmt);
            }
        }

        // Store the entire block and return its handle
        let block = TypedBlock { statements: stmts, span };
        self.store_block(block)
    }

    fn create_expr_stmt(&mut self, expr: NodeHandle) -> NodeHandle {
        let span = self.default_span();

        let expr_node = self.get_expr(expr)
            .unwrap_or_else(|| self.inner.unit_literal(span));

        let stmt = self.inner.expression_statement(expr_node, span);
        self.store_stmt(stmt)
    }

    fn create_primitive_type(&mut self, _name: &str) -> NodeHandle {
        // Types are handled inline - just return placeholder
        self.alloc_handle()
    }

    fn create_pointer_type(&mut self, _pointee: NodeHandle) -> NodeHandle {
        self.alloc_handle()
    }

    fn create_array_type(&mut self, _element: NodeHandle, _size: Option<usize>) -> NodeHandle {
        self.alloc_handle()
    }

    fn create_function_type(
        &mut self,
        _params: Vec<NodeHandle>,
        _return_type: NodeHandle,
    ) -> NodeHandle {
        self.alloc_handle()
    }

    fn create_named_type(&mut self, _name: &str) -> NodeHandle {
        self.alloc_handle()
    }

    fn create_struct(&mut self, _name: &str, _fields: Vec<NodeHandle>) -> NodeHandle {
        // TODO: Implement struct declaration once TypedDeclaration supports it
        // For now, just allocate a handle
        self.alloc_handle()
    }

    fn create_enum(&mut self, _name: &str, _variants: Vec<NodeHandle>) -> NodeHandle {
        // TODO: Implement enum declaration once TypedDeclaration supports it
        // For now, just allocate a handle
        self.alloc_handle()
    }

    fn create_field(&mut self, _name: &str, _ty: NodeHandle) -> NodeHandle {
        // TODO: Implement field once TypedField is available
        // For now, just allocate a handle
        self.alloc_handle()
    }

    fn create_variant(&mut self, _name: &str) -> NodeHandle {
        // TODO: Implement variant once TypedEnumVariant is available
        // For now, just allocate a handle
        self.alloc_handle()
    }

    fn set_span(&mut self, _node: NodeHandle, _start: usize, _end: usize) {
        // Spans are handled inline during node creation
        // This could be extended to update spans if needed
    }

    fn create_char_literal(&mut self, value: char) -> NodeHandle {
        let span = self.default_span();
        let expr = self.inner.char_literal(value, span);
        self.store_expr(expr)
    }

    fn create_let(
        &mut self,
        name: &str,
        ty: Option<NodeHandle>,
        init: Option<NodeHandle>,
        is_const: bool,
    ) -> NodeHandle {
        // Delegate to create_var_decl
        self.create_var_decl(name, ty, init, is_const)
    }

    fn create_break(&mut self, value: Option<NodeHandle>) -> NodeHandle {
        let span = self.default_span();

        if let Some(h) = value {
            if let Some(expr) = self.get_expr(h) {
                let stmt = self.inner.break_with_value(expr, span);
                return self.store_stmt(stmt);
            }
        }

        let stmt = self.inner.break_stmt(span);
        self.store_stmt(stmt)
    }

    fn create_continue(&mut self) -> NodeHandle {
        let span = self.default_span();
        let stmt = self.inner.continue_stmt(span);
        self.store_stmt(stmt)
    }

    fn create_expression_stmt(&mut self, expr: NodeHandle) -> NodeHandle {
        // Delegate to create_expr_stmt
        self.create_expr_stmt(expr)
    }

    fn create_variable(&mut self, name: &str) -> NodeHandle {
        let span = self.default_span();
        let expr = self.inner.variable(name, Type::Primitive(PrimitiveType::I32), span);
        self.store_expr(expr)
    }

    fn create_method_call(&mut self, receiver: NodeHandle, method: &str, args: Vec<NodeHandle>) -> NodeHandle {
        let span = self.default_span();

        let receiver_expr = self.get_expr(receiver)
            .unwrap_or_else(|| self.inner.variable("self", Type::Primitive(PrimitiveType::I32), span));

        let arg_exprs: Vec<_> = args.iter()
            .filter_map(|h| self.get_expr(*h))
            .collect();

        let expr = self.inner.method_call(receiver_expr, method, arg_exprs, Type::Primitive(PrimitiveType::I32), span);
        self.store_expr(expr)
    }

    fn create_array(&mut self, elements: Vec<NodeHandle>) -> NodeHandle {
        let span = self.default_span();

        let elem_exprs: Vec<_> = elements.iter()
            .filter_map(|h| self.get_expr(*h))
            .collect();

        let array_type = Type::Array {
            element_type: Box::new(Type::Primitive(PrimitiveType::I32)),
            size: Some(zyntax_typed_ast::ConstValue::Int(elem_exprs.len() as i64)),
            nullability: zyntax_typed_ast::NullabilityKind::NonNull,
        };

        let expr = self.inner.array_literal(elem_exprs, array_type, span);
        self.store_expr(expr)
    }

    fn create_struct_literal(&mut self, name: &str, fields: Vec<(String, NodeHandle)>) -> NodeHandle {
        let span = self.default_span();

        let field_exprs: Vec<(&str, TypedNode<TypedExpression>)> = fields.iter()
            .filter_map(|(field_name, h)| {
                self.get_expr(*h).map(|expr| (field_name.as_str(), expr))
            })
            .collect();

        // Use Unknown type for now - proper struct types would need TypeRegistry lookup
        let struct_type = Type::Unknown;

        let expr = self.inner.struct_literal(name, field_exprs, struct_type, span);
        self.store_expr(expr)
    }

    fn create_cast(&mut self, expr_handle: NodeHandle, _target_type: NodeHandle) -> NodeHandle {
        let span = self.default_span();

        let expr = self.get_expr(expr_handle)
            .unwrap_or_else(|| self.inner.int_literal(0, span));

        // For now, cast to i64
        let target = Type::Primitive(PrimitiveType::I64);
        let cast_expr = self.inner.cast(expr, target, span);
        self.store_expr(cast_expr)
    }

    fn create_lambda(&mut self, _params: Vec<NodeHandle>, body: NodeHandle) -> NodeHandle {
        let span = self.default_span();

        let body_expr = self.get_expr(body)
            .unwrap_or_else(|| self.inner.unit_literal(span));

        // Simple lambda with no params for now
        let lambda_type = Type::Function {
            params: vec![],
            return_type: Box::new(Type::Primitive(PrimitiveType::I32)),
            is_varargs: false,
            has_named_params: false,
            has_default_params: false,
            async_kind: zyntax_typed_ast::AsyncKind::Sync,
            calling_convention: zyntax_typed_ast::CallingConvention::Default,
            nullability: zyntax_typed_ast::NullabilityKind::NonNull,
        };

        let expr = self.inner.lambda(vec![], body_expr, lambda_type, span);
        self.store_expr(expr)
    }
}

// ============================================================================
// Command Interpreter
// ============================================================================

/// Interpreter that executes zpeg commands against a parse tree
pub struct CommandInterpreter<'a, H: AstHostFunctions> {
    /// The zpeg module containing rule commands
    module: &'a ZpegModule,
    /// Host functions for AST construction
    host: H,
    /// Current value stack
    value_stack: Vec<RuntimeValue>,
    /// Named variables
    variables: HashMap<String, RuntimeValue>,
}

impl<'a, H: AstHostFunctions> CommandInterpreter<'a, H> {
    /// Create a new interpreter
    pub fn new(module: &'a ZpegModule, host: H) -> Self {
        Self {
            module,
            host,
            value_stack: Vec::new(),
            variables: HashMap::new(),
        }
    }

    /// Get the host functions (consuming the interpreter)
    pub fn into_host(self) -> H {
        self.host
    }

    /// Get mutable reference to host functions
    pub fn host_mut(&mut self) -> &mut H {
        &mut self.host
    }

    /// Execute commands for a rule given its parse tree node
    pub fn execute_rule(&mut self, rule_name: &str, text: &str, children: Vec<RuntimeValue>) -> Result<RuntimeValue> {
        // Get commands for this rule
        let commands = match self.module.rule_commands(rule_name) {
            Some(cmds) => cmds.commands.clone(),
            None => {
                // No commands defined - default behavior depends on children
                if children.len() == 1 {
                    return Ok(children.into_iter().next().unwrap());
                } else if children.is_empty() {
                    // Leaf node - return text
                    return Ok(RuntimeValue::String(text.to_string()));
                } else {
                    // Multiple children - return as list
                    return Ok(RuntimeValue::List(children));
                }
            }
        };

        // Clear previous child variables before storing new ones
        // This is critical - otherwise old $2, $3, etc. from child rules pollute parent rules
        self.variables.retain(|k, _| !k.starts_with('$') || k == "$text");

        // Store children as $1, $2, etc.
        for (i, child) in children.into_iter().enumerate() {
            self.variables.insert(format!("${}", i + 1), child);
        }
        // Store text as $text
        self.variables.insert("$text".to_string(), RuntimeValue::String(text.to_string()));

        // Execute commands
        for cmd in &commands {
            self.execute_command(cmd)?;
        }

        // Return top of stack or null
        Ok(self.value_stack.pop().unwrap_or(RuntimeValue::Null))
    }

    /// Execute a single command
    fn execute_command(&mut self, cmd: &AstCommand) -> Result<()> {
        match cmd {
            AstCommand::Define { node, args } => {
                // Resolve named arguments to a map of RuntimeValues
                let mut resolved_args: HashMap<String, RuntimeValue> = HashMap::new();
                for (key, arg) in &args.0 {
                    resolved_args.insert(key.clone(), self.resolve_arg(arg)?);
                }

                let result = self.define_node(node, resolved_args)?;
                self.value_stack.push(result);
            }

            AstCommand::Call { func, args } => {
                let resolved_args: Vec<RuntimeValue> = args.iter()
                    .map(|a| self.resolve_arg(a))
                    .collect::<Result<Vec<_>>>()?;

                let result = self.call_host_function(func, resolved_args)?;
                self.value_stack.push(result);
            }

            AstCommand::GetChild { index, name } => {
                let value = if let Some(idx) = index {
                    let key = format!("${}", idx + 1);
                    self.variables.get(&key).cloned().unwrap_or(RuntimeValue::Null)
                } else if let Some(n) = name {
                    let key = format!("${}", n);
                    self.variables.get(&key).cloned().unwrap_or(RuntimeValue::Null)
                } else {
                    RuntimeValue::Null
                };
                self.value_stack.push(value);
            }

            AstCommand::GetAllChildren => {
                // Collect all $N variables as a list
                let mut children: Vec<RuntimeValue> = Vec::new();
                let mut i = 1;
                while let Some(val) = self.variables.get(&format!("${}", i)) {
                    children.push(val.clone());
                    i += 1;
                }
                self.value_stack.push(RuntimeValue::List(children));
            }

            AstCommand::GetText => {
                let text = self.variables.get("$text")
                    .cloned()
                    .unwrap_or(RuntimeValue::String(String::new()));
                self.value_stack.push(text);
            }

            AstCommand::ParseInt => {
                if let Some(RuntimeValue::String(s)) = self.value_stack.pop() {
                    let value: i64 = s.trim().parse().unwrap_or(0);
                    self.value_stack.push(RuntimeValue::Int(value));
                }
            }

            AstCommand::ParseFloat => {
                if let Some(RuntimeValue::String(s)) = self.value_stack.pop() {
                    let value: f64 = s.trim().parse().unwrap_or(0.0);
                    self.value_stack.push(RuntimeValue::Float(value));
                }
            }

            AstCommand::Span => {
                // TODO: Get span from current parse position
                self.value_stack.push(RuntimeValue::Span { start: 0, end: 0 });
            }

            AstCommand::FoldBinary { operand_rule: _, operator_rule: _ } => {
                // Binary fold: takes alternating operands and operators from variables
                // e.g., for "1 + 2 - 3": $1=1, $2="+", $3=2, $4="-", $5=3
                // Result: ((1 + 2) - 3)

                // Collect all children in order
                let mut children: Vec<RuntimeValue> = Vec::new();
                let mut i = 1;
                while let Some(val) = self.variables.get(&format!("${}", i)) {
                    children.push(val.clone());
                    i += 1;
                }

                if children.is_empty() {
                    self.value_stack.push(RuntimeValue::Null);
                } else if children.len() == 1 {
                    // Single operand - just return it
                    self.value_stack.push(children.into_iter().next().unwrap());
                } else {
                    // Fold left-to-right: operand op operand op operand ...
                    // children[0], children[2], children[4], ... are operands (nodes)
                    // children[1], children[3], children[5], ... are operators (strings)
                    let mut result = children[0].clone();

                    let mut idx = 1;
                    while idx + 1 < children.len() {
                        let op = &children[idx];
                        let right = &children[idx + 1];

                        // Get operator string
                        let op_str = match op {
                            RuntimeValue::String(s) => s.clone(),
                            RuntimeValue::Node(_) => {
                                // Operator was parsed as a node, try to get its text
                                // This happens with rules like add_op = { "+" }
                                "+".to_string() // Default fallback
                            }
                            _ => "+".to_string(),
                        };

                        // Get node handles
                        if let (RuntimeValue::Node(left_h), RuntimeValue::Node(right_h)) = (&result, right) {
                            let new_node = self.host.create_binary_op(&op_str, *left_h, *right_h);
                            result = RuntimeValue::Node(new_node);
                        }

                        idx += 2;
                    }

                    self.value_stack.push(result);
                }
            }

            AstCommand::MapChildren { rule, commands } => {
                let _ = rule; // Filter children by rule name
                let results: Vec<RuntimeValue> = Vec::new();

                // Execute commands for each matching child
                for cmd in commands {
                    self.execute_command(cmd)?;
                }

                self.value_stack.push(RuntimeValue::List(results));
            }

            AstCommand::MatchRule { cases } => {
                // Get the rule name from first child
                // For now, execute first case that exists
                for (_rule_name, cmds) in cases {
                    for cmd in cmds {
                        self.execute_command(cmd)?;
                    }
                    break;
                }
            }

            AstCommand::Store { name } => {
                if let Some(value) = self.value_stack.last().cloned() {
                    // Store with $ prefix for consistency with resolve_arg
                    let key = if name.starts_with('$') {
                        name.clone()
                    } else {
                        format!("${}", name)
                    };
                    self.variables.insert(key, value);
                }
            }

            AstCommand::Load { name } => {
                // Look up with $ prefix if not provided
                let key = if name.starts_with('$') {
                    name.clone()
                } else {
                    format!("${}", name)
                };
                let value = self.variables.get(&key).cloned().unwrap_or(RuntimeValue::Null);
                self.value_stack.push(value);
            }

            AstCommand::Return => {
                // Value is already on stack - nothing to do
            }
        }

        Ok(())
    }

    /// Resolve a command argument to a value
    fn resolve_arg(&self, arg: &CommandArg) -> Result<RuntimeValue> {
        match arg {
            CommandArg::ChildRef(ref_str) => {
                // $1, $2, $name, $text, $result
                if ref_str == "$result" {
                    Ok(self.value_stack.last().cloned().unwrap_or(RuntimeValue::Null))
                } else {
                    Ok(self.variables.get(ref_str).cloned().unwrap_or(RuntimeValue::Null))
                }
            }
            CommandArg::StringLit(s) => Ok(RuntimeValue::String(s.clone())),
            CommandArg::IntLit(n) => Ok(RuntimeValue::Int(*n)),
            CommandArg::BoolLit(b) => Ok(RuntimeValue::Bool(*b)),
            CommandArg::List(items) => {
                // Resolve each item in the list
                let resolved: Vec<RuntimeValue> = items.iter()
                    .map(|item| self.resolve_arg(item))
                    .collect::<Result<Vec<_>>>()?;
                Ok(RuntimeValue::List(resolved))
            }
            CommandArg::Nested(cmd) => {
                // Execute nested command and return result
                // This is a simplified version - full impl would need mutable self
                let _ = cmd;
                Ok(RuntimeValue::Null)
            }
        }
    }

    /// Define an AST node with named arguments (new format)
    fn define_node(&mut self, node_type: &str, args: HashMap<String, RuntimeValue>) -> Result<RuntimeValue> {
        match node_type {
            "int_literal" => {
                let value = match args.get("value") {
                    Some(RuntimeValue::Int(n)) => *n,
                    Some(RuntimeValue::String(s)) => s.parse().unwrap_or(0),
                    _ => 0,
                };
                let handle = self.host.create_int_literal(value);
                Ok(RuntimeValue::Node(handle))
            }

            "float_literal" => {
                let value = match args.get("value") {
                    Some(RuntimeValue::Float(n)) => *n,
                    Some(RuntimeValue::String(s)) => s.parse().unwrap_or(0.0),
                    _ => 0.0,
                };
                let handle = self.host.create_float_literal(value);
                Ok(RuntimeValue::Node(handle))
            }

            "string_literal" => {
                let value = match args.get("value") {
                    Some(RuntimeValue::String(s)) => s.clone(),
                    _ => String::new(),
                };
                let handle = self.host.create_string_literal(&value);
                Ok(RuntimeValue::Node(handle))
            }

            "bool_literal" => {
                let value = match args.get("value") {
                    Some(RuntimeValue::Bool(b)) => *b,
                    Some(RuntimeValue::String(s)) => s == "true",
                    _ => false,
                };
                let handle = self.host.create_bool_literal(value);
                Ok(RuntimeValue::Node(handle))
            }

            "identifier" => {
                let name = match args.get("name") {
                    Some(RuntimeValue::String(s)) => s.clone(),
                    _ => String::new(),
                };
                let handle = self.host.create_identifier(&name);
                Ok(RuntimeValue::Node(handle))
            }

            "binary_op" => {
                let op = match args.get("op").or(args.get("operator")) {
                    Some(RuntimeValue::String(s)) => s.clone(),
                    _ => "+".to_string(),
                };
                let left = match args.get("left") {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => return Err(crate::error::ZynPegError::CodeGenError("binary_op: missing left operand".into())),
                };
                let right = match args.get("right") {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => return Err(crate::error::ZynPegError::CodeGenError("binary_op: missing right operand".into())),
                };
                let handle = self.host.create_binary_op(&op, left, right);
                Ok(RuntimeValue::Node(handle))
            }

            "unary_op" | "unary" => {
                let op = match args.get("op").or(args.get("operator")) {
                    Some(RuntimeValue::String(s)) => s.clone(),
                    _ => "-".to_string(),
                };
                let operand = match args.get("operand") {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => return Err(crate::error::ZynPegError::CodeGenError("unary_op: missing operand".into())),
                };
                let handle = self.host.create_unary_op(&op, operand);
                Ok(RuntimeValue::Node(handle))
            }

            "return_stmt" => {
                let value = match args.get("value") {
                    Some(RuntimeValue::Node(h)) => Some(*h),
                    _ => None,
                };
                let handle = self.host.create_return(value);
                Ok(RuntimeValue::Node(handle))
            }

            "function" => {
                let name = match args.get("name") {
                    Some(RuntimeValue::String(s)) => s.clone(),
                    _ => "anonymous".to_string(),
                };

                let params: Vec<NodeHandle> = match args.get("params") {
                    Some(RuntimeValue::List(list)) => {
                        list.iter()
                            .filter_map(|v| match v {
                                RuntimeValue::Node(h) => Some(*h),
                                _ => None,
                            })
                            .collect()
                    }
                    _ => vec![],
                };

                let body = match args.get("body") {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => self.host.create_block(vec![]),
                };

                let return_type = self.host.create_primitive_type("i32");
                let handle = self.host.create_function(&name, params, return_type, body);
                Ok(RuntimeValue::Node(handle))
            }

            "struct" => {
                let name = match args.get("name") {
                    Some(RuntimeValue::String(s)) => s.clone(),
                    _ => "AnonymousStruct".to_string(),
                };
                let fields: Vec<NodeHandle> = match args.get("fields") {
                    Some(RuntimeValue::List(list)) => {
                        list.iter()
                            .filter_map(|v| match v {
                                RuntimeValue::Node(h) => Some(*h),
                                _ => None,
                            })
                            .collect()
                    }
                    _ => vec![],
                };
                let handle = self.host.create_struct(&name, fields);
                Ok(RuntimeValue::Node(handle))
            }

            "enum" => {
                let name = match args.get("name") {
                    Some(RuntimeValue::String(s)) => s.clone(),
                    _ => "AnonymousEnum".to_string(),
                };
                let variants: Vec<NodeHandle> = match args.get("variants") {
                    Some(RuntimeValue::List(list)) => {
                        list.iter()
                            .filter_map(|v| match v {
                                RuntimeValue::Node(h) => Some(*h),
                                _ => None,
                            })
                            .collect()
                    }
                    _ => vec![],
                };
                let handle = self.host.create_enum(&name, variants);
                Ok(RuntimeValue::Node(handle))
            }

            "field" => {
                let name = match args.get("name") {
                    Some(RuntimeValue::String(s)) => s.clone(),
                    _ => "field".to_string(),
                };
                let ty = match args.get("type") {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => self.host.create_primitive_type("i32"),
                };
                let handle = self.host.create_field(&name, ty);
                Ok(RuntimeValue::Node(handle))
            }

            "variant" => {
                let name = match args.get("name") {
                    Some(RuntimeValue::String(s)) => s.clone(),
                    _ => "Variant".to_string(),
                };
                let handle = self.host.create_variant(&name);
                Ok(RuntimeValue::Node(handle))
            }

            "program" => {
                let handle = self.host.create_program();
                // Add declarations from args
                if let Some(RuntimeValue::List(decls)) = args.get("declarations") {
                    for decl in decls {
                        if let RuntimeValue::Node(decl_h) = decl {
                            self.host.program_add_decl(handle, *decl_h);
                        }
                    }
                } else if let Some(RuntimeValue::Node(decl)) = args.get("declarations") {
                    self.host.program_add_decl(handle, *decl);
                }
                Ok(RuntimeValue::Node(handle))
            }

            "block" => {
                let statements: Vec<NodeHandle> = match args.get("statements") {
                    Some(RuntimeValue::List(list)) => {
                        list.iter()
                            .filter_map(|v| match v {
                                RuntimeValue::Node(h) => Some(*h),
                                _ => None,
                            })
                            .collect()
                    }
                    // Handle single statement (when statement* matches exactly one)
                    Some(RuntimeValue::Node(h)) => vec![*h],
                    _ => vec![],
                };
                let handle = self.host.create_block(statements);
                Ok(RuntimeValue::Node(handle))
            }

            "param" => {
                let name = match args.get("name") {
                    Some(RuntimeValue::String(s)) => s.clone(),
                    _ => "arg".to_string(),
                };
                let ty = match args.get("type") {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => self.host.create_primitive_type("i32"),
                };
                let handle = self.host.create_param(&name, ty);
                Ok(RuntimeValue::Node(handle))
            }

            "primitive_type" => {
                let name = match args.get("name") {
                    Some(RuntimeValue::String(s)) => s.clone(),
                    _ => "i32".to_string(),
                };
                let handle = self.host.create_primitive_type(&name);
                Ok(RuntimeValue::Node(handle))
            }

            // ===== ADDITIONAL EXPRESSIONS =====

            "char_literal" => {
                let value = match args.get("value") {
                    Some(RuntimeValue::String(s)) if !s.is_empty() => s.chars().next().unwrap_or('\0'),
                    _ => '\0',
                };
                let handle = self.host.create_char_literal(value);
                Ok(RuntimeValue::Node(handle))
            }

            "variable" | "var_ref" => {
                let name = match args.get("name") {
                    Some(RuntimeValue::String(s)) => s.clone(),
                    _ => String::new(),
                };
                let handle = self.host.create_variable(&name);
                Ok(RuntimeValue::Node(handle))
            }

            "call_expr" | "call" => {
                // Callee can be either a Node (expression) or String (identifier)
                let callee = match args.get("callee") {
                    Some(RuntimeValue::Node(h)) => *h,
                    Some(RuntimeValue::String(name)) => {
                        // Create a variable reference for the callee
                        self.host.create_variable(name)
                    }
                    _ => return Err(crate::error::ZynPegError::CodeGenError("call: missing callee".into())),
                };
                let call_args: Vec<NodeHandle> = match args.get("args") {
                    Some(RuntimeValue::List(list)) => {
                        list.iter()
                            .filter_map(|v| match v {
                                RuntimeValue::Node(h) => Some(*h),
                                _ => None,
                            })
                            .collect()
                    }
                    Some(RuntimeValue::Null) => vec![],
                    None => vec![],
                    _ => vec![],
                };
                let handle = self.host.create_call(callee, call_args);
                Ok(RuntimeValue::Node(handle))
            }

            "method_call" => {
                let receiver = match args.get("receiver") {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => return Err(crate::error::ZynPegError::CodeGenError("method_call: missing receiver".into())),
                };
                let method = match args.get("method") {
                    Some(RuntimeValue::String(s)) => s.clone(),
                    _ => String::new(),
                };
                let call_args: Vec<NodeHandle> = match args.get("args") {
                    Some(RuntimeValue::List(list)) => {
                        list.iter()
                            .filter_map(|v| match v {
                                RuntimeValue::Node(h) => Some(*h),
                                _ => None,
                            })
                            .collect()
                    }
                    _ => vec![],
                };
                let handle = self.host.create_method_call(receiver, &method, call_args);
                Ok(RuntimeValue::Node(handle))
            }

            "field_access" => {
                let object = match args.get("object") {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => return Err(crate::error::ZynPegError::CodeGenError("field_access: missing object".into())),
                };
                let field = match args.get("field") {
                    Some(RuntimeValue::String(s)) => s.clone(),
                    _ => String::new(),
                };
                let handle = self.host.create_field_access(object, &field);
                Ok(RuntimeValue::Node(handle))
            }

            "index" | "index_expr" => {
                let object = match args.get("object").or(args.get("array")) {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => return Err(crate::error::ZynPegError::CodeGenError("index: missing object".into())),
                };
                let index = match args.get("index") {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => return Err(crate::error::ZynPegError::CodeGenError("index: missing index".into())),
                };
                let handle = self.host.create_index(object, index);
                Ok(RuntimeValue::Node(handle))
            }

            "array" | "array_literal" => {
                let elements: Vec<NodeHandle> = match args.get("elements") {
                    Some(RuntimeValue::List(list)) => {
                        list.iter()
                            .filter_map(|v| match v {
                                RuntimeValue::Node(h) => Some(*h),
                                _ => None,
                            })
                            .collect()
                    }
                    _ => vec![],
                };
                let handle = self.host.create_array(elements);
                Ok(RuntimeValue::Node(handle))
            }

            "struct_literal" => {
                let name = match args.get("name") {
                    Some(RuntimeValue::String(s)) => s.clone(),
                    _ => String::new(),
                };
                let fields: Vec<(String, NodeHandle)> = match args.get("fields") {
                    Some(RuntimeValue::List(list)) => {
                        list.iter()
                            .filter_map(|v| match v {
                                RuntimeValue::Node(h) => Some((String::new(), *h)),
                                _ => None,
                            })
                            .collect()
                    }
                    _ => vec![],
                };
                let handle = self.host.create_struct_literal(&name, fields);
                Ok(RuntimeValue::Node(handle))
            }

            "cast" => {
                let expr = match args.get("expr") {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => return Err(crate::error::ZynPegError::CodeGenError("cast: missing expr".into())),
                };
                let target_type = match args.get("target_type").or(args.get("type")) {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => self.host.create_primitive_type("i32"),
                };
                let handle = self.host.create_cast(expr, target_type);
                Ok(RuntimeValue::Node(handle))
            }

            "lambda" => {
                let params: Vec<NodeHandle> = match args.get("params") {
                    Some(RuntimeValue::List(list)) => {
                        list.iter()
                            .filter_map(|v| match v {
                                RuntimeValue::Node(h) => Some(*h),
                                _ => None,
                            })
                            .collect()
                    }
                    _ => vec![],
                };
                let body = match args.get("body") {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => return Err(crate::error::ZynPegError::CodeGenError("lambda: missing body".into())),
                };
                let handle = self.host.create_lambda(params, body);
                Ok(RuntimeValue::Node(handle))
            }

            // ===== ADDITIONAL STATEMENTS =====

            "let_stmt" | "var_decl" => {
                let name = match args.get("name") {
                    Some(RuntimeValue::String(s)) => s.clone(),
                    _ => String::new(),
                };
                let ty = match args.get("type") {
                    Some(RuntimeValue::Node(h)) => Some(*h),
                    _ => None,
                };
                let init = match args.get("init").or(args.get("value")) {
                    Some(RuntimeValue::Node(h)) => Some(*h),
                    _ => None,
                };
                let is_const = match args.get("is_const").or(args.get("const")) {
                    Some(RuntimeValue::Bool(b)) => *b,
                    _ => false,
                };
                let handle = self.host.create_let(&name, ty, init, is_const);
                Ok(RuntimeValue::Node(handle))
            }

            "assignment" | "assign" => {
                // Target can be either a Node (expression) or String (identifier)
                let target = match args.get("target") {
                    Some(RuntimeValue::Node(h)) => *h,
                    Some(RuntimeValue::String(name)) => {
                        // Create a variable reference for the target
                        self.host.create_variable(name)
                    }
                    _ => return Err(crate::error::ZynPegError::CodeGenError("assignment: missing target".into())),
                };
                let value = match args.get("value") {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => return Err(crate::error::ZynPegError::CodeGenError("assignment: missing value".into())),
                };
                let handle = self.host.create_assignment(target, value);
                Ok(RuntimeValue::Node(handle))
            }

            "if_stmt" | "if" => {
                let condition = match args.get("condition") {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => return Err(crate::error::ZynPegError::CodeGenError("if: missing condition".into())),
                };
                let then_block = match args.get("then").or(args.get("then_block")).or(args.get("then_branch")) {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => return Err(crate::error::ZynPegError::CodeGenError("if: missing then block".into())),
                };
                let else_block = match args.get("else").or(args.get("else_block")).or(args.get("else_branch")) {
                    Some(RuntimeValue::Node(h)) => Some(*h),
                    _ => None,
                };
                let handle = self.host.create_if(condition, then_block, else_block);
                Ok(RuntimeValue::Node(handle))
            }

            "while_stmt" | "while" => {
                let condition = match args.get("condition") {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => return Err(crate::error::ZynPegError::CodeGenError("while: missing condition".into())),
                };
                let body = match args.get("body") {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => return Err(crate::error::ZynPegError::CodeGenError("while: missing body".into())),
                };
                let handle = self.host.create_while(condition, body);
                Ok(RuntimeValue::Node(handle))
            }

            "for_stmt" | "for" => {
                let variable = match args.get("variable").or(args.get("iterator")).or(args.get("binding")) {
                    Some(RuntimeValue::String(s)) => s.clone(),
                    Some(RuntimeValue::Node(h)) => {
                        // If it's a node, try to extract the name
                        format!("iter_{:?}", h)
                    }
                    _ => "it".to_string(),
                };
                let iterable = match args.get("iterable") {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => return Err(crate::error::ZynPegError::CodeGenError("for: missing iterable".into())),
                };
                let body = match args.get("body") {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => return Err(crate::error::ZynPegError::CodeGenError("for: missing body".into())),
                };
                let handle = self.host.create_for(&variable, iterable, body);
                Ok(RuntimeValue::Node(handle))
            }

            "break_stmt" | "break" => {
                let value = match args.get("value") {
                    Some(RuntimeValue::Node(h)) => Some(*h),
                    _ => None,
                };
                let handle = self.host.create_break(value);
                Ok(RuntimeValue::Node(handle))
            }

            "continue_stmt" | "continue" => {
                let handle = self.host.create_continue();
                Ok(RuntimeValue::Node(handle))
            }

            "expression_stmt" => {
                let expr = match args.get("expr") {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => return Err(crate::error::ZynPegError::CodeGenError("expression_stmt: missing expr".into())),
                };
                let handle = self.host.create_expression_stmt(expr);
                Ok(RuntimeValue::Node(handle))
            }

            // ===== TYPES =====

            "pointer_type" => {
                let pointee = match args.get("pointee") {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => self.host.create_primitive_type("i32"),
                };
                let handle = self.host.create_pointer_type(pointee);
                Ok(RuntimeValue::Node(handle))
            }

            "array_type" => {
                let element = match args.get("element") {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => self.host.create_primitive_type("i32"),
                };
                let size = match args.get("size") {
                    Some(RuntimeValue::Int(n)) => Some(*n as usize),
                    _ => None,
                };
                let handle = self.host.create_array_type(element, size);
                Ok(RuntimeValue::Node(handle))
            }

            "named_type" => {
                let name = match args.get("name") {
                    Some(RuntimeValue::String(s)) => s.clone(),
                    _ => "Unknown".to_string(),
                };
                let handle = self.host.create_named_type(&name);
                Ok(RuntimeValue::Node(handle))
            }

            "function_type" => {
                let params: Vec<NodeHandle> = match args.get("params") {
                    Some(RuntimeValue::List(list)) => {
                        list.iter()
                            .filter_map(|v| match v {
                                RuntimeValue::Node(h) => Some(*h),
                                _ => None,
                            })
                            .collect()
                    }
                    _ => vec![],
                };
                let return_type = match args.get("return_type") {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => self.host.create_primitive_type("void"),
                };
                let handle = self.host.create_function_type(params, return_type);
                Ok(RuntimeValue::Node(handle))
            }

            // ===== ZIG-SPECIFIC NODES =====

            "null_literal" => {
                // Create null literal - for now use an int with special marker
                // TypedASTBuilder may need a null_literal method added
                let handle = self.host.create_identifier("null");
                Ok(RuntimeValue::Node(handle))
            }

            "try_expr" | "try" => {
                // try expression unwraps error union
                let expr = match args.get("expr").or(args.get("operand")) {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => return Err(crate::error::ZynPegError::CodeGenError("try: missing expr".into())),
                };
                // For now, represent as unary with special op
                let handle = self.host.create_unary_op("try", expr);
                Ok(RuntimeValue::Node(handle))
            }

            "defer_stmt" | "defer" => {
                // defer statement - execute on scope exit
                let body = match args.get("body") {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => return Err(crate::error::ZynPegError::CodeGenError("defer: missing body".into())),
                };
                // Represent as expression statement for now
                let handle = self.host.create_expr_stmt(body);
                Ok(RuntimeValue::Node(handle))
            }

            "errdefer_stmt" | "errdefer" => {
                // errdefer - execute on scope exit if error
                let body = match args.get("body") {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => return Err(crate::error::ZynPegError::CodeGenError("errdefer: missing body".into())),
                };
                let handle = self.host.create_expr_stmt(body);
                Ok(RuntimeValue::Node(handle))
            }

            "optional_type" => {
                // ?T - optional type
                let inner = match args.get("inner").or(args.get("element")) {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => self.host.create_primitive_type("i32"),
                };
                // Represent as pointer for now (optional is like nullable pointer)
                let handle = self.host.create_pointer_type(inner);
                Ok(RuntimeValue::Node(handle))
            }

            "error_union_type" => {
                // !T - error union type
                let ok_type = match args.get("ok_type").or(args.get("inner")) {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => self.host.create_primitive_type("i32"),
                };
                // For now, just use the ok type (error handling comes later)
                Ok(RuntimeValue::Node(ok_type))
            }

            "struct_decl" => {
                // Struct type declaration
                let name = match args.get("name") {
                    Some(RuntimeValue::String(s)) => s.clone(),
                    _ => "AnonymousStruct".to_string(),
                };
                // Create as named type for now
                let handle = self.host.create_named_type(&name);
                Ok(RuntimeValue::Node(handle))
            }

            "enum_decl" => {
                // Enum type declaration
                let name = match args.get("name") {
                    Some(RuntimeValue::String(s)) => s.clone(),
                    _ => "AnonymousEnum".to_string(),
                };
                let handle = self.host.create_named_type(&name);
                Ok(RuntimeValue::Node(handle))
            }

            "union_decl" => {
                // Union type declaration
                let name = match args.get("name") {
                    Some(RuntimeValue::String(s)) => s.clone(),
                    _ => "AnonymousUnion".to_string(),
                };
                let handle = self.host.create_named_type(&name);
                Ok(RuntimeValue::Node(handle))
            }

            "orelse_expr" | "orelse" => {
                // x orelse y - unwrap optional or use default
                let lhs = match args.get("lhs").or(args.get("left")) {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => return Err(crate::error::ZynPegError::CodeGenError("orelse: missing lhs".into())),
                };
                let rhs = match args.get("rhs").or(args.get("right")) {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => return Err(crate::error::ZynPegError::CodeGenError("orelse: missing rhs".into())),
                };
                // Represent as binary op with special operator
                let handle = self.host.create_binary_op("orelse", lhs, rhs);
                Ok(RuntimeValue::Node(handle))
            }

            "catch_expr" | "catch" => {
                // x catch y - unwrap error or use default/handler
                let lhs = match args.get("lhs").or(args.get("left")) {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => return Err(crate::error::ZynPegError::CodeGenError("catch: missing lhs".into())),
                };
                let rhs = match args.get("rhs").or(args.get("right")) {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => return Err(crate::error::ZynPegError::CodeGenError("catch: missing rhs".into())),
                };
                let handle = self.host.create_binary_op("catch", lhs, rhs);
                Ok(RuntimeValue::Node(handle))
            }

            _ => {
                Err(crate::error::ZynPegError::CodeGenError(format!("Unknown node type: {}", node_type)))
            }
        }
    }

    /// Call a host function with resolved arguments (legacy positional format)
    fn call_host_function(&mut self, func: &str, args: Vec<RuntimeValue>) -> Result<RuntimeValue> {
        match func {
            "int_literal" => {
                let value = match args.first() {
                    Some(RuntimeValue::Int(n)) => *n,
                    Some(RuntimeValue::String(s)) => s.parse().unwrap_or(0),
                    _ => 0,
                };
                let handle = self.host.create_int_literal(value);
                Ok(RuntimeValue::Node(handle))
            }

            "float_literal" => {
                let value = match args.first() {
                    Some(RuntimeValue::Float(n)) => *n,
                    Some(RuntimeValue::String(s)) => s.parse().unwrap_or(0.0),
                    _ => 0.0,
                };
                let handle = self.host.create_float_literal(value);
                Ok(RuntimeValue::Node(handle))
            }

            "string_literal" => {
                let value = match args.first() {
                    Some(RuntimeValue::String(s)) => s.clone(),
                    _ => String::new(),
                };
                let handle = self.host.create_string_literal(&value);
                Ok(RuntimeValue::Node(handle))
            }

            "bool_literal" => {
                let value = match args.first() {
                    Some(RuntimeValue::Bool(b)) => *b,
                    _ => false,
                };
                let handle = self.host.create_bool_literal(value);
                Ok(RuntimeValue::Node(handle))
            }

            "identifier" => {
                let name = match args.first() {
                    Some(RuntimeValue::String(s)) => s.clone(),
                    _ => String::new(),
                };
                let handle = self.host.create_identifier(&name);
                Ok(RuntimeValue::Node(handle))
            }

            "binary_op" => {
                let op = match args.get(0) {
                    Some(RuntimeValue::String(s)) => s.clone(),
                    _ => "Unknown".to_string(),
                };
                let left = match args.get(1) {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => return Err(crate::error::ZynPegError::CodeGenError("binary_op: missing left operand".into())),
                };
                let right = match args.get(2) {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => return Err(crate::error::ZynPegError::CodeGenError("binary_op: missing right operand".into())),
                };
                let handle = self.host.create_binary_op(&op, left, right);
                Ok(RuntimeValue::Node(handle))
            }

            "unary_op" => {
                let op = match args.get(0) {
                    Some(RuntimeValue::String(s)) => s.clone(),
                    _ => "Unknown".to_string(),
                };
                let operand = match args.get(1) {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => return Err(crate::error::ZynPegError::CodeGenError("unary_op: missing operand".into())),
                };
                let handle = self.host.create_unary_op(&op, operand);
                Ok(RuntimeValue::Node(handle))
            }

            "return" => {
                let value = match args.first() {
                    Some(RuntimeValue::Node(h)) => Some(*h),
                    Some(RuntimeValue::Null) => None,
                    None => None,
                    _ => None,
                };
                let handle = self.host.create_return(value);
                Ok(RuntimeValue::Node(handle))
            }

            "block" => {
                let statements: Vec<NodeHandle> = args.iter()
                    .filter_map(|a| match a {
                        RuntimeValue::Node(h) => Some(*h),
                        RuntimeValue::List(items) => {
                            // Flatten list of nodes
                            None // TODO: Handle nested lists
                        }
                        _ => None,
                    })
                    .collect();
                let handle = self.host.create_block(statements);
                Ok(RuntimeValue::Node(handle))
            }

            "primitive_type" => {
                let name = match args.first() {
                    Some(RuntimeValue::String(s)) => s.clone(),
                    _ => "unknown".to_string(),
                };
                let handle = self.host.create_primitive_type(&name);
                Ok(RuntimeValue::Node(handle))
            }

            "program" => {
                let handle = self.host.create_program();
                // Add declarations from args
                for arg in args {
                    if let RuntimeValue::Node(decl) = arg {
                        self.host.program_add_decl(handle, decl);
                    }
                }
                Ok(RuntimeValue::Node(handle))
            }

            "return_stmt" => {
                // Create a return statement from an expression
                let value = match args.first() {
                    Some(RuntimeValue::Node(h)) => Some(*h),
                    _ => None,
                };
                let handle = self.host.create_return(value);
                Ok(RuntimeValue::Node(handle))
            }

            "function" => {
                // Create a function: function(name, params, body)
                // args: [name: string, params: list of nodes, body: node]
                let name = match args.get(0) {
                    Some(RuntimeValue::String(s)) => s.clone(),
                    _ => "anonymous".to_string(),
                };

                let params: Vec<NodeHandle> = match args.get(1) {
                    Some(RuntimeValue::List(list)) => {
                        list.iter()
                            .filter_map(|v| match v {
                                RuntimeValue::Node(h) => Some(*h),
                                _ => None,
                            })
                            .collect()
                    }
                    _ => vec![],
                };

                let body = match args.get(2) {
                    Some(RuntimeValue::Node(h)) => *h,
                    _ => {
                        // Create empty block as default body
                        self.host.create_block(vec![])
                    }
                };

                let return_type = self.host.create_primitive_type("i32");
                let handle = self.host.create_function(&name, params, return_type, body);
                Ok(RuntimeValue::Node(handle))
            }

            // Add more host functions as needed...

            _ => {
                Err(crate::error::ZynPegError::CodeGenError(format!("Unknown host function: {}", func)))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_zpeg_module_serialization() {
        let module = ZpegModule {
            metadata: ZpegMetadata {
                name: "Test".to_string(),
                version: "1.0".to_string(),
                file_extensions: vec![".test".to_string()],
                entry_point: None,
                zpeg_version: "0.1.0".to_string(),
            },
            pest_grammar: "program = { expr }".to_string(),
            rules: HashMap::from([(
                "expr".to_string(),
                RuleCommands {
                    return_type: Some("TypedExpression".to_string()),
                    commands: vec![
                        AstCommand::GetChild {
                            index: Some(0),
                            name: None,
                        },
                        AstCommand::Return,
                    ],
                },
            )]),
        };

        let json = serde_json::to_string_pretty(&module).unwrap();
        let parsed: ZpegModule = serde_json::from_str(&json).unwrap();

        assert_eq!(parsed.metadata.name, "Test");
        assert_eq!(parsed.rules.len(), 1);
    }

    #[test]
    fn test_command_serialization() {
        let cmd = AstCommand::Call {
            func: "binary_op".to_string(),
            args: vec![
                CommandArg::StringLit("Add".to_string()),
                CommandArg::ChildRef("$1".to_string()),
                CommandArg::ChildRef("$3".to_string()),
            ],
        };

        let json = serde_json::to_string(&cmd).unwrap();
        assert!(json.contains("binary_op"));
        assert!(json.contains("$1"));
    }
}
