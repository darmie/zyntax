//! # Typed Control Flow Graph Builder
//!
//! This module creates CFG structures from TypedAST without converting to HIR first.
//! This breaks the circular dependency between CFG and SSA construction:
//! - TypedCfgBuilder creates CFG structure from TypedAST (control flow only)
//! - SsaBuilder then processes TypedStatements to emit HIR instructions
//!
//! This is the solution to Gap #4 (CFG Construction) described in INTEGRATION_GAPS_ANALYSIS.md

use petgraph::graph::{DiGraph, NodeIndex};
use std::collections::HashMap;
use zyntax_typed_ast::{
    InternedString,
    typed_ast::{TypedBlock, TypedStatement, TypedNode, TypedExpression, typed_node},
};
use crate::hir::HirId;
use crate::CompilerResult;

/// Builder for creating CFG from TypedAST
pub struct TypedCfgBuilder {
    /// Next block ID to allocate
    next_block_id: u32,
    /// Stack of loop contexts for Break/Continue handling
    /// Each entry is (header_id, after_id) for the loop
    loop_stack: Vec<(HirId, HirId)>,
}

/// Control flow graph with TypedAST statements (not yet converted to HIR)
pub struct TypedControlFlowGraph {
    /// Graph structure: nodes are basic blocks, edges are control flow
    pub graph: DiGraph<TypedBasicBlock, ()>,
    /// Entry block node
    pub entry: NodeIndex,
    /// Exit block node
    pub exit: NodeIndex,
    /// Map from HirId to graph node
    pub block_map: HashMap<HirId, NodeIndex>,
    /// Map from graph node to HirId
    pub node_map: HashMap<NodeIndex, HirId>,
}

/// A basic block containing TypedAST statements
#[derive(Debug, Clone)]
pub struct TypedBasicBlock {
    /// Unique ID for this block
    pub id: HirId,
    /// Label for this block (optional)
    pub label: Option<InternedString>,
    /// Statements in this block
    pub statements: Vec<TypedNode<TypedStatement>>,
    /// How control flow exits this block
    pub terminator: TypedTerminator,
}

/// Control flow terminator for TypedBasicBlock
#[derive(Debug, Clone)]
pub enum TypedTerminator {
    /// Return from function
    Return(Option<Box<TypedNode<TypedExpression>>>),
    /// Unconditional jump to block
    Jump(HirId),
    /// Conditional branch
    CondBranch {
        condition: Box<TypedNode<TypedExpression>>,
        true_target: HirId,
        false_target: HirId,
    },
    /// Unreachable code
    Unreachable,
}

impl TypedCfgBuilder {
    pub fn new() -> Self {
        Self {
            next_block_id: 0,
            loop_stack: Vec::new(),
        }
    }

    /// Generate a new unique block ID
    fn new_block_id(&mut self) -> HirId {
        let id = HirId::new();
        self.next_block_id += 1;
        id
    }

    /// Build CFG from a typed block
    /// entry_block_id should be the ID of the entry block from the HirFunction
    pub fn build_from_block(&mut self, block: &TypedBlock, entry_block_id: HirId) -> CompilerResult<TypedControlFlowGraph> {
        let mut graph = DiGraph::new();
        let mut block_map = HashMap::new();
        let mut node_map = HashMap::new();

        // Use the provided entry block ID (from HirFunction)
        let entry_id = entry_block_id;

        // Process block with control flow splitting
        let (blocks, entry_id_final, exit_id) = self.split_at_control_flow(block, entry_id)?;

        // Add all blocks to graph and create mapping
        for typed_block in blocks {
            let block_id = typed_block.id;
            let node = graph.add_node(typed_block);
            block_map.insert(block_id, node);
            node_map.insert(node, block_id);
        }

        // Add edges based on terminators
        // Collect edges first to avoid borrow checker issues
        let edges_to_add: Vec<(NodeIndex, NodeIndex)> = graph.node_indices()
            .filter_map(|node| {
                let block = &graph[node];
                match &block.terminator {
                    TypedTerminator::Jump(target) => {
                        block_map.get(target).map(|&target_node| vec![(node, target_node)])
                    }
                    TypedTerminator::CondBranch { true_target, false_target, .. } => {
                        let mut edges = Vec::new();
                        if let Some(&true_node) = block_map.get(true_target) {
                            edges.push((node, true_node));
                        }
                        if let Some(&false_node) = block_map.get(false_target) {
                            edges.push((node, false_node));
                        }
                        if edges.is_empty() { None } else { Some(edges) }
                    }
                    _ => None
                }
            })
            .flatten()
            .collect();

        // Add collected edges
        for (source, target) in edges_to_add {
            graph.add_edge(source, target, ());
        }

        let entry_node = block_map[&entry_id_final];
        let exit_node = block_map[&exit_id];

        Ok(TypedControlFlowGraph {
            graph,
            entry: entry_node,
            exit: exit_node,
            block_map,
            node_map,
        })
    }

    /// Process a TypedBlock into a TypedBasicBlock
    fn process_block(
        &mut self,
        block: &TypedBlock,
        block_id: HirId,
    ) -> CompilerResult<(TypedBasicBlock, HirId)> {
        let mut statements = Vec::new();
        let mut terminator = TypedTerminator::Unreachable;

        // Process each statement
        for stmt in &block.statements {
            match &stmt.node {
                TypedStatement::Return(expr) => {
                    // Return terminates the block
                    terminator = TypedTerminator::Return(expr.clone());
                    break; // No more statements after return
                }

                // For now, treat all other statements as non-terminating
                // TODO: Handle If, While, Match, etc. that create control flow
                _ => {
                    statements.push(stmt.clone());
                }
            }
        }

        Ok((
            TypedBasicBlock {
                id: block_id,
                label: None,
                statements,
                terminator,
            },
            block_id, // exit_id (same as entry for simple blocks)
        ))
    }

    /// Split a block at control flow boundaries
    /// Returns (all_blocks, entry_block_id, exit_block_id)
    fn split_at_control_flow(
        &mut self,
        block: &TypedBlock,
        entry_id: HirId,
    ) -> CompilerResult<(Vec<TypedBasicBlock>, HirId, HirId)> {
        let mut all_blocks = Vec::new();
        let mut current_statements = Vec::new();
        let mut current_block_id = entry_id;
        let mut exit_id = entry_id;

        for stmt in &block.statements {
            match &stmt.node {
                TypedStatement::If(if_stmt) => {
                    // Create block for statements before If
                    let then_id = self.new_block_id();
                    let else_id = if if_stmt.else_block.is_some() {
                        self.new_block_id()
                    } else {
                        self.new_block_id() // Merge block
                    };
                    let merge_id = self.new_block_id();

                    // Current block ends with conditional branch
                    all_blocks.push(TypedBasicBlock {
                        id: current_block_id,
                        label: None,
                        statements: current_statements.clone(),
                        terminator: TypedTerminator::CondBranch {
                            condition: if_stmt.condition.clone(),
                            true_target: then_id,
                            false_target: else_id,
                        },
                    });

                    // Process then block
                    let (then_blocks, _, then_exit) = self.split_at_control_flow(&if_stmt.then_block, then_id)?;
                    all_blocks.extend(then_blocks);

                    // Make then block jump to merge
                    if let Some(last_block) = all_blocks.iter_mut().rev().find(|b| b.id == then_exit) {
                        if matches!(last_block.terminator, TypedTerminator::Unreachable) {
                            last_block.terminator = TypedTerminator::Jump(merge_id);
                        }
                    }

                    // Process else block or create empty else
                    if let Some(ref else_block) = if_stmt.else_block {
                        let (else_blocks, _, else_exit) = self.split_at_control_flow(else_block, else_id)?;
                        all_blocks.extend(else_blocks);

                        // Make else block jump to merge
                        if let Some(last_block) = all_blocks.iter_mut().rev().find(|b| b.id == else_exit) {
                            if matches!(last_block.terminator, TypedTerminator::Unreachable) {
                                last_block.terminator = TypedTerminator::Jump(merge_id);
                            }
                        }
                    } else {
                        // Empty else block jumps directly to merge
                        all_blocks.push(TypedBasicBlock {
                            id: else_id,
                            label: None,
                            statements: vec![],
                            terminator: TypedTerminator::Jump(merge_id),
                        });
                    }

                    // Start new block after If (merge point)
                    current_statements = Vec::new();
                    current_block_id = merge_id;
                    exit_id = merge_id;
                }

                TypedStatement::While(while_stmt) => {
                    // Create blocks for while loop
                    let header_id = self.new_block_id();
                    let body_id = self.new_block_id();
                    let after_id = self.new_block_id();

                    // Current block ends with jump to header
                    all_blocks.push(TypedBasicBlock {
                        id: current_block_id,
                        label: None,
                        statements: current_statements.clone(),
                        terminator: TypedTerminator::Jump(header_id),
                    });

                    // Header block evaluates condition
                    all_blocks.push(TypedBasicBlock {
                        id: header_id,
                        label: None,
                        statements: vec![],
                        terminator: TypedTerminator::CondBranch {
                            condition: while_stmt.condition.clone(),
                            true_target: body_id,
                            false_target: after_id,
                        },
                    });

                    // Push loop context for Break/Continue
                    self.loop_stack.push((header_id, after_id));

                    // Process body block
                    let (body_blocks, _, body_exit) = self.split_at_control_flow(&while_stmt.body, body_id)?;
                    all_blocks.extend(body_blocks);

                    // Pop loop context
                    self.loop_stack.pop();

                    // Make body block jump back to header
                    if let Some(last_block) = all_blocks.iter_mut().rev().find(|b| b.id == body_exit) {
                        if matches!(last_block.terminator, TypedTerminator::Unreachable) {
                            last_block.terminator = TypedTerminator::Jump(header_id);
                        }
                    }

                    // Continue with after block for subsequent statements
                    current_statements = Vec::new();
                    current_block_id = after_id;
                    exit_id = after_id;
                }

                TypedStatement::Loop(loop_stmt) => {
                    // Infinite loop: loop { body }
                    // Creates: entry → header → body → header
                    //                     ↓
                    //                   exit (for break)

                    use zyntax_typed_ast::typed_ast::TypedLoop;

                    match loop_stmt {
                        TypedLoop::Infinite { body, .. } => {
                            let header_id = self.new_block_id();
                            let body_id = self.new_block_id();
                            let after_id = self.new_block_id();

                            // Current block jumps to header
                            all_blocks.push(TypedBasicBlock {
                                id: current_block_id,
                                label: None,
                                statements: current_statements.clone(),
                                terminator: TypedTerminator::Jump(header_id),
                            });

                            // Header block (no condition, always enters body)
                            all_blocks.push(TypedBasicBlock {
                                id: header_id,
                                label: None,
                                statements: vec![],
                                terminator: TypedTerminator::Jump(body_id),
                            });

                            // Push loop context for Break/Continue
                            self.loop_stack.push((header_id, after_id));

                            // Process body block
                            let (body_blocks, _, body_exit) = self.split_at_control_flow(body, body_id)?;
                            all_blocks.extend(body_blocks);

                            // Pop loop context
                            self.loop_stack.pop();

                            // Make body block jump back to header (unless it has break/return)
                            if let Some(last_block) = all_blocks.iter_mut().rev().find(|b| b.id == body_exit) {
                                if matches!(last_block.terminator, TypedTerminator::Unreachable) {
                                    last_block.terminator = TypedTerminator::Jump(header_id);
                                }
                            }

                            // Continue with after block
                            current_statements = Vec::new();
                            current_block_id = after_id;
                            exit_id = after_id;
                        }
                        TypedLoop::ForEach { pattern, iterator, body } => {
                            // For-each loop: for item in collection
                            // Similar to While: entry → header → body → header → exit
                            // Header evaluates iterator.next(), body processes item

                            let header_id = self.new_block_id();
                            let body_id = self.new_block_id();
                            let after_id = self.new_block_id();

                            // Current block jumps to header
                            all_blocks.push(TypedBasicBlock {
                                id: current_block_id,
                                label: None,
                                statements: current_statements.clone(),
                                terminator: TypedTerminator::Jump(header_id),
                            });

                            // Header block (iterator logic will be handled by SSA builder)
                            // For now, we model it as: if iterator.has_next() then body else exit
                            // The actual iterator protocol will be implemented in SSA/HIR lowering
                            all_blocks.push(TypedBasicBlock {
                                id: header_id,
                                label: None,
                                statements: vec![],
                                // TODO: Create proper iterator condition expression
                                // For now, treat as unconditional to body (will be fixed in SSA)
                                terminator: TypedTerminator::Jump(body_id),
                            });

                            // Push loop context
                            self.loop_stack.push((header_id, after_id));

                            // Process body block
                            let (body_blocks, _, body_exit) = self.split_at_control_flow(body, body_id)?;
                            all_blocks.extend(body_blocks);

                            // Pop loop context
                            self.loop_stack.pop();

                            // Body loops back to header
                            if let Some(last_block) = all_blocks.iter_mut().rev().find(|b| b.id == body_exit) {
                                if matches!(last_block.terminator, TypedTerminator::Unreachable) {
                                    last_block.terminator = TypedTerminator::Jump(header_id);
                                }
                            }

                            // Continue with after block
                            current_statements = Vec::new();
                            current_block_id = after_id;
                            exit_id = after_id;
                        }

                        TypedLoop::ForCStyle { init, condition, update, body } => {
                            // C-style for: for (init; condition; update) body
                            // Structure: entry → init → header → body → update → header → exit
                            //                              ↓
                            //                            exit

                            // Process init statement first (if present)
                            if let Some(init_stmt) = init {
                                current_statements.push(*init_stmt.clone());
                            }

                            let header_id = self.new_block_id();
                            let body_id = self.new_block_id();
                            let update_id = self.new_block_id();
                            let after_id = self.new_block_id();

                            // Current block (with init) jumps to header
                            all_blocks.push(TypedBasicBlock {
                                id: current_block_id,
                                label: None,
                                statements: current_statements.clone(),
                                terminator: TypedTerminator::Jump(header_id),
                            });

                            // Header evaluates condition
                            if let Some(cond) = condition {
                                all_blocks.push(TypedBasicBlock {
                                    id: header_id,
                                    label: None,
                                    statements: vec![],
                                    terminator: TypedTerminator::CondBranch {
                                        condition: cond.clone(),
                                        true_target: body_id,
                                        false_target: after_id,
                                    },
                                });
                            } else {
                                // No condition = infinite loop (like while(true))
                                all_blocks.push(TypedBasicBlock {
                                    id: header_id,
                                    label: None,
                                    statements: vec![],
                                    terminator: TypedTerminator::Jump(body_id),
                                });
                            }

                            // Push loop context (continue goes to update, not header)
                            self.loop_stack.push((update_id, after_id));

                            // Process body
                            let (body_blocks, _, body_exit) = self.split_at_control_flow(body, body_id)?;
                            all_blocks.extend(body_blocks);

                            // Pop loop context
                            self.loop_stack.pop();

                            // Body goes to update block
                            if let Some(last_block) = all_blocks.iter_mut().rev().find(|b| b.id == body_exit) {
                                if matches!(last_block.terminator, TypedTerminator::Unreachable) {
                                    last_block.terminator = TypedTerminator::Jump(update_id);
                                }
                            }

                            // Update block executes update expression and loops back to header
                            let mut update_statements = vec![];
                            if let Some(upd) = update {
                                // Update expression becomes a statement in the block
                                update_statements.push(typed_node(
                                    zyntax_typed_ast::typed_ast::TypedStatement::Expression(upd.clone()),
                                    upd.ty.clone(),
                                    upd.span,
                                ));
                            }

                            all_blocks.push(TypedBasicBlock {
                                id: update_id,
                                label: None,
                                statements: update_statements,
                                terminator: TypedTerminator::Jump(header_id),
                            });

                            // Continue with after block
                            current_statements = Vec::new();
                            current_block_id = after_id;
                            exit_id = after_id;
                        }

                        TypedLoop::While { condition, body } => {
                            // While loop inside Loop enum
                            // Same structure as TypedStatement::While
                            let header_id = self.new_block_id();
                            let body_id = self.new_block_id();
                            let after_id = self.new_block_id();

                            all_blocks.push(TypedBasicBlock {
                                id: current_block_id,
                                label: None,
                                statements: current_statements.clone(),
                                terminator: TypedTerminator::Jump(header_id),
                            });

                            all_blocks.push(TypedBasicBlock {
                                id: header_id,
                                label: None,
                                statements: vec![],
                                terminator: TypedTerminator::CondBranch {
                                    condition: condition.clone(),
                                    true_target: body_id,
                                    false_target: after_id,
                                },
                            });

                            self.loop_stack.push((header_id, after_id));
                            let (body_blocks, _, body_exit) = self.split_at_control_flow(body, body_id)?;
                            all_blocks.extend(body_blocks);
                            self.loop_stack.pop();

                            if let Some(last_block) = all_blocks.iter_mut().rev().find(|b| b.id == body_exit) {
                                if matches!(last_block.terminator, TypedTerminator::Unreachable) {
                                    last_block.terminator = TypedTerminator::Jump(header_id);
                                }
                            }

                            current_statements = Vec::new();
                            current_block_id = after_id;
                            exit_id = after_id;
                        }

                        TypedLoop::DoWhile { body, condition } => {
                            // Do-while: body executes at least once, then checks condition
                            // Structure: entry → body → header → body (if true) → exit (if false)

                            let body_id = self.new_block_id();
                            let header_id = self.new_block_id();
                            let after_id = self.new_block_id();

                            // Entry jumps directly to body (executes at least once)
                            all_blocks.push(TypedBasicBlock {
                                id: current_block_id,
                                label: None,
                                statements: current_statements.clone(),
                                terminator: TypedTerminator::Jump(body_id),
                            });

                            self.loop_stack.push((header_id, after_id));
                            let (body_blocks, _, body_exit) = self.split_at_control_flow(body, body_id)?;
                            all_blocks.extend(body_blocks);
                            self.loop_stack.pop();

                            // Body goes to header for condition check
                            if let Some(last_block) = all_blocks.iter_mut().rev().find(|b| b.id == body_exit) {
                                if matches!(last_block.terminator, TypedTerminator::Unreachable) {
                                    last_block.terminator = TypedTerminator::Jump(header_id);
                                }
                            }

                            // Header checks condition: true = loop back to body, false = exit
                            all_blocks.push(TypedBasicBlock {
                                id: header_id,
                                label: None,
                                statements: vec![],
                                terminator: TypedTerminator::CondBranch {
                                    condition: condition.clone(),
                                    true_target: body_id,
                                    false_target: after_id,
                                },
                            });

                            current_statements = Vec::new();
                            current_block_id = after_id;
                            exit_id = after_id;
                        }
                    }
                }

                TypedStatement::For(for_stmt) => {
                    // For-each loop (separate statement type)
                    // Same structure as TypedLoop::ForEach
                    let header_id = self.new_block_id();
                    let body_id = self.new_block_id();
                    let after_id = self.new_block_id();

                    all_blocks.push(TypedBasicBlock {
                        id: current_block_id,
                        label: None,
                        statements: current_statements.clone(),
                        terminator: TypedTerminator::Jump(header_id),
                    });

                    // Header (iterator protocol handled by SSA)
                    all_blocks.push(TypedBasicBlock {
                        id: header_id,
                        label: None,
                        statements: vec![],
                        terminator: TypedTerminator::Jump(body_id),
                    });

                    self.loop_stack.push((header_id, after_id));
                    let (body_blocks, _, body_exit) = self.split_at_control_flow(&for_stmt.body, body_id)?;
                    all_blocks.extend(body_blocks);
                    self.loop_stack.pop();

                    if let Some(last_block) = all_blocks.iter_mut().rev().find(|b| b.id == body_exit) {
                        if matches!(last_block.terminator, TypedTerminator::Unreachable) {
                            last_block.terminator = TypedTerminator::Jump(header_id);
                        }
                    }

                    current_statements = Vec::new();
                    current_block_id = after_id;
                    exit_id = after_id;
                }

                TypedStatement::ForCStyle(for_c_stmt) => {
                    // C-style for loop (separate statement type)
                    // Same structure as TypedLoop::ForCStyle

                    // Process init
                    if let Some(init_stmt) = &for_c_stmt.init {
                        current_statements.push(*init_stmt.clone());
                    }

                    let header_id = self.new_block_id();
                    let body_id = self.new_block_id();
                    let update_id = self.new_block_id();
                    let after_id = self.new_block_id();

                    all_blocks.push(TypedBasicBlock {
                        id: current_block_id,
                        label: None,
                        statements: current_statements.clone(),
                        terminator: TypedTerminator::Jump(header_id),
                    });

                    // Header with condition
                    if let Some(cond) = &for_c_stmt.condition {
                        all_blocks.push(TypedBasicBlock {
                            id: header_id,
                            label: None,
                            statements: vec![],
                            terminator: TypedTerminator::CondBranch {
                                condition: cond.clone(),
                                true_target: body_id,
                                false_target: after_id,
                            },
                        });
                    } else {
                        all_blocks.push(TypedBasicBlock {
                            id: header_id,
                            label: None,
                            statements: vec![],
                            terminator: TypedTerminator::Jump(body_id),
                        });
                    }

                    self.loop_stack.push((update_id, after_id));
                    let (body_blocks, _, body_exit) = self.split_at_control_flow(&for_c_stmt.body, body_id)?;
                    all_blocks.extend(body_blocks);
                    self.loop_stack.pop();

                    if let Some(last_block) = all_blocks.iter_mut().rev().find(|b| b.id == body_exit) {
                        if matches!(last_block.terminator, TypedTerminator::Unreachable) {
                            last_block.terminator = TypedTerminator::Jump(update_id);
                        }
                    }

                    // Update block
                    let mut update_statements = vec![];
                    if let Some(upd) = &for_c_stmt.update {
                        update_statements.push(typed_node(
                            zyntax_typed_ast::typed_ast::TypedStatement::Expression(upd.clone()),
                            upd.ty.clone(),
                            upd.span,
                        ));
                    }

                    all_blocks.push(TypedBasicBlock {
                        id: update_id,
                        label: None,
                        statements: update_statements,
                        terminator: TypedTerminator::Jump(header_id),
                    });

                    current_statements = Vec::new();
                    current_block_id = after_id;
                    exit_id = after_id;
                }

                TypedStatement::Block(block) => {
                    // Nested block - recursively process
                    let (block_blocks, block_entry, block_exit) = self.split_at_control_flow(block, current_block_id)?;

                    // Add all blocks from the nested block
                    all_blocks.extend(block_blocks);

                    // Continue after the nested block
                    current_statements = Vec::new();
                    current_block_id = block_exit;
                    exit_id = block_exit;
                }

                TypedStatement::Break(value_opt) => {
                    // Break jumps to loop exit
                    if let Some(&(_header_id, exit_id)) = self.loop_stack.last() {
                        all_blocks.push(TypedBasicBlock {
                            id: current_block_id,
                            label: None,
                            statements: current_statements.clone(),
                            terminator: TypedTerminator::Jump(exit_id),
                        });

                        // Create a new unreachable block for any statements after break
                        let unreachable_id = self.new_block_id();
                        current_statements = Vec::new();
                        current_block_id = unreachable_id;
                        // Don't update exit_id - break already jumped
                    } else {
                        // Break outside loop - treat as error or unreachable
                        // For now, just add to current block and let type checker catch it
                        current_statements.push(stmt.clone());
                    }
                }

                TypedStatement::Continue => {
                    // Continue jumps to loop header
                    if let Some(&(header_id, _exit_id)) = self.loop_stack.last() {
                        all_blocks.push(TypedBasicBlock {
                            id: current_block_id,
                            label: None,
                            statements: current_statements.clone(),
                            terminator: TypedTerminator::Jump(header_id),
                        });

                        // Create a new unreachable block for any statements after continue
                        let unreachable_id = self.new_block_id();
                        current_statements = Vec::new();
                        current_block_id = unreachable_id;
                    } else {
                        // Continue outside loop
                        current_statements.push(stmt.clone());
                    }
                }

                TypedStatement::Match(match_stmt) => {
                    // Match statement: evaluate scrutinee, then check each arm's pattern
                    // Structure: entry(scrutinee) → arm1_check → arm1_body → merge
                    //                                    ↓           ↓
                    //                               arm2_check → arm2_body → merge
                    //                                    ↓           ↓
                    //                               arm3_check → arm3_body → merge
                    //                                    ↓
                    //                               unreachable (if exhaustive)

                    if match_stmt.arms.is_empty() {
                        // Empty match - just treat as regular statement
                        current_statements.push(stmt.clone());
                        return Ok((all_blocks, entry_id, exit_id));
                    }

                    let merge_id = self.new_block_id();

                    // Entry block evaluates scrutinee and jumps to first pattern check
                    let first_pattern_id = self.new_block_id();
                    all_blocks.push(TypedBasicBlock {
                        id: current_block_id,
                        label: None,
                        statements: current_statements.clone(),
                        terminator: TypedTerminator::Jump(first_pattern_id),
                    });

                    let mut prev_pattern_id = first_pattern_id;

                    // Create blocks for each arm
                    for (i, arm) in match_stmt.arms.iter().enumerate() {
                        let body_id = self.new_block_id();
                        let next_pattern_id = if i + 1 < match_stmt.arms.len() {
                            self.new_block_id() // Next arm's pattern check
                        } else {
                            self.new_block_id() // Unreachable (match should be exhaustive)
                        };

                        // Pattern check block
                        // TODO: Actual pattern matching will be implemented in SSA layer
                        // For now, we model guard conditions and unconditional jumps
                        if let Some(guard) = &arm.guard {
                            // With guard: conditional on guard expression
                            all_blocks.push(TypedBasicBlock {
                                id: prev_pattern_id,
                                label: None,
                                statements: vec![],
                                terminator: TypedTerminator::CondBranch {
                                    condition: guard.clone(),
                                    true_target: body_id,
                                    false_target: next_pattern_id,
                                },
                            });
                        } else {
                            // No guard: jump directly to body
                            // (Pattern check will be handled by SSA builder)
                            all_blocks.push(TypedBasicBlock {
                                id: prev_pattern_id,
                                label: None,
                                statements: vec![],
                                terminator: TypedTerminator::Jump(body_id),
                            });
                        }

                        // Body block - arm body is an expression
                        let body_stmt = typed_node(
                            zyntax_typed_ast::typed_ast::TypedStatement::Expression(arm.body.clone()),
                            arm.body.ty.clone(),
                            arm.body.span,
                        );

                        all_blocks.push(TypedBasicBlock {
                            id: body_id,
                            label: None,
                            statements: vec![body_stmt],
                            terminator: TypedTerminator::Jump(merge_id),
                        });

                        prev_pattern_id = next_pattern_id;
                    }

                    // Last pattern check block is unreachable (if match is exhaustive)
                    all_blocks.push(TypedBasicBlock {
                        id: prev_pattern_id,
                        label: None,
                        statements: vec![],
                        terminator: TypedTerminator::Unreachable,
                    });

                    // Continue with merge block
                    current_statements = Vec::new();
                    current_block_id = merge_id;
                    exit_id = merge_id;
                }

                TypedStatement::Return(expr) => {
                    // Current block ends with return
                    all_blocks.push(TypedBasicBlock {
                        id: current_block_id,
                        label: None,
                        statements: current_statements.clone(),
                        terminator: TypedTerminator::Return(expr.clone()),
                    });

                    // No more processing after return
                    exit_id = current_block_id;
                    return Ok((all_blocks, entry_id, exit_id));
                }

                _ => {
                    // Regular statement - add to current block
                    current_statements.push(stmt.clone());
                }
            }
        }

        // Always create a final block if we have a current_block_id that hasn't been added yet
        // This handles the case where control flow statements leave us with an empty continuation block
        if !all_blocks.iter().any(|b| b.id == current_block_id) {
            all_blocks.push(TypedBasicBlock {
                id: current_block_id,
                label: None,
                statements: current_statements,
                terminator: TypedTerminator::Unreachable,
            });
            exit_id = current_block_id;
        }

        Ok((all_blocks, entry_id, exit_id))
    }
}

impl Default for TypedCfgBuilder {
    fn default() -> Self {
        Self::new()
    }
}
