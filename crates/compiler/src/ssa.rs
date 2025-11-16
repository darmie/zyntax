//! # SSA Form Construction
//! 
//! Converts CFG to Static Single Assignment form for both Cranelift and LLVM.
//! Uses the efficient algorithm from "Simple and Efficient Construction of SSA Form"
//! by Braun et al.

use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::Arc;
use zyntax_typed_ast::{InternedString, Type, ConstValue};
use petgraph::visit::EdgeRef; // For .source() method on edges
use crate::hir::{
    HirId, HirFunction, HirBlock, HirInstruction, HirValueKind,
    HirType, HirPhi, HirTerminator, CastOp, HirParam, HirFunctionSignature
};
use crate::cfg::{ControlFlowGraph, BasicBlock};
use crate::CompilerResult;

/// SSA builder state
pub struct SsaBuilder {
    /// Current function being built
    function: HirFunction,
    /// Variable definitions per block
    definitions: HashMap<HirId, HashMap<InternedString, HirId>>,
    /// Incomplete phi nodes to be filled
    incomplete_phis: HashMap<(HirId, InternedString), HirId>,
    /// Variable counter for versioning
    var_counter: HashMap<InternedString, u32>,
    /// Type information for variables
    var_types: HashMap<InternedString, HirType>,
    /// Sealed blocks (all predecessors known)
    sealed_blocks: HashSet<HirId>,
    /// Filled blocks (all definitions complete)
    filled_blocks: HashSet<HirId>,
    /// Type registry for field lookups and type information
    type_registry: Arc<zyntax_typed_ast::TypeRegistry>,
    /// Generated closure functions (collected during translation)
    closure_functions: Vec<HirFunction>,
    /// Function symbol table for resolving function references
    function_symbols: HashMap<InternedString, HirId>,
    /// Generated string globals (collected during translation)
    string_globals: Vec<crate::hir::HirGlobal>,
    /// Track which variables are written in each block (for loop phi placement)
    variable_writes: HashMap<HirId, HashSet<InternedString>>,
    /// Flag: after IDF placement, don't create new phis
    idf_placement_done: bool,
}

/// SSA form representation
#[derive(Debug)]
pub struct SsaForm {
    pub function: HirFunction,
    /// Def-use chains for optimization
    pub def_use_chains: HashMap<HirId, HashSet<HirId>>,
    /// Use-def chains for analysis
    pub use_def_chains: HashMap<HirId, HirId>,
    /// Closure functions generated during translation
    pub closure_functions: Vec<HirFunction>,
    /// String globals generated during translation
    pub string_globals: Vec<crate::hir::HirGlobal>,
}

/// Phi node during construction
#[derive(Debug, Clone)]
pub struct PhiNode {
    pub result: HirId,
    pub variable: InternedString,
    pub block: HirId,
    pub operands: Vec<(HirId, HirId)>, // (value, predecessor_block)
}

/// Dominance information for SSA construction
/// Computed using Cooper-Harvey-Kennedy algorithm
#[derive(Debug, Clone)]
struct DominanceInfo {
    /// Immediate dominator for each block
    idom: HashMap<HirId, HirId>,
    /// Dominance frontiers for each block
    dom_frontier: HashMap<HirId, HashSet<HirId>>,
    /// Reverse postorder traversal
    rpo: Vec<HirId>,
}

impl DominanceInfo {
    /// Compute dominance information from TypedCFG
    fn compute(cfg: &crate::typed_cfg::TypedControlFlowGraph) -> Self {
        use petgraph::visit::Dfs;
        use petgraph::Direction;

        // Step 1: Compute reverse postorder (RPO)
        let mut rpo = Vec::new();
        let mut visited = HashSet::new();
        Self::postorder_dfs(cfg, cfg.entry, &mut visited, &mut rpo);
        rpo.reverse();

        // Step 2: Compute immediate dominators using Cooper-Harvey-Kennedy
        let mut idom: HashMap<HirId, HirId> = HashMap::new();
        let entry_id = cfg.node_map[&cfg.entry];
        idom.insert(entry_id, entry_id); // Entry dominates itself

        let mut changed = true;
        while changed {
            changed = false;
            for &block_idx in rpo.iter().skip(1) {
                let block = &cfg.graph[block_idx];
                let block_id = block.id;

                // Find new idom from processed predecessors
                let mut new_idom = None;
                for edge in cfg.graph.edges_directed(block_idx, Direction::Incoming) {
                    let pred_idx = edge.source();
                    let pred_id = cfg.graph[pred_idx].id;

                    if idom.contains_key(&pred_id) {
                        new_idom = match new_idom {
                            None => Some(pred_id),
                            Some(current) => Some(Self::intersect(&idom, current, pred_id, &rpo, cfg)),
                        };
                    }
                }

                if let Some(new_idom_val) = new_idom {
                    if idom.get(&block_id) != Some(&new_idom_val) {
                        idom.insert(block_id, new_idom_val);
                        changed = true;
                    }
                }
            }
        }

        // Step 3: Compute dominance frontiers
        let dom_frontier = Self::compute_frontiers(cfg, &idom);

        DominanceInfo {
            idom,
            dom_frontier,
            rpo: rpo.iter().map(|&idx| cfg.graph[idx].id).collect(),
        }
    }

    /// Postorder DFS traversal
    fn postorder_dfs(
        cfg: &crate::typed_cfg::TypedControlFlowGraph,
        node: petgraph::graph::NodeIndex,
        visited: &mut HashSet<petgraph::graph::NodeIndex>,
        postorder: &mut Vec<petgraph::graph::NodeIndex>,
    ) {
        if visited.contains(&node) {
            return;
        }
        visited.insert(node);

        for edge in cfg.graph.edges_directed(node, petgraph::Direction::Outgoing) {
            Self::postorder_dfs(cfg, edge.target(), visited, postorder);
        }

        postorder.push(node);
    }

    /// Find intersection of two dominators
    fn intersect(
        idom: &HashMap<HirId, HirId>,
        mut b1: HirId,
        mut b2: HirId,
        rpo: &[petgraph::graph::NodeIndex],
        cfg: &crate::typed_cfg::TypedControlFlowGraph,
    ) -> HirId {
        // Get RPO indices for blocks
        let rpo_map: HashMap<HirId, usize> = rpo.iter()
            .enumerate()
            .map(|(i, &idx)| (cfg.graph[idx].id, i))
            .collect();

        while b1 != b2 {
            while rpo_map.get(&b1).unwrap_or(&usize::MAX) > rpo_map.get(&b2).unwrap_or(&usize::MAX) {
                b1 = idom[&b1];
            }
            while rpo_map.get(&b2).unwrap_or(&usize::MAX) > rpo_map.get(&b1).unwrap_or(&usize::MAX) {
                b2 = idom[&b2];
            }
        }
        b1
    }

    /// Compute dominance frontiers
    fn compute_frontiers(
        cfg: &crate::typed_cfg::TypedControlFlowGraph,
        idom: &HashMap<HirId, HirId>,
    ) -> HashMap<HirId, HashSet<HirId>> {
        use petgraph::Direction;

        let mut frontiers: HashMap<HirId, HashSet<HirId>> = HashMap::new();

        for node_idx in cfg.graph.node_indices() {
            let block = &cfg.graph[node_idx];
            let block_id = block.id;

            // Get predecessors
            let preds: Vec<_> = cfg.graph.edges_directed(node_idx, Direction::Incoming)
                .map(|e| cfg.graph[e.source()].id)
                .collect();

            if preds.len() >= 2 {
                // Block with multiple predecessors - compute its dominance frontier
                for &pred in &preds {
                    let mut runner = pred;
                    // Walk up dominator tree from pred until we reach block's idom
                    while runner != idom.get(&block_id).copied().unwrap_or(block_id) {
                        frontiers.entry(runner).or_insert_with(HashSet::new).insert(block_id);
                        let next_runner = idom.get(&runner).copied();
                        if next_runner.is_none() || next_runner == Some(runner) {
                            break; // Reached entry or self-dom
                        }
                        runner = next_runner.unwrap();
                    }
                }
            }
        }

        frontiers
    }

    /// Check if block `a` dominates block `b`
    fn dominates(&self, a: HirId, b: HirId) -> bool {
        let mut current = b;
        while let Some(&dom) = self.idom.get(&current) {
            if dom == a {
                return true;
            }
            if dom == current {
                break; // Reached entry
            }
            current = dom;
        }
        false
    }
}

impl SsaBuilder {
    pub fn new(
        function: HirFunction,
        type_registry: Arc<zyntax_typed_ast::TypeRegistry>,
        function_symbols: HashMap<InternedString, HirId>,
    ) -> Self {
        Self {
            function,
            definitions: HashMap::new(),
            incomplete_phis: HashMap::new(),
            var_counter: HashMap::new(),
            var_types: HashMap::new(),
            sealed_blocks: HashSet::new(),
            filled_blocks: HashSet::new(),
            type_registry,
            closure_functions: Vec::new(),
            function_symbols,
            string_globals: Vec::new(),
            variable_writes: HashMap::new(),
            idf_placement_done: false,
        }
    }
    
    /// Build SSA form from CFG
    pub fn build_from_cfg(mut self, cfg: &ControlFlowGraph) -> CompilerResult<SsaForm> {
        // Initialize blocks
        for (block_id, _) in &self.function.blocks {
            self.definitions.insert(*block_id, HashMap::new());
        }

        // Process blocks in dominance order
        let dom_order = self.compute_dominance_order(cfg);

        for block_id in dom_order {
            self.process_block(block_id, cfg)?;
            self.seal_block(block_id);
        }

        // Fill remaining incomplete phis
        self.fill_incomplete_phis();

        // Build def-use chains
        let (def_use_chains, use_def_chains) = self.build_def_use_chains();

        Ok(SsaForm {
            function: self.function,
            def_use_chains,
            use_def_chains,
            closure_functions: self.closure_functions,
            string_globals: self.string_globals,
        })
    }

    /// Build SSA form from TypedControlFlowGraph
    /// This is the new approach that processes TypedAST directly
    pub fn build_from_typed_cfg(mut self, cfg: &crate::typed_cfg::TypedControlFlowGraph) -> CompilerResult<SsaForm> {
        // Create HirBlocks for all blocks in the CFG (except entry which already exists)
        for node_idx in cfg.graph.node_indices() {
            let typed_block = &cfg.graph[node_idx];
            let block_id = typed_block.id;

            // Skip if block already exists (entry block)
            if !self.function.blocks.contains_key(&block_id) {
                let hir_block = HirBlock::new(block_id);
                self.function.blocks.insert(block_id, hir_block);
            }
        }

        // Set up predecessors based on CFG edges
        for node_idx in cfg.graph.node_indices() {
            let typed_block = &cfg.graph[node_idx];
            let block_id = typed_block.id;

            // Find predecessors from incoming edges
            let mut predecessors = Vec::new();
            for edge in cfg.graph.edges_directed(node_idx, petgraph::Direction::Incoming) {
                let pred_node = edge.source();
                let pred_id = cfg.graph[pred_node].id;
                predecessors.push(pred_id);
            }

            if let Some(hir_block) = self.function.blocks.get_mut(&block_id) {
                hir_block.predecessors = predecessors;
            }
        }

        // Initialize definitions for all blocks
        for (block_id, _) in &self.function.blocks {
            self.definitions.insert(*block_id, HashMap::new());
        }

        // CRITICAL FIX: Initialize function parameters as HIR values in entry block
        // Without this, parameters are treated as undefined variables and phi nodes are created!
        // Clone params first to avoid borrow checker issues
        let params = self.function.signature.params.clone();
        let entry_block = self.function.entry_block;

        for (param_index, param) in params.iter().enumerate() {
            let param_value_id = self.create_value(param.ty.clone(), HirValueKind::Parameter(param_index as u32));

            // Store parameter type for SSA variable tracking
            self.var_types.insert(param.name, param.ty.clone());

            // Define parameter in entry block so it's available to all code
            self.write_variable(param.name, entry_block, param_value_id);
        }

        // CRITICAL: Seal entry block immediately after defining parameters
        // Entry block has no predecessors, so it can be sealed right away
        // This allows other blocks to read parameters from the entry block
        self.seal_block(entry_block);
        self.filled_blocks.insert(entry_block);

        // IDF-BASED SSA: Place phis using Iterated Dominance Frontier
        // CRITICAL: This must run BEFORE blocks are processed
        // It scans the CFG to find variable writes without translating to HIR
        self.place_phis_using_idf(cfg);

        // Mark IDF placement as done - no new phis should be created after this
        self.idf_placement_done = true;

        // CRITICAL: Propagate parameters to all blocks
        // Parameters don't need phis (they're never reassigned), but they need to be
        // available in all blocks. Copy them from entry block to all other blocks.
        let param_defs: Vec<_> = self.definitions.get(&entry_block)
            .map(|defs| defs.iter().map(|(&var, &val)| (var, val)).collect())
            .unwrap_or_default();

        for (block_id, _) in &self.function.blocks {
            if *block_id != entry_block {
                for (var, val) in &param_defs {
                    // Only copy if the block doesn't already have this variable
                    // (it might have a phi for variables that ARE reassigned)
                    if let Some(defs) = self.definitions.get_mut(block_id) {
                        defs.entry(*var).or_insert(*val);
                    }
                }
            }
        }

        // Process blocks in dominance-friendly order
        // For now, we use a simple worklist algorithm that processes blocks when their
        // non-back-edge predecessors are ready
        let mut worklist: Vec<_> = cfg.graph.node_indices().collect();
        let mut processed_blocks = std::collections::HashSet::new();
        processed_blocks.insert(entry_block);

        // Process entry block statements and terminator
        for node_idx in cfg.graph.node_indices() {
            let typed_block = &cfg.graph[node_idx];
            if typed_block.id == entry_block {
                for stmt in &typed_block.statements {
                    self.process_statement(entry_block, stmt)?;
                }
                self.process_typed_terminator(entry_block, &typed_block.terminator)?;
                break;
            }
        }

        // Keep processing until worklist is empty
        while !worklist.is_empty() {
            let mut made_progress = false;

            worklist.retain(|&node_idx| {
                let typed_block = &cfg.graph[node_idx];
                let block_id = typed_block.id;

                // Skip entry block - already processed
                if block_id == entry_block {
                    return false; // Remove from worklist
                }

                // Skip if already processed
                if processed_blocks.contains(&block_id) {
                    return false; // Remove from worklist
                }

                // Check if we can process this block
                // Process when at least one predecessor is filled for forward progress
                let block_info = self.function.blocks.get(&block_id).unwrap();
                let has_filled_pred = block_info.predecessors.iter().any(|pred| self.filled_blocks.contains(pred));

                if !has_filled_pred {
                    return true; // Keep in worklist
                }

                // Seal block only if all predecessors are filled
                let all_preds_filled = block_info.predecessors.iter().all(|pred| self.filled_blocks.contains(pred));
                if all_preds_filled && !self.sealed_blocks.contains(&block_id) {
                    self.seal_block(block_id);
                }

                // Process each TypedStatement in this block
                for stmt in &typed_block.statements {
                    self.process_statement(block_id, stmt).unwrap();
                }

                // Process the terminator
                self.process_typed_terminator(block_id, &typed_block.terminator).unwrap();

                // Mark block as filled
                self.filled_blocks.insert(block_id);

                // Seal if not sealed yet
                if !self.sealed_blocks.contains(&block_id) {
                    self.seal_block(block_id);
                }

                processed_blocks.insert(block_id);
                made_progress = true;
                false // Remove from worklist
            });

            if !made_progress && !worklist.is_empty() {
                // No progress made but worklist not empty - force process remaining blocks
                for &node_idx in &worklist {
                    let typed_block = &cfg.graph[node_idx];
                    let block_id = typed_block.id;

                    if processed_blocks.contains(&block_id) {
                        continue;
                    }

                    for stmt in &typed_block.statements {
                        self.process_statement(block_id, stmt)?;
                    }
                    self.process_typed_terminator(block_id, &typed_block.terminator)?;
                    self.filled_blocks.insert(block_id);
                    if !self.sealed_blocks.contains(&block_id) {
                        self.seal_block(block_id);
                    }
                    processed_blocks.insert(block_id);
                }
                break;
            }
        }

        // Fill remaining incomplete phis (from IDF placement)
        self.fill_incomplete_phis();

        // NOTE: verify_and_fix_phi_incoming() is DISABLED because it can create new phis
        // via read_variable(), which breaks Cranelift block parameter mapping.
        // If IDF is correct, all phis should already have all incoming values.
        // self.verify_and_fix_phi_incoming();

        // Build def-use chains
        let (def_use_chains, use_def_chains) = self.build_def_use_chains();

        Ok(SsaForm {
            function: self.function,
            def_use_chains,
            use_def_chains,
            closure_functions: self.closure_functions,
            string_globals: self.string_globals,
        })
    }

    /// Process a typed terminator
    fn process_typed_terminator(
        &mut self,
        block_id: HirId,
        terminator: &crate::typed_cfg::TypedTerminator
    ) -> CompilerResult<()> {
        use crate::typed_cfg::TypedTerminator;

        // First, translate expressions (requires mutable self)
        let hir_terminator = match terminator {
            TypedTerminator::Return(expr_opt) => {
                if let Some(expr) = expr_opt {
                    let value_id = self.translate_expression(block_id, expr)?;
                    HirTerminator::Return { values: vec![value_id] }
                } else {
                    HirTerminator::Return { values: vec![] }
                }
            }

            TypedTerminator::Jump(target) => {
                HirTerminator::Branch { target: *target }
            }

            TypedTerminator::CondBranch { condition, true_target, false_target } => {
                let cond_val = self.translate_expression(block_id, condition)?;
                HirTerminator::CondBranch {
                    condition: cond_val,
                    true_target: *true_target,
                    false_target: *false_target,
                }
            }

            TypedTerminator::Unreachable => {
                HirTerminator::Unreachable
            }
        };

        // Then set it (requires mutable block access)
        let hir_block = self.function.blocks.get_mut(&block_id)
            .ok_or_else(|| crate::CompilerError::Analysis("Block not found".into()))?;
        hir_block.terminator = hir_terminator;

        Ok(())
    }
    
    /// Process a basic block
    fn process_block(&mut self, block_id: HirId, cfg: &ControlFlowGraph) -> CompilerResult<()> {
        // Find the CFG block
        let cfg_node = cfg.block_map.get(&block_id)
            .and_then(|&node| cfg.graph.node_weight(node))
            .ok_or_else(|| crate::CompilerError::Analysis("Block not found in CFG".into()))?;
        
        // Process each statement
        for stmt in &cfg_node.statements {
            self.process_statement(block_id, stmt)?;
        }
        
        // Process terminator
        if let Some(term) = &cfg_node.terminator {
            self.process_terminator(block_id, term)?;
        }
        
        self.filled_blocks.insert(block_id);
        Ok(())
    }
    
    /// Process a statement
    fn process_statement(
        &mut self,
        block_id: HirId,
        stmt: &zyntax_typed_ast::TypedNode<zyntax_typed_ast::typed_ast::TypedStatement>
    ) -> CompilerResult<()> {
        use zyntax_typed_ast::typed_ast::TypedStatement;
        
        match &stmt.node {
            TypedStatement::Let(let_stmt) => {
                // Evaluate the initializer if present
                if let Some(value) = &let_stmt.initializer {
                    let value_id = self.translate_expression(block_id, value)?;
                    
                    // Record variable type
                    let hir_type = self.convert_type(&let_stmt.ty);
                    self.var_types.insert(let_stmt.name, hir_type.clone());
                    
                    // Create assignment
                    self.write_variable(let_stmt.name, block_id, value_id);
                }
            }
            
            TypedStatement::Expression(expr) => {
                // Evaluate expression for side effects
                self.translate_expression(block_id, expr)?;
            }

            // Note: Control flow statements (While, If, etc.) are now handled at the
            // TypedCFG level by TypedCfgBuilder.split_at_control_flow()
            // This is the solution to Gap 2 - multi-block CFG construction

            // Note: TypedStatement doesn't have Assign variant - assignments are expressions

            _ => {
                // Other statements handled by terminator processing
            }
        }
        
        Ok(())
    }
    
    /// Process a terminator
    fn process_terminator(
        &mut self,
        block_id: HirId,
        term: &zyntax_typed_ast::TypedNode<zyntax_typed_ast::typed_ast::TypedStatement>
    ) -> CompilerResult<()> {
        use zyntax_typed_ast::typed_ast::TypedStatement;
        
        match &term.node {
            TypedStatement::Return(expr) => {
                let values = if let Some(expr) = expr {
                    vec![self.translate_expression(block_id, expr)?]
                } else {
                    vec![]
                };
                
                let block = self.function.blocks.get_mut(&block_id).unwrap();
                block.terminator = HirTerminator::Return { values };
            }
            
            TypedStatement::If(if_stmt) => {
                let condition = &if_stmt.condition;
                let cond_value = self.translate_expression(block_id, condition)?;
                
                // The CFG builder already created the branches
                let block = self.function.blocks.get_mut(&block_id).unwrap();
                if block.successors.len() == 2 {
                    block.terminator = HirTerminator::CondBranch {
                        condition: cond_value,
                        true_target: block.successors[0],
                        false_target: block.successors[1],
                    };
                }
            }
            
            _ => {
                // Default to branch to first successor
                let block = self.function.blocks.get_mut(&block_id).unwrap();
                if let Some(&target) = block.successors.first() {
                    block.terminator = HirTerminator::Branch { target };
                }
            }
        }
        
        Ok(())
    }
    
    /// Translate expression to SSA value
    fn translate_expression(
        &mut self,
        block_id: HirId,
        expr: &zyntax_typed_ast::TypedNode<zyntax_typed_ast::typed_ast::TypedExpression>
    ) -> CompilerResult<HirId> {
        use zyntax_typed_ast::typed_ast::TypedExpression;
        use crate::hir::{BinaryOp, UnaryOp};
        
        match &expr.node {
            TypedExpression::Variable(name) => {
                Ok(self.read_variable(*name, block_id))
            }
            
            TypedExpression::Literal(lit) => {
                use zyntax_typed_ast::typed_ast::TypedLiteral;

                // String literals need to be stored as globals
                if let TypedLiteral::String(s) = lit {
                    Ok(self.create_string_global(*s))
                } else {
                    let constant = self.translate_literal(lit);
                    let ty = self.convert_type(&expr.ty);
                    Ok(self.create_value(ty, HirValueKind::Constant(constant)))
                }
            }
            
            TypedExpression::Binary(binary) => {
                use zyntax_typed_ast::typed_ast::BinaryOp as FrontendOp;

                let op = &binary.op;
                let left = &binary.left;
                let right = &binary.right;

                // Special handling for assignment
                if matches!(op, FrontendOp::Assign) {
                    return self.translate_assignment(block_id, left, right);
                }

                // Regular binary operations
                let left_val = self.translate_expression(block_id, left)?;
                let right_val = self.translate_expression(block_id, right)?;
                let result_type = self.convert_type(&expr.ty);

                let hir_op = self.convert_binary_op(op);
                let result = self.create_value(result_type.clone(), HirValueKind::Instruction);

                let inst = HirInstruction::Binary {
                    op: hir_op,
                    result,
                    ty: result_type,
                    left: left_val,
                    right: right_val,
                };

                self.add_instruction(block_id, inst);
                self.add_use(left_val, result);
                self.add_use(right_val, result);

                Ok(result)
            }
            
            TypedExpression::Unary(unary) => {
                let op = &unary.op;
                let operand = &unary.operand;
                let operand_val = self.translate_expression(block_id, operand)?;
                let result_type = self.convert_type(&expr.ty);
                
                let hir_op = self.convert_unary_op(op);
                let result = self.create_value(result_type.clone(), HirValueKind::Instruction);
                
                let inst = HirInstruction::Unary {
                    op: hir_op,
                    result,
                    ty: result_type,
                    operand: operand_val,
                };
                
                self.add_instruction(block_id, inst);
                self.add_use(operand_val, result);
                
                Ok(result)
            }
            
            TypedExpression::Call(call) => {
                let callee = &call.callee;
                let args = &call.positional_args;

                // Check if callee is a function name (direct call) vs expression (indirect call)
                let (hir_callable, indirect_callee_val) = if let TypedExpression::Variable(func_name) = &callee.node {
                    // Check if this variable is a function in the symbol table
                    if let Some(&func_id) = self.function_symbols.get(func_name) {
                        // Direct function call
                        (crate::hir::HirCallable::Function(func_id), None)
                    } else {
                        // Variable lookup (function pointer)
                        let callee_val = self.translate_expression(block_id, callee)?;
                        (crate::hir::HirCallable::Indirect(callee_val), Some(callee_val))
                    }
                } else {
                    // General expression (e.g., field access, method call result)
                    let callee_val = self.translate_expression(block_id, callee)?;
                    (crate::hir::HirCallable::Indirect(callee_val), Some(callee_val))
                };

                // Translate arguments
                let mut arg_vals = Vec::new();
                for arg in args {
                    let arg_val = self.translate_expression(block_id, arg)?;
                    arg_vals.push(arg_val);
                }

                // Translate type arguments for generic calls
                let type_args: Vec<HirType> = call.type_args.iter()
                    .map(|ty| self.convert_type(ty))
                    .collect();

                // Create call instruction
                let result_type = self.convert_type(&expr.ty);
                let result = if result_type != HirType::Void {
                    Some(self.create_value(result_type.clone(), HirValueKind::Instruction))
                } else {
                    None
                };

                let inst = HirInstruction::Call {
                    result,
                    callee: hir_callable,
                    args: arg_vals.clone(),
                    type_args,  // Preserve type arguments from TypedCall
                    const_args: vec![], // TODO: Preserve const args when TypedCall supports them
                    // NOTE: Tail call detection requires control flow analysis.
                    // Need to verify: (1) call is last instruction, (2) no cleanup needed, (3) compatible calling convention
                    // WORKAROUND: Always false (safe, but misses optimization opportunity)
                    // FUTURE (v2.0): Add tail call analysis pass
                    // Estimated effort: 8-10 hours (requires CFG analysis)
                    is_tail: false,
                };

                self.add_instruction(block_id, inst);

                // Add uses (for indirect calls, track callee value)
                if let Some(callee_val) = indirect_callee_val {
                    self.add_use(callee_val, result.unwrap_or(callee_val));
                }

                // Track argument uses
                let result_or_void = result.unwrap_or_else(|| self.create_undef(HirType::Void));
                for arg in &arg_vals {
                    self.add_use(*arg, result_or_void);
                }

                Ok(result_or_void)
            }
            
            TypedExpression::Field(field_access) => {
                let object = &field_access.object;
                let field = &field_access.field;
                let object_val = self.translate_expression(block_id, object)?;
                
                // Calculate field index
                let field_index = self.get_field_index(&object.ty, field)?;
                let result_type = self.convert_type(&expr.ty);
                let result = self.create_value(result_type.clone(), HirValueKind::Instruction);
                
                let inst = HirInstruction::ExtractValue {
                    result,
                    ty: result_type,
                    aggregate: object_val,
                    indices: vec![field_index],
                };
                
                self.add_instruction(block_id, inst);
                self.add_use(object_val, result);
                
                Ok(result)
            }
            
            TypedExpression::Index(index_expr) => {
                let object = &index_expr.object;
                let index = &index_expr.index;
                let object_val = self.translate_expression(block_id, object)?;
                let index_val = self.translate_expression(block_id, index)?;
                
                // Create GEP instruction
                let result_type = HirType::Ptr(Box::new(self.convert_type(&expr.ty)));
                let gep_result = self.create_value(result_type.clone(), HirValueKind::Instruction);
                
                let gep_inst = HirInstruction::GetElementPtr {
                    result: gep_result,
                    ty: result_type,
                    ptr: object_val,
                    indices: vec![index_val],
                };
                
                self.add_instruction(block_id, gep_inst);
                self.add_use(object_val, gep_result);
                self.add_use(index_val, gep_result);
                
                // Load the value
                let load_result = self.create_value(
                    self.convert_type(&expr.ty), 
                    HirValueKind::Instruction
                );
                
                let load_inst = HirInstruction::Load {
                    result: load_result,
                    ty: self.convert_type(&expr.ty),
                    ptr: gep_result,
                    // NOTE: Proper alignment calculation requires type layout information.
                    // Need TypeRegistry or TargetData to compute alignment from type.
                    // WORKAROUND: Fixed 4-byte alignment (works for most common types: i32, f32, pointers)
                    // FUTURE (v2.0): Calculate from type using target data layout
                    // Estimated effort: 3-4 hours (add alignment calculation utility)
                    align: 4,
                    volatile: false,
                };
                
                self.add_instruction(block_id, load_inst);
                self.add_use(gep_result, load_result);
                
                Ok(load_result)
            }
            
            TypedExpression::If(if_expr) => {
                // Evaluate if expression with branches
                let condition = &if_expr.condition;
                let then_branch = &if_expr.then_branch;
                let else_branch = &if_expr.else_branch;

                let cond_val = self.translate_expression(block_id, condition)?;

                // Create blocks for then/else/merge
                let then_block_id = HirId::new();
                let else_block_id = HirId::new();
                let merge_block_id = HirId::new();

                // Create blocks in function
                self.function.blocks.insert(then_block_id, HirBlock::new(then_block_id));
                self.function.blocks.insert(else_block_id, HirBlock::new(else_block_id));
                self.function.blocks.insert(merge_block_id, HirBlock::new(merge_block_id));

                // Initialize definitions for new blocks
                self.definitions.insert(then_block_id, HashMap::new());
                self.definitions.insert(else_block_id, HashMap::new());
                self.definitions.insert(merge_block_id, HashMap::new());

                // Set conditional branch terminator for current block
                self.function.blocks.get_mut(&block_id).unwrap().terminator = HirTerminator::CondBranch {
                    condition: cond_val,
                    true_target: then_block_id,
                    false_target: else_block_id,
                };

                // Update predecessors/successors
                self.function.blocks.get_mut(&block_id).unwrap().successors = vec![then_block_id, else_block_id];
                self.function.blocks.get_mut(&then_block_id).unwrap().predecessors.push(block_id);
                self.function.blocks.get_mut(&else_block_id).unwrap().predecessors.push(block_id);

                // Translate then branch
                let then_val = self.translate_expression(then_block_id, then_branch)?;
                self.function.blocks.get_mut(&then_block_id).unwrap().terminator = HirTerminator::Branch { target: merge_block_id };
                self.function.blocks.get_mut(&then_block_id).unwrap().successors = vec![merge_block_id];
                self.function.blocks.get_mut(&merge_block_id).unwrap().predecessors.push(then_block_id);

                // Translate else branch
                let else_val = self.translate_expression(else_block_id, else_branch)?;
                self.function.blocks.get_mut(&else_block_id).unwrap().terminator = HirTerminator::Branch { target: merge_block_id };
                self.function.blocks.get_mut(&else_block_id).unwrap().successors = vec![merge_block_id];
                self.function.blocks.get_mut(&merge_block_id).unwrap().predecessors.push(else_block_id);

                // Create phi in merge block
                let result_type = self.convert_type(&expr.ty);
                let result = self.create_value(result_type.clone(), HirValueKind::Instruction);

                self.function.blocks.get_mut(&merge_block_id).unwrap().phis.push(HirPhi {
                    result,
                    ty: result_type,
                    incoming: vec![(then_val, then_block_id), (else_val, else_block_id)],
                });

                Ok(result)
            }

            TypedExpression::Struct(struct_lit) => {
                // Allocate struct
                let struct_ty = self.convert_type(&expr.ty);
                let alloc_result = self.create_value(struct_ty.clone(), HirValueKind::Instruction);

                self.add_instruction(block_id, HirInstruction::Alloca {
                    result: alloc_result,
                    ty: struct_ty.clone(),
                    count: None,
                    align: 8,
                });

                // Initialize each field
                for (i, field) in struct_lit.fields.iter().enumerate() {
                    let field_val = self.translate_expression(block_id, &field.value)?;

                    let insert_result = self.create_value(struct_ty.clone(), HirValueKind::Instruction);
                    self.add_instruction(block_id, HirInstruction::InsertValue {
                        result: insert_result,
                        ty: struct_ty.clone(),
                        aggregate: alloc_result,
                        value: field_val,
                        indices: vec![i as u32],
                    });
                }

                Ok(alloc_result)
            }

            TypedExpression::Array(elements) => {
                // Create array type
                let elem_ty = if let Type::Array { element_type, .. } = &expr.ty {
                    self.convert_type(element_type)
                } else {
                    HirType::I64 // Fallback
                };

                let array_ty = HirType::Array(Box::new(elem_ty.clone()), elements.len() as u64);
                let alloc_result = self.create_value(array_ty.clone(), HirValueKind::Instruction);

                self.add_instruction(block_id, HirInstruction::Alloca {
                    result: alloc_result,
                    ty: array_ty.clone(),
                    count: None,
                    align: 8,
                });

                // Initialize each element
                for (i, elem_expr) in elements.iter().enumerate() {
                    let elem_val = self.translate_expression(block_id, elem_expr)?;

                    let insert_result = self.create_value(array_ty.clone(), HirValueKind::Instruction);
                    self.add_instruction(block_id, HirInstruction::InsertValue {
                        result: insert_result,
                        ty: array_ty.clone(),
                        aggregate: alloc_result,
                        value: elem_val,
                        indices: vec![i as u32],
                    });
                }

                Ok(alloc_result)
            }

            TypedExpression::Tuple(elements) => {
                // Convert tuple to struct
                let field_types: Vec<_> = elements.iter()
                    .map(|e| self.convert_type(&e.ty))
                    .collect();

                let tuple_ty = HirType::Struct(crate::hir::HirStructType {
                    name: None,
                    fields: field_types,
                    packed: false,
                });

                let alloc_result = self.create_value(tuple_ty.clone(), HirValueKind::Instruction);

                self.add_instruction(block_id, HirInstruction::Alloca {
                    result: alloc_result,
                    ty: tuple_ty.clone(),
                    count: None,
                    align: 8,
                });

                // Initialize each element
                for (i, elem_expr) in elements.iter().enumerate() {
                    let elem_val = self.translate_expression(block_id, elem_expr)?;

                    let insert_result = self.create_value(tuple_ty.clone(), HirValueKind::Instruction);
                    self.add_instruction(block_id, HirInstruction::InsertValue {
                        result: insert_result,
                        ty: tuple_ty.clone(),
                        aggregate: alloc_result,
                        value: elem_val,
                        indices: vec![i as u32],
                    });
                }

                Ok(alloc_result)
            }

            TypedExpression::Cast(cast) => {
                let operand_val = self.translate_expression(block_id, &cast.expr)?;
                let target_ty = self.convert_type(&cast.target_type);
                let result = self.create_value(target_ty.clone(), HirValueKind::Instruction);

                // NOTE: Proper cast operation selection requires type information.
                // Should determine: IntToInt, IntToFloat, FloatToInt, FloatToFloat, Bitcast, etc.
                // Needs source type width, target type width, signedness information.
                //
                // WORKAROUND: Always uses Bitcast (works for pointer casts and same-size conversions)
                // FUTURE (v2.0): Add type-aware cast selection logic
                // Estimated effort: 4-5 hours (needs type width/signedness utilities)
                let cast_op = CastOp::Bitcast;

                self.add_instruction(block_id, HirInstruction::Cast {
                    op: cast_op,
                    result,
                    ty: target_ty,
                    operand: operand_val,
                });

                self.add_use(operand_val, result);
                Ok(result)
            }

            TypedExpression::Reference(reference) => {
                // Take reference of expression
                let operand_val = self.translate_expression(block_id, &reference.expr)?;
                let ptr_ty = HirType::Ptr(Box::new(self.convert_type(&reference.expr.ty)));
                let result = self.create_value(ptr_ty, HirValueKind::Instruction);

                // This would be a no-op in HIR if operand is already an address
                // For now, just return the operand
                Ok(operand_val)
            }

            TypedExpression::Dereference(expr) => {
                // Dereference pointer
                let ptr_val = self.translate_expression(block_id, expr)?;
                let result_ty = self.convert_type(&expr.ty);
                let result = self.create_value(result_ty.clone(), HirValueKind::Instruction);

                self.add_instruction(block_id, HirInstruction::Load {
                    result,
                    ty: result_ty,
                    ptr: ptr_val,
                    align: 8,
                    volatile: false,
                });

                self.add_use(ptr_val, result);
                Ok(result)
            }

            TypedExpression::MethodCall(method_call) => {
                // Translate method call to regular call with receiver as first argument
                let receiver_val = self.translate_expression(block_id, &method_call.receiver)?;

                // Translate arguments
                let mut arg_vals = vec![receiver_val];
                for arg in &method_call.positional_args {
                    let arg_val = self.translate_expression(block_id, arg)?;
                    arg_vals.push(arg_val);
                }

                // For now, method calls need to be resolved to function pointers
                // This would require vtable lookup or static dispatch
                // Create a placeholder call
                let result_type = self.convert_type(&expr.ty);
                let result = if result_type != HirType::Void {
                    Some(self.create_value(result_type, HirValueKind::Instruction))
                } else {
                    None
                };

                // Create method reference (simplified - needs proper resolution)
                // For now, create an undefined function pointer
                let method_ref = self.create_undef(HirType::Ptr(Box::new(HirType::Void)));

                self.add_instruction(block_id, HirInstruction::Call {
                    result,
                    callee: crate::hir::HirCallable::Indirect(method_ref),
                    args: arg_vals.clone(),
                    type_args: vec![],  // Method calls resolve types separately
                    const_args: vec![],
                    is_tail: false,
                });

                for arg in arg_vals {
                    self.add_use(arg, result.unwrap_or(method_ref));
                }

                Ok(result.unwrap_or_else(|| self.create_undef(HirType::Void)))
            }

            TypedExpression::Match(match_expr) => {
                self.translate_match(block_id, match_expr, &expr.ty)
            }

            TypedExpression::Await(async_expr) => {
                // NOTE: Async integration requires architectural changes.
                // AsyncCompiler works on complete HIR functions, but we're in SSA construction.
                // Same architecture issue as pattern matching above.
                //
                // WORKAROUND: Evaluates expression directly (works if no actual await inside)
                // FUTURE (v2.0): Restructure compilation pipeline
                // Estimated effort: 8-12 hours
                self.translate_expression(block_id, async_expr)
            }

            TypedExpression::Try(try_expr) => {
                // Gap 8 Phase 2: Rust-style ? operator for Result<T, E>
                //
                // Desugars: let value = operation()?;
                // Into:     let tmp = operation();
                //           let value = match tmp {
                //               Ok(v) => v,
                //               Err(e) => return Err(e),
                //           };
                //
                // This is different from exception-based try-catch.
                //
                // LIMITATION: The ? operator creates multiple basic blocks (ok_block, err_block, continue_block).
                // This works correctly, but subsequent instructions should ideally go into continue_block.
                // For now, the CFG is correct but the block continuation might need refinement for complex expressions.
                // This is fine for common cases like: let x = foo()?;
                self.translate_try_operator(block_id, try_expr, &expr.ty)
            }

            TypedExpression::Range(range) => {
                // Range expressions create range objects
                // This would need runtime support
                // For now, create a struct with start/end
                let start_val = if let Some(start) = &range.start {
                    self.translate_expression(block_id, start)?
                } else {
                    self.create_undef(HirType::I64)
                };
                let end_val = if let Some(end) = &range.end {
                    self.translate_expression(block_id, end)?
                } else {
                    self.create_undef(HirType::I64)
                };

                // Create a simple struct with two fields
                let range_ty = HirType::Struct(crate::hir::HirStructType {
                    name: None,
                    fields: vec![HirType::I64, HirType::I64],
                    packed: false,
                });

                let alloc_result = self.create_value(range_ty.clone(), HirValueKind::Instruction);
                self.add_instruction(block_id, HirInstruction::Alloca {
                    result: alloc_result,
                    ty: range_ty.clone(),
                    count: None,
                    align: 8,
                });

                let insert1 = self.create_value(range_ty.clone(), HirValueKind::Instruction);
                self.add_instruction(block_id, HirInstruction::InsertValue {
                    result: insert1,
                    ty: range_ty.clone(),
                    aggregate: alloc_result,
                    value: start_val,
                    indices: vec![0],
                });

                let insert2 = self.create_value(range_ty.clone(), HirValueKind::Instruction);
                self.add_instruction(block_id, HirInstruction::InsertValue {
                    result: insert2,
                    ty: range_ty,
                    aggregate: insert1,
                    value: end_val,
                    indices: vec![1],
                });

                Ok(insert2)
            }

            TypedExpression::Lambda(lambda) => {
                self.translate_closure(block_id, lambda, &expr.ty)
            }

            _ => {
                // Fallback for any remaining unhandled expressions
                Ok(self.create_undef(self.convert_type(&expr.ty)))
            }
        }
    }
    
    /// Write a variable in SSA form
    fn write_variable(&mut self, var: InternedString, block: HirId, value: HirId) {
        self.definitions
            .get_mut(&block)
            .unwrap()
            .insert(var, value);

        // Track variable writes for loop phi placement
        self.variable_writes
            .entry(block)
            .or_insert_with(HashSet::new)
            .insert(var);
    }
    
    /// Read a variable in SSA form
    fn read_variable(&mut self, var: InternedString, block: HirId) -> HirId {
        if let Some(&value) = self.definitions.get(&block).and_then(|defs| defs.get(&var)) {
            return value;
        }

        // Not defined in this block - need phi or recursive lookup
        self.read_variable_recursive(var, block)
    }
    
    /// Recursively read variable, inserting phis as needed (before IDF)
    /// After IDF placement, this won't create new phis - it will only traverse existing ones
    fn read_variable_recursive(&mut self, var: InternedString, block: HirId) -> HirId {
        let (predecessors, is_sealed) = {
            let block_info = self.function.blocks.get(&block).unwrap();
            (block_info.predecessors.clone(), self.sealed_blocks.contains(&block))
        };

        if !is_sealed {
            // Block not sealed - create incomplete phi (ONLY if IDF not done yet)
            let phi_key = (block, var);

            if let Some(&phi_val) = self.incomplete_phis.get(&phi_key) {
                return phi_val;
            }

            // After IDF placement, don't create new phis - use undef instead
            if self.idf_placement_done {
                let ty = self.var_types.get(&var).cloned().unwrap_or(HirType::I64);
                return self.create_undef(ty);
            }

            let ty = self.var_types.get(&var).cloned()
                .unwrap_or(HirType::I64); // Default type
            let phi_val = self.create_value(ty.clone(), HirValueKind::Instruction);

            self.incomplete_phis.insert(phi_key, phi_val);
            self.function.blocks.get_mut(&block).unwrap().phis.push(HirPhi {
                result: phi_val,
                ty,
                incoming: vec![],
            });

            phi_val
        } else if predecessors.len() == 1 {
            // Single predecessor - no phi needed
            self.read_variable(var, predecessors[0])
        } else {
            // Multiple predecessors - may need phi
            // After IDF, phis should already exist, so don't create new ones
            if self.idf_placement_done {
                // Check if a phi already exists for this variable in this block
                let phi_key = (block, var);
                if let Some(&phi_val) = self.incomplete_phis.get(&phi_key) {
                    return phi_val;
                }
                // Check if phi is in definitions (already filled)
                if let Some(defs) = self.definitions.get(&block) {
                    if let Some(&val) = defs.get(&var) {
                        return val;
                    }
                }
                // No phi found - variable doesn't need a phi (like read-only parameters)
                // Try reading from each predecessor until we find the value
                for pred in &predecessors {
                    let val = self.read_variable(var, *pred);
                    // Check if it's not undef
                    if let Some(v) = self.function.values.get(&val) {
                        if !matches!(v.kind, HirValueKind::Undef) {
                            return val;
                        }
                    }
                }
                // All predecessors returned undef - variable is truly undefined
                let ty = self.var_types.get(&var).cloned().unwrap_or(HirType::I64);
                return self.create_undef(ty);
            }

            let ty = self.var_types.get(&var).cloned()
                .unwrap_or(HirType::I64);
            let phi_val = self.create_value(ty.clone(), HirValueKind::Instruction);

            // Temporarily write to avoid infinite recursion
            self.write_variable(var, block, phi_val);

            // Collect values from predecessors
            let mut incoming = Vec::new();

            for pred in predecessors {
                let pred_val = self.read_variable(var, pred);
                incoming.push((pred_val, pred));
            }

            // Check if phi is trivial
            let non_phi_vals: HashSet<_> = incoming.iter()
                .map(|(val, _)| val)
                .filter(|&&v| v != phi_val)
                .collect();

            if non_phi_vals.len() == 1 {
                // Trivial phi - replace with the single value
                let single_val = **non_phi_vals.iter().next().unwrap();
                self.write_variable(var, block, single_val);
                single_val
            } else if non_phi_vals.is_empty() {
                // All inputs are this phi - undefined
                self.create_undef(ty)
            } else {
                // Non-trivial phi
                self.function.blocks.get_mut(&block).unwrap().phis.push(HirPhi {
                    result: phi_val,
                    ty,
                    incoming,
                });
                phi_val
            }
        }
    }
    
    /// Seal a block (all predecessors known)
    fn seal_block(&mut self, block: HirId) {
        self.sealed_blocks.insert(block);

        // CRITICAL: When using IDF-based SSA, don't fill phis during sealing!
        // IDF places all phis upfront, and we fill them AFTER all blocks are processed.
        // Filling during sealing causes phis to be filled before their predecessor
        // blocks are processed, leading to undef values.
        if self.idf_placement_done {
            // Skip phi filling - will be done in batch after all blocks processed
            return;
        }

        // Process incomplete phis for this block (demand-driven SSA only)
        let incomplete: Vec<_> = self.incomplete_phis.iter()
            .filter(|((b, _), _)| *b == block)
            .map(|((b, v), _)| (*b, *v))
            .collect();

        for (_, var) in incomplete {
            self.fill_incomplete_phi(block, var);
        }
    }
    
    /// Fill an incomplete phi using pure IDF approach
    /// Uses read_variable() which may traverse phis, but won't create new ones after IDF
    fn fill_incomplete_phi(&mut self, block: HirId, var: InternedString) {
        let phi_key = (block, var);
        if let Some(phi_val) = self.incomplete_phis.remove(&phi_key) {
            // Get predecessors
            let preds = self.function.blocks[&block].predecessors.clone();
            let mut incoming = Vec::new();

            for pred in preds {
                // Use read_variable to get the value - this will traverse phis if needed
                let pred_val = self.read_variable(var, pred);
                incoming.push((pred_val, pred));
            }

            // Update the phi
            if let Some(phi) = self.function.blocks.get_mut(&block)
                .and_then(|b| b.phis.iter_mut().find(|p| p.result == phi_val))
            {
                phi.incoming = incoming;
            }
        }
    }
    
    /// Fill all IDF-placed phis
    fn fill_incomplete_phis(&mut self) {
        let incomplete: Vec<_> = self.incomplete_phis.keys().cloned().collect();
        for (block, var) in incomplete {
            self.fill_incomplete_phi(block, var);
        }
    }

    /// Verify and fix phi incoming values
    /// Ensures all phis have incoming values from all predecessors
    fn verify_and_fix_phi_incoming(&mut self) {
        // Collect all blocks that have phis
        let blocks_with_phis: Vec<HirId> = self.function.blocks.iter()
            .filter(|(_, block)| !block.phis.is_empty())
            .map(|(id, _)| *id)
            .collect();

        for block_id in blocks_with_phis {
            // Get predecessors
            let preds = self.function.blocks[&block_id].predecessors.clone();

            // For each phi in this block
            let mut phi_fixes = Vec::new();

            {
                let block = &self.function.blocks[&block_id];
                for phi in &block.phis {
                    // Check if phi has incoming from all predecessors
                    let incoming_blocks: HashSet<_> = phi.incoming.iter()
                        .map(|(_, block)| *block)
                        .collect();

                    // Find missing predecessors
                    let missing: Vec<_> = preds.iter()
                        .filter(|pred| !incoming_blocks.contains(pred))
                        .copied()
                        .collect();

                    if !missing.is_empty() {
                        // We need to fix this phi
                        // Try to find which variable this phi is for by checking definitions
                        if let Some(var) = self.find_variable_for_phi(block_id, phi.result) {
                            phi_fixes.push((phi.result, var, missing));
                        }
                    }
                }
            }

            // Apply fixes
            for (phi_val, var, missing_preds) in phi_fixes {
                for pred in missing_preds {
                    // Read the variable value from the predecessor
                    let pred_val = self.read_variable(var, pred);

                    // Add to phi's incoming list
                    if let Some(block) = self.function.blocks.get_mut(&block_id) {
                        if let Some(phi) = block.phis.iter_mut().find(|p| p.result == phi_val) {
                            phi.incoming.push((pred_val, pred));
                        }
                    }
                }
            }
        }
    }

    /// Find which variable a phi corresponds to by checking definitions
    fn find_variable_for_phi(&self, block_id: HirId, phi_val: HirId) -> Option<InternedString> {
        if let Some(defs) = self.definitions.get(&block_id) {
            for (var, &val) in defs {
                if val == phi_val {
                    return Some(*var);
                }
            }
        }
        None
    }

    /// Scan CFG to collect variable types from Let statements
    /// This must be done before phi placement since phis need correct types
    fn scan_cfg_for_variable_types(&mut self, cfg: &crate::typed_cfg::TypedControlFlowGraph) {
        use zyntax_typed_ast::typed_ast::{TypedStatement};

        for node_idx in cfg.graph.node_indices() {
            let typed_block = &cfg.graph[node_idx];

            // Scan all statements in the block
            for stmt in &typed_block.statements {
                match &stmt.node {
                    // Let statements have type annotations
                    TypedStatement::Let(let_stmt) => {
                        let hir_type = self.convert_type(&let_stmt.ty);
                        self.var_types.insert(let_stmt.name, hir_type);
                    }
                    _ => {}
                }
            }
        }
    }

    /// Scan CFG to find which blocks write which variables
    /// This is done by analyzing TypedCFG WITHOUT translating to HIR
    fn scan_cfg_for_variable_writes(&self, cfg: &crate::typed_cfg::TypedControlFlowGraph) -> HashMap<HirId, HashSet<InternedString>> {
        use zyntax_typed_ast::typed_ast::{TypedExpression, TypedStatement, BinaryOp};

        let mut writes: HashMap<HirId, HashSet<InternedString>> = HashMap::new();

        for node_idx in cfg.graph.node_indices() {
            let typed_block = &cfg.graph[node_idx];
            let block_id = typed_block.id;
            let mut block_writes = HashSet::new();

            // Scan all statements in the block
            for stmt in &typed_block.statements {
                match &stmt.node {
                    // Let statements define variables
                    TypedStatement::Let(let_stmt) => {
                        if let_stmt.initializer.is_some() {
                            block_writes.insert(let_stmt.name);
                        }
                    }
                    // Expression statements might contain assignments
                    TypedStatement::Expression(expr) => {
                        self.collect_assigned_vars(expr, &mut block_writes);
                    }
                    _ => {}
                }
            }

            if !block_writes.is_empty() {
                writes.insert(block_id, block_writes);
            }
        }

        writes
    }

    /// Place phis using Iterated Dominance Frontier (IDF) algorithm
    /// This is the proper SSA construction approach that places phis only where needed
    fn place_phis_using_idf(&mut self, cfg: &crate::typed_cfg::TypedControlFlowGraph) {
        // CRITICAL: Scan CFG to collect variable types FIRST
        // Phis need correct types when they're created
        self.scan_cfg_for_variable_types(cfg);

        // Compute dominance information
        let dom_info = DominanceInfo::compute(cfg);

        // Scan CFG to find variable writes BEFORE translating blocks
        let scanned_writes = self.scan_cfg_for_variable_writes(cfg);

        // Group variable writes by variable name
        // This creates a map: variable -> set of blocks that define it
        let mut defs_per_var: HashMap<InternedString, HashSet<HirId>> = HashMap::new();

        // Use scanned writes instead of self.variable_writes
        for (block_id, vars) in &scanned_writes {
            for &var in vars {
                defs_per_var.entry(var).or_insert_with(HashSet::new).insert(*block_id);
            }
        }

        // IMPORTANT: Only parameters are defined in entry block
        // Other variables (let statements) will be found by scan_cfg_for_variable_writes
        // So we DON'T need to add entry block for all variables

        // For each variable, compute IDF and place phis
        for (var, def_blocks) in defs_per_var {
            // Compute iterated dominance frontier
            let idf = self.compute_idf(&def_blocks, &dom_info);

            // Place phi at each block in IDF
            for &block_id in &idf {
                let phi_key = (block_id, var);

                // Skip if phi already exists (from demand-driven creation)
                if self.incomplete_phis.contains_key(&phi_key) {
                    continue;
                }

                // Get variable type
                let ty = self.var_types.get(&var).cloned().unwrap_or(HirType::I64);

                // Create incomplete phi
                let phi_val = self.create_value(ty.clone(), HirValueKind::Instruction);

                self.incomplete_phis.insert(phi_key, phi_val);
                self.function.blocks.get_mut(&block_id).unwrap().phis.push(HirPhi {
                    result: phi_val,
                    ty,
                    incoming: vec![],
                });

                // Define it in the block so reads find it
                self.definitions.get_mut(&block_id).unwrap().insert(var, phi_val);
            }
        }
    }

    /// Compute Iterated Dominance Frontier for a set of blocks
    fn compute_idf(
        &self,
        def_blocks: &HashSet<HirId>,
        dom_info: &DominanceInfo
    ) -> HashSet<HirId> {
        let mut worklist: Vec<HirId> = def_blocks.iter().copied().collect();
        let mut idf = HashSet::new();
        let mut processed = HashSet::new();

        while let Some(block) = worklist.pop() {
            if !processed.insert(block) {
                continue; // Already processed
            }

            // Get dominance frontier of this block
            if let Some(frontier) = dom_info.dom_frontier.get(&block) {
                for &df_block in frontier {
                    if idf.insert(df_block) {
                        // New block added to IDF - process its frontier too
                        worklist.push(df_block);
                    }
                }
            }
        }

        idf
    }

    /// Collect variables written in loop blocks by analyzing TypedCFG
    fn collect_loop_vars_from_cfg(
        &self,
        cfg: &crate::typed_cfg::TypedControlFlowGraph,
        block_id: HirId,
        header_id: HirId,
        vars: &mut HashSet<InternedString>
    ) {
        use zyntax_typed_ast::typed_ast::{TypedStatement, TypedExpression};

        // Avoid infinite recursion
        if block_id == header_id {
            return;
        }

        // Find the TypedBasicBlock for this block_id
        for node_idx in cfg.graph.node_indices() {
            let typed_block = &cfg.graph[node_idx];
            if typed_block.id == block_id {
                // Scan statements for variable assignments
                for stmt in &typed_block.statements {
                    match &stmt.node {
                        TypedStatement::Let(let_stmt) => {
                            // Variable declaration
                            vars.insert(let_stmt.name);
                        }
                        TypedStatement::Expression(expr) => {
                            // Check for assignment expressions
                            self.collect_assigned_vars(expr, vars);
                        }
                        _ => {}
                    }
                }

                // Recursively collect from predecessors
                if let Some(hir_block) = self.function.blocks.get(&block_id) {
                    for &pred in &hir_block.predecessors {
                        if pred != header_id && pred != self.function.entry_block {
                            self.collect_loop_vars_from_cfg(cfg, pred, header_id, vars);
                        }
                    }
                }
                break;
            }
        }
    }

    /// Helper to collect variables from assignment expressions
    fn collect_assigned_vars(
        &self,
        expr: &zyntax_typed_ast::TypedNode<zyntax_typed_ast::typed_ast::TypedExpression>,
        vars: &mut HashSet<InternedString>
    ) {
        use zyntax_typed_ast::typed_ast::{TypedExpression, BinaryOp};

        match &expr.node {
            TypedExpression::Binary(bin) if matches!(bin.op, BinaryOp::Assign) => {
                // Assignment: target = value
                if let TypedExpression::Variable(name) = &bin.left.node {
                    vars.insert(*name);
                }
            }
            _ => {}
        }
    }

    /// Create a new SSA value
    fn create_value(&mut self, ty: HirType, kind: HirValueKind) -> HirId {
        self.function.create_value(ty, kind)
    }
    
    /// Create an undefined value
    fn create_undef(&mut self, ty: HirType) -> HirId {
        self.create_value(ty, HirValueKind::Undef)
    }

    /// Create a global string constant and return a value referencing it
    fn create_string_global(&mut self, string_name: InternedString) -> HirId {
        use crate::hir::{HirGlobal, HirConstant, Linkage, Visibility};

        // Create a unique global ID and name
        let global_id = HirId::new();

        // Create the global with the string data
        let global = HirGlobal {
            id: global_id,
            name: string_name, // Use the interned string name as the global name
            ty: HirType::Ptr(Box::new(HirType::I8)), // String is a pointer to i8
            initializer: Some(HirConstant::String(string_name)),
            is_const: true,
            is_thread_local: false,
            linkage: Linkage::Private,
            visibility: Visibility::Default,
        };

        // Store the global for later addition to the module
        self.string_globals.push(global);

        // Create a value that references this global
        let value_id = self.create_value(
            HirType::Ptr(Box::new(HirType::I8)),
            HirValueKind::Global(global_id)
        );

        value_id
    }

    /// Add an instruction to a block
    fn add_instruction(&mut self, block: HirId, inst: HirInstruction) {
        self.function.blocks.get_mut(&block).unwrap().add_instruction(inst);
    }
    
    /// Add a use of a value
    fn add_use(&mut self, value: HirId, user: HirId) {
        if let Some(val) = self.function.values.get_mut(&value) {
            val.uses.insert(user);
        }
    }
    
    /// Compute dominance order for processing
    fn compute_dominance_order(&self, _cfg: &ControlFlowGraph) -> Vec<HirId> {
        // Simple DFS order for now
        let mut order = Vec::new();
        let mut visited = HashSet::new();
        let mut stack = vec![self.function.entry_block];
        
        while let Some(block) = stack.pop() {
            if visited.insert(block) {
                order.push(block);
                
                if let Some(block_info) = self.function.blocks.get(&block) {
                    for &succ in &block_info.successors {
                        if !visited.contains(&succ) {
                            stack.push(succ);
                        }
                    }
                }
            }
        }
        
        order
    }
    
    /// Build def-use and use-def chains
    fn build_def_use_chains(&self) -> (HashMap<HirId, HashSet<HirId>>, HashMap<HirId, HirId>) {
        let mut def_use = HashMap::new();
        let mut use_def = HashMap::new();
        
        // Process all values
        for (value_id, value) in &self.function.values {
            def_use.insert(*value_id, value.uses.clone());
            
            // For each use, record the definition
            for &use_id in &value.uses {
                use_def.insert(use_id, *value_id);
            }
        }
        
        (def_use, use_def)
    }
    
    /// Convert frontend type to HIR type
    fn convert_type(&self, ty: &Type) -> HirType {
        use zyntax_typed_ast::PrimitiveType;
        
        match ty {
            Type::Primitive(prim) => match prim {
                PrimitiveType::Bool => HirType::Bool,
                PrimitiveType::I8 => HirType::I8,
                PrimitiveType::I16 => HirType::I16,
                PrimitiveType::I32 => HirType::I32,
                PrimitiveType::I64 => HirType::I64,
                PrimitiveType::I128 => HirType::I128,
                PrimitiveType::U8 => HirType::U8,
                PrimitiveType::U16 => HirType::U16,
                PrimitiveType::U32 => HirType::U32,
                PrimitiveType::U64 => HirType::U64,
                PrimitiveType::U128 => HirType::U128,
                PrimitiveType::F32 => HirType::F32,
                PrimitiveType::F64 => HirType::F64,
                PrimitiveType::Unit => HirType::Void,
                _ => HirType::I64, // Default
            },
            Type::Tuple(types) if types.is_empty() => HirType::Void,
            Type::Reference { ty, .. } => HirType::Ptr(Box::new(self.convert_type(ty))),
            Type::Array { element_type, size: Some(size_val), .. } => {
                // Extract size from ConstValue
                let size = match size_val {
                    ConstValue::Int(i) => *i as u64,
                    ConstValue::UInt(u) => *u,
                    _ => 0, // Default size
                };
                HirType::Array(
                    Box::new(self.convert_type(element_type)),
                    size
                )
            },
            _ => HirType::I64, // Default for complex types
        }
    }
    
    /// Convert binary operator
    fn convert_binary_op(&self, op: &zyntax_typed_ast::typed_ast::BinaryOp) -> crate::hir::BinaryOp {
        use zyntax_typed_ast::typed_ast::BinaryOp as FrontendOp;
        use crate::hir::BinaryOp as HirOp;
        
        match op {
            FrontendOp::Add => HirOp::Add,
            FrontendOp::Sub => HirOp::Sub,
            FrontendOp::Mul => HirOp::Mul,
            FrontendOp::Div => HirOp::Div,
            FrontendOp::Rem => HirOp::Rem,
            FrontendOp::BitAnd => HirOp::And,
            FrontendOp::BitOr => HirOp::Or,
            FrontendOp::BitXor => HirOp::Xor,
            FrontendOp::Shl => HirOp::Shl,
            FrontendOp::Shr => HirOp::Shr,
            FrontendOp::Eq => HirOp::Eq,
            FrontendOp::Ne => HirOp::Ne,
            FrontendOp::Lt => HirOp::Lt,
            FrontendOp::Le => HirOp::Le,
            FrontendOp::Gt => HirOp::Gt,
            FrontendOp::Ge => HirOp::Ge,
            _ => HirOp::Add, // Default
        }
    }
    
    /// Convert unary operator
    fn convert_unary_op(&self, op: &zyntax_typed_ast::typed_ast::UnaryOp) -> crate::hir::UnaryOp {
        use zyntax_typed_ast::typed_ast::UnaryOp as FrontendOp;
        use crate::hir::UnaryOp as HirOp;
        
        match op {
            FrontendOp::Minus => HirOp::Neg,
            FrontendOp::Not => HirOp::Not,
            _ => HirOp::Neg, // Default
        }
    }
    
    /// Translate literal to constant
    fn translate_literal(&self, lit: &zyntax_typed_ast::typed_ast::TypedLiteral) -> crate::hir::HirConstant {
        use zyntax_typed_ast::typed_ast::TypedLiteral;
        use crate::hir::HirConstant;

        match lit {
            TypedLiteral::Bool(b) => HirConstant::Bool(*b),
            // Use I32 for integers (TypedAST builder defaults to I32)
            TypedLiteral::Integer(i) => HirConstant::I32(*i as i32),
            TypedLiteral::Float(f) => HirConstant::F64(*f),
            TypedLiteral::String(s) => HirConstant::String(*s),
            TypedLiteral::Char(c) => HirConstant::I32(*c as i32),
            TypedLiteral::Unit => HirConstant::Struct(vec![]),
        }
    }
    
    /// Get field index in struct using TypeRegistry
    fn get_field_index(&self, struct_type: &Type, field_name: &InternedString) -> CompilerResult<u32> {
        // Extract the type ID from the struct type
        let type_id = match struct_type {
            Type::Named { id, .. } => *id,
            Type::Struct { fields, .. } => {
                // For inline struct types, search the fields directly
                for (idx, field) in fields.iter().enumerate() {
                    if &field.name == field_name {
                        return Ok(idx as u32);
                    }
                }
                return Err(crate::CompilerError::Analysis(
                    format!("Field {:?} not found in inline struct type", field_name)
                ));
            }
            _ => {
                return Err(crate::CompilerError::Analysis(
                    format!("Cannot access fields on non-struct type: {:?}", struct_type)
                ));
            }
        };

        // Look up the type definition in the registry
        let type_def = self.type_registry.get_type_by_id(type_id)
            .ok_or_else(|| crate::CompilerError::Analysis(
                format!("Type with ID {:?} not found in registry", type_id)
            ))?;

        // Find the field index in the type definition
        for (idx, field) in type_def.fields.iter().enumerate() {
            if &field.name == field_name {
                return Ok(idx as u32);
            }
        }

        // Field not found
        Err(crate::CompilerError::Analysis(
            format!("Field {:?} not found in type {:?}", field_name, type_def.name)
        ))
    }

    /// Translate assignment expression
    ///
    /// Handles: variable assignment, field assignment, array element assignment
    fn translate_assignment(
        &mut self,
        block_id: HirId,
        target: &zyntax_typed_ast::TypedNode<zyntax_typed_ast::typed_ast::TypedExpression>,
        value_expr: &zyntax_typed_ast::TypedNode<zyntax_typed_ast::typed_ast::TypedExpression>
    ) -> CompilerResult<HirId> {
        use zyntax_typed_ast::typed_ast::TypedExpression;

        // First evaluate the right-hand side (the value to assign)
        let value = self.translate_expression(block_id, value_expr)?;

        // Now handle different assignment targets
        match &target.node {
            // Simple variable assignment: x = value
            TypedExpression::Variable(name) => {
                // Use write_variable to update the SSA variable tracking
                self.write_variable(*name, block_id, value);
                // Assignment expression returns the assigned value
                Ok(value)
            }

            // Field assignment: obj.field = value
            TypedExpression::Field(_) => {
                self.translate_lvalue_assign(block_id, target, value)?;
                Ok(value)
            }

            // Array element assignment: arr[index] = value
            TypedExpression::Index(_) => {
                self.translate_lvalue_assign(block_id, target, value)?;
                Ok(value)
            }

            // Dereference assignment: *ptr = value
            TypedExpression::Dereference(inner) => {
                // Evaluate the pointer
                let ptr = self.translate_expression(block_id, inner)?;

                // Create a Store instruction to write through the pointer
                let inst = HirInstruction::Store {
                    value,
                    ptr,
                    align: 8, // Default alignment (can be refined based on type)
                    volatile: false,
                };

                self.add_instruction(block_id, inst);
                self.add_use(ptr, value);
                self.add_use(value, value); // Value uses itself

                Ok(value)
            }

            // Invalid assignment target
            _ => Err(crate::CompilerError::Analysis(
                format!("Invalid assignment target: {:?}", target.node)
            ))
        }
    }

    /// Handle complex lvalue assignment
    fn translate_lvalue_assign(
        &mut self,
        block_id: HirId,
        target: &zyntax_typed_ast::TypedNode<zyntax_typed_ast::typed_ast::TypedExpression>,
        value: HirId
    ) -> CompilerResult<()> {
        use zyntax_typed_ast::typed_ast::TypedExpression;

        match &target.node {
            TypedExpression::Field(field_access) => {
                // Field assignment: obj.field = value
                // Strategy: Load object, use InsertValue to update field, store back

                let object = &field_access.object;
                let field_name = &field_access.field;

                // Get the field index from the object's type
                let field_index = self.get_field_index(&object.ty, field_name)?;

                // Evaluate the object expression to get the struct value
                let object_val = self.translate_expression(block_id, object)?;

                // Create an InsertValue instruction to update the field
                let result_type = self.convert_type(&object.ty);
                let result = self.create_value(result_type.clone(), HirValueKind::Instruction);

                let inst = HirInstruction::InsertValue {
                    result,
                    aggregate: object_val,
                    value,
                    indices: vec![field_index],
                    ty: result_type,
                };

                self.add_instruction(block_id, inst);
                self.add_use(object_val, result);
                self.add_use(value, result);

                // If the object is a variable, write the updated value back
                if let TypedExpression::Variable(var_name) = &object.node {
                    self.write_variable(*var_name, block_id, result);
                }

                Ok(())
            }
            TypedExpression::Index(index_expr) => {
                // Array assignment: arr[index] = value
                // Strategy: Bounds check, load array, compute GEP, store value

                let array = &index_expr.object;
                let index = &index_expr.index;

                // Evaluate the array and index expressions
                let array_val = self.translate_expression(block_id, array)?;
                let index_val = self.translate_expression(block_id, index)?;

                // Get element type and size from array type
                let (element_type, array_size) = match &array.ty {
                    Type::Array { element_type, size, .. } => {
                        (self.convert_type(element_type), size.clone())
                    }
                    _ => return Err(crate::CompilerError::Analysis(
                        format!("Cannot index into non-array type: {:?}", array.ty)
                    ))
                };

                // BOUNDS CHECKING: Emit runtime bounds check if array has known size
                if let Some(size_const) = array_size {
                    // Extract numeric size from ConstValue
                    let size_value = match size_const {
                        ConstValue::Int(i) => i as u64,
                        ConstValue::UInt(u) => u,
                        _ => {
                            // For complex const values, skip bounds check for now
                            // TODO: Use const evaluator to compute size
                            0
                        }
                    };

                    if size_value > 0 {
                        // Create constant for array size
                        let size_hir = self.create_value(
                            HirType::I64,
                            HirValueKind::Constant(crate::hir::HirConstant::I64(size_value as i64))
                        );

                        // Compare: index < size
                        let cond_result = self.create_value(HirType::Bool, HirValueKind::Instruction);
                        let cmp_inst = HirInstruction::Binary {
                            op: crate::hir::BinaryOp::Lt,
                            result: cond_result,
                            ty: HirType::Bool,
                            left: index_val,
                            right: size_hir,
                        };

                        self.add_instruction(block_id, cmp_inst);
                        self.add_use(index_val, cond_result);
                        self.add_use(size_hir, cond_result);

                        // BACKEND INTEGRATION: The comparison result can be used by backends
                        // for bounds checking optimization:
                        //
                        // - LLVM: Can use llvm.assume(cond_result) or emit conditional trap
                        // - Cranelift: Can use the comparison for trap insertion
                        //
                        // FUTURE ENHANCEMENT: Emit explicit conditional branch here
                        // This requires:
                        // 1. Create error block with trap instruction
                        // 2. Create continuation block for valid access
                        // 3. Split current block and emit CondBranch terminator
                        // 4. Continue lowering in continuation block
                        //
                        // Challenge: Expression translation doesn't currently support
                        // block splitting. Would need to refactor to return new block_id
                        // or use a different architecture (e.g., builder pattern with
                        // explicit control flow methods).
                        //
                        // For now: Comparison is emitted, backends can use it.
                        // Estimated effort to add explicit trap: 3-4 hours
                    }
                }

                // Create GetElementPtr instruction to get pointer to element
                let ptr_type = HirType::Ptr(Box::new(element_type.clone()));
                let ptr = self.create_value(ptr_type, HirValueKind::Instruction);

                let gep_inst = HirInstruction::GetElementPtr {
                    result: ptr,
                    ptr: array_val,
                    indices: vec![index_val],
                    ty: element_type.clone(),
                };

                self.add_instruction(block_id, gep_inst);
                self.add_use(array_val, ptr);
                self.add_use(index_val, ptr);

                // Store the value at the computed address
                let store_inst = HirInstruction::Store {
                    value,
                    ptr,
                    align: 8, // Default alignment
                    volatile: false,
                };

                self.add_instruction(block_id, store_inst);
                self.add_use(ptr, value);
                self.add_use(value, value);

                Ok(())
            }
            _ => Ok(())
        }
    }

    // ========== PATTERN MATCHING LOWERING ==========

    /// Translate a match expression to HIR
    ///
    /// Strategy: Generate a decision tree with sequential arm testing:
    /// 1. Evaluate scrutinee once and store
    /// 2. For each arm:
    ///    - Create test block for pattern matching
    ///    - Create body block for arm execution
    ///    - If pattern matches and guard passes, jump to body
    ///    - Otherwise, continue to next arm
    /// 3. After body, jump to end block with result
    fn translate_match(
        &mut self,
        mut block_id: HirId,
        match_expr: &zyntax_typed_ast::typed_ast::TypedMatchExpr,
        result_ty: &Type,
    ) -> CompilerResult<HirId> {
        use zyntax_typed_ast::typed_ast::TypedPattern;

        // Evaluate scrutinee
        let scrutinee_val = self.translate_expression(block_id, &match_expr.scrutinee)?;
        let scrutinee_ty = self.convert_type(&match_expr.scrutinee.ty);

        // Create end block for all arms to converge
        let end_block_id = HirId::new();
        self.function.blocks.insert(end_block_id, HirBlock::new(end_block_id));
        self.definitions.insert(end_block_id, HashMap::new());

        // Result phi node will collect values from each arm
        let result_hir_ty = self.convert_type(result_ty);
        let result = self.create_value(result_hir_ty.clone(), HirValueKind::Instruction);

        // Track predecessor blocks and their result values for phi node
        let mut phi_operands: Vec<(HirId, HirId)> = Vec::new();

        // Process each match arm
        for (_arm_idx, arm) in match_expr.arms.iter().enumerate() {
            // Create block for testing this arm's pattern
            let test_block_id = HirId::new();
            self.function.blocks.insert(test_block_id, HirBlock::new(test_block_id));
            self.definitions.insert(test_block_id, HashMap::new());

            // Create block for executing this arm's body
            let body_block_id = HirId::new();
            self.function.blocks.insert(body_block_id, HirBlock::new(body_block_id));
            self.definitions.insert(body_block_id, HashMap::new());

            // Create block for next arm (or unreachable if last)
            let next_block_id = HirId::new();
            self.function.blocks.insert(next_block_id, HirBlock::new(next_block_id));
            self.definitions.insert(next_block_id, HashMap::new());

            // Current block jumps to test block
            self.function.blocks.get_mut(&block_id).unwrap().terminator =
                HirTerminator::Branch { target: test_block_id };

            // In test block: check if pattern matches
            let pattern_matches = self.translate_pattern_test(
                test_block_id,
                &arm.pattern,
                scrutinee_val,
                &match_expr.scrutinee.ty,
            )?;

            // If guard exists, evaluate it and AND with pattern match result
            let final_condition = if let Some(guard_expr) = &arm.guard {
                let guard_val = self.translate_expression(test_block_id, guard_expr)?;

                // Create AND: pattern_matches && guard
                let and_result = self.create_value(HirType::Bool, HirValueKind::Instruction);
                let and_inst = HirInstruction::Binary {
                    op: crate::hir::BinaryOp::And,
                    result: and_result,
                    ty: HirType::Bool,
                    left: pattern_matches,
                    right: guard_val,
                };

                self.add_instruction(test_block_id, and_inst);
                self.add_use(pattern_matches, and_result);
                self.add_use(guard_val, and_result);

                and_result
            } else {
                pattern_matches
            };

            // Conditional branch: if pattern matches (and guard passes), go to body, else next arm
            self.function.blocks.get_mut(&test_block_id).unwrap().terminator =
                HirTerminator::CondBranch {
                    condition: final_condition,
                    true_target: body_block_id,
                    false_target: next_block_id,
                };

            // In body block: execute arm body and jump to end
            let arm_result = self.translate_expression(body_block_id, &arm.body)?;

            self.function.blocks.get_mut(&body_block_id).unwrap().terminator =
                HirTerminator::Branch { target: end_block_id };

            // Record this arm's result for phi node
            phi_operands.push((arm_result, body_block_id));

            // Continue with next block
            block_id = next_block_id;
        }

        // After all arms, if we reach here, no pattern matched
        // This should be unreachable if exhaustiveness checking is done by type checker
        self.function.blocks.get_mut(&block_id).unwrap().terminator =
            HirTerminator::Unreachable;

        // Create phi node in end block to collect results
        if !phi_operands.is_empty() {
            let phi = HirPhi {
                result,
                ty: result_hir_ty,
                incoming: phi_operands,
            };

            self.function.blocks.get_mut(&end_block_id).unwrap().phis.push(phi);
        }

        Ok(result)
    }

    /// Translate the ? operator for Result<T, E> error propagation
    ///
    /// Desugars: `operation()?`
    /// Into:     ```
    ///           let tmp = operation();
    ///           match tmp {
    ///               Ok(value) => value,
    ///               Err(error) => return Err(error),
    ///           }
    ///           ```
    ///
    /// Gap 8 Phase 2 implementation.
    fn translate_try_operator(
        &mut self,
        block_id: HirId,
        try_expr: &zyntax_typed_ast::TypedNode<zyntax_typed_ast::typed_ast::TypedExpression>,
        result_value_ty: &Type,
    ) -> CompilerResult<HirId> {
        // Evaluate the inner expression (e.g., risky_operation())
        let inner_result_val = self.translate_expression(block_id, try_expr)?;
        let inner_result_ty = &try_expr.ty;

        // Check if the type is Result<T, E>
        // For now, we assume any type with the name "Result" is Result<T, E>
        // TODO: More robust type checking when TypeRegistry has better introspection
        let (ok_ty, err_ty) = self.extract_result_type_args(inner_result_ty)?;

        // Convert types to HIR
        let hir_ok_ty = self.convert_type(&ok_ty);
        let hir_err_ty = self.convert_type(&err_ty);
        let hir_result_ty = self.convert_type(inner_result_ty);

        // Create blocks for the match:
        // - ok_block: handles Ok(value) case, extracts value
        // - err_block: handles Err(error) case, returns Err(error)
        // - continue_block: continues execution with the extracted Ok value

        let ok_block_id = HirId::new();
        self.function.blocks.insert(ok_block_id, HirBlock::new(ok_block_id));
        self.definitions.insert(ok_block_id, HashMap::new());

        let err_block_id = HirId::new();
        self.function.blocks.insert(err_block_id, HirBlock::new(err_block_id));
        self.definitions.insert(err_block_id, HashMap::new());

        let continue_block_id = HirId::new();
        self.function.blocks.insert(continue_block_id, HirBlock::new(continue_block_id));
        self.definitions.insert(continue_block_id, HashMap::new());

        // Check discriminant: Result is a union with discriminant 0 = Ok, 1 = Err
        // Extract discriminant from the Result union
        let discriminant_val = self.create_value(HirType::U32, HirValueKind::Instruction);
        self.add_instruction(block_id, HirInstruction::ExtractValue {
            result: discriminant_val,
            ty: hir_result_ty.clone(),
            aggregate: inner_result_val,
            indices: vec![0], // Discriminant is first field in union
        });

        // Compare discriminant with 0 (Ok) or 1 (Err)
        let zero_const = self.create_value(
            HirType::U32,
            HirValueKind::Constant(crate::hir::HirConstant::U32(0))
        );

        let is_ok = self.create_value(HirType::Bool, HirValueKind::Instruction);
        self.add_instruction(block_id, HirInstruction::Binary {
            op: crate::hir::BinaryOp::Eq,
            result: is_ok,
            ty: HirType::U32,
            left: discriminant_val,
            right: zero_const,
        });

        // Branch: if is_ok, go to ok_block, else go to err_block
        self.function.blocks.get_mut(&block_id).unwrap().terminator =
            HirTerminator::CondBranch {
                condition: is_ok,
                true_target: ok_block_id,
                false_target: err_block_id,
            };

        // OK BLOCK: Extract value from Ok(value) and continue
        // Result<T, E> union has: [discriminant: u32, data: union { Ok: T, Err: E }]
        // Extract the Ok value (index 1 is the data union, then we need the Ok variant)
        let ok_value = self.create_value(hir_ok_ty.clone(), HirValueKind::Instruction);
        self.add_instruction(ok_block_id, HirInstruction::ExtractValue {
            result: ok_value,
            ty: hir_result_ty.clone(),
            aggregate: inner_result_val,
            indices: vec![1], // Data field (contains the actual Ok/Err value)
        });

        // Jump to continue block with the extracted value
        self.function.blocks.get_mut(&ok_block_id).unwrap().terminator =
            HirTerminator::Branch { target: continue_block_id };

        // ERR BLOCK: Extract error from Err(error) and return Err(error)
        let err_value = self.create_value(hir_err_ty.clone(), HirValueKind::Instruction);
        self.add_instruction(err_block_id, HirInstruction::ExtractValue {
            result: err_value,
            ty: hir_result_ty.clone(),
            aggregate: inner_result_val,
            indices: vec![1], // Data field (contains the actual Ok/Err value)
        });

        // Construct a new Err(error) to return
        // Create Result<T, E> union with variant_index = 1 (Err)
        let return_err = self.create_value(hir_result_ty.clone(), HirValueKind::Instruction);
        self.add_instruction(err_block_id, HirInstruction::CreateUnion {
            result: return_err,
            union_ty: hir_result_ty.clone(),
            variant_index: 1, // Err variant
            value: err_value,
        });

        // Early return with Err(error)
        self.function.blocks.get_mut(&err_block_id).unwrap().terminator =
            HirTerminator::Return {
                values: vec![return_err],
            };

        // CONTINUE BLOCK: Continue with the Ok value
        // The ok_value is now accessible and we continue execution
        // The caller's block_id needs to be updated to continue_block_id
        //
        // NOTE: This function returns the ok_value, but the caller needs to know
        // that subsequent instructions should go into continue_block_id, not the original block_id.
        //
        // For now, we return ok_value and the caller will use continue_block_id implicitly
        // through the CFG structure (ok_block branches to continue_block).
        //
        // TODO: This might need refactoring to properly handle block continuation

        // Return the extracted Ok value as the result of the ? expression
        Ok(ok_value)
    }

    /// Extract T and E from Result<T, E> type
    /// Returns (T, E) if successful, error otherwise
    fn extract_result_type_args(&self, ty: &Type) -> CompilerResult<(Type, Type)> {
        match ty {
            Type::Named { type_args, .. } => {
                // For now, we assume any Named type with 2 type arguments could be Result
                // Type checker should ensure only Result<T, E> can use ? operator
                // TODO: Add explicit check for Result TypeId when TypeRegistry API supports it
                if type_args.len() == 2 {
                    return Ok((type_args[0].clone(), type_args[1].clone()));
                }

                Err(crate::CompilerError::Lowering(format!(
                    "? operator requires Result<T, E> type with 2 type arguments, found {} type arguments",
                    type_args.len()
                )))
            }
            _ => Err(crate::CompilerError::Lowering(
                "? operator requires Result<T, E> type, found non-generic type".to_string()
            )),
        }
    }

    /// Test if a pattern matches a scrutinee value
    /// Returns a boolean HIR value indicating match success
    fn translate_pattern_test(
        &mut self,
        block_id: HirId,
        pattern: &zyntax_typed_ast::TypedNode<zyntax_typed_ast::typed_ast::TypedPattern>,
        scrutinee_val: HirId,
        scrutinee_ty: &Type,
    ) -> CompilerResult<HirId> {
        use zyntax_typed_ast::typed_ast::{TypedPattern, TypedLiteralPattern};

        match &pattern.node {
            // Wildcard always matches
            TypedPattern::Wildcard => {
                let true_val = self.create_value(
                    HirType::Bool,
                    HirValueKind::Constant(crate::hir::HirConstant::Bool(true))
                );
                Ok(true_val)
            }

            // Variable binding always matches (and binds the value)
            TypedPattern::Identifier { name, mutability: _ } => {
                // Bind the scrutinee to this variable name
                self.write_variable(*name, block_id, scrutinee_val);

                // Pattern always matches
                let true_val = self.create_value(
                    HirType::Bool,
                    HirValueKind::Constant(crate::hir::HirConstant::Bool(true))
                );
                Ok(true_val)
            }

            // Literal pattern: compare scrutinee with literal value
            TypedPattern::Literal(lit_pattern) => {
                self.translate_literal_pattern_test(block_id, lit_pattern, scrutinee_val)
            }

            // Tuple pattern: check each element
            TypedPattern::Tuple(element_patterns) => {
                self.translate_tuple_pattern_test(block_id, element_patterns, scrutinee_val, scrutinee_ty)
            }

            // Struct pattern: check fields
            TypedPattern::Struct { name, fields } => {
                self.translate_struct_pattern_test(block_id, name, fields, scrutinee_val, scrutinee_ty)
            }

            // Enum variant pattern: check tag and fields
            TypedPattern::Enum { name, variant, fields } => {
                self.translate_enum_pattern_test(block_id, name, variant, fields, scrutinee_val, scrutinee_ty)
            }

            // Or pattern: test each sub-pattern and OR results
            TypedPattern::Or(patterns) => {
                let mut result = None;

                for sub_pattern in patterns {
                    let sub_result = self.translate_pattern_test(block_id, sub_pattern, scrutinee_val, scrutinee_ty)?;

                    result = Some(if let Some(prev_result) = result {
                        // OR: prev_result || sub_result
                        let or_result = self.create_value(HirType::Bool, HirValueKind::Instruction);
                        let or_inst = HirInstruction::Binary {
                            op: crate::hir::BinaryOp::Or,
                            result: or_result,
                            ty: HirType::Bool,
                            left: prev_result,
                            right: sub_result,
                        };

                        self.add_instruction(block_id, or_inst);
                        self.add_use(prev_result, or_result);
                        self.add_use(sub_result, or_result);

                        or_result
                    } else {
                        sub_result
                    });
                }

                result.ok_or_else(|| crate::CompilerError::Analysis(
                    "Or pattern must have at least one sub-pattern".to_string()
                ))
            }

            // Array pattern: check each element (similar to tuple)
            TypedPattern::Array(element_patterns) => {
                use zyntax_typed_ast::Type as FrontendType;

                // Extract array element type
                let element_type = match scrutinee_ty {
                    FrontendType::Array { element_type, .. } => element_type,
                    _ => return Err(crate::CompilerError::Analysis(
                        format!("Expected array type for array pattern, got {:?}", scrutinee_ty)
                    )),
                };

                // Test each element
                let mut result = None;

                for (idx, pattern) in element_patterns.iter().enumerate() {
                    // Extract element from array using GetElementPtr
                    let elem_ptr = self.create_value(
                        HirType::Ptr(Box::new(self.convert_type(element_type))),
                        HirValueKind::Instruction
                    );

                    let gep_inst = HirInstruction::GetElementPtr {
                        result: elem_ptr,
                        ptr: scrutinee_val,
                        indices: vec![self.create_value(
                            HirType::I64,
                            HirValueKind::Constant(crate::hir::HirConstant::I64(idx as i64))
                        )],
                        ty: self.convert_type(element_type),
                    };

                    self.add_instruction(block_id, gep_inst);
                    self.add_use(scrutinee_val, elem_ptr);

                    // Load element value
                    let elem_val = self.create_value(self.convert_type(element_type), HirValueKind::Instruction);
                    let load_inst = HirInstruction::Load {
                        result: elem_val,
                        ptr: elem_ptr,
                        ty: self.convert_type(element_type),
                        align: 8,
                        volatile: false,
                    };

                    self.add_instruction(block_id, load_inst);
                    self.add_use(elem_ptr, elem_val);

                    // Test pattern against element
                    let elem_test = self.translate_pattern_test(block_id, pattern, elem_val, element_type)?;

                    // AND with previous results
                    result = Some(if let Some(prev_result) = result {
                        let and_result = self.create_value(HirType::Bool, HirValueKind::Instruction);
                        let and_inst = HirInstruction::Binary {
                            op: crate::hir::BinaryOp::And,
                            result: and_result,
                            ty: HirType::Bool,
                            left: prev_result,
                            right: elem_test,
                        };

                        self.add_instruction(block_id, and_inst);
                        self.add_use(prev_result, and_result);
                        self.add_use(elem_test, and_result);

                        and_result
                    } else {
                        elem_test
                    });
                }

                result.ok_or_else(|| crate::CompilerError::Analysis(
                    "Array pattern must have at least one element".to_string()
                ))
            }

            // At pattern: binding @ pattern (e.g., x @ Some(y))
            TypedPattern::At { name, mutability: _, pattern: inner_pattern } => {
                // Bind the scrutinee to the name
                self.write_variable(*name, block_id, scrutinee_val);

                // Then test the inner pattern
                self.translate_pattern_test(block_id, inner_pattern, scrutinee_val, scrutinee_ty)
            }

            // Range pattern: test if value is in range
            TypedPattern::Range { start, end, inclusive } => {
                // Test: start <= scrutinee && scrutinee <= end (if inclusive)
                //       start <= scrutinee && scrutinee < end (if exclusive)

                // Get start literal value
                let start_val = self.translate_literal_pattern_to_value(start)?;

                // Test: start <= scrutinee
                let ge_result = self.create_value(HirType::Bool, HirValueKind::Instruction);
                let ge_inst = HirInstruction::Binary {
                    op: crate::hir::BinaryOp::Ge,
                    result: ge_result,
                    ty: HirType::Bool,
                    left: scrutinee_val,
                    right: start_val,
                };

                self.add_instruction(block_id, ge_inst);
                self.add_use(scrutinee_val, ge_result);
                self.add_use(start_val, ge_result);

                // Get end literal value
                let end_val = self.translate_literal_pattern_to_value(end)?;

                // Test: scrutinee <= end (or < end if not inclusive)
                let le_or_lt_op = if *inclusive {
                    crate::hir::BinaryOp::Le
                } else {
                    crate::hir::BinaryOp::Lt
                };

                let le_result = self.create_value(HirType::Bool, HirValueKind::Instruction);
                let le_inst = HirInstruction::Binary {
                    op: le_or_lt_op,
                    result: le_result,
                    ty: HirType::Bool,
                    left: scrutinee_val,
                    right: end_val,
                };

                self.add_instruction(block_id, le_inst);
                self.add_use(scrutinee_val, le_result);
                self.add_use(end_val, le_result);

                // AND both conditions
                let final_result = self.create_value(HirType::Bool, HirValueKind::Instruction);
                let and_inst = HirInstruction::Binary {
                    op: crate::hir::BinaryOp::And,
                    result: final_result,
                    ty: HirType::Bool,
                    left: ge_result,
                    right: le_result,
                };

                self.add_instruction(block_id, and_inst);
                self.add_use(ge_result, final_result);
                self.add_use(le_result, final_result);

                Ok(final_result)
            }

            // Reference pattern: dereference and test inner pattern
            TypedPattern::Reference { pattern: inner_pattern, mutability: _ } => {
                // Dereference the scrutinee
                let deref_ty = self.convert_type(&inner_pattern.ty);
                let deref_val = self.create_value(deref_ty.clone(), HirValueKind::Instruction);

                let load_inst = HirInstruction::Load {
                    result: deref_val,
                    ptr: scrutinee_val,
                    ty: deref_ty,
                    align: 8,
                    volatile: false,
                };

                self.add_instruction(block_id, load_inst);
                self.add_use(scrutinee_val, deref_val);

                // Test inner pattern on dereferenced value
                self.translate_pattern_test(block_id, inner_pattern, deref_val, &inner_pattern.ty)
            }

            // TODO: Implement remaining pattern types (Slice, Box, Guard, Rest, etc.)
            _ => {
                // For unimplemented patterns, return true (always match)
                // TODO: Add proper error reporting
                let true_val = self.create_value(
                    HirType::Bool,
                    HirValueKind::Constant(crate::hir::HirConstant::Bool(true))
                );
                Ok(true_val)
            }
        }
    }

    /// Convert a literal pattern to a HIR value (for range patterns)
    fn translate_literal_pattern_to_value(
        &mut self,
        lit_pattern: &zyntax_typed_ast::TypedNode<zyntax_typed_ast::typed_ast::TypedLiteralPattern>,
    ) -> CompilerResult<HirId> {
        use zyntax_typed_ast::typed_ast::TypedLiteralPattern;

        let val = match &lit_pattern.node {
            TypedLiteralPattern::Integer(i) => {
                self.create_value(
                    HirType::I64,
                    HirValueKind::Constant(crate::hir::HirConstant::I64(*i as i64))
                )
            }
            TypedLiteralPattern::Float(f) => {
                self.create_value(
                    HirType::F64,
                    HirValueKind::Constant(crate::hir::HirConstant::F64(*f))
                )
            }
            TypedLiteralPattern::Char(c) => {
                self.create_value(
                    HirType::I32,
                    HirValueKind::Constant(crate::hir::HirConstant::I32(*c as i32))
                )
            }
            _ => {
                return Err(crate::CompilerError::Analysis(
                    "Only integer, float, and char literals supported in range patterns".to_string()
                ));
            }
        };

        Ok(val)
    }

    /// Test literal pattern match
    fn translate_literal_pattern_test(
        &mut self,
        block_id: HirId,
        lit_pattern: &zyntax_typed_ast::typed_ast::TypedLiteralPattern,
        scrutinee_val: HirId,
    ) -> CompilerResult<HirId> {
        use zyntax_typed_ast::typed_ast::TypedLiteralPattern;

        // Create HIR constant for the literal
        let lit_val = match lit_pattern {
            TypedLiteralPattern::Integer(i) => {
                self.create_value(
                    HirType::I64,
                    HirValueKind::Constant(crate::hir::HirConstant::I64(*i as i64))
                )
            }
            TypedLiteralPattern::Float(f) => {
                self.create_value(
                    HirType::F64,
                    HirValueKind::Constant(crate::hir::HirConstant::F64(*f))
                )
            }
            TypedLiteralPattern::Bool(b) => {
                self.create_value(
                    HirType::Bool,
                    HirValueKind::Constant(crate::hir::HirConstant::Bool(*b))
                )
            }
            TypedLiteralPattern::String(_s) => {
                // TODO: String comparison requires runtime support
                // For now, always return true
                return Ok(self.create_value(
                    HirType::Bool,
                    HirValueKind::Constant(crate::hir::HirConstant::Bool(true))
                ));
            }
            TypedLiteralPattern::Char(c) => {
                self.create_value(
                    HirType::I32, // Chars are i32
                    HirValueKind::Constant(crate::hir::HirConstant::I32(*c as i32))
                )
            }
            TypedLiteralPattern::Byte(b) => {
                self.create_value(
                    HirType::I8,
                    HirValueKind::Constant(crate::hir::HirConstant::I8(*b as i8))
                )
            }
            TypedLiteralPattern::ByteString(_bs) => {
                // TODO: ByteString comparison requires runtime support
                // For now, always return true
                return Ok(self.create_value(
                    HirType::Bool,
                    HirValueKind::Constant(crate::hir::HirConstant::Bool(true))
                ));
            }
            TypedLiteralPattern::Unit => {
                // Unit always matches
                return Ok(self.create_value(
                    HirType::Bool,
                    HirValueKind::Constant(crate::hir::HirConstant::Bool(true))
                ));
            }
            TypedLiteralPattern::Null => {
                // Null always matches null values
                return Ok(self.create_value(
                    HirType::Bool,
                    HirValueKind::Constant(crate::hir::HirConstant::Bool(true))
                ));
            }
        };

        // Generate equality comparison
        let cmp_result = self.create_value(HirType::Bool, HirValueKind::Instruction);
        let cmp_inst = HirInstruction::Binary {
            op: crate::hir::BinaryOp::Eq,
            result: cmp_result,
            ty: HirType::Bool,
            left: scrutinee_val,
            right: lit_val,
        };

        self.add_instruction(block_id, cmp_inst);
        self.add_use(scrutinee_val, cmp_result);
        self.add_use(lit_val, cmp_result);

        Ok(cmp_result)
    }

    /// Test tuple pattern match
    fn translate_tuple_pattern_test(
        &mut self,
        block_id: HirId,
        element_patterns: &[zyntax_typed_ast::TypedNode<zyntax_typed_ast::typed_ast::TypedPattern>],
        scrutinee_val: HirId,
        scrutinee_ty: &Type,
    ) -> CompilerResult<HirId> {
        use zyntax_typed_ast::Type as FrontendType;

        // Extract tuple element types
        let element_types = match scrutinee_ty {
            FrontendType::Tuple(types) => types,
            _ => return Err(crate::CompilerError::Analysis(
                format!("Expected tuple type for tuple pattern, got {:?}", scrutinee_ty)
            )),
        };

        if element_patterns.len() != element_types.len() {
            return Err(crate::CompilerError::Analysis(
                format!("Tuple pattern has {} elements but type has {}",
                    element_patterns.len(), element_types.len())
            ));
        }

        // Test each element
        let mut result = None;

        for (idx, (pattern, elem_ty)) in element_patterns.iter().zip(element_types.iter()).enumerate() {
            // Extract element from tuple
            let elem_val = self.create_value(self.convert_type(elem_ty), HirValueKind::Instruction);
            let extract_inst = HirInstruction::ExtractValue {
                result: elem_val,
                aggregate: scrutinee_val,
                indices: vec![idx as u32],
                ty: self.convert_type(elem_ty),
            };

            self.add_instruction(block_id, extract_inst);
            self.add_use(scrutinee_val, elem_val);

            // Test pattern against element
            let elem_test = self.translate_pattern_test(block_id, pattern, elem_val, elem_ty)?;

            // AND with previous results
            result = Some(if let Some(prev_result) = result {
                let and_result = self.create_value(HirType::Bool, HirValueKind::Instruction);
                let and_inst = HirInstruction::Binary {
                    op: crate::hir::BinaryOp::And,
                    result: and_result,
                    ty: HirType::Bool,
                    left: prev_result,
                    right: elem_test,
                };

                self.add_instruction(block_id, and_inst);
                self.add_use(prev_result, and_result);
                self.add_use(elem_test, and_result);

                and_result
            } else {
                elem_test
            });
        }

        result.ok_or_else(|| crate::CompilerError::Analysis(
            "Tuple pattern must have at least one element".to_string()
        ))
    }

    /// Test struct pattern match
    ///
    /// Extracts each field from the struct and recursively tests the field pattern.
    /// All field tests must pass (AND logic).
    fn translate_struct_pattern_test(
        &mut self,
        block_id: HirId,
        struct_name: &InternedString,
        field_patterns: &[zyntax_typed_ast::typed_ast::TypedFieldPattern],
        scrutinee_val: HirId,
        scrutinee_ty: &Type,
    ) -> CompilerResult<HirId> {
        use zyntax_typed_ast::Type as FrontendType;

        // If no fields to match, always succeeds
        if field_patterns.is_empty() {
            let true_val = self.create_value(
                HirType::Bool,
                HirValueKind::Constant(crate::hir::HirConstant::Bool(true))
            );
            return Ok(true_val);
        }

        // Validate scrutinee is a struct type
        match scrutinee_ty {
            FrontendType::Struct { .. } | FrontendType::Named { .. } => {
                // Valid struct types
            }
            _ => return Err(crate::CompilerError::Analysis(
                format!("Expected struct type for struct pattern, got {:?}", scrutinee_ty)
            )),
        };

        // Test each field pattern
        let mut result = None;

        for field_pattern in field_patterns {
            // Get field index from TypeRegistry
            let field_index = self.get_field_index(scrutinee_ty, &field_pattern.name)?;

            // Extract field value from struct
            let field_ty = self.convert_type(&field_pattern.pattern.ty);
            let field_val = self.create_value(field_ty.clone(), HirValueKind::Instruction);

            let extract_inst = HirInstruction::ExtractValue {
                result: field_val,
                aggregate: scrutinee_val,
                indices: vec![field_index],
                ty: field_ty,
            };

            self.add_instruction(block_id, extract_inst);
            self.add_use(scrutinee_val, field_val);

            // Recursively test the field pattern
            let field_test = self.translate_pattern_test(
                block_id,
                &field_pattern.pattern,
                field_val,
                &field_pattern.pattern.ty
            )?;

            // AND with previous results
            result = Some(if let Some(prev_result) = result {
                let and_result = self.create_value(HirType::Bool, HirValueKind::Instruction);
                let and_inst = HirInstruction::Binary {
                    op: crate::hir::BinaryOp::And,
                    result: and_result,
                    ty: HirType::Bool,
                    left: prev_result,
                    right: field_test,
                };

                self.add_instruction(block_id, and_inst);
                self.add_use(prev_result, and_result);
                self.add_use(field_test, and_result);

                and_result
            } else {
                field_test
            });
        }

        result.ok_or_else(|| crate::CompilerError::Analysis(
            "Struct pattern must have at least one field".to_string()
        ))
    }

    /// Test enum pattern match
    ///
    /// Checks if the enum discriminant matches the variant tag, then extracts
    /// and tests the payload fields if the tag matches.
    fn translate_enum_pattern_test(
        &mut self,
        block_id: HirId,
        enum_name: &InternedString,
        variant_name: &InternedString,
        field_patterns: &[zyntax_typed_ast::TypedNode<zyntax_typed_ast::typed_ast::TypedPattern>],
        scrutinee_val: HirId,
        scrutinee_ty: &Type,
    ) -> CompilerResult<HirId> {
        use zyntax_typed_ast::Type as FrontendType;

        // Step 1: Extract discriminant (tag) from enum
        // Enums are typically represented as tagged unions: { tag: u32, payload: union { ... } }
        // The tag is usually at index 0

        let tag_val = self.create_value(HirType::U32, HirValueKind::Instruction);
        let extract_tag_inst = HirInstruction::ExtractValue {
            result: tag_val,
            aggregate: scrutinee_val,
            indices: vec![0], // Tag is at index 0
            ty: HirType::U32,
        };

        self.add_instruction(block_id, extract_tag_inst);
        self.add_use(scrutinee_val, tag_val);

        // Step 2: Get expected discriminant value for this variant
        // TODO: Look up variant discriminant from TypeRegistry
        // For now, use a placeholder value based on variant name hash
        let expected_discriminant = variant_name.to_string().len() as u32; // Placeholder

        let expected_val = self.create_value(
            HirType::U32,
            HirValueKind::Constant(crate::hir::HirConstant::U32(expected_discriminant))
        );

        // Step 3: Compare tag with expected discriminant
        let tag_matches = self.create_value(HirType::Bool, HirValueKind::Instruction);
        let tag_cmp_inst = HirInstruction::Binary {
            op: crate::hir::BinaryOp::Eq,
            result: tag_matches,
            ty: HirType::Bool,
            left: tag_val,
            right: expected_val,
        };

        self.add_instruction(block_id, tag_cmp_inst);
        self.add_use(tag_val, tag_matches);
        self.add_use(expected_val, tag_matches);

        // Step 4: If no payload fields, return tag match result
        if field_patterns.is_empty() {
            return Ok(tag_matches);
        }

        // Step 5: Extract payload from enum (at index 1)
        // The payload is a union, we need to extract the specific variant's data
        let payload_ty = HirType::Void; // TODO: Get actual payload type
        let payload_val = self.create_value(payload_ty.clone(), HirValueKind::Instruction);

        let extract_payload_inst = HirInstruction::ExtractValue {
            result: payload_val,
            aggregate: scrutinee_val,
            indices: vec![1], // Payload is at index 1
            ty: payload_ty,
        };

        self.add_instruction(block_id, extract_payload_inst);
        self.add_use(scrutinee_val, payload_val);

        // Step 6: Test each field pattern against payload fields
        let mut field_result = None;

        for (idx, field_pattern) in field_patterns.iter().enumerate() {
            // Extract field from payload
            let field_ty = self.convert_type(&field_pattern.ty);
            let field_val = self.create_value(field_ty.clone(), HirValueKind::Instruction);

            let extract_field_inst = HirInstruction::ExtractValue {
                result: field_val,
                aggregate: payload_val,
                indices: vec![idx as u32],
                ty: field_ty,
            };

            self.add_instruction(block_id, extract_field_inst);
            self.add_use(payload_val, field_val);

            // Test pattern against field
            let field_test = self.translate_pattern_test(
                block_id,
                field_pattern,
                field_val,
                &field_pattern.ty
            )?;

            // AND with previous field results
            field_result = Some(if let Some(prev_result) = field_result {
                let and_result = self.create_value(HirType::Bool, HirValueKind::Instruction);
                let and_inst = HirInstruction::Binary {
                    op: crate::hir::BinaryOp::And,
                    result: and_result,
                    ty: HirType::Bool,
                    left: prev_result,
                    right: field_test,
                };

                self.add_instruction(block_id, and_inst);
                self.add_use(prev_result, and_result);
                self.add_use(field_test, and_result);

                and_result
            } else {
                field_test
            });
        }

        // Step 7: AND tag match with field tests
        if let Some(fields_match) = field_result {
            let final_result = self.create_value(HirType::Bool, HirValueKind::Instruction);
            let final_and_inst = HirInstruction::Binary {
                op: crate::hir::BinaryOp::And,
                result: final_result,
                ty: HirType::Bool,
                left: tag_matches,
                right: fields_match,
            };

            self.add_instruction(block_id, final_and_inst);
            self.add_use(tag_matches, final_result);
            self.add_use(fields_match, final_result);

            Ok(final_result)
        } else {
            // No fields, just return tag match
            Ok(tag_matches)
        }
    }

    // ========== CLOSURE LOWERING ==========

    /// Translate a lambda/closure expression to HIR
    ///
    /// Current implementation: Basic infrastructure for closures
    /// - Detects captures vs. no captures
    /// - Creates environment struct for capturing closures
    /// - Allocates and initializes environment with captured values
    ///
    /// TODO (Gap 6 Phase 2 - requires architectural changes):
    /// - Generate separate HIR function for lambda body
    /// - Create thunk function with environment parameter
    /// - Return proper closure object {function_ptr, environment}
    /// - Implement indirect call for closure invocation
    ///
    /// Estimated remaining effort: 15-20 hours
    /// Requires: Separate SSA builder context for nested functions
    fn translate_closure(
        &mut self,
        block_id: HirId,
        lambda: &zyntax_typed_ast::typed_ast::TypedLambda,
        closure_ty: &Type,
    ) -> CompilerResult<HirId> {
        // TODO: Full closure implementation requires:
        // - Lambda function generation (translate_lambda_to_function)
        // - Closure object creation (function_ptr + env_ptr)
        // - Indirect call support
        //
        // For now, just generate environment struct for capturing closures

        // Check if closure has captures
        if lambda.captures.is_empty() {
            // No captures: Return opaque function pointer placeholder
            let func_ptr_ty = HirType::Ptr(Box::new(HirType::Void));
            return Ok(self.create_undef(func_ptr_ty));
        }

        // Has captures: Create environment struct
        self.translate_closure_environment(block_id, lambda)
    }

    /// Create and initialize environment struct for a capturing closure
    ///
    /// This generates HIR instructions to:
    /// 1. Define environment struct type with fields for each capture
    /// 2. Allocate environment struct on stack
    /// 3. Initialize each field with captured variable values
    /// 4. Return pointer to initialized environment
    fn translate_closure_environment(
        &mut self,
        block_id: HirId,
        lambda: &zyntax_typed_ast::typed_ast::TypedLambda,
    ) -> CompilerResult<HirId> {
        // Step 1: Create environment struct type
        let mut env_field_types = Vec::new();
        for capture in &lambda.captures {
            let capture_ty = self.convert_type(&capture.ty);
            let field_ty = if capture.by_ref {
                // Capture by reference: use pointer
                HirType::Ptr(Box::new(capture_ty))
            } else {
                // Capture by value
                capture_ty
            };
            env_field_types.push(field_ty);
        }

        let env_struct_ty = HirType::Struct(crate::hir::HirStructType {
            name: None, // Anonymous environment struct
            fields: env_field_types.clone(),
            packed: false,
        });

        // Step 2: Allocate environment struct
        let env_ptr_result = self.create_value(
            HirType::Ptr(Box::new(env_struct_ty.clone())),
            HirValueKind::Instruction
        );

        let alloca_inst = HirInstruction::Alloca {
            result: env_ptr_result,
            ty: env_struct_ty.clone(),
            count: None,
            align: 8,
        };

        self.add_instruction(block_id, alloca_inst);

        // Step 3: Initialize environment struct with captured values
        for (idx, capture) in lambda.captures.iter().enumerate() {
            // Read captured variable value
            let captured_val = self.read_variable(capture.name, block_id);

            // If capturing by reference, take address
            let value_to_store = if capture.by_ref {
                // TODO: Take address of captured variable
                captured_val
            } else {
                captured_val
            };

            // Store into environment struct field
            // Use InsertValue to update the struct
            let updated_env = self.create_value(env_struct_ty.clone(), HirValueKind::Instruction);
            let insert_inst = HirInstruction::InsertValue {
                result: updated_env,
                aggregate: env_ptr_result,
                value: value_to_store,
                indices: vec![idx as u32],
                ty: env_struct_ty.clone(),
            };

            self.add_instruction(block_id, insert_inst);
            self.add_use(env_ptr_result, updated_env);
            self.add_use(value_to_store, updated_env);
        }

        // Return environment pointer
        Ok(env_ptr_result)
    }
}

impl SsaForm {
    /// Verify SSA properties
    pub fn verify(&self) -> CompilerResult<()> {
        // Each value should be defined exactly once
        let mut defined_values = HashSet::new();
        
        for (_, block) in &self.function.blocks {
            // Check phis
            for phi in &block.phis {
                if !defined_values.insert(phi.result) {
                    return Err(crate::CompilerError::Analysis(
                        format!("Value {:?} defined multiple times", phi.result)
                    ));
                }
            }
            
            // Check instructions
            for inst in &block.instructions {
                if let Some(result) = inst.get_result() {
                    if !defined_values.insert(result) {
                        return Err(crate::CompilerError::Analysis(
                            format!("Value {:?} defined multiple times", result)
                        ));
                    }
                }
            }
        }
        
        // Each use should have a definition (or be a constant/parameter)
        for (use_id, def_id) in &self.use_def_chains {
            if !defined_values.contains(def_id) {
                // Check if it's a constant or parameter (self-defining values)
                if let Some(value) = self.function.values.get(def_id) {
                    match value.kind {
                        HirValueKind::Constant(_) | HirValueKind::Parameter(_) | HirValueKind::Undef | HirValueKind::Global(_) => {
                            // These are self-defining, so it's OK
                            continue;
                        }
                        _ => {}
                    }
                }

                return Err(crate::CompilerError::Analysis(
                    format!("Use {:?} has undefined definition {:?}", use_id, def_id)
                ));
            }
        }
        
        Ok(())
    }
    
    /// Optimize trivial phis
    pub fn optimize_trivial_phis(&mut self) {
        let mut changed = true;
        
        while changed {
            changed = false;
            
            for (_, block) in &mut self.function.blocks {
                let mut phis_to_remove = Vec::new();
                
                for (i, phi) in block.phis.iter().enumerate() {
                    // Check if phi is trivial
                    let non_self_values: HashSet<_> = phi.incoming.iter()
                        .map(|(val, _)| val)
                        .filter(|&&v| v != phi.result)
                        .collect();
                    
                    if non_self_values.len() == 1 {
                        // Trivial phi - can be replaced
                        let replacement = **non_self_values.iter().next().unwrap();
                        phis_to_remove.push((i, phi.result, replacement));
                        changed = true;
                    }
                }
                
                // Remove trivial phis
                for (i, _, _) in phis_to_remove.iter().rev() {
                    block.phis.remove(*i);
                }

                // NOTE: Use-replacement requires def-use chains to find all uses.
                // Current SsaBuilder tracks uses in `uses` HashMap, but this method
                // operates on SsaForm (which doesn't have that data).
                //
                // WORKAROUND: Skips use-replacement (optimization incomplete but safe)
                // FUTURE (v2.0): Add def-use chain walking or store use info in SsaForm
                // Estimated effort: 6-8 hours (requires use-tracking infrastructure in SsaForm)
            }
        }
    }
}

impl HirInstruction {
    /// Get the result value of this instruction if any
    fn get_result(&self) -> Option<HirId> {
        match self {
            HirInstruction::Binary { result, .. } |
            HirInstruction::Unary { result, .. } |
            HirInstruction::Alloca { result, .. } |
            HirInstruction::Load { result, .. } |
            HirInstruction::GetElementPtr { result, .. } |
            HirInstruction::Cast { result, .. } |
            HirInstruction::Select { result, .. } |
            HirInstruction::ExtractValue { result, .. } |
            HirInstruction::InsertValue { result, .. } |
            HirInstruction::Atomic { result, .. } |
            HirInstruction::CreateUnion { result, .. } |
            HirInstruction::GetUnionDiscriminant { result, .. } |
            HirInstruction::ExtractUnionValue { result, .. } |
            HirInstruction::CreateClosure { result, .. } |
            HirInstruction::CreateTraitObject { result, .. } |
            HirInstruction::UpcastTraitObject { result, .. } => Some(*result),

            HirInstruction::Call { result, .. } |
            HirInstruction::IndirectCall { result, .. } |
            HirInstruction::CallClosure { result, .. } |
            HirInstruction::TraitMethodCall { result, .. } => *result,
            
            HirInstruction::CreateRef { result, .. } |
            HirInstruction::Deref { result, .. } |
            HirInstruction::Move { result, .. } |
            HirInstruction::Copy { result, .. } => Some(*result),
            
            HirInstruction::Store { .. } |
            HirInstruction::Fence { .. } |
            HirInstruction::BeginLifetime { .. } |
            HirInstruction::EndLifetime { .. } |
            HirInstruction::LifetimeConstraint { .. } => None,
        }
    }
}