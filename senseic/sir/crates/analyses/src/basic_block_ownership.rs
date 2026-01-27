use sir_data::{BasicBlockId, EthIRProgram, FunctionId, IndexVec, index_vec};

#[derive(Debug, Clone)]
pub struct BasicBlockOwnershipAndReachability {
    /// Maps each basic block to its owning function.
    /// None means the block is unreachable from any function.
    ownership: IndexVec<BasicBlockId, Option<FunctionId>>,
}

impl BasicBlockOwnershipAndReachability {
    /// Analyze the program to determine which function owns each basic block.
    pub fn analyze(program: &EthIRProgram) -> Self {
        // Initialize ownership vector with None for each basic block
        let mut ownership = index_vec![None; program.basic_blocks.len()];

        // Process each function
        for (func_id, func) in program.functions.iter_enumerated() {
            // Start DFS from the function's entry block
            Self::mark_reachable_blocks(&mut ownership, program, func.entry(), func_id);
        }

        Self { ownership }
    }

    /// Depth-first traversal to mark all blocks reachable from the given entry
    fn mark_reachable_blocks(
        ownership: &mut IndexVec<BasicBlockId, Option<FunctionId>>,
        program: &EthIRProgram,
        current: BasicBlockId,
        owner: FunctionId,
    ) {
        // Check if already visited
        if ownership[current].is_some() {
            return;
        }

        ownership[current] = Some(owner);
        let bb = &program.basic_blocks[current];

        // Use the new iter_outgoing API to get all successor basic blocks
        for successor in bb.control.iter_outgoing(program) {
            Self::mark_reachable_blocks(ownership, program, successor, owner);
        }
    }

    /// Get the function that owns a basic block, if any
    pub fn get_owner(&self, block: BasicBlockId) -> Option<FunctionId> {
        self.ownership[block]
    }

    /// Check if a basic block is reachable from any function
    pub fn is_reachable(&self, block: BasicBlockId) -> bool {
        self.ownership[block].is_some()
    }

    /// Get all basic blocks owned by a specific function
    pub fn blocks_owned_by(&self, func: FunctionId) -> impl Iterator<Item = BasicBlockId> + '_ {
        self.ownership
            .iter_enumerated()
            .filter_map(move |(bb_id, owner)| if *owner == Some(func) { Some(bb_id) } else { None })
    }

    /// Get all unreachable basic blocks
    pub fn unreachable_blocks(&self) -> impl Iterator<Item = BasicBlockId> + '_ {
        self.ownership
            .iter_enumerated()
            .filter_map(move |(bb_id, owner)| if owner.is_none() { Some(bb_id) } else { None })
    }

    /// Display IR with basic blocks grouped by function
    pub fn display_ir_with_function_grouping(&self, program: &EthIRProgram) -> String {
        use std::fmt::Write;
        let mut output = String::new();

        // Display functions with their owned basic blocks
        for (func_id, _func) in program.functions.iter_enumerated() {
            writeln!(&mut output, "fn @{}:", func_id).unwrap();

            // Display all basic blocks owned by this function
            for bb_id in self.blocks_owned_by(func_id) {
                let bb = &program.basic_blocks[bb_id];
                bb.fmt_display(&mut output, bb_id, program).unwrap();
                writeln!(&mut output).unwrap();
            }
        }

        // Display unreachable basic blocks
        let mut unreachable = self.unreachable_blocks().peekable();
        if unreachable.peek().is_some() {
            writeln!(&mut output, "// Unreachable basic blocks").unwrap();
            for bb_id in unreachable {
                let bb = &program.basic_blocks[bb_id];
                bb.fmt_display(&mut output, bb_id, program).unwrap();
                writeln!(&mut output).unwrap();
            }
        }

        // Display data segments
        if !program.data_segments_start.is_empty() {
            writeln!(&mut output).unwrap();

            for (segment_id, _) in program.data_segments_start.iter_enumerated() {
                write!(&mut output, "data .{segment_id} ").unwrap();

                // Display hex bytes for the segment
                let range = program.get_segment_range(segment_id);
                write!(&mut output, "0x").unwrap();
                for i in range.start.get()..range.end.get() {
                    write!(&mut output, "{:02x}", program.data_bytes[sir_data::DataOffset::new(i)])
                        .unwrap();
                }
                writeln!(&mut output).unwrap();
            }
        }

        output
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use sir_data::{Branch, Control, builder::EthIRBuilder, operation::*};

    #[test]
    fn test_simple_ownership() {
        // Create a simple program with one function and two basic blocks
        let mut builder = EthIRBuilder::new();
        let mut func = builder.begin_function();

        // First basic block - continues to second block
        let mut bb0 = func.begin_basic_block();
        bb0.add_operation(Operation::Noop(Default::default()));
        let bb0_id = bb0.finish(Control::ContinuesTo(BasicBlockId::new(1))).unwrap();

        // Second basic block - terminates
        let mut bb1 = func.begin_basic_block();
        bb1.add_operation(Operation::Stop(Default::default()));
        let bb1_id = bb1.finish(Control::LastOpTerminates).unwrap();

        let func_id = func.finish(bb0_id);
        let program = builder.build(func_id, None);

        let analysis = BasicBlockOwnershipAndReachability::analyze(&program);

        // Both blocks should be owned by function 0
        assert_eq!(analysis.get_owner(bb0_id), Some(func_id));
        assert_eq!(analysis.get_owner(bb1_id), Some(func_id));

        // Both blocks should be reachable
        assert!(analysis.is_reachable(bb0_id));
        assert!(analysis.is_reachable(bb1_id));

        // No unreachable blocks
        assert!(analysis.unreachable_blocks().next().is_none());
    }

    #[test]
    fn test_unreachable_blocks() {
        // Create a program with unreachable blocks
        let mut builder = EthIRBuilder::new();

        // Create first function with one reachable block
        let mut func = builder.begin_function();
        let mut bb0 = func.begin_basic_block();
        bb0.add_operation(Operation::Stop(Default::default()));
        let bb0_id = bb0.finish(Control::LastOpTerminates).unwrap();
        let func_id = func.finish(bb0_id);

        // Create an unreachable basic block (not part of any function)
        let mut orphan_func = builder.begin_function();
        let mut bb1 = orphan_func.begin_basic_block();
        bb1.add_operation(Operation::Stop(Default::default()));
        let bb1_id = bb1.finish(Control::LastOpTerminates).unwrap();
        // Don't register this as a function entry, making the block unreachable

        let program = builder.build(func_id, None);

        let analysis = BasicBlockOwnershipAndReachability::analyze(&program);

        // Block 0 is reachable, block 1 is not
        assert!(analysis.is_reachable(bb0_id));
        assert!(!analysis.is_reachable(bb1_id));

        // Block 1 should be in unreachable list
        assert_eq!(analysis.unreachable_blocks().collect::<Vec<_>>(), vec![bb1_id]);
    }

    #[test]
    fn test_multiple_functions() {
        // Create a program with two functions
        let mut builder = EthIRBuilder::new();

        // Function 0: two blocks (bb0 -> bb1)
        let mut func0 = builder.begin_function();

        // Create bb0 with a temporary control, we'll update it later
        let mut bb0 = func0.begin_basic_block();
        bb0.add_operation(Operation::Noop(Default::default()));
        let bb0_id = bb0.finish(Control::LastOpTerminates).unwrap();

        let mut bb1 = func0.begin_basic_block();
        bb1.add_operation(Operation::Stop(Default::default()));
        let bb1_id = bb1.finish(Control::LastOpTerminates).unwrap();

        // Update bb0's control to point to bb1
        func0.set_control(bb0_id, Control::ContinuesTo(bb1_id)).unwrap();

        let func0_id = func0.finish(bb0_id);

        // Function 1: one block with internal return
        let mut func1 = builder.begin_function();
        let mut bb2 = func1.begin_basic_block();
        bb2.add_operation(Operation::Noop(Default::default()));
        let bb2_id = bb2.finish(Control::InternalReturn).unwrap();
        let func1_id = func1.finish(bb2_id);

        let program = builder.build(func0_id, None);

        let analysis = BasicBlockOwnershipAndReachability::analyze(&program);

        // Check ownership
        assert_eq!(analysis.get_owner(bb0_id), Some(func0_id));
        assert_eq!(analysis.get_owner(bb1_id), Some(func0_id));
        assert_eq!(analysis.get_owner(bb2_id), Some(func1_id));

        // Check blocks owned by each function
        assert_eq!(analysis.blocks_owned_by(func0_id).collect::<Vec<_>>(), vec![bb0_id, bb1_id]);
        assert_eq!(analysis.blocks_owned_by(func1_id).collect::<Vec<_>>(), vec![bb2_id]);
    }

    #[test]
    fn test_branching_control_flow() {
        // Create a program with branching
        let mut builder = EthIRBuilder::new();
        let mut func = builder.begin_function();

        let condition = func.new_local();

        // Pre-allocate the branch target IDs
        let bb1_id = func.ir_builder.next_basic_block_id() + 1;
        let bb2_id = func.ir_builder.next_basic_block_id() + 2;

        // Entry block with branch
        let mut bb0 = func.begin_basic_block();
        bb0.add_operation(Operation::Noop(Default::default()));
        let bb0_id = bb0
            .finish(Control::Branches(Branch {
                condition,
                zero_target: bb1_id,
                non_zero_target: bb2_id,
            }))
            .unwrap();

        // Zero branch target
        let mut bb1 = func.begin_basic_block();
        bb1.add_operation(Operation::Stop(Default::default()));
        let bb1_id_actual = bb1.finish(Control::LastOpTerminates).unwrap();

        // Non-zero branch target
        let mut bb2 = func.begin_basic_block();
        bb2.add_operation(Operation::Stop(Default::default()));
        let bb2_id_actual = bb2.finish(Control::LastOpTerminates).unwrap();

        assert_eq!(bb1_id, bb1_id_actual);
        assert_eq!(bb2_id, bb2_id_actual);

        let func_id = func.finish(bb0_id);
        let program = builder.build(func_id, None);

        let analysis = BasicBlockOwnershipAndReachability::analyze(&program);

        // All blocks should be owned by function 0
        assert_eq!(analysis.get_owner(bb0_id), Some(func_id));
        assert_eq!(analysis.get_owner(bb1_id), Some(func_id));
        assert_eq!(analysis.get_owner(bb2_id), Some(func_id));

        // All blocks should be reachable
        assert!(analysis.is_reachable(bb0_id));
        assert!(analysis.is_reachable(bb1_id));
        assert!(analysis.is_reachable(bb2_id));
    }
}
