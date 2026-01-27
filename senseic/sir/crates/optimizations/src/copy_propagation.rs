use std::collections::HashMap;

use sir_data::{
    BasicBlockId, Control, EthIRProgram, LocalId,
    operation::{InlineOperands, OpVisitorMut},
};

struct CopyReplacer<'a> {
    copy_map: &'a HashMap<LocalId, LocalId>,
    locals: &'a mut [LocalId],
}

impl OpVisitorMut<()> for CopyReplacer<'_> {
    fn visit_inline_operands_mut<const INS: usize, const OUTS: usize>(
        &mut self,
        data: &mut InlineOperands<INS, OUTS>,
    ) {
        for input in &mut data.ins {
            if let Some(&replacement) = self.copy_map.get(input) {
                *input = replacement;
            }
        }
    }

    fn visit_allocated_ins_mut<const INS: usize, const OUTS: usize>(
        &mut self,
        data: &mut sir_data::operation::AllocatedIns<INS, OUTS>,
    ) {
        let start = data.ins_start.idx();
        for i in start..start + INS {
            if let Some(&replacement) = self.copy_map.get(&self.locals[i]) {
                self.locals[i] = replacement;
            }
        }
    }

    fn visit_static_alloc_mut(&mut self, _data: &mut sir_data::operation::StaticAllocData) {}

    fn visit_memory_load_mut(&mut self, data: &mut sir_data::operation::MemoryLoadData) {
        if let Some(&replacement) = self.copy_map.get(&data.ptr) {
            data.ptr = replacement;
        }
    }

    fn visit_memory_store_mut(&mut self, data: &mut sir_data::operation::MemoryStoreData) {
        if let Some(&r) = self.copy_map.get(&data.ptr) {
            data.ptr = r;
        }
        if let Some(&r) = self.copy_map.get(&data.value) {
            data.value = r;
        }
    }

    fn visit_set_small_const_mut(&mut self, _data: &mut sir_data::operation::SetSmallConstData) {}

    fn visit_set_large_const_mut(&mut self, _data: &mut sir_data::operation::SetLargeConstData) {}

    fn visit_set_data_offset_mut(&mut self, _data: &mut sir_data::operation::SetDataOffsetData) {}

    fn visit_icall_mut(&mut self, data: &mut sir_data::operation::InternalCallData) {
        let start = data.ins_start.idx();
        let end = data.outs_start.idx();
        for i in start..end {
            if let Some(&replacement) = self.copy_map.get(&self.locals[i]) {
                self.locals[i] = replacement;
            }
        }
    }

    fn visit_void_mut(&mut self) {}
}

pub fn run(program: &mut EthIRProgram) {
    for idx in 0..program.basic_blocks.len() {
        let block_idx = BasicBlockId::new(idx as u32);
        let ops_range = program.basic_blocks[block_idx].operations.clone();
        let mut copy_map: HashMap<LocalId, LocalId> = HashMap::new();

        for op in &mut program.operations[ops_range.clone()] {
            if let sir_data::Operation::SetCopy(InlineOperands { ins: [src], outs: [dst] }) = op {
                let resolved_src = copy_map.get(src).unwrap_or(src);
                copy_map.insert(*dst, *resolved_src);
            }
        }

        let locals = program.locals.as_raw_slice_mut();
        let mut replacer = CopyReplacer { copy_map: &copy_map, locals };
        for op in &mut program.operations[ops_range] {
            op.visit_data_mut(&mut replacer);
        }

        let outputs_range = program.basic_blocks[block_idx].outputs.clone();
        for index in outputs_range.start.idx()..outputs_range.end.idx() {
            let local = &mut locals[index];
            if let Some(&replacement) = copy_map.get(local) {
                *local = replacement;
            }
        }

        match &mut program.basic_blocks[block_idx].control {
            Control::Branches(branch) => {
                if let Some(&r) = copy_map.get(&branch.condition) {
                    branch.condition = r;
                }
            }
            Control::Switch(switch) => {
                if let Some(&r) = copy_map.get(&switch.condition) {
                    switch.condition = r;
                }
            }
            _ => {}
        }
    }
}
