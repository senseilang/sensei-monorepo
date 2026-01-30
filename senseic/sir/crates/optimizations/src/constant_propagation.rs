use std::collections::HashMap;

use sir_data::{
    BasicBlockId, EthIRProgram, LargeConstId, LocalId, Operation,
    operation::{InlineOperands, SetLargeConstData, SetSmallConstData},
};

#[derive(PartialEq, Clone, Eq, Hash)]
enum ConstValue {
    SmallConst(u32),
    LargeConst(LargeConstId),
    Address,
    Origin,
    Caller,
    CallValue,
    CallDataSize,
    CodeSize,
    GasPrice,
    Coinbase,
    Timestamp,
    Number,
    ChainId,
    BaseFee,
    BlobBaseFee,
}

fn track_constant(
    constant_map: &mut HashMap<LocalId, ConstValue>,
    first_occurence: &mut HashMap<ConstValue, LocalId>,
    replacements: &mut HashMap<LocalId, LocalId>,
    idx: LocalId,
    value: ConstValue,
) {
    constant_map.insert(idx, value.clone());
    match first_occurence.get(&value) {
        Some(first) => replacements.insert(idx, *first),
        None => first_occurence.insert(value, idx),
    };
}

fn track_constant_definition(
    program: &EthIRProgram,
    constant_map: &mut HashMap<LocalId, ConstValue>,
    first_occurence: &mut HashMap<ConstValue, LocalId>,
    replacements: &mut HashMap<LocalId, LocalId>,
    predecessors: &mut HashMap<BasicBlockId, Vec<BasicBlockId>>,
) {
    for (id, bb) in program.basic_blocks.enumerate_idx() {
        for successor in bb.control.iter_outgoing(program) {
            predecessors.entry(successor).or_default().push(id);
        }
        for op in &program.operations[bb.operations] {
            match op {
                Operation::Address(InlineOperands { ins: [], outs: [out] }) => {
                    track_constant(
                        constant_map,
                        first_occurence,
                        replacements,
                        *out,
                        ConstValue::Address,
                    );
                }
                Operation::Origin(InlineOperands { ins: [], outs: [out] }) => {
                    track_constant(
                        constant_map,
                        first_occurence,
                        replacements,
                        *out,
                        ConstValue::Origin,
                    );
                }
                Operation::Caller(InlineOperands { ins: [], outs: [out] }) => {
                    track_constant(
                        constant_map,
                        first_occurence,
                        replacements,
                        *out,
                        ConstValue::Caller,
                    );
                }
                Operation::CallValue(InlineOperands { ins: [], outs: [out] }) => {
                    track_constant(
                        constant_map,
                        first_occurence,
                        replacements,
                        *out,
                        ConstValue::CallValue,
                    );
                }
                Operation::CallDataSize(InlineOperands { ins: [], outs: [out] }) => {
                    track_constant(
                        constant_map,
                        first_occurence,
                        replacements,
                        *out,
                        ConstValue::CallDataSize,
                    );
                }
                Operation::CodeSize(InlineOperands { ins: [], outs: [out] }) => {
                    track_constant(
                        constant_map,
                        first_occurence,
                        replacements,
                        *out,
                        ConstValue::CodeSize,
                    );
                }
                Operation::GasPrice(InlineOperands { ins: [], outs: [out] }) => {
                    track_constant(
                        constant_map,
                        first_occurence,
                        replacements,
                        *out,
                        ConstValue::GasPrice,
                    );
                }
                Operation::Coinbase(InlineOperands { ins: [], outs: [out] }) => {
                    track_constant(
                        constant_map,
                        first_occurence,
                        replacements,
                        *out,
                        ConstValue::Coinbase,
                    );
                }
                Operation::Timestamp(InlineOperands { ins: [], outs: [out] }) => {
                    track_constant(
                        constant_map,
                        first_occurence,
                        replacements,
                        *out,
                        ConstValue::Timestamp,
                    );
                }
                Operation::Number(InlineOperands { ins: [], outs: [out] }) => {
                    track_constant(
                        constant_map,
                        first_occurence,
                        replacements,
                        *out,
                        ConstValue::Number,
                    );
                }
                Operation::ChainId(InlineOperands { ins: [], outs: [out] }) => {
                    track_constant(
                        constant_map,
                        first_occurence,
                        replacements,
                        *out,
                        ConstValue::ChainId,
                    );
                }
                Operation::BaseFee(InlineOperands { ins: [], outs: [out] }) => {
                    track_constant(
                        constant_map,
                        first_occurence,
                        replacements,
                        *out,
                        ConstValue::BaseFee,
                    );
                }
                Operation::BlobBaseFee(InlineOperands { ins: [], outs: [out] }) => {
                    track_constant(
                        constant_map,
                        first_occurence,
                        replacements,
                        *out,
                        ConstValue::BlobBaseFee,
                    );
                }
                Operation::SetSmallConst(SetSmallConstData { sets, value }) => {
                    track_constant(
                        constant_map,
                        first_occurence,
                        replacements,
                        *sets,
                        ConstValue::SmallConst(*value),
                    );
                }
                Operation::SetLargeConst(SetLargeConstData { sets, value }) => {
                    track_constant(
                        constant_map,
                        first_occurence,
                        replacements,
                        *sets,
                        ConstValue::LargeConst(*value),
                    );
                }
                _ => {}
            }
        }
    }
}

fn update_inputs(
    program: &EthIRProgram,
    predecessors: &HashMap<BasicBlockId, Vec<BasicBlockId>>,
    constant_map: &mut HashMap<LocalId, ConstValue>,
    first_occurence: &mut HashMap<ConstValue, LocalId>,
    replacements: &mut HashMap<LocalId, LocalId>,
) {
    for (bb_id, pred_blocks_ids) in predecessors {
        let bb = &program.basic_blocks[*bb_id];
        let inputs = &program.locals[bb.inputs];

        for (i, input) in inputs.iter().enumerate() {
            let mut const_value: Option<ConstValue> = None;
            let mut is_unknown = false;

            for pred_id in pred_blocks_ids {
                let pred_bb = &program.basic_blocks[*pred_id];
                let pred_output = &program.locals[pred_bb.outputs][i];

                match constant_map.get(&pred_output) {
                    None => {
                        is_unknown = true;
                        break;
                    }
                    Some(c) => match &const_value {
                        None => const_value = Some(c.clone()),
                        Some(existing) => {
                            if existing != c {
                                is_unknown = true;
                                break;
                            }
                        }
                    },
                }
            }

            if !is_unknown {
                if let Some(c) = const_value {
                    let first = first_occurence.get(&c).expect("constant should exist");
                    replacements.insert(*input, *first);
                    constant_map.insert(*input, c);
                }
            }
        }
    }
}

pub fn run(program: &mut EthIRProgram) {
    let mut constant_map: HashMap<LocalId, ConstValue> = HashMap::new();
    let mut first_occurence: HashMap<ConstValue, LocalId> = HashMap::new();
    let mut replacements: HashMap<LocalId, LocalId> = HashMap::new();
    let mut predecessors: HashMap<BasicBlockId, Vec<BasicBlockId>> = HashMap::new();

    track_constant_definition(
        program,
        &mut constant_map,
        &mut first_occurence,
        &mut replacements,
        &mut predecessors,
    );

    update_inputs(
        program,
        &predecessors,
        &mut constant_map,
        &mut first_occurence,
        &mut replacements,
    );

    for bb in program.basic_blocks.iter() {
        for op in &program.operations[bb.operations] {}
    }
}
