// @reliability: experimental
// @ai: assisted
//! S3: PESAT emission for the Steel reference program. Drive the reference
//! program's step circuit (LLVM frontend -> VAFFLE -> Volar IR -> movfuscate
//! -> circuit, the S2 path) through `emit_step_index` + `eval_step_wires` +
//! `step_accumulators` + `build_witness`, emitting one PESAT instance per
//! executed step. Gate: each step's instance is well-formed (`check_instance`),
//! and the honest chain links (`verify_chain`). Uses the emitter's own wire
//! trace for state threading (consistent storage), as in E3.
//! See `docs/steel-in-volar-plan.md` milestone S3.

use std::path::PathBuf;

use inkwell::context::Context;
use inkwell::memory_buffer::MemoryBuffer;
use volar_ir::ir::{IRBlock, IRBlockTargetId, IRTerminator, IRTypeId};
use volar_ir_common::{IrType, Type};
use volar_ir_passes::lower_to_circuit::{LoweringMode, lower_to_circuit_ir};
use volar_ir_passes::movfuscate::movfuscate_ir;
use volar_pesat_emit::{
    build_witness, chain_manifest, derive_challenges, emit_step_index, eval_step_wires,
    step_accumulators, verify_chain,
};
use volar_primitives::Galois128;

fn reference_llvm_ir() -> String {
    let dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../../../volar-ir/crates/tools/volar-steel/programs");
    std::fs::read_to_string(dir.join("reference.ll")).expect("reference.ll")
}

fn reference_step_circuit() -> (volar_ir::ir::IRBlocks<()>, volar_ir::ir::IRTypes) {
    let ll = reference_llvm_ir();
    let context = Context::create();
    let module = context
        .create_module_from_ir(MemoryBuffer::create_from_memory_range_copy(
            ll.as_bytes(),
            "reference.ll",
        ))
        .expect("reference.ll parses");
    let vaffle =
        volar_llvm_vaffle_import::import_module(&module, &["steel_view_call"]).expect("LLVM import");
    let (blocks, mut types) = volar_vaffle_target::lower_vaffle_to_ir(&vaffle);
    let movfuscated = movfuscate_ir(&blocks, &mut types);
    let bit_ty = types
        .0
        .iter()
        .position(|t| matches!(t, IrType::Primitive(Type::Bit)))
        .map(|i| IRTypeId(i as u32))
        .expect("Bit interned");
    let circuit = lower_to_circuit_ir(&movfuscated, &bit_ty, 1, LoweringMode::WithTerminationFlag);
    (circuit, types)
}

fn return_values(step_block: &IRBlock<()>, wires: &[Vec<bool>]) -> Vec<Vec<bool>> {
    match &step_block.terminator {
        IRTerminator::Jmp { target } if target.dest == IRBlockTargetId::Return => target
            .args
            .iter()
            .map(|v| wires[v.0 as usize].clone())
            .collect(),
        other => panic!("step block must return; got {other:?}"),
    }
}

#[test]
fn s3_pesat_emission_for_reference_program() {
    let (circuit, types) = reference_step_circuit();
    let step_block = &circuit.blocks[0];
    let emission = emit_step_index(step_block, &types).expect("emit step index");
    assert!(emission.has_storage, "the calling convention uses a frame");
    emission.index.validate().expect("valid index");
    let manifest = chain_manifest(&emission, step_block);
    let challenges = derive_challenges(step_block);

    let n_state = step_block.params.len();
    let widths: Vec<usize> = step_block
        .params
        .iter()
        .map(|&t| types.value_bit_width(t).expect("data type"))
        .collect();
    let pc_width = widths.iter().take_while(|&&w| w == 1).count();

    // Initial state: pc=0, program inputs [5,7,2,11,13,17,19] in the state
    // slots (after the PC bits).
    let inputs = [5u64, 7, 2, 11, 13, 17, 19];
    let mut state: Vec<Vec<bool>> = widths.iter().map(|&w| vec![false; w]).collect();
    for (k, &inp) in inputs.iter().enumerate() {
        let slot = pc_width + k;
        if slot < state.len() && state[slot].len() == 64 {
            for b in 0..64 {
                state[slot][b] = (inp >> b) & 1 == 1;
            }
        }
    }

    let mut storage = volar_pesat_emit::StorageMap::default();
    let mut prod = Galois128(0);
    let mut cons = Galois128(0);
    let mut traces = Vec::new();
    let mut step_idx = 0usize;
    loop {
        let mut accesses = Vec::new();
        let wires = eval_step_wires(step_block, &types, &state, &mut storage, &mut accesses)
            .expect("eval");
        let ts = Galois128(step_idx as u128);
        let (acc_chain, prod_out, cons_out) =
            step_accumulators(challenges, prod, cons, ts, &accesses);
        let witness = build_witness(&emission, &wires, prod, cons, ts, &acc_chain, prod_out, cons_out);
        emission
            .index
            .check_instance(&witness)
            .unwrap_or_else(|e| panic!("step {step_idx} rejected: {e:?}"));
        traces.push(witness);

        // Step forward from the emitter's own wire trace (consistent storage).
        let outputs = return_values(step_block, &wires);
        let done = outputs[0].iter().any(|&b| b);
        state = outputs[1..1 + n_state].to_vec();
        prod = prod_out;
        cons = cons_out;
        step_idx += 1;
        if done || step_idx > 32 {
            break;
        }
    }
    assert!(step_idx >= 1, "expected at least 1 step, got {step_idx}");

    // The chain links (including the accumulator boundary).
    verify_chain(&manifest, &traces).expect("honest chain links");
    eprintln!(
        "S3: {} steps, {} constraints, {} vars (all instances well-formed, chain links)",
        step_idx,
        emission.index.constraint_count(),
        emission.index.variable_count()
    );
}
