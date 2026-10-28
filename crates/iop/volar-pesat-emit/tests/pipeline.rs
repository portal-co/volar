// @reliability: experimental
// @ai: assisted
//! End-to-end pipeline probe: program CFG -> movfuscate -> per-step PESAT
//! instances -> WARP accumulation -> a proof -> succinct verify + decide, at a
//! documented official-style sampling. Measures the proof/verifier structure
//! (Merkle openings, sumcheck rounds, field ops) that drives the on-chain gas.

use volar_ir::ir::{
    IRBlock, IRBlockTargetId, IRBlocks, IRBranchTarget, IRStmt, IRTerminator, IRVarId,
};
use volar_ir_common::{Constant, IrType, Node, PolyCoeffs, Type, TypeId, TypeTable};
use volar_ir_passes::lower_to_circuit::{LoweringMode, lower_to_circuit_ir};
use volar_ir_passes::movfuscate::movfuscate_ir;
use volar_iop::warp::{ReedSolomonCode, WarpSampling, warp_decide, warp_verify};
use volar_pesat_emit::{
    build_witness, emit_step_index, eval_step_wires, stream_fold, stream_start,
};
use volar_primitives::Galois128;

fn xor_chain_cfg(n: usize) -> (IRBlocks<()>, TypeTable) {
    let types = TypeTable(vec![IrType::Primitive(Type::_8), IrType::Primitive(Type::Bit)]);
    let mk = |kind: IRStmt| Node::new(kind, (), None);
    let blocks = (0..n)
        .map(|i| {
            let last = i + 1 == n;
            IRBlock {
                params: vec![TypeId(0)],
                stmts: vec![mk(IRStmt::Poly {
                    ty: TypeId(0),
                    coeffs: PolyCoeffs::from_iter([(vec![IRVarId(0)], 1u8)]),
                    constant: Constant {
                        hi: 0,
                        lo: (0xA5usize.wrapping_mul(i + 1) & 0xFF) as u128,
                    },
                })],
                terminator: IRTerminator::Jmp {
                    target: IRBranchTarget::new(
                        if last {
                            IRBlockTargetId::Return
                        } else {
                            IRBlockTargetId::Block(volar_ir::ir::IRBlockId((i + 1) as u32))
                        },
                        vec![IRVarId(1)],
                    ),
                },
            }
        })
        .collect();
    (IRBlocks::new(blocks), types)
}

fn collect_witnesses(
    blocks: &IRBlocks<()>,
    types: &TypeTable,
) -> (
    volar_pesat_emit::StepEmission,
    Vec<volar_iop::warp::PesatWitness<Galois128>>,
) {
    let mut types = types.clone();
    let movfuscated = movfuscate_ir(blocks, &mut types);
    let bit_ty = types
        .0
        .iter()
        .position(|t| matches!(t, IrType::Primitive(Type::Bit)))
        .map(|i| volar_ir::ir::IRTypeId(i as u32))
        .expect("Bit interned");
    let circuit = lower_to_circuit_ir(&movfuscated, &bit_ty, 1, LoweringMode::WithTerminationFlag);
    let step_block = &circuit.blocks[0];
    let emission = emit_step_index(step_block, &types).expect("emit");
    emission.index.validate().expect("valid index");

    let param_widths: Vec<usize> = step_block
        .params
        .iter()
        .map(|&t| types.value_bit_width(t).expect("data type"))
        .collect();
    let mut state: Vec<Vec<bool>> = param_widths.iter().map(|&w| vec![false; w]).collect();
    let mut storage = volar_pesat_emit::StorageMap::default();
    let mut traces = Vec::new();
    loop {
        let mut accesses = Vec::new();
        let wires = eval_step_wires(step_block, &types, &state, &mut storage, &mut accesses)
            .expect("eval");
        let witness = build_witness(
            &emission,
            &wires,
            Galois128(0),
            Galois128(0),
            Galois128(0),
            &[],
            Galois128(0),
            Galois128(0),
        );
        traces.push(witness);
        let mut ref_storage = Default::default();
        let outputs = volar_fuzz::interpreter::ir::eval_ir_circuit_step(
            step_block,
            &types,
            &circuit.oracles,
            &state,
            &mut ref_storage,
        );
        let done = outputs[0].iter().any(|&b| b);
        state = outputs[1..1 + param_widths.len()].to_vec();
        if done || traces.len() > 16 {
            break;
        }
    }
    (emission, traces)
}

// Slow E2E validation (~23 min): the naive O(n*k) Reed-Solomon encode plus the
// 4x-longer codeword make the prover/verifier expensive. Run explicitly with
// `cargo test -p volar-pesat-emit --test pipeline -- --ignored --nocapture`.
#[test]
#[ignore = "slow E2E WARP-over-Reed-Solomon pipeline validation (~23 min)"]
fn pipeline_proof_and_verifier_structure() {
    let (blocks, types) = xor_chain_cfg(6);
    let (emission, traces) = collect_witnesses(&blocks, &types);
    let n = traces.len();
    let index = &emission.index;
    let witness_len = index.witness_len;
    // Reed-Solomon rate-1/4 (k=witness_len, n=4k): the paper's named
    // larger-distance code. MDS => relative distance (1-rate)=3/4, so the
    // unique-decoding proximity radius is delta=(1-rate)/2=3/8 with s=0
    // out-of-domain samples. At delta=3/8, t=255 shift queries yield a REAL
    // lambda = 255 * -log2(5/8) = 149-bit soundness (vs the IdentityCode's
    // delta->0, i.e. no real soundness at the same t).
    let code = ReedSolomonCode::new(witness_len, 4 * witness_len).expect("rate-1/4 RS");
    let sampling = WarpSampling {
        log_codeword_len: (4 * witness_len).trailing_zeros() as usize,
        out_of_domain: 0,
        shift_queries: 255,
    };
    assert!((1 + sampling.out_of_domain + sampling.shift_queries).is_power_of_two());
    eprintln!(
        "[PIPELINE] RS rate-1/4: delta={:.3} (unique-decoding), real lambda at t=255 = {:.0}-bit",
        code.unique_decoding_delta(),
        255.0 * (1.0 - code.unique_decoding_delta()).log2().abs()
    );

    // Fold the whole stream, keeping each fold's proof.
    let mut token = stream_start(index, &code, &traces[0]).expect("genesis");
    let mut last_proof = None;
    for w in &traces[1..] {
        let (next, proof) = stream_fold(index, &code, sampling, token, w).expect("fold");
        last_proof = Some(proof);
        token = next;
    }
    let proof = last_proof.expect("at least one fold");
    assert_eq!(token.steps, n);

    // Verifier structure: the on-chain-relevant op counts.
    let ell = 2; // one fresh + one prior accumulator per fold
    let t = sampling.shift_queries;
    let log_n = sampling.log_codeword_len;
    let merkle_openings = proof.initial_paths.len()
        + proof.accumulator_paths.iter().map(|p| p.len()).sum::<usize>();
    let sumcheck_rounds = proof.first_sumcheck.len() + proof.second_sumcheck.len();
    let openings_count: usize = proof.openings.iter().map(|r| r.len()).sum();
    eprintln!("[PIPELINE] steps={n} witness_len={witness_len} log_n={log_n}");
    eprintln!(
        "[PIPELINE] proof: initial_paths={} acc_paths={} first_sumcheck={} second_sumcheck={} openings={}",
        proof.initial_paths.len(),
        proof.accumulator_paths.len(),
        proof.first_sumcheck.len(),
        proof.second_sumcheck.len(),
        proof.openings.len()
    );
    eprintln!(
        "[PIPELINE] verifier: ell={ell} t={t} merkle_openings={merkle_openings} (x log_n={log_n} hashes) sumcheck_rounds={sumcheck_rounds} opening_field_elems={openings_count}"
    );

    // Succinct verify (the on-chain artifact) + decide (linear, off-chain).
    let fresh_publics: Vec<Vec<Galois128>> = vec![traces[n - 1].public.clone()];
    let prior_instances = vec![token.acc.instance.clone()];
    // Rebuild the fold's input accumulator: the (n-1)-step prefix.
    let mut prefix = stream_start(index, &code, &traces[0]).expect("genesis");
    for w in &traces[1..n - 1] {
        prefix = stream_fold(index, &code, sampling, prefix, w).expect("prefix fold").0;
    }
    warp_verify(
        index,
        sampling,
        &fresh_publics,
        &[prefix.acc.instance.clone()],
        &token.acc.instance,
        &proof,
    )
    .expect("succinct verify accepts the proof");
    warp_decide(index, &code, &token.acc).expect("decide accepts the final accumulator");
    let _ = prior_instances;
    eprintln!("[PIPELINE] succinct verify + decide both accept");
}
