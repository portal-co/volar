// @reliability: experimental
// @ai: assisted
//! E4 gate: endless streams via accumulation across multiple calls. A step
//! stream is folded into a running WARP accumulator; splitting the stream into
//! two segments (two "verifier calls") with a resumption token must yield the
//! same final accumulator as a single-shot run, and a partial stream's
//! accumulator must be a verifiable statement about the prefix.

use volar_ir::ir::{
    IRBlock, IRBlockTargetId, IRBlocks, IRBranchTarget, IRStmt, IRTerminator, IRVarId,
};
use volar_ir_common::{Constant, IrType, Node, PolyCoeffs, Type, TypeId, TypeTable};
use volar_ir_passes::lower_to_circuit::{LoweringMode, lower_to_circuit_ir};
use volar_ir_passes::movfuscate::movfuscate_ir;
use volar_iop::warp::{IdentityCode, WarpSampling};
use volar_pesat_emit::{
    build_witness, emit_step_index, eval_step_wires, stream_fold, stream_fold_segment, stream_start,
};
use volar_primitives::Galois128;

/// An `n`-block XOR pipeline (as in E2): `n` steps, `n-1` links.
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

/// Run the chain, collecting per-step witnesses (each satisfying the shared
/// step instance).
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
        // Step with the reference interpreter for the (pc, state) transition.
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

#[test]
fn e4_endless_stream_segment_transparent() {
    let (blocks, types) = xor_chain_cfg(6);
    let (emission, traces) = collect_witnesses(&blocks, &types);
    let n = traces.len();
    assert!(n >= 6, "expected >= 6 steps, got {n}");

    let index = &emission.index;
    let witness_len = index.witness_len;
    let code = IdentityCode::new(witness_len).expect("pow2 witness");
    let sampling = WarpSampling {
        log_codeword_len: witness_len.trailing_zeros() as usize,
        out_of_domain: 0,
        shift_queries: 1,
    };

    // Single-shot: fold the whole stream in one call.
    let mut single = stream_start(index, &code, &traces[0]).expect("genesis");
    single = stream_fold_segment(index, &code, sampling, single, &traces[1..]).expect("single-shot");

    // Segmented: two "verifier calls" — fold steps [1..3] in call 1, resume
    // and fold [3..] in call 2.
    let call1 = stream_start(index, &code, &traces[0]).expect("genesis");
    let (call1, _proof1) = stream_fold(index, &code, sampling, call1, &traces[1]).expect("fold 1");
    let call1 = stream_fold_segment(index, &code, sampling, call1, &traces[2..3])
        .expect("call 1 segment");
    // Resume from the token: call 2 continues the same stream.
    let call2 = stream_fold_segment(index, &code, sampling, call1, &traces[3..]).expect("call 2");

    // Segment-transparency: the resumed accumulator equals the single-shot
    // accumulator's public statement.
    assert_eq!(single.steps, n, "single-shot folded all steps");
    assert_eq!(call2.steps, n, "segmented folded all steps");
    assert_eq!(
        single.acc.instance, call2.acc.instance,
        "segmented accumulation must equal single-shot"
    );

    // A partial stream's accumulator is a well-formed statement about the
    // prefix: the prefix token's accumulator is internally consistent (its
    // codeword tree root matches its committed instance root).
    let prefix = stream_start(index, &code, &traces[0])
        .and_then(|t| stream_fold_segment(index, &code, sampling, t, &traces[1..3]))
        .expect("prefix");
    assert_eq!(prefix.steps, 3);
    assert_eq!(
        prefix.acc.witness.tree.root(),
        prefix.acc.instance.root,
        "partial-stream accumulator is a well-formed statement about the prefix"
    );

    eprintln!(
        "[E4] {} steps, single-shot == segmented, prefix well-formed (root {:x}...)",
        n,
        prefix.acc.instance.root[0]
    );
}
