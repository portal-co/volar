// @reliability: experimental
// @ai: assisted
//! E2 gate: `(pc, state)` linking + chain manifest. A multi-block CFG is
//! movfuscated and stepped to completion; the per-step witnesses must link
//! (each step's boundary-out equals the next step's boundary-in), and a
//! perturbed boundary wire must break the chain.

use volar_ir::ir::{
    IRBlock, IRBlockTargetId, IRBlocks, IRBranchTarget, IRStmt, IRTerminator, IRVarId,
};
use volar_ir_common::{Constant, IrType, PolyCoeffs, Type, TypeId, TypeTable};
use volar_ir_passes::lower_to_circuit::{LoweringMode, lower_to_circuit_ir};
use volar_ir_passes::movfuscate::movfuscate_ir;
use volar_pesat_emit::{build_witness, chain_manifest, emit_step_index, eval_step_wires, verify_chain};

/// An `n`-block XOR pipeline: block `i` computes `v ^= C_i` and jumps to block
/// `i+1`; the last block returns. Runs `n` steps, giving `n-1` links.
fn xor_chain_cfg(n: usize) -> (IRBlocks<()>, TypeTable) {
    let types = TypeTable(vec![IrType::Primitive(Type::_8), IrType::Primitive(Type::Bit)]);
    let mk = |kind: IRStmt| volar_ir_common::Node::new(kind, (), None);
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

/// Step the circuit to done, collecting each step's emission witness.
fn run_chain(
    blocks: &IRBlocks<()>,
    types: &TypeTable,
) -> (
    volar_pesat_emit::StepEmission,
    volar_pesat_emit::ChainManifest,
    Vec<volar_iop::warp::PesatWitness<volar_primitives::Galois128>>,
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
    let manifest = chain_manifest(&emission, step_block);

    let param_widths: Vec<usize> = step_block
        .params
        .iter()
        .map(|&t| types.value_bit_width(t).expect("data type"))
        .collect();
    let mut state: Vec<Vec<bool>> = param_widths.iter().map(|&w| vec![false; w]).collect();

    let mut storage = Default::default();
    let mut traces = Vec::new();
    loop {
        let wires = eval_step_wires(step_block, &types, &state).expect("eval");
        traces.push(build_witness(&emission, &wires));
        let outputs = volar_fuzz::interpreter::ir::eval_ir_circuit_step(
            step_block,
            &types,
            &circuit.oracles,
            &state,
            &mut storage,
        );
        let done = outputs[0].iter().any(|&b| b);
        state = outputs[1..1 + param_widths.len()].to_vec();
        if done || traces.len() > 16 {
            break;
        }
    }
    (emission, manifest, traces)
}

#[test]
fn e2_chain_links_hold_and_perturbation_breaks() {
    let (blocks, types) = xor_chain_cfg(4);
    let (emission, manifest, traces) = run_chain(&blocks, &types);
    assert!(traces.len() >= 4, "expected >= 4 steps, got {}", traces.len());

    // Each step's instance is satisfied by its own honest trace.
    for (i, trace) in traces.iter().enumerate() {
        emission
            .index
            .check_instance(trace)
            .unwrap_or_else(|e| panic!("step {i} rejected: {e:?}"));
    }

    // The chain links.
    verify_chain(&manifest, &traces).expect("honest chain links");

    // Perturbing a boundary-out wire in step 0 breaks the link to step 1.
    let mut bad = traces.clone();
    let out_idx = manifest.boundary_out[0] - manifest.public_len;
    bad[0].witness[out_idx] = volar_primitives::Galois128(bad[0].witness[out_idx].0 ^ 1);
    assert!(
        verify_chain(&manifest, &bad).is_err(),
        "perturbed boundary wire must break the chain"
    );

    // Structural invariant (the one place shape assertions are permitted):
    // the boundary widths match and the boundary-in is exactly the public
    // prefix in order.
    assert_eq!(manifest.boundary_in.len(), manifest.boundary_out.len());
    assert_eq!(
        manifest.boundary_in,
        (0..manifest.public_len).collect::<Vec<_>>()
    );

    // The manifest round-trips through its canonical text form.
    let canon = manifest.to_canonical();
    let back = volar_pesat_emit::ChainManifest::from_canonical(&canon).expect("parse");
    assert_eq!(back, manifest, "manifest canonical round-trip");
    eprintln!(
        "[E2] {} steps, {} links verified, {} boundary wires",
        traces.len(),
        traces.len() - 1,
        manifest.boundary_in.len()
    );
}
