// @reliability: experimental
// @ai: assisted
//! E3 gate: storage boundary. A memory-writing loop's step circuit carries the
//! `(mem_prod, mem_cons)` multiset accumulators across the chain boundary; the
//! honest trace satisfies every step and links, and a flipped read value is
//! caught by the absorption constraint.

use volar_ir::ir::{
    IRBlock, IRBlockTargetId, IRBlocks, IRBranchTarget, IRStmt, IRTerminator, IRVarId,
};
use volar_ir_common::{Constant, IrType, PolyCoeffs, Type, TypeId, TypeTable};
use volar_ir_common::{Node, StorageId};
use volar_ir_passes::lower_to_circuit::{LoweringMode, lower_to_circuit_ir};
use volar_ir_passes::movfuscate::movfuscate_ir;
use volar_pesat_emit::{
    ChainManifest, StepEmission, build_witness, chain_manifest, derive_challenges, emit_step_index,
    eval_step_wires, step_accumulators, verify_chain,
};
use volar_primitives::Galois128;

/// A memory loop: block 0 writes `x` to `mem[0]`, reads `mem[0]` into `y`,
/// jumps to block 1; block 1 writes `y` to `mem[1]`, reads `mem[1]` into `z`,
/// returns `z`. Exercises writes AND reads on both steps.
fn mem_loop_cfg() -> (IRBlocks<()>, TypeTable) {
    let types = TypeTable(vec![IrType::Primitive(Type::_8), IrType::Primitive(Type::Bit)]);
    let mk = |kind: IRStmt| Node::new(kind, (), None);
    let block0 = IRBlock {
        params: vec![TypeId(0)], // x: _8
        stmts: vec![
            mk(IRStmt::Const(Constant { hi: 0, lo: 0 }, TypeId(0))), // v1: addr 0
            mk(IRStmt::StorageWrite {
                storage: StorageId(1),
                src: IRVarId(0),
                ty: TypeId(0),
                addr: IRVarId(1),
            }),
            mk(IRStmt::StorageRead {
                storage: StorageId(1),
                ty: TypeId(0),
                addr: IRVarId(1),
            }), // v3: y
        ],
        terminator: IRTerminator::Jmp {
            target: IRBranchTarget::new(
                IRBlockTargetId::Block(volar_ir::ir::IRBlockId(1)),
                vec![IRVarId(3)],
            ),
        },
    };
    let block1 = IRBlock {
        params: vec![TypeId(0)], // y: _8
        stmts: vec![
            mk(IRStmt::Const(Constant { hi: 0, lo: 1 }, TypeId(0))), // v1: addr 1
            mk(IRStmt::StorageWrite {
                storage: StorageId(1),
                src: IRVarId(0),
                ty: TypeId(0),
                addr: IRVarId(1),
            }),
            mk(IRStmt::StorageRead {
                storage: StorageId(1),
                ty: TypeId(0),
                addr: IRVarId(1),
            }), // v3: z
        ],
        terminator: IRTerminator::Jmp {
            target: IRBranchTarget::new(IRBlockTargetId::Return, vec![IRVarId(3)]),
        },
    };
    (IRBlocks::new(vec![block0, block1]), types)
}

/// The step block's `Return` argument wire values from a wire trace.
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
fn e3_storage_boundary_carries_and_catches() {
    let (blocks, mut types) = mem_loop_cfg();
    let movfuscated = movfuscate_ir(&blocks, &mut types);
    let bit_ty = types
        .0
        .iter()
        .position(|t| matches!(t, IrType::Primitive(Type::Bit)))
        .map(|i| volar_ir::ir::IRTypeId(i as u32))
        .expect("Bit interned");
    let circuit = lower_to_circuit_ir(&movfuscated, &bit_ty, 1, LoweringMode::WithTerminationFlag);
    let step_block = &circuit.blocks[0];

    let emission: StepEmission = emit_step_index(step_block, &types).expect("emit");
    assert!(emission.has_storage, "mem loop must have storage");
    emission.index.validate().expect("valid index");
    let manifest: ChainManifest = chain_manifest(&emission, step_block);
    let challenges = derive_challenges(step_block);

    let n_state = step_block.params.len();
    let param_widths: Vec<usize> = step_block
        .params
        .iter()
        .map(|&t| types.value_bit_width(t).expect("data type"))
        .collect();

    // Drive the chain with the emitter's own evaluator (consistent storage),
    // threading (pc, state) and the memory accumulators.
    let mut storage = volar_pesat_emit::StorageMap::default();
    let mut state: Vec<Vec<bool>> = param_widths.iter().map(|&w| vec![false; w]).collect();
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
        let witness = build_witness(
            &emission,
            &wires,
            prod,
            cons,
            ts,
            &acc_chain,
            prod_out,
            cons_out,
        );
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
        if done || step_idx > 16 {
            break;
        }
    }
    assert!(step_idx >= 2, "expected >= 2 steps, got {step_idx}");

    // The chain links (including the accumulator boundary).
    verify_chain(&manifest, &traces).expect("honest chain links");

    // Tamper A: flip a read RESULT bit in step 0 — the read's absorption
    // constraint (mem_cons) must catch it. Find the first StorageRead stmt's
    // output wire.
    let n_params = step_block.params.len();
    let read_stmt = step_block
        .stmts
        .iter()
        .position(|n| matches!(n.kind, IRStmt::StorageRead { .. }))
        .expect("a read in the mem loop");
    let read_var = IRVarId((n_params + read_stmt) as u32);
    let read_witness_idx = emission.var_base[read_var.0 as usize] - emission.public_len;
    let mut bad = traces.clone();
    bad[0].witness[read_witness_idx] = Galois128(bad[0].witness[read_witness_idx].0 ^ 1);
    assert!(
        emission.index.check_instance(&bad[0]).is_err(),
        "flipped read value must be caught by the absorption constraint"
    );

    // Tamper B: perturb the accumulator boundary — set step 1's public
    // `mem_cons_in` (the last boundary-in wire) to a wrong value; the link to
    // step 0's `mem_cons_out` must break.
    let mut bad2 = traces.clone();
    let cons_in_idx = *manifest.boundary_in.last().expect("cons_in in boundary");
    bad2[1].public[cons_in_idx] = Galois128(bad2[1].public[cons_in_idx].0 ^ 1);
    assert!(
        verify_chain(&manifest, &bad2).is_err(),
        "perturbed accumulator boundary must break the chain"
    );

    eprintln!(
        "[E3] {} steps, {} constraints, {} vars, boundary {} wires",
        step_idx,
        emission.index.constraint_count(),
        emission.index.variable_count(),
        manifest.boundary_in.len()
    );
}
