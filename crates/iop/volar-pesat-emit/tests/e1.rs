// @reliability: experimental
// @ai: assisted
//! E1 gate: step-circuit → PESAT emission. A small CFG is movfuscated to a
//! self-looping step block (then lowered to a stepped circuit with a
//! termination flag); each executed step emits a PESAT instance whose honest
//! native trace satisfies it and whose tampered trace is rejected.

use volar_ir::ir::{
    IRBlock, IRBlockTargetId, IRBlocks, IRBranchTarget, IRStmt, IRTerminator, IRVarId,
};
use volar_ir_common::{Constant, IrType, PolyCoeffs, Type, TypeId, TypeTable};
use volar_ir_passes::lower_to_circuit::{LoweringMode, lower_to_circuit_ir};
use volar_ir_passes::movfuscate::movfuscate_ir;
use volar_pesat_emit::{build_witness, emit_step_index, eval_step_wires};

/// A two-block pipeline: block 0 computes `y = x XOR 0xFF`, jumps to block 1;
/// block 1 computes `z = y XOR 0x0F`, returns `z`.
fn two_block_cfg() -> (IRBlocks<()>, TypeTable) {
    let types = TypeTable(vec![IrType::Primitive(Type::_8), IrType::Primitive(Type::Bit)]);
    let mk = |kind: IRStmt| volar_ir_common::Node::new(kind, (), None);
    let block0 = IRBlock {
        params: vec![TypeId(0)], // x: _8
        stmts: vec![mk(IRStmt::Poly {
            ty: TypeId(0),
            coeffs: PolyCoeffs::from_iter([(vec![IRVarId(0)], 1u8)]),
            constant: Constant { hi: 0, lo: 0xFF },
        })],
        terminator: IRTerminator::Jmp {
            target: IRBranchTarget::new(
                IRBlockTargetId::Block(volar_ir::ir::IRBlockId(1)),
                vec![IRVarId(1)],
            ),
        },
    };
    let block1 = IRBlock {
        params: vec![TypeId(0)], // y: _8
        stmts: vec![mk(IRStmt::Poly {
            ty: TypeId(0),
            coeffs: PolyCoeffs::from_iter([(vec![IRVarId(0)], 1u8)]),
            constant: Constant { hi: 0, lo: 0x0F },
        })],
        terminator: IRTerminator::Jmp {
            target: IRBranchTarget::new(IRBlockTargetId::Return, vec![IRVarId(1)]),
        },
    };
    (IRBlocks::new(vec![block0, block1]), types)
}

#[test]
fn e1_movfuscate_emit_satisfies() {
    let (blocks, mut types) = two_block_cfg();
    let movfuscated = movfuscate_ir(&blocks, &mut types);
    assert!(movfuscated.is_movfuscated());

    let bit_ty = types
        .0
        .iter()
        .position(|t| matches!(t, IrType::Primitive(Type::Bit)))
        .map(|i| volar_ir::ir::IRTypeId(i as u32))
        .expect("Bit type interned by movfuscate_ir");
    let circuit = lower_to_circuit_ir(&movfuscated, &bit_ty, 1, LoweringMode::WithTerminationFlag);
    let step_block = &circuit.blocks[0];

    if std::env::var("E1_DUMP").is_ok() {
        let n_params = step_block.params.len();
        eprintln!("[DUMP] n_params={} param_widths={:?}", n_params, param_widths(&step_block, &types));
        if let IRTerminator::Jmp { target } = &step_block.terminator {
            eprintln!("[DUMP] return args: {}", target.args.len());
        }
    }
    let emission = emit_step_index(step_block, &types).expect("emit step index");
    emission.index.validate().expect("emitted index is valid");

    // Step the circuit from an all-zero state (pc = entry, x = 0) to done,
    // emitting + checking one PESAT instance per executed step.
    let param_widths: Vec<usize> = step_block
        .params
        .iter()
        .map(|&t| types.value_bit_width(t).expect("data type"))
        .collect();
    let mut state: Vec<Vec<bool>> = param_widths.iter().map(|&w| vec![false; w]).collect();

    let mut storage = Default::default();
    let mut steps = 0usize;
    loop {
        // The honest trace: the emitter's own evaluator, cross-checked
        // against the reference interpreter below.
        let wires = eval_step_wires(step_block, &types, &state).expect("eval step");
        let witness = build_witness(&emission, &wires);
        emission
            .index
            .check_instance(&witness)
            .unwrap_or_else(|e| panic!("step {steps} honest trace rejected: {e:?}"));

        // Tamper: flip the first witness bit; the instance must reject it.
        let mut bad = witness.clone();
        if let Some(first) = bad.witness.first_mut() {
            *first = volar_primitives::Galois128(first.0 ^ 1);
        }
        assert!(
            emission.index.check_instance(&bad).is_err(),
            "step {steps} tampered trace must be rejected"
        );

        // Step forward with the reference interpreter (the source of truth
        // for the (pc, state) -> (pc', state') transition).
        let outputs = volar_fuzz::interpreter::ir::eval_ir_circuit_step(
            step_block,
            &types,
            &circuit.oracles,
            &state,
            &mut storage,
        );
        // C2 differential: the emitter's own evaluator must agree with the
        // reference interpreter on the step's return values, not just be
        // self-consistent with the constraints.
        let ret_args = return_arg_wires(step_block);
        for (k, out) in outputs.iter().enumerate() {
            let my = &wires[ret_args[k].0 as usize];
            assert_eq!(
                my.as_slice(),
                out.as_slice(),
                "step {steps}: emitter evaluator diverges from reference on return {k}"
            );
        }
        let done = outputs[0].iter().any(|&b| b);
        let n_state = param_widths.len();
        state = outputs[1..1 + n_state].to_vec();
        steps += 1;
        if done {
            break;
        }
        if steps > 16 {
            panic!("step circuit did not reach Return in 16 steps");
        }
    }
    assert!(steps >= 2, "expected at least 2 steps, got {steps}");
    eprintln!(
        "[E1] {steps} steps, {} constraints, {} vars",
        emission.index.constraint_count(),
        emission.index.variable_count()
    );
}

/// The step block's `Return` terminator's argument wire ids.
fn return_arg_wires(block: &IRBlock<()>) -> Vec<IRVarId> {
    match &block.terminator {
        IRTerminator::Jmp { target } if target.dest == IRBlockTargetId::Return => {
            target.args.clone()
        }
        other => panic!("step block must return; got {other:?}"),
    }
}

fn param_widths(block: &IRBlock<()>, types: &TypeTable) -> Vec<usize> {
    block.params.iter().map(|&t| types.value_bit_width(t).unwrap()).collect()
}
