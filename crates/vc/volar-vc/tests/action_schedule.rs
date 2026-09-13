//! `compile_schedule` lowers `BIrStmt::ActionCall` (the call handle — no
//! gate wire) plus one `ActionBit` per result bit into a schedule-level
//! action spec + `Gate::ActionBit` gates, with the call's wires resolved
//! through `stmt_wire`.

use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
use volar_ir::ir::{IRBlockTargetId, IRVarId};
use volar_ir_common::Node;
use volar_mpc::Gate;

fn build() -> BIrBlocks {
    // params: p0 p1
    let (p0, p1) = (IRVarId(0), IRVarId(1));
    let mut stmts: Vec<Node<BIrStmt, ()>> = Vec::new();
    let mut next = 2u32;
    let mut push = |s: BIrStmt| {
        stmts.push(Node::new(s, (), None));
        let id = IRVarId(next);
        next += 1;
        id
    };
    let a = push(BIrStmt::And(p0, p1)); // wire 2
    let call = push(BIrStmt::ActionCall {
        name: "echo".into(),
        guard: a,
        args: vec![p0, a],
        fallback: vec![p1, p1],
        num_bits: 2,
    }); // handle (no wire)
    let b0 = push(BIrStmt::ActionBit { call, bit: 0 });
    let b1 = push(BIrStmt::ActionBit { call, bit: 1 });
    let out = push(BIrStmt::Xor(b0, b1));
    BIrBlocks {
        blocks: vec![BIrBlock {
            params: 2,
            stmts,
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: vec![out],
            }),
        }],
        pre_init: vec![],
    }
}

#[test]
fn action_call_lowers_to_action_bit_gates() {
    let sched = volar_vc::compile_schedule(&build()).expect("compile");
    // One action spec with the resolved wires.
    assert_eq!(sched.actions.len(), 1);
    let spec = &sched.actions[0];
    assert_eq!(spec.name, "echo");
    assert_eq!(spec.guard, 2, "guard is the And's wire");
    assert_eq!(spec.arg_wires, vec![0, 2]);
    assert_eq!(spec.fallback_wires, vec![1, 1]);
    assert_eq!(spec.num_bits, 2);
    // Gates: And, ActionBit0, ActionBit1, Xor — the ActionCall produced none.
    assert_eq!(sched.gates.len(), 4);
    assert!(matches!(sched.gates[0], Gate::And(0, 1)));
    assert!(matches!(
        sched.gates[1],
        Gate::ActionBit { call: 0, bit: 0 }
    ));
    assert!(matches!(
        sched.gates[2],
        Gate::ActionBit { call: 0, bit: 1 }
    ));
    assert!(matches!(sched.gates[3], Gate::Xor(3, 4)));
    assert_eq!(sched.output_wires(), &[5]);
    // The action-bearing schedule must run through the strict-actions session.
    let elim = volar_mpc::strict::eliminate_nots(&sched).expect("eliminate");
    assert_eq!(elim.schedule.actions.len(), 1);
    assert!(elim.schedule.actions[0].arg_polarity.iter().all(|&p| !p));
}
