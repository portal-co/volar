// @reliability: experimental
// @ai: assisted
//! Regression test for a latent `compile_schedule` bug: a `Return` argument
//! that is the result of a **multi-gate statement** (an `Or`, which expands to
//! `Not/Not/And/Not`) was resolved to its raw var id instead of the wire the
//! statement's result actually landed on, so an `Or` producing a circuit output
//! decoded against the wrong base. Found by the symbolic-ORAM S2 work, whose
//! gadget is `Or`-heavy.

use hybrid_array::Array;
use sha2::Sha256;
use typenum::U16;
use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
use volar_ir::ir::{IRBlockTargetId, IRVarId};
use volar_ir_common::Node;
use volar_mpc::InputOwner;
use volar_mpc::ot::LoopbackOt;
use volar_spec::garble::{Garble, GlobalSecret};
use volar_vc::{VcEmbedder, VcOutcome};

type N = U16;
type D = Sha256;
fn det_label(seed: u8) -> Garble<N> {
    Garble {
        base: Array::clone_from_slice(&[seed; 16]),
    }
}

fn build(which: u8) -> BIrBlocks {
    let (a, b) = (IRVarId(0), IRVarId(1));
    let mut stmts: Vec<Node<BIrStmt, ()>> = Vec::new();
    let mut next = 2u32;
    let mut push = |s: BIrStmt| {
        stmts.push(Node::new(s, (), None));
        let id = IRVarId(next);
        next += 1;
        id
    };
    let out = match which {
        // Pure And baseline.
        0 => push(BIrStmt::And(a, b)),
        // De Morgan inner: And(Not a, Not b) — multi-gate result feeding the output.
        1 => {
            let na = push(BIrStmt::Not(a));
            let nb = push(BIrStmt::Not(b));
            push(BIrStmt::And(na, nb))
        }
        // Explicit De Morgan OR: Not(And(Not a, Not b)).
        2 => {
            let na = push(BIrStmt::Not(a));
            let nb = push(BIrStmt::Not(b));
            let an = push(BIrStmt::And(na, nb));
            push(BIrStmt::Not(an))
        }
        // The Or gate itself (expands to the same 4 gates) as a circuit output.
        _ => push(BIrStmt::Or(a, b)),
    };
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
fn or_output_wire_two_party_matches_concrete() {
    // All four circuits have exactly one And gate (the Or's expansion included).
    for which in 0u8..4 {
        let circuit = build(which);
        let secret = GlobalSecret::<N>::new(Array::clone_from_slice(&[9; 16]));
        let embedder = VcEmbedder::<N, 2, 1>::with_secret(secret, [det_label(1), det_label(2)]);
        let schedule = VcEmbedder::<N, 2, 1>::compile(&circuit).expect("schedules");
        let mut ot = LoopbackOt::<N>::new();
        for a in [false, true] {
            for b in [false, true] {
                let concrete =
                    volar_fuzz::interpreter::biir::eval_biir(&circuit, &[a, b]).expect("concrete");
                let partition = [InputOwner::Public, InputOwner::Public];
                match embedder.invoke_schedule::<D>(
                    &schedule,
                    &partition,
                    &[a, b],
                    &[],
                    &[],
                    &mut ot,
                ) {
                    VcOutcome::Value(bits) => {
                        assert_eq!(bits, concrete, "which={which} a={a} b={b}")
                    }
                    other => panic!("which={which} a={a} b={b}: {other:?}"),
                }
            }
        }
    }
}
