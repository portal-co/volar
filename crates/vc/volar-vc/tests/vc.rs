// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//!
//! End-to-end vc embedder tests: a lowered guest circuit (`BIrBlocks`) is
//! compiled to a `GateSchedule`, partitioned by argument visibility, and run
//! through the two-party session — asserting the vc-spec outcome (concrete
//! result revealed) matches concrete evaluation for every visibility
//! combination.

use hybrid_array::Array;
use sha2::Sha256;
use typenum::U16;
use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
use volar_ir::ir::{IRBlockTargetId, IRVarId};
use volar_ir_common::Node;
use volar_mpc::InputOwner;
use volar_mpc::ot::LoopbackOt;
use volar_spec::garble::{Garble, GlobalSecret};
use volar_vc::{VcEmbedder, VcOutcome, partition_from_sides};
use volar_side::SideId;

type N = U16;
type D = Sha256;

fn det_bytes(seed: u8) -> Array<u8, N> {
    Array::<u8, N>::from_fn(|i| seed.wrapping_mul(31).wrapping_add(i as u8))
}
fn det_label(seed: u8) -> Garble<N> {
    Garble { base: det_bytes(seed) }
}

/// `(x0 ^ x1) & x2` — a 3-input, 1-AND circuit: wires 0,1,2 inputs;
/// wire 3 = Xor(0,1); wire 4 = And(3,2); Return wire 4. In `BIrBlocks`,
/// stmt `i` (0-indexed) produces `IRVarId(params + i)`, so the Xor result is
/// `IRVarId(3)` and the And reads it plus input `IRVarId(2)` (x2).
fn xor_and_circuit() -> BIrBlocks {
    BIrBlocks {
        blocks: vec![BIrBlock {
            params: 3,
            stmts: vec![
                Node::new(BIrStmt::Xor(IRVarId(0), IRVarId(1)), (), None),
                Node::new(BIrStmt::And(IRVarId(3), IRVarId(2)), (), None),
            ],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: vec![IRVarId(4)],
            }),
        }],
        pre_init: vec![],
    }
}

fn concrete(inputs: &[bool; 3]) -> bool {
    (inputs[0] ^ inputs[1]) & inputs[2]
}

fn embedder() -> VcEmbedder<N, 3, 1> {
    let secret = GlobalSecret::<N>::new(det_bytes(13));
    let labels = [det_label(7), det_label(91), det_label(33)];
    VcEmbedder::with_secret(secret, labels)
}

/// The schedule compiler produces the right gate structure.
#[test]
fn compile_xor_and_schedule() {
    let schedule = VcEmbedder::<N, 3, 1>::compile(&xor_and_circuit()).expect("compiles");
    assert_eq!(schedule.num_inputs, 3);
    assert_eq!(schedule.and_count(), 1);
    assert_eq!(schedule.output, 4);
    // Xor then And → two gates.
    assert_eq!(schedule.gates.len(), 2);
}

/// `Or` is expanded by De Morgan into Not/And/Not.
#[test]
fn compile_or_expands() {
    use volar_mpc::Gate;
    let circuit = BIrBlocks {
        blocks: vec![BIrBlock {
            params: 2,
            stmts: vec![Node::new(BIrStmt::Or(IRVarId(0), IRVarId(1)), (), None)],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: vec![IRVarId(2)],
            }),
        }],
        pre_init: vec![],
    };
    let schedule = VcEmbedder::<N, 2, 1>::compile(&circuit).expect("compiles");
    // Or(a,b) → Not, Not, And, Not: 4 gates, exactly 1 AND.
    assert_eq!(schedule.gates.len(), 4);
    assert_eq!(schedule.and_count(), 1);
    assert!(matches!(schedule.gates[2], Gate::And(_, _)));
}

/// Full vc invoke: every visibility assignment of the 3 inputs runs through
/// the embedder and matches concrete evaluation, across all 8 input combos.
#[test]
fn vc_invoke_all_visibilities() {
    let circuit = xor_and_circuit();
    let owners = [
        volar_vc::VcVisibility::Public,
        volar_vc::VcVisibility::Private,
        volar_vc::VcVisibility::Blind,
    ];
    // Intern side ids 0/1/2 = public/local/remote, mirroring the lowering.
    let (public, local, remote) = (SideId(0), SideId(1), SideId(2));

    for combo in 0u32..27 {
        // Assign each input a visibility.
        let mut vis = [volar_vc::VcVisibility::Public; 3];
        let mut c = combo;
        for v in vis.iter_mut() {
            *v = owners[(c % 3) as usize];
            c /= 3;
        }
        // Map visibility → per-bit side, then to InputOwner.
        let side_of = |i: usize| match vis[i] {
            volar_vc::VcVisibility::Public => Some(public),
            volar_vc::VcVisibility::Private => Some(local),
            volar_vc::VcVisibility::Blind => Some(remote),
        };
        let partition = partition_from_sides(3, public, local, remote, side_of);

        for inputs in 0u32..8 {
            let b = [
                (inputs >> 0) & 1 == 1,
                (inputs >> 1) & 1 == 1,
                (inputs >> 2) & 1 == 1,
            ];
            // Split inputs into the three visibility vectors.
            let mut public_b = Vec::new();
            let mut private_b = Vec::new();
            let mut blind_b = Vec::new();
            for (i, &bit) in b.iter().enumerate() {
                match vis[i] {
                    volar_vc::VcVisibility::Public => public_b.push(bit),
                    volar_vc::VcVisibility::Private => private_b.push(bit),
                    volar_vc::VcVisibility::Blind => blind_b.push(bit),
                }
            }
            let mut ot = LoopbackOt::<N>::new();
            let out = embedder().invoke::<D, _>(
                &circuit,
                &partition,
                &public_b,
                &private_b,
                &blind_b,
                &mut ot,
            );
            match out {
                VcOutcome::Value(bits) => {
                    assert_eq!(bits.len(), 1);
                    assert_eq!(bits[0], concrete(&b), "vis {vis:?} inputs {b:?}");
                }
                other => panic!("expected Value, got {other:?} for vis {vis:?} inputs {b:?}"),
            }
        }
    }
}

/// Mutual privacy: a blind (remote) input bit must cross only via OT — the
/// embedder never sees it in cleartext. Structural assertion via the
/// partition: blind ⇒ `InputOwner::Evaluator`, which the session delivers by
/// OT, never as a garbler-sent label.
#[test]
fn vc_blind_input_is_ot_delivered() {
    let (public, local, remote) = (SideId(0), SideId(1), SideId(2));
    let partition = partition_from_sides(3, public, local, remote, |i| match i {
        0 => Some(public),
        1 => Some(local),
        2 => Some(remote),
        _ => None,
    });
    assert_eq!(
        partition,
        [
            InputOwner::Public,
            InputOwner::Garbler,
            InputOwner::Evaluator
        ]
    );
}

/// A non-circuit (multi-block) input is rejected as an embedder error, not a
/// panic — vc-spec `Error`, not a crash.
#[test]
fn vc_rejects_non_circuit() {
    // Two blocks → not a fused single-block circuit.
    let bad = BIrBlocks {
        blocks: vec![
            BIrBlock::<()> {
                params: 1,
                stmts: vec![],
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Block(volar_ir::ir::IRBlockId(1)),
                    args: vec![IRVarId(0)],
                }),
            },
            BIrBlock::<()> {
                params: 1,
                stmts: vec![],
                terminator: BIrTerminator::Jmp(BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: vec![IRVarId(0)],
                }),
            },
        ],
        pre_init: vec![],
    };
    let mut ot = LoopbackOt::<N>::new();
    let out = embedder().invoke::<D, _>(
        &bad,
        &[InputOwner::Public],
        &[true],
        &[],
        &[],
        &mut ot,
    );
    assert!(matches!(out, VcOutcome::Error(_)), "got {out:?}");
}
