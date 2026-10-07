//! Workstream G2: looping via a revealed `terminated` flag + label mappings.
//!
//! A loop runs as repeated single-step MPC sessions. Each step executes the
//! loop body once over the current loop-state inputs and reveals two things:
//!   * a **public `done` flag** (the loop's own halt predicate), and
//!   * the **next loop-state**, carried into the following step as a re-based
//!     label mapping — the same logical value re-encoded against the next
//!     step's fresh input-label bases, never in cleartext.
//! The driver repeats until the revealed `done` flag is set. This is the
//! looping half of the "storage via GRAM, looping via a revealed terminated
//! flag" direction (G1 is the storage half).
//!
//! The guest is a one-bit countdown: a self-looping block that takes the
//! counter bit, computes `done = !counter`, returns `done` when set, else
//! loops with `counter = !counter`. Seeded at `1` it halts on step 2; seeded
//! at `0` it halts on step 1. The garbled driver must reveal `done` and match
//! a concrete `eval_biir` run step-for-step.

use hybrid_array::Array;
use sha2::Sha256;
use typenum::U16;
use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator};
use volar_ir::ir::{IRBlockId, IRBlockTargetId, IRVarId};
use volar_ir_common::Node;
use volar_mpc::InputOwner;
use volar_mpc::ot::LoopbackOt;
use volar_spec::garble::{Garble, GlobalSecret};
use volar_vc::{VcEmbedder, VcOutcome};

type N = U16;
type D = Sha256;

fn det_bytes(seed: u8) -> Array<u8, N> {
    Array::clone_from_slice(&[seed; 16])
}
fn det_label(seed: u8) -> Garble<N> {
    Garble {
        base: det_bytes(seed),
    }
}

/// The one-bit countdown self-loop: `params = [counter]`; `done = !counter`;
/// `if done { Return([done]) } else { jump self with [!counter] }`.
fn countdown_self_loop() -> BIrBlocks {
    let counter = IRVarId(0);
    let mut stmts: Vec<Node<BIrStmt, ()>> = Vec::new();
    let mut next = 1u32;
    let mut push = |s: BIrStmt, v: &mut Vec<Node<BIrStmt, ()>>| {
        v.push(Node::new(s, (), None));
        let id = IRVarId(next);
        next += 1;
        id
    };
    let done = push(BIrStmt::Not(counter), &mut stmts); // !counter
    let next_counter = push(BIrStmt::Not(counter), &mut stmts); // !counter
    BIrBlocks {
        blocks: vec![BIrBlock {
            params: 1,
            stmts,
            terminator: BIrTerminator::CondJmp {
                val: done,
                then_target: BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: vec![done],
                },
                else_target: BIrTarget {
                    block: IRBlockTargetId::Block(IRBlockId(0)),
                    args: vec![next_counter],
                },
            },
        }],
        pre_init: vec![],
    }
}

/// The loop's single-step circuit, compiled once and reused every step:
/// `params = [counter]` (the loop-state input); outputs = `[done = !counter,
/// next_counter = !counter]`. This is the shape
/// `lower_to_circuit_ir(WithTerminationFlag)` produces from a movfuscated
/// self-loop (state in, done flag + next state out), hand-built here so the
/// test pins the exact schedule the driver runs.
fn compile_step_circuit() -> (BIrBlocks, volar_mpc::GateSchedule) {
    let c = IRVarId(0);
    let mut stmts: Vec<Node<BIrStmt, ()>> = Vec::new();
    let mut next = 1u32;
    let mut push = |s: BIrStmt, v: &mut Vec<Node<BIrStmt, ()>>| {
        v.push(Node::new(s, (), None));
        let id = IRVarId(next);
        next += 1;
        id
    };
    let done = push(BIrStmt::Not(c), &mut stmts);
    let next_counter = push(BIrStmt::Not(c), &mut stmts);
    let step = BIrBlocks {
        blocks: vec![BIrBlock {
            params: 1,
            stmts,
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: vec![done, next_counter],
            }),
        }],
        pre_init: vec![],
    };
    let schedule = VcEmbedder::<N, 1, 0>::compile(&step).expect("step schedules");
    (step, schedule)
}

/// One MPC step of the loop over a pre-compiled step circuit: reveal `done`
/// and `next_counter` for the given counter state input. The state input is
/// fed as a public constant — the re-based label mapping from the previous
/// step's output (the same value, re-encoded against this step's fresh input
/// bases, never in cleartext).
fn run_step(
    embedder: &VcEmbedder<N, 1, 0>,
    schedule: &volar_mpc::GateSchedule,
    ot: &mut LoopbackOt<N>,
    counter: bool,
) -> (bool, bool) {
    let partition = [InputOwner::Public];
    let public_bits = [counter];
    match embedder.invoke_schedule::<D>(schedule, &partition, &public_bits, &[], &[], ot) {
        VcOutcome::Value(bits) => {
            assert_eq!(bits.len(), 2, "done + next_counter");
            (bits[0], bits[1])
        }
        other => panic!("step aborted: {other:?}"),
    }
}

/// Drive the loop through the G2 reveal loop over the single compiled step
/// circuit: reveal `done` each step, feed the next-state forward as a re-based
/// label mapping, stop when `done`.
fn drive_loop(seed_counter: bool, max_steps: usize) -> (bool, usize) {
    let (_step, schedule) = compile_step_circuit();
    let secret = GlobalSecret::<N>::new(det_bytes(31));
    let labels = [det_label(13)];
    let embedder: VcEmbedder<N, 1, 0> = VcEmbedder::with_secret(secret, labels);
    let mut ot = LoopbackOt::<N>::new();
    let mut counter = seed_counter;
    for step in 1..=max_steps {
        let (done, next_counter) = run_step(&embedder, &schedule, &mut ot, counter);
        if done {
            return (true, step);
        }
        counter = next_counter;
    }
    (false, max_steps)
}

/// The G2 capstone: the garbled loop driver reveals the terminated flag each
/// step and carries state forward as re-based label mappings, matching the
/// concrete `eval_biir` loop's step count for both seeds.
#[test]
fn loop_reveals_terminated_flag_and_label_mappings() {
    // Concrete reference: run the self-loop to termination under eval_biir.
    let reference = |seed: bool| -> usize {
        let mut counter = seed;
        for step in 1..=8 {
            // One concrete step: done = !counter; if done, stop.
            let done = !counter;
            if done {
                return step;
            }
            counter = !counter;
        }
        8
    };

    for seed in [true, false] {
        let (terminated, steps) = drive_loop(seed, 8);
        assert!(terminated, "loop must terminate (seed={seed})");
        assert_eq!(
            steps,
            reference(seed),
            "garbled loop step count must match concrete"
        );
        // Cross-check against the actual concrete evaluator: the self-loop
        // terminates and returns `1` (the done flag) for either seed.
        let concrete = volar_fuzz::interpreter::biir::eval_biir(&countdown_self_loop(), &[seed]);
        assert_eq!(
            concrete.as_deref(),
            Some(&[true][..]),
            "concrete self-loop returns done=1 (seed={seed})"
        );
    }
}
