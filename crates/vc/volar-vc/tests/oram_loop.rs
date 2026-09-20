// @reliability: experimental
// @ai: assisted
//! S4 loop integration: a **looping** guest whose loop body does a
//! *symbolic-address* storage read, run two-party through the ORAM, threading
//! both the loop state and the ORAM client state across step-circuit
//! invocations as re-based labels that never decode.
//!
//! This is the mechanism that closes the original loop gap (the movfuscated
//! loop step circuit whose spill addresses are symbolic). It is demonstrated on
//! a hand-built narrow-address loop — 2-bit addresses → a 4-cell ORAM — because
//! a real waffle-lowered loop's 32-bit spill addresses would span a 2^32-cell
//! ORAM (the scaling wall deferred to S5's recursive/encrypted ORAM).
//!
//! The guest: a setup program writes known bits to cells 1..=3, then a loop
//! `acc ^= storage[idx]; idx--` runs until `idx == 0`, revealing `done` each
//! step and the accumulated `acc` at the end. The read address `idx` is loop
//! state — genuinely symbolic. Result checked against a model for several loop
//! bounds.

use hybrid_array::Array;
use sha2::Sha256;
use typenum::U16;
use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator, LaneId};
use volar_ir::ir::{IRBlockTargetId, IRVarId, StorageId};
use volar_ir_common::Node;
use volar_mpc::ot::LoopbackOt;
use volar_oram::OramTree;
use volar_spec::garble::{Eval, Garble, GlobalSecret};
use volar_vc::oram_2pc::Oram2pc;
use volar_vc::oram_lower::{OramLowerConfig, storage_to_oram};

type N = U16;
type D = Sha256;

const AB: usize = 2; // 2-bit addresses -> 4 cells
const LEVELS: usize = 3;
const Z: usize = 2;
const MAX_STASH: usize = 2 * LEVELS + Z + (1 << AB);

fn config() -> OramLowerConfig {
    OramLowerConfig {
        levels: LEVELS,
        bucket_size: Z,
        max_stash: MAX_STASH,
        secure: false,
        shared_tree_key: false,
        narrow_bits: None,
    }
}

// Setup guest: write 1 to cells 1, 2, 3 (concrete addresses, constant value).
// params = 0. Cell 0 stays 0.
fn setup_guest() -> BIrBlocks<()> {
    let storage = StorageId(0);
    let lane = LaneId(0);
    let mut block: BIrBlock<()> = BIrBlock {
        params: 0,
        stmts: vec![],
        terminator: BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Return,
            args: vec![],
        }),
    };
    let mut next = 0u32;
    let mut push = |s: BIrStmt, b: &mut BIrBlock<()>| {
        b.stmts.push(Node::new(s, (), None));
        let id = IRVarId(next);
        next += 1;
        id
    };
    let z = push(BIrStmt::Zero, &mut block);
    let o = push(BIrStmt::One, &mut block);
    // Addresses are LSB-first [bit0, bit1]: cell 1 = [o, z], 2 = [z, o], 3 = [o, o].
    for addr in [vec![o, z], vec![z, o], vec![o, o]] {
        push(
            BIrStmt::StorageWrite {
                storage,
                lane,
                src: o,
                addr,
            },
            &mut block,
        );
    }
    block.terminator = BIrTerminator::Jmp(BIrTarget {
        block: IRBlockTargetId::Return,
        args: vec![],
    });
    BIrBlocks {
        blocks: vec![block],
        pre_init: vec![],
    }
}

// Loop step circuit. Params: [idx0, idx1, acc] (idx is the 2-bit counter).
// Body: acc ^= storage[idx]; idx -= 1; done = (idx == 0).
// Outputs (Return args): [done, next_idx0, next_idx1, next_acc, result = acc].
fn loop_step() -> BIrBlocks<()> {
    let storage = StorageId(0);
    let lane = LaneId(0);
    let (idx0, idx1, acc) = (IRVarId(0), IRVarId(1), IRVarId(2));
    let mut block: BIrBlock<()> = BIrBlock {
        params: 3,
        stmts: vec![],
        terminator: BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Return,
            args: vec![],
        }),
    };
    let mut next = 3u32;
    let mut push = |s: BIrStmt, b: &mut BIrBlock<()>| {
        b.stmts.push(Node::new(s, (), None));
        let id = IRVarId(next);
        next += 1;
        id
    };
    let v = push(
        BIrStmt::StorageRead {
            storage,
            lane,
            addr: vec![idx0, idx1],
        },
        &mut block,
    ); // var 3
    let acc2 = push(BIrStmt::Xor(acc, v), &mut block); // var 4
    let nidx0 = push(BIrStmt::Not(idx0), &mut block); // var 5  (next idx0)
    let nidx1 = push(BIrStmt::Xor(idx1, nidx0), &mut block); // var 6  (next idx1)
    let not_idx1 = push(BIrStmt::Not(idx1), &mut block); // var 7
    let done = push(BIrStmt::And(nidx0, not_idx1), &mut block); // var 8
    block.terminator = BIrTerminator::Jmp(BIrTarget {
        block: IRBlockTargetId::Return,
        // [done, next_idx0, next_idx1, next_acc, result = acc]
        args: vec![done, nidx0, nidx1, acc2, acc],
    });
    BIrBlocks {
        blocks: vec![block],
        pre_init: vec![],
    }
}

fn reveal(l: &Eval<N>, b: &Garble<N>) -> bool {
    l.open(b)[0] & 1 == 1
}

#[test]
fn s4_loop_symbolic_storage_two_party() {
    let cfg = config();
    let setup = storage_to_oram(&setup_guest(), &cfg).expect("setup lowers");
    let step = storage_to_oram(&loop_step(), &cfg).expect("step lowers");
    // Same geometry -> one driver serves both, sharing the ORAM.
    assert_eq!(setup.oram.num_addrs, step.oram.num_addrs);

    let secret = GlobalSecret::<N>::new(Array::clone_from_slice(&[7u8; 16]));
    // One driver across the whole run: ORAM state (posmap/stash) persists from
    // the setup writes into the loop reads.
    let mut driver = Oram2pc::<N>::new::<D>(&setup.oram, secret);
    let mut tree = OramTree::<Z, 1>::new(LEVELS);
    let mut ot = LoopbackOt::<N>::new();

    // Setup: populate storage[1..=3] = 1 (cell 0 = 0). params = 0.
    driver.run_program::<D, Z>(&setup, &[], &mut tree, &mut ot);

    // Loop for several bounds n. State layout per step: [idx0, idx1, acc].
    // Storage model: cell 0 = 0, cells 1..=3 = 1.
    let storage_model = [false, true, true, true];
    for n in 1u64..=3 {
        // Initial loop state: idx = n, acc = 0 (fresh-encoded first-step params).
        let mut state: Vec<(Eval<N>, Garble<N>)> = vec![
            driver.fresh_input::<D>(n & 1 == 1),
            driver.fresh_input::<D>(n & 2 == 2),
            driver.fresh_input::<D>(false),
        ];
        let mut result = None;
        // Bounded steps: n+1 to reach idx == 0, plus margin.
        for _ in 0..(n + 3) {
            let outputs = driver.run_program::<D, Z>(&step, &state, &mut tree, &mut ot);
            assert_eq!(outputs.len(), 5, "step outputs [done, state x3, result]");
            let done = reveal(&outputs[0].0, &outputs[0].1);
            if done {
                result = Some(reveal(&outputs[4].0, &outputs[4].1));
                break;
            }
            // Thread the next loop state (re-based labels, never decoded).
            state = vec![outputs[1].clone(), outputs[2].clone(), outputs[3].clone()];
        }
        let result = result.expect("loop terminates");
        // Model: acc = XOR of storage[n..=1].
        let want = (1..=n).fold(false, |a, i| a ^ storage_model[i as usize]);
        assert_eq!(result, want, "loop bound {n}");
    }
}
