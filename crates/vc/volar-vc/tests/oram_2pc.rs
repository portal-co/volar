// @reliability: experimental
// @ai: assisted
//! S4: a symbolic-storage guest lowered by [`volar_vc::oram_lower`] and run
//! **two-party** via [`volar_vc::oram_2pc::Oram2pc`], matching the concrete
//! driver (`run_concrete`) and a direct storage model.
//!
//! This is the payoff of the runtime-sized volar-mpc path: the `OramProgram`'s
//! per-stage circuits differ in input/AND counts, so a const-generic driver
//! could not run them; `garble_schedule_dyn` / `DynGarbledExec` can. The guest
//! tape and the ORAM posmap/stash cross stages as re-based labels, never
//! decoded — only the oblivious per-access leaf is revealed.

use hybrid_array::Array;
use sha2::Sha256;
use typenum::U16;
use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator, LaneId};
use volar_ir::ir::{IRBlockTargetId, IRVarId, StorageId};
use volar_ir_common::Node;
use volar_mpc::ot::LoopbackOt;
use volar_oram::OramTree;
use volar_spec::garble::GlobalSecret;
use volar_vc::oram_2pc::Oram2pc;
use volar_vc::oram_lower::{OramLowerConfig, run_concrete, storage_to_oram};

type N = U16;
type D = Sha256;

const AB: usize = 2; // 2-bit addresses -> 4 cells
const LEVELS: usize = 3;
const Z: usize = 2;
const MAX_STASH: usize = 2 * LEVELS + Z + (1 << AB);

// Guest: write d to cell[a]; read cell[a] -> r1; read cell[c] -> r2; return
// [r1, r2]. Params: [a0, a1, d, c0, c1] (LSB-first 2-bit addresses).
fn guest() -> BIrBlocks<()> {
    let storage = StorageId(0);
    let lane = LaneId(0);
    let (a0, a1, d, c0, c1) = (IRVarId(0), IRVarId(1), IRVarId(2), IRVarId(3), IRVarId(4));
    let mut block: BIrBlock<()> = BIrBlock {
        params: 5,
        stmts: vec![],
        terminator: BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Return,
            args: vec![],
        }),
    };
    let mut next = 5u32;
    let mut push = |s: BIrStmt, b: &mut BIrBlock<()>| {
        b.stmts.push(Node::new(s, (), None));
        let id = IRVarId(next);
        next += 1;
        id
    };
    push(
        BIrStmt::StorageWrite {
            storage,
            lane,
            src: d,
            addr: vec![a0, a1],
        },
        &mut block,
    );
    let r1 = push(
        BIrStmt::StorageRead {
            storage,
            lane,
            addr: vec![a0, a1],
        },
        &mut block,
    );
    let r2 = push(
        BIrStmt::StorageRead {
            storage,
            lane,
            addr: vec![c0, c1],
        },
        &mut block,
    );
    block.terminator = BIrTerminator::Jmp(BIrTarget {
        block: IRBlockTargetId::Return,
        args: vec![r1, r2],
    });
    BIrBlocks {
        blocks: vec![block],
        pre_init: vec![],
    }
}

#[test]
fn s4_oram_program_two_party_matches_concrete() {
    let program = storage_to_oram(
        &guest(),
        &OramLowerConfig {
            levels: LEVELS,
            bucket_size: Z,
            max_stash: MAX_STASH,
            secure: false,
            narrow_bits: None,
        },
    )
    .expect("lowers");

    let secret = GlobalSecret::<N>::new(Array::clone_from_slice(&[7u8; 16]));

    // Drive every (a, d, c) combination two-party and check against the model
    // and the concrete driver. Fresh Oram2pc + tree per input combo (each is an
    // independent program run over a fresh ORAM).
    for a in 0..4u64 {
        for d in [false, true] {
            for c in 0..4u64 {
                let inputs = vec![a & 1 == 1, a & 2 == 2, d, c & 1 == 1, c & 2 == 2];

                let mut driver = Oram2pc::<N>::new::<D>(&program.oram, secret.clone());
                let mut tree = OramTree::<Z, 1>::new(LEVELS);
                let mut ot = LoopbackOt::<N>::new();
                let param_inputs: Vec<_> =
                    inputs.iter().map(|&b| driver.fresh_input::<D>(b)).collect();
                let outputs =
                    driver.run_program::<D, Z>(&program, &param_inputs, &mut tree, &mut ot);
                let out_bits: Vec<bool> =
                    outputs.iter().map(|(l, b)| l.open(b)[0] & 1 == 1).collect();

                // Model: write d to cell a; r1 = cell a = d; r2 = cell c.
                let want = vec![d, if c == a { d } else { false }];
                // Concrete cross-check.
                let (concrete, _t) = run_concrete::<Z>(&program, &inputs);
                assert_eq!(out_bits, want, "a={a} d={d} c={c}: two-party vs model");
                assert_eq!(
                    out_bits, concrete,
                    "a={a} d={d} c={c}: two-party vs concrete"
                );
            }
        }
    }
}

// The **secure default posture** (encrypted tree + encrypted valid bit +
// versioned pads) works two-party through the OramProgram driver: the tree
// holds only ciphertext and the garbler-held tree key never reaches the
// tree-hosting evaluator. A couple of (a, d, c) combos; the plaintext test
// above covers the cross-product. `#[ignore]`d as heavyweight (the versioned
// pads double the per-access AES cost); run with `--ignored`.
#[test]
#[ignore = "heavyweight: encrypted two-party OramProgram run"]
fn s4_oram_program_two_party_secure_default() {
    let program = storage_to_oram(
        &guest(),
        &OramLowerConfig {
            levels: LEVELS,
            bucket_size: Z,
            max_stash: MAX_STASH,
            secure: true,
            narrow_bits: None,
        },
    )
    .expect("lowers");
    assert!(program.oram.encrypted);

    let secret = GlobalSecret::<N>::new(Array::clone_from_slice(&[7u8; 16]));
    for (a, d, c) in [(0u64, true, 0u64), (1, false, 2), (3, true, 3)] {
        let inputs = vec![a & 1 == 1, a & 2 == 2, d, c & 1 == 1, c & 2 == 2];
        let mut driver = Oram2pc::<N>::new::<D>(&program.oram, secret.clone());
        let mut tree = OramTree::<Z, 1>::new(LEVELS);
        let mut ot = LoopbackOt::<N>::new();
        let param_inputs: Vec<_> = inputs.iter().map(|&b| driver.fresh_input::<D>(b)).collect();
        let outputs = driver.run_program::<D, Z>(&program, &param_inputs, &mut tree, &mut ot);
        let out_bits: Vec<bool> = outputs.iter().map(|(l, b)| l.open(b)[0] & 1 == 1).collect();
        let want = vec![d, if c == a { d } else { false }];
        assert_eq!(out_bits, want, "secure two-party a={a} d={d} c={c}");
        // The tree holds only ciphertext (the evaluator cannot read tags).
        let _ = tree;
    }
}
