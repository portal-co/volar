// @reliability: experimental
// @ai: assisted
//! S3: a symbolic-storage guest lowered to per-access ORAM segments, driven
//! concretely, matches direct storage semantics.
//!
//! The guest writes `d` to cell `[a]`, then reads cell `[a]` and cell `[c]`,
//! all at *symbolic* (input) addresses — the regime `compile_schedule` rejects
//! with `SymbolicStorageAddress`. Lowered via [`volar_vc::oram_lower`], it runs
//! through the in-circuit ORAM with the physical tree external.

use volar_ir::boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator, LaneId};
use volar_ir::ir::{IRBlockTargetId, IRVarId, StorageId};
use volar_ir_common::Node;
use volar_vc::oram_lower::{OramLowerConfig, Stage, run_concrete, storage_to_oram};

const AB: usize = 2; // 2-bit addresses -> 4 cells
const LEVELS: usize = 3;
const Z: usize = 2;
const MAX_STASH: usize = 2 * LEVELS + Z + (1 << AB);

// Guest: write d to cell[a]; read cell[a] -> r1; read cell[c] -> r2; return
// [r1, r2]. Params: [a0, a1, d, c0, c1] (LSB-first 2-bit addresses).
fn guest() -> BIrBlocks<()> {
    let storage = StorageId(0);
    let lane = LaneId(0);
    let (a0, a1, d, c0, c1) = (
        IRVarId(0),
        IRVarId(1),
        IRVarId(2),
        IRVarId(3),
        IRVarId(4),
    );
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
fn s3_symbolic_storage_matches_model() {
    let program = storage_to_oram(
        &guest(),
        &OramLowerConfig {
            storage: StorageId(0),
            levels: LEVELS,
            bucket_size: Z,
            max_stash: MAX_STASH,
        },
    )
    .expect("lowers");

    // One write + two reads = three accesses, hence three access stages and
    // four compute segments (some possibly empty).
    let accesses = program
        .stages
        .iter()
        .filter(|s| matches!(s, Stage::Access(_)))
        .count();
    assert_eq!(accesses, 3);
    // The ORAM serves the 4-cell address space, one bit per cell.
    assert_eq!(program.oram.num_addrs, 4);
    assert_eq!(program.oram.data_bits, 1);

    // Drive every (a, d, c) combination and check against a model.
    for a in 0..4u64 {
        for d in [false, true] {
            for c in 0..4u64 {
                let inputs = vec![
                    a & 1 == 1,
                    a & 2 == 2,
                    d,
                    c & 1 == 1,
                    c & 2 == 2,
                ];
                let (out, _tree) = run_concrete::<Z>(&program, &inputs);
                // Model: write d to cell a; r1 = cell a = d; r2 = cell c.
                let r1 = d;
                let r2 = if c == a { d } else { false };
                assert_eq!(
                    out,
                    vec![r1, r2],
                    "a={a} d={d} c={c}: got {out:?}, want [{r1}, {r2}]"
                );
            }
        }
    }
}

// Guest: write d to cell 0, write e to cell 1 (concrete addresses), then read
// cell[a] (symbolic) -> r; return [r]. Params: [d, e, a0, a1].
fn guest_two_cells() -> BIrBlocks<()> {
    let storage = StorageId(0);
    let lane = LaneId(0);
    let (d, e, a0, a1) = (IRVarId(0), IRVarId(1), IRVarId(2), IRVarId(3));
    let mut block: BIrBlock<()> = BIrBlock {
        params: 4,
        stmts: vec![],
        terminator: BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Return,
            args: vec![],
        }),
    };
    let mut next = 4u32;
    let mut push = |s: BIrStmt, b: &mut BIrBlock<()>| {
        b.stmts.push(Node::new(s, (), None));
        let id = IRVarId(next);
        next += 1;
        id
    };
    let z = push(BIrStmt::Zero, &mut block);
    let o = push(BIrStmt::One, &mut block);
    push(
        BIrStmt::StorageWrite {
            storage,
            lane,
            src: d,
            addr: vec![z, z], // cell 0
        },
        &mut block,
    );
    push(
        BIrStmt::StorageWrite {
            storage,
            lane,
            src: e,
            addr: vec![o, z], // cell 1 (LSB-first)
        },
        &mut block,
    );
    let r = push(
        BIrStmt::StorageRead {
            storage,
            lane,
            addr: vec![a0, a1],
        },
        &mut block,
    );
    block.terminator = BIrTerminator::Jmp(BIrTarget {
        block: IRBlockTargetId::Return,
        args: vec![r],
    });
    BIrBlocks {
        blocks: vec![block],
        pre_init: vec![],
    }
}

#[test]
fn s3_write_cells_then_symbolic_read() {
    let program = storage_to_oram(
        &guest_two_cells(),
        &OramLowerConfig {
            storage: StorageId(0),
            levels: LEVELS,
            bucket_size: Z,
            max_stash: MAX_STASH,
        },
    )
    .expect("lowers");
    for d in [false, true] {
        for e in [false, true] {
            for a in 0..4u64 {
                let inputs = vec![d, e, a & 1 == 1, a & 2 == 2];
                let (out, _tree) = run_concrete::<Z>(&program, &inputs);
                let want = match a {
                    0 => d,
                    1 => e,
                    _ => false,
                };
                assert_eq!(out, vec![want], "d={d} e={e} a={a}: got {out:?}, want [{want}]");
            }
        }
    }
}
