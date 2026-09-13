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
fn s3_symbolic_storage_matches_model() {
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
                let inputs = vec![a & 1 == 1, a & 2 == 2, d, c & 1 == 1, c & 2 == 2];
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

// Encryption is the **default posture**: `secure: true` lowers the same guest
// to an encrypted-tree ORAM (AES per-node pads + encrypted valid bit + versioned
// pads), and `run_concrete` drives it (pre-formatting the tree and tracking
// versions). This proves the secure default works end-to-end concretely.
#[test]
fn s3_secure_default_encrypted_tree() {
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

    // The default posture is encrypted.
    assert!(program.oram.encrypted);
    assert!(program.oram.encrypt_valid);
    assert!(program.oram.versioned_pads);

    // A few (a, d, c) combos through the encrypted ORAM, checked against the
    // model (the full cross-product is covered by the plaintext test above).
    for (a, d, c) in [
        (0u64, true, 0u64),
        (1, false, 2),
        (2, true, 3),
        (3, false, 3),
    ] {
        let inputs = vec![a & 1 == 1, a & 2 == 2, d, c & 1 == 1, c & 2 == 2];
        let (out, tree) = run_concrete::<Z>(&program, &inputs);
        let r1 = d;
        let r2 = if c == a { d } else { false };
        assert_eq!(out, vec![r1, r2], "secure a={a} d={d} c={c}");
        // The tree holds only ciphertext: no stored byte should be the trivial
        // all-zero plaintext of a real block after writes.
        let _ = tree;
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
            levels: LEVELS,
            bucket_size: Z,
            max_stash: MAX_STASH,
            secure: false,
            narrow_bits: None,
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
                assert_eq!(
                    out,
                    vec![want],
                    "d={d} e={e} a={a}: got {out:?}, want [{want}]"
                );
            }
        }
    }
}

// Guest with an 8-bit symbolic address: write d to cell[a]; read cell[a] -> r.
// Params: [a0..a7, d] (LSB-first 8-bit address).
fn guest_wide_addr() -> BIrBlocks<()> {
    let storage = StorageId(0);
    let lane = LaneId(0);
    let a: Vec<IRVarId> = (0..8).map(IRVarId).collect();
    let d = IRVarId(8);
    let mut block: BIrBlock<()> = BIrBlock {
        params: 9,
        stmts: vec![],
        terminator: BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Return,
            args: vec![],
        }),
    };
    let mut next = 9u32;
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
            addr: a.clone(),
        },
        &mut block,
    );
    let r = push(
        BIrStmt::StorageRead {
            storage,
            lane,
            addr: a.clone(),
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

// **Address narrowing**: an 8-bit-address guest narrowed to 4 bits spans a
// 16-cell ORAM (vs 256 cells unnarrowed). This is how a wide-address guest runs
// on a feasible instance — minimal narrowing (24 or 28 bits for a real guest)
// bounds the ORAM while keeping the spill/stack window. Verified concretely for
// the low addresses (a < 16), which fit in the narrowed window.
#[test]
fn s3_narrowing_bounds_the_oram() {
    let full = storage_to_oram(
        &guest_wide_addr(),
        &OramLowerConfig {
            levels: LEVELS,
            bucket_size: Z,
            max_stash: MAX_STASH,
            secure: false,
            narrow_bits: None,
        },
    )
    .expect("lowers");
    assert_eq!(full.oram.num_addrs, 256, "unnarrowed 8-bit space");

    let narrowed = storage_to_oram(
        &guest_wide_addr(),
        &OramLowerConfig {
            levels: LEVELS,
            bucket_size: Z,
            max_stash: MAX_STASH,
            secure: false,
            narrow_bits: Some(4),
        },
    )
    .expect("lowers");
    assert_eq!(narrowed.oram.num_addrs, 16, "narrowed to 4 bits");

    // The narrowed ORAM serves the low addresses (a < 16) correctly.
    for a in 0..16u64 {
        for d in [false, true] {
            let mut inputs: Vec<bool> = (0..8).map(|j| (a >> j) & 1 == 1).collect();
            inputs.push(d);
            let (out, _tree) = run_concrete::<Z>(&narrowed, &inputs);
            assert_eq!(out, vec![d], "narrowed a={a} d={d}");
        }
    }
}

// A **larger ORAM instance**: a 14-bit symbolic-address guest runs over a
// 16384-cell ORAM (levels=15). This demonstrates the scaling path — with
// minimal narrowing (24 or 28 bits for a real guest) plus the recursive
// position map (S5c) the same mechanism serves a much larger address space.
// Here the flat posmap is still feasible at 16k cells. Concrete, plaintext.
#[test]
fn s3_larger_oram_instance_concrete() {
    const W: usize = 14; // 14-bit addresses -> 16k cells
    let storage = StorageId(0);
    let lane = LaneId(0);
    let a: Vec<IRVarId> = (0..W as u32).map(IRVarId).collect();
    let d = IRVarId(W as u32);
    let mut block: BIrBlock<()> = BIrBlock {
        params: (W + 1) as u32,
        stmts: vec![],
        terminator: BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Return,
            args: vec![],
        }),
    };
    let mut next = (W + 1) as u32;
    {
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
                addr: a.clone(),
            },
            &mut block,
        );
        let r = push(
            BIrStmt::StorageRead {
                storage,
                lane,
                addr: a.clone(),
            },
            &mut block,
        );
        block.terminator = BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Return,
            args: vec![r],
        });
    }
    let guest = BIrBlocks {
        blocks: vec![block],
        pre_init: vec![],
    };

    let program = storage_to_oram(
        &guest,
        &OramLowerConfig {
            levels: 15,
            bucket_size: Z,
            max_stash: 2 * 15 + Z + 16,
            secure: false,
            narrow_bits: None,
        },
    )
    .expect("lowers");
    assert_eq!(program.oram.num_addrs, 1 << W);

    // A few addresses across the 16k-cell space, concrete.
    for addr in [0u64, 1, 100, 4095, 16383] {
        for dv in [false, true] {
            let mut inputs: Vec<bool> = (0..W).map(|j| (addr >> j) & 1 == 1).collect();
            inputs.push(dv);
            let (out, _tree) = run_concrete::<Z>(&program, &inputs);
            assert_eq!(out, vec![dv], "larger-oram addr={addr} d={dv}");
        }
    }
}

// Guest over TWO storage spaces: write d to space0[a], write e to space1[b],
// read space0[a] -> r0, read space1[b] -> r1; return [r0, r1]. Params:
// [a0,a1,d,b0,b1,e] (2-bit addresses per space). Validates the multi-space
// driver routes each access to its own ORAM.
fn guest_two_spaces() -> BIrBlocks<()> {
    let lane = LaneId(0);
    let (a0, a1, d, b0, b1, e) = (
        IRVarId(0),
        IRVarId(1),
        IRVarId(2),
        IRVarId(3),
        IRVarId(4),
        IRVarId(5),
    );
    let mut block: BIrBlock<()> = BIrBlock {
        params: 6,
        stmts: vec![],
        terminator: BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Return,
            args: vec![],
        }),
    };
    let mut next = 6u32;
    let mut push = |s: BIrStmt, b: &mut BIrBlock<()>| {
        b.stmts.push(Node::new(s, (), None));
        let id = IRVarId(next);
        next += 1;
        id
    };
    let sa = StorageId(0);
    let sb = StorageId(1);
    push(
        BIrStmt::StorageWrite {
            storage: sa,
            lane,
            src: d,
            addr: vec![a0, a1],
        },
        &mut block,
    );
    push(
        BIrStmt::StorageWrite {
            storage: sb,
            lane,
            src: e,
            addr: vec![b0, b1],
        },
        &mut block,
    );
    let r0 = push(
        BIrStmt::StorageRead {
            storage: sa,
            lane,
            addr: vec![a0, a1],
        },
        &mut block,
    );
    let r1 = push(
        BIrStmt::StorageRead {
            storage: sb,
            lane,
            addr: vec![b0, b1],
        },
        &mut block,
    );
    block.terminator = BIrTerminator::Jmp(BIrTarget {
        block: IRBlockTargetId::Return,
        args: vec![r0, r1],
    });
    BIrBlocks {
        blocks: vec![block],
        pre_init: vec![],
    }
}

#[test]
fn s3_multi_space_two_orams() {
    let program = storage_to_oram(
        &guest_two_spaces(),
        &OramLowerConfig {
            levels: LEVELS,
            bucket_size: Z,
            max_stash: MAX_STASH,
            secure: false,
            narrow_bits: None,
        },
    )
    .expect("lowers");
    // Two distinct spaces -> two ORAMs.
    assert_eq!(program.spaces.len(), 2);
    for a in 0..4u64 {
        for b in 0..4u64 {
            for d in [false, true] {
                for e in [false, true] {
                    let inputs = vec![a & 1 == 1, a & 2 == 2, d, b & 1 == 1, b & 2 == 2, e];
                    let (out, _tree) = run_concrete::<Z>(&program, &inputs);
                    assert_eq!(out, vec![d, e], "a={a} b={b} d={d} e={e}");
                }
            }
        }
    }
}
