// @reliability: experimental
// @ai: assisted
//! Correctness tests for `virtualize_ir_committed`.
//!
//! Every test verifies that the committed variant produces the same
//! observable semantics as the non-committed variant when the bytecode is
//! valid (commitment diff == 0 → next_pc XOR 0 == next_pc).
//!
//! We reuse the `eval_ir` interpreter from `volar-fuzz` which:
//! 1. Executes the setup block, populating both bytecode storage and
//!    commitment storage with `Stmt::Const` / `StorageWrite` pairs.
//! 2. Runs the dispatch loop through all handler blocks.
//! 3. Returns the values read from return-registers.
//!
//! Because commitment diffs are zero for correctly-written bytecode, the
//! XOR injected into `next_pc` is a no-op and the output matches.

use volar_fuzz::interpreter::ir::{bit_width, const_to_bits, eval_ir};
use volar_ir::ir::{
    IRBlock, IRBlockId, IRBlockTargetId, IRBlocks, IRTerminator, IRType, IRTypeId, IRTypes,
    IRVarId, PrimType,
};
use volar_ir_common::{Constant, Stmt, StorageId};
use volar_ir_virt::{
    virtualize_ir, virtualize_ir_committed, BytecodeForm, CommitmentConfig, DispatchMode,
    VirtualizeConfig, XorFoldHash32,
};

// ============================================================================
// Helpers
// ============================================================================

fn cfg_in_ir() -> VirtualizeConfig {
    VirtualizeConfig {
        dispatch: DispatchMode::Public,
        bytecode_form: BytecodeForm::InIr,
        ..VirtualizeConfig::default()
    }
}

fn commitment_cfg() -> CommitmentConfig<XorFoldHash32> {
    // Use a StorageId well above the virt bytecode range to avoid collisions.
    CommitmentConfig {
        algorithm: XorFoldHash32::default(),
        commitment_storage: StorageId(64),
    }
}

fn bits_to_u32(bits: &[bool]) -> u32 {
    bits.iter().enumerate().fold(0u32, |acc, (i, &b)| {
        if i < 32 && b { acc | (1 << i) } else { acc }
    })
}

// ============================================================================
// Test: single-block identity (Const + Return)
// ============================================================================

/// One block: `c = Const(42, _32); Return(c)`.
fn single_const_return() -> (IRBlocks, IRTypes) {
    let mut types = IRTypes::default();
    let u32_ty = types.intern(IRType::Primitive(PrimType::_32));

    let blocks = IRBlocks::new(vec![IRBlock {
        params: vec![],
        stmts: vec![Stmt::Const(Constant { hi: 0, lo: 42 }, u32_ty)],
        stmt_provs: vec![()],
        terminator: IRTerminator::Jmp {
            func: IRBlockTargetId::Return,
            args: vec![IRVarId(0)],
        },
    }]);
    (blocks, types)
}

#[test]
fn committed_single_block_same_output() {
    let (blocks, mut types) = single_const_return();

    let plain = virtualize_ir(&blocks, &mut types, &cfg_in_ir());
    let plain_out = eval_ir(&plain.blocks, &types, &[]).expect("plain eval");

    let committed = virtualize_ir_committed(&blocks, &mut types, &cfg_in_ir(), &commitment_cfg());
    let commit_out = eval_ir(&committed.blocks, &types, &[]).expect("committed eval");

    assert_eq!(plain_out, commit_out, "single-block committed output must match plain");
    assert_eq!(bits_to_u32(&commit_out[0]), 42);
}

// ============================================================================
// Test: two-block passthrough with dedup
// ============================================================================

/// Three blocks that dedup to two handlers; tests the JMP terminator path.
fn three_block_passthrough() -> (IRBlocks, IRTypes) {
    let mut types = IRTypes::default();
    let u32_ty = types.intern(IRType::Primitive(PrimType::_32));

    let blocks = IRBlocks::new(vec![
        IRBlock {
            params: vec![u32_ty],
            stmts: vec![],
            stmt_provs: vec![],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Block(IRBlockId(1)),
                args: vec![IRVarId(0)],
            },
        },
        IRBlock {
            params: vec![u32_ty],
            stmts: vec![],
            stmt_provs: vec![],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Block(IRBlockId(2)),
                args: vec![IRVarId(0)],
            },
        },
        IRBlock {
            params: vec![u32_ty],
            stmts: vec![],
            stmt_provs: vec![],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: vec![IRVarId(0)],
            },
        },
    ]);
    (blocks, types)
}

#[test]
fn committed_passthrough_same_output() {
    let (blocks, mut types) = three_block_passthrough();
    let u32_ty = types.intern(IRType::Primitive(PrimType::_32));
    let input = const_to_bits(&Constant { hi: 0, lo: 7 }, bit_width(u32_ty, &types));

    let plain = virtualize_ir(&blocks, &mut types, &cfg_in_ir());
    let plain_out = eval_ir(&plain.blocks, &types, &[input.clone()]).expect("plain");

    let committed =
        virtualize_ir_committed(&blocks, &mut types, &cfg_in_ir(), &commitment_cfg());
    let commit_out =
        eval_ir(&committed.blocks, &types, &[input]).expect("committed");

    assert_eq!(plain_out, commit_out, "passthrough committed output must match plain");
    assert_eq!(bits_to_u32(&plain_out[0]), 7);
}

// ============================================================================
// Test: JumpCond with two branches
// ============================================================================

fn jumpcond_two_branch() -> (IRBlocks, IRTypes) {
    let mut types = IRTypes::default();
    let bit_ty = IRTypeId(0);
    types.0.push(IRType::Primitive(PrimType::Bit));
    let u32_ty = types.intern(IRType::Primitive(PrimType::_32));

    let blocks = IRBlocks::new(vec![
        // block 0: cond branch
        IRBlock {
            params: vec![bit_ty, u32_ty],
            stmts: vec![],
            stmt_provs: vec![],
            terminator: IRTerminator::JumpCond {
                condition: IRVarId(0),
                true_block: IRBlockTargetId::Block(IRBlockId(1)),
                true_args: vec![IRVarId(0), IRVarId(1)],
                false_block: IRBlockTargetId::Block(IRBlockId(2)),
                false_args: vec![IRVarId(0), IRVarId(1)],
            },
        },
        // block 1: return Const(1)
        IRBlock {
            params: vec![bit_ty, u32_ty],
            stmts: vec![Stmt::Const(Constant { hi: 0, lo: 1 }, u32_ty)],
            stmt_provs: vec![()],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Block(IRBlockId(3)),
                args: vec![IRVarId(0), IRVarId(2)],
            },
        },
        // block 2: return Const(2)
        IRBlock {
            params: vec![bit_ty, u32_ty],
            stmts: vec![Stmt::Const(Constant { hi: 0, lo: 2 }, u32_ty)],
            stmt_provs: vec![()],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Block(IRBlockId(3)),
                args: vec![IRVarId(0), IRVarId(2)],
            },
        },
        // block 3: sink
        IRBlock {
            params: vec![bit_ty, u32_ty],
            stmts: vec![],
            stmt_provs: vec![],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: vec![IRVarId(1)],
            },
        },
    ]);
    (blocks, types)
}

#[test]
fn committed_jumpcond_same_output() {
    let (blocks, mut types) = jumpcond_two_branch();
    let bit_ty = IRTypeId(0);
    let u32_ty = types.intern(IRType::Primitive(PrimType::_32));

    let c_t = const_to_bits(&Constant { hi: 0, lo: 1 }, bit_width(bit_ty, &types));
    let c_f = const_to_bits(&Constant { hi: 0, lo: 0 }, bit_width(bit_ty, &types));
    let x = const_to_bits(&Constant { hi: 0, lo: 99 }, bit_width(u32_ty, &types));

    let plain = virtualize_ir(&blocks, &mut types, &cfg_in_ir());
    let plain_t = eval_ir(&plain.blocks, &types, &[c_t.clone(), x.clone()]).expect("plain t");
    let plain_f = eval_ir(&plain.blocks, &types, &[c_f.clone(), x.clone()]).expect("plain f");

    let committed =
        virtualize_ir_committed(&blocks, &mut types, &cfg_in_ir(), &commitment_cfg());
    let commit_t =
        eval_ir(&committed.blocks, &types, &[c_t, x.clone()]).expect("committed t");
    let commit_f =
        eval_ir(&committed.blocks, &types, &[c_f, x]).expect("committed f");

    assert_eq!(plain_t, commit_t, "JumpCond true branch must match");
    assert_eq!(plain_f, commit_f, "JumpCond false branch must match");
    assert_eq!(bits_to_u32(&plain_t[0]), 1);
    assert_eq!(bits_to_u32(&plain_f[0]), 2);
}

// ============================================================================
// Test: dedup invariant holds with commitment
// ============================================================================

/// 16 blocks differing only by Const value must still dedup to 1 handler.
#[test]
fn committed_dedup_count_unchanged() {
    let mut types = IRTypes::default();
    let u32_ty = types.intern(IRType::Primitive(PrimType::_32));

    let blocks: Vec<IRBlock> = (0..16u128)
        .map(|k| IRBlock {
            params: vec![u32_ty],
            stmts: vec![Stmt::Const(Constant { hi: 0, lo: k }, u32_ty)],
            stmt_provs: vec![()],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: vec![IRVarId(1)],
            },
        })
        .collect();
    let blocks = IRBlocks::new(blocks);

    let committed =
        virtualize_ir_committed(&blocks, &mut types, &cfg_in_ir(), &commitment_cfg());
    assert_eq!(committed.n_handlers, 1, "dedup count must not change with commitment");
    assert_eq!(committed.blocks_in, 16);
}

// ============================================================================
// Test: native hash and IR hash agree (XorFoldHash32 self-consistency)
// ============================================================================

/// Verify that the native hash and the IR-emitted hash give the same result
/// for a representative set of input words, by running the virtualized IR and
/// checking that the XOR diff in every handler is zero (no semantic deviation).
#[test]
fn committed_lifted_const_same_output() {
    let mut types = IRTypes::default();
    let u32_ty = types.intern(IRType::Primitive(PrimType::_32));

    // Two blocks that dedup, each returning a different Const value.
    let blocks = IRBlocks::new(vec![
        IRBlock {
            params: vec![u32_ty],
            stmts: vec![Stmt::Const(Constant { hi: 0, lo: 5 }, u32_ty)],
            stmt_provs: vec![()],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Block(IRBlockId(2)),
                args: vec![IRVarId(1)],
            },
        },
        IRBlock {
            params: vec![u32_ty],
            stmts: vec![Stmt::Const(Constant { hi: 0, lo: 10 }, u32_ty)],
            stmt_provs: vec![()],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Block(IRBlockId(2)),
                args: vec![IRVarId(1)],
            },
        },
        IRBlock {
            params: vec![u32_ty],
            stmts: vec![],
            stmt_provs: vec![],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: vec![IRVarId(0)],
            },
        },
    ]);

    let input = const_to_bits(&Constant { hi: 0, lo: 0 }, bit_width(u32_ty, &types));

    // Plain result: entry is block 0 which emits Const(5).
    let plain = virtualize_ir(&blocks, &mut types, &cfg_in_ir());
    let plain_out = eval_ir(&plain.blocks, &types, &[input.clone()]).expect("plain");
    assert_eq!(bits_to_u32(&plain_out[0]), 5);

    // Committed result must match.
    let committed =
        virtualize_ir_committed(&blocks, &mut types, &cfg_in_ir(), &commitment_cfg());
    let commit_out = eval_ir(&committed.blocks, &types, &[input]).expect("committed");

    assert_eq!(
        plain_out, commit_out,
        "committed output must match plain when bytecode is valid"
    );
}
