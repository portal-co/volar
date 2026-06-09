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
extern crate alloc;

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
    CommitmentConfig {
        algorithm: XorFoldHash32::default(),
        commitment_storage: StorageId(64),
        key: alloc::vec![],
        key_storage: None,
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
            stmts: vec![Stmt::Const(Constant { hi: 0, lo: 0 }, u32_ty)],
            stmt_provs: vec![()],
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

// ============================================================================
// SipHash48 tests
// ============================================================================

/// Build a SipHash48 CommitmentConfig with a fixed compile-time key.
fn siphash_commitment_cfg() -> CommitmentConfig<volar_ir_virt::SipHash48> {
    CommitmentConfig {
        algorithm: volar_ir_virt::SipHash48,
        commitment_storage: StorageId(64),
        // k0 = 0x_DEAD_BEEF_CAFE_BABE, k1 = 0x_0123_4567_89AB_CDEF
        key: alloc::vec![
            Constant { hi: 0, lo: 0xDEAD_BEEF_CAFE_BABE_u64 as u128 },
            Constant { hi: 0, lo: 0x0123_4567_89AB_CDEF_u64 as u128 },
        ],
        key_storage: Some(StorageId(128)),
    }
}

/// Run `eval_ir` prepending the two SipHash key constants as the first
/// arguments (they are the extra entry-block params added by the keyed pass).
fn eval_with_sip_key<I>(
    blocks: &volar_ir::ir::IRBlocks,
    types: &volar_ir::ir::IRTypes,
    original_inputs: I,
) -> Vec<Vec<bool>>
where
    I: IntoIterator<Item = Vec<bool>>,
{
    use volar_fuzz::interpreter::ir::eval_ir;
    // The u64 type is always index 0 of a freshly-built IRTypes table,
    // but we can find it by scanning — or just ask for bit_width of _64.
    let u64_ty_id = types.0.iter().position(|t| {
        matches!(t, volar_ir::ir::IRType::Primitive(volar_ir::ir::PrimType::_64))
    }).expect("_64 type not in table") as u32;
    let u64_ty = volar_ir::ir::IRTypeId(u64_ty_id);
    let k0_bits = const_to_bits(
        &Constant { hi: 0, lo: 0xDEAD_BEEF_CAFE_BABE_u64 as u128 },
        bit_width(u64_ty, types),
    );
    let k1_bits = const_to_bits(
        &Constant { hi: 0, lo: 0x0123_4567_89AB_CDEF_u64 as u128 },
        bit_width(u64_ty, types),
    );
    let mut args = alloc::vec![k0_bits, k1_bits];
    args.extend(original_inputs);
    eval_ir(blocks, types, &args).expect("eval_ir with sip key failed")
}

#[test]
fn siphash48_committed_single_block_same_output() {
    let (blocks, mut types) = single_const_return();

    let plain = virtualize_ir(&blocks, &mut types, &cfg_in_ir());
    let plain_out = eval_ir(&plain.blocks, &types, &[]).expect("plain eval");

    let committed =
        virtualize_ir_committed(&blocks, &mut types, &cfg_in_ir(), &siphash_commitment_cfg());
    // The pass adds two key params at the front; supply them.
    assert_eq!(committed.key_params.len(), 2, "SipHash48 must add 2 key params");
    let commit_out = eval_with_sip_key(&committed.blocks, &types, []);

    assert_eq!(plain_out, commit_out, "SipHash48 single-block output must match plain");
    assert_eq!(bits_to_u32(&commit_out[0]), 42);
}

#[test]
fn siphash48_key_params_reported_correctly() {
    let (blocks, mut types) = single_const_return();
    let committed =
        virtualize_ir_committed(&blocks, &mut types, &cfg_in_ir(), &siphash_commitment_cfg());

    // key_params must carry the two compile-time key constants.
    assert_eq!(committed.key_params.len(), 2);
    assert_eq!(committed.key_params[0].0.lo, 0xDEAD_BEEF_CAFE_BABE_u64 as u128);
    assert_eq!(committed.key_params[1].0.lo, 0x0123_4567_89AB_CDEF_u64 as u128);
}

#[test]
fn siphash48_committed_passthrough_same_output() {
    let (blocks, mut types) = three_block_passthrough();
    let u32_ty = types.intern(IRType::Primitive(volar_ir::ir::PrimType::_32));
    let input = const_to_bits(&Constant { hi: 0, lo: 7 }, bit_width(u32_ty, &types));

    let plain = virtualize_ir(&blocks, &mut types, &cfg_in_ir());
    let plain_out =
        eval_ir(&plain.blocks, &types, &[input.clone()]).expect("plain");

    let committed =
        virtualize_ir_committed(&blocks, &mut types, &cfg_in_ir(), &siphash_commitment_cfg());
    let commit_out = eval_with_sip_key(&committed.blocks, &types, [input]);

    assert_eq!(plain_out, commit_out, "SipHash48 passthrough must match plain");
    assert_eq!(bits_to_u32(&plain_out[0]), 7);
}

#[test]
fn siphash48_committed_jumpcond_same_output() {
    let (blocks, mut types) = jumpcond_two_branch();
    let bit_ty = IRTypeId(0);
    let u32_ty = types.intern(IRType::Primitive(volar_ir::ir::PrimType::_32));

    let c_t = const_to_bits(&Constant { hi: 0, lo: 1 }, bit_width(bit_ty, &types));
    let c_f = const_to_bits(&Constant { hi: 0, lo: 0 }, bit_width(bit_ty, &types));
    let x = const_to_bits(&Constant { hi: 0, lo: 99 }, bit_width(u32_ty, &types));

    let plain = virtualize_ir(&blocks, &mut types, &cfg_in_ir());
    let plain_t = eval_ir(&plain.blocks, &types, &[c_t.clone(), x.clone()]).expect("plain t");
    let plain_f = eval_ir(&plain.blocks, &types, &[c_f.clone(), x.clone()]).expect("plain f");

    let committed =
        virtualize_ir_committed(&blocks, &mut types, &cfg_in_ir(), &siphash_commitment_cfg());
    let commit_t = eval_with_sip_key(&committed.blocks, &types, [c_t, x.clone()]);
    let commit_f = eval_with_sip_key(&committed.blocks, &types, [c_f, x]);

    assert_eq!(plain_t, commit_t, "SipHash48 JumpCond true must match");
    assert_eq!(plain_f, commit_f, "SipHash48 JumpCond false must match");
}
