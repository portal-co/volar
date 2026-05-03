// @reliability: experimental
// @ai: assisted
//! End-to-end equivalence tests for `virtualize_ir`.
//!
//! Each test constructs an `IRBlocks` module, evaluates it with the
//! reference interpreter from `volar-fuzz`, then evaluates the output of
//! `virtualize_ir` under the same inputs and asserts the results match.
//!
//! The virtualized IR uses `IRTerminator::JumpTable` for public
//! dispatch; the reference interpreter now supports `JumpTable` so the
//! full loop can be executed directly.

use std::collections::BTreeMap;

use volar_fuzz::interpreter::ir::{bit_width, const_to_bits, eval_ir};
use volar_ir::ir::{
    IRBlock, IRBlockId, IRBlockTargetId, IRBlocks, IRTerminator, IRType, IRTypeId, IRTypes,
    IRVarId, PrimType,
};
use volar_ir_common::{Constant, Stmt};
use volar_ir_virt::{virtualize_ir, BytecodeForm, DispatchMode, VirtualizeConfig};

// ============================================================================
// Helpers
// ============================================================================

fn cfg_default() -> VirtualizeConfig {
    VirtualizeConfig {
        dispatch: DispatchMode::Public,
        bytecode_form: BytecodeForm::InIr,
        ..VirtualizeConfig::default()
    }
}

/// Convert a `Vec<bool>` value (LSB-first) into a u32 by taking the low
/// 32 bits.  Panics if `bits` is empty.
fn bits_to_u32(bits: &[bool]) -> u32 {
    let mut v: u32 = 0;
    for (i, b) in bits.iter().enumerate() {
        if i >= 32 {
            break;
        }
        if *b {
            v |= 1u32 << i;
        }
    }
    v
}

// ============================================================================
// Test: three-block pass-through (tests dedup of two identical jmp blocks)
// ============================================================================

fn three_block_passthrough() -> (IRBlocks, IRTypes) {
    let mut types = IRTypes(vec![IRType::Primitive(PrimType::Bit)]);
    let u32_ty = types.intern(IRType::Primitive(PrimType::_32));

    let blocks = IRBlocks::new(vec![
        // block 0: jmp block 1 with x
        IRBlock {
            params: vec![u32_ty],
            stmts: vec![],
            stmt_provs: vec![],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Block(IRBlockId(1)),
                args: vec![IRVarId(0)],
            },
        },
        // block 1: jmp block 2 with x (identical canonical key to block 0)
        IRBlock {
            params: vec![u32_ty],
            stmts: vec![],
            stmt_provs: vec![],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Block(IRBlockId(2)),
                args: vec![IRVarId(0)],
            },
        },
        // block 2: return x
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
fn passthrough_dedup_gives_two_handlers() {
    let (blocks, mut types) = three_block_passthrough();
    let out = virtualize_ir(&blocks, &mut types, &cfg_default());
    assert_eq!(out.blocks_in, 3);
    // blocks 0 and 1 share a key (Jmp with Block target); block 2 has
    // Return.  So exactly two unique handlers.
    assert_eq!(out.n_handlers, 2);
}

#[test]
fn passthrough_semantics_preserved() {
    let (blocks, mut types) = three_block_passthrough();

    // Reference: input 7 should be returned.
    let u32_ty = types.intern(IRType::Primitive(PrimType::_32));
    let input = const_to_bits(&Constant { hi: 0, lo: 7 }, bit_width(u32_ty, &types));

    let ref_out = eval_ir(&blocks, &types, &[input.clone()]).expect("ref eval terminated");
    assert_eq!(ref_out.len(), 1);
    assert_eq!(bits_to_u32(&ref_out[0]), 7);

    let virt = virtualize_ir(&blocks, &mut types, &cfg_default());
    let virt_out = eval_ir(&virt.blocks, &types, &[input]).expect("virt eval terminated");
    assert_eq!(ref_out, virt_out);
}

// ============================================================================
// Test: two blocks with lifted Const immediates
// ============================================================================

/// Layout:
///   block 0: params=[x: _32]
///     stmts: c = Const(5, _32)
///     term:  Jmp(Block(2), [c])
///   block 1: params=[x: _32]
///     stmts: c = Const(10, _32)
///     term:  Jmp(Block(2), [c])
///   block 2: params=[x: _32]
///     term:  Jmp(Return, [x])
fn two_const_blocks_reach_sink() -> (IRBlocks, IRTypes) {
    let mut types = IRTypes(vec![IRType::Primitive(PrimType::Bit)]);
    let u32_ty = types.intern(IRType::Primitive(PrimType::_32));

    let mk_const_block = |val: u128, target: IRBlockId| IRBlock {
        params: vec![u32_ty],
        stmts: vec![Stmt::Const(Constant { hi: 0, lo: val }, u32_ty)],
        stmt_provs: vec![()],
        terminator: IRTerminator::Jmp {
            func: IRBlockTargetId::Block(target),
            args: vec![IRVarId(1)], // the Const stmt var
        },
    };

    let blocks = IRBlocks::new(vec![
        // Block 0 is the entry and never reached at runtime in this
        // tiny test (we dispatch immediately to Block 1, which is the
        // "runs first" block).  But block 0 is the entry.  For the
        // semantics test we will actually only have two blocks.  Let
        // me restructure: entry runs the "const=5" block, which jumps
        // to the sink.
        mk_const_block(5, IRBlockId(2)),
        mk_const_block(10, IRBlockId(2)),
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
fn lifted_const_blocks_dedup_to_two_handlers() {
    let (blocks, mut types) = two_const_blocks_reach_sink();
    let out = virtualize_ir(&blocks, &mut types, &cfg_default());
    assert_eq!(out.blocks_in, 3);
    // blocks 0 and 1 have canonical key "Const(ZERO, _32); Jmp(Block(0), [c])"
    // — same key modulo the Const value.  Block 2 is a plain Return.
    // So exactly two unique handlers.
    assert_eq!(out.n_handlers, 2);
}

#[test]
fn lifted_const_blocks_semantics_entry_is_block_zero() {
    // Semantics check: the entry is block 0, which emits Const=5 and
    // jumps to block 2 returning its arg.  Result = 5.
    let (blocks, mut types) = two_const_blocks_reach_sink();
    let u32_ty = types.intern(IRType::Primitive(PrimType::_32));
    let input = const_to_bits(&Constant { hi: 0, lo: 99 }, bit_width(u32_ty, &types));

    let ref_out = eval_ir(&blocks, &types, &[input.clone()]).expect("ref eval terminated");
    assert_eq!(bits_to_u32(&ref_out[0]), 5);

    let virt = virtualize_ir(&blocks, &mut types, &cfg_default());
    let virt_out = eval_ir(&virt.blocks, &types, &[input]).expect("virt eval terminated");
    assert_eq!(ref_out, virt_out);
}

// ============================================================================
// Test: Poly stmt preserved through virtualisation
// ============================================================================

/// Compute `x XOR 1` using Poly, then return.
///   block 0: params=[x: Bit], stmts=[r = Poly(x + 1)], term=Jmp(Block(1), [r])
///   block 1: params=[x: Bit], term=Jmp(Return, [x])
fn poly_xor_one() -> (IRBlocks, IRTypes) {
    let types = IRTypes(vec![IRType::Primitive(PrimType::Bit)]);
    let bit_ty = IRTypeId(0);

    let mut coeffs: BTreeMap<Vec<IRVarId>, u8> = BTreeMap::new();
    coeffs.insert(vec![IRVarId(0)], 1u8);

    let blocks = IRBlocks::new(vec![
        IRBlock {
            params: vec![bit_ty],
            stmts: vec![Stmt::Poly {
                ty: bit_ty,
                coeffs,
                constant: Constant { hi: 0, lo: 1 }, // XOR 1
            }],
            stmt_provs: vec![()],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Block(IRBlockId(1)),
                args: vec![IRVarId(1)],
            },
        },
        IRBlock {
            params: vec![bit_ty],
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
fn poly_semantics_preserved() {
    let (blocks, mut types) = poly_xor_one();

    let in_true = vec![true];
    let in_false = vec![false];

    let ref_t = eval_ir(&blocks, &types, &[in_true.clone()]).expect("ref eval t");
    let ref_f = eval_ir(&blocks, &types, &[in_false.clone()]).expect("ref eval f");
    assert_eq!(ref_t, vec![vec![false]]);
    assert_eq!(ref_f, vec![vec![true]]);

    let virt = virtualize_ir(&blocks, &mut types, &cfg_default());
    let virt_t = eval_ir(&virt.blocks, &types, &[in_true]).expect("virt eval t");
    let virt_f = eval_ir(&virt.blocks, &types, &[in_false]).expect("virt eval f");
    assert_eq!(ref_t, virt_t);
    assert_eq!(ref_f, virt_f);
}

// ============================================================================
// Test: JumpCond with both branches targeting Block(_)
// ============================================================================

/// Branch on a Bit param.  Layout:
///   block 0: params=[c: Bit, x: _32],
///            stmts=[], term=JumpCond(c, Block(1), [x], Block(2), [x])
///   block 1: params=[c: Bit, x: _32],
///            stmts=[k=Const(1, _32)], term=Jmp(Block(3), [c, k])
///   block 2: params=[c: Bit, x: _32],
///            stmts=[k=Const(2, _32)], term=Jmp(Block(3), [c, k])
///   block 3: params=[c: Bit, x: _32],
///            stmts=[], term=Jmp(Return, [x])
fn jumpcond_two_branch() -> (IRBlocks, IRTypes) {
    let mut types = IRTypes(vec![IRType::Primitive(PrimType::Bit)]);
    let bit_ty = IRTypeId(0);
    let u32_ty = types.intern(IRType::Primitive(PrimType::_32));

    let blocks = IRBlocks::new(vec![
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
        IRBlock {
            params: vec![bit_ty, u32_ty],
            stmts: vec![Stmt::Const(Constant { hi: 0, lo: 1 }, u32_ty)],
            stmt_provs: vec![()],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Block(IRBlockId(3)),
                args: vec![IRVarId(0), IRVarId(2)],
            },
        },
        IRBlock {
            params: vec![bit_ty, u32_ty],
            stmts: vec![Stmt::Const(Constant { hi: 0, lo: 2 }, u32_ty)],
            stmt_provs: vec![()],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Block(IRBlockId(3)),
                args: vec![IRVarId(0), IRVarId(2)],
            },
        },
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
fn jumpcond_dedup_and_semantics() {
    let (blocks, mut types) = jumpcond_two_branch();
    let u32_ty = types.intern(IRType::Primitive(PrimType::_32));

    let c_true = vec![true];
    let c_false = vec![false];
    let x = const_to_bits(&Constant { hi: 0, lo: 42 }, bit_width(u32_ty, &types));

    let ref_t = eval_ir(&blocks, &types, &[c_true.clone(), x.clone()]).expect("ref eval t");
    let ref_f = eval_ir(&blocks, &types, &[c_false.clone(), x.clone()]).expect("ref eval f");
    assert_eq!(bits_to_u32(&ref_t[0]), 1);
    assert_eq!(bits_to_u32(&ref_f[0]), 2);

    let virt = virtualize_ir(&blocks, &mut types, &cfg_default());
    // blocks 1 and 2 dedup (same shape modulo Const value); blocks 0
    // and 3 are unique.  So n_handlers == 3.
    assert_eq!(virt.n_handlers, 3);

    let virt_t = eval_ir(&virt.blocks, &types, &[c_true, x.clone()]).expect("virt eval t");
    let virt_f = eval_ir(&virt.blocks, &types, &[c_false, x]).expect("virt eval f");
    assert_eq!(ref_t, virt_t);
    assert_eq!(ref_f, virt_f);
}

// ============================================================================
// Test: StorageRead / StorageWrite preserved
// ============================================================================

fn storage_read_write() -> (IRBlocks, IRTypes) {
    let mut types = IRTypes(vec![IRType::Primitive(PrimType::Bit)]);
    let u32_ty = types.intern(IRType::Primitive(PrimType::_32));

    // Use StorageId(16) (MEMORY_BASE) to avoid colliding with the virt
    // bytecode storage range.
    let user_storage = volar_ir_common::StorageId(16);

    let blocks = IRBlocks::new(vec![
        // block 0: params=[x: _32], writes x to storage, jumps block 1.
        IRBlock {
            params: vec![u32_ty],
            stmts: vec![
                Stmt::Const(Constant { hi: 0, lo: 0 }, u32_ty),
                Stmt::StorageWrite {
                    storage: user_storage,
                    src: IRVarId(0),
                    ty: u32_ty,
                    addr: IRVarId(1),
                },
            ],
            stmt_provs: vec![(), ()],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Block(IRBlockId(1)),
                args: vec![IRVarId(0)],
            },
        },
        // block 1: reads from same storage, returns that.
        IRBlock {
            params: vec![u32_ty],
            stmts: vec![
                Stmt::Const(Constant { hi: 0, lo: 0 }, u32_ty),
                Stmt::StorageRead {
                    storage: user_storage,
                    ty: u32_ty,
                    addr: IRVarId(1),
                },
            ],
            stmt_provs: vec![(), ()],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: vec![IRVarId(2)],
            },
        },
    ]);
    (blocks, types)
}

#[test]
fn storage_semantics_preserved() {
    let (blocks, mut types) = storage_read_write();
    let u32_ty = types.intern(IRType::Primitive(PrimType::_32));
    let input = const_to_bits(&Constant { hi: 0, lo: 0x1234 }, bit_width(u32_ty, &types));

    let ref_out = eval_ir(&blocks, &types, &[input.clone()]).expect("ref eval");
    assert_eq!(bits_to_u32(&ref_out[0]), 0x1234);

    let virt = virtualize_ir(&blocks, &mut types, &cfg_default());
    let virt_out = eval_ir(&virt.blocks, &types, &[input]).expect("virt eval");
    assert_eq!(ref_out, virt_out);
}

