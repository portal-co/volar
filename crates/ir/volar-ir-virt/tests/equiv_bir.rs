// @reliability: experimental
// @ai: assisted
//! End-to-end equivalence tests for `virtualize_bir`.

use volar_fuzz::interpreter::biir::eval_biir;
use volar_ir::{
    boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator},
    ir::{IRBlockId, IRBlockTargetId, IRVarId},
};
use volar_ir_virt::{virtualize_bir, BytecodeForm, DispatchMode, VirtualizeConfig};

fn cfg_default() -> VirtualizeConfig {
    VirtualizeConfig {
        dispatch: DispatchMode::Public,
        bytecode_form: BytecodeForm::InIr,
        ..VirtualizeConfig::default()
    }
}

// ============================================================================
// Test: three-block pass-through (dedup of two identical jmp blocks)
// ============================================================================

fn three_block_passthrough() -> BIrBlocks {
    BIrBlocks(vec![
        BIrBlock {
            params: 1,
            stmts: vec![],
            stmt_provs: vec![],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Block(IRBlockId(1)),
                args: vec![IRVarId(0)],
            }),
        },
        BIrBlock {
            params: 1,
            stmts: vec![],
            stmt_provs: vec![],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Block(IRBlockId(2)),
                args: vec![IRVarId(0)],
            }),
        },
        BIrBlock {
            params: 1,
            stmts: vec![],
            stmt_provs: vec![],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: vec![IRVarId(0)],
            }),
        },
    ])
}

#[test]
fn bir_passthrough_dedup_two_handlers() {
    let blocks = three_block_passthrough();
    let out = virtualize_bir(&blocks, &cfg_default());
    assert_eq!(out.blocks_in, 3);
    assert_eq!(out.n_handlers, 2);
}

#[test]
fn bir_passthrough_semantics() {
    let blocks = three_block_passthrough();
    let ref_out = eval_biir(&blocks, &[true]).expect("ref eval terminated");
    assert_eq!(ref_out, vec![true]);

    let virt = virtualize_bir(&blocks, &cfg_default());
    let virt_out = eval_biir(&virt.blocks, &[true]).expect("virt eval terminated");
    assert_eq!(ref_out, virt_out);

    // Also check input=false.
    let ref_f = eval_biir(&blocks, &[false]).expect("ref false");
    let virt_f = eval_biir(&virt.blocks, &[false]).expect("virt false");
    assert_eq!(ref_f, virt_f);
}

#[test]
fn bir_passthrough_oblivious_is_movfuscated_shape() {
    let blocks = three_block_passthrough();
    let cfg = VirtualizeConfig {
        dispatch: DispatchMode::Oblivious,
        bytecode_form: BytecodeForm::InIr,
        ..VirtualizeConfig::default()
    };
    let virt = virtualize_bir(&blocks, &cfg);

    // Oblivious dispatch collapses the virt output through
    // `movfuscate_biir`, producing a single self-looping block.
    assert_eq!(
        virt.blocks.0.len(),
        1,
        "oblivious dispatch must produce the movfuscated single-block shape"
    );
}

// ============================================================================
// Test: Xor + And gates
// ============================================================================

fn xor_and_chain() -> BIrBlocks {
    // block 0: params=[a, b]
    //   x = Xor(a, b)
    //   jmp block 1 with [x]
    // block 1: params=[x]
    //   y = Not(x)
    //   return [y]
    BIrBlocks(vec![
        BIrBlock {
            params: 2,
            stmts: vec![BIrStmt::Xor(IRVarId(0), IRVarId(1))],
            stmt_provs: vec![()],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Block(IRBlockId(1)),
                args: vec![IRVarId(2)],
            }),
        },
        BIrBlock {
            params: 1,
            stmts: vec![BIrStmt::Not(IRVarId(0))],
            stmt_provs: vec![()],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: vec![IRVarId(1)],
            }),
        },
    ])
}

#[test]
fn bir_xor_and_chain_semantics() {
    // All blocks must share param count for virtualize_bir; this one
    // has blocks with params=2 and params=1, so virtualize_bir should
    // panic.  We only assert the reference interpreter here.
    let blocks = xor_and_chain();
    // Input: a=1, b=0 -> x=1 -> NOT(1) = 0.
    let out = eval_biir(&blocks, &[true, false]).expect("eval");
    assert_eq!(out, vec![false]);
}

// ============================================================================
// Test: single block (trivial, no dispatch needed)
// ============================================================================

#[test]
fn bir_single_block_is_trivial() {
    let blocks = BIrBlocks(vec![BIrBlock {
        params: 1,
        stmts: vec![BIrStmt::Not(IRVarId(0))],
        stmt_provs: vec![()],
        terminator: BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Return,
            args: vec![IRVarId(1)],
        }),
    }]);
    let ref_out = eval_biir(&blocks, &[true]).expect("ref eval");
    assert_eq!(ref_out, vec![false]);

    let virt = virtualize_bir(&blocks, &cfg_default());
    assert_eq!(virt.n_handlers, 1);
    // For a single handler we go through setup -> dispatcher -> handler
    // -> return.  Semantics should still match.
    let virt_out = eval_biir(&virt.blocks, &[true]).expect("virt eval");
    assert_eq!(ref_out, virt_out);
}

// ============================================================================
// Test: CondJmp with both branches Block(_)
// ============================================================================

fn bir_condjmp_two_branch() -> BIrBlocks {
    // All blocks have params=1 (just the bit that is threaded).
    // block 0: params=[a]  -- the condition, threaded as state too
    //   cond = Not(Not(a))   (identity, to keep a lineage var)
    //   CondJmp(a, then_target=block 1, args=[a], else_target=block 2, args=[a])
    // block 1: params=[a]  -- return 1
    //   one = One
    //   return [one]
    // block 2: params=[a]  -- return 0
    //   zero = Zero
    //   return [zero]
    BIrBlocks(vec![
        BIrBlock {
            params: 1,
            stmts: vec![],
            stmt_provs: vec![],
            terminator: BIrTerminator::CondJmp {
                val: IRVarId(0),
                then_target: BIrTarget {
                    block: IRBlockTargetId::Block(IRBlockId(1)),
                    args: vec![IRVarId(0)],
                },
                else_target: BIrTarget {
                    block: IRBlockTargetId::Block(IRBlockId(2)),
                    args: vec![IRVarId(0)],
                },
            },
        },
        BIrBlock {
            params: 1,
            stmts: vec![BIrStmt::One],
            stmt_provs: vec![()],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: vec![IRVarId(1)],
            }),
        },
        BIrBlock {
            params: 1,
            stmts: vec![BIrStmt::Zero],
            stmt_provs: vec![()],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: vec![IRVarId(1)],
            }),
        },
    ])
}

#[test]
fn bir_condjmp_dedup_and_semantics() {
    let blocks = bir_condjmp_two_branch();
    let virt = virtualize_bir(&blocks, &cfg_default());
    assert_eq!(virt.blocks_in, 3);
    // Blocks 1 and 2 differ only by Zero vs One; those are structural
    // in BIR so they are distinct handlers in v1.  Block 0 is also
    // different (CondJmp).  So n_handlers == 3.
    assert_eq!(virt.n_handlers, 3);

    let ref_t = eval_biir(&blocks, &[true]).expect("ref t");
    let ref_f = eval_biir(&blocks, &[false]).expect("ref f");
    assert_eq!(ref_t, vec![true]);
    assert_eq!(ref_f, vec![false]);

    let virt_t = eval_biir(&virt.blocks, &[true]).expect("virt t");
    let virt_f = eval_biir(&virt.blocks, &[false]).expect("virt f");
    assert_eq!(ref_t, virt_t);
    assert_eq!(ref_f, virt_f);
}
