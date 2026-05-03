// @reliability: experimental
// @ai: assisted
//! Structural invariant tests: 16 blocks differing only by `Stmt::Const`
//! value must dedup to exactly one handler.

use volar_ir::ir::{
    IRBlock, IRBlockTargetId, IRBlocks, IRTerminator, IRType, IRTypes, IRVarId, PrimType,
};
use volar_ir_common::{Constant, Stmt};
use volar_ir_virt::{virtualize_ir, BytecodeForm, DispatchMode, VirtualizeConfig};

fn cfg_default() -> VirtualizeConfig {
    VirtualizeConfig {
        dispatch: DispatchMode::Public,
        bytecode_form: BytecodeForm::External,
        ..VirtualizeConfig::default()
    }
}

#[test]
fn sixteen_const_only_blocks_dedup_to_single_handler() {
    let mut types = IRTypes(vec![IRType::Primitive(PrimType::Bit)]);
    let u32_ty = types.intern(IRType::Primitive(PrimType::_32));

    // 16 blocks, each with shape `c = Const(k, _32); Jmp(Return, [c])`.
    // All share the same canonical key.
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

    let out = virtualize_ir(&blocks, &mut types, &cfg_default());

    assert_eq!(
        out.blocks_in, 16,
        "input block count sanity"
    );
    assert_eq!(
        out.n_handlers, 1,
        "all 16 blocks share canonical shape (Stmt::Const differs \
         only by value, which is lifted to an immediate); expected \
         exactly one handler after dedup"
    );

    // External bytecode should have 16 entries, each pointing at
    // handler 0, with a single `Constant` immediate slot.
    let bc = out.bytecode.expect("external bytecode requested");
    assert_eq!(bc.n_handlers, 1);
    assert_eq!(bc.entries.len(), 16);
    for (pc, entry) in bc.entries.iter().enumerate() {
        assert_eq!(entry.handler_idx, 0, "pc={}", pc);
        assert_eq!(entry.consts.len(), 1, "pc={}", pc);
        assert_eq!(entry.consts[0].lo, pc as u128, "pc={}", pc);
    }
}

#[test]
fn handler_count_monotone_in_structural_variety() {
    // Adding a block with a structurally different terminator should
    // strictly increase n_handlers by one.
    let mut types = IRTypes(vec![IRType::Primitive(PrimType::Bit)]);
    let u32_ty = types.intern(IRType::Primitive(PrimType::_32));

    let mk_const_return = |k: u128| IRBlock {
        params: vec![u32_ty],
        stmts: vec![Stmt::Const(Constant { hi: 0, lo: k }, u32_ty)],
        stmt_provs: vec![()],
        terminator: IRTerminator::Jmp {
            func: IRBlockTargetId::Return,
            args: vec![IRVarId(1)],
        },
    };

    // 4 const-return blocks (all dedup) + 1 distinct "jump to block 5
    // with passthrough" block + 1 passthrough-return block.  That last
    // block shares shape with the 4 above (Jmp(Return, [param])) only
    // if it actually returns a param and not a const.  Make the two
    // distinct explicitly.
    let blocks = IRBlocks::new(vec![
        mk_const_return(0),
        mk_const_return(1),
        mk_const_return(2),
        mk_const_return(3),
        // Distinct block: passes through its param without emitting a
        // Const.
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

    let out = virtualize_ir(&blocks, &mut types, &cfg_default());
    assert_eq!(out.blocks_in, 5);
    assert_eq!(
        out.n_handlers, 2,
        "4 const-return blocks collapse to one handler; the passthrough \
         block is structurally distinct (no Const stmt)"
    );
}
