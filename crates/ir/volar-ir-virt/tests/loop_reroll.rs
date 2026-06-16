// @reliability: experimental
// @ai: assisted
//! Intra-block RerollLoop with adaptive split enabled.

use volar_fuzz::interpreter::ir::{bit_width, const_to_bits, eval_ir};
use volar_ir::ir::{
    IRBlock, IRBlockTargetId, IRBlocks, IRTerminator, IRType, IRTypes, IRVarId, PrimType,
};
use volar_ir_common::{Constant, Stmt};
use volar_ir_virt::{virtualize_ir, AdaptiveSplitConfig, BytecodeForm, DispatchMode, VirtualizeConfig};

fn cfg_split() -> VirtualizeConfig {
    VirtualizeConfig {
        dispatch: DispatchMode::Public,
        bytecode_form: BytecodeForm::InIr,
        adaptive_split: AdaptiveSplitConfig {
            enabled: true,
            cross_block: false,
            loop_reroll: true,
            min_reroll_iterations: 3,
            min_reroll_body_len: 2,
            ..AdaptiveSplitConfig::default()
        },
        ..VirtualizeConfig::default()
    }
}

fn repeated_body_block() -> (IRBlocks, IRTypes) {
    let mut types = IRTypes(vec![IRType::Primitive(PrimType::Bit)]);
    let ty = types.intern(IRType::Primitive(PrimType::_32));
    let mut stmts = Vec::new();
    for k in 0..3u128 {
        stmts.push(Stmt::Const(Constant { hi: 0, lo: k + 1 }, ty));
        stmts.push(Stmt::Const(Constant { hi: 0, lo: k + 10 }, ty));
    }
    let blocks = IRBlocks::new(vec![IRBlock {
        params: vec![ty],
        stmts,
        stmt_provs: vec![(); 6],
        terminator: IRTerminator::Jmp {
            func: IRBlockTargetId::Return,
            args: vec![IRVarId(5)],
        },
    }]);
    (blocks, types)
}

#[test]
fn reroll_appends_descriptor_row() {
    let (blocks, mut types) = repeated_body_block();
    let out = virtualize_ir(&blocks, &mut types, &cfg_split());
    assert_eq!(out.n_appended_regions, 1);
    let bc = out.bytecode.expect("bytecode");
    assert_eq!(bc.entries.len(), bc.outer_block_count + 1);
}

#[test]
fn reroll_split_preserves_semantics() {
    let (blocks, mut types) = repeated_body_block();
    let ty = types.intern(IRType::Primitive(PrimType::_32));
    let input = const_to_bits(&Constant { hi: 0, lo: 0 }, bit_width(ty, &types));
    let ref_out = eval_ir(&blocks, &types, &[input.clone()]).expect("ref");
    let virt = virtualize_ir(&blocks, &mut types, &cfg_split());
    let virt_out = eval_ir(&virt.blocks, &types, &[input]).expect("virt");
    assert_eq!(ref_out, virt_out);
}
