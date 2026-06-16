// @reliability: experimental
// @ai: assisted
//! Cross-block SharedCore dedup with adaptive split enabled.

use volar_ir::ir::{
    IRBlock, IRBlockTargetId, IRBlocks, IRTerminator, IRType, IRTypes, IRVarId, PrimType,
};
use volar_ir_common::{Constant, Stmt};
use volar_ir_virt::{virtualize_ir, AdaptiveSplitConfig, BytecodeForm, DispatchMode, VirtualizeConfig};

fn cfg_split() -> VirtualizeConfig {
    VirtualizeConfig {
        dispatch: DispatchMode::Public,
        bytecode_form: BytecodeForm::External,
        adaptive_split: AdaptiveSplitConfig {
            enabled: true,
            min_sequence_len: 4,
            min_reuse_count: 2,
            ..AdaptiveSplitConfig::default()
        },
        ..VirtualizeConfig::default()
    }
}

#[test]
fn shared_core_reduces_handlers_across_blocks() {
    let mut types = IRTypes(vec![IRType::Primitive(PrimType::Bit)]);
    let ty = types.intern(IRType::Primitive(PrimType::_32));
    let core = vec![
        Stmt::Const(Constant { hi: 0, lo: 1 }, ty),
        Stmt::Const(Constant { hi: 0, lo: 2 }, ty),
        Stmt::Const(Constant { hi: 0, lo: 3 }, ty),
        Stmt::Const(Constant { hi: 0, lo: 4 }, ty),
    ];
    let mk = |p: u128, s: u128| {
        let mut stmts = vec![Stmt::Const(Constant { hi: 0, lo: p }, ty)];
        stmts.extend(core.clone());
        stmts.push(Stmt::Const(Constant { hi: 0, lo: s }, ty));
        IRBlock {
            params: vec![ty],
            stmts,
            stmt_provs: vec![(); 6],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: vec![IRVarId(5)],
            },
        }
    };
    let blocks = IRBlocks::new(vec![mk(0, 1), mk(2, 3), mk(4, 5), mk(6, 7)]);
    let out = virtualize_ir(&blocks, &mut types, &cfg_split());
    assert!(out.n_appended_regions >= 1);
    let bc = out.bytecode.expect("bytecode");
    assert!(bc.entries.len() > bc.outer_block_count);
    assert!(bc.regions.iter().any(|r| matches!(
        r.kind,
        volar_ir_virt::AppendedRegionKind::SharedCore { .. }
    )));
}
