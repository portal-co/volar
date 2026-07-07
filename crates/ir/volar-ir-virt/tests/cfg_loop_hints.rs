// @reliability: experimental
// @ai: assisted
//! CFG hinted loops via reentry metadata on back-edges.

use volar_ir::ir::{
    IRBlock, IRBlockTargetId, IRBlocks, IRBranchTarget, IRTerminator, IRType, IRTypes, IRVarId,
    PrimType,
};
use volar_ir_common::{Constant, Node, ReentryHint, Stmt};
use volar_ir_virt::split::plan_adaptive_split;
use volar_ir_virt::{AdaptiveSplitConfig, DispatchMode, VirtualizeConfig};

fn cfg_split() -> VirtualizeConfig {
    VirtualizeConfig {
        dispatch: DispatchMode::Public,
        adaptive_split: AdaptiveSplitConfig {
            enabled: true,
            cross_block: false,
            loop_reroll: true,
            prefer_reentry_hints: true,
            min_reroll_iterations: 3,
            min_reroll_body_len: 1,
            ..AdaptiveSplitConfig::default()
        },
        ..VirtualizeConfig::default()
    }
}

/// Three-block ascending loop: preheader → header → body → (back) header.
fn hinted_cfg_loop() -> (IRBlocks, IRTypes) {
    let mut types = IRTypes(vec![IRType::Primitive(PrimType::Bit)]);
    let ty = types.intern(IRType::Primitive(PrimType::_32));
    let blocks = IRBlocks::new(vec![
        IRBlock {
            params: vec![],
            stmts: vec![
                Stmt::Const(Constant { hi: 0, lo: 0 }, ty),
                Stmt::Const(Constant { hi: 0, lo: 3 }, ty),
            ].into_iter().map(|s| Node::new(s, (), None)).collect(),
            terminator: IRTerminator::Jmp {
                target: IRBranchTarget::new(
                    IRBlockTargetId::Block(volar_ir::ir::IRBlockId(1)),
                    vec![IRVarId(0), IRVarId(1)],
                ),
            },
        },
        IRBlock {
            params: vec![ty, ty],
            stmts: vec![],
            terminator: IRTerminator::JumpCond {
                condition: IRVarId(2),
                then_target: IRBranchTarget::new(
                    IRBlockTargetId::Block(volar_ir::ir::IRBlockId(2)),
                    vec![],
                ),
                else_target: IRBranchTarget::new(
                    IRBlockTargetId::Return,
                    vec![IRVarId(3)],
                ),
            },
        },
        IRBlock {
            params: vec![],
            stmts: vec![
                Stmt::Const(Constant { hi: 0, lo: 11 }, ty),
                Stmt::Const(Constant { hi: 0, lo: 22 }, ty),
            ].into_iter().map(|s| Node::new(s, (), None)).collect(),
            terminator: IRTerminator::Jmp {
                target: IRBranchTarget {
                    dest: IRBlockTargetId::Block(volar_ir::ir::IRBlockId(1)),
                    args: vec![IRVarId(0), IRVarId(1)],
                    reentry: Some(ReentryHint::bounded_loop_ascending()),
                },
            },
        },
    ]);
    (blocks, types)
}

#[test]
fn cfg_hint_plans_reroll_region() {
    let (blocks, _) = hinted_cfg_loop();
    let plan = plan_adaptive_split(&blocks, &cfg_split().adaptive_split);
    assert_eq!(plan.reroll_loops.len(), 1);
    assert_eq!(plan.reroll_loops[0].owner_block, 2);
    assert!(matches!(
        plan.reroll_loops[0].trip_count,
        volar_ir_virt::TripCount::Fixed(3)
    ));
}
