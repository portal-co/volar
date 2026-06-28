// @reliability: experimental
// @ai: assisted
//! Adaptive split planners: cross-block SharedCore and intra-block RerollLoop.

use alloc::{
    collections::BTreeMap,
    vec,
    vec::Vec,
};

use volar_ir::ir::{IRBlock, IRBlocks, IRStmt};

use crate::bytecode::{AppendedRegionKind, OperandMode, TripCount};
use crate::canon::{canonicalize_stmt_slice, StmtSliceKey};
use crate::layout::{
    AdaptiveSplitPlan, BlockCompositePlan, RerollLoopSpec, SegmentInvoke, SharedCoreSpec,
    UnifiedBytecodeLayout,
};
use crate::AdaptiveSplitConfig;

#[derive(Clone, Debug)]
struct WindowOccurrence {
    block_id: usize,
    range: core::ops::Range<usize>,
}

/// Build an adaptive split plan for the input module (may be empty).
pub fn plan_adaptive_split<P: Clone>(
    blocks: &IRBlocks<P>,
    cfg: &AdaptiveSplitConfig,
) -> AdaptiveSplitPlan {
    if !cfg.enabled {
        return AdaptiveSplitPlan::default();
    }

    let n_blocks = blocks.blocks.len();
    let mut block_plans: Vec<BlockCompositePlan> = (0..n_blocks)
        .map(|_| BlockCompositePlan {
            prologue: 0..0,
            segments: Vec::new(),
            epilogue: 0..0,
        })
        .collect();

    let mut shared_cores: Vec<SharedCoreSpec> = Vec::new();
    let mut reroll_loops: Vec<RerollLoopSpec> = Vec::new();
    let mut layout = UnifiedBytecodeLayout {
        outer_block_count: n_blocks,
        ..Default::default()
    };

    let mut used_ranges: Vec<Vec<core::ops::Range<usize>>> = vec![Vec::new(); n_blocks];

    if cfg.cross_block {
        plan_cross_block(
            blocks,
            cfg,
            &mut block_plans,
            &mut shared_cores,
            &mut layout,
            &mut used_ranges,
        );
    }

    if cfg.loop_reroll {
        crate::cfg_hints::plan_cfg_loops_from_hints(
            blocks,
            cfg,
            &mut block_plans,
            &mut reroll_loops,
            &mut layout,
            &mut used_ranges,
        );
        plan_reroll_loops(
            blocks,
            cfg,
            &mut block_plans,
            &mut reroll_loops,
            &mut layout,
            &mut used_ranges,
        );
    }

    for (block_id, plan) in block_plans.iter_mut().enumerate() {
        let n_stmts = blocks.blocks[block_id].stmts.len();
        if plan.segments.is_empty() {
            plan.prologue = 0..n_stmts;
            plan.epilogue = n_stmts..n_stmts;
        } else {
            let mut covered = vec![false; n_stmts];
            for seg in &plan.segments {
                let r = segment_stmt_range(block_id, seg, &shared_cores, &reroll_loops);
                for i in r {
                    if i < n_stmts {
                        covered[i] = true;
                    }
                }
            }
            let first = covered.iter().position(|&c| !c).unwrap_or(n_stmts);
            let last = covered
                .iter()
                .rposition(|&c| !c)
                .map(|i| i + 1)
                .unwrap_or(0);
            if first >= last {
                // Segment(s) cover the whole block (e.g. reroll-only outer shell).
                plan.prologue = 0..0;
                plan.epilogue = n_stmts..n_stmts;
            } else {
                plan.prologue = 0..first;
                plan.epilogue = last..n_stmts;
            }
        }
    }

    AdaptiveSplitPlan {
        block_plans,
        layout,
        shared_cores,
        reroll_loops,
    }
}

fn segment_stmt_range(
    block_id: usize,
    seg: &SegmentInvoke,
    shared_cores: &[SharedCoreSpec],
    reroll_loops: &[RerollLoopSpec],
) -> core::ops::Range<usize> {
    match seg {
        SegmentInvoke::SharedCore { region_index, .. } => shared_cores[*region_index]
            .members
            .iter()
            .find(|(b, _)| *b == block_id)
            .map(|(_, r)| r.clone())
            .unwrap_or(0..0),
        SegmentInvoke::RerollLoop { region_index } => {
            let spec = &reroll_loops[*region_index];
            if spec.owner_block == block_id {
                spec.covered_range.clone()
            } else {
                0..0
            }
        }
    }
}

fn overlaps(ranges: &[core::ops::Range<usize>], range: &core::ops::Range<usize>) -> bool {
    ranges.iter().any(|r| r.start < range.end && range.start < r.end)
}

fn plan_cross_block<P: Clone>(
    blocks: &IRBlocks<P>,
    cfg: &AdaptiveSplitConfig,
    block_plans: &mut [BlockCompositePlan],
    shared_cores: &mut Vec<SharedCoreSpec>,
    layout: &mut UnifiedBytecodeLayout,
    used_ranges: &mut [Vec<core::ops::Range<usize>>],
) {
    let mut window_map: BTreeMap<StmtSliceKey, Vec<WindowOccurrence>> = BTreeMap::new();
    for (block_id, block) in blocks.blocks.iter().enumerate() {
        index_windows(block, block_id, cfg.min_sequence_len, &mut window_map);
    }

    let mut candidates: Vec<(isize, StmtSliceKey, Vec<WindowOccurrence>)> = Vec::new();
    for (key, occs) in window_map {
        if occs.len() < cfg.min_reuse_count {
            continue;
        }
        let len = key.stmts.len();
        let benefit = (occs.len() as isize * len as isize) - cfg.sub_interp_entry_cost as isize;
        if benefit > 0 {
            candidates.push((benefit, key, occs));
        }
    }
    candidates.sort_by(|a, b| b.0.cmp(&a.0));

    for (_benefit, key, occs) in candidates {
        if shared_cores.len() >= cfg.max_appended_regions {
            break;
        }
        let mut members: Vec<(usize, core::ops::Range<usize>)> = Vec::new();
        for o in &occs {
            if overlaps(&used_ranges[o.block_id], &o.range) {
                continue;
            }
            members.push((o.block_id, o.range.clone()));
        }
        if members.len() < cfg.min_reuse_count {
            continue;
        }
        let body_len = key.stmts.len();
        layout.push_region(
            AppendedRegionKind::SharedCore {
                members: members
                    .iter()
                    .map(|(b, r)| (*b, r.start as u32, r.end as u32))
                    .collect(),
            },
            body_len,
        );
        let region_index = shared_cores.len();
        shared_cores.push(SharedCoreSpec { members: members.clone() });
        for (block_id, range) in members {
            used_ranges[block_id].push(range);
            block_plans[block_id].segments.push(SegmentInvoke::SharedCore {
                region_index,
                entry_offset: 0,
            });
        }
        let _ = key;
    }
}

fn plan_reroll_loops<P: Clone>(
    blocks: &IRBlocks<P>,
    cfg: &AdaptiveSplitConfig,
    block_plans: &mut [BlockCompositePlan],
    reroll_loops: &mut Vec<RerollLoopSpec>,
    layout: &mut UnifiedBytecodeLayout,
    used_ranges: &mut [Vec<core::ops::Range<usize>>],
) {
    for (block_id, block) in blocks.blocks.iter().enumerate() {
        if reroll_loops.len() >= cfg.max_appended_regions {
            break;
        }
        if let Some((body_range, iterations)) = find_reroll_body(block, cfg) {
            if overlaps(&used_ranges[block_id], &body_range) {
                continue;
            }
            let body_len = body_range.end - body_range.start;
            let benefit =
                (iterations as isize * body_len as isize) - cfg.sub_interp_entry_cost as isize;
            if benefit <= 0 {
                continue;
            }
            let region_index = reroll_loops.len();
            layout.push_region(
                AppendedRegionKind::RerollLoop {
                    owner_block: block_id,
                    body_handler_idx: 0,
                    trip_count: TripCount::Fixed(iterations as u32),
                    operand_mode: OperandMode::RegisterFile,
                },
                1,
            );
            reroll_loops.push(RerollLoopSpec {
                owner_block: block_id,
                body_range: body_range.clone(),
                trip_count: TripCount::Fixed(iterations as u32),
                operand_mode: OperandMode::RegisterFile,
                covered_range: 0..(body_len * iterations),
            });
            used_ranges[block_id].push(body_range);
            block_plans[block_id].segments.push(SegmentInvoke::RerollLoop { region_index });
        }
    }
}

fn find_reroll_body<P: Clone>(
    block: &IRBlock<P>,
    cfg: &AdaptiveSplitConfig,
) -> Option<(core::ops::Range<usize>, usize)> {
    let n = block.stmts.len();
    if n < cfg.min_reroll_body_len * cfg.min_reroll_iterations {
        return None;
    }
    for body_len in (cfg.min_reroll_body_len..=n / cfg.min_reroll_iterations).rev() {
        if n % body_len != 0 {
            continue;
        }
        let iterations = n / body_len;
        if iterations < cfg.min_reroll_iterations {
            continue;
        }
        let kinds: Vec<IRStmt> = block.stmts.iter().map(|n| n.kind.clone()).collect();
        let (key0, _) = canonicalize_stmt_slice(&kinds[0..body_len]);
        let mut all_match = true;
        for rep in 1..iterations {
            let start = rep * body_len;
            let (key, _) = canonicalize_stmt_slice(&kinds[start..start + body_len]);
            if key != key0 {
                all_match = false;
                break;
            }
        }
        if all_match {
            return Some((0..body_len, iterations));
        }
    }
    None
}

fn index_windows<P: Clone>(
    block: &IRBlock<P>,
    block_id: usize,
    min_len: usize,
    window_map: &mut BTreeMap<StmtSliceKey, Vec<WindowOccurrence>>,
) {
    let n = block.stmts.len();
    if n < min_len {
        return;
    }
    let kinds: Vec<IRStmt> = block.stmts.iter().map(|n| n.kind.clone()).collect();
    for start in 0..=(n - min_len) {
        for end in (start + min_len)..=n {
            let (key, _) = canonicalize_stmt_slice(&kinds[start..end]);
            window_map
                .entry(key)
                .or_default()
                .push(WindowOccurrence {
                    block_id,
                    range: start..end,
                });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use volar_ir::ir::{
        IRBlock, IRBlockTargetId, IRBlocks, IRBranchTarget, IRTerminator, IRType, IRTypes, IRVarId, PrimType,
    };
    use volar_ir_common::{Constant, Node, Stmt};

    fn u32_ty(types: &mut IRTypes) -> volar_ir::ir::IRTypeId {
        types.intern(IRType::Primitive(PrimType::_32))
    }

    #[test]
    fn cross_block_finds_shared_core() {
        let mut types = IRTypes(vec![IRType::Primitive(PrimType::Bit)]);
        let ty = u32_ty(&mut types);
        let core = vec![
            Stmt::Const(Constant { hi: 0, lo: 1 }, ty),
            Stmt::Const(Constant { hi: 0, lo: 2 }, ty),
            Stmt::Const(Constant { hi: 0, lo: 3 }, ty),
            Stmt::Const(Constant { hi: 0, lo: 4 }, ty),
        ];
        let mk_block = |prefix: u128, suffix: u128| {
            let mut stmts = vec![Stmt::Const(Constant { hi: 0, lo: prefix }, ty)];
            stmts.extend(core.clone());
            stmts.push(Stmt::Const(Constant { hi: 0, lo: suffix }, ty));
            IRBlock {
                params: vec![ty],
                stmts: stmts.into_iter().map(|s| Node::new(s, (), None)).collect(),
                terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Return, vec![IRVarId(5)],) },
            }
        };
        let blocks = IRBlocks::new(vec![mk_block(10, 20), mk_block(11, 21)]);
        let cfg = AdaptiveSplitConfig {
            enabled: true,
            min_sequence_len: 4,
            min_reuse_count: 2,
            ..AdaptiveSplitConfig::default()
        };
        let plan = plan_adaptive_split(&blocks, &cfg);
        assert_eq!(plan.shared_cores.len(), 1);
        assert!(plan.block_plans[0]
            .segments
            .iter()
            .any(|s| matches!(s, SegmentInvoke::SharedCore { .. })));
    }

    #[test]
    fn reroll_finds_repeated_body() {
        let mut types = IRTypes(vec![IRType::Primitive(PrimType::Bit)]);
        let ty = u32_ty(&mut types);
        let mut stmts = Vec::new();
        for k in 0..3u128 {
            stmts.push(Stmt::Const(Constant { hi: 0, lo: k * 10 + 1 }, ty));
            stmts.push(Stmt::Const(Constant { hi: 0, lo: k * 10 + 2 }, ty));
        }
        let n_stmts = stmts.len();
        let block = IRBlock {
            params: vec![ty],
            stmts: stmts.into_iter().map(|s| Node::new(s, (), None)).collect(),
            terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Return, vec![IRVarId(n_stmts as u32)],) },
        };
        let blocks = IRBlocks::new(vec![block]);
        let cfg = AdaptiveSplitConfig {
            enabled: true,
            cross_block: false,
            loop_reroll: true,
            min_reroll_iterations: 3,
            min_reroll_body_len: 2,
            ..AdaptiveSplitConfig::default()
        };
        let plan = plan_adaptive_split(&blocks, &cfg);
        assert_eq!(plan.reroll_loops.len(), 1);
        assert!(matches!(
            plan.reroll_loops[0].trip_count,
            TripCount::Fixed(3)
        ));
    }

    #[test]
    fn disabled_returns_empty_plan() {
        let mut types = IRTypes(vec![IRType::Primitive(PrimType::Bit)]);
        let ty = u32_ty(&mut types);
        let block = IRBlock {
            params: vec![ty],
            stmts: vec![Stmt::Const(Constant { hi: 0, lo: 5 }, ty)].into_iter().map(|s| Node::new(s, (), None)).collect(),
            terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Return, vec![IRVarId(1)],) },
        };
        let blocks = IRBlocks::new(vec![block]);
        let plan = plan_adaptive_split(&blocks, &AdaptiveSplitConfig::default());
        assert!(plan.shared_cores.is_empty());
        assert!(plan.reroll_loops.is_empty());
    }
}
