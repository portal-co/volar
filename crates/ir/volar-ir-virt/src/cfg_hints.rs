// @reliability: experimental
// @ai: assisted
//! CFG loop detection via reentry hints on back-edges.

use alloc::vec::Vec;

use volar_ir::ir::{IRBlockTargetId, IRBlocks, IRTerminator, IRVarId};
use volar_ir_common::{MeasureSpec, ReentryHint, Stmt};

use crate::bytecode::{AppendedRegionKind, OperandMode, TripCount};
use crate::layout::{BlockCompositePlan, RerollLoopSpec, SegmentInvoke, UnifiedBytecodeLayout};
use crate::AdaptiveSplitConfig;

#[derive(Clone, Debug)]
struct HintedLoop {
    header: usize,
    body: usize,
    trip_count: TripCount,
}

/// Plan reroll regions from explicit/autoderived reentry hints on back-edges.
pub fn plan_cfg_loops_from_hints<P: Clone>(
    blocks: &IRBlocks<P>,
    cfg: &AdaptiveSplitConfig,
    block_plans: &mut [BlockCompositePlan],
    reroll_loops: &mut Vec<RerollLoopSpec>,
    layout: &mut UnifiedBytecodeLayout,
    used_ranges: &mut [Vec<core::ops::Range<usize>>],
) {
    if !cfg.prefer_reentry_hints {
        return;
    }
    for hinted in collect_hinted_loops(blocks) {
        if reroll_loops.len() >= cfg.max_appended_regions {
            break;
        }
        let body_block = &blocks.blocks[hinted.body];
        let body_range = 0..body_block.stmts.len();
        if body_range.is_empty() {
            continue;
        }
        if overlaps(&used_ranges[hinted.body], &body_range) {
            continue;
        }
        let body_len = body_range.len();
        let trip_count = hinted.trip_count;
        let iterations = match trip_count {
            TripCount::Fixed(k) => k as usize,
            TripCount::BytecodeSlot(_) => continue,
        };
        if iterations < cfg.min_reroll_iterations {
            continue;
        }
        let benefit =
            (iterations as isize * body_len as isize) - cfg.sub_interp_entry_cost as isize;
        if benefit <= 0 {
            continue;
        }
        let region_index = reroll_loops.len();
        layout.push_region(
            AppendedRegionKind::RerollLoop {
                owner_block: hinted.body,
                body_handler_idx: 0,
                trip_count: trip_count.clone(),
                operand_mode: OperandMode::RegisterFile,
            },
            1,
        );
        reroll_loops.push(RerollLoopSpec {
            owner_block: hinted.body,
            body_range: body_range.clone(),
            trip_count,
            operand_mode: OperandMode::RegisterFile,
            covered_range: 0..(body_len * iterations),
        });
        used_ranges[hinted.body].push(body_range);
        block_plans[hinted.body].segments.push(SegmentInvoke::RerollLoop { region_index });
        let _ = hinted.header;
    }
}

fn collect_hinted_loops<P: Clone>(blocks: &IRBlocks<P>) -> Vec<HintedLoop> {
    let mut out = Vec::new();
    for (from, block) in blocks.blocks.iter().enumerate() {
        for bt in branch_targets(&block.terminator) {
            let Some(reentry) = bt.reentry.as_ref() else {
                continue;
            };
            let Some(header) = dest_block_id(&bt.dest) else {
                continue;
            };
            if header > from {
                continue;
            }
            if !matches_bounded_loop_hint(reentry) {
                continue;
            }
            let trip = infer_bounded_trip(blocks, header);
            out.push(HintedLoop {
                header,
                body: from,
                trip_count: trip,
            });
        }
    }
    out
}

fn branch_targets(term: &IRTerminator) -> Vec<&volar_ir::ir::IRBranchTarget> {
    match term {
        IRTerminator::Jmp { target } => alloc::vec![target],
        IRTerminator::JumpCond {
            then_target,
            else_target,
            ..
        } => alloc::vec![then_target, else_target],
        IRTerminator::JumpTable { cases, .. } => cases.values().collect(),
        _ => alloc::vec::Vec::new(),
    }
}

fn dest_block_id(dest: &IRBlockTargetId) -> Option<usize> {
    match dest {
        IRBlockTargetId::Block(b) => Some(b.0 as usize),
        _ => None,
    }
}

fn matches_bounded_loop_hint(h: &ReentryHint) -> bool {
    h.measures.len() == 1
        && matches!(
            &h.measures[0],
            MeasureSpec::Diff {
                minuend,
                subtrahend,
                signed: false,
            } if matches!(**minuend, MeasureSpec::Strict { param: 1, signed: false })
                && matches!(**subtrahend, MeasureSpec::Strict { param: 0, signed: false })
        )
}

fn infer_bounded_trip<P: Clone>(blocks: &IRBlocks<P>, header: usize) -> TripCount {
    for pred in blocks.blocks.iter() {
        for bt in branch_targets(&pred.terminator) {
            if bt.reentry.is_some() {
                continue;
            }
            if dest_block_id(&bt.dest) != Some(header) {
                continue;
            }
            if let Some((start, limit)) = u64_pair_from_vars(pred, &bt.args) {
                if limit >= start {
                    return TripCount::Fixed((limit - start) as u32);
                }
            }
        }
    }
    TripCount::BytecodeSlot(0)
}

/// Read the last two jump args as u64 constants defined in `pred` block stmts.
fn u64_pair_from_vars<P: Clone>(
    pred: &volar_ir::ir::IRBlock<P>,
    args: &[IRVarId],
) -> Option<(u64, u64)> {
    if args.len() < 2 {
        return None;
    }
    let start = const_u64_from_var(pred, args[args.len() - 2])?;
    let limit = const_u64_from_var(pred, args[args.len() - 1])?;
    Some((start, limit))
}

fn const_u64_from_var<P: Clone>(block: &volar_ir::ir::IRBlock<P>, var: IRVarId) -> Option<u64> {
    let idx = var.0 as usize;
    if idx < block.params.len() {
        return None;
    }
    let stmt_idx = idx - block.params.len();
    match &block.stmts.get(stmt_idx)?.kind {
        Stmt::Const(c, _) => Some(c.lo as u64),
        _ => None,
    }
}

fn overlaps(ranges: &[core::ops::Range<usize>], probe: &core::ops::Range<usize>) -> bool {
    ranges
        .iter()
        .any(|r| r.start < probe.end && probe.start < r.end)
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;
    use volar_ir::ir::{IRBlock, IRBranchTarget, IRBlockTargetId, IRType, IRTypes, PrimType};
    use volar_ir_common::{Constant, Node};

    fn u32_ty(types: &mut IRTypes) -> volar_ir::ir::IRTypeId {
        types.intern(IRType::Primitive(PrimType::_32))
    }

    #[test]
    fn infers_trip_from_preheader_constants() {
        let mut types = IRTypes(vec![IRType::Primitive(PrimType::Bit)]);
        let ty = u32_ty(&mut types);
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
                    condition: IRVarId(0),
                    then_target: IRBranchTarget::new(
                        IRBlockTargetId::Block(volar_ir::ir::IRBlockId(2)),
                        vec![],
                    ),
                    else_target: IRBranchTarget::new(IRBlockTargetId::Return, vec![]),
                },
            },
            IRBlock {
                params: vec![],
                stmts: vec![Stmt::Const(Constant { hi: 0, lo: 7 }, ty)].into_iter().map(|s| Node::new(s, (), None)).collect(),
                terminator: IRTerminator::Jmp {
                    target: IRBranchTarget {
                        dest: IRBlockTargetId::Block(volar_ir::ir::IRBlockId(1)),
                        args: vec![IRVarId(0), IRVarId(1)],
                        reentry: Some(ReentryHint::bounded_loop_ascending()),
                    },
                },
            },
        ]);
        let loops = collect_hinted_loops(&blocks);
        assert_eq!(loops.len(), 1);
        assert_eq!(loops[0].body, 2);
        assert_eq!(loops[0].trip_count, TripCount::Fixed(3));
    }
}
