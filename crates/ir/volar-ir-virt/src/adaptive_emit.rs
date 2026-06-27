// @reliability: experimental
// @ai: assisted
//! Adaptive split emission (SharedCore sub-interpreter + RerollLoop drivers).

use alloc::{vec, vec::Vec};

use volar_ir::ir::{
    IRBlock, IRBlockId, IRBlockTargetId, IRBlocks, IRStmt, IRTerminator, IRType, IRTypeId,
    IRTypes, IRVarId, IRBranchTarget};
use volar_ir_common::{Stmt, StorageId, Type as PrimType};

use crate::bytecode::AppendedRegionKind;
use crate::canon::{
    canon_ir_stmt_public, canon_ir_terminator_public, canonicalize_ir_block,
    canonicalize_stmt_slice, BlockImmediates, IrHandlerKey,
};
use crate::ctx::{DedupTable, VirtOutput};
use crate::preinit::{build_ir_storage_init_adaptive, merge_pre_init};
use crate::hash::IrHashAlgorithm;
use crate::layout::{AdaptiveSplitPlan, BlockCompositePlan, SegmentInvoke};
use crate::VirtualizeConfig;
use crate::ir::{
    const_u32, emit_dispatch_block_with_base, emit_dispatcher_block,
    emit_handler_block, emit_prologue_stmts, emit_return_block, emit_setup_block, GlobalLayout,
    HandlerSchema, RegAlloc, IRBlockUnfinished, RETURN_BID,
};

const ADAPTIVE_SUB_DISPATCHER_BID: u32 = 4;
const ADAPTIVE_SUB_DISPATCH_BID: u32 = 5;
const ADAPTIVE_SUB_RETURN_BID: u32 = 6;
const ADAPTIVE_REROLL_DRIVER_BID: u32 = 7;
const ADAPTIVE_HANDLER_BID_BASE: u32 = 8;

pub(super) fn virtualize_ir_adaptive<P: Clone + Default, H: IrHashAlgorithm>(
    cse_blocks: &IRBlocks<P>,
    types: &mut IRTypes,
    cfg: &VirtualizeConfig,
    split_plan: AdaptiveSplitPlan,
    blocks_in: usize,
) -> VirtOutput<IRBlocks<P>> {
    let addr_ty = types.intern(IRType::Primitive(PrimType::_32));
    let bit_ty = types.intern(IRType::Primitive(PrimType::Bit));

    let per_block_canon = build_outer_canon_keys(cse_blocks, &split_plan.block_plans);
    let dedup = DedupTable::build(per_block_canon);
    let layout = GlobalLayout::from_dedup(&dedup, addr_ty, bit_ty, cfg.bytecode_storage);
    let reg_storage_base = layout.next_free_storage_after_bytecode(cfg.bytecode_storage);
    let reg_alloc = RegAlloc::build(cse_blocks, reg_storage_base, &types.0);

    let ctrl_prov: P = cse_blocks
        .blocks
        .iter()
        .flat_map(|b| b.stmts.iter())
        .map(|n| &n.prov)
        .next()
        .cloned()
        .unwrap_or_default();

    let (sub_micro, reroll_bodies, region_pc_bases) =
        build_appended_program(cse_blocks, &split_plan, addr_ty);

    let mut all_handler_keys = dedup.handler_keys.clone();
    all_handler_keys.extend(sub_micro.keys.clone());
    all_handler_keys.extend(reroll_bodies.keys.clone());

    let merged_layout = GlobalLayout::from_keys(
        &all_handler_keys,
        addr_ty,
        bit_ty,
        cfg.bytecode_storage,
    );

    let out_blocks = emit_adaptive_module::<P, H>(
        cse_blocks,
        &dedup,
        &merged_layout,
        &reg_alloc,
        &split_plan,
        &region_pc_bases,
        addr_ty,
        bit_ty,
        cfg,
        types,
        &ctrl_prov,
    );

    let storage_init = build_ir_storage_init_adaptive(
        cse_blocks,
        &dedup,
        &merged_layout,
        &reg_alloc,
        cfg.bytecode_storage,
        addr_ty,
        &types.0,
        &split_plan,
    );
    let merged_pre_init = merge_pre_init(&cse_blocks.pre_init, &storage_init.pre_init);

    VirtOutput {
        blocks: IRBlocks {
            pre_init: merged_pre_init,
            ..out_blocks
        },
        bytecode: Some(storage_init.bytecode),
        n_handlers: all_handler_keys.len(),
        blocks_in,
        key_params: Vec::new(),
        n_appended_regions: split_plan.layout.regions.len(),
    }
}

fn build_outer_canon_keys<P: Clone>(
    blocks: &IRBlocks<P>,
    plans: &[BlockCompositePlan],
) -> Vec<(IrHandlerKey, BlockImmediates)> {
    blocks
        .blocks
        .iter()
        .zip(plans.iter())
        .map(|(block, plan)| {
            if plan.segments.is_empty() {
                canonicalize_ir_block(block)
            } else {
                canonicalize_composite_block(block, plan)
            }
        })
        .collect()
}

fn canonicalize_composite_block<P: Clone>(
    block: &IRBlock<P>,
    plan: &BlockCompositePlan,
) -> (IrHandlerKey, BlockImmediates) {
    let mut consts = Vec::new();
    let mut targets = Vec::new();

    let prologue: Vec<IRStmt> = block.stmts[plan.prologue.clone()]
        .iter()
        .map(|s| canon_ir_stmt_public(&s.kind, &mut consts))
        .collect();
    let epilogue: Vec<IRStmt> = block.stmts[plan.epilogue.clone()]
        .iter()
        .map(|s| canon_ir_stmt_public(&s.kind, &mut consts))
        .collect();
    let canon_term = canon_ir_terminator_public(&block.terminator, &mut targets);

    let mut combined = prologue;
    combined.extend(epilogue);
    let key = IrHandlerKey {
        params: block.params.clone(),
        stmts: combined,
        terminator: canon_term,
    };
    (key, BlockImmediates { consts, targets })
}

struct AppendedHandlers {
    keys: Vec<IrHandlerKey>,
}

fn build_appended_program<P: Clone>(
    blocks: &IRBlocks<P>,
    plan: &AdaptiveSplitPlan,
    addr_ty: IRTypeId,
) -> (AppendedHandlers, AppendedHandlers, Vec<u32>) {
    let mut micro = AppendedHandlers { keys: Vec::new() };
    let mut reroll = AppendedHandlers { keys: Vec::new() };
    let mut region_pc_bases = Vec::new();
    let mut region_idx = 0usize;

    for region in &plan.layout.regions {
        region_pc_bases.push(region.pc_start);
        match &region.kind {
            AppendedRegionKind::SharedCore { members } => {
                let (_, start, end) = members.first().copied().unwrap_or((0, 0, 0));
                let block = &blocks.blocks[start as usize];
                for step in start..end {
                    let stmt = block.stmts[step as usize].kind.clone();
                    let key = IrHandlerKey {
                        params: vec![addr_ty],
                        stmts: vec![stmt],
                        terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Block(IRBlockId(ADAPTIVE_SUB_RETURN_BID)), vec![IRVarId(0)],) },
                    };
                    push_unique_key(&mut micro.keys, key);
                }
            }
            AppendedRegionKind::RerollLoop { owner_block, .. } => {
                let spec = &plan.reroll_loops[region_idx];
                let block = &blocks.blocks[*owner_block];
                let kinds: Vec<IRStmt> = block.stmts[spec.body_range.clone()]
                    .iter().map(|n| n.kind.clone()).collect();
                let (slice_key, _) = canonicalize_stmt_slice(&kinds);
                let key = IrHandlerKey {
                    params: vec![addr_ty],
                    stmts: slice_key.stmts,
                    terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Block(IRBlockId(ADAPTIVE_REROLL_DRIVER_BID)), vec![IRVarId(0)],) },
                };
                push_unique_key(&mut reroll.keys, key);
            }
        }
        region_idx += 1;
    }

    (micro, reroll, region_pc_bases)
}

fn push_unique_key(keys: &mut Vec<IrHandlerKey>, key: IrHandlerKey) {
    if !keys.iter().any(|k| k == &key) {
        keys.push(key);
    }
}

#[allow(clippy::too_many_arguments)]
fn emit_adaptive_module<P: Clone, H: IrHashAlgorithm>(
    cse_blocks: &IRBlocks<P>,
    dedup: &DedupTable<IrHandlerKey>,
    layout: &GlobalLayout,
    reg_alloc: &RegAlloc,
    split_plan: &AdaptiveSplitPlan,
    region_pc_bases: &[u32],
    addr_ty: IRTypeId,
    bit_ty: IRTypeId,
    cfg: &VirtualizeConfig,
    types: &mut IRTypes,
    ctrl_prov: &P,
) -> IRBlocks<P> {
    let return_arg_tys: Vec<IRTypeId> = reg_alloc.return_regs.iter().map(|r| r.ty).collect();

    let setup = emit_setup_block::<P, H>(
        &cse_blocks.blocks[0].params.clone(),
        reg_alloc,
        addr_ty,
        bit_ty,
        cfg,
        None,
        ctrl_prov,
    );

    let dispatcher = emit_dispatcher_block(addr_ty, bit_ty, ctrl_prov);
    let return_block = emit_return_block(reg_alloc, &return_arg_tys, addr_ty, ctrl_prov);
    let dispatch = emit_dispatch_block_with_base(
        dedup,
        cfg.bytecode_storage,
        addr_ty,
        ADAPTIVE_HANDLER_BID_BASE,
        ctrl_prov,
    );

    let sub_dispatcher = emit_sub_dispatcher_block(addr_ty, ctrl_prov);
    let sub_dispatch = emit_dispatch_block_with_base(
        dedup,
        cfg.bytecode_storage,
        addr_ty,
        ADAPTIVE_HANDLER_BID_BASE + dedup.n_handlers() as u32,
        ctrl_prov,
    );
    let sub_return = emit_sub_return_block(addr_ty, ctrl_prov);
    let reroll_driver = emit_reroll_driver_block(addr_ty, ctrl_prov);

    let mut handler_blocks: Vec<IRBlock<P>> = Vec::new();
    let mut extra_blocks: Vec<IRBlock<P>> = Vec::new();
    let mut next_sub_bid = ADAPTIVE_HANDLER_BID_BASE + dedup.n_handlers() as u32 + 64;

    for block_id in 0..cse_blocks.blocks.len() {
        let (h_idx, _) = &dedup.per_block[block_id];
        let h_idx = *h_idx as usize;
        let key = &dedup.handler_keys[h_idx];
        let schema = &layout.schemas[h_idx];
        let slot_ids = &layout.per_handler_slot[h_idx];
        let block_plan = &split_plan.block_plans[block_id];

        if block_plan.segments.is_empty() {
            let (handler, extras) = emit_handler_block::<P, H>(
                key,
                schema,
                slot_ids,
                reg_alloc,
                addr_ty,
                bit_ty,
                cfg.bytecode_storage,
                types,
                &mut next_sub_bid,
                None,
                None,
                ctrl_prov,
            );
            handler_blocks.push(handler);
            extra_blocks.extend(extras);
        } else if block_plan.segments.len() == 1
            && matches!(block_plan.segments[0], SegmentInvoke::RerollLoop { .. })
            && block_plan.epilogue.is_empty()
        {
            let handler = emit_reroll_only_handler::<P>(
                cse_blocks,
                block_id,
                split_plan,
                &block_plan.segments[0],
                addr_ty,
                ctrl_prov,
            );
            handler_blocks.push(handler);
        } else {
            let resume_bid = next_sub_bid;
            next_sub_bid += 1;
            let (part, resume) = emit_composite_handler::<P, H>(
                cse_blocks,
                block_id,
                key,
                schema,
                slot_ids,
                reg_alloc,
                block_plan,
                split_plan,
                region_pc_bases,
                resume_bid,
                addr_ty,
                bit_ty,
                cfg.bytecode_storage,
                types,
                &mut next_sub_bid,
                ctrl_prov,
            );
            handler_blocks.push(part);
            extra_blocks.push(resume);
        }
    }

    let mut all_blocks = vec![
        setup,
        dispatcher,
        return_block,
        dispatch,
        sub_dispatcher,
        sub_dispatch,
        sub_return,
        reroll_driver,
    ];
    all_blocks.extend(handler_blocks);
    all_blocks.extend(extra_blocks);

    IRBlocks {
        oracles: cse_blocks.oracles.clone(),
        actions: cse_blocks.actions.clone(),
        rngs: cse_blocks.rngs.clone(),
        blocks: all_blocks,
        pre_init: Vec::new(),
    }
}

#[allow(clippy::too_many_arguments)]
fn emit_composite_handler<P: Clone, H: IrHashAlgorithm>(
    blocks: &IRBlocks<P>,
    block_id: usize,
    key: &IrHandlerKey,
    schema: &HandlerSchema,
    slot_ids: &[StorageId],
    reg_alloc: &RegAlloc,
    plan: &BlockCompositePlan,
    split_plan: &AdaptiveSplitPlan,
    region_pc_bases: &[u32],
    resume_bid: u32,
    addr_ty: IRTypeId,
    bit_ty: IRTypeId,
    bytecode_storage: StorageId,
    types: &mut IRTypes,
    next_sub_bid: &mut u32,
    ctrl_prov: &P,
) -> (IRBlock<P>, IRBlock<P>) {
    let block = &blocks.blocks[block_id];
    let mut b = IRBlockUnfinished::new(vec![addr_ty]);
    let pc = IRVarId(0);

    emit_prologue_stmts(
        &mut b,
        &block.stmts[plan.prologue.clone()].iter().map(|n| n.kind.clone()).collect::<Vec<_>>(),
        schema,
        slot_ids,
        addr_ty,
        pc,
    );

    let mut term_set = false;
    if let Some(seg) = plan.segments.first() {
        match seg {
            SegmentInvoke::SharedCore { region_index, .. } => {
                let entry_pc = region_pc_bases[*region_index];
                let entry = b.push(Stmt::Const(const_u32(entry_pc), addr_ty));
                b.terminator = IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Block(IRBlockId(ADAPTIVE_SUB_DISPATCHER_BID)), vec![entry],) };
                term_set = true;
            }
            SegmentInvoke::RerollLoop { region_index } => {
                let spec = &split_plan.reroll_loops[*region_index];
                let body_len = spec.body_range.len();
                let trips = match spec.trip_count {
                    crate::bytecode::TripCount::Fixed(k) => k as usize,
                    _ => 1,
                };
                for rep in 0..trips {
                    let start = spec.covered_range.start + rep * body_len;
                    let end = start + body_len;
                    emit_body_stmts(
                        &mut b,
                        &block.stmts[start..end].iter().map(|n| n.kind.clone()).collect::<Vec<_>>(),
                        schema,
                        slot_ids,
                        addr_ty,
                        pc,
                    );
                }
                b.terminator = IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Block(IRBlockId(resume_bid)), vec![pc],) };
                term_set = true;
            }
        }
    }

    if !term_set {
        b.terminator = block.terminator.clone();
    }

    let mut resume = IRBlockUnfinished::new(vec![addr_ty]);
    let pc_r = IRVarId(0);
    emit_prologue_stmts(
        &mut resume,
        &block.stmts[plan.epilogue.clone()].iter().map(|n| n.kind.clone()).collect::<Vec<_>>(),
        schema,
        slot_ids,
        addr_ty,
        pc_r,
    );

    resume.terminator = block.terminator.clone();

    (b.into_ir_block::<P>(ctrl_prov), resume.into_ir_block::<P>(ctrl_prov))
}

fn emit_sub_dispatcher_block<P: Clone>(addr_ty: IRTypeId, ctrl_prov: &P) -> IRBlock<P> {
    let b = IRBlockUnfinished {
        params: vec![addr_ty],
        stmts: Vec::new(),
        terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Block(IRBlockId(ADAPTIVE_SUB_DISPATCH_BID)), vec![IRVarId(0)],) },
    };
    b.into_ir_block::<P>(ctrl_prov)
}

fn emit_sub_return_block<P: Clone>(addr_ty: IRTypeId, ctrl_prov: &P) -> IRBlock<P> {
    let b = IRBlockUnfinished {
        params: vec![addr_ty],
        stmts: Vec::new(),
        terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Block(IRBlockId(RETURN_BID)), vec![],) },
    };
    b.into_ir_block::<P>(ctrl_prov)
}

fn emit_reroll_only_handler<P: Clone>(
    blocks: &IRBlocks<P>,
    block_id: usize,
    split_plan: &AdaptiveSplitPlan,
    seg: &SegmentInvoke,
    addr_ty: IRTypeId,
    ctrl_prov: &P,
) -> IRBlock<P> {
    let block = &blocks.blocks[block_id];
    let mut b = IRBlockUnfinished::new(block.params.clone());
    if let SegmentInvoke::RerollLoop { region_index } = seg {
        let spec = &split_plan.reroll_loops[*region_index];
        let body_len = spec.body_range.len();
        let trips = match spec.trip_count {
            crate::bytecode::TripCount::Fixed(k) => k as usize,
            _ => 1,
        };
        for rep in 0..trips {
            let start = spec.covered_range.start + rep * body_len;
            let end = start + body_len;
            for s in &block.stmts[start..end] {
                if let Stmt::Const(c, ty) = &s.kind {
                    let _ = b.push(Stmt::Const(*c, *ty));
                }
            }
        }
    }
    b.terminator = block.terminator.clone();
    b.into_ir_block::<P>(ctrl_prov)
}

fn emit_body_stmts(
    b: &mut IRBlockUnfinished,
    stmts: &[IRStmt],
    schema: &HandlerSchema,
    slot_ids: &[StorageId],
    addr_ty: IRTypeId,
    pc: IRVarId,
) {
    for (i, s) in stmts.iter().enumerate() {
        match s {
            Stmt::Const(c, ty) => {
                let _ = b.push(Stmt::Const(*c, *ty));
                let _ = i;
                let _ = schema;
                let _ = slot_ids;
                let _ = pc;
            }
            Stmt::Poly { ty, coeffs, constant } => {
                let _ = b.push(Stmt::Poly {
                    ty: *ty,
                    coeffs: coeffs.clone(),
                    constant: *constant,
                });
            }
            _ => {}
        }
    }
}

fn emit_reroll_driver_block<P: Clone>(addr_ty: IRTypeId, ctrl_prov: &P) -> IRBlock<P> {
    let b = IRBlockUnfinished {
        params: vec![addr_ty],
        stmts: Vec::new(),
        terminator: IRTerminator::Jmp { target: IRBranchTarget::new(IRBlockTargetId::Block(IRBlockId(ADAPTIVE_SUB_RETURN_BID)), vec![IRVarId(0)],) },
    };
    b.into_ir_block::<P>(ctrl_prov)
}
