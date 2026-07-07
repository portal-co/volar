// @reliability: experimental
// @ai: assisted
//! Canonical virt storage initialization via [`PreInitSegment`].

use alloc::{
    collections::BTreeMap,
    vec,
    vec::Vec,
};

use volar_ir::ir::{IRBlocks, IRTypeId};
use volar_ir_common::{Constant, PreInitSegment, StorageId, TypeId};

use crate::bytecode::{
    AppendedRegionKind, BytecodeEntry, BytecodeRowKind, HandlerImmSchema, VirtBytecode,
};
use crate::canon::{BirHandlerKey, HandlerKey, IrHandlerKey};
use crate::ctx::DedupTable;
use crate::ir::{compute_slot_values, const_u32, GlobalLayout, RegAlloc};
use crate::layout::AdaptiveSplitPlan;

/// Static commitment lane written into `pre_init` (key params remain runtime).
pub(crate) struct CommitmentPreInit<'a> {
    pub storage: StorageId,
    pub hash_output_ty: IRTypeId,
    pub per_block: &'a [Constant],
}

/// Result of the canonical virt storage builder.
#[derive(Clone, Debug)]
pub(crate) struct VirtStorageInit {
    pub pre_init: Vec<PreInitSegment>,
    pub bytecode: VirtBytecode,
}

type LaneKey = (StorageId, TypeId);

/// BIR virt preinit segments always tag element type `TypeId(0)` (single bit in `lo`).
const BIR_PREINIT_BIT_TY: TypeId = TypeId(0);

fn zero_constant() -> Constant {
    Constant { hi: 0, lo: 0 }
}

fn const_bit(b: bool) -> Constant {
    Constant {
        hi: 0,
        lo: if b { 1 } else { 0 },
    }
}

fn lane_mut<'a>(
    lanes: &'a mut BTreeMap<LaneKey, Vec<Constant>>,
    storage: StorageId,
    ty: TypeId,
    total_rows: usize,
) -> &'a mut Vec<Constant> {
    lanes
        .entry((storage, ty))
        .or_insert_with(|| vec![zero_constant(); total_rows])
}

fn lanes_to_pre_init(lanes: BTreeMap<LaneKey, Vec<Constant>>) -> Vec<PreInitSegment> {
    lanes
        .into_iter()
        .map(|((storage, ty), data)| PreInitSegment {
            storage,
            ty,
            offset: 0,
            data,
        })
        .collect()
}

fn segments_overlap(a: &PreInitSegment, b: &PreInitSegment) -> bool {
    if a.storage != b.storage || a.ty != b.ty {
        return false;
    }
    let a_end = a.offset + a.data.len();
    let b_end = b.offset + b.data.len();
    a.offset < b_end && b.offset < a_end
}

/// Append virt segments after input `pre_init`, asserting no cell overlap.
pub(crate) fn merge_pre_init(
    input: &[PreInitSegment],
    virt: &[PreInitSegment],
) -> Vec<PreInitSegment> {
    for v in virt {
        for i in input {
            debug_assert!(
                !segments_overlap(i, v),
                "virt pre_init overlaps input pre_init at storage={:?} ty={:?}",
                v.storage,
                v.ty
            );
        }
    }
    let mut out = input.to_vec();
    out.extend_from_slice(virt);
    out
}

fn virt_bytecode_from_dedup<K: HandlerKey>(dedup: &DedupTable<K>) -> VirtBytecode {
    let handler_schemas: Vec<HandlerImmSchema> = dedup
        .handler_keys
        .iter()
        .map(|k| HandlerImmSchema {
            kinds: k.immediate_schema(),
        })
        .collect();
    let entries: Vec<BytecodeEntry> = dedup
        .per_block
        .iter()
        .map(|(h, imm)| BytecodeEntry::outer(*h, imm.consts.clone(), imm.targets.clone()))
        .collect();
    let outer_block_count = entries.len();
    VirtBytecode {
        n_handlers: dedup.handler_keys.len(),
        handler_schemas,
        entries,
        outer_block_count,
        regions: Vec::new(),
    }
}

/// Build dense `pre_init` lanes and matching [`VirtBytecode`] for standard IR virt.
pub(crate) fn build_ir_storage_init<P: Clone>(
    blocks_in: &IRBlocks<P>,
    dedup: &DedupTable<IrHandlerKey>,
    layout: &GlobalLayout,
    reg_alloc: &RegAlloc,
    bytecode_storage: StorageId,
    addr_ty: IRTypeId,
    ir_types: &[volar_ir::ir::IRType],
    commitment: Option<CommitmentPreInit<'_>>,
) -> VirtStorageInit {
    let total_rows = blocks_in.blocks.len();
    let mut lanes: BTreeMap<LaneKey, Vec<Constant>> = BTreeMap::new();

    fill_ir_outer_rows(
        blocks_in,
        dedup,
        layout,
        reg_alloc,
        bytecode_storage,
        addr_ty,
        ir_types,
        0..total_rows,
        &mut lanes,
        total_rows,
    );

    if let Some(c) = commitment {
        let lane = lane_mut(&mut lanes, c.storage, c.hash_output_ty, total_rows);
        for (pc, val) in c.per_block.iter().enumerate().take(total_rows) {
            lane[pc] = *val;
        }
    }

    VirtStorageInit {
        pre_init: lanes_to_pre_init(lanes),
        bytecode: virt_bytecode_from_dedup(dedup),
    }
}

/// Adaptive split: outer rows plus appended region rows.
pub(crate) fn build_ir_storage_init_adaptive<P: Clone>(
    blocks_in: &IRBlocks<P>,
    dedup: &DedupTable<IrHandlerKey>,
    layout: &GlobalLayout,
    reg_alloc: &RegAlloc,
    bytecode_storage: StorageId,
    addr_ty: IRTypeId,
    ir_types: &[volar_ir::ir::IRType],
    split_plan: &AdaptiveSplitPlan,
) -> VirtStorageInit {
    let total_rows = split_plan.layout.total_rows;
    let outer = split_plan.layout.outer_block_count;
    let mut lanes: BTreeMap<LaneKey, Vec<Constant>> = BTreeMap::new();

    fill_ir_outer_rows(
        blocks_in,
        dedup,
        layout,
        reg_alloc,
        bytecode_storage,
        addr_ty,
        ir_types,
        0..outer,
        &mut lanes,
        total_rows,
    );

    let mut entries: Vec<BytecodeEntry> = dedup
        .per_block
        .iter()
        .enumerate()
        .map(|(block_id, (h, imm))| {
            let _ = compute_slot_values(
                &blocks_in.blocks[block_id],
                block_id,
                &layout.schemas[*h as usize],
                reg_alloc,
                ir_types,
            );
            BytecodeEntry::outer(*h, imm.consts.clone(), imm.targets.clone())
        })
        .collect();

    for region in &split_plan.layout.regions {
        match &region.kind {
            AppendedRegionKind::SharedCore { .. } => {
                for pc in region.pc_start..region.pc_end {
                    let pc = pc as usize;
                    lane_mut(&mut lanes, bytecode_storage, addr_ty, total_rows)[pc] =
                        const_u32(0);
                    entries.push(BytecodeEntry {
                        handler_idx: 0,
                        consts: Vec::new(),
                        targets: Vec::new(),
                        row_kind: BytecodeRowKind::SharedCoreStep,
                    });
                }
            }
            AppendedRegionKind::RerollLoop {
                trip_count,
                body_handler_idx,
                ..
            } => {
                let pc = region.pc_start as usize;
                lane_mut(&mut lanes, bytecode_storage, addr_ty, total_rows)[pc] =
                    const_u32(*body_handler_idx);
                let mut consts = Vec::new();
                if let crate::bytecode::TripCount::Fixed(k) = trip_count {
                    consts.push(const_u32(*k));
                }
                entries.push(BytecodeEntry {
                    handler_idx: *body_handler_idx,
                    consts,
                    targets: Vec::new(),
                    row_kind: BytecodeRowKind::RerollDescriptor,
                });
            }
        }
    }

    let handler_schemas: Vec<HandlerImmSchema> = dedup
        .handler_keys
        .iter()
        .map(|k| HandlerImmSchema {
            kinds: k.immediate_schema(),
        })
        .collect();

    VirtStorageInit {
        pre_init: lanes_to_pre_init(lanes),
        bytecode: VirtBytecode {
            n_handlers: handler_schemas.len(),
            handler_schemas,
            entries,
            outer_block_count: outer,
            regions: split_plan.layout.regions.clone(),
        },
    }
}

#[allow(clippy::too_many_arguments)]
fn fill_ir_outer_rows<P: Clone>(
    blocks_in: &IRBlocks<P>,
    dedup: &DedupTable<IrHandlerKey>,
    layout: &GlobalLayout,
    reg_alloc: &RegAlloc,
    bytecode_storage: StorageId,
    addr_ty: IRTypeId,
    ir_types: &[volar_ir::ir::IRType],
    pcs: core::ops::Range<usize>,
    lanes: &mut BTreeMap<LaneKey, Vec<Constant>>,
    total_rows: usize,
) {
    for block_id in pcs {
        let block = &blocks_in.blocks[block_id];
        let (handler_idx, _) = &dedup.per_block[block_id];
        let h = *handler_idx as usize;
        let schema = &layout.schemas[h];
        let slot_ids = &layout.per_handler_slot[h];
        let slot_values = compute_slot_values(block, block_id, schema, reg_alloc, ir_types);

        lane_mut(lanes, bytecode_storage, addr_ty, total_rows)[block_id] =
            const_u32(*handler_idx);
        for (slot_idx, slot) in schema.slots.iter().enumerate() {
            lane_mut(lanes, slot_ids[slot_idx], slot.ty, total_rows)[block_id] =
                slot_values[slot_idx];
        }
    }
}

/// Build BIR bit-stuffed storage lanes and matching [`VirtBytecode`].
pub(crate) fn build_bir_storage_init(
    dedup: &DedupTable<BirHandlerKey>,
    per_handler_slots: &[Vec<StorageId>],
    bytecode_storage: StorageId,
    handler_bits: usize,
    pc_bits: usize,
) -> VirtStorageInit {
    let total_rows = dedup.per_block.len();
    let mut lanes: BTreeMap<LaneKey, Vec<Constant>> = BTreeMap::new();

    for (pc, (h_idx, imm)) in dedup.per_block.iter().enumerate() {
        for k in 0..handler_bits {
            let bit = (*h_idx as usize >> k) & 1;
            lane_mut(
                &mut lanes,
                StorageId(bytecode_storage.0 + k as u32),
                BIR_PREINIT_BIT_TY,
                total_rows,
            )[pc] = const_bit(bit == 1);
        }

        let slots = &per_handler_slots[*h_idx as usize];
        for (slot_idx, tgt) in imm.targets.iter().enumerate() {
            let slot_base = slots[slot_idx];
            for k in 0..pc_bits {
                let bit = (tgt.0 as usize >> k) & 1;
                lane_mut(
                    &mut lanes,
                    StorageId(slot_base.0 + k as u32),
                    BIR_PREINIT_BIT_TY,
                    total_rows,
                )[pc] = const_bit(bit == 1);
            }
        }
    }

    VirtStorageInit {
        pre_init: lanes_to_pre_init(lanes),
        bytecode: virt_bytecode_from_dedup(dedup),
    }
}
