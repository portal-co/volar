// @reliability: experimental
// @ai: assisted
//! Virtualisation entry point for [`IRBlocks`] with a register-based
//! calling convention.
//!
//! Unlike a classical SSA-to-bytecode VM, this pass keeps each original
//! block as a single "instruction" (one handler) but routes all
//! cross-block values through a per-type *register file* stored in
//! dedicated [`StorageId`]s.  Each bytecode row encodes:
//!
//! * `handler_idx` — which handler runs at this pc;
//! * one `_32` slot per block-param register index (where the handler
//!   reads its inputs);
//! * one typed slot per lifted `Stmt::Const` value;
//! * one `_32` slot per terminator-target's `next_pc`;
//! * one `Bit` slot per terminator-target's `done` flag (1 iff the
//!   target is `Jmp(Return)`);
//! * one `_32` slot per terminator-arg destination register (where the
//!   handler writes into the *target block's* param registers, or into
//!   the *return registers* for a `Return` target).
//!
//! At runtime the dispatcher is a tiny interpreter loop that fetches
//! `handler_idx` at the current pc, dispatches via
//! [`IRTerminator::JumpTable`] to the appropriate handler, then receives
//! `(next_pc, done)` back.  When `done == 1` the dispatcher reads the
//! return registers and exits via `Jmp(Return)`.

use alloc::{collections::BTreeMap, vec, vec::Vec};

use volar_ir::ir::{
    IRBlock, IRBlockId, IRBlockTargetId, IRBlocks, IRStmt, IRTerminator, IRType, IRTypeId, IRTypes,
    IRVarId,
};
use volar_ir_common::{Constant, Stmt, StorageId, Type as PrimType};

use crate::canon::{canonicalize_ir_block, BlockImmediates, IrHandlerKey};
use crate::ctx::{DedupTable, VirtOutput};
use crate::{DedupPolicy, DispatchMode, VirtualizeConfig};

// ============================================================================
// Public API
// ============================================================================

/// Virtualise an [`IRBlocks`] module.
///
/// Block parameters may vary across the input — each block reads its
/// parameters from a per-type register file whose indices are encoded
/// in the bytecode.  `Jmp(Return, args)` terminators are honoured as a
/// special "exit" branch via a `done` flag in the bytecode row; `args`
/// are written into dedicated return registers that the dispatcher
/// reads before exiting.
///
/// # Preconditions
/// * The input must have at least one block.
/// * No terminator may target `IRBlockTargetId::Dyn` in v1.
/// * All `Jmp(Return, args)` terminators must agree on the return arg
///   type list (which defines the function's return shape).
/// * The input must not read or write any `StorageId` colliding with
///   the bytecode storage or the per-type register-file storages
///   (allocated at `StorageId::VIRT_REGISTERS_BASE..`).
pub fn virtualize_ir<P: Clone + Default>(
    blocks: &IRBlocks<P>,
    types: &mut IRTypes,
    cfg: &VirtualizeConfig,
) -> VirtOutput<IRBlocks<P>> {
    assert!(
        matches!(cfg.dedup, DedupPolicy::ConstantsAndTargets),
        "volar-ir-virt: only DedupPolicy::ConstantsAndTargets is implemented"
    );
    assert!(
        !blocks.blocks.is_empty(),
        "virtualize_ir: input has no blocks"
    );

    let blocks_in = blocks.blocks.len();
    validate_input(blocks);

    // Canonicalise every block — lifts Const values and terminator
    // block-target ids into BlockImmediates.
    let per_block_canon: Vec<(IrHandlerKey, BlockImmediates)> =
        blocks.blocks.iter().map(canonicalize_ir_block).collect();
    let dedup = DedupTable::build(per_block_canon);
    let n_handlers = dedup.n_handlers();

    let addr_ty = types.intern(IRType::Primitive(PrimType::_32));
    let bit_ty = types.intern(IRType::Primitive(PrimType::Bit));

    // Compute the bytecode layout *first* so we know which StorageIds
    // the bytecode range occupies; the register file is placed strictly
    // *above* that range to avoid collisions.
    let layout = GlobalLayout::from_dedup(&dedup, addr_ty, bit_ty, cfg.bytecode_storage);
    let reg_storage_base = layout.next_free_storage_after_bytecode(cfg.bytecode_storage);

    // Allocate per-type register storages starting at reg_storage_base.
    let reg_alloc = RegAlloc::build(blocks, reg_storage_base);

    // Emit the module using the pre-computed layout.
    let out_blocks =
        emit_output_ir(blocks, &dedup, &layout, &reg_alloc, addr_ty, bit_ty, cfg);

    let bytecode = if cfg.bytecode_form.wants_external() {
        Some(dedup.to_bytecode())
    } else {
        None
    };

    // Oblivious dispatch runs movfuscate_ir over the Public output.
    // Note: movfuscate_ir does not yet support JumpTable terminators;
    // that path remains a deferred upstream task.
    let final_blocks = match cfg.dispatch {
        DispatchMode::Public => out_blocks,
        DispatchMode::Oblivious => volar_ir_passes::movfuscate_ir(&out_blocks, types),
    };

    VirtOutput {
        blocks: final_blocks,
        bytecode,
        n_handlers,
        blocks_in,
    }
}

fn validate_input<P: Clone + Default>(blocks: &IRBlocks<P>) {
    for (i, b) in blocks.blocks.iter().enumerate() {
        let check_target = |t: &IRBlockTargetId| {
            assert!(
                !matches!(t, IRBlockTargetId::Dyn(_)),
                "virtualize_ir: block {} uses Dyn jump target (unsupported)",
                i
            );
        };
        match &b.terminator {
            IRTerminator::Jmp { func, .. } => check_target(func),
            IRTerminator::JumpCond {
                true_block,
                false_block,
                ..
            } => {
                check_target(true_block);
                check_target(false_block);
            }
            IRTerminator::JumpTable { cases, .. } => {
                for (_, (t, _)) in cases {
                    check_target(t);
                }
            }
        }
    }
}

// ============================================================================
// Small helper for building blocks incrementally
// ============================================================================

struct IRBlockUnfinished {
    params: Vec<IRTypeId>,
    stmts: Vec<IRStmt>,
    terminator: IRTerminator,
}

impl IRBlockUnfinished {
    fn new(params: Vec<IRTypeId>) -> Self {
        Self {
            params,
            stmts: Vec::new(),
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: Vec::new(),
            },
        }
    }

    fn push(&mut self, s: IRStmt) -> IRVarId {
        let id = IRVarId(self.params.len() as u32 + self.stmts.len() as u32);
        self.stmts.push(s);
        id
    }

    fn into_ir_block<P: Clone + Default>(self) -> IRBlock<P> {
        let n = self.stmts.len();
        IRBlock {
            params: self.params,
            stmts: self.stmts,
            stmt_provs: vec![P::default(); n],
            terminator: self.terminator,
        }
    }
}

fn const_u32(x: u32) -> Constant {
    Constant {
        hi: 0,
        lo: x as u128,
    }
}

fn const_bit(b: bool) -> Constant {
    Constant {
        hi: 0,
        lo: if b { 1 } else { 0 },
    }
}

// ============================================================================
// Register allocation
// ============================================================================

/// The assignment of block params and return args to typed register
/// cells.
///
/// Each type `T` that appears as a block-param type or as a return-arg
/// type is given its own [`StorageId`] — register index is the address
/// within that storage.
///
/// Register numbering (v1):
///   * Per block `B`, per type `T`, B's params of type T are numbered
///     `0..n_T_params_of_B` within the block's own param list.
///     Because each block reads its params at entry (before any
///     writes), different blocks can reuse the same low indices.
///   * Return registers live above `max_T_params` for their type:
///     `return_reg(i, T) = max_T_params + i_th_position_of_type_T`.
struct RegAlloc {
    /// Per original block: for each param slot, the assigned register.
    per_block_params: Vec<Vec<RegRef>>,
    /// Return shape: for each return arg position `i`, the assigned
    /// register.
    return_regs: Vec<RegRef>,
    /// Dedicated [`StorageId`] for each type's register file.
    storage_per_type: BTreeMap<IRTypeId, StorageId>,
}

#[derive(Clone, Copy, Debug)]
struct RegRef {
    ty: IRTypeId,
    idx: u32,
}

impl RegAlloc {
    fn build<P: Clone + Default>(blocks: &IRBlocks<P>, storage_base: u32) -> Self {
        let mut per_block_params: Vec<Vec<RegRef>> = Vec::with_capacity(blocks.blocks.len());
        let mut max_param_idx: BTreeMap<IRTypeId, u32> = BTreeMap::new();

        for b in &blocks.blocks {
            let mut within_block: BTreeMap<IRTypeId, u32> = BTreeMap::new();
            let mut row: Vec<RegRef> = Vec::with_capacity(b.params.len());
            for &ty in &b.params {
                let idx = within_block.entry(ty).or_insert(0);
                let reg = RegRef { ty, idx: *idx };
                row.push(reg);
                let max = max_param_idx.entry(ty).or_insert(0);
                *max = (*max).max(*idx + 1);
                *idx += 1;
            }
            per_block_params.push(row);
        }

        // Extract return shape.
        let return_arg_tys = extract_return_shape(blocks);

        // Allocate return registers above the param range.
        let mut return_regs: Vec<RegRef> = Vec::with_capacity(return_arg_tys.len());
        {
            let mut next_ret_idx: BTreeMap<IRTypeId, u32> = BTreeMap::new();
            for &ty in &return_arg_tys {
                let base = max_param_idx.get(&ty).copied().unwrap_or(0);
                let off = next_ret_idx.entry(ty).or_insert(0);
                return_regs.push(RegRef {
                    ty,
                    idx: base + *off,
                });
                *off += 1;
            }
        }

        // Assign a StorageId to each type used.  The caller passes in
        // a base above the bytecode range so the two never collide.
        let mut storage_per_type: BTreeMap<IRTypeId, StorageId> = BTreeMap::new();
        {
            let mut next_sid = storage_base;
            for &ty in max_param_idx.keys() {
                storage_per_type.insert(ty, StorageId(next_sid));
                next_sid += 1;
            }
            for &ty in &return_arg_tys {
                if !storage_per_type.contains_key(&ty) {
                    storage_per_type.insert(ty, StorageId(next_sid));
                    next_sid += 1;
                }
            }
        }

        RegAlloc {
            per_block_params,
            return_regs,
            storage_per_type,
        }
    }

    fn storage_for(&self, ty: IRTypeId) -> StorageId {
        *self
            .storage_per_type
            .get(&ty)
            .expect("RegAlloc: no storage for type (missing from allocation)")
    }
}

/// Walk all `Jmp(Return, args)` terminators and derive the function's
/// return type list.  Panics if different Return terminators disagree.
fn extract_return_shape<P: Clone + Default>(blocks: &IRBlocks<P>) -> Vec<IRTypeId> {
    let mut shape: Option<Vec<IRTypeId>> = None;
    let mut record = |args: &[IRVarId], from_block: &IRBlock<P>| {
        let arg_tys: Vec<IRTypeId> = args
            .iter()
            .map(|v| resolve_var_type(from_block, *v))
            .collect();
        if let Some(existing) = &shape {
            assert_eq!(
                existing, &arg_tys,
                "virtualize_ir: Jmp(Return) arg types {:?} differ from \
                 earlier Return shape {:?}",
                arg_tys, existing
            );
        } else {
            shape = Some(arg_tys);
        }
    };

    for b in &blocks.blocks {
        match &b.terminator {
            IRTerminator::Jmp { func, args } if matches!(func, IRBlockTargetId::Return) => {
                record(args, b);
            }
            IRTerminator::JumpCond {
                true_block,
                true_args,
                false_block,
                false_args,
                ..
            } => {
                if matches!(true_block, IRBlockTargetId::Return) {
                    record(true_args, b);
                }
                if matches!(false_block, IRBlockTargetId::Return) {
                    record(false_args, b);
                }
            }
            IRTerminator::JumpTable { cases, .. } => {
                for (_, (t, a)) in cases {
                    if matches!(t, IRBlockTargetId::Return) {
                        record(a, b);
                    }
                }
            }
            _ => {}
        }
    }

    shape.unwrap_or_default()
}

/// Look up the IR type of a variable id within a block.
fn resolve_var_type<P: Clone + Default>(block: &IRBlock<P>, v: IRVarId) -> IRTypeId {
    let n_params = block.params.len() as u32;
    if v.0 < n_params {
        block.params[v.0 as usize]
    } else {
        let s_idx = (v.0 - n_params) as usize;
        let stmt = &block.stmts[s_idx];
        stmt_output_type(stmt).expect(
            "virtualize_ir: Return arg refers to a stmt that does not \
             define a value",
        )
    }
}

fn stmt_output_type(s: &IRStmt) -> Option<IRTypeId> {
    match s {
        Stmt::Const(_, ty) => Some(*ty),
        Stmt::StorageRead { ty, .. } => Some(*ty),
        Stmt::StorageWrite { .. } => None,
        Stmt::Transmute { dst_ty, .. } => Some(*dst_ty),
        Stmt::Poly { ty, .. } => Some(*ty),
        Stmt::Rol { ty, .. } => Some(*ty),
        Stmt::Ror { ty, .. } => Some(*ty),
        Stmt::Merge { ty, .. } => Some(*ty),
        Stmt::Splat { ty, .. } => Some(*ty),
        Stmt::Shuffle { ty, .. } => Some(*ty),
        Stmt::OracleCall { result_ty, .. } => Some(*result_ty),
        Stmt::OracleOutput { ty, .. } => Some(*ty),
        Stmt::ActionCall { result_ty, .. } => Some(*result_ty),
        Stmt::ActionOutput { ty, .. } => Some(*ty),
        Stmt::Rng { ty, .. } => Some(*ty),
    }
}

// ============================================================================
// Bytecode schema (per-handler slot plan)
// ============================================================================

/// Semantic kind of a slot.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum SlotKind {
    /// Lifted `Stmt::Const` value.  The slot's type is the const's IR
    /// type.
    ConstValue,
    /// Register index this block reads a param from (`_32`).
    ParamSrcReg,
    /// Register index an arm writes an arg into — either a target
    /// block's param register or a return register (`_32`).
    ArgDstReg,
    /// Target pc for an arm (`_32`).  Unused / arbitrary for Return arms.
    TargetPc,
    /// Done flag for an arm (`Bit`); 1 iff the arm targets `Return`.
    DoneFlag,
}

#[derive(Clone, Debug)]
struct HandlerSlot {
    /// Semantic kind of the slot.  Currently only used by setup-time
    /// computation of slot values — the handler body looks each slot
    /// up by index via the per-slot `StorageId`.
    #[allow(dead_code)]
    kind: SlotKind,
    /// IR type of the stored value.
    ty: IRTypeId,
}

#[derive(Clone, Debug, Default)]
struct HandlerSchema {
    slots: Vec<HandlerSlot>,
    /// Slot index for each block-param source register.
    param_src_slot: Vec<usize>,
    /// Slot index for each lifted `Stmt::Const`; `None` for other stmts.
    const_value_slot: Vec<Option<usize>>,
    /// Per-arm: slot indices for next_pc, done, arg_dst_regs.
    arms: Vec<ArmSchema>,
}

#[derive(Clone, Debug)]
struct ArmSchema {
    next_pc_slot: usize,
    done_slot: usize,
    arg_dst_slots: Vec<usize>,
}

impl HandlerSchema {
    fn build(key: &IrHandlerKey, addr_ty: IRTypeId, bit_ty: IRTypeId) -> Self {
        let mut schema = HandlerSchema::default();

        let mut param_src_slot = Vec::with_capacity(key.params.len());
        for _ in &key.params {
            param_src_slot.push(push_slot(&mut schema.slots, SlotKind::ParamSrcReg, addr_ty));
        }
        schema.param_src_slot = param_src_slot;

        let mut const_value_slot = Vec::with_capacity(key.stmts.len());
        for s in &key.stmts {
            if let Stmt::Const(_, ty) = s {
                const_value_slot.push(Some(push_slot(
                    &mut schema.slots,
                    SlotKind::ConstValue,
                    *ty,
                )));
            } else {
                const_value_slot.push(None);
            }
        }
        schema.const_value_slot = const_value_slot;

        let mut arms: Vec<ArmSchema> = Vec::new();
        for n_args in terminator_arm_shape(&key.terminator) {
            let next_pc_slot = push_slot(&mut schema.slots, SlotKind::TargetPc, addr_ty);
            let done_slot = push_slot(&mut schema.slots, SlotKind::DoneFlag, bit_ty);
            let mut arg_dst_slots = Vec::with_capacity(n_args);
            for _ in 0..n_args {
                arg_dst_slots.push(push_slot(&mut schema.slots, SlotKind::ArgDstReg, addr_ty));
            }
            arms.push(ArmSchema {
                next_pc_slot,
                done_slot,
                arg_dst_slots,
            });
        }
        schema.arms = arms;

        schema
    }
}

fn push_slot(slots: &mut Vec<HandlerSlot>, kind: SlotKind, ty: IRTypeId) -> usize {
    let idx = slots.len();
    slots.push(HandlerSlot { kind, ty });
    idx
}

fn terminator_arm_shape(t: &IRTerminator) -> Vec<usize> {
    match t {
        IRTerminator::Jmp { args, .. } => vec![args.len()],
        IRTerminator::JumpCond {
            true_args,
            false_args,
            ..
        } => vec![true_args.len(), false_args.len()],
        IRTerminator::JumpTable { cases, .. } => {
            cases.values().map(|(_, a)| a.len()).collect()
        }
    }
}

// ============================================================================
// Global slot layout (one StorageId per global slot id)
// ============================================================================

struct GlobalLayout {
    /// `per_handler_slot[h][s] = absolute StorageId for handler h's s-th slot`.
    per_handler_slot: Vec<Vec<StorageId>>,
    schemas: Vec<HandlerSchema>,
}

impl GlobalLayout {
    fn from_dedup(
        dedup: &DedupTable<IrHandlerKey>,
        addr_ty: IRTypeId,
        bit_ty: IRTypeId,
        base: StorageId,
    ) -> Self {
        let mut schemas: Vec<HandlerSchema> = Vec::with_capacity(dedup.handler_keys.len());
        for key in &dedup.handler_keys {
            schemas.push(HandlerSchema::build(key, addr_ty, bit_ty));
        }

        let mut per_handler_slot: Vec<Vec<StorageId>> = Vec::with_capacity(schemas.len());
        // StorageId(base.0) stores handler_idx; per-slot storages
        // start at base.0 + 1.
        let mut next = base.0 + 1;
        for schema in &schemas {
            let mut ids = Vec::with_capacity(schema.slots.len());
            for _ in &schema.slots {
                ids.push(StorageId(next));
                next += 1;
            }
            per_handler_slot.push(ids);
        }

        GlobalLayout {
            per_handler_slot,
            schemas,
        }
    }

    /// Return the first `StorageId` strictly above the bytecode range.
    /// The register file is placed here so the two never collide.
    fn next_free_storage_after_bytecode(&self, base: StorageId) -> u32 {
        let total_slots: u32 = self
            .per_handler_slot
            .iter()
            .map(|row| row.len() as u32)
            .sum();
        // +1 because base.0 itself is the handler_idx slot.
        base.0 + 1 + total_slots
    }
}

// ============================================================================
// Output IR emission
// ============================================================================

/// Block-id constants for the rewritten module.
const SETUP_BID: u32 = 0;
const DISPATCHER_BID: u32 = 1;
const RETURN_BID: u32 = 2;
const DISPATCH_BID: u32 = 3;
const HANDLER_BID_BASE: u32 = 4;

fn emit_output_ir<P: Clone + Default>(
    blocks_in: &IRBlocks<P>,
    dedup: &DedupTable<IrHandlerKey>,
    layout: &GlobalLayout,
    reg_alloc: &RegAlloc,
    addr_ty: IRTypeId,
    bit_ty: IRTypeId,
    cfg: &VirtualizeConfig,
) -> IRBlocks<P> {
    let entry_params = blocks_in.blocks[0].params.clone();
    let return_arg_tys: Vec<IRTypeId> = reg_alloc.return_regs.iter().map(|r| r.ty).collect();

    let n_handlers = dedup.n_handlers();
    let mut extra_subblocks: Vec<IRBlock<P>> = Vec::new();
    let mut next_sub_bid = HANDLER_BID_BASE + n_handlers as u32;

    // --- setup ---
    let setup = emit_setup_block(
        blocks_in,
        &entry_params,
        dedup,
        layout,
        reg_alloc,
        addr_ty,
        bit_ty,
        cfg,
    );

    // --- dispatcher ---
    let dispatcher = emit_dispatcher_block(addr_ty, bit_ty);

    // --- return ---
    let return_block = emit_return_block(reg_alloc, &return_arg_tys, addr_ty);

    // --- dispatch ---
    let dispatch = emit_dispatch_block(dedup, cfg.bytecode_storage, addr_ty);

    // --- handlers (+ arm sub-blocks) ---
    let mut handler_blocks: Vec<IRBlock<P>> = Vec::with_capacity(n_handlers);
    for (h_idx, key) in dedup.handler_keys.iter().enumerate() {
        let schema = &layout.schemas[h_idx];
        let slot_ids = &layout.per_handler_slot[h_idx];
        let (handler, extras) = emit_handler_block::<P>(
            key,
            schema,
            slot_ids,
            reg_alloc,
            addr_ty,
            bit_ty,
            &mut next_sub_bid,
        );
        handler_blocks.push(handler);
        extra_subblocks.extend(extras);
    }

    // Assemble.
    let mut all_blocks: Vec<IRBlock<P>> = Vec::with_capacity(4 + n_handlers + extra_subblocks.len());
    all_blocks.push(setup);
    all_blocks.push(dispatcher);
    all_blocks.push(return_block);
    all_blocks.push(dispatch);
    all_blocks.extend(handler_blocks);
    all_blocks.extend(extra_subblocks);

    IRBlocks {
        oracles: blocks_in.oracles.clone(),
        actions: blocks_in.actions.clone(),
        rngs: blocks_in.rngs.clone(),
        blocks: all_blocks,
    }
}

// ----------------------------------------------------------------------------
// Setup block (block 0)
// ----------------------------------------------------------------------------

fn emit_setup_block<P: Clone + Default>(
    blocks_in: &IRBlocks<P>,
    entry_params: &[IRTypeId],
    dedup: &DedupTable<IrHandlerKey>,
    layout: &GlobalLayout,
    reg_alloc: &RegAlloc,
    addr_ty: IRTypeId,
    bit_ty: IRTypeId,
    cfg: &VirtualizeConfig,
) -> IRBlock<P> {
    let mut b = IRBlockUnfinished::new(entry_params.to_vec());

    // 1. Write the entry block's params into their assigned registers.
    let entry_regs = &reg_alloc.per_block_params[0];
    assert_eq!(
        entry_regs.len(),
        entry_params.len(),
        "setup: entry block param arity mismatch"
    );
    for (param_idx, reg) in entry_regs.iter().enumerate() {
        let addr = b.push(Stmt::Const(const_u32(reg.idx), addr_ty));
        b.push(Stmt::StorageWrite {
            storage: reg_alloc.storage_for(reg.ty),
            ty: reg.ty,
            addr,
            src: IRVarId(param_idx as u32),
        });
    }

    // 2. InIr bytecode — write each row's handler_idx + slot values.
    if cfg.bytecode_form.wants_in_ir() {
        for (block_id, block) in blocks_in.blocks.iter().enumerate() {
            let (h_idx, _imm) = &dedup.per_block[block_id];
            let pc_const = b.push(Stmt::Const(const_u32(block_id as u32), addr_ty));
            let hidx_const = b.push(Stmt::Const(const_u32(*h_idx), addr_ty));
            b.push(Stmt::StorageWrite {
                storage: cfg.bytecode_storage,
                ty: addr_ty,
                addr: pc_const,
                src: hidx_const,
            });

            let schema = &layout.schemas[*h_idx as usize];
            let slot_ids = &layout.per_handler_slot[*h_idx as usize];
            let slot_values = compute_slot_values(block, block_id, schema, reg_alloc);
            assert_eq!(slot_values.len(), schema.slots.len());

            for (slot_idx, slot) in schema.slots.iter().enumerate() {
                let storage = slot_ids[slot_idx];
                let val_const = b.push(Stmt::Const(slot_values[slot_idx], slot.ty));
                b.push(Stmt::StorageWrite {
                    storage,
                    ty: slot.ty,
                    addr: pc_const,
                    src: val_const,
                });
            }
        }
    }

    // 3. pc=0, done=0, jump to dispatcher.
    let pc = b.push(Stmt::Const(const_u32(0), addr_ty));
    let done = b.push(Stmt::Const(const_bit(false), bit_ty));

    b.terminator = IRTerminator::Jmp {
        func: IRBlockTargetId::Block(IRBlockId(DISPATCHER_BID)),
        args: vec![pc, done],
    };
    b.into_ir_block::<P>()
}

/// Compute the concrete Constant value for each slot of a handler
/// schema, instantiated for a specific original block.
fn compute_slot_values<P: Clone + Default>(
    block: &IRBlock<P>,
    block_id: usize,
    schema: &HandlerSchema,
    reg_alloc: &RegAlloc,
) -> Vec<Constant> {
    let mut out: Vec<Constant> = vec![const_u32(0); schema.slots.len()];

    // Param source registers.
    let my_regs = &reg_alloc.per_block_params[block_id];
    assert_eq!(my_regs.len(), schema.param_src_slot.len());
    for (i, slot_idx) in schema.param_src_slot.iter().enumerate() {
        out[*slot_idx] = const_u32(my_regs[i].idx);
    }

    // Const value slots.
    for (stmt_idx, slot_opt) in schema.const_value_slot.iter().enumerate() {
        if let Some(slot_idx) = slot_opt {
            match &block.stmts[stmt_idx] {
                Stmt::Const(c, _) => {
                    out[*slot_idx] = *c;
                }
                _ => panic!(
                    "compute_slot_values: schema says stmt {} is Const \
                     but source block disagrees",
                    stmt_idx
                ),
            }
        }
    }

    // Terminator arm slots.
    fill_terminator_slots(block, schema, reg_alloc, &mut out);

    out
}

fn fill_terminator_slots<P: Clone + Default>(
    block: &IRBlock<P>,
    schema: &HandlerSchema,
    reg_alloc: &RegAlloc,
    out: &mut [Constant],
) {
    let fill_arm = |out: &mut [Constant],
                    arm: &ArmSchema,
                    target: &IRBlockTargetId,
                    args: &[IRVarId]| {
        let (next_pc, done) = match target {
            IRBlockTargetId::Block(bid) => (bid.0, false),
            IRBlockTargetId::Return => (0u32, true),
            IRBlockTargetId::Dyn(_) => panic!("fill_terminator_slots: Dyn"),
        };
        out[arm.next_pc_slot] = const_u32(next_pc);
        out[arm.done_slot] = const_bit(done);
        assert_eq!(arm.arg_dst_slots.len(), args.len());
        for (i, _arg) in args.iter().enumerate() {
            let dst_reg = match target {
                IRBlockTargetId::Block(target_bid) => {
                    reg_alloc.per_block_params[target_bid.0 as usize][i]
                }
                IRBlockTargetId::Return => reg_alloc.return_regs[i],
                IRBlockTargetId::Dyn(_) => unreachable!(),
            };
            out[arm.arg_dst_slots[i]] = const_u32(dst_reg.idx);
        }
    };

    match &block.terminator {
        IRTerminator::Jmp { func, args } => {
            assert_eq!(schema.arms.len(), 1);
            fill_arm(out, &schema.arms[0], func, args);
        }
        IRTerminator::JumpCond {
            true_block,
            true_args,
            false_block,
            false_args,
            ..
        } => {
            assert_eq!(schema.arms.len(), 2);
            fill_arm(out, &schema.arms[0], true_block, true_args);
            fill_arm(out, &schema.arms[1], false_block, false_args);
        }
        IRTerminator::JumpTable { cases, .. } => {
            assert_eq!(schema.arms.len(), cases.len());
            for (arm_idx, (_, (t, a))) in cases.iter().enumerate() {
                fill_arm(out, &schema.arms[arm_idx], t, a);
            }
        }
    }
}

// ----------------------------------------------------------------------------
// Dispatcher / Return / Dispatch blocks
// ----------------------------------------------------------------------------

fn emit_dispatcher_block<P: Clone + Default>(
    addr_ty: IRTypeId,
    bit_ty: IRTypeId,
) -> IRBlock<P> {
    let b = IRBlockUnfinished {
        params: vec![addr_ty, bit_ty],
        stmts: Vec::new(),
        terminator: IRTerminator::JumpCond {
            condition: IRVarId(1),
            true_block: IRBlockTargetId::Block(IRBlockId(RETURN_BID)),
            true_args: vec![],
            false_block: IRBlockTargetId::Block(IRBlockId(DISPATCH_BID)),
            false_args: vec![IRVarId(0)],
        },
    };
    b.into_ir_block::<P>()
}

fn emit_return_block<P: Clone + Default>(
    reg_alloc: &RegAlloc,
    return_arg_tys: &[IRTypeId],
    addr_ty: IRTypeId,
) -> IRBlock<P> {
    let mut b = IRBlockUnfinished::new(vec![]);
    let mut val_vars: Vec<IRVarId> = Vec::with_capacity(return_arg_tys.len());
    for (i, &ty) in return_arg_tys.iter().enumerate() {
        let addr = b.push(Stmt::Const(const_u32(reg_alloc.return_regs[i].idx), addr_ty));
        let val = b.push(Stmt::StorageRead {
            storage: reg_alloc.storage_for(ty),
            ty,
            addr,
        });
        val_vars.push(val);
    }
    b.terminator = IRTerminator::Jmp {
        func: IRBlockTargetId::Return,
        args: val_vars,
    };
    b.into_ir_block::<P>()
}

fn emit_dispatch_block<P: Clone + Default>(
    dedup: &DedupTable<IrHandlerKey>,
    bytecode_storage: StorageId,
    addr_ty: IRTypeId,
) -> IRBlock<P> {
    let mut b = IRBlockUnfinished::new(vec![addr_ty]);
    let handler_idx = b.push(Stmt::StorageRead {
        storage: bytecode_storage,
        ty: addr_ty,
        addr: IRVarId(0),
    });
    let mut cases: BTreeMap<Constant, (IRBlockTargetId, Vec<IRVarId>)> = BTreeMap::new();
    for (h_idx, _) in dedup.handler_keys.iter().enumerate() {
        cases.insert(
            const_u32(h_idx as u32),
            (
                IRBlockTargetId::Block(IRBlockId(HANDLER_BID_BASE + h_idx as u32)),
                vec![IRVarId(0)],
            ),
        );
    }
    b.terminator = IRTerminator::JumpTable {
        index: handler_idx,
        cases,
    };
    b.into_ir_block::<P>()
}

// ----------------------------------------------------------------------------
// Handler blocks (+ sub-blocks for conditional arms)
// ----------------------------------------------------------------------------

/// Emit a handler block for a canonical key.  May emit additional
/// sub-blocks, one per arm of a conditional terminator.
fn emit_handler_block<P: Clone + Default>(
    key: &IrHandlerKey,
    schema: &HandlerSchema,
    slot_ids: &[StorageId],
    reg_alloc: &RegAlloc,
    addr_ty: IRTypeId,
    bit_ty: IRTypeId,
    next_sub_bid: &mut u32,
) -> (IRBlock<P>, Vec<IRBlock<P>>) {
    let mut b = IRBlockUnfinished::new(vec![addr_ty]);
    let pc = IRVarId(0);

    // Canonical-to-new variable map.  Canonical var ids are:
    //   [0 .. n_params)                     — block params
    //   [n_params .. n_params + n_stmts)    — stmt outputs
    // We fill in params first (via register reads), then stmts (via
    // emission), and map each canonical id to the new IRVarId.
    let n_params = key.params.len();
    let n_stmts = key.stmts.len();
    let mut canonical_var: Vec<IRVarId> = vec![IRVarId(u32::MAX); n_params + n_stmts];

    // --- Read each block param from its register ---------------------
    for (param_idx, &ty) in key.params.iter().enumerate() {
        let slot_storage = slot_ids[schema.param_src_slot[param_idx]];
        let reg_idx_var = b.push(Stmt::StorageRead {
            storage: slot_storage,
            ty: addr_ty,
            addr: pc,
        });
        let val = b.push(Stmt::StorageRead {
            storage: reg_alloc.storage_for(ty),
            ty,
            addr: reg_idx_var,
        });
        canonical_var[param_idx] = val;
    }

    // --- Re-emit canonical stmts, remapping operands -----------------
    for (stmt_idx, s) in key.stmts.iter().enumerate() {
        let canonical_id = n_params + stmt_idx;
        match s {
            Stmt::Const(_, ty) => {
                let slot_storage = slot_ids[schema.const_value_slot[stmt_idx]
                    .expect("Const stmt must have a value slot")];
                let v = b.push(Stmt::StorageRead {
                    storage: slot_storage,
                    ty: *ty,
                    addr: pc,
                });
                canonical_var[canonical_id] = v;
            }
            other => {
                let remapped = remap_stmt(other, &canonical_var);
                if stmt_output_type(&remapped).is_some() {
                    let v = b.push(remapped);
                    canonical_var[canonical_id] = v;
                } else {
                    // StorageWrite — no output var, just push.
                    b.push(remapped);
                    canonical_var[canonical_id] = IRVarId(u32::MAX);
                }
            }
        }
    }

    // --- Terminator --------------------------------------------------
    let mut extras: Vec<IRBlock<P>> = Vec::new();

    // Compute the IR type of each canonical SSA id.  Used to type the
    // StorageWrite that forwards a terminator arg.  Because the
    // canonical terminator has its target block ids zeroed, we cannot
    // resolve arg types through the target; we read them from the
    // source var instead (which, by IR well-formedness, must equal the
    // dst register's type).
    let canon_types = canonical_types(key);

    let terminator = match &key.terminator {
        IRTerminator::Jmp { func, args } => {
            // Single-arm: emit writes inline.
            let arm = &schema.arms[0];
            emit_inline_arm_writes(
                &mut b,
                arm,
                slot_ids,
                reg_alloc,
                addr_ty,
                pc,
                args,
                &canonical_var,
                &canon_types,
            );
            let _ = func;
            build_return_to_dispatcher(&mut b, arm, slot_ids, addr_ty, bit_ty, pc)
        }
        IRTerminator::JumpCond {
            condition,
            true_block,
            true_args,
            false_block,
            false_args,
        } => {
            let cond_var = canonical_var[condition.0 as usize];
            let true_bid = *next_sub_bid;
            *next_sub_bid += 1;
            let false_bid = *next_sub_bid;
            *next_sub_bid += 1;

            let true_arg_tys: Vec<IRTypeId> =
                true_args.iter().map(|v| canon_types[v.0 as usize]).collect();
            let false_arg_tys: Vec<IRTypeId> =
                false_args.iter().map(|v| canon_types[v.0 as usize]).collect();

            extras.push(emit_arm_subblock::<P>(
                &schema.arms[0],
                slot_ids,
                reg_alloc,
                addr_ty,
                bit_ty,
                &true_arg_tys,
            ));
            extras.push(emit_arm_subblock::<P>(
                &schema.arms[1],
                slot_ids,
                reg_alloc,
                addr_ty,
                bit_ty,
                &false_arg_tys,
            ));

            let _ = (true_block, false_block);

            let true_call_args = core::iter::once(pc)
                .chain(true_args.iter().map(|v| canonical_var[v.0 as usize]))
                .collect::<Vec<_>>();
            let false_call_args = core::iter::once(pc)
                .chain(false_args.iter().map(|v| canonical_var[v.0 as usize]))
                .collect::<Vec<_>>();

            IRTerminator::JumpCond {
                condition: cond_var,
                true_block: IRBlockTargetId::Block(IRBlockId(true_bid)),
                true_args: true_call_args,
                false_block: IRBlockTargetId::Block(IRBlockId(false_bid)),
                false_args: false_call_args,
            }
        }
        IRTerminator::JumpTable { index, cases } => {
            let idx_var = canonical_var[index.0 as usize];
            let mut out_cases: BTreeMap<Constant, (IRBlockTargetId, Vec<IRVarId>)> =
                BTreeMap::new();
            for (arm_idx, (k, (_target, args))) in cases.iter().enumerate() {
                let sub_bid = *next_sub_bid;
                *next_sub_bid += 1;
                let arg_tys: Vec<IRTypeId> =
                    args.iter().map(|v| canon_types[v.0 as usize]).collect();
                extras.push(emit_arm_subblock::<P>(
                    &schema.arms[arm_idx],
                    slot_ids,
                    reg_alloc,
                    addr_ty,
                    bit_ty,
                    &arg_tys,
                ));
                let arm_args = core::iter::once(pc)
                    .chain(args.iter().map(|v| canonical_var[v.0 as usize]))
                    .collect::<Vec<_>>();
                out_cases.insert(*k, (IRBlockTargetId::Block(IRBlockId(sub_bid)), arm_args));
            }
            IRTerminator::JumpTable {
                index: idx_var,
                cases: out_cases,
            }
        }
    };

    b.terminator = terminator;
    (b.into_ir_block::<P>(), extras)
}

/// Inline arm-arg forwarding for the non-conditional (single-arm) case.
#[allow(clippy::too_many_arguments)]
fn emit_inline_arm_writes(
    b: &mut IRBlockUnfinished,
    arm: &ArmSchema,
    slot_ids: &[StorageId],
    reg_alloc: &RegAlloc,
    addr_ty: IRTypeId,
    pc: IRVarId,
    args: &[IRVarId],
    canonical_var: &[IRVarId],
    canon_types: &[IRTypeId],
) {
    for (i, canon_arg) in args.iter().enumerate() {
        let arg_val = canonical_var[canon_arg.0 as usize];
        let arg_ty = canon_types[canon_arg.0 as usize];
        let dst_reg_idx = b.push(Stmt::StorageRead {
            storage: slot_ids[arm.arg_dst_slots[i]],
            ty: addr_ty,
            addr: pc,
        });
        b.push(Stmt::StorageWrite {
            storage: reg_alloc.storage_for(arg_ty),
            ty: arg_ty,
            addr: dst_reg_idx,
            src: arg_val,
        });
    }
}

/// Build `Jmp(dispatcher, [next_pc, done])` for a single-arm terminator.
fn build_return_to_dispatcher(
    b: &mut IRBlockUnfinished,
    arm: &ArmSchema,
    slot_ids: &[StorageId],
    addr_ty: IRTypeId,
    bit_ty: IRTypeId,
    pc: IRVarId,
) -> IRTerminator {
    let next_pc = b.push(Stmt::StorageRead {
        storage: slot_ids[arm.next_pc_slot],
        ty: addr_ty,
        addr: pc,
    });
    let done = b.push(Stmt::StorageRead {
        storage: slot_ids[arm.done_slot],
        ty: bit_ty,
        addr: pc,
    });
    IRTerminator::Jmp {
        func: IRBlockTargetId::Block(IRBlockId(DISPATCHER_BID)),
        args: vec![next_pc, done],
    }
}

/// Sub-block emitted for each arm of a conditional terminator.
/// Params: `[pc: _32, arg_0: ty_0, ... arg_{n-1}: ty_{n-1}]`.
/// Body: writes each arg to its dst register (dst reg idx read from
/// bytecode at pc), then jumps to dispatcher with (next_pc, done) read
/// from bytecode.
#[allow(clippy::too_many_arguments)]
fn emit_arm_subblock<P: Clone + Default>(
    arm: &ArmSchema,
    slot_ids: &[StorageId],
    reg_alloc: &RegAlloc,
    addr_ty: IRTypeId,
    bit_ty: IRTypeId,
    arg_tys: &[IRTypeId],
) -> IRBlock<P> {
    let mut params: Vec<IRTypeId> = vec![addr_ty];
    params.extend_from_slice(arg_tys);
    let mut b = IRBlockUnfinished::new(params);
    let pc = IRVarId(0);

    for (i, &arg_ty) in arg_tys.iter().enumerate() {
        let arg_val = IRVarId(1 + i as u32);
        let dst_reg_idx = b.push(Stmt::StorageRead {
            storage: slot_ids[arm.arg_dst_slots[i]],
            ty: addr_ty,
            addr: pc,
        });
        b.push(Stmt::StorageWrite {
            storage: reg_alloc.storage_for(arg_ty),
            ty: arg_ty,
            addr: dst_reg_idx,
            src: arg_val,
        });
    }

    let next_pc = b.push(Stmt::StorageRead {
        storage: slot_ids[arm.next_pc_slot],
        ty: addr_ty,
        addr: pc,
    });
    let done = b.push(Stmt::StorageRead {
        storage: slot_ids[arm.done_slot],
        ty: bit_ty,
        addr: pc,
    });
    b.terminator = IRTerminator::Jmp {
        func: IRBlockTargetId::Block(IRBlockId(DISPATCHER_BID)),
        args: vec![next_pc, done],
    };
    b.into_ir_block::<P>()
}

/// Return the IR type of each canonical SSA id in the handler key:
/// `canon_types[0..n_params]` are block-param types, and
/// `canon_types[n_params..]` are stmt-output types (`IRTypeId::INVALID`
/// sentinel for stmts that don't define a value; those are never
/// referenced as an arg).
fn canonical_types(key: &IrHandlerKey) -> Vec<IRTypeId> {
    let mut out: Vec<IRTypeId> = Vec::with_capacity(key.params.len() + key.stmts.len());
    for &p in &key.params {
        out.push(p);
    }
    for s in &key.stmts {
        out.push(stmt_output_type(s).unwrap_or(IRTypeId(u32::MAX)));
    }
    out
}

// ============================================================================
// Stmt operand remapping
// ============================================================================

fn remap_var(v: IRVarId, canonical_var: &[IRVarId]) -> IRVarId {
    let idx = v.0 as usize;
    let mapped = canonical_var[idx];
    assert_ne!(
        mapped.0,
        u32::MAX,
        "remap_var: canonical var id {} was never defined",
        idx
    );
    mapped
}

fn remap_vars(vs: &[IRVarId], canonical_var: &[IRVarId]) -> Vec<IRVarId> {
    vs.iter().map(|v| remap_var(*v, canonical_var)).collect()
}

fn remap_coeffs(
    coeffs: &BTreeMap<Vec<IRVarId>, u8>,
    canonical_var: &[IRVarId],
) -> BTreeMap<Vec<IRVarId>, u8> {
    let mut out: BTreeMap<Vec<IRVarId>, u8> = BTreeMap::new();
    for (key, &c) in coeffs {
        let mut new_key: Vec<IRVarId> =
            key.iter().map(|v| remap_var(*v, canonical_var)).collect();
        new_key.sort();
        let entry = out.entry(new_key).or_insert(0);
        *entry ^= c;
    }
    out.retain(|_, c| *c != 0);
    out
}

fn remap_stmt(s: &IRStmt, canonical_var: &[IRVarId]) -> IRStmt {
    match s {
        Stmt::Const(c, ty) => Stmt::Const(*c, *ty),
        Stmt::StorageRead { storage, ty, addr } => Stmt::StorageRead {
            storage: *storage,
            ty: *ty,
            addr: remap_var(*addr, canonical_var),
        },
        Stmt::StorageWrite {
            storage,
            src,
            ty,
            addr,
        } => Stmt::StorageWrite {
            storage: *storage,
            src: remap_var(*src, canonical_var),
            ty: *ty,
            addr: remap_var(*addr, canonical_var),
        },
        Stmt::Transmute {
            src,
            src_ty,
            dst_ty,
        } => Stmt::Transmute {
            src: remap_var(*src, canonical_var),
            src_ty: *src_ty,
            dst_ty: *dst_ty,
        },
        Stmt::Poly {
            ty,
            coeffs,
            constant,
        } => Stmt::Poly {
            ty: *ty,
            coeffs: remap_coeffs(coeffs, canonical_var),
            constant: *constant,
        },
        Stmt::Rol { src, ty, n } => Stmt::Rol {
            src: remap_var(*src, canonical_var),
            ty: *ty,
            n: *n,
        },
        Stmt::Ror { src, ty, n } => Stmt::Ror {
            src: remap_var(*src, canonical_var),
            ty: *ty,
            n: *n,
        },
        Stmt::Merge { parts, ty } => Stmt::Merge {
            parts: remap_vars(parts, canonical_var),
            ty: *ty,
        },
        Stmt::Splat { src, ty } => Stmt::Splat {
            src: remap_var(*src, canonical_var),
            ty: *ty,
        },
        Stmt::Shuffle { result_bits, ty } => Stmt::Shuffle {
            result_bits: result_bits
                .iter()
                .map(|(bit, v)| (*bit, remap_var(*v, canonical_var)))
                .collect(),
            ty: *ty,
        },
        Stmt::OracleCall {
            name,
            args,
            output_tys,
            result_ty,
        } => Stmt::OracleCall {
            name: name.clone(),
            args: remap_vars(args, canonical_var),
            output_tys: output_tys.clone(),
            result_ty: *result_ty,
        },
        Stmt::OracleOutput { call, idx, ty } => Stmt::OracleOutput {
            call: remap_var(*call, canonical_var),
            idx: *idx,
            ty: *ty,
        },
        Stmt::ActionCall {
            name,
            guard,
            args,
            fallbacks,
            output_tys,
            result_ty,
        } => Stmt::ActionCall {
            name: name.clone(),
            guard: remap_var(*guard, canonical_var),
            args: remap_vars(args, canonical_var),
            fallbacks: remap_vars(fallbacks, canonical_var),
            output_tys: output_tys.clone(),
            result_ty: *result_ty,
        },
        Stmt::ActionOutput { call, idx, ty } => Stmt::ActionOutput {
            call: remap_var(*call, canonical_var),
            idx: *idx,
            ty: *ty,
        },
        Stmt::Rng { name, ty } => Stmt::Rng {
            name: name.clone(),
            ty: *ty,
        },
    }
}

// Silence the unused-SETUP_BID warning; kept for clarity in the
// documentation above.
#[allow(dead_code)]
const _SETUP_BID: u32 = SETUP_BID;
