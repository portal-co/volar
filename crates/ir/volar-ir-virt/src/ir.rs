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

use crate::canon::{canonicalize_ir_block, BlockImmediates, IrHandlerKey, ZERO_CONSTANT};
use crate::ctx::{DedupTable, VirtOutput};
use crate::hash::{bytes_to_constant, constant_to_le_bytes, CommitmentConfig, IrEmitter, IrHashAlgorithm};
use crate::{DedupPolicy, DispatchMode, VirtualizeConfig};

// ============================================================================
// Uninhabited sentinel used so `virtualize_ir` need not be generic.
// ============================================================================

/// Uninhabited type that satisfies `IrHashAlgorithm` but can never be
/// constructed.  Used as the `H` type parameter for the non-commitment path so
/// `virtualize_ir` does not need to carry a generic parameter.
enum NoOpHashAlgorithm {}

impl IrHashAlgorithm for NoOpHashAlgorithm {
    fn emit_ir(&self, _: &mut dyn IrEmitter, _: &[(IRVarId, IRTypeId)]) -> IRVarId {
        match *self {}
    }
    fn output_type_id(&self, _: &mut IRTypes) -> IRTypeId {
        match *self {}
    }
    fn hash_bytes_native(&self, _: &[&[u8]]) -> Vec<u8> {
        match *self {}
    }
    fn name(&self) -> &str {
        match *self {}
    }
}

// ============================================================================
// Commitment context (internal)
// ============================================================================

/// Internal commitment context built once inside `virtualize_ir_impl` and
/// threaded through to the setup-block and handler emitters.
struct CommitmentCtx<'a, H: IrHashAlgorithm> {
    config: &'a CommitmentConfig<H>,
    /// Pre-computed native hash for each original block, in block order.
    /// `per_block[i]` is the `Constant`-encoded hash of block `i`'s entry.
    per_block: Vec<Constant>,
    /// Pre-interned IR type of the hash output.
    hash_output_ty: IRTypeId,
}

// ============================================================================
// BlockEmitter — IrEmitter impl for IRBlockUnfinished
// ============================================================================

struct BlockEmitter<'a> {
    block: &'a mut IRBlockUnfinished,
    types: &'a mut IRTypes,
    addr_ty: IRTypeId,
    bit_ty: IRTypeId,
}

impl IrEmitter for BlockEmitter<'_> {
    fn emit(&mut self, stmt: IRStmt) -> IRVarId {
        self.block.push(stmt)
    }
    fn intern_type(&mut self, ty: IRType) -> IRTypeId {
        self.types.intern(ty)
    }
    fn addr_ty(&self) -> IRTypeId {
        self.addr_ty
    }
    fn bit_ty(&self) -> IRTypeId {
        self.bit_ty
    }
}

// ============================================================================
// Public API
// ============================================================================

/// Virtualise an [`IRBlocks`] module (no bytecode commitment).
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
/// * `Dyn(v)` targets are supported; `v` must have type `IRType::Block { params }`.
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
    virtualize_ir_impl::<P, NoOpHashAlgorithm>(blocks, types, cfg, None)
}

/// Virtualise an [`IRBlocks`] module with per-PC bytecode commitment.
///
/// In addition to the standard virtualisation, the setup block writes
/// `commitment[pc] = H(handler_idx, slot_0, …, slot_n)` as a constant for
/// every program counter into `commitment_cfg.commitment_storage`.  Every
/// handler then re-reads its slots, recomputes the hash via
/// `H::emit_ir`, and XOR-injects the diff into `next_pc` — making the
/// commitment structurally binding without a separate assertion oracle.
///
/// All preconditions of [`virtualize_ir`] apply.  Additionally,
/// `commitment_cfg.commitment_storage` must not overlap with
/// `cfg.bytecode_storage` or the register-file range.
pub fn virtualize_ir_committed<P: Clone + Default, H: IrHashAlgorithm>(
    blocks: &IRBlocks<P>,
    types: &mut IRTypes,
    cfg: &VirtualizeConfig,
    commitment_cfg: &CommitmentConfig<H>,
) -> VirtOutput<IRBlocks<P>> {
    virtualize_ir_impl(blocks, types, cfg, Some(commitment_cfg))
}

// ============================================================================
// Core implementation
// ============================================================================

fn virtualize_ir_impl<P: Clone + Default, H: IrHashAlgorithm>(
    blocks: &IRBlocks<P>,
    types: &mut IRTypes,
    cfg: &VirtualizeConfig,
    commitment: Option<&CommitmentConfig<H>>,
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

    // CSE: merge duplicate OracleCall stmts within each block before
    // canonicalisation so identical blocks that differ only by redundant
    // oracle calls can share handlers.
    let cse_blocks: IRBlocks<P> = IRBlocks {
        oracles: blocks.oracles.clone(),
        actions: blocks.actions.clone(),
        rngs: blocks.rngs.clone(),
        blocks: blocks.blocks.iter().map(deduplicate_oracle_calls_in_block).collect(),
    };

    // Canonicalise every block — lifts Const values and terminator
    // block-target ids into BlockImmediates.
    let per_block_canon: Vec<(IrHandlerKey, BlockImmediates)> =
        cse_blocks.blocks.iter().map(canonicalize_ir_block).collect();
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
    let ir_types_slice = &types.0;
    let reg_alloc = RegAlloc::build(&cse_blocks, reg_storage_base, ir_types_slice);

    // Compute hash output type first (needs &mut types) before any immutable
    // borrows of `types.0` are created.
    let commitment_hash_ty: Option<IRTypeId> =
        commitment.map(|cfg| cfg.algorithm.output_type_id(types));

    let ir_types_slice = &types.0;
    let reg_alloc = RegAlloc::build(&cse_blocks, reg_storage_base, ir_types_slice);

    // Build commitment context (pre-computes native hashes for every block).
    let commitment_ctx: Option<CommitmentCtx<'_, H>> =
        commitment.zip(commitment_hash_ty).map(|(cfg, hash_output_ty)| {
            build_commitment_ctx(
                cfg,
                &cse_blocks,
                &dedup,
                &layout,
                &reg_alloc,
                ir_types_slice,
                hash_output_ty,
            )
        });

    // Emit the module using the pre-computed layout.
    let out_blocks =
        emit_output_ir::<P, H>(&cse_blocks, &dedup, &layout, &reg_alloc, addr_ty, bit_ty, cfg, types, commitment_ctx.as_ref());

    let bytecode = if cfg.bytecode_form.wants_external() {
        Some(dedup.to_bytecode())
    } else {
        None
    };

    // Oblivious dispatch runs movfuscate_ir over the Public output.
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

fn validate_input<P: Clone + Default>(_blocks: &IRBlocks<P>) {
    // All terminator forms — including Dyn targets — are now valid inputs.
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
    fn build<P: Clone + Default>(
        blocks: &IRBlocks<P>,
        storage_base: u32,
        ir_types: &[IRType],
    ) -> Self {
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

        // Pre-register param types from Dyn jump-target Block signatures.
        // Without this, storage_for(T) panics if T appears only in a Dyn
        // target's Block.params and nowhere else as a block param.
        for b in &blocks.blocks {
            let mut visit = |target: &IRBlockTargetId| {
                if let IRBlockTargetId::Dyn(v) = target {
                    let v_ty_id = resolve_var_type(b, *v);
                    if let IRType::Block { params } = &ir_types[v_ty_id.0 as usize] {
                        for &param_ty in params {
                            max_param_idx.entry(param_ty).or_insert(0);
                        }
                    }
                }
            };
            match &b.terminator {
                IRTerminator::Jmp { func, .. } => visit(func),
                IRTerminator::JumpCond { true_block, false_block, .. } => {
                    visit(true_block);
                    visit(false_block);
                }
                IRTerminator::JumpTable { cases, .. } => {
                    for (target, _) in cases.values() {
                        visit(target);
                    }
                }
            }
        }

        // Extract return shape.
        let return_arg_tys = extract_return_shape(blocks);

        // Allocate return registers.  Return regs are only live in the
        // final execution step (the done=1 arm writes them; the dispatcher
        // reads them immediately after and exits).  They are never live
        // simultaneously with any block's param registers, so they can
        // share the same slots — starting from index 0 of each type's
        // storage.  The total register file depth for type T is then
        // max(max_T_params, n_T_returns) rather than max_T_params + n_T_returns.
        let mut return_regs: Vec<RegRef> = Vec::with_capacity(return_arg_tys.len());
        {
            let mut next_ret_idx: BTreeMap<IRTypeId, u32> = BTreeMap::new();
            for &ty in &return_arg_tys {
                let off = next_ret_idx.entry(ty).or_insert(0);
                return_regs.push(RegRef { ty, idx: *off });
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
            match s {
                Stmt::Const(_, ty) | Stmt::Poly { ty, .. } => {
                    const_value_slot.push(Some(push_slot(
                        &mut schema.slots,
                        SlotKind::ConstValue,
                        *ty,
                    )));
                }
                _ => {
                    const_value_slot.push(None);
                }
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

/// Block-id constants for the legacy (indirect) dispatch layout.
///
/// Layout: SETUP | DISPATCHER | RETURN | DISPATCH | handler_0 .. handler_n | subblocks
const SETUP_BID: u32 = 0;
const DISPATCHER_BID: u32 = 1;
const RETURN_BID: u32 = 2;
const DISPATCH_BID: u32 = 3;
const HANDLER_BID_BASE: u32 = 4;

/// Block-id constants for the direct-dispatch layout.
///
/// Layout: SETUP | RETURN | INIT_DISPATCH | handler_0 .. handler_n | subblocks
///
/// Handlers jump directly to their successor via an inline dispatch sub-block,
/// skipping the DISPATCHER and DISPATCH blocks entirely.
const DD_RETURN_BID: u32 = 1;
const DD_INIT_DISPATCH_BID: u32 = 2;
const DD_HANDLER_BID_BASE: u32 = 3;

/// Context threaded into handler/sub-block emitters when `direct_dispatch=true`.
struct DirectDispatch<'a> {
    dedup: &'a DedupTable<IrHandlerKey>,
    bytecode_storage: StorageId,
}

fn emit_output_ir<P: Clone + Default, H: IrHashAlgorithm>(
    blocks_in: &IRBlocks<P>,
    dedup: &DedupTable<IrHandlerKey>,
    layout: &GlobalLayout,
    reg_alloc: &RegAlloc,
    addr_ty: IRTypeId,
    bit_ty: IRTypeId,
    cfg: &VirtualizeConfig,
    types: &mut IRTypes,
    commitment: Option<&CommitmentCtx<'_, H>>,
) -> IRBlocks<P> {
    let entry_params = blocks_in.blocks[0].params.clone();
    let return_arg_tys: Vec<IRTypeId> = reg_alloc.return_regs.iter().map(|r| r.ty).collect();

    let n_handlers = dedup.n_handlers();
    let mut extra_subblocks: Vec<IRBlock<P>> = Vec::new();

    let handler_bid_base = if cfg.direct_dispatch { DD_HANDLER_BID_BASE } else { HANDLER_BID_BASE };
    let mut next_sub_bid = handler_bid_base + n_handlers as u32;

    // --- setup ---
    let setup = emit_setup_block::<P, H>(
        blocks_in,
        &entry_params,
        dedup,
        layout,
        reg_alloc,
        addr_ty,
        bit_ty,
        cfg,
        &types.0,
        commitment,
    );

    let dd = cfg.direct_dispatch.then(|| DirectDispatch {
        dedup,
        bytecode_storage: cfg.bytecode_storage,
    });

    // --- handlers (+ arm sub-blocks) ---
    let mut handler_blocks: Vec<IRBlock<P>> = Vec::with_capacity(n_handlers);
    for (h_idx, key) in dedup.handler_keys.iter().enumerate() {
        let schema = &layout.schemas[h_idx];
        let slot_ids = &layout.per_handler_slot[h_idx];
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
            dd.as_ref(),
            commitment,
        );
        handler_blocks.push(handler);
        extra_subblocks.extend(extras);
    }

    // Assemble — layout differs between modes.
    let mut all_blocks: Vec<IRBlock<P>>;
    if cfg.direct_dispatch {
        // Layout: SETUP | RETURN | INIT_DISPATCH | handlers | subblocks
        let return_block = emit_return_block(reg_alloc, &return_arg_tys, addr_ty);
        let init_dispatch = emit_dispatch_block_with_base(
            dedup,
            cfg.bytecode_storage,
            addr_ty,
            DD_HANDLER_BID_BASE,
        );
        all_blocks = Vec::with_capacity(3 + n_handlers + extra_subblocks.len());
        all_blocks.push(setup);
        all_blocks.push(return_block);
        all_blocks.push(init_dispatch);
    } else {
        // Layout: SETUP | DISPATCHER | RETURN | DISPATCH | handlers | subblocks
        let dispatcher = emit_dispatcher_block(addr_ty, bit_ty);
        let return_block = emit_return_block(reg_alloc, &return_arg_tys, addr_ty);
        let dispatch = emit_dispatch_block(dedup, cfg.bytecode_storage, addr_ty);
        all_blocks = Vec::with_capacity(4 + n_handlers + extra_subblocks.len());
        all_blocks.push(setup);
        all_blocks.push(dispatcher);
        all_blocks.push(return_block);
        all_blocks.push(dispatch);
    }
    all_blocks.extend(handler_blocks);
    all_blocks.extend(extra_subblocks);

    // Merge algorithm-internal oracle declarations.
    let mut out_oracles = blocks_in.oracles.clone();
    if let Some(ctx) = commitment {
        // Collect declarations the algorithm needs (deduplicating by name).
        // We call output_type_id only to drive intern; the type table is
        // already mutated by build_commitment_ctx so this is a no-op lookup.
        // We can't call internal_oracle_decls here without &mut types, and
        // the caller already did that in build_commitment_ctx.  For now the
        // algorithm is responsible for populating them at build time; leave
        // the merge point here for future use.
        let _ = ctx;
    }

    IRBlocks {
        oracles: out_oracles,
        actions: blocks_in.actions.clone(),
        rngs: blocks_in.rngs.clone(),
        blocks: all_blocks,
    }
}

// ----------------------------------------------------------------------------
// Setup block (block 0)
// ----------------------------------------------------------------------------

fn emit_setup_block<P: Clone + Default, H: IrHashAlgorithm>(
    blocks_in: &IRBlocks<P>,
    entry_params: &[IRTypeId],
    dedup: &DedupTable<IrHandlerKey>,
    layout: &GlobalLayout,
    reg_alloc: &RegAlloc,
    addr_ty: IRTypeId,
    bit_ty: IRTypeId,
    cfg: &VirtualizeConfig,
    ir_types: &[IRType],
    commitment: Option<&CommitmentCtx<'_, H>>,
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
            let slot_values = compute_slot_values(block, block_id, schema, reg_alloc, ir_types);
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

    // 2b. Commitment: write per-PC hash values as constants.
    if let Some(ctx) = commitment {
        for (block_id, &commitment_const) in ctx.per_block.iter().enumerate() {
            let pc_cst = b.push(Stmt::Const(const_u32(block_id as u32), addr_ty));
            let val_cst = b.push(Stmt::Const(commitment_const, ctx.hash_output_ty));
            b.push(Stmt::StorageWrite {
                storage: ctx.config.commitment_storage,
                ty: ctx.hash_output_ty,
                addr: pc_cst,
                src: val_cst,
            });
        }
    }

    // 3. Jump to the first dispatch point.
    //    Legacy mode: jump to DISPATCHER with (pc=0, done=0).
    //    Direct mode: jump to INIT_DISPATCH with (pc=0).
    let pc = b.push(Stmt::Const(const_u32(0), addr_ty));
    let entry_bid = if cfg.direct_dispatch { DD_INIT_DISPATCH_BID } else { DISPATCHER_BID };
    let mut jump_args = vec![pc];
    if !cfg.direct_dispatch {
        let done = b.push(Stmt::Const(const_bit(false), bit_ty));
        jump_args.push(done);
    }
    b.terminator = IRTerminator::Jmp {
        func: IRBlockTargetId::Block(IRBlockId(entry_bid)),
        args: jump_args,
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
    ir_types: &[IRType],
) -> Vec<Constant> {
    let mut out: Vec<Constant> = vec![const_u32(0); schema.slots.len()];

    // Param source registers.
    let my_regs = &reg_alloc.per_block_params[block_id];
    assert_eq!(my_regs.len(), schema.param_src_slot.len());
    for (i, slot_idx) in schema.param_src_slot.iter().enumerate() {
        out[*slot_idx] = const_u32(my_regs[i].idx);
    }

    // Const value slots (also covers Poly.constant).
    for (stmt_idx, slot_opt) in schema.const_value_slot.iter().enumerate() {
        if let Some(slot_idx) = slot_opt {
            match &block.stmts[stmt_idx] {
                Stmt::Const(c, _) => {
                    out[*slot_idx] = *c;
                }
                Stmt::Poly { constant, .. } => {
                    out[*slot_idx] = *constant;
                }
                _ => panic!(
                    "compute_slot_values: schema says stmt {} is Const/Poly \
                     but source block disagrees",
                    stmt_idx
                ),
            }
        }
    }

    // Terminator arm slots.
    fill_terminator_slots(block, schema, reg_alloc, ir_types, &mut out);

    out
}

fn fill_terminator_slots<P: Clone + Default>(
    block: &IRBlock<P>,
    schema: &HandlerSchema,
    reg_alloc: &RegAlloc,
    ir_types: &[IRType],
    out: &mut [Constant],
) {
    let fill_arm = |out: &mut [Constant],
                    arm: &ArmSchema,
                    target: &IRBlockTargetId,
                    args: &[IRVarId]| {
        assert_eq!(arm.arg_dst_slots.len(), args.len());
        match target {
            IRBlockTargetId::Block(bid) => {
                out[arm.next_pc_slot] = const_u32(bid.0);
                out[arm.done_slot] = const_bit(false);
                for (i, _arg) in args.iter().enumerate() {
                    let dst = reg_alloc.per_block_params[bid.0 as usize][i];
                    out[arm.arg_dst_slots[i]] = const_u32(dst.idx);
                }
            }
            IRBlockTargetId::Return => {
                out[arm.next_pc_slot] = const_u32(0);
                out[arm.done_slot] = const_bit(true);
                for (i, _arg) in args.iter().enumerate() {
                    out[arm.arg_dst_slots[i]] = const_u32(reg_alloc.return_regs[i].idx);
                }
            }
            IRBlockTargetId::Dyn(v) => {
                // done = false; next_pc is a dummy (handler reads it from the
                // runtime Dyn var via Transmute — see emit_handler_block).
                out[arm.next_pc_slot] = const_u32(0);
                out[arm.done_slot] = const_bit(false);
                // Compute arg-destination register indices from the Block type's
                // params signature (same sequential-per-type rule as RegAlloc).
                let v_ty_id = resolve_var_type(block, *v);
                let sig: &[IRTypeId] = match &ir_types[v_ty_id.0 as usize] {
                    IRType::Block { params } => params,
                    _ => panic!(
                        "fill_terminator_slots: Dyn var {} does not have Block type",
                        v.0
                    ),
                };
                let mut type_counter: BTreeMap<IRTypeId, u32> = BTreeMap::new();
                for (i, _arg) in args.iter().enumerate() {
                    let param_ty = sig[i];
                    let idx = type_counter.entry(param_ty).or_insert(0);
                    out[arm.arg_dst_slots[i]] = const_u32(*idx);
                    *idx += 1;
                }
            }
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
    emit_dispatch_block_with_base(dedup, bytecode_storage, addr_ty, HANDLER_BID_BASE)
}

/// Emit a block that reads `handler_idx` from bytecode at `pc` and
/// dispatches to the appropriate handler via `JumpTable`.
/// Used both for the legacy `DISPATCH_BID` block and for the inline
/// dispatch sub-blocks in direct-dispatch mode.
fn emit_dispatch_block_with_base<P: Clone + Default>(
    dedup: &DedupTable<IrHandlerKey>,
    bytecode_storage: StorageId,
    addr_ty: IRTypeId,
    handler_bid_base: u32,
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
                IRBlockTargetId::Block(IRBlockId(handler_bid_base + h_idx as u32)),
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
fn emit_handler_block<P: Clone + Default, H: IrHashAlgorithm>(
    key: &IrHandlerKey,
    schema: &HandlerSchema,
    slot_ids: &[StorageId],
    reg_alloc: &RegAlloc,
    addr_ty: IRTypeId,
    bit_ty: IRTypeId,
    bytecode_storage: StorageId,
    types: &mut IRTypes,
    next_sub_bid: &mut u32,
    dd: Option<&DirectDispatch<'_>>,
    commitment: Option<&CommitmentCtx<'_, H>>,
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
            Stmt::Poly { ty, coeffs, .. } => {
                // The constant term was lifted into a bytecode slot; read it
                // back at runtime and fold it in via a degree-1 XOR poly.
                let slot_storage = slot_ids[schema.const_value_slot[stmt_idx]
                    .expect("Poly stmt must have a value slot")];
                let const_var = b.push(Stmt::StorageRead {
                    storage: slot_storage,
                    ty: *ty,
                    addr: pc,
                });
                let remapped_coeffs = remap_coeffs(coeffs, &canonical_var);
                let poly_out = b.push(Stmt::Poly {
                    ty: *ty,
                    coeffs: remapped_coeffs,
                    constant: ZERO_CONSTANT,
                });
                let mut xor_coeffs = BTreeMap::new();
                xor_coeffs.insert(alloc::vec![poly_out], 1u8);
                xor_coeffs.insert(alloc::vec![const_var], 1u8);
                let v = b.push(Stmt::Poly {
                    ty: *ty,
                    coeffs: xor_coeffs,
                    constant: ZERO_CONSTANT,
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
            match func {
                IRBlockTargetId::Dyn(canon_v) => {
                    // For Dyn: next_pc comes from the Block-typed var at runtime.
                    // Commitment protection is not applied to Dyn targets.
                    let raw    = canonical_var[canon_v.0 as usize];
                    let v_ty   = canon_types[canon_v.0 as usize];
                    let next_pc = b.push(Stmt::Transmute {
                        src: raw,
                        src_ty: v_ty,
                        dst_ty: addr_ty,
                    });
                    let done = b.push(Stmt::Const(const_bit(false), bit_ty));
                    IRTerminator::Jmp {
                        func: IRBlockTargetId::Block(IRBlockId(DISPATCHER_BID)),
                        args: vec![next_pc, done],
                    }
                }
                _ => {
                    // Compute commitment protection (zero when valid; XOR'd into next_pc).
                    let protection: Option<IRVarId> = commitment.map(|ctx| {
                        emit_commitment_check_ir(
                            &mut b, ctx, schema, slot_ids,
                            bytecode_storage, addr_ty, bit_ty, pc, types,
                        )
                    });
                    if let Some(dd) = dd {
                        build_direct_dispatch_terminator(
                            &mut b, arm, slot_ids, addr_ty, bit_ty, pc, dd,
                            next_sub_bid, &mut extras, protection,
                        )
                    } else {
                        build_return_to_dispatcher(
                            &mut b, arm, slot_ids, addr_ty, bit_ty, pc, protection,
                        )
                    }
                }
            }
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

            // Compute commitment protection once for this handler activation.
            // Option<IRVarId> is Copy so it can be reused across arms.
            let commitment_protection: Option<IRVarId> = commitment.map(|ctx| {
                emit_commitment_check_ir(
                    &mut b, ctx, schema, slot_ids,
                    bytecode_storage, addr_ty, bit_ty, pc, types,
                )
            });

            // Emit sub-block for each arm — Dyn arms use emit_dyn_arm_subblock.
            match true_block {
                IRBlockTargetId::Dyn(canon_v) => {
                    let dyn_ty = canon_types[canon_v.0 as usize];
                    extras.push(emit_dyn_arm_subblock::<P>(
                        &schema.arms[0], slot_ids, reg_alloc, addr_ty, bit_ty,
                        dyn_ty, &true_arg_tys,
                    ));
                }
                _ => {
                    let (sub, dd_extras) = emit_arm_subblock::<P>(
                        &schema.arms[0], slot_ids, reg_alloc, addr_ty, bit_ty,
                        &true_arg_tys, dd, next_sub_bid,
                        commitment_protection.is_some(),
                    );
                    extras.push(sub);
                    extras.extend(dd_extras);
                }
            }
            match false_block {
                IRBlockTargetId::Dyn(canon_v) => {
                    let dyn_ty = canon_types[canon_v.0 as usize];
                    extras.push(emit_dyn_arm_subblock::<P>(
                        &schema.arms[1], slot_ids, reg_alloc, addr_ty, bit_ty,
                        dyn_ty, &false_arg_tys,
                    ));
                }
                _ => {
                    let (sub, dd_extras) = emit_arm_subblock::<P>(
                        &schema.arms[1], slot_ids, reg_alloc, addr_ty, bit_ty,
                        &false_arg_tys, dd, next_sub_bid,
                        commitment_protection.is_some(),
                    );
                    extras.push(sub);
                    extras.extend(dd_extras);
                }
            }

            // Build call args: for non-Dyn arms insert the commitment diff
            // after `pc` so the sub-block can XOR it into next_pc.
            let true_call_args: Vec<IRVarId> = match true_block {
                IRBlockTargetId::Dyn(canon_v) => core::iter::once(pc)
                    .chain(core::iter::once(canonical_var[canon_v.0 as usize]))
                    .chain(true_args.iter().map(|v| canonical_var[v.0 as usize]))
                    .collect(),
                _ => {
                    if let Some(diff_var) = commitment_protection {
                        core::iter::once(pc)
                            .chain(core::iter::once(diff_var))
                            .chain(true_args.iter().map(|v| canonical_var[v.0 as usize]))
                            .collect()
                    } else {
                        core::iter::once(pc)
                            .chain(true_args.iter().map(|v| canonical_var[v.0 as usize]))
                            .collect()
                    }
                }
            };
            let false_call_args: Vec<IRVarId> = match false_block {
                IRBlockTargetId::Dyn(canon_v) => core::iter::once(pc)
                    .chain(core::iter::once(canonical_var[canon_v.0 as usize]))
                    .chain(false_args.iter().map(|v| canonical_var[v.0 as usize]))
                    .collect(),
                _ => {
                    if let Some(diff_var) = commitment_protection {
                        core::iter::once(pc)
                            .chain(core::iter::once(diff_var))
                            .chain(false_args.iter().map(|v| canonical_var[v.0 as usize]))
                            .collect()
                    } else {
                        core::iter::once(pc)
                            .chain(false_args.iter().map(|v| canonical_var[v.0 as usize]))
                            .collect()
                    }
                }
            };

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

            // Compute commitment protection once for all arms.
            let commitment_protection: Option<IRVarId> = commitment.map(|ctx| {
                emit_commitment_check_ir(
                    &mut b, ctx, schema, slot_ids,
                    bytecode_storage, addr_ty, bit_ty, pc, types,
                )
            });

            let mut out_cases: BTreeMap<Constant, (IRBlockTargetId, Vec<IRVarId>)> =
                BTreeMap::new();
            for (arm_idx, (k, (target, args))) in cases.iter().enumerate() {
                let sub_bid = *next_sub_bid;
                *next_sub_bid += 1;
                let arg_tys: Vec<IRTypeId> =
                    args.iter().map(|v| canon_types[v.0 as usize]).collect();
                let arm_args: Vec<IRVarId> = match target {
                    IRBlockTargetId::Dyn(canon_v) => {
                        let dyn_ty = canon_types[canon_v.0 as usize];
                        extras.push(emit_dyn_arm_subblock::<P>(
                            &schema.arms[arm_idx], slot_ids, reg_alloc, addr_ty, bit_ty,
                            dyn_ty, &arg_tys,
                        ));
                        core::iter::once(pc)
                            .chain(core::iter::once(canonical_var[canon_v.0 as usize]))
                            .chain(args.iter().map(|v| canonical_var[v.0 as usize]))
                            .collect()
                    }
                    _ => {
                        let (arm_sub, arm_dd_extras) = emit_arm_subblock::<P>(
                            &schema.arms[arm_idx], slot_ids, reg_alloc, addr_ty, bit_ty,
                            &arg_tys, dd, next_sub_bid,
                            commitment_protection.is_some(),
                        );
                        extras.push(arm_sub);
                        extras.extend(arm_dd_extras);
                        if let Some(diff_var) = commitment_protection {
                            core::iter::once(pc)
                                .chain(core::iter::once(diff_var))
                                .chain(args.iter().map(|v| canonical_var[v.0 as usize]))
                                .collect()
                        } else {
                            core::iter::once(pc)
                                .chain(args.iter().map(|v| canonical_var[v.0 as usize]))
                                .collect()
                        }
                    }
                };
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
/// When `protection` is `Some(diff_var)`, XOR-injects `diff_var` into
/// `next_pc` before the jump.  If the commitment is valid `diff_var == 0`
/// and the XOR is a no-op; otherwise the PC is corrupted, binding the
/// commitment structurally.
fn build_return_to_dispatcher(
    b: &mut IRBlockUnfinished,
    arm: &ArmSchema,
    slot_ids: &[StorageId],
    addr_ty: IRTypeId,
    bit_ty: IRTypeId,
    pc: IRVarId,
    protection: Option<IRVarId>,
) -> IRTerminator {
    let next_pc_raw = b.push(Stmt::StorageRead {
        storage: slot_ids[arm.next_pc_slot],
        ty: addr_ty,
        addr: pc,
    });
    // XOR-inject the commitment diff into next_pc.
    let next_pc = if let Some(diff_var) = protection {
        let mut coeffs: BTreeMap<Vec<IRVarId>, u8> = BTreeMap::new();
        coeffs.insert(alloc::vec![next_pc_raw], 1);
        coeffs.insert(alloc::vec![diff_var], 1);
        b.push(Stmt::Poly { ty: addr_ty, coeffs, constant: Constant { hi: 0, lo: 0 } })
    } else {
        next_pc_raw
    };
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

/// Build a direct-dispatch terminator: reads `next_pc` and `done` from
/// bytecode, then emits `JumpCond(done, DD_RETURN_BID, dispatch_sub(next_pc))`.
/// When `protection` is `Some(diff_var)`, XOR-injects `diff_var` into
/// `next_pc` before the branch.
#[allow(clippy::too_many_arguments)]
fn build_direct_dispatch_terminator<P: Clone + Default>(
    b: &mut IRBlockUnfinished,
    arm: &ArmSchema,
    slot_ids: &[StorageId],
    addr_ty: IRTypeId,
    bit_ty: IRTypeId,
    pc: IRVarId,
    dd: &DirectDispatch<'_>,
    next_sub_bid: &mut u32,
    extras: &mut Vec<IRBlock<P>>,
    protection: Option<IRVarId>,
) -> IRTerminator {
    let next_pc_raw = b.push(Stmt::StorageRead {
        storage: slot_ids[arm.next_pc_slot],
        ty: addr_ty,
        addr: pc,
    });
    let next_pc = if let Some(diff_var) = protection {
        let mut coeffs: BTreeMap<Vec<IRVarId>, u8> = BTreeMap::new();
        coeffs.insert(alloc::vec![next_pc_raw], 1);
        coeffs.insert(alloc::vec![diff_var], 1);
        b.push(Stmt::Poly { ty: addr_ty, coeffs, constant: Constant { hi: 0, lo: 0 } })
    } else {
        next_pc_raw
    };
    let done = b.push(Stmt::StorageRead {
        storage: slot_ids[arm.done_slot],
        ty: bit_ty,
        addr: pc,
    });
    let dispatch_sub_bid = *next_sub_bid;
    *next_sub_bid += 1;
    extras.push(emit_dispatch_block_with_base(
        dd.dedup,
        dd.bytecode_storage,
        addr_ty,
        DD_HANDLER_BID_BASE,
    ));
    IRTerminator::JumpCond {
        condition: done,
        true_block: IRBlockTargetId::Block(IRBlockId(DD_RETURN_BID)),
        true_args: vec![],
        false_block: IRBlockTargetId::Block(IRBlockId(dispatch_sub_bid)),
        false_args: vec![next_pc],
    }
}

/// Sub-block emitted for each arm of a conditional terminator.
/// Params: `[pc: _32, (diff: _32,)? arg_0: ty_0, ... arg_{n-1}: ty_{n-1}]`.
/// When `has_commitment` is true the second param is the commitment diff
/// (computed once in the parent handler block) and is XOR-injected into
/// `next_pc` before returning to the dispatcher.
///
/// In direct-dispatch mode (`dd` is `Some`) also returns an inline
/// dispatch sub-block that routes to the successor handler.
#[allow(clippy::too_many_arguments)]
fn emit_arm_subblock<P: Clone + Default>(
    arm: &ArmSchema,
    slot_ids: &[StorageId],
    reg_alloc: &RegAlloc,
    addr_ty: IRTypeId,
    bit_ty: IRTypeId,
    arg_tys: &[IRTypeId],
    dd: Option<&DirectDispatch<'_>>,
    next_sub_bid: &mut u32,
    has_commitment: bool,
) -> (IRBlock<P>, Vec<IRBlock<P>>) {
    let mut params: Vec<IRTypeId> = vec![addr_ty];
    if has_commitment {
        params.push(addr_ty); // commitment diff
    }
    params.extend_from_slice(arg_tys);
    let mut b = IRBlockUnfinished::new(params);
    let pc = IRVarId(0);
    // IRVarId(1) is the commitment diff when has_commitment; else first arg.
    let diff_opt: Option<IRVarId> = if has_commitment { Some(IRVarId(1)) } else { None };
    let arg_base: u32 = if has_commitment { 2 } else { 1 };

    for (i, &arg_ty) in arg_tys.iter().enumerate() {
        let arg_val = IRVarId(arg_base + i as u32);
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

    let mut dd_extras: Vec<IRBlock<P>> = Vec::new();
    b.terminator = if let Some(dd) = dd {
        build_direct_dispatch_terminator(
            &mut b, arm, slot_ids, addr_ty, bit_ty, pc, dd,
            next_sub_bid, &mut dd_extras,
            diff_opt,
        )
    } else {
        let next_pc_raw = b.push(Stmt::StorageRead {
            storage: slot_ids[arm.next_pc_slot],
            ty: addr_ty,
            addr: pc,
        });
        let next_pc = if let Some(diff_var) = diff_opt {
            let mut coeffs: BTreeMap<Vec<IRVarId>, u8> = BTreeMap::new();
            coeffs.insert(alloc::vec![next_pc_raw], 1);
            coeffs.insert(alloc::vec![diff_var], 1);
            b.push(Stmt::Poly { ty: addr_ty, coeffs, constant: Constant { hi: 0, lo: 0 } })
        } else {
            next_pc_raw
        };
        let done = b.push(Stmt::StorageRead {
            storage: slot_ids[arm.done_slot],
            ty: bit_ty,
            addr: pc,
        });
        IRTerminator::Jmp {
            func: IRBlockTargetId::Block(IRBlockId(DISPATCHER_BID)),
            args: vec![next_pc, done],
        }
    };
    (b.into_ir_block::<P>(), dd_extras)
}

/// Sub-block for a `Dyn` terminator arm.
///
/// Params: `[pc: _32, dyn_val: dyn_var_ty, arg_0: ty_0, …]`.
/// Body: write each arg to its destination register (indexed from bytecode),
/// then compute `next_pc = Transmute(dyn_val → addr_ty)` and jump to DISPATCHER.
#[allow(clippy::too_many_arguments)]
fn emit_dyn_arm_subblock<P: Clone + Default>(
    arm: &ArmSchema,
    slot_ids: &[StorageId],
    reg_alloc: &RegAlloc,
    addr_ty: IRTypeId,
    bit_ty: IRTypeId,
    dyn_var_ty: IRTypeId,
    arg_tys: &[IRTypeId],
) -> IRBlock<P> {
    let mut params: Vec<IRTypeId> = vec![addr_ty, dyn_var_ty];
    params.extend_from_slice(arg_tys);
    let mut b = IRBlockUnfinished::new(params);
    let pc       = IRVarId(0);
    let dyn_val  = IRVarId(1);

    for (i, &arg_ty) in arg_tys.iter().enumerate() {
        let arg_val = IRVarId(2 + i as u32);
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

    let next_pc = b.push(Stmt::Transmute { src: dyn_val, src_ty: dyn_var_ty, dst_ty: addr_ty });
    let done    = b.push(Stmt::Const(const_bit(false), bit_ty));
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
// Oracle call CSE (pre-canonicalisation)
// ============================================================================

/// Merge duplicate `OracleCall` stmts within a single block.
///
/// Two `OracleCall`s are identical when they have the same name and the same
/// (already-remapped) argument var ids.  The second and any further duplicates
/// are dropped; their output var is redirected to the first call's output var
/// so that all `OracleOutput` users resolve correctly.
///
/// All subsequent stmt var ids are renumbered to close the gaps, and the
/// terminator is updated accordingly.
fn deduplicate_oracle_calls_in_block<P: Clone + Default>(block: &IRBlock<P>) -> IRBlock<P> {
    let n_params = block.params.len();
    let total = n_params + block.stmts.len();
    // var_remap[old_var.0] = new IRVarId (identity until a stmt is dropped)
    let mut var_remap: Vec<IRVarId> = (0..total as u32).map(IRVarId).collect();
    // (name, remapped-args) → first-call new var
    let mut seen: BTreeMap<(alloc::string::String, Vec<IRVarId>), IRVarId> = BTreeMap::new();
    let mut new_stmts: Vec<IRStmt> = Vec::with_capacity(block.stmts.len());
    let mut new_provs: Vec<P> = Vec::with_capacity(block.stmts.len());

    for (stmt_idx, (s, prov)) in block.stmts.iter().zip(block.stmt_provs.iter()).enumerate() {
        let old_var = IRVarId((n_params + stmt_idx) as u32);
        match s {
            Stmt::OracleCall { name, args, output_tys, result_ty } => {
                let remapped_args: Vec<IRVarId> =
                    args.iter().map(|v| var_remap[v.0 as usize]).collect();
                let key = (name.clone(), remapped_args.clone());
                if let Some(&first_var) = seen.get(&key) {
                    var_remap[old_var.0 as usize] = first_var;
                } else {
                    let new_var = IRVarId((n_params + new_stmts.len()) as u32);
                    seen.insert(key, new_var);
                    var_remap[old_var.0 as usize] = new_var;
                    new_stmts.push(Stmt::OracleCall {
                        name: name.clone(),
                        args: remapped_args,
                        output_tys: output_tys.clone(),
                        result_ty: *result_ty,
                    });
                    new_provs.push(prov.clone());
                }
            }
            other => {
                let new_var = IRVarId((n_params + new_stmts.len()) as u32);
                var_remap[old_var.0 as usize] = new_var;
                new_stmts.push(remap_stmt(other, &var_remap));
                new_provs.push(prov.clone());
            }
        }
    }
    let new_terminator = remap_ir_terminator_vars(&block.terminator, &var_remap);
    IRBlock {
        params: block.params.clone(),
        stmts: new_stmts,
        stmt_provs: new_provs,
        terminator: new_terminator,
    }
}

fn remap_ir_terminator_vars(t: &IRTerminator, var_remap: &[IRVarId]) -> IRTerminator {
    match t {
        IRTerminator::Jmp { func, args } => IRTerminator::Jmp {
            func: func.clone(),
            args: remap_vars(args, var_remap),
        },
        IRTerminator::JumpCond {
            condition,
            true_block,
            true_args,
            false_block,
            false_args,
        } => IRTerminator::JumpCond {
            condition: remap_var(*condition, var_remap),
            true_block: true_block.clone(),
            true_args: remap_vars(true_args, var_remap),
            false_block: false_block.clone(),
            false_args: remap_vars(false_args, var_remap),
        },
        IRTerminator::JumpTable { index, cases } => {
            let mut new_cases = BTreeMap::new();
            for (k, (target, args)) in cases {
                new_cases.insert(*k, (target.clone(), remap_vars(args, var_remap)));
            }
            IRTerminator::JumpTable {
                index: remap_var(*index, var_remap),
                cases: new_cases,
            }
        }
    }
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

// ============================================================================
// Commitment helpers
// ============================================================================

/// Emit IR that re-reads every bytecode slot for the current handler,
/// computes the commitment hash, and returns `diff_as_addr` — the XOR of
/// the computed hash against the stored commitment, reinterpreted as `addr_ty`.
///
/// When the commitment is valid `diff_as_addr == 0`, so XOR-ing it into
/// `next_pc` is a no-op.  When the bytecode has been tampered with,
/// `diff_as_addr != 0` and the program jumps to a wrong address.
fn emit_commitment_check_ir<H: IrHashAlgorithm>(
    b: &mut IRBlockUnfinished,
    ctx: &CommitmentCtx<'_, H>,
    schema: &HandlerSchema,
    slot_ids: &[StorageId],
    bytecode_storage: StorageId,
    addr_ty: IRTypeId,
    bit_ty: IRTypeId,
    pc: IRVarId,
    types: &mut IRTypes,
) -> IRVarId {
    // 1. Re-read handler_idx from bytecode_storage.
    let handler_idx_var = b.push(Stmt::StorageRead {
        storage: bytecode_storage,
        ty: addr_ty,
        addr: pc,
    });

    // 2. Re-read all per-handler slot values in schema order.
    let mut inputs: Vec<(IRVarId, IRTypeId)> = Vec::with_capacity(1 + schema.slots.len());
    inputs.push((handler_idx_var, addr_ty));
    for (slot_idx, slot) in schema.slots.iter().enumerate() {
        let v = b.push(Stmt::StorageRead {
            storage: slot_ids[slot_idx],
            ty: slot.ty,
            addr: pc,
        });
        inputs.push((v, slot.ty));
    }

    // 3. Emit hash IR via BlockEmitter (borrows b + types for the duration).
    let computed = {
        let mut emitter = BlockEmitter { block: b, types, addr_ty, bit_ty };
        ctx.config.algorithm.emit_ir(&mut emitter, &inputs)
    };

    // 4. Read expected commitment from commitment_storage.
    let hash_ty = ctx.hash_output_ty;
    let expected = b.push(Stmt::StorageRead {
        storage: ctx.config.commitment_storage,
        ty: hash_ty,
        addr: pc,
    });

    // 5. diff = computed XOR expected (zero iff commitment is valid).
    let mut diff_coeffs: BTreeMap<Vec<IRVarId>, u8> = BTreeMap::new();
    diff_coeffs.insert(alloc::vec![computed], 1);
    diff_coeffs.insert(alloc::vec![expected], 1);
    let diff = b.push(Stmt::Poly {
        ty: hash_ty,
        coeffs: diff_coeffs,
        constant: Constant { hi: 0, lo: 0 },
    });

    // 6. Reinterpret diff as addr_ty (_32) for XOR-injection into next_pc.
    if hash_ty == addr_ty {
        diff
    } else {
        b.push(Stmt::Transmute { src: diff, src_ty: hash_ty, dst_ty: addr_ty })
    }
}

/// Build a [`CommitmentCtx`] by computing the native hash of every
/// bytecode entry in `cse_blocks`.
///
/// `hash_output_ty` must already be interned into the module's type table
/// (the caller computes it via `algorithm.output_type_id(types)` before
/// creating the context).
fn build_commitment_ctx<'a, P: Clone + Default, H: IrHashAlgorithm>(
    config: &'a CommitmentConfig<H>,
    cse_blocks: &IRBlocks<P>,
    dedup: &DedupTable<IrHandlerKey>,
    layout: &GlobalLayout,
    reg_alloc: &RegAlloc,
    ir_types: &[IRType],
    hash_output_ty: IRTypeId,
) -> CommitmentCtx<'a, H> {

    let mut per_block: Vec<Constant> = Vec::with_capacity(cse_blocks.blocks.len());
    for (block_id, block) in cse_blocks.blocks.iter().enumerate() {
        let (handler_idx, _) = &dedup.per_block[block_id];
        let schema = &layout.schemas[*handler_idx as usize];
        let slot_values = compute_slot_values(block, block_id, schema, reg_alloc, ir_types);

        // Serialise: handler_idx (4 bytes LE) followed by each slot value
        // in the canonical little-endian encoding for its IR type.
        let mut words: Vec<Vec<u8>> = Vec::with_capacity(1 + schema.slots.len());
        words.push((*handler_idx as u32).to_le_bytes().to_vec());
        for (slot_idx, slot) in schema.slots.iter().enumerate() {
            let ir_ty = &ir_types[slot.ty.0 as usize];
            words.push(constant_to_le_bytes(&slot_values[slot_idx], ir_ty));
        }

        let word_slices: Vec<&[u8]> = words.iter().map(|v| v.as_slice()).collect();
        let hash_bytes = config.algorithm.hash_bytes_native(&word_slices);
        per_block.push(bytes_to_constant(&hash_bytes));
    }

    CommitmentCtx { config, per_block, hash_output_ty }
}

// Silence the unused-SETUP_BID warning; kept for clarity in the
// documentation above.
#[allow(dead_code)]
const _SETUP_BID: u32 = SETUP_BID;
