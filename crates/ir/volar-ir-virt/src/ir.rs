// @reliability: experimental
// @ai: assisted
//! Virtualisation entry point for [`IRBlocks`].

use alloc::{collections::BTreeMap, vec, vec::Vec};

use volar_ir::ir::{
    IRBlock, IRBlockId, IRBlockTargetId, IRBlocks, IRStmt, IRTerminator, IRType, IRTypeId, IRTypes,
    IRVarId,
};
use volar_ir_common::{Constant, Stmt, StorageId, Type as PrimType};

use crate::canon::{canonicalize_ir_block, BlockImmediates, ImmediateKind, IrHandlerKey};
use crate::ctx::{DedupTable, VirtOutput};
use crate::{DedupPolicy, DispatchMode, VirtualizeConfig};

// ============================================================================
// Public API
// ============================================================================

/// Virtualise an [`IRBlocks`] module.
///
/// # Preconditions
/// * All input blocks must share the same parameter signature (same
///   param count, same [`IRTypeId`] at each position).  Panics otherwise.
/// * No input block may read from or write to any [`StorageId`] colliding
///   with the bytecode storage range
///   `[cfg.bytecode_storage, cfg.bytecode_storage + n_slots]`.  Panics
///   otherwise.
/// * No terminator may use `IRBlockTargetId::Dyn` in v1.  Panics.
///
/// # Returns
/// A [`VirtOutput`] holding the rewritten module.  If
/// [`BytecodeForm::wants_external`] is true the bytecode artefact is
/// also returned.
pub fn virtualize_ir<P: Clone + Default>(
    blocks: &IRBlocks<P>,
    types: &mut IRTypes,
    cfg: &VirtualizeConfig,
) -> VirtOutput<IRBlocks<P>> {
    assert!(
        matches!(cfg.dedup, DedupPolicy::ConstantsAndTargets),
        "volar-ir-virt v1 only implements DedupPolicy::ConstantsAndTargets"
    );
    assert!(
        !blocks.blocks.is_empty(),
        "virtualize_ir: input has no blocks"
    );

    let blocks_in = blocks.blocks.len();
    let common_params = blocks.blocks[0].params.clone();

    validate_input(blocks, &common_params);

    // Canonicalise every block.
    let per_block_canon: Vec<(IrHandlerKey, BlockImmediates)> =
        blocks.blocks.iter().map(canonicalize_ir_block).collect();

    let dedup = DedupTable::build(per_block_canon);
    let n_handlers = dedup.n_handlers();

    // Intern the address type we will use for pc and block targets (_32).
    let addr_ty = types.intern(IRType::Primitive(PrimType::_32));

    // Compute the global slot layout: for each (handler, slot_in_h) we
    // assign a unique global slot id.
    let layout = GlobalSlotLayout::from_dedup(&dedup, addr_ty);

    // Emit output IR.
    let out_blocks = emit_output_ir(
        blocks,
        &common_params,
        &dedup,
        &layout,
        addr_ty,
        cfg,
        types,
    );

    let bytecode = if cfg.bytecode_form.wants_external() {
        Some(dedup.to_bytecode())
    } else {
        None
    };

    // For Oblivious dispatch we first emit the Public-dispatch output,
    // then hand it to `movfuscate_ir` which collapses the whole module
    // (setup + dispatcher + handlers) into a single self-looping block
    // with the per-slot accumulator machinery.  This reuses the
    // formulas refactored into `dispatch_accumulator` rather than
    // re-implementing them here.
    let final_blocks = match cfg.dispatch {
        DispatchMode::Public => out_blocks,
        DispatchMode::Oblivious => {
            volar_ir_passes::movfuscate_ir(&out_blocks, types)
        }
    };

    VirtOutput {
        blocks: final_blocks,
        bytecode,
        n_handlers,
        blocks_in,
    }
}

// ============================================================================
// Input validation
// ============================================================================

fn validate_input<P: Clone + Default>(blocks: &IRBlocks<P>, common_params: &[IRTypeId]) {
    for (i, b) in blocks.blocks.iter().enumerate() {
        assert_eq!(
            b.params, common_params,
            "virtualize_ir: block {} has param signature {:?}, expected {:?}",
            i, b.params, common_params
        );
        match &b.terminator {
            IRTerminator::Jmp { func, .. } => {
                assert!(
                    !matches!(func, IRBlockTargetId::Dyn(_)),
                    "virtualize_ir: block {} uses Dyn jump target (unsupported in v1)",
                    i
                );
            }
            IRTerminator::JumpCond {
                true_block,
                false_block,
                ..
            } => {
                assert!(
                    !matches!(true_block, IRBlockTargetId::Dyn(_))
                        && !matches!(false_block, IRBlockTargetId::Dyn(_)),
                    "virtualize_ir: block {} uses Dyn jump target (unsupported in v1)",
                    i
                );
            }
            IRTerminator::JumpTable { cases, .. } => {
                for (_, (t, _)) in cases {
                    assert!(
                        !matches!(t, IRBlockTargetId::Dyn(_)),
                        "virtualize_ir: block {} uses Dyn jump target (unsupported in v1)",
                        i
                    );
                }
            }
        }
    }
}

// ============================================================================
// Global slot layout
// ============================================================================

/// Per-handler, per-slot storage assignment.
///
/// A *global slot id* `g` uniquely identifies `(handler_h, slot_in_h)`
/// and is assigned a dedicated [`StorageId`] =
/// `bytecode_storage + 1 + g`.  The handler_idx itself lives at
/// `bytecode_storage + 0`.
struct GlobalSlotLayout {
    /// `per_handler[h][k] = global_slot_id` for handler `h`'s k-th slot.
    per_handler: Vec<Vec<u32>>,
}

impl GlobalSlotLayout {
    fn from_dedup(dedup: &DedupTable<IrHandlerKey>, addr_ty: IRTypeId) -> Self {
        let mut per_handler: Vec<Vec<u32>> = Vec::with_capacity(dedup.handler_keys.len());
        let mut next_gid: u32 = 0;

        for key in &dedup.handler_keys {
            let schema = key.typed_immediate_schema(addr_ty);
            let mut ids = Vec::with_capacity(schema.len());
            for _slot in schema {
                ids.push(next_gid);
                next_gid += 1;
            }
            per_handler.push(ids);
        }

        Self { per_handler }
    }

    /// StorageId for the k-th global slot (offset by the handler_idx slot).
    fn slot_storage(&self, base: StorageId, g: u32) -> StorageId {
        StorageId(base.0 + 1 + g)
    }
}

// ============================================================================
// Output emission (Public dispatch)
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

    /// Push a statement and return the IRVarId it defines.
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

fn emit_output_ir<P: Clone + Default>(
    _blocks_in: &IRBlocks<P>,
    common_params: &[IRTypeId],
    dedup: &DedupTable<IrHandlerKey>,
    layout: &GlobalSlotLayout,
    addr_ty: IRTypeId,
    cfg: &VirtualizeConfig,
    _types: &mut IRTypes,
) -> IRBlocks<P> {
    // Block layout (indices after emission):
    //   0: setup (or a trivial setup that just initialises pc when
    //      BytecodeForm::External)
    //   1: dispatcher
    //   2..2+H: handlers
    let n_handlers = dedup.handler_keys.len();
    let dispatcher_id: IRBlockId = IRBlockId(1);
    let handler_ids: Vec<IRBlockId> = (0..n_handlers as u32)
        .map(|h| IRBlockId(2 + h))
        .collect();

    // ---- Setup block -----------------------------------------------------
    let setup = emit_setup_block(
        common_params,
        dedup,
        layout,
        addr_ty,
        cfg,
        dispatcher_id,
    );

    // ---- Dispatcher block ------------------------------------------------
    let dispatcher = emit_dispatcher_block(
        common_params,
        addr_ty,
        cfg.bytecode_storage,
        &handler_ids,
        n_handlers,
    );

    // ---- Handler blocks --------------------------------------------------
    let mut handler_blocks: Vec<IRBlock<P>> = Vec::with_capacity(n_handlers);
    for (h_idx, key) in dedup.handler_keys.iter().enumerate() {
        let slots = &layout.per_handler[h_idx];
        let h_block = emit_handler_block::<P>(
            key,
            common_params,
            addr_ty,
            slots,
            layout,
            cfg.bytecode_storage,
            dispatcher_id,
        );
        handler_blocks.push(h_block);
    }

    let mut out_blocks: Vec<IRBlock<P>> = Vec::with_capacity(2 + n_handlers);
    out_blocks.push(setup);
    out_blocks.push(dispatcher);
    out_blocks.extend(handler_blocks);

    IRBlocks {
        oracles: _blocks_in.oracles.clone(),
        actions: _blocks_in.actions.clone(),
        rngs: _blocks_in.rngs.clone(),
        blocks: out_blocks,
    }
}

fn emit_setup_block<P: Clone + Default>(
    common_params: &[IRTypeId],
    dedup: &DedupTable<IrHandlerKey>,
    layout: &GlobalSlotLayout,
    addr_ty: IRTypeId,
    cfg: &VirtualizeConfig,
    dispatcher_id: IRBlockId,
) -> IRBlock<P> {
    let mut b = IRBlockUnfinished::new(common_params.to_vec());

    // Allocate a pc=0 value (always needed to enter the dispatcher).
    let pc_zero = b.push(IRStmt::Const(Constant { hi: 0, lo: 0 }, addr_ty));

    if cfg.bytecode_form.wants_in_ir() {
        // For each pc row, write handler_idx + per-slot immediates into
        // the bytecode storage.
        for (pc, (h_idx, imm)) in dedup.per_block.iter().enumerate() {
            let pc_var = b.push(IRStmt::Const(
                Constant {
                    hi: 0,
                    lo: pc as u128,
                },
                addr_ty,
            ));

            // Write handler_idx at (bytecode_storage, addr_ty, pc).
            let hidx_var = b.push(IRStmt::Const(
                Constant {
                    hi: 0,
                    lo: *h_idx as u128,
                },
                addr_ty,
            ));
            b.push(IRStmt::StorageWrite {
                storage: cfg.bytecode_storage,
                src: hidx_var,
                ty: addr_ty,
                addr: pc_var,
            });

            // Write each immediate into its dedicated storage.  Only the
            // slots used by this row's handler are written; others stay
            // at zero.
            let handler_slots = &layout.per_handler[*h_idx as usize];
            let schema = dedup.handler_keys[*h_idx as usize].typed_immediate_schema(addr_ty);
            let mut imm_iter_consts = imm.consts.iter().copied();
            let mut imm_iter_targets = imm.targets.iter().copied();
            for (slot_idx, slot) in schema.iter().enumerate() {
                let gid = handler_slots[slot_idx];
                let storage = layout.slot_storage(cfg.bytecode_storage, gid);
                let value_const = match slot.kind {
                    ImmediateKind::Constant => imm_iter_consts
                        .next()
                        .expect("immediate schema/consts mismatch"),
                    ImmediateKind::BlockTarget => {
                        let id = imm_iter_targets
                            .next()
                            .expect("immediate schema/targets mismatch");
                        Constant {
                            hi: 0,
                            lo: id.0 as u128,
                        }
                    }
                };
                let val_var = b.push(IRStmt::Const(value_const, slot.ty));
                b.push(IRStmt::StorageWrite {
                    storage,
                    src: val_var,
                    ty: slot.ty,
                    addr: pc_var,
                });
            }
        }
    }

    // Enter the dispatcher with the entry state and pc=0.
    let param_vars: Vec<IRVarId> = (0..common_params.len() as u32).map(IRVarId).collect();
    let mut disp_args = param_vars;
    disp_args.push(pc_zero);
    b.terminator = IRTerminator::Jmp {
        func: IRBlockTargetId::Block(dispatcher_id),
        args: disp_args,
    };
    b.into_ir_block::<P>()
}

fn emit_dispatcher_block<P: Clone + Default>(
    common_params: &[IRTypeId],
    addr_ty: IRTypeId,
    base_storage: StorageId,
    handler_ids: &[IRBlockId],
    n_handlers: usize,
) -> IRBlock<P> {
    let mut params = common_params.to_vec();
    params.push(addr_ty);
    let mut b = IRBlockUnfinished::new(params);

    let pc_var = IRVarId(common_params.len() as u32);

    // handler_idx = StorageRead((base_storage, addr_ty, pc))
    let handler_idx = b.push(IRStmt::StorageRead {
        storage: base_storage,
        ty: addr_ty,
        addr: pc_var,
    });

    // Build the jump table: one case per handler.
    let mut state_args: Vec<IRVarId> = (0..common_params.len() as u32).map(IRVarId).collect();
    state_args.push(pc_var);

    let mut cases: BTreeMap<Constant, (IRBlockTargetId, Vec<IRVarId>)> = BTreeMap::new();
    for h in 0..n_handlers {
        let target = IRBlockTargetId::Block(handler_ids[h]);
        cases.insert(
            Constant {
                hi: 0,
                lo: h as u128,
            },
            (target, state_args.clone()),
        );
    }

    b.terminator = IRTerminator::JumpTable {
        index: handler_idx,
        cases,
    };
    b.into_ir_block::<P>()
}

fn emit_handler_block<P: Clone + Default>(
    key: &IrHandlerKey,
    common_params: &[IRTypeId],
    addr_ty: IRTypeId,
    handler_slots: &[u32],
    layout: &GlobalSlotLayout,
    base_storage: StorageId,
    dispatcher_id: IRBlockId,
) -> IRBlock<P> {
    let mut params = common_params.to_vec();
    params.push(addr_ty);
    let mut b = IRBlockUnfinished::new(params);

    let n_params = common_params.len() as u32;
    let pc_var = IRVarId(n_params);

    // Step 1: read every immediate slot this handler needs.
    // schema[k] corresponds to handler_slots[k].
    let schema = key.typed_immediate_schema(addr_ty);
    let mut imm_vars: Vec<IRVarId> = Vec::with_capacity(schema.len());
    for (k, slot) in schema.iter().enumerate() {
        let gid = handler_slots[k];
        let storage = layout.slot_storage(base_storage, gid);
        let v = b.push(IRStmt::StorageRead {
            storage,
            ty: slot.ty,
            addr: pc_var,
        });
        imm_vars.push(v);
    }

    // Step 2: re-emit the canonical stmts, mapping canonical var ids to
    // new ids.  `canonical -> new` map; a lifted Const stmt aliases to
    // the corresponding imm_var (no new stmt emitted).
    let mut canon_to_new: BTreeMap<IRVarId, IRVarId> = BTreeMap::new();

    // Params in the canonical key occupy canonical ids 0..n_params.
    for i in 0..n_params {
        canon_to_new.insert(IRVarId(i), IRVarId(i));
    }

    let mut const_imm_idx: usize = 0;
    let n_consts_in_schema = schema
        .iter()
        .take_while(|s| matches!(s.kind, ImmediateKind::Constant))
        .count();
    // All Constant slots come before BlockTarget slots in
    // `typed_immediate_schema` by construction.
    debug_assert!({
        let mut seen_target = false;
        let mut ok = true;
        for s in &schema {
            match s.kind {
                ImmediateKind::Constant if seen_target => {
                    ok = false;
                    break;
                }
                ImmediateKind::BlockTarget => seen_target = true,
                _ => {}
            }
        }
        ok
    });

    for (s_idx, stmt) in key.stmts.iter().enumerate() {
        let canonical_var = IRVarId(n_params + s_idx as u32);
        match stmt {
            IRStmt::Const(_, _) => {
                // Lifted — alias to the imm var.
                let imm_var = imm_vars[const_imm_idx];
                const_imm_idx += 1;
                canon_to_new.insert(canonical_var, imm_var);
            }
            other => {
                let new_stmt = remap_stmt(other, &canon_to_new);
                let new_var = b.push(new_stmt);
                canon_to_new.insert(canonical_var, new_var);
            }
        }
    }
    let _ = n_consts_in_schema; // retained for debug assertion readability

    // Step 3: build the terminator.  Target slots start at
    // imm_vars[n_consts_in_schema..].
    let target_imm_vars = &imm_vars[const_imm_idx..];
    let common_state: Vec<IRVarId> = (0..n_params).map(IRVarId).collect();
    let new_term = rewrite_terminator(
        &key.terminator,
        &canon_to_new,
        target_imm_vars,
        &common_state,
        dispatcher_id,
    );
    b.terminator = new_term;

    b.into_ir_block::<P>()
}

// ============================================================================
// Stmt operand remapping
// ============================================================================

fn remap_var(v: IRVarId, map: &BTreeMap<IRVarId, IRVarId>) -> IRVarId {
    map.get(&v).copied().unwrap_or(v)
}

fn remap_vars(vs: &[IRVarId], map: &BTreeMap<IRVarId, IRVarId>) -> Vec<IRVarId> {
    vs.iter().map(|v| remap_var(*v, map)).collect()
}

fn remap_coeffs(
    coeffs: &BTreeMap<Vec<IRVarId>, u8>,
    map: &BTreeMap<IRVarId, IRVarId>,
) -> BTreeMap<Vec<IRVarId>, u8> {
    let mut out: BTreeMap<Vec<IRVarId>, u8> = BTreeMap::new();
    for (key, &c) in coeffs {
        let mut new_key: Vec<IRVarId> = key.iter().map(|v| remap_var(*v, map)).collect();
        new_key.sort();
        let entry = out.entry(new_key).or_insert(0);
        *entry ^= c;
    }
    out.retain(|_, c| *c != 0);
    out
}

fn remap_stmt(s: &IRStmt, map: &BTreeMap<IRVarId, IRVarId>) -> IRStmt {
    match s {
        Stmt::StorageRead { storage, ty, addr } => Stmt::StorageRead {
            storage: *storage,
            ty: *ty,
            addr: remap_var(*addr, map),
        },
        Stmt::StorageWrite { storage, src, ty, addr } => Stmt::StorageWrite {
            storage: *storage,
            src: remap_var(*src, map),
            ty: *ty,
            addr: remap_var(*addr, map),
        },
        Stmt::Const(c, ty) => Stmt::Const(*c, *ty),
        Stmt::Transmute { src, src_ty, dst_ty } => Stmt::Transmute {
            src: remap_var(*src, map),
            src_ty: *src_ty,
            dst_ty: *dst_ty,
        },
        Stmt::Poly { ty, coeffs, constant } => Stmt::Poly {
            ty: *ty,
            coeffs: remap_coeffs(coeffs, map),
            constant: *constant,
        },
        Stmt::Rol { src, ty, n } => Stmt::Rol {
            src: remap_var(*src, map),
            ty: *ty,
            n: *n,
        },
        Stmt::Ror { src, ty, n } => Stmt::Ror {
            src: remap_var(*src, map),
            ty: *ty,
            n: *n,
        },
        Stmt::Merge { parts, ty } => Stmt::Merge {
            parts: remap_vars(parts, map),
            ty: *ty,
        },
        Stmt::Splat { src, ty } => Stmt::Splat {
            src: remap_var(*src, map),
            ty: *ty,
        },
        Stmt::Shuffle { result_bits, ty } => Stmt::Shuffle {
            result_bits: result_bits
                .iter()
                .map(|(b, v)| (*b, remap_var(*v, map)))
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
            args: remap_vars(args, map),
            output_tys: output_tys.clone(),
            result_ty: *result_ty,
        },
        Stmt::OracleOutput { call, idx, ty } => Stmt::OracleOutput {
            call: remap_var(*call, map),
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
            guard: remap_var(*guard, map),
            args: remap_vars(args, map),
            fallbacks: remap_vars(fallbacks, map),
            output_tys: output_tys.clone(),
            result_ty: *result_ty,
        },
        Stmt::ActionOutput { call, idx, ty } => Stmt::ActionOutput {
            call: remap_var(*call, map),
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
// Terminator rewriting
// ============================================================================

/// Given the canonical terminator (with zeroed block ids and original var
/// ids), and the per-target immediate vars from storage, build the new
/// terminator that jumps back to the dispatcher.
fn rewrite_terminator(
    term: &IRTerminator,
    map: &BTreeMap<IRVarId, IRVarId>,
    target_imm_vars: &[IRVarId],
    state_vars: &[IRVarId],
    dispatcher_id: IRBlockId,
) -> IRTerminator {
    let to_dispatcher = |target_pc: IRVarId, extra_args: &[IRVarId]| -> (IRBlockTargetId, Vec<IRVarId>) {
        // Jump args: [remapped extra_args..., target_pc]
        let mut args: Vec<IRVarId> = extra_args
            .iter()
            .map(|v| remap_var(*v, map))
            .collect();
        // Note: args from canonical terminator are the "forward args" for
        // the original block params.  In v1, all blocks share the same
        // param signature so the dispatcher's state params are those
        // same types.  The extra args are consumed by the dispatcher as
        // the new state.  We keep exactly n_params args (falling back to
        // the current state vars when the canonical term supplied fewer,
        // which happens for Return-targeted terminators).
        args.truncate(state_vars.len());
        while args.len() < state_vars.len() {
            args.push(state_vars[args.len()]);
        }
        args.push(target_pc);
        (IRBlockTargetId::Block(dispatcher_id), args)
    };

    match term {
        IRTerminator::Jmp { func, args } => match func {
            IRBlockTargetId::Block(_) => {
                // Lifted target — single BlockTarget slot.
                let pc = target_imm_vars[0];
                let (t, a) = to_dispatcher(pc, args);
                IRTerminator::Jmp { func: t, args: a }
            }
            IRBlockTargetId::Return => {
                // Plain return — pass args through, remapped.
                IRTerminator::Jmp {
                    func: IRBlockTargetId::Return,
                    args: remap_vars(args, map),
                }
            }
            IRBlockTargetId::Dyn(_) => unreachable!("validated away upstream"),
        },
        IRTerminator::JumpCond {
            condition,
            true_block,
            true_args,
            false_block,
            false_args,
        } => {
            // Enumerate which of true/false was a Block(_) to index into
            // target_imm_vars in the correct order.  The canonicalizer
            // pushes true then false, skipping Returns.
            let mut imm_cursor = 0usize;
            let (t_target, t_args) = match true_block {
                IRBlockTargetId::Block(_) => {
                    let pc = target_imm_vars[imm_cursor];
                    imm_cursor += 1;
                    to_dispatcher(pc, true_args)
                }
                IRBlockTargetId::Return => (
                    IRBlockTargetId::Return,
                    remap_vars(true_args, map),
                ),
                IRBlockTargetId::Dyn(_) => unreachable!(),
            };
            let (f_target, f_args) = match false_block {
                IRBlockTargetId::Block(_) => {
                    let pc = target_imm_vars[imm_cursor];
                    to_dispatcher(pc, false_args)
                }
                IRBlockTargetId::Return => (
                    IRBlockTargetId::Return,
                    remap_vars(false_args, map),
                ),
                IRBlockTargetId::Dyn(_) => unreachable!(),
            };
            IRTerminator::JumpCond {
                condition: remap_var(*condition, map),
                true_block: t_target,
                true_args: t_args,
                false_block: f_target,
                false_args: f_args,
            }
        }
        IRTerminator::JumpTable { index, cases } => {
            let mut imm_cursor = 0usize;
            let mut new_cases: BTreeMap<Constant, (IRBlockTargetId, Vec<IRVarId>)> =
                BTreeMap::new();
            for (k, (t, a)) in cases {
                let (tt, tt_args) = match t {
                    IRBlockTargetId::Block(_) => {
                        let pc = target_imm_vars[imm_cursor];
                        imm_cursor += 1;
                        to_dispatcher(pc, a)
                    }
                    IRBlockTargetId::Return => (
                        IRBlockTargetId::Return,
                        remap_vars(a, map),
                    ),
                    IRBlockTargetId::Dyn(_) => unreachable!(),
                };
                new_cases.insert(*k, (tt, tt_args));
            }
            IRTerminator::JumpTable {
                index: remap_var(*index, map),
                cases: new_cases,
            }
        }
    }
}
