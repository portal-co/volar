// @reliability: experimental
// @ai: assisted
//! Virtualisation entry point for [`BIrBlocks`].
//!
//! BIR has no `Stmt::Const` variant (boolean constants are the
//! structural `BIrStmt::Zero`/`BIrStmt::One` gates) so the v1 pass only
//! lifts jump target block ids.  BIR also has no `JumpTable` terminator,
//! so public dispatch is emitted as a balanced binary tree of
//! `CondJmp` over the bits of the handler index.

use alloc::{collections::BTreeMap, vec, vec::Vec};

use volar_ir::{
    boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator},
    ir::{IRBlockId, IRBlockTargetId, IRVarId},
};
use volar_ir_common::StorageId;

use crate::canon::{canonicalize_bir_block, BirHandlerKey, BlockImmediates};
use crate::ctx::{DedupTable, VirtOutput};
use crate::{DedupPolicy, DispatchMode, VirtualizeConfig};

// ============================================================================
// Public API
// ============================================================================

/// Virtualise a [`BIrBlocks`] module.
///
/// # Preconditions
/// * All blocks must share the same parameter count (BIR params are
///   always `Bit`).  Panics otherwise.
/// * No terminator may target `IRBlockTargetId::Dyn`.  Panics.
/// * The input must not read or write any `StorageId` in the bytecode
///   range `[cfg.bytecode_storage, cfg.bytecode_storage + total_slots]`.
///   Caller responsibility.
pub fn virtualize_bir<P: Clone + Default>(
    blocks: &BIrBlocks<P>,
    cfg: &VirtualizeConfig,
) -> VirtOutput<BIrBlocks<P>> {
    assert!(
        matches!(cfg.dedup, DedupPolicy::ConstantsAndTargets),
        "volar-ir-virt v1 only implements DedupPolicy::ConstantsAndTargets"
    );
    assert!(!blocks.0.is_empty(), "virtualize_bir: input has no blocks");

    let blocks_in = blocks.0.len();
    let common_params = blocks.0[0].params;

    validate_input(blocks, common_params);

    // CSE: merge duplicate OracleCall stmts within each block before
    // canonicalisation.
    let cse_blocks = BIrBlocks(
        blocks.0.iter().map(deduplicate_bir_oracle_calls_in_block).collect(),
    );

    // Canonicalise every block.
    let per_block_canon: Vec<(BirHandlerKey, BlockImmediates)> =
        cse_blocks.0.iter().map(canonicalize_bir_block).collect();

    let dedup = DedupTable::build(per_block_canon);
    let n_handlers = dedup.n_handlers();

    // Bits needed to encode handler_idx.
    let pc_bits = bits_needed(blocks_in);
    let handler_bits = bits_needed(n_handlers);

    // One BIR "slot" per bit: pc is an addr Vec of length pc_bits;
    // handler_idx is handler_bits bits at StorageIds 0..handler_bits
    // (relative to bytecode_storage); each BlockTarget slot is pc_bits
    // wide at subsequent StorageIds.

    // Compute per-handler slot layout (just target slots for BIR v1).
    let layout = BirSlotLayout::from_dedup(&dedup, handler_bits, pc_bits, cfg.bytecode_storage);

    // Emit output BIR.
    let out_blocks = emit_output_bir::<P>(
        &cse_blocks,
        common_params,
        &dedup,
        &layout,
        cfg,
        pc_bits,
        handler_bits,
    );

    let bytecode = if cfg.bytecode_form.wants_external() {
        Some(dedup.to_bytecode())
    } else {
        None
    };

    // Oblivious dispatch: hand the Public-dispatch output to
    // `movfuscate_biir`, which collapses it to a single self-looping
    // block using the shared slot-accumulator helpers.
    let final_blocks = match cfg.dispatch {
        DispatchMode::Public => out_blocks,
        DispatchMode::Oblivious => volar_ir_passes::movfuscate_biir(&out_blocks),
    };

    VirtOutput {
        blocks: final_blocks,
        bytecode,
        n_handlers,
        blocks_in,
    }
}

// ============================================================================
// Helpers
// ============================================================================

/// Minimum number of bits to encode `n` distinct values (0 for n ≤ 1).
fn bits_needed(n: usize) -> usize {
    if n <= 1 {
        return 0;
    }
    (usize::BITS - (n - 1).leading_zeros()) as usize
}

fn validate_input<P: Clone + Default>(blocks: &BIrBlocks<P>, common_params: u32) {
    for (i, b) in blocks.0.iter().enumerate() {
        assert_eq!(
            b.params, common_params,
            "virtualize_bir: block {} has {} params, expected {}",
            i, b.params, common_params
        );
        match &b.terminator {
            BIrTerminator::Jmp(t) => {
                assert!(
                    !matches!(t.block, IRBlockTargetId::Dyn(_)),
                    "virtualize_bir: block {} uses Dyn target (unsupported in v1)",
                    i
                );
            }
            BIrTerminator::CondJmp {
                then_target,
                else_target,
                ..
            } => {
                assert!(
                    !matches!(then_target.block, IRBlockTargetId::Dyn(_))
                        && !matches!(else_target.block, IRBlockTargetId::Dyn(_)),
                    "virtualize_bir: block {} uses Dyn target (unsupported in v1)",
                    i
                );
            }
        }
    }
}

// ============================================================================
// Slot layout (BIR target-only in v1)
// ============================================================================

struct BirSlotLayout {
    /// `per_handler[h]` = list of target-slot base storage ids (each is
    /// a `pc_bits`-wide stored value).
    per_handler: Vec<Vec<StorageId>>,
}

impl BirSlotLayout {
    fn from_dedup(
        dedup: &DedupTable<BirHandlerKey>,
        handler_bits: usize,
        pc_bits: usize,
        base: StorageId,
    ) -> Self {
        let mut per_handler: Vec<Vec<StorageId>> = Vec::with_capacity(dedup.handler_keys.len());
        // handler_idx occupies storages [base .. base + handler_bits);
        // each subsequent target slot occupies `pc_bits` StorageIds (one
        // per bit of the encoded target).
        let mut next_slot: u32 = base.0 + handler_bits as u32;
        for key in &dedup.handler_keys {
            let mut slots = Vec::new();
            let n_targets = count_targets(&key.terminator);
            for _ in 0..n_targets {
                slots.push(StorageId(next_slot));
                next_slot += pc_bits as u32;
            }
            per_handler.push(slots);
        }
        Self { per_handler }
    }
}

fn count_targets(term: &BIrTerminator) -> usize {
    match term {
        BIrTerminator::Jmp(t) => match t.block {
            IRBlockTargetId::Block(_) => 1,
            _ => 0,
        },
        BIrTerminator::CondJmp {
            then_target,
            else_target,
            ..
        } => {
            let mut n = 0;
            if matches!(then_target.block, IRBlockTargetId::Block(_)) {
                n += 1;
            }
            if matches!(else_target.block, IRBlockTargetId::Block(_)) {
                n += 1;
            }
            n
        }
    }
}

// ============================================================================
// Output emission
// ============================================================================

struct BirBlockUnfinished {
    params: u32,
    stmts: Vec<BIrStmt>,
    terminator: BIrTerminator,
}

impl BirBlockUnfinished {
    fn new(params: u32) -> Self {
        Self {
            params,
            stmts: Vec::new(),
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: Vec::new(),
            }),
        }
    }

    fn push(&mut self, s: BIrStmt) -> IRVarId {
        let id = IRVarId(self.params + self.stmts.len() as u32);
        self.stmts.push(s);
        id
    }

    fn into_bir_block<P: Clone + Default>(self) -> BIrBlock<P> {
        let n = self.stmts.len();
        BIrBlock {
            params: self.params,
            stmts: self.stmts,
            stmt_provs: vec![P::default(); n],
            terminator: self.terminator,
        }
    }
}

fn emit_output_bir<P: Clone + Default>(
    _blocks_in: &BIrBlocks<P>,
    common_params: u32,
    dedup: &DedupTable<BirHandlerKey>,
    layout: &BirSlotLayout,
    cfg: &VirtualizeConfig,
    pc_bits: usize,
    handler_bits: usize,
) -> BIrBlocks<P> {
    // Block layout:
    //   0: setup
    //   1: dispatcher
    //   2..2+n_dispatch_nodes: dispatcher tree interior nodes (if handler_bits > 1)
    //   ...: handler blocks
    //
    // For simplicity we materialise the dispatcher tree as a single
    // "dispatcher entry" block followed by (handler_bits - 1) interior
    // CondJmp nodes and then the handler blocks.  For handler_bits == 0
    // there is a single handler and dispatch is an unconditional Jmp.
    // For handler_bits == 1 we only need the entry (one CondJmp).
    //
    // To keep the layout predictable we emit in this order:
    //   0: setup
    //   1: dispatcher (reads handler_bits bits, then chains CondJmp
    //      using intermediate blocks if needed)
    //   2..2+interior: intermediate dispatcher nodes
    //   2+interior..2+interior+n_handlers: handler blocks

    let n_handlers = dedup.handler_keys.len();
    let setup_id = IRBlockId(0);
    let dispatcher_entry = IRBlockId(1);

    let n_interior = if handler_bits == 0 {
        0
    } else {
        // A full binary tree decoder reading `handler_bits` bits has
        // `2^handler_bits - 1` internal CondJmps.  The dispatcher entry
        // block is the root; the remaining `2^handler_bits - 2` are
        // interior blocks.  We only allocate entries for handlers we
        // actually use (so at most `n_handlers - 1` interior nodes).
        //
        // For a simple linear cascade, we use exactly `n_handlers - 1`
        // CondJmps: each level tests one predicate "is_handler_i" until
        // we reach the final one.  This is less balanced but much
        // simpler to emit.
        n_handlers.saturating_sub(1).saturating_sub(1)
    };

    let interior_base = 2u32;
    let handler_base = 2u32 + n_interior as u32;

    let handler_ids: Vec<IRBlockId> = (0..n_handlers as u32)
        .map(|h| IRBlockId(handler_base + h))
        .collect();
    let interior_ids: Vec<IRBlockId> = (0..n_interior as u32)
        .map(|i| IRBlockId(interior_base + i))
        .collect();

    // ---- Setup block -----------------------------------------------------
    let setup = emit_setup_block(
        common_params,
        dedup,
        layout,
        cfg,
        dispatcher_entry,
        pc_bits,
        handler_bits,
    );

    // ---- Dispatcher + interior nodes -------------------------------------
    let (dispatcher, interior_blocks) = emit_dispatcher_blocks::<P>(
        common_params,
        cfg.bytecode_storage,
        pc_bits,
        handler_bits,
        &handler_ids,
        &interior_ids,
    );

    // ---- Handler blocks --------------------------------------------------
    let mut handler_blocks: Vec<BIrBlock<P>> = Vec::with_capacity(n_handlers);
    for (h_idx, key) in dedup.handler_keys.iter().enumerate() {
        let h_block = emit_handler_block::<P>(
            key,
            common_params,
            &layout.per_handler[h_idx],
            pc_bits,
            dispatcher_entry,
        );
        handler_blocks.push(h_block);
    }

    let mut out_blocks: Vec<BIrBlock<P>> = Vec::new();
    out_blocks.push(setup);
    out_blocks.push(dispatcher);
    out_blocks.extend(interior_blocks);
    out_blocks.extend(handler_blocks);

    // Sanity-check that our block id allocation matches what we emit.
    debug_assert_eq!(out_blocks.len(), 2 + n_interior + n_handlers);
    let _ = setup_id;

    BIrBlocks(out_blocks)
}

// ============================================================================
// Setup block
// ============================================================================

fn emit_setup_block<P: Clone + Default>(
    common_params: u32,
    dedup: &DedupTable<BirHandlerKey>,
    layout: &BirSlotLayout,
    cfg: &VirtualizeConfig,
    dispatcher_id: IRBlockId,
    pc_bits: usize,
    handler_bits: usize,
) -> BIrBlock<P> {
    let mut b = BirBlockUnfinished::new(common_params);

    // zero/one constants we may need.
    let zero = b.push(BIrStmt::Zero);
    let one = b.push(BIrStmt::One);

    if cfg.bytecode_form.wants_in_ir() {
        // For each pc row, write handler_idx (encoded as handler_bits
        // bits) and every target slot (encoded as pc_bits bits).
        for (pc, (h_idx, imm)) in dedup.per_block.iter().enumerate() {
            let pc_addr = encode_bits_as_vars(pc, pc_bits, zero, one);

            // Write handler_idx bits.
            for k in 0..handler_bits {
                let bit = (*h_idx as usize >> k) & 1;
                let src = if bit == 1 { one } else { zero };
                b.push(BIrStmt::StorageWrite {
                    storage: StorageId(cfg.bytecode_storage.0 + k as u32),
                    src,
                    bit_width: 1,
                    addr: pc_addr.clone(),
                });
            }

            // Write target slot bits.  The target list for this handler
            // is `imm.targets`.
            let slots = &layout.per_handler[*h_idx as usize];
            for (slot_idx, tgt) in imm.targets.iter().enumerate() {
                let slot_storage = slots[slot_idx];
                for k in 0..pc_bits {
                    let bit = ((tgt.0 as usize) >> k) & 1;
                    let src = if bit == 1 { one } else { zero };
                    b.push(BIrStmt::StorageWrite {
                        storage: StorageId(slot_storage.0 + k as u32),
                        src,
                        bit_width: 1,
                        addr: pc_addr.clone(),
                    });
                }
            }
        }
    }

    // Jump to dispatcher with state ++ pc=0 (encoded as pc_bits zero-bits).
    let mut args: Vec<IRVarId> = (0..common_params).map(IRVarId).collect();
    for _ in 0..pc_bits {
        args.push(zero);
    }
    b.terminator = BIrTerminator::Jmp(BIrTarget {
        block: IRBlockTargetId::Block(dispatcher_id),
        args,
    });
    b.into_bir_block::<P>()
}

/// Emit Zero/One stmts picking the right constant for each bit of
/// `value` and return the resulting IRVarId vector (LSB-first).
///
/// The caller has already allocated shared `zero` and `one` vars.
fn encode_bits_as_vars(value: usize, n_bits: usize, zero: IRVarId, one: IRVarId) -> Vec<IRVarId> {
    let mut out = Vec::with_capacity(n_bits);
    for k in 0..n_bits {
        let bit = (value >> k) & 1;
        out.push(if bit == 1 { one } else { zero });
    }
    out
}

// ============================================================================
// Dispatcher
// ============================================================================

/// Emit the dispatcher entry block plus the interior cascade blocks.
///
/// The dispatcher reads `handler_bits` bits (`h_bit_0..h_bit_{K-1}`) and
/// then threads a linear cascade: test bit 0 XOR bit 1 XOR … but
/// pragmatically we just test `is_handler_0`, `is_handler_1`, … one at a
/// time using a small chain of `CondJmp`s with intermediate blocks
/// carrying the state + pc + remaining-bit-compares.
fn emit_dispatcher_blocks<P: Clone + Default>(
    common_params: u32,
    base_storage: StorageId,
    pc_bits: usize,
    handler_bits: usize,
    handler_ids: &[IRBlockId],
    interior_ids: &[IRBlockId],
) -> (BIrBlock<P>, Vec<BIrBlock<P>>) {
    // Dispatcher params: [state..., pc_bit_0..pc_bit_{pc_bits-1}].
    let dispatcher_param_count = common_params + pc_bits as u32;
    let mut entry = BirBlockUnfinished::new(dispatcher_param_count);

    // Read handler_bits bits from (base_storage + k, width=1, addr=pc).
    let pc_addr: Vec<IRVarId> = (common_params..common_params + pc_bits as u32)
        .map(IRVarId)
        .collect();

    let mut h_bits: Vec<IRVarId> = Vec::with_capacity(handler_bits);
    for k in 0..handler_bits {
        let v = entry.push(BIrStmt::StorageRead {
            storage: StorageId(base_storage.0 + k as u32),
            bit_width: 1,
            addr: pc_addr.clone(),
        });
        h_bits.push(v);
    }

    // For a linear cascade, we test against each handler's index in
    // sequence.  is_h_i = AND over bits of (h_bits[k] iff i_k).  For
    // handler 0 (all zeros), is_h_0 = AND over NOT(h_bits[k]).
    //
    // The cascade is: at step i, test is_h_i; true -> jump to handler
    // i; false -> continue to interior block i+1 (or handler n-1 if
    // last).
    //
    // Entry block does step 0; interior block k does step k+1.
    // Interior blocks carry the full state ++ pc as params.

    let n_handlers = handler_ids.len();
    let mut interior_blocks: Vec<BIrBlock<P>> = Vec::with_capacity(interior_ids.len());

    // Helper: emit `is_h_i` into a block-builder given its h_bits.  We
    // re-emit h_bits-derived work inside the dispatcher entry block; for
    // interior blocks we re-read from storage (simpler than threading
    // the h_bits through block params).
    //
    // That avoids growing the dispatcher param list by handler_bits.
    let emit_is_handler = |b: &mut BirBlockUnfinished, h_bits: &[IRVarId], i: usize| -> IRVarId {
        if h_bits.is_empty() {
            return b.push(BIrStmt::One);
        }
        let bit0 = if (i & 1) == 1 {
            h_bits[0]
        } else {
            b.push(BIrStmt::Not(h_bits[0]))
        };
        let mut acc = bit0;
        for j in 1..h_bits.len() {
            let bj = if (i >> j) & 1 == 1 {
                h_bits[j]
            } else {
                b.push(BIrStmt::Not(h_bits[j]))
            };
            acc = b.push(BIrStmt::And(acc, bj));
        }
        acc
    };

    // State ++ pc arg list (passed to handlers + to interior blocks).
    let state_pc_args: Vec<IRVarId> = (0..dispatcher_param_count).map(IRVarId).collect();

    if n_handlers == 0 {
        panic!("virtualize_bir: cannot dispatch zero handlers");
    } else if n_handlers == 1 {
        // Only one handler — unconditional Jmp.
        entry.terminator = BIrTerminator::Jmp(BIrTarget {
            block: IRBlockTargetId::Block(handler_ids[0]),
            args: state_pc_args.clone(),
        });
        return (entry.into_bir_block::<P>(), interior_blocks);
    }

    // Dispatcher entry: test is_handler_0.
    let is_h0 = emit_is_handler(&mut entry, &h_bits, 0);
    // Where do we go on "false"?
    let next_block_id = if n_handlers == 2 {
        // No interior needed; false goes straight to handler 1.
        handler_ids[1]
    } else {
        interior_ids[0]
    };
    entry.terminator = BIrTerminator::CondJmp {
        val: is_h0,
        then_target: BIrTarget {
            block: IRBlockTargetId::Block(handler_ids[0]),
            args: state_pc_args.clone(),
        },
        else_target: BIrTarget {
            block: IRBlockTargetId::Block(next_block_id),
            args: state_pc_args.clone(),
        },
    };

    // Interior cascade: each interior block re-reads handler_idx bits
    // and tests is_handler_{k+1}.
    for k in 0..interior_ids.len() {
        let step_handler = k + 1;
        let mut node = BirBlockUnfinished::new(dispatcher_param_count);
        let pc_addr_in_node: Vec<IRVarId> = (common_params..common_params + pc_bits as u32)
            .map(IRVarId)
            .collect();

        let mut h_bits_in_node = Vec::with_capacity(handler_bits);
        for kk in 0..handler_bits {
            let v = node.push(BIrStmt::StorageRead {
                storage: StorageId(base_storage.0 + kk as u32),
                bit_width: 1,
                addr: pc_addr_in_node.clone(),
            });
            h_bits_in_node.push(v);
        }
        let is_hk = emit_is_handler(&mut node, &h_bits_in_node, step_handler);

        let next = if k + 1 < interior_ids.len() {
            interior_ids[k + 1]
        } else {
            // Last interior: false goes to the final handler.
            handler_ids[n_handlers - 1]
        };

        node.terminator = BIrTerminator::CondJmp {
            val: is_hk,
            then_target: BIrTarget {
                block: IRBlockTargetId::Block(handler_ids[step_handler]),
                args: state_pc_args.clone(),
            },
            else_target: BIrTarget {
                block: IRBlockTargetId::Block(next),
                args: state_pc_args.clone(),
            },
        };
        interior_blocks.push(node.into_bir_block::<P>());
    }

    (entry.into_bir_block::<P>(), interior_blocks)
}

// ============================================================================
// Handler block
// ============================================================================

fn emit_handler_block<P: Clone + Default>(
    key: &BirHandlerKey,
    common_params: u32,
    target_slots: &[StorageId],
    pc_bits: usize,
    dispatcher_id: IRBlockId,
) -> BIrBlock<P> {
    // Handler params: [state..., pc_bit_0..pc_bit_{pc_bits-1}].
    let full_params = common_params + pc_bits as u32;
    let mut b = BirBlockUnfinished::new(full_params);

    let pc_addr: Vec<IRVarId> = (common_params..common_params + pc_bits as u32)
        .map(IRVarId)
        .collect();

    // Step 1: read each target slot as a pc_bits-wide bit-vector.
    // target_slot k: StorageId(slot_base + bit_k) for bit_k in 0..pc_bits.
    let mut target_vecs: Vec<Vec<IRVarId>> = Vec::with_capacity(target_slots.len());
    for slot_base in target_slots {
        let mut bits = Vec::with_capacity(pc_bits);
        for k in 0..pc_bits {
            let v = b.push(BIrStmt::StorageRead {
                storage: StorageId(slot_base.0 + k as u32),
                bit_width: 1,
                addr: pc_addr.clone(),
            });
            bits.push(v);
        }
        target_vecs.push(bits);
    }

    // Step 2: re-emit the canonical stmts.  BIR has no lifted-Const
    // placeholders, so this is a straight remap with canonical var ids
    // mapped to the new block's var ids.
    let n_params = common_params;
    let mut canon_to_new: BTreeMap<IRVarId, IRVarId> = BTreeMap::new();
    for i in 0..n_params {
        canon_to_new.insert(IRVarId(i), IRVarId(i));
    }

    for (s_idx, stmt) in key.stmts.iter().enumerate() {
        let canonical_var = IRVarId(n_params + s_idx as u32);
        let new_stmt = remap_bir_stmt(stmt, &canon_to_new);
        let new_var = b.push(new_stmt);
        canon_to_new.insert(canonical_var, new_var);
    }

    // Step 3: rewrite the terminator.  Any Block(_) target is replaced
    // by a Jmp to the dispatcher with state ++ target_vec as args.
    // Args passed via the canonical terminator are remapped and packed
    // into the dispatcher state (truncated/extended to common_params
    // count).
    let state_vars: Vec<IRVarId> = (0..n_params).map(IRVarId).collect();
    let new_term = rewrite_bir_terminator(
        &key.terminator,
        &canon_to_new,
        &target_vecs,
        &state_vars,
        dispatcher_id,
    );
    b.terminator = new_term;

    b.into_bir_block::<P>()
}

// ============================================================================
// Stmt and terminator remapping
// ============================================================================

// ============================================================================
// Oracle call CSE (pre-canonicalisation)
// ============================================================================

/// Merge duplicate `OracleCall` stmts within a single BIR block.
fn deduplicate_bir_oracle_calls_in_block<P: Clone + Default>(
    block: &BIrBlock<P>,
) -> BIrBlock<P> {
    let n_params = block.params as usize;
    let mut var_remap: BTreeMap<IRVarId, IRVarId> = BTreeMap::new();
    // (name, remapped-args) → first-call new var
    let mut seen: BTreeMap<(alloc::string::String, Vec<IRVarId>), IRVarId> = BTreeMap::new();
    let mut new_stmts: Vec<BIrStmt> = Vec::with_capacity(block.stmts.len());
    let mut new_provs: Vec<P> = Vec::with_capacity(block.stmts.len());

    let rv = |v: IRVarId, map: &BTreeMap<IRVarId, IRVarId>| -> IRVarId {
        map.get(&v).copied().unwrap_or(v)
    };

    for (stmt_idx, (s, prov)) in block.stmts.iter().zip(block.stmt_provs.iter()).enumerate() {
        let old_var = IRVarId((n_params + stmt_idx) as u32);
        match s {
            BIrStmt::OracleCall { name, args, num_bits } => {
                let remapped_args: Vec<IRVarId> =
                    args.iter().map(|v| rv(*v, &var_remap)).collect();
                let key = (name.clone(), remapped_args.clone());
                if let Some(&first_var) = seen.get(&key) {
                    var_remap.insert(old_var, first_var);
                } else {
                    let new_var = IRVarId((n_params + new_stmts.len()) as u32);
                    seen.insert(key, new_var);
                    var_remap.insert(old_var, new_var);
                    new_stmts.push(BIrStmt::OracleCall {
                        name: name.clone(),
                        args: remapped_args,
                        num_bits: *num_bits,
                    });
                    new_provs.push(prov.clone());
                }
            }
            other => {
                let new_var = IRVarId((n_params + new_stmts.len()) as u32);
                var_remap.insert(old_var, new_var);
                new_stmts.push(remap_bir_stmt(other, &var_remap));
                new_provs.push(prov.clone());
            }
        }
    }
    let new_terminator = remap_bir_terminator_vars(&block.terminator, &var_remap);
    BIrBlock {
        params: block.params,
        stmts: new_stmts,
        stmt_provs: new_provs,
        terminator: new_terminator,
    }
}

fn remap_bir_terminator_vars(
    t: &BIrTerminator,
    map: &BTreeMap<IRVarId, IRVarId>,
) -> BIrTerminator {
    let rv = |v: IRVarId| map.get(&v).copied().unwrap_or(v);
    let rt = |tgt: &BIrTarget| BIrTarget {
        block: tgt.block.clone(),
        args: tgt.args.iter().map(|v| rv(*v)).collect(),
    };
    match t {
        BIrTerminator::Jmp(tgt) => BIrTerminator::Jmp(rt(tgt)),
        BIrTerminator::CondJmp { val, then_target, else_target } => BIrTerminator::CondJmp {
            val: rv(*val),
            then_target: rt(then_target),
            else_target: rt(else_target),
        },
    }
}

fn remap_v(v: IRVarId, map: &BTreeMap<IRVarId, IRVarId>) -> IRVarId {
    map.get(&v).copied().unwrap_or(v)
}

fn remap_vs(vs: &[IRVarId], map: &BTreeMap<IRVarId, IRVarId>) -> Vec<IRVarId> {
    vs.iter().map(|v| remap_v(*v, map)).collect()
}

fn remap_bir_stmt(s: &BIrStmt, map: &BTreeMap<IRVarId, IRVarId>) -> BIrStmt {
    match s {
        BIrStmt::Zero => BIrStmt::Zero,
        BIrStmt::One => BIrStmt::One,
        BIrStmt::And(a, b) => BIrStmt::And(remap_v(*a, map), remap_v(*b, map)),
        BIrStmt::Or(a, b) => BIrStmt::Or(remap_v(*a, map), remap_v(*b, map)),
        BIrStmt::Xor(a, b) => BIrStmt::Xor(remap_v(*a, map), remap_v(*b, map)),
        BIrStmt::Not(a) => BIrStmt::Not(remap_v(*a, map)),
        BIrStmt::OracleCall { name, args, num_bits } => BIrStmt::OracleCall {
            name: name.clone(),
            args: remap_vs(args, map),
            num_bits: *num_bits,
        },
        BIrStmt::OracleBit { call, bit } => BIrStmt::OracleBit {
            call: remap_v(*call, map),
            bit: *bit,
        },
        BIrStmt::ActionCall {
            name,
            guard,
            args,
            fallback,
            num_bits,
        } => BIrStmt::ActionCall {
            name: name.clone(),
            guard: remap_v(*guard, map),
            args: remap_vs(args, map),
            fallback: remap_vs(fallback, map),
            num_bits: *num_bits,
        },
        BIrStmt::ActionBit { call, bit } => BIrStmt::ActionBit {
            call: remap_v(*call, map),
            bit: *bit,
        },
        BIrStmt::Rng { name } => BIrStmt::Rng { name: name.clone() },
        BIrStmt::StorageRead {
            storage,
            bit_width,
            addr,
        } => BIrStmt::StorageRead {
            storage: *storage,
            bit_width: *bit_width,
            addr: remap_vs(addr, map),
        },
        BIrStmt::StorageWrite {
            storage,
            src,
            bit_width,
            addr,
        } => BIrStmt::StorageWrite {
            storage: *storage,
            src: remap_v(*src, map),
            bit_width: *bit_width,
            addr: remap_vs(addr, map),
        },
    }
}

fn rewrite_bir_terminator(
    term: &BIrTerminator,
    map: &BTreeMap<IRVarId, IRVarId>,
    target_vecs: &[Vec<IRVarId>],
    state_vars: &[IRVarId],
    dispatcher_id: IRBlockId,
) -> BIrTerminator {
    let to_dispatcher =
        |pc_bits: &[IRVarId], extra_args: &[IRVarId]| -> BIrTarget {
            let mut args: Vec<IRVarId> = remap_vs(extra_args, map);
            args.truncate(state_vars.len());
            while args.len() < state_vars.len() {
                args.push(state_vars[args.len()]);
            }
            args.extend_from_slice(pc_bits);
            BIrTarget {
                block: IRBlockTargetId::Block(dispatcher_id),
                args,
            }
        };

    match term {
        BIrTerminator::Jmp(t) => match t.block {
            IRBlockTargetId::Block(_) => {
                BIrTerminator::Jmp(to_dispatcher(&target_vecs[0], &t.args))
            }
            IRBlockTargetId::Return => BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: remap_vs(&t.args, map),
            }),
            IRBlockTargetId::Dyn(_) => unreachable!("validated away"),
        },
        BIrTerminator::CondJmp {
            val,
            then_target,
            else_target,
        } => {
            let mut cursor = 0usize;
            let then_t = match then_target.block {
                IRBlockTargetId::Block(_) => {
                    let pc = &target_vecs[cursor];
                    cursor += 1;
                    to_dispatcher(pc, &then_target.args)
                }
                IRBlockTargetId::Return => BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: remap_vs(&then_target.args, map),
                },
                IRBlockTargetId::Dyn(_) => unreachable!(),
            };
            let else_t = match else_target.block {
                IRBlockTargetId::Block(_) => {
                    let pc = &target_vecs[cursor];
                    to_dispatcher(pc, &else_target.args)
                }
                IRBlockTargetId::Return => BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: remap_vs(&else_target.args, map),
                },
                IRBlockTargetId::Dyn(_) => unreachable!(),
            };
            BIrTerminator::CondJmp {
                val: remap_v(*val, map),
                then_target: then_t,
                else_target: else_t,
            }
        }
    }
}

