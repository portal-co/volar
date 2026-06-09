// @reliability: experimental
// @ai: assisted
//! Substitution of oracle, action, and RNG declarations in Volar IR blocks.
//!
//! Each named oracle/action/RNG is replaced by an outlined replacement
//! `IRBlocks` program.  All call sites for a given name share one
//! `spill_storage`, which holds the continuation pointer, args, and results
//! at fixed slot indices (safe because replacement programs must not recurse).
//! Live values from before the call are spilled to per-site slot offsets
//! within the same storage.

use alloc::{
    collections::BTreeMap,
    string::String,
    vec,
    vec::Vec,
};
use volar_ir::ir::{
    IRBlock, IRBlockId, IRBlockTargetId, IRBlocks, IRStmt, IRTerminator, IRVarId, IRTypes,
};
use volar_ir_common::{
    Constant, IrType, Stmt, StorageAllocator, StorageId,
    Type as PrimType, TypeId, TypeRemapper,
};

use crate::common::{apply_aliases_to_stmt, stmt_output_type};
use crate::ir::apply_aliases_to_ir_terminator;

// ============================================================================
// Public API
// ============================================================================

/// One substitution entry for Volar IR.
pub enum IrSubstitution {
    Oracle { name: String, replacement: IRBlocks, types: IRTypes },
    Action { name: String, replacement: IRBlocks, types: IRTypes },
    Rng    {
        name: String,
        replacement: IRBlocks,
        types: IRTypes,
        /// `StorageId` (in the replacement's guest namespace) used for
        /// persistent RNG state.  Mapped to a fresh host ID by the pass.
        state_storage_guest: StorageId,
    },
}

impl IrSubstitution {
    fn name(&self) -> &str {
        match self {
            IrSubstitution::Oracle { name, .. } => name,
            IrSubstitution::Action { name, .. } => name,
            IrSubstitution::Rng    { name, .. } => name,
        }
    }
    fn repl(&self) -> (&IRBlocks, &IRTypes) {
        match self {
            IrSubstitution::Oracle { replacement, types, .. } => (replacement, types),
            IrSubstitution::Action { replacement, types, .. } => (replacement, types),
            IrSubstitution::Rng    { replacement, types, .. } => (replacement, types),
        }
    }
}

/// Apply all substitutions to `blocks`, updating `types` in place.
/// Returns number of call sites substituted.
pub fn substitute_ir_blocks(
    blocks: &mut IRBlocks,
    types: &mut IRTypes,
    subs: &[IrSubstitution],
    allocator: &mut StorageAllocator,
) -> usize {
    let mut total = 0;
    for sub in subs {
        total += apply_one(blocks, types, sub, allocator);
    }
    total
}

/// Build a [`StorageAllocator`] starting above all `StorageId`s in use in `blocks`.
pub fn ir_storage_allocator(blocks: &IRBlocks) -> StorageAllocator {
    let max = scan_max_storage(blocks);
    StorageAllocator::new(max.max(63) + 1)
}

// ============================================================================
// Per-substitution setup
// ============================================================================

fn apply_one(
    blocks: &mut IRBlocks,
    types: &mut IRTypes,
    sub: &IrSubstitution,
    allocator: &mut StorageAllocator,
) -> usize {
    let (repl_blocks, repl_types) = sub.repl();

    // ── 1. Merge type tables ─────────────────────────────────────────────────
    let tr = TypeRemapper::merge(types, repl_types);

    // ── 2. Merge nested declarations ─────────────────────────────────────────
    for mut d in repl_blocks.oracles.iter().cloned() { tr.remap_oracle_decl(&mut d); blocks.oracles.push(d); }
    for mut d in repl_blocks.actions.iter().cloned() { tr.remap_action_decl(&mut d); blocks.actions.push(d); }
    for mut d in repl_blocks.rngs.iter().cloned()    { tr.remap_rng_decl(&mut d);    blocks.rngs.push(d); }

    // ── 3. Allocate storages ─────────────────────────────────────────────────
    let spill_storage = allocator.alloc();
    let mut storage_map: BTreeMap<u32, StorageId> = BTreeMap::new();
    storage_map.insert(StorageId::DEFAULT.0, spill_storage);
    storage_map.insert(StorageId::STACK.0, allocator.alloc());
    if let IrSubstitution::Rng { state_storage_guest, .. } = sub {
        storage_map.entry(state_storage_guest.0).or_insert_with(|| allocator.alloc());
    }
    for rb in &repl_blocks.blocks {
        for stmt in &rb.stmts {
            match stmt {
                Stmt::StorageRead { storage, .. } | Stmt::StorageWrite { storage, .. } => {
                    storage_map.entry(storage.0).or_insert_with(|| allocator.alloc());
                }
                _ => {}
            }
        }
    }

    // ── 4. Clone, remap, and append replacement blocks ───────────────────────
    let block_offset = blocks.blocks.len();
    for rb in repl_blocks.blocks.iter() {
        let new_block = IRBlock {
            params: rb.params.iter().map(|&t| tr.remap(t)).collect(),
            stmts: rb.stmts.iter().map(|s| {
                let mut s2 = s.clone();
                tr.remap_stmt_types(&mut s2);
                remap_storage_in_stmt(&mut s2, &storage_map);
                s2
            }).collect(),
            stmt_provs: rb.stmt_provs.clone(),
            terminator: remap_block_ids(&rb.terminator, block_offset),
        };
        blocks.blocks.push(new_block);
    }
    let repl_entry_bi = block_offset;

    // ── 5. Determine call signature from declarations ─────────────────────────
    let (n_args, output_tys) = call_signature(sub, blocks, &tr);
    let n_results = output_tys.len();
    let addr_ty  = types.primitive(PrimType::_64);
    let block_ty = types.intern(IrType::Block { params: vec![] });

    // ── 6. Rewrite all call sites ─────────────────────────────────────────────
    let mut live_slot_offset = 0usize;
    let mut count = 0;
    let mut bi = 0;
    while bi < blocks.blocks.len() {
        let si = match find_call_site(&blocks.blocks[bi], sub.name(), sub) {
            Some(s) => s,
            None => { bi += 1; continue; }
        };

        // Snapshot call-site info BEFORE any mutation.
        let (call_args, opt_guard, fallbacks) = snapshot_call(&blocks.blocks[bi].stmts[si], sub);
        let n_params = blocks.blocks[bi].params.len();
        let call_vid = IRVarId(n_params as u32 + si as u32);

        // Identify OracleOutput/ActionOutput stmts for this call.
        let eliminated = find_eliminated_outputs(&blocks.blocks[bi], si, n_params, call_vid, sub);

        // Determine live vars across the call.
        let live_vars = collect_live_vars(
            &blocks.blocks[bi], si, n_params, call_vid, &eliminated, types,
        );
        let n_live = live_vars.len();
        let live_base = 1 + n_args + n_results + live_slot_offset;
        live_slot_offset += n_live;

        // Compute prefix size in the continuation block.
        // Each result and each live var needs (Const addr, StorageRead) = 2 stmts.
        let result_prefix = 2 * n_results;
        let live_prefix   = 2 * n_live;
        let kept_base     = result_prefix + live_prefix;

        // Build alias map: old IRVarId → new IRVarId in continuation block.
        let mut alias: BTreeMap<IRVarId, IRVarId> = BTreeMap::new();
        for (&old_var, &ridx) in &eliminated {
            alias.insert(IRVarId(old_var), IRVarId((2 * ridx + 1) as u32));
        }
        for (k, &(old_vid, _)) in live_vars.iter().enumerate() {
            alias.insert(old_vid, IRVarId((result_prefix + 2 * k + 1) as u32));
        }
        // Map kept stmts from stmts[si+1..].
        let mut kept_idx = 0usize;
        for j in (si + 1)..blocks.blocks[bi].stmts.len() {
            let orig_var = n_params as u32 + j as u32;
            if !eliminated.contains_key(&orig_var) {
                alias.insert(IRVarId(orig_var), IRVarId((kept_base + kept_idx) as u32));
                kept_idx += 1;
            }
        }

        // Pre-compute arg types while we still have an immutable borrow.
        let arg_tys: Vec<TypeId> = call_args.iter()
            .map(|&a| var_type_in_block(&blocks.blocks[bi], a, types))
            .collect();

        // The continuation block index (set before we push it).
        let fallback_bi_opt = opt_guard.map(|_| blocks.blocks.len());
        // Continuation will be at: fallback_bi + 1 for Action, or current len for others.
        let cont_bi = blocks.blocks.len() + fallback_bi_opt.is_some() as usize;

        // Build the continuation block (immutable borrow — must finish before mutation).
        let cont_block = build_continuation_block(
            &blocks.blocks[bi],
            si,
            call_vid,
            &eliminated,
            &live_vars,
            &alias,
            n_args,
            n_results,
            live_base,
            spill_storage,
            addr_ty,
            &output_tys,
        );

        // Build the fallback block (Action only).
        let fallback_block = opt_guard.map(|_guard| {
            build_fallback_block(
                &fallbacks,
                cont_bi,
                n_results,
                n_args,
                spill_storage,
                addr_ty,
                &output_tys,
            )
        });

        // Mutate the pre-call block: truncate + emit spill writes + set terminator.
        {
            let block = &mut blocks.blocks[bi];
            block.stmts.truncate(si);
            block.stmt_provs.truncate(si);

            let base = n_params as u32;

            // Const(cont_bi) → cont_ref_var.
            let cont_ref_var = IRVarId(base + block.stmts.len() as u32);
            block.push_stmt(Stmt::Const(
                Constant { hi: 0, lo: cont_bi as u128 }, block_ty,
            ), ());

            // Write cont ref to slot 0.
            let a0 = emit_const_addr(block, base, 0, addr_ty);
            block.push_stmt(Stmt::StorageWrite {
                storage: spill_storage, src: cont_ref_var, ty: block_ty, addr: a0,
            }, ());

            // Write args to slots 1..1+n_args (using pre-computed types).
            for (j, (&arg, &aty)) in call_args.iter().zip(arg_tys.iter()).enumerate() {
                let a = emit_const_addr(block, base, 1 + j, addr_ty);
                block.push_stmt(Stmt::StorageWrite {
                    storage: spill_storage, src: arg, ty: aty, addr: a,
                }, ());
            }

            // Write live vars to slots live_base..
            for (k, &(lv, lty)) in live_vars.iter().enumerate() {
                let a = emit_const_addr(block, base, live_base + k, addr_ty);
                block.push_stmt(Stmt::StorageWrite {
                    storage: spill_storage, src: lv, ty: lty, addr: a,
                }, ());
            }

            // Set terminator.
            block.terminator = if let Some(guard) = opt_guard {
                let fb_bi = fallback_bi_opt.unwrap();
                IRTerminator::JumpCond {
                    condition: guard,
                    true_block:  IRBlockTargetId::Block(IRBlockId(repl_entry_bi as u32)),
                    true_args:   vec![],
                    false_block: IRBlockTargetId::Block(IRBlockId(fb_bi as u32)),
                    false_args:  vec![],
                }
            } else {
                IRTerminator::Jmp {
                    func: IRBlockTargetId::Block(IRBlockId(repl_entry_bi as u32)),
                    args: vec![],
                }
            };
        }

        // Push fallback block (Action only) then continuation block.
        if let Some(fb) = fallback_block {
            blocks.blocks.push(fb);
        }
        blocks.blocks.push(cont_block);

        count += 1;
        // Don't advance bi — the pre-call block at bi is now clean (no more stmts).
        // A subsequent find_call_site on blocks[bi] will return None, so the next
        // iteration will increment bi.
    }
    count
}

// ============================================================================
// Block builders
// ============================================================================

fn build_continuation_block(
    orig_block: &IRBlock,
    si: usize,
    _call_vid: IRVarId,
    eliminated: &BTreeMap<u32, usize>,
    live_vars: &[(IRVarId, TypeId)],
    alias: &BTreeMap<IRVarId, IRVarId>,
    n_args: usize,
    n_results: usize,
    live_base: usize,
    spill: StorageId,
    addr_ty: TypeId,
    output_tys: &[TypeId],
) -> IRBlock {
    let n_live = live_vars.len();
    let result_prefix = 2 * n_results;
    let _live_prefix   = 2 * n_live;

    let mut stmts: Vec<IRStmt> = Vec::new();
    let mut provs: Vec<()> = Vec::new();

    let push = |stmts: &mut Vec<IRStmt>, provs: &mut Vec<()>, s: IRStmt| {
        stmts.push(s);
        provs.push(());
    };

    // Result reads: (Const addr, StorageRead) × n_results.
    for j in 0..n_results {
        let addr_var = IRVarId((2 * j) as u32);
        push(&mut stmts, &mut provs, Stmt::Const(
            Constant { hi: 0, lo: (1 + n_args + j) as u128 },
            addr_ty,
        ));
        push(&mut stmts, &mut provs, Stmt::StorageRead {
            storage: spill, ty: output_tys[j], addr: addr_var,
        });
    }

    // Live reloads: (Const addr, StorageRead) × n_live.
    for (k, &(_, lty)) in live_vars.iter().enumerate() {
        let addr_var = IRVarId((result_prefix + 2 * k) as u32);
        push(&mut stmts, &mut provs, Stmt::Const(
            Constant { hi: 0, lo: (live_base + k) as u128 }, addr_ty,
        ));
        push(&mut stmts, &mut provs, Stmt::StorageRead {
            storage: spill, ty: lty, addr: addr_var,
        });
    }

    // Kept stmts from orig_block.stmts[si+1..], var-remapped.
    let n_params = orig_block.params.len() as u32;
    for (j, stmt) in orig_block.stmts[si + 1..].iter().enumerate() {
        let orig_var = n_params + (si + 1 + j) as u32;
        if eliminated.contains_key(&orig_var) { continue; }
        let mut s = stmt.clone();
        apply_aliases_to_stmt(&mut s, alias);
        push(&mut stmts, &mut provs, s);
    }

    // Terminator: original, var-remapped.
    let mut terminator = orig_block.terminator.clone();
    apply_aliases_to_ir_terminator(&mut terminator, alias);

    IRBlock { params: vec![], stmts, stmt_provs: provs, terminator }
}

fn build_fallback_block(
    fallbacks: &[IRVarId],
    cont_bi: usize,
    n_results: usize,
    n_args: usize,
    spill: StorageId,
    addr_ty: TypeId,
    output_tys: &[TypeId],
) -> IRBlock {
    let mut stmts: Vec<IRStmt> = Vec::new();
    let mut provs: Vec<()> = Vec::new();

    for (j, &fb) in fallbacks.iter().enumerate().take(n_results) {
        let addr_var = IRVarId((2 * j) as u32);
        stmts.push(Stmt::Const(Constant { hi: 0, lo: (1 + n_args + j) as u128 }, addr_ty));
        provs.push(());
        stmts.push(Stmt::StorageWrite {
            storage: spill, src: fb, ty: output_tys.get(j).copied().unwrap_or(TypeId(0)), addr: addr_var,
        });
        provs.push(());
    }

    let terminator = IRTerminator::Jmp {
        func: IRBlockTargetId::Block(IRBlockId(cont_bi as u32)),
        args: vec![],
    };
    IRBlock { params: vec![], stmts, stmt_provs: provs, terminator }
}

// ============================================================================
// Helper: emit Const addr stmt, return its var
// ============================================================================

fn emit_const_addr(block: &mut IRBlock, base: u32, slot: usize, addr_ty: TypeId) -> IRVarId {
    let vid = IRVarId(base + block.stmts.len() as u32);
    block.push_stmt(Stmt::Const(Constant { hi: 0, lo: slot as u128 }, addr_ty), ());
    vid
}

// ============================================================================
// Snapshot / introspection helpers
// ============================================================================

fn snapshot_call(
    stmt: &IRStmt,
    sub: &IrSubstitution,
) -> (Vec<IRVarId>, Option<IRVarId>, Vec<IRVarId>) {
    match (sub, stmt) {
        (IrSubstitution::Oracle { .. }, Stmt::OracleCall { args, .. }) => {
            (args.clone(), None, vec![])
        }
        (IrSubstitution::Action { .. }, Stmt::ActionCall { guard, args, fallbacks, .. }) => {
            (args.clone(), Some(*guard), fallbacks.clone())
        }
        (IrSubstitution::Rng { .. }, Stmt::Rng { .. }) => {
            (vec![], None, vec![])
        }
        _ => (vec![], None, vec![]),
    }
}

fn find_call_site(block: &IRBlock, name: &str, sub: &IrSubstitution) -> Option<usize> {
    for (si, stmt) in block.stmts.iter().enumerate() {
        let m = match (sub, stmt) {
            (IrSubstitution::Oracle { .. }, Stmt::OracleCall { name: n, .. }) => n == name,
            (IrSubstitution::Action { .. }, Stmt::ActionCall { name: n, .. }) => n == name,
            (IrSubstitution::Rng    { .. }, Stmt::Rng { name: n, .. })        => n == name,
            _ => false,
        };
        if m { return Some(si); }
    }
    None
}

fn find_eliminated_outputs(
    block: &IRBlock,
    si: usize,
    n_params: usize,
    call_vid: IRVarId,
    sub: &IrSubstitution,
) -> BTreeMap<u32, usize> {
    let mut out = BTreeMap::new();
    if matches!(sub, IrSubstitution::Rng { .. }) { return out; }
    for (j, stmt) in block.stmts[si + 1..].iter().enumerate() {
        let orig = n_params as u32 + (si + 1 + j) as u32;
        match stmt {
            Stmt::OracleOutput { call, idx, .. } if *call == call_vid => { out.insert(orig, *idx); }
            Stmt::ActionOutput { call, idx, .. } if *call == call_vid => { out.insert(orig, *idx); }
            _ => {}
        }
    }
    out
}

fn collect_live_vars(
    block: &IRBlock,
    si: usize,
    n_params: usize,
    call_vid: IRVarId,
    eliminated: &BTreeMap<u32, usize>,
    types: &IRTypes,
) -> Vec<(IRVarId, TypeId)> {
    let threshold = n_params as u32 + si as u32;
    let mut seen: BTreeMap<u32, TypeId> = BTreeMap::new();

    let mut visit = |vid: IRVarId| {
        if vid.0 < threshold && vid != call_vid && !seen.contains_key(&vid.0) {
            if !eliminated.contains_key(&vid.0) {
                let ty = var_type_in_block(block, vid, types);
                seen.insert(vid.0, ty);
            }
        }
    };

    for stmt in &block.stmts[si + 1..] {
        visit_stmt_vars(stmt, &mut visit);
    }
    visit_terminator_vars(&block.terminator, &mut visit);

    seen.into_iter().map(|(id, ty)| (IRVarId(id), ty)).collect()
}

fn var_type_in_block(block: &IRBlock, vid: IRVarId, _types: &IRTypes) -> TypeId {
    let n = block.params.len();
    if (vid.0 as usize) < n {
        block.params[vid.0 as usize]
    } else {
        let si = vid.0 as usize - n;
        block.stmts.get(si)
            .and_then(|s| stmt_output_type(s))
            .unwrap_or(TypeId(0))
    }
}

fn visit_stmt_vars<F: FnMut(IRVarId)>(stmt: &IRStmt, f: &mut F) {
    match stmt {
        Stmt::StorageRead  { addr, .. }           => f(*addr),
        Stmt::StorageWrite { src, addr, .. }       => { f(*src); f(*addr); }
        Stmt::Const(_, _) | Stmt::Rng { .. }      => {}
        Stmt::Transmute { src, .. }
        | Stmt::Rol { src, .. } | Stmt::Ror { src, .. } | Stmt::Splat { src, .. } => f(*src),
        Stmt::Merge { parts, .. }                 => parts.iter().for_each(|&v| f(v)),
        Stmt::Shuffle { result_bits, .. }         => result_bits.iter().for_each(|&(_, v)| f(v)),
        Stmt::Poly { coeffs, .. }                 => {
            coeffs.keys().for_each(|k| k.iter().for_each(|&v| f(v)));
        }
        Stmt::OracleCall  { args, .. }            => args.iter().for_each(|&a| f(a)),
        Stmt::OracleOutput { call, .. }           => f(*call),
        Stmt::ActionCall  { guard, args, fallbacks, .. } => {
            f(*guard);
            args.iter().for_each(|&a| f(a));
            fallbacks.iter().for_each(|&a| f(a));
        }
        Stmt::ActionOutput { call, .. }           => f(*call),
        _ => {}
    }
}

fn visit_terminator_vars<F: FnMut(IRVarId)>(term: &IRTerminator, f: &mut F) {
    match term {
        IRTerminator::Jmp { func, args } => {
            if let IRBlockTargetId::Dyn(v) = func { f(*v); }
            args.iter().for_each(|&a| f(a));
        }
        IRTerminator::JumpCond { condition, true_block, true_args, false_block, false_args } => {
            f(*condition);
            if let IRBlockTargetId::Dyn(v) = true_block  { f(*v); }
            if let IRBlockTargetId::Dyn(v) = false_block { f(*v); }
            true_args.iter().for_each(|&a| f(a));
            false_args.iter().for_each(|&a| f(a));
        }
        IRTerminator::JumpTable { index, cases } => {
            f(*index);
            cases.values().for_each(|(tgt, args)| {
                if let IRBlockTargetId::Dyn(v) = tgt { f(*v); }
                args.iter().for_each(|&a| f(a));
            });
        }
        _ => {}
    }
}

// ============================================================================
// Call signature from declarations
// ============================================================================

fn call_signature(
    sub: &IrSubstitution,
    blocks: &IRBlocks,
    tr: &TypeRemapper,
) -> (usize, Vec<TypeId>) {
    let (repl, _) = sub.repl();
    let name = sub.name();
    match sub {
        IrSubstitution::Oracle { .. } => {
            let d = blocks.oracles.iter().rev().find(|d| d.name == name)
                .or_else(|| repl.oracles.iter().find(|d| d.name == name));
            if let Some(d) = d {
                let results: Vec<TypeId> = d.results.iter().map(|&t| tr.remap(t)).collect();
                (d.params.len(), results)
            } else { (0, vec![TypeId(0)]) }
        }
        IrSubstitution::Action { .. } => {
            let d = blocks.actions.iter().rev().find(|d| d.name == name)
                .or_else(|| repl.actions.iter().find(|d| d.name == name));
            if let Some(d) = d {
                let results: Vec<TypeId> = d.results.iter().map(|&t| tr.remap(t)).collect();
                (d.params.len(), results)
            } else { (0, vec![TypeId(0)]) }
        }
        IrSubstitution::Rng { .. } => {
            let d = blocks.rngs.iter().rev().find(|d| d.name == name)
                .or_else(|| repl.rngs.iter().find(|d| d.name == name));
            if let Some(d) = d {
                (0, vec![tr.remap(d.ty)])
            } else { (0, vec![TypeId(0)]) }
        }
    }
}

// ============================================================================
// Storage / block-id remapping
// ============================================================================

fn remap_storage_in_stmt(stmt: &mut IRStmt, map: &BTreeMap<u32, StorageId>) {
    match stmt {
        Stmt::StorageRead  { storage, .. } | Stmt::StorageWrite { storage, .. } => {
            if let Some(&new_id) = map.get(&storage.0) { *storage = new_id; }
        }
        _ => {}
    }
}

fn remap_block_ids(term: &IRTerminator, offset: usize) -> IRTerminator {
    let rt = |t: &IRBlockTargetId| match t {
        IRBlockTargetId::Block(b) => IRBlockTargetId::Block(IRBlockId(b.0 + offset as u32)),
        other => other.clone(),
    };
    match term {
        IRTerminator::Jmp { func, args } =>
            IRTerminator::Jmp { func: rt(func), args: args.clone() },
        IRTerminator::JumpCond { condition, true_block, true_args, false_block, false_args } =>
            IRTerminator::JumpCond {
                condition: *condition,
                true_block: rt(true_block), true_args: true_args.clone(),
                false_block: rt(false_block), false_args: false_args.clone(),
            },
        IRTerminator::JumpTable { index, cases } =>
            IRTerminator::JumpTable {
                index: *index,
                cases: cases.iter().map(|(c, (t, a))| (*c, (rt(t), a.clone()))).collect(),
            },
        _ => panic!("remap_block_ids: unhandled IRTerminator variant — add block-id remapping for this variant"),
    }
}

// ============================================================================
// Scanning
// ============================================================================

fn scan_max_storage(blocks: &IRBlocks) -> u32 {
    let mut max = 0u32;
    for block in &blocks.blocks {
        for stmt in &block.stmts {
            match stmt {
                Stmt::StorageRead { storage, .. } | Stmt::StorageWrite { storage, .. } => {
                    if storage.0 > max { max = storage.0; }
                }
                _ => {}
            }
        }
    }
    for seg in &blocks.pre_init {
        if seg.storage.0 > max { max = seg.storage.0; }
    }
    max
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    extern crate std;
    use alloc::{string::ToString, vec, vec::Vec};
    use volar_ir::ir::{
        IRBlock, IRBlockId, IRBlockTargetId, IRBlocks, IRTerminator, IRVarId, IRTypes,
    };
    use volar_ir_common::{
        Constant, IrType, OracleDecl, RngDecl, Stmt, StorageId, Type, TypeId,
    };
    use super::{IrSubstitution, ir_storage_allocator, substitute_ir_blocks};

    fn ret() -> IRTerminator {
        IRTerminator::Jmp { func: IRBlockTargetId::Return, args: vec![] }
    }

    fn block(stmts: Vec<IRStmt>, terminator: IRTerminator) -> IRBlock {
        let stmt_provs = vec![(); stmts.len()];
        IRBlock { params: vec![], stmts, stmt_provs, terminator }
    }

    type IRStmt = volar_ir::ir::IRStmt;

    fn types_with_u64() -> (IRTypes, TypeId, TypeId) {
        let mut t = IRTypes::new();
        let u64_ty = t.primitive(Type::_64);
        let addr_ty = t.primitive(Type::_64);
        (t, u64_ty, addr_ty)
    }

    // ── Oracle: basic substitution ──────────────────────────────────────────

    /// Host block: [OracleCall("h", args=[]), OracleOutput(call=v0,idx=0)], ret v1
    /// After substitution: pre-call block truncated; replacement appended; cont block has reload.
    #[test]
    fn oracle_call_site_split_into_three_blocks() {
        let (mut types, u64_ty, _) = types_with_u64();
        let result_ty = types.intern(IrType::Tuple(vec![u64_ty]));

        let host_stmts = vec![
            Stmt::OracleCall {
                name: "h".to_string(),
                args: vec![],
                output_tys: vec![u64_ty],
                result_ty,
            },
            Stmt::OracleOutput { call: IRVarId(0), idx: 0, ty: u64_ty },
        ];
        let mut host = IRBlocks::new(vec![block(host_stmts, ret())]);
        host.oracles.push(OracleDecl {
            name: "h".to_string(),
            params: vec![],
            results: vec![u64_ty],
        });

        // Replacement: one block, reads cont from DEFAULT[0], exits via Dyn.
        let cont_var = IRVarId(0);
        let repl_stmts = vec![
            // Read cont from spill[0] — slot address
            Stmt::Const(Constant { hi: 0, lo: 0 }, types.primitive(Type::_64)),
            // StorageRead continuation
            Stmt::StorageRead {
                storage: StorageId::DEFAULT,
                ty: types.intern(IrType::Block { params: vec![] }),
                addr: IRVarId(0),
            },
            // Write a constant result to spill[1]
            Stmt::Const(Constant { hi: 0, lo: 1 }, types.primitive(Type::_64)),
            Stmt::Const(Constant { hi: 0, lo: 99 }, u64_ty),
            Stmt::StorageWrite {
                storage: StorageId::DEFAULT,
                src: IRVarId(3),
                ty: u64_ty,
                addr: IRVarId(2),
            },
        ];
        let repl_term = IRTerminator::Jmp {
            func: IRBlockTargetId::Dyn(cont_var),
            args: vec![],
        };
        let mut repl = IRBlocks::new(vec![block(repl_stmts, repl_term)]);
        repl.oracles.push(OracleDecl {
            name: "h".to_string(),
            params: vec![],
            results: vec![u64_ty],
        });

        let subs = [IrSubstitution::Oracle {
            name: "h".to_string(),
            replacement: repl,
            types: types.clone(),
        }];
        let mut alloc = ir_storage_allocator(&host);
        let count = substitute_ir_blocks(&mut host, &mut types, &subs, &mut alloc);
        assert_eq!(count, 1);

        // Original 1 block + 1 replacement block + 1 continuation block = 3.
        assert_eq!(host.blocks.len(), 3, "expected pre-call, replacement, continuation blocks");
    }

    #[test]
    fn oracle_pre_call_block_has_no_oracle_call() {
        let (mut types, u64_ty, _) = types_with_u64();
        let result_ty = types.intern(IrType::Tuple(vec![u64_ty]));

        let host_stmts = vec![
            Stmt::OracleCall {
                name: "h".to_string(),
                args: vec![],
                output_tys: vec![u64_ty],
                result_ty,
            },
            Stmt::OracleOutput { call: IRVarId(0), idx: 0, ty: u64_ty },
        ];
        let mut host = IRBlocks::new(vec![block(host_stmts, ret())]);
        host.oracles.push(OracleDecl { name: "h".to_string(), params: vec![], results: vec![u64_ty] });

        let repl = make_trivial_oracle_repl(&mut types, u64_ty);
        let subs = [IrSubstitution::Oracle {
            name: "h".to_string(),
            replacement: repl,
            types: types.clone(),
        }];
        let mut alloc = ir_storage_allocator(&host);
        substitute_ir_blocks(&mut host, &mut types, &subs, &mut alloc);

        // Pre-call block (index 0) should have no OracleCall.
        for stmt in &host.blocks[0].stmts {
            assert!(
                !matches!(stmt, Stmt::OracleCall { name, .. } if name == "h"),
                "OracleCall should have been removed from the pre-call block"
            );
        }
        // Pre-call block should jump to replacement entry (block 1).
        assert!(
            matches!(
                &host.blocks[0].terminator,
                IRTerminator::Jmp { func: IRBlockTargetId::Block(IRBlockId(1)), .. }
            ),
            "pre-call block should jump to the replacement entry"
        );
    }

    #[test]
    fn oracle_two_sites_both_substituted() {
        let (mut types, u64_ty, _) = types_with_u64();
        let result_ty = types.intern(IrType::Tuple(vec![u64_ty]));

        let make_call = || Stmt::OracleCall {
            name: "h".to_string(),
            args: vec![],
            output_tys: vec![u64_ty],
            result_ty,
        };
        let stmts = vec![make_call(), make_call()];
        let mut host = IRBlocks::new(vec![block(stmts, ret())]);
        host.oracles.push(OracleDecl { name: "h".to_string(), params: vec![], results: vec![u64_ty] });

        let repl = make_trivial_oracle_repl(&mut types, u64_ty);
        let subs = [IrSubstitution::Oracle {
            name: "h".to_string(),
            replacement: repl,
            types: types.clone(),
        }];
        let mut alloc = ir_storage_allocator(&host);
        let count = substitute_ir_blocks(&mut host, &mut types, &subs, &mut alloc);
        assert_eq!(count, 2, "both sites should be substituted");
    }

    // ── Rng: basic substitution ─────────────────────────────────────────────

    #[test]
    fn rng_site_substituted() {
        let (mut types, u64_ty, _) = types_with_u64();

        let host_stmts = vec![
            Stmt::Rng { name: "rand".to_string(), ty: u64_ty },
        ];
        let mut host = IRBlocks::new(vec![block(host_stmts, ret())]);
        host.rngs.push(RngDecl { name: "rand".to_string(), ty: u64_ty });

        let repl = make_trivial_rng_repl(&mut types, u64_ty);
        let state_storage_guest = StorageId(10);
        let subs = [IrSubstitution::Rng {
            name: "rand".to_string(),
            replacement: repl,
            types: types.clone(),
            state_storage_guest,
        }];
        let mut alloc = ir_storage_allocator(&host);
        let count = substitute_ir_blocks(&mut host, &mut types, &subs, &mut alloc);
        assert_eq!(count, 1);

        // Pre-call block has no Rng stmt.
        for stmt in &host.blocks[0].stmts {
            assert!(!matches!(stmt, Stmt::Rng { name, .. } if name == "rand"));
        }
    }

    // ── Storage non-collision ───────────────────────────────────────────────

    #[test]
    fn two_substitutions_get_distinct_spill_storages() {
        let (mut types, u64_ty, _) = types_with_u64();
        let result_ty = types.intern(IrType::Tuple(vec![u64_ty]));

        let stmts = vec![
            Stmt::OracleCall {
                name: "a".to_string(), args: vec![],
                output_tys: vec![u64_ty], result_ty,
            },
            Stmt::OracleCall {
                name: "b".to_string(), args: vec![],
                output_tys: vec![u64_ty], result_ty,
            },
        ];
        let mut host = IRBlocks::new(vec![block(stmts, ret())]);
        host.oracles.push(OracleDecl { name: "a".to_string(), params: vec![], results: vec![u64_ty] });
        host.oracles.push(OracleDecl { name: "b".to_string(), params: vec![], results: vec![u64_ty] });

        let repl_a = make_trivial_oracle_repl(&mut types, u64_ty);
        let repl_b = make_trivial_oracle_repl(&mut types, u64_ty);
        let subs = [
            IrSubstitution::Oracle { name: "a".to_string(), replacement: repl_a, types: types.clone() },
            IrSubstitution::Oracle { name: "b".to_string(), replacement: repl_b, types: types.clone() },
        ];
        let mut alloc = ir_storage_allocator(&host);
        let count = substitute_ir_blocks(&mut host, &mut types, &subs, &mut alloc);
        assert_eq!(count, 2);

        // Collect all spill StorageIds used in StorageWrite stmts of the pre-call blocks.
        // They must be distinct across the two substitutions.
        let mut spill_ids: std::collections::BTreeSet<u32> = std::collections::BTreeSet::new();
        for b in &host.blocks {
            for stmt in &b.stmts {
                if let Stmt::StorageWrite { storage, .. } = stmt {
                    spill_ids.insert(storage.0);
                }
            }
        }
        // Two substitutions → at least two distinct spill storages (plus potentially STACK).
        // Each substitution allocates its own spill, so IDs must differ.
        assert!(spill_ids.len() >= 2, "each substitution must use a distinct spill storage");
    }

    // ── Helpers ─────────────────────────────────────────────────────────────

    fn make_trivial_oracle_repl(types: &mut IRTypes, u64_ty: TypeId) -> IRBlocks {
        let addr_ty = types.primitive(Type::_64);
        let block_ty = types.intern(IrType::Block { params: vec![] });

        // Reads cont from DEFAULT[0], writes const 0 to result slot, exits via Dyn.
        let stmts = vec![
            Stmt::Const(Constant { hi: 0, lo: 0 }, addr_ty),       // v0: addr 0
            Stmt::StorageRead { storage: StorageId::DEFAULT, ty: block_ty, addr: IRVarId(0) }, // v1: cont
            Stmt::Const(Constant { hi: 0, lo: 1 }, addr_ty),       // v2: addr 1
            Stmt::Const(Constant { hi: 0, lo: 0 }, u64_ty),        // v3: result value
            Stmt::StorageWrite { storage: StorageId::DEFAULT, src: IRVarId(3), ty: u64_ty, addr: IRVarId(2) },
        ];
        let term = IRTerminator::Jmp { func: IRBlockTargetId::Dyn(IRVarId(1)), args: vec![] };
        let mut repl = IRBlocks::new(vec![block(stmts, term)]);
        repl.oracles.push(OracleDecl { name: "h".to_string(), params: vec![], results: vec![u64_ty] });
        repl
    }

    fn make_trivial_rng_repl(types: &mut IRTypes, u64_ty: TypeId) -> IRBlocks {
        let addr_ty = types.primitive(Type::_64);
        let block_ty = types.intern(IrType::Block { params: vec![] });

        let stmts = vec![
            Stmt::Const(Constant { hi: 0, lo: 0 }, addr_ty),
            Stmt::StorageRead { storage: StorageId::DEFAULT, ty: block_ty, addr: IRVarId(0) },
            Stmt::Const(Constant { hi: 0, lo: 1 }, addr_ty),
            Stmt::Const(Constant { hi: 0, lo: 0 }, u64_ty),
            Stmt::StorageWrite { storage: StorageId::DEFAULT, src: IRVarId(3), ty: u64_ty, addr: IRVarId(2) },
        ];
        let term = IRTerminator::Jmp { func: IRBlockTargetId::Dyn(IRVarId(1)), args: vec![] };
        let mut repl = IRBlocks::new(vec![block(stmts, term)]);
        repl.rngs.push(RngDecl { name: "rand".to_string(), ty: u64_ty });
        repl
    }
}
