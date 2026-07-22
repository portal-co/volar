//! VAFFLE-to-VAFFLE cross-block value spilling, dominator-verified,
//! recursion-safe via an explicit threaded stack pointer.
//!
//! VAFFLE has a flat, dominance-based value space: any block may reference
//! a `ValueId` computed by a block it assumes dominates it, directly by ID,
//! with no explicit block-param/phi threading required by VAFFLE's own
//! representation. `lower_to_ir.rs` used to handle this by spilling every
//! such cross-block value to a synthetic STACK storage address immediately
//! after it's computed, and reloading it via `StorageRead` wherever it's
//! used (`compute_cross_block_values` + `frame_spill`/`frame_reload`).
//!
//! This pass replaces that ad-hoc mechanism with the same *kind* of
//! spill/reload, but built on an explicit, dominator-tree-backed guarantee
//! instead of an unverified assumption: for every cross-block value, this
//! pass computes a real dominator tree for the function's CFG (cheap —
//! real functions here top out in the low hundreds of blocks) and checks,
//! once, that the value's owning block genuinely dominates every one of
//! its uses. If that check ever fails, this panics loudly and immediately,
//! naming the value and blocks involved, instead of silently producing a
//! wrong-but-plausible spill/reload pair.
//!
//! **Recursion safety.** VAFFLE, unlike the flat single-function lowered
//! IR it targets, can have multiple, mutually- or self-recursive
//! functions. A spill addressed purely by its own `ValueId` (a static,
//! compile-time constant) would collide across recursive invocations: an
//! inner call's own spill for value V would land at the exact same
//! address as an outer, still-live call's own spill for V, silently
//! clobbering it. To prevent this, every *non-entry* function receives an
//! extra, threaded `SPILL_ADDR_BITS`-wide "stack pointer" parameter, and
//! every call site advances it by a fixed, generous per-call step
//! (`SP_STEP`, chosen larger than the largest `ValueId` in the whole
//! module) before passing it to the callee. Every spill/reload address
//! becomes `SP + ValueId` instead of the raw `ValueId` alone — so
//! nested/recursive invocations, each carrying a different SP, never
//! share an address, regardless of recursion depth. Since `SP_STEP` is
//! always an exact power of two and every `ValueId < SP_STEP`, `SP`'s low
//! bits are always zero, so `SP + ValueId == SP | ValueId` exactly —
//! this is built as a cheap bitwise `Merge`, not a ripple-carry adder
//! (see [`emit_spill_address`]'s own doc comment for why, and the actual
//! adder, [`bc_add`], is reserved for *advancing* SP at call sites).
//!
//! The module's own designated entry function is never called by anything
//! inside the module, so it can never recurse — its own SP is provably
//! the constant 0 at every point in its body. Rather than materialize
//! that constant as 64 real `Stmt::Const` values and thread it through
//! every one of its blocks like an ordinary non-entry function's SP, this
//! pass skips SP-threading for it entirely and synthesizes the zero bits
//! inline, only at the (possibly zero) call sites and spill/reload
//! addresses that actually need them (see `ssa_ify_function`'s Phase 1
//! comment). A function with no internal calls and no cross-block values
//! -- true of most single-function WASM-interpreter-style modules this
//! pipeline targets -- costs this pass nothing at all.
//!
//! For a *non-entry* function, SP is threaded through *every* block (not
//! just blocks that spill or use something) as an ordinary block param/
//! jump-arg — the "max SSA" mechanism an earlier version of this pass
//! used for *every* cross-block value, before that was found to scale
//! badly (`Σ over values (blocks on the path from definition to use)`
//! blew up to tens of GB on the real ~120-block, ~44,000-cross-block-
//! value RISC-V interpreter this compiles). Threading is fine for SP
//! specifically because there's only ever one SP per function, not
//! thousands of independent values — the per-block param cost is
//! negligible (`O(n_blocks)`, not `O(n_values × path_length)`), and in
//! practice this pipeline's non-entry functions are rare to nonexistent.
//!
//! **Future extension point**: because both mechanisms (spilling and
//! threading) share this same dominator-verified pathing foundation, a
//! value-by-value choice between spilling (cheap at compile time, small
//! runtime storage-traffic cost, good for "cold" values with few uses)
//! and threading (no storage traffic, good for "hot" values reused very
//! close to their definition) is a natural follow-up — not implemented
//! here; this pass always spills non-SP values.
//!
//! After [`ssa_ify_function`] runs, [`compute_cross_block_values`] on its
//! output is guaranteed to return an empty set (checked via
//! `debug_assert`) — `lower_to_ir.rs`'s old cross-block spill/reload
//! machinery becomes permanently inert, with no flag needed to disable
//! it. Real call-argument spilling and
//! `StackAlloc`/`PtrLoad`/`PtrStore`/`PtrOffset` use entirely separate
//! mechanisms, addressing schemes, and `StorageId`s, and are untouched by
//! this pass.

use alloc::{
    collections::{BTreeMap, BTreeSet},
    vec::Vec,
};

use vaffle::{Block, BlockId, FuncBody, FuncDecl, FuncId, Module, Terminator, Value, ValueId};
use volar_ir_common::{Constant, IrType, Node, Stmt, StorageId, TypeId};
use volar_lir::circuits::{bc_add, BitCircuitBuilder};

use crate::lower_to_ir::{collect_uses, compute_cross_block_values, compute_owner, vaffle_value_vtid};

/// Bit-width of every `vaffle_ssa` address (`SP + ValueId`) and of SP
/// itself. Wide enough for the largest `ValueId` in any module this
/// pipeline compiles today (hundreds of thousands, at most) times a
/// generous recursion-depth margin — 64 bits leaves an enormous margin
/// (SP can advance `SP_STEP` roughly `2^64 / SP_STEP` times before
/// wrapping, i.e. billions of recursive calls even for a large module).
const SPILL_ADDR_BITS: usize = 64;

/// Per-call SP advance. Chosen larger than the largest `ValueId` in the
/// whole module, so two different call depths' own `SP + vid` ranges can
/// never overlap regardless of which function is called or which values
/// it happens to spill.
fn compute_sp_step<P: Clone>(module: &Module<P>) -> u128 {
    let max_vid = module.funcs.iter().map(|f| match f {
        FuncDecl::Body(b) => b.values.len(),
        _ => 0,
    }).max().unwrap_or(0);
    // Round up generously so the step is easy to eyeball in diagnostics
    // and has headroom against off-by-one errors in the max above.
    ((max_vid as u128) + 1).next_power_of_two().max(1 << 20)
}

/// Minimal [`BitCircuitBuilder`] adapter over a `vaffle_ssa`-in-progress
/// value arena, so this pass can reuse the same tested `bc_add` ripple-
/// carry adder every other VAFFLE-emitting path in this crate uses,
/// instead of hand-rolling bit arithmetic.
struct VecBuilder<'a, P: Clone> {
    values: &'a mut Vec<Node<Value, P>>,
    prov: P,
    side: Option<volar_side::SideId>,
    bit_tid: TypeId,
}

impl<'a, P: Clone> BitCircuitBuilder for VecBuilder<'a, P> {
    type Bit = ValueId;

    fn bc_const(&mut self, val: bool) -> ValueId {
        let vid = ValueId(self.values.len());
        self.values.push(Node::new(
            Value::Op(Stmt::Const(Constant { hi: 0, lo: val as u128 }, self.bit_tid)),
            self.prov.clone(), self.side,
        ));
        vid
    }

    fn bc_poly(&mut self, coeffs: BTreeMap<Vec<ValueId>, u8>, constant: u128) -> ValueId {
        let vid = ValueId(self.values.len());
        self.values.push(Node::new(
            Value::Op(Stmt::Poly { ty: self.bit_tid, coeffs, constant: Constant { hi: 0, lo: constant } }),
            self.prov.clone(), self.side,
        ));
        vid
    }
}

/// Transform every function body in `module` via [`ssa_ify_function`].
/// The module's first function (`module.funcs[0]`) is treated as the
/// designated entry point, matching `lower_to_ir.rs`'s own "the first
/// function in the module is treated as the entry point" convention —
/// see `lower_vaffle_to_ir`'s doc comment. Only the entry function's own
/// SP is hardcoded to zero; every other function receives it as an extra
/// threaded parameter from its own callers, since only non-entry
/// functions can be reached via a VAFFLE-internal (and therefore
/// possibly recursive) call.
pub fn ssa_ify_module<P: Clone>(module: &Module<P>) -> Module<P> {
    let mut types = module.types.clone();
    let bit_tid = types.bit();
    let addr_tid = types.intern(IrType::Vec(SPILL_ADDR_BITS, bit_tid));
    let sp_step = compute_sp_step(module);

    let funcs = module.funcs.iter().enumerate().map(|(fi, f)| match f {
        FuncDecl::Import { module: m, name, sig } => {
            FuncDecl::Import { module: m.clone(), name: name.clone(), sig: *sig }
        }
        FuncDecl::Body(body) => FuncDecl::Body(ssa_ify_function(module, body, addr_tid, bit_tid, sp_step, fi == 0)),
        // `FuncDecl` is `#[non_exhaustive]` (defined in the `vaffle`
        // crate, matched here from a different crate) -- wildcard
        // required even though only these two variants exist today.
        _ => panic!("vaffle_ssa: unexpected FuncDecl variant"),
    }).collect();

    Module {
        types,
        oracles: module.oracles.clone(),
        actions: module.actions.clone(),
        funcs,
        sigs: module.sigs.clone(),
        exports: module.exports.clone(),
        pre_init: module.pre_init.clone(),
    }
}

/// Rewrite `body` so that no `ValueId` is ever referenced outside the
/// block that owns its defining occurrence — every cross-block reference
/// becomes an explicit spill (in the owning block) + reload (in each
/// using block) pair through `StorageId::VAFFLE_SSA_SPILL`, addressed by
/// `SP + ValueId` and verified safe by a real dominator-tree computation
/// over `body`'s own CFG. `is_entry` selects whether this function's own
/// SP is a fresh, hardcoded zero (the module's designated entry point) or
/// an extra threaded parameter received from its own callers (every
/// other function, reachable only via a VAFFLE-internal, possibly
/// recursive call). See the module doc comment for the full design.
pub fn ssa_ify_function<P: Clone>(
    module: &Module<P>,
    body: &FuncBody<P>,
    addr_tid: TypeId,
    bit_tid: TypeId,
    sp_step: u128,
    is_entry: bool,
) -> FuncBody<P> {
    let mut blocks: Vec<Block> = body.blocks.clone();
    let mut values: Vec<Node<Value, P>> = body.values.clone();
    let entry = body.entry.0;

    let preds = build_preds(&blocks);
    let rpo = compute_rpo(blocks.len(), entry, &preds);
    let mut rpo_index = alloc::vec![usize::MAX; blocks.len()];
    for (i, &b) in rpo.iter().enumerate() {
        rpo_index[b] = i;
    }
    let idom = compute_idom(&rpo, &rpo_index, &preds, entry);

    // ---- Phase 1: thread SP through every block -----------------------------
    // Nothing inside the module ever calls the entry function (checked by
    // `wire_call_sites`'s own FuncId(0) guard), so it can never recurse --
    // its own SP is provably the constant 0 at every point in its body,
    // with no need to thread it through any block at all. Skip Phase 1
    // entirely for it; `emit_spill_address`/`advance_sp`'s `None` case
    // (see their own doc comments) synthesizes that zero inline,
    // wherever it's actually needed, instead of paying a `SPILL_ADDR_BITS`-
    // wide param on every single block regardless of whether that block
    // ever computes a spill/reload address or makes a call. Only non-entry
    // functions -- whose SP genuinely varies by the caller's own call
    // depth -- need the full per-block threading.
    let sp_bits_for: BTreeMap<usize, Vec<ValueId>> = if is_entry {
        BTreeMap::new()
    } else {
        thread_sp(&mut blocks, &mut values, &rpo, bit_tid)
    };

    // ---- Phase 2: advance SP at every internal call site --------------------
    wire_call_sites(&mut blocks, &mut values, &rpo, &sp_bits_for, sp_step, bit_tid);

    // ---- Phase 3: dominator-verified cross-block spill/reload ---------------
    let owner = compute_owner(&blocks);

    // Discover every cross-block use from the body as it stands after
    // phases 1-2 (SP threading/call wiring never turns a same-block
    // reference into a cross-block one, or vice versa, so this is
    // equivalent to discovering it from the original body, but simpler
    // to reason about against the current state).
    // Only blocks reachable from `entry` (i.e. in `rpo`) have their own SP
    // -- an unreachable block is dead code that can never execute, so it's
    // simply left untouched here (no SP, no spill/reload wiring); any
    // reference *from* a reachable block always resolves to a value owned
    // by another reachable, genuinely-dominating block (checked below), so
    // this never masks a real violation.
    let mut uses_per_block: Vec<BTreeSet<usize>> = alloc::vec![BTreeSet::new(); blocks.len()];
    for &bi in &rpo {
        let mut cross = BTreeSet::new();
        for u in collect_uses(&values, &blocks[bi].stmts, &blocks[bi].terminator) {
            if owner.get(&u).copied() != Some(bi) {
                cross.insert(u);
            }
        }
        uses_per_block[bi] = cross;
    }

    for (bi, uses) in uses_per_block.iter().enumerate() {
        for &u in uses {
            let owner_bi = owner[&u];
            assert!(
                dominates(owner_bi, bi, &idom),
                "vaffle_ssa: value {u} (owned by block {owner_bi}) is used at block {bi} but \
                 block {owner_bi} does not dominate block {bi} -- VAFFLE's dominance invariant \
                 is violated for this function",
            );
        }
    }

    let mut needs_spill: BTreeSet<usize> = BTreeSet::new();
    for uses in &uses_per_block {
        needs_spill.extend(uses.iter().copied());
    }

    // Emit spills: append (address computation, storage-write) to the END
    // of each owner block's own stmts -- always safe, since a block's own
    // value (and its own SP) is fully defined by the time its own stmts
    // list ends.
    let mut spills_by_owner: BTreeMap<usize, Vec<u32>> = BTreeMap::new();
    for &v in &needs_spill {
        let vid = ValueId(v);
        let owner_bi = owner[&v];
        let ty = vaffle_value_vtid(module, &values, vid);
        let prov = values[v].prov.clone();
        let side = values[v].side;
        let sp_bits = sp_bits_for.get(&owner_bi).cloned();
        let (mut new_stmts, addr_vid) = emit_spill_address(&mut values, prov.clone(), side, bit_tid, addr_tid, sp_bits.as_deref(), sp_step, v);
        new_stmts.push(values.len() as u32);
        values.push(Node::new(
            Value::Op(Stmt::StorageWrite { storage: StorageId::VAFFLE_SSA_SPILL, src: vid, ty, addr: ValueId(addr_vid as usize) }),
            prov, side,
        ));
        spills_by_owner.entry(owner_bi).or_default().extend(new_stmts);
    }
    for (owner_bi, new_stmt_ids) in spills_by_owner {
        blocks[owner_bi].stmts.extend(new_stmt_ids.into_iter().map(|v| ValueId(v as usize)));
    }

    // Emit reloads: prepend (address computation, storage-read) to the
    // START of each use block's own stmts, then substitute every
    // reference to the original value with the reload's own result
    // throughout that block's stmts and terminator.
    for bi in 0..blocks.len() {
        let uses = &uses_per_block[bi];
        if uses.is_empty() {
            continue;
        }
        let mut prelude: Vec<ValueId> = Vec::new();
        let mut subst: BTreeMap<u32, ValueId> = BTreeMap::new();
        let sp_bits = sp_bits_for.get(&bi).cloned();
        for &v in uses {
            let vid = ValueId(v);
            let ty = vaffle_value_vtid(module, &values, vid);
            let prov = values[v].prov.clone();
            let side = values[v].side;
            let (new_stmts, addr_vid) = emit_spill_address(&mut values, prov.clone(), side, bit_tid, addr_tid, sp_bits.as_deref(), sp_step, v);
            let reload_vid = values.len();
            values.push(Node::new(
                Value::Op(Stmt::StorageRead { storage: StorageId::VAFFLE_SSA_SPILL, ty, addr: ValueId(addr_vid) }),
                prov, side,
            ));
            prelude.extend(new_stmts.into_iter().map(|v| ValueId(v as usize)));
            prelude.push(ValueId(reload_vid));
            subst.insert(v as u32, ValueId(reload_vid));
        }
        let old_stmts = core::mem::take(&mut blocks[bi].stmts);
        blocks[bi].stmts = prelude.into_iter().chain(old_stmts).collect();

        let subst_fn = |vid: ValueId| -> Result<ValueId, core::convert::Infallible> {
            Ok(subst.get(&(vid.0 as u32)).copied().unwrap_or(vid))
        };
        for &svid in &blocks[bi].stmts {
            let placeholder = Value::Op(Stmt::Const(Constant { hi: 0, lo: 0 }, TypeId(0)));
            let old = core::mem::replace(&mut values[svid.0].kind, placeholder);
            values[svid.0].kind = old.map(&mut (), |_: &mut (), v: ValueId| subst_fn(v)).unwrap();
        }
        let placeholder_term = Terminator::Return { values: Vec::new() };
        let old_term = core::mem::replace(&mut blocks[bi].terminator, placeholder_term);
        blocks[bi].terminator = old_term.map(&mut (), |_: &mut (), v: ValueId| subst_fn(v)).unwrap();
    }

    let out = FuncBody { sig: body.sig, blocks, values, entry: body.entry };
    debug_assert!(
        compute_cross_block_values(&out).is_empty(),
        "vaffle_ssa: postcondition violated -- cross-block values remain after spilling"
    );
    out
}

/// Emit `SP + vid` as a sequence of new value-arena entries, appended
/// directly to `values` (the `vaffle_ssa`-in-progress arena, so this
/// composes with the rest of the pass's own append-only insertion
/// style). Returns the *new* statement ids created (in emission order,
/// ending with the address's own top-level `Merge` id) and that
/// `Merge`'s own `ValueId.0` (the address to use).
///
/// **Not a real adder.** `sp_step` (see [`compute_sp_step`]) is always an
/// exact power of two, and every `vid` this pass ever spills/reloads is
/// `< sp_step` by that same function's construction. Since `SP` only
/// ever advances by whole multiples of `sp_step` (`ssa_ify_function`'s
/// entry seeds `SP = 0`; [`advance_sp`] only ever adds one more
/// `sp_step`), `SP`'s own low `k = log2(sp_step)` bits are always zero
/// -- so `SP + vid == SP | vid` exactly, with **no carries possible**
/// (`vid` occupies only those same low `k` bits; `SP`'s set bits all sit
/// at position `>= k`). That turns the classic "spill address" ripple-
/// carry adder (`~3 * SPILL_ADDR_BITS` new statements, paid at *every*
/// spill and *every* reload -- tens of thousands of times in a real
/// module) into a `Merge` of `vid`'s own `k` constant low bits with `SP`'s
/// existing high bits *reused directly, with zero new statements* for
/// them.
fn emit_spill_address<P: Clone>(
    values: &mut Vec<Node<Value, P>>,
    prov: P,
    side: Option<volar_side::SideId>,
    bit_tid: TypeId,
    addr_tid: TypeId,
    sp_bits: Option<&[ValueId]>,
    sp_step: u128,
    vid: usize,
) -> (Vec<u32>, usize) {
    let k = sp_step.trailing_zeros() as usize;
    debug_assert!((vid as u128) < sp_step, "vaffle_ssa: value id {vid} >= sp_step ({sp_step}) -- compute_sp_step's own invariant was violated");
    debug_assert!(k <= SPILL_ADDR_BITS, "vaffle_ssa: sp_step ({sp_step}) needs more than SPILL_ADDR_BITS ({SPILL_ADDR_BITS}) low bits");

    let start = values.len();
    let mut builder = VecBuilder { values, prov: prov.clone(), side, bit_tid };
    let low_bits: Vec<ValueId> = (0..k)
        .map(|i| builder.bc_const(((vid >> i) & 1) != 0))
        .collect();
    // `sp_bits = None` means the entry function (see `ssa_ify_function`'s
    // Phase 1 comment) -- its SP is statically 0 everywhere, so its own
    // high (`SPILL_ADDR_BITS - k`) address bits are synthesized as fresh
    // zero consts right here instead of reused from a threaded param.
    let high_bits: Vec<ValueId> = match sp_bits {
        Some(sp) => sp[k..SPILL_ADDR_BITS].to_vec(),
        None => (k..SPILL_ADDR_BITS).map(|_| builder.bc_const(false)).collect(),
    };
    let sum_bits: Vec<ValueId> = low_bits.into_iter().chain(high_bits).collect();
    // Pack the sum's individual bits into one `SPILL_ADDR_BITS`-wide
    // value via Merge, matching how every other address in this pipeline
    // (e.g. `lower_to_ir.rs`'s own `StackPtr`-based addressing) composes
    // a multi-bit address from individual bits.
    let addr_vid = values.len();
    values.push(Node::new(Value::Op(Stmt::Merge { parts: sum_bits, ty: addr_tid }), prov, side));
    let new_ids: Vec<u32> = (start as u32..=addr_vid as u32).collect();
    (new_ids, addr_vid)
}

/// Thread a fresh SP through every block of a **non-entry** function, in
/// RPO order (guaranteeing every non-back-edge predecessor is processed
/// before its successors). Returns each block's own SP bits, received as
/// an extra parameter appended to every block's existing params (wired
/// from its own callers at its entry block -- see [`wire_call_sites`] --
/// and threaded onward from there via ordinary jump/branch args). Never
/// called for the entry function -- see `ssa_ify_function`'s Phase 1
/// comment for why its SP needs no threading at all.
fn thread_sp<P: Clone>(
    blocks: &mut [Block],
    values: &mut Vec<Node<Value, P>>,
    rpo: &[usize],
    bit_tid: TypeId,
) -> BTreeMap<usize, Vec<ValueId>> {
    // Needs an existing `Node` to clone `P`'s provenance/side from for
    // every new SP param this creates -- there is no `P: Default` bound
    // available to conjure one from nothing.
    assert!(
        !values.is_empty(),
        "vaffle_ssa: a non-entry function body has zero values -- cannot seed provenance \
         for the SP params it must receive; give it at least one real value (even an \
         unused one), e.g. one of its own formal parameters",
    );
    let mut sp_bits_for: BTreeMap<usize, Vec<ValueId>> = BTreeMap::new();
    let seed_prov = values[0].prov.clone();
    let seed_side = values[0].side;

    for &bi in rpo {
        let bits: Vec<ValueId> = (0..SPILL_ADDR_BITS)
            .map(|_| push_block_param(blocks, values, bi, bit_tid, seed_prov.clone(), seed_side))
            .collect();
        sp_bits_for.insert(bi, bits);
    }

    // Wire SP args on every ordinary (non-call) intra-function edge.
    // Terminator::Return/ReturnCall don't target a block in this body at
    // all (Return exits the function; ReturnCall is a tail call to a
    // DIFFERENT function, handled by `wire_call_sites`), so neither needs
    // an entry here.
    for &bi in rpo {
        let sp = sp_bits_for[&bi].clone();
        match &mut blocks[bi].terminator {
            Terminator::Jump(t) => {
                t.args.extend(sp.iter().copied());
            }
            Terminator::IfNonzero { then_target, else_target, .. } => {
                then_target.args.extend(sp.iter().copied());
                else_target.args.extend(sp.iter().copied());
            }
            Terminator::Table { targets, default_target, .. } => {
                for t in targets.iter_mut() {
                    t.args.extend(sp.iter().copied());
                }
                default_target.args.extend(sp.iter().copied());
            }
            _ => {}
        }
    }

    sp_bits_for
}

/// Append one fresh `Bit`-typed param to block `bi`: allocate a new
/// arena entry (`Value::Param { block, ty, idx }`, at the arena's next
/// free `ValueId`) and register it in `blocks[bi].params`, matching the
/// convention used by every other block-param-emitting site in this
/// crate (`target.rs`'s own `emit_block_param`).
fn push_block_param<P: Clone>(
    blocks: &mut [Block],
    values: &mut Vec<Node<Value, P>>,
    bi: usize,
    ty: TypeId,
    prov: P,
    side: Option<volar_side::SideId>,
) -> ValueId {
    let idx = blocks[bi].params.len();
    let vid = ValueId(values.len());
    values.push(Node::new(Value::Param { block: BlockId(bi), ty, idx }, prov, side));
    blocks[bi].params.push((vid, ty));
    vid
}

/// Advance SP by `sp_step` at every VAFFLE-internal call site (both a
/// `Value::Call` statement's own args, and a `Terminator::ReturnCall`'s
/// own args) and append the result as that call's own extra trailing
/// args. A call targeting the module's own entry function (`FuncId(0)`,
/// matching [`ssa_ify_module`]'s own convention) is asserted against —
/// the entry function has no threaded-SP param to receive such an arg,
/// since nothing inside the module is expected to call it.
fn wire_call_sites<P: Clone>(
    blocks: &mut [Block],
    values: &mut Vec<Node<Value, P>>,
    rpo: &[usize],
    sp_bits_for: &BTreeMap<usize, Vec<ValueId>>,
    sp_step: u128,
    bit_tid: TypeId,
) {
    // `sp_bits_for` is empty for the entry function (see
    // `ssa_ify_function`'s Phase 1 comment) -- `.get` yields `None` for
    // every block there, correctly routing every one of its own call
    // sites through `advance_sp`'s "statically-zero SP" case below.
    for &bi in rpo {
        let sp: Option<Vec<ValueId>> = sp_bits_for.get(&bi).cloned();
        let old_stmts = core::mem::take(&mut blocks[bi].stmts);
        let mut new_stmts: Vec<ValueId> = Vec::with_capacity(old_stmts.len());
        for svid in old_stmts {
            if matches!(&values[svid.0].kind, Value::Call { func, .. } if func.0 == 0) {
                panic!("vaffle_ssa: a call site targets the module's own entry function (FuncId(0)) -- unsupported, nothing inside a VAFFLE module should call its own entry point");
            }
            let is_call = matches!(&values[svid.0].kind, Value::Call { func, .. } if func.0 != 0);
            if is_call {
                let prov = values[svid.0].prov.clone();
                let side = values[svid.0].side;
                // Insert the SP-advance computation immediately before the
                // call statement it feeds -- `blocks[bi].stmts` is an
                // ordered list that `lower_to_ir.rs` binds left-to-right
                // into its per-block `val_map`, so any value the call's
                // own (now-extended) args reference must already appear
                // earlier in this list.
                let start = values.len();
                let callee_sp = advance_sp(values, prov, side, bit_tid, sp.as_deref(), sp_step);
                new_stmts.extend((start..values.len()).map(ValueId));
                if let Value::Call { args, .. } = &mut values[svid.0].kind {
                    args.extend(callee_sp);
                }
            }
            new_stmts.push(svid);
        }
        blocks[bi].stmts = new_stmts;

        if let Terminator::ReturnCall { func, args } = &mut blocks[bi].terminator {
            assert_ne!(func.0, 0, "vaffle_ssa: a tail-call site targets the module's own entry function (FuncId(0)) -- unsupported");
            assert!(
                !values.is_empty(),
                "vaffle_ssa: a function whose entire body is a single tail call has zero \
                 values -- cannot seed provenance for the SP-advance this call needs; give \
                 it at least one real value (even an unused one)",
            );
            let prov = values[0].prov.clone();
            let side = values[0].side;
            let start = values.len();
            let callee_sp = advance_sp(values, prov, side, bit_tid, sp.as_deref(), sp_step);
            // A tail call is the terminator itself -- appending at the end
            // of `stmts` (which always runs before the terminator) is
            // sufficient here, no specific insertion point needed.
            blocks[bi].stmts.extend((start..values.len()).map(ValueId));
            args.extend(callee_sp);
        }
    }
}

/// Compute the SP a callee should receive: `sp + sp_step`. `sp = None`
/// means the caller is the entry function (see `ssa_ify_function`'s
/// Phase 1 comment) -- its own SP is statically 0, so `0 + sp_step`
/// collapses to `sp_step`'s own bit pattern directly, no adder needed.
fn advance_sp<P: Clone>(
    values: &mut Vec<Node<Value, P>>,
    prov: P,
    side: Option<volar_side::SideId>,
    bit_tid: TypeId,
    sp: Option<&[ValueId]>,
    sp_step: u128,
) -> Vec<ValueId> {
    let mut builder = VecBuilder { values, prov, side, bit_tid };
    let step_bits: Vec<ValueId> = (0..SPILL_ADDR_BITS)
        .map(|i| builder.bc_const(((sp_step >> i) & 1) != 0))
        .collect();
    match sp {
        Some(sp) => bc_add(&mut builder, sp, &step_bits, false),
        None => step_bits,
    }
}

fn block_successors(term: &Terminator) -> Vec<usize> {
    match term {
        Terminator::Return { .. } | Terminator::ReturnCall { .. } => Vec::new(),
        Terminator::Jump(t) => alloc::vec![t.block.0],
        Terminator::IfNonzero { then_target, else_target, .. } => {
            alloc::vec![then_target.block.0, else_target.block.0]
        }
        Terminator::Table { targets, default_target, .. } => {
            let mut v: Vec<usize> = targets.iter().map(|t| t.block.0).collect();
            v.push(default_target.block.0);
            v
        }
        // `Terminator` is `#[non_exhaustive]`.
        _ => Vec::new(),
    }
}

fn build_preds(blocks: &[Block]) -> Vec<BTreeSet<usize>> {
    let mut preds = alloc::vec![BTreeSet::new(); blocks.len()];
    for (bi, block) in blocks.iter().enumerate() {
        for succ in block_successors(&block.terminator) {
            preds[succ].insert(bi);
        }
    }
    preds
}

/// Reverse postorder over the CFG reachable from `entry`. Blocks
/// unreachable from `entry` are omitted (their `idom` stays `usize::MAX`,
/// and no cross-block value should ever have its owner or a use in such a
/// block for a well-formed function).
fn compute_rpo(n: usize, entry: usize, preds: &[BTreeSet<usize>]) -> Vec<usize> {
    let mut succs: Vec<Vec<usize>> = alloc::vec![Vec::new(); n];
    for (bi, ps) in preds.iter().enumerate() {
        for &p in ps {
            succs[p].push(bi);
        }
    }

    let mut visited = alloc::vec![false; n];
    let mut post_order = Vec::with_capacity(n);
    let mut stack: Vec<(usize, usize)> = alloc::vec![(entry, 0)];
    visited[entry] = true;
    while let Some(&mut (node, ref mut next_child)) = stack.last_mut() {
        if *next_child < succs[node].len() {
            let child = succs[node][*next_child];
            *next_child += 1;
            if !visited[child] {
                visited[child] = true;
                stack.push((child, 0));
            }
        } else {
            post_order.push(node);
            stack.pop();
        }
    }
    post_order.reverse();
    post_order
}

/// Standard iterative dominator computation (Cooper/Harvey/Kennedy "A
/// Simple, Fast Dominance Algorithm"). `idom[b] == b` for the entry block;
/// `idom[b] == usize::MAX` for a block never reached during the RPO walk
/// (unreachable from `entry`).
fn compute_idom(rpo: &[usize], rpo_index: &[usize], preds: &[BTreeSet<usize>], entry: usize) -> Vec<usize> {
    let n = preds.len();
    let mut idom = alloc::vec![usize::MAX; n];
    idom[entry] = entry;
    let mut changed = true;
    while changed {
        changed = false;
        for &b in rpo {
            if b == entry {
                continue;
            }
            let mut new_idom = usize::MAX;
            for &p in &preds[b] {
                if idom[p] == usize::MAX {
                    continue;
                }
                new_idom = if new_idom == usize::MAX {
                    p
                } else {
                    intersect(new_idom, p, &idom, rpo_index)
                };
            }
            if new_idom != idom[b] {
                idom[b] = new_idom;
                changed = true;
            }
        }
    }
    idom
}

fn intersect(mut a: usize, mut b: usize, idom: &[usize], rpo_index: &[usize]) -> usize {
    while a != b {
        while rpo_index[a] > rpo_index[b] {
            a = idom[a];
        }
        while rpo_index[b] > rpo_index[a] {
            b = idom[b];
        }
    }
    a
}

/// Does block `a` dominate block `b` (every path from the function's
/// entry to `b` passes through `a`)? `a` trivially dominates itself.
fn dominates(a: usize, mut b: usize, idom: &[usize]) -> bool {
    loop {
        if a == b {
            return true;
        }
        if b == usize::MAX || idom[b] == usize::MAX {
            return false;
        }
        let next = idom[b];
        if next == b {
            // Reached the entry block without finding `a`.
            return false;
        }
        b = next;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;
    use vaffle::{SigDecl, SigId, Target};
    use volar_ir_common::{IrType, Stmt as CommonStmt, TypeTable};

    fn mk_module(n_types: usize) -> Module<()> {
        let mut types = TypeTable::new();
        for _ in 0..n_types {
            types.intern(IrType::Primitive(volar_ir_common::Type::Bit));
        }
        Module {
            types,
            oracles: Vec::new(),
            actions: Vec::new(),
            funcs: Vec::new(),
            sigs: alloc::vec![SigDecl { params: Vec::new(), results: Vec::new() }],
            exports: BTreeMap::new(),
            pre_init: Vec::new(),
        }
    }

    fn node(v: Value) -> Node<Value, ()> {
        Node::new(v, (), None)
    }

    fn const_op(lo: u128) -> Value {
        Value::Op(CommonStmt::Const(Constant { hi: 0, lo }, TypeId(0)))
    }

    fn setup(module: &mut Module<()>) -> (TypeId, TypeId, u128) {
        let bit_tid = module.types.bit();
        let addr_tid = module.types.intern(IrType::Vec(SPILL_ADDR_BITS, bit_tid));
        (bit_tid, addr_tid, 1u128 << 20)
    }

    fn is_storage_write(values: &[Node<Value, ()>], vid: ValueId) -> bool {
        matches!(&values[vid.0].kind, Value::Op(CommonStmt::StorageWrite { storage, .. }) if *storage == StorageId::VAFFLE_SSA_SPILL)
    }
    fn is_storage_read(values: &[Node<Value, ()>], vid: ValueId) -> bool {
        matches!(&values[vid.0].kind, Value::Op(CommonStmt::StorageRead { storage, .. }) if *storage == StorageId::VAFFLE_SSA_SPILL)
    }

    #[test]
    fn test_entry_function_gets_no_sp_threading_at_all() {
        // block0: defines v0, jumps to block1 (no args).
        // block1: defines v1 (its own), returns v1.
        // Nothing calls the entry function and it has no cross-block
        // values here -- it should come out of this pass completely
        // untouched (no SP params anywhere, no new stmts), since its own
        // SP is provably always 0 and nothing needs it materialized.
        let values = vec![node(const_op(1)), node(const_op(2))];
        let blocks = vec![
            Block { params: Vec::new(), stmts: vec![ValueId(0)], terminator: Terminator::Jump(Target { block: BlockId(1), args: Vec::new(), reentry: None }) },
            Block { params: Vec::new(), stmts: vec![ValueId(1)], terminator: Terminator::Return { values: vec![ValueId(1)] } },
        ];
        let body = FuncBody { sig: SigId(0), blocks, values, entry: BlockId(0) };
        let mut module = mk_module(1);
        let (bit_tid, addr_tid, sp_step) = setup(&mut module);

        let out = ssa_ify_function(&module, &body, addr_tid, bit_tid, sp_step, true);
        assert_eq!(out.blocks[0].params.len(), 0);
        assert_eq!(out.blocks[0].stmts.len(), 1, "no SP threading needed -- entry has no calls and no cross-block values");
        assert_eq!(out.blocks[1].params.len(), 0, "no SP param threaded to block1 either -- entry never needs it");
        assert!(compute_cross_block_values(&out).is_empty());
    }

    #[test]
    fn test_entry_function_with_cross_block_value_spills_with_no_sp_reference() {
        // block0 (entry): defines v0, jumps to block1.
        // block1: uses v0 (cross-block) -- v0 must be spilled/reloaded,
        // but since the *entry* function owns it, the spill/reload
        // address should be `vid` alone (SP known statically-0), with no
        // SP param threaded anywhere.
        let values = vec![node(const_op(7))];
        let blocks = vec![
            Block { params: Vec::new(), stmts: vec![ValueId(0)], terminator: Terminator::Jump(Target { block: BlockId(1), args: Vec::new(), reentry: None }) },
            Block { params: Vec::new(), stmts: Vec::new(), terminator: Terminator::Return { values: vec![ValueId(0)] } },
        ];
        let body = FuncBody { sig: SigId(0), blocks, values, entry: BlockId(0) };
        let mut module = mk_module(1);
        let (bit_tid, addr_tid, sp_step) = setup(&mut module);

        let out = ssa_ify_function(&module, &body, addr_tid, bit_tid, sp_step, true);
        assert_eq!(out.blocks[0].params.len(), 0, "entry never receives/threads an SP param");
        assert_eq!(out.blocks[1].params.len(), 0, "no SP param threaded to the use-block either");
        assert!(out.blocks[0].stmts.iter().any(|&v| is_storage_write(&out.values, v)));
        assert!(out.blocks[1].stmts.iter().any(|&v| is_storage_read(&out.values, v)));
        assert!(compute_cross_block_values(&out).is_empty());
    }

    #[test]
    fn test_non_entry_function_sp_is_threaded_param() {
        let values = vec![node(const_op(1))];
        let blocks = vec![
            Block { params: Vec::new(), stmts: vec![ValueId(0)], terminator: Terminator::Return { values: vec![ValueId(0)] } },
        ];
        let body = FuncBody { sig: SigId(0), blocks, values, entry: BlockId(0) };
        let mut module = mk_module(1);
        let (bit_tid, addr_tid, sp_step) = setup(&mut module);

        let out = ssa_ify_function(&module, &body, addr_tid, bit_tid, sp_step, false);
        assert_eq!(out.blocks[0].params.len(), SPILL_ADDR_BITS, "non-entry function receives SP as 32 extra params");
    }

    #[test]
    fn test_diamond_join_spills_once_reloads_at_each_use_with_sp_addressing() {
        let values = vec![
            node(const_op(1)),
            node(Value::Op(CommonStmt::Splat { src: ValueId(0), ty: TypeId(0) })),
            node(Value::Op(CommonStmt::Splat { src: ValueId(0), ty: TypeId(0) })),
        ];
        let blocks = vec![
            Block {
                params: Vec::new(), stmts: vec![ValueId(0)],
                terminator: Terminator::IfNonzero {
                    cond: ValueId(0),
                    then_target: Target { block: BlockId(1), args: Vec::new(), reentry: None },
                    else_target: Target { block: BlockId(2), args: Vec::new(), reentry: None },
                },
            },
            Block {
                params: Vec::new(), stmts: vec![ValueId(1)],
                terminator: Terminator::Jump(Target { block: BlockId(3), args: Vec::new(), reentry: None }),
            },
            Block {
                params: Vec::new(), stmts: vec![ValueId(2)],
                terminator: Terminator::Jump(Target { block: BlockId(3), args: Vec::new(), reentry: None }),
            },
            Block {
                params: Vec::new(), stmts: Vec::new(),
                terminator: Terminator::Return { values: vec![ValueId(0)] },
            },
        ];
        let body = FuncBody { sig: SigId(0), blocks, values, entry: BlockId(0) };
        let mut module = mk_module(1);
        let (bit_tid, addr_tid, sp_step) = setup(&mut module);

        let out = ssa_ify_function(&module, &body, addr_tid, bit_tid, sp_step, true);
        // block0 gains: 32 SP-zero consts, then (vid-const-bits + bc_add
        // stmts + Merge + StorageWrite) for the spill -- exact count
        // depends on bc_add's own internal stmt count, so just check the
        // LAST new stmt is genuinely a StorageWrite into our spill space.
        assert!(is_storage_write(&out.values, *out.blocks[0].stmts.last().unwrap()));
        // block1/block2 each directly use v0 -- each gets its own reload,
        // prefixed before their own original stmt.
        let block1_reload = out.blocks[1].stmts.iter().find(|&&v| is_storage_read(&out.values, v));
        assert!(block1_reload.is_some(), "block1 should contain a reload of v0");
        let block2_reload = out.blocks[2].stmts.iter().find(|&&v| is_storage_read(&out.values, v));
        assert!(block2_reload.is_some(), "block2 should contain a reload of v0");
        assert!(compute_cross_block_values(&out).is_empty());
    }

    #[test]
    fn test_call_site_advances_sp_and_appends_it_to_args() {
        // func0 (entry, calls func1): block0 calls func1 with no real
        // args, returns its output.
        // func1 (non-entry, callee): block0 just returns a fresh value.
        let f0_values = vec![
            node(Value::Call { func: FuncId(1), args: Vec::new() }),
            node(Value::Output { value: ValueId(0), idx: 0 }),
        ];
        let f0_blocks = vec![
            Block { params: Vec::new(), stmts: vec![ValueId(0), ValueId(1)], terminator: Terminator::Return { values: vec![ValueId(1)] } },
        ];
        let f0 = FuncBody { sig: SigId(0), blocks: f0_blocks, values: f0_values, entry: BlockId(0) };

        let f1_values = vec![node(const_op(7))];
        let f1_blocks = vec![
            Block { params: Vec::new(), stmts: vec![ValueId(0)], terminator: Terminator::Return { values: vec![ValueId(0)] } },
        ];
        let f1 = FuncBody { sig: SigId(0), blocks: f1_blocks, values: f1_values, entry: BlockId(0) };

        let mut module = mk_module(1);
        module.funcs.push(FuncDecl::Body(f0));
        module.funcs.push(FuncDecl::Body(f1));

        let out = ssa_ify_module(&module);
        let FuncDecl::Body(out_f0) = &out.funcs[0] else { panic!("expected Body") };
        let FuncDecl::Body(out_f1) = &out.funcs[1] else { panic!("expected Body") };

        // func1 (callee, non-entry) should have gained 32 SP params.
        assert_eq!(out_f1.blocks[0].params.len(), SPILL_ADDR_BITS);

        // func0's own call site should now pass 32 extra args (the
        // advanced SP) beyond whatever it originally passed (zero).
        let call_stmt = out_f0.blocks[0].stmts.iter()
            .find_map(|&svid| match &out_f0.values[svid.0].kind {
                Value::Call { func, args } if func.0 == 1 => Some(args.clone()),
                _ => None,
            })
            .expect("call to func1 must still exist");
        assert_eq!(call_stmt.len(), SPILL_ADDR_BITS, "call site should carry exactly the 32 advanced-SP bits (no original args)");
    }

    /// The core recursion-safety property this whole SP-threading design
    /// exists for: `compute_sp_step` must return a step strictly larger
    /// than every `ValueId` in the module, so that at any two distinct
    /// recursion depths `d1 != d2`, `SP_d1 + vid1 == SP_d2 + vid2` is
    /// only possible if `vid1 == vid2` and `d1 == d2` (`SP_d = d *
    /// sp_step`, and `|vid2 - vid1| < sp_step` always holds since both
    /// are less than `sp_step`, so a nonzero multiple of `sp_step` can
    /// never equal that difference) -- i.e. no two call depths' own
    /// `SP + ValueId` spill ranges can ever collide.
    #[test]
    fn test_compute_sp_step_exceeds_every_value_id_in_the_module() {
        let f_values: Vec<Node<Value, ()>> = (0..5000).map(|i| node(const_op(i))).collect();
        let f_blocks = vec![
            Block { params: Vec::new(), stmts: (0..5000).map(ValueId).collect(), terminator: Terminator::Return { values: Vec::new() } },
        ];
        let f = FuncBody { sig: SigId(0), blocks: f_blocks, values: f_values, entry: BlockId(0) };
        let mut module = mk_module(1);
        module.funcs.push(FuncDecl::Body(f));

        let sp_step = compute_sp_step(&module);
        assert!(sp_step > 5000, "sp_step ({sp_step}) must exceed every ValueId in the module (max 4999) with margin");
    }

    /// A self-recursive function's own internal call site (calling its
    /// own `FuncId`) must be wired exactly like a call to any other
    /// function: SP advanced by `sp_step` and appended as extra args --
    /// exercising the `func.0 == this function's own id` path through
    /// `wire_call_sites`, distinct from the ordinary cross-function case
    /// [`test_call_site_advances_sp_and_appends_it_to_args`] already
    /// covers.
    #[test]
    fn test_self_recursive_call_site_advances_its_own_sp() {
        // func1 (non-entry, self-recursive): calls itself, returns the result.
        let f0_values = vec![node(const_op(1))];
        let f0_blocks = vec![
            Block { params: Vec::new(), stmts: vec![ValueId(0)], terminator: Terminator::Return { values: vec![ValueId(0)] } },
        ];
        let f0 = FuncBody { sig: SigId(0), blocks: f0_blocks, values: f0_values, entry: BlockId(0) };

        let f1_values = vec![
            node(Value::Call { func: FuncId(1), args: Vec::new() }),
            node(Value::Output { value: ValueId(0), idx: 0 }),
        ];
        let f1_blocks = vec![
            Block { params: Vec::new(), stmts: vec![ValueId(0), ValueId(1)], terminator: Terminator::Return { values: vec![ValueId(1)] } },
        ];
        let f1 = FuncBody { sig: SigId(0), blocks: f1_blocks, values: f1_values, entry: BlockId(0) };

        let mut module = mk_module(1);
        module.funcs.push(FuncDecl::Body(f0));
        module.funcs.push(FuncDecl::Body(f1));

        let out = ssa_ify_module(&module);
        let FuncDecl::Body(out_f1) = &out.funcs[1] else { panic!("expected Body") };

        assert_eq!(out_f1.blocks[0].params.len(), SPILL_ADDR_BITS, "self-recursive function still receives SP as a normal threaded param");

        let call_args = out_f1.blocks[0].stmts.iter()
            .find_map(|&svid| match &out_f1.values[svid.0].kind {
                Value::Call { func, args } if func.0 == 1 => Some(args.clone()),
                _ => None,
            })
            .expect("self-recursive call to func1 must still exist");
        assert_eq!(call_args.len(), SPILL_ADDR_BITS, "self-recursive call site should carry the 64 advanced-SP bits");
        // The advanced SP must be a genuinely different value chain than
        // the block's own (unadvanced) SP params -- i.e. this is a real
        // `bc_add`, not an accidental pass-through of the caller's own SP.
        let own_sp: BTreeSet<usize> = out_f1.blocks[0].params[..SPILL_ADDR_BITS].iter().map(|(v, _)| v.0).collect();
        assert!(call_args.iter().all(|v| !own_sp.contains(&v.0)), "advanced SP must not alias the caller's own unadvanced SP bits");
    }

    #[test]
    #[should_panic(expected = "unsupported, nothing inside a VAFFLE module should call its own entry point")]
    fn test_call_to_entry_function_panics() {
        let f0_values = vec![node(const_op(1))];
        let f0_blocks = vec![
            Block { params: Vec::new(), stmts: vec![ValueId(0)], terminator: Terminator::Return { values: vec![ValueId(0)] } },
        ];
        let f0 = FuncBody { sig: SigId(0), blocks: f0_blocks, values: f0_values, entry: BlockId(0) };

        let f1_values = vec![node(Value::Call { func: FuncId(0), args: Vec::new() })];
        let f1_blocks = vec![
            Block { params: Vec::new(), stmts: vec![ValueId(0)], terminator: Terminator::Return { values: Vec::new() } },
        ];
        let f1 = FuncBody { sig: SigId(0), blocks: f1_blocks, values: f1_values, entry: BlockId(0) };

        let mut module = mk_module(1);
        module.funcs.push(FuncDecl::Body(f0));
        module.funcs.push(FuncDecl::Body(f1));
        let _ = ssa_ify_module(&module);
    }

    #[test]
    #[should_panic(expected = "dominance invariant is violated")]
    fn test_dominance_violation_panics() {
        // block0 (entry): branches on v_cond to block1 or block2.
        // block1: defines v, jumps to block3.
        // block2: does NOT define v, jumps to block3.
        // block3 (reachable via block2's path, which never computes v):
        // uses v -- block1 (v's owner) does not dominate block3, a real
        // violation on a genuinely reachable block (unlike an unreachable
        // dangling reference, which `vaffle_ssa` deliberately leaves alone
        // as dead code -- see `ssa_ify_function`'s Phase 3 comment).
        let values = vec![node(const_op(1)), node(const_op(2))];
        let blocks = vec![
            Block {
                params: Vec::new(), stmts: vec![ValueId(0)],
                terminator: Terminator::IfNonzero {
                    cond: ValueId(0),
                    then_target: Target { block: BlockId(1), args: Vec::new(), reentry: None },
                    else_target: Target { block: BlockId(2), args: Vec::new(), reentry: None },
                },
            },
            Block { params: Vec::new(), stmts: vec![ValueId(1)], terminator: Terminator::Jump(Target { block: BlockId(3), args: Vec::new(), reentry: None }) },
            Block { params: Vec::new(), stmts: Vec::new(), terminator: Terminator::Jump(Target { block: BlockId(3), args: Vec::new(), reentry: None }) },
            Block { params: Vec::new(), stmts: Vec::new(), terminator: Terminator::Return { values: vec![ValueId(1)] } },
        ];
        let body = FuncBody { sig: SigId(0), blocks, values, entry: BlockId(0) };
        let mut module = mk_module(1);
        let (bit_tid, addr_tid, sp_step) = setup(&mut module);
        let _ = ssa_ify_function(&module, &body, addr_tid, bit_tid, sp_step, true);
    }

    #[test]
    fn test_dominates_basic_diamond() {
        let preds: Vec<BTreeSet<usize>> = vec![
            BTreeSet::new(),
            BTreeSet::from([0]),
            BTreeSet::from([0]),
            BTreeSet::from([1, 2]),
        ];
        let rpo = compute_rpo(4, 0, &preds);
        let mut rpo_index = alloc::vec![usize::MAX; 4];
        for (i, &b) in rpo.iter().enumerate() { rpo_index[b] = i; }
        let idom = compute_idom(&rpo, &rpo_index, &preds, 0);
        assert!(dominates(0, 3, &idom));
        assert!(dominates(0, 1, &idom));
        assert!(!dominates(1, 3, &idom));
        assert!(!dominates(2, 3, &idom));
        assert!(dominates(3, 3, &idom));
    }
}
