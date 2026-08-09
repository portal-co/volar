//! Milestone 1.6: generates the runtime-loop driver's own statement
//! sequence -- interleaving calls to a *split* VOLE weave's real, compiled
//! prover/`QSim`/verifier functions -- one function per movfuscated block,
//! plus chunked accumulator functions, plus a finish function -- threading
//! each function's own exported state into the next's matching input
//! params.
//!
//! Built once here (validated first on `mem_probe`'s small circuit) and
//! reused as-is for the real interpreter (Stage 2, `interpreter_e2e.rs`):
//! the shape of "one function per block, chunked accumulator, finish" is
//! identical in both cases (same `movfuscate_ir_with_boundary`/split-weave
//! machinery), only the *size* differs.
//!
//! # AST, not text
//!
//! Builds a real `Vec<IrStmt>` (via `crate::ir_builder`'s small toolkit),
//! not a hand-formatted `String` -- the runtime-loop driver's own
//! statement sequence is now genuinely representable in the same IR the
//! rest of this pipeline uses, a prerequisite for ever routing it through
//! `lower_module_monomorphized` (the LIR/C-backend path) instead of only
//! the Rust-text printer. `generate_split_step`'s own public signature is
//! unchanged -- still returns a printed `String` (via
//! `ir_builder::print_stmts`, called once at the very end) -- so today's
//! callers (`mem_probe.rs`/`wat_gen.rs`) need no changes at all; a new
//! `generate_split_step_ir` sibling returns the real `Vec<IrStmt>` for a
//! future caller that wants it directly.
//!
//! Most *leaf* sub-expressions still carry pre-formatted Rust expression
//! text as a plain `String` (`role_one`, `all_ok_in`/`fold_state_in`,
//! `running_in`'s own tuple-field accessors, ...) rather than a fully
//! structured `IrExpr` tree -- wrapped into the AST via `var(&text)` at
//! the point of use. This is the same "smuggle text through `Var`"
//! convention `volar-weaver`'s own pool-index addressing already relies
//! on (see `volar_compiler::ir::digit_var_as_literal`'s own doc), applied
//! more broadly here: the *statement*-level structure (which function
//! gets called, in what order, which locals bind which results, real
//! tuple destructuring/array literals/indexed pool writes) is real,
//! walkable IR; a handful of already-simple leaf expressions stay as
//! opaque text. Safe for any consumer that only ever *prints* this AST
//! (this module's only consumer today, via `print_stmts`); a future
//! consumer needing real semantics from every leaf (e.g. lowering the
//! driver itself through `lower_module_monomorphized`, not just the woven
//! functions it calls) would need to finish structuring these remaining
//! leaves into real `IrExpr` trees -- not attempted here, flagged as the
//! natural next increment.
//!
//! # Design
//!
//! Every param/return-slot name this module relies on comes directly from
//! `crates/compiler/volar-weaver/src/vole.rs`'s own split-weave naming
//! conventions (`weave_vole_prover_ir_split`/`weave_vole_qsim_ir_split`/
//! `weave_vole_verifier_ir_split_with_trace`) -- this module does not
//! invent its own naming scheme, it mirrors the real one exactly, so it
//! can never drift from what the weaver actually emits:
//!
//! - Block `i`'s own exported `is_active`/`done`/`next_pc_bits`/
//!   `next_state`/`ret_vals` are consumed by the accumulator chunk that
//!   covers it as `is_active_{i}`/`done_{i}`/`next_pc_{i}_{j}`/
//!   `next_state_{i}_{k}`/`ret_val_{i}_{m}` (`next_pc` bits are always
//!   scalar, one param per bit; `next_state`/`ret_val` slots are
//!   array-batched -- one param per slot, `[T; width]`-typed for a wide
//!   slot, plain-typed for a scalar one -- not one param per lane).
//! - Chunk `c`'s own running accumulator (`done_acc`/`next_pc`/
//!   `next_state`/`ret_vals`) is consumed by chunk `c+1` (or `finish`, if
//!   `c` is the last chunk) as `in_done_acc`/`in_next_pc_{j}`/
//!   `in_next_state_{k}`/`in_ret_val_{m}` (same array-batching as above).
//!   Chunk 0's own inputs are always literal zero (movfuscation's own
//!   accumulator seed).
//! - A block/chunk/finish function's own trailing `hat`s (prover) are fed
//!   *directly* to the matching (same name, same index) `QSim` function's
//!   `hat` input (one array param covering every local AND gate, not one
//!   scalar per gate); that `QSim` function's own derived `q_and` values
//!   are fed to the matching `Verifier` function's `q_and` input the same
//!   way -- this threading is *within* one function-index, across roles,
//!   not between function indices.
//! - The verifier's `all_ok`/`fold_state` thread linearly across *every*
//!   verifier-role call in call order (block 0, block 1, ..., chunk 0, ...,
//!   finish) -- unlike next_state, this is a single continuous chain, not
//!   scoped to movfuscation's own block/chunk topology.
//! - Oracle reads (`oracle_rd_k`) are local to each function (reset to 0
//!   per function); this module consumes real committed values
//!   sequentially, in call order, matching the order the split weave's own
//!   aggregated `MemoryTrace` records real `StorageRead`s in (verified by
//!   construction: both come from the same statement-order walk).
//!
//! The `r_ands` "fold challenge" formula (see `r_ands_decl_stmt`) is a
//! plain deterministic arithmetic formula, not real Fiat-Shamir/verifier
//! randomness -- soundness doesn't depend on it being unpredictable here,
//! this is a driven *test*, not a real deployment.

use std::collections::BTreeMap;
use volar_compiler::ir::{ArrayLength, IrExpr, IrExprKind, IrFunction, IrLit, IrStmt, IrType};
use volar_ir_passes::{MovfuscAccumInfo, MovfuscBlockBoundary};

use crate::ir_builder::{
    array_lit_expr, assert_true_stmt, call_expr, clone_expr, clone_var, field_expr, index_lit_expr,
    ir_expr, let_stmt, let_tuple_stmt, let_typed_stmt, parse_leaf_expr, pool_write_stmts,
    print_stmts, r_ands_decl_stmt, ref_expr, ref_mut_expr, slice_ref_mut_expr,
    synth_pool_decl_stmts, tuple_lit_expr, var,
};

/// A driver-tracked value: either one Rust local (a scalar Q/Vope), or a
/// `[T; n]` Rust local a caller can index (a wide slot, one array value
/// covering all `n` lanes) -- mirrors `vole.rs`'s own `WireRepr` split.
#[derive(Clone, Debug)]
pub enum Slot {
    Scalar(String),
    Array(String, usize),
}

fn slot_from_ty(local_name: &str, ty: &IrType) -> Slot {
    match ty {
        IrType::Array { len: ArrayLength::Const(n), .. } => Slot::Array(local_name.to_string(), *n),
        _ => Slot::Scalar(local_name.to_string()),
    }
}

/// The Rust local name a slot is bound to, regardless of whether it's a
/// scalar or a whole (unindexed) array value. `pub`: real-runtime-loop
/// callers (e.g. `mem_probe.rs`) need this to build their own end-of-
/// iteration reassignment statements (`w{i}_vope = {slot_name};`) from
/// `StepResult::next_entry_w`.
pub fn slot_name(s: &Slot) -> &str {
    match s {
        Slot::Scalar(n) | Slot::Array(n, _) => n,
    }
}

/// `<slot>.clone()` as a real IR expression.
fn slot_clone_expr(s: &Slot) -> IrExpr {
    clone_var(slot_name(s))
}

/// Record an exported slot under `base_name` for later lookup by a
/// consuming function's own params -- `bind_scalar` (`vole.rs`) now
/// array-batches a wide slot into *one* param named exactly `base_name`
/// (not `n` separate `{base_name}_{j}` scalar params), so this is a plain
/// insert regardless of whether `slot` is scalar or array-valued.
fn insert_export(exported: &mut BTreeMap<String, Slot>, base_name: String, slot: Slot) {
    exported.insert(base_name, slot);
}

/// As [`insert_export`], for cross-chunk-shared (`synthetic_out`) values,
/// keyed by the var's own stable global id rather than a param-name string.
fn insert_synth_export(exported: &mut BTreeMap<u32, Slot>, var_id: u32, slot: Slot) {
    exported.insert(var_id, slot);
}

fn tuple_elems(ty: &IrType) -> Vec<IrType> {
    match ty {
        IrType::Tuple(v) => v.clone(),
        other => vec![other.clone()],
    }
}

/// Unlike block/chunk functions (whose return type is one *flat* tuple,
/// see `vole.rs`'s `ret_tuple_tys`), the **finish** function's return type
/// is doubly-nested: `(output, hats)` where `output` is itself the raw
/// circuit terminator's own return-arg tuple (`done` followed by one
/// element per original circuit param, or a single bare type if the
/// terminator has exactly one return arg) -- see `vole.rs`'s
/// `output_ty`/`ret_args` construction. `state_local` names the Rust local
/// already bound to that whole (possibly-tuple) `output` value; this
/// destructures it into one fresh, individually-addressable `Slot` per
/// terminator return arg.
fn destructure_finish_output(stmts: &mut Vec<IrStmt>, f: &IrFunction, state_local: &str, uid: &str) -> Vec<Slot> {
    let output_ty = tuple_elems(f.return_type.as_ref().expect("finish function must have a return type"))
        .into_iter().next().expect("finish function's return type must have an output element");
    match output_ty {
        IrType::Tuple(elem_tys) => {
            let names: Vec<String> = (0..elem_tys.len()).map(|k| format!("_{uid}_out_{k}")).collect();
            stmts.push(let_tuple_stmt(&names, var(state_local)));
            names.iter().zip(elem_tys.iter()).map(|(n, ty)| slot_from_ty(n, ty)).collect()
        }
        other => vec![slot_from_ty(state_local, &other)],
    }
}

/// Count of params whose name starts with `prefix` -- used to read back
/// how many `oracle_rd_k`/etc. a given woven function declares (still one
/// scalar param per read; oracle reads aren't array-batched), directly
/// from its own real signature (never assumed).
fn count_params_prefixed(f: &IrFunction, prefix: &str) -> usize {
    f.params.iter().filter(|p| p.name.starts_with(prefix)).count()
}

/// This function's own per-gate AND-check count -- read back from its
/// `q_and` param's own declared array length (`q_and`/`hat`/`r_and` are
/// array-batched, one param each, not one scalar per gate), or 0 if the
/// function has no AND gates at all (param absent).
fn and_count_of(f: &IrFunction) -> usize {
    match f.params.iter().find(|p| p.name == "q_and") {
        Some(p) => match &p.ty {
            IrType::Array { len: ArrayLength::Const(n), .. } => *n,
            other => panic!("expected q_and to be an array param, got {other:?}"),
        },
        None => 0,
    }
}

/// Per-slot width (1 = scalar, >1 = wide/array), read back from a chunk
/// function's own `{prefix}{k}` running-accumulator param's own declared
/// type -- e.g. `prefix = "in_next_state_"` -- rather than assumed, since
/// `MovfuscAccumInfo` itself only records slot *count*, not width.
/// `bind_scalar` array-batches a wide slot into one param (no `_{j}`
/// suffix), so the width is read directly off the param's own type.
fn slot_widths_from_params(f: &IrFunction, prefix: &str, n_slots: usize) -> Vec<usize> {
    let mut widths = vec![1usize; n_slots];
    for p in &f.params {
        if let Some(rest) = p.name.strip_prefix(prefix) {
            let k: usize = rest.parse().unwrap();
            widths[k] = match &p.ty {
                IrType::Array { len: ArrayLength::Const(n), .. } => *n,
                _ => 1,
            };
        }
    }
    widths
}

/// One step's worth of generated statements, calling every block/chunk/
/// finish function (prover, then qsim, then verifier, per function-index)
/// for `prover_funcs`/`qsim_funcs`/`verifier_funcs` (each: blocks ++
/// chunks ++ [finish], in that order -- exactly the order `emit_fn` is
/// called in by the split weave).
///
/// `entry_w`: the current entry-state, as `(vope_slots, q_slots)` pairs,
/// one per original circuit param (index-matched to `circuit.blocks[0].params`).
/// `all_ok_fold_state_in`: `(all_ok_expr, fold_state_expr)` to seed the
/// verifier chain's first call; `None` means "use fresh literals"
/// (`true`/`iop_accumulator_fresh()`).
/// `oracle_bit_exprs`: one already-formatted Rust `bool` EXPRESSION (not a
/// literal `bool` value) per bit, for every real `StorageRead` this step
/// performs, **in the same statement order** the aggregated `MemoryTrace`
/// (from any one of the three split weaves -- identical shape across
/// roles) lists them; each inner `Vec<String>` is LSB-first and must have
/// exactly the read's own bit-width. This function is agnostic to whether
/// each expression is a compile-time literal (`"true"`/`"false"`, the
/// unrolled-per-step calling convention) or a runtime array reference
/// (`"witness[step].oracle_bits[3][2]"`, the real-runtime-loop calling
/// convention) -- the caller decides, this just splices the text in.
/// `step_expr`: a Rust expression (again, either a compile-time literal
/// like `"0"` or a runtime variable like `"step"`) used only to keep
/// `r_and`/RNG-seed values distinct across outer steps (soundness doesn't
/// need this to be unpredictable here -- this is a driven *test*, not a
/// real deployment). Every OTHER per-call uniqueness concern (local
/// variable names) no longer depends on this at all: this function is
/// meant to be called ONCE per real-runtime-loop driver (its own returned
/// `stmts` becomes the loop BODY, executed many times at runtime) or,
/// for the older unrolled calling convention, once per outer step with a
/// distinct literal `step_expr` -- either way, plain Rust block-scoping
/// already gives every `let` binding inside `stmts` a fresh value per
/// call/iteration, so names never needed to be step-qualified for
/// correctness (only `step_expr`'s own r_and-seed use does).
///
/// Returns `(statements, next_entry_w, final_all_ok_expr, final_fold_state_expr, finish_output_slots)`
/// where `finish_output_slots` are the finish function's own *output*
/// tuple slots (the terminator's real return args) -- callers use these
/// for whatever they need the circuit's real output for (e.g. deriving
/// the next step's entry state, or reading a final result).
pub struct StepResult {
    pub stmts: String,
    pub next_entry_w: Vec<(Slot, Slot)>,
    pub final_all_ok_expr: String,
    pub final_fold_state_expr: String,
    pub finish_output_slots: Vec<Slot>,
}

/// As [`StepResult`], but returning the real `Vec<IrStmt>` AST instead of
/// printed text -- for a future caller that wants to route the driver
/// itself through `lower_module_monomorphized` rather than only print it.
/// [`generate_split_step`] is a thin wrapper: calls this, then prints.
pub struct StepResultIr {
    pub stmts: Vec<IrStmt>,
    pub next_entry_w: Vec<(Slot, Slot)>,
    pub final_all_ok_expr: String,
    pub final_fold_state_expr: String,
    pub finish_output_slots: Vec<Slot>,
}

pub fn generate_split_step(
    prover_funcs: &[IrFunction],
    qsim_funcs: &[IrFunction],
    verifier_funcs: &[IrFunction],
    boundary: &[MovfuscBlockBoundary],
    accum_info: &MovfuscAccumInfo,
    n_chunks: usize,
    total_vars: usize,
    entry_w: &[(Slot, Slot)],
    all_ok_fold_state_in: Option<(String, String)>,
    oracle_bit_exprs: &[Vec<String>],
    step_expr: &str,
) -> StepResult {
    let ir = generate_split_step_ir(
        prover_funcs, qsim_funcs, verifier_funcs, boundary, accum_info, n_chunks, total_vars,
        entry_w, all_ok_fold_state_in, oracle_bit_exprs, step_expr,
    );
    StepResult {
        stmts: print_stmts(&ir.stmts),
        next_entry_w: ir.next_entry_w,
        final_all_ok_expr: ir.final_all_ok_expr,
        final_fold_state_expr: ir.final_fold_state_expr,
        finish_output_slots: ir.finish_output_slots,
    }
}

pub fn generate_split_step_ir(
    prover_funcs: &[IrFunction],
    qsim_funcs: &[IrFunction],
    verifier_funcs: &[IrFunction],
    boundary: &[MovfuscBlockBoundary],
    accum_info: &MovfuscAccumInfo,
    n_chunks: usize,
    total_vars: usize,
    entry_w: &[(Slot, Slot)],
    all_ok_fold_state_in: Option<(String, String)>,
    oracle_bit_exprs: &[Vec<String>],
    step_expr: &str,
) -> StepResultIr {
    let n_blocks = boundary.len();
    assert_eq!(prover_funcs.len(), n_blocks + n_chunks + 1);
    assert_eq!(qsim_funcs.len(), n_blocks + n_chunks + 1);
    assert_eq!(verifier_funcs.len(), n_blocks + n_chunks + 1);

    let mut stmts: Vec<IrStmt> = Vec::new();
    let mut oracle_cursor = 0usize;
    let mut and_gate_seed = 0u64;
    // `all_ok`/`fold_state` are threaded as plain identifier names from
    // here on (every later reassignment is always a `Slot::Scalar` name,
    // e.g. `v_all_ok_blk3`) -- when no seed is given, bind fresh locals
    // right here instead of smuggling `"iop_accumulator_fresh()"` (a call,
    // not a valid identifier) through `var()`, which the printer's own
    // `IrExprKind::Var` guardrail rejects (see this module's own doc).
    let (mut all_ok_expr, mut fold_state_expr) = match &all_ok_fold_state_in {
        Some((a, f)) => (a.clone(), f.clone()),
        None => {
            stmts.push(let_stmt("_all_ok_init", ir_expr(IrExprKind::Lit(IrLit::Bool(true)))));
            stmts.push(let_stmt("_fold_state_init", call_expr("iop_accumulator_fresh", vec![])));
            ("_all_ok_init".to_string(), "_fold_state_init".to_string())
        }
    };

    // `synth_v` pooling (Phase C): if ANY block/chunk function (in ANY
    // role) takes `_synth_pool`, declare the backing storage once here,
    // sized to `total_vars` (the whole circuit's own var-id space --
    // `_synth_pool[v]` addresses var `v` directly, no compact remapping,
    // matching `vole.rs`'s own `WireRepr::Pooled("_synth_pool", v as
    // usize)` convention exactly). Two independently-typed pools, same
    // reason `entry_w`/`exported_vope`/`exported_q` are dual-threaded:
    // prover functions take a `Box<[Vope<..>]>`-backed `_synth_pool`,
    // qsim/verifier functions take a `Box<[Q<..>]>`-backed one -- same
    // param NAME, different TYPE, since they're always in separate
    // functions with their own independent scopes. Declared once per
    // real-runtime-loop call (i.e. inside the loop body this function's
    // own `stmts` becomes), so a fresh, all-unwritten pool starts every
    // step -- `debug_check_pool_written` genuinely re-arms per step,
    // catching a per-step ordering bug rather than being silently
    // masked by an earlier step's own writes. Statically sized via `Box`
    // (see `ir_builder::synth_pool_decl_stmts`), not `Vec` -- the same
    // heap-allocation abstraction `volar-weaver`'s own pools use.
    let synth_pool_active = prover_funcs.iter().chain(qsim_funcs).chain(verifier_funcs)
        .any(|f| f.params.iter().any(|p| p.name == "_synth_pool"));
    if synth_pool_active {
        stmts.extend(synth_pool_decl_stmts(total_vars));
    }

    // Per-block-or-chunk-or-finish exported next_pc/next_state/ret_vals
    // slots, keyed by the *consuming* param-name prefix (e.g.
    // "next_state_2" for block 2's slot 2) -- populated after each block
    // call, consumed when building the covering chunk's own call. Two maps,
    // same reason `entry_w`/`running_in` are dual-threaded: the prover's
    // chunk function needs its own (Vope-typed) block exports, the
    // qsim/verifier chunk functions need the verifier's (Q-typed) ones.
    let mut exported_vope: BTreeMap<String, Slot> = BTreeMap::new();
    let mut exported_q: BTreeMap<String, Slot> = BTreeMap::new();

    // Cross-chunk-shared values (CSE-discovered post-movfuscation sharing,
    // `MovfuscBlockBoundary`/`MovfuscAccumStep`/`MovfuscAccumInit`'s own
    // `synthetic_in`/`synthetic_out` -- see
    // `docs/interpreter-honest-e2e-zk-plan.md`'s "Cross-chunk locality"
    // section), keyed by the var's own STABLE global id (not a
    // param-name-prefix string like `exported_vope`/`exported_q`, since a
    // synthetic var's own name -- `synth_{v}` -- is already globally
    // unique and stable across every function that references it). Same
    // Vope/Q role split as `exported_vope`/`exported_q`; qsim's own
    // synthetic outputs are discarded, same as its `is_active`/`done`/etc
    // -- only the verifier's own (Q-typed) output is threaded onward,
    // exactly mirroring `exported_q`'s own population from `v_*`, not `q_*`.
    let mut synth_exported_vope: BTreeMap<u32, Slot> = BTreeMap::new();
    let mut synth_exported_q: BTreeMap<u32, Slot> = BTreeMap::new();

    // Pooling (Phase C: synth_v, and now block-boundary exports/running-
    // accumulator too) makes a producer OMIT any pooled (scalar) exported
    // var from its own return tuple entirely -- it's written straight to
    // `_synth_pool` instead. That means a region's FULL export list
    // (`synthetic_out`, or is_active/done/next_pc/next_state/ret_vals/
    // running-accumulator) no longer lines up 1:1, in order, with the
    // trailing tuple slots a producer actually returns: naively slicing/
    // zipping the two together (as this code used to, unconditionally)
    // silently pairs the wrong var/slot with the wrong value whenever a
    // region has a MIX of pooled and unpooled exports. Fix: recover,
    // once, the full set of param names that are still real, tuple-
    // threaded params SOMEWHERE (unpooled) -- "is v scalar" is a fixed,
    // type-determined property of the var (the same decision `vole.rs`
    // makes wherever it appears), so if a var/category is ever unpooled,
    // every real consumer has the matching named param; if it's pooled,
    // none does (consumers read `_synth_pool[v]` directly instead).
    let named_param_present: std::collections::BTreeSet<String> = prover_funcs.iter().chain(qsim_funcs).chain(verifier_funcs)
        .flat_map(|f| f.params.iter())
        .map(|p| p.name.clone())
        .collect();
    let unpooled_synth_out = |all: &[u32]| -> Vec<u32> {
        all.iter().copied().filter(|v| named_param_present.contains(&format!("synth_{v}"))).collect()
    };
    // Take the next slot from `slots[*idx]` and advance `idx`, but only
    // if `name` is still a real, tuple-threaded param somewhere (i.e.
    // NOT pooled) -- the position-named (`is_active_i`/`next_pc_i_j`/
    // `in_next_state_k`/etc) counterpart of `unpooled_synth_out`'s own
    // var-id-keyed reasoning above. Returns `None` for a pooled entry:
    // its own value never needs tracking here at all, since its consumer
    // reads `_synth_pool[v]` directly rather than taking a named param,
    // so `exported_vope`/`exported_q`/`running_in` never need (or get) an
    // entry for it.
    fn take_if_named(slots: &[Slot], idx: &mut usize, named_param_present: &std::collections::BTreeSet<String>, name: &str) -> Option<Slot> {
        if named_param_present.contains(name) {
            let s = slots[*idx].clone();
            *idx += 1;
            Some(s)
        } else {
            None
        }
    }

    // Emit one committed oracle read (real value known host-side, or a
    // runtime witness-array reference -- `bit_expr` already decides
    // which) as a fresh `vole_commit_bit` call pair, returning
    // (vope_name, q_name).
    let mut emit_oracle = |stmts: &mut Vec<IrStmt>, uid: &str| -> (String, String) {
        let bit_exprs = &oracle_bit_exprs[oracle_cursor];
        oracle_cursor += 1;
        let width = bit_exprs.len();
        let mut vope_names = Vec::with_capacity(width);
        let mut q_names = Vec::with_capacity(width);
        for (j, bit_expr) in bit_exprs.iter().enumerate() {
            let vn = format!("_orv_{uid}_{j}");
            let qn = format!("_orq_{uid}_{j}");
            let call = call_expr("vole_commit_bit", vec![
                ref_expr(var("cot")), ref_mut_expr(var("rng")), var("sample_g"), var("lift_bit_g"), parse_leaf_expr(bit_expr),
            ]);
            stmts.push(let_tuple_stmt(&[vn.clone(), qn.clone()], call));
            vope_names.push(vn);
            q_names.push(qn);
        }
        // Bundle into one array-valued local each, so downstream Slot
        // handling is uniform regardless of width.
        let vope_arr = format!("_orv_{uid}_arr");
        let q_arr = format!("_orq_{uid}_arr");
        stmts.push(let_stmt(&vope_arr, array_lit_expr(vope_names.iter().map(|n| clone_var(n)).collect())));
        stmts.push(let_stmt(&q_arr, array_lit_expr(q_names.iter().map(|n| clone_var(n)).collect())));
        (vope_arr, q_arr)
    };

    // Build one function call's full argument list and emit the call +
    // destructure. `entry_w_index` (0 = vope/prover side, 1 = q/qsim-or-
    // verifier side) decides the `vope_one`/`q_one` param's own argument
    // (`vope_one(&delta)`/`q_one(&delta)`) -- built as a real call
    // expression here, not smuggled through `var` as text (see this
    // module's own doc).
    struct CallOutcome {
        finish_output: Vec<Slot>,
    }

    #[allow(clippy::too_many_arguments)]
    fn build_call(
        stmts: &mut Vec<IrStmt>,
        f: &IrFunction,
        entry_w: &[(Slot, Slot)],
        entry_w_index: usize, // 0 = vope side, 1 = q side
        hat_in: Option<&Slot>, // hat_k input array: qsim consumes the prover's hats; verifier does too (both hat_k and q_and_k are separate verifier params)
        q_and_in: Option<&Slot>, // q_and_k input array: verifier only, sourced from qsim's own output
        r_ands: Option<&str>, // verifier only: name of a pre-bound [Gf128; n] local
        running_in: Option<(&str, &str, &str, &str)>, // (done_acc, next_pc_arr, next_state_arr_of_arrs, ret_val_arr_of_arrs) local names, chunk/finish only
        block_exports: &BTreeMap<String, Slot>,
        synth_exported: &BTreeMap<u32, Slot>, // cross-chunk-shared values, keyed by var id -- see its own declaration site
        all_ok_in: &str,
        fold_state_in: &str,
        oracle_arr: Option<&str>, // pre-bound oracle array local name (vope or q side, already selected)
        call_uid: &str,
    ) -> CallOutcome {
        let mut args: Vec<IrExpr> = Vec::new();
        for p in &f.params {
            let n = &p.name;
            if n == "delta" { args.push(ref_expr(var("delta"))); continue; }
            if n == "vope_one" { args.push(call_expr("vope_one", vec![ref_expr(var("delta"))])); continue; }
            if n == "q_one" { args.push(call_expr("q_one", vec![ref_expr(var("delta"))])); continue; }
            if n == "hat" {
                let arr = hat_in.expect("hat param but no hat input array given");
                match arr {
                    Slot::Array(..) => args.push(slot_clone_expr(arr)),
                    Slot::Scalar(_) => unreachable!("hat array must be Slot::Array"),
                }
                continue;
            }
            if n == "q_and" {
                let arr = q_and_in.expect("q_and param but no q_and input array given");
                match arr {
                    Slot::Array(..) => args.push(slot_clone_expr(arr)),
                    Slot::Scalar(_) => unreachable!("q_and array must be Slot::Array"),
                }
                continue;
            }
            if n == "r_and" {
                let r = r_ands.expect("r_and param but no r_ands array given");
                args.push(clone_var(r));
                continue;
            }
            if n.starts_with("w_") {
                // Always exactly `w_{i}` now -- wide entry-state params are
                // array-batched (one `w_{i}: [T; width]` param), no more
                // `w_{i}_{j}` per-lane scalars.
                let i: usize = n["w_".len()..].parse().unwrap();
                let slot = if entry_w_index == 0 { &entry_w[i].0 } else { &entry_w[i].1 };
                args.push(slot_clone_expr(slot));
                continue;
            }
            // `synth_v` pooling (Phase C): every function that reads or
            // writes any pooled synthetic value takes these two params --
            // pass the role-appropriate backing storage (Vope-typed for
            // prover calls, Q-typed for qsim/verifier calls, exactly
            // like `w_*` above) by `&mut` reference. `vole.rs`'s own
            // generated code addresses `_synth_pool[v]` by the var's raw
            // id directly, so no lookup/translation is needed here at
            // all -- unlike `is_active_*`/`synth_{v}`, this is not a
            // per-value named param, just a shared handle passed once.
            if n == "_synth_pool" {
                args.push(slice_ref_mut_expr(if entry_w_index == 0 { "_synth_pool_vope" } else { "_synth_pool_q" }));
                continue;
            }
            if n == "_synth_pool_written" {
                args.push(slice_ref_mut_expr(if entry_w_index == 0 { "_synth_pool_vope_written" } else { "_synth_pool_q_written" }));
                continue;
            }
            // Phase B (`w_i` pooling): same dispatch shape as `_synth_pool`
            // above, but `_w_pool` is declared ONCE by the caller, outside
            // the real runtime loop (a param's own value must persist
            // across steps, unlike cross-region `_synth_pool` values,
            // which never need to survive past the step that produced
            // them) -- so, unlike `_synth_pool`, `generate_split_step`
            // itself never emits this pool's own declaration; it just
            // references `_w_pool_vope`/`_w_pool_q` as already in scope.
            if n == "_w_pool" {
                args.push(slice_ref_mut_expr(if entry_w_index == 0 { "_w_pool_vope" } else { "_w_pool_q" }));
                continue;
            }
            if n == "_w_pool_written" {
                args.push(slice_ref_mut_expr(if entry_w_index == 0 { "_w_pool_vope_written" } else { "_w_pool_q_written" }));
                continue;
            }
            if n.starts_with("oracle_rd_") {
                let idx: usize = n.rsplit('_').next().unwrap().parse().unwrap();
                let arr = oracle_arr.expect("oracle_rd_k param but no oracle array given");
                args.push(clone_expr(index_lit_expr(arr, idx)));
                continue;
            }
            if n == "all_ok_in" { args.push(var(all_ok_in)); continue; }
            if n == "fold_state_in" { args.push(var(fold_state_in)); continue; }
            if n.starts_with("in_done_acc") {
                args.push(clone_var(running_in.expect("running accumulator input expected").0));
                continue;
            }
            if n.starts_with("in_next_pc_") {
                let j: usize = n.rsplit('_').next().unwrap().parse().unwrap();
                args.push(clone_expr(index_lit_expr(running_in.unwrap().1, j)));
                continue;
            }
            if n.starts_with("in_next_state_") {
                // `running_in`'s next_state/ret_vals are heterogeneous
                // (each slot can be a different concrete type), so they're
                // built as a Rust *tuple* (`.{k}` field access), not an
                // array -- unlike `in_next_pc_`, which is uniform Q and
                // uses a real array. Always exactly `in_next_state_{k}`
                // now (`bind_scalar` array-batches a wide slot into one
                // param, no more `_{j}` suffix) -- the tuple field itself
                // is already either a scalar or a whole `[T; w]` array
                // (built that way host-side, see the zero-init/chunk
                // accumulator construction below), matching the param's
                // own (possibly array) type directly.
                let k: usize = n["in_next_state_".len()..].parse().unwrap();
                let tup = running_in.unwrap().2;
                args.push(clone_expr_of(field_expr(tup, &k.to_string())));
                continue;
            }
            if n.starts_with("in_ret_val_") {
                let m: usize = n["in_ret_val_".len()..].parse().unwrap();
                let tup = running_in.unwrap().3;
                args.push(clone_expr_of(field_expr(tup, &m.to_string())));
                continue;
            }
            if n.starts_with("is_active_") || n.starts_with("done_")
                || n.starts_with("next_pc_") || n.starts_with("next_state_") || n.starts_with("ret_val_")
            {
                let slot = block_exports.get(n).unwrap_or_else(|| panic!("missing export for param {n}"));
                // `bind_scalar` array-batches a wide export into one param
                // (no per-lane `_{j}` suffix), so `slot` may be a whole
                // array here -- clone it directly either way.
                args.push(slot_clone_expr(slot));
                continue;
            }
            if let Some(rest) = n.strip_prefix("synth_") {
                let v: u32 = rest.parse().unwrap_or_else(|_| panic!("malformed synthetic param name: {n}"));
                let slot = synth_exported.get(&v).unwrap_or_else(|| panic!(
                    "missing synthetic export for var {v} (param {n}) -- its own producer range must be called before this consumer, in real driver call order"
                ));
                args.push(slot_clone_expr(slot));
                continue;
            }
            panic!("unrecognized param name: {n}");
        }

        let ret_ty = f.return_type.as_ref().expect("woven function must have a return type");
        let elems = tuple_elems(ret_ty);
        let call = call_expr(&f.name, args);
        let n_locals = elems.len();
        let local_names: Vec<String> = (0..n_locals).map(|i| format!("_r_{call_uid}_{i}")).collect();
        if n_locals == 1 {
            stmts.push(let_stmt(&local_names[0], call));
        } else {
            stmts.push(let_tuple_stmt(&local_names, call));
        }
        let slots: Vec<Slot> = local_names.iter().zip(elems.iter()).map(|(n, ty)| slot_from_ty(n, ty)).collect();
        CallOutcome { finish_output: slots }
    }

    // `<expr>.clone()` for an already-built expression (`field_expr`'s own
    // result, e.g. `tup.0`) -- `clone_var` only takes a bare name.
    fn clone_expr_of(e: IrExpr) -> IrExpr {
        crate::ir_builder::clone_expr(e)
    }

    // ---- Blocks -----------------------------------------------------------
    for (i, b) in boundary.iter().enumerate() {
        // No step component in `uid` -- this function is now called ONCE
        // (its own `stmts` becomes a real loop body, or is called once
        // per step with a literal `step_expr`); either way, plain Rust
        // block-scoping already gives every `let` a fresh value per
        // call/iteration.
        let uid = format!("blk{i}");
        let n_pc = b.next_pc_bits.len();
        let n_state = b.next_state.len();
        let n_ret = b.ret_vals.len();

        let pf = &prover_funcs[i];
        let qf = &qsim_funcs[i];
        let vf = &verifier_funcs[i];

        let local_oracle_count = count_params_prefixed(pf, "oracle_rd_");
        let (oracle_vope, oracle_q) = if local_oracle_count > 0 {
            let (v, q) = emit_oracle(&mut stmts, &uid);
            (Some(v), Some(q))
        } else { (None, None) };

        // Prover.
        let p_outcome = build_call(
            &mut stmts, pf, entry_w, 0, None, None, None, None, &exported_vope, &synth_exported_vope,
            "", "", oracle_vope.as_deref(), &format!("p_{uid}"),
        );
        let p_slots = p_outcome.finish_output;
        // Layout: [is_active, done, next_pc.., next_state.., ret_vals.., hats, synth_out..] --
        // MINUS any pooled (scalar) entry, which `take_if_named` skips
        // over entirely (see its own doc, and `unpooled_synth_out`'s
        // matching reasoning below).
        let mut idx = 0usize;
        let p_is_active = take_if_named(&p_slots, &mut idx, &named_param_present, &format!("is_active_{i}"));
        let p_done = take_if_named(&p_slots, &mut idx, &named_param_present, &format!("done_{i}"));
        let p_next_pc: Vec<Option<Slot>> = (0..n_pc).map(|j| take_if_named(&p_slots, &mut idx, &named_param_present, &format!("next_pc_{i}_{j}"))).collect();
        let p_next_state: Vec<Option<Slot>> = (0..n_state).map(|k| take_if_named(&p_slots, &mut idx, &named_param_present, &format!("next_state_{i}_{k}"))).collect();
        let p_ret_vals: Vec<Option<Slot>> = (0..n_ret).map(|m| take_if_named(&p_slots, &mut idx, &named_param_present, &format!("ret_val_{i}_{m}"))).collect();
        let p_hats = p_slots[idx].clone(); idx += 1;
        let p_synth_out = unpooled_synth_out(&b.synthetic_out);
        assert_eq!(p_synth_out.len(), p_slots.len() - idx, "block {i}: prover unpooled synth_out count must match trailing tuple slots");
        for (&v, s) in p_synth_out.iter().zip(&p_slots[idx..]) {
            insert_synth_export(&mut synth_exported_vope, v, s.clone());
        }

        // QSim: consumes prover's hats as hat_k input.
        let local_oracle_count_q = count_params_prefixed(qf, "oracle_rd_");
        assert_eq!(local_oracle_count_q, local_oracle_count, "block {i}: qsim/prover oracle count mismatch");
        let q_outcome = build_call(
            &mut stmts, qf, entry_w, 1, Some(&p_hats), None, None, None, &exported_q, &synth_exported_q,
            "", "", oracle_q.as_deref(), &format!("q_{uid}"),
        );
        let q_slots = q_outcome.finish_output;
        // QSim's own is_active/done/next_pc/next_state/ret_vals values are
        // discarded downstream regardless (only Verifier's are threaded
        // onward) -- but `idx` must still be advanced correctly past
        // whichever of them are NOT pooled, to land on `q_and_arr` at the
        // right position.
        let mut idx = 0usize;
        let _q_is_active = take_if_named(&q_slots, &mut idx, &named_param_present, &format!("is_active_{i}"));
        let _q_done = take_if_named(&q_slots, &mut idx, &named_param_present, &format!("done_{i}"));
        for j in 0..n_pc { take_if_named(&q_slots, &mut idx, &named_param_present, &format!("next_pc_{i}_{j}")); }
        for k in 0..n_state { take_if_named(&q_slots, &mut idx, &named_param_present, &format!("next_state_{i}_{k}")); }
        for m in 0..n_ret { take_if_named(&q_slots, &mut idx, &named_param_present, &format!("ret_val_{i}_{m}")); }
        let q_and_arr = q_slots[idx].clone();
        // QSim's own synthetic outputs are discarded, same as its is_active/done/etc above.

        // r_and challenges for this block's own gate count.
        let and_count = and_count_of(vf);
        let r_ands_name = format!("_rands_{uid}");
        and_gate_seed += 1;
        stmts.push(r_ands_decl_stmt(&r_ands_name, and_count, step_expr, and_gate_seed));

        let local_oracle_count_v = count_params_prefixed(vf, "oracle_rd_");
        assert_eq!(local_oracle_count_v, local_oracle_count, "block {i}: verifier/prover oracle count mismatch");
        let v_outcome = build_call(
            &mut stmts, vf, entry_w, 1, Some(&p_hats), Some(&q_and_arr), Some(&r_ands_name), None, &exported_q, &synth_exported_q,
            &all_ok_expr, &fold_state_expr, oracle_q.as_deref(), &format!("v_{uid}"),
        );
        let v_slots = v_outcome.finish_output;
        let mut idx = 0usize;
        let v_is_active = take_if_named(&v_slots, &mut idx, &named_param_present, &format!("is_active_{i}"));
        let v_done = take_if_named(&v_slots, &mut idx, &named_param_present, &format!("done_{i}"));
        let v_next_pc: Vec<Option<Slot>> = (0..n_pc).map(|j| take_if_named(&v_slots, &mut idx, &named_param_present, &format!("next_pc_{i}_{j}"))).collect();
        let v_next_state: Vec<Option<Slot>> = (0..n_state).map(|k| take_if_named(&v_slots, &mut idx, &named_param_present, &format!("next_state_{i}_{k}"))).collect();
        let v_ret_vals: Vec<Option<Slot>> = (0..n_ret).map(|m| take_if_named(&v_slots, &mut idx, &named_param_present, &format!("ret_val_{i}_{m}"))).collect();
        let v_all_ok = match &v_slots[idx] { Slot::Scalar(n) => n.clone(), _ => unreachable!() }; idx += 1;
        let v_fold_state = match &v_slots[idx] { Slot::Scalar(n) => n.clone(), _ => unreachable!() }; idx += 1;
        let v_synth_out = unpooled_synth_out(&b.synthetic_out);
        assert_eq!(v_synth_out.len(), v_slots.len() - idx, "block {i}: verifier unpooled synth_out count must match trailing tuple slots");
        for (&v, s) in v_synth_out.iter().zip(&v_slots[idx..]) {
            insert_synth_export(&mut synth_exported_q, v, s.clone());
        }

        stmts.push(assert_true_stmt(var(&v_all_ok), &format!("block {i}: honest run must pass the woven verifier's own check")));
        all_ok_expr = v_all_ok;
        fold_state_expr = v_fold_state;

        // The prover's own (Vope-typed) block exports feed the *prover*
        // chunk function; the verifier's own (Q-typed) block exports feed
        // the qsim/verifier chunk functions -- two separate maps, exactly
        // like `entry_w`/`running_in`. QSim's own export values are never
        // consumed downstream (only its q_and output is, already handled
        // above via `_q_is_active` etc). A pooled (`None`) entry needs no
        // map insertion at all -- its consumer reads `_synth_pool[v]`
        // directly instead of taking a named param, so nothing ever looks
        // it up in `exported_vope`/`exported_q`.
        if let Some(s) = p_is_active { insert_export(&mut exported_vope, format!("is_active_{i}"), s); }
        if let Some(s) = p_done { insert_export(&mut exported_vope, format!("done_{i}"), s); }
        for (j, s) in p_next_pc.into_iter().enumerate() {
            if let Some(s) = s { insert_export(&mut exported_vope, format!("next_pc_{i}_{j}"), s); }
        }
        for (k, s) in p_next_state.into_iter().enumerate() {
            if let Some(s) = s { insert_export(&mut exported_vope, format!("next_state_{i}_{k}"), s); }
        }
        for (m, s) in p_ret_vals.into_iter().enumerate() {
            if let Some(s) = s { insert_export(&mut exported_vope, format!("ret_val_{i}_{m}"), s); }
        }

        if let Some(s) = v_is_active { insert_export(&mut exported_q, format!("is_active_{i}"), s); }
        if let Some(s) = v_done { insert_export(&mut exported_q, format!("done_{i}"), s); }
        for (j, s) in v_next_pc.into_iter().enumerate() {
            if let Some(s) = s { insert_export(&mut exported_q, format!("next_pc_{i}_{j}"), s); }
        }
        for (k, s) in v_next_state.into_iter().enumerate() {
            if let Some(s) = s { insert_export(&mut exported_q, format!("next_state_{i}_{k}"), s); }
        }
        for (m, s) in v_ret_vals.into_iter().enumerate() {
            if let Some(s) = s { insert_export(&mut exported_q, format!("ret_val_{i}_{m}"), s); }
        }
    }

    // ---- Chunks -------------------------------------------------------
    //
    // The running cross-chunk accumulator needs *two* independently-typed
    // threads, exactly like `entry_w`: a Vope-typed one (the prover's own
    // real witness accumulator, fed only to prover calls) and a Q-typed one
    // (fed to both qsim and verifier calls, mirroring how qsim is
    // Verifier-shaped throughout). They are NOT interchangeable -- the
    // prover's own chunk output is what feeds the *next* chunk's prover
    // call; qsim's own chunk output is never consumed downstream (only its
    // q_and array is, same as in the Blocks section above), so only the
    // verifier's own chunk output feeds the next chunk's qsim/verifier call.
    let chunk_size = (n_blocks + n_chunks - 1) / n_chunks.max(1);
    let mut lo = 0usize;
    let mut running_done_acc_vope = "_acc_init_done_vope".to_string();
    let mut running_next_pc_vope = "_acc_init_pc_vope".to_string();
    let mut running_next_state_vope = "_acc_init_state_vope".to_string();
    let mut running_ret_vals_vope = "_acc_init_ret_vope".to_string();
    let mut running_done_acc_q = "_acc_init_done_q".to_string();
    let mut running_next_pc_q = "_acc_init_pc_q".to_string();
    let mut running_next_state_q = "_acc_init_state_q".to_string();
    let mut running_ret_vals_q = "_acc_init_ret_q".to_string();
    stmts.push(let_stmt(&running_done_acc_vope, call_expr("vope_zero", vec![])));
    stmts.push(let_stmt(&running_done_acc_q, call_expr("q_zero", vec![])));
    let vope_n_ty = IrType::Struct {
        kind: volar_compiler::ir::StructKind::Custom("Vope".into()),
        type_args: vec![
            IrType::TypeParam("N".into()), IrType::TypeParam("Galois".into()), IrType::TypeParam("cipher::consts::U1".into()),
        ],
    };
    let q_n_ty = IrType::Struct {
        kind: volar_compiler::ir::StructKind::Custom("Q".into()),
        type_args: vec![IrType::TypeParam("N".into()), IrType::TypeParam("Galois".into())],
    };
    let fixed_array_ty = |elem: IrType, n: usize| IrType::Array {
        kind: volar_compiler::ir::ArrayKind::FixedArray, elem: Box::new(elem), len: ArrayLength::Const(n),
    };
    stmts.push(let_typed_stmt(
        &running_next_pc_vope, fixed_array_ty(vope_n_ty.clone(), accum_info.init.next_pc.len()),
        crate::ir_builder::array_from_fn_expr("_", call_expr("vope_zero", vec![])),
    ));
    stmts.push(let_typed_stmt(
        &running_next_pc_q, fixed_array_ty(q_n_ty.clone(), accum_info.init.next_pc.len()),
        crate::ir_builder::array_from_fn_expr("_", call_expr("q_zero", vec![])),
    ));
    // Slot widths aren't recorded by `MovfuscAccumInfo` itself (only slot
    // *count* is) -- `ret_vals`' own zero-init below still needs them (a
    // wide slot's zero-init is a `[T; w]` array literal, not a bare
    // scalar). Read back from the first chunk-or-finish function's own
    // `in_ret_val_` param names.
    let n_state = accum_info.init.next_state.len();
    let n_ret = accum_info.init.ret_vals.len();
    let first_chunk_or_finish = &verifier_funcs[n_blocks];
    let ret_widths = slot_widths_from_params(first_chunk_or_finish, "in_ret_val_", n_ret);
    // `accum_info.init.next_state[k]`'s own real value is `state_vars[k]`
    // (the circuit's own incoming param for that slot, per
    // `movfuscate.rs`'s tunnelled-slot elimination) -- *not* zero.
    // `entry_w` (this driver's own per-param entry-state, index-matched to
    // `circuit.blocks[0].params` exactly like `state_vars`) already holds
    // the real value for every step, including step 0 (a driver-level,
    // honest concern, not movfuscation's) -- reuse its own existing
    // locals directly rather than binding a fresh zero, matching the fact
    // that `accum_init` itself needs no new statement for the very same
    // reason. State slots start right after the `pc_width` PC bits in
    // both `state_vars` and `entry_w`'s own param-indexed layout.
    let pc_width_for_state = accum_info.init.next_pc.len();
    let mut init_state_locals_vope = Vec::with_capacity(n_state);
    let mut init_state_locals_q = Vec::with_capacity(n_state);
    for k in 0..n_state {
        let (vope_slot, q_slot) = &entry_w[pc_width_for_state + k];
        // `.clone()`, not a bare move: under the real-runtime-loop calling
        // convention `entry_w`'s own slots are OUTER `mut` bindings reused
        // across iterations (and referenced again later in this same
        // iteration, e.g. by block/chunk calls) -- unlike the older
        // per-step-unrolled convention, where each step's `entry_w` names
        // were fresh and single-use, so a bare move here was safe.
        init_state_locals_vope.push(slot_name(vope_slot).to_string());
        init_state_locals_q.push(slot_name(q_slot).to_string());
    }
    stmts.push(let_stmt(&running_next_state_vope, tuple_lit_expr(init_state_locals_vope.iter().map(|n| clone_var(n)).collect())));
    stmts.push(let_stmt(&running_next_state_q, tuple_lit_expr(init_state_locals_q.iter().map(|n| clone_var(n)).collect())));
    let mut init_ret_locals_vope = Vec::with_capacity(n_ret);
    let mut init_ret_locals_q = Vec::with_capacity(n_ret);
    for (m, &w) in ret_widths.iter().enumerate() {
        let name_vope = format!("_acc_init_ret_vope_{m}");
        let name_q = format!("_acc_init_ret_q_{m}");
        if w <= 1 {
            stmts.push(let_typed_stmt(&name_vope, vope_n_ty.clone(), call_expr("vope_zero", vec![])));
            stmts.push(let_typed_stmt(&name_q, q_n_ty.clone(), call_expr("q_zero", vec![])));
        } else {
            stmts.push(let_typed_stmt(&name_vope, fixed_array_ty(vope_n_ty.clone(), w), crate::ir_builder::array_from_fn_expr("_", call_expr("vope_zero", vec![]))));
            stmts.push(let_typed_stmt(&name_q, fixed_array_ty(q_n_ty.clone(), w), crate::ir_builder::array_from_fn_expr("_", call_expr("q_zero", vec![]))));
        }
        init_ret_locals_vope.push(name_vope);
        init_ret_locals_q.push(name_q);
    }
    stmts.push(let_stmt(&running_ret_vals_vope, tuple_lit_expr(init_ret_locals_vope.iter().map(|n| var(n)).collect())));
    stmts.push(let_stmt(&running_ret_vals_q, tuple_lit_expr(init_ret_locals_q.iter().map(|n| var(n)).collect())));

    // Chunk 0's own INCOMING running-accumulator state (`accum_info.init`)
    // is a pure host-side value (zero for done_acc/next_pc/ret_vals; the
    // circuit's own real param for next_state, per the comment above) --
    // unlike every OTHER var `_synth_pool` covers, it is NEVER produced by
    // a real vole.rs-generated function call, so nothing would otherwise
    // ever write it into the pool before chunk 0's own `in_*` read tries
    // to read it -- `debug_check_pool_written` correctly catches this as
    // "read from unwritten pool slot" if skipped (confirmed: this is
    // exactly what happened before this fix was added). Write any pooled
    // entry here explicitly, mirroring what a real producer's own
    // `export_scalar_or_tuple` would have emitted.
    if synth_pool_active {
        let emit_pool_init = |stmts: &mut Vec<IrStmt>, var_id: u32, name: &str, value_vope: IrExpr, value_q: IrExpr| {
            if named_param_present.contains(name) { return; } // still tuple-threaded (wide) -- no pool write needed
            stmts.extend(pool_write_stmts("_synth_pool_vope", "_synth_pool_vope_written", var_id as usize, value_vope));
            stmts.extend(pool_write_stmts("_synth_pool_q", "_synth_pool_q_written", var_id as usize, value_q));
        };
        emit_pool_init(&mut stmts, accum_info.init.done_acc, "in_done_acc", call_expr("vope_zero", vec![]), call_expr("q_zero", vec![]));
        for (j, &v) in accum_info.init.next_pc.iter().enumerate() {
            emit_pool_init(&mut stmts, v, &format!("in_next_pc_{j}"), call_expr("vope_zero", vec![]), call_expr("q_zero", vec![]));
        }
        for (k, &v) in accum_info.init.next_state.iter().enumerate() {
            emit_pool_init(&mut stmts, v, &format!("in_next_state_{k}"), clone_var(&init_state_locals_vope[k]), clone_var(&init_state_locals_q[k]));
        }
        for (m, &v) in accum_info.init.ret_vals.iter().enumerate() {
            emit_pool_init(&mut stmts, v, &format!("in_ret_val_{m}"), clone_var(&init_ret_locals_vope[m]), clone_var(&init_ret_locals_q[m]));
        }
    }

    let pc_w = accum_info.init.next_pc.len();
    let st_w = accum_info.init.next_state.len();
    let rv_w = accum_info.init.ret_vals.len();

    // Parse a chunk-shaped flat `[done_acc, next_pc.., next_state..,
    // ret_vals.., ..trailing]` output into its running-state prefix (the
    // trailing elements -- hats, or all_ok+fold_state -- are the caller's
    // own concern) -- MINUS any pooled entries, which `take_if_named`
    // skips over (see its own doc). `done_acc`/`next_pc` are
    // unconditionally `vope_type()`/`q_type()` (never `Array`) by
    // `bind_running`/`running_export`'s own design in `vole.rs`, so they
    // are ALWAYS pooled in practice -- but this stays generic (uniform
    // `take_if_named` calls) rather than hardcoding that fact, so it
    // stays correct if that ever changes. Takes `idx` by `&mut` (rather
    // than returning it) so callers can locate whatever trails (hats, or
    // all_ok/fold_state) at the correct position afterward.
    let parse_running_output = |slots: &[Slot], idx: &mut usize| -> (Option<Slot>, Vec<Option<Slot>>, Vec<Option<Slot>>, Vec<Option<Slot>>) {
        let done_acc = take_if_named(slots, idx, &named_param_present, "in_done_acc");
        let next_pc: Vec<Option<Slot>> = (0..pc_w).map(|j| take_if_named(slots, idx, &named_param_present, &format!("in_next_pc_{j}"))).collect();
        let next_state: Vec<Option<Slot>> = (0..st_w).map(|k| take_if_named(slots, idx, &named_param_present, &format!("in_next_state_{k}"))).collect();
        let ret_vals: Vec<Option<Slot>> = (0..rv_w).map(|m| take_if_named(slots, idx, &named_param_present, &format!("in_ret_val_{m}"))).collect();
        (done_acc, next_pc, next_state, ret_vals)
    };
    // A field of the running-accumulator's own OUTGOING tuple/array local
    // (`_acc_st_vope_{uid}` etc.): the real extracted value if this slot
    // is still tuple-threaded, or a harmless zero placeholder if it's
    // pooled -- a pooled field is never actually read back (its real
    // current value lives in `_synth_pool[v]` instead, addressed by the
    // same var id `vole.rs`'s own host-side `running_*` tracking already
    // threads producer-to-consumer), so the placeholder only needs to be
    // syntactically valid, not meaningful.
    let running_field_expr = |s: &Option<Slot>, zero_call: &str| -> IrExpr {
        match s {
            Some(s) => slot_clone_expr(s),
            None => call_expr(zero_call, vec![]),
        }
    };

    for c in 0..n_chunks {
        let hi = (lo + chunk_size).min(n_blocks);
        let uid = format!("chunk{c}");
        let pf = &prover_funcs[n_blocks + c];
        let qf = &qsim_funcs[n_blocks + c];
        let vf = &verifier_funcs[n_blocks + c];

        let local_oracle_count = count_params_prefixed(pf, "oracle_rd_");
        let (oracle_vope, oracle_q) = if local_oracle_count > 0 {
            let (v, q) = emit_oracle(&mut stmts, &uid);
            (Some(v), Some(q))
        } else { (None, None) };

        let running_in_vope = (running_done_acc_vope.as_str(), running_next_pc_vope.as_str(), running_next_state_vope.as_str(), running_ret_vals_vope.as_str());
        let running_in_q = (running_done_acc_q.as_str(), running_next_pc_q.as_str(), running_next_state_q.as_str(), running_ret_vals_q.as_str());

        let p_outcome = build_call(
            &mut stmts, pf, entry_w, 0, None, None, None, Some(running_in_vope), &exported_vope, &synth_exported_vope,
            "", "", oracle_vope.as_deref(), &format!("p_{uid}"),
        );
        let p_slots = p_outcome.finish_output;
        // Layout: [done_acc, next_pc.., next_state.., ret_vals.., hats, synth_out..]
        // -- MINUS any pooled entries (see `parse_running_output`'s own doc).
        let mut p_idx = 0usize;
        let (p_new_done_acc, p_new_next_pc, p_new_next_state, p_new_ret_vals) = parse_running_output(&p_slots, &mut p_idx);
        let p_hats = p_slots[p_idx].clone(); p_idx += 1;
        let out_step_for_lo_hi = &accum_info.steps[hi - 1];
        let p_chunk_synth_out = unpooled_synth_out(&out_step_for_lo_hi.synthetic_out);
        assert_eq!(p_chunk_synth_out.len(), p_slots.len() - p_idx, "chunk {c}: prover unpooled synth_out count must match trailing tuple slots");
        for (&v, s) in p_chunk_synth_out.iter().zip(&p_slots[p_idx..]) {
            insert_synth_export(&mut synth_exported_vope, v, s.clone());
        }

        let q_outcome = build_call(
            &mut stmts, qf, entry_w, 1, Some(&p_hats), None, None, Some(running_in_q), &exported_q, &synth_exported_q,
            "", "", oracle_q.as_deref(), &format!("q_{uid}"),
        );
        let q_slots = q_outcome.finish_output;
        // QSim's own running state is discarded downstream (only Verifier's
        // is threaded onward), but `idx` must still advance correctly past
        // it to land on `q_and_arr` at the right position.
        let mut q_idx = 0usize;
        let _ = parse_running_output(&q_slots, &mut q_idx);
        let q_and_arr = q_slots[q_idx].clone();
        // QSim's own synthetic outputs are discarded too, same reasoning.

        let and_count = and_count_of(vf);
        let r_ands_name = format!("_rands_{uid}");
        and_gate_seed += 1;
        stmts.push(r_ands_decl_stmt(&r_ands_name, and_count, step_expr, and_gate_seed));
        let v_outcome = build_call(
            &mut stmts, vf, entry_w, 1, Some(&p_hats), Some(&q_and_arr), Some(&r_ands_name), Some(running_in_q), &exported_q, &synth_exported_q,
            &all_ok_expr, &fold_state_expr, oracle_q.as_deref(), &format!("v_{uid}"),
        );
        let v_slots = v_outcome.finish_output;
        // Layout: [done_acc, next_pc.., next_state.., ret_vals.., all_ok, fold_state, synth_out..]
        // -- MINUS any pooled entries, same as the prover call above.
        let mut v_idx = 0usize;
        let (v_new_done_acc, v_new_next_pc, v_new_next_state, v_new_ret_vals) = parse_running_output(&v_slots, &mut v_idx);
        let v_all_ok = match &v_slots[v_idx] { Slot::Scalar(n) => n.clone(), _ => unreachable!() }; v_idx += 1;
        let v_fold_state = match &v_slots[v_idx] { Slot::Scalar(n) => n.clone(), _ => unreachable!() }; v_idx += 1;
        let v_chunk_synth_out = unpooled_synth_out(&out_step_for_lo_hi.synthetic_out);
        assert_eq!(v_chunk_synth_out.len(), v_slots.len() - v_idx, "chunk {c}: verifier unpooled synth_out count must match trailing tuple slots");
        for (&v, s) in v_chunk_synth_out.iter().zip(&v_slots[v_idx..]) {
            insert_synth_export(&mut synth_exported_q, v, s.clone());
        }

        stmts.push(assert_true_stmt(var(&v_all_ok), &format!("chunk {c}: honest run must pass the woven verifier's own check")));
        all_ok_expr = v_all_ok;
        fold_state_expr = v_fold_state;

        // Build this chunk's own OUTGOING running-state locals -- real
        // extracted values for still-tuple-threaded (wide) slots, harmless
        // zero placeholders for pooled ones (see `running_field_expr`'s own
        // doc: a pooled field is never read back from here at all).
        let done_acc_vope_name = format!("_acc_done_vope_{uid}");
        stmts.push(let_stmt(&done_acc_vope_name, running_field_expr(&p_new_done_acc, "vope_zero")));
        let pc_arr_vope = format!("_acc_pc_vope_{uid}");
        stmts.push(let_stmt(&pc_arr_vope, array_lit_expr(p_new_next_pc.iter().map(|s| running_field_expr(s, "vope_zero")).collect())));
        let st_arr_vope = format!("_acc_st_vope_{uid}");
        stmts.push(let_stmt(&st_arr_vope, tuple_lit_expr(p_new_next_state.iter().map(|s| running_field_expr(s, "vope_zero")).collect())));
        let rv_arr_vope = format!("_acc_rv_vope_{uid}");
        stmts.push(let_stmt(&rv_arr_vope, tuple_lit_expr(p_new_ret_vals.iter().map(|s| running_field_expr(s, "vope_zero")).collect())));

        let done_acc_q_name = format!("_acc_done_q_{uid}");
        stmts.push(let_stmt(&done_acc_q_name, running_field_expr(&v_new_done_acc, "q_zero")));
        let pc_arr_q = format!("_acc_pc_q_{uid}");
        stmts.push(let_stmt(&pc_arr_q, array_lit_expr(v_new_next_pc.iter().map(|s| running_field_expr(s, "q_zero")).collect())));
        let st_arr_q = format!("_acc_st_q_{uid}");
        stmts.push(let_stmt(&st_arr_q, tuple_lit_expr(v_new_next_state.iter().map(|s| running_field_expr(s, "q_zero")).collect())));
        let rv_arr_q = format!("_acc_rv_q_{uid}");
        stmts.push(let_stmt(&rv_arr_q, tuple_lit_expr(v_new_ret_vals.iter().map(|s| running_field_expr(s, "q_zero")).collect())));

        running_done_acc_vope = done_acc_vope_name;
        running_next_pc_vope = pc_arr_vope;
        running_next_state_vope = st_arr_vope;
        running_ret_vals_vope = rv_arr_vope;
        running_done_acc_q = done_acc_q_name;
        running_next_pc_q = pc_arr_q;
        running_next_state_q = st_arr_q;
        running_ret_vals_q = rv_arr_q;
        lo = hi;
    }

    // ---- Finish -------------------------------------------------------
    let (next_entry_w, final_all_ok_expr, final_fold_state_expr) = {
        let uid = "finish".to_string();
        let pf = &prover_funcs[n_blocks + n_chunks];
        let qf = &qsim_funcs[n_blocks + n_chunks];
        let vf = &verifier_funcs[n_blocks + n_chunks];

        let local_oracle_count = count_params_prefixed(pf, "oracle_rd_");
        let (oracle_vope, oracle_q) = if local_oracle_count > 0 {
            let (v, q) = emit_oracle(&mut stmts, &uid);
            (Some(v), Some(q))
        } else { (None, None) };

        let running_in_vope = (running_done_acc_vope.as_str(), running_next_pc_vope.as_str(), running_next_state_vope.as_str(), running_ret_vals_vope.as_str());
        let running_in_q = (running_done_acc_q.as_str(), running_next_pc_q.as_str(), running_next_state_q.as_str(), running_ret_vals_q.as_str());

        let p_outcome = build_call(
            &mut stmts, pf, entry_w, 0, None, None, None, Some(running_in_vope), &exported_vope, &synth_exported_vope,
            "", "", oracle_vope.as_deref(), &format!("p_{uid}"),
        );
        // Unlike block/chunk functions, finish's return type is doubly
        // nested (`(output, hats)` where `output` is itself the raw
        // terminator's own return-arg tuple) -- destructure it explicitly
        // rather than treating `finish_output` as flat.
        let p_slots = p_outcome.finish_output;
        let p_hats = p_slots[1].clone();
        let p_state_local = match &p_slots[0] { Slot::Scalar(n) => n.clone(), _ => unreachable!() };
        let p_terminator_out = destructure_finish_output(&mut stmts, pf, &p_state_local, &format!("p_{uid}"));
        // Element 0 of the terminator's own return args is the movfuscated
        // circuit's `done` flag, not one of the original circuit params --
        // drop it so `p_output` lines up 1:1 with `entry_w`.
        let p_output: Vec<Slot> = p_terminator_out[1..].to_vec();

        let q_outcome = build_call(
            &mut stmts, qf, entry_w, 1, Some(&p_hats), None, None, Some(running_in_q), &exported_q, &synth_exported_q,
            "", "", oracle_q.as_deref(), &format!("q_{uid}"),
        );
        let q_slots = q_outcome.finish_output;
        let q_and_arr = q_slots[1].clone();
        let q_state_local = match &q_slots[0] { Slot::Scalar(n) => n.clone(), _ => unreachable!() };
        let q_terminator_out = destructure_finish_output(&mut stmts, qf, &q_state_local, &format!("q_{uid}"));
        let q_output: Vec<Slot> = q_terminator_out[1..].to_vec();

        let and_count = and_count_of(vf);
        let r_ands_name = format!("_rands_{uid}");
        and_gate_seed += 1;
        stmts.push(r_ands_decl_stmt(&r_ands_name, and_count, step_expr, and_gate_seed));
        let v_outcome = build_call(
            &mut stmts, vf, entry_w, 1, Some(&p_hats), Some(&q_and_arr), Some(&r_ands_name), Some(running_in_q), &exported_q, &synth_exported_q,
            &all_ok_expr, &fold_state_expr, oracle_q.as_deref(), &format!("v_{uid}"),
        );
        let v_slots = v_outcome.finish_output;
        let v_all_ok = match &v_slots[v_slots.len() - 2] { Slot::Scalar(n) => n.clone(), _ => unreachable!() };
        let v_fold_state = match &v_slots[v_slots.len() - 1] { Slot::Scalar(n) => n.clone(), _ => unreachable!() };
        stmts.push(assert_true_stmt(var(&v_all_ok), "finish: honest run must pass the woven verifier's own check"));

        // Next step's entry state: the finish function's own real output
        // (the terminator's actual return args), Vope side from the real
        // prover call and Q side from the real qsim call -- structurally
        // identical shape (same original circuit params), different value
        // representation per side, exactly like `entry_w` itself.
        assert_eq!(p_output.len(), q_output.len());
        let next_entry_w: Vec<(Slot, Slot)> = p_output.into_iter().zip(q_output).collect();
        (next_entry_w, v_all_ok, v_fold_state)
    };

    StepResultIr {
        stmts,
        next_entry_w,
        final_all_ok_expr,
        final_fold_state_expr,
        finish_output_slots: vec![],
    }
}
