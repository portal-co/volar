//! Milestone 1.6: generates the Rust driver source that interleaves calls
//! to a *split* VOLE weave's real, compiled prover/`QSim`/verifier
//! functions -- one function per movfuscated block, plus chunked
//! accumulator functions, plus a finish function -- threading each
//! function's own exported state into the next's matching input params.
//!
//! Built once here (validated first on `mem_probe`'s small circuit) and
//! reused as-is for the real interpreter (Stage 2, `interpreter_e2e.rs`):
//! the shape of "one function per block, chunked accumulator, finish" is
//! identical in both cases (same `movfuscate_ir_with_boundary`/split-weave
//! machinery), only the *size* differs.
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
//!   `next_state_{i}_{k}[_{j}]`/`ret_val_{i}_{m}[_{j}]` (the `_{j}` suffix
//!   only for wide slots, one scalar param per lane).
//! - Chunk `c`'s own running accumulator (`done_acc`/`next_pc`/
//!   `next_state`/`ret_vals`) is consumed by chunk `c+1` (or `finish`, if
//!   `c` is the last chunk) as `in_done_acc`/`in_next_pc_{j}`/
//!   `in_next_state_{k}[_{j}]`/`in_ret_val_{m}[_{j}]`. Chunk 0's own inputs
//!   are always literal zero (movfuscation's own accumulator seed).
//! - A block/chunk/finish function's own trailing `hat`s (prover) are fed
//!   *directly* to the matching (same name, same index) `QSim` function's
//!   `hat_k` inputs; that `QSim` function's own derived `q_and` values are
//!   fed to the matching `Verifier` function's `q_and_k` inputs -- this
//!   threading is *within* one function-index, across roles, not between
//!   function indices.
//! - The verifier's `all_ok`/`fold_state` thread linearly across *every*
//!   verifier-role call in call order (block 0, block 1, ..., chunk 0, ...,
//!   finish) -- unlike next_state, this is a single continuous chain, not
//!   scoped to movfuscation's own block/chunk topology.
//! - Oracle reads (`oracle_rd_k`) are local to each function (reset to 0
//!   per function); this module consumes real committed values
//!   sequentially, in call order, matching the order the split weave's own
//!   aggregated `MemoryTrace` records real `StorageRead`s in (verified by
//!   construction: both come from the same statement-order walk).

use std::collections::BTreeMap;
use volar_compiler::ir::{ArrayLength, IrFunction, IrType};
use volar_ir_passes::{MovfuscAccumInfo, MovfuscBlockBoundary};

/// A driver-tracked value: either one Rust local (a scalar Q/Vope), or a
/// `[T; n]` Rust local a caller can index (a wide slot, one array value
/// covering all `n` lanes) -- mirrors `vole.rs`'s own `WireRepr` split.
#[derive(Clone, Debug)]
pub enum Slot {
    Scalar(String),
    Array(String, usize),
}

impl Slot {
    /// Expand into the exact positional argument expressions a callee's
    /// own params expect for this slot (one `name.clone()`, or `n` separate
    /// `name[j].clone()`s for a wide slot -- matching `bind_scalar`'s own
    /// param-splitting convention exactly).
    fn args(&self) -> Vec<String> {
        match self {
            Slot::Scalar(n) => vec![format!("{n}.clone()")],
            Slot::Array(n, w) => (0..*w).map(|j| format!("{n}[{j}].clone()")).collect(),
        }
    }
}

fn slot_from_ty(local_name: &str, ty: &IrType) -> Slot {
    match ty {
        IrType::Array { len: ArrayLength::Const(n), .. } => Slot::Array(local_name.to_string(), *n),
        _ => Slot::Scalar(local_name.to_string()),
    }
}

/// The Rust local name a slot is bound to, regardless of whether it's a
/// scalar or a whole (unindexed) array value.
fn slot_name(s: &Slot) -> &str {
    match s {
        Slot::Scalar(n) | Slot::Array(n, _) => n,
    }
}

/// A valid Rust tuple literal for any element count, including exactly
/// one -- `(x)` is just a parenthesized expression, not a 1-tuple, so a
/// trailing comma is required whenever `items` is non-empty.
fn tuple_literal(items: &[String]) -> String {
    if items.is_empty() {
        "()".to_string()
    } else {
        format!("({},)", items.join(", "))
    }
}

/// Record an exported slot under `base_name` for later lookup by a
/// consuming function's own params -- for a *wide* slot, this means `n`
/// separate `{base_name}_{j}` entries (matching `bind_scalar`'s own
/// per-lane param-splitting convention exactly), not one `{base_name}`
/// entry pointing at an array.
fn insert_export(exported: &mut BTreeMap<String, Slot>, base_name: String, slot: Slot) {
    match slot {
        Slot::Scalar(_) => { exported.insert(base_name, slot); }
        Slot::Array(name, w) => {
            for j in 0..w {
                exported.insert(format!("{base_name}_{j}"), Slot::Scalar(format!("{name}[{j}]")));
            }
        }
    }
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
fn destructure_finish_output(out: &mut String, f: &IrFunction, state_local: &str, uid: &str) -> Vec<Slot> {
    let output_ty = tuple_elems(f.return_type.as_ref().expect("finish function must have a return type"))
        .into_iter().next().expect("finish function's return type must have an output element");
    match output_ty {
        IrType::Tuple(elem_tys) => {
            let names: Vec<String> = (0..elem_tys.len()).map(|k| format!("_{uid}_out_{k}")).collect();
            out.push_str(&format!("let ({}) = {state_local};\n", names.join(", ")));
            names.iter().zip(elem_tys.iter()).map(|(n, ty)| slot_from_ty(n, ty)).collect()
        }
        other => vec![slot_from_ty(state_local, &other)],
    }
}

/// Count of params whose name starts with `prefix` -- used to read back
/// how many `hat_k`/`oracle_rd_k`/etc. a given woven function declares,
/// directly from its own real signature (never assumed).
fn count_params_prefixed(f: &IrFunction, prefix: &str) -> usize {
    f.params.iter().filter(|p| p.name.starts_with(prefix)).count()
}

/// Per-slot width (1 = scalar, >1 = wide/array), read back from a chunk
/// function's own `{prefix}{k}` / `{prefix}{k}_{j}` running-accumulator
/// param names -- e.g. `prefix = "in_next_state_"` -- rather than assumed,
/// since `MovfuscAccumInfo` itself only records slot *count*, not width.
fn slot_widths_from_params(f: &IrFunction, prefix: &str, n_slots: usize) -> Vec<usize> {
    let mut widths = vec![1usize; n_slots];
    for p in &f.params {
        if let Some(rest) = p.name.strip_prefix(prefix) {
            let parts: Vec<&str> = rest.split('_').collect();
            let k: usize = parts[0].parse().unwrap();
            if let Some(j_str) = parts.get(1) {
                let j: usize = j_str.parse().unwrap();
                widths[k] = widths[k].max(j + 1);
            }
        }
    }
    widths
}

/// One step's worth of generated Rust statements (as a single string),
/// calling every block/chunk/finish function (prover, then qsim, then
/// verifier, per function-index) for `prover_funcs`/`qsim_funcs`/
/// `verifier_funcs` (each: blocks ++ chunks ++ [finish], in that order --
/// exactly the order `emit_fn` is called in by the split weave).
///
/// `entry_w`: the current entry-state, as `(vope_slots, q_slots)` pairs,
/// one per original circuit param (index-matched to `circuit.blocks[0].params`).
/// `all_ok_fold_state_in`: `(all_ok_expr, fold_state_expr)` to seed the
/// verifier chain's first call; `None` means "use fresh literals"
/// (`true`/`iop_accumulator_fresh()`).
/// `oracle_bits`: real plain bit-values for every real `StorageRead` this
/// step performs, **in the same statement order** the aggregated
/// `MemoryTrace` (from any one of the three split weaves -- identical
/// shape across roles) lists them; each inner `Vec<bool>` is LSB-first and
/// must have exactly the read's own bit-width.
/// `step_idx`: used only to keep `r_and`/RNG-seed values distinct across
/// outer steps (soundness doesn't need this to be unpredictable here --
/// this is a driven *test*, not a real deployment).
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

pub fn generate_split_step(
    prover_funcs: &[IrFunction],
    qsim_funcs: &[IrFunction],
    verifier_funcs: &[IrFunction],
    boundary: &[MovfuscBlockBoundary],
    accum_info: &MovfuscAccumInfo,
    n_chunks: usize,
    entry_w: &[(Slot, Slot)],
    all_ok_fold_state_in: Option<(String, String)>,
    oracle_bits: &[Vec<bool>],
    step_idx: usize,
) -> StepResult {
    let n_blocks = boundary.len();
    assert_eq!(prover_funcs.len(), n_blocks + n_chunks + 1);
    assert_eq!(qsim_funcs.len(), n_blocks + n_chunks + 1);
    assert_eq!(verifier_funcs.len(), n_blocks + n_chunks + 1);

    let mut out = String::new();
    let mut oracle_cursor = 0usize;
    let mut and_gate_seed = 0u64;
    let mut all_ok_expr = all_ok_fold_state_in.as_ref().map(|(a, _)| a.clone()).unwrap_or_else(|| "true".to_string());
    let mut fold_state_expr = all_ok_fold_state_in.as_ref().map(|(_, f)| f.clone()).unwrap_or_else(|| "iop_accumulator_fresh()".to_string());

    // Per-block-or-chunk-or-finish exported next_pc/next_state/ret_vals
    // slots, keyed by the *consuming* param-name prefix (e.g.
    // "next_state_2" for block 2's slot 2) -- populated after each block
    // call, consumed when building the covering chunk's own call. Two maps,
    // same reason `entry_w`/`running_in` are dual-threaded: the prover's
    // chunk function needs its own (Vope-typed) block exports, the
    // qsim/verifier chunk functions need the verifier's (Q-typed) ones.
    let mut exported_vope: BTreeMap<String, Slot> = BTreeMap::new();
    let mut exported_q: BTreeMap<String, Slot> = BTreeMap::new();

    // Emit one committed oracle read (real value known host-side) as a
    // fresh `vole_commit_bit` call pair, returning (vope_name, q_name).
    let mut emit_oracle = |out: &mut String, uid: &str| -> (String, String) {
        let bits = &oracle_bits[oracle_cursor];
        oracle_cursor += 1;
        let width = bits.len();
        let mut vope_names = Vec::with_capacity(width);
        let mut q_names = Vec::with_capacity(width);
        for (j, &bit) in bits.iter().enumerate() {
            let vn = format!("_orv_{uid}_{j}");
            let qn = format!("_orq_{uid}_{j}");
            out.push_str(&format!(
                "let ({vn}, {qn}) = vole_commit_bit(&cot, &mut rng, sample_g, lift_bit_g, {bit});\n"
            ));
            vope_names.push(vn);
            q_names.push(qn);
        }
        // Bundle into one array-valued local each, so downstream Slot
        // handling is uniform regardless of width.
        let vope_arr = format!("_orv_{uid}_arr");
        let q_arr = format!("_orq_{uid}_arr");
        out.push_str(&format!("let {vope_arr} = [{}];\n", vope_names.iter().map(|n| format!("{n}.clone()")).collect::<Vec<_>>().join(", ")));
        out.push_str(&format!("let {q_arr} = [{}];\n", q_names.iter().map(|n| format!("{n}.clone()")).collect::<Vec<_>>().join(", ")));
        (vope_arr, q_arr)
    };

    // Build one function call's full argument list and emit the call +
    // destructure. `role` in {"vope", "q"} decides `vope_one`/`q_one`
    // literal naming; `is_qsim`/`is_verifier` decide extra trailing
    // params/outputs (all_ok/fold_state for verifier only).
    struct CallOutcome {
        is_active: Option<Slot>,
        done: Option<Slot>,
        next_pc: Vec<Slot>,
        next_state: Vec<Slot>,
        ret_vals: Vec<Slot>,
        trailing_array: Option<Slot>, // hats (prover) or q_and (qsim)
        all_ok: Option<String>,
        fold_state: Option<String>,
        finish_output: Vec<Slot>,
    }

    #[allow(clippy::too_many_arguments)]
    fn build_call(
        out: &mut String,
        f: &IrFunction,
        role_one: &str, // "vope_one(&delta)" or "q_one(&delta)"
        entry_w: &[(Slot, Slot)],
        entry_w_index: usize, // 0 = vope side, 1 = q side
        hat_in: Option<&Slot>, // hat_k input array: qsim consumes the prover's hats; verifier does too (both hat_k and q_and_k are separate verifier params)
        q_and_in: Option<&Slot>, // q_and_k input array: verifier only, sourced from qsim's own output
        r_ands: Option<&str>, // verifier only: name of a pre-bound [Gf128; n] local
        running_in: Option<(&str, &str, &str, &str)>, // (done_acc, next_pc_arr, next_state_arr_of_arrs, ret_val_arr_of_arrs) local names, chunk/finish only
        block_exports: &BTreeMap<String, Slot>,
        block_idx_range: Option<(usize, usize)>, // [lo, hi) block indices this chunk covers
        is_verifier: bool,
        all_ok_in: &str,
        fold_state_in: &str,
        oracle_arr: Option<&str>, // pre-bound oracle array local name (vope or q side, already selected)
        call_uid: &str,
    ) -> CallOutcome {
        let mut args: Vec<String> = Vec::new();
        for p in &f.params {
            let n = &p.name;
            if n == "delta" { args.push("&delta".to_string()); continue; }
            if n == "vope_one" || n == "q_one" { args.push(role_one.to_string()); continue; }
            if n.starts_with("hat_") {
                let idx: usize = n.rsplit('_').next().unwrap().parse().unwrap();
                let arr = hat_in.expect("hat_k param but no hat input array given");
                match arr {
                    Slot::Array(base, _) => args.push(format!("{base}[{idx}].clone()")),
                    Slot::Scalar(_) => unreachable!("hats array must be Slot::Array"),
                }
                continue;
            }
            if n.starts_with("q_and_") {
                let idx: usize = n.rsplit('_').next().unwrap().parse().unwrap();
                let arr = q_and_in.expect("q_and_k param but no q_and input array given");
                match arr {
                    Slot::Array(base, _) => args.push(format!("{base}[{idx}].clone()")),
                    Slot::Scalar(_) => unreachable!("q_and array must be Slot::Array"),
                }
                continue;
            }
            if n.starts_with("r_and_") {
                let idx: usize = n.rsplit('_').next().unwrap().parse().unwrap();
                let r = r_ands.expect("r_and_k param but no r_ands array given");
                args.push(format!("{r}[{idx}].clone()"));
                continue;
            }
            if n.starts_with("w_") {
                // w_{i} or w_{i}_{j}
                let rest = &n["w_".len()..];
                let i: usize = rest.split('_').next().unwrap().parse().unwrap();
                let slot = if entry_w_index == 0 { &entry_w[i].0 } else { &entry_w[i].1 };
                match slot {
                    Slot::Scalar(name) => { args.push(format!("{name}.clone()")); }
                    Slot::Array(name, w) => {
                        let j: usize = rest.rsplitn(2, '_').next().unwrap().parse().unwrap_or(0);
                        let _ = w;
                        args.push(format!("{name}[{j}].clone()"));
                    }
                }
                continue;
            }
            if n.starts_with("oracle_rd_") {
                let idx: usize = n.rsplit('_').next().unwrap().parse().unwrap();
                let arr = oracle_arr.expect("oracle_rd_k param but no oracle array given");
                args.push(format!("{arr}[{idx}].clone()"));
                continue;
            }
            if n == "all_ok_in" { args.push(all_ok_in.to_string()); continue; }
            if n == "fold_state_in" { args.push(fold_state_in.to_string()); continue; }
            if n.starts_with("in_done_acc") {
                args.push(format!("{}.clone()", running_in.expect("running accumulator input expected").0));
                continue;
            }
            if n.starts_with("in_next_pc_") {
                let j: usize = n.rsplit('_').next().unwrap().parse().unwrap();
                args.push(format!("{}[{j}].clone()", running_in.unwrap().1));
                continue;
            }
            if n.starts_with("in_next_state_") {
                // `running_in`'s next_state/ret_vals are heterogeneous
                // (each slot can be a different concrete type), so they're
                // built as a Rust *tuple* (`.{k}` field access), not an
                // array -- unlike `in_next_pc_`, which is uniform Q and
                // uses a real array.
                let rest = &n["in_next_state_".len()..];
                let parts: Vec<&str> = rest.split('_').collect();
                let k: usize = parts[0].parse().unwrap();
                let tup = running_in.unwrap().2;
                if parts.len() > 1 {
                    let j: usize = parts[1].parse().unwrap();
                    args.push(format!("{tup}.{k}[{j}].clone()"));
                } else {
                    args.push(format!("{tup}.{k}.clone()"));
                }
                continue;
            }
            if n.starts_with("in_ret_val_") {
                let rest = &n["in_ret_val_".len()..];
                let parts: Vec<&str> = rest.split('_').collect();
                let m: usize = parts[0].parse().unwrap();
                let tup = running_in.unwrap().3;
                if parts.len() > 1 {
                    let j: usize = parts[1].parse().unwrap();
                    args.push(format!("{tup}.{m}[{j}].clone()"));
                } else {
                    args.push(format!("{tup}.{m}.clone()"));
                }
                continue;
            }
            if n.starts_with("is_active_") || n.starts_with("done_")
                || n.starts_with("next_pc_") || n.starts_with("next_state_") || n.starts_with("ret_val_")
            {
                let slot = block_exports.get(n).unwrap_or_else(|| panic!("missing export for param {n}"));
                match slot {
                    Slot::Scalar(name) => args.push(format!("{name}.clone()")),
                    Slot::Array(name, _) => {
                        // Shouldn't happen: exported entries are stored
                        // pre-split per-lane for wide slots (see caller),
                        // so a direct name match implies scalar.
                        args.push(format!("{name}.clone()"));
                    }
                }
                continue;
            }
            panic!("unrecognized param name: {n}");
        }
        let _ = block_idx_range;
        let _ = is_verifier;

        let ret_ty = f.return_type.as_ref().expect("woven function must have a return type");
        let elems = tuple_elems(ret_ty);
        let call_expr = format!("{}({})", f.name, args.join(", "));
        let n_locals = elems.len();
        let local_names: Vec<String> = (0..n_locals).map(|i| format!("_r_{call_uid}_{i}")).collect();
        if n_locals == 1 {
            out.push_str(&format!("let {} = {};\n", local_names[0], call_expr));
        } else {
            out.push_str(&format!("let ({}) = {};\n", local_names.join(", "), call_expr));
        }
        let slots: Vec<Slot> = local_names.iter().zip(elems.iter()).map(|(n, ty)| slot_from_ty(n, ty)).collect();
        CallOutcome { is_active: None, done: None, next_pc: vec![], next_state: vec![], ret_vals: vec![], trailing_array: None, all_ok: None, fold_state: None, finish_output: slots }
    }

    // ---- Blocks -----------------------------------------------------------
    for (i, b) in boundary.iter().enumerate() {
        let uid = format!("s{step_idx}_blk{i}");
        let n_pc = b.next_pc_bits.len();
        let n_state = b.next_state.len();
        let n_ret = b.ret_vals.len();

        let pf = &prover_funcs[i];
        let qf = &qsim_funcs[i];
        let vf = &verifier_funcs[i];

        let local_oracle_count = count_params_prefixed(pf, "oracle_rd_");
        let (oracle_vope, oracle_q) = if local_oracle_count > 0 {
            let (v, q) = emit_oracle(&mut out, &uid);
            (Some(v), Some(q))
        } else { (None, None) };

        // Prover.
        let p_outcome = build_call(
            &mut out, pf, "vope_one(&delta)", entry_w, 0, None, None, None, None, &exported_vope, Some((i, i + 1)), false,
            "", "", oracle_vope.as_deref(), &format!("p_{uid}"),
        );
        let p_slots = p_outcome.finish_output;
        // Layout: [is_active, done, next_pc.., next_state.., ret_vals.., hats]
        let mut idx = 0usize;
        let p_is_active = p_slots[idx].clone(); idx += 1;
        let p_done = p_slots[idx].clone(); idx += 1;
        let p_next_pc: Vec<Slot> = p_slots[idx..idx + n_pc].to_vec(); idx += n_pc;
        let p_next_state: Vec<Slot> = p_slots[idx..idx + n_state].to_vec(); idx += n_state;
        let p_ret_vals: Vec<Slot> = p_slots[idx..idx + n_ret].to_vec(); idx += n_ret;
        let p_hats = p_slots[idx].clone();

        // QSim: consumes prover's hats as hat_k input.
        let local_oracle_count_q = count_params_prefixed(qf, "oracle_rd_");
        assert_eq!(local_oracle_count_q, local_oracle_count, "block {i}: qsim/prover oracle count mismatch");
        let q_outcome = build_call(
            &mut out, qf, "q_one(&delta)", entry_w, 1, Some(&p_hats), None, None, None, &exported_q, Some((i, i + 1)), false,
            "", "", oracle_q.as_deref(), &format!("q_{uid}"),
        );
        let q_slots = q_outcome.finish_output;
        let mut idx = 0usize;
        let q_is_active = q_slots[idx].clone(); idx += 1;
        let q_done = q_slots[idx].clone(); idx += 1;
        let q_next_pc: Vec<Slot> = q_slots[idx..idx + n_pc].to_vec(); idx += n_pc;
        let q_next_state: Vec<Slot> = q_slots[idx..idx + n_state].to_vec(); idx += n_state;
        let q_ret_vals: Vec<Slot> = q_slots[idx..idx + n_ret].to_vec(); idx += n_ret;
        let q_and_arr = q_slots[idx].clone();

        // r_and challenges for this block's own gate count.
        let and_count = count_params_prefixed(vf, "q_and_");
        let r_ands_name = format!("_rands_{uid}");
        and_gate_seed += 1;
        out.push_str(&format!(
            "let {r_ands_name}: [Gf128; {and_count}] = core::array::from_fn(|k| Gf128::from_u64({} * 1_000_003 + k as u64));\n",
            (step_idx as u64) * 10_000_000 + and_gate_seed * 100_000
        ));

        let local_oracle_count_v = count_params_prefixed(vf, "oracle_rd_");
        assert_eq!(local_oracle_count_v, local_oracle_count, "block {i}: verifier/prover oracle count mismatch");
        let v_outcome = build_call(
            &mut out, vf, "q_one(&delta)", entry_w, 1, Some(&p_hats), Some(&q_and_arr), Some(&r_ands_name), None, &exported_q, Some((i, i + 1)), true,
            &all_ok_expr, &fold_state_expr, oracle_q.as_deref(), &format!("v_{uid}"),
        );
        let v_slots = v_outcome.finish_output;
        let mut idx = 0usize;
        let v_is_active = v_slots[idx].clone(); idx += 1;
        let v_done = v_slots[idx].clone(); idx += 1;
        let v_next_pc: Vec<Slot> = v_slots[idx..idx + n_pc].to_vec(); idx += n_pc;
        let v_next_state: Vec<Slot> = v_slots[idx..idx + n_state].to_vec(); idx += n_state;
        let v_ret_vals: Vec<Slot> = v_slots[idx..idx + n_ret].to_vec(); idx += n_ret;
        let v_all_ok = match &v_slots[idx] { Slot::Scalar(n) => n.clone(), _ => unreachable!() }; idx += 1;
        let v_fold_state = match &v_slots[idx] { Slot::Scalar(n) => n.clone(), _ => unreachable!() };

        out.push_str(&format!("assert!({v_all_ok}, \"step {step_idx} block {i}: honest run must pass the woven verifier's own check\");\n"));
        all_ok_expr = v_all_ok;
        fold_state_expr = v_fold_state;

        // The prover's own (Vope-typed) block exports feed the *prover*
        // chunk function; the verifier's own (Q-typed) block exports feed
        // the qsim/verifier chunk functions -- two separate maps, exactly
        // like `entry_w`/`running_in`. QSim's own export values are never
        // consumed downstream (only its q_and output is).
        let _ = (q_is_active, q_done, q_next_pc, q_next_state, q_ret_vals);

        insert_export(&mut exported_vope, format!("is_active_{i}"), p_is_active);
        insert_export(&mut exported_vope, format!("done_{i}"), p_done);
        for (j, s) in p_next_pc.into_iter().enumerate() {
            insert_export(&mut exported_vope, format!("next_pc_{i}_{j}"), s);
        }
        for (k, s) in p_next_state.into_iter().enumerate() {
            insert_export(&mut exported_vope, format!("next_state_{i}_{k}"), s);
        }
        for (m, s) in p_ret_vals.into_iter().enumerate() {
            insert_export(&mut exported_vope, format!("ret_val_{i}_{m}"), s);
        }

        insert_export(&mut exported_q, format!("is_active_{i}"), v_is_active);
        insert_export(&mut exported_q, format!("done_{i}"), v_done);
        for (j, s) in v_next_pc.into_iter().enumerate() {
            insert_export(&mut exported_q, format!("next_pc_{i}_{j}"), s);
        }
        for (k, s) in v_next_state.into_iter().enumerate() {
            insert_export(&mut exported_q, format!("next_state_{i}_{k}"), s);
        }
        for (m, s) in v_ret_vals.into_iter().enumerate() {
            insert_export(&mut exported_q, format!("ret_val_{i}_{m}"), s);
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
    out.push_str(&format!("let {running_done_acc_vope} = vope_zero();\n"));
    out.push_str(&format!("let {running_done_acc_q} = q_zero();\n"));
    out.push_str(&format!(
        "let {running_next_pc_vope}: [Vope<N, Galois, cipher::consts::U1>; {}] = core::array::from_fn(|_| vope_zero());\n",
        accum_info.init.next_pc.len()
    ));
    out.push_str(&format!(
        "let {running_next_pc_q}: [Q<N, Galois>; {}] = core::array::from_fn(|_| q_zero());\n",
        accum_info.init.next_pc.len()
    ));
    // Slot widths aren't recorded by `MovfuscAccumInfo` itself (only slot
    // *count* is) -- read them back from the first chunk-or-finish
    // function's own `in_next_state_`/`in_ret_val_` param names, since a
    // wide slot's zero-init must be a `[T; w]` array literal, not a bare
    // scalar `{q,vope}_zero()`. Widths are the same on both sides (only the
    // element type differs), so one reading suffices.
    let n_state = accum_info.init.next_state.len();
    let n_ret = accum_info.init.ret_vals.len();
    let first_chunk_or_finish = &verifier_funcs[n_blocks];
    let state_widths = slot_widths_from_params(first_chunk_or_finish, "in_next_state_", n_state);
    let ret_widths = slot_widths_from_params(first_chunk_or_finish, "in_ret_val_", n_ret);
    let mut init_state_locals_vope = Vec::with_capacity(n_state);
    let mut init_state_locals_q = Vec::with_capacity(n_state);
    for (k, &w) in state_widths.iter().enumerate() {
        let name_vope = format!("_acc_init_state_vope_{k}");
        let name_q = format!("_acc_init_state_q_{k}");
        if w <= 1 {
            out.push_str(&format!("let {name_vope}: Vope<N, Galois, cipher::consts::U1> = vope_zero();\n"));
            out.push_str(&format!("let {name_q}: Q<N, Galois> = q_zero();\n"));
        } else {
            out.push_str(&format!("let {name_vope}: [Vope<N, Galois, cipher::consts::U1>; {w}] = core::array::from_fn(|_| vope_zero());\n"));
            out.push_str(&format!("let {name_q}: [Q<N, Galois>; {w}] = core::array::from_fn(|_| q_zero());\n"));
        }
        init_state_locals_vope.push(name_vope);
        init_state_locals_q.push(name_q);
    }
    out.push_str(&format!("let {running_next_state_vope} = {};\n", tuple_literal(&init_state_locals_vope)));
    out.push_str(&format!("let {running_next_state_q} = {};\n", tuple_literal(&init_state_locals_q)));
    let mut init_ret_locals_vope = Vec::with_capacity(n_ret);
    let mut init_ret_locals_q = Vec::with_capacity(n_ret);
    for (m, &w) in ret_widths.iter().enumerate() {
        let name_vope = format!("_acc_init_ret_vope_{m}");
        let name_q = format!("_acc_init_ret_q_{m}");
        if w <= 1 {
            out.push_str(&format!("let {name_vope}: Vope<N, Galois, cipher::consts::U1> = vope_zero();\n"));
            out.push_str(&format!("let {name_q}: Q<N, Galois> = q_zero();\n"));
        } else {
            out.push_str(&format!("let {name_vope}: [Vope<N, Galois, cipher::consts::U1>; {w}] = core::array::from_fn(|_| vope_zero());\n"));
            out.push_str(&format!("let {name_q}: [Q<N, Galois>; {w}] = core::array::from_fn(|_| q_zero());\n"));
        }
        init_ret_locals_vope.push(name_vope);
        init_ret_locals_q.push(name_q);
    }
    out.push_str(&format!("let {running_ret_vals_vope} = {};\n", tuple_literal(&init_ret_locals_vope)));
    out.push_str(&format!("let {running_ret_vals_q} = {};\n", tuple_literal(&init_ret_locals_q)));

    let pc_w = accum_info.init.next_pc.len();
    let st_w = accum_info.init.next_state.len();
    let rv_w = accum_info.init.ret_vals.len();

    // Parse a chunk/finish-shaped flat `[done_acc, next_pc.., next_state..,
    // ret_vals.., ..trailing]` output into its running-state prefix (the
    // trailing elements -- hats, or all_ok+fold_state -- are the caller's
    // own concern).
    let parse_running_output = |slots: &[Slot]| -> (Slot, Vec<Slot>, Vec<Slot>, Vec<Slot>) {
        let mut idx = 0usize;
        let done_acc = slots[idx].clone(); idx += 1;
        let next_pc: Vec<Slot> = slots[idx..idx + pc_w].to_vec(); idx += pc_w;
        let next_state: Vec<Slot> = slots[idx..idx + st_w].to_vec(); idx += st_w;
        let ret_vals: Vec<Slot> = slots[idx..idx + rv_w].to_vec();
        (done_acc, next_pc, next_state, ret_vals)
    };

    for c in 0..n_chunks {
        let hi = (lo + chunk_size).min(n_blocks);
        let uid = format!("s{step_idx}_chunk{c}");
        let pf = &prover_funcs[n_blocks + c];
        let qf = &qsim_funcs[n_blocks + c];
        let vf = &verifier_funcs[n_blocks + c];

        let local_oracle_count = count_params_prefixed(pf, "oracle_rd_");
        let (oracle_vope, oracle_q) = if local_oracle_count > 0 {
            let (v, q) = emit_oracle(&mut out, &uid);
            (Some(v), Some(q))
        } else { (None, None) };

        let running_in_vope = (running_done_acc_vope.as_str(), running_next_pc_vope.as_str(), running_next_state_vope.as_str(), running_ret_vals_vope.as_str());
        let running_in_q = (running_done_acc_q.as_str(), running_next_pc_q.as_str(), running_next_state_q.as_str(), running_ret_vals_q.as_str());

        let p_outcome = build_call(
            &mut out, pf, "vope_one(&delta)", entry_w, 0, None, None, None, Some(running_in_vope), &exported_vope, Some((lo, hi)), false,
            "", "", oracle_vope.as_deref(), &format!("p_{uid}"),
        );
        let p_slots = p_outcome.finish_output;
        let p_hats = p_slots.last().unwrap().clone();
        let (p_new_done_acc, p_new_next_pc, p_new_next_state, p_new_ret_vals) = parse_running_output(&p_slots);

        let q_outcome = build_call(
            &mut out, qf, "q_one(&delta)", entry_w, 1, Some(&p_hats), None, None, Some(running_in_q), &exported_q, Some((lo, hi)), false,
            "", "", oracle_q.as_deref(), &format!("q_{uid}"),
        );
        let q_and_arr = q_outcome.finish_output.last().unwrap().clone();

        let and_count = count_params_prefixed(vf, "q_and_");
        let r_ands_name = format!("_rands_{uid}");
        and_gate_seed += 1;
        out.push_str(&format!(
            "let {r_ands_name}: [Gf128; {and_count}] = core::array::from_fn(|k| Gf128::from_u64({} * 1_000_003 + k as u64));\n",
            (step_idx as u64) * 10_000_000 + and_gate_seed * 100_000
        ));
        let v_outcome = build_call(
            &mut out, vf, "q_one(&delta)", entry_w, 1, Some(&p_hats), Some(&q_and_arr), Some(&r_ands_name), Some(running_in_q), &exported_q, Some((lo, hi)), true,
            &all_ok_expr, &fold_state_expr, oracle_q.as_deref(), &format!("v_{uid}"),
        );
        let v_slots = v_outcome.finish_output;
        // Layout: [done_acc, next_pc.., next_state.., ret_vals.., all_ok, fold_state]
        let (v_new_done_acc, v_new_next_pc, v_new_next_state, v_new_ret_vals) = parse_running_output(&v_slots);
        let v_all_ok = match &v_slots[1 + pc_w + st_w + rv_w] { Slot::Scalar(n) => n.clone(), _ => unreachable!() };
        let v_fold_state = match &v_slots[2 + pc_w + st_w + rv_w] { Slot::Scalar(n) => n.clone(), _ => unreachable!() };

        out.push_str(&format!("assert!({v_all_ok}, \"step {step_idx} chunk {c}: honest run must pass the woven verifier's own check\");\n"));
        all_ok_expr = v_all_ok;
        fold_state_expr = v_fold_state;

        let done_acc_vope_name = match &p_new_done_acc { Slot::Scalar(n) => n.clone(), _ => unreachable!() };
        let pc_arr_vope = format!("_acc_pc_vope_{uid}");
        out.push_str(&format!("let {pc_arr_vope} = [{}];\n", p_new_next_pc.iter().map(|s| format!("{}.clone()", slot_name(s))).collect::<Vec<_>>().join(", ")));
        let st_arr_vope = format!("_acc_st_vope_{uid}");
        out.push_str(&format!(
            "let {st_arr_vope} = {};\n",
            tuple_literal(&p_new_next_state.iter().map(|s| format!("{}.clone()", slot_name(s))).collect::<Vec<_>>())
        ));
        let rv_arr_vope = format!("_acc_rv_vope_{uid}");
        out.push_str(&format!(
            "let {rv_arr_vope} = {};\n",
            tuple_literal(&p_new_ret_vals.iter().map(|s| format!("{}.clone()", slot_name(s))).collect::<Vec<_>>())
        ));

        let done_acc_q_name = match &v_new_done_acc { Slot::Scalar(n) => n.clone(), _ => unreachable!() };
        let pc_arr_q = format!("_acc_pc_q_{uid}");
        out.push_str(&format!("let {pc_arr_q} = [{}];\n", v_new_next_pc.iter().map(|s| format!("{}.clone()", slot_name(s))).collect::<Vec<_>>().join(", ")));
        let st_arr_q = format!("_acc_st_q_{uid}");
        out.push_str(&format!(
            "let {st_arr_q} = {};\n",
            tuple_literal(&v_new_next_state.iter().map(|s| format!("{}.clone()", slot_name(s))).collect::<Vec<_>>())
        ));
        let rv_arr_q = format!("_acc_rv_q_{uid}");
        out.push_str(&format!(
            "let {rv_arr_q} = {};\n",
            tuple_literal(&v_new_ret_vals.iter().map(|s| format!("{}.clone()", slot_name(s))).collect::<Vec<_>>())
        ));

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
    {
        let uid = format!("s{step_idx}_finish");
        let pf = &prover_funcs[n_blocks + n_chunks];
        let qf = &qsim_funcs[n_blocks + n_chunks];
        let vf = &verifier_funcs[n_blocks + n_chunks];

        let local_oracle_count = count_params_prefixed(pf, "oracle_rd_");
        let (oracle_vope, oracle_q) = if local_oracle_count > 0 {
            let (v, q) = emit_oracle(&mut out, &uid);
            (Some(v), Some(q))
        } else { (None, None) };

        let running_in_vope = (running_done_acc_vope.as_str(), running_next_pc_vope.as_str(), running_next_state_vope.as_str(), running_ret_vals_vope.as_str());
        let running_in_q = (running_done_acc_q.as_str(), running_next_pc_q.as_str(), running_next_state_q.as_str(), running_ret_vals_q.as_str());

        let p_outcome = build_call(
            &mut out, pf, "vope_one(&delta)", entry_w, 0, None, None, None, Some(running_in_vope), &exported_vope, None, false,
            "", "", oracle_vope.as_deref(), &format!("p_{uid}"),
        );
        // Unlike block/chunk functions, finish's return type is doubly
        // nested (`(output, hats)` where `output` is itself the raw
        // terminator's own return-arg tuple) -- destructure it explicitly
        // rather than treating `finish_output` as flat.
        let p_slots = p_outcome.finish_output;
        let p_hats = p_slots[1].clone();
        let p_state_local = match &p_slots[0] { Slot::Scalar(n) => n.clone(), _ => unreachable!() };
        let p_terminator_out = destructure_finish_output(&mut out, pf, &p_state_local, &format!("p_{uid}"));
        // Element 0 of the terminator's own return args is the movfuscated
        // circuit's `done` flag, not one of the original circuit params --
        // drop it so `p_output` lines up 1:1 with `entry_w`.
        let p_output: Vec<Slot> = p_terminator_out[1..].to_vec();

        let q_outcome = build_call(
            &mut out, qf, "q_one(&delta)", entry_w, 1, Some(&p_hats), None, None, Some(running_in_q), &exported_q, None, false,
            "", "", oracle_q.as_deref(), &format!("q_{uid}"),
        );
        let q_slots = q_outcome.finish_output;
        let q_and_arr = q_slots[1].clone();
        let q_state_local = match &q_slots[0] { Slot::Scalar(n) => n.clone(), _ => unreachable!() };
        let q_terminator_out = destructure_finish_output(&mut out, qf, &q_state_local, &format!("q_{uid}"));
        let q_output: Vec<Slot> = q_terminator_out[1..].to_vec();

        let and_count = count_params_prefixed(vf, "q_and_");
        let r_ands_name = format!("_rands_{uid}");
        and_gate_seed += 1;
        out.push_str(&format!(
            "let {r_ands_name}: [Gf128; {and_count}] = core::array::from_fn(|k| Gf128::from_u64({} * 1_000_003 + k as u64));\n",
            (step_idx as u64) * 10_000_000 + and_gate_seed * 100_000
        ));
        let v_outcome = build_call(
            &mut out, vf, "q_one(&delta)", entry_w, 1, Some(&p_hats), Some(&q_and_arr), Some(&r_ands_name), Some(running_in_q), &exported_q, None, true,
            &all_ok_expr, &fold_state_expr, oracle_q.as_deref(), &format!("v_{uid}"),
        );
        let v_slots = v_outcome.finish_output;
        let v_all_ok = match &v_slots[v_slots.len() - 2] { Slot::Scalar(n) => n.clone(), _ => unreachable!() };
        let v_fold_state = match &v_slots[v_slots.len() - 1] { Slot::Scalar(n) => n.clone(), _ => unreachable!() };
        out.push_str(&format!("assert!({v_all_ok}, \"step {step_idx} finish: honest run must pass the woven verifier's own check\");\n"));

        // Next step's entry state: the finish function's own real output
        // (the terminator's actual return args), Vope side from the real
        // prover call and Q side from the real qsim call -- structurally
        // identical shape (same original circuit params), different value
        // representation per side, exactly like `entry_w` itself.
        assert_eq!(p_output.len(), q_output.len());
        let next_entry_w: Vec<(Slot, Slot)> = p_output.into_iter().zip(q_output).collect();

        StepResult {
            stmts: out,
            next_entry_w,
            final_all_ok_expr: v_all_ok,
            final_fold_state_expr: v_fold_state,
            finish_output_slots: vec![],
        }
    }
}
