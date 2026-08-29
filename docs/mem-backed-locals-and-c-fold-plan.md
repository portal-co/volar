# Memory-backed locals in the LIR weaver + C emitter expression folding

> Status: **implemented** (stages 1–3 complete, 2026-08-29). Measurements in
> the stage record below.

## Stage record (2026-08-29)

| Stage | Commit | Content |
|---|---|---|
| 0 | volar `16a8433` | this plan |
| 1 | volar-ir `a6be4c6` | record-then-render C emission: use-count DCE + single-use folding + comma sequencing + hazard-checked jump edges |
| 2 | volar `e5f8a62` | memory-backed aggregate locals by default (`aggregate_locals`, `bind_ident_mem_backed`, pointer-walk index/assign, native-loop exemption) |
| 3 | (this doc) | measurements below |

**`lir_probe_prover_lowers_to_c` (mem_probe split prover, 14 woven fns,
2,740 real AND gates):**

| Configuration | C source bytes | lower wall-clock |
|---|---|---|
| session start (flat locals, eager emitter) | 1,025,561,412 | ~330 s |
| + folding emitter only (stage 1) | 425,100,841 | 329 s |
| + memory-backed locals (stage 2) | **53,616,407** | **18.6 s** |

**Harness wall-clock:** native_loop_equiv 813 s → 409 s;
lir_backend_components 805 s → 406 s (same pre-existing failures).

Suite status after both stages: volar-c-backend-spec-tests basic 21
(18 + 3 new memory-backed tests), lir_backend 1, curve_e2e 6, e2e 14,
box_pool 1, native_loop_equiv 3 — all pass; lir_backend_components 3
passed / 2 failed (standing vole_setup + faest_core `R: SpecRng` gap);
volar-riscv-e2e 20 passed / 1 failed (standing
`trace_mem_probe_plain_values_has_correct_widths`); volar-weaver 138;
volar-ir workspace green except the standing `prop_d2` proptest case.

The `VOLAR_C_FOLD_STATS=1` env var prints per-function fold counters
(defs / dropped / inlined / body bytes); `VOLAR_C_NOFOLD=1` disables the
C-side folding pass for A/B debugging.

## Problem

Two compounding defects dominate generated C size (and `cc` wall-clock) at
mem_probe scale:

1. **Flat-scalar aggregate locals.** `volar-lir-codegen` binds every local as
   `Vec<T::Value>` (flat SSA scalars). A runtime-indexed read `arr[i]` over an
   `n`-element array of `w`-scalar elements builds a `w`-deep, `n`-way
   **select mux tree** (`lower_index`), and an aggregate if-join threads
   `n*w` join-block params with per-edge assignments
   (`lower_if`). The memory-backed alternative (`PromotedSlot`,
   `ptr_index_load`/`ptr_index_store`) exists but only for native-loop
   aggregates (Stage 2 of `lir-native-loops-plan.md`) and for `>=512`-scalar
   fixed-array lets.

2. **Eager one-assignment-per-value C emission.** `CBackend::emit_instr`
   writes `T vN = expr;` for every instruction, unconditionally: unused defs
   are emitted (aggregate **unpacks** emit one statement per scalar of every
   call result — mostly never read), single-use temps are never folded into
   their use, and every LIR op costs a full statement of text.

## Part A — memory-backed aggregate locals (volar-lir-codegen)

Make memory the default representation for **aggregate locals** (arrays of
any length, structs, tuples) whenever the target has `StackAllocExt` (C
backend). Non-memory targets (e.g. `VolarIrTarget`, which lacks alloca)
keep the flat model unchanged. Scalars stay flat SSA.

Core invariant (replaces the ad-hoc `>=512` rule): **binding is storing.**
`let name = init;` with an aggregate type allocates (once) and stores into a
slot; every subsequent read/write of `name` — including in later unrolled
iterations, where today a second `let` silently re-binds flat `env` values
and can desynchronize from slot writes (latent shadowing bug) — goes through
memory.

Routing points:

| Site | Change |
|---|---|
| `lower_stmt` Let (Ident pattern, aggregate type) | promote to slot always (any size); existing slot ⇒ `promoted_whole_store` (rebind = store) |
| Let with `If` init | new joinless if-lowering: both branches `promoted_whole_store` into the slot; join block has no params — kills aggregate select/param threading |
| `lower_expr` Var | already loads through `ctx.promoted` (unchanged) |
| `lower_index` | base (after peeling nested `Index`) resolves to a slot ⇒ `ptr_index_load` (constant and runtime indices); intermediate chain levels ⇒ `ptr_offset`; flat-env bases keep the const fast path / mux fallback |
| `lower_assign` Index/Field | slot-rooted chains ⇒ `ptr_index_store` (single level) / `ptr_offset` folding (nested); `Whole` slots keep load-splice-store for field writes |
| native loops | pre-promoted names are already memory: never carried as block params, never re-promoted; scan treats them as promoted |
| tuple patterns | aggregate sub-elements bind flat in v1 (recorded limitation) |

Option plumbing: `MonoPlanOptions.aggregate_locals: bool` (default `true`).
`false` restores the previous flat model for A/B equivalence and debugging.

## Part B — C emitter expression folding (volar-ir `volar-c-backend`)

Restructure each function's emission into **record then render**:

- **Record:** the `LirTarget` impl writes a structured item stream instead of
  text: `Value { result, ty, expr, purity }`, `Store`, side-effecting
  statements (`Call`/`Rng`/`Oracle`/`Action`), `Label`,
  `Jump`/`Branch`/`Switch` (args as expressions), `Ret`. `Expr` is a small
  owned tree whose leaves reference operand value IDs.
- **Use counts:** one pass over the stream counts every `Name` occurrence.
- **Decision per value:**
  - side-effecting defs (calls, rng): always emitted, never dropped, never
    inlined;
  - memory-read defs (`*ptr`, `ptr[idx]`): dropped when unused, otherwise
    materialized (v1: never inlined);
  - pure defs: `0` surviving uses ⇒ **dropped** (DCE, cascading through
    operands); `1` use ⇒ **inlined** ("lazy values") at that use; `>=2` ⇒
    materialized once as `T vN = <expr>;`.
- **Render:** statements render operand expressions by substitution
  (materialized ⇒ name, inlined ⇒ recursively rendered expression — a
  single-use def has exactly one use site, so no duplication blow-up).
- **Assignments-as-values / comma operator:** jump-edge temps become folded
  assignments (`blockM_pK = expr;` directly when the expression references no
  target block param — the two-phase temp only exists for parallel-assign
  hazards); when more than one side-effecting def must fold into a single
  statement (C call-argument evaluation order is unspecified), sequence them
  with the comma operator through pre-declared comma temps:
  `(t0 = g(..), t1 = h(..), body)`.
- Soundness: pure expressions over SSA names are safe to re-evaluate at any
  point textually after their def (names are write-once except block params,
  which only ever take newer values of the same latency), and inlining only
  ever moves evaluation **later**. Memory reads and calls are never moved.

`finish()` output shape (typedefs, externs, siblings, function shells,
preamble) is unchanged; only per-function bodies shrink. The `LirTarget`
value/block handle API is unchanged, so callers (volar-lir-codegen) are
unaffected.

## Stages

## Success criteria

- `lir_probe_prover_lowers_to_c` output drops from ~1.03 GB toward the MB
  range; `cc` wall-clock drops correspondingly.
- No new failures anywhere; the standing pre-existing set is unchanged
  (lir_backend_components vole_setup/faest_core, `lir_backend` const-L,
  `trace_mem_probe_plain_values_has_correct_widths`, `test_ts_backend_no_errors`).
- Unroll-vs-Native instance-set equivalence (native_loop_equiv) still holds.

## Risks

- **Cross-block mis-folding** (Part B): mitigated by pure-only inlining and
  the textual def-before-use argument; guarded by the compile-and-run suites
  (volar-c-backend tests, spec-tests, native_loop_equiv).
- Struct-typed slot reads do a struct copy + unpack per read (no field GEP in
  LIR); acceptable — Part B folds the unpacked single-use fields into their
  uses. A memcpy fast path for whole-array copies is future work.
- Part A increases slot declarations; Part B drops far more statements than
  it adds.
