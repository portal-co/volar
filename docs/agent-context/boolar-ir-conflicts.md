# Agent Context: Boolar IR vs. Volar IR — conflict log, and Boolar IR's status

**Load this when:** touching `volar-ir-passes` (movfuscation, `lower_to_circuit`),
`volar-weaver/src/vole.rs`, or any other shared pass/weaver code that has to
serve both `BIrBlocks` (Boolar IR) and `IRBlocks` (Volar IR).

## Status: Boolar IR support is **backlogged**

As of this log, Boolar IR (`BIrBlocks`) is **not actively extended**. Its
existing call sites keep working (via shims, see below), but new work
targets Volar IR (`IRBlocks`) directly, and Boolar IR does not get parallel
new features unless something specifically needs it.

**Why:** Boolar IR is Bit-only by construction (every wire is exactly 1
bit). Repeatedly, shared code written to serve both IRs has had to choose
between (a) staying Bit-only and blocking Volar IR improvements that need
real width (`_32`, `_64`, `Vec(k, _)`, ...), or (b) becoming width-aware and
risking Boolar IR's Bit-only assumptions being silently violated. This has
happened enough times in this project's history — sometimes caught by
review, sometimes not — that treating Boolar IR as a co-equal, actively
maintained general-purpose target IR is no longer the effective default.
Volar IR is the general-purpose target going forward; Boolar IR is kept
working for existing callers via thin shims, not extended.

**What this means in practice:**
- New passes/weaver features: write them for `IRBlocks` (Volar IR) directly.
  Only add a `BIrBlocks` counterpart if something concretely still needs it.
- When a function must serve both: write the width-aware version as the
  "core" implementation, and make the Boolar-IR-facing name a thin shim
  that calls the core at width 1 — **the shim's output must be
  byte-for-byte identical to what the old, Boolar-only implementation
  produced**, so existing Boolar-IR-consuming tests and code don't
  regress. See the entries below for concrete examples of this pattern.
- If a genuine conflict shows up that can't be resolved by shimming (the
  Bit-only assumption is load-bearing and can't be generalized without
  changing Boolar IR's own semantics), **log it here** with: what broke,
  why it can't be shimmed, and — if any code gets removed rather than
  shimmed — the git commit/blob hash it can be restored from.

---

## Conflict log

### 1. `movfuscate`/`lower_to_circuit`: width-1 assumption in `dispatch_accumulator`-adjacent code

**Symptom:** The existing `lower_to_circuit` (BIrBlocks-only) and its
private `emit_mux`/`emit_or` helpers assume every wire is a single Bit —
correct for Boolar IR, wrong for Volar IR's typed (possibly wide) state
slots.

**Resolution:** Not a shim — a clean parallel addition. Added
`movfuscate_ir`/`lower_to_circuit_ir` as new, separate functions
(`crates/ir/volar-ir-passes/src/movfuscate.rs`,
`crates/ir/volar-ir-passes/src/lower_to_circuit.rs`) reusing the
*already width-generic* free functions in `dispatch_accumulator.rs`
(`emit_select_bit`/`emit_select_slot`, parameterized by
`DispatchBitPrimitives`/`DispatchSlotPrimitives::SlotTy`). No conflict —
`dispatch_accumulator.rs` was already the right level of genericity;
Boolar IR's `lower_to_circuit`/`movfuscate` are untouched.

**Status:** Resolved by addition, no shim needed, no Boolar IR code touched.

### 2. `VoleIrCtx::emit_poly`/`operand_lane`: width-1 assumption in the VOLE weaver's `Poly` handling

**Symptom:** `VoleIrCtx` (in `crates/compiler/volar-weaver/src/vole.rs`) is
the shared context used by *both* the legacy `BIrBlocks`-driven weave path
(`weave_vole_prover_inner`/`weave_vole_verifier_inner`) and the newer
`IRBlocks`-driven path (`weave_vole_prover_ir_with_mode`/
`weave_vole_verifier_ir_with_mode_and_trace`). Its original `emit_poly`
only ever produced a single scalar wire per `Poly` statement — correct for
Boolar IR (`Poly` on a Bit-typed value), silently wrong for Volar IR
(`Poly` on a `_32`/`_64`/`Vec(k,_)`-typed value needs `k` independent
per-lane checks, not one).

**Resolution:** Made `emit_poly` width-aware (`emit_poly_lane` +
`operand_lane`, broadcasting the same monomial structure per bit lane,
reading each operand's *own* width via `WireRepr` rather than assuming the
statement's declared width). Width 1 is the width-1 case of the same
function — this one *did* require touching the function both Boolar IR and
Volar IR call through, so it was validated to produce identical output at
width 1 (existing BIrBlocks weaver tests, all still green) before being
relied on for width > 1.

**Status:** Resolved by making the shared function itself width-generic,
with width 1 verified byte-identical to the pre-change behavior.

### 3. `emit_prover_and_gate`/`emit_verifier_and_gate`: per-bit AND-gate emission doesn't scale to wide values

**Symptom:** Both the BIrBlocks path (`weave_vole_prover_inner`/
`weave_vole_verifier_inner`) and the IR path (`VoleIrCtx::emit_and`) call
the *same* two free functions to emit one Quicksilver AND-check. They take
bare `&str` operand/wire names and always emit exactly one check. For a
wide (`_32`-typed) AND monomial, `emit_poly_lane` was calling these once
per bit lane — correct, but it means a single wide AND site prints as up
to 32 (or 64) separate, nearly-identical Rust statements, each with its own
named `hat_k`/`q_and_k` function parameter. This is the dominant
contributor to the ~7.25M-statement / 46MB-source blowup measured on the
Milestone-1 RISC-V interpreter's one-step circuit (117,199 real AND gates,
mostly from movfuscation's own `is_active · val` accumulation, which is
itself degree-2 — i.e. this is not an edge case, it's the majority of the
circuit's cost).

**Why this one is harder than #2:** unlike a pure width-broadcast, cutting
the per-lane statement count for real requires (a) the per-lane
`hat`/`q_and` witnesses to be **array-indexable** (`hat_bundle[i]`, not `N`
separately-named scalar parameters), which changes what the IR-path's
parameter-list generation emits, and (b) the AND-check body itself to
become a runtime loop (`Array::from_fn`) rather than `N` unrolled
statements. Both of these are safe to do *only* for the IR path's own
parameter/statement generation — the BIrBlocks path's parameter generation
is separate code and is untouched.

**Resolution shipped:** `emit_prover_and_gate`/`emit_verifier_and_gate`
(the Boolar-IR-facing, width-1-only names) are completely untouched — no
shim was even needed in the end, since the new logic lives entirely in a
new, separate function. `VoleIrCtx::emit_poly` (IR path only) now dispatches
to `emit_poly_wide` for `width > 1` circuits whose monomials are all
degree ≤ 2 (checked by `poly_wide_supported`; anything else, e.g. a
degree-≥3 monomial, falls back unchanged to the original per-lane
`emit_poly_unrolled`). `emit_poly_wide` emits **one** `core::array::from_fn`
statement computing every bit lane's full Quicksilver AND/XOR-chain formula
at once — wide operands and every AND-monomial's `hat`/`q_and`/(trace-sink)
`r_and` parameters are bundled into local arrays once each and indexed by
the closure's symbolic lane variable — followed by `width` trivial
one-line extractions, so `WireRepr::Vec`'s contract (independently-named
per-lane wires) is unchanged for every other `Stmt` handler. Handles any
number of AND-monomials per statement (movfuscation's own
`is_active·(a+b) + b` slot-accumulation formula expands to *two*, not
one — an earlier, narrower version of this fix that assumed "at most one
AND per Poly" caught only ~1,228 of ~117,692 real Poly statements and
barely moved the needle; this generalized version is what actually shipped).
Verified via `run_compile_check`-backed weaver tests (`test_wide_and_*`,
118 total in `volar-weaver`, all green) before and after.

**Measured impact — smaller than expected, and here's the real finding:**
on the Milestone-1 RISC-V interpreter's real one-step circuit, woven prover
statement count went 7,249,314 → 5,342,031 (~26% smaller). That's real, but
far short of the "single loop instead of `width`" win this was expected to
deliver, and a follow-up diagnostic (`count_woven_statements_after_optimization`
in `crates/examples/volar-riscv-e2e/src/wat_gen.rs`, `#[ignore]`d, run
manually) explains why: **of 117,692 total `Poly` statements in the
movfuscated circuit, only 1,228 (≈1%) are actually `width > 1`.**
`volar-vaffle-target` bit-decomposes every i32/i64 arithmetic result down
to individual `Bit`-typed SSA values *before* movfuscation ever runs, so
movfuscation's own per-block, per-slot `is_active · val` accumulation is
already operating on ~117K individual **scalar** (width-1) `Poly`
statements — `emit_poly_wide` structurally cannot help there, because
there's no width to collapse. The dominant cost of this circuit is
movfuscation's raw *statement count* (execute-every-block,
accumulate-every-slot, at bit granularity), not per-statement width. This
redirects future optimization work back to the movfuscation-level ideas
(tunnelled/unchanged-state-slot dedup exploiting `Σ_k is_active_k = 1`,
`is_active`-bit-pattern-check dedup across blocks, block-body CSE/
fallthrough merging) rather than further weaver-level codegen changes —
those operate at the level where the actual statement *count* gets
generated, which is where the real leverage is.

**Restoration point:** the pre-this-change implementations of `emit_poly`
and `emit_poly_lane` (i.e. before `emit_poly_wide`/`emit_poly_unrolled`/
`poly_wide_supported` existed) in `crates/compiler/volar-weaver/src/vole.rs`
are at git commit `6df0d11f09143f2700354711670b1ffc99d3034c` (`git show
6df0d11f09143f2700354711670b1ffc99d3034c:crates/compiler/volar-weaver/src/vole.rs`).

**Status:** Resolved (shipped, tested, measured) — but superseded in
priority by the movfuscation-level finding above for anyone picking up
further optimization work here.
