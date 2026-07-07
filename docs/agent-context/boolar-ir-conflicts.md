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

### 4. Frontend bit-decomposition: keeping i32/i64 "wide at rest" across block boundaries

**Symptom:** Per conflict #3's finding, `volar-vaffle-target` bit-decomposed
every i32/i64 value down to individual `Bit`-typed SSA values *before*
movfuscation ever ran, so movfuscation's own per-block, per-slot
`is_active · val` accumulation was operating on ~117K individual scalar
(width-1) `Poly` statements — one movfuscation state slot *per bit*, not
per logical value. This is a Boolar-IR-shaped assumption again: Boolar IR
is Bit-only by construction, and `VaffleValue { bits: Vec<ValueId>, ty }`
(one `ValueId` per bit, always) mirrors that, even though nothing about
VAFFLE's own representation or the Volar-IR movfuscation path requires it.

**Resolution:** Not a shim — a narrow, surgical fix at exactly the two
places that determine movfuscation's state-slot *count*: block-parameter
declaration and branch-argument passing (`crates/ir/volar-vaffle-target/src/target.rs`).
`add_block_param` now declares **one** packed (`Vec(n, Bit)`, or bare `Bit`
if `n <= 1`) VAFFLE block param instead of `n` separate `Bit` params, then
immediately unpacks it via `n` calls to the pre-existing, already-tested
`StorageEmitter::extract_bit` — so `VaffleValue.bits` still presents `n`
individually-addressable bit `ValueId`s to every existing caller, unchanged.
`jump`/`branch` symmetrically pack each argument via `StorageEmitter::compose_address`
before crossing the block boundary (with `.filter(|v| !v.bits.is_empty())`
preserved so an empty-bits value still contributes zero positional args).
Every WASM operator's own internal logic (`add`/`and`/`shl`/... via
`BitCircuitBuilder`) is completely untouched — it still operates bit-by-bit
inside a block; only the *between-blocks* representation changed.

**A second, pre-existing bug this exposed:** `lower_to_ir.rs`'s
`lower_function` (VAFFLE → Volar IR) pushed non-entry blocks' param types
by copying the raw **VAFFLE** `TypeId` directly into the IR block's param
list, instead of remapping it through `self.type_map` (the VAFFLE-TypeId →
IR-TypeId table built by `remap_type_id`). This was invisible before this
fix because every VAFFLE block param was `Bit`-typed, and `Bit` happens to
be reserved as id 0 in *both* type tables — so the unmapped raw id was
coincidentally correct. Once block params became genuinely wide
(`Vec(32, Bit)`), the raw VAFFLE id no longer matched the IR id at the same
position (e.g. it collided with `ADDR_TID`, the IR-level `Vec(16, Bit)`
stack-address type reserved at IR `TypeId(1)`), causing
`index out of bounds` panics deep in the weaver's `emit_shuffle` (a
downstream symptom of an upstream type-table mismatch, not a weaver bug).
Fixed with a one-line remap: `params.push(self.type_map[ty_id.0 as usize])`.
This is a genuine, unrelated latent bug fix, not a Boolar-IR shim — logged
here because it was found and fixed in the same pass as this frontend
change and is easy to otherwise lose track of.

**Measured impact — the width-at-rest fix alone was decisive.** On the
same Milestone-1 RISC-V interpreter one-step circuit used throughout this
log:
- Movfuscated `Poly` statement count: 117,692 → 20,182 (an ~83% drop in
  raw statement count, since state is now packed per logical value instead
  of exploded per bit).
- Of those, the *wide* (`width > 1`) fraction jumped from ~1% (1,228) to
  ~22% (4,375) — `emit_poly_wide` (conflict #3) now actually applies to a
  meaningful share of the circuit instead of a rounding error.
- Woven prover statement count: 5,342,031 → **3,272,222** (~39% further
  reduction on top of conflict #3's `emit_poly_wide` win, ~55% total
  reduction from the original, pre-any-optimization baseline of 7,249,314).

Per the user's own stated conditional ("unless the decreased state width
alone is that big of a win"), this qualifies — a ~55% total reduction from
one narrowly-scoped fix (two functions in `target.rs`, one one-line fix in
`lower_to_ir.rs`) is enough to defer the remaining planned optimizations
(bitwise-op-level widening, movfuscation-level tunnelled-state/`is_active`
dedup, polynomial merging) rather than implement them speculatively before
re-measuring against real further use of this pipeline (Milestone 2's
RV32I expansion).

**Status:** Resolved (shipped, tested, measured). All of
`volar-vaffle-target`'s and `volar-riscv-e2e`'s test suites pass with this
change (see below for the one known-unrelated pre-existing failure).

**Unrelated pre-existing failure, not caused by this change:**
`volar-vaffle-target::lower_to_ir::tests::test_pack_unpack_stmts_present`
fails both with and without this session's changes (verified by reverting
`target.rs` to its pre-session state via `git show HEAD:...` and re-running
the test in isolation — same failure both ways), with
`"emit_entry_and_exit: entry function (func 0) must be a Body with at least
one value to seed provenance from"`. This is in the SP-threading/spill-reload
trampoline mechanism, unrelated on the surface to i32/i64 width handling.
Not fixed here — out of scope for this change, left as a known gap for
whoever next touches `emit_entry_and_exit`.
