# Direct-to-LIR Weaver Fast Path — Plan

**Status:** Phase 1 complete (2026-08-28); Phase 2 in progress. TS backend
work is explicitly deferred (see Phase 2 note).
**@ai:** assisted
**Base evidence:** local `volar` tree @ `76a670b` + working tree (115 modified
files), local `volar-ir` checkout @ `71597e6` ("execute packed wide values in
wasm"). The `volar-ir.git` family is redirected to that sibling checkout by
`/Users/g/Code-local/portal-hot/.cargo/config.toml` `[patch]` entries, so
"current `volar-ir`" always means that checkout's HEAD.

## Goal

Two ordered outcomes:

1. **Green baseline.** The `volar` codebase compiles and its tests pass against
   the *current* local `volar-ir` checkout.
2. **Direct-to-LIR fast path for weavers.** Woven output (`IrModule` /
   `IrCfgModule`) can be lowered straight to any `LirTarget` (C backend, WASM
   backend, `RecordingTarget`/`SavedLirModule` replay) without ever printing
   Rust text or invoking `rustc` — with consistency tests against the normal
   print-and-compile path on small compiles. This necessarily includes
   **finishing spec-to-LIR compilation**, because every woven module links spec
   functions; if the linked spec cannot lower through LIR, the fast path cannot
   replace the printer.

**Why:** the M1 (real-interpreter / mem-probe) track currently drains woven
circuits through the Rust text printer, and generated Rust source size is the
measured wall — `chunk_size=8` prints a 44.6 MB accumulator-chunk function
that OOMs `rustc` after ~28 min; even the fixed, pooled code at `chunk_size=1`
(3.15 MB, 47 s) is only tractable because of heavy pooling work in
`split_driver.rs`/`movfuscate.rs` (see
`docs/interpreter-honest-e2e-zk-plan.md`). LIR targets emit C99 or WASM whose
consumers (cc, wasmtime) do not exhibit `rustc`'s source-size pathologies, so
routing M1 through LIR removes that entire class of blocker. Spec code size is
the second wall: the spec tree must lower through LIR for the fast path to be
usable at all.

## Current-state evidence (recorded before any repair commit)

`cargo check -p <crate>` over the workspace against `volar-ir@71597e6`:

| Crate | Errors | Nature |
|---|---|---|
| `volar-spec` | 0 | clean |
| `volar-compiler` / `-passes` / `-common` | 0 | clean |
| `volar-lir-codegen` | 0 | clean |
| `ephemera` | 0 | clean |
| `volar-c-backend-spec-tests` | 0 | clean (compile; test run below) |
| `volar-iop` / `volar-fold` / `volar-verifier-iop-runtime` | 0 | clean |
| `xtask` | 0 | clean |
| **`volar-weaver`** | **13** | `BIrStmt` variant drift (see below) |
| **`volar-ir-lir-target`** | **2** | missing trait items (see below) |
| `volar-build` | 2 | **environment**: `llvm-sys 201` finds no system LLVM |
| `volar-riscv-e2e` | (via weaver) | fails only because `volar-weaver` does |
| `volar-fuzz` | 0 | clean |

### `volar-weaver`: `BIrStmt` drift (source failure, fix here)

`volar-ir` @ `d56248a` / `975fc21` reshaped Boolar:

- `OracleBit` is now `{ name, args, bit, occurrence }` (direct oracle-bit
  invocation with stable call-site identity); the old handle-projection form is
  the separate legacy variant `OracleProjectedBit { call, bit }`, kept only for
  decoding old serializations. New lowering never emits it.
- `StorageRead`/`StorageWrite` are now 1-bit-per-cell with `lane: LaneId` and
  **no** `bit_width` field; multi-bit values decompose to one op per bit with
  the value's bit index appended to the address as high-order bits.

Stale pattern matches (11 error sites):

- `crates/compiler/volar-weaver/src/vole.rs:1425`, `vole.rs:1964` —
  `BIrStmt::OracleBit { call, bit }`
- `crates/compiler/volar-weaver/src/fhe.rs:1147`, `:1152` —
  `StorageRead/Write { bit_width, .. }`
- `fhe.rs:2007`, `:2030`, `:2054` — `OracleBit { call, bit }`

Fix strategy: update the matches to the new shapes (oracle handling should
follow `OracleBit`'s `name`/`args`/`occurrence` directly, mirroring what the
weaver's own action/oracle emission in `volar-weaver` already does for
`ActionCall`; `bit_width` loops collapse to the per-bit semantics the variant
now encodes). Do **not** route through `OracleProjectedBit` — it is a legacy
decode-only variant. Preserve provenance threading at each rewritten site
(these sites already carry `P` provenance into emitted statements).

### `volar-ir-lir-target`: trait drift (source failure, fix here)

`impl<P: Clone> LirTarget<P> for VolarIrTarget<P>` is missing `call` and
`switch`, added to the `LirTarget` trait by `volar-ir@ a70c4cd` ("Phase 4a:
expand LirTarget with sibling calls, switch, and dynamic jumps").

Fix strategy: implement both on `VolarIrTarget` in
`crates/ir/volar-ir-lir-target/src/lib.rs` (~line 1211). `call` maps to the
existing inline/extern machinery (`add_extern` / `inline_blocks` path);
`switch` maps to IR jump-table emission where the target supports it, or a
lowered branch cascade consistent with how `VaffleTarget` in `volar-ir`
implements the same methods — read `volar-ir`'s `crates/ir/volar-vaffle-target`
impl first and mirror its semantics rather than inventing a second convention.

### Environment blockers (record separately; not source failures)

- `volar-build` (and anything pulling `inkwell`/`llvm-sys 201`) cannot build
  until system LLVM 20 is installed or `LLVM_SYS_201_PREFIX` is set. This is
  the pre-existing LLVM blocker from
  `docs/handoffs/merge-recovery/index.md`; owner decision on install-vs-CI-only
  is still open. Phase 1 work must not require building `volar-build`'s LLVM
  feature path.
- `cargo fmt --check` cannot parse
  `crates/spec/volar-spec-dyn/src/generated.rs` (generated associated types
  with `<G as _>::Element`). Must be resolved through the generator pipeline
  (`cargo generate-spec`), never hand-edited.

### Known test-side state (not compile blockers)

- `volar-c-backend-spec-tests` `lir_backend.rs` now asserts the *fixed*
  root-selection policy (orphan generic `encrypt_branch` must not be planned
  without a concrete `L`); its own doc says full-spec **body lowering** still
  has gaps tracked by component/vole e2e tests. That is the spec-to-LIR gap
  Phase 2 closes.
- Historical merge-recovery failures (TFHE conformance API drift, fuzz
  provenance panics, VAFFLE target empty-entry panic) were reported fixed on
  2026-07-22; Phase 1 must re-run those suites on today's tree rather than
  trusting the record.

---

## Phase 1 — Green baseline at current `volar-ir`

Smallest scope that makes the workspace compile and its suites pass.

1. **Fix `volar-weaver` `BIrStmt` matches** (5 sites, 13 errors) as above.
   Acceptance: `cargo check -p volar-weaver` clean; `cargo test -p volar-weaver`
   green (123+ tests were green historically; all must pass now, plus the
   linking/TS/C integration tests already in-tree).
2. **Implement `LirTarget::call`/`switch` on `VolarIrTarget`**, mirroring
   `volar-vaffle-target`. Acceptance: `cargo check -p volar-ir-lir-target`
   clean; its focused tests pass; add one compile-and-run test per new trait
   item (small WASM function with a call and a switch, executed through the
   pipeline) — structural IR asserts alone are not acceptance evidence.
3. **Re-run the standing suites** and record results in `PROGRESS.md`:
   `cargo test -p volar-spec`, `-p volar-vaffle-target`, `-p volar-ir-passes`,
   `-p volar-ir-opt -p volar-ir-virt -p volar-fuzz`, `-p volar-weaver`,
   `-p volar-lir-codegen`, `-p volar-c-backend-spec-tests`, and
   `cargo check --workspace` (excluding/allowing-fail the llvm-sys path until
   the environment decision lands).
4. **Do not** widen scope into the interpreter plan's open items (CSE/batch
   split-weave boundary, movfuscation fall-through, etc.). Those are separate
   lines with their own handoffs; this phase is a baseline, not a reconciliation
   of them.

Exit criteria: every non-LLVM-dependent crate compiles and its tests pass on
the current `volar-ir` checkout; both environment blockers recorded with exact
commands in `PROGRESS.md`.

> **Phase 1 complete (2026-08-28).** Evidence in `PROGRESS.md` top section.
> The LLVM blocker resolved differently than anticipated: the workspace was
> bumped to inkwell 0.10 / `llvm22-1` (`0594043`), matching the installed
> Homebrew LLVM 22 — no llvm-sys 20 environment needed. Standing-suite
> results recorded; the `lir_backend_components` monomorphization failures
> feed directly into Phase 2's inventory.

## Phase 2 — Finish spec-to-LIR compilation

> **Scope note (2026-08-28):** TypeScript backend work is deferred. The TS
> printer's generated output carries ~178 pre-existing strict-mode errors
> (verified identical under tsc 5.9 and 7.0 — not a TS7 regression); the
> `test_ts_backend_no_errors` harness now supports TS7 via `--ignoreConfig`
> probing (`0f7a324`). Phase 4 dual-path coverage should treat the LIR→WASM
> backend as the second target, not the TS printer.

The fast path is only real if linked spec functions lower through LIR. Today
the LIR path covers the VOLE-relevant spec slice (rooted instances of
`vole_prove_*` / `vole_verify_*` etc. pass in `vole_e2e.rs`) but *body lowering
for the whole spec tree* has known gaps (`lir_backend.rs` header), and TFHE
spec functions (`encrypt_branch::<R, L>`) are the named hard case.

1. **Inventory the gaps.** For each spec module (`vole*`, `ot/*`, `faest/*`,
   `garble.rs`, `tfhe.rs`, `curve.rs`), attempt `plan_flat_module` +
   `lower_module_monomorphized` to C with the root policy of
   `lir_backend.rs`; record every failure as a concrete reproducer (function,
   IR node, error). Output: a table in this document or a sibling handoff —
   no fix without a named lowering gap.
2. **Fix gaps in `volar-lir-codegen`**, ordered by the M1 critical path first
   (whatever the mem-probe / split-weave driver and its linked helpers
   actually call). Each fix lands with a real compile-and-run regression in
   `volar-c-backend-spec-tests` (C compiled with cc and executed), per the
   generated-code testing rule — never a structural-only assert. Keep the
   invariants from the static-shapes handoff: no unresolved `TypeParam`,
   type-parameter array length, or projection may reach lowering; never pick a
   test-only default for an unresolved const parameter; never silently select a
   specialization.
3. **Keep the root discipline.** Generic functions lower only under
   instance-discovered, concrete bindings (the `MonoPlan` root policy). The
   `encrypt_branch` unbound-`L` regression stays closed: orphan generic roots
   are rejected, not defaulted.
4. **Widen to CFG.** `lower_cfg_module_monomorphized` must handle the same
   spec slice (CFG auxiliary functions share shadowing/witness patterns; see
   `docs/pipeline.md`). Same acceptance pattern: compile-and-run, not shape.

Exit criteria: every spec function reachable from a woven module lowers to C
and runs; the inventory table has no "unfixed" rows on the M1 path.

## Phase 3 — Direct-to-LIR fast path for weavers

New public API (crate: `volar-weaver` re-export or a thin `volar-lir-codegen`
entry — decide by dependency direction, `volar-weaver` already depends on
`volar-lir-codegen`):

```text
weave_vole_prover_ir_split(...) -> IrModule
    └─ lower_module_monomorphized(&module, target, MonoPlanOptions { roots: woven roots + linked spec roots, .. })
    └─ <any LirTarget>: CBackend, WasmBackend, RecordingTarget/SavedLirModule
```

1. **API surface, flat path first.** A helper that takes a woven `IrModule`
   plus its `MonoEnv`/`MonoPlanOptions` and lowers to an arbitrary
   `LirTarget`, using the same root-selection policy as `vole_e2e.rs`
   (`roots_by_name_prefix` over woven entries + explicitly env-bound spec
   helpers). No printer involvement anywhere in the path.
2. **CFG path.** Same for split-weave/CFG output via
   `lower_cfg_module_monomorphized` (the split weaver's runtime-loop driver
   companions are CFG-shaped; `split_driver.rs` already builds a real
   `Vec<IrStmt>` AST with `generate_split_step_ir`, so its driver functions are
   LIR-lowerable *in principle* once its remaining leaf text is structured —
   that structuring is in scope only when the M1 driver needs it).
3. **Handle the extern surface.** Woven modules reference spec ops, oracles,
   actions, and `TypeStub`s. LIR codegen already registers `TypeStub` for
   return-type inference and excludes it from `external_fns`; verify the same
   handling for `ExternalKind::Action` / oracle bits under the *new*
   `OracleBit` shape from Phase 1 (LIR `call_extern` with per-bit projection).
4. **Record/replay as the default consumer.** Weave once → `RecordingTarget`
   → `SavedLirModule` → `replay_pair`/`replay_into_many` into C and WASM.
   This is the cheapest way to get all backends from one weave and matches the
   existing `vole_and_record_replay_c_and_wasm` precedent.
5. **Start Unpinned / Very unstable.** This is new non-cryptographic
   infrastructure: mark the new module
   `// @pinnedness: unpinned` / `// @stability: very-unstable`, `//! @ai:`
   per policy. No stability promotion without evidence and a human decision.

Exit criteria: a small woven circuit (AND/XOR) goes weaver → LIR → C and
weaver → LIR → WASM and executes correctly, with zero calls into the Rust
printer in the path.

## Phase 4 — Consistency tests vs the normal path (small compiles)

For every fast-path consumer, the semantics contract is: **the LIR path and
the normal print-and-`rustc` path agree on observable behavior.**

1. **Dual-path harness** in `volar-c-backend-spec-tests` (or `volar-weaver`
   tests_common): given a small circuit builder fn, produce
   - path A: woven `IrModule` → `printer.rs` → generated Rust →
     `run_compile_check` + execute (existing helpers), and
   - path B: the same woven `IrModule` → LIR → C (`cc -O0`) → execute, and
     optionally → WASM (`wasmtime`),
   then assert **identical observable outputs** on a fixed input truth table
   (honest prover/verifier accept/reject, garbler/evaluator table agreement,
   or plain function outputs). Only small compiles use path A — it exists as
   the oracle, not the product.
2. **Coverage matrix (grow over time, start with):** VOLE AND, VOLE XOR,
   half-adder, the mem-probe circuit, one garble pair, one FHE AND (noiseless).
   Each new case is one test fn wired into the harness.
3. **Cross-backend consistency:** path B runs the *same* `SavedLirModule`
   replayed into C and WASM and asserts equal outputs — catching backend drift
   without re-weaving.
4. **Failure discipline:** a mismatch is a bug in exactly one of
   {weaver output, LIR codegen, backend emitter, test harness}. Bisect by
   lowering the same module to `volar-lir-text` and comparing statements;
   never "fix" a mismatch by changing the test's expected outputs.
5. **Keep the ZK/non-ZK discipline intact.** The fast path moves `IrModule`
   values, not proof artifacts; nothing here seals a prover as `Transparent`
   or relaxes `NonZk` bounds. See `docs/agent-context/discipline.md` before
   touching anything that carries a `Tagged<Z, _>`.

Exit criteria: the harness is merged with ≥5 circuits green on both paths, and
CI-runnable without the LLVM feature path.

## Phase 5 — M1 enablement

With Phases 1–4 done, the M1 track (mem-probe → real-interpreter split weave)
stops printing Rust for circuit bodies:

1. Route the split-weave woven roles (prover/QSim/verifier) through the fast
   path; the runtime-loop driver (`generate_split_step_ir`) follows once its
   remaining leaf expressions are structured (its own doc names this as the
   natural next increment).
2. Measure: largest generated function source bytes (C) and wall-clock compile
   vs the Rust-printer baseline table in
   `docs/interpreter-honest-e2e-zk-plan.md`. Record in `PROGRESS.md`.
3. Only then re-evaluate `chunk_size`: the pooling machinery's complexity
   budget may simplify if LIR consumers tolerate the sizes that forced
   `chunk_size=1`.

## Documents to update while executing

- `PROGRESS.md` — after each phase, with exact commands and results.
- `docs/pipeline.md` — once the fast path exists (it changes the
  "woven output can be printed" story to "woven output lowers to LirTarget
  directly"; the doc already claims this generally, so verify rather than
  rewrite).
- `docs/handoffs/merge-recovery/static-shapes-and-monomorphization.md` —
  Phase 2 gap fixes and their completion evidence.
- `docs/README.md` index — add this plan when Phase 1 lands.
- This document — mark phases complete with evidence dates, per the
  agents-guide rule that plans must not disagree with current source.

## Risks

| Risk | Mitigation |
|---|---|
| `BIrStmt` fixes in `vole.rs`/`fhe.rs` change woven semantics silently | Phase 4 dual-path tests catch this; run existing weaver truth-table tests in Phase 1 before/after |
| Spec-to-LIR gaps balloon (FAEST/AES is large) | Fix strictly in M1-path order; the rest is a tracked inventory, not a blocker |
| `LirTarget` `call`/`switch` semantics diverge between `VolarIrTarget` and `VaffleTarget` | Mirror the `volar-ir` implementation; add one shared compile-and-run test in `volar-ir`'s own suite if divergence is plausible |
| Fast path drifts from printer path over time | Phase 4 harness stays in CI; any printer-visible weaver change must keep the dual-path green |
| LLVM environment stays unresolved | Nothing in Phases 1–4 requires `volar-build`/`inkwell`; WASM backend path uses `wasmtime` + `volar-wasm-backend`, both LLVM-free |
