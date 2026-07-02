# Prove-the-Verifier Folding

> **Reliability:** the folding core (`volar-fold`) is `@reliability: experimental`.
> The binary-field ↔ prime-field embedding of the verifier check is a
> **Tier 3** cryptographic seam (see [§ Honest scope](#honest-scope)).
> Agents: read [`agent-context/discipline.md`](agent-context/discipline.md) before
> touching anything on the ZK ↔ non-ZK boundary.

This document describes the **prove-the-verifier** flow: after the pre-ZK passes
and the ZK weave, the VOLE **verifier** is itself folded into a single
relaxed-R1CS instance and checked natively — reusing the Nova-style machinery in
[`volar-fold`](../crates/fold/volar-fold/) without a final zkSNARK.

It complements [`vole-weaving.md`](vole-weaving.md) (how the verifier is
generated), [`vcb-ivc-folding.md`](vcb-ivc-folding.md) (the gap-folding scheme it
reuses), and [`pipeline.md`](pipeline.md) (where it sits in the build).

---

## 1. Why prove the verifier

The standard pipeline produces, from one boolean circuit, a VOLE **prover** and a
VOLE **verifier** (`weave_vole_prover_ir` / `weave_vole_verifier_ir`,
[`vole.rs`](../crates/compiler/volar-weaver/src/vole.rs)). The verifier's job is a
stream of per-AND-gate checks plus a memory consistency check:

```text
    K_a · K_b + V̂ = K_c · Δ           (one per AND gate)
    multiset_hash(reads) = multiset_hash(writes)   (memory, StorageMode::Commitment)
```

We want to **prove that the verifier accepted** — i.e. produce a succinct,
re-checkable artifact attesting "I ran the verifier on this proof and every gate
check held." This is recursion: the verifier becomes a computation we prove.

**The key observation — we can drop the zkSNARK.** The *inner* VOLE proof already
accounts for zero-knowledge of the witness. The verifier only ever touches
**public / committed** data: gate MACs `K_*`, the verifier-only secret Δ, the
prover-sent openings `V̂`, and the memory hash. So the *outer* proof of the
verifier's execution does **not** need to be zero-knowledge. That is exactly why
[`volar-fold`](../crates/fold/volar-fold/src/lib.rs) — "Nova **minus** the
zkSNARK", whose folded instance is checked natively/interactively — is the right
tool: there is no secret left to hide, only soundness to preserve.

A *regular* (non-ZK) SNARK may later compress the folded instance; that is the
typed future seam [`compress_with_snark`](#5-api).

---

## 2. The per-step relation as R1CS

Each AND-gate check is one folding step. [`and_check_r1cs`](../crates/fold/volar-fold/src/verifier.rs)
encodes `K_a · K_b + V̂ = K_c · Δ` over the folding scalar field as three
constraints with witness layout `W = [K_a, K_b, K_c, Δ, V̂, P₁, P₂]`
(`u = z[7]` is the relaxation/constant column):

| # | constraint | meaning |
|---|---|---|
| 1 | `K_a · K_b = P₁` | left product |
| 2 | `K_c · Δ   = P₂` | right product |
| 3 | `(P₁ + V̂ − P₂) · u = 0` | the check (`u = 1` in a fresh instance) |

A satisfying assignment exists **iff** the gate check holds, so a satisfying
opening of the folded instance implies every folded gate held (Nova's folding
theorem; see [`verify.rs`](../crates/fold/volar-fold/src/verify.rs)).

---

## 3. Folding the whole verifier

[`prove_verifier`](../crates/fold/volar-fold/src/verifier.rs) drives **all** gate
steps through the same IVC machinery that [`ivc::prove_gap`](../crates/fold/volar-fold/src/ivc.rs)
uses for a network gap — there is no distinction; the whole verifier *is* the
"gap":

1. Each [`VerifierStep`] (`K_a, K_b, K_c, Δ, V̂` + folding challenges) becomes a
   fresh R1CS instance via `nifs::fresh`.
2. Instances are folded pairwise with `nifs::prove_fold` into one
   `RelaxedInstance` of size `O(|F|)` — **independent of the gate count**.
3. The **memory accumulator** (`mem_acc_in` / `mem_acc_out`, the multiset-hash
   state under `StorageMode::Commitment`) is the boundary state, Pedersen-committed
   as `c_in` / `c_out`. This is the *memory-commitment reuse*: the same boundary
   machinery the continuation bridge uses to link VOLE state across a gap links
   the verifier's start/end memory state here.

The result is a [`VerifierFold`] = one relaxed instance + opened witness +
boundary commitments.

---

## 4. Native verification (no zkSNARK)

[`verify_folded`](../crates/fold/volar-fold/src/verifier.rs) calls
[`native_verify`](../crates/fold/volar-fold/src/verify.rs): open `(W, E)`, check
the Pedersen commitments open, and check the relaxed relation
`(A z) ∘ (B z) = u·(C z) + E`. Sound, `O(|F|)`, non-succinct in proof size — the
accepted trade-off (cheap online bandwidth) inherited from the continuation
bridge.

---

## 5. API

In [`volar-fold/src/verifier.rs`](../crates/fold/volar-fold/src/verifier.rs):

| Item | Role |
|---|---|
| `and_check_r1cs() -> R1CS` | the per-gate verifier-check relation |
| `VerifierStep` | one gate's verifier-side wires (`K_a,K_b,K_c,Δ,V̂`) + challenges |
| `VerifierTrace` | the whole-verifier step list + `mem_acc` boundary |
| `prove_verifier<Z: NonZk>(Tagged<Z, VerifierTrace>, &PedersenParams) -> Tagged<Transparent, VerifierFold>` | fold the whole verifier |
| `verify_folded(&Tagged<Transparent, VerifierFold>, &PedersenParams) -> bool` | native check |
| `compress_with_snark<Z: NonZk>(…)` | **future** regular-SNARK compression seam |

### Discipline safety (compile-time)

`prove_verifier` and `compress_with_snark` are bound `where Z: NonZk`
(implemented only for `Transparent`). A `Tagged<Zk, …>` artifact — anything from
`weave_vole_prover*` — therefore **cannot** be folded here: it is a compile
error, not a runtime check. The verifier weavers return `Tagged<Transparent, …>`
precisely so they flow into this path and the prover cannot. See
[`agent-context/discipline.md`](agent-context/discipline.md). A `compile_fail`
doctest in `verifier.rs` pins this guarantee.

---

## 6. Build wiring

Compile-time and runtime concerns live in **separate crates**, split along the
same line as the rest of this plan: `volar-verifier-fold` only ever produces
*text* (C or Rust source); `volar-verifier-runtime` is the only crate that
*executes* anything.

**Compile-time — [`volar-verifier-fold`](../crates/fold/volar-verifier-fold/)**
(depends on `volar-fold`, `volar-compiler`, `volar-lir-codegen`, `volar-c-backend`,
`volar-weaver` — no `volar-verifier-runtime`):

| Item | Role |
|---|---|
| `emit_verifier_c(&Tagged<Transparent, IrModule>, &MonoEnv) -> String` | lower the woven verifier to **C** via `CBackend` (unaffected by the `u128` gap, see [`agent-context/lir-u128-support.md`](agent-context/lir-u128-support.md), as long as the module doesn't touch curve/`u128` spec functions) |
| `emit_verifier_rust(&Tagged<Transparent, IrModule>) -> String` | lower to **Rust source** via `volar_weaver::vole::print_weaved_vole_module` — the terminal for a `NovaFoldSink`-woven verifier (§7), whose `FoldScalar`/`FoldAccumulator`/etc. names aren't C/LIR-compatible today |

**Run-time — [`volar-verifier-runtime`](../crates/fold/volar-verifier-runtime/)**
(depends only on `volar-fold`, `volar-spec`, `volar-discipline`, `std` — never
`volar-compiler`/`volar-lir-codegen`/`volar-c-backend`/`volar-weaver`; it only ever
consumes already-generated source **text**):

| Item | Role |
|---|---|
| `GateObservation` + `verifier_trace(…)` + `prove_and_verify_folded<Z: NonZk>(…)` | the original **batch** path — build a `VerifierTrace` from a fully-materialized `&[GateObservation]` slice and fold it via `volar_fold::verifier`'s general machinery. Still useful for hand-constructed traces/tests; superseded as the production path by §7 |
| `FoldScalar`, `FoldLift`, `FoldAccumulator`, `fold_accumulator_fresh`, `fold_and_gate` | the concrete definitions a `NovaFoldSink`-woven verifier links against (§7) |
| `run_folded_verifier(rust_source, driver_src) -> String` | compile + link + **run for real** (`cargo`/`rustc`, same "print → temp Cargo project → real backend" pattern `AGENTS.md` rule 2 mandates) — the terminal that actually executes the Rust leg |

See [`pipeline.md`](pipeline.md) for where both fit in the overall build.

---

## 7. Dynamic (weave-time) trace assembly — `NovaFoldSink`

The batch path above (§6, `GateObservation`/`verifier_trace`) requires
materializing the *whole* per-gate trace before folding — `O(loop length)`
memory, and it can't hide the loop length, defeating one motivation for
prove-the-verifier at all (the VOLE weaver supports looped/resumable circuits,
`hybrid_net.rs`/`storage_loop.rs`, with potentially hidden iteration counts).

[`VerifierTraceSink`](../crates/compiler/volar-weaver/src/vole.rs) (a weave-time
extension point on `weave_vole_verifier_with_trace`) and its concrete
implementation `NovaFoldSink` close this: they thread a **typed Nova
relaxed-witness accumulator** (`w`, `e`, `u` — no commitments; those are computed
once, outside the loop, from the small fixed-size final witness, since Nova
folding keeps the witness the same fixed shape across folds regardless of gate
count) through the woven verifier, updated via a real `fold_and_gate` call once
per AND gate, in step with each gate's own check — genuinely `O(1)` state
regardless of how many gates run.

**Real typed IR, not a string hook:** `and_gate_step` emits actual `IrExpr`/
`IrStmt` nodes referencing the gate's real variables (`k_a`, `k_b`, `k_c`,
`delta`, `hat`) — `AGENTS.md` rule 1 (never raw strings as expression data).

**Genuinely compiled and executed, not logged:** the fold math
(`volar_spec::fold::gate_witness`/`cross_term`/`fold_witness`/`fold_u`) runs as
part of the same compiled binary the verifier itself runs in — via
`volar-verifier-runtime`'s `fold_and_gate`, executed by real `rustc`
(§6) — not printed to a log for a separate process to reinterpret later.

**The lift (`FoldLift`) is the one deliberately open seam.** `FoldScalar`,
`FoldAccumulator`, `fold_accumulator_fresh`, and `fold_and_gate` are bare,
*externally-resolved* identifiers in the woven IR — not declared as generic
parameters of the woven function, so ordinary Rust name resolution requires
whoever compiles the output (`volar-verifier-runtime`, today) to supply concrete
definitions. See [`agent-context/gf2k-to-fell-embedding.md`](agent-context/gf2k-to-fell-embedding.md)
for what that means and why it's still open — `tests/e2e_fold_verifier.rs` in
`volar-verifier-fold` runs the whole thing for real and pins the current,
known-unsound state of the default lift as a failing assertion, on purpose.

---

## Honest scope

- **The trace-emission hook is closed** for the dynamic path (§7): the woven
  verifier itself produces the fold state as it runs, compiled and executed for
  real (`tests/e2e_fold_verifier.rs`) — no separate capture step needed. The
  batch path (§6) still requires externally-captured `GateObservation`s if used.
- **The GF(2^k) ↔ F_ℓ embedding is still open** — now with **concrete, empirical
  evidence** it's unsound as currently implemented (not just a theoretical gap):
  `tests/e2e_fold_verifier.rs` runs a real GF(2^8) VOLE proof through a woven,
  compiled, `NovaFoldSink` verifier — the real Quicksilver check passes, but the
  naively-embedded F_ℓ witness does **not** satisfy `and_check_r1cs`'s relation
  (`FoldLift for Galois`'s reinterpret-the-byte embedding doesn't preserve
  GF(2^8)'s actual polynomial multiplication). Tracked in
  [`agent-context/gf2k-to-fell-embedding.md`](agent-context/gf2k-to-fell-embedding.md) —
  **Tier 3**, needs cryptographic review before any concrete lift is treated as
  more than a structural placeholder. This is the same boundary the batch path's
  `and_check_r1cs` R1CS always modeled over `F_ℓ` rather than the real `GF(2^k)`
  check; the dynamic path just makes the gap runnable and measurable instead of
  implicit.
