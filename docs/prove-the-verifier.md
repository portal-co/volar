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

The arithmetization frontend and the pipeline terminal live in
[`volar-verifier-fold`](../crates/fold/volar-verifier-fold/) — the build-side
counterpart to `volar_fold::verifier`:

| Item | Role |
|---|---|
| `GateObservation` + `verifier_trace(…)` | **frontend** — assemble a `Tagged<Transparent, VerifierTrace>` from a verifier's per-gate observations + memory boundary |
| `prove_and_verify_folded<Z: NonZk>(…)` | **terminal (fold leg)** — fold the whole verifier and check it natively |
| `emit_verifier_c(&Tagged<Transparent, IrModule>, &MonoEnv) -> String` | **terminal (C leg)** — lower the woven verifier to C via `CBackend` |

The terminal deliberately lowers through the **C backend**
([`volar-c-backend`](../crates/compiler/volar-c-backend/), `CBackend`) rather than
LLVM: it is the executable substrate available everywhere (the LLVM backend in
[`volar-build`](../crates/compiler/volar-build/) is environment-gated). The C
verifier is what produces the concrete `GateObservation`s that feed the frontend.
`volar-verifier-fold` depends only on `volar-fold`, `volar-compiler`,
`volar-lir-codegen`, and `volar-c-backend`, so it builds and tests without LLVM.
See [`pipeline.md`](pipeline.md).

---

## Honest scope

- The R1CS in `and_check_r1cs` models the gate check over the **folding scalar
  field** `F_ℓ`. The real verifier check is over the binary extension field
  `GF(2^k)`. Faithfully folding it requires the **binary-field ↔ prime-field
  embedding** — the same swappable boundary-link component discussed in
  [`boundary-link-embedding.md`](boundary-link-embedding.md) and implemented for
  the memory hash by [`keccak_r1cs`](../crates/fold/volar-fold/src/keccak_r1cs.rs).
  Wiring that embedding into the verifier-step witness is **Tier 3** and is a
  documented seam, not yet closed.
- `VerifierTrace` carries concrete per-gate scalar values. The frontend extracts
  the gate **structure** from the woven verifier IR; the concrete values come
  from executing the verifier (the C-backend path) on a specific proof. The
  trace-emission hook is the build-side counterpart of the seam above.
