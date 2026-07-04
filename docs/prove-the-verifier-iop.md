# Prove-the-Verifier: IOP-based (Merkle + Fiat–Shamir) Backend

> **Reliability:** the whole construction (`volar-iop`) is
> `@reliability: experimental` — **Tier 3**, needs cryptographic review
> before production trust, same posture as its sibling `volar-fold`. Agents:
> read [`agent-context/discipline.md`](agent-context/discipline.md) before
> touching anything on the ZK ↔ non-ZK boundary.

This document describes a **second backend** for the same problem
[`prove-the-verifier.md`](prove-the-verifier.md) solves: succinctly attest
that the VOLE verifier's per-gate checks all held, without a final zkSNARK
on the *inner* proof (the same "no ZK needed here" argument applies
identically — see that doc's §1). Both backends are real and coexist; they
share the same target relation and the same weave-time plugin point
(`VerifierTraceSink`), and differ only in how the whole-verifier reduction
is finalized:

| | [`prove-the-verifier.md`](prove-the-verifier.md) (Nova) | This document (IOP) |
|---|---|---|
| Per-gate fold | Relaxed-R1CS, `F_ℓ` (Ed25519 scalar field) | Relaxed-R1CS, native `GF(2^k)` tower field |
| Cross-field cast | `GF(2^k)→F_ℓ`, needs `and_check_gf2k`'s 174-constraint bit-expansion (Tier-3 open seam, [`gf2k-to-fell-embedding.md`](agent-context/gf2k-to-fell-embedding.md)) | None — `and_check_r1cs`'s native 3-constraint/7-slot shape used directly |
| Finalization | `native_verify`: open `(W,E)` in the clear, check natively (`O(\|F\|)`, not succinct) | A small Merkle+Fiat–Shamir IOP proof over the same small `(W,E,u)` — this is `compress_with_snark`'s stubbed role, implemented for real |
| Weaver sink | `NovaFoldSink` | `IopSink` |
| Runtime crate | `volar-verifier-runtime` | `volar-verifier-iop-runtime` |

---

## 1. Why a second backend

Nova folding already makes the accumulator's *size* independent of gate
count (`docs/vcb-ivc-folding.md` §1). What it doesn't give you is a
genuinely succinct, self-contained *proof artifact* — `native_verify` opens
the small `(W, E)` in the clear, so the "proof" is exactly as large as that
opening, and checking it means literally recomputing the relaxed relation.
This document's contribution is a real Merkle+Fiat–Shamir argument
(a "traditional IOP-based zkSNARK", in the sense of Ligero/Aurora/STARKs)
that attests to the *same* small accumulator without opening it via a plain
native check — while also sidestepping the `GF(2^k)→F_ℓ` embedding
entirely, by never leaving the VOLE's own field.

**This is not a replacement.** Both backends are exposed side by side; pick
whichever a given deployment wants (interactive/native-check-cheap vs.
Merkle-committed/self-contained-proof).

---

## 2. Two phases

### 2.1 Phase 1 — the per-gate fold (`volar_iop::fold`, O(1) memory)

Structurally identical to `NovaFoldSink`/`fold_and_gate` — a fixed-size
accumulator `(W: 7 slots, E: 3 slots, u: 1 scalar)` threaded through the
woven verifier's loop, updated once per AND gate via the same relaxed-R1CS
cross-term algebra Nova uses (`crates/fold/volar-fold/src/nifs.rs`'s shape,
reimplemented in `crates/iop/volar-iop/src/fold.rs` — deliberately a
parallel, not shared, implementation, so `volar-fold`'s own `Scalar`/
`EdPoint`/Pedersen types never need genericizing).

**The concrete payoff of staying native**: the relation folded each round is
`and_check_r1cs`'s original 3-constraint/7-slot shape
(`crates/fold/volar-fold/src/verifier.rs:70-101`), reused *verbatim in
shape*, just re-typed over the tower field. `and_check_gf2k`'s 174-constraint
bit-expansion (`crates/fold/volar-fold/src/gf2k.rs`) — which exists
specifically to make the check sound over a *different* field — simply
isn't needed. `crates/fold/volar-verifier-fold/tests/e2e_iop_verifier.rs`
asserts this directly: the IOP path's single-gate witness has 7/3 slots,
against the Nova path's 165/174 (`e2e_fold_verifier.rs`).

**Field**: `crates/iop/volar-iop/src/field.rs` builds a binary tower field
`GF(2^8) → GF(2^16) → GF(2^32) → GF(2^64) → GF(2^128)` by repeated
quadratic extension (`F_{i+1} = F_i[x]/(x^2+x+β_i)`) on top of the VOLE's
own `GF(2^8)` (`volar_primitives::Galois`) — chosen over a single
degree-`m` extension or a binary-tower/additive-NTT (classic FRI)
construction specifically because a *quadratic* extension in characteristic
2 has a simple, textbook irreducibility criterion (trace = 1), avoiding both
an arbitrary-degree irreducibility test and additive-NTT machinery. See that
file's module doc for the full derivation and the three independent
correctness checks (multiplicative-group order, distributivity, defining
relation) run across all five tower levels.

**Per-gate challenges**: same honest posture as the existing Nova path —
see § Honest scope below. `crates/compiler/volar-weaver/src/vole.rs`'s
`IopSink` threads the per-gate challenge `r_and_{k}: IopChallenge` the same
mechanical way `NovaFoldSink` does.

**`IopLift`, not `FoldLift`**: `T` (the VOLE's own field, e.g. `Galois`) is
still a different Rust type from the fold's tower field (`Gf128`), so a
lift is still needed — but it's a **canonical, characteristic-preserving
ring embedding** (`T` sits at the tower's base level, zero above), sound by
construction, unlike `FoldLift`'s old one-scalar reinterpret-cast (fixed by
bit-expansion) into an *unrelated* field. See
`crates/iop/volar-verifier-iop-runtime/src/lib.rs`'s module doc for the
full comparison.

### 2.2 Phase 2 — finalization (`volar_iop::ligero`)

Once Phase 1 produces the final, fixed-size `(W, E, u)` (11 field elements
total), Phase 2 proves it satisfies `and_check_r1cs`'s relaxed relation via
a one-shot Merkle+Fiat–Shamir argument:

1. **Reveal, don't hide.** Classic Ligero hides the witness and proves a
   *quadratic* constraint about hidden values using few revealed positions.
   This proof doesn't need to hide anything — same "no secret left to hide"
   argument as the rest of prove-the-verifier — so it reveals `(W,E,u)`
   directly and checks the relation on the revealed values. The genuinely
   new IOP part is a standard **Reed–Solomon proximity test**: encode
   `(W,E,u)` (`K=11` elements) systematically into a length-`32` codeword,
   Merkle-commit it, Fiat–Shamir-sample `Q=21` query positions, and check
   the opened positions are consistent with *some* degree-`<K` polynomial
   (interpolate from `K` of the queries, cross-check the rest) before
   trusting the recovered values.
2. This is the standard "encode + spot-check" IOP of proximity underlying
   Ligero/Aurora/FRI, simplified to a single round because the message
   never needs BaseFold/FRI-style recursive folding-in-half — that
   machinery only pays for itself on a *growing* object, and Phase 1
   already prevents the object from growing.
3. Merkle leaf/node hashing is domain-separated (SHA3-256, one-byte
   prefix) — `crates/iop/volar-iop/src/merkle.rs`.
4. Fiat–Shamir transcript: `crates/iop/volar-iop/src/transcript.rs`'s
   `IopTranscript`, modeled directly on `volar_spec::faest::transcript`'s
   `FaestTranscript` shape (running SHAKE128 sponge, non-destructive
   squeeze) but generic rather than FAEST-specific.

`Q`/`K`/`N` (21/11/32) are a first reasonable choice, not a derived and
proven soundness bound — flagged below.

---

## 3. API

In [`volar-iop`](../crates/iop/volar-iop/):

| Item | Role |
|---|---|
| `field::{Gf128, Ext, BetaOf, Field}` | the native tower field and its trait |
| `fold::{and_check_r1cs, gate_witness, cross_term_z, IopAccumulator, fold_gate}` | Phase 1 |
| `merkle::{MerkleTree, verify}` | generic Merkle commitment |
| `transcript::{IopTranscript, FromBytes}` | Fiat–Shamir |
| `ligero::{prove, verify, LigeroProof}` | Phase 2 |
| `prove_verifier_iop<Z: NonZk>(Tagged<Z, IopAccumulator>) -> Tagged<Transparent, IopProof>` | fold's terminal — precisely, `compress_with_snark`'s role, implemented |
| `verify_iop(&Tagged<Transparent, IopProof>) -> bool` | native check of the finalization proof |

### Discipline safety (compile-time)

Same as the Nova path exactly: `prove_verifier_iop` is bound
`where Z: NonZk` (implemented only for `Transparent`), so a `Tagged<Zk, _>`
accumulator cannot reach it — a compile error, pinned by a `compile_fail`
doctest in `verifier.rs`, mirroring
`crates/fold/volar-fold/src/verifier.rs`'s own.

---

## 4. Build wiring

Same compile-time/runtime split as the Nova path, one crate lighter:

- **Compile-time — `volar-verifier-fold`** (unchanged, reused as-is):
  `emit_verifier_c`/`emit_verifier_rust` are already generic over any
  `Tagged<Transparent, IrModule<IrFunction>>` and never reference
  `NovaFoldSink`'s bare names, so an `IopSink`-woven module lowers through
  them unmodified. Confirmed by
  `crates/fold/volar-verifier-fold/tests/e2e_iop_verifier.rs`, which reuses
  `emit_verifier_rust` directly. There is deliberately no
  `volar-verifier-iop-fold` crate — it would have been empty.

- **Runtime — [`volar-verifier-iop-runtime`](../crates/iop/volar-verifier-iop-runtime/)**
  (depends only on `volar-iop`, `volar-spec`, `volar-discipline`, `std` —
  never `volar-compiler`/`volar-lir-codegen`/`volar-c-backend`/
  `volar-weaver`, same one-directional rule `volar-verifier-runtime`
  follows): `IopLift`, `IopChallenge`, `IopAccumulator`,
  `iop_accumulator_fresh`, `iop_fold_gate` (the bare names an
  `IopSink`-woven verifier links against), `prove_and_verify_iop` (the
  pipeline terminal), and `run_iop_verifier` (the "print → temp Cargo
  project → real `cargo`/`rustc`" harness, same shape as
  `run_folded_verifier`).

---

## 5. Weave-time plugin: `IopSink`

`crates/compiler/volar-weaver/src/vole.rs`'s `IopSink` implements the same
`VerifierTraceSink<P>` extension point `NovaFoldSink` does
(`weave_vole_verifier_with_trace`) — no weaver-internals changes were
needed; a new sink implementation was the entire integration surface, as
[`prove-the-verifier.md`](prove-the-verifier.md)'s own architecture
promised it would be for any future backend.

---

## 6. Actions and RNG: no special handling needed

Tracing `weave_vole_verifier_inner`'s statement lowering
(`crates/compiler/volar-weaver/src/vole.rs`, the `BIrStmt::ActionBit`/
`BIrStmt::Rng` arms) shows both simply bind a pre-supplied Q-share wire
variable (`q_action_{k}_bit_{j}` / `action_{k}_bit_{j}` if public,
`q_rng_{r}`) — exactly like an ordinary circuit input. **Neither introduces
a new algebraic check at the verifier level**: their VOLE-authentication is
established the same way any wire's is, and the only thing Phase 1's fold
ever checks is the AND-gate relation these wires eventually feed into. So
`IopAccumulator`/`fold_gate` need no Action/RNG-specific variant — they
already handle any circuit that produces AND gates, regardless of where the
gates' input wires came from. This was verified by inspection before
writing this doc, not assumed.

## 7. Oracles: deferred

Oracle outputs are *also* just Q-share-bound wires at this weaving layer
(`q_oracle_{k}_bit_{j}`, same mechanism as Actions/RNG above) — in the
common case, this repo's existing oracle-lowering infrastructure
(`docs/external-primitives-plan.md`) resolves an oracle call to concrete
circuit gates before this stage, so by the time a circuit reaches
`weave_vole_verifier_with_trace` there is often no "oracle" left to treat
specially. Whether a *future*, not-yet-built mode (an oracle left opaque
through weaving, needing its own attested check in the finalization proof)
would need a new constraint type in Phase 1's AIR-style constraint set is
an open question, deliberately left for a future document rather than
speculated on here — this document only covers what the current pipeline
actually produces.

---

## Honest scope

- **Tier 3 / needs cryptographic review**: the tower field's soundness
  parameters, the native-field fold algebra (a new instantiation of Nova's
  math, over a new field — proven-elsewhere shape, not proven-elsewhere
  correctness), the Merkle domain separation, and the Fiat–Shamir absorb
  ordering in the finalization IOP. `(K, N, Q) = (11, 32, 21)` is a
  reasonable first choice, not a derived-and-proven soundness bound.
- **Per-gate Fiat–Shamir soundness (Phase 1) is inherited, not solved
  here.** Making the per-gate fold challenges non-interactively sound
  against a third-party verifier, without a recursive/IVC augmented
  circuit, is the same open gap `docs/vcb-ivc-folding.md` §5 already flags
  for the *existing* Nova path ("a production IVC adds Nova's
  augmented-circuit continuity constraint... this is the remaining
  refinement"). This document's genuinely new, fully-buildable-without-
  recursion contribution is Phase 2 (the finalization IOP over the small,
  fixed-size final accumulator) — a single, one-shot statement, not a chain
  of per-gate ones.
- **What this implements, named precisely**: `compress_with_snark`
  (`crates/fold/volar-fold/src/verifier.rs:216-220`), an `unimplemented!`
  stub for "compress the folded verifier with a regular (non-ZK) SNARK" —
  done for real, for a native-`GF(2^k)` fold rather than the existing
  `F_ℓ` one.
- **Not done here**: a FRI/binary-tower-field upgrade of the Phase 2
  finalization IOP for even smaller proofs, if the Ligero-shaped
  construction's concrete parameters prove worse than wanted in practice —
  flagged as future work, same staged-improvement posture as
  `docs/vcb-ivc-folding.md`'s own naive→Montgomery/Pippenger progression.
  A streaming/incremental Merkle-tree construction is *not* a needed
  refinement here (unlike it would be for a monolithic whole-trace design)
  — Phase 2's statement is always small by construction.
- **Scope boundary — narrower than the Nova path today.** This
  implementation covers the AND-gate check (`and_check_r1cs`) only.
  `VerifierTrace`'s `mem_acc_in`/`mem_acc_out` boundary linking
  (`StorageMode::Commitment`'s multiset-hash accumulator, Pedersen-committed
  as `c_in`/`c_out` on the Nova side) is **not yet implemented** here —
  `volar_iop::fold::IopAccumulator`/`ligero::LigeroProof` carry `(W,E,u)`
  only, no memory boundary. A circuit using `StorageMode::Commitment` is
  not yet fully covered by this backend; extending Phase 1/Phase 2 to carry
  the same boundary (natively committed via the Merkle/transcript machinery
  already built here, rather than Pedersen) is future work, not implemented
  in this pass. §6/§7 above record why Actions/RNG need no extension and
  why Oracles are deliberately left open rather than assumed — those two
  sections' scope is accurate; this one is a real gap, not a design choice.
