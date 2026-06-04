# VCB-IVC: gap-succinct continuation by folding (`volar-fold`)

**Status:** Implemented (scaffold) — folding machinery concrete and self-tested;
two cryptographic components explicitly isolated as open tasks (the boundary
**embedding** and binding **generators**). Prepared by Opus 4.8, 2026-06-03.

Companion to [`vole-continuation-bridge.md`](vole-continuation-bridge.md) §3.3
(this realises the "VCB-IVC — gate succinctness in `m`" frontier). Crate:
[`crates/fold/volar-fold`](../crates/fold/volar-fold).

---

## 1. What problem this solves

A network gap of `m` iterations is currently bridged by **replay-from-anchor**
(VCB-RX): sound and memory-succinct, but the gap *gates* are re-proven, so the
reconnect cost is `O(m)`. VCB-IVC makes the reconnect cost **independent of `m`**:
the `m` cleartext gap steps `S_{k+1}…S_{k+m} = F^m(S_k)` are *folded* into a single
relaxed-R1CS instance of size `O(|F|)`, checked once on reconnect.

## 2. Design decision: Nova **minus** the zkSNARK, interactive

Nova = a folding scheme **plus** a final zkSNARK that compresses the accumulated
instance to `O(1)`. We **drop the final SNARK**: the folded instance is opened
and checked **natively and interactively** by the verifier in `O(|F|)`. This is
the accepted trade-off — *online network is cheap and interactive*, and the win
(independence from `m`) is already captured by the fold; proof *size* `O(|F|)` is
fine.

**Consequence — no cycle of curves.** The cycle-of-curves in recursive SNARKs
exists only to verify the previous step's proof *inside* the next circuit
(recursion). With no in-circuit verifier (no SNARK, native final check), there is
**no recursion**, so a **single** curve suffices. We reuse Ed25519 from
[`volar_spec::curve`].

## 3. Construction

### 3.1 Scalar field `F_ℓ` ([`scalar`](../crates/fold/volar-fold/src/scalar.rs))
Folding R1CS arithmetic is over the Ed25519 scalar field (`ℓ ≈ 2^252`).
`volar_spec::curve` provides only the base field `Fe25519`, so `F_ℓ` is
implemented here. **Reference-speed:** 512-bit products are reduced mod `ℓ` by
bit-by-bit long division (`O(512)`/mul) — obviously correct, not fast; a
Barrett/Montgomery path is a drop-in later optimisation.

### 3.2 Pedersen commitment ([`pedersen`](../crates/fold/volar-fold/src/pedersen.rs))
`Commit(x⃗; ρ) = Σ x_i·G_i + ρ·H` over `EdPoint` (MSM via `ed_scalar_mul`/`ed_add`),
additively homomorphic (the property folding needs).

### 3.3 (Relaxed) R1CS + NIFS ([`r1cs`](../crates/fold/volar-fold/src/r1cs.rs), [`nifs`](../crates/fold/volar-fold/src/nifs.rs))
Assignment `z = [W ‖ u]`; plain satisfaction `(Az)∘(Bz)=Cz`, relaxed
`(Az)∘(Bz)=u·(Cz)+E`. The folding step computes the cross term `T`, commits it,
draws a public-coin challenge `r`, and folds instance + witness:
`W←W1+r·W2`, `E←E1+r·T+r²·E2`, `u←u1+r·u2`, and the commitments correspondingly.
The verifier folds the *instance* from `comm_T` and `r` alone. Tested:
folding two/three satisfying instances yields a relaxed-satisfied,
commitment-consistent instance.

### 3.4 Native verification ([`verify`](../crates/fold/volar-fold/src/verify.rs))
The prover opens `(W,E)`; the verifier checks the commitment openings + the
relaxed relation in `O(|F|)`. Sound because folding is knowledge-sound (a
satisfying opening of the fold implies satisfying openings of every folded step).

### 3.5 Gap folding + boundaries ([`ivc`](../crates/fold/volar-fold/src/ivc.rs))
`prove_gap` folds `m` per-step `F`-instances into one accumulator and commits the
boundary states `C_in = Commit(S_k)`, `C_out = Commit(S_{k+m})`. Tested: 8 honest
steps fold to one natively-verifiable instance; a single lying step is rejected.

### 3.6 The bridge strategy ([`bridge_adapter`](../crates/fold/volar-fold/src/bridge_adapter.rs))
`FoldingBridge<L>` assembles the reconnect message (`C_in, C_out, U_final, W_final`,
two link proofs) and, verifier-side, returns `GapVerdict::Proven` iff
`native_verify` and both boundary links pass. Generated weaver code is unchanged
— it calls only the `ResilientVoleTransport::{prover,verifier}_bridge` methods, so
a concrete transport delegates streaming to an inner transport and the bridge
methods here.

## 4. The embedding (the open component) — `BoundaryLink`

The boundary state is a bit-string committed in **two** worlds: VOLE (`GF(2^k)`
MACs) and folding (`F_ℓ` Pedersen). The fields differ, so there is **no free
linear check** tying them. Per the design decision, this is abstracted behind the
[`BoundaryLink`](../crates/fold/volar-fold/src/link.rs) trait (`prove`/`verify`),
so the embedding is a swappable component. The shipped `DummyLink` **always
accepts** (placeholder, **not sound**) so the bridge wires and tests end-to-end.

**Candidate sound embeddings (interactive — online is cheap):**

1. **Random-linear-combination bit equality.** Verifier sends a challenge; both
   sides reveal a masked linear combination of the boundary bits and check
   equality. Cross-field: combine over the integers / `{0,1}` and compare the
   *bit-string*, not a field element. Cheap (`O(ℓ)` online), but the cross-field
   masking needs care.
2. **In-circuit bit-decomposition + opening.** Constrain the folding witness bits
   boolean in R1CS and open a verifier-chosen subset, cross-checked against the
   VOLE openings. Simple, leaks a subset (pad/ZK as needed).
3. **Binary-field-friendly fold commitment.** Replace the prime-field Pedersen on
   the fold side with a commitment over the *same* `GF(2^k)`, making the link a
   free VOLE linear check — at the cost of a folding scheme whose commitment is
   additively homomorphic over a binary field (open design question).

**Recommendation:** (1) for the first sound version (matches the interactive,
cheap-online setting); (3) is the elegant end state if a binary-field homomorphic
commitment with a hard relation is found. **This needs cryptographic review.**

## 5. Other caveats (honest)

- **Pedersen generators are not binding** until a *hash-to-curve* derivation
  replaces the current base-scalar-mult one (`curve.rs` lacks hash-to-curve). The
  commitment is correctly homomorphic (folding/tests work) but a prover who knows
  the generator DLs can equivocate. Isolated task, same status as the embedding.
- **IVC cross-step continuity** (output of step `t` = input of step `t+1`) is
  assumed by witness chaining, not enforced in-circuit; a production IVC adds
  Nova's augmented-circuit continuity constraint. The per-step `F` constraints +
  boundary links are in place; this is the remaining refinement.
- **Public-coin challenges** (`r`, `r_T`) are passed explicitly for determinism;
  the real protocol derives them by Fiat–Shamir over the transcript (or
  interactively from the verifier).

## 6. Security sketch

Completeness: an honest folded gap verifies (NIFS completeness + native check +
honest links). Soundness reduces to: NIFS knowledge-soundness for
`S_{k+m}=F^m(S_k)` (under commitment **binding** — caveat §5) + `BoundaryLink`
soundness (the embedding — §4) at both boundaries (union bound). ZK: from
commitment blinding + (optional) padding of `m`. Reconnect cost: `O(|F|)` native
check + `2` boundary links, **independent of `m`**, consuming **no streaming VOLE
correlations** during the gap.

## 7. Status summary

| Component | State |
|---|---|
| `F_ℓ`, Pedersen, R1CS, NIFS, native verify, IVC, FoldingBridge | **implemented + tested** (25 unit tests) |
| `BoundaryLink` embedding | **trait + `DummyLink` placeholder** — sound impl is the open task (§4) |
| Binding generators (hash-to-curve) | **open** (§5) |
| In-circuit continuity, Fiat–Shamir, fast reduction | refinements (§5) |
