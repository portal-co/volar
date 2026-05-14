# ADR 0001: OT and VOLE primitives as commitments-with-deferred-opening

**Status:** Findings — drives trait surface for FAEST work, not yet implemented
**Tier of recommendations:** Tier 3 (touches `volar-spec`)
**Prepared by:** Opus 4.7, session 2026-05-14, M0a of the FAEST plan

---

## Why this ADR exists

The FAEST plan ([`plans/plan-to-implement-the-compressed-stallman.md`](../../../.claude/plans/plan-to-implement-the-compressed-stallman.md), Diversity finding §8) asks: before
codifying a FAEST-flavoured commitment surface, *audit* what is already in
[`volar-spec/src/ot/`](../../crates/spec/volar-spec/src/ot) and
[`volar-spec/src/vole/setup.rs`](../../crates/spec/volar-spec/src/vole/setup.rs)
and determine, per primitive:

1. Is it already a **commitment with deferred opening** — i.e. some piece of
   data hidden inside it is binding+hiding until a later message reveals it?
2. If yes, can that opening be made **public-coin** — derived from a random
   oracle / Fiat–Shamir transcript rather than party-private randomness?

The answers shape the trait surface that BAVC and ConvertToVOLE will land
behind in M2/M3, so this audit is a hard prerequisite.

The TL;DR: **every primitive in `ot/` and `vole/setup.rs` is already a
commitment-with-deferred-opening** in some sense, but they split cleanly
into two camps along the public-coin axis. The split is structural, not
incidental, and naming it explicitly avoids a category error in the
FAEST trait design.

---

## The two camps

### Camp A: "OT-private opening" — opening party holds the secret

| Primitive | What's committed | Decommitment | Held by |
|---|---|---|---|
| [`IdealCot`](../../crates/spec/volar-spec/src/ot/ideal_cot.rs) | sender's `r0` and `r0+Δ` — a 2-out-of-2 commitment to "which row" | revealing `Δ` (sender-side) or the choice bit `b` (receiver-side) | sender = verifier in VOLE setup; `Δ` is verifier-only |
| [`base.rs`](../../crates/spec/volar-spec/src/ot/base.rs) Chou-Orlandi | `(k_0, k_1)` derived from receiver's masked DH key `R` | OT payload `(e_0, e_1) = (m_0 ⊕ k_0, m_1 ⊕ k_1)` plus receiver's `c` opens the chosen branch | receiver holds `c` (private) |
| [`iknp.rs`](../../crates/spec/volar-spec/src/ot/iknp.rs) | columns `q^i = t^i ⊕ Δ_ot[i]·r` — batched commitment to receiver's correlation vector `r` | sender publishes `c_j = k0_j ⊕ k1_j ⊕ Δ_msg`; receiver opens via `b_j` | receiver holds `r = (b_j)` (private) |
| [`lwe.rs`](../../crates/spec/volar-spec/src/ot/lwe.rs) | `(pk_0, pk_1 = h - pk_0)` — LWE-pseudorandom branch keys | sender's `(u_i, v_i)` ciphertexts; receiver opens via LWE secret `s` | receiver holds `s` (private) |
| [`vole_commit_bit`](../../crates/spec/volar-spec/src/vole/setup.rs) | `v_w = r0 + b·Δ` — single-bit VOLE commitment | revealing `Δ` (verifier-side) opens the binding to `b` | `Δ` is verifier-only, `b` is prover-only |

The common shape: **two parties, one of whom holds private randomness
(`c` / `r` / `s` / `Δ`) that is the *opening capability*.** The
commitment is binding+hiding against the *other* party. The opening
party never needs to publish anything random — they just use the secret
they already have to read off the committed value.

**These cannot be made public-coin on the opening dimension.** Making
`c`/`r`/`s`/`Δ` public would defeat the OT property. They *can* be made
public-coin on the **setup** dimension (the CRS, the base group element
`S`, the IKNP matrix coordinates, the LWE matrix `A`), and several
already are.

### Camp B: "Public-coin opening" — opening is a derived challenge

| Primitive | What's committed | Decommitment | Source of opening |
|---|---|---|---|
| [`softspoken.rs`](../../crates/spec/volar-spec/src/ot/softspoken.rs) consistency tag | OT-extension transcript | hash tag `H("ssp-v1" ‖ Δ_msg ‖ r0_j ‖ …)` | deterministic from transcript |
| FAEST BAVC opening *(M2, not yet built)* | τ vectors of seeds via GGM tree | the missing-leaf index `Δ` per BAVC instance | derived from Fiat–Shamir challenge `chall_3` |
| FAEST ConvertToVOLE Δ *(M3, not yet built)* | small-field VOLE output | `Δ` = XOR-derived from the missing seed | the missing-seed choice itself comes from `chall_3` (public-coin) |

The common shape: **opening is a function of a verifier challenge that is
itself derived from the prover's commitments via a random oracle.** The
opening party (the verifier, in the Fiat–Shamir transformed protocol) has
no private randomness — anyone can re-derive the opening from the
transcript. This is exactly what the FAEST paper calls "the BAVC-based
VOLE setup", and it is structurally incompatible with Camp A: there is no
notion of an OT receiver here, just a verifier executing a public hash.

---

## SoftSpoken is the closest existing analogue to FAEST

[`softspoken.rs`](../../crates/spec/volar-spec/src/ot/softspoken.rs) sits
on the boundary between Camp A and Camp B. Its IKNP body is Camp A
(receiver holds private `r`), but its consistency tag is Camp B (the tag
is a public hash of the transcript). The Roy 2022 small-field VOLE
construction that SoftSpoken is heading toward is *structurally the same
object* as what FAEST's ConvertToVOLE produces: τ small-field VOLE
correlations over GF(2^k) that get concatenated to a big-field VOLE over
GF(2^λ).

The difference is the seed source:

- SoftSpoken-style: seeds come from a base OT plus correlation-robust
  hashing — Camp A.
- FAEST BAVC-style: seeds come from a GGM tree where the verifier
  punctures one leaf via a Fiat–Shamir-derived index — Camp B.

These produce *the same `Vope<…, T, U1>`* type and feed *the same*
downstream QuickSilver check. **The abstraction that bridges them is
`LinearHomomorphicCommitment` (per FAEST plan Diversity §1) — both
setups output a `Vope` that implements this trait, and downstream code
does not need to know which camp the setup came from.**

---

## Recommended trait surface

The audit confirms the plan's proposed traits cover the existing
primitives cleanly. Concretely, these traits should land in `volar-spec`
during the FAEST work (Tier 3, Opus only), with retrofitted impls for
existing OT primitives in a follow-up:

### `LinearHomomorphicCommitment` (per FAEST plan Diversity §1)

Implementers in Camp A: `vole_commit_bit`-derived `Vope<N, T, U1>`.
Implementers in Camp B: BAVC-derived `Vope<N, T, U1>` (M2/M3 work).

Both camps produce the same output type and satisfy the same `q = v +
u·Δ` invariant. The trait abstracts over *which Δ-distribution* a
given setup uses.

### `DeferredOpening` *(new — covers both camps)*

```rust
pub trait DeferredOpening {
    type Commitment;
    type Witness;
    type OpeningCapability;
    /// Given the opening capability, recover the committed witness.
    /// Binding: any two distinct witnesses cannot share an opening.
    fn open(commit: &Self::Commitment, cap: &Self::OpeningCapability)
        -> Option<Self::Witness>;
}
```

In Camp A, `OpeningCapability` is party-private (the OT receiver's
choice bit, or the verifier's Δ). In Camp B, `OpeningCapability` is a
public-coin derivation. Both implementations are by-value identical
modulo where the capability came from.

### `PublicCoinOpening: DeferredOpening` *(new, marker for Camp B)*

```rust
pub trait PublicCoinOpening: DeferredOpening {
    /// Derive the opening capability deterministically from a transcript.
    /// Any verifier with the same transcript prefix produces the same
    /// capability — no party-private randomness involved.
    fn derive_capability(transcript: &[u8]) -> Self::OpeningCapability;
}
```

This is the marker that distinguishes BAVC-style openings from OT-style
openings. Camp A primitives implement `DeferredOpening` but NOT
`PublicCoinOpening`. The FAEST signer/verifier weaver in M5 should be
generic over a `PublicCoinOpening` impl (so the same weaver could in
principle drive a future non-FAEST scheme that also has public-coin
openings).

### `VectorCommitment` (per FAEST plan Diversity §2)

The `ABO` in [`byte_gen.rs`](../../crates/spec/volar-spec/src/byte_gen.rs)
already implements something structurally identical to this trait, just
not as a trait yet. M2's BAVC adds the FAEST `LeafCommit` parameterization
and grinding loop. Other VOLEitH-style schemes (e.g. a future direct port
of Picnic3) would reuse the same trait with a different leaf-commit choice.

`VectorCommitment` is naturally a `PublicCoinOpening` impl: the missing
index is the opening capability, and Fiat–Shamir derives it from the
transcript.

---

## What this means concretely for the FAEST work

1. **Do not duplicate the commitment surface in FAEST.** Land the four
   traits above in `volar-spec/src/commitment/` (new module) during M2,
   and define BAVC as `impl VectorCommitment + impl PublicCoinOpening`.
2. **Retrofit Camp A primitives as `DeferredOpening` impls in a
   follow-up.** This is a low-risk Tier 3 task: add the impl block,
   add a small test verifying the trait contract holds, no behavioural
   change. Keep it out of the FAEST critical path.
3. **The FAEST weaver in M5 should take a `&impl PublicCoinOpening` for
   the BAVC step**, not a concrete `BavcCommitment`. This makes the
   weaver agnostic to leaf-commit choice (FAEST vs FAEST-EM) and
   future-proofs it for non-FAEST public-coin-VOLE schemes.
4. **`derive_and_q` in `setup.rs` is the public-coin analogue of the
   binding check.** Its Δ-invertibility precondition becomes an
   invariant of the `LinearHomomorphicCommitment` impl: M3's
   `ConvertToVOLE` output type should statically witness "Δ is non-zero
   in every lane", either via a wrapper newtype or via runtime
   rejection-sampling at construction time. Same pattern as
   `random_nonzero_delta`.
5. **No category error.** The plan's Divergence §2 stays: do not layer
   FAEST's BAVC-derived VOLE on top of `vole_commit_bit`. They are
   different impls of the same `LinearHomomorphicCommitment` trait, in
   different camps, and the trait is the only thing that bridges them.

---

## Non-findings

- Nothing in `ot/` or `vole/setup.rs` blocks the FAEST work. Every
  primitive is structurally orthogonal — FAEST adds Camp-B impls of
  traits whose Camp-A impls already exist.
- No existing primitive is "secretly already FAEST's BAVC" — the
  closest, SoftSpoken, is still Camp A on its OT body and only Camp B
  on its tag.
- The two new traits (`DeferredOpening`, `PublicCoinOpening`) are not
  load-bearing for FAEST correctness — they are load-bearing for
  *reuse*. A FAEST-only implementation could skip them; the plan says
  not to.

---

## Open questions for the M2 reviewer (Opus)

1. Should `DeferredOpening::Witness` be a fixed associated type, or
   carry a lifetime so Camp A implementations can borrow rather than
   move out of the commitment?
2. Is the `PublicCoinOpening::derive_capability` signature broad enough
   to cover the FAEST grinding loop, or does grinding need its own
   trait method (`derive_capability_with_grind(transcript, counter)`)?
3. Where should the `commitment/` module live — `volar-spec/src/` or
   `volar-common/`? The traits don't depend on VOLE specifically;
   `volar-common` may be the right home, with VOLE-specific impls in
   `volar-spec`.
