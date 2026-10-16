# WARP paper pinning and Volar implementation map

**Status:** research pinned, implementation in progress.
**Paper:** Benedikt Bünz, Alessandro Chiesa, Giacomo Fenzi, and William
Wang, *Linear-Time Accumulation Schemes*, IACR ePrint 2025/753, major TCC
2025 revision dated 2026-06-17.
**Archived artifact:** `docs/research/papers/2025-753.pdf`, SHA-256
`22944c8ef358bf973c4eaca7617fc534f024e3658142244668ae0030b4ae904c`.
**Archived text extraction:** `docs/research/papers/2025-753.txt`, SHA-256
`1cd3959b31c5baca36100c12ea99c86620dff011a414005c52ce2ed9cc8f7a81`.
**Canonical source:** <https://eprint.iacr.org/2025/753> and
<https://eprint.iacr.org/2025/753.pdf>.

## What WARP is

WARP is a split accumulation scheme for polynomial equation satisfiability
(PESAT). The paper's headline construction is hash-based and operates in the
random-oracle model: the prover performs linearly many field operations and
the accumulation verifier performs logarithmically many field operations for
the accumulated relation. It supports unbounded accumulation depth and uses an
interactive oracle reduction of proximity that works with any linear code over
a sufficiently large field. The straightline extractor relies on erasure
correction rather than efficient error-tolerant decoding.

## Paper-pinned construction

The implementation target is Construction 10.4, which composes:

1. **Construction 5.10 — PESAT to constrained code.** Encode each witness as
   `f = C(w)`, sample `τ ∈ F^{log M}`, and set the bundled constraint
   `β := (x, τ)` and `η := 0`. Remark 5.9 permits revealing a deterministic
   evaluation claim at `α := 0^{log n}` to turn the `R_{C,0}` output into the
   `R_C` input expected by Construction 9.4.
2. **Construction 6.3 — twin-constraint pseudo-batching.** Batch `ℓ` instances
   with a `log ℓ`-round multilinear sumcheck over
   `eq(τ, i) · (eval_claim + ω · bundled_PESAT_claim)`.
3. **Construction 7.2 — codeword batching.** Commit to the batched codeword,
   add `s` out-of-domain MLE samples and `t` in-domain shift-query claims, and
   authenticate the queried input-codeword coordinates with Merkle paths.
4. **Construction 8.2 — multilinear constraint batching.** Batch the
   `r = 1 + s + t` claims with a `log n`-round quadratic sumcheck and output a
   single evaluation claim `(α, μ)`.
5. **Construction 10.4 — ROM accumulation.** Replace public randomness with
   Fiat–Shamir squeezes, bind the PESAT index, public inputs, input
   accumulator instances, Merkle roots, targets, and every sumcheck polynomial
   into one domain-separated transcript, and decide by re-checking the final
   codeword commitment, MLE claim, bundled PESAT claim, and `f = C(w)`.

All of `n`, `M`, `ℓ`, and `1+s+t` are required by Theorem 9.1/Remark 9.3 to
be powers of two. The implementation must reject non-power-of-two shapes
rather than silently padding protocol objects in a way that changes the
statement.

## Ordering detail

Construction 5.10's prose shows `β := (τ, x)`, while Construction 10.4 step
2(f) explicitly sets `β_i := (x_i, τ_i)`. The Volar port follows
Construction 10.4's `x || τ` order and documents that choice at the code seam.

## Reference implementation found

Public implementation: <https://github.com/willow-network/warp-folding>,
reviewed at commit
`a24c049fdbe7eb2275a3813421b7a9d958eec182` (`2026-07-26`, “docs: relicense to
AGPL-3.0 and correct the Expander attribution”).

Its README says the repository is a complete and tested pre-release research
implementation over M31Ext3/Orion/BLAKE3, but also says:

- the soundness story is not settled;
- the headline 128-bit claim depends on a parallel-repetition argument not
  proven in the paper;
- the shipped `n_ood = 1, n_shifts = 2` sampling profile is a
  development/benchmark profile, not a high-soundness profile;
- the repository is retired research code and is AGPL-3.0 because it vendors
  AGPL Polyhedra Expander material.

Because Volar is CC0/MIT/Apache-2.0, the Volar implementation must not copy
that repository's code. It can be used as an external behavioral cross-check,
but the implementation in Volar is written from the paper's equations and
Volar's existing field/Merkle/transcript abstractions.

## Volar placement

WARP belongs in `volar-iop`, beside the existing field, Merkle, transcript,
and IOP modules:

- use `crate::field::Field` for generic field arithmetic and `Gf128` for the
  default sufficiently large challenge field;
- use `crate::merkle` for codeword commitments/openings;
- use a domain-separated `IopTranscript` for the ROM's Fiat–Shamir oracle;
- expose a generic `LinearCode` trait, with identity and simple systematic
  test codes first; an Orion/expander production code is a separate,
  explicitly reviewed adapter;
- keep extraction/proof-system security claims out of the type docs unless the
  code, distance/MCA parameters, field size, and query counts are all bound to
  a reviewed parameter set.

## Security and implementation boundaries

The first Volar milestone is a construction-faithful algebraic implementation
and differential test harness. It is not a production soundness claim. A
usable deployment still needs:

- a linear code with proved distance and mutual-correlated-agreement
  parameters;
- field/query choices satisfying Theorem 9.1 Equations 12–13 and Theorem 10.3;
- reviewed Fiat–Shamir serialization/domain separation;
- independent review of the sumcheck implementations and transcript order;
- tests for malformed proofs, wrong roots, wrong openings, wrong PESAT
  witnesses, out-of-order transcript fields, and decider rejection.
