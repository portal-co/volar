# Agent Context: the GF(2^k) → F_ℓ embedding (implemented, needs cryptographic review)

> **ARCHIVED — REMOVED FROM THE CODEBASE.** `FoldLift`, `NovaFoldSink`, and
> `volar-verifier-runtime` no longer exist — the Nova-based prove-the-verifier
> path was removed once the IOP-based path
> (`../prove-the-verifier-iop.md`) subsumed it, staying native to `GF(2^k)`
> and needing no embedding at all. Kept here as a historical/academic record
> of this open cryptographic seam and how it was closed for the (now
> removed) construction; nothing in this document describes live code.

**Load this when:** (historical) touching `volar-verifier-runtime`'s
`FoldLift`, `NovaFoldSink` (`volar-weaver`'s `vole.rs`), or anything folding
the VOLE verifier (`docs/archive/prove-the-verifier.md`).

> **Update:** a constraint-expansion construction replacing the one-scalar lift
> is now specified **and implemented** per `docs/fold-lift-expansion.md`
> (spaced-packing carry-less multiplication; one `T` element ↦ its GF(2)
> coefficient bits + R1CS gadget rows — `volar_fold::gf2k`). Read that spec
> before touching this seam. The history and "Do not" guidance below still
> stand: the construction has a written soundness argument and deterministic
> tests, but the seam stays flagged for cryptographic review —
> "specified/implemented" is not "closed".

## The problem

A VOLE AND-gate check lives in the VOLE field `T` (e.g. `Galois`, GF(2^8), real
polynomial-mod-irreducible arithmetic): `K_a·K_b + V̂ = K_c·Δ`, all in `T`. The
Nova fold math (`volar_spec::fold`) needs those same values lifted into the
folding scalar field `F_ℓ` (`volar_fold::scalar::Scalar`, a ~2^252 prime field) to
build an `and_check` R1CS witness. **There is no sound field homomorphism from
`GF(2^k)` to `F_ℓ` in general** — they have different characteristics and
different multiplicative structure.

This is not a new observation: `docs/prove-the-verifier.md`'s "Honest scope" and
`docs/vcb-ivc-folding.md` §4 both flagged the analogous boundary as needing
cryptographic review before this session. What changed is that the mechanism now
exists and was run for real, producing **direct empirical evidence** the naive
embedding is unsound (see below), not just a theoretical gap.

## Current state

`crates/fold/volar-verifier-runtime/src/lib.rs` defines `FoldLift` (a *local*
trait, not `std::convert::From`/`Into` — see that module's doc for why: `T` and
`Scalar` are both foreign to any crate but `volar-fold`, so the orphan rule would
force a `From`/`Into` impl into `volar-fold`). Since the constraint-expansion
change it no longer maps an element to a single scalar; its shape is:

- `const BITS: usize` — the GF(2)-degree k of `T`;
- `const POLY: u128` — the irreducible polynomial's low bits (`x^BITS` implicit);
- `fn lift_bits(&self) -> Vec<bool>` — the element's LSB-first GF(2)
  coefficient bits.

Implemented for `Galois` (`BITS = 8, POLY = 0x1b`) and `Bit` (`BITS = 1,
POLY = 0`). `fold_and_gate` feeds those bits to
`volar_fold::gf2k::and_check_gf2k` (`docs/fold-lift-expansion.md` §3: 174
constraints / 165 witness variables per GF(2^8) gate) and Nova-folds
witness-only via `volar_fold::nifs::cross_term_z`. The old byte-reinterpret and
degenerate 0/1 lifts are gone; both impls are sound-by-construction expansions
per that spec's §5 argument (still subject to the review below).

**`crates/fold/volar-verifier-fold/tests/e2e_fold_verifier.rs` exercises the
seam concretely**, not hypothetically: it weaves a real one-AND-gate circuit
with `NovaFoldSink`, compiles the woven verifier to real Rust, links it against
`FoldLift for Galois`, and runs it with a genuinely honest VOLE proof (via real
`vole_and_prover_step`/`derive_and_q` — actual GF(2^8) field arithmetic, not
hand-picked integers). Result:

- The **real GF(2^8) Quicksilver check passes** (`all_ok == true`) — the woven
  verifier's own logic is correct and unaffected by any of this.
- The **expanded F_ℓ witness satisfies `and_check_gf2k`'s relaxed relation** —
  `assert!(r1cs.is_satisfied_relaxed(w, e, u), …)`, a pinned **success**.

History: that same assert used to be pinned known-**failing** (`assert!(!…)`)
while the one-scalar lift was in place — plain F_ℓ integer multiplication on
the transmuted bytes is simply not what `K_a ·_{GF(2^8)} K_b` computes, and the
test kept that empirical evidence in the suite on purpose. The constraint
expansion is what flipped it.

## What needs review

A sound (or explicitly-scoped-as-unsound-but-acceptable-for-X) embedding, likely
one of:

1. **Bit-decomposition.** Decompose each `T` element into its `GF(2)` coefficients
   and build an F_ℓ-side circuit that reconstructs/checks the GF(2^k)
   multiplication structurally (the R1CS would need to encode polynomial
   reduction, not just multiply the embedded integers) — sound but adds real
   constraint count. **This is the implemented route** (`volar_fold::gf2k`,
   spec + soundness argument in `docs/fold-lift-expansion.md`); what remains
   is the review itself, plus the residual simplifications that spec's §7
   lists (no transcript binding, per-gate independent Δ bits, lane-0
   projection, single-limb `k ≤ 16`).
2. **A genuinely degenerate demo scope**, e.g. real `T = GF(2)` weaves (using
   `FoldLift for Bit`, already provided) — sound for that narrow case, not
   representative of production VOLE parameters.
3. Something with a real cryptographic argument this document's author isn't
   positioned to invent — this is the crux of why it's tracked here rather than
   guessed at.

## Do not

- Silently swap in a different-looking embedding and call the seam "closed"
  without the review this doc tracks.
- Weaken or remove `e2e_fold_verifier.rs`'s positive
  `assert!(r1cs.is_satisfied_relaxed(w, e, u), …)` or its `assert!(all_ok)` —
  together they are the end-to-end evidence that a genuinely honest GF(2^8)
  VOLE proof both passes the real Quicksilver check *and* folds to a
  satisfying gf2k instance. If either starts failing, that's a regression in
  the embedding (or the weave), not a bug in the test — investigate before
  touching the asserts.
