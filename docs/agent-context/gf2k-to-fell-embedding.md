# Agent Context: the GF(2^k) → F_ℓ embedding (open, needs cryptographic review)

**Load this when:** touching `volar-verifier-runtime`'s `FoldLift`, `NovaFoldSink`
(`volar-weaver`'s `vole.rs`), or anything folding the VOLE verifier
(`docs/prove-the-verifier.md`).

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

## Current state (as of this session)

`crates/fold/volar-verifier-runtime/src/lib.rs` defines `FoldLift` (a *local*
trait, not `std::convert::From`/`Into` — see that module's doc for why: `T` and
`Scalar` are both foreign to any crate but `volar-fold`, so the orphan rule would
force a `From`/`Into` impl into `volar-fold` and quietly present one embedding as
canonical) with two implementations:

- `impl FoldLift for Galois` — reinterprets the byte directly (`Scalar::from_u64(x
  as u64)`). The **default**, matching `GateObservation`/`VerifierStep`'s existing
  simplification elsewhere in this codebase (not a new decision, just extended to
  the weave-time mechanism).
- `impl FoldLift for Bit` — the `0 ↦ 0, 1 ↦ 1` embedding, sound only for a
  degenerate `T = GF(2)` weave (`Δ` can only be `1`). Kept pluggable per explicit
  direction, not because it's believed sound in general.

**`crates/fold/volar-verifier-fold/tests/e2e_fold_verifier.rs` demonstrates the
gap concretely**, not hypothetically: it weaves a real one-AND-gate circuit with
`NovaFoldSink`, compiles the woven verifier to real Rust, links it against
`FoldLift for Galois`, and runs it with a genuinely honest VOLE proof (via real
`vole_and_prover_step`/`derive_and_q` — actual GF(2^8) field arithmetic, not
hand-picked integers). Result:

- The **real GF(2^8) Quicksilver check passes** (`all_ok == true`) — the woven
  verifier's own logic is correct and unaffected by any of this.
- The **embedded F_ℓ witness does not satisfy `and_check_r1cs`'s relaxed
  relation** — `Scalar(byte(K_a)) · Scalar(byte(K_b))` (plain F_ℓ integer
  multiplication) is simply not what `K_a ·_{GF(2^8)} K_b` computes, so an honest
  GF(2^8) relation does not carry over to the naively-embedded F_ℓ one.

The test pins this as `assert!(!r1cs.is_satisfied_relaxed(w, e, u), …)` — a
known-failing check, kept in the suite on purpose so that fixing the embedding
shows up as "this test needs updating," not a silent gap.

## What needs review

A sound (or explicitly-scoped-as-unsound-but-acceptable-for-X) embedding, likely
one of:

1. **Bit-decomposition.** Decompose each `T` element into its `GF(2)` coefficients
   and build an F_ℓ-side circuit that reconstructs/checks the GF(2^k)
   multiplication structurally (the R1CS would need to encode polynomial
   reduction, not just multiply the embedded integers) — sound but adds real
   constraint count.
2. **A genuinely degenerate demo scope**, e.g. real `T = GF(2)` weaves (using
   `FoldLift for Bit`, already provided) — sound for that narrow case, not
   representative of production VOLE parameters.
3. Something with a real cryptographic argument this document's author isn't
   positioned to invent — this is the crux of why it's tracked here rather than
   guessed at.

## Do not

- Silently swap in a different-looking embedding and call the seam "closed"
  without the review this doc tracks.
- Delete or "fix" the `e2e_fold_verifier.rs` pinned-failure assertion without
  understanding *why* it currently fails — that's the evidence this gap is real,
  not a bug in the test.
