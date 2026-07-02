# Agent Context: higher-K gate degree in the VOLE weaver (future track, not started)

**Load this when:** touching `weave_vole_verifier_inner`/`weave_vole_prover_inner`'s
`BIrStmt::And` handling (`volar-weaver`'s `vole.rs`), `ProvenanceHandler::gate_degree`,
or FAEST-style AES pinning.

## The gap

`ProvenanceHandler::gate_degree` documents a K=2 case (S-box gates,
`vole_sbox_prover_step`/`vole_sbox_verifier_check`, no `hat` correction needed)
and notes K=3 as a future extension. `volar_spec::vole::prove::
vole_mul3_prover_step`/`vole_mul3_verifier_check` (K=3 product-gate check) are
already implemented and unit-tested in `volar-spec`. But
`weave_vole_verifier_inner`'s `BIrStmt::And` arm only branches on
`gate_degree(prov) == 2` — there's no K=3 (or general K>2) emission path at all;
it falls through to the K=1 case.

## The right shape (per direction given while scoping the prove-the-verifier work)

Higher-K gates (K=3 and beyond) should be handled **generically, the same way
K=2 is** — not as one-off special cases per K. The motivating real-world use case
for K=3 specifically is **pinning AES implementations to the FAEST proof
scheme** (FAEST uses a K=3 product-gate check for the AES S-box norm constraint —
see `vole_mul3_prover_step`'s doc). That's a real, scoped use case, but it's
unrelated to the prove-the-verifier folding track this document was written
alongside.

## Status

Not started. Explicitly out of scope for the prove-the-verifier folding plan
(`docs/prove-the-verifier.md`) — that work's completeness tests deliberately do
not exercise K=3/FAEST paths, and any *future* completeness tests for this gap,
once it's implemented, should stay scoped to what K-generalization actually
changes (gate emission, witness/hat handling) rather than pulling in
FAEST/AES-pinning-specific behavior as part of testing the generalization itself.

## What starting this would involve

- Generalizing `BIrStmt::And`'s degree dispatch in `weave_vole_verifier_inner`/
  `weave_vole_prover_inner` from a hardcoded `== 2` check to a real K-parametric
  path, reusing `vole_mul3_*` (and whatever K>3 needs) the same way K=1/K=2 reuse
  `vole_and_*`/`vole_sbox_*`.
- Deciding how `and_count`/`sbox_count`-style per-degree counters generalize to
  more than two buckets.
- A test that pins today's behavior first (K=3 silently falls through to K=1 —
  worth a regression test on its own, since a real K=3 gate compiled today would
  silently miscompile) before generalizing.
