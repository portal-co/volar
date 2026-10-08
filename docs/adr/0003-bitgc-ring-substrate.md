# ADR 0003: BitGC ring-arithmetic substrate is in-crate, no external dependencies

**Status:** accepted, 2026-10-08.

## Context

The BitGC variants plan (`docs/bitgc-variants-plan.md`) requires
NTT-friendly prime-modulus ring arithmetic (SIMD SWHE per ePrint
2025/1810 §3) inside `volar-spec`. The crate is `#![no_std]` + `alloc`
and carries a deliberately minimal dependency set (`cipher`, `digest`,
`sha3`, `hybrid-array`, `elliptic-curve`, workspace-internal crates).

The maintainer decision on 2026-10-08 is explicit: **no external
libraries may be imported into `volar-spec`** for this work.

## Decision

`volar_spec::bitgc` implements its own ring arithmetic:

- `bitgc::ring` — negacyclic ring `Z_q[X]/(X^N+1)` for NTT-friendly
  prime `q`, with the same negacyclic-NTT pattern already proven in
  `tinylabels/ring_lwe.rs` (`Ntt`, `bit_reverse`, `find_negacyclic_root`,
  u128-widened `mul_mod`, rejection-sampled `sample_uniform`). Slot
  (evaluation-form) encode/decode provides the SIMD batching surface the
  paper's construction requires.
- `bitgc::swhe` — leveled BGV-style arithmetic (add/mul/relinearize/
  modulus-switch) over `bitgc::ring`, with caller-supplied
  `RandomSource`/`NoiseSource` in the `tinylabels` style. This is an
  experimental mathematical implementation, not a reviewed SWHE library.
- `bitgc::prg` — the low-depth PRG expansion driver with explicit depth
  accounting, so the paper's pinned PRG (BITGC-PIN-03) can replace the
  reference profile without touching the expansion machinery.

## Consequences

- No new entries in `volar-spec/Cargo.toml`'s dependency table for BitGC;
  `Cargo.lock` is unaffected by the construction modules.
- Arithmetic is constant-prime `u64` with `u128` widening — adequate for
  the paper's 60-bit-prime modulus chains and for the scaled test
  profiles, and auditable line-by-line.
- Performance (HEXL-class SIMD instructions, AVX2) is explicitly
  out of scope for the spec crate; that is provider-artifact territory
  covered by the ledger's FHE-PLUMB-TOOLCHAIN rows.
- The per-gate stitching equations sit behind a `GateEquation` seam
  (ledger row BITGC-LEDGER-04) so the paper-pinned equations
  (Figures 4–5, §5.1 of ePrint 2025/1810) can replace the reference
  adapter without changing ring, SWHE, record, or transport code.
