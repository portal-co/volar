# BitGC variants — research notes and benchmark accounting

**Status:** 2026-10-08. Companion to `docs/bitgc-variants-plan.md` and
`docs/research/bitgc-paper-pinning.md` (the exact equations). This file
captures the benchmark accounting (BITGC-LEDGER-05) and the paper notes
behind the pinning table.

## Communication accounting (exact, paper-faithful)

BitGC's entire value proposition is per-gate communication. The pinned
drivers' record accounting is **exact** (no approximation):

| Variant | per AND | per XOR | per INV |
|---|---:|---:|---:|
| BitGC-small | 1 bit (determination) | 1 bit (mask) | 0 (free) |
| BitGC-fast  | 5 bits (4 mask + 1 determination) | 0 (free) | 0 (free) |

These match the artifact's `wtd2/more-efficient-bitgc` accounting exactly
(sub64: small 376 bits, fast 315 bits — confirmed by both the artifact's
wire-log `Required=` lines and `volar-spec`'s
`tests/bitgc_sub64_conformance.rs`).

### vs. the half-gate locked baseline (Thumb SHA-256)

The locked Thumb SHA-256 compression fixture is 124,160 non-free (AND)
gates at 16-byte labels (the ERT interpreters constant-fold; earlier
baseline was 164,288):

| Scheme | Bytes/gate | Total (Thumb SHA-256) | vs. 4-row |
|---|---:|---:|---:|
| half-gate 4-row | 64 B | 7,946,240 B (7.58 MiB) | — |
| half-gate 3-row (row-reduced) | 48 B | 5,959,680 B (5.68 MiB) | −25% |
| **BitGC-small** | **0.125 B** | **15,520 B (15.2 KiB)** | **512× smaller** |
| **BitGC-fast** | **0.625 B** | **77,600 B (75.8 KiB)** | **102× smaller** |

(These are gate-table/record bytes only — they exclude the offline seed
expansion, input-label delivery, and any protocol framing, matching how
the half-gate baseline counts tables.)

This is the paper's Table 1 claim realized on the actual fixture's gate
count: sub-kilobyte-to-tens-of-kilobytes garbled circuits where the
half-gate path needs megabytes.

## Timing structure

The authoritative timing is the artifact's own `--check=wire` breakdown
(captured in `docs/research/artifact-vectors/`). For sub64 under the
low-memory correctness profile:

- **small**: online per-gate AND=14.46 ms (CT=9.8, ExtMul=3.5, HSS/DD=1.1),
  XOR=0.53, INV=0.33 (all under software emulation; relative structure is
  the signal).
- **fast**: online per-gate AND=1.33 ms (CT=0.33, HSS/DD=0.999 — note the
  ExtMul column is **absent** from fast's online path, confirming the
  paper's no-online-multiplication structure), XOR=0.51, INV=0.008.

The `volar-spec` reference (schoolbook O(N²) negacyclic mul, degree-8
ring, exact `R_p` DD) is a correctness oracle, not a performance target:
it garbles+evaluates sub64 in ~1 ms per variant locally
(`tests/bitgc_bench.rs`). Production timing requires the NTT-accelerated
ring (landed for `lattice_dd`) plus the full SWHE-with-DD online phase at
the paper's parameters (N_PRG=2^15, N_gate=2^13/2^14) — that is the
provider-artifact territory (FHE-PLUMB-TOOLCHAIN-03), not the spec crate.

**Scope note:** running the full Thumb SHA-256 fixture *timing* under the
BitGC reference adapter is out of scope for the current level budget (the
reference GateEquation is shallow-depth); the communication envelope above
is the meaningful, exact benchmark. A full Thumb timing run is gated on
the production NTT + leveled-SWHE path and the QEMU Thumb ERT fixture.

## Paper notes behind the pinning table

See `bitgc-paper-pinning.md` for the extracted equations. The pinning table
(`docs/bitgc-variants-plan.md` §10) tracks each structural claim to its
paper anchor and the artifact cross-check. Key cross-cutting facts:

- **Δ is the SWHE secret key**, ternary, `LSB(Δ)=1`; labels are additive
  key-shares `W^π = W^0 + π·Δ`; per-gate communication is the determination
  bit `d_γ = LSB(W⁰_γ) ⊕ π_γ` (plus 4 PRG mask bits per AND under fast).
- **Per-wire offline value is one permutation bit π** (N = |I|+|W| for
  small, 4·|W_AND| for fast) — the first-cut `(A0, π)` reference-adapter
  wire shape was wrong relative to the paper and is superseded by the
  `pinned` layer.
- **The DD `(U,V,W)` CP-RLWE triple is a shared evaluation key** — garbler
  and evaluator must use the same one or the `Eval` reconstruction identity
  breaks.
- **Free NOT is a label copy + π flip with the DD base unchanged**
  (`W_c^{π_c} = W_a^{π_a}`); shifting `w0` to compensate is required, else
  every NOT→AND breaks (found by the sub64 differential test).
- **The LSB wrap-around is the per-gate failure term** (~N/p per gate;
  paper's log p≈60 targets <2⁻⁴⁰). Measured 1.03%/gate at p=97; negligible
  at the test profile's p=2⁶².
