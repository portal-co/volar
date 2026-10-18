# Official BitGC parameters

**Date:** 2026-10-12
**Sources:** ePrint 2025/1810 (Eurocrypt 2026, "BitGC Made (More) Efficient", rev. 2026-05-19) §6 "SWHEwDD parameters" and Table 2; the authors' artifact `github.com/wtd2/more-efficient-bitgc` (`test/bench/e2e_benchmark.cc`, `test/support/benchmark_support.cc`); ePrint 2024/1988 §5 (original BitGC RLWE instantiation).
**Ledger:** feeds BITGC-LEDGER-03 (noise/parameter review) and BITGC-LEDGER-07 (profile registry for the session protocol).

## TL;DR

Yes — the Eurocrypt 2026 paper and its artifact publish a complete,
concrete 128-bit parameter set for BitGC-fast and BitGC-small (and an
estimated set for BitGC-tiny). These are the parameters any Volar BitGC
profile should target; our current `LatticeRnsDd` toy profile
(`n=8, p=97, q0=1649, q1=1153`) is orders of magnitude below them and is
test-only.

## Published parameter sets (128-bit RLWE security, per-gate failure < 2⁻⁴⁰)

All schemes use BGV in RNS form with 60-bit data limbs, two contexts
(large PRG context → smaller gate context via ring-dimension reduction
[CDKS21]), and NTT-friendly primes throughout (the paper's SIMD
decoupling is what permits this).

| Parameter | BitGC-fast | BitGC-small | BitGC-tiny (estimated) |
|---|---|---|---|
| N_PRG (offline ring degree) | 2^15 | 2^15 | 2^16 |
| N_Gate (online ring degree) | 2^13 | 2^14 | 2^15 |
| log Q_PRG (offline coeff modulus) | ≈600 (10 limbs) | ≈780 (13 limbs) | 1200 |
| log Q_Gate (online coeff modulus) | ≈180 (3 limbs) | ≈300 (5 limbs) | 660 |
| log p (plaintext modulus) | ≈60 | ≈60 | ≈60 |
| log q (k=1 DD modulus) | ≈120 | ≈120 | ≈120 |
| log r (k=2 DD modulus) | ≈240 | ≈240 | ≈240 |

Security is estimated following the lattice estimator [ACC+19]; the
per-gate failure target is 2⁻⁴⁰.

### Exact prime generation (artifact)

The artifact does not hardcode primes; it generates them with SEAL's
NTT-friendly prime sampler:

```text
seal::CoeffModulus::Create(N_PRG, vec![60; data_levels + 2])
```

i.e. `data_levels + 1` 60-bit primes for the coefficient chain (the last
is SEAL's special key-switching prime, not a data level) plus **one more
60-bit prime split off to serve as the plaintext modulus p** — this is
exactly how the paper's "log p ≈ 60" NTT-friendly plaintext modulus is
obtained, and it matches our RNS requirement `p | q0` in spirit (here p
is a chain sibling, not a divisor; see below).

Artifact CLI defaults (`bench_bitgc_e2e`):

| Variant | `--offline-dim` | `--online-dim` | `--offline-level` | `--online-level` |
|---|---|---|---|---|
| fast | 15 | 13 | 10 | 3 |
| small | 15 | 14 | 13 | 5 |

The correctness-demo profile `--offline-dim=9 --online-dim=8` is
explicitly **not** 128-bit secure (artifact README).

### Setup payload

One-time setup communication (public parameters, Enc(Δ), DD preprocessing,
evaluation keys): ≈1.6 GiB (fast), ≈2.7 GiB (small), amortizable across
garblings of the same parameter set. This is the dominant real-world
deployment cost and belongs in the session profile registry.

### Offline PRG

Goldreich-style low-depth PRG with the ABBS25 instantiation, k1=4, k2=7,
ℓ=2^16, s=2 (paper §6). 1 PRG bit per gate (small) / 4 per AND (fast).

## Noise / correctness conditions (for BITGC-LEDGER-03)

- Paper Appendix A, Lemma 1: DD correctness needs
  `n·(E_out+1)·p/q ≤ negl(λ)` and `n·(‖Δ‖∞+1)/p ≤ negl(λ)`, with Δ
  uniform ternary (‖Δ‖∞ ≤ 1) and E_out the BGV level-0 output noise
  bound. This is the Scooby-style wrap condition.
- Our measured toy-profile failures (depth 1 stable, depth 2 ~10%
  failure at `8/97/1649/1153`) are exactly this wrap term firing at a
  modulus ~2^11 instead of 2^120.
- Labels live in R_p with p ≈ 2^60; the DD rounding is
  `((b − a·Δ) mod q) mod p`, matching our `round_limb0_to_rp` limb-0
  shortcut when `p | q0`. **Caveat:** the artifact instead uses SEAL BGV
  where p is a sibling prime, not a divisor of the chain — our `p | q0`
  construction is a deliberate simplification for exactness; the paper's
  DD analysis (Lemma 1) only needs the wrap bound, not divisibility.

## What we can use

1. **Profile registry entries** (BITGC-LEDGER-07 Phase 4): register
   `bitgc-fast-128` and `bitgc-small-128` with the table above —
   `(variant, N_PRG, N_Gate, log Q_PRG, log Q_Gate, log p, log q, log r,
   limb count, 2^-40 failure target)`. The session's
   `canonical_parameter_fingerprint(variant, p, degree, modulus_chain)`
   already has the right shape; the chain entries become the 60-bit
   limbs.
2. **Online depth**: N_Gate = 2^13 (fast) / 2^14 (small) with 3/5 online
   data limbs is the paper's answer to "how much online AND depth is
   supported" — far beyond our toy profile. `max_and_depth` for these
   profiles should be derived from the Lemma 1 bound with the BGV noise
   model, not guessed.
3. **Plaintext modulus**: p must be a 60-bit NTT-friendly prime
   (`p ≡ 1 mod 2·N_Gate`). SEAL's `CoeffModulus::Create` output is the
   authoritative source of such primes; we can reproduce them
   deterministically (SEAL documents its descending prime walk) or pin
   exact values in the registry once chosen.
4. **Not yet usable**: the k=2 DD modulus r ≈ 240 needs our third RNS
   limb (mechanical per the LatticeRnsDd design notes); the full-size
   N=2^13/2^15 rings need our generalized NTT substrate (already in
   `src/ntt.rs`) plus production noise distributions (Gaussian, not the
   reference ternary), which remain BITGC-LEDGER-03 work.

## References

- ePrint 2025/1810 §6, §7, Appendix A (parameters, DD construction,
  Lemma 1/2 conditions).
- ePrint 2024/1988 §5 (original RLWE SWHEwDD; `q > 4B·p·(N+1)^L·n^L`
  parameter condition, `ℓ = ⌊log q⌋+1`, `s = ⌊log(q/p)⌋`).
- `wtd2/more-efficient-bitgc` artifact: `benchmark_support.cc` (prime
  pool generation, plain-modulus split), `e2e_benchmark.cc` (variant
  defaults), README (security caveats).
