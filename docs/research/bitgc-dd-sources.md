# BitGC DD gadget — source constructions (COS+22 / Scooby, ILL25b)

**Status:** 2026-10-08. Resolves the DD-source references that
`bitgc-paper-pinning.md` left at "App. A / [COS+22] / [ILL25b]". This is the
performance-path prerequisite: the production SWHEwDD gadget's correctness
*and* parameters come from these, not from the BitGC papers alone.

## The k=1 DD is Scooby's 2-party lsb distributed decryption (COS+22)

**Citation fix:** the `[COS+22]` the BitGC papers reference is **Scooby:
Improved Multi-Party Homomorphic Secret Sharing Based on FHE**, Chillotti,
Orsini, Scholl, Smart, Van Leeuwen — ePrint **2022/862**, SCN 2022
(DOI 10.1007/978-3-031-14791-3_24; journal version Inf. Comput. 297:105133).
Earlier notes conflated it with the TFHE large-precision PBS paper (CLOT21,
ePrint 2021/729); that is a *different* Chillotti et al. and is **not** the DD
source.

### The exact algorithm (Scooby §4.1, Type lsb)

With `sk = sk1 + sk2 (mod q)` split additively, and a BGV-style (lsb)
ciphertext `ct` with `ct·(1,−sk) = m + p·e (mod q)`, `‖e‖∞ ≤ B`:

```
R ← Z_q  (public nonce; removed via PRF in the real construction)
P1:  d1 ← ct·(1, −sk1) + R  (mod q);   m1 ← d1 (mod p)
P2:  d2 ← ct·(0, −sk2) − R  (mod q);   m2 ← d2 (mod p)
```
Then `m = m1 + m2 (mod p)` with probability `≥ 1 − N·2^(-sec)`.

### The correctness condition (Scooby Eq. 1) — drives the modulus

```text
q > 2 · p · (B+1) · 2^sec
```

`sec` is the statistical security parameter; the failure is a **wrap-around**
in the `mod q` reduction of `d1` when a coefficient lands within `p·(B+1)` of
the `±q/2` boundary (prob. `2·p·(B+1)/q < 2^-sec` per coefficient, `N`
coefficients ⇒ `1 − N·2^-sec` overall).

**This is the super-polynomial-modulus requirement.** It is the reason the
BitGC paper's DD modulus is `log q ≈ 120` for k=1 ([ZLL+26] §6): with
`log p ≈ 60` and `sec ≈ 40`, `q` must exceed `2·2^60·(B+1)·2^40`. Our
`lattice_dd` noise is a multiple of `p` (so it vanishes on the mod-`p`
reduction and the identities are exact), which is the *stronger* exactness the
BitGC DD gadget relies on — but the wrap-around bound above is the same term
as the LSB-wrap failure we measured empirically (see
`bitgc-paper-pinning.md` §"Landed", finding 3).

### Mapping to `volar_spec::bitgc::lattice_dd`

| Scooby | `lattice_dd` |
|---|---|
| `ct = (c0, c1)`, `ct·(1,−sk)` | row ciphertext `τ = (u1, u2)`, phase `u1 + u2·Δ` |
| `sk` split `sk1+sk2` | Δ (garbler) vs. the label share `A = W^π + x̂·Δ` |
| `d1 = ct·(1,−sk1)+R` | `DD_i(A, τ) = round_p(u2·A + i·u1)` |
| `R` nonce (→ PRF) | the `PRF_K(γ)` per-gate offset in `pinned` |
| correctness `q > 2p(B+1)2^sec` | the `q` modulus budget (not yet enforced) |

## The k=2 DD is a CP-RLWE triple (ILL25b)

The correlated-key DD (`DD_{i,j}`, the `dd_ij`/`hss_mul` path) uses a
**CP-RLWE triple** `[ILL25b]` replacing the original GSW-style ciphertexts
([ZLL+26] §6: "log r ≈ 240 for k=2"). The artifact's `HSSMul` implements it
with the triple `(U, V, W)` satisfying `U·Δ + V ≈ 0`, `U·Δ² + W ≈ −Δ`
(`lattice_dd`'s `triple`); the garbling only needs the *share-product*
identity `HssMul(A0+iΔ, B0+jΔ) ≡ U·A0·B0 + i·j·Δ (mod p)`, which `lattice_dd`
already realizes exactly. The production form is the RLWE encryption of those
relations with the modulus budget `log r ≈ 240`.

## Performance-path status (2026-10-08)

**Landed:**
- NTT-accelerated negacyclic mul in `lattice_dd` (`mul_q`/`mul_p` dispatch to
  `ring::negacyclic_mul_ntt` when the modulus is `≡ 1 mod 2N`, else
  schoolbook), with a fail-fast NTT-friendliness guard in `ring::Ntt::new`.
- NTT-vs-schoolbook exactness verified at scale (`N` up to 1024,
  `ring::tests::ntt_vs_schoolbook_scaling`) — the win is the O(N log N) vs
  O(N²) op count (~341× fewer ops at N=1024).

**The production-parameter form landed (2026-10-08).** `lattice_dd`'s
single-`u64` ceiling (`log q ≤ 63`) is lifted by
`bitgc::lattice_dd_rns::LatticeRnsDd`, a **two-limb RNS** gadget
(`q = q0·q1`, both NTT-friendly, `p | q0`). The key enabler is the limb-0
rounding: because `p | q0`, `x mod p = (x mod q0) mod p` and the phase noise
(`O(p) ≪ q0/2`) is centered exactly from limb 0 alone, so **no CRT
reconstruction is needed** — the one step that historically broke multi-prime
`swhe`. Ring products run per-limb (NTT where the limb prime is friendly,
schoolbook fallback). Verified: `rns_dd_linear_identity`,
`rns_dd_correlated_identity`, `rns_eval_row_reconstruction`, and a full
`rns_circuit_roundtrip` (both variants through the `DdGadget` seam). Two
60-bit limbs give `log q ≈ 120` (the k=1 target); a third limb reaches
`log r ≈ 240` (k=2) — the structure is limb-count-generic and extending it
is mechanical.

## Performance-path implications (BITGC-LEDGER-04 production form)

1. **The modulus chain is large by design** (super-poly for DD). The NTT is
   mandatory: schoolbook `O(N²)` at `N = 2^13..2^15`, `log q ≈ 120..300` is
   not viable. `lattice_dd`'s `mul_q`/`mul_p` NTT dispatch (landed) is the
   foundation; the RNS multi-limb representation for `log q > 64` is the next
   layer (currently `lattice_dd` uses a single `u64` q).
2. **The DD noise analysis for BITGC-LEDGER-03** should cite Scooby §4.1
   Prop. 4.1 + Eq. (1) directly, and the KDM point from [LWYY25b]: the rows
   encrypt `t·Δ` under Δ itself (key-dependent message), which is an
   *assumption* the paper makes and a reviewer must sign off on.
3. **BGV parameter reuse** (Scooby §5.4): bootstrappable BGV already satisfies
   Eq. (1) at usable levels (`2^(b_p·U) ≥ 2^128`), so the super-poly blow-up is
   *free* on a standard BGV chain — this is the paper's whole efficiency point.
