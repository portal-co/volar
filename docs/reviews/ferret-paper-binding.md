# Ferret COT paper binding

**Classification:** Unpinned, Very unstable. This artifact maps ePrint 2020/924
figures and sections onto the `volar-spec` Ferret modules. It is **not** a
promotion to paper-pinned; that remains a human decision under
[`docs/reliability.md`](../reliability.md).

**Bound source:** Yang, Weng, Lan, Zhang, Wang, *Ferret: Fast Extension for
coRRElated oT with small communication*, ePrint [2020/924](https://eprint.iacr.org/2020/924).
Code lives in `crates/spec/volar-spec/src/ot/ferret/` and the LWE / SoftSpoken
seams it consumes.

## Code-to-source map

| Paper | Code | Bound claim |
|---|---|---|
| Fig. 6 ΠSPCOT steps 2–5 (GGM + puncture) | `ot/ferret/spcot.rs` (`spcot_sender_extend`, `spcot_receiver_extend`, `spcot_choice_bits`) | Semi-honest single-point COT: `w[i]⊕v[i] = 0` off `α`, `= Δ` at `α`. `G` is `AesCtrLengthDoubler`. |
| Fig. 6 steps 6–9 / §4.2 Fiat–Shamir check | `spcot_consistency_check` | API present; extra-COT monomial embedding of `χ_α` is **not** wired. Honest in-process tests check `H'(∑χ v) = H'(∑χ w ⊕ χ_α Δ)`. |
| §5 regular-indices MPCOT | `ot/ferret/mpcot_reg.rs` | `t` SPCOT calls on intervals of length `n/t` (power of two). |
| Fig. 7 ΠMPCOT Cuckoo | `ot/ferret/mpcot_uni.rs` | `m = ⌈1.5 t⌉`, `τ = 3`, extra dummy cell, SHA3 stand-in for AES-128 `h_i`. |
| §6.2 10-local primal LPN / RO matrix `A` | `ot/ferret/lpn.rs` | Columns have weight 10 from a public seed; `A` is never sent. |
| Fig. 9 ΠCOT + §6.2 keep-`M` bootstrap | `ot/ferret/cot.rs`, `pool.rs` | Consume `M = k + t log(n/t)` seed COTs, emit `n − M`, keep first `M`. |
| Table 2 Ferret-Reg `(n,k,t)` / `(n0,k0,t0)` | `ot/ferret/params.rs` `FERRET_REG_SETUP` / `FERRET_REG_MAIN` | Documented constants. **Not used in unit tests.** Setup `n/t` is not a power of two; do not call `splen()` on it. |
| Table 2 Ferret-Uni | `FERRET_UNI_SETUP` / `FERRET_UNI_MAIN` | Same: documented only. |
| Insecure toy sizes | `FERRET_REG_TOY` / `FERRET_UNI_TOY` | `n=256, k=32, t=4` for correctness tests. |
| Bea95 random-COT → chosen-bit | `ot/ferret/pool.rs` `bea95_chosen_bit` | Receiver sends `d = b ⊕ x`. |
| LWE base OT / SoftSpoken / IKNP seams | `ot/lwe.rs`, `ot/base_ot.rs`, `ot/iknp.rs`, `ot/softspoken.rs` | Role-separated `BaseOt`; SoftSpoken `K>1` remains a no-op (Roy 2022 at `k=1`). |

## Scope and excluded claims

This landing does not claim 128-bit attack cost, production LWE/LPN
parameters, malicious security of the SPCOT check, SoftSpoken subfield-VOLE,
or a replacement of Chou-Orlandi for existing callers. Ferret is not woven
into generated ZK prover IR. Passing the toy tests establishes only the
stated algebraic COT / VOLE relations on those sizes.

## Reproduction

```sh
cargo test -p volar-spec --lib ferret
cargo test -p volar-spec --lib two_party
```

Independent external review has not been recorded. Any change to a mapped
step, cited figure, or parameter constant must update this file; promoting
pinnedness past Unpinned requires the human reclassification in
`docs/reliability.md`.
