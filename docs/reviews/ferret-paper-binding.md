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
| Fig. 6 steps 6–9 / §4.2 Fiat–Shamir check | `spcot_consistency_check`, `spcot_fs_chis`, `spcot_masked_choice`, `spcot_sender_hash_v`, `spcot_receiver_hash_w` | **Wired (M5).** Extra-COT masked consistency check: `V = ∑χ_i·v[i] + Y`, `W = ∑χ_i·w[i] + Z`, `Y = Z ⊕ Δ·ϕ` for honest. Two-party split (`sender_hash_v`/`receiver_hash_w`/`masked_choice`). |
| Appendix C batched consistency check | `spcot_batched_fs_chis`, `spcot_batched_masked_choice`, `spcot_batched_sender_hash_v`, `spcot_batched_receiver_hash_w` | One masking over m SPCOTs (`ϕ = ∑_l χ_{α_l}^l`). For the malicious-secure MPCOT (§5). |
| §5 regular-indices MPCOT | `ot/ferret/mpcot_reg.rs` | `t` SPCOT calls on floor-partitioned intervals; each call pads the interval to the next power of two and discards unused leaves from the logical output. The malicious extend checks complete padded SPCOT vectors with `mpcot_reg_consistency_check_padded`; the unpadded helper is available for emitted-output checks. |
| Fig. 7 ΠMPCOT Cuckoo | `ot/ferret/mpcot_uni.rs` | `m = ⌈1.5 t⌉`, `τ = 3`, extra dummy cell, SHA3 stand-in for AES-128 `h_i`. |
| §6.2 10-local primal LPN / RO matrix `A` | `ot/ferret/lpn.rs` | Columns have weight 10 from a public seed; `A` is never sent. The per-column SHA3 derivation remains an unpinned implementation choice, not a validated binding to the paper's optimized expansion. The Ferret finish path reuses each generated column across all three encoded outputs and folds MPCOT/noise into the outputs in one pass; the recorded speedup is exploratory and does not affect the paper binding. |
| Fig. 9 ΠCOT + §6.2 keep-`M` bootstrap | `ot/ferret/cot.rs`, `pool.rs` | Consume `M = k + t·h` seed COTs, where `h = log2` of the padded regular-interval SPCOT length; emit `n − M`, keep first `M`. `ferret_extend_malicious` and the malicious regular-Ferret pool add the Appendix C batched consistency check (extra κ COTs, keep `M = seed_cot_count(true)`). |
| Table 2 Ferret-Reg `(n0,k0,t0)` / `(n,k,t)` | `ot/ferret/params.rs` `FERRET_REG_SETUP` / `FERRET_REG_MAIN`; `mpcot_reg.rs` | Constants are checked arithmetically. Setup uses `n0=609,728`, `k0=36,288`, `t0=1,269`; its floor-partitioned intervals have length 480 or 481 and are padded to SPCOT length 512, with padded leaves discarded. Main uses `n=10,805,248`, `k=589,760`, `t=1,319`, and SPCOT length 8,192. Full-size execution is not unit tested.
| Table 2 Ferret-Uni | `FERRET_UNI_SETUP` / `FERRET_UNI_MAIN` | Same: documented only. |
| Insecure toy sizes | `FERRET_REG_TOY` / `FERRET_UNI_TOY` | `n=256, k=32, t=4` for correctness tests. |
| Bea95 random-COT → chosen-bit | `ot/ferret/pool.rs` `bea95_chosen_bit` | Receiver sends `d = b ⊕ x`. |
| LWE base OT / SoftSpoken / IKNP seams | `ot/lwe.rs`, `ot/base_ot.rs`, `ot/iknp.rs`, `ot/softspoken.rs`, `ot/stack.rs` | Role-separated `BaseOt`; `OtStack::setup_with_base` now allows an alternate base OT to seed the same local extension/pool path. The default setup is still LWE-based; SoftSpoken `K>1` remains a no-op (Roy 2022 at `k=1`). The optional ML-KEM-1024 adapter in `volar-mpc::ot_mlkem` is an unreviewed in-process candidate, not a paper-bound or networked Ferret base-OT deployment. The benchmark-only `ot_kem_candidate_rank` example plugs 512/768/1024 variants into that common seam; those variants are not shipped adapters or approved security parameters. |

## Scope and excluded claims

This landing does not claim 128-bit attack cost, production LWE/LPN
parameters, security of the malicious SPCOT consistency path, SoftSpoken
subfield-VOLE, or a replacement of Chou-Orlandi for existing callers. The
SPCOT consistency check (Fig. 6 steps 6–9 + Appendix C batched) is wired into
`ferret_extend_malicious` and the malicious regular-Ferret pool refill path;
the Ferret-Uni (Cuckoo) malicious extend has a consistency-check wrapper but is
not wired into `CotPool`. The default base-OT seeding remains LWE-based; the
optional ML-KEM adapter is only tested in process and does not replace that
default or provide transport. The documented parameter values are the paper's
target sets, not a security claim for this implementation. Ferret is not woven
into generated ZK prover IR. Passing toy and arithmetic tests establishes only
the stated COT / VOLE relations and parameter calculations, not paper
conformance or security.

## Reproduction

```sh
cargo test -p volar-spec --lib ferret
cargo test -p volar-spec --lib two_party
```

Independent external review has not been recorded. Any change to a mapped
step, cited figure, or parameter constant must update this file; promoting
pinnedness past Unpinned requires the human reclassification in
`docs/reliability.md`.
