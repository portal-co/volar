# ADR 0002: One semi-honest KEM-seeded Ferret correlation-provider candidate

**Status:** Architecture selected; concrete construction, parameters, and deployment are not approved.<br>
**Scope:** Initial semi-honest `n=2,t=1` secret-shared Volar MPC; shared COT/VOLE establishment for all protocols.<br>
**Pinnedness:** Unpinned. **Stability:** Very unstable.<br>
**Prepared:** 2026-10-03.

## Decision

Use one session-level correlation-provider pipeline for the initial semi-honest profile:

```text
experimental ML-KEM-seeded 1-of-2 base-OT candidate
    → semi-honest IKNP/SoftSpoken seed-COT extension
    → Ferret-Reg COT pool (retain seed correlations and refill iteratively)
    → Bea95 random-COT to chosen-bit COT
    → common CotSource / VOLE adapter consumed by every protocol
```

Protocol implementations request correlations from that shared provider. They do **not** choose their own OT, COT, VOLE, or extension construction. A later profile change must replace or version the provider as a whole; do not quietly select a different primitive per multiplication/triple protocol.

This settles the **layering and candidate family**, not an approved cryptographic construction or security claim. ML-KEM-512/768/1024 are now compared at the benchmark seam; the shipped experimental adapter remains ML-KEM-1024 and in-process. The exact KEM-to-OT proof, adversary game (including quantum adversaries), security-level requirement, Ferret LPN parameters, network framing, and provider approval remain validation gates. Malicious security is not part of this profile. “KEM-seeded” is not a post-quantum security claim for the composed OT/COT/MPC pipeline.

## Why this pipeline

- The project already has `BaseOt<L>`, `OtStack`, Ferret refill pools, Bea95 chosen-bit conversion, and `CotSource<U1, Galois128>`. `OtStack::setup_with_base` now permits a candidate base OT to feed the same extension/pool interface.
- Ferret's steady-state mechanism retains a seed of `M = k + t·log2(n/t)` semi-honest COTs while yielding `n−M` output COTs per iteration; the retained correlations seed the next iteration. Count bootstrap, refill, conversion, framing, and memory costs rather than quoting paper throughput alone.
- Centralizing the stack avoids every VOLE-using protocol independently deciding which OT/COT/VOLE construction to adopt.

## Security boundary and caveats

1. NIST FIPS 203 standardizes ML-KEM as a KEM, **not** this OT composition. The KEM's post-quantum claim does not automatically transfer to an OT compiler, OT extension, Ferret's primal-LPN assumption, or a complete MPC protocol.
2. The current ML-KEM OT is a custom real/fake-public-key candidate, not Masny–Rindal (MR19). Its fake key must be sampled without a corresponding secret key. The earlier prototype generated a second real key pair and discarded its decapsulation key; a corrupted receiver's view could retain both secrets, so that version did not meet the ordinary semi-honest receiver-privacy definition. It has been changed to sample a canonical uniform ML-KEM-1024 public-key encoding for the fake slot. This repair and the OT candidate still need an independent proof/review; correctness tests are not security evidence. The new `OtStack::setup_with_base` seam and adapter are in-process only: the default networked `two_party`/`FerretOtChannel` path still uses LWE base OT, so this architecture is not yet the runtime default.
3. MR19's classical random-oracle/UC claims must not be restated as post-quantum UC security. Bartusek (2026) gives a quantum-adversary separation for UC security of Masny–Rindal OT instantiated with a post-quantum KEM, while also studying weaker game-based notions. The required target is the exact semi-honest composition used by Volar, with a stated quantum-adversary game; no UC claim is made here.
4. Ferret-Reg's cited parameter table targets a classical attack-cost estimate. Do not infer post-quantum LPN security from its ML-KEM bootstrap. Recompute/review the full composed parameters for the requested quantum threat model.
5. The in-process `SpecRng`/`SeedRng` benchmarks are deterministic microbenchmarks, not deployment timings. The base-OT adapter has no IPC/TCP transport, and the measured setup shares both roles in one process.

## Initial performance probe

Reproducer (from the workspace root):

```sh
cargo run --release -p volar-mpc --features mlkem --example ot_ferret_probe
cargo run --release -p volar-mpc --features mlkem --example ot_ferret_probe -- --main
```

The probe ran on an Apple M3 Ultra (28 cores), Rust `1.98.1`, release mode, 2026-10-03. It uses deterministic `SeedRng`; figures are local observations, not paper benchmarks.

| Probe | Result | What is included / excluded |
|---|---:|---|
| 128 ML-KEM-1024 base OTs only | 20.84–21.49 ms across 5 runs | Key generation, fake-key sampling, two encapsulations and one decapsulation per OT; in-process, no frame/transport costs. |
| Encoded base-OT payload | 806,912 bytes total | 128 × (2 × 1,568-byte encapsulation keys + 2 × (1,568-byte ciphertext + 16-byte seed)); excludes framing, IKNP messages, and Ferret communication. |
| Toy Ferret-Reg stack | setup 21.30–21.75 ms; 1,000 chosen-COT→VOLE rows 2.59–2.65 ms | `n=256,k=32,t=4`; correctness-only/insecure. Not an application performance estimate. |
| Ferret-Reg main parameters (fused LPN pass) | setup 16.24 s; 10,198,341 chosen-COT→VOLE rows drained in 15.46 s (~659,717 rows/s) | Same main profile and probe as the earlier baseline; one in-process run with deterministic RNG, no network or IPC. Setup includes 128 KEM base OTs, IKNP seed generation, and initial Ferret iteration; drain includes a second refill at the pool watermark. |
| Peak resident set for main probe | 2,034,401,280 bytes | `/usr/bin/time -l`; the fused pass removes intermediate LPN vectors, but MPCOT/pool working buffers still dominate peak RSS. |

The Ferret finish path now generates each 10-local SHA3-derived column once and simultaneously computes all three encoded outputs, folding the MPCOT outputs/noise into those results. It also splits retained seed and emitted COTs by moving vector tails rather than copying them. Against the previously recorded single-run baseline (23.91 s setup, 23.08 s drain, ~441,894 rows/s, 2,040,102,912-byte peak RSS), this run was about 32.5% lower in combined setup+drain time and 49% higher in drain throughput, while peak RSS changed by less than 0.3%. These are exploratory, non-controlled single-run comparisons; the implementation's SHA3 matrix expansion remains unpinned and has not been paper-bound to Ferret's optimized expansion. The probe is still dominated by Ferret/LPN and pool costs rather than the roughly 21 ms for 128 KEM base OTs, and does not establish a target deployment throughput.

**Bootstrap blocker:** the checked-in `FERRET_REG_SETUP` table entry is not divisible as required by `mpcot_reg` (`n % t == 0`), and therefore cannot be passed through the current regular-MPCOT implementation. This probe bypasses that setup profile and directly generates the main iteration's 606,907 seed COTs with IKNP. The proposed paper-style smaller setup → main transition is not measured or implemented; repair the parameter/algorithm binding before claiming that bootstrap optimization.

## ML-KEM parameter-set comparison

Reproduce with `cargo run --release -p volar-mpc --features mlkem --example ot_kem_candidate_rank`; add `-- --main --only=512` (or `768`/`1024`) for one full main-profile run. Replayed on 2026-10-06 from the local Volar `feat/mpc` tip on an Apple M3 Ultra (28 cores), Rust `1.98.1`, release mode. The benchmark-only `BaseOt<16>` plugins feed each parameter set into the same `OtStack` and Ferret-Reg code. Each main-profile row is one deterministic in-process run; base-OT time is the median of five 128-OT samples embedded in that run. The toy-stack column comes from a separate deterministic invocation. The wire estimate includes both public keys per OT and both ciphertext-plus-16-byte-label payloads, but excludes framing and transport.

| Candidate | 128 base OTs, median | Base-OT wire | Toy-stack setup / 1,000 chosen-COT→VOLE | Main setup / 10,198,341-row drain | Main max RSS |
|---|---:|---:|---:|---:|---:|
| ML-KEM-512 | 7.837 ms | 405,504 B | 7.899 ms / 1.810 ms | 16.350 s / 15.533 s (656,579 rows/s) | 2,027,405,312 B |
| ML-KEM-768 | 12.720 ms | 585,728 B | 12.985 ms / 1.745 ms | 16.332 s / 15.542 s (656,165 rows/s) | 2,027,192,320 B |
| ML-KEM-1024 | 20.062 ms | 806,912 B | 20.815 ms / 1.773 ms | 16.331 s / 15.586 s (654,324 rows/s) | 2,033,614,848 B |

Performance-only ranking is 512, then 768, then 1024: base-OT cost and bytes rise with parameter size. In this replay, the full main-profile setup+drain totals differ by only 43 ms (about 0.13%) across the single runs, and peak RSS is effectively unchanged; Ferret/LPN dominates. Thus these measurements do **not** select a parameter set. Choose only after the required security level and exact OT proof are established; do not trade away a required security margin for a sub-percent end-to-end difference. The example-local adapters demonstrate that the candidates plug into the existing `BaseOt`/`OtStack` seam, but are benchmark code, not shipped or transport-ready adapters.

No alternate PQ PKE/KEM implementation was available in this workspace to benchmark. A different OT compiler (including the cited PKE-to-OT construction) is not a drop-in KEM parameter set; establish its assumptions and implement a correct adapter before comparing it. Candidate replacement remains at the shared `BaseOt`/provider boundary, not independently inside protocols. The replay is exploratory and single-run for each toy/main stack profile; timing variation does not justify parameter selection.

## Required M0 work before admitting the provider

- Paper-bind the exact semi-honest KEM/PKE OT and its fake-key sampler. State whether the guarantee is classical or quantum game-based, its composition assumptions, and why it is enough for IKNP/SoftSpoken in this runtime. Do not identify the custom candidate as MR19.
- [x] Compare ML-KEM-512/768/1024 at the benchmark-only common `BaseOt`/`OtStack` seam for base-OT CPU, nominal wire bytes, and main-profile peak RSS. This is a performance ordering only, not a security ranking or parameter selection.
- Benchmark an alternate PKE/KEM only after a suitable implementation and its OT-composition assumptions are established; recalculate reductions/parameters for the exact OT and adversary model.
- Repair and paper-bind Ferret's setup/main profiles, 10-local matrix expansion, and quantum LPN parameter estimate. Measure one-time setup, iterative refill, chosen-COT conversion, VOLE/triple throughput, end-to-end framed IPC, and peak memory at representative demand sizes.
- Compare only at this common provider boundary. If KEM→IKNP/SoftSpoken→Ferret is not fast or adequately supported, select a replacement provider once for the profile, not separately in consuming protocols.
- Add independent review and a two-process IPC conformance test before using the prototype outside exploratory performance work.

## References

- NIST, *FIPS 203: Module-Lattice-Based Key-Encapsulation Mechanism Standard* (2024), §§1–2, 7. ML-KEM is a KEM; its underlying security is related to Module-LWE and is believed to resist quantum adversaries. <https://csrc.nist.gov/pubs/fips/203/final>
- Daniel Masny and Peter Rindal, *Endemic Oblivious Transfer*, ACM CCS 2019 / IACR ePrint 2019/706. Its OT functionality and classical/random-oracle assumptions must be read precisely; it is not the construction implemented by Volar's current real/fake-key code. <https://eprint.iacr.org/2019/706>
- Saikrishna Badrinarayanan, Daniel Masny, and Pratyay Mukherjee, *Efficient and Tight Oblivious Transfer from PKE with Tight Multi-User Security*, IACR ePrint 2022/415. Requires multi-user PKE security and pseudorandom public keys; the paper's protocol and model are not interchangeable with this direct semi-honest prototype. <https://eprint.iacr.org/2022/415>
- James Bartusek, *Natural Barriers to Quantum Extraction: On the Post-Quantum (In)security of (O)EKE and Masny-Rindal OT*, arXiv:2609.39844 (2026). <https://arxiv.org/abs/2609.39844>
- Yang, Weng, Lan, Zhang, and Wang, *Ferret: Fast Extension for coRrelated oT with small communication*, IACR ePrint 2020/924, especially §§5–6 and Table 2. Existing local code binding: [`reviews/ferret-paper-binding.md`](../reviews/ferret-paper-binding.md). <https://eprint.iacr.org/2020/924>
- Yuval Ishai, Joe Kilian, Kobbi Nissim, and Erez Petrank, *Extending Oblivious Transfers Efficiently*, CRYPTO 2003 (IKNP). <https://www.iacr.org/archive/crypto2003/27290145/27290145.pdf>
