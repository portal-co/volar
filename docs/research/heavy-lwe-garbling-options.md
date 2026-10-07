# Heavy LWE-based garbling and FHE-adjacent options

**Status:** research and cost-planning note, 2026-09-14.  This compares
research constructions against the current streaming Yao-style Cirrus/Volar
workload. It is **not** an implementation recommendation, a parameter set, or
a security/deployment claim.

The executable arithmetic used below is committed alongside this note:

```sh
python3 docs/research/heavy_garbling_cost.py \
  --and-gates 124160 --selected-labels 512 --reuse 1000000 \
  --depth 256 --leveled-overhead-per-depth-kib 4
```

The `--leveled-overhead-per-depth-kib` input is deliberately a scenario knob.
The literature usually states a `D * poly(lambda)` asymptotic term rather than
concrete compatible bytes for this repository. The script refuses to invent
that polynomial.

## Decision

Do not replace the current streaming 16-byte-label Yao/half-gate-like path.
The only near-term action is to preserve an experimental, explicit research
seam for a **server/offline coordinator**. The normal direct OT/Ferret delivery
and Cirrus ERT streaming remain the default. TinyLabels is complementary: it
compresses selected external input delivery, not the garbled-program/table
stream.

FHE integration work is likewise complementary, not a reason to merge a
heavy reusable-garbling scheme into the current TFHE track. Volar's TFHE
material is explicitly Unpinned and Very unstable; it has no reviewed
parameter/interoperability/deployment claim. Any FHE-backed evaluator or
hybrid is its own construction/protocol and must receive an independent clear
oracle, parameter, transcript, and resource review.

## Workload baseline

The locked Cirrus Thumb SHA-256 replay reports:

| Fact | Value | Source |
| --- | ---: | --- |
| Non-free gates | 124,160 | `THUMB_SHA256_MEASUREMENT.md` |
| Stable four-row tables, 16-byte labels | 7,946,240 B / 7.58 MiB | same |
| First-row-fixed tables | 5,959,680 B / 5.68 MiB | same |
| Selected input labels | 512 / 8,192 B | same |
| Illustrative table reuse | 1,000,000 | same |

The circuit **depth is not measured by that record**. The script defaults to a
clearly marked `D = 256` scenario only to demonstrate sensitivity. Substitute a
compiler-measured multiplicative depth before comparing a leveled construction.

At one million uses, table persistence already amortizes the existing first-row
fixed stream to about 5.96 B/use before persistence/framing/input delivery.
That changes the question: a massive reusable-encoding setup must beat not only
first-run traffic but also a very cheap already-persisted table stream.

## Candidate families

### 1. LWE functional encryption → reusable garbling

Goldwasser, Kalai, Popa, Vaikuntanathan, and Zeldovich construct reusable
circuit garbling from succinct single-key functional encryption under LWE.
Their construction uses leveled FHE: fresh FHE keys encrypt inputs and a Yao
component garbles FHE decryption. This is the closest prior art to a planned
FHE hybrid, but it is a feasibility construction rather than a drop-in fast
Yao backend. [GKP+13]

**Fit:** potentially useful when a fixed function is evaluated repeatedly and
FHE ciphertext inputs are already required by another subsystem.

**Non-fit:** unsuitable as an embedded default. It introduces FHE key,
ciphertext, and decryption-circuit costs, plus assumptions/security definitions
that differ from the current semi-honest strict session.

### 2. Compressed reusable garbling via ABE / key-homomorphic encryption

Gentry et al. give a reusable garbling size of:

```text
|GC| = |C| + poly(lambda, D)
```

under **subexponential LWE** hardness. The important result is additive rather
than multiplicative security-parameter overhead, not a concrete claim that one
Boolean AND takes one byte in this implementation. [GGH+13]

Later work gives LWE-based reusable garbling with stronger security definitions,
but it remains a functional-encryption-level construction, not a lightweight
streaming table format. [A+16]

**Fit:** a server/offline experiment where the garbled program is broadcast or
persisted and the circuit description itself is expensive to replicate.

**Non-fit:** the current Thumb circuit's persisted 5.68 MiB table stream is
already approximately 5.96 B/use at one million reuses. A `poly(lambda, D)`
setup has to be concretely accounted for, not hidden behind the attractive
asymptotic expression.

### 3. Constant-size / fully succinct reusable garbling from stronger lattice assumptions

Hsieh, Lin, and Luo obtain compact reusable garbling for unbounded-depth
circuits from **circular small-secret LWE**. For Boolean outputs the garbled
circuit can be constant-size independent of circuit size/depth up to polynomial
security factors. This is powerful theory, but circular-security and concrete
instantiation/resource costs put it outside the current deployment claim.
[HLL23]

**Fit:** long-lived server-side research for stored/broadcast function tokens.

**Non-fit:** do not treat “constant-size” as “small”; keys, input encodings,
evaluation work, parameter sizes, and the nonstandard assumption dominate the
missing concrete cost model.

### 4. HSS / Power-RLWE succinct garbling

Recent HSS-based work claims one amortized bit per Boolean gate under circular
Power-RLWE and a leveled alternative with:

```text
|GC| = |C| + D * poly(lambda)
```

under non-circular Power-RLWE. It reports that evaluation/garbling is much
slower but may be feasible for multi-million-gate circuits when broadcast or
storage dominates. [GLOS25]

This is the most promising **size** research candidate for a server-side
experiment because it explicitly trades compute for communication and includes
a leveled, non-circular form. It is not “the LWE parameters we already have”:
TinyLabels' degree/modulus/noise profile and the current OT LWE profile are not
an automatically valid Power-RLWE parameterization.

### 5. Low-depth succinct garbling

A recent result claims low-depth one-bit-per-gate garbling from RLWE/local-PRG
assumptions, fully succinct reusable garbling from decomposable LWE, and a
privacy-free succinct variant. It is particularly relevant if parallel FHE
work can provide low-depth circuit decompositions, but it is too recent and
lacks a repository-compatible concrete parameter/evaluation profile here.
[LYWY25]

**Fit:** research only; first extract exact assumptions, error/correctness
bounds, concrete keys/ciphertexts, and whether the garbling output can be
streamed without materializing the construction.

## Cost scenarios

Running the copied Python script with the documented Thumb counts, `D=256`,
and an **illustrative only** `4 KiB` per-depth overhead gives:

| Scenario | Program/table traffic | At 1,000,000 uses |
| --- | ---: | ---: |
| Four-row, 16-byte baseline | 7.58 MiB | 7.946 B/use |
| Existing first-row-fixed stream | 5.68 MiB | 5.960 B/use |
| Ideal one-bit-per-AND payload | 15.16 KiB | 0.016 B/use |
| Leveled one-bit/AND + `256 × 4 KiB` | 1.01 MiB | 1.064 B/use |

Only the first two rows are current implementation measurements. The ideal
row omits all keys, inputs, topology, frames, and assumptions. The leveled row
is a sensitivity example: change `D` and the knob based on a construction's
actual concrete profile.

### TinyLabels remains separate

For 512 selected 16-byte inputs, direct selected-label delivery is 8 KiB; the
information-theoretic one-bit-per-label floor is 64 B. TinyLabels' published
reference profile is not a 512-label configuration: its raw public parameters
are about 32.5 MiB, reusable `ct1` about 2.375 GiB, per-use `ct2` 32 MiB, and
selection key 64 KiB. Its current benefit is a future large-batch/server reuse
experiment, not an explanation for changing the garbled-table backend.

### FHE comparison boundary

Volar's historical TFHE document records a structural LWE ciphertext shape of
`(630 + 1) * 4 = 2,524` bytes. If one naïvely materialized one such ciphertext
per Thumb AND/PBS, 124,160 ANDs imply approximately 298.86 MiB. This is **not**
a TFHE runtime, key-size, bandwidth, or security estimate: ciphertexts may be
streamed, reused, packed, or not exported at all; PBS dominates compute; and
the documented TFHE module is not a deployment construction.

It is still a useful discipline check: “one bit per gate” garbling does not
mean the corresponding FHE path has one-bit communication or cheap evaluation.
A hybrid must list every FHE ciphertext/key/transcript artifact and explain
which party evaluates which circuit.

## FHE integration decision tree

1. If FHE work needs encrypted external inputs and repeated fixed evaluation,
   first test **TinyLabels + current streaming Yao tables**. This changes only
   external selected-label delivery and has a clean fallback to direct Ferret
   OT.
2. If stored/broadcast table bytes dominate and a server/offline coordinator is
   acceptable, evaluate a **leveled Power-RLWE/HSS** candidate on a small
   independently parameterized prototype. Compare total setup, GC, input,
   key, and evaluation cost—not bits/AND alone.
3. If FHE evaluation already dominates and a reusable-GC construction requires
   garbled FHE decryption, benchmark the complete end-to-end construction
   against direct FHE evaluation and against current table reuse. Do not assume
   the hybrid inherits the better side of each primitive.
4. Keep the current TFHE Track S/V2 work separate until its own evidence ledger
   supports an exact profile. No heavy garbling construction may rely on its
   stated dimensions as a security proof.

## Required research gate before code

Before implementing any candidate beyond a cost-model prototype, capture:

- exact assumption (`LWE`, subexponential LWE, circular/small-secret LWE,
  Power-RLWE, decomposable LWE, iO, etc.);
- exact security model: single/bounded/unbounded key, reusable inputs, active
  adversary, function privacy, and output privacy;
- concrete parameter generator, correctness/failure bound, and estimate source;
- all public parameters, offline state, online encodings, evaluation keys,
  ciphertexts, and transcript/frame bytes;
- garbling and evaluation CPU/RAM, not just output size;
- compatibility with current fixed interpreter topology and `Pusher`/iterator
  streaming; and
- an independent cryptographic review plan.

A prior-art gap remains for a concrete, reviewed, embedded-compatible reusable
LWE garbling scheme with better total cost than the existing persisted stream.
That is a finding, not a reason to silently promote an asymptotic construction.

## Sources

- **[GKP+13]** Shafi Goldwasser, Yael Kalai, Raluca Popa, Vinod
  Vaikuntanathan, and Nickolai Zeldovich, *Reusable Garbled Circuits and
  Succinct Functional Encryption*, IACR ePrint 2012/733.
  https://eprint.iacr.org/2012/733
- **[GGH+13]** Craig Gentry, Sergey Gorbunov, Shai Halevi, Vinod
  Vaikuntanathan, and D. Vinayagamurthy, *How to Compress (Reusable) Garbled
  Circuits*, IACR ePrint 2013/687.
  https://eprint.iacr.org/2013/687
- **[A+16]** Prabhanjan Ananth et al., *Stronger Security for Reusable Garbled
  Circuits, General Definitions and Attacks*, IACR ePrint 2016/654.
  https://eprint.iacr.org/2016/654
- **[HLL23]** Yilei Hsieh, Huijia Lin, and Jiahui Luo, *Attribute-Based
  Encryption for Circuits of Unbounded Depth from Lattices: Garbled Circuits
  of Optimal Size, Laconic Functional Evaluation, and More*, IACR ePrint
  2023/1716. https://eprint.iacr.org/2023/1716
- **[GLOS25]** *Succinct Garbling from Homomorphic Secret Sharing*, IACR ePrint
  2025/442. https://eprint.iacr.org/2025/442
- **[LYWY25]** *Succinct Garbled Circuits with Low-Depth Garbling Algorithms*,
  IACR ePrint 2025/2308. https://eprint.iacr.org/2025/2308
- Current baseline: Cirrus
  [`THUMB_SHA256_MEASUREMENT.md`](../../../cirrus/crates/garbled-circuit/THUMB_SHA256_MEASUREMENT.md).
