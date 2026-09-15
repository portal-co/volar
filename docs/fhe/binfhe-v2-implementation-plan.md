# Plan: `binfhe` — licensed, paper-bound Boolean-FHE spec module (Track V2)

**Status:** plan + research record. It authorizes no cryptographic
implementation, parameter claim, API change, or pinnedness/stability
reclassification by itself. Any resulting code starts **Unpinned** and
**Very unstable** under [reliability.md](../reliability.md); paper-pinned
requires the binding artifacts in §10.

**Scope:** a new module family in `crates/spec/volar-spec/` (working name
`binfhe`) that **entirely replaces** the scheme and implementation of the
legacy `tfhe.rs` surface with a licensed, paper-bound GINX/CGGI construction,
plus the weaver- and interpreter-facing bootstrap-plan structure. The legacy
`tfhe.rs` remains the separate Track-S experiment
([two-track plan](tfhe-two-track-cleanup-plan.md)); this plan neither patches
it nor inherits correctness from it.

**Related records:** [two-track plan](tfhe-two-track-cleanup-plan.md) ·
[Track S evidence ledger](tfhe-steady-state-evidence.md) ·
[integer-sampled V2 draft](tfhe-mlkem-rework-draft.md) ·
[PBS rework plan](tfhe-pbs-rework-plan.md) ·
[GINX core-spec draft](tfhe-ginx-core-spec.md) ·
[multi-input PBS/weaver plan](tfhe-multi-input-pbs-weaver-plan.md) ·
[generic FHE weaver](weaver.md) ·
[metadata container plan](../metadata-container-plan.md) ·
[pipeline.md](../pipeline.md)

**Position relative to the V2 draft:** `tfhe-mlkem-rework-draft.md` left the
parameter/ring source open and considered FIPS-203-derived mechanics. This
plan makes a concrete, different selection: the construction is CGGI/GINX
over power-of-two moduli, the primary parameter profile is OpenFHE's
published **STD128** set (BSD 2-Clause), and FIPS 203 remains only a source
of integer sampling/NTT mechanics (unchanged from the draft).

---

## 1. Research: licensable implementations and parameter sources

Requirement: public, permissively licensed, **not tfhe-rs**, suitable as
parameter sources, executable references, and test-vector generators.

### 1.1 License inventory (verified against repositories, 2026-07)

| Project | License | Role for this plan |
|---|---|---|
| **OpenFHE** (`openfheorg/openfhe-development`) | **BSD 2-Clause** | Primary parameter source (`binfhe` module: FHEW/AP, TFHE/GINX, LMKCDEY). Header and param-table verified in source. |
| **`tfhe/tfhe`** (original CGGI reference library) | **Apache 2.0** | Algorithm reference for GINX bootstrapping and the reference parameter set (`n=630, N=1024, q=2^32`). |
| **`sp301415/tfhe-go`** | **Apache 2.0** | Already pinned in-repo at `a5cdbf4c…` ([reference reconnaissance](reviews/tfhe-ginx-tfhe-go-reference.md)). Convention cross-checks and future shared test vectors. |
| **TFHEpp** (`virtualsecureplatform/TFHEpp`) | Apache 2.0 (caution: optional FFTW3 dep is GPLv3) | Secondary reference; MOSFHET benchmarks use its Level-2 parameters. Do not vendor FFTW paths. |
| **MOSFHET** (`antoniocgj/MOSFHET`) | Apache 2.0 | Optimized-C reference for functional + **circuit bootstrapping** (ePrint 2022/515). |
| **LightFHE/CircuitBootstrap** | BSD 2-Clause (OpenFHE fork) | Reference implementation for WWL⁺24 circuit bootstrapping (§2.4). |
| FHEW (`lducas/FHEW`) | **GPLv2** — copyleft | Excluded as a code reference; the AP algorithm is used only from the papers. |
| nuFHE, phantom-fhe | **GPLv3** | Excluded. |
| tfhe-rs | (excluded by requirement) | Not used in any role. |

**Conclusion:** parameters and conventions are drawn from OpenFHE (BSD) and
the CGGI/TFHE reference library (Apache 2.0); algorithms from the public
papers. No copyleft material enters the spec crate.

### 1.2 Primary parameter profile — OpenFHE `STD128` (verified in source)

From `src/binfhe/lib/binfhecontext.cpp` (parameter-table row, format
`{bits, cycOrder, latParam, modq, modKS, Bks, Bg, Brk, autoKeys, keyDist, stdDev}`):

```
{ STD128, { 27, 2048, 556, 2048, 32768, 32, 128, 64, 10, UNIFORM_TERNARY, 3.19 } }
```

| Parameter | Value | Meaning |
|---|---|---|
| `n` (LWE dim) | 556 | small LWE secret dimension |
| `N` (ring dim) | 1024 | `cycOrder 2048 / 2`, ring `Z_Q[X]/(X^1024+1)` |
| `log Q` | 27 | ring ciphertext modulus `Q = 2^27` (fits `u32`, power-of-two → no NTT-modulus constraint) |
| `q` | 2048 | LWE modulus at bootstrap input (`q = 2N`, enables full-domain evaluation) |
| `modKS`, `Bks` | `2^15`, `2^5` | key-switch modulus and gadget base (3 digits) |
| `Bg` | `2^7` | RGSW/bootstrapping gadget base (ℓ = ⌈27/7⌉ = 4 levels) |
| secrets | uniform ternary | per `UNIFORM_TERNARY` |
| `σ` | 3.19 | LWE/RLWE error standard deviation |
| claim of the source | >128-bit classical security, failure ≈ 2^-135 | **to be re-validated, not adopted on faith** (§9) |

`Brk=2^6` and `autoKeys=10` belong to the LMKCDEY automorphism path and are
out of scope for the base profile.

The same `(Q=2^27, N=1024)` shape appears in Micciancio–Polyakov
(ePrint 2020/086), so the paper-bound noise analysis in our existing oracle
binding applies directly.

### 1.3 Secondary reference profile — CGGI/TFHElib

`n=630`, `N=1024`, `q=Q=2^32` (full torus), `σ≈2^-15` (LWE) / `2^-25` (BSK),
from the `tfhe/tfhe` documentation. Recorded as a cross-check profile only;
exact values must be re-verified against the pinned `tfhe-go` revision before
any vector comparison, per the convention table in the reconnaissance record.

### 1.4 Algorithm inventory (papers; no license encumbrance)

| Algorithm | Source | Use here |
|---|---|---|
| GINX gate bootstrapping, PBS, sample extract, key switch | CGGI16/18, ePrint 2018/421; MP20, ePrint 2020/086 (Table 1 gate certificates) | Core Boolean gates + programmable bootstrap. MP20 certificates are already paper-bound in `tfhe_ginx_oracle.rs`. |
| Multi-input PBS (affine input combination + one blind rotation) | folklore, formalized in Carpov ePrint 2024/1204 (Boolean-circuit → functional-bootstrap mapping) | Weaver's aggressive cone fusion; lifts the legacy `ADDR_BITS ≤ 2` restriction through a reviewed selector. |
| Multi-value PBS (many LUTs, one blind rotation) | CIM19, CT-RSA 2019, ePrint 2018/622 | Amortize blind rotation across LUT outputs. |
| **Circuit bootstrapping** (LWE→RGSW) | CGGI17, ASIACRYPT 2017 | RGSW outputs for external-product composition (CMUX trees, RGSW wires). |
| Circuit bootstrapping, ring-only redesign | WWL⁺24, EUROCRYPT 2024, ePrint 2024/323 (Wang, Wen, Li, Lu, Wei, Liu, Wang) — 9.9× speedup, 15.6× smaller keys | **Optimization phase only**, with the corrected error analysis of ePrint 2024/1318 (Wang, Ha, Shen, Lu, Chen, Lee), whose noise-amplification and failure-probability corrections are mandatory. |
| WoP-PBS, FDFB variants | CLOT21 ePrint 2021/729; TCHES 2023 ePrint 2021/1135 | Out of scope for V2.0; the `q = 2N` profile keeps the door open. |
| Automorphism (LMKCDEY) bootstrapping | ePrint 2022/198 | Out of scope for V2.0. |

---

## 2. Decision record

1. **New module, new names.** The replacement lands as
   `volar_spec::binfhe` with `binfhe_*` free functions. It does not reuse,
   shim, or rename `tfhe.rs` items. Track S is untouched until §11's
   retirement gate.
2. **Construction:** CGGI/GINX over power-of-two moduli (u32 wrapping
   arithmetic), GINX blind rotation, gadget-decomposed RGSW external
   products, MP20-style gate certificates. Canonical Boolean wire encoding
   `{0, q/4}` with signed `±q/8` bootstrap outputs restored to canonical —
   exactly the semantics the repo's MP20-bound clear oracle models.
3. **Parameters as typed profiles** (§4), primary = OpenFHE STD128.
4. **Programmable bootstrapping is the primary gate primitive.** Every
   Boolean gate is a LUT; multi-input cones collapse to one blind rotation
   where the reviewed selector admits it (§7).
5. **Circuit bootstrapping (CGGI17 first; WWL⁺24+2024/1318 as a gated
   optimization)** exposes RGSW wires so the weaver can emit external-product
   composition without re-bootstrapping (§7.3).
6. **The bootstrap-fusion logic exists once, as data.** A serializable
   `BootstrapPlan` (§8) is produced by the weaver and consumed identically by
   generated code and by the `volar-dyn`/`volar-spec-dyn` interpreters.
7. **Integer-only, deterministic, `#![no_std]` + `alloc`, `SpecRng`-driven.**
   Noise is sampled by an integer CDT/CBD sampler whose tables are generated
   by a documented deterministic procedure (FIPS 203 §4 mechanics are the
   style reference; no floating point anywhere).
8. **Security claims are gated** (§9): the STD128 profile ships labeled
   "parameter source: OpenFHE, unvalidated by Volar" until the validation
   steps run.

## 3. Module architecture (`crates/spec/volar-spec/src/binfhe/`)

```
binfhe/
  mod.rs            // module docs, markers, re-exports
  params.rs         // Params trait + profiles (§4)
  torus.rs          // modular arithmetic over u32 with const LOG_MODULUS
  sampler.rs        // deterministic integer Gaussian/CDT + ternary samplers (SpecRng)
  lwe.rs            // LweCiphertext<P>, encrypt/decrypt, linear ops, NOT, trivial
  rlwe.rs           // RlweCiphertext<P>, poly arithmetic, sample extract
  gadget.rs         // signed/rounded gadget decomposition (BS and KS flavors)
  rgsw.rs           // RgswCiphertext<P>, external product, RLWE CMUX
  modswitch.rs      // modulus switching q ↔ 2N, round-to-nearest
  blind_rotate.rs   // GINX blind rotation over the accumulator
  pbs.rs            // programmable bootstrap + validated LUT tables (multi-input)
  circuit_bs.rs     // CGGI17 circuit bootstrap (LWE → RGSW); WWL⁺ behind a feature-gated profile flag
  keys.rs           // secret keys, bootstrapping key, key-switching key, circuit-bs key
  lut.rs            // Lut<P> validated tables, multi-value LUT bundles (CIM19)
  plan.rs           // BootstrapPlan data model (§8)
```

Design rules carried over from repo policy and learned from Track S:

- **Typed wires.** `LweCiphertext<P: Params>` etc. carry the profile at the
  type level; key types are keyed by the same `P`, so cross-profile mixing is
  a compile error (the legacy `u32`-soup failure mode is designed out).
- **No raw phase exposure.** No public function returns or accepts an
  unencoded torus phase; the canonical-encoding invariant is enforced by
  construction and checked in tests.
- **`no_std` + `alloc`; no `rand`;** all randomness through `SpecRng`
  (lib.rs rule 5). Keys and ciphertexts are fixed-shape via const generics on
  the profile, matching the static-shapes requirement of the compiler.
- **Schoolbook first.** O(N²) negacyclic convolution for V2.0 (spec
  clarity); a negacyclic NTT for `Q=2^27`-shaped profiles is a later,
  separately-tested optimization (FIPS 203-style mechanics draft applies).

## 4. Parameter profiles

```rust
pub trait Params {
    const N_LWE: usize;   const BIG_N: usize;      // 556 / 1024 for Std128
    const LOG_Q: u32;     const LOG_Q_LWE: u32;    // 27 / 11 (q = 2048)
    const LOG_MOD_KS: u32; const KS_BASE_LOG: u32; const KS_ELL: usize;
    const BS_BASE_LOG: u32; const BS_ELL: usize;   // 7 / 4
    const SIGMA_MILLIS: u32;                       // 3.19 scaled, integer only
    // + failure-probability budget constant recorded for the plan metadata
}
```

| Profile | Purpose | Security claim |
|---|---|---|
| `params::Toy` | compile-and-run tests; tiny exact-decomposition dims, optional zero noise | none (explicitly labeled) |
| `params::ToyNoisy` | failure-count and noise-budget tests at small scale | none |
| `params::Std128` | the §1.2 OpenFHE set | gated per §9 |
| `params::CggiRef` (later) | §1.3 cross-check profile vs `tfhe-go` | gated per §9 |

## 5. Spec-crate subset required for compilation

The compiler-facing surface (what the weaver and interpreters need), all in
`binfhe`:

- keygen: LWE/RLWE secret keys, bootstrapping key (RGSW of each LWE key bit),
  key-switching key, circuit-bootstrapping key;
- encrypt/decrypt (host-side, for tests and interpreters);
- free gates: trivial encrypt, NOT (linear), LWE add/sub by cleartext;
- **PBS family:** single-bit identity/AND/OR/XOR/… as validated LUTs;
  multi-input LUT read (`binfhe_lut_read`) with the reviewed generalized
  selector (arbitrary `ADDR_BITS` subject to capacity `2·BIG_N` and the
  profile's error budget); multi-value LUT bundles;
- **circuit bootstrap:** `binfhe_circuit_bootstrap` (LWE → RGSW) and the
  RGSW external product / CMUX as public typed operations;
- oblivious read/write building blocks matching `FheScheme`'s
  `emit_oblivious_read/write` expectations (MUX-tree over PBSD wires);
- `plan.rs` (§8) plus a `execute_plan` reference interpreter in the spec
  crate (this is the oracle both the weaver's generated code and the dyn
  interpreters differential-test against).

## 6. Test plan — same shape, entirely new implementation

Mirrors the legacy suite's *categories* with new code and the new profiles.
Per repo rule, correctness signal = real behavior, never syntactic IR
analysis.

1. **Roundtrip/truth tables:** encrypt→decrypt; all 1- and 2-input gates on
   `Toy`, exhaustive over inputs × fixed seed corpus, checking **decrypted
   value and exact canonical phase** (zero-noise profile).
2. **Independent-formula conformance** (each check recomputed by a
   separately written clear model, never the helper under test):
   poly-rotate vs schoolbook monomial multiply; blind rotation on an exact
   torus grid vs clear rotation + clear decryption; sample-extract vs direct
   RLWE decryption (2020/086 p.11 identity); key-switch roundtrip across
   dimensions; signed pre-restoration bootstrap outputs vs MP20 Table 1
   certificates — reusing the **existing paper-bound clear oracle**
   (`tfhe_ginx_oracle.rs`, generic over power-of-two `q`) as the reference.
   The oracle is lifted from `#[cfg(test)] mod` into a shared test-support
   module so V2 tests bind to it directly; this is the usage it was written
   for, not Track-S inheritance.
3. **PBS conformance:** multi-input LUT reads (2..=6 address bits on `Toy`)
   vs brute-force cleartext table evaluation; negacyclic-capacity and
   selector-validation rejections; multi-value bundles vs per-LUT PBS.
4. **Circuit bootstrap:** LWE→RGSW roundtrip — external product of the
   resulting RGSW with trivial RLWE operands decrypts to the selected
   operand; CMUX-tree equivalence vs PBSD CMUX.
5. **Composable-DAG fuzzing (proptest):** random gate DAGs over encrypted
   inputs, compared to a plaintext oracle at every intermediate, on `Toy`
   (exact) and `ToyNoisy` (statistical, seeded, shrinkable — failure rate
   must stay under the profile's recorded budget).
6. **Noise-budget tests:** nonzero-σ runs asserting observed failure counts
   are consistent with the profile's stated per-bootstrap failure
   probability (binomial bound over the seed corpus).
7. **Cross-implementation vectors (Gate C material, reconnaissance only):**
   deterministic-tape ciphertext/key pairs compared byte-level against the
   pinned `tfhe-go` revision for the `CggiRef` profile, and against
   OpenFHE-generated vectors for `Std128`; recorded exactly like the
   existing reconnaissance record — an executable compatibility check, not
   an interop claim until reviewed.
8. **Compiler tests:** the emitted weaver path lowers and compiles through
   `print_module` → `rustc` and runs against the spec reference interpreter
   (repo rule 2); the dyn mirror is regenerated via `crates/spec/generate.sh`
   and differential-tested against `execute_plan`.

## 7. Weaver integration — the aggressive bootstrap optimizations

New `BinFheScheme` in `crates/compiler/volar-weaver/src/fhe.rs` implementing
the existing `FheScheme` trait (no trait changes needed for V2.0), coexisting
with `TfheScheme` until §11.

1. **Cone-fused multi-input PBS.** A pass over the lowered Boolean network
   finds maximal cones whose input count and table fit the reviewed
   selector/capacity/error budget, and emits one blind rotation per cone
   (Carpov 2024/1204 mapping heuristic). This replaces the legacy
   3-bootstrap XOR and the AND/OR-per-gate emission; every fusion is
   justified by a recorded equivalence check against the unfused cone.
2. **Multi-value PBS (CIM19).** Cones sharing all inputs and differing only
   in output tables are batched into one blind rotation with per-output
   sample extraction — the blind rotation is the dominant cost, so
   same-input LUT bundles are near-free additional outputs.
3. **Circuit bootstrapping for RGSW wires.** Where a value fans out into
   many CMUX/external-product positions (oblivious read/write address bits,
   branch selectors), the weaver emits one `binfhe_circuit_bootstrap` and
   reuses the RGSW ciphertext, instead of one PBS per use. V2.0 uses CGGI17
   (level-by-level PBS construction, one circuit-bootstrapping key); the
   WWL⁺24 ring-only redesign — **with the 2024/1318 corrected noise
   analysis** — is a later profile-flagged optimization with its own binding
   and tests.
4. **Layered scheduling.** Bootstrap ops are scheduled into parallel layers
   (fixed shapes, no runtime discovery — per the static-shapes rule), and
   the layer schedule is emitted into the `BootstrapPlan` (§8).
5. **Budget accounting.** Every emitted bootstrap decrements a
   failure-probability budget recorded in the plan; a circuit that exceeds
   the profile's budget is a compile-time error, not a silent risk.

## 8. `BootstrapPlan` — the shared plan structure for interpreters

The weaver's fusion/scheduling logic is exported as **data**, defined once
in `volar_spec::binfhe::plan` (`no_std` + `alloc`, `rkyv`-compatible, no
`TypeId`/stringly maps — per the metadata-container plan's non-goals):

```rust
pub enum BootstrapOp<W> {          // W = wire id
    Lut { inputs: SmallVec<W>, table: LutId, out: W },
    LutMany { inputs: SmallVec<W>, tables: SmallVec<LutId>, outs: SmallVec<W> },
    CircuitBootstrap { input: W, out: W },               // LWE wire → RGSW wire
    ExternalProduct { rgsw: W, rlwe_a: W, rlwe_b: W, out: W },
    // free linear ops are explicit too, so the plan is self-contained
}
pub struct BootstrapPlan {
    pub layers: Vec<Vec<BootstrapOp<WireId>>>,  // topological layers
    pub luts: Vec<LutSpec>,                     // validated logical tables
    pub budget: FailureBudget,                  // per-op and total failure bound
    pub profile: ProfileId,                     // Std128 | Toy | …
}
```

- **Weaver** consumes the plan to generate code (typed `IrExpr` calls into
  `binfhe_*`, never raw strings — repo rule 1).
- **Interpreters** (`volar-dyn`, `volar-spec-dyn`) consume the same plan to
  execute against the dyn runtime; the spec's `execute_plan` is the shared
  oracle. Plan serialization is deterministic so a plan hash identifies the
  exact scheduled computation in test evidence.
- **Metadata carriage:** the plan is module-level metadata attached through
  the per-node metadata container / instruction-group machinery
  ([metadata container plan](../metadata-container-plan.md)), preserved by
  default through passes, and remapped on cloning/inlining by the existing
  remapping APIs.

## 9. Security-parameter validation gate (before any claim)

The STD128 profile is recorded with its source and is **not** a Volar
security claim until: (a) `(n=556, σ=3.19, q=2048)` ternary-LWE and
`(N=1024, Q=2^27)` RLWE are re-estimated with the current Lattice Estimator
and the run logged as evidence; (b) the composite per-bootstrap failure
probability is recomputed from our decomposition/modulus choices (including
the 2024/1318 corrections if WWL⁺ is enabled); (c) the recorded budget in
`BootstrapPlan` matches the recomputation. The Homomorphic Encryption
Standard v1.1 tables are context, not a substitute for the estimator run.
This reclassification is a human decision under reliability.md.

## 10. Milestones and gates

| # | Milestone | Gate to proceed |
|---|---|---|
| M1 | `params`, `torus`, `sampler`, `lwe` (+ Toy roundtrip tests) | tests green; module markers `unpinned`/`very-unstable` |
| M2 | `gadget`, `rlwe`, `rgsw`, `modswitch`, `blind_rotate`, `keys` | §6.2 independent-formula tests green |
| M3 | `pbs`, `lut` (single- and multi-input) | §6.1, §6.3, §6.5 green; MP20-oracle binding doc for V2 written |
| M4 | `circuit_bs` (CGGI17) + external product surface | §6.4 green; CGGI17 binding doc written |
| M5 | `BootstrapPlan` + `execute_plan` reference interpreter | plan roundtrip (serialize/execute) tests green |
| M6 | `BinFheScheme` weaver + cone-fusion pass | generated code compiles and runs vs `execute_plan`; budget enforcement test |
| M7 | dyn regeneration + interpreter plan execution | differential tests spec vs spec-dyn green |
| M8 | `Std128` profile + noise-budget suite | §6.6 green; §9 estimator evidence logged |
| M9 | (optional) WWL⁺24 circuit-bs optimization | 2024/1318-corrected analysis documented; equivalence + noise tests green |
| M10 | (optional) cross-implementation vectors | reconnaissance record in the existing style |

## 11. Legacy retirement (separate decision)

Once M6–M8 hold, propose: switch default FHE weaving to `BinFheScheme`,
regenerate spec-dyn without the legacy surface, and remove or archive
`tfhe.rs` per the two-track plan's Track-S procedures. That removal is its
own recorded decision; nothing here silently deletes Track-S material.

## 12. Non-goals and risks

**Non-goals (V2.0):** WoP-PBS/FDFB, LMKCDEY automorphisms, GPU/SIMD, NTT,
public-key encryption, packed/leveled arithmetic beyond Boolean wires + the
listed oblivious-access blocks.

**Risks (tracked):**
- *Paper-transcription errors in CGGI17/WWL⁺ circuit bootstrapping* →
  binding docs precede implementation (M4/M9 gates); 2024/1318 corrections
  are mandatory reading, since the original WWL⁺ failure analysis was shown
  optimistic.
- *Multi-input selector error growth* → the generalized selector is admitted
  only with its own error analysis and capacity check in `lut.rs`; the
  legacy `ADDR_BITS > 2` rejection documented in `TfheBootstrapTable` is the
  cautionary precedent.
- *Parameter drift from sources* → profiles record exact source URL +
  revision + retrieval date; OpenFHE main-branch values are pinned to a
  commit hash when the `Std128` profile lands.
- *Integer Gaussian sampler bias* → sampler tables are generated by a
  documented deterministic procedure with goodness-of-fit tests against the
  target σ in the test suite.
