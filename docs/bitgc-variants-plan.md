# BitGC-fast / BitGC-small / BitGC-tiny as an optional garbling substitute

**Status:** implementation in progress, 2026-10-08. Phase 0 (pinning setup,
ledger, ADR `docs/adr/0003-bitgc-ring-substrate.md`) committed. **Structural
finding (2026-10-08, artifact read):** the real construction is HSS-based with
per-gate distributed decryption and 1-bit determination, not the
self-contained SWHE table assembly the current `ReferenceEquation` implements
— see `docs/research/bitgc-artifact-notes.md`. The reference adapter is a
transport/stream placeholder to be replaced, per BITGC-LEDGER-04. `bitgc-tiny`
is deferred by maintainer decision (ledger row BITGC-LEDGER-06). Maintainer
constraint: no external libraries in `volar-spec`; all ring/SWHE arithmetic
is in-crate (ADR 0003). The ePrint PDFs were not retrievable from this host
(HTTP 403), so per-gate equations ship behind a `GateEquation` seam pending
pinning evidence (ledger row BITGC-LEDGER-04). This document does not
change the default garbling scheme, the streaming table format, or any
deployment profile. It plans an *optional, opt-in* substitute for the
half-gate garbled-circuit path in Volar and Cirrus.

Companion codebases at worktree tips when written:

- Volar: `Code-local/portal-hot/mpc/volar` @ `5306e25` (`feat/mpc`)
- Cirrus: `Code-local/portal-hot/mpc/cirrus` @ `4708087` (`feat/mpc`)

---

## 1. Paper references and terminology

All paper citations below use stable short keys. "Paper-pinned" status for
an implementation element means: the element's construction is taken from a
cited section/figure and is recorded here with that citation, its parameters
match the paper's stated profile, and its correctness evidence is recorded
in §11. Pinning is achieved element-by-element by filling the tracking table
in §10.

### Primary references

- **[ZLL+26]** Wenhao Zhang, Hanlin Liu, Kang Yang, Wen-jie Lu, Yu Yu,
  Xiao Wang, Chenkai Weng. *BitGC Made (More) Efficient*. EUROCRYPT 2026,
  pp. 214–245, DOI 10.1007/978-3-032-25324-8_8. IACR ePrint **2025/1810**
  (<https://eprint.iacr.org/2025/1810>). **This is the defining paper for
  all three variants in this plan.**
  - **BitGC-small: §4** (detailed construction in Figures 4 and 5).
  - **BitGC-fast: §5.1.**
  - **BitGC-tiny: §5.2.**
- **[LWYY25b]** Hanlin Liu, Xiao Wang, Kang Yang, Yu Yu. *BitGC: Garbled
  Circuits with 1 Bit per Gate*. EUROCRYPT 2025, pp. 437–466, DOI
  10.1007/978-3-031-91098-2_16. IACR ePrint **2024/1988**
  (<https://eprint.iacr.org/2024/1988>). The original rate-one scheme all
  three variants optimize.
- **[ZLL+26-A]** Authors' artifact:
  <https://github.com/wtd2/more-efficient-bitgc>. C++17, Microsoft SEAL
  4.1.x + Intel HEXL, Bristol-format circuits. Exposes `--variant=fast|small`
  (no `tiny` binary), with a correctness-only low-memory profile
  (`--offline-dim=9 --online-dim=8`, explicitly *not* 128-bit secure).

### Supporting references

- **[LWYY25a]** Liu, Wang, Yang, Yu. *Authenticated BitGC for Actively
  Secure Rate-One 2PC*. CRYPTO 2025, pp. 652–687; ePrint **2025/232**.
  Out of scope for this plan (semi-honest only) but the natural follow-up.
- **[DLL24]** Dietz, Li, Lin. *TinyLabels*. EUROCRYPT 2025; ePrint
  **2024/2048**. Referenced only because both repos already carry a
  TinyLabels cross-repo plan whose seam and module-division discipline this
  plan deliberately reuses (see §4.1).
- **[ZRE15]** Zahur, Rosulek, Evans. *Two Halves Make a Whole*. EUROCRYPT
  2015. The half-gate baseline currently implemented in
  `volar-spec/src/garble.rs` that this plan's schemes may substitute for.

### What each variant is (from [ZLL+26] Table 1 and §§4–5)

| Variant | Paper section | Comm. per AND | Comm. per XOR | AES-128 GC size | AES-128 garble time (1 core) |
|---|---|---|---|---|---|
| BitGC [LWYY25b] | — | 1 bit | 1 bit | 4.3 KB | ≥ 14 h (est.) |
| **BitGC-small** | §4 | 1 bit | 1 bit | 4.3 KB | ~345 s (paper benchmark; Lacuna summary reports 91 min — see §11 risk) |
| **BitGC-fast** | §5.1 | 5 bits | 0 bits | 4.0 KB | ~116 s |
| **BitGC-tiny** | §5.2 | 1 bit | 0 bits | 0.8 KB | ≥ 56 min |

(AES-128 circuit: 6,400 AND + 28,176 XOR gates.)

The three enabling techniques, all from [ZLL+26]:

1. **SIMD-enabled SWHE.** The original BitGC requires an even plaintext
   modulus (because the permutation bit is the LSB of the zero label),
   which blocks NTT-friendly prime moduli and thus SIMD batching. [ZLL+26]
   decouples permutation bits from labels (storing them separately), so any
   ciphertext space — including SIMD/NTT-friendly prime-modulus rings — can
   be used. *Applies to all three variants.*
2. **Free-XOR for BitGC.** A way to make XOR gates zero-communication
   inside the BitGC arithmetic framework by ensuring each XOR output's
   permutation bit equals the XOR of the input permutation bits.
   *Used by bitgc-fast and bitgc-tiny.*
3. **5-bit fast construction.** A variant that spends 5 bits per AND gate
   to eliminate *all* homomorphic multiplications from the online garbling
   phase. *This is bitgc-fast; bitgc-small keeps the online multiplications
   to stay at 1 bit/AND.*

Common cryptographic base for all three (inherited from [LWYY25b]):
**Ring-LWE with key-dependent-message (KDM / circular) security**; the
garbled circuit = one SWHE-encrypted seed (homomorphically expanded by a
low-depth PRG into encryptions of many pseudorandom bits) plus a per-gate
"stitching" value (1 bit, or 5 bits for fast) from which the evaluator
homomorphically reconstructs the garbled table. Security model: semi-honest.

Artifact parameter shape (from [ZLL+26-A] `e2e_benchmark.cc`, informative
— the paper's own parameter table must be pinned before implementation,
see Phase 0):

| Variant | Offline ring degree (N_PRG) | Online ring degree (N_Gate) | Offline levels | Online levels | Coeff. modulus |
|---|---|---|---|---|---|
| fast | 2^15 | 2^13 | 10 | 3 | 60-bit primes, 11 limbs offline |
| small | 2^15 | 2^14 | 13 | 5 | 60-bit primes, 14 limbs offline |

One generated 60-bit prime doubles as the plaintext modulus; the online
context is derived from the offline context after PRG expansion and modulus
switching.

---

## 2. Current state of the two codebases

### 2.1 Volar (`mpc/volar` @ 5306e25)

- **Spec-level half-gate garbling**: `crates/spec/volar-spec/src/garble.rs`
  (389 lines). Defines `Garble<N>` / `Eval<N>` / `GarbleTable<N>` /
  `GlobalSecret<N>` over `VoleArray<u8>` byte labels, plus
  `GarbledCircuit`/`EvalSetup` multi-evaluation types and the GRAM action
  gadgets. Marked `@reliability: experimental`. This is the half-gate
  [ZRE15] construction over VOLE-typed labels.
- **Weaver passes**: `crates/compiler/volar-weaver/src/garble.rs` emits
  evaluator/garbler `IrModule`s from `BIrBlocks` (`weave_evaluator`,
  `weave_garbler`, `weave_into_gc`, `weave_eval_from_setup`, and
  `*_bounded` variants). Generic over `<N: ArraySize, D: Digest>`. The
  pipeline is documented in `docs/garbling-pipeline.md`.
- **Lattice precedent**: `crates/spec/volar-spec/src/tinylabels/` already
  contains a `no_std + alloc` Ring-LWE implementation
  (`tinylabels/ring_lwe.rs`: staged `setup`/`enc1`/`enc2`/`keygen`/`dec`
  with caller-supplied `RandomSource`/`NoiseSource`, deliberate
  no-wire-format and no-production boundaries). This is the strongest local
  evidence that a `volar_spec::bitgc` module with the same discipline is
  feasible. There is **no** NTT/SIMD batching in the local Ring-LWE code;
  it is per-coefficient schoolbook arithmetic (verify before reuse —
  Phase 0 task).
- **Provider plumbing ledger**: `docs/fhe/future-provider-integration-ledger.md`
  is the authoritative deferred-check ledger. Its row **FHE-PLUMB-TOOLCHAIN-03
  ("Heavy-garbling LLVM artifact import")** is directly on point for BitGC:
  "A practical, reviewed heavy-garbling implementation" is a listed
  prerequisite. Per the repo AGENTS.md, this ledger **must be read and
  updated** before adding heavy-garbling imports or deferred provider
  tests, with `TODO(provider-ledger: ID)` comments at each deferral.
  This plan adds rows in Phase 0 (§10).
- **Input-label seam**: `crates/mpc/volar-mpc/src/input_labels.rs`
  (`DirectOtDelivery` default; `TinyLabelsServer` reserved) and
  `tinylabels_delivery.rs` (resource admission shapes). BitGC changes
  table/circuit delivery, *not* input-label delivery — but the manifest /
  admission / explicit-profile discipline there is the model to copy.

### 2.2 Cirrus (`mpc/cirrus` @ 4708087)

- **Streaming baseline**: `crates/garbled-circuit/cirrus-garbled-circuit`
  (`GC` pusher-side garbler + pull-based `Evaluator`, `no_std`,
  `GarblingRecord::Table([[u8; N]; 4])`, one record per AND, XOR free) and
  `cirrus-garbled-circuit-row-reduced` (3-row tables). Both implement the
  `cirrus_core` `ContextWith*` traits so `cirrus-ert` / LLVM interpreters
  run over them. Locked traffic baselines: Thumb SHA-256 = 164,288 tables
  (10.5 MB), RV32 = 429,216 tables (27.5 MB) at 16-byte labels.
- **Volar-backed backend**: `crates/volar/cirrus-volar-garble`
  (`VolarGarbleBackend` / `VolarEvalBackend`) mirrors the same
  streaming/pull shape but delegates crypto to `volar_spec::garble`.
  **This crate is the natural Cirrus home for a BitGC backend**: a
  `BitGcGarbleBackend` / `BitGcEvalBackend` pair that delegates to a new
  `volar_spec::bitgc` module, exactly as the existing backend delegates to
  `volar_spec::garble`.
- **TinyLabels precedent**: `crates/garbled-circuit/cirrus-garbled-circuit-tinylabels`
  and `crates/garbled-circuit/TINYLABELS_CROSS_REPO_PLAN.md` establish the
  deployment-split table (embedded rejects the heavy mode at profile
  construction; server opt-in only) and the "shared core in Volar, thin
  adapters in each repo" division. BitGC inherits both.
- **Workspace patch**: Cirrus consumes `volar-spec` via
  `[patch."https://github.com/portal-co/volar.git"]` pointing at
  `../volar/crates/spec/volar-spec`, so a new `volar_spec::bitgc` module is
  immediately visible to Cirrus. (Note: the mpc workspace itself carries no
  `.git`; each repo is its own checkout/worktree.)

### 2.3 Gap analysis

| Needed for BitGC | Status |
|---|---|
| NTT-friendly prime-modulus ring arithmetic (SWHE with SIMD slots) | **Absent.** `tinylabels/ring_lwe.rs` is schoolbook, even/large-modulus oriented. New `volar_spec::bitgc::ring` (or reused reviewed crate — decision in Phase 0) required. |
| Low-depth PRG evaluated homomorphically (seed expansion) | **Absent.** Needs a paper-pinned PRG choice [LWYY25b] + Boolar/volar-spec circuit realization. |
| Odd/even modulus handling decoupled from permutation bits | **Absent** (the [ZLL+26] redesign). |
| Bit-level stitching record format (1 or 5 bits/gate) | **Absent.** `GarblingRecord` has only 4-row/3-row table variants. |
| Odd-modulus / SIMD slot encode+decode of labels | **Absent.** |
| Free-XOR delta inside the arithmetic framework | Present in spirit (`GlobalSecret`) but the BitGC version is a ring element Δ ∈ R_p with LSB constraint per [ZLL+26] — new code. |
| Semi-honest 2PC session plumbing (OT for input labels, transport) | **Present** (`volar-mpc` OT/Ferret, `input_labels.rs`, Cirrus `miniot`). Reused unchanged. |

---

## 3. Scope and non-goals

**In scope**

- Three new `no_std + alloc` construction modules in `volar-spec` —
  `volar_spec::bitgc::{common, small, fast, tiny}` — implementing the
  paper's garbling/evaluation arithmetic with caller-supplied randomness
  and noise samplers, mirroring `tinylabels` discipline.
- Cirrus `cirrus-volar-garble` backends exposing the schemes through the
  existing `cirrus_core::Context` streaming seam.
- Volar weaver support so `BIrBlocks` circuits can be emitted as
  bitgc-garbler/evaluator IR (or, at minimum, so the circuit geometry —
  AND/XOR counts, topology — feeds the offline seed sizing).
- Explicit opt-in profile plumbing in both repos; embedded targets reject
  the mode at profile construction.
- Conformance vectors against the authors' artifact [ZLL+26-A]
  (`circuits/sub64.txt`, `--offline-dim=9 --online-dim=8` correctness
  profile) as the interoperability oracle.
- Ledger updates in `docs/fhe/future-provider-integration-ledger.md`.

**Out of scope (explicitly)**

- Active security / Authenticated BitGC [LWYY25a].
- Replacing or deprecating the half-gate path. BitGC is a substitute
  selected by profile, never a default.
- Embedded (ERT/MCU) execution of BitGC garbling or evaluation. The
  artifact's own AES profile needs 120–200 GiB RAM; even scaled-down, the
  online phase is SWHE arithmetic — categorically off the 256 KiB RAM /
  2 MiB flash profile, same as TinyLabels.
- NTRU-based instantiation (the [LWYY25b] alternative assumption). RLWE
  only, matching [ZLL+26].
- Bootstrapping/FHE reuse of `binfhe`; BitGC uses leveled SWHE with a fixed
  multiplication depth and no bootstrapping.

---

## 4. Architecture

### 4.1 Module division (mirrors the TinyLabels cross-repo split)

```text
volar (source of truth, no_std + alloc, construction seam)
  crates/spec/volar-spec/src/bitgc/
    mod.rs        – profile types, PaperProfile-style constants, security
                    boundary docs, variant enum, stitching-record codec
    ring.rs       – NTT-friendly prime-modulus ring arithmetic, SIMD slot
                    encode/decode, modulus-switch chain (offline→online)
    prg.rs        – low-depth PRG circuit for homomorphic seed expansion
    seed.rs       – encrypted-seed setup, expansion driver, slot extraction
    common.rs     – permutation-bit bookkeeping decoupled from labels
                    [ZLL+26 §3/§4 preamble], Δ ∈ R_p handling, free-XOR rule
    small.rs      – BitGC-small garble/eval [ZLL+26 §4, Figs. 4–5]
    fast.rs       – BitGC-fast garble/eval, 5-bit AND records [ZLL+26 §5.1]
    tiny.rs       – BitGC-tiny garble/eval [ZLL+26 §5.2]
  crates/mpc/volar-mpc/src/bitgc_delivery.rs
                  – session profile selection, manifest, resource admission
                    (clone the shape of input_labels.rs / tinylabels_delivery.rs)

cirrus (thin adapters, interpreter seam)
  crates/volar/cirrus-volar-garble/src/bitgc.rs
                  – BitGcGarbleBackend / BitGcEvalBackend implementing the
                    cirrus_core Context traits, delegating to volar_spec::bitgc
  crates/garbled-circuit/cirrus-garbled-circuit (untouched baseline)
```

Rules carried over from the TinyLabels plan, restated for BitGC:

1. **One mathematical implementation.** All ring arithmetic, PRG, stitching
   logic, and parameter validation live in `volar_spec::bitgc`. Cirrus
   never forks them; it consumes via the existing workspace patch.
2. **Caller-supplied randomness.** `RandomSource`/`NoiseSource` traits, a
   `ZeroNoise` test-only sampler, and a loud doc boundary — exactly the
   `tinylabels/ring_lwe.rs` pattern.
3. **No wire format in the core module.** Typed in-memory values only;
   canonical stage-frame codecs live in a separate `frame.rs` (as
   `tinylabels/frame.rs` does) and are versioned.
4. **Streaming preserved.** BitGC changes *what* is in a record (1-bit or
   5-bit stitching values, batched) and adds an offline seed phase; it does
   not change the pusher/pull-evaluator shape. Batched bit-records are
   packed into byte frames at the transport adapter, not in the core.
5. **Explicit opt-in only.** A `GcScheme::{HalfGate, BitGcSmall, BitGcFast,
   BitGcTiny}` profile enum; the default stays `HalfGate`; the BitGC
   variants are rejected on `target_os = "none"` and embedded profiles at
   construction time.

### 4.2 Protocol shape introduced by BitGC (both repos)

Unlike half-gate, BitGC has a genuine **offline/online split**:

```text
OFFLINE (per circuit geometry, reusable across evaluations):
  garbler: sample seed s, SWHE-encrypt → ct_seed          [LWYY25b]
  both:    homomorphically evaluate low-depth PRG on ct_seed
           → encrypted pseudorandom bits for every wire   [LWYY25b]
  garbler→evaluator: the expanded ciphertext material (or
           evaluator computes it itself from ct_seed)

ONLINE (per garbling):
  garbler: for each AND gate, emit stitching record
           (1 bit small/tiny, 5 bits fast)                [ZLL+26 §4, §5]
  evaluator: reconstruct each gate's table homomorphically
           from expanded material + stitching bits,
           then evaluate on selected input labels
```

Consequences for integration:

- The **seed size depends on circuit geometry** (number of expanded bits ∝
  wire count), so the weaver/`BIrBlocks` topology must be available to the
  offline phase — Cirrus gets this from fixed interpreter traces, Volar
  from the `BIrBlocks` itself.
- The streaming contract changes granularity: instead of one 16-byte table
  per AND gate, the online stream is a **bit-packed stitching stream**
  (1/8 byte per AND for small/tiny, 5/8 byte for fast) plus the offline
  ciphertext material. New `GarblingRecord` variants belong in the *bitgc*
  adapter crates, not in the baseline crates.
- Evaluation is **homomorphic**, not table-lookup: the evaluator runs ring
  arithmetic per gate. This is why BitGC backends are server-class only
  and why the pull-based `Evaluator` shape needs a ring-context handle.

### 4.3 Volar weaver integration

Two integration depths, phased:

- **Phase 2 (minimum): geometry export.** `volar-weaver` already walks
  `BIrBlocks`; add `bitgc_geometry(circuit) -> BitGcGeometry { and_gates,
  xor_gates, wires, inputs, outputs }` plus a stitching-stream emitter that
  matches circuit order. The crypto runs in `volar-spec`; the weaver only
  emits the host calls.
- **Phase 4 (full): woven bitgc evaluator/garbler IR.** New passes
  `weave_bitgc_garbler` / `weave_bitgc_evaluator` next to the existing
  `weave_garbler` / `weave_evaluator` in
  `crates/compiler/volar-weaver/src/garble.rs` (or a new `bitgc.rs`
  sibling), emitting `IrModule`s that call `volar_spec::bitgc` per gate
  instead of `Garble::and_via_table`. These remain generic over the ring
  profile, mirroring how the current passes are generic over
  `<N: ArraySize, D: Digest>`.

### 4.4 Cirrus integration

- New `crates/volar/cirrus-volar-garble/src/bitgc.rs`:
  `BitGcGarbleBackend<'a, 'b, V: BitGcVariant, P>` implementing
  `ContextWithBitAnd`/`ContextWithBitXor`/`ContextWithBitOr` etc., with
  `queue: &mut dyn Pusher<BitGcRecord>` — same streaming discipline as
  `VolarGarbleBackend`. XOR is handled per-variant: charged 1 bit under
  small, free under fast/tiny (per [ZLL+26] Table 1 — the backend must not
  hard-code freeness).
- The pull evaluator (`BitGcEvalBackend`) consumes ordered `BitGcRecord`s
  and holds the online ring context. Existing interpreters (`cirrus-ert`,
  LLVM frontend) then run unchanged over the new context — that is the
  point of implementing the `Context` traits.
- ERT/embedded: the `GcScheme` profile rejects BitGC; the existing ERT
  self-tests (Thumb/RV32 SHA-256 locked baselines) keep running on
  half-gate and are the regression fence.

---

## 5. Phased implementation plan

### Phase 0 — Paper pinning and ledger setup (no crypto code)

0.1. Obtain and archive the [ZLL+26] PDF (ePrint 2025/1810) and
     [LWYY25b] PDF (ePrint 2024/1988) into `docs/research/` (or a
     `docs/research/papers/` store) so section/figure references are
     checkable offline. Record the ePrint version hashes.
0.2. Extract and record, in this document's §10 table, the exact
     construction equations for: BitGC-small garble/eval (Figs. 4–5),
     BitGC-fast AND record format (§5.1), BitGC-tiny (§5.2), the free-XOR
     permutation-bit rule, the SIMD slot mapping, and both variants' full
     parameter sets (ring degrees, modulus chains, noise distributions,
     PRG choice) from the paper text — the artifact's `e2e_benchmark.cc`
     values above are a starting hint, not the pin.
0.3. Resolve the Table 1 timing discrepancy for BitGC-small (~345 s in the
     ePrint Table 1 excerpt vs ~91 min in the Lacuna summary) and for the
     original-BitGC baseline (14 h vs "> 101 h") by reading the paper's
     evaluation section directly; record the reconciled numbers.
0.4. Clone and build [ZLL+26-A] on a Linux host (the repo AGENTS.md
     requires QEMU with software emulation for Linux coverage on this
     macOS host; treat missing QEMU/guest prerequisites as fail-closed and
     report, don't substitute host behavior). Capture `sub64` correctness
     vectors for `fast` and `small` at `--offline-dim=9 --online-dim=8`,
     plus intermediate wire dumps under `--check=wire`, as the
     interoperability oracle.
0.5. Update `docs/fhe/future-provider-integration-ledger.md`: mark
     FHE-PLUMB-TOOLCHAIN-03 as partially actionable (a practical
     heavy-garbling implementation now has a concrete plan) and add the
     new BITGC rows from §10. Every deferred check gets its ledger ID and
     a `TODO(provider-ledger: ID)` comment at the deferral site.
0.6. Decide the ring-arithmetic substrate: extend
     `tinylabels/ring_lwe.rs`-style schoolbook code vs. a new NTT module
     vs. a reviewed external crate. Criteria: `no_std + alloc` feasibility,
     auditability, SIMD-slot support required by [ZLL+26] §3. Record as an
     ADR in `docs/adr/`.

### Phase 1 — `volar_spec::bitgc::ring` + `prg` + `seed` (offline machinery)

1.1. `ring.rs`: prime-modulus ring R_q, NTT (iterative, const-generic
     degree), slot encode/decode, modulus-switch chain
     (offline → post-PRG → online per the artifact's two-context shape).
     Known-answer tests against vectors extracted from the artifact's SEAL
     context (Phase 0.4) or hand-computed small-degree cases.
1.2. `prg.rs`: the paper's low-depth PRG as a Boolar/`BIrBlocks` circuit
     plus a direct arithmetic realization, with differential tests between
     the two.
1.3. `seed.rs`: seed sampling (caller `RandomSource`), SWHE encrypt
     (caller `NoiseSource`), homomorphic PRG expansion driver, slot
     extraction to per-wire encrypted bits.
1.4. Deliberate-boundary docs in module headers (same style as
     `tinylabels/ring_lwe.rs`): not a wire format, not reviewed noise,
     semi-honest only, RLWE-KDM assumption stated explicitly.

### Phase 2 — `common` + one variant end-to-end (`small` first)

`small` first because it shares the most structure with the original
[LWYY25b] (both charge XOR), making differential debugging against the
paper's §4 figures and the artifact's `small` mode easiest.

2.1. `common.rs`: decoupled permutation-bit bookkeeping, Δ ∈ R_p,
     wire-label ↔ slot mapping, stitching-record types.
2.2. `small.rs`: garbler and evaluator per [ZLL+26] §4 / Figs. 4–5.
2.3. `volar-mpc/src/bitgc_delivery.rs`: `GcScheme` profile enum, manifest,
     admission budget (reuse `TinyLabelsResourceBudget` shape), strict
     session binding.
2.4. Differential test against the artifact's `small`/`sub64` wire dumps.
2.5. Geometry export from `volar-weaver` (§4.3 Phase-2 depth) so the
     offline seed is sized from real circuits.

### Phase 3 — Cirrus adapter

3.1. `cirrus-volar-garble/src/bitgc.rs`: `BitGcGarbleBackend` /
     `BitGcEvalBackend` + `BitGcRecord` stream type.
3.2. `GcScheme` profile plumbing with embedded rejection; ERT profiles
     statically exclude it.
3.3. Interpreter round-trip test: run the existing Thumb SHA-256 fixture
     garbler/evaluator over `BitGcSmall` (geometry-sized seed) and check
     output labels open correctly — reusing the locked SHA-256 fixtures as
     the correctness oracle, with relaxed (small-parameter) ring profile.
3.4. Cross-repo test that Cirrus's adapter and Volar's direct session path
     agree on the same circuit (the two execution models must not fork the
     math).

### Phase 4 — `fast` and `tiny` variants + full weaver passes

4.1. `fast.rs` ([ZLL+26] §5.1): 5-bit AND records, free-XOR rule, no
     online homomorphic multiplications. Differential vs. artifact `fast`.
4.2. `tiny.rs` ([ZLL+26] §5.2): 1-bit AND, free-XOR, the compact 0.8 KB
     AES-128 shape. **Note:** the public artifact has no `tiny` mode;
     the paper text is the only oracle → this variant stays Unpinned
     longest and needs the most careful equation-level pinning in Phase 0.
4.3. `weave_bitgc_garbler` / `weave_bitgc_evaluator` IR passes
     (§4.3 Phase-4 depth) with generated-code compile-and-run tests.
4.4. Bit-packed stitching stream codec in a `frame.rs`, versioned, with
     malformed-frame rejection tests.

### Phase 4.5 — Paper-pinned gate equations and DD gadget (landed 2026-10-08)

The ePrint PDFs (`2024/1988`, `2025/1810`) were supplied and archived in
`docs/research/papers/`; the exact construction is extracted and
cross-checked against the artifact in
`docs/research/bitgc-paper-pinning.md`. A new `volar_spec::bitgc::pinned`
module lands the paper-pinned pieces:

- **Exact gate algebra** (unit-tested): `masked_table` (ẑ[i,j]),
  `row_multipliers` (t₁,t₂,t₃), `fast_mask_bits` / `fast_unmask_coeffs`,
  `determination_bit`.
- **A reference DD gadget** (`ReferenceDd`) whose `DD_i`/`DD_{i,j}`/
  `HssMul` satisfy the paper's DD identities **exactly** over `R_p`
  (verified by `dd_linear_identity`, `dd_correlated_identity`,
  `hss_mul_product_share` tests). It is cryptographically hollow (no
  lattice noise) and exists to pin the garbling algebra; the production
  DD gadget is the lattice SWHEwDD instantiation of [ZLL+26] App. A,
  plugged into the same seam.
- The `DdGadget` trait is the seam for the production lattice gadget.

**Confirmed structure (paper):** wire labels are additive key-shares over
`R_p` with `W^π = W⁰ + π·Δ`, Δ = the ternary SWHE secret key
(`LSB(Δ)=1`); per-gate communication is the determination bit
`d_γ = LSB(W⁰_γ) ⊕ π_γ` (plus 4 PRG mask bits per AND under fast). The
per-wire offline value is **one permutation bit π** (N = |I|+|W| for
small, 4·|W_AND| for fast), not the (A0, π) pair the first-cut
`ReferenceEquation` assumed.

**Remaining for a faithful implementation** (next milestone, tracked by
BITGC-LEDGER-04): port the production SWHEwDD gadget — RLWE'
gadget-product ciphertexts for k=1 DD and the CP-RLWE triple for k=2 —
over the coefficient-form SWHE ring, implement the full circuit drivers
(`garble_circuit`/`evaluate_circuit` against `ReferenceDd` first, then
the lattice gadget), and run the artifact conformance vectors
(BITGC-LEDGER-01/02).

### Phase 5 — Hardening, benchmarks, docs

> **Status after implementation round (2026-10-08):** Phases 0–4
> landed. `volar_spec::bitgc` (ring/SWHE/PRG/seed/common/small) compiles
> and passes 12 unit tests covering both the small and fast reference
> adapters end-to-end on the gated `(x0∧x1)⊕x2` circuit; `volar-mpc`
> admission tests pass; the Cirrus `cirrus-volar-garble` adapter compiles
> and its `bitgc_roundtrip` integration test is written. The Cirrus test
> **build** is blocked by a pre-existing, unrelated breakage:
> `cirrus-volar-boolar` and the `volar-*` compiler crates reference
> `Type::AES8`/`Galois64`/`Z3`, which the patched `volar-ir` checkout
> removed in commit `80b0f82` ("Replace AES8 and Galois64 with canonical
> ExtField"). This is a pre-existing merge inconsistency, not a BitGC
> defect; the adapter library itself compiles clean.
>
> Remaining: Phase 5 (hardening/benchmarks/docs) below, plus the
> equation-pinning work in BITGC-LEDGER-04.

5.1. Parameter-security review gate: independent check of ring degrees,
     modulus chains, noise bounds against the RLWE-KDM assumption and the
     paper's claimed 128-bit profile. Until this passes, every BitGC
     profile is marked experimental/Unpinned in type docs.
5.2. Benchmarks: garble/eval time and stream bytes for the Thumb SHA-256
     fixture under each variant vs. the half-gate locked baseline
     (164,288 tables / 10.5 MB), on a QEMU Linux guest per repo policy.
5.3. Docs: update `docs/garbling-pipeline.md` with the bitgc branch of the
     pipeline diagram; write `docs/research/bitgc-variants-research.md`
     capturing the paper notes behind the pinning table.
5.5. **Pre-existing volar-ir drift (mostly resolved by merging `main`).**
     The `Type::AES8`/`Galois64`/`Z3` → `ExtField` migration landed on
     `main` in both repos. Merging `main` into `feat/mpc` (volar commit
     after `434cea9`, cirrus `9ad9441`) resolved it:
       - **volar** workspace builds; `parser.rs` and `structs.rs` now
         lower field types to `LirType::ExtField`/`PrimeField`.
       - **cirrus** `cirrus-volar-boolar` compiles (main's `types.rs`
         matches `IRType::ExtField`/`PrimeField`); the
         `bitgc_roundtrip` integration test now **runs and passes**
         both small and fast truth tables (after fixing a
         `ReferencePrg::new(2)`→`new(1)` slot-discipline issue,
         committed `fba383c`).

     **One residual gap:** `volar-lir-codegen`/`volar-weaver` now
     reference `LirType::ExtField`/`PrimeField`, which exist on
     volar-ir `main` but **not** on the patched sibling checkout's
     `feat/mpc` branch (the parent `.cargo/config.toml` patches
     `volar-lir` to that checkout). volar-ir `feat/mpc` needs to merge
     `main` (coordinated with that worktree's owner over the pi link).
     Until then the weaver tests still can't build. BitGC's own
     construction and integration tests are unaffected and green.
5.4. Cut-and-choose interaction review: `cirrus-volar-garble` has an
     existing `cut_and_choose.rs` for the half-gate backend; record
     explicitly whether/how BitGC garblings participate (likely: not at
     first — semi-honest only — documented as a boundary).

---

## 6. Paper reference map (implementation → paper)

Every module lands with these citations in its header so paper-pinned
status is a lookup, not a rediscovery:

| Code element | Paper anchor |
|---|---|
| `bitgc::ring` NTT + slot mapping | [ZLL+26] §3 (SIMD-enabled SWHE; even-modulus barrier removal) |
| `bitgc::prg` | [LWYY25b] low-complexity PRG choice; [ZLL+26] §3 |
| `bitgc::seed` expansion driver | [LWYY25b] main construction; [ZLL+26] §2/§3 preliminaries |
| `bitgc::common` permutation-bit decoupling | [ZLL+26] §3 (labels vs. permutation bits; π_a stored explicitly, A₁ = A₀ + (−1)^πa·Δ, Δ ∈ R_p with LSB(Δ) = 1) |
| `bitgc::common` free-XOR rule | [ZLL+26] free-XOR contribution (output permutation bit = XOR of input permutation bits); used by §5.1/§5.2 |
| `bitgc::small` garble/eval | [ZLL+26] §4.1, Figures 4–5 |
| `bitgc::fast` garble/eval, 5-bit record | [ZLL+26] §5.1 |
| `bitgc::tiny` garble/eval | [ZLL+26] §5.2 |
| Parameter profiles | [ZLL+26] evaluation section + [ZLL+26-A] `e2e_benchmark.cc` (cross-checked in Phase 0.2) |
| Conformance vectors | [ZLL+26-A] `circuits/sub64.txt`, `--check=wire` dumps |
| Security assumption statement | [LWYY25b] (RLWE + KDM); semi-honest model |

---

## 7. Testing strategy

- **Layer 1 (unit):** ring KATs (NTT round-trip, slot codec, modulus
  switch), PRG differential (circuit vs. arithmetic), per-variant gate
  truth tables with `ZeroNoise` determinism.
- **Layer 2 (construction):** full small-circuit garble/eval against the
  artifact's `sub64` wire dumps at the correctness-only profile.
- **Layer 3 (integration):** Cirrus interpreter round-trips (Thumb
  SHA-256 fixture) and Volar strict-session runs on the same circuits;
  cross-repo agreement.
- **Layer 4 (negative):** malformed stitching-stream frames, exhausted
  record iterators, profile rejection on embedded targets, admission
  budget overruns.
- **Fuzzing:** extend `volar/fuzz` with a stitching-stream decoder target
  once `frame.rs` exists.
- **Linux coverage:** artifact builds and benchmark runs under Homebrew
  QEMU software emulation with explicit args, ephemeral disks, bounded
  timeouts; fail-closed if unavailable.

---

## 8. Risks and open questions

1. **Artifact gap for `tiny`.** No public `tiny` implementation exists;
   §5.2 must be implemented from paper text alone. Highest pinning risk;
   consider contacting authors or waiting for artifact updates before
   Phase 4.2.
2. **Parameter extraction.** The paper's parameter table (not just the
   artifact's CLI defaults) must be pinned in Phase 0.2; the two may
   differ (e.g. Table 1 timing discrepancies already observed).
3. **KDM/circular security** is a stronger-than-standard assumption; the
   profile docs must state it wherever `GcScheme::BitGc*` is selectable.
4. **Memory.** Even the correctness profile is heavy; server admission
   budgets (Phase 2.3) are mandatory, and streaming designs must avoid
   materializing the full expanded ciphertext set where the paper allows
   on-the-fly extraction.
5. **Noise management.** Two multiplication levels after PRG expansion
   ([LWYY25b]); modulus-chain depth must match per-wire worst case, not
   averages.
6. **SWC-style hygiene analogue:** wire/slot indices keyed by
   circuit-position identity, never by flattened debug strings, when
   building manifests (matches the repo SWC identifier-hygiene rule's
   intent).
7. **No production claim.** Mirroring `tinylabels`: until Phase 5.1's
   review passes, all three schemes are experimental, semi-honest,
   Unpinned, and excluded from embedded and from any default profile.

---

## 9. Relationship to existing documents

- Extends, does not supersede: `docs/garbling-pipeline.md` (half-gate
  pipeline stays the default), `docs/research/tinylabels-cross-repository-plan.md`
  (input-label delivery; BitGC is orthogonal — one compresses input
  labels, the other compresses gate material), Cirrus
  `crates/garbled-circuit/TINYLABELS_CROSS_REPO_PLAN.md` (deployment-split
  and module-division discipline reused).
- Feeds: `docs/fhe/future-provider-integration-ledger.md` row
  FHE-PLUMB-TOOLCHAIN-03 and new BITGC rows (§10).
- Note the orthogonal name collision: `bitgc-tiny` ([ZLL+26] §5.2) is
  unrelated to TinyLabels [DLL24]; docs must not abbreviate either as
  just "tiny".

---

## 10. Paper-pinned status tracking table

Fill `Status` from {TODO, PINNED (with ePrint version hash + figure/eq.
numbers verified), PASS (with test/vector references)}. New deferred
checks get ledger IDs in `future-provider-integration-ledger.md` with
`TODO(provider-ledger: BITGC-…)` comments at the deferral sites.

| ID | Element | Paper anchor | Status |
|---|---|---|---|
| BITGC-PIN-01 | Even-modulus barrier removal; permutation-bit decoupling | [ZLL+26] §3 | PARTIAL: structure identified (decoupled π, Δ = SWHE secret key embedded in Rp, LSB(Δ)=1, sign from label LSB) — see `docs/research/bitgc-artifact-notes.md`; the current `GlobalDelta` (small odd constant) is a placeholder |
| BITGC-PIN-02 | SIMD slot mapping / NTT-friendly modulus choice | [ZLL+26] §3 | PINNED (structure): evaluation-form slot codec + modulus-switch chain landed Phase 1 |
| BITGC-PIN-03 | Low-depth PRG choice + expansion depth | [ZLL+26] §6 | **PINNED (paper):** Goldreich random-local PRG, XOR₄–Majority₇ predicate, depth t=4, seed ℓ=2^16, stretch 2^32 (§6); artifact confirms Goldreich. `ExpansionPrg` seam landed Phase 1; pinned PRG circuit is remaining impl work |
| BITGC-PIN-04 | Free-XOR permutation-bit rule | [ZLL+26] free-XOR section | PINNED (structure): `π_out = π_a ⊕ π_b` confirmed in artifact `fast_gen.cc::HandleXorGate`; reference adapter implements it |
| BITGC-PIN-05 | BitGC-small garble procedure | [ZLL+26] §4.1 Fig. 4 | **PINNED (paper + artifact agree):** `ẑ[i,j]=(π_α⊕i)∧(π_β⊕j)⊕π_γ`, `τ₁/τ₂/τ₃` subtractions, `Eval_{0,0} − ẑ[0,0]·Δ + PRF_K(γ)`, `d_γ=LSB(W⁰_γ)⊕π_γ` — `docs/research/bitgc-paper-pinning.md` §2 |
| BITGC-PIN-06 | BitGC-small eval procedure | [ZLL+26] §4.1 Fig. 5 | **PINNED (paper + artifact agree):** `W_γ=Eval_{x̂_α,x̂_β}(…)+PRF_K(γ)`, `x̂_γ=LSB(W_γ)⊕d_γ`; XOR `x̂_γ=x̂_α⊕x̂_β⊕d_γ` |
| BITGC-PIN-07 | BitGC-fast 5-bit AND record + garble | [ZLL+26] §5.1 | **PINNED (paper + artifact agree):** 4 PRG-masked `d_γ^{i,j}=ẑ[i,j]⊕r_γ^{i,j}` + `[[ẑ·Δ]]=d·[[Δ]]+(1−2d)·[[r·Δ]]` (addition-only) + 1 determination bit = 5 bits/AND |
| BITGC-PIN-08 | BitGC-fast eval (no online mults) | [ZLL+26] §5.1 | **PINNED (paper + artifact agree):** evaluator unmaskes `d_γ^{i,j}` linearly from PRG ciphertexts; free-XOR via `π_γ=π_α⊕π_β`; no online HE multiplication |
| BITGC-PIN-09 | BitGC-tiny garble/eval | [ZLL+26] §5.2 | DEFERRED (maintainer decision 2026-10-08; ledger BITGC-LEDGER-06) |
| BITGC-PIN-10 | Parameter sets (both variants, 128-bit claim) | [ZLL+26] §6 | **PINNED (paper):** log p≈60; DD moduli log q≈120 / log r≈240; fast log Q_PRG≈600 / Q_gate≈180 at N=2^15/2^13; small log Q_PRG≈780 / Q_gate≈300 at N=2^15/2^14 — pinning doc §5 |
| BITGC-PIN-11 | Table 1 reconciliation (sizes, timings) | [ZLL+26] Table 1 | **PINNED (paper, verbatim):** fast 5/0 bits 4.0 KB 116 s; small 1/1 4.3 KB 345 s; tiny 1/0 0.8 KB ≥56 min; baseline BitGC 1/1 4.3 KB ≥14 h (est.) |
| BITGC-LEDGER-01 | Artifact `sub64` conformance (small) | [ZLL+26-A] | TODO (QEMU build) |
| BITGC-LEDGER-02 | Artifact `sub64` conformance (fast) | [ZLL+26-A] | TODO (QEMU build) |
| BITGC-LEDGER-03 | Independent noise/parameter security review | [LWYY25b] assumption statement | TODO |

---

## 11. Evidence and sources consulted for this plan

- ePrint 2025/1810 abstract, Table 1 excerpt, and §§3–5 structure
  (via search-indexed PDF text): variant names, per-gate communication,
  AES-128 sizes/timings, section numbers, permutation-bit notation
  (Δ ∈ R_p, LSB(Δ) = 1, A₁ = A₀ + (−1)^πa·Δ).
- ePrint 2024/1988 abstract: seed + stitching-bit structure, RLWE/NTRU
  KDM assumption, O(1) homomorphic ops per gate, no bootstrapping.
- Lacuna summary of [ZLL+26] (lacuna.tiptreesystems.com): technique
  descriptions (SIMD barrier, free-XOR, 5-bit trade-off), benchmark
  context (AES-128 = 6,400 AND + 28,176 XOR).
- [ZLL+26-A] README + `test/bench/e2e_benchmark.cc`: variant CLI,
  ring-degree/level/modulus shapes, memory estimates, correctness-only
  profile, Bristol circuits.
- Volar @ 5306e25: `volar-spec/src/garble.rs`, `volar-spec/src/tinylabels/`,
  `volar-weaver/src/garble.rs`, `volar-mpc/src/input_labels.rs`,
  `docs/garbling-pipeline.md`, `docs/fhe/future-provider-integration-ledger.md`,
  `docs/research/tinylabels-cross-repository-plan.md`.
- Cirrus @ 4708087: `cirrus-garbled-circuit{,-row-reduced,-tinylabels}`,
  `cirrus-volar-garble`, workspace Cargo.toml (volar-spec patch),
  `TINYLABELS_CROSS_REPO_PLAN.md`.
