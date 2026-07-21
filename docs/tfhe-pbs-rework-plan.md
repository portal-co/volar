# TFHE / GINX Rework and Validation Plan

**Status:** review document — **no implementation is authorized by this plan.**

**Scope:** `crates/spec/volar-spec/src/tfhe.rs` (Tier 3), its generated
artifacts, and `crates/compiler/volar-weaver/src/fhe.rs` (Tier 2, treated as
Tier 3 for this work).

**Reliability:** remain Experimental. The present implementation must not be
advertised as having a validated security level, a production failure
probability, or interoperable TFHE semantics.

**Decision requested from the owner/reviewers:** approve a validation-first
**possible full rework**, not a PBS API migration. The old plan incorrectly
assumed enough about the current kernel to make a new table encoder and an IR
planner the next step. The paper review and the failed prototype show that this
assumption is not justified.

---

## 1. Executive decision

Do **not** implement a generic `pbs_bool`, cone-fusion pass, removal of old
APIs, or generated-artifact migration until the existing TFHE core has passed a
paper-derived correctness audit.

The immediate deliverable is a small, executable mathematical specification and
an independent test oracle for the following pipeline:

```text
canonical TLWE input
  -> affine pre-processing / rounding domain
  -> accumulator initialization
  -> GINX blind rotation
  -> sample extraction
  -> key switching
  -> canonical Boolean output
```

Only after that audit may the project choose one of the following paths:

1. **Repair in place:** the core is shown equivalent to the chosen reference
   model and only has localized, reviewed defects. Build a narrowly specified
   gate/PBS layer on top of it.
2. **New experimental implementation:** the representation, RGSW layout,
   decomposition, key switching, or phase conventions differ materially from
   the reference model. Preserve the current code as an experimental research
   record and implement a separately named, module-isolated rework; do not
   silently “fix” individual gates in place.
3. **Suspend native TFHE semantics:** the team does not want to own the
   validation burden. Keep the current code experimental and do not extend the
   weaver beyond validated behavior; investigate binding to an independently
   maintained TFHE implementation instead.

This plan deliberately permits outcome 2. A working toy/noiseless truth-table
suite is not evidence that a hand-written GINX implementation is a suitable
base for a more general PBS system.

---

## 2. Evidence review and corrections to the prior plan

### 2.1 Sources and provenance

The following local papers were reread using their selectable-text PDF layer
with bundled PDF.js. Page numbers below are physical PDF pages. Formulae and
algorithm layout must be visually checked by the Tier-3 reviewer against the
original PDF before they are transcribed into code.

| Source | Re-read evidence | What it establishes here |
|---|---|---|
| Chillotti, Gama, Georgieva, Izabachène, *TFHE: Fast Fully Homomorphic Encryption over the Torus*, ePrint 2018/421 | pp. 40–44: Algorithms 9–10 and the native gate formulas; pp. 45–47: circuit bootstrapping TLWE→TRGSW; pp. 53–57: concrete parameter/noise methodology and two-/three-level parameter distinctions. | Gate bootstrapping is a precisely rounded blind-rotation construction, not merely “a test polynomial plus key switch.” Circuit bootstrapping is a different operation with richer output and additional keys/parameters. |
| Micciancio, Polyakov, *Bootstrapping in FHEW-like Cryptosystems*, ePrint 2020/086 | pp. 9–17: accumulator, the condition \(f(v+q/2)=-f(v)\), GINX update, and Boolean gate table; pp. 21–24: separate modulus/noise parameters, failure estimates, and sample parameter sets. | A fixed-function LWE-output PBS is constrained by the accumulator’s antipodal relation. Nevertheless, more than binary gates can be expressible when a correct affine preparation exists: Table 1 explicitly includes 3-input Majority. |
| Ilango, *Gödel in Cryptography…*, ePrint 2025/1296 | Reviewed; not a TFHE/FHEW/PBS construction or parameter source. | Out of scope. It provides no basis for this design. |

### 2.2 Material corrections

The prior document contained two incompatible claims. They are replaced by the
following.

1. **“Arity ≥ 3 requires circuit bootstrapping” is false.** 2020/086 Table 1
   gives a single-bootstrap three-input Majority using the affine preparation
   `c1 + c2 + c3`. Thus, a gate is not classified by arity alone.

2. **“Every complete Boolean table is available through one TLWE-output PBS” is
   also false.** An accumulator initialized for a fixed function requires
   \(f(v+q/2)=-f(v)\) (2020/086 p. 11). For a candidate gate, the input affine
   map, rounding bins, and signed accumulator completion must jointly satisfy
   that relation. A generic table API must therefore not promise all truth
   tables merely because the table fits in a ring polynomial.

3. **Layering remains useful, but it cannot repair an unvalidated core.** Once
   a correct gate-PBS basis exists, a larger program may be evaluated as layers
   of refreshed canonical outputs. A nonrepresentable *one-bootstrap* cone is
   a planning boundary, not necessarily a program failure. But this does not
   license a guessed affine-embedding search or an arbitrary-table API.

4. **Circuit bootstrapping is a distinct future track, not a fallback spelling
   of `LweCiphertext -> LweCiphertext`.** The 2018 construction produces a
   TRGSW ciphertext through repeated TLWE bootstraps and private key switching
   (2018/421 pp. 45–47). It needs distinct ciphertext/key types, parameter
   levels, noise accounting, and a leveled/polynomial evaluation design.

### 2.3 Why the previous prototype was correctly rejected

The attempted finite “eight phase slots” encoder failed exhaustive binary-table
and layered-composition tests. It was reverted. That failure is useful evidence:
it proves that an inferred slot convention is not a substitute for deriving the
actual accumulator indexing, rotation direction, phase quantization, and
sample-extraction convention from the implementation and the paper.

No code from that prototype remains in the tree.

---

## 3. Current-code audit: facts, risks, and non-conclusions

The observations below are code facts or audit questions — **not claims that a
cryptographic defect is proven.** The audit must resolve every question before
new semantics build on the code.

### 3.1 Verified present behavior

- The Boolean encoding is `false = 0`, `true = Q4 = 2^30` in a wrapping `u32`
  torus (`tfhe.rs`). `tfhe_xor` linearly adds ciphertexts. It can decrypt as XOR
  by the local decoder, but `true XOR true` carries phase `2*Q4`, not canonical
  zero, so it is not a composable gate input.
- AND and OR use an affine pre-offset, `blind_rotate`, sample extraction, key
  switch, then add `Q4/2`. Their toy, noiseless tests pass.
- `tfhe_lut_read` independently builds an address combination and a signed test
  polynomial. It accepts `lut.len() <= 2*BIG_N`, pads to a power of two, and has
  a constant-table trivial-encryption special case. Its contract is not the
  paper’s general fixed-function accumulator specification.
- `blind_rotate_with_poly` computes the ring exponent by a rounded shift and
  applies `X^{-b}` followed by per-key-bit CMUX updates. `sample_extract` and
  `key_switch` are handwritten implementations.
- Tests use `N_LWE=8`, `BIG_N=64`, exact 32-bit decomposition, and **zero
  noise**. They verify only local functional behavior under a deterministic
  toy configuration.

### 3.2 Blocking audit questions

| Area | Question to resolve against an oracle/reference | Why this blocks PBS work |
|---|---|---|
| Phase model | Does the repository’s `Q4` encoding, pre-offset, `torus_to_exp` rounding, and rotation sign implement the exact threshold intervals intended by the selected reference? | A table builder cannot be correct without knowing which plaintext phase reaches which accumulator coefficient. |
| Ring-domain assumptions | `torus_to_exp` uses `trailing_zeros` and `exp & (2N-1)`, which only implements a power-of-two `2N` domain. Is `BIG_N` constrained and validated as a power of two everywhere? | Otherwise the stated exponent mapping is wrong for accepted parameters. |
| Fixed-function accumulator | Does the sign/order of `poly_rotate`, `blind_rotate_with_poly`, and `sample_extract` agree with the paper’s `ACC_f[v]` indexing and signed negacyclic completion? | This is exactly where the failed prototype disagreed with reality. |
| RGSW/external product | Is the gadget contribution placed in the correct RLWE component with the required sign, and is `poly_decompose` the required centered/approximate decomposition? | An error can pass small zero-noise gate tests yet invalidate noise bounds or different test vectors. |
| Key switching | Does `ks_decompose` match the reference digit convention, precision, and KSK message placement? | The key switch is part of the output correctness/noise proof, not just data movement. |
| Noise/security | The implementation samples a truncated uniform signed value, while the cited parameter analyses use specified noise distributions and separate moduli/levels. What are the actual security and failure claims? | No paper parameter table can be imported into the current code without a mapping/proof. |
| LUT API | Does `tfhe_lut_read` validate its address domain, address-bit count, padded entries, antipodal completion, and rounding margin? | It must not be promoted to the generic PBS primitive simply because selected tests pass. |

### 3.3 Important non-conclusions

- Passing the existing 23 TFHE tests does **not** establish a conforming GINX
  implementation, nonzero-noise correctness, or an admissible parameter set.
- The current code may have a useful functional core. This plan does not label
  it broken without the audit; it prevents the project from compounding an
  unresolved mismatch.
- A full rewrite is not assumed, but it is an explicitly supported result of
  the audit.

---

## 4. Target mathematical contract (must exist before API design)

Write `docs/tfhe-ginx-core-spec.md` before changing cryptographic code. It must
be a self-contained, paper-cited description of **the exact implementation
variant**, including all signs and indices. Its normative contents are:

1. **Domains and encodings.** Define the input TLWE modulus/torus, RLWE/RGSW
   modulus, ring \(\mathbb Z_Q[X]/(X^N+1)\), secret distributions, Boolean
   encoding, decoder intervals, and which values are exact fixed-point
   representatives. Do not conflate the small input modulus \(q\) and the
   accumulator modulus \(Q\) merely because a toy implementation stores both
   in `u32`.
2. **Phase quantization.** State the map from a ciphertext phase to an exponent
   in \(\mathbb Z_{2N}\), its rounding tie rule, and a bound for the aggregate
   input/rounding error. This must be compared directly with Algorithm 9 in
   2018/421 pp. 41–43 and the accumulator formulation in 2020/086 pp. 9–12.
3. **Accumulator convention.** Define `ACC_f[v]`, the coefficient order,
   whether initialization contains `f(v-i)` or `f(v+i)`, the sign induced by
   blind rotation, and sample-extraction key ordering. Prove/execute the
   identity between the abstract accumulator and the repository’s polynomial
   rotation code.
4. **Admissible fixed functions.** State the exact antipodal condition and the
   signed output representation. Define canonical output restoration exactly
   once, including why it preserves the next gate’s input decoder interval.
5. **Affine gate preparation.** A supported gate is represented by a public
   certificate:
   - input order;
   - integer/scaled affine coefficients and constant;
   - rounding/decision intervals including their error margin;
   - a fixed signed accumulator function satisfying the antipodal condition;
   - expected canonical Boolean result for every assignment.

   The certificate is verified by a deterministic **clear arithmetic oracle**;
   it is not discovered by a heuristic finite phase search.
6. **Noise and parameters.** Derive the contribution from affine preparation,
   blind rotation/external product, extraction, key switching, and any modulus
   switching. State which cited theorem/parameter model applies and exactly
   where the repository diverges. Until this exists, all tests are functional
   tests only.

The spec must cite, at minimum: 2020/086 Fig. 2 and pp. 13–17; 2018/421
Algorithms 9–10 and pp. 41–44. Circuit-bootstrap material belongs in a separate
spec and must not be mixed into this one.

---

## 5. Validation-first implementation sequence

### Phase 0 — freeze scope and establish baselines

- [ ] Inventory every public TFHE symbol, generated mirror, compiler component
  test, and runtime reference. Do **not** delete or rename any API yet.
- [ ] Add a capability/reliability header identifying the implementation as
  Experimental and record this plan as its pending review artifact.
- [ ] Record the known direct-XOR composition counterexample as a regression:
  its standalone decoder result is insufficient evidence that it can be fed to
  a later bootstrapped operation.
- [ ] Capture the current toy test results as a baseline, explicitly labelled
  `noiseless-functional`, not as a security or failure test.

### Phase 1 — executable cleartext oracle

Implement a `#[cfg(test)]` or test-only reference model that has no secret
keys, ciphertext arithmetic, RGSW, or key switching. It must:

1. enumerate canonical Boolean inputs;
2. apply a candidate affine preparation in the specified torus/modular domain;
3. apply the exact quantization rule;
4. index a complete signed accumulator vector;
5. apply the canonical output restoration; and
6. compare all assignments with the intended Boolean function.

Use it first for the paper-derived AND, OR, XOR/XNOR, and Majority
certificates. Table 1 on 2020/086 p. 15 is the source for the preparations and
intervals; 2018/421 p. 44 independently gives the familiar TFHE gate forms.

**Gate A — do not proceed unless:** the oracle validates every cited gate and
catches intentional sign, offset, LSB-order, antipodal-completion, and rounding
mutations.

### Phase 2 — core conformance tests

Add test-only instrumentation or a non-secret mathematical view that compares,
for tiny parameters, these stages independently:

- `poly_rotate` against multiplication by \(X^e\) in a separately written
  negacyclic oracle;
- blind rotation of a trivial accumulator against the clear accumulator model;
- RGSW CMUX against both selector values and arbitrary signed test polynomials;
- sample extraction against direct RLWE decryption;
- key switching against direct decryption under source and destination keys;
- the complete bootstrap before and after output restoration.

Each test must use independently calculated expected values, not a second path
through the same helper. Include exact-grid inputs where phase quantization
should be exact, then carefully chosen near-boundary cases.

**Gate B — do not proceed unless:** all stage tests pass for paper-derived
vectors, and any divergence has a written explanation in the core spec.

### Phase 3 — independent reference and nonzero-noise review

- [ ] Differential-test the selected gate certificates against an independent,
  maintained FHEW/TFHE-compatible implementation or a reviewer-approved
  executable reference. A duplicated helper inside `tfhe.rs` is not
  independent.
- [ ] Select a concrete, supported parameter model. The 2018 paper’s library
  parameters (pp. 53–55) and 2020 paper’s STD parameter tables (pp. 21–24) are
  distinct constructions/assumptions; neither can be copied into the current
  structs without a precise translation.
- [ ] Run nonzero-noise experiments only after the reviewer approves the noise
  distribution, dimensions, decomposition, and failure criterion. Report
  observed failures separately from a mathematical failure bound.

**Gate C — decision point:**

- If the current representation can be mapped faithfully to the reference,
  continue with a limited repair-in-place.
- If it cannot, create a new module such as `tfhe_rework`/`tfhe_ginx_v2` with a
  new experimental API and migration plan. Leave the legacy implementation
  available only as a clearly documented experimental record until a human
  decides its disposition.

### Phase 4 — narrow, certificate-based gate PBS API

Only after Gates A–C, introduce a module-isolated surface:

```rust
pub mod pbs {
    // Exact names/types subject to the core spec.
    pub fn evaluate_gate<...>(
        inputs: &[LweCiphertext<N_LWE>],
        certificate: &BooleanGateCertificate,
        bk: &BootstrappingKey<...>,
    ) -> Result<LweCiphertext<N_LWE>, TfhePbsError>;
}
```

The initial public contracts are deliberately narrow:

- certificates for AND, OR, XOR/XNOR, and Majority are paper-derived and
  exhaustively checked by the clear oracle;
- inputs have documented LSB-first order;
- each successful output is freshly canonical and composable;
- malformed public inputs/certificates return an explicit error;
- raw caller-supplied test polynomials are not a public API;
- arbitrary truth-table admission is **not** claimed.

A later generic table API may exist only if the core spec defines a complete,
deterministic synthesis/verification procedure for a table plus affine map,
with a proof of all quantization margins. A fixed small search over guessed
phase slots is expressly not that procedure.

### Phase 5 — program lowering, after the cryptographic boundary is stable

The weaver may then lower a program using a **validated gate basis**. This is
already program-relevant without pretending that every cone is one PBS:

- straight-line Boolean programs compose refreshed PBS outputs in layers;
- a three-input Majority may use its own valid one-bootstrap certificate;
- an encrypted MUX may retain a reviewed layered/native construction; it must
  preserve `sel ? when_true : when_false` and never use direct linear XOR as a
  generic Boolean wire;
- storage remains oblivious MUX/demux lowering. PBS does not read encrypted
  mutable cells from a public accumulator polynomial.

Only after the gate basis has E2E execution coverage should an optimizer be
considered. Its first form should choose among **certified primitive gates**,
not synthesize arbitrary Boolean tables. A table/cone fusion pass requires its
own design review covering semantic evaluation, publicness, provenance,
fan-out, effects, and cost.

All emitted calls must be typed `IrExpr::Call` / `IrExpr::Path` constructions;
no rendered-Rust workaround is permitted. FHE output remains
`Tagged<Transparent, _>`; no discipline bound may be weakened.

### Phase 6 — separate circuit-bootstrap / arbitrary-LUT specification

This is a new Tier-3 project, not Phase 4 feature work. It must specify:

- TLWE→TRGSW circuit bootstrap and its multi-level keys;
- output TRGSW/RLWE types and ownership/lifetime in Rust, TS, and C targets;
- the leveled representation/evaluation used for LUTs (packing, automata, or
  another reviewed method);
- parameter sets, noise transitions, and the assumptions introduced by private
  key switching/circular security;
- what “arbitrary k-input table” means operationally and its cost.

The 2018 paper’s circuit bootstrap output and its 8-bit LUT examples (pp. 45–48
and 56–59) are the starting references. It is not acceptable to expose this as
an overloaded `LweCiphertext -> LweCiphertext` call.

---

## 6. Module isolation and reliability policy

The user-requested isolation mechanism is a Rust module, not a Cargo feature:
`tfhe::pbs` (or a new rework module) keeps source-discovering compiler paths
able to parse `volar-spec/src/tfhe.rs`.

However, `docs/reliability.md` currently says Experimental code must also be
behind a `volar_experimental` Cargo feature, while the crate presently exposes
`tfhe` unconditionally and the compiler recursively discovers source files.
These facts conflict. This plan does not silently waive either requirement.
Before exposing a new API, a human owner must choose and document one of:

1. add compiler-aware experimental exclusion/import handling;
2. make a documented project-policy exception for source-discovered experimental
   spec modules; or
3. redesign the source-discovery boundary.

A Rust module is valid API isolation and preserves `volar-compiler`
compatibility; it is not by itself a substitute for an unresolved reliability
policy decision.

---

## 7. Test and review acceptance criteria

No implementation may be called ready for Tier-3 cryptographic review unless:

- the mathematical core spec exists and names the exact paper algorithms/pages;
- the clear oracle validates AND, OR, XOR/XNOR, and Majority certificates, with
  mutation tests for every sign/offset/rotation convention;
- stage-by-stage conformance tests establish accumulator, CMUX, extraction, and
  key-switch behavior independently of the public gate wrappers;
- all successful gate outputs feed later certified gates over exhaustive small
  truth tables;
- nonzero-noise/parameter claims are either independently validated or plainly
  absent; the tests never present toy parameters as security parameters;
- a differential test against an independent reference or independently
  reviewed mathematical implementation is present;
- generated Rust is compiled **and executed** through weave → print → keygen →
  encrypt → evaluate → decrypt for the supported gate basis;
- TypeScript/C checks use regenerated artifacts and do not retain stale symbols;
- no direct `tfhe_xor` result is used as a generic composable Boolean wire;
- no optimizer crosses action, oracle, RNG, storage, publicness, provenance,
  CFG, or ZK-discipline boundaries; and
- a human cryptographer has reviewed the parameter/security/noise claims.

---

## 8. Review assignments

| Review | Required reviewer | Blocking question |
|---|---|---|
| Core algebra | Tier-3 lattice/FHE reviewer | Do phases, signs, rotations, RGSW layout, decomposition, extraction, and key switch faithfully implement the selected GINX model? |
| Oracle/certificates | Independent reviewer | Does the clear model use paper-derived semantics and catch the realistic convention mistakes? |
| Parameters/noise | Human cryptographer | Is the secret/noise/modulus/key-switch model coherent, and what failure/security claim is actually justified? |
| Compiler integration | Tier-2+ compiler reviewer plus Tier-3 sign-off | Does typed IR preserve semantics/publicness/provenance and retain `Transparent` discipline? |
| Reliability policy | Human owner | How is module isolation reconciled with the documented Experimental feature-gate policy? |

---

## 9. Explicitly deferred work

- generic arbitrary truth-table single-PBS synthesis;
- a one-bootstrap arbitrary 3+-input cone/MUX claim;
- TLWE→TRGSW circuit bootstrapping and packed/leveled LUT evaluation;
- encrypted mutable-storage PBS reads;
- copying published parameters into the current implementation;
- removal of legacy public primitives before an audited replacement and complete
  generated-artifact migration exist.

The end state of this plan may be a full rework. That is preferable to
optimizing or generalizing a core whose mathematical conventions and parameter
model have not yet been reconciled with the references.