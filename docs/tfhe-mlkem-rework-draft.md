# Draft: self-consistent TFHE rework with integer-sampled noise

**Status:** design draft — it authorizes no cryptographic implementation,
parameter claim, API change, or reclassification.

**Pinnedness/stability of any resulting implementation:** Unpinned and Very
unstable. This document is a review plan, not a paper binding for a new
ciphertext construction. The existing `tfhe.rs` remains a legacy experimental
migration record until an owner decides otherwise.

**Related records:**
[`tfhe-pbs-rework-plan.md`](tfhe-pbs-rework-plan.md), the
[clear-oracle binding](reviews/tfhe-ginx-oracle-paper-binding.md), and the
[`tfhe-go` reference reconnaissance](reviews/tfhe-ginx-tfhe-go-reference.md).

## 1. Problem and decision

The immediate problem is **self-consistency**, not a security proof. The
current implementation has a raw linear `tfhe_xor`: `true XOR true` decrypts
as false under a final decoder but has phase `2·Q4`, rather than the canonical
false phase zero. It is therefore not a valid input to a subsequent operation
that requires a canonical Boolean ciphertext. Toy gate truth tables and
stage-local conformance tests do not adequately exercise this failure mode or
other cross-operation inconsistencies.

The proposed direction is a separately named `tfhe_rework` / `tfhe_ginx_v2`
module, initially test-only or otherwise unreachable from the compiler. It
will be designed around all-integer arithmetic, reproducible seeded sampling,
and **ciphertext-vs-plaintext completeness tests**. The legacy module must not
be gradually patched into a mixture of incompatible conventions.

The clear oracle remains the reference for Boolean certificate semantics. The
pinned `tfhe-go` revision remains a convention and executable-reference aid.
Neither one binds the new construction or proves it interoperable.

## 2. FIPS 203 is a source of mechanics, not a TFHE parameter transplant

Local source asset:

| Source | Local filename | SHA-256 | Intended use |
|---|---|---|---|
| NIST, *Module-Lattice-Based Key-Encapsulation Mechanism Standard* (FIPS 203) | `~/Downloads/NIST.FIPS.203.pdf` | `fe1f12f32a7e44ec9fdebbf400cda843a40b506dee676725234dc6f7923b6cac` | integer-only polynomial/noise-sampling mechanics, fixed-width encoding discipline, and reproducible test-vector style |

The exact PDF was read through PDF.js text extraction. FIPS 203 §4.1 (printed
pp.18–19) specifies its SHA3/SHAKE wrappers; §4.2.1 (pp.20–22) defines
little-endian bit/byte conversion and explicitly says that its
compression/decompression divisions and rounding **shall not use floating
point**; §4.2.2 and Algorithm 8 (pp.22–23) specify `SamplePolyCBD`; §4.3
(p.24) specifies the NTT ring; and §8/Table 2 (p.39) lists the approved
ML-KEM parameters. These source locations must be checked again visually
against the hash above before an implementation is paper-pinned.

FIPS 203's ML-KEM setting is not TFHE: it fixes `n = 256`, prime `q = 3329`,
and `R_q = Z_q[X]/(X^256 + 1)`. Section 4.3 says this quotient is isomorphic
to a direct sum of 128 quadratic extensions and that ML-KEM's NTT is integral
to its multiplication. It is therefore neither a binary field nor a drop-in
TFHE torus/ring. Its modulus/ring/noise choices must not be presented as a
TFHE, FHEW, GINX, bootstrap, or failure-probability parameter set. In
particular, neither `q = 3329` nor an ML-KEM parameter-set name is a
"conservative baseline" for a TFHE bootstrap.

The initial reusable candidate is precisely bounded: **FIPS 203 Algorithm 8's
integer CBD mechanics**, not ML-KEM encryption or its parameter set. For
`eta ∈ {2, 3}`, it consumes exactly `64*eta` bytes; `BytesToBits` uses
little-endian bits within each byte; for each of 256 coefficients it subtracts
the population count of the next `eta` bits from that of the preceding `eta`
bits. The signed result is in `[-eta, eta]`; Algorithm 8 represents the
negative values modulo its own `q`. A rework sampler must retain the signed
integer result until an explicitly reviewed profile embeds/scales it in its
own coefficient domain.

The other initial candidates are engineering mechanics that can be specified
independently of the scheme:

- fixed-width, overflow-audited integer arithmetic and integer-only rounding;
- deterministic expansion from an explicitly supplied test seed;
- explicit domain separation for every test-only seed stream; and
- known-answer, boundary, byte-order, and serialization tests for the sampler
  itself.

FIPS 203 §4.1 defines its PRF as SHAKE256 over a 32-byte seed and one byte,
with an output length determined by `eta`; that exact wrapper may be adopted
only after the rework chooses and documents a hash dependency. Until then the
sampler may consume supplied test bytes directly. A SHAKE/XOF, byte encoding,
or sampler copied or adapted from FIPS 203 needs its own exact source mapping,
implementation review, and test vectors. No floating-point sampler,
acceptance criterion, or parameter/security claim is implied by this draft.

## 3. Construction choices that must remain explicit

Before code, the owner and a cryptographic reviewer must select one coherent
construction profile. A profile records all of the following together:

1. coefficient domain(s): input LWE, accumulator RLWE/RGSW, and key-switch
   modulus/domain; whether they are powers of two or use explicit modulus
   switching;
2. polynomial quotient, degree, rank, coefficient representation, reduction,
   and negacyclic coefficient order;
3. secret distribution and independently specified error sampler, including
   seed expansion, centering, rejection behavior if any, and exact overflow
   semantics;
4. Boolean encoding, decoder intervals, canonicality predicate, and the type
   distinction between a fresh canonical ciphertext and an affine/raw phase;
5. gadget base/level, decomposition convention, RGSW layout, external product,
   sample extraction, key-switch direction, and all rounding rules; and
6. a parameter-exploration range plus a statement that observed test failures
   are not a security estimate or a proven failure bound.

The initial sampler-only exploration profile is `N = 256`, CBD `eta = 3`, and
an explicit integer scale into the selected TFHE coefficient domain. `N` and
`eta = 3` are borrowed solely as a reproducible starting shape from FIPS 203
(Table 2 uses `eta1 = 3` for ML-KEM-512); `q = 3329`, ML-KEM matrix rank,
NTT, compression, and security category are excluded. The scale, secret
sampling, torus/modulus, and every bootstrap/key-switch parameter remain
unselected until the profile review. This is a *test baseline*, not a
conservative security baseline.

A binary extension field is a possible research question, not a drop-in fix.
Changing from a torus/coefficient ring to `GF(2^k)` changes the plaintext
embedding, error model, multiplication/decomposition behavior, and the
construction to which TFHE/GINX proofs apply. It may be considered only as a
separate profile with a cited construction and new oracle; it must not be
introduced merely because it has a convenient size.

## 4. Completeness harness: the first implementation deliverable

The primary acceptance signal is a deterministic differential harness that
executes a ciphertext operation sequence and its clear counterpart in parallel.
It is **self-consistency evidence only**: it can find incorrect signs,
canonicalization, parameter interactions, and boundary failures, but cannot
prove a security level, a noise bound, or paper equivalence.

### 4.1 Model and invariants

For every generated case the harness records:

- a reproducible master seed and domain-separated sub-seeds for keys,
  encryption, noise, program generation, and shrinking;
- the clear Boolean value and, when relevant, the exact expected affine phase;
- the ciphertext value, decrypted phase, decoded value, and canonicality;
- the profile identifier and every parameter used; and
- the operation index, operation inputs, and an auditable failure category.

The clear side must use `tfhe_ginx_oracle.rs` certificates for refreshed gates
and a small, standalone Boolean-program evaluator for composition. It must not
invoke the ciphertext helper under test to compute an expected answer.

Every public operation must state one of two contracts:

- **canonical output:** decrypts to the expected Boolean and meets the profile's
  canonical phase/decoder-margin predicate, so it may feed another supported
  public operation; or
- **raw affine output:** only an explicitly named internal/test operation may
  return it; it carries the expected phase relation and is rejected by APIs
  requiring a canonical ciphertext.

The legacy raw-XOR counterexample becomes a permanent negative regression:
`true XOR true` must be detected as noncanonical and must not be permitted as a
composable Boolean wire. A rework XOR is accepted only when the harness shows
that it returns a canonical value for every generated input and composition.

### 4.2 Required test families

| Family | Generated action | Required assertion |
|---|---|---|
| Encryption baseline | keys, both Boolean messages, fresh seeds/noise | decrypts correctly; output is canonical; failures retain seed/profile |
| Primitive operations | NOT, affine preparation, AND/OR/NAND/NOR/XOR/XNOR, and any LUT/PBS wrapper | decrypted clear value matches the independent clear model; each advertised composable result is canonical |
| Program composition | random typed Boolean DAGs and fixed adversarial chains, with fan-out and repeated refresh | ciphertext evaluation equals standalone clear evaluation at every observable output; no raw result crosses a canonical boundary |
| Boundary corpus | phases adjacent to every decoder/rounding boundary; zero, one, maximal, and sign-changing decomposition digits | expected interval/canonicality classification is exact and reproducible |
| Noise ladder | same corpus across an explicitly labelled sequence from zero noise through exploratory nonzero noise | report failures by profile and seed; never label an empirical rate a security/failure bound |
| Sampler KAT/property tests | fixed seeds, output lengths, centering, range, reproducibility, and stream separation | exact expected byte/coefficients for approved vectors plus structural invariants |
| Metamorphic checks | encrypt/decrypt round trips, independently re-encrypted equivalent plaintext programs, and operation identities only when they preserve the canonical contract | disagreement is a failure; identities must not hide a noncanonical intermediate |

The default CI corpus should be bounded, deterministic, and shrunken on failure.
A separate manually invoked stress corpus may use many seeds and generated DAGs,
but its seed list and result summary must be retained. Randomness without a
recorded seed is not acceptable evidence.

### 4.3 Failure taxonomy

Tests report one of: decrypt mismatch, noncanonical advertised output,
decoder-margin breach, phase/rounding mismatch, sampler KAT mismatch,
overflow/reduction mismatch, unsupported-profile rejection, or harness defect.
A failure is not fixed by changing an expected fixture until the clear model,
profile, and reference record explain the discrepancy.

## 5. Rework sequence and gates

1. **Freeze and inventory.** Preserve legacy public symbols and tests; enumerate
   all `tfhe.rs` callers and generated mirrors. Add the raw-XOR negative
   regression before moving an API.
2. **Define profile data, not global constants.** Introduce an internal,
   fixed-shape profile descriptor that makes all domains, sampler inputs,
   canonicality rules, and decomposition parameters explicit. No dynamic
   parameter discovery in generated targets.
3. **Implement and test the sampler in isolation.** Use no floating point.
   Bind exact FIPS 203 material only after a reviewer verifies the hash and
   cited algorithm; publish known-answer vectors before using it for noise.
4. **Build the clear/ciphertext completeness harness.** Start with zero noise
   and a minimal coherent profile, then add the approved sampler and an
   exploratory nonzero-noise ladder. A green encryption-only test is not a
   gate-PBS result.
5. **Implement one refreshed Boolean operation at a time.** Require primitive
   and composition coverage before exposing the next operation. The proposed
   default first target is canonical XOR specifically because it addresses the
   known inconsistency; it must use a reviewed refresh mechanism, not raw LWE
   addition relabelled as XOR.
6. **Stage-level and external comparisons.** Reuse the existing rotation/CMUX/
   extraction/key-switch conformance tests only after their profile mapping is
   explicit. Produce shared vectors and compare them with the pinned `tfhe-go`
   record only when parameters and layouts are deliberately aligned.
7. **Human review decision.** A cryptographic reviewer decides whether the
   selected profile has enough construction/parameter/noise evidence to proceed
   beyond exploratory testing. Passing the harness never advances pinnedness by
   itself.

## 6. Non-goals and stop conditions

This draft does not authorize generic PBS, arbitrary LUTs, circuit
bootstrapping, a parameter/security claim, interoperability, use in a weaver,
or replacement/removal of legacy APIs. Stop and update the core specification
if a profile cannot state a canonical Boolean invariant, if a sampler cannot be
made reproducible and integer-only, if a generated composition produces a
noncanonical result, or if a required source-to-operation mapping is unclear.

The next smallest action after owner/reviewer profile selection is to add the
legacy raw-XOR negative regression and a test-only profile/harness skeleton;
it is not to transplant ML-KEM parameters into `tfhe.rs`.