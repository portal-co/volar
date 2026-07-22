# TFHE two-track decision: legacy steady state and v2 research

**Status:** active legacy-cleanup plan plus a v2 design boundary. It makes no
cryptographic security, parameter, RLWE/TFHE conformance, interoperability, or
dependent-compatibility claim.

**Scope:** the legacy experimental module
`crates/spec/volar-spec/src/tfhe.rs`. The existing GINX oracle and review
records remain evidence about their stated, narrow scopes only.

## Decision

TFHE work now has two deliberately separate tracks. They share a first action:
remove known-inconsistent legacy public behavior and do not preserve weaver,
generated-mirror, C, or TypeScript compatibility while that removal occurs.

1. **Track S — legacy steady state.** Reduce the existing `tfhe.rs` to the
   smallest public toy subset for which deterministic ciphertext-vs-cleartext
   cross-tests demonstrate the stated *toy functional* contract. Its intended
   workflow is: preserve the test corpus; remove an operation with a failing,
   ambiguous, raw-phase, or untested composability contract; then make a small
   parameter/noise exploration only while retaining the same completeness
   corpus. This is an evidence-backed maintenance target, not a review,
   security result, or promise that the retained subset is a conforming TFHE
   implementation.
2. **Track V2 — new construction research.** Keep
   [`tfhe-mlkem-rework-draft.md`](tfhe-mlkem-rework-draft.md) as a draft while
   the owner researches an explicit field/ring representation and
   optimization-aware construction. It may investigate new scheme-level ground
   or compiler/IR optimization strategies, but begins only from an explicit
   profile, clear oracle, paper-binding plan, and its own tests. It does not
   patch, extend, or infer correctness from Track S.

Neither track currently targets the weaver. No present generated backend is a
compatibility requirement, and no removal in Track S is to be masked by a
shim, alias, or generated artifact edit. A future owner decision may define a
new integration boundary only after Track S has an evidence record or a V2
profile has an explicit claim.

## Why the legacy module must shrink first

The legacy module combines torus values, ring coefficients, gadget digits,
encoded booleans, and raw phases as mostly plain `u32`s. It is intentionally
small and pre-optimized, but that representation makes domain boundaries
implicit and makes it unsuitable as a safe place for incremental feature
addition. It is also a poor source for speculative agent changes: a passing
single truth table can conceal a noncanonical intermediate or a mismatched
ring/phase convention.

The known example is `tfhe_xor`: linear addition decodes `true XOR true` as
false only at final observation, while its phase is `2*Q4`, not the canonical
false phase zero. It is therefore neither a composable Boolean wire nor an
acceptable public operation for Track S. Its removal is a cleanup action, not
a claim that a replacement XOR has been constructed.

Similarly, legacy caller-provided programmable bootstrap and fixed-table/LUT
surfaces combine incomplete accumulator/selector semantics with generated
weaver dependence. They have no stable Track-S contract and are scheduled for
removal rather than broadening or repair. This includes their table descriptor
and the specialized LUT-XOR wrapper. Their removal does not decide whether V2
will have PBS or LUTs.

**Current cleanup snapshot:** direct raw `tfhe_xor` and the specialized
`tfhe_lut_xor` wrapper have been removed. The generic caller-programmable
bootstrap/table surfaces remain only until their dedicated canonicality and
composition decision is recorded; they are not Track-S admission evidence and
must not be used to restore weaver compatibility. At this cleanup snapshot,
`cargo test -p volar-spec --lib --no-fail-fast` passes 165 tests, including the
new deterministic canonical-phase cross-operation test. That is only
zero-noise toy-profile self-consistency evidence for the named survivors.

## Track S acceptance rule and initial subset

Every surviving public Boolean-producing operation must satisfy all of the
following under the named deterministic, zero-noise toy fixture:

- it has a clear Boolean counterpart independent of the operation under test;
- its result decrypts to that counterpart for the exhaustive Boolean inputs and
  a fixed multi-seed corpus;
- its decrypted torus phase is exactly the canonical fixture representative
  (`0` for false, `Q4` for true), not merely in the final decoder interval;
- that result is accepted by every other surviving composable operation in the
  exhaustive cross-operation corpus; and
- random bounded Boolean-DAG tests compare every intermediate ciphertext with
  the parallel clear evaluation and check canonicality at every step.

Tests must report their fixture dimensions, decomposition parameters, noise
setting, operation sequence, and reproducible seed. These assertions are
**self-consistency evidence for one toy profile only**. They are not a noise
bound, parameter selection, TFHE/GINX conformance, or security claim.

The candidate initial subset is intentionally only a candidate: encryption and
decryption test helpers, trivial constants, `NOT`, bootstrapped `AND`,
bootstrapped `OR`, and `CMUX` composed from those gates. Each remains only if
it passes the new cross-tests. Internal stage helpers may remain solely to
implement a surviving operation and retain their existing conformance tests;
they are not a new public contract.

## Ordered cleanup procedure

1. Add the canonical-phase helper and deterministic plaintext/ciphertext
   cross-tests to `tfhe.rs`; retain any failure as a regression.
2. Remove direct linear XOR and its property/composition tests.
3. Remove the caller-programmable bootstrap, table descriptor/error, LUT read,
   and LUT-XOR APIs and their tests. Do not edit generated mirrors to preserve
   them.
4. Run the focused `volar-spec` suite. If a candidate survivor fails either
   truth-value or canonical-phase composition, remove it with its dependent
   legacy tests and record the exact reproducer here or in a handoff before
   proceeding.
5. Commit the evidence-backed reduced surface. Only then may a separate branch
   explore a parameter/noise adjustment while running the unmodified
   completeness corpus.

At the end of Track S, document one of two honest claims: either a named,
self-consistent toy subset and its reproducible test command, or that no
public legacy TFHE operation remains. Neither outcome changes its legacy
Unpinned/Very-unstable status.

## Track V2 boundary

V2 is larger work, not a cleanup continuation. It must make the coefficient
field/ring, encoding, key domains, noise sampler, and optimization boundary
explicit rather than preserving the legacy `u32` packing. It may assess both
in-scheme field/ring handling and IR/optimization-pipeline approaches, but
those are research choices with distinct claims and evidence. The FIPS 203
material recorded in the V2 draft is currently only a source of
integer-arithmetic/sampling mechanics, never a TFHE parameter transplant.

A V2 implementation starts in a new module and stays unreachable from the
weaver until its profile and composition evidence justify a separately reviewed
integration decision.