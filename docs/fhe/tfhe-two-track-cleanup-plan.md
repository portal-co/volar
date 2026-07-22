# TFHE two-track decision: speculative steady state and V2 research

**Status:** active legacy-cleanup plan plus a separate V2 design boundary. It
makes no cryptographic security, parameter, RLWE/TFHE conformance,
interoperability, or deployment claim.

**Scope:** `crates/spec/volar-spec/src/tfhe.rs`. The authoritative operational
record for Track S is the [steady-state evidence ledger](tfhe-steady-state-evidence.md).
The GINX oracle, reference reconnaissance, and review records retain only their
stated narrow scopes.

## Decision

TFHE work has two deliberately separate tracks. Both begin by removing known
inconsistent behavior instead of preserving an API for a weaver or generated
backend.

1. **Track S — speculative legacy steady state.** Shrink `tfhe.rs` to a
   self-consistent baseline, then let agents make speculative changes against a
   growing clear/ciphertext completeness corpus. This is explicitly an agentic
   experimentation track: the legacy module started that way, remains that
   way, and its packed `u32` torus/ring/digit/phase representation is not
   type-safe enough to infer correctness from local code review. The steady
   state exists to make experiments debuggable and non-vacuous, not to make a
   new V1 construction.
2. **Track V2 — separate construction research.** Keep the
   [integer-sampled rework draft](tfhe-mlkem-rework-draft.md) as a draft while
   the owner researches explicit field/ring representation and
   optimization-aware construction choices. V2 starts from a declared profile,
   clear oracle, paper-binding plan, and its own tests; it neither patches nor
   inherits correctness from Track S.

Track S is permanently Unpinned and Very unstable unless an implementation-linked
formal proof establishes it as Proven. Tests, larger parameters, nonzero noise,
papers, or informal review may strengthen a narrow evidence entry but cannot
promote this legacy track. This is stricter than V2's ordinary new-construction
path because Track S deliberately retains the legacy `u32` representation and
agent-experiment character.

## The steady-state objective

The objective is a durable **experimental** baseline:

- every retained public Boolean wire operation has an independent clear model;
- generated circuit topology and operation sequence are fuzzing inputs, not
  fixed test scaffolding;
- every intermediate is checked for Boolean equality and canonical phase;
- failures retain a seed, profile, and shrunken circuit; and
- the same corpus remains green as dimensions/noise are explored.

The evidence ledger, not a design plan, is the strongest possible document for
Track S. It must state exactly what was run and what it observed, so that a
later agent can distinguish a supported experiment from an attractive but
untested guess. It cannot establish a security level or reliably classify a
construction as cryptographically valid; unlike a strongly typed or proven
ZK boundary, the legacy packing leaves too much semantic intent implicit.

## Compatibility

Shrinking comes before compatibility. No existing weaver, dynamic mirror, C,
or TypeScript surface is preserved merely to keep compiling. However,
weaver-compatible Track S is a valid *future experiment*: after the core
completeness corpus is established, an agent may propose an emitted-path test
corpus and record it in the ledger. Such a result remains speculative Track S,
not V1/V2 or a promoted FHE implementation.

## Current cleanup snapshot — `92dfc28`

Raw linear `tfhe_xor` and the specialized LUT-XOR wrapper are removed. The
baseline deterministic toy corpus checks the surviving candidate subset across
exhaustive multi-seed cross-operation cases and bounded generated Boolean DAGs,
including exact canonical phase. `cargo test -p volar-spec --lib --no-fail-fast`
passed 165 tests at this snapshot. The generic programmable-bootstrap/table
surfaces remain excluded from Track-S admission while their removal/assessment
is decided.

## Ordered Track-S procedure

1. Preserve and extend the evidence-led completeness corpus before changing an
   operation.
2. Remove a raw-phase, ambiguous, untested, or cross-test-failing surface; do
   not provide a shim for a generated backend.
3. Add agent-proposed operations, parameter changes, noise experiments, or
   possible weaver paths only with a clear counterpart and shrinkable circuit
   corpus.
4. Record exact profile, command, seed/circuit evidence, successes, and
   failures in the ledger.
5. Keep the baseline corpus unchanged when widening the exploration. A changed
   expected result requires an explained clear-model change, not fixture drift.

## V2 boundary

V2 may investigate explicit coefficient fields/rings, encoding and key domains,
noise sampling, or optimization-pipeline strategies that the legacy module
cannot express cleanly. The FIPS 203 material in its draft is only a source of
integer-arithmetic/sampling mechanics, never a TFHE parameter transplant. A V2
implementation starts in a new module and needs a separately justified
integration decision.