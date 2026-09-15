# FHE / Homomorphic Evaluation

This directory contains the generic FHE-weaver material and the legacy TFHE
records. The TFHE implementation is **Unpinned and Very unstable**; none of
these documents supplies a security, parameter, interoperability, or deployment
claim.

## Current direction

- [TFHE two-track plan](tfhe-two-track-cleanup-plan.md) — Track S is the
  speculative legacy steady state, backed by a growing completeness/fuzzing
  corpus; Track V2 is separate construction research.
- [Track S evidence ledger](tfhe-steady-state-evidence.md) — the authoritative
  record of the exact legacy profile, reproducible corpus, failures, and
  admissible next experiments.
- [Integer-sampled V2 draft](tfhe-mlkem-rework-draft.md) — design draft only.
- [`binfhe` V2 implementation plan](binfhe-v2-implementation-plan.md) —
  research + plan for a licensed, paper-bound GINX/CGGI replacement module
  with programmable and circuit bootstrapping and a shared weaver/interpreter
  bootstrap-plan structure.
- [Vec elimination + IR linter plan](vec-elimination-and-linter-plan.md) —
  remove weaver-known `Vec` from the spec and lint `Vec` in compiler IR
  unless a documented `@volar-allow-vec:` exemption is present.

## Validation and historical planning

- [TFHE PBS rework/validation plan](tfhe-pbs-rework-plan.md)
- [GINX core-spec draft](tfhe-ginx-core-spec.md)
- [Multi-input PBS/weaver plan](tfhe-multi-input-pbs-weaver-plan.md)
- [Oracle paper binding](reviews/tfhe-ginx-oracle-paper-binding.md)
- [`tfhe-go` reference reconnaissance](reviews/tfhe-ginx-tfhe-go-reference.md)

## Integration

- [Generic FHE weaver](weaver.md) documents the compiler abstraction. It is not
  a requirement for Track S. A future Track-S experiment may become
  weaver-compatible only after the evidence ledger covers the exact emitted
  behavior; it is never preserved merely for legacy compatibility.