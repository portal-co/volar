# TFHE GINX validation handoff

- **Current merged base:** `08d1d33`; source stream `fc1c5b2`, `b7da7eb`,
  `46dce80`, with static-shape interaction in `63e2ddc`/`8604107`.
- **Evidence:** Pi session `019f82d8-65e9-71a0-b6c8-c82198c83420`, Git diffs,
  [`docs/tfhe-pbs-rework-plan.md`](../../tfhe-pbs-rework-plan.md), and
  [`docs/tfhe-ginx-core-spec.md`](../../tfhe-ginx-core-spec.md).
- **Implemented and verified:** Phase 1 updated new conformance tests for the
  const-generic decomposition-base API. `cargo test -p volar-spec` passed on
  2026-07-22. Test-only clear-oracle/conformance material and the core-spec
  record are present; the historical three-address-bit/general-table prototype
  was rejected, not retained.
- **Current failures / blockers:** no Phase-1 `volar-spec` failure remains.
  This does not establish Gate B or Gate C cryptographic acceptance: local toy,
  zero-noise tests are functional evidence only. Wider matrix work is currently
  stopped first by the LIR unresolved-`L` issue, not a TFHE conclusion.
- **Invariants and non-goals:** remain Experimental. Do not claim a security
  level, noise bound, interoperability, generic PBS, arbitrary table, or a
  one-bootstrap three-bit selector. Never make direct linear `tfhe_xor` a
  generic composable Boolean wire. Preserve typed IR and `Transparent`
  discipline.
- **Collision changes since the original plan:** tests that were written for
  runtime decomposition-base arguments now target static const generics. The
  two-address-bit limit is a current selector/encoding boundary, not proof that
  higher-arity Boolean functions are intrinsically impossible.
- **Next smallest safe action:**
  1. Read the core spec and rework plan before changing `tfhe.rs`.
  2. Independently review each stage oracle against its cited paper operation;
     add a mutation test only when it uses an independently calculated result.
  3. Record whether the Gate-B test set is complete against the plan; if a
     phase/sign/rotation/extraction discrepancy appears, stop and update the
     core spec rather than adjusting a fixture.
  4. Only after Gate B, seek the independent-reference and nonzero-noise review
     specified for Gate C.
- **Completion evidence:** the rework-plan Gates A–C, an independent reference
  or reviewer-approved implementation, and generated-code weave → print →
  keygen → encrypt → execute → decrypt tests. Passing the current unit suite is
  necessary but not sufficient.
- **Documents to update before coding:** `docs/tfhe-pbs-rework-plan.md`,
  `docs/tfhe-ginx-core-spec.md`, `docs/tfhe-multi-input-pbs-weaver-plan.md`,
  `docs/spec-static-shapes-plan.md`, and this handoff.