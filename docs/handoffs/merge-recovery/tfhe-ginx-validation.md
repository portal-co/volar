# TFHE GINX validation handoff

- **Current merged base:** `08d1d33`; source stream `fc1c5b2`, `b7da7eb`,
  `46dce80`, with static-shape interaction in `63e2ddc`/`8604107`. Current
  TFHE evidence snapshot: `75db00c`.
- **Evidence:** Pi session `019f82d8-65e9-71a0-b6c8-c82198c83420`, Git diffs,
  [`docs/tfhe-pbs-rework-plan.md`](../../tfhe-pbs-rework-plan.md),
  [`docs/tfhe-ginx-core-spec.md`](../../tfhe-ginx-core-spec.md), the
  [oracle paper binding](../../reviews/tfhe-ginx-oracle-paper-binding.md), and
  the [tfhe-go reference record](../../reviews/tfhe-ginx-tfhe-go-reference.md).
- **Implemented and verified:** Phase 1's clear oracle is paper-pinned at
  `75db00c` with direct operation citations and PDF hashes. The Phase 2 suite
  now independently checks polynomial rotation, exact-grid whole blind
  rotation, CMUX, sample extraction, and key switching. The focused blind
  rotation test and the selected external reference's `go test ./tfhe` passed
  on 2026-07-22. The historical three-address-bit/general-table prototype was
  rejected, not retained.
- **Current failures / blockers:** no focused `volar-spec` failure remains.
  Phase 2 coverage is not a Gate B reviewer verdict. `tfhe-go` has a different
  LWE dimension, GLWE rank, noise/decomposition configuration, and ciphertext
  layout, so it cannot yet consume Volar test vectors; Gate C remains open.
  Local toy zero-noise tests are functional evidence only. Wider matrix work is
  currently stopped first by the LIR unresolved-`L` issue, not a TFHE conclusion.
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
  1. Read the core spec, rework plan, paper binding, and external-reference
     record before changing `tfhe.rs` or the oracle.
  2. Have an independent cryptographic reviewer decide whether the existing
     stage cases and their paper mappings satisfy Gate B; if a
     phase/sign/rotation/extraction discrepancy appears, stop and update the
     core spec rather than adjusting a fixture.
  3. Before attempting a differential test, obtain approval for one mapped
     parameter/noise/decomposition model and a shared stage-vector format as
     specified by the external-reference record.
  4. Only after that mapping, run the independent-reference and separately
     approved nonzero-noise work required for Gate C.
- **Completion evidence:** the rework-plan Gates A–C, an independent reference
  or reviewer-approved implementation, and generated-code weave → print →
  keygen → encrypt → execute → decrypt tests. Passing the current unit suite is
  necessary but not sufficient.
- **Documents to update before coding:** `docs/tfhe-pbs-rework-plan.md`,
  `docs/tfhe-ginx-core-spec.md`, `docs/tfhe-multi-input-pbs-weaver-plan.md`,
  `docs/spec-static-shapes-plan.md`, and this handoff.