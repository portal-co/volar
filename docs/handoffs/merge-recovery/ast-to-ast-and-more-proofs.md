# AST-to-AST and more-proofs handoff

- **Current merged base:** `08d1d33`; evidence-only checkout
  `/Users/g/Code-local/portal-labs/volar-more-proofs` is at `7938618`.
- **Evidence:** `feat/ast-to-ast-weavers` at `c5ec593`, `stash@{0}` based on
  that branch, the more-proofs conversation family in [sources.md](sources.md),
  and [`docs/agent-context/ast-to-ast-weaving.md`](../../agent-context/ast-to-ast-weaving.md).
- **Implemented and verified:** no AST-to-AST weaving implementation has landed
  in this merged tree. The context document is a preserved future-track scope,
  not a design approval or backend replacement.
- **Current failures / blockers:** the unmerged branch and stash may contain
  experiments that assume a different tree. They have not been reconciled with
  static shapes, metadata/groups, current pipeline semantics, or the
  ZK/non-ZK discipline. Never apply the stash or cherry-pick the branch as
  recovery.
- **Invariants and non-goals:** the established `print_module` path remains the
  real-backend route for current prove-the-verifier work. A direct AST transform
  may be considered only for a concrete new target; it must use typed IR/AST
  nodes, preserve provenance and discipline, and earn compile-and-run
  correctness evidence. This handoff does not authorize a generic bypass of
  LIR.
- **Collision changes since the original plan:** current LIR
  monomorphization is actively being refactored and has an unresolved `L`
  widening failure. That may motivate assessing an AST-to-AST track, but it is
  not evidence that bypassing LIR is correct; first characterize the LIR
  failure and its scope.
- **Next smallest safe action:** inspect the branch/stash with read-only Git
  commands, list conceptual prerequisites and touched subsystems, and compare
  them with `docs/pipeline.md`, the discipline context, and current source.
  Write a fresh target-specific design/review plan before moving any code.
- **Completion evidence:** a reviewed target-specific proposal, explicit
  pipeline placement, preservation tests, and generated-backend execution;
  none is currently satisfied.
- **Documents to update before coding:**
  `docs/agent-context/ast-to-ast-weaving.md`, `docs/pipeline.md`,
  `docs/prove-the-verifier-iop.md`, and this handoff.