# Static shapes and monomorphization handoff

- **Current merged base:** `08d1d33`; relevant stream `242a3f4`, `ff42bfd`,
  `63e2ddc`, `8604107`, `e914e00`.
- **Evidence:** Pi session `019f79e0-07a9-7ae6-9dd4-b6470683f481` and Git
  history; see [sources.md](sources.md). Primary design record:
  [`docs/lir-lowering-monomorphization-plan.md`](../../lir-lowering-monomorphization-plan.md).
- **Implemented and verified:** TFHE decomposition-base/static-shape call sites
  now use the current const-generic APIs; the Phase-1 TFHE suite passes. The
  existing `MonoEnv`/whole-module lowering model and its downstream consumers
  still exist. The LIR plan describes a replacement based on per-function
  instance discovery, concrete nominal-type identities, and deterministic
  mangling; it is not evidence that that refactor is complete.
- **Completion evidence (2026-08-04):** Unbound `L` was caused by rooting every
  Normal function (including `encrypt_branch::<R,L>`) under one `MonoEnv`.
  Fix: plan roots are non-generic or explicitly env-bound; `lir_backend` uses
  empty/`plan_flat_module` and asserts `encrypt_branch` is not planned unbound.
  Instance-keyed `Vope`/`Wrap` layouts, typenum `U{n}` canonicalization,
  `lower_cfg_module_monomorphized`, `SavedLirModule::replay_pair` /
  `replay_into_many`, `volar-wasm-backend`, and woven AND e2e
  (`vole_e2e` + `vole_and_record_replay_c_and_wasm`) pass.
- **Invariants and non-goals:** LIR layouts must be concrete; no unresolved
  `TypeParam`, type-parameter array length, or projection may reach lowering.
  Never silently select a specialization. Keep the ZK/non-ZK discipline intact.
  Generated-code tests must compile and run; rendered names or LIR shape alone
  are insufficient. Do not paper over this by choosing a test-only `L`.
- **Collision changes since the original plan:** the original transcript scoped
  the lowering-time monomorphization refactor to `volar-lir-codegen` and
  treated existing backend callers largely as later consumers. **Phase-1
  widening now exposes an unresolved `L` through `encrypt_branch`; moreover,
  an in-progress LIR monomorphization refactor may have effects beyond that
  transcript's original call graph and stated scope.** Treat the failure as a
  cross-boundary reconciliation signal: inspect instance planning, signature
  lookup, nominal layout registration, CFG/auxiliary paths, and all
  `MonoEnv`-accepting callers before attributing it solely to C backend tests.
- **Next smallest safe action:**
  1. Reproduce the command above with structured logs and preserve the first
     unresolved-`L` stack/error context.
  2. Trace `encrypt_branch` from its IR definition through every call and
     `MonoEnv` construction; identify whether `L` should be a root binding,
     explicit callee binding, or rejected unresolved layout.
  3. Compare that path to the plan's instance-key/signature/registry invariants
     and make a minimal typed diagnostic or propagation fix—never a fallback
     default.
  4. Add a real C compile-and-run regression for the resolved generic case,
     then widen to CFG and weaver callers.
- **Completion evidence:** `volar-lir-codegen` and `volar-c-backend` tests,
  plus generated C compilation and execution for more than one specialization;
  then the Phase-1 widening ring and LLVM-capable workspace matrix.
- **Documents to update before coding:**
  `docs/lir-lowering-monomorphization-plan.md`,
  `docs/spec-static-shapes-plan.md`, `PROGRESS.md`, and this handoff with the
  exact diagnosis and test result.