# Real interpreter and virtualisation handoff

- **Current merged base:** `08d1d33`; key pre-merge evidence includes
  `07ee764`, `a79ac68`, and `a00c8db`.
- **Evidence:** Claude main-checkout session
  `75ca62ab-89b0-47ab-ba2f-c46037635378`, the associated RISC-V/IOP context,
  Git history, `PROGRESS.md`, and [sources.md](sources.md).
- **Implemented and verified:** small reproducers separated a false
  step-budget diagnosis from the real dispatch read-modify-write/back-edge
  investigation. A return-slot invariant was hardened. Independently,
  `volar-ir-virt` has public and oblivious dispatch work plus focused
  equivalence/property tests recorded in `PROGRESS.md`.
- **Current failures / blockers:** do not describe the original non-halting
  report as either universally fixed or a current regression without rerunning
  its smallest reproduction. `PROGRESS.md` still lists important virtualisation
  limits: IR oblivious dispatch does not yet support `JumpTable`; external
  bytecode printer integration and BIR varied-parameter support are deferred.
- **Invariants and non-goals:** preserve minimal reproducers and distinguish
  observed behavior from diagnosis. Do not hide a dispatch, return-slot, or
  back-edge bug by increasing a step budget. Retain the generated-backend
  compile-and-run requirement; virtualisation must preserve storage and return
  semantics, not merely deduplicate handlers structurally.
- **Collision changes since the original plan:** metadata/provenance and
  instruction-group work now add preservation/consumption obligations to IR
  transforms. A virtualisation or interpreter change must not discard required
  group metadata or manufacture provenance. The active LIR monomorphization
  failure also blocks a broad C/weaver validation ring but is not itself a
  virtualisation diagnosis.
- **Next smallest safe action:**
  1. Run the named minimal dispatch/back-edge repro and record its exact
     current behavior and command.
  2. If it fails, minimize again before changing movfuscation, VAFFLE spill
     logic, or virtualisation.
  3. Separately select one deferred virtualisation item, add a real backend
     compile-and-run test, and preserve provenance/group invariants.
- **Completion evidence:** stable reproducer result, focused equivalence and
  property tests, and generated Rust/C/TS execution where applicable.
- **Documents to update before coding:** `PROGRESS.md`, the relevant test
  fixture/repro comment, `docs/pipeline.md` for pipeline changes, and this
  handoff.