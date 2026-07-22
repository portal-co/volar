# Metadata and instruction groups handoff

- **Current merged base:** `08d1d33`; source stream `9aa70b0`, `da42b31`,
  `979b35a`.
- **Evidence:** Pi session `019f7edf-6dec-7c05-ae46-70c40f1142f0`, source
  history, [provenance pipeline context](../../agent-context/provenance-pipeline.md),
  [`metadata-container-plan.md`](../../metadata-container-plan.md), and
  [`instruction-groups-plan.md`](../../instruction-groups-plan.md).
- **Implemented and verified:** the merged stream introduced metadata/group
  declarations, instances, typed captures, group membership propagation,
  marker recognition/validation, lossy-pass barriers, Volar-IR v2 text
  support, and advisory-group generator/property coverage. The historical
  focused checks recorded 7 `volar-ir-common`, 2 VAFFLE, 82 IR-passes, 14
  IR-opt, and focused text/fuzz passes; do not replace current reruns with this
  record.
- **Current failures / blockers:** Phase 1 found legal statement-free inputs
  that lacked an automatic provenance owner. It added explicit
  control-provenance entry points for movfuscation, circuit lowering, and
  VAFFLE lowering; fuzz supplies `()` only as an explicit provenance-free
  model. `cargo test -p volar-vaffle-target`, `-p volar-ir-passes`, and the
  selected fuzz/virt ring now pass. The broader LIR monomorphization failure is
  separate.
- **Invariants and non-goals:** provenance is never invented: no `P: Default`,
  `synthetic()`, guessed owner, or fuzz-only special case. Required instruction
  groups must be consumed/rejected before lossy lowering; ordinary transforms
  preserve membership and captures, while combining sources requires an
  explicit policy. Group-bearing calls may not enter unmodelled stack-frame
  lowering.
- **Collision changes since the original plan:** the plans still contain
  planning-era wording, but substantial implementation landed before the
  merge. Empty-program behavior is now an additional cross-cutting provenance
  case: it must use caller-supplied enclosing control provenance or a typed
  error, not weaken metadata/provenance rules.
- **Next smallest safe action:**
  1. Re-run the current focused metadata/group tests and identify remaining
     consumers from the real source, not plan status headings.
  2. Audit every infrastructure emission site for an explicit provenance owner
     and every group-aware transformation/table remapper for membership/capture
     preservation.
  3. Add a compile-and-run regression for a legal empty program and typed
     diagnostics for malformed no-body input.
  4. Update stale plan ledgers before adding a group consumer or metadata axis.
- **Completion evidence:** backend compile-and-run coverage, text/serialization
  round trips, valid-group generator/property coverage, and explicit rejection
  tests at every lossy boundary.
- **Documents to update before coding:** `docs/metadata-container-plan.md`,
  `docs/instruction-groups-plan.md`, `docs/agent-context/provenance-pipeline.md`,
  `docs/agent-context/ir-map-conventions.md`, and this handoff.