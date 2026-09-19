# MPC external actions and oracles ledger

**Status:** authoritative implementation/evidence ledger for
[`mpc-external-actions-oracles-plan.md`](mpc-external-actions-oracles-plan.md).

This ledger covers external actions, pure oracles, executor assignment,
revelation, reinsertion, and boundary planning across MPC surfaces. It is
separate from the FHE provider ledger: an FHE/provider action boundary may
reference these rows but cannot satisfy them by using legacy `FheScheme` or an
unreviewed provider implementation.

Every deferred external-MPC check must cite one row here in a nearby
`TODO(mpc-external-ledger: ID)` comment. Do not claim executor-private
confidentiality, malicious security, replay resistance, or a concrete host
semantic guarantee merely because a plan/model type exists.

| ID | Seam | Required evidence | Status |
|---|---|---|---|
| `MPC-EXT-DECL-01` | `ExternalExecutor`, `ExternalRevealPolicy`, action/oracle fingerprints carried through schema-generated declarations and lowering | IR/VAFFLE/text/serialized round trips; explicit policy test at every frontend and declaration merger | **PARTIAL:** `volar-ir` schema records explicit policy/fingerprint and legacy evaluator policy constructors. WASM `WaffleImportConfig` now accepts explicit oracle/action policy registrations and preserves them through VAFFLE-to-Volar-IR declaration remapping; configured mappings are prevalidated against actual imported functions with fixed scalar-integer signatures, exact action arity, an `i32` guard, and result-typed fallbacks before body lowering. Compatibility helpers remain legacy evaluator policy. Existing LLVM AES lowering, other declaration construction, substitution, fuzz generation, and text parser paths retain explicit compatibility policy. LLVM structural import now has an opt-in symbol-keyed `LlvmImportConfig` registry that lowers scalar direct calls into real `Stmt::OracleCall` / `Stmt::ActionCall` declarations carrying explicit policies; a fixture covers garbler + `BothRoles` action/oracle metadata. The importer validates the entire configured symbol registry before walking entry bodies: each mapping must resolve to a declaration (not a defined LLVM function) with a non-variadic scalar-integer ABI, exact action arity, an `i1` guard, and a result-typed fallback. Multi-result/pointer ABI external lowering and text syntax remain open. |
| `MPC-EXT-OPT-01` | Generic action/oracle/storage dependency graph and deterministic boundary batching | Independent planner oracle plus action-order, oracle-CSE, and storage co-batching corpus | **PARTIAL:** `volar_vc::external_boundary` has stable request identity, explicit action ordinal, dependency edges, pure-oracle equivalence aliases, demand dropping, deterministic batching, and no default action/storage edge. `plan_boolar_external_boundaries` extracts action/oracle/storage requests, exact `IRVarId` projections, value dependencies, and source storage order from real fused Boolar, requiring explicit policy registries. It now CSEs only explicitly fingerprinted assigned-oracle calls with the same declaration identity/profile, output geometry, and exact `IRVarId` argument sequence; the zero legacy fingerprint disables cross-occurrence CSE. It is not yet wired to the storage planner or runtime segmentation. |
| `MPC-EXT-SCHED-01` | Action executor policy reaches `GateSchedule::ActionSpec` | Schedule compilation fixture and fail-closed unsupported-policy test | **PARTIAL:** explicit policy passes through `compile_schedule_with_action_policies`; default wrapper is deliberately marked legacy evaluator compatibility. `ActionSpec` now carries stable source request ID and action ordinal, and strict code validates a canonical action manifest. Real declaration registry lookup, oracle request table, and segmented lowering remain open. |
| `MPC-EXT-STRICT-01` | Evaluator-executor strict action batch/reinsertion | Cross-process semantic and malformed-frame corpus; session/circuit/request binding | **PARTIAL:** existing strict actions are explicitly restricted to evaluator + `BothRoles`; unsupported executor/reveal policy fails closed. `StrictGateCursor` pauses at `NeedsExternalBoundary` and resumes only after the compatibility action adapter reinserts the whole call result. `ExternalBatchFrame` provides bounded, exact-length manifest/reveal/clear-input/result/reinsertion envelopes; peer manifests must equal locally derived state and result width is checked before reinsertion. `ExternalBatchTranscript` validates `Reveal → ClearInputs → Result → Reinserted`. `EvaluatorBatchExecutor` executes the explicit legacy profile against request-ID-bound host registrations, uses fallback without host invocation for guard false, and requires reinsertion acknowledgement. Strict label transport exposes garbler exact-match reveal decoding, request-base-derived OT result pairs, and cursor reveal/reinsertion helpers; existing strict-actions TCP runs through them. A loopback integration test now drives `Reveal → exact-match decode → ClearInputs → evaluator host → Result → OT label reinsertion → Reinserted` through the batch executor and strict helpers. The strict TCP action runners now exchange the actual versioned `ExternalBatchFrame` manifest/reveal/clear-input/result/reinserted sequence before/alongside the strict table stream, with result OT reinsertion before cursor resume. No replay-resistance claim exists. |
| `MPC-EXT-STRICT-02` | Garbler-executor strict action batch/reinsertion | Independent disclosure/transcript and OT-direction tests | **PARTIAL:** `GarblerBatchExecutor` executes explicit Garbler + `BothRoles` request-bound actions with guarded fallback/width enforcement. `run_garbler_strict_actions_garbler_host` wires the actual strict TCP batch manifest/reveal/clear-input/result/reinserted flow, with the garbler host producing result bits and the existing request-base-derived OT direction delivering evaluator labels. A cross-process TCP fixture covers both executor roles. `ExecutorOnly`, mixed-executor batches, and independent malicious/disclosure review remain open. |
| `MPC-EXT-ORACLE-01` | Assigned and replicated pure oracle execution | Deterministic oracle adapter test; reviewed replication consistency protocol | **PARTIAL:** pure oracle CSE/demand/batching model supports only `Assigned`; `Replicated` is rejected pending a consistency protocol. `volar_vc::external_executor` runs assigned evaluator + `BothRoles` pure-oracle callbacks in planned boundary order with exact width checks, but it is a clear-value seam awaiting strict label reveal/reinsertion binding. |
| `MPC-EXT-CHAIN-01` | Strict-chain external boundary phase and held result material | Boundary/retry/epoch fixture plus storage co-batching test | **PARTIAL:** `ChainBoundaryScript` and `ChainBoundaryPhase` provide one boundary admission point with distinct storage and external sections. Current drivers reject a nonempty external section *before* they issue durable storage, preserving retry/epoch safety; storage-only scripts remain supported. `reserve_external_held_results` reserves contiguous opaque `HeldSlots` ranges for validated result geometry. `volar_vc::external_executor` merges planned storage/action/oracle callbacks in canonical batch order, but chain drivers do not yet invoke it or write reinserted material. |
| `MPC-EXT-VC-01` | VC external registry and WAT/LLVM imported-guest policy validation | End-to-end guest fixtures and `VcOutcome::Abort` mapping | **PARTIAL:** `VcExternalRegistry` validates unique public declaration names, non-sentinel declaration fingerprints, and output geometry, then supplies explicit policies/geometries to fused-Boolar boundary planning. The legacy zero fingerprint sentinel is accepted for distinct compatibility declarations until frontend migration supplies real fingerprints. `VcEmbedder::{compile,invoke}_with_external_registry` maps invalid/missing/unsupported external planning or any nonempty batch plan to `VcOutcome::Abort(UnsupportedExternalPolicy)`, never legacy evaluator execution. A real LLVM fixture and a real WAT-import fixture both carry explicit declarations through VAFFLE/Volar IR/Boolar into that registry's boundary plan; neither route executes a host callback. It still has no executable batch adapter. |
| `MPC-EXT-WEAVER-01` | Generated-weaver/backend policy support or explicit rejection | Generated compile/run corpus for every supported backend | **PARTIAL:** generated garble/MPC/net weavers still reject or skip external Boolar statements rather than silently selecting an executor. Full common boundary-plan consumption and generated compile/run evidence remain open. |

## Current compatibility posture

The executable strict batch paths are explicit **evaluator-hosted** and
**garbler-hosted**, each with `BothRoles` disclosure only. Legacy evaluator
compatibility remains represented by `ActionExecutionPolicy::legacy_evaluator()`
rather than an implicit metadata default. Neither path makes an
executor-private disclosure claim; `ExecutorOnly` remains rejected.

`compile_schedule(...)` retains a compatibility fallback for legacy callers;
new scheduling code must use `compile_schedule_with_action_policies(...)`.
`plan_boolar_external_boundaries(...)` is stricter: it requires both explicit
policy registries and oracle declaration output geometry, so it cannot invent
a policy or infer a direct-`OracleBit` result width from use sites. The
schedule fallback must be removed only after all action-producing frontends
carry a validated declaration policy and `MPC-EXT-SCHED-01` is complete.

## Completion rule

A surface may advertise an external action/oracle mode only when its row is
marked PASS with an independently runnable fixture. A planner-only record,
compile-only generated code, or evaluator-only host path does not authorize a
garbler executor, executor-only reveal, replicated oracle, or production
transport claim.
