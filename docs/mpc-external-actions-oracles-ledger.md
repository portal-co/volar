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
| `MPC-EXT-DECL-01` | `ExternalExecutor`, `ExternalRevealPolicy`, action/oracle fingerprints carried through schema-generated declarations and lowering | IR/VAFFLE/text/serialized round trips; explicit policy test at every frontend and declaration merger | **PARTIAL:** `volar-ir` schema records explicit policy/fingerprint and legacy evaluator policy constructors. Existing declaration construction, LLVM import, VAFFLE lowering, substitution, fuzz generation, and text parser paths attach the explicit compatibility policy. Text syntax and real frontend policy configuration remain open. |
| `MPC-EXT-OPT-01` | Generic action/oracle/storage dependency graph and deterministic boundary batching | Independent planner oracle plus action-order, oracle-CSE, and storage co-batching corpus | **PARTIAL:** `volar_vc::external_boundary` has stable request identity, explicit action ordinal, dependency edges, pure-oracle equivalence aliases, demand dropping, deterministic batching, and no default action/storage edge. `plan_boolar_external_boundaries` now extracts action/oracle/storage requests, exact `IRVarId` projections, value dependencies, and source storage order from real fused Boolar, requiring explicit policy registries. It deliberately disables cross-occurrence Oracle CSE until a declaration/profile-aware canonical argument key is supplied, and is not yet wired to the storage planner or runtime segmentation. |
| `MPC-EXT-SCHED-01` | Action executor policy reaches `GateSchedule::ActionSpec` | Schedule compilation fixture and fail-closed unsupported-policy test | **PARTIAL:** explicit policy passes through `compile_schedule_with_action_policies`; default wrapper is deliberately marked legacy evaluator compatibility. `ActionSpec` now carries stable source request ID and action ordinal, and strict code validates a canonical action manifest. Real declaration registry lookup, oracle request table, and segmented lowering remain open. |
| `MPC-EXT-STRICT-01` | Evaluator-executor strict action batch/reinsertion | Cross-process semantic and malformed-frame corpus; session/circuit/request binding | **PARTIAL:** existing strict actions are explicitly restricted to evaluator + `BothRoles`; unsupported executor/reveal policy fails closed. The strict actions path validates canonical request IDs/action order via `ExternalBatchManifest`; manifests can now derive a domain-separated session/circuit binding digest. Legacy per-call frames do not transmit or authenticate that binding yet, so this is not replay resistance or a batch/reinsertion protocol. |
| `MPC-EXT-STRICT-02` | Garbler-executor strict action batch/reinsertion | Independent disclosure/transcript and OT-direction tests | TODO |
| `MPC-EXT-ORACLE-01` | Assigned and replicated pure oracle execution | Deterministic oracle adapter test; reviewed replication consistency protocol | **PARTIAL:** pure oracle CSE/demand/batching model supports only `Assigned`; `Replicated` is rejected by the generic planner pending a consistency protocol. No execution adapter exists. |
| `MPC-EXT-CHAIN-01` | Strict-chain external boundary phase and held result material | Boundary/retry/epoch fixture plus storage co-batching test | TODO |
| `MPC-EXT-VC-01` | VC external registry and WAT/LLVM imported-guest policy validation | End-to-end guest fixtures and `VcOutcome::Abort` mapping | TODO |
| `MPC-EXT-WEAVER-01` | Generated-weaver/backend policy support or explicit rejection | Generated compile/run corpus for every supported backend | TODO |

## Current compatibility posture

The only currently executable action path remains the existing strict
**evaluator-hosted, revealed-to-both-roles** protocol. It is represented by an
explicit `ActionExecutionPolicy::legacy_evaluator()` value rather than an
implicit default in action metadata. This preserves current behavior while
making any claim of executor-private disclosure impossible.

`compile_schedule(...)` retains a compatibility fallback for legacy callers;
new scheduling code must use `compile_schedule_with_action_policies(...)`. The
fallback must be removed only after all action-producing frontends carry a
validated declaration policy and `MPC-EXT-SCHED-01` is complete.

## Completion rule

A surface may advertise an external action/oracle mode only when its row is
marked PASS with an independently runnable fixture. A planner-only record,
compile-only generated code, or evaluator-only host path does not authorize a
garbler executor, executor-only reveal, replicated oracle, or production
transport claim.
