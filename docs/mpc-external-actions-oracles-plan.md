# Plan: deferred external actions and pure oracles across MPC surfaces

**Status:** proposed design plan. **Pinnedness:** unpinned. **Stability:** very
unstable. This plan changes scheduling/protocol seams only; it makes no
malicious-security, confidentiality, replay-resistance, transport, or concrete
host-implementation claim until the listed tests and reviews exist.

## Purpose

Support declared external primitives across every MPC execution surface with a
single semantics-preserving model:

- an **action** is an ordered, conditionally executed side effect, assigned to
  exactly one MPC party as its **executor**;
- the executor receives the action's revealed guard/arguments, conditionally
  calls its local host implementation, and its result bits are reinserted into
  the MPC wire domain;
- an **oracle** is pure and deterministic, so it can be memoized, reordered,
  and batched whenever its argument dependencies allow;
- action, oracle, and storage work are deferred until their outputs are
  demanded, then jointly scheduled at explicit circuit boundaries to maximize
  one-round external work without changing visible action order.

The immediate target is the existing semi-honest strict garbling / VC /
strict-chain stack. The compiler-side design must remain generic enough for
other MPC surface adapters and later provider/FHE boundaries. Legacy
`FheScheme` / TFHE is not a validation substitute.

Existing `Gate::ActionBit` and `StrictActionHost` are evaluator-hosted only and
currently perform a per-call interactive decode sequence at the gate's original
position. That is a useful prototype, but it is **not** this plan's final
interface: it exposes decoded guard/arguments to both roles in the current
round trip, has no explicit executor identity in `ActionSpec`, represents no
oracle schedule primitive, and cannot batch deferred external work.

## Ubiquitous language

These terms are normative in this document.

| Term | Meaning |
|---|---|
| **External request** | One deferred operation at a circuit boundary: storage read/write, pure oracle call, or action call. It contains public request metadata and MPC wire references, never a host plaintext value. |
| **Action** | A named conditional external operation with a potentially observable side effect. One source `ActionCall` is one logical action invocation, independent of how many output bits project from it. |
| **Oracle** | A named deterministic, side-effect-free external operation. Equal declarations and equal logical argument values must give equal outputs. |
| **Executor** | The one party assigned to run an action or local oracle adapter: `Garbler` or `Evaluator`. An executor is a public declaration/property, never selected from a secret guard. |
| **Reveal-to-executor** | The protocol operation that makes an action/oracle's guard and arguments available in clear to its assigned executor at an external boundary. It is an explicit leakage authorization, not ordinary wire evaluation. |
| **Reinsertion** | Converting host result bits back into MPC wires/labels with bases agreed by both parties. It does not mean the executor simply returns raw bits to the other party. |
| **Demand** | The first dataflow point at which an external result must exist: a non-deferrable computation, a required output, an action dependency, or a boundary flush. |
| **Boundary batch** | The maximal set of currently ready external requests executed at one explicit protocol boundary, respecting all dependency and ordering rules. |
| **Action chain** | The total source/declared order of actions that may have interfering side effects. A scheduler never changes this order. |
| **Storage chain** | The existing storage/ORAM ordering relation. This plan assumes actions do not interfere with storage, per the stated MPC model. |
| **Oracle equivalence key** | Canonical `(declaration fingerprint, argument wire identity/value representation, profile/version)` used only for pure oracle memoization. It never uses source spelling as a key. |

`IRVarId` is the canonical identity at Boolar/IR scheduling seams. At a SWC
origin seam use hygienic `Ident`/`Id`, not a flattened string. A name is valid
for an action/oracle declaration lookup, but not for tracking the identity of a
source value or its availability.

## Semantics and non-negotiable invariants

### Actions

An action retains the existing conditional semantics:

```text
result = if guard { executor_host.action(name, args) } else { fallbacks }
```

- `guard = 0`: the executor does **not** invoke the host action; all result
  bits equal the declared fallback bits; no action side effect occurs.
- `guard = 1`: exactly one executor invokes the host action exactly once;
  result width must exactly match the declaration; results are reinserted as
  MPC wires.
- all `ActionBit` / `ActionOutput` projections observe one atomic invocation;
  they cannot independently rerun the action.
- an action may be deferred until a demand, but it retains source action-chain
  order. If action `B` appears after action `A`, `B` cannot execute before `A`
  even when `B`'s value is demanded earlier.
- actions have no ordering edge with storage in the stated model. Thus a ready
  action may share a boundary batch with ready storage requests, but it may not
  leapfrog another action-chain predecessor.
- no optimizer may CSE, duplicate, eliminate a reachable action, speculate an
  action before its guard is available, or move it across an action-chain edge.
  It may delete an action only after proving its guard is constant false and
  that the source semantics consequently has no invocation.

### Oracles

- an oracle has no side effect and must be deterministic under its declaration
  fingerprint; it can run on either assigned executor or on both roles only
  when the chosen adapter makes that safe and produces the same result;
- the optimizer may CSE/memoize equivalent oracle calls, schedule a ready
  oracle earlier or later, and batch it with any other ready external request;
- an oracle cannot observe or mutate storage or action state. A declaration
  requiring either is an action, not an oracle;
- an oracle result is still not available until its argument wires are
  available and its selected boundary batch has completed.

### Reveal and reinsertion

The executor assignment is a public capability decision. It must be present in
the action/oracle declaration or the execution-plan policy before lowering;
there is no implicit "evaluator host" default in the final generic model.

The first implementation may use a conservative **revealed-to-both** transport
mode to preserve the current strict-actions behavior, but it must label that
mode explicitly and not call it executor-private. The target protocol mode is
**revealed-to-executor**: only the chosen executor obtains clear guard/argument
bits; the non-executor keeps wire labels/bases. Both parties then participate in
result reinsertion so downstream computation gets valid secret wire
representations. A selected executor must therefore be authorized to learn
that action's guard/arguments and results as required by its host contract.

No raw host result is silently treated as a garbled label. Reinsertion has a
fixed transcript shape, binds action/oracle invocation identity and output bit
index, verifies result width, and creates/writes the agreed result labels/bases
before dependent circuit work resumes.

## Scope: all MPC surfaces

The design must land through adapters, not separate ad hoc action systems.

1. **Volar IR / Boolar / schedule compiler**: declaration metadata, call and
   projection identities, dependency extraction, boundary plan generation, and
   lowering to a schedule that refers to reinsertion wires rather than an
   immediate evaluator-only `Gate::ActionBit` behavior.
2. **Strict garbling**: both garbler- and evaluator-executed action adapters;
   cursor pause/resume; table streaming; label-safe reinsertion; the existing
   strict-actions TCP path.
3. **Strict chain**: `ChainParty` boundary phases and `HeldSlots` must route
   external result material between rounds without decoding held data. Storage
   prefetch and external batches share a round boundary but retain independent
   ordering relations.
4. **VC embedder and circuit schedules**: `compile_schedule_optimized`,
   `VcEmbedder`, WAT/LLVM guest paths, action host registration, input/output
   partitioning, abort mapping, and public trace auditing.
5. **GRAM / ORAM adapters**: existing ORAM actions migrate to explicit executor
   declarations and boundary batches. Storage semantics remain in the storage
   chain; no action optimizer may rewrite a secret storage access into a host
   lookup.
6. **Generated weavers and other MPC backends**: garble, VOLE/ZK, net, and
   any backend that currently rejects or hand-rolls `Oracle*`/`Action*` gets a
   common external-plan adapter. A backend may advertise unsupported executor
   modes, but must fail closed rather than silently choose evaluator execution.
7. **Circuit-provider and FHE transition plumbing**: action/oracle requests
   become explicit demands around a provider/GC boundary; neither a provider
   module nor a no-std WASM artifact may retain an untracked action result in
   globals or storage across an invocation.

## Generic external-boundary optimizer

Implement a provider-neutral compiler module, provisionally named
`external_boundary_plan`, over a fused Boolar/IR region *before* schedule
execution. Its output is an immutable `ExternalBoundaryPlan`; it does not run
a host function, transport labels, decrypt storage, or pick an FHE scheme.

### Input facts

The planner consumes:

- validated action and oracle declarations, including executor assignment,
  declared argument/output geometry, profile/version/fingerprint, and leakage
  policy;
- exact call identities and projections (`ActionCall`/`ActionBit`,
  `OracleCall`/`OracleBit`, including legacy projection forms only through a
  compatibility adapter);
- statement data dependencies keyed by `IRVarId` / Boolar wire identity;
- storage-chain operations from existing `PreFheStoragePlan`, public storage
  planning, and any scheduled GRAM/ORAM interface;
- circuit boundary candidates (entry, material prefetch boundary, before a
  consumer that cannot continue without a result, output/reveal, loop boundary,
  explicit host barrier);
- source action order and declaration-level interference metadata; and
- public resource caps for a batch (maximum request count/bit count/frame
  bytes), which split a ready set but never alter dataflow semantics.

It returns:

```text
ExternalBoundaryPlan {
  batches: [ExternalBatch],
  result_bindings: exact call/projection -> reinserted wire material,
  action_chain: ordered ActionRequestId list,
  oracle_equivalence: canonical oracle request -> chosen representative,
  storage_script: existing storage requests retained in their own chain,
  audit: public execution/leakage/dependency manifest
}
```

The plan uses separate public identities for `ActionRequestId`,
`OracleRequestId`, and `StorageRequestId`; no identity is derived from an
extern name alone. A source action occurrence / `ActionCall` SSA identity is
stable even if declarations share a name.

### Dependency graph

Build a directed graph with these edge types:

| Edge | Rule |
|---|---|
| Value edge | Producer result must be reinserted before a consumer obtains that value. |
| Action-chain edge | Every action has an edge from the previous potentially executable action to the next one in source order. This preserves side effects even when their data values are independent. |
| Guard/argument edge | An action/oracle cannot reveal/run until guard and all argument wires are available. Fallback values are also required when action semantics need them. |
| Projection edge | A result projection depends on its one call request; multiple projections share one result binding. |
| Storage edge | Existing storage operation order is retained as produced by the storage planner / ORAM driver. |
| Boundary edge | A request is assigned to a boundary only after all predecessors are available and before its first non-deferrable consumer. |
| Explicit host barrier | A declared action/oracle/guest host barrier prevents movement across it. |

**No action–storage edge is added by default.** This encodes the given model:
action calls cannot interfere with storage. It is still possible for an action
to depend on a storage *value*, or vice versa through dataflow; those are value
edges and must be respected. A provider-specific declaration may later opt into
an action/storage interference edge, but absent explicit evidence the generic
optimizer must not invent one.

**No oracle–storage/action ordering edge is added by default.** A pure oracle
can be reordered around either once its arguments are available. If it consumes
an action/storage result, the ordinary value edge determines readiness.

### Demand discovery and deferral

Use a backwards liveness/demand walk from:

- required circuit outputs/reveals;
- a non-external Boolean gate that consumes a call projection;
- a downstream action/oracle argument/guard/fallback that consumes a result;
- storage planning input needing the value;
- an explicit boundary or loop transition; and
- a side-effect action itself once its action-chain predecessor and inputs are
  ready.

An external call is **not** scheduled just because it appears lexically in a
fused circuit. It remains pending until either a demand reaches one of its
projections or its action-chain position becomes the necessary next effect.

For actions, this distinction is important:

- a guard known constant false lets constant folding erase the action and bind
  projections to fallbacks before the external planner sees it;
- an action with a nonconstant / true guard is live as a side effect even if no
  projection is used, and must execute in action-chain order before the next
  action/barrier/output completion;
- a projection-only dead action can be DCE'd only when that side effect cannot
  occur, not merely because outputs are dead.

For oracles, an unused pure call may be DCE'd; otherwise the optimizer chooses
its latest useful or earliest batchable point subject to the resource policy.

### Batch construction

At each boundary, select the maximal deterministic ready set:

1. include every ready storage request allowed by the existing storage chain;
2. include the next contiguous ready prefix of the action chain, stopping at an
   unavailable predecessor/input/guard or batch resource cap;
3. include every ready oracle whose dependencies are met and whose canonical
   representative has not already been scheduled;
4. close the batch under value dependencies needed for request preparation;
5. sort each category by stable public request identity and emit a canonical
   category order:

```text
storage prefetch/read/write preparation
→ action requests in action-chain order
→ pure oracle requests in canonical equivalence-key order
→ result reinsertion in request/output-bit order
```

The category order is a transport/audit convention only. Storage and actions
may coexist in one boundary because the model declares no interference, but the
existing storage driver remains responsible for its own transaction ordering.
If a particular transport must run storage first or action first, it exposes a
capability and the plan's canonical order is adapted without changing graph
edges or action order.

Batching cannot delay a request past its first required consumer. If an action
is the next chain effect but an earlier non-external consumer does not depend on
it, the optimizer may place it at a later boundary only if no subsequent action,
output completion, explicit barrier, or host-visible ordering point is crossed.

### Oracle CSE and memoization

The canonical oracle equivalence key contains:

```text
OracleDeclFingerprint
+ canonical argument wire identities after alias/fold rewriting
+ declaration/profile version
+ declared output geometry
+ any explicit public domain-separation inputs
```

It excludes source location/name-only aliases and excludes secret logical
values that are not represented by the same exact wire material. Equal keys
share one request and one reinsertion result. Different executor policy,
profile, epoch, output geometry, or domain separation forbids sharing.

Oracle CSE occurs before batch sizing. If the representative is dead after
whole-circuit fold/CSE/DCE, remove it and all request material. An action never
participates in this equivalence relation.

### Optimization pipeline placement

The compiler pipeline becomes:

```text
source / imported IR
→ normal IR simplification and action-safe DCE
→ control lowering / movfuscation as applicable
→ fused Boolar circuit
→ Boolean fold → action-safe CSE → action-safe DCE
→ external-boundary demand planning
→ materialize boundary split requests/result placeholders
→ fold → CSE → DCE again
→ lower each pure circuit segment to GateSchedule
→ execute boundary batches between segments
```

The standard Boolar CSE/DCE currently treats external statements specially;
this work must audit that policy. In particular it must never make a reachable
`ActionCall` disappear, merge two actions, or rewrite call/projection identity.
A dedicated `external_boundary_plan` owns oracle CSE so generic Boolean CSE
cannot accidentally make an unsafe name-based decision.

The second optimize pass is required: batching and result placeholder
substitution can expose constant fallbacks, duplicate oracle result use, and
dead cache/provider work. Recompute demand metadata from the surviving circuit
rather than allocating requests, labels, held slots, or randomness from a
pre-optimization trace.

### Loops and provider boundaries

For a statically finite unrolled loop, the planner sees each action occurrence
as distinct and preserves source action-chain order across iterations. Oracles
may share across iterations only when their full equivalence key matches. For a
symbolic/movfuscated loop, the plan must either use an explicit bounded
per-iteration external boundary protocol with stable occurrence identity, or
reject the external primitive; it must not silently execute an action at every
flattened path.

Circuit-provider and FHE cache invocations use the same rule: a discarded cache
cannot carry an action/oracle result to the next invocation except through an
explicit reinserted wire or explicitly versioned/exported storage representation.

## External protocol and executor adapters

### Declaration and policy changes

Extend the current external declaration surface with public execution policy.
`ActionDecl` and `OracleDecl` currently carry only `name`, parameter types, and
result types. Introduce a versioned policy object rather than inferring a host
from a name suffix:

```rust
pub enum ExternalExecutor {
    Garbler,
    Evaluator,
}

pub enum OracleExecution {
    /// One designated role evaluates and reinserts results.
    Assigned(ExternalExecutor),
    /// Both roles evaluate from an allowed clear/shared input representation;
    /// mismatch handling is profile-owned and required before admission.
    Replicated,
}

pub struct ActionExecutionPolicy {
    pub executor: ExternalExecutor,
    pub reveal: RevealPolicy,
    pub declaration_fingerprint: [u8; 32],
}

pub struct OracleExecutionPolicy {
    pub execution: OracleExecution,
    pub reveal: RevealPolicy,
    pub declaration_fingerprint: [u8; 32],
}
```

`RevealPolicy` must make the authorized disclosure explicit. Initial strict-GC
support should accept only a reviewed conservative `BothRoles` policy or a new
executor-only protocol mode with proof/test coverage. It must reject a
policy-free declaration; compatibility adapters may translate existing
strict-actions declarations into an explicitly marked `Evaluator + BothRoles`
policy while migration is in progress.

The action policy is stored with the declaration and carried through IR,
VAFFLE, Boolar lowering, schedule construction, code generation, and session
manifest binding. It is never selected based on a secret wire. `ActionSpec`
and the schedule's future `OracleSpec` carry a declaration index/fingerprint and
executor, not merely a string host name.

The policy has to distinguish an **executor** from an **input owner**:
execution by the evaluator does not imply evaluator ownership of source wire
inputs. Inputs are MPC wires and are revealed only under the call's explicit
reveal policy at the boundary.

### Boundary-batch transport protocol

Replace the current one-`ActionBit`-at-a-time `ActionArgs` /
`ActionArgsClear` walk with a batch protocol. Retain adapters for old frames
only while a versioned session manifest selects the legacy path.

A future `ExternalBatchManifest` binds:

- session/circuit digest and protocol version;
- boundary ordinal;
- all public request IDs, kinds, declaration fingerprints, executors, and
  output widths;
- action-chain predecessor identity for every action;
- request/output reinsertion order;
- exact reveal policy and input/output label geometry;
- storage operation script digest, where storage shares the boundary; and
- provider/base/cache epoch identities if the batch surrounds a provider
  transition.

Proposed phases for each batch:

```text
1. Both roles derive and validate the same manifest.
2. Pause pure gate cursor(s) at the boundary; no dependent segment begins.
3. Run existing storage phase/pre-fetch transaction(s) in storage-chain order.
4. For every ready action/oracle request, reveal guard/args to the assigned
   executor according to its RevealPolicy.
5. Executor conditionally runs its action host, or evaluates the pure oracle.
6. Both roles run canonical result-reinsertion subprotocols for all requests.
7. Validate width, request IDs, executor, ordering, and transcript binding.
8. Resume the next pure circuit segment with result wires installed.
```

For an action with guard false, phase 5 does not call the host; phase 6 inserts
fallback values. This is still one completed action request in the manifest,
which preserves action-chain position and auditability.

For an assigned oracle, phase 5 is unconditional once its inputs are ready.
For a replicated oracle, the adapter must require an independent equality /
commitment strategy before it is admitted; “both hosts probably returned the
same result” is not a protocol check.

### Result reinsertion modes

Provide a small adapter interface rather than putting label mechanics in the
optimizer:

```rust
trait ExternalBatchRoleAdapter {
    fn reveal_inputs(..., executor: ExternalExecutor, policy: RevealPolicy)
        -> Result<ExecutorInputs, MpcError>;
    fn execute_assigned_action(..., inputs: ExecutorInputs)
        -> Result<Vec<bool>, MpcError>;
    fn execute_oracle(..., inputs: ExecutorInputs)
        -> Result<Vec<bool>, MpcError>;
    fn reinsert_results(..., result_bits: &[bool])
        -> Result<ReinsertedWires, MpcError>;
}
```

The role adapter, not a guest host, is responsible for guard/fallback handling,
width validation, label/base derivation, OT if required, and session frame
validation. `ExternalBatchRoleAdapter` has at least two concrete strict-GC
implementations—garbler executor and evaluator executor—and test doubles only
for compiler planning tests.

The current strict action protocol can inform a conservative implementation:
labels are transferred to the role able to decode under its private bases,
logical bits are delivered to the assigned host, and each result bit is offered
against a deterministic invocation-result base. But final executor-private
reveal requires the transcript direction and result OT direction to be defined
for **both** roles. Do not copy evaluator-only logic and merely rename it.

### Strict garbling and cursor changes

Refactor `StrictGateCursor` from immediate `action_bit()` interaction to a
segmented evaluator:

- cursor reports `NeedsExternalBoundary { ready_requests, next_gate }` rather
  than transmitting as soon as it sees an `ActionBit`;
- repeated result-bit gates reference the one already reinserted request
  result; they never trigger a second host execution;
- a garbler-side cursor/adapter exists for garbler-executed actions, rather
  than assuming the evaluator can always run the host;
- `run_*_strict_actions` becomes a compatibility wrapper over the generic
  batch executor, preserving current evaluator-hosted behavior under an
  explicit legacy policy;
- table-streaming state, OT state, and label bases remain valid across a
  boundary pause/resume; malformed/missing/out-of-order frames abort.

`GateSchedule` gains an external request table for actions and oracles. Storage
is retained in its own existing table and is referenced by the unified
boundary plan. A bare `Gate::ActionBit` is transitional; the final schedule
representation references result-wire material allocated by a request table.

### Strict chain, held material, and storage

Add a generic `ChainExternalPhase` sibling to `ChainStoragePhase`, or replace
both with one `ChainBoundaryPhase` whose script has distinct storage and
external sections. The script runs only before/after ordinary strict rounds.
It must:

- accept public batch manifests and public executor assignment;
- reuse `HeldSlots` for opaque reinserted result material where a later round
  needs it, without decoding the material;
- compose with `StorageOperation::Prefetch` but never recursively open storage
  in an ordinary gate evaluation;
- preserve the storage chain exactly and preserve action-chain ordering;
- map failure to the existing abort semantics, without advancing a storage or
  action epoch after a failed transaction.

An action result may be held, revealed, passed to a provider conversion, or fed
directly to the next circuit segment only through an explicit chain disposition.
It may not be cached in an executor process outside the registry.

### VC, WAT, and LLVM imported guests

Add one public VC external registry which maps declaration fingerprint to:

- kind (`Action` / `Oracle`);
- executor and reveal policy;
- local garbler/evaluator host capability;
- input/output bit geometry;
- optional oracle equivalence/profile data;
- action host implementation or oracle implementation; and
- allowed output disposition.

`VcEmbedder` validates the registry against every guest declaration before
invocation. A missing declaration, executor capability, width mismatch, wrong
request ID, malformed frame, host error, or unsupported policy maps to
`VcOutcome::Abort`, not to a fallback evaluator-host execution. WAT and LLVM
imports use the same registry after frontend declaration lowering; direct
extern paths cannot bypass it.

TLS/ORAM action adapters migrate by declaring their executor explicitly. The
current socket/ORAM host is initially an evaluator executor only under a
conservative reveal policy. A garbler-hosted network action is not inferred
from a `SocketHost` implementation; it requires a separate capability and
transport test.

### Generated weavers and other backends

`volar-weaver` needs a target-neutral external request emission layer. Garble,
FHE/provider, VOLE/ZK, net, and no-op weavers consume the same declaration and
boundary plan but implement only their supported modes. Required behavior:

- emit declaration stubs/types from the common registry;
- preserve call/projection occurrence identity through generated IR;
- surface an explicit `UnsupportedExternalExecutor` / `UnsupportedRevealPolicy`
  diagnostic where a backend lacks a mode;
- never lower an action to a plain pure function merely to compile;
- use oracle equivalence only after profile/fingerprint validation;
- retain existing action publicness/output-mode rules as an additional result
  disposition policy, not as a substitute for executor assignment.

## Delivery slices and commit boundaries

Each slice is independently reviewable, has tests in the same commit, and
fails closed while later protocol work is absent.

### A. Canonical model and declarations

1. Add `ExternalExecutor`, `RevealPolicy`, action/oracle execution policy, and
   declaration fingerprint/version fields to the shared IR declaration types.
2. Thread them through IR/VAFFLE serialization/text format, frontend import
   configuration, parser/macro registrations, and all declaration remappers.
3. Provide an explicit compatibility policy for existing evaluator-hosted
   actions. Reject missing policy in new declarations.
4. Document/validate executor availability per backend; add a test that same
   action name with different fingerprints/policy does not coalesce.

### B. Generic boundary graph and optimizer

1. Add request IDs, dependency graph construction, liveness/demand discovery,
   action-chain edges, storage-chain input adapter, and oracle equivalence key.
2. Implement deterministic ready-batch partitioning with resource caps and a
   public audit manifest.
3. Add action-safe DCE integration and oracle-only CSE/memoization. Ensure
   general Boolean CSE cannot merge action calls.
4. Add a pure planning interpreter/test model; it does not execute hosts or
   handle labels.

### C. Schedule/segment lowering

1. Extend `GateSchedule` with versioned external request/result tables,
   declaration fingerprint, executor policy, result geometry, and request
   occurrence IDs.
2. Teach `compile_schedule_optimized` and the lowerers to split pure segments
   at boundary plan points, retain source-to-result bindings, and reject
   unsupported legacy/partial forms.
3. Update strict cursor APIs to pause at batches rather than invoke a host at a
   `Gate::ActionBit` immediately.
4. Preserve a legacy schedule adapter only for a manifest that explicitly opts
   into current evaluator-hosted conservative behavior.

### D. Strict-GC batch protocol

1. Add versioned `SessionFrame` variants for a manifest, reveal material,
   executor result, and reinsertion acknowledgment. Bind them to session and
   circuit digests.
2. Implement evaluator-executor action batch adapter, preserving old
   `StrictActionHost` behavior through a compatibility adapter.
3. Implement garbler-executor action batch adapter; do not mark it supported
   until the reveal/reinsertion direction and OT behavior are independently
   tested.
4. Add assigned/replicated oracle adapters; only enable replicated mode after
   its equality/consistency protocol is reviewed.
5. Make malformed ordering/width/identity/frame/host failure abort before
   dependent pure gate segments resume.

### E. Chain/VC/ORAM integration

1. Add boundary phase support to strict chain and held result range allocation.
2. Integrate `PreFheStoragePlan` / `StorageOperation::Prefetch` as a separate
   storage script inside a unified boundary manifest; preserve no default
   action-storage interference edge.
3. Add VC external registry and WAT/LLVM imported-guest validation.
4. Migrate existing ORAM/TLS actions to explicit evaluator executor policy,
   then add a separate garbler-host fixture rather than assuming symmetry.
5. Add provider/FHE-boundary adapter so external result/cache state cannot
   survive a module invocation without explicit registry material.

### F. Generated/weaver coverage and removal of compatibility path

1. Update garble, VOLE/ZK, FHE/provider, net, and no-op weavers to consume
   common declaration/request metadata or fail with explicit diagnostics.
2. Add compile-generated-Rust and generated-IR tests for action and oracle
   declarations under supported policies.
3. Remove evaluator-only implicit execution from the default schedule path only
   after all supported surface adapters choose an explicit policy.
4. Keep a migration ledger mapping all existing action/oracle call sites to
   their declaration fingerprint, executor, leakage policy, and test.

## Required test matrix

### Generic optimizer tests

- two independent ready pure oracles batch together and CSE when their full
  equivalence keys match;
- same oracle name but different profile/fingerprint/domain separator does not
  CSE;
- oracle dependent on an action result waits for that action's reinsertion;
- oracle independent of a pending action can share a boundary without changing
  action-chain order;
- actions `A`, `B`, `C` retain `A → B → C` execution even if only `C` output
  is demanded first;
- action with constant-false guard is removed and projections become fallback
  values; nonconstant action with unused outputs still executes when its chain
  position is reached;
- independent storage and action requests batch; a data-dependent storage
  value creates the needed value edge; existing storage order remains intact;
- action-safe DCE/CSE never changes call occurrence identities;
- exact `IRVarId` identity prevents same-name shadow bindings from merging;
- recomputing after fold/CSE/DCE removes dead request/slot/randomness demand.

### Strict protocol tests

- evaluator executor and garbler executor each run a fixed-width action with
  guard true/false, multiple results, bad width, host failure, and malformed
  request identity;
- executor-only reveal (when enabled) proves the non-executor receives no
  clear input/result bits through the test adapter transcript; conservative
  both-role compatibility mode is labeled and tested separately;
- result reinsertion feeds a later AND/XOR segment correctly without exposing
  the free-XOR delta;
- batched actions perform one manifest validation and canonical frame sequence,
  not one ambiguous independent protocol per `ActionBit`;
- action ordering survives batched independent oracle/storage work;
- replayed batch, wrong boundary ordinal, wrong executor, swapped output bit,
  stale session/circuit digest, malformed label, and missing acknowledgment
  abort;
- interrupted boundary does not advance action completion or storage epoch and
  cannot resume into a dependent segment.

### Surface integration tests

- `compile_schedule_optimized` rejects a policy-free external declaration and
  emits stable request identities for valid declarations;
- strict-chain held action/oracle result is available to a later round but not
  decoded by either role;
- VC WAT and LLVM import both resolve registry declarations, execute assigned
  action hosts, and map failures to `VcOutcome::Abort`;
- TLS/ORAM existing evaluator-hosted action behavior remains equivalent under
  its explicit compatibility policy;
- a provider circuit/action boundary resets provider globals/cache while an
  explicitly reinserted result remains available;
- generated weaver snapshots and compile tests cover each supported backend.

## Security, admission, and operational constraints

- Executor assignment and reveal policy are part of the public session/circuit
  manifest. Changing either creates a different protocol instance.
- Host actions must be registered locally only after the declaration
  fingerprint, executor role, and result geometry match. A string name match
  alone is insufficient.
- An action host failure is an abort, not a fallback. Fallback applies only to
  a valid guard-false logical action.
- Oracle purity is an admission claim: a host implementation that reads time,
  randomness, network state, storage state, or mutable global state is an
  action until separately proven otherwise.
- Actions may be reordered only if a future declaration explicitly supplies a
  reviewed commutativity/independence proof. This plan initially assumes all
  actions form one total action chain.
- No generic action may initiate a hidden storage transaction. If an existing
  ORAM adapter has internal storage effects, its declaration/action chain and
  storage transaction behavior must be modeled explicitly before batching.
- External result material crossing provider/GC/cache boundaries is bound to
  session, circuit, invocation/cache epoch, request ID, and output index.
- Resource caps split batches deterministically; exceeding a cap must produce
  additional boundaries or a compilation diagnostic, never drop/reorder a
  request.

## Plan records and follow-up ledger

Create a dedicated external-MPC ledger after this plan is accepted, rather than
adding unrelated action protocol rows to the FHE provider ledger. Proposed IDs:

| ID | Seam | Completion evidence |
|---|---|---|
| `MPC-EXT-DECL-01` | Executor/reveal/declaration-fingerprint metadata through every IR surface | serialization/text/frontend round trips and rejection tests |
| `MPC-EXT-OPT-01` | Generic action/oracle/storage dependency graph and deterministic boundary batching | pure planning oracle plus action-order/Oracle-CSE corpus |
| `MPC-EXT-STRICT-01` | Evaluator-executor strict action batch/reinsertion | cross-process adversarial-frame and semantic tests |
| `MPC-EXT-STRICT-02` | Garbler-executor strict action batch/reinsertion | independent disclosure/transcript and OT-direction tests |
| `MPC-EXT-ORACLE-01` | Assigned and replicated pure oracle execution | deterministic oracle and replication-consistency tests |
| `MPC-EXT-CHAIN-01` | Strict-chain held external result material and storage co-batching | boundary/retry/epoch fixture |
| `MPC-EXT-VC-01` | VC, WAT, LLVM import registry integration | end-to-end guest fixtures and abort mapping |
| `MPC-EXT-WEAVER-01` | Generated-weaver/backend policy coverage | generated compile/run corpus per supported backend |

## Out of scope

- selecting a malicious-secure action protocol;
- proving a host implementation's semantic correctness or purity;
- arbitrary dynamic/symbolic external call counts without a separately reviewed
  occurrence protocol;
- silent migration of existing evaluator-only actions to executor-private
  disclosure;
- treating FHE ciphertext conversion, ORAM material opening, or a provider WASM
  call as generic pure oracle evaluation;
- host-side plaintext caching of MPC wire values.

