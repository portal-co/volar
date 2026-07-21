# Instruction Groups for VAFFLE and Volar IR

**Status:** Partially implemented: shared metadata/types, host-configured WASM markers, VAFFLE/Volar-IR propagation, substitution remapping, lossy-pass barriers, Volar-IR v2 text round-tripping, `rkyv` derives, and advisory-group fuzz fixtures are in place. Dedicated group consumers and a VAFFLE text format remain pending.
**Prerequisite:** The implemented per-node metadata system described in [`metadata-container-plan.md`](metadata-container-plan.md).
**Scope:** Tier-2 compiler/IR infrastructure.
**@ai:** assisted

---

## 1. Problem

Some low-level optimisations need to act on a *region* of a program, not on a
single IR operation.  Existing external primitives model one operation:

- `OracleCall` is a pure multi-result call;
- `ActionCall` is one guarded side-effecting call; and
- `Rng` is one fresh sample.

They cannot say that a sequence of ordinary instructions belongs to one
logical unit.  In particular, they cannot safely convey either of these
intentions through the WASM → WAFFLE → VAFFLE → Volar-IR pipeline:

1. a sequence is a candidate for lane/batch/SIMD lowering; or
2. a loop has a declared bound and must be handled by a dedicated loop
   lowering rather than being flattened by generic movfuscation.

The per-node metadata refactor is now the foundation for this feature:
`Node<T, M: NodeMetadata = StandardMetadata<()>>` owns private complete
metadata, and ordinary rewrites use `Node::derived` or `Node::map_kind` to
preserve it.  Instruction groups must extend
`StandardMetadata<P>` and its focused APIs; they must not restore a public
field or introduce a parallel membership vector.

The initial source surface is a pair of WASM imports per group type:
`begin_{region}(args...)` and `end_{region}()`.  They are compiler markers,
not host calls that remain in the final program.

## 2. Goals and non-goals

### Goals

- Preserve a logical region across WAFFLE, VAFFLE, and Volar IR.
- Let `begin_*` capture typed SSA values, such as a loop bound or desired lane
  count.
- Permit arbitrarily many regions of the same type, including nested regions
  of the same type.  Each begin site receives a distinct identity.
- Support a group that spans multiple CFG blocks, including a natural loop.
- Make unknown, malformed, or unconsumed required groups fail explicitly;
  never silently erase their meaning before movfuscation.
- Keep group declarations and identities independent of oracle/action/RNG
  declarations, while following their useful pattern of module-level,
  typed declaration tables.
- Let optimisation consumers choose their own group-specific validation and
  replacement lowering.

### Non-goals for the first implementation

- Defining a universal vector ISA, a loop transformation algorithm, or a
  protocol-specific batching semantics.
- Giving group markers runtime behaviour, return values, or dynamic handles.
- Supporting crossing regions (`begin A; begin B; end A; end B`).  Groups are
  lexical/CFG-nested, not arbitrary interval annotations.
- Inferring a group from arbitrary source code.  The first version requires
  explicit marker imports and an import configuration.
- Treating `ReentryHint` as a group representation.  A reentry hint describes
  a decreasing branch measure; it does not identify a region, capture inputs,
  or distinguish multiple loops.

## 3. Semantic model

### 3.1 Declaration, static instance, and dynamic execution

A module contains declarations such as `bounded_loop` and `batch`.  A call to
`begin_bounded_loop` creates a **static group instance** with a fresh,
module-local `InstructionGroupId`.  The corresponding `end_bounded_loop`
closes that instance.

The ID identifies a source region, not a runtime activation.  If a begin/end
pair occurs in a loop or recursive function, the same static instance can be
executed repeatedly.  If two begin sites name `batch`, they still produce two
different IDs.  Thus nested or otherwise simultaneously active groups of the
same declaration are unambiguous:

```text
begin_batch(8)       // GroupId(12)
  ...
  begin_batch(4)     // GroupId(13), same declaration, distinct instance
    ...
  end_batch()
  ...
end_batch()
```

The active group context of an instruction is an ordered **group stack** of
IDs.  The stack, rather than a set, records nesting and rules out ambiguous
crossing scopes.  An instruction in the inner body above belongs to both
`[GroupId(12), GroupId(13)]`.

### 3.2 Captured inputs

`begin_*` arguments are metadata inputs to the group instance.  They must:

- match the declaration's parameter types and order;
- dominate the group entry and every member instruction that a consumer may
  use them with; and
- be treated as liveness uses even though the begin marker itself disappears
  after marker lowering.

For example, `begin_bounded_loop(limit)` can capture an `i32`/`i64` limit;
`begin_batch(lanes)` can capture a lane count or shape.  A particular group
kind defines how to interpret its inputs.  Core IR only guarantees their
identity, type, and lifetime.

### 3.3 Region boundaries and CFG validity

The begin/end calls have no runtime result and no runtime side effect after
recognition.  Their meaning is static.  The WAFFLE frontend must therefore
validate group scopes with a CFG data-flow analysis, not by relying on the
order in which blocks happen to be stored.

For every program point, the validator computes an incoming group stack:

- a begin marker pushes its newly assigned ID after the call;
- an end marker requires a non-empty stack whose top has the matching group
  declaration, then pops it after the call;
- all predecessor stacks at a join must be identical; and
- a loop back-edge must return the same stack required at the loop header.

The function entry and every return/unreachable exit must have an empty stack.
A mismatched end, different stacks at a join, a group escaping a return, or a
captured value that does not dominate the region is a lowering error.  This
allows a group to contain branches and loops while rejecting paths on which a
marker would have only dynamically conditional, ambiguous scope.

## 4. Core IR representation

Groups must be metadata on enclosed instructions, not another `Stmt` variant.
A `Stmt::BeginGroup`/`Stmt::EndGroup` design would leave every consumer to
reconstruct cross-block scope, would be fragile under block splitting, and
would not say which derived instruction remains in the region after an
optimisation.  Marker calls exist only at the WASM/WAFFLE boundary; they are
lowered into explicit metadata before general IR passes run.

### 4.1 Shared declaration types

`volar-ir-common` should define the common declaration and ID types, alongside
`OracleDecl`, `ActionDecl`, and `RngDecl`:

```rust
pub struct InstructionGroupDecl {
    pub name: String,
    pub params: Vec<TypeId>,
    pub disposition: GroupDisposition,
}

pub enum GroupDisposition {
    /// A consumer may decline the optimisation and explicitly erase the group.
    Advisory,
    /// A registered consumer must validate and lower the group before a
    /// pass that cannot preserve region semantics, notably movfuscation.
    MustConsumeBeforeMovfuscation,
}

pub struct InstructionGroupId(pub u32);
pub struct InstructionGroupDeclId(pub u32);

pub struct GroupMembership {
    /// Outer-to-inner static group-instance IDs.
    pub stack: Vec<InstructionGroupId>,
}
```

`GroupDisposition` is deliberately a compiler-contract classification, not a
claim about cryptographic safety.  `bounded_loop` is expected to be required;
a speculative SIMD/batch region is normally advisory.  A group-specific
consumer may impose stricter policy through its own configuration.

The exact Rust names can be adjusted during implementation, but the separation
between declaration identity, instance identity, and membership is required.

### 4.2 Group membership as a `StandardMetadata` axis

`volar_ir_common` now provides the concrete foundation:

```rust
pub struct Node<T, M: NodeMetadata = StandardMetadata<()>> {
    pub kind: T,
    metadata: M, // private
}

pub struct StandardMetadata<P> {
    provenance: P,
    side: Option<SideId>,
    // instruction_groups is added here, not to Node.
}
```

The group implementation adds a private, always-present `GroupMembership`
field to `StandardMetadata<P>`:

```rust
pub struct GroupMembership {
    /// Static instance IDs, ordered outermost to innermost.
    stack: Vec<InstructionGroupId>,
}
```

`StandardMetadata::new(provenance, side)` must initialise this field to the
empty stack, so existing `Node::new(kind, provenance, side)` introduction
sites keep their established meaning: nodes introduced outside a group have no
membership.  No `Default` or synthetic metadata constructor is added.

Expose focused methods rather than the field itself, at least:

```rust
impl<P: Clone> StandardMetadata<P> {
    pub fn instruction_groups(&self) -> &GroupMembership;
    pub fn with_instruction_groups(self, groups: GroupMembership) -> Self;
    pub fn map_instruction_groups<E>(
        self,
        f: impl FnOnce(GroupMembership) -> Result<GroupMembership, E>,
    ) -> Result<Self, E>;
}

impl<T, P: Clone> Node<T, StandardMetadata<P>> {
    pub fn instruction_groups(&self) -> &GroupMembership;
    pub fn with_instruction_groups(self, groups: GroupMembership) -> Self;
    pub fn map_instruction_groups<E>(
        self,
        f: impl FnOnce(GroupMembership) -> Result<GroupMembership, E>,
    ) -> Result<Self, E>;
}
```

The node helper is a thin, auditable wrapper over the existing fallible
`Node::map_metadata`; it does not expose or reconstruct provenance and side.
Its error is exactly the error returned by `f`.  `GroupMembership` itself
provides a fallible ID remapper, whose callback is invoked once for every
stored `InstructionGroupId`.  It is for cloning/inlining/renumbering static
instances only; it does **not** remap captured SSA references in instance
tables.

The existing default node operations have precise group behaviour:

1. `source.derived(new_kind)` clones the *entire* metadata container, including
   the exact group stack;
2. `source.map_kind(...)` moves the container unchanged when its infallible
   payload callback succeeds; and
3. a true frontend introduction constructs ordinary `StandardMetadata` with an
   empty group stack, then explicitly calls `with_instruction_groups` only
   when the validated source program point is inside a group.

Thus one-source and one-source-to-many transforms do not have to mention
instruction groups merely to preserve them.

### 4.3 Merges are explicit `map2` metadata policies

The implemented generic merge API is:

```rust
left.map2(right, payload_mapper, metadata_mapper)
```

where both mappers are fallible and failure is reported as either
`Map2Error::Payload` or `Map2Error::Metadata`.  For standard metadata,
`StandardMetadataMap2` already delegates provenance to
`DualProvenanceHandler` and delegates side handling to a `MapSide2` policy
(the default is `volar_side::propagate`).

Adding group membership must extend this standard merge policy with a
**required group-stack policy**.  It must not silently inherit the left stack,
clear both stacks, or take a common prefix merely because that is convenient.
The standard policy constructor used by a merge must name one of:

- an equality/preservation policy that succeeds only for equal stacks;
- a consumer-specific policy that proves a common-prefix or other replacement
  stack is valid; or
- a rejecting policy for transformations that must not combine grouped values.

The group policy is fallible, participates in the metadata branch of
`Map2Error`, and is independent of the provenance and side policies.  It is
normal for an SSA simplification, CSE candidate, phi construction, or
inlining rewrite to reject a proposed merge when the group policy cannot
justify it.

For a result that has only one semantic source, do **not** use `map2`:
`derived`/`map_kind` is the correct automatic propagation path.  The existing
`StandardMetadataMap2::map_left` and `map_right` helpers are appropriate when
a provenance conversion policy deliberately selects exactly one input; they
copy that input's side and, after this extension, its exact group stack.

### 4.4 Instance tables remain module-level metadata

A group declaration alone does not store captured SSA values.  Each IR layer
needs an instance table.

```rust
pub struct InstructionGroupInstance<V> {
    pub id: InstructionGroupId,
    pub decl: InstructionGroupDeclId,
    pub inputs: Vec<V>,
}
```

- `vaffle::Module` gains `instruction_group_decls`; each `FuncBody` gains the
  instances defined in that function, with `V` a **typed packed VAFFLE value**
  (rather than a naked bit vector).
- `IRBlocks` gains the declaration table and a flattened instance table.  Its
  inputs use an explicit block-qualified reference, conceptually
  `IRGroupValueRef { block: IRBlockId, var: IRVarId }`, because an `IRVarId`
  is local to an IR block.
- A dedicated **fallible** table-reference remapper must update group input
  references when blocks are split, cloned, inlined, or SSA values are
  substituted.  Its callback error propagates to the calling pass.  It is
  separate from `GroupMembership::map_ids`: group inputs are data uses, not
  metadata IDs, strings, or debug coordinates.

A module-wide allocator must ensure that `InstructionGroupId` stays unique
when VAFFLE functions are lowered into the single `IRBlocks` block collection.

### 4.5 Explicit introduction, consumption, and scaffolding policy

A generated instruction must use one of these deliberate policies:

1. **one source instruction → derived instructions:** use `derived` or
   `map_kind`, which automatically preserves the exact complete metadata;
2. **combining sources:** use `map2` with an explicit group-stack metadata
   policy; reject the rewrite if the policy cannot justify the result;
3. **control-flow/frame scaffolding:** construct fresh standard metadata, hence
   an empty group stack, unless it is semantically part of the source operation
   being transformed; and
4. **group consumer output:** either retain an explicitly selected replacement
   stack or consume the group and remove its membership only after recording
   the equivalent specialised lowering.

This keeps a source-level region from accidentally absorbing unrelated
call-frame packing, spill/reload, or virtual-machine bookkeeping merely
because that scaffolding was emitted nearby.

### 4.6 Optimisation boundary rules

Membership is semantic optimisation metadata.  Until a group is consumed,
passes must not silently change which computations are members of a group.
In particular:

- CSE and value numbering must not merge values with different group stacks.
- Hoisting, sinking, block fusion, and code motion may cross a boundary only if
  they preserve the member stack of every resulting instruction.
- A VAFFLE `Value::Call` whose membership stack is non-empty is not permitted
  to reach VAFFLE-to-Volar-IR call lowering.  It must be inlined while the
  group is still represented directly, or the program is rejected.  The
  normal stack-frame-based call lowering remains available outside groups.
- DCE may remove a member only if it also updates the group representation in
  a way accepted by that group's consumer.  Group inputs count as live while
  their instance remains live.
- Existing ordering rules remain additive: actions cannot be reordered or
  deleted; RNG occurrences cannot be merged or reordered; oracle calls remain
  pure only within a transformation that also preserves group membership.

A small shared helper should expose predicates such as `same_group_stack` and
`common_group_prefix`, so each pass does not invent subtly different rules.

## 5. WASM marker ABI

### 5.1 Import shape

The initial ABI reserves a marker import namespace, for example:

```wat
(import "volar" "begin_bounded_loop" (func $begin_bounded_loop (param i32)))
(import "volar" "end_bounded_loop"   (func $end_bounded_loop))

(import "volar" "begin_batch" (func $begin_batch (param i32)))
(import "volar" "end_batch"   (func $end_batch))
```

The exact parameter types come from the registered declaration.  A begin
function has precisely the declaration's parameters and no results.  Its end
function has no parameters and no results in the first version.  The imports
are intentionally ordinary WASM calls so language toolchains preserve them as
externally observable calls; Volar removes them only after recognising and
validating them.

Source-language support can initially provide thin, `noinline` wrappers around
these imports.  Those wrappers are compilation annotations, not functions an
application is expected to implement at runtime.

### 5.2 Import configuration

Recognition must be explicit, like the existing `WaffleImportConfig` mappings
for oracles and actions.  Extend it (or add a sibling configuration) with a
paired group registration:

```rust
config.with_instruction_group(
    "begin_bounded_loop",
    "end_bounded_loop",
    InstructionGroupDecl {
        name: "bounded_loop".into(),
        params: vec![u32_tid],
        disposition: GroupDisposition::MustConsumeBeforeMovfuscation,
    },
);
```

The implementation must reject these conditions before lowering a function:

- an import configured as both an oracle/action and a group marker;
- a begin/end import whose WASM signature does not match its declaration;
- marker calls with results, indirect marker calls, or a call to an end marker
  with arguments;
- unknown imports in the reserved `volar` marker namespace (to catch spelling
  errors instead of lowering a normal external call); and
- an end marker whose top-of-stack declaration differs from the configured
  one.

A group declaration is registered once in the output VAFFLE module and then
propagated to Volar IR, just as external primitive declarations are propagated
now.

### 5.3 Why `end_*` does not take a handle

A dynamic handle would make grouping an ordinary runtime protocol and permit
non-nested/escaping regions that cannot be represented reliably through CFG
transforms.  The nearest matching begin on the validated static group stack is
the pairing rule.  Distinct static IDs still make multiple same-type groups
unambiguous to every later pass.

## 6. WAFFLE → VAFFLE lowering

`lower_waffle_function` currently walks WAFFLE instructions and emits many
bit-level VAFFLE values per WASM operation.  Group handling should be a
separate preparation stage rather than mutable state based on block iteration
order:

1. **Classify marker calls.** Scan WAFFLE `Call` operations using the group
   import configuration.  Assign each begin call a fresh static ID and retain
   its source argument values.
2. **Validate CFG scope.** Run the stack data-flow algorithm from §3.3 at
   instruction granularity, producing the active stack before each non-marker
   instruction and the begin/end pairing.
3. **Register instances.** Lower begin arguments normally to typed packed
   `VaffleValue`s, then record those values in the function's group-instance
   table.  Marker calls themselves emit no `Value::Call` and no
   `Value::Output`.
4. **Tag emitted VAFFLE nodes.** Before lowering each ordinary WASM operation,
   set the target's current group stack from the validated program point.
   Every VAFFLE node emitted for that source operation receives the stack.
5. **Preserve CFG edges.** Block parameters and terminators remain ordinary
   VAFFLE structure.  The validator, not a marker pseudo-instruction, carries
   group context across edges.

`VaffleTarget` should mirror its existing per-node side mechanism with an
explicit current membership context.  The context is reset/restored by the
frontend; it is not inferred from the target's current block.

### 6.1 Calls in a group

The first implementation deliberately does not define group-aware call
lowering.  `lower_vaffle_to_ir` turns VAFFLE calls into stack-frame and
spill/reload machinery, and assigning that derived machinery a correct region
meaning would require a separate, interprocedural design.

Accordingly, a VAFFLE `Value::Call` with a non-empty membership stack must be
**rejected or inlined before VAFFLE-to-Volar-IR lowering**:

- the frontend may inline a direct, available, non-recursive callee while the
  group is still explicit; all operations derived from the inlined call carry
  the caller's active stack (and any callee-local group instances are renamed
  and nested correctly);
- a direct call that cannot satisfy those conditions is a lowering error; and
- indirect, imported non-marker, recursive, and mutually recursive calls in a
  group are rejected in the initial implementation.

Calls outside a group retain the existing lowering unchanged.  A future
interprocedural design may lift this restriction, but it must specify both
call-frame membership and recursion semantics rather than inheriting group
metadata incidentally.

## 7. VAFFLE → Volar IR and downstream passes

### 7.1 VAFFLE lowering

`lower_vaffle_to_ir` must copy declarations and instances, translate each
VAFFLE `ValueId` group stack to the emitted IR node(s), and map captured
instance inputs to block-qualified IR variables.  It must reject a
`Value::Call` with non-empty membership as a defence in depth: §6.1 requires
such calls to have been inlined or rejected already.  Existing call splitting,
packing, spill/reload, and recursion-frame machinery is consequently only
used outside instruction groups.

`vaffle_ssa`, store forwarding, substitution, inlining, type remapping, and
all `Value::map`/`Stmt::map`-based rewrites must rely on `derived`/
`map_kind` for one-source results and use `map2` with an explicit
`MapMetadata2` policy for genuine merges.  They also need the separate
fallible instance-table input-reference remapper.  Group metadata cannot rely
on a `Stmt` match arm alone because it lives in `StandardMetadata` and the
instance tables.

### 7.2 Consumption barrier

The initial pipeline should introduce an explicit `consume_instruction_groups`
stage before movfuscation/booleanisation.  It dispatches registered consumers
by declaration name and disposition:

- an **advisory** group may be specialised or may be deliberately erased after
  the consumer decides the generic scalar lowering is still correct;
- a **required** group must be validated and replaced by its consumer; and
- a declaration with no eligible consumer is an error if any of its instances
  are required.

`movfuscate`, Boolar lowering, virtualisation, and code generators must reject
remaining `MustConsumeBeforeMovfuscation` groups rather than dropping their
metadata.  This is especially important for a bounded-loop marker: generic
movfuscation is precisely the fallback the marker is intended to prohibit.

Advisory metadata may initially be preserved through these passes only where
that is mechanically safe, but it should still be consumed explicitly at the
pipeline boundary so consumers never need to reconstruct source regions from
movfuscated code.

### 7.3 Text, serialisation, fuzzing, and interpreters

- Volar-IR v2 text format prints/parses declaration tables, instances, typed
  block-qualified inputs, and per-node stacks; older v1 snapshots are rejected
  rather than silently dropping group metadata.
- `rkyv` derives cover the core shared, VAFFLE, and Volar-IR group containers.
- Advisory-group fuzz fixtures generate a valid declaration, instance, typed
  capture, and per-node membership for both VAFFLE and Volar IR. They exercise
  preservation/lowering metadata only; they must not be fed into a lossy pass
  before a consumer exists.

A VAFFLE text format still does not exist and remains separate future work.
Before group-bearing artefacts become persistent or fuzzable, continue to update:

- any future VAFFLE text format to print declaration tables, typed packed
  instance inputs, and per-value stack annotations; and
- interpreters to ignore group metadata **only after validation**, since groups
  do not independently change ordinary computation semantics.

The interpreter's ability to ignore consumed/advisory groups must not become a
license for a compiler pass to ignore a required group.

## 8. Initial group kinds

The core representation remains open-ended.  These two group kinds establish
the intended use without hard-coding either optimisation into generic IR.

### 8.1 `bounded_loop`

```text
begin_bounded_loop(limit)
  // a natural loop with a validated counter/limit recurrence
end_bounded_loop()
```

- **Inputs:** initially one unsigned `i32` or `i64` limit; the final contract
  may also capture initial counter and step if validation needs them.
- **Disposition:** `MustConsumeBeforeMovfuscation`.
- **Consumer:** identifies a single-entry/single-exit natural loop inside the
  group, checks that the captured bound agrees with the loop's induction
  recurrence and exit condition, then emits the dedicated bounded-loop
  lowering.
- **Failure:** a non-natural CFG, mismatched bound, unknown trip count, or
  unsupported side effect is an error, not permission to movfuscate the loop.

Existing `ReentryHint::bounded_loop_ascending()` remains useful evidence for a
well-founded back edge.  The group consumer may require or create the hint,
but the hint and group serve different purposes: the hint describes a branch
measure; the group identifies the whole region and carries its declared bound.

### 8.2 `batch`

```text
begin_batch(lanes)
  // candidate scalar operations that a backend may vectorise together
end_batch()
```

- **Inputs:** initially a lane count or static batch shape represented by the
  declaration's typed arguments.
- **Disposition:** normally `Advisory`.
- **Consumer:** validates compatible operations, dependencies, memory access,
  and target availability, then emits vector/batched operations or another
  equivalent representation.
- **Fallback:** if vectorisation is not legal or profitable, explicitly consume
  the advisory group and retain scalar semantics.

A batch group is not a claim that every enclosed instruction independently
runs once per lane.  Its declaration-specific consumer defines that mapping;
the core merely gives it a stable, typed region to inspect.

## 9. Implementation sequence

This sequence begins from the implemented `NodeMetadata` / `StandardMetadata`
foundation.  It must use `Node::derived`, `Node::map_kind`, fallible
`Node::map_metadata`, and fallible `Node::map2`; it must not add a direct
`Node` field or reintroduce parallel stack propagation.

1. **Metadata-extension contract and tests.** Add `GroupMembership` as the
   private `StandardMetadata` axis; add the focused fallible membership/ID
   remappers; extend `StandardMetadataMap2` with the required group-stack
   policy; and test one-source preservation, rejected merges, approved merges,
   and ID remapping errors.  Define the group validation error enum and add
   WASM fixtures for nested same-type groups, CFG joins, loops, mismatched
   ends, and captured values.
2. **Shared group and instance types.** Add declaration/ID/instance types in
   `volar-ir-common`, including the separate fallible table-reference remapper.
   Capture aggregate inputs as typed packed values, not bare bit-ID vectors.
   Update mapping and archive derives.
3. **VAFFLE containers and target.** Add declaration and instance tables;
   teach `VaffleTarget` to apply an already-validated current group stack via
   the focused metadata API; update all module constructors and
   cloning/remapping helpers.
4. **WASM marker frontend.** Extend `WaffleImportConfig`, classify marker
   imports, run CFG-stack validation, omit marker calls, and attach group
   membership to normal emitted values. A direct non-recursive call inside a
   group must be inlined at this stage or rejected; indirect/imported
   non-marker and recursive calls in a group are rejected.
5. **VAFFLE-to-IR propagation.** Transfer declarations, instances, stacks,
   and captured input references through `vaffle_ssa` and `lower_to_ir`; add a
   defence-in-depth rejection for a group-bearing `Value::Call`.
6. **Pass audit.** Update optimisers, substitution, virtualisation,
   movfuscation, Boolar lowering, text IR, serialisation, generators, and
   interpreters. Use `derived` for one-source outputs and audited `map2`
   policies for merges; add a hard rejection for unconsumed required groups at
   each lossy boundary.
7. **Consumer framework.** Add the dispatch/consumption stage plus a no-op
   advisory consumer for controlled fallback.
8. **Bounded-loop consumer.** Implement and validate the first required
   consumer before exposing `begin_bounded_loop` as a supported public API.
9. **Batch consumer.** Add target-specific vector/batch lowering and its
   profitability/legality checks. It may initially be experimental while the
   marker infrastructure remains general.
10. **Documentation and API wrappers.** Document source wrappers, import ABI,
    supported group declarations, pipeline placement, and generated-artifact
    format changes.

## 10. Test strategy

Structural tests are appropriate only for hard invariants: unique IDs, typed
input storage, well-nested stack validation, and refusal to cross a group
boundary during an otherwise-invalid rewrite.  They are not sufficient to
validate an optimisation.

For every consumer, integration tests must:

1. compile a WASM fixture containing begin/end imports;
2. lower it through WAFFLE, VAFFLE, and Volar IR;
3. run the group-consumption pass and the real selected backend; and
4. compile and execute the generated Rust/C/TypeScript artefact against a
   scalar reference result.

Required-group negative tests must prove that a malformed or unsupported
bounded loop fails *before* movfuscation.  Advisory batch tests must exercise
both successful vectorisation and an explicit scalar fallback.  Differential
fuzzing should generate only CFG-valid, properly nested marker programs and
compare consumed output with the ungrouped program.

## 11. Decisions and deferred work

The following choices are settled for the initial design.

1. **Group declarations are host-configured.**  They are registered through
   `WaffleImportConfig` (or its group-specific sibling), exactly as other
   imported interfaces are.  Core IR stores the declarations selected by the
   host, but does not embed a standard declaration registry.
2. **Captured aggregate values remain typed and packed.**  A group instance
   stores the typed packed VAFFLE value, rather than exposing an implementation
   detail such as a bare vector of bit `ValueId`s or a source-level reference.
   Its lowering must preserve that type and map the packed representation to
   its block-qualified Volar-IR input without using pre-rendered text.
3. **Calls inside groups are rejected or inlined.**  As specified in §6.1,
   group-aware stack-frame call lowering is deferred.  No group-bearing
   `Value::Call` may enter `lower_vaffle_to_ir`.
4. **The public ABI remains import-based.**  A Rust, C, or other language
   wrapper may provide a language-native interface, but it must emit and
   preserve the same `begin_{region}`/`end_{region}` imports.  The compiler
   recognises imports, not wrapper-specific source syntax.
5. **Interprocedural groups are deferred.**  A later design may establish
   explicit call-graph, inlining, call-frame, and recursion semantics.  Until
   then, the only permitted way for a direct callee's ordinary work to occur
   in a group is frontend inlining under §6.1.

---

## Decision summary

Instruction groups are **typed, named, static region instances** with unique
IDs, captured SSA inputs, and properly nested per-instruction membership.
WASM `begin_{region}`/`end_{region}` imports establish them; they are validated
and erased into metadata at the WAFFLE frontend.  Generic passes preserve the
metadata and do not optimise across its boundaries casually.  A mandatory
consumption barrier ensures that a group such as `bounded_loop` receives its
dedicated lowering rather than silently falling into movfuscation.