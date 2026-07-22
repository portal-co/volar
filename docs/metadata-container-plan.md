# Per-Node Metadata Container Refactor

**Status:** Planning — prerequisite to instruction groups.  
**Scope:** Tier-2 compiler/IR infrastructure.  
**@ai:** assisted

---

## 1. Purpose

Volar's per-value annotations currently live directly on
`volar_ir_common::Node<T, P>` (where `P` is provenance):

```rust
pub struct Node<T, P> {
    pub kind: T,
    pub prov: P,
    pub side: Option<SideId>,
}
```

This was appropriate while provenance and side were the only metadata axes.
It does not scale safely to instruction-group membership or later metadata.
Every new direct field would require every transform that reconstructs a node
rather than cloning it to remember every existing and future annotation.  A
pass that has no semantic interest in a new annotation should not need to
know that it exists merely to preserve it.

Before adding instruction groups, refactor `Node` to own one **per-node
metadata container**.  Ordinary one-to-one and one-to-many transformations
will propagate that container by default.  A pass only handles a metadata axis
when it deliberately introduces, merges, erases, validates, or remaps that
axis.

This is an infrastructure refactor, not a change to circuit or program
semantics.  It precedes and unblocks
[`instruction-groups-plan.md`](instruction-groups-plan.md).

## 2. Goals

- Make payload rewriting preserve all per-node metadata by default.
- Keep the established provenance and side invariants intact:
  - provenance is never invented; and
  - a side is never invented or guessed.
- Provide explicit, auditable APIs for the exceptional cases:
  provenance conversion, side assignment, metadata replacement, ID remapping,
  and multi-source merging.
- Give future metadata a single propagation path instead of adding parallel
  vectors or expanding every constructor at every IR layer.
- Support the two remapping cases instruction groups will require:
  - remapping a group-instance ID after cloning/inlining; and
  - retaining/remapping value references stored outside a node (the group
    instance table).
- Work in `#![no_std]` and remain compatible with the optional `rkyv`
  serialization path.
- Make metadata preservation visible in code review: a fresh node must be
  either derived from a source node, created with an explicit metadata value,
  or be a documented true introduction point.

## 3. Non-goals

- A dynamic `Any`/`TypeId` typemap, plugin registry, or string-keyed bag of
  annotations.  Those designs are unsuitable for `no_std`, difficult to
  serialize deterministically, and hide required remapping from the compiler.
- A universal rule for merging metadata from two or more source values.  Such
  a rule would be unsound for both side and future instruction-group metadata.
- Moving module-level metadata (declarations, storage pre-initialization,
  function signatures, or group instance tables) into per-node metadata.
- Making all metadata semantically advisory.  A future metadata consumer may
  impose a required optimisation boundary; the container only ensures that
  uninterested passes preserve it.
- Implementing instruction-group declarations, marker imports, or consumers.
  Those remain the next plan's work.

## 4. Target model

### 4.1 `Node` has one metadata type parameter

Replace the public parallel fields with a metadata **trait** and a concrete
standard container.  `Node` is parameterised by the one metadata type, not by
a growing list of independent annotation types:

```rust
pub trait NodeMetadata: Clone {
    type Provenance: Clone;
    type Side: Clone;

    fn provenance(&self) -> &Self::Provenance;
    fn side(&self) -> &Self::Side;
}

pub struct StandardMetadata<P: Clone> {
    provenance: P,
    side: Option<SideId>,
    // Future first-party metadata fields live here, behind accessors.
}

impl<P: Clone> NodeMetadata for StandardMetadata<P> {
    type Provenance = P;
    type Side = Option<SideId>;
    // ...
}

pub struct Node<T, M: NodeMetadata = StandardMetadata<()>> {
    pub kind: T,
    pub metadata: M,
}
```

The single `M` parameter is intentional.  If an extension ever requires a
metadata type parameter in addition to provenance, it becomes an associated
type of `M`; `Node` does not become `Node<T, P, S, G, ...>`.  First-party
metadata that has a fixed representation, such as `Option<SideId>` and the
future instruction-group stack, stays private in `StandardMetadata` and is
exposed through focused accessors.

`StandardMetadata<P>` is the normal pipeline choice, so provenance remains
caller-supplied.  A specialised IR consumer can provide another `M` only when
it implements the complete metadata contract.  This makes its propagation and
mapping obligations explicit rather than letting it add an untracked parallel
annotation.

Fields are private outside `volar-ir-common`.  This prevents callers from
constructing a partial metadata value when fields are added later.  Read-only
accessors include at least those on `NodeMetadata`; `StandardMetadata` also
provides its concrete side/group extension accessors.

The migration intentionally removes direct `.prov` and `.side` field access.
It is preferable for this to be a source-visible, compiler-enforced break than
for a future metadata field to be silently dropped by legacy construction.

### 4.2 Default derivation and fallible one-source mapping

The normal operation for an SSA rewrite is to change a payload while retaining
all metadata from a source node.  Every method whose purpose is to *map* a
payload or metadata is fallible; its error is the error supplied by the passed
mapping function:

```rust
impl<T, M: NodeMetadata> Node<T, M> {
    pub fn map_kind<U, E>(
        self,
        f: impl FnOnce(T) -> Result<U, E>,
    ) -> Result<Node<U, M>, E>;

    pub fn map_metadata<N: NodeMetadata, E>(
        self,
        f: impl FnOnce(M) -> Result<N, E>,
    ) -> Result<Node<T, N>, E>;

    pub fn derived<U>(&self, kind: U) -> Node<U, M>;
}
```

`map_kind` moves the existing metadata; `derived` clones it.  A
one-source-to-many lowering calls `source.derived(...)` for each output.
This is the only default propagation rule: **derived values receive an exact
copy of their declared source node's complete metadata.**  `derived` is not a
mapping operation and remains infallible because it neither calls a user
mapping function nor makes a policy decision.

The helpers replace patterns such as:

```rust
Node::new(new_kind, source.prov.clone(), source.side)
```

and prevent a later metadata field from becoming another required constructor
argument throughout the workspace.

For true introduction points (for example a frontend parameter annotation or
a freshly configured external output), construction remains explicit:

```rust
Node::with_metadata(kind, StandardMetadata::new(provenance, side))
```

There is no `Default` implementation or `synthetic()` escape hatch for
metadata carrying arbitrary provenance.  This preserves the provenance
pipeline's existing “never invented” property.

### 4.3 Controlled and fallible metadata transformations

A pass that actually changes one axis uses a narrow, fallible operation rather
than reconstructing the entire container:

```rust
impl<P: Clone> StandardMetadata<P> {
    pub fn map_provenance<Q: Clone, E>(
        self,
        f: impl FnOnce(P) -> Result<Q, E>,
    ) -> Result<StandardMetadata<Q>, E>;

    pub fn map_side<E>(
        self,
        f: impl FnOnce(Option<SideId>) -> Result<Option<SideId>, E>,
    ) -> Result<Self, E>;
}

impl<T, M: NodeMetadata> Node<T, M> {
    pub fn map_metadata<N: NodeMetadata, E>(
        self,
        f: impl FnOnce(M) -> Result<N, E>,
    ) -> Result<Node<T, N>, E>;
}
```

Convenience operations such as `with_side` may remain infallible because they
do not invoke a mapping callback.  `Node::map_prov` and `Node::map_kind_prov`
remain source-compatible *semantic* entry points, but become fallible and are
implemented in terms of the metadata mapper.  Callers with an infallible
conversion use `Result<_, Infallible>` and unwrap/convert at their own API
boundary; the core must not hide errors that a metadata conversion reports.

Side-aware consumers use `with_side`/`map_side`; ordinary passes do not call
them. A metadata transformation must document why it changes an axis. In
particular, it must not manufacture a provenance value, choose an arbitrary
`SideId`, or clear metadata just to make an intermediate representation
convenient.

### 4.4 Fallible `map2` and pluggable two-source metadata mapping

Two-source operations must use a single `map2` operation. It receives a
fallible payload mapper and a metadata mapping trait instance; it does not
silently select the left or right node's metadata:

```rust
pub trait MapMetadata2<L: NodeMetadata, R: NodeMetadata> {
    type Output: NodeMetadata;
    type Error;

    fn map_metadata2(
        &self,
        left: L,
        right: R,
    ) -> Result<Self::Output, Self::Error>;
}

pub enum Map2Error<PayloadError, MetadataError> {
    Payload(PayloadError),
    Metadata(MetadataError),
}

impl<T, M: NodeMetadata> Node<T, M> {
    pub fn map2<U, N, V, PayloadError, MM>(
        self,
        other: Node<U, N>,
        map_kind: impl FnOnce(T, U) -> Result<V, PayloadError>,
        map_metadata: &MM,
    ) -> Result<Node<V, MM::Output>, Map2Error<PayloadError, MM::Error>>
    where
        N: NodeMetadata,
        MM: MapMetadata2<M, N>;
}
```

The production signature is shown with a closure deliberately: the payload
mapping function receives both kinds and returns `Result`, while the metadata
mapper receives both metadata values and returns `Result`. `Map2Error`
identifies whether the payload or metadata policy failed (or an equivalent
concrete error strategy is used without losing that distinction).

This makes a two-source transformation auditable in one place. CSE,
substitution, folding, phi/block-argument construction, and pairwise weaving
must call `map2` or define an explicitly named equivalent wrapper around it.
They cannot accidentally inherit one operand's provenance, side, or future
metadata.

`MapMetadata2` is the extension point for cases that genuinely need a custom
policy. The standard implementation is a compositional mapper whose
provenance portion exposes a `DualProvenanceHandler` and whose side portion
uses a default mapper unless overridden:

```rust
pub struct StandardMetadataMap2<Prov, Side = DefaultSideMap2> {
    pub provenance: Prov,
    pub side: Side,
    // Each later metadata axis has an equivalent field with a safe default
    // only if the axis defines one.
}

pub trait MapSide2 {
    type Error;
    fn map_side2(
        &self,
        left: Option<SideId>,
        right: Option<SideId>,
    ) -> Result<Option<SideId>, Self::Error>;
}

pub struct DefaultSideMap2;

impl MapSide2 for DefaultSideMap2 {
    type Error = Infallible;
    fn map_side2(
        &self,
        left: Option<SideId>,
        right: Option<SideId>,
    ) -> Result<Option<SideId>, Infallible> {
        Ok(volar_side::propagate(&[left, right]))
    }
}

impl<P1, P2, Prov, Side> MapMetadata2<StandardMetadata<P1>, StandardMetadata<P2>>
    for StandardMetadataMap2<Prov, Side>
where
    Prov: DualProvenanceHandler<P1, P2>,
    Side: MapSide2,
{
    type Output = StandardMetadata<Prov::Output>;
    type Error = StandardMetadataMap2Error<Side::Error>;
    // provenance uses Prov::merge; side uses Side::map_side2.
}
```

The pseudocode above omits only routine bounds/error plumbing; its production
implementation must preserve both axis identity and the original error. The
important shape is that the standard two-metadata mapper is a trait instance,
not an implicit hard-coded branch in `Node::map2`.

The exact adapter must distinguish `map_left`, `map_right`, and `merge` at
operations that need those attribution modes. `map2` uses `merge`; wrappers
for one-sided host/replacement output select `map_left` or `map_right` rather
than pretending they are a two-source merge. This exposes the established
`DualProvenanceHandler` through the general metadata machinery without baking
provenance-specific logic into every transform.

#### Default two-source policies

The standard metadata mapper supplies defaults **only where the field's
semantics already define one**:

- **Provenance:** no universal default. A `DualProvenanceHandler` (or a
  specialised metadata mapper) is required, preserving the existing rule that
  attribution is an explicit decision.
- **Side:** default to `volar_side::propagate([left.side(), right.side()])`.
  This carries forward the existing common-side rule and is fallible at the
  mapper interface even though the current operation itself cannot fail.
- **Future metadata:** each field declares one of: an explicit default
  two-source policy; a required mapper; or “not mergeable,” which rejects the
  operation. Instruction-group membership is expected to require a dedicated
  policy that validates equal stacks or an explicitly justified common-prefix
  result; it must never inherit either stack by accident.

A custom `MapMetadata2` can override any axis, but must state why. This makes
pluggability available for specialised lowerings while keeping the ordinary
case concise and complete.

### 4.5 All mapping APIs are fallible

Every public API named `map`, `map_*`, `try_map`, `map2`, or serving as an
internal implementation of one takes a callback/trait method returning
`Result<_, E>` and itself returns `Result<_, E>` (or a documented composite
error such as `Map2Error`). This applies recursively to:

- payload reference maps such as `Stmt::map`, `Value::map`, branch-target
  mapping, and map/as-ref/as-mut helpers where a callback is involved;
- node payload mapping, metadata mapping, provenance mapping, and `map2`; and
- extension-specific ID and instance-table remappers.

Non-mapping convenience operations that cannot fail — cloning/deriving a
node, direct `with_*` replacement, and simple read-only projections — remain
infallible. No `map` API may swallow an error or convert it to a panic merely
because today's built-in metadata rule is infallible.

Migration may use `Infallible` adapters for existing transformations, but all
new implementations must expose fallibility at the first public mapping layer.
This allows metadata extensions to reject invalid remaps/merges and allows
callers to propagate a structured lowering error instead of discovering
metadata corruption later.

## 5. Metadata remapping contract

### 5.1 Why propagation is not enough

Copying metadata is correct for ordinary lowering but not for transforms that
create a new identity namespace.  Inlining or cloning a function may need to
rename static instruction-group IDs.  A group-instance table may hold SSA
references that must be remapped when values or blocks are substituted.

Those are two different operations:

1. **per-node metadata remapping** rewrites opaque metadata identifiers stored
   on each `Node`; and
2. **container/table remapping** rewrites references owned by a module or
   function table.

The metadata container plan standardises the first.  The instruction-groups
plan will define the second on its own typed instance-table types.

### 5.2 Extension-specific remappers

When metadata gains a remappable extension, it supplies a focused trait or
method with an explicit mapping domain.  The intended pattern is:

```rust
pub trait RemapMetadataIds<IdMap> {
    type Error;

    fn remap_metadata_ids(
        &mut self,
        ids: &IdMap,
    ) -> Result<(), Self::Error>;
}

impl<P: Clone> StandardMetadata<P> {
    pub fn remap_instruction_group_ids<E>(
        &mut self,
        map: &impl Fn(InstructionGroupId) -> Result<InstructionGroupId, E>,
    ) -> Result<(), E>;
}
```

The exact group-specific method is introduced only with instruction groups;
the generic refactor must establish the location and naming convention, not
add placeholder group types.

Requirements for every future remapper:

- It maps only the metadata domain it names; provenance and side pass through.
- The mapping is total for identities that may occur in the input, or its
  fallible callback/trait result reports a typed error. It must never silently
  leave an unknown renamed ID behind.
- It is callable on `Metadata`/`Node` without exposing unrelated fields.
- The owner of an external reference table provides a matching remapper and
  is tested with the node remapper in the same clone/inline operation.
- A pass that does not create a new namespace only clones metadata and does
  not invoke a remapper.

### 5.3 Payload reference remapping stays separate

`Stmt::map`, `Value::map`, branch-target mapping, and value substitution
already remap references in a payload.  They must not be overloaded to know
about every metadata extension.  The standard transform sequence is:

1. remap the payload with its existing typed mapper;
2. derive/copy the source metadata unchanged; and
3. only if the operation renamed metadata identities, invoke that extension's
   focused metadata remapper and the matching module/function-table remapper.

This separation avoids a new metadata field forcing every `Stmt` match arm or
IR mapper to become aware of it.

## 6. Required API and migration inventory

### 6.1 `volar-ir-common`

`volar-ir-common` becomes the sole owner of `NodeMetadata`,
`StandardMetadata<P>`, and `Node`'s construction/mapping APIs. It must retain
derives and `rkyv` attributes needed by all current `Node` users.

Initial APIs:

| API | Intended use |
|---|---|
| `StandardMetadata::new(provenance, side)` | Explicit standard frontend/introduction construction |
| `Node::new(kind, provenance, side)` | Compatibility convenience; delegates to `StandardMetadata::new` |
| `Node::with_metadata(kind, metadata)` | Explicit full metadata construction |
| `Node::derived(&self, kind)` | One source → one or many derived values |
| `Node::map_kind(self, f)` | Fallibly consume/rewrite payload, preserve metadata |
| `Node::map_metadata(self, f)` | Fallibly and deliberately transform metadata |
| `Node::map2(other, map_kind, mapper)` | Fallibly merge two payloads through a `MapMetadata2` policy |
| `Node::map_prov` / `Node::map_kind_prov` | Fallible provenance contracts, routed through metadata |
| `NodeMetadata` / `StandardMetadata` accessors and focused mapping methods | Consumers that own a particular axis |

`Node::new` may remain during the migration because its arguments represent
the two current introduction axes.  New generic passes should prefer
`derived`/`map_kind`, and the plan's completion criterion includes removing
internal reconstruction patterns that manually copy provenance and side.

### 6.2 IR containers and lowering targets

The following currently construct or rebuild nodes and must migrate to the
new helpers:

- VAFFLE target emission and WAFFLE lowering;
- VAFFLE SSA, substitution, inlining, and VAFFLE-to-Volar-IR lowering;
- Volar IR block builders and all provenance mapping methods;
- IR optimisation, store forwarding, movfuscation, circuit lowering, virtual
  IR, and Boolar lowering where an `IRBlock` is rebuilt;
- text parsing/printing and archive/serialization round trips;
- LIR targets and compiler lowering paths that emit node-wrapped statements;
- weavers that lower, duplicate, or derive IR statements.

A pass that merely clones an entire `Node` needs no semantic modification. A
pass that replaces `.kind` must use fallible `map_kind` or `derived`. A pass
that allocates a node with no source must obtain explicit metadata from its
existing provenance/side introduction policy.

### 6.3 Public field migration

Convert direct reads:

```rust
node.prov.clone()  -> node.metadata.provenance().clone()
node.side          -> node.metadata.side()
```

and direct construction:

```rust
Node { kind, prov, side } -> Node::with_metadata(kind, metadata)
```

The final API should not expose `StandardMetadata` fields publicly. Rust
compilation will identify remaining direct field accesses, while targeted tests
establish that propagation is not merely type-correct but retains provenance
and side.

## 7. Validation and testing

This refactor needs behavioural tests in addition to workspace compilation.
Tests should use real lowering/consumer paths where practical, per the
compiler/IR testing policy.

1. **Core metadata tests (`volar-ir-common`).**
   - `derived` and fallible `map_kind` retain both provenance and side exactly.
   - Fallible `map_provenance` changes only provenance.
   - `with_side`/fallible `map_side` changes only side.
   - `map_kind_prov` remaps nested provenance while retaining all other
     metadata.
   - `map2` routes provenance through a `DualProvenanceHandler`, applies the
     default side propagation rule, and returns distinguishable payload and
     metadata mapping errors.
   - `rkyv` round-trip tests, when enabled, retain metadata.

2. **Existing provenance/side regression tests.**
   Exercise an existing generic lowering and side-aware weaving path; confirm
   output still carries the source provenance/side assignment.  Do not replace
   behavioural backend tests with assertions only about intermediate field
   layout.

3. **Transformation-class tests.**
   Cover one-source-to-many lowering, payload substitution, block rebuilding,
   and a true metadata introduction point.  These tests establish the default
   propagation contract across the kinds of transforms the workspace uses.

4. **Future instruction-group remapping test hook.**
   The generic refactor should add a small test-only `NodeMetadata` extension
   or a generic remapper test harness demonstrating that a focused, fallible
   ID remapper changes its own field while preserving provenance and side. It
   must not introduce production group types early. The instruction-group work
   then replaces this with real group-ID and instance-reference integration
   tests.

5. **Full checks.**
   Run all affected crate tests and the workspace check subject to toolchain
   availability.  Record unrelated blockers, such as a missing system LLVM,
   separately from metadata failures.

## 8. Implementation phases

### Phase 0 — Audit and invariants

- Inventory all `Node` constructors, literal struct construction, direct
  `.prov`/`.side` access, and node-rebuilding helpers.
- Classify each emission site as derived, multi-source, or true introduction.
- Document any existing exception that lacks a source provenance and verify it
  already uses the established control/fallback provenance policy.

### Phase 1 — Core container

- Add `NodeMetadata` plus `StandardMetadata<P>` and migrate `Node` to
  `kind + metadata` with one metadata type parameter.
- Implement fallible construction-adjacent mapping, derivation, payload
  mapping, `map2`, provenance mapping, and focused side APIs in §6.1.
- Preserve no-std and `rkyv` compatibility.
- Add core unit tests for the propagation contract.

### Phase 2 — Mechanical propagation migration

- Migrate every first-party node producer/rebuilder to `derived`, fallible
  `map_kind`, `map2`, or `with_metadata` as appropriate.
- Replace direct `prov`/`side` access with accessors.
- Preserve existing public provenance APIs as wrappers where feasible; make
  direct field access unavailable.
- Run affected crate checks continuously, then full compiler/IR checks.

### Phase 3 — Explicit multi-source audit

- Review CSE, substitution, inlining, phi/block-argument lowering, and
  weaving for operations that combine sources.
- Require each to state an explicit provenance/side policy and use `map2`,
  `MapMetadata2`, or an existing one-sided handler rather than accidental
  first-source inheritance.
- Add regression tests for each distinct policy.

### Phase 4 — Remapping convention

- Add the generic documentation and test harness for focused metadata-ID
  remapping.
- Establish typed owner-side table remappers as the companion pattern.
- Do not add a generic untyped remapping callback to every pass; remappers
  belong to the extension and are invoked only by namespace-changing passes.

### Phase 5 — Gate instruction groups

Mark this plan complete only when all normal node transformations preserve the
container without knowing its individual fields.  Then start the instruction
-groups implementation in its documented order:

1. group declaration/instance types and a `Metadata` group-membership
   extension;
2. typed group instance-table remappers;
3. CFG marker validation and WAFFLE propagation; and
4. group consumers and required-consumption barriers.

## 9. Open decisions resolved by this plan

| Question | Decision |
|---|---|
| Dynamic typemap or structured metadata? | Structured `NodeMetadata` contract with private `StandardMetadata<P>` fields and explicit extension APIs. |
| Does every pass receive a metadata type parameter? | `Node` has exactly one metadata parameter (`M: NodeMetadata`); associated types carry any further metadata types. |
| Default metadata behaviour? | Exact cloning from one declared source node. |
| Can generic code silently merge metadata? | No; `map2` requires an explicit `MapMetadata2` policy, with only field-specific defaults. |
| Are mapping operations fallible? | Yes: all callback/trait-based maps return `Result`; non-mapping convenience operations remain infallible. |
| Where does an instruction-group stack live? | In a future `StandardMetadata` extension, not on `Stmt`, `Value`, or parallel vectors. |
| Who remaps instance-table SSA references? | The typed group instance-table owner, coordinated with but separate from node metadata remapping. |
| Should direct fields remain public for compatibility? | No after migration; accessors and derivation helpers enforce future-safe propagation. |

## 10. Completion criteria

This plan is complete when:

- `Node` has one `NodeMetadata`-bounded metadata type parameter, with private
  `StandardMetadata` fields instead of direct public provenance/side fields;
- the default node-rewrite APIs preserve complete metadata;
- provenance conversion and side changes use focused APIs without invention;
- multi-source transforms use explicit, tested `map2`/`MapMetadata2` policies
  and all callback-based mapping APIs propagate `Result` errors;
- a documented, tested extension-specific remapping convention exists; and
- `instruction-groups-plan.md` is updated to depend on this completed
  refactor before introducing group membership.