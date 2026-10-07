# Plan: storage-capable circuit providers

**Status:** accepted implementation plan; no provider or cryptographic profile is
admitted by this document. **Pinnedness:** unpinned. **Stability:** very
unstable.

This extends [`circuit-provider-abi.md`](circuit-provider-abi.md) from pure
KDF/encrypt/decrypt programs to bounded storage-bearing provider programs. It
uses the local, unpushed `main` merge at `528e5ff` as a dependency: in
particular the strict-chain held-slot registry, contiguous `HeldRange`, and
boundary-only `StorageOperation::Prefetch` support. That local main will be
pushed in batches; do not replace it with `origin/main` while implementing this
plan.

Read and update
[`future-provider-integration-ledger.md`](future-provider-integration-ledger.md)
before changing provider plumbing. The legacy `FheScheme` / TFHE surface is not
a validation substitute.

## Outcome

A provider invocation receives an immutable base storage snapshot and owns a
per-invocation mutable cache. The compiler can tailor its access circuit to the
base representation, execute a statically finite provider loop as a sequence
of fused circuit steps, and discard the cache after the invocation. The next
invocation starts from a newly copied cache, never from accidental retained
provider globals.

```text
base storage (read-only, epoch e)
        │ copy selected/candidate cells at invocation boundary
        ▼
invocation cache (private to provider invocation)
        │ provider loop: reads/writes use cache only
        ▼
outputs + explicitly exported write set
        │ cache discarded, or exported writes become next explicit base version
        ▼
next invocation gets a fresh cache
```

The terms **base storage**, **invocation cache**, **copy**, **export**, and
**discard** are semantic operations, not requests to clone plaintext in a host
process. Their implementation is ordinary composed Boolean/GC dataflow and
role-local held material.

## Module and seams

Create one deep module in `volar-vc`:

```text
circuit_provider_storage
```

Its interface accepts a validated provider descriptor, storage layout, source
access trace/region, and exact held-slot registry allocation. It returns a
fully composed and optimized circuit plan plus its explicit `StorageOperation`
prefetch script. It owns:

- base/cache layout validation;
- all source-storage to provider-storage mapping;
- readonly base optimization selection;
- copy-in and optional explicit export circuits;
- finite loop scheduling;
- cache lifetime/reset/discard rules;
- tracker integration and conversion demands; and
- preservation of exact `IRVarId` identities across rewrites.

Callers must not independently decide whether a provider read refers to the
base or cache, allocate cache slots, attach an ORAM request, or carry the cache
to another invocation. That would make the storage seam shallow and allow
accidental cross-invocation state.

The concrete adapter seam is a new `StorageCapableCircuitProvider` descriptor:

```rust
trait StorageCapableCircuitProvider {
    type Program;

    fn readonly_layout(&self) -> ReadonlyStorageLayout;
    fn loop_step(&self) -> &Self::Program;
    fn copy_in(&self) -> &Self::Program;
    fn export_writes(&self) -> Option<&Self::Program>;
}
```

The final Rust types may differ, but the behavior may not: it must be possible
to have multiple adapters with different FHE/provider programs while the
storage planner stays provider-neutral. The descriptor is fixed geometry and
programs only; it contains no host key, ciphertext object, raw storage value,
or provider security assertion.

## Phase 1 — read-only base representation

### 1.1 Layout descriptor

Implement a public `ReadonlyStorageLayout` containing only public shape:

- `BaseStorageId` — stable identity of the immutable source snapshot;
- base version / source epoch — public monotonically advancing identity;
- element bit width and public element count/capacity;
- permitted access shapes (whole scan, direct public index, bounded indexed
  scan, static read set);
- storage source classification: public constant, held material range, or
  pre-run opaque ORAM output;
- a canonical ordered mapping to `HeldRange`/`HeldSlots` where durable labels
  are needed.

Do not use string names as map keys. Track source values by `IRVarId`; where
front-end provenance comes from SWC, retain hygienic `Ident`/`Id` equality.

The layout constructor rejects zero-width / overflowed geometry, duplicate
source identity, duplicate physical cell mapping, stale epoch, and a mutable
base request. A “read-only base” must have no write API at all.

### 1.2 Tailored readonly optimizations

The planner chooses only transformations proven by the declared layout:

| Access shape | Required generated circuit / policy |
|---|---|
| Static read set | Copy only distinct demanded cells, sorted by canonical base-cell order; share duplicate reads. |
| Public direct index | Address is public and in range: resolve at plan time to one cell; no MUX/ORAM. |
| Public contiguous range | Copy precisely that interval; coalesce adjacent intervals. |
| Secret bounded index | Keep secret addressing in GC/ORAM semantics. A provider does not get a host lookup. Use a bounded MUX scan or existing pre-run ORAM token plan, selected by public cost policy. |
| Full sequential scan | Stream/copy in canonical order; do not build a selector tree. |

A base-source access may be CSEd only when its `BaseStorageId`, epoch, cell,
and representation match. It is an error to share across different epochs or
across public/held/ORAM representation kinds merely because the source variable
has the same textual name.

Existing `PreFheStoragePlan`, `OramReadPreRun`, public-address plans, and
held-material prefetch planning are inputs to this module. They remain the only
seams allowed to schedule strict ORAM and split-AES material work. The storage
provider planner must not reimplement their transport or open labels itself.

### 1.3 Copy-in protocol

Copy-in is visible composition:

1. derive the public base access manifest;
2. reserve exactly its durable source/result slot ranges through `HeldSlots`;
3. request any required `StorageOperation::Prefetch` before a strict round;
4. compose source-to-cache copies as Boolean wires/circuits;
5. build a fresh cache indexed from zero in canonical cell order;
6. register every cache value as `Clear` or `Encrypted` in
   `ProviderWireTracker`, preserving its exact identity.

The initial cache is therefore a snapshot. It must not alias the base in an
implementation data structure that makes a subsequent cache write observable
as a base mutation.

### 1.4 Phase-1 tests

Add fixture adapters over tiny pure Boolean provider circuits, not TFHE:

- static duplicate reads copy once and retain one source mapping;
- public contiguous reads coalesce;
- secret index yields declared MUX/pre-run operation, never host lookup;
- base write request fails before compilation;
- stale source epoch and duplicate mapping fail;
- copied cache write does not affect a subsequent read-only base invocation;
- output gates agree with an independent clear storage evaluator.

Ledger: `FHE-PLUMB-CIRCUIT-STORAGE-01`.

## Phase 2 — looped provider programs

### 2.1 Program contract

A looped provider is not an opaque runtime loop. Its descriptor provides a
single **finite, pure step circuit**:

```text
step(inputs, readonly base view, cache state, public loop index constants)
    -> next cache state, step outputs, continue/publicly bounded state
```

It must be transformed from IR with `unroll_ir_everything_unbounded` whenever
its control is statically finite. Symbolic control is either lowered to an
explicit Boolean select circuit by existing movfuscation/lowering machinery or
rejected. There is never a numeric “provider loop limit” that changes behavior.

The initial implementation operates over a public static trip count and builds
`N` inlined/fused step instances. A future descriptor may use a finite
compile-time schedule with heterogeneous step shapes, but it must give the same
fully materialized circuit semantics.

### 2.2 Loop scheduler

Implement `ProviderLoopPlan` with:

- exact public trip count or explicit static schedule;
- step input/output geometry;
- cache state mapping at each iteration;
- live-across-iteration wire mapping;
- read manifest for each iteration and coalesced global copy-in manifest;
- a public statement/circuit size accounting result.

The scheduler starts iteration `i + 1` solely from `i`'s declared next-cache
and live output mapping. It may reuse immutable base read wires, but it cannot
reuse mutable cache wires from any different invocation.

Optimizations at this seam:

- constant-fold the iteration index, domain labels, and dead branch work after
  inlining;
- CSE pure immutable base reads and provider KDF subgraphs with an identical
  `KeyUse`;
- DCE cache cells never read after a write and never exported;
- liveness-prune a cache copy-in cell not read by any surviving step;
- retain all distinct randomized encryptions (`CiphertextUse`) even when their
  structure matches;
- never CSE mutable cache writes across loop iterations.

Run fold → CSE → DCE after the entire unrolled loop is composed. Then recompute
the access/copy manifest from surviving exact wires before reserving final
randomness and held ranges. This prevents dead provider code from consuming
material slots or randomness provenance.

### 2.3 Phase-2 tests

- static three-step loop agrees with a scalar reference interpreter;
- a dead iteration output and its cache copy disappear after optimization;
- a loop-carried cache update is visible to the next iteration only;
- a loop with symbolic unlowered branch fails closed;
- unbounded static loop beyond former default limits compiles or fails only on
  genuine host resource exhaustion, never a semantic truncation;
- randomized encryptions remain distinct across iterations.

Ledger: `FHE-PLUMB-CIRCUIT-STORAGE-02`.

## Phase 3 — storage sharing through invocation caches

### 3.1 Sharing model

Storage sharing means **copy from a versioned readonly base into a fresh cache,
use the cache, then discard it**. It does not mean shared mutable provider
memory and does not make ciphertext cache durability implicit.

Each invocation gets:

```text
InvocationId
BaseStorageId + BaseEpoch
CacheId (unique to InvocationId)
CacheEpoch = 0
```

`CacheId` must be unforgeably unique in the compiler plan (a monotonically
allocated public identity is sufficient for current compilation). The key-use /
ciphertext-use labels include invocation/cache identity where appropriate, so
randomized events cannot accidentally CSE across invocations.

### 3.2 Cache operations

The cache has three explicit stages:

1. **Populate:** construct from the readonly base access manifest.
2. **Use:** all provider reads and writes target this cache. Writes form a
   versioned shadow map over populated cells, with last-write-wins only in the
   declared program order.
3. **Finish:** default is `Discard`. An adapter may choose `Export(write_set)`;
   export is an explicit Boolean/strict transition that produces a new
   readonly-base version, never a mutation of the old base.

The next invocation can therefore either copy from the original base (after a
Discard) or from the explicit newly exported version. It may not consume a
previous invocation’s in-memory cache object or module global.

### 3.3 Cache sharing optimizations

Within **one** invocation only:

- deduplicate initial reads of the same base cell;
- coalesce copy ranges;
- forward a cache write to later reads when no intervening write exists;
- eliminate overwritten cache writes not exported/output/live;
- represent a known-zero or constant base cache line as a constant wire so the
  combined optimizer can remove provider work.

Across invocations:

- share the immutable *descriptor/manifest*, not cache state;
- share a base material prefetch only if the same `HeldRange`, base epoch, and
  chain phase are proven compatible by the existing strict material planner;
- never share encryption randomness, ciphertext uses, mutable cache wires, or
  plaintext host values;
- any exported write increments the base version and invalidates old-base
  read/CSE keys for later invocations.

### 3.4 Registry integration

Use `volar_mpc::strict_chain::{HeldSlots, HeldRange, StorageOperation}` as the
public durable-label allocation seam:

- reserve source base material and cache result ranges in deterministic manifest
  order;
- construct `Prefetch` operations only at chain boundaries;
- keep all slot arithmetic inside `circuit_provider_storage`;
- bind the manifest’s base epoch and invocation ID to the outer session before
  transport implementation.

The registry is not a plaintext/ciphertext cache and must never be used as a
shortcut around cache discard semantics.

### 3.5 Phase-3 tests

- two invocations from one base obtain independent cache identities;
- write in invocation A followed by Discard cannot affect invocation B;
- explicit export creates base epoch `e+1`, while a stale `e` request fails;
- write forwarding and dead-overwrite elimination match scalar evaluation;
- held ranges are deterministic, non-overlapping, and prefetches occur only at
  a chain boundary;
- replay/mixed invocation identity, key use, ciphertext use, or randomness
  provenance fails before a provider circuit is built.

Ledger: `FHE-PLUMB-CIRCUIT-STORAGE-03`.

## Delivery order and commits

Each item is one reviewable commit, with tests in the same commit:

1. Add ledger rows, domain value objects, readonly layout validation, and
   storage registry allocation adapter.
2. Add readonly manifest derivation and static/public read optimizations.
3. Add source-to-cache composition and immutable-base/cache-isolation tests.
4. Add finite provider step descriptor and full-unroll preparation integration.
5. Add loop scheduler and full-loop optimizer/rematerialization pass.
6. Add cache lifecycle, discard/default behavior, and explicit export/version
   handling.
7. Add held-slot `Prefetch` integration and independent strict-chain fixture.
8. Add adversarial/replay and scalar differential tests, then update every
   ledger status with exact artifact/profile prerequisites.

No concrete FHE provider is selected in phases 1–7. The first provider adapter
requires the existing ABI rows plus all three storage rows to have their stated
independent oracle, session binding, and fixture prerequisites met.

## Deferred IDs

- `FHE-PLUMB-CIRCUIT-STORAGE-01` — readonly base layout and tailored accesses.
- `FHE-PLUMB-CIRCUIT-STORAGE-02` — finite loop step composition/scheduling.
- `FHE-PLUMB-CIRCUIT-STORAGE-03` — cache lifecycle, sharing, registry, and
  export/version semantics.
- `FHE-PLUMB-CIRCUIT-STORAGE-04` — provider-specific storage oracle,
  session/epoch binding, adversarial transport, and replay coverage.
