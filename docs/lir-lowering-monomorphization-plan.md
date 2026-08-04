# Plan: Lowering-Time Monomorphization in `volar-lir-codegen`

**Status:** Phases 2–5 landed for small woven/flat modules (2026-08-04).
`encrypt_branch` / unbound `L` is a planner rooting issue (do not root
orphan generics); plan-API + instance-keyed nominals + restricted call-site
inference are in `volar-lir-codegen`. Evidence: `lir_backend`,
`monomorphization`, `vole_e2e` (including record→C+WASM AND).
**Implementation scope:** centered on `crates/compiler/volar-lir-codegen/`, with
callers migrated to `lower_module_monomorphized` / `MonoPlan` roots.

## Merged-tree update — 2026-07-22

Evidence: `08d1d33` and the [static-shapes/monomorphization handoff](handoffs/merge-recovery/static-shapes-and-monomorphization.md).

The current widening reproducer, `cargo test -p volar-weaver -p
volar-lir-codegen -p volar-c-backend`, reaches C-backend lowering of
`encrypt_branch` with unresolved const parameter `L`. This is a source
reconciliation failure, distinct from the missing-LLVM environment blocker.
Do not choose a test-only `L` or limit diagnosis to C tests: trace roots,
callee bindings, nominal layouts, `MonoEnv` callers, and CFG/auxiliary paths;
then add a C compile-and-run regression for more than one specialization.

## Goal

Move monomorphization from a module-wide precondition to an on-demand part of
LIR lowering. A single source `IrFunction` must be lowerable as multiple,
distinct concrete LIR functions, each with its own type/length substitution.

For example, if the source contains:

```rust
fn map<N: ArraySize>(x: [u8; N]) -> [u8; N] { ... }

fn entry() {
    let a = map::<U16>(...);
    let b = map::<U32>(...);
}
```

then codegen emits two concrete functions (with stable distinct names and
layouts), rather than attempting to apply one global `MonoEnv` to both calls.
The same rule applies when two different functions use independently named
generics (`outer<A>` calling `inner<B>`), and to concrete generic structs,
tuples, enums, and return types reachable from each function instance.

This plan is deliberately **not** a parser, `volar-compiler` IR, dynamic
lowering, printer, weaver, or backend refactor. It consumes the generic
information already preserved in `IrFunction.generics`, `IrType`,
`ArrayLength`, and `IrExprKind::Path { type_args, .. }`.

## Current state and failure modes

`volar-lir-codegen` currently threads a single `&MonoEnv` through an entire
lowering invocation:

- `lower_module_with_opts` builds one `StructRegistry`, function-signature
  table, and external-function table using that environment, then lowers every
  normal function with it.
- `lower_cfg_module_with_opts` has the same model for flat auxiliary and CFG
  functions.
- `LowerCtx`, `StructRegistry::ir_type_to_lir`, tuple registration, array
  length resolution, and crypto-method suffix generation all read that shared
  environment.
- `lower_call` identifies a callee only by its source name and emits that same
  name through `LirTarget::call_extern`; it does not select or name a concrete
  callee instance.
- `StructRegistry` and `EnumRegistry` key layouts by source `StructKind` /
  enum name. Therefore `Wrap<U8>` and `Wrap<U64>` cannot coexist safely in one
  target even if call lowering were made per-function.

`mono.rs` already contains whole-module cloning helpers, but the active path
uses substitution lazily instead. Applying either approach once to the whole
module necessarily makes a name such as `N` mean one value everywhere. It
also leaves duplicate function and type names if two specializations are
introduced manually.

## Non-goals and boundaries

- Do **not** add a new IR variant or change the parser to carry a second form
  of turbofish. Existing path `type_args` are the explicit-specialization
  source.
- Do **not** change `LirTarget`, `volar-c-backend`, `volar-compiler`,
  `volar-weaver`, `volar-ir-lir-target`, or any spec crate as part of this
  work. The codegen crate will still express both local and opaque calls with
  the existing `call_extern` builder operation.
- Do **not** silently choose an arbitrary instantiation when type arguments
  cannot be resolved. Unresolved or ambiguous generic calls are codegen
  errors with source/callee context.
- Do **not** promise full Rust trait selection, overload resolution, or
  generic method monomorphization. This plan covers direct local free-function
  calls represented by `IrExprKind::Call` with a `Path` or `Var` callee.
  Existing `MethodCall` behavior remains external dispatch, including the
  existing `hash_suffix` mechanism.
- Dynamic lowering remains responsible for runtime length-argument
  propagation. This is a different path: LIR lowering requires static layouts
  and therefore resolves lengths to constants.

## Design invariants

1. **An emitted function is identified by source definition plus a fully
   resolved substitution**, not by source name alone.
2. **All LIR layouts are concrete before emission.** No `TypeParam`, unresolved
   `ArrayLength::TypeParam`, or unresolved projection can reach
   `ir_type_to_lir` / `const_len`.
3. **One source generic function may have arbitrarily many instances in one
   target.** Repeated requests for the same instance deduplicate.
4. **Concrete nominal types are also instance-keyed.** Two source types with
   different substituted arguments never share a `StructId`, enum layout, or
   emitted C-facing type name merely because their source `StructKind` matches.
5. **Names are deterministic and collision-resistant.** Naming depends only on
   the source definition identity and a canonical concrete specialization key,
   never discovery order or pointer identity.
6. **All tables agree.** A call target, its LIR signature, its IR-level return
   type, and its emitted definition must use the same `FunctionInstanceKey`.
7. **Flat and CFG lowering use exactly the same planning and specialization
   machinery.** CFG auxiliary functions are not a second monomorphization
   implementation.

## Proposed internal model

### 1. Make substitutions owned, canonical, and per instance

Retain `MonoEnv` as the public spelling for a concrete substitution if useful,
but make it owned/cloneable, comparable, and canonical rather than borrowing a
single environment from an enclosing lowering call. Its contents remain the
existing concrete length, type, projection, and dispatch-suffix bindings.

Introduce internal keys roughly equivalent to:

```rust
struct FunctionInstanceKey {
    definition: FunctionDefKey, // module path + source function name
    subst: CanonicalMonoEnv,
}

struct NominalTypeInstanceKey {
    definition: NominalDefKey,  // struct/enum source identity
    type_args: Vec<ConcreteTypeKey>,
}
```

`CanonicalMonoEnv` must encode every resolved binding that can affect layout or
emitted external dispatch: length bindings, type bindings, projections, and
`hash_suffix` (or its successor dispatch-name binding). `BTreeMap` ordering is
suitable for deterministic serialization after values have been recursively
normalized through the caller's environment.

A human-readable stable mangler should emit source names unchanged for
non-generic definitions where possible and append an encoded canonical
specialization for generic definitions, for example:

```text
map__N_16
map__N_32
Wrap__T_U8
Wrap__T_U64
```

The real encoding must escape identifiers and include enough structural detail
(type arguments, arrays, tuples, projections, and module path) to avoid
collisions. A short stable digest may follow a readable prefix; it must be a
deterministic digest of the canonical key, not a process-random hash.

### 2. Resolve a callee specialization at every local call

Build a definition index once per input module, keyed by the existing local
function-name convention. At every direct call, resolve a `FunctionInstanceKey`
in this order:

1. Resolve the callee source definition.
2. Normalize explicit `Path.type_args` through the *caller's* substitution.
3. Bind those arguments positionally to the callee's declared generic
   parameters, validating kind and arity.
4. For generic parameters omitted by the explicit path, infer bindings by
   unifying the callee parameter types with the already-specialized argument
   types at the call site. The initial unifier needs only the IR forms LIR
   codegen actually accepts: type parameters, arrays/lengths, references,
   tuples, structs with type arguments, and projections.
5. Merge explicit and inferred bindings; reject contradictory bindings.
6. Inherit the caller's dispatch suffix only when the resolved callee binding
   denotes the same substituted dispatch parameter. A callee that introduces a
   distinct dispatch type must receive an explicit root/configured dispatch
   binding rather than accidentally reusing an unrelated suffix.
7. Require every layout-relevant generic to be concrete. Produce a structured
   diagnostic naming the call, callee, parameter, and unresolved IR type or
   length otherwise.

This intentionally gives explicit turbofish precedence, while allowing the
common `f<N>(x: [T; N]) -> ...; g(x)` case to work when its types determine
`N`. It stays local to codegen and does not claim to solve compiler-wide type
inference.

Calls that do not resolve to a local source function retain current
`call_extern` behavior and their original name. Calls resolved locally use the
callee instance's mangled name and precomputed concrete signature.

### 3. Plan the closed set of instances before emitting LIR

Replace “lower every source function once” with a two-stage lowering plan:

1. **Seed selection.** The new public API receives concrete root requests.
   A root is `(source function, MonoEnv)`. Convenience constructors may seed
   every non-generic normal function with an empty environment; generic roots
   must be supplied explicitly.
2. **Discovery.** Traverse direct local calls starting from roots. For each
   newly discovered `FunctionInstanceKey`, resolve its body under that
   instance's environment, add all local callee instances to a work queue, and
   recursively collect every concrete nominal type used by its signature,
   block annotations, and expressions that need a layout.
3. **Registration/metadata.** Once discovery reaches a fixed point, register
   all required concrete struct, enum, and synthetic tuple layouts in stable
   key order. Build signatures and IR return-type tables keyed by function
   instance, not source name.
4. **Emission.** Lower each discovered normal function instance exactly once,
   using the owning instance environment and the immutable planned tables.
   Calls consult the plan rather than re-resolving names ad hoc.

Discovery must track an `in_progress` set separately from `completed` so
self-recursive or mutually recursive generic instances terminate. Recursive
calls target the already-reserved mangled instance name. If recursion creates
an infinite family (`f<N>` calling `f<N+1>`), fail with a clear expansion-limit
/ non-finite-specialization error rather than looping or exhausting memory.

### 4. Specialize nominal layouts, not just function signatures

Refactor `StructRegistry` and `EnumRegistry` around concrete nominal instance
keys. The registry-building pass must:

- bind a source struct/enum's own declared generic parameters to the type
  arguments at each use site;
- substitute field/payload types with that type-local environment;
- recursively register dependencies before their users;
- give every registered definition a mangled concrete name; and
- preserve the existing `@volar-native` mapping and lenient opaque behavior
  without conflating different concrete instances.

Synthetic tuple names must derive from canonical *concrete* LIR type keys,
not allocation-order `StructId`s. Arrays and references remain structural
LIR types, but their nested nominal elements must resolve through the instance
registry.

This removes the current incorrect assumption that `StructKind` alone is a
layout identity. It also keeps C type definitions valid because registration
is complete before any function body uses a concrete `StructId`.

### 5. Use one generic planner for flat and CFG modules

Factor shared planning into `mono` (or a new private `instances` module):

- a module-neutral function-definition view for `IrFunction` and
  `IrCfgFunction`;
- shared call collection and call-instance resolution;
- instance-keyed metadata, mangling, and type collection; and
- separate emitters for flat bodies and CFG bodies.

For `IrCfgModule`, discovery spans both `IrAnyFunction::Cfg` and
`IrAnyFunction::Flat`. `include_auxiliary = false` remains an emission policy:
metadata for needed flat instances is still planned so CFG calls receive the
right concrete name/signature, but their bodies are omitted and therefore
remain target-level extern references.

## Public API migration

Replace the API that accepts one environment for a whole module:

```rust
lower_module_with_opts(module, target, &MonoEnv)
lower_cfg_module_with_opts(module, target, &MonoEnv, include_auxiliary)
lower_module_seeded(module, target, &MonoEnv, seeds)
```

with a plan/request API, conceptually:

```rust
struct MonoRoot {
    function: String,
    env: MonoEnv,
}

struct MonoPlanOptions {
    roots: Vec<MonoRoot>,
    include_auxiliary: bool,
    // bounded expansion / diagnostics configuration
}

fn lower_module_monomorphized(module, target, options) -> Result<(), MonoError>;
fn lower_cfg_module_monomorphized(module, target, options) -> Result<(), MonoError>;
```

Exact names may differ, but the API must make per-root specialization explicit
and return a typed `MonoError` instead of relying on downstream “add it to
MonoEnv” panics.

As breaking changes are allowed, remove the single-`MonoEnv` module APIs once
all in-crate code paths have migrated. Keep `lower_function` only as a clearly
documented primitive for already-concrete, standalone functions, or make it
private if retaining it would invite bypassing the planner. `MonoEnv` itself
can remain a useful root-construction type (`with_len`, `with_type`,
`with_projection`).

## Implementation phases

### Phase 1 — Establish concrete-instance primitives

Files: `mono.rs`, `structs.rs`, `lib.rs` in `volar-lir-codegen`.

1. Define canonical concrete type/length/substitution representations,
   `FunctionInstanceKey`, nominal-type keys, and deterministic mangling.
2. Make `MonoEnv` cloneable and add normalization/validation helpers. Ensure
   substitution failures return `MonoError` at planner boundaries.
3. Unit-test canonical equality and mangling: reordered map insertion produces
   the same key/name; different type, length, projection, module path, or
   dispatch suffix produces a different key/name.
4. Do not alter entry points yet; retain the old lowering behavior while these
   helpers are introduced.

### Phase 2 — Instance-aware type registry

Files: `structs.rs`, `mono.rs`.

1. Change registry keys from bare kind/name to concrete nominal instance keys.
2. Add a pre-registration collector over all planned concrete types, including
   nested struct fields, enum payloads, tuple elements, arrays, and references.
3. Substitute a nominal definition's own generic bindings when computing its
   fields/payloads.
4. Register in dependency-safe stable order and retain the existing native and
   lenient cases.
5. Update field lookup and type conversion to use the fully specialized
   `IrType`, including type arguments, rather than discarding them.

### Phase 3 — Flat function discovery and emission

Files: `lib.rs`, `mono.rs` (or new private `instances.rs`).

1. Index source function definitions and implement direct-call collection.
2. Implement explicit-argument binding plus the restricted local unifier
   described above.
3. Build a fixed-point instance graph from `MonoRoot`s; reserve keys before
   traversing recursive edges; collect types at the same time.
4. Build function signatures, IR return types, and external-function metadata
   keyed by `FunctionInstanceKey` / emitted name.
5. Change `lower_call` to ask the plan for local call targets and emit the
   planned mangled name. Keep opaque/external calls unchanged.
6. Change `LowerCtx` to carry the current instance environment and immutable
   plan lookup, not a module-wide environment.
7. Replace `lower_module_with_opts` and `lower_module_seeded` with the
   root/plan API.

### Phase 4 — Bring CFG lowering onto the same plan

Files: `lib.rs` only, using the shared planner from Phase 3.

1. Add CFG and flat entries from `IrCfgModule` to the one definition index.
2. Discover CFG terminator expressions and block statements under their
   function-instance environment.
3. Emit CFG instances with planned names/signatures and the instance-aware
   registry.
4. Preserve `include_auxiliary` as an emission filter without dropping the
   metadata needed by a CFG call to an omitted auxiliary function.
5. Replace `lower_cfg_module_with_opts` with the plan API and delete duplicate
   single-environment table construction.

### Phase 5 — Remove obsolete whole-module monomorphization paths

Files: `mono.rs`, `lib.rs`, `structs.rs`.

1. Delete or make private `monomorphize_module` / `monomorphize_cfg_module`
   helpers that imply one environment applies to all definitions.
2. Remove old global-name signature tables and bare-kind registry assumptions.
3. Refresh module docs and rustdoc examples so they describe root instances,
   the discovery step, concrete naming, and the remaining direct-call scope.
4. Run `cargo fmt`, crate tests, and focused downstream compile checks.

## Test plan

All behavioral tests should lower to the real C backend, compile with `cc`,
and execute the result; do not assert only on generated names or IR shape.
Because the implementation remains inside codegen, place new integration-like
tests in `volar-lir-codegen` and add only the necessary **dev** dependencies
there (or use an existing no-cycle test harness if one is available). Existing C-backend tests are active consumers of the current failure and may
need a regression once the instance-planning diagnosis is known.

1. **One generic function, two lengths:** a concrete root calls one generic
   array function at two different lengths. Compile and run a C `main` that
   checks both results.
2. **One generic function, two element types:** specialize a type-generic
   function for compatible concrete primitive types and verify both results.
3. **Nested forwarding with different parameter names:** `outer<A>` calls
   `inner<B>` through an explicit and an inferred specialization. Verify the
   correct values at two independently chosen lengths.
4. **Generic nominal type layouts:** use `Wrap<N>` (and, if supported by the
   test subset, a generic enum) at two lengths in the same module. Execute
   accesses to each layout so a mistaken shared `StructId` cannot pass.
5. **Deduplication:** two calls requesting the same specialization must link
   and run as one concrete definition; inspect only a hard invariant exposed by
   a test target if needed, not incidental statement counts.
6. **CFG parity:** build a small `IrCfgModule` whose CFG function calls an
   auxiliary generic function at two instantiations, lower it through the C
   backend, compile, and run.
7. **Diagnostics:** unresolved generic arguments, contradictory explicit vs
   inferred bindings, and an intentionally non-finite recursive
   specialization return deterministic `MonoError`s. These are appropriate
   targeted error assertions, not generated-IR structural tests.
8. **Regression:** run the existing `volar-lir-codegen` tests and relevant
   `volar-c-backend` C compile/run tests. Migrate their former uniform
   `MonoEnv` call sites only after the new API is stable; this migration is a
   separate consumer update and outside the implementation scope of this
   plan.

## Acceptance criteria

- No module-level lowering implementation accepts or shares one `MonoEnv` as
  the specialization for every emitted function.
- A generic local function used at two distinct concrete specializations
  lowers in one target invocation without duplicate-name or layout collisions.
- Concrete struct/enum/tuple layouts used by those functions coexist with
  distinct stable identities.
- Direct local calls resolve to the matching planned concrete callee name and
  concrete signature; non-local calls preserve existing external behavior.
- Flat and CFG paths share specialization resolution and pass compile-and-run
  coverage.
- Failure to determine a static layout is a contextual `MonoError`, not an
  accidental unresolved-type panic.
- The primary implementation may be in `volar-lir-codegen`, but caller and
  target-facing regressions must be reconciled across compiler-IR, backend, weaver, and
  pipeline semantic change is required.
