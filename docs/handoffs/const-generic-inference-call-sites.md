# Task: call-site const-generic inference for dyn-lowered functions

> **Status:** implemented for static calls, module-qualified free calls, local
> call-result type propagation, and unique integer const-generic turbofish
> values; the remaining TS2554 cases have other causes. Historical base:
> `1bc0fd7` (469 errors, TS2554 = 91). The original current-tree progression
> reached 190 errors (TS2554 = 24). With independent Ferret source edits now
> present in the worktree, fresh-generation strict TS is 196 errors (TS2554 = 18);
> reference-field inference reduced this updated surface from 202 to 196. The
> role-separated Ferret APIs are not part of the compiler milestone.

## Problem

A static method whose const/length generic is **not present in the caller** is
called with the generic fixed to a concrete value by the argument types. The dyn
lowering erases the generic to a leading `usize` length param on the callee, but
the call site forwards nothing — producing TS2554 arity errors (and the analogous
under-application in Rust-dyn).

Canonical instance (`crates/spec/volar-spec/src/tinylabels/mod.rs`):

```rust
impl EncodedLabelBatch {
    pub fn from_pairs(pairs: &[LabelPair<16>], offset: [u8; 16]) -> Result<Self, BatchError> {
        let batch = LabelBatch::new(pairs, offset)?;   // <-- N is NOT a generic of from_pairs
        ...
    }
}

impl<'a, const N: usize> LabelBatch<'a, N> {
    pub fn new(pairs: &'a [LabelPair<N>], offset: [u8; N]) -> Result<Self, BatchError> { ... }
}
```

`LabelBatch::new` lowers to `static new(n: bigint, pairs, offset)` (the struct's
const generic `N` becomes a leading length param). The call `LabelBatch::new(pairs,
offset)` must become `LabelBatchDyn.new(16, pairs, offset)` — `N` is the **concrete
constant 16**, solved by matching the arg `offset`'s type `[u8; 16]` against the
param `offset: [u8; N]`.

This is the dominant remaining TS2554 sub-class (~most of the 91). It is **not**
runtime-param forwarding: the caller has no `n` to forward. It is **compile-time
const resolution** at the call site.

## What was already tried (and reverted)

A naive **call-graph length-need dataflow** (a function gains a length param iff a
transitive callee needs one) was prototyped and **reverted**: it mis-models this
case as param propagation (forwarding a param the caller cannot produce) and
regressed the strict count 469 → 483. Do not resurrect that approach. The correct
model is const-value resolution, not param threading.

## Implementation status

The machinery is in `crates/compiler/volar-compiler-passes/src/lowering_dyn.rs`:

- `LoweringContext` stores source signatures for unambiguous inherent static
  methods (`Type::method`) and uniquely named free functions. Conflicting
  signatures are marked ambiguous and are not guessed. Bare and module-qualified
  free-function paths are supported; module qualifiers are not emitted as values.
  Static signatures retain impl- and method-generic order, so const turbofish
  arguments can be matched to the exact length parameter. Direct integer module
  constants are used only when their names are unique.
- A function-scoped `FnScope` carries source generic parameters and parameter
  types. Sequential block lowering adds local `let` types from annotations or
  uniquely-resolved call return types; scopes do not leak bindings out of nested
  blocks.
- Argument type recovery covers variables, references/dereferences, casts,
  struct expressions/fields (including struct generic substitution through
  references), indexing, array/repeat literals, and tuples. Call return types are
  recovered only from unambiguous signatures, substituting explicit turbofish
  generic arguments; this lets later calls infer lengths through local producer
  bindings.
- Type unification matches array lengths and nested struct/tuple/vector/reference
  types. It can infer either a concrete const or forward a differently named
  in-scope length generic. A call receives injected leading length arguments only
  when the complete prefix is resolvable, avoiding partial argument shifts.
- The canonical `EncodedLabelBatch::from_pairs` call now emits
  `LabelBatchDyn.new(16n, pairs, offset)`; it does not add a spurious witness to
  `from_pairs` itself. Qualified turbofish calls such as
  `torus::torus_add::<LOG_M>` now emit the caller witness before source arguments.

The registries and scope are broader than the original static-method-only sketch:
unique free-function signatures are eligible too. Still unsupported: instance-
method return inference, call-result generic inference from argument types when
turbofish is absent, ambiguous signatures, and projection-based consts. Do not
guess in these cases.

## Verification loop (same as the rest of this work)

```
cargo build -p volar-compiler-passes --features parsing
cargo xtask gen-specs
cd packages/volar-runtime
cp ../volar-spec-ts/generated.ts src/.chk.ts && sed -i '' '/^\/\/ @ts-nocheck/d' src/.chk.ts
../../node_modules/.bin/tsc --noEmit --strict --moduleResolution bundler --target esnext --module esnext src/.chk.ts 2>&1 | grep -cE 'error TS'
rm -f src/.chk.ts
```
Verified: `cargo test -p volar-compiler-passes --features parsing --lib` passes
(61 tests, including direct const-length inference, qualified-call forwarding,
local producer propagation, reference-field inference, shadowed-name
invalidation, unique/ambiguous const resolution, and trait-path
non-misclassification regressions);
`cargo xtask gen-specs` followed by `cargo xtask check-specs` reports all four
artifacts fresh. On this worktree, generated TypeScript remains red at 196
errors (TS2554 = 18); an A/B run with only the six reference-field call witnesses
removed reports 202 errors (TS2554 = 24). The six-error delta is entirely
TS2554. Independent uncommitted Ferret source changes are preserved and excluded
from the compiler commit. `cargo check -p volar-spec-dyn --features
generated` still fails with its pre-existing 1486-error generated-Rust surface;
the Rust diagnostic count was unchanged by the forwarding work. Runtime
export-count checks have not been rerun for this implementation.

Watch for: `LabelBatchDyn.new(16, pairs, offset)` and
`BavcDyn.commit(ctx, 32, ...)` should receive their concrete witnesses; the
`from_pairs`-style callers should NOT gain a spurious `n` param.

## Out of scope

- Instance-method calls (`self.m(..)` / `recv.m(..)`) — those resolve length via the
  receiver's type, a separate mechanism.
- Projection lengths (`D::OutputSize`) at call sites — already handled by the
  witness system / placeholder inference.
- The remaining TS2322/TS2345/TS2339 classes — separate root causes.
