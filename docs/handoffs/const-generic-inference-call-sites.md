# Task: call-site const-generic inference for static method calls

> **Status:** scoped, not started. **Base commit:** `1bc0fd7` (strict-TS count 469,
> TS2554 = 91). **Owner:** pick up when ready. **Effort:** ~1 focused change
> (a param-type context threaded through the lowering) + the matching logic.
> **Risk:** medium — touches the shared lowering path used by BOTH the TS and
> Rust-dyn backends, so verify `volar-spec-dyn --features generated` error count
> (currently **1463**) does not regress, and the npm build still loads 641 exports.

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

## Implementation sketch

The machinery lives in `crates/compiler/volar-compiler-passes/src/lowering_dyn.rs`.

1. **Callee-signature registry.** Add a field to `LoweringContext`:
   ```rust
   /// Bare fn/static-method name → param types (generic placeholders like `N` intact).
   pub fn param_tys: BTreeMap<String, Vec<IrType>>,
   ```
   Built once in `new_with_deps` from `module.functions` and `module.impls[*].items`
   (`f.params.iter().map(|p| p.ty.clone())`). (This was written once already and
   reverted only because the consumer wasn't ready — it compiles fine.)

2. **Thread the caller's param-type map into expression lowering.** The call-site
   forwarding lives in `lower_expr_dyn` (~73 call sites) / `lower_block_dyn` (~7).
   The clean way is to add a small struct threaded alongside `fn_gen`:
   ```rust
   struct FnScope<'a> { gen: &'a [IrGenericParam], var_tys: BTreeMap<String, IrType> }
   ```
   where `var_tys` is seeded from `f.params` (and `let`-bindings with annotations as
   the block is lowered). To avoid editing 73 sites by hand, either:
   - (preferred) bundle `gen` + `var_tys` into one new context type and rename the
     parameter, so the mechanical change is a single type substitution; or
   - keep `lower_expr_dyn`'s signature and store the current fn's param map in a
     field the forwarding reads (least invasive, but the context is shared across
     functions, so it must be set/restored per function — error-prone).

3. **Solve the const generic at the static call.** In the call-forwarding block
   (the `IrExprKind::Call` handling that already does turbofish/static forwarding,
   ~line 2854-2930), for a 2-segment `Type::method(args)` whose head is a
   dyn-lowered generic struct and whose method has leading length params
   (`ctx.get_fn_length_params(method)`):
   - Get the callee's param types from `fn_param_tys[method]`.
   - The callee's leading length params correspond to the *first* `k` params after
     lowering, but the *source* params (in `fn_param_tys`) do NOT include them — so
     match `args[i]` against source `params[i]` for the source arity.
   - For each source param type that mentions a length generic `N` (e.g.
     `IrType::Array { len: ArrayLength::TypeParam("N"), .. }` or a struct
     `LabelPair<N>`), and each corresponding arg whose type is known (a `Var` resolved
     via `var_tys`, an array literal `[..; K]`, or a `Repeat` with a const count),
     unify to get `N = K`.
   - Emit the leading length arg as a literal `IrExpr::Lit(Usize(K))` (or the
     resolved expr) instead of a `Var`.

4. **Unification helper.** A small `fn unify_len(param_ty: &IrType, arg_ty: &IrType,
   out: &mut BTreeMap<String, usize>)` that walks both types in parallel and, where
   `param_ty` has `ArrayLength::TypeParam(N)` / `ArrayLength::Const(k)` mismatches,
   records `N -> k` when `arg_ty` carries a `Const(k)`. Recurse into `Array.elem`,
   `Vector.elem`, `Struct.type_args`, `Reference.elem`, `Tuple`. (Note: `Vec<E>` may
   parse as `Struct{Custom("Vec"), [E]}` — reuse the `array_like_elem` helper added
   in the placeholder-inference work.)

## Verification loop (same as the rest of this work)

```
cargo build -p volar-compiler-passes --features parsing
cargo xtask gen-specs
cd packages/volar-runtime
cp ../volar-spec-ts/generated.ts src/.chk.ts && sed -i '' '/^\/\/ @ts-nocheck/d' src/.chk.ts
../../node_modules/.bin/tsc --noEmit --strict --moduleResolution bundler --target esnext --module esnext src/.chk.ts 2>&1 | grep -cE 'error TS'
rm -f src/.chk.ts
```
Confirm: total drops from 469, TS2554 drops from 91, AND
`cargo build -p volar-spec-dyn --features generated` stays at 1463 errors (no
regression), `npm test --workspace @portal-solutions/volar-spec-ts` still loads 641
exports, and `cargo xtask check-specs` reports 0 stale.

Watch for: `LabelBatchDyn.new(16, pairs, offset)` should appear; the
`from_pairs`-style callers should NOT gain a spurious `n` param.

## Out of scope

- Instance-method calls (`self.m(..)` / `recv.m(..)`) — those resolve length via the
  receiver's type, a separate mechanism.
- Projection lengths (`D::OutputSize`) at call sites — already handled by the
  witness system / placeholder inference.
- The remaining TS2322/TS2345/TS2339 classes — separate root causes.
