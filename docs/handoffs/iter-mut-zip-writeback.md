# Task: `iter_mut().zip(..)` write-back

> **Status:** scoped, unstarted. **Base commit:** `8a52981` (strict-TS count 397).
> **Estimated effort:** 1 focused change, medium risk. **Owner:** pick up when ready.

## The gap

A `deref-assign` through `iter_mut().zip(..)` is emitted as a plain
`for (const [var1, var2] of zip(...)) { var1 = ... }`, which:

1. Fails at the type level (`const` reassignment — TS2588, 24 errors)
2. At runtime, **discards the write entirely** (assigns a local `const`, never
   touching the source array).

Example source (`ring_lwe.rs`):

```rust
for ((pair, choice), selected) in self
    .pairs
    .iter()
    .zip(choices.iter().copied())
    .zip(output.iter_mut())
{
    *selected = if choice { pair.one } else { pair.zero };
}
```

Current TS:

```ts
for (const [[pair, choice], selected] of this.$fpairs.map(__a => ...)
  .zip(...).zip(...))
{
    selected = (() => { if (choice) return pair.$fone; else return pair.$fzero; })();
}
```

`selected` is a `const` loop variable; the reassignment fails and is not
reflected in `output`.

## Why this is hard

`iter_mut()` is a *mutable reference* into `output`. The same index is shared
between the two zips, and the zip is structured as `iter().zip(iter()).zip(iter_mut())`.
Only the last element (the mutable iterator) is the write-back surface, and
its index must match the outer iteration. This is an **indexed-element write-back**:

```ts
for (let __i = 0n; __i < count; __i += 1n) {
    const pair = this.$fpairs[Number(__i)];
    const choice = choices[Number(__i)];
    const val = (() => { if (choice) return pair.$fone; else return pair.$fzero })();
    output[Number(__i)] = val;
}
```

The existing `iter_mut` indexed-write-back handling (printer_ts.rs ~3231) only
fires for a **bare** `arr.iter_mut()` collection:

```rust
if matches!(method, MethodKind::Other(s) if s == "iter_mut") && args.is_empty() {
    // emit indexed loop with arr[__mut_i]
}
```

It does NOT fire when `iter_mut()` is nested inside `.zip(..)` or `.enumerate()`.

## Correct implementation sketch

Two approaches:

### Approach A — destructure the zip into an indexed loop

When the `IterLoop` collection is a chain ending in `iter_mut()` (or the
`zip` nested structure contains `iter_mut()`), fall back to an indexed guard
`for` loop instead of an `IterLoop` emission:

1. Examine the collection `IrExpr`. If it's an `IterPipeline` whose terminal
   is `Collect` and which contains a `Map`/`zip` pattern with `iter_mut()`
   somewhere in the source chain: switch to indexed loop emission.
2. Emit a counter loop using `BigInt` indexing:

```ts
const __len = BigInt(this.$fpairs.length);
for (let __i = 0n; __i < __len; __i += 1n) {
  const pair = this.$fpairs[Number(__i)];
  const choice = choices[Number(__i)];
  const selected = output[Number(__i)];
  selected = (() => { ... })();
  output[Number(__i)] = (() => { ... })();
}
```

This handles the type error (all variables are mutable `let` or direct element
references) and correctly writes back `output[Number(__i)] = ...` only for the
`iter_mut` element.

### Approach B — MutRef tracking through zip

Extend the existing MutRef mechanism (printer_ts.rs ~1356) to support
*multi-source* zip references. Currently `MutRef` tracks one `(var, array, index)`
triplet. A zip's variables need their own arrays and indices.

1. In the `IterLoop` handler, when collection is a `zip`, extract each source
   array expression and its index.
2. For sources that are `iter_mut()`, mark the corresponding loop variable as
   `MutRef` with source `array_expr[__i]`.
3. In `emit_statement_expr`, when the LHS is such a tuple var, emit
   `array_expr[Number(index)] = rhs`.

This is the more general approach but requires modifying the mut-ref
assignment recognition to look at the zip pattern.

### Preferred approach

**Approach B** is more maintainable: keep the `for..of` zip emission for
type-identity but override assignments to known `iter_mut` variables to
`array[index] =`.

The key is in printer_ts.rs ~3072, the `IrStmtKind::Let` / `Semi` emission.
When the init is a `MethodCall` whose receiver is `Var` (selected) and the
`mut_refs` list contains a `MutRef { var: "selected" }`, re-emit as
`array_expr[Number(index_var)] = rhs`.

Also need to fix `const selected` → `let selected` in the zip pattern or
handle as a dangling ref without changing the declaration.

## Verification

```
cargo build -p volar-compiler-passes --features parsing
cargo xtask gen-specs
cd packages/volar-runtime
cp ../volar-spec-ts/generated.ts src/.chk.ts && sed -i '' '/^\/\/ @ts-nocheck/d' src/.chk.ts
../../node_modules/.bin/tsc --noEmit --strict --moduleResolution bundler --target esnext --module esnext src/.chk.ts 2>&1 | grep -cE 'error TS'
```

TS2588 should drop from 24 toward 0. Runtime smoke test: the wrapping-smoke
and import-smoke tests still pass.

## Out of scope

- Generic `&mut` references (e.g. a standalone `&mut T` arg) — those are
  currently handled as value semantics in the TS backend.
- Other iterator chains with `iter_mut` in non-terminal position — only
  `.zip()` is known to be used in the wild.
