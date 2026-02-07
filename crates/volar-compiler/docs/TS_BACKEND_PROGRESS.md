# TypeScript Backend Progress

> Last updated: 2026-02-06
> Strict-mode errors: **7** (down from ~131 initial)
> With `@ts-nocheck`: **0**

## Error Inventory

### TS2322 — Type Assignment (4 errors)

| Line(s) | Description | Root Cause |
|---------|-------------|------------|
| 646, 653 | `VopeDyn<unknown>` not assignable to `number` | Math-trait `Add::Output`/`Mul::Output` projection resolves to `number` after erasing the associated type, but the method actually returns `VopeDyn`. The witness system doesn't yet rewrite trait-method return types to match the struct's own type. |
| 712 | `QDyn<unknown>` not assignable to `number` | Same issue — `Add::Output` for `QDyn` resolves to `number` instead of `QDyn`. |
| 749 | `any` not assignable to `never` | An assignment target's type narrows to `never` because the array element type is inferred as `never` (empty array literal typed too narrowly). |
| 917 | `undefined[]` not assignable to `number[][]` | `Array(n).fill(undefined)` used to initialize a 2D array — TS infers `undefined[]` instead of `number[][]`. The IR's `ArrayDefault` should emit a properly-typed default. |

### TS2345 — Argument Type Mismatch (2 errors)

| Line(s) | Description | Root Cause |
|---------|-------------|------------|
| 933, 936 | `number[]` not assignable to `Uint8Array` | `asRefU8()` returns `Uint8Array` in the runtime, but iterator pipeline `.map()` produces `number[]`. The `asRefU8` call should wrap in `new Uint8Array(...)` or the pipeline should produce `Uint8Array`. |

### TS2448 / TS7022 — Self-Referencing Variables

✅ **Solved** — the `deshadow` analysis pass (see below) now renames shadowed
bindings before TS emission.  `let x = f(x)` becomes `const x_1 = f(x)`.

## Features Needed to Reach Zero Errors

### 1. Correct math-trait return types (fixes 3 errors)

The `Add`/`Mul` impls on `VopeDyn` and `QDyn` declare `Output = Self`, but
after type erasure the associated-type projection resolves to `number` (the
default field-element type).  The printer needs a lookup:

> *When the return type of a math-trait method resolves to a projection
> `<T as Add>::Output` and `T` is a known struct, substitute the struct type
> itself (e.g. `VopeDyn<any>`) instead of the erased base type.*

This requires extending the witness/type-resolution system with a
"self-type return" rule for math-trait impls.

**Estimated effort:** ~2 hours (extend `resolve_return_type` in printer_ts.rs
to check the impl's `self_ty` when the return is `Output`).

### 2. Typed array defaults (fixes 1 error)

`ArrayDefault { elem_ty: Some(Array(...)), len }` currently emits
`Array(len).fill(undefined)`.  For nested arrays it should emit
`Array(len).fill(null).map(() => [])` or use a typed fill.

**Estimated effort:** ~1 hour (special-case nested-array defaults in
`TsExprWriter::ArrayDefault`).

### 3. `number[]` → `Uint8Array` coercion (fixes 2 errors)

Two call sites pass `number[]` where `Uint8Array` is expected.  This is a
type-level mismatch from iterator pipelines producing `number[]` while the
runtime `asRefU8()` and `Digest.update()` expect `Uint8Array`.

Options:
- Wrap pipeline results in `new Uint8Array([...])` at the call site
- Change the runtime to accept `number[] | Uint8Array`
- Emit `Uint8Array.from(...)` in the pipeline terminal

**Estimated effort:** ~1 hour (add a coercion helper or widen the
runtime signatures).

### 4. Narrow empty-array assignment (fixes 1 error)

`res_u[k][lane] = fieldAdd(...)` where `res_u` is typed `number[][]` but
initialized with `Array(n).fill(0)` inside a map that returns `number[]`.
TS narrows the outer array element to `never` because of conflicting
inference.  Fix: add explicit type annotation on the `let res_u: number[][] = ...`
declaration.

**Estimated effort:** ~30 min (emit `: number[][]` type annotation on
specific `Let` patterns with known nested-array types).

## Completed Analyses & Passes

| Pass | Module | Purpose |
|------|--------|---------|
| **Deshadow** | `deshadow.rs` | Renames `let x = f(x)` self-shadows to `const x_1 = f(x)`. Reusable `deshadow_block()` API with `pattern_names()` helper. 7 unit tests. |
| **Witness analysis** | `printer_ts.rs` | Scans functions for `DefaultValue`, `LengthOf`, `TypenumUsize`, constructor/default calls on type params. Builds `ctx` parameter with runtime witnesses. |
| **Type-param erasure** | `printer_ts.rs` | Classifies type params as "surviving" (kept as TS generics) vs "erased" (emitted as `any`). Handles transitive Fn-bound returns. |
| **Unused type-param pruning** | `printer_ts.rs` | `struct_used_type_params()` / `function_used_type_params()` scan types for `TypeParam` references, prune unused ones from signatures. |
| **Transitive witness propagation** | `printer_ts.rs` | Fixpoint loop in `build_module_witness_map()` — if function A calls function B that needs witnesses, A inherits B's witnesses. |
| **Parameter-field conflict rename** | `lowering_dyn.rs` | When injected field bindings (`const n = this.n`) collide with parameter names, the parameter is renamed to `n_param` with full body rewrite. |

## Future Refactors

### IR-Level Improvements

1. **`IrExpr::Slice` variant** — Currently `Index { base, index: Range { .. } }`
   is special-cased in the TS printer to emit `.slice()`.  A dedicated `Slice`
   node would make this explicit and benefit both Rust and TS backends.

2. **Deshadow as IR pass, not printer pass** — Currently `deshadow_module()` is
   called inside `print_module_ts()` which clones the module.  Moving it to a
   separate IR transform pass (e.g. called from the example/CLI) would avoid
   the clone and make the deshadowed IR available for inspection/testing.

3. **Identifier validation in `ir.rs`** — Add `debug_assert!(is_valid_ident(name))`
   in `IrPattern::ident()`, `IrExpr::Var()` constructors.  Would have caught
   the `self.per_byte[(i*n)..]` string-stuffing bugs earlier.

4. **`IrExpr::TupleStructCtor`** — Distinguish tuple-struct construction
   (`Bit(x)`) from function calls at the IR level instead of pattern-matching
   on the callee name in the printer.

5. **Merge `rename_var_in_*` implementations** — `lowering_dyn.rs` and
   `deshadow.rs` both have full AST-walker rename functions.  Extract to a
   shared `ir_walk` module with visitor/transformer traits.

### TypeScript Backend Improvements

6. **Math-trait return type resolution** — The witness system should map
   `<Self as Add>::Output` → `Self` for struct impls.  This requires tracking
   which impl is being printed and checking its `trait_` and `self_ty`.

7. **`Uint8Array` awareness** — The IR doesn't distinguish `&[u8]` (→ `Uint8Array`)
   from `Vec<u8>` (→ `number[]`).  Adding an `IrType::ByteSlice` or tagging
   arrays whose element type is `u8` would let the printer emit correct types.

8. **Const-generic length propagation** — Currently lengths are either
   `number` (runtime) or erased.  A future pass could track which `number`
   variables represent array lengths and use TypeScript tuple types
   (`[number, number, ...]`) for fixed-size arrays, improving type safety.

9. **Integration test harness** — Generate TS, compile with `tsc --strict`
   (no `@ts-nocheck`), and assert zero errors.  Run as a Rust integration test
   that invokes `npx tsc` via `std::process::Command`.

10. **Dual-backend round-trip testing** — For each Rust test, generate both
    Rust (via `printer.rs`) and TS (via `printer_ts.rs`) output, compile both,
    and run with identical inputs to verify semantic equivalence.

11. **Remove `@ts-nocheck`** — Once errors 1–4 above are fixed, remove the
    `@ts-nocheck` pragma and make the strict typecheck a CI gate.

### Compiler Architecture

12. **Visitor/transformer pattern** — The IR has grown many expression
    variants.  A generic `IrVisitor` trait (with default implementations that
    recurse) would reduce boilerplate in deshadow, rename, witness scan, etc.

13. **Per-backend lowering** — Some transforms (field-element operator dispatch,
    `structuredClone` insertion, `Array.from` for ranges) are TS-specific.
    Factor these into a `ts_lowering.rs` pass that runs before printing,
    keeping `printer_ts.rs` as a pure formatter.

14. **Source-map support** — Emit TS source maps pointing back to the
    original Rust source positions.  Would require threading `Span` info
    through the IR.

## Test Summary

| Suite | Count |
|-------|-------|
| Compiler lib tests | 40 (incl. 7 deshadow) |
| Parse volar-spec | 15 |
| Specialized IR | 7 |
| Traits and impls | 13 |
| Manifest | 45 |
| **Total** | **120** |
