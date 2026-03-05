# TypeScript Backend Progress

> Last updated: 2026-02-06
> Strict-mode errors: **6** (down from ~131 initial)
> With `@ts-nocheck`: **0**

## Error Inventory

### TS2322 — Type Assignment (3 errors)

| Line(s) | Description | Root Cause |
|---------|-------------|------------|
| 646, 653 | `VopeDyn<unknown>` not assignable to `number` | Math-trait `Add::Output`/`Mul::Output` projection resolves to `number` after erasing the associated type, but the method actually returns `VopeDyn`. The witness system doesn't yet rewrite trait-method return types to match the struct's own type. |
| 712 | `QDyn<unknown>` not assignable to `number` | Same issue — `Add::Output` for `QDyn` resolves to `number` instead of `QDyn`. |
| 749 | `any` not assignable to `never` | An assignment target's type narrows to `never` because the array element type is inferred as `never` (empty array literal typed too narrowly). Now exposed by recursive `DefaultValue` lowering correctly producing `[]` instead of `undefined` — the TS type narrowing on `res_u[k][lane] = ...` triggers a `never` assignment. |

### TS2304 — Cannot find name `ctx`

✅ **Solved** — `compute_function_witnesses` now includes impl-level generics
when scanning method bodies, so type params declared on the struct/impl (not
just the method) are recognised as needing witnesses.  `lower_default_value`
in dyn lowering no longer emits `ctx.defaultT()` directly — it keeps
`DefaultValue { TypeParam("T") }` as a backend-agnostic IR node, and the TS
printer's witness system emits the `ctx` parameter and references.

### TS2345 — Argument Type Mismatch (2 errors)

| Line(s) | Description | Root Cause |
|---------|-------------|------------|
| 933, 936 | `number[]` not assignable to `Uint8Array` | `asRefU8()` returns `Uint8Array` in the runtime, but iterator pipeline `.map()` produces `number[]`. The `asRefU8` call should wrap in `new Uint8Array(...)` or the pipeline should produce `Uint8Array`. |

### TS2448 / TS7022 — Self-Referencing Variables

✅ **Solved** — the `deshadow` analysis pass (see below) now renames shadowed
bindings before TS emission.  `let x = f(x)` becomes `const x_1 = f(x)`.

### TS2322 — `undefined[]` not assignable to `number[][]`

✅ **Solved** — unified `ArrayDefault`/`DefaultValue` with recursive lowering
now produces `[]` (empty vec) instead of `undefined` for nested array defaults.
The parser now rejects bare `GenericArray::default()` without turbofish as a
hard error, requiring explicit type annotations in the Rust source.

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

### 2. ~~Method-level witness propagation~~ ✅ DONE

Methods now receive `ctx` just like free functions.  `compute_function_witnesses`
includes impl-level generics, and dyn lowering keeps `DefaultValue` as a
backend-agnostic IR node.

### 3. `number[]` → `Uint8Array` coercion (fixes 2 errors)

Two call sites pass `number[]` where `Uint8Array` is expected.

Options:
- Wrap pipeline results in `new Uint8Array([...])` at the call site
- Change the runtime to accept `number[] | Uint8Array`
- Emit `Uint8Array.from(...)` in the pipeline terminal

**Estimated effort:** ~1 hour.

### 4. Narrow empty-array assignment (fixes 1 error)

`res_u[k][lane] = fieldAdd(...)` where `res_u` is typed `number[][]` but
TS narrows the element to `never` due to `[]` initialization.  Fix: add
explicit type annotation or use typed initialization.

**Estimated effort:** ~30 min.

## Completed Analyses & Passes

| Pass | Module | Purpose |
|------|--------|---------|
| **Deshadow** | `deshadow.rs` | Renames `let x = f(x)` self-shadows to `const x_1 = f(x)`. Reusable `deshadow_block()` API with `pattern_names()` helper. 7 unit tests. |
| **Recursive DefaultValue** | `lowering_dyn.rs` | Unified `ArrayDefault` + `DefaultValue` into a single recursive `lower_default_value()`. Arrays expand to `IterPipeline(0..N).map(_ => default(elem))`. Primitives → `Lit(0)`, type params → `ctx.defaultT()`, vectors → `DefaultValue(Vec)` → `[]`. Hard error on `Infer` types. |
| **IR Dumper** | `dump_ir.rs` | Human-readable IR dump. `dump_module()` produces concise pseudocode for all structs, traits, impls, and functions. Used via `--dump-ir` / `--dump-ir-dyn` flags in `generate_volar_ts`. |
| **Witness analysis** | `printer_ts.rs` | Scans functions for `DefaultValue`, `LengthOf`, `TypenumUsize`, constructor/default calls on type params. Builds `ctx` parameter with runtime witnesses. Extended to recursively scan array types for nested default/length witnesses. |
| **Type-param erasure** | `printer_ts.rs` | Classifies type params as "surviving" (kept as TS generics) vs "erased" (emitted as `any`). Handles transitive Fn-bound returns. |
| **Unused type-param pruning** | `printer_ts.rs` | `struct_used_type_params()` / `function_used_type_params()` scan types for `TypeParam` references, prune unused ones from signatures. |
| **Transitive witness propagation** | `printer_ts.rs` | Fixpoint loop in `build_module_witness_map()` — if function A calls function B that needs witnesses, A inherits B's witnesses. |
| **Parameter-field conflict rename** | `lowering_dyn.rs` | When injected field bindings (`const n = this.n`) collide with parameter names, the parameter is renamed to `n_param` with full body rewrite. |

## Early Error Detection

The compiler now rejects ambiguous type-inference cases at parse time:

- **Bare `GenericArray::default()`** without turbofish → `InvalidType` error
  with message: *"Add explicit types, e.g. `GenericArray::<ElemType, LenType>::default()`."*
- **`IrType::Infer` in `DefaultValue`** at dyn-lowering time → panic with
  message explaining the need for explicit type annotations

These errors prevent silently-wrong codegen (e.g. `undefined` instead of `[]`)
and surface the fix needed in the Rust source.

## IR Dump Tool

Use `--dump-ir` and/or `--dump-ir-dyn` with the generator:

```bash
cargo run --example generate_volar_ts --features parsing -- --dump-ir --dump-ir-dyn
```

This produces:
- `ir_dump.txt` — parsed IR (pre-lowering), showing the raw AST from Rust source
- `ir_dump_dyn.txt` — dyn-lowered IR (post-lowering), showing the IR as fed to the TS printer

**TypeScript `[T; N]` representation trick:** `T[] & {length: N}` where `N extends number`.
This can express fixed-length arrays in TypeScript's type system, useful for
future IR manipulation and testing.

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
   variables represent array lengths and use TypeScript intersection types
   (`T[] & { length: N }`) for fixed-size arrays, improving type safety.

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
| Compiler lib tests | 41 (incl. 7 deshadow, 1 should_panic) |
| Parse volar-spec | 15 |
| Specialized IR | 7 |
| Traits and impls | 13 |
| Manifest | 45 |
| **Total** | **121** |
