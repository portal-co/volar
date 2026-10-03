# Task: implement `?` (Try) desugaring for `Result<T, E>` unions

> **Status:** scoped, unstarted — partial infrastructure was prototyped but
> reverted to keep the tree clean. **Base commit:** `74dc6fc` (strict-TS count
> 397). **Estimated effort:** 1 focused change (error-base-class wiring + let-stmt
> handling), medium risk. **Owner:** pick up when ready.

## The gap

`Result<T,E>` was fixed to emit as the union `T | E` (commit `61f2162`). This
exposed that `IrExprKind::Try` (`?`) is currently a **no-op** — it emits the
inner expr directly with no early-return on error and no unwrap. So:

```rust
let ring = Ring::new(parameters)?;
```

emits as `let ring = Ring.new(parameters)` where `Ring.new` returns
`Result<Ring, Error>` (i.e. `Ring | Error`). This means `ring` is typed as the
union, `.uniform(...)` later fails (TS2339), and (worse) runtime errors are
silently dropped rather than propagated.

## Why this is hard

`?` needs to **short-circuit return** from the enclosing function. In JS/TS
that's `return value` from the *statement* level; expressions cannot do it.
So `let x = expr?` must desugar to something like:

```ts
const __tmp = expr;
if (__tmp instanceof __VolarError) return __tmp;
const x = <unwrapped-type>__tmp;
```

The `instanceof` check needs to know whether the value is an error. Since error
variants are emitted as separate classes:

```ts
export class Error_LengthMismatch { ... }
export type Error = Error_LengthMismatch | Error_Arithmetic | ...
```

they share no common base class. So `instanceof` must target `__VolarError`,
a new base class they would extend.

## What was prototyped and reverted

Added `collect_error_enums()` (scans all function/method `Result` return types,
pulls out the `E` enum name) and wired a new `error_enums` field through
`TsContext`. The field was populated at 4+ context construction sites in the
module printers. Build broke on a `volar_compiler` crate name in the helper
(corrected to `crate::ir::StructKind`) and missing `error_enums` at 7+ builder
sites. Instead of patching all builders while tools are flaky, the prototype
was reverted to keep the tree clean.

## Correct implementation sketch

### 1. Add `__VolarError` to the preamble

In `TsPreambleWriter::ts_fmt` (near where `__StubDigest` etc. are emitted):

```rust
writeln!(f, "export abstract class __VolarError {{}}")?;
```

### 2. Extend error-enum variant classes from `__VolarError`

In `ts_write_enum`, when the enum name appears in the `error_enums` set,
emit variants with `extends __VolarError`:

```ts
export class Error_LengthMismatch extends __VolarError {
  __zero(): this { ... }
}
```

Non-error enums stay unchanged.

### 3. Build `error_enums` and pass it through context

1. Write `collect_error_enums(module: &IrModule) -> HashSet<String>` (the
   version already prototyped in `printer_ts.rs`). It walks all function
   `return_type` nodes, finds `Struct{Custom("Result"), [.., error_type]}` and
   records `error_type.kind.to_string()` (the enum name).
2. Thread `&error_enums` into every `TsContext` construction site (4 call
   sites in the top-level `print_module_ts*` and `print_cfg_module_ts`).

### 4. Emit `?` as statement-level desugar

`?` only appears inside `IrStmtKind::Let { init: Some(Try(expr)), .. }` or
`IrStmtKind::Semi(Try(expr))`. There are **74** `?` usages in the generated
module.

**Strategy — inline statement substitution** (preferred):

In `TsBlockWriter::ts_fmt` (statement iteration), when the init is a `Try`:

```rust
if matches!(&init.kind, IrExprKind::Try(_)) {
  // Emit: const __tmp = <expr>; if (__tmp instanceof __VolarError) return __tmp; let pat = __tmp;
}
```

Similarly for `Semi(Try(expr))`:
```ts
const __tmp = <expr>;
if (__tmp instanceof __VolarError) return __tmp;
```

This is the **_STMT_** level not the **EXPR** level, because `return` must
break out of the function.

### 5. Challenge: nested `?` and shadowing

Multiple `?` in the same block need unique temp names (`__tmp`, `__tmp2`).
The block writer already handles indent; use `let mut try_count = 0` in the
block writer and format names as `__try{count}`.

### 6. Type narrowing

After `if (__tmp instanceof __VolarError) return __tmp`, TypeScript's control
flow narrows `__tmp` to `T` on the `false` branch. So the `let pat = __tmp`
line correctly infers `T` (stripping the `| __VolarError`). This eliminates the
TS2339 `.uniform` errors.

## Verification

```
cargo build -p volar-compiler-passes --features parsing
cargo xtask gen-specs
cd packages/volar-runtime
cp ../volar-spec-ts/generated.ts src/.chk.ts && sed -i '' '/^\/\/ @ts-nocheck/d' src/.chk.ts
../../node_modules/.bin/tsc --noEmit --strict --moduleResolution bundler --target esnext --module esnext src/.chk.ts 2>&1 | grep -cE 'error TS'
```

Expected: 397 -> lower. Key: TS2339 `Property does not exist on type Result<..>`
should drop. Runtime: the wrapping-smoke
and import-smoke tests should still pass.

## Out of scope

- Non-Result enums already have no `?` usage.
- `?` on `Option` doesn't exist in this codebase.
- The `iter_mut().zip()` write-back gap — tracked separately.
