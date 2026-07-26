# Noir Backend

`volar-compiler-noir-codegen` lowers Volar's `IrModule` AST directly into
[Noir](https://noir-lang.org) source (Aztec's zero-knowledge DSL). It targets
`nargo`/`bb` (verified against 1.0.0-beta.24 / bb 5.1.0 while this backend was
built).

## Architecture: AST layer, not LIR

Unlike `volar-c-backend`/`volar-llvm-backend`, which implement the
`LirTarget` trait and consume LIR produced by `volar-lir-codegen`, this
backend hooks in at the `IrModule` layer — architecturally parallel to the
`print_module_typescript`/`print_module_rust_dyn` printers, not the LIR-based
backends. Two reasons:

- **Generics.** `volar-lir-codegen`'s lowering fully monomorphizes generics
  into duplicated concrete functions. Noir has native generics, including
  numeric/const generics — exactly the tool for parameterizing over array
  lengths without duplication. Monomorphizing that away would produce needless
  code bloat.
- **Control flow.** LIR converts structured control flow into a CFG (basic
  blocks, `jump`/`branch`, block parameters), which would need a
  control-flow-*reconstruction* pass to become valid Noir `if`/`for` syntax
  again. The `IrModule` AST is already structured — no reconstruction needed.

Crate layout: `error.rs` (`NoirCodegenError`), `const_eval.rs` (Noir
compile-time-constant analysis), `lowering_noir.rs` (pre-print validation:
loop-bound legality, `WhileLoop` rejection), `printer_noir.rs` (the recursive-
match text emitter — hand-written, like `printer.rs`/`printer_ts.rs`; no
shared `IrVisitor` trait exists anywhere in this codebase). Depends only on
`volar-compiler` + `volar-compiler-passes` (for `const_analysis`) — zero LIR
dependency, matching `xtask`/`volar-compiler-passes`'s own footprint.

## Type mapping

| `IrType` | Noir | Notes |
|---|---|---|
| `Bool` | `bool` | |
| `U8`/`U32`/`U64` | `u8`/`u32`/`u64` | |
| `Usize` | `u64` | Noir has no `usize`; verification item — confirm the idiomatic width (`u32` for index/length position vs `u64` elsewhere) against real usage patterns |
| `U128` | `u128` | |
| `I128` | unsupported | Noir has no 128-bit signed integer |
| `Bit`/`Galois`/`Galois64`/`Galois128`/`Galois256`/`BitsInBytes`/`BitsInBytes64`/`Z3` | bare name (`Galois`, etc.) | GF(2^k)/GF(3) fallback via `volar-primitives` — see below. The parser tags *references* to these types (an impl's `self_ty`, a method's param/return type) as `IrType::Primitive(PrimitiveType::Galois)`, distinct from `IrType::Struct(Custom("Galois"))` used for the struct's own *declaration* — both map to the same Noir identifier so emitted source stays internally consistent |
| `Array{elem,len}` | `[elem; len]` | `len` via `const_eval::eval_array_length` (`Const`→literal, `TypeNum`→resolved literal, `TypeParam`→bare generic name, `Projection`→unsupported) |
| `Vector` | unsupported | no compile-time length; use `[T; N]` |
| `Struct{kind: GenericArray}` | unsupported | `type_args` shape not yet empirically verified against a real fixture |
| `Struct{kind: Custom(name)}` | `name` or `name<args>` | |
| `TypeParam(name)` | bare `name` | assumes it was accepted into the enclosing `print_generics` list; not independently re-validated here |
| `Param{..}` | unsupported | not observed reaching this layer yet in practice |
| `Tuple`/`Unit` | `(T1, T2, ...)` / `()` | |
| `Reference{mutable: false, elem}` | transparently unwrapped to `elem` | in **parameter position only** — safe for a pure function reading through `&T`. In **return-type or struct-field position**, rejected with `EscapingReference` instead (silently turning that into an owned-value return/field would be a real semantic change, not cosmetic) |
| `Reference{mutable: true, ..}` | unsupported everywhere | deferred, not v1 |
| `Projection{base: TypeParam("Self"), assoc: Output}` | resolves to `Self` | narrow rule for the `impl Add for X { type Output = X; fn add(...) -> Self::Output }` pattern real Rust source produces — Noir's own arithmetic traits have no `Output` associated type at all (`fn add(self, other: Self) -> Self`) |
| `Projection` (any other shape) | unsupported | no general associated-type resolution |
| `Existential`/`FnPtr` | unsupported | no `impl Trait`/function-pointer values in constrained Noir |
| `Never` | `!` | |
| `Infer` | unsupported | should never reach codegen — incomplete inference upstream |

**Noir has no tuple-struct syntax at all** (confirmed via `nargo check`:
`Expected a '{' but found '('`) — real, not hypothetical, since
`volar-primitives`'s `Bit`/`Galois`/etc. are all tuple structs. Handled by
lowering rather than rejecting: `print_struct` synthesizes named fields
`_0`, `_1`, ... for a tuple struct's positional fields, and `print_expr`
correspondingly rewrites both directions —

- **Construction**: `Galois(x)` → `Galois { _0: x }`. Detected purely by the
  callee being a bare name matching a known tuple-struct name
  (`PrintCtx::tuple_structs`, built once per module) — no type inference
  needed, since a call whose callee name literally *is* a tuple-struct name
  can't be anything else.
- **Field access**: `g.0` → `g._0`, but *only* when `g`'s type is statically
  known to be a tuple struct (`infer_simple_type`) — otherwise left as plain
  `.0`, the correct choice for a real `IrType::Tuple` value (Noir supports
  `.0`/`.1` natively there). `infer_simple_type` covers: a parameter's or
  `self`'s declared type (always known); a `let`-bound local's type, either
  its explicit annotation or inferred from its initializer when the
  initializer is itself a tuple-struct constructor call or a call to one of
  the curated `MathTrait` operator-overload methods (`g1.mul(g2)` has the
  same type as `g1`, since every v1-supported operator impl matches Noir's
  own `Self`-returning `std::ops` signatures). A `let`-bound local with
  neither an annotation nor an inferable initializer defaults to plain `.0`
  access — a known, narrow limitation (never a wrong *value*, only a
  possible wrong field-name choice for an actual tuple-struct field access
  the printer couldn't trace), not silently wrong.

Proven end-to-end against the real `volar-primitives` source (not a
paraphrase): `crates/compiler/volar-compiler-noir-codegen/tests/nargo_smoke.rs`'s
`volar_primitives_galois_multiply_round_trips_through_nargo` copies `Galois`,
`gf_mul_u8`, and the `GF8_POLY` constant verbatim from
`crates/spec/volar-primitives/src/lib.rs`, and validates the GF(2^8) product
via a real `nargo execute` against an independently-computed expected result.

## Generics / length-param strategy

Reuses `volar_compiler_passes::const_analysis::classify_generic_with_aliases`
unchanged to classify each `IrGenericParam` as `GenericKind::Length` or
`GenericKind::Type`. Where `lowering_dyn` (used by the Rust-dyn/TS printers)
converts every `Length`-kind generic into a runtime `usize` parameter — those
targets have no const generics — this backend takes the opposite branch:
`Length`-kind params are *retained* as real generic parameters and printed as
Noir numeric generics.

Confirmed exact syntax against current Noir docs (not guessed): the `let`
keyword is **mandatory** in numeric-generic position —

```noir
fn foo<let N: u32>(x: [Field; N]) -> Field { ... }
```

not `fn foo<N: u32>(...)`. Default type when unspecified: `u32`. Bare
`Type`-kind generics with no bounds print as ordinary Noir generics; bounded
ones are rejected in v1 (trait-bound translation beyond the narrow
operator-overload set below is out of scope).

A generic numeric parameter never needs turbofish call-site syntax in
practice: Noir infers `N` from an argument's array-length type at the call
site (`sum_array(arr)` where `arr: [u32; 4]` infers `N = 4`). This is how the
milestone-4/5 integration tests avoid turbofish entirely — a fixture with a
numeric generic used only as a bare value (never tied to a parameter's type)
would need it, but that's a contrived shape real spec code doesn't produce.

## Loop-bound policy

`const_eval::eval_const_expr` (in `const_eval.rs`) is a from-scratch analysis
— **not** modeled after `volar-lir-codegen`'s `concrete_usize_expr`, which
requires a value to fully resolve to a literal post-monomorphization. Noir
loop bounds are allowed to be *symbolic* generic-const expressions (a numeric
generic `N`, or `N + 1`); the printer needs the unresolved symbolic form
preserved as printable source text.

`lowering_noir::validate_module` runs before printing (never inside it) and
collects *all* violations across a module, not just the first:

- Any `WhileLoop` → `UnsupportedWhileLoop`, unconditionally. Noir constrained
  code has no runtime-conditional loop primitive at all — not a v1
  limitation, a fundamental one.
- Any `BoundedLoop`/`IterLoop` whose bound doesn't resolve via `eval_const_expr`
  → `NonConstantLoopBound`. This is the "runtime-but-fixed bound" gap named in
  the original task: Volar's AST permits a loop bound that's only known at
  witness/runtime time; Noir's compiler must know the trip count before any
  witness data exists.
- `IterLoop` over a `Vector` (no compile-time length) or a collection whose
  length can't be read off a literal expression (this layer has no
  type-checked IR to consult) → the same.

Accepted loops print as real Noir `for` loops (`for i in start..end { .. }` /
`..=` for inclusive) — both literal and generic-const bounds, once generics
support (above) allows the enclosing function to have generics at all.

Never a panic, never silently-wrong output: rejected modules return
`Err(Vec<NoirCodegenError>)` and no `.nr` file is ever written for them.

## Operator-overload impls / GF(2^k) fallback

`print_impl` handles both inherent impls (`impl Galois { .. }`) and trait
impls restricted to the `MathTrait` operator-overload set (`Add`/`Sub`/`Mul`/
`Div`/`BitAnd`/`BitOr`/`BitXor`/`Shl`/`Shr`/`Not`/`Neg`) — confirmed against
Noir's actual stdlib trait definitions
(`noir_stdlib/src/ops/{arith,bit}.nr`): same trait names and method names as
Rust's `std::ops` (`fn add(self, other: Self) -> Self`), so `IrFunction.name`
already carries the right Noir method name straight from the source — no
remapping table needed.

This is the mechanism the `volar-primitives` GF(2^k)/GF(3) software-fallback
strategy depends on: `crates/spec/volar-primitives/src/lib.rs` is deliberately
written in Volar's compiler-parseable subset specifically so backends can
parse it as ordinary source and get correct field arithmetic "for free" — no
bespoke field-arithmetic codegen. The TS backend already proves the mechanism
works generally (parses the same source, generic struct/impl printer handles
it with zero field-specific special-casing). Proven for Noir too, now
end-to-end against the *real* source, not a stand-in — see the tuple-struct
section above (`volar_primitives_galois_multiply_round_trips_through_nargo`
copies `Galois`/`gf_mul_u8`/`GF8_POLY` verbatim and validates the GF(2^8)
product via `nargo execute`). Real gaps found only by actually parsing
`volar-primitives` and testing against `nargo`, not guessed — all fixed:

1. Unlike Rust, Noir does not put `Add`/`Sub`/`Mul`/etc. — nor
   `WrappingAdd`/`WrappingSub` — in scope for `impl` purposes without an
   explicit `use` (`error: Trait Add not found` otherwise).
   `print_module_noir` auto-injects `use std::ops::{...}` for exactly the
   trait/method names actually used, collected by an **exhaustive** walk
   over every `IrExprKind` variant (mirroring `lowering_noir::validate_expr`'s
   coverage) — an earlier, narrower walker missed calls nested inside a
   struct-literal field, array element, match arm, etc., silently dropping
   the needed `use` even though the call site itself printed correctly
   (`nested_std_method_call_import_is_collected_through_nargo` is the
   regression test for this).
2. Tuple structs — `Bit`/`Galois`/`Galois64`/`Galois128`/`Galois256`/
   `BitsInBytes`/`BitsInBytes64`/`Z3` are all tuple structs, and Noir has no
   tuple-struct syntax at all. See the dedicated section above for the fix
   (synthesized `_0`/`_1`/... fields, construction/access rewriting).
3. `type Output = Self;` plus a method written as `-> Self::Output` (what
   real `core::ops`-derived Rust source actually writes, not `-> Self`
   directly) — the associated-type item itself is consumed for resolution
   (not printed; Noir's operator traits have no such item) and the
   `Self::Output` projection substituted with the resolved concrete type
   before the return type reaches `type_to_noir`.
4. Module-level `const GF8_POLY: u8 = 0x1b;` (a reduction-polynomial
   constant the multiply/invert functions reference) wasn't printed at
   all — `module.consts` was never iterated. Fixed by `print_const`,
   emitting Noir's `global` (confirmed via `nargo check`: Noir has no
   `const` keyword at all, only `global`).

One required upstream fix, still not made (out of scope for this backend's
own crate — it's a fix to `volar-primitives` itself): `Bit`
(`volar-primitives/src/lib.rs`) currently only implements `BitXor<u8>`, not
`Add`/`Sub`/`Mul`, even though `is_field_element()` claims parity with the
other field types. Should mirror `BitsInBytes` (XOR for add/sub, AND for
mul) — no longer blocked by tuple-struct support, just not yet done.

## Curated `StdMethod` subset

Each entry below was verified against `nargo check` first, not assumed:

- `Len` — works directly, `arr.len()`, no import.
- `WrappingAdd`/`WrappingSub` — real Noir methods, but (like the operator
  traits above) need `use std::ops::{WrappingAdd, WrappingSub};` — an initial
  assumption that Rust's `std::ops` methods would just work was wrong.
- `Min`/`Max` — **rewritten from method-call to free-function-call syntax**
  (`a.min(b)` → `min(a, b)`). Corrects an initial assumption that these would
  be plain methods the way Rust's `Ord::min`/`max` are; Noir only exposes
  `std::cmp::min`/`max` as free functions, confirmed by `nargo check` failing
  with "No method named 'min' found for type 'u32'" until rewritten.
- `Pow` — deliberately **not** supported. No verified working integer
  `.pow()` pattern was found (Noir's `pow` appears to be `Field`-only);
  left unsupported rather than guessed.
- `Checked*`/`Saturating*` — not implemented; would need deliberate emulation
  (e.g. an explicit assert-then-subtract pattern), not a rushed v1 add.
- `Vole`-kind methods — backend-specific (VOLE-in-the-head protocol), not
  applicable to Noir at all.

## Reachability-pruned printing

`print_module_noir_seeded(module, seeds)` reuses
`volar_compiler::reachability::compute_reachable` unchanged, filtering
`module.functions`/`module.impls` down to what's transitively reachable from
`seeds`. Simpler than `print_module_typescript_seeded`'s own variant, which
additionally threads witness maps and name-collision resolution through the
reachability data for TS-specific reasons — none of that applies here, so
this is just name-based filtering.

## Deferred, not out of scope

These are explicit product decisions with a path back, not gaps papered
over:

- **`Field`-based optimization.** No use of Noir's native `Field` type at
  all in v1 — every value maps to a typed integer. Revisit once the
  typed-integer approach is validated against real spec code; `Field` is
  cheaper but loses overflow checking.
- **`unconstrained`-assisted dynamic-length loops.** The real fix for
  Volar's "runtime-but-fixed bound" loops that Noir's `for` can't express —
  a fixed max-iteration-count loop with per-iteration masking, or a future
  witness-length pattern. Requires `unconstrained` (Brillig), itself
  excluded from v1.
- **Full reference/aliasing support.** Beyond parameter-position transparent
  unwrapping — returning/storing references, genuine `&mut` mutation through
  a parameter.
- **`Bit`'s missing `Add`/`Sub`/`Mul` impls.** An upstream `volar-primitives`
  fix (mirror `BitsInBytes`'s XOR/AND semantics), not a codegen gap — the
  tuple-struct mechanism that would carry it through is already in place.
- **CLI wiring.** `crates/compiler/volar-compiler-noir-codegen/src/bin/
  volar_codegen_noir.rs` is a minimal stub (parse a `--spec-dir`, print to
  `--out` or stdout, no dedup/reachability pruning). Broader integration —
  e.g. a `volar-codegen noir` target alongside `ts`/`dyn`, or driving
  `nargo compile`/`bb prove` end-to-end from `volar-build` — is future work.

## Verification

Every milestone was validated by actually running `nargo check`/`execute`
against emitted Noir source for a real fixture, not just inspecting generated
text — see `crates/compiler/volar-compiler-noir-codegen/tests/nargo_smoke.rs`.
Negative-path tests assert `Err` (never a panic) and confirm no `.nr` file is
ever written for a rejected module.
