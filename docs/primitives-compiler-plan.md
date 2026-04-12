# Plan: Compiling `volar-primitives` Through `volar-compiler`

> **Phase 1 (manifest), Phase 3 (TS backend), and Phase 4 (integration) are
> COMPLETE.** Phase 2 (registry-driven primitives) is deferred — the hard-coded
> `PrimitiveType` enum works and new types are rare enough that manual
> registration is acceptable for now.

## Problem

`volar-primitives` defines the field element types (`Bit`, `Galois`, `Galois64`,
`Galois128`, `Galois256`, `BitsInBytes`, `BitsInBytes64`, `Tropical`) and their
arithmetic.  Today these types are:

- **Implemented** in hand-written `no_std` Rust with macros.
- **Manually registered** in `volar-compiler/src/ir.rs` as `PrimitiveType` enum
  variants, with per-variant match arms in:
  - `from_str` / `Display` (parser + printer)
  - `bit_width` / `is_field_element` (const analysis)
  - `lowering_dyn.rs` (default values)
  - `printer_ts.rs` (TS names + TS defaults)
  - `volar-lir-codegen/src/structs.rs` (LIR lowering)
- **Manually translated** to TypeScript in `packages/volar-runtime/`.

Every time a new primitive is added (e.g. `Galois128`), we need to touch all
of those sites.  If the arithmetic changes, the hand-written TS port must be
updated in lockstep.

## Goal

Make `volar-primitives` a first-class input to `volar-compiler`, so that:

1. Adding a new field type requires **only** writing the Rust struct + trait
   impls in `volar-primitives/src/`.
2. The compiler discovers the types, registers them as primitives, and
   generates correct dynamic Rust **and** TypeScript output automatically.
3. The `backend.rs` carry-less multiplication + Itoh–Tsujii inversion code
   is transpiled to TypeScript/C rather than hand-ported.

## Scope

This plan covers compilation of the **type definitions and trait impls** in
`volar-primitives`.  It does not cover:

- Hardware-accelerated backends (CLMUL, PCLMULQDQ).  Those remain native Rust
  with a runtime feature flag; the compiler only targets the portable reference
  implementation.
- The `Tropical` semiring (already generically handled by the compiler's trait
  system; no special primitive registration needed).

---

## Current Architecture

```
volar-primitives/src/
├── lib.rs          Newtypes + Add/Mul/Sub/Invert impls
└── backend.rs      Generic field_mul, field_invert, U256, GF(2^128/256) ops

volar-compiler/src/
├── ir.rs           PrimitiveType enum (hard-coded variants)
├── parser.rs       PrimitiveType::from_str (hard-coded match)
├── printer.rs      Display for PrimitiveType (hard-coded match)
├── printer_ts.rs   ts_primitive, ts_default_value (hard-coded match)
└── lowering_dyn.rs default value generation (hard-coded match)
```

## Proposed Architecture

### Phase 1: Parse `volar-primitives` as a Manifest

**What:** Extend `volar-compiler`'s manifest system to import `volar-primitives`
as a dependency, just like `volar-spec` already imports it for type resolution.

- Generate a `.volar.d` manifest for `volar-primitives` (struct definitions +
  trait impls, no function bodies).
- When the compiler builds `volar-spec`, it reads the primitives manifest to
  resolve `Galois`, `Galois128`, etc.

**Why this is low-risk:** The manifest system already exists (`manifest.rs`).
We'd just run `emit_manifest` on the parsed primitives module.

**Blockers:**
- The parser must handle `#[repr(transparent)]` (currently silently ignored —
  OK, just skip the attribute).
- The parser must handle macro-generated items.  Today `bool_field!`,
  `u8_field!`, `u64_field!` generate the struct definitions.  **Options:**
  - (a) Run `cargo expand` as a pre-step to desugar macros before parsing.
  - (b) Rewrite the macros as explicit struct definitions (5 types, ~30 LOC).
  - (c) Teach the parser to expand a whitelist of known macros.
  - **Recommended:** (b) — remove the macros entirely.  They save very little
    and the explicit forms are more readable.

**Deliverable:** `volar-primitives.volar.d` manifest file, auto-generated.

### Phase 2: Replace Hard-Coded `PrimitiveType` with Manifest-Driven Registration

**What:** Change the `PrimitiveType` enum from a closed set of variants to a
data-driven registry populated from the manifest.

- At compiler init, load the primitives manifest.
- For each struct in the manifest that has `#[repr(transparent)]` over a
  known backing type (`bool`, `u8`, `u64`, `u128`, `[u64; 4]`):
  - Register it as a "primitive-like" type with known bit width.
  - Check whether it implements `Add`, `Mul`, `Sub`, `Invert` by scanning
    the manifest's impls → set flags for `is_field_element`, `has_invert`.
- The parser's `PrimitiveType::from_str` becomes a manifest lookup instead
  of a hard-coded match.
- The printers emit the type name directly from the registry.

**Impact:** Adding `Galois512` in the future requires zero compiler changes.

**Risk:**
- The `PrimitiveType` enum is used in many pattern matches.  Converting all
  of them to registry-based dispatch is a large refactor.
- **Mitigation:** Keep the existing enum for the currently known types as an
  optimization fast-path.  Add a `Custom(String)` variant for manifest-discovered
  types.  Existing matches handle known variants; the `Custom` arm delegates
  to the registry.

### Phase 3: Compile `backend.rs` to TypeScript

**What:** Parse and transpile the carry-less multiplication, squaring, and
inversion functions from `backend.rs` to TypeScript (and optionally C via
`volar-c-backend`).

**Gaps in the total-Rust subset that `backend.rs` currently uses:**

| Construct | Status | Fix |
|-----------|--------|-----|
| `size_of_val` | Not in total subset | Replace with a const generic `W: usize` parameter |
| `From<u8>` bound | Parsed but not lowered to TS | Add TS lowering for `From` as a constructor |
| `<<` / `>>` / `&` / `^=` on generic T | Parsed (Shl/Shr/BitAnd/BitXorAssign) | Verify TS printer handles these |
| `for` loop with `0..(W * 8)` | Bounded loop OK | Already in total subset |
| `u128` type | Not in PrimitiveType | Add `U128` variant (backing type, not a field) |
| `U256` struct with methods | Struct + impls | Already parseable |
| `let mut` + reassignment | Already supported | — |
| `if` / `!=` / `==` | Already supported | — |

**Recommended approach:**
1. Refactor `field_mul` to take a const generic `W` (bit width) instead of
   using `size_of_val`.  This makes it parseable by the compiler.
2. Add `u128` as a new `PrimitiveType` backing type.
3. Parse `backend.rs` into an `IrModule`.
4. Dyn-lower it (field types become their backing types).
5. Print to TypeScript using `printer_ts`.
6. The TS runtime imports the generated `field_mul` function and the type
   wrappers are thin classes that call it.

**Alternative:** Keep `field_mul` as a manually-ported TS function and only
compile the *type wrappers* (Add/Mul/Sub/Invert impls).  This is simpler but
still requires a manual TS port of the core loop.

### Phase 4: End-to-End Integration

**What:** Wire the full pipeline so that `cargo run --example generate_volar_ts`
produces a TypeScript runtime that includes field types generated from
`volar-primitives` source, not hand-written.

- The codegen binary reads primitives manifest + primitives source.
- It compiles the impls to TS and emits them as part of `volar-runtime`.
- The existing `volar-spec` TS output imports these types.

---

## Ordering and Dependencies

```
Phase 1 (manifest)
    │
    ├── Phase 2 (registry)      ← can be deferred; useful but not blocking
    │
    └── Phase 3 (TS backend)
            │
            └── Phase 4 (integration)
```

Phase 1 is the critical path.  Phase 2 is a quality-of-life refactor that
can happen any time.  Phase 3 requires Phase 1 (to parse the primitives) and
is the main payoff.  Phase 4 is a thin wiring step.

## Estimated Effort

| Phase | LOC (est.) | Risk |
|-------|-----------|------|
| 1 — Manifest | ~100 | Low (manifest system exists) |
| 2 — Registry | ~300 | Medium (many match-arm sites) |
| 3 — TS backend | ~200 | Medium (const-generic refactor of `field_mul`) |
| 4 — Integration | ~50 | Low |

## Open Questions

1. **Should `U256` be a `PrimitiveType` or stay as a `Struct`?**
   It's a `[u64; 4]` wrapper, not a scalar.  Keeping it as a struct lets the
   compiler handle its methods generically.  The TS backend would emit it as a
   class.  **Recommendation:** struct, not primitive.

2. **Should `backend.rs` use `const W: usize` or stay generic over `T`?**
   The generic-over-T design is elegant in Rust but harder to transpile because
   the TS backend doesn't have traits.  A const-generic `W` + explicit `u8`,
   `u64`, `u128` specializations would be simpler to lower.
   **Recommendation:** const-generic `W` with trait bounds only for the ops.

3. **CLMUL acceleration?**
   Not in scope for the compiler.  The compiled output is the *reference*
   implementation.  Native Rust builds can use `cfg(target_feature = "pclmul")`
   to substitute an intrinsic-based `field_mul` at link time.  The compiler
   output and the accelerated path must agree on the reduction polynomial.

4. **What about `Bit` and `BitsInBytes` / `BitsInBytes64`?**
   `Bit` wraps `bool`; `BitsInBytes` wraps `u8` with AND-as-mul semantics
   (GF(2)^8 packed, not GF(2^8)).  These are structurally different from the
   Galois types.  The manifest approach handles them uniformly — the compiler
   discovers which ops they implement and generates the correct output.
