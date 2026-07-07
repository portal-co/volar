# Agent Context: `u128` support in the LIR/C backend (deferred)

**Load this when:** touching `volar-lir-codegen` (`primitive_to_lir`), the C
backend (`volar-c-backend`), or anything that would spec-link `u128`-using code
(e.g. `volar_fold::scalar::Scalar`'s Montgomery arithmetic, `volar_spec::curve`'s
`Fe25519`/`EdPoint`) into a compiled target.

## The gap

`crates/compiler/volar-c-backend/tests/curve_e2e.rs`'s own header documents this:
`u128` multiplications get silently truncated to `i64` by `primitive_to_lir`
(`PrimitiveType::U128 → LirType::I64`) — there is no `LirType::U128` /
`__uint128_t` emission in the C backend. That test works around it by
hand-writing the entire Ed25519 arithmetic directly in C, bypassing the IR
pipeline for that one test.

The same gap blocks `volar_fold::scalar::Scalar` (the `F_ℓ` folding scalar,
Montgomery reduction, u128-widened products throughout — both the fast path and
the "reference" `mul_ref` path) from ever being spec-linked and lowered through
the C backend, not just `volar_spec::curve`.

## Why this matters (context from the continuation bridge's folding work)

Anything touching `volar_fold::scalar::Scalar` (the continuation bridge's
`F_ℓ` folding scalar) or `volar_spec::curve`'s `Fe25519`/`EdPoint` currently
has to execute via `print_module` → real `rustc`, specifically *because*
this gap blocks the C/LIR path. (Separately, the IOP-based prove-the-verifier
path's `IopSink`-woven verifiers, `docs/prove-the-verifier-iop.md`, also use
the `rustc` path via `emit_verifier_rust` — but for an unrelated reason: its
`IopChallenge`/`IopAccumulator`/`iop_fold_gate` bare names aren't part of the
IR's own type system, not a `u128` issue — its `Gf128` tower field doesn't
use `u128` arithmetic at all.)

## What closing this would unlock

- `curve_e2e.rs` could drop its hand-written-C workaround.
- The fold math (`volar_spec::fold`, already spec-link-shaped) could target the C
  backend too, not just the `rustc` path — useful wherever C is the only
  available executable substrate (this repo's `docs/`/memory notes: the LLVM
  backend isn't reliably buildable in every environment).

## Scope of the fix (not attempted here)

Add `LirType::U128` and its `__uint128_t` C emission, and the corresponding
128-bit arithmetic lowering in `primitive_to_lir` / the LIR-to-C printer. Real,
bounded compiler work — not attempted as part of the prove-the-verifier folding
plan; tracked here as a prerequisite for anyone who wants the C-backend path for
`Scalar`/curve code later.
