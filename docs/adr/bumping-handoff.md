# Handoff: Generic Field Bumping (`BumpField` / `Bumped<F>`)

**Status:** Deferred — requires Tier 3 (Opus 4.6+)  
**Blocking component:** `volar-spec`, `volar-primitives`, `volar-common`  
**Prepared by:** Sonnet 4.6 (Tier 2), session 2026-05-11

---

## Context

Volar needs a way to express "a value of field type `F`, or a missing/None
sentinel" without using Rust's `Option<F>` (which the compiler cannot lower to
a fixed-width integer).  The solution is *generic field bumping*: each GF(2^k)
field `F` has a canonical extension field `F::Next` with one extra element that
serves as the `None` sentinel.

The prototype already exists for the simplest case:

- `Bit` lives in GF(2) = {0, 1}.
- `Z3` lives in GF(3) = {0, 1, 2}.  `Z3(2)` is the `None` sentinel.

This document captures the design so a Tier-3 agent can complete it.

---

## Design

### `BumpField` trait  *(goes in `volar-spec`)*

```rust
/// A field type `F` that can be lifted into a strictly larger field `F::Next`
/// whose extra element serves as a `None` sentinel.
pub trait BumpField: Sized + Copy {
    /// The next larger field.  Must have exactly one more element than `Self`.
    type Next: Copy + PartialEq;

    /// The sentinel value in `Next` that represents `None`.
    const NONE_SENTINEL: Self::Next;

    /// Embed `self` into `Next`.  Must never produce `NONE_SENTINEL`.
    fn bump(self) -> Self::Next;

    /// Recover `Some(self)` if `v != NONE_SENTINEL`, else `None`.
    fn unbump(v: Self::Next) -> Option<Self>;
}
```

### `Bumped<F>` newtype  *(goes in `volar-spec`)*

```rust
/// An element of `F::Next` that is semantically `Option<F>`.
///
/// - `Bumped(F::NONE_SENTINEL)` → `None`.
/// - `Bumped(v)` where `v != NONE_SENTINEL` → `Some(F::unbump(v).unwrap())`.
#[repr(transparent)]
pub struct Bumped<F: BumpField>(pub F::Next);

impl<F: BumpField> Bumped<F> {
    pub fn none() -> Self { Bumped(F::NONE_SENTINEL) }
    pub fn some(v: F) -> Self { Bumped(v.bump()) }
    pub fn into_option(self) -> Option<F> { F::unbump(self.0) }
    pub fn is_none(self) -> bool { self.0 == F::NONE_SENTINEL }
}
```

### `impl BumpField for Bit`  *(goes in `volar-primitives`)*

```rust
impl BumpField for Bit {
    type Next = Z3;
    const NONE_SENTINEL: Z3 = Z3(2);
    fn bump(self) -> Z3 { Z3(self.0 as u8) }
    fn unbump(v: Z3) -> Option<Bit> {
        if v.0 == 0 { Some(Bit(false)) }
        else if v.0 == 1 { Some(Bit(true)) }
        else { None }
    }
}
```

### `BumpedVec<F, K>` *(goes in `volar-spec` or `volar-common`)*

A fixed-size array of `Bumped<F>` elements.  Required for ORAM-style
optional-slot structures:

```rust
pub struct BumpedVec<F: BumpField, const K: usize>(pub [Bumped<F>; K]);
```

---

## Compiler lowering  *(goes in `volar-compiler` or `volar-compiler-passes`)*

`Bumped::into_option()` must be lowered to an `IrExpr::If`:

```
IrExpr::If {
    cond: IrExpr::Binary { op: Ne, left: Var("v"), right: Lit(NONE_SENTINEL) },
    then_branch: { IrExpr::Call(Some, Var("v")) },
    else_branch: IrExpr::Path { segments: ["None"] },
}
```

The LIR/TFHE backend receives a `Z3` value and a branch; the `None` path is
a TFHE bootstrapping gate that selects the zero ciphertext.

---

## TFHE motivation

TFHE natively supports mod-`p` arithmetic for any small `p`.  GF(3) is the
smallest field that can express a ternary sentinel without padding.  The
`BumpField` abstraction lets us use TFHE's bootstrapping table (programmable
bootstrapping, PBS) to implement `Option`-like semantics without paying the
cost of a full `Option<bool>` circuit.

The `Z3` primitive type and raise pass (`raise_to_z3`) added in this session
are the prerequisite building blocks.

---

## Remaining work (for Tier 3)

1. Define `BumpField` trait in `crates/spec/volar-spec/src/lib.rs`.
2. Define `Bumped<F>` newtype in the same file.
3. Implement `BumpField for Bit` in `crates/spec/volar-primitives/src/lib.rs`.
4. Add `BumpedVec<F, K>` to `volar-spec` or `volar-common`.
5. Add compiler lowering of `Bumped::into_option()` → `IrExpr::If` in the
   `volar-compiler-passes` lowering pipeline.
6. Add TFHE-backend handling for `Z3` selectors in the weaver.
7. Update `docs/reliability.md` to track `BumpField` as Experimental until
   the PBS bootstrapping path is reviewed.

---

## Files that must not be touched by Tier 2

- `crates/spec/volar-spec/src/lib.rs` — `@ai-tier: 3`
- `crates/spec/volar-primitives/src/lib.rs` — `@ai-tier: 3`  
  (already modified this session under explicit owner direction for `Z3` only;
  `BumpField` impl is the remaining Tier-3 work)

---

*This document was generated as a Tier-2 handoff per `docs/agents-guide.md § 5`.*
