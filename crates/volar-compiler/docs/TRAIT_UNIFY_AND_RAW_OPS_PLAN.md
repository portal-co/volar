# TRAIT_UNIFY_AND_RAW_OPS_PLAN.md

**Status**: All phases complete.

**Goal**: Three cleanups:

1. **Collapse `CryptoTrait` and `GenericKind::Crypto`** — all traits become `TraitKind::Custom(String)` or `TraitKind::Math(MathTrait)`. Classification uses predicate functions that inspect the builtin trait definitions, not hardcoded string lists. `GenericKind` reduces to `{ Length, Type }`.

2. **Add `RawMap`, `RawZip`, `RawFold`** — dedicated IR nodes for GenericArray/`[T;N]` element-wise ops that are NOT iterator chains.

3. **Remove `ArrayLength::Computed`** — non-analyzable computed lengths shouldn't exist in the IR.

---

## Phase 1: Collapse `CryptoTrait` into `Custom`; remove `GenericKind::Crypto`

### 1a. Remove enums from `ir.rs`

- Delete `CryptoTrait` enum and its `from_path()` impl
- Delete `CryptoMethod` enum and its `try_from_str()` impl
- Remove `TraitKind::Crypto(CryptoTrait)` variant
- Remove `MethodKind::Crypto(CryptoMethod)` variant

All `Crypto(CryptoTrait::Foo)` → `Custom("Foo".into())`.
All `Crypto(CryptoMethod::Foo)` → `Unknown("snake_case_name".into())`.

### 1b. Update `TraitKind::from_path()`

Remove the crypto check. Known crypto names now fall through to `Custom(last_segment)` naturally.

### 1c. Update `const_analysis.rs`

Remove `GenericKind::Crypto` entirely. The enum becomes:

```rust
pub enum GenericKind {
    Length,  // type-level constant → runtime usize
    Type,   // regular type parameter → remains generic
}
```

**Key change**: Classification predicates derive their knowledge from `builtin_trait_defs()` rather than hardcoded lists.

`is_length_bound()`: A bound is a length bound if its trait kind matches any builtin trait that has no methods and no non-length-related associated types. Concretely: `Unsigned` (via `MathTrait`) and any `Custom` trait whose name appears in a builtin def that is purely a type-level marker. In practice this means checking:

```rust
pub fn is_length_bound(bound: &IrTraitBound) -> bool {
    matches!(&bound.trait_kind, TraitKind::Math(MathTrait::Unsigned))
        || matches!(&bound.trait_kind, TraitKind::Custom(name) if name == "ArrayLength")
}
```

But `ArrayLength` is now `Custom("ArrayLength")`, and we check for it by name. Since it's defined in `builtin_trait_defs()`, we can derive it: iterate `builtin_trait_defs()`, collect names of traits that are marker/length traits (items are empty or only contain assoc types with length-like bounds), and check against that set.

However, computing this on every call would be expensive. **Pragmatic approach**: The list of length-trait names is small and stable (`ArrayLength`, `Unsigned`). Keep `is_length_bound` recognizing `MathTrait::Unsigned` directly (it's still in `MathTrait`) and `Custom("ArrayLength")` by name — but the source of truth is still the builtin def (if someone removes `ArrayLength` from builtins, the check becomes dead, not silently wrong).

`is_crypto_bound()` → **deleted**. There is no `Crypto` classification anymore. Everything that was `Crypto` is now `Type`. The lowering code that previously filtered by `GenericKind::Crypto` will filter by trait name directly (e.g., "does this parameter's bound list contain `Digest` or `BlockEncrypt` or `LengthDoubler`?").

`classify_generic()` returns only `Length` or `Type`:

```rust
pub fn classify_generic(param: &IrGenericParam, all_params: &[&[IrGenericParam]]) -> GenericKind {
    if param.kind == IrGenericParamKind::Const {
        return GenericKind::Length;
    }
    for bound in &param.bounds {
        if is_length_bound(bound) { return GenericKind::Length; }
        if is_fn_bound(bound) { return GenericKind::Type; }
    }
    // Recursive length check via param name
    let mut visited = Vec::new();
    if is_length_name(&param.name, all_params, &mut visited) {
        return GenericKind::Length;
    }
    GenericKind::Type
}
```

### 1d. Parse `LengthDoubler` from input, not hardcode

Remove `LengthDoubler` from `builtin_trait_defs()`. Instead, `LengthDoubler` is parsed from `volar-spec/src/byte_gen.rs` (or from a manifest) like any user-defined trait. The manifest system + `TypeContext::from_module_with_deps()` already handles this: when volar-spec is parsed, its traits (including `LengthDoubler`, `PuncturableLengthDoubler`, `VoleArray`) enter the module's `traits` vec and get registered in the `TypeContext`.

The lowering then discovers `B: LengthDoubler` as a `Custom("LengthDoubler")` bound. Since it's not a length bound, `classify_generic` returns `Type` — and the lowering code for function calls looks up the parameter by name pattern + bound name directly.

**Also in builtins**: Remove `BlockEncrypt`, `BlockCipher`, `Digest`, `Rng` from `builtin_trait_defs()` if they can instead be parsed from `volar-primitives` expanded source. If some are truly external (not in our source), keep minimal builtin definitions for them but as `Custom("Digest")` etc.

**Decision needed**: Which traits MUST stay as builtins (because they come from external crates not in our source tree) vs which can be parsed? Current builtins that come from external crates:
- `ArrayLength` — from `generic-array` / `typenum`
- `Unsigned` — from `typenum` (stays in `MathTrait`)
- `BlockEncrypt`, `BlockCipher` — from `cipher` crate
- `Digest` — from `digest` crate
- `Rng` / `RngCore` — from `rand` crate

These all come from external crates, so they DO need builtin definitions. `LengthDoubler` comes from `volar-spec`, so it should be parsed.

### 1e. Update `lowering_dyn.rs`

- Remove all `GenericKind::Crypto` references
- Replace `classify_generic(p, ...) == GenericKind::Crypto` with direct trait-name checks on the parameter's bounds:

```rust
fn has_trait_bound(param: &IrGenericParam, trait_name: &str) -> bool {
    param.bounds.iter().any(|b| match &b.trait_kind {
        TraitKind::Custom(name) => name == trait_name,
        _ => false,
    })
}

// Before: classify_generic(p, &[fn_gen]) == GenericKind::Crypto && p.name.starts_with('B')
// After:  has_trait_bound(p, "LengthDoubler") || has_trait_bound(p, "BlockEncrypt")
```

- Replace `TraitKind::Crypto(CryptoTrait::Foo)` matches with `TraitKind::Custom(name) if name == "Foo"`
- Replace `MethodKind::Crypto(CryptoMethod::Foo)` with `MethodKind::Unknown(name) if name == "encrypt_block"` etc.

### 1f. Update `printer.rs`

- `method_name()`: remove `MethodKind::Crypto` arm. `Unknown(s)` already returns `s.clone()`.
- Type/trait printing: remove `TraitKind::Crypto` arm. `Custom(name)` already prints the name.

### 1g. Update `manifest.rs`, `lowering.rs`

Search for any `Crypto` references and update to `Custom`.

---

## Phase 2: Add `RawMap`, `RawZip`, `RawFold`

### 2a. Add variants to `IrExpr` in `ir.rs`

```rust
/// Non-iterator element-wise map: `receiver.map(|var| body)`
/// GenericArray::map / [T; N]::map. Length-preserving, bounded.
RawMap {
    receiver: Box<IrExpr>,
    elem_var: String,
    body: Box<IrExpr>,
},

/// Non-iterator element-wise zip-with-map: `receiver.zip(other, |a, b| body)`
/// GenericArray::zip. Length-preserving, bounded.
RawZip {
    left: Box<IrExpr>,
    right: Box<IrExpr>,
    left_var: String,
    right_var: String,
    body: Box<IrExpr>,
},

/// Non-iterator fold over array: `receiver.fold(init, |acc, elem| body)`
/// When applied directly on GenericArray/[T;N] (no .iter() prefix).
RawFold {
    receiver: Box<IrExpr>,
    init: Box<IrExpr>,
    acc_var: String,
    elem_var: String,
    body: Box<IrExpr>,
},
```

### 2b. Parser changes in `convert_method_call()`

After `try_build_iter_chain` returns `None` (no iterator source found):

```rust
// Non-iterator .map() → RawMap
if method == "map" && args.len() == 1 {
    if let Expr::Closure(c) = args[0] {
        if c.inputs.len() == 1 {
            return Ok(IrExpr::RawMap { ... });
        }
    }
}

// Non-iterator .zip() with closure → RawZip
if method == "zip" && args.len() == 2 {
    if let Expr::Closure(c) = args[1] {
        return Ok(IrExpr::RawZip { ... });
    }
}

// Non-iterator .fold() → RawFold
if method == "fold" && args.len() == 2 {
    if let Expr::Closure(c) = args[1] {
        if c.inputs.len() == 2 {
            return Ok(IrExpr::RawFold { ... });
        }
    }
}
```

The existing `.zip()` 2-arg special case (which currently produces `IterPipeline`) gets replaced by the `RawZip` case above.

### 2c. Printer changes

In both `ExprWriter` and `ExprChainWriter`:

```rust
IrExpr::RawMap { receiver, elem_var, body } => {
    ExprWriter { expr: receiver }.fmt(f)?;
    write!(f, ".map(|{}| ", elem_var)?;
    ExprWriter { expr: body }.fmt(f)?;
    write!(f, ")")?;
}
IrExpr::RawZip { left, right, left_var, right_var, body } => {
    ExprWriter { expr: left }.fmt(f)?;
    write!(f, ".zip(")?;
    ExprWriter { expr: right }.fmt(f)?;
    write!(f, ", |{}, {}| ", left_var, right_var)?;
    ExprWriter { expr: body }.fmt(f)?;
    write!(f, ")")?;
}
IrExpr::RawFold { receiver, init, acc_var, elem_var, body } => {
    ExprWriter { expr: receiver }.fmt(f)?;
    write!(f, ".fold(")?;
    ExprWriter { expr: init }.fmt(f)?;
    write!(f, ", |{}, {}| ", acc_var, elem_var)?;
    ExprWriter { expr: body }.fmt(f)?;
    write!(f, ")")?;
}
```

### 2d. Lowering changes in `lowering_dyn.rs`

Recurse into sub-expressions:

```rust
IrExpr::RawMap { receiver, elem_var, body } => IrExpr::RawMap {
    receiver: Box::new(lower_expr_dyn(receiver, ctx, fn_gen)),
    elem_var: elem_var.clone(),
    body: Box::new(lower_expr_dyn(body, ctx, fn_gen)),
},
// ... similarly for RawZip, RawFold
```

### 2e. Tests

- Update `test_array_operations`: `.map()` → `RawMap`, `.zip()` → `RawZip`
- New: `test_raw_map_structure`, `test_raw_zip_structure`, `test_raw_fold_structure`
- New: `test_raw_map_vs_iter_map` — verify `arr.iter().map().collect()` is `IterPipeline`, `arr.map()` is `RawMap`
- New: `test_raw_ops_printer_round_trip`

---

## Phase 3: Remove `ArrayLength::Computed`

### 3a. Remove variant from `ir.rs`

Delete `ArrayLength::Computed(Box<IrExpr>)`.

### 3b. Update parser

In `convert_array_length_from_syn_expr()`, the catch-all `_ => Computed(...)` needs a replacement. Options:
- For path expressions like `N`, already handled by other variants (`TypeParam`)
- For complex expressions: these are rare and arguably shouldn't appear in the IR as array lengths. Return an error, or parse as `TypeParam` if it's a simple ident.

Need to audit what actually hits this path in practice.

### 3c. Update `lowering_dyn.rs`

`lower_array_length()` currently produces `Computed(TypenumUsize { ... })` as output. Replace with a new representation — likely `ArrayLength::Projection` already covers the semantic need (a type projection that resolves to a length at monomorphization time). The lowering can keep `Projection` for these cases instead of wrapping in `Computed`.

For `TypeParam` → lowered to lowercase var: this stays as `TypeParam("n")`.
For `Projection` → stays as `Projection` (the printer already handles it).

### 3d. Update printer

Remove `Computed(_) => write!(f, "_")` arm.

---

## Phase 4: Integration & Cleanup

### 4a. Run full test suite + generate tool

Verify:
- All tests pass
- `generate_volar_dyn` no longer panics on `LengthDoubler`
- Generated code correct

### 4b. Update IR_DOCUMENTATION.md

- Document `RawMap`, `RawZip`, `RawFold`
- Document trait unification
- Remove `CryptoTrait` references
- Remove `ArrayLength::Computed` references

---

## Summary of Enum Changes

| Before | After |
|--------|-------|
| `CryptoTrait { BlockEncrypt, BlockCipher, Digest, ArrayLength, Rng }` | Deleted — all become `TraitKind::Custom(name)` |
| `CryptoMethod { EncryptBlock, GenAbo, ... }` | Deleted — all become `MethodKind::Unknown(name)` |
| `TraitKind::Crypto(CryptoTrait)` | Deleted — use `Custom(String)` |
| `MethodKind::Crypto(CryptoMethod)` | Deleted — use `Unknown(String)` |
| `GenericKind { Length, Crypto, Type }` | `GenericKind { Length, Type }` |
| `ArrayLength::Computed(Box<IrExpr>)` | Deleted |
| — | `IrExpr::RawMap { receiver, elem_var, body }` |
| — | `IrExpr::RawZip { left, right, left_var, right_var, body }` |
| — | `IrExpr::RawFold { receiver, init, acc_var, elem_var, body }` |

## Risk Notes

- **`MathTrait` stays as enum**: Genuine semantic coupling (binop ↔ trait, operator syntax). Not touched.
- **External crypto traits stay as builtins**: `Digest`, `BlockEncrypt`, etc. come from external crates. Their definitions remain in `builtin_trait_defs()` as `Custom("Digest")` etc. `LengthDoubler` is parsed from source.
- **`RawMap` detection heuristic**: `.map(closure)` without iterator source → `RawMap`. If called on a type that doesn't have `.map()`, the printer still emits valid Rust (the Rust compiler catches the error). Not our concern.
- **`RawFold` vs iterator fold**: Iterator `.fold()` (after `.iter()`) stays in `IterPipeline` with `IterTerminal::Fold`. Non-iterator `.fold()` becomes `RawFold`. Distinction is made by whether `try_build_iter_chain` succeeds.
