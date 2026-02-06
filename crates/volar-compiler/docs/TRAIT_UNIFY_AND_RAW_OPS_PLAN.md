# TRAIT_UNIFY_AND_RAW_OPS_PLAN.md

**Goal**: Two tightly related cleanups:

1. **Unify `CryptoTrait` and custom traits** — collapse the `CryptoTrait` enum into `TraitKind::Custom(String)` so that `LengthDoubler`, `Digest`, `BlockEncrypt`, etc. are all just named traits. Classification moves entirely to `const_analysis.rs` predicate functions.

2. **Add `RawMap` / `RawZip` IR variants** — dedicated IR nodes for GenericArray/`[T;N]` element-wise `.map()` and `.zip()` that are *not* iterator chains. These produce bounded, length-preserving operations that the lowering can emit directly (no `.into_iter().map().collect()`).

---

## Motivation

### Trait unification

`CryptoTrait` is a hardcoded enum with 5 variants: `BlockEncrypt`, `BlockCipher`, `Digest`, `ArrayLength`, `Rng`. Meanwhile, user-defined traits like `LengthDoubler` land in `TraitKind::Custom(String)`. This split causes:

- **`LengthDoubler` misclassification**: `B: LengthDoubler` produces `Custom("LengthDoubler")`, so `is_crypto_bound()` returns false. The lowering (which looks for `GenericKind::Crypto`) panics with "could not find B generic parameter".
- **No real semantic difference**: The "crypto" traits have no special IR behavior. Their structure (associated types, methods) is defined in `builtin_trait_defs()` just like `MathTrait` traits. The classification logic in `const_analysis.rs` already uses *predicate functions* (`is_crypto_bound`, `is_length_bound`) — these can simply match on trait name strings.
- **Extensibility**: Any new domain trait (e.g., `PuncturableLengthDoubler`, `VoleArray`) would need another enum variant. With `Custom(String)`, it's just another name.

`MathTrait` stays as an enum because it has real semantic meaning: the compiler maps `BinOp::Add` ↔ `MathTrait::Add`, generates operator syntax (`a + b`), and drives the binop/unary trait builder helpers. `CryptoTrait` has none of this.

### RawMap / RawZip

GenericArray and `[T; N]` have their own `.map()` and `.zip()` methods that are *not* iterator adaptors:
- `GenericArray<T, N>::map(self, |t| ...) -> GenericArray<U, N>` — preserves length
- `GenericArray<T, N>::zip(self, other, |t, u| ...) -> GenericArray<O, N>` — preserves length

Currently:
- `.map(|x| ...)` without a preceding `.iter()` falls through `try_build_iter_chain` → `MethodCall`. This *works* for printing but loses structure.
- `.zip(other, |a, b| ...)` (2-arg form) is special-cased into an `IterPipeline` with `Zip` source + `Map` step + `Collect` terminal. This is **semantically wrong** — it's not an iterator, it doesn't need `collect()`, and the lowering will generate incorrect code.

With dedicated `RawMap`/`RawZip` variants:
- The parser recognizes them as bounded array operations (length-preserving)
- The printer emits `receiver.map(|x| body)` / `receiver.zip(other, |a, b| body)` directly
- The lowering handles them as bounded operations (no iterator machinery needed)
- `const_analysis.rs` can recognize them as terminating (bounded by array length)

---

## Phase 1: Collapse `CryptoTrait` into `TraitKind::Custom`

### 1a. Remove `CryptoTrait` enum

In `ir.rs`:
- Delete the `CryptoTrait` enum and its `from_path()` impl
- Delete the `CryptoMethod` enum and its `try_from_str()` impl
- Remove `TraitKind::Crypto(CryptoTrait)` variant
- Remove `MethodKind::Crypto(CryptoMethod)` variant

All current `Crypto(CryptoTrait::Foo)` usages become `Custom("Foo".into())`.
All current `Crypto(CryptoMethod::Foo)` usages become `Unknown("foo_method_name".into())`.

### 1b. Update `TraitKind::from_path()`

Currently does:
```rust
if let Some(crypto) = CryptoTrait::from_path(segments) {
    return Self::Crypto(crypto);
}
```

After: the known crypto names just fall through to `Custom(name)` like everything else. No special arm needed — `from_path` already produces `Custom(last_segment)` for unrecognized single-segment names.

### 1c. Update `const_analysis.rs`

Replace:
```rust
pub fn is_crypto_bound(bound: &IrTraitBound) -> bool {
    matches!(&bound.trait_kind,
        TraitKind::Crypto(CryptoTrait::BlockEncrypt)
        | TraitKind::Crypto(CryptoTrait::BlockCipher)
        | TraitKind::Crypto(CryptoTrait::Digest)
        | TraitKind::Crypto(CryptoTrait::Rng))
}
```

With:
```rust
const CRYPTO_TRAITS: &[&str] = &[
    "BlockEncrypt", "BlockCipher", "Digest", "Rng",
    "LengthDoubler", "PuncturableLengthDoubler",
];

pub fn is_crypto_bound(bound: &IrTraitBound) -> bool {
    match &bound.trait_kind {
        TraitKind::Custom(name) => CRYPTO_TRAITS.contains(&name.as_str()),
        _ => false,
    }
}
```

And update `is_length_bound`:
```rust
const LENGTH_TRAITS: &[&str] = &["ArrayLength", "Unsigned"];

pub fn is_length_bound(bound: &IrTraitBound) -> bool {
    match &bound.trait_kind {
        TraitKind::Math(MathTrait::Unsigned) => true,
        TraitKind::Custom(name) => LENGTH_TRAITS.contains(&name.as_str()),
        _ => false,
    }
}
```

(`Unsigned` stays in `MathTrait` because it's a typenum trait with genuine math semantics. `ArrayLength` moves to `Custom`.)

### 1d. Update `builtin_trait_defs()`

Change all `kind: TraitKind::Crypto(CryptoTrait::Foo)` to `kind: TraitKind::Custom("Foo".into())`.

Add `LengthDoubler` definition:
```rust
IrTrait {
    kind: TraitKind::Custom("LengthDoubler".into()),
    generics: vec![],
    super_traits: vec![],
    items: vec![
        IrTraitItem::AssociatedType {
            name: AssociatedType::OutputSize,
            bounds: vec![IrTraitBound {
                trait_kind: TraitKind::Custom("ArrayLength".into()),
                type_args: vec![IrType::Primitive(PrimitiveType::U8)],
                assoc_bindings: vec![],
            }],
            default: None,
        },
        IrTraitItem::Method(IrMethodSig {
            name: "double".into(),
            generics: vec![],
            receiver: None,  // static method
            params: vec![IrParam {
                name: "a".into(),
                ty: IrType::Array {
                    kind: ArrayKind::GenericArray,
                    elem: Box::new(IrType::Primitive(PrimitiveType::U8)),
                    len: ArrayLength::Projection {
                        r#type: Box::new(IrType::TypeParam("Self".into())),
                        field: "OutputSize".into(),
                    },
                },
            }],
            return_type: Some(IrType::Array {
                kind: ArrayKind::Fixed,
                elem: Box::new(IrType::Array {
                    kind: ArrayKind::GenericArray,
                    elem: Box::new(IrType::Primitive(PrimitiveType::U8)),
                    len: ArrayLength::Projection {
                        r#type: Box::new(IrType::TypeParam("Self".into())),
                        field: "OutputSize".into(),
                    },
                }),
                len: ArrayLength::Const(2),
            }),
            where_clause: vec![],
        }),
    ],
}
```

### 1e. Update `lowering_dyn.rs`

- Replace all `TraitKind::Crypto(CryptoTrait::Foo)` pattern matches with `TraitKind::Custom(name) if name == "Foo"`.
- Replace all `MethodKind::Crypto(CryptoMethod::Foo)` with `MethodKind::Unknown(name) if name == "foo_method"`.
- The `B` generic lookup now works: `LengthDoubler` → `is_crypto_bound` → `GenericKind::Crypto` ✓

### 1f. Update `printer.rs`

- `method_name()`: remove `MethodKind::Crypto` arm. `Unknown(s)` already returns `s.clone()`.
- `GenericsWriter` / `TypeParamBoundsWriter`: remove `TraitKind::Crypto` arm. `Custom(name)` already prints the name.

### 1g. Update `manifest.rs`

Search for `Crypto` references and update to `Custom`.

---

## Phase 2: Add `RawMap` and `RawZip` IR variants

### 2a. Add variants to `IrExpr`

In `ir.rs`:
```rust
/// Non-iterator element-wise map: `receiver.map(|var| body)`
/// Used for GenericArray::map and [T; N]::map.
/// Length-preserving; bounded by the receiver's length.
RawMap {
    receiver: Box<IrExpr>,
    elem_var: String,
    body: Box<IrExpr>,
},

/// Non-iterator element-wise zip-with-map: `receiver.zip(other, |left_var, right_var| body)`
/// Used for GenericArray::zip.
/// Length-preserving; bounded by the shorter input's length.
RawZip {
    left: Box<IrExpr>,
    right: Box<IrExpr>,
    left_var: String,
    right_var: String,
    body: Box<IrExpr>,
},
```

### 2b. Parser changes

In `convert_method_call()`:

**`.map()` with closure, no iterator source**: When `try_build_iter_chain` returns `None` and method is `"map"` with a single closure arg:
```rust
if method == "map" && args.len() == 1 {
    if let Expr::Closure(c) = args[0] {
        if c.inputs.len() == 1 {
            return Ok(IrExpr::RawMap {
                receiver: Box::new(convert_expr(receiver)?),
                elem_var: extract_pat_name(&c.inputs[0]),
                body: Box::new(convert_expr(&c.body)?),
            });
        }
    }
}
```

**`.zip()` with 2 args** (current special case): Change the existing `zip` + 2 args handler from producing `IterPipeline` to producing `RawZip`:
```rust
if method == "zip" && args.len() == 2 {
    if let Expr::Closure(c) = args[1] {
        return Ok(IrExpr::RawZip {
            left: Box::new(convert_expr(receiver)?),
            right: Box::new(convert_expr(args[0])?),
            left_var: extract_pat_name(&c.inputs[0]),
            right_var: extract_pat_name(&c.inputs[1]),
            body: Box::new(convert_expr(&c.body)?),
        });
    }
}
```

**Note**: std's `.zip()` (1 arg, no closure) still goes through `try_build_iter_chain` as an iterator step. Only the 2-arg GenericArray-style form becomes `RawZip`.

### 2c. Printer changes

In `ExprWriter`:
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
```

In `ExprChainWriter`: same arms (for when used inside an iterator chain, the raw map/zip on the collection feeds into the chain).

### 2d. Lowering changes

In `lowering_dyn.rs`:
```rust
IrExpr::RawMap { receiver, elem_var, body } => IrExpr::RawMap {
    receiver: Box::new(lower_expr_dyn(receiver, ctx, fn_gen)),
    elem_var: elem_var.clone(),
    body: Box::new(lower_expr_dyn(body, ctx, fn_gen)),
},
IrExpr::RawZip { left, right, left_var, right_var, body } => IrExpr::RawZip {
    left: Box::new(lower_expr_dyn(left, ctx, fn_gen)),
    right: Box::new(lower_expr_dyn(right, ctx, fn_gen)),
    left_var: left_var.clone(),
    right_var: right_var.clone(),
    body: Box::new(lower_expr_dyn(body, ctx, fn_gen)),
},
```

### 2e. Tests

Update `test_array_operations` — the non-iterator `.map()` case should now match `RawMap` instead of `MethodCall`. The `.zip()` case should match `RawZip` instead of `IterPipeline`.

Add new tests:
- `test_raw_map_preserves_structure`: parse `arr.map(|x| x + 1)`, verify `RawMap` node, print round-trip
- `test_raw_zip_preserves_structure`: parse `a.zip(b, |x, y| x + y)`, verify `RawZip` node, print round-trip
- `test_nested_raw_ops`: `a.zip(b, |x, y| x.map(|z| z + 1))` — outer is `RawZip`, inner is `RawMap`
- `test_raw_map_vs_iter_map`: verify `arr.iter().map(|x| x + 1).collect()` is `IterPipeline` while `arr.map(|x| x + 1)` is `RawMap`

---

## Phase 3: Cleanup & Integration

### 3a. Remove dead code

- Delete `CryptoTrait` and `CryptoMethod` enums (done in Phase 1a)
- Clean up any remaining `#[allow(dead_code)]` if present

### 3b. Update `const_analysis.rs` to recognize `RawMap`/`RawZip` as bounded

If there's an `is_bounded_expr` or similar function, add:
```rust
IrExpr::RawMap { .. } => true,
IrExpr::RawZip { .. } => true,
```

### 3c. Run full test suite + generate tool

Verify:
- All 79+ tests pass
- `generate_volar_dyn` no longer panics on `LengthDoubler` functions
- Generated code for volar-spec is correct

### 3d. Update IR_DOCUMENTATION.md

Add `RawMap`, `RawZip` descriptions. Update trait classification section.

---

## Risk Notes

- **`MathTrait` stays**: It has genuine semantic coupling (binop ↔ trait mapping, operator syntax). Unifying it would mean losing those compile-time guarantees.
- **`is_length_bound` / `is_crypto_bound` now string-based**: Slightly fragile, but the alternative (enum proliferation) is worse. The string lists are in one place each, easy to extend.
- **`RawMap` detection heuristic**: Any `.map(|x| body)` without an iterator source becomes `RawMap`. If a user calls `.map()` on a non-GenericArray, non-`[T;N]` type, it still works — the printer emits valid Rust. The IR just models it more specifically than `MethodCall`.
- **`RawZip` only for 2-arg form**: std's `.zip()` (1 arg) is iterator-only. GenericArray's `.zip()` (2 args: other + closure) is the only case that becomes `RawZip`.
