# Plan: Custom Traits & Implementations in volar-compiler IR

## Motivation

Today, the IR classifies traits into a fixed taxonomy: `MathTrait` (Add, Mul, …), `CryptoTrait` (BlockEncrypt, Digest, …), or `Custom(String)` / `External { path }`. This works for *recognizing* known traits, but the IR cannot **define** custom traits with their full structure—associated types, associated constants, generic parameters, methods—and faithfully represent their `impl` blocks. We need this for:

1. User-defined traits like `LengthDoubler`, `PuncturableLengthDoubler` from `volar-spec`.
2. Built-in math traits (`Add<Rhs>`, `BitXor<Rhs>`) that need special treatment in non-Rust backends (C, HDL) but whose structure (associated `Output` type, `fn op(self, rhs: Rhs) -> Output`) must be known to the IR.
3. Trait-level constants via `typenum` (e.g., `type OutputSize: ArrayLength<u8>`).
4. Totality: all trait methods must be total (no unbounded loops), enforced at the IR level.

## Scope (6 features)

| # | Feature | Status | Where |
|---|---------|--------|-------|
| 1 | Associated types | Partially parsed, need full IR definition support | `IrTraitItem::AssociatedType`, `IrImplItem::AssociatedType` |
| 2 | Associated constants (type-level via `typenum`) | Not yet represented | New `IrTraitItem::AssociatedConst`, `IrImplItem::AssociatedConst` |
| 3 | Methods (generic, `self`/no-receiver, any args) | Parsed, need richer classification in custom traits | `IrTraitItem::Method`, `IrImplItem::Method` |
| 4 | Trait-level generics | Parsed into `IrTrait.generics` | Already works but needs testing/validation |
| 5 | Custom trait definitions | `TraitKind::Custom(String)` exists but is a black box | New `IrTraitDef` or enriched `IrTrait` |
| 6 | Built-in trait special treatment | `MathTrait` enum recognizes them, but their *structure* is implicit | New `BuiltinTraitInfo` or canonical `IrTrait` for each |

## Design

### 1. Associated Types (enrichment of existing)

**Current state**: `IrTraitItem::AssociatedType { name, bounds, default }` and `IrImplItem::AssociatedType { name, ty }` already exist and are parsed. The `AssociatedType` enum captures known names (`Output`, `Key`, `BlockSize`, …).

**Changes needed**:
- Add `AssociatedType::Custom(String)` → **already exists** as `Other(String)`.  ✅ No change needed.
- Ensure the parser correctly extracts bounds on associated types (e.g., `type OutputSize: ArrayLength<u8>;`). → Already handled in `convert_trait_item`. ✅
- Ensure the printer (`write_trait`, `write_impl`) faithfully round-trips associated type declarations with their bounds. → Needs fix: currently prints `type {:?};` using Debug, should print `type Name: Bound1 + Bound2;` properly.

**Action items**:
- [ ] Fix `write_trait` in `printer.rs` to print associated type names as strings (not Debug format) and include bounds.
- [ ] Fix `write_impl` in `printer.rs` to print associated type names as strings.
- [ ] Add round-trip test: parse a trait with bounded associated types, print it, and verify.

### 2. Associated Constants (type-level via `typenum`)

**Current state**: Not represented. In `volar-spec`, type-level constants are modeled via `typenum` associated types (e.g., `type OutputSize: ArrayLength<u8>`), not Rust `const` items. However, for backend targets (C, HDL), these are logically *compile-time integer constants*.

**Design choice**: Represent them as a new IR node that explicitly marks an associated type as a **type-level constant** when its bounds indicate it is one (i.e., bounded by `ArrayLength`, `Unsigned`, or is a `typenum` const).

```rust
// New variant in IrTraitItem
pub enum IrTraitItem {
    Method(IrMethodSig),
    AssociatedType {
        name: AssociatedType,
        bounds: Vec<IrTraitBound>,
        default: Option<IrType>,
    },
    /// Type-level constant (typenum-style associated type that resolves to a usize)
    AssociatedConst {
        name: AssociatedType,
        /// The typenum bound that makes this a constant (e.g., ArrayLength, Unsigned)
        const_kind: TypeLevelConstKind,
        /// Additional bounds beyond the const kind
        bounds: Vec<IrTraitBound>,
        default: Option<IrType>,
    },
}

/// Classification of type-level constants
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeLevelConstKind {
    /// Bounded by `ArrayLength<T>` — an array size
    ArrayLength(Option<Box<IrType>>),
    /// Bounded by `Unsigned` — a generic unsigned integer
    Unsigned,
}
```

Similarly for impl items:

```rust
pub enum IrImplItem {
    Method(IrFunction),
    AssociatedType { name: AssociatedType, ty: IrType },
    /// Constant value for a type-level constant
    AssociatedConst {
        name: AssociatedType,
        const_kind: TypeLevelConstKind,
        /// The concrete typenum type (e.g., `U32`)
        ty: IrType,
    },
}
```

**Detection logic** (in parser, post-parse, or as an analysis pass):
- When parsing `IrTraitItem::AssociatedType`, check if any bound is `ArrayLength<_>` or `Unsigned`.
- If so, emit `AssociatedConst` instead of `AssociatedType`.
- In impl blocks, the corresponding `type OutputSize = D::OutputSize;` becomes `AssociatedConst` when the trait item it satisfies is a const.

**Action items**:
- [ ] Add `TypeLevelConstKind` enum to `ir.rs`.
- [ ] Add `IrTraitItem::AssociatedConst` and `IrImplItem::AssociatedConst` variants.
- [ ] Add classification function `fn classify_assoc_type(bounds: &[IrTraitBound]) -> Option<TypeLevelConstKind>`.
- [ ] Modify parser's `convert_trait_item` to classify and emit the right variant.
- [ ] Modify parser's `convert_impl_item` to match (needs trait context or post-parse fixup).
- [ ] Update printer to emit these correctly.
- [ ] Update lowering/lowering_dyn to handle the new variants.
- [ ] Add tests.

### 3. Methods (generic, `self`/no-receiver, any args)

**Current state**: Already well-supported. `IrMethodSig` captures generics, receiver, params, return type, where clauses. `IrFunction` (used in impl items) captures the body too. Totality is enforced by rejecting `while`/`loop` in `convert_expr`.

**Changes needed**:
- Validate that method generics' where-clauses are preserved through the full pipeline (parse → lower → print).
- Ensure methods without a receiver (static/associated functions) in trait definitions are handled — currently `IrMethodSig.receiver` is `Option<IrReceiver>`, which is correct.

**Action items**:
- [ ] Add test for trait with generic method (e.g., `fn remap<M, F>(&self, f: F) -> Self where F: FnMut(usize) -> usize`).
- [ ] Add test for trait with static method (no receiver).
- [ ] Verify round-trip through printer.

### 4. Trait-Level Generics

**Current state**: `IrTrait.generics: Vec<IrGenericParam>` is populated by the parser. `IrGenericParam` captures name, kind (Type/Const/Lifetime), bounds, and default.

**Changes needed**: Mostly validation. Trait-level generics appear in `volar-spec` as e.g., `trait VoleArray<T>: ArrayLength<T>` and in built-in traits as `trait Add<Rhs = Self>`.

**Action items**:
- [ ] Add test for parsing `trait Add<Rhs = Self> { type Output; fn add(self, rhs: Rhs) -> Self::Output; }`.
- [ ] Verify default type parameters (`Rhs = Self`) are preserved in `IrGenericParam.default`.
- [ ] Verify supertraits are captured in `IrTrait.super_traits`.

### 5. Custom Trait Definitions

**Current state**: `TraitKind::Custom(String)` is used for any trait not recognized as `MathTrait` or `CryptoTrait`. The `IrTrait` struct stores the full definition (generics, supertraits, items). The issue is that `TraitKind::Custom(String)` carries only a name—it's not linked back to its `IrTrait` definition.

**Design**: We do NOT need to change `TraitKind` — it's a *classification key*, not a definition. The definitions live in `IrModule.traits`. What we need is:

1. A **trait registry** that maps `TraitKind` → `&IrTrait` for lookups.
2. When an `IrImpl` references `TraitKind::Custom("LengthDoubler")`, lowering/codegen should be able to look up the full trait definition to know what associated types/consts/methods it expects.

```rust
// In lowering.rs or a new resolution module
pub struct TraitRegistry {
    /// Maps trait name → trait definition
    pub defs: BTreeMap<String, IrTrait>,
}

impl TraitRegistry {
    pub fn from_module(module: &IrModule) -> Self { ... }
    
    /// Look up a trait by its TraitKind
    pub fn lookup(&self, kind: &TraitKind) -> Option<&IrTrait> { ... }
    
    /// Get the expected associated types/consts for a trait
    pub fn expected_items(&self, kind: &TraitKind) -> Vec<&IrTraitItem> { ... }
}
```

**Action items**:
- [ ] Add `TraitRegistry` to `lowering.rs` (or new `resolution.rs` module).
- [ ] Populate it from `IrModule.traits`.
- [ ] Integrate with `TypeContext` (which already partially does this via `traits: BTreeMap<String, IrTrait>`).
- [ ] Add validation: every `IrImpl` for a custom trait must satisfy all trait items (associated types defined, methods implemented).
- [ ] Add test: define `LengthDoubler` trait, implement it, verify registry lookup works.

### 6. Built-in Trait Special Treatment

**Current state**: `MathTrait` and `CryptoTrait` enums recognize built-in traits by name, but their *structure* (what associated types they have, what methods, what generics) is implicit—hardcoded knowledge scattered through the codebase.

**Design**: Create canonical `IrTrait` definitions for each built-in trait, stored in the `TraitRegistry`. These serve as the single source of truth for:
- What `Add<Rhs>` looks like: one generic param `Rhs` (default `Self`), one associated type `Output`, one method `fn add(self, rhs: Rhs) -> Self::Output`.
- What `Clone` looks like: no generics, no associated types, one method `fn clone(&self) -> Self`.
- What `BlockEncrypt` looks like: associated type `BlockSize`, method `fn encrypt_block(&self, block: &mut Block<Self>)`.

This lets backends query the trait structure uniformly without hardcoding.

```rust
// New in ir.rs or resolution.rs
pub fn builtin_trait_defs() -> Vec<IrTrait> {
    vec![
        // Add<Rhs = Self> { type Output; fn add(self, rhs: Rhs) -> Self::Output; }
        IrTrait {
            kind: TraitKind::Math(MathTrait::Add),
            generics: vec![IrGenericParam {
                name: "Rhs".into(),
                kind: IrGenericParamKind::Type,
                bounds: vec![],
                default: Some(IrType::TypeParam("Self".into())),
            }],
            super_traits: vec![],
            items: vec![
                IrTraitItem::AssociatedType {
                    name: AssociatedType::Output,
                    bounds: vec![],
                    default: None,
                },
                IrTraitItem::Method(IrMethodSig {
                    name: "add".into(),
                    generics: vec![],
                    receiver: Some(IrReceiver::Value),
                    params: vec![IrParam {
                        name: "rhs".into(),
                        ty: IrType::TypeParam("Rhs".into()),
                    }],
                    return_type: Some(IrType::Projection {
                        base: Box::new(IrType::TypeParam("Self".into())),
                        trait_path: Some("Add".into()),
                        trait_args: vec![],
                        assoc: AssociatedType::Output,
                    }),
                    where_clause: vec![],
                }),
            ],
        },
        // ... Sub, Mul, Div, BitAnd, BitOr, BitXor, Shl, Shr, Neg, Not similarly
        // ... Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord
        // ... BlockEncrypt, Digest, etc.
    ]
}
```

**Why this matters for non-Rust targets**: When compiling `a + b` where `a: T` and `T: Add<U, Output = O>`, a C backend needs to know that `+` maps to `Add::add`, and that the output type is `O`. An HDL backend might map `BitXor` to a gate. Having the canonical definition in the IR makes this uniform.

**Action items**:
- [ ] Define `builtin_trait_defs()` returning canonical `IrTrait` for all `MathTrait` and `CryptoTrait` variants.
- [ ] Auto-register these in `TraitRegistry`.
- [ ] Add a mapping function: `SpecBinOp → MathTrait` (e.g., `SpecBinOp::Add → MathTrait::Add`).
- [ ] Add a mapping function: `MathTrait → SpecBinOp` (inverse, for backends that want to desugar trait calls to operators).
- [ ] Add test: look up `Add` trait definition, verify it has `Output` and `add` method.

## Implementation Order

```
Phase 1: Foundation (IR changes)
  1a. Add TypeLevelConstKind, IrTraitItem::AssociatedConst, IrImplItem::AssociatedConst
  1b. Add TraitRegistry to lowering.rs
  1c. Add builtin_trait_defs() and register them
  1d. Fix associated type printing (Debug → proper names + bounds)

Phase 2: Parser updates
  2a. Classify associated types as consts when bounded by ArrayLength/Unsigned
  2b. (Impl items need trait-context for classification — use post-parse fixup via TraitRegistry)

Phase 3: Lowering & printer updates
  3a. Handle AssociatedConst in lowering_dyn (these become runtime usize witnesses)
  3b. Handle AssociatedConst in printer (print as `type Name = ...;` for Rust, or `const size_t name = ...;` for C)
  3c. Ensure round-trip fidelity for custom traits

Phase 4: Testing
  4a. Unit tests for each feature
  4b. Integration test: parse volar-spec, verify LengthDoubler is a custom trait with AssociatedConst
  4c. Integration test: parse volar-spec, verify Add<Vope<...>> impls have Output type resolved
  4d. Round-trip test: parse → print → re-parse → compare
```

## Totality Invariant

All methods in trait definitions and implementations must be **total**. This is already enforced by `convert_expr` rejecting `while`/`loop`. The new trait infrastructure does not weaken this:
- `IrMethodSig` (trait items) has no body — totality is only enforced at impl time.
- `IrFunction` (impl items) goes through `convert_block` → `convert_expr`, which rejects unbounded loops.
- Type-level constants (`AssociatedConst`) are values, not computations — trivially total.

## Files Changed

| File | Changes |
|------|---------|
| `src/ir.rs` | Add `TypeLevelConstKind`, `AssociatedConst` variants, `builtin_trait_defs()`, `SpecBinOp ↔ MathTrait` mappings |
| `src/parser.rs` | Classify associated types as consts during parsing |
| `src/lowering.rs` | Add `TraitRegistry`, integrate with `TypeContext` |
| `src/lowering_dyn.rs` | Handle `AssociatedConst` in lowering |
| `src/printer.rs` | Fix associated type printing, handle `AssociatedConst` |
| `src/printer_dyn.rs` | No changes expected (delegates to `printer.rs`) |
| `tests/` | New test file `tests/traits_and_impls.rs` |

## Open Questions

1. **Should `AssociatedConst` be a separate variant or a flag on `AssociatedType`?**
   - Separate variant: clearer, but more match arms everywhere.
   - Flag: `AssociatedType { ..., is_type_level_const: Option<TypeLevelConstKind> }` — fewer changes but muddier semantics.
   - **Recommendation**: Separate variant. The semantic difference is real (affects codegen), and the additional match arms are manageable.

2. **Should `TraitRegistry` be a standalone struct or merged into `TypeContext`?**
   - `TypeContext` already has `traits: BTreeMap<String, IrTrait>`. We could just extend it.
   - **Recommendation**: Extend `TypeContext` with the lookup methods. Add `builtin_trait_defs()` to its constructor. Keeps things centralized.

3. **Post-parse fixup vs. parser-time classification for impl `AssociatedConst`?**
   - At parse time, we don't know which trait is being implemented when we see `type OutputSize = D::OutputSize;` inside an `impl` block. We'd need to look up the trait.
   - **Recommendation**: Two-pass. Parse as `AssociatedType` first. Then run a fixup pass using `TraitRegistry` to reclassify impl items that correspond to trait `AssociatedConst` items. This keeps the parser simple and the logic in one place.
