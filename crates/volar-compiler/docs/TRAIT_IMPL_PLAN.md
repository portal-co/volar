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
| 2 | Associated constants (type-level via `typenum`) | Not yet represented | Reuse extracted constant analysis |
| 3 | Methods (generic, `self`/no-receiver, any args) | Parsed, need richer classification in custom traits | `IrTraitItem::Method`, `IrImplItem::Method` |
| 4 | Trait-level generics | Parsed into `IrTrait.generics` | Already works but needs testing/validation |
| 5 | Custom trait definitions | `TraitKind::Custom(String)` exists but is a black box | Extend `TypeContext` with `TraitRegistry` |
| 6 | Built-in trait special treatment | `MathTrait` enum recognizes them, but their *structure* is implicit | Canonical `IrTrait` defs registered in `TypeContext` |

---

## Design

### 1. Associated Types (enrichment of existing)

**Current state**: `IrTraitItem::AssociatedType { name, bounds, default }` and `IrImplItem::AssociatedType { name, ty }` already exist and are parsed. The `AssociatedType` enum captures known names (`Output`, `Key`, `BlockSize`, …) plus `Other(String)` for custom names. ✅

**Changes needed**:
- Printer currently uses `{:?}` (Debug) for `AssociatedType` names in `write_trait` and `write_impl`. Fix to emit proper string names and include bounds.

**Action items**:
- [ ] Add `impl fmt::Display for AssociatedType` to `ir.rs`.
- [ ] Fix `write_trait` in `printer.rs` to print `type Name: Bound1 + Bound2;` (with optional default).
- [ ] Fix `write_impl` in `printer.rs` to print `type Name = Ty;`.
- [ ] Add round-trip test: parse a trait with bounded associated types, print it, verify.

### 2. Associated Constants (type-level via `typenum`) — piggyback on extracted constant analysis

**Current state**: `lowering_dyn.rs` already has `classify_generic()`, `GenericKind`, `is_length_bound()`, etc. that determine whether a type parameter represents a type-level constant (i.e., `GenericKind::Length`). This same logic applies to associated types: if a trait's associated type has bounds like `ArrayLength<u8>` or `Unsigned`, it is logically a type-level constant.

**Key insight from feedback**: Rather than adding new `AssociatedConst` IR variants, we **extract the existing constant-classification logic** out of `lowering_dyn.rs` into a standalone, reusable analysis. Both features (detecting length generics for the dyn lowering, and detecting type-level constants in trait associated types) use the same underlying analysis. The `IrTraitItem::AssociatedType` variant stays as-is in the IR; the *interpretation* of whether it's a type-level constant lives in a separate analysis result struct.

**New module**: `src/const_analysis.rs`

```rust
//! Constant analysis: classifies generics and associated types as type-level constants.
//!
//! Extracted from lowering_dyn so it can be reused for trait associated type
//! classification without polluting the IR with lowering-specific concerns.

/// Classification of a generic parameter or associated type.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GenericKind {
    /// Type-level constant (length/size) — becomes runtime usize in dyn lowering
    Length,
    /// Crypto trait parameter (BlockCipher, Digest) — remains generic
    Crypto,
    /// Regular type parameter — remains generic
    Type,
}

/// Results of constant analysis over an IrModule.
/// Kept separate from TypeContext to avoid mixing analysis results with
/// type resolution state.
#[derive(Debug, Clone, Default)]
pub struct ConstAnalysis {
    /// For each struct: which generics are lengths, which are types, etc.
    pub struct_generics: BTreeMap<String, Vec<(String, GenericKind, IrGenericParamKind)>>,

    /// For each trait: which associated types are type-level constants.
    /// Key: (trait_name, assoc_type_name) → GenericKind
    /// If the value is GenericKind::Length, the associated type is a type-level constant.
    pub trait_assoc_kinds: BTreeMap<(String, String), GenericKind>,

    /// For each impl block's associated type assignments:
    /// Key: (self_ty_string, assoc_type_name) → GenericKind (inherited from trait)
    pub impl_assoc_kinds: BTreeMap<(String, String), GenericKind>,
}

impl ConstAnalysis {
    pub fn from_module(module: &IrModule) -> Self { ... }

    /// Is this associated type a type-level constant in the given trait?
    pub fn is_type_level_const(&self, trait_name: &str, assoc_name: &str) -> bool {
        self.trait_assoc_kinds
            .get(&(trait_name.to_string(), assoc_name.to_string()))
            .map(|k| *k == GenericKind::Length)
            .unwrap_or(false)
    }
}

// Extracted from lowering_dyn.rs — these become pub functions in this module:
pub fn classify_generic(param: &IrGenericParam, all_params: &[&[IrGenericParam]]) -> GenericKind { ... }
pub fn is_length_bound(bound: &IrTraitBound) -> bool { ... }
pub fn is_crypto_bound(bound: &IrTraitBound) -> bool { ... }
pub fn is_fn_bound(bound: &IrTraitBound) -> bool { ... }
fn is_math_op_bound(bound: &IrTraitBound) -> bool { ... }
fn is_length_name(name: &str, all_params: &[&[IrGenericParam]], visited: &mut Vec<String>) -> bool { ... }
fn type_refers_to_length(ty: &IrType, all_params: &[&[IrGenericParam]], visited: &mut Vec<String>) -> bool { ... }

/// Classify an associated type's bounds to determine if it's a type-level constant.
pub fn classify_assoc_type_bounds(bounds: &[IrTraitBound]) -> GenericKind {
    for bound in bounds {
        if is_length_bound(bound) {
            return GenericKind::Length;
        }
        if is_crypto_bound(bound) {
            return GenericKind::Crypto;
        }
    }
    GenericKind::Type
}
```

**How this solves features 2 and 3 together**:
- `lowering_dyn.rs` calls `ConstAnalysis::from_module()` and uses it instead of inline classification. The `StructInfo`, `LoweringContext`, and dyn-specific lowering logic stay in `lowering_dyn.rs`, but the pure analysis functions move out.
- Trait/impl associated type classification falls out naturally: `ConstAnalysis` walks `IrModule.traits`, calling `classify_assoc_type_bounds()` on each `IrTraitItem::AssociatedType`, and records the results. Then when processing impls, it inherits the classification from the trait.
- Backends (C, HDL) can query `ConstAnalysis` to decide how to codegen an associated type without ever touching dyn-lowering code.

**Action items**:
- [ ] Create `src/const_analysis.rs`.
- [ ] Move `GenericKind`, `classify_generic`, `is_length_bound`, `is_crypto_bound`, `is_fn_bound`, `is_math_op_bound`, `is_length_name`, `type_refers_to_length` from `lowering_dyn.rs` → `const_analysis.rs`.
- [ ] Add `classify_assoc_type_bounds()` function.
- [ ] Add `ConstAnalysis` struct with `from_module()` constructor.
- [ ] Refactor `lowering_dyn.rs` to `use crate::const_analysis::*` instead of inline functions.
- [ ] Add `pub mod const_analysis;` to `lib.rs`.
- [ ] Add tests: verify `LengthDoubler::OutputSize` classified as `Length`, `Add::Output` classified as `Type`.
- [ ] Verify existing tests still pass after extraction.

### 3. Methods (generic, `self`/no-receiver, any args)

**Current state**: Already well-supported. `IrMethodSig` captures generics, receiver, params, return type, where clauses. `IrFunction` (used in impl items) captures the body too. Totality is enforced by rejecting `while`/`loop` in `convert_expr`.

**Action items**:
- [ ] Add test for trait with generic method (e.g., `fn remap<M, F>(&self, f: F) -> Self where F: FnMut(usize) -> usize`).
- [ ] Add test for trait with static method (no receiver, e.g., `fn double(a: Array) -> [Array; 2]`).
- [ ] Verify round-trip through printer.

### 4. Trait-Level Generics

**Current state**: `IrTrait.generics: Vec<IrGenericParam>` is populated by the parser. Trait-level generics appear in `volar-spec` as e.g., `trait VoleArray<T>: ArrayLength<T>` and in built-in traits as `trait Add<Rhs = Self>`.

**Action items**:
- [ ] Add test for parsing `trait Add<Rhs = Self> { type Output; fn add(self, rhs: Rhs) -> Self::Output; }`.
- [ ] Verify default type parameters (`Rhs = Self`) are preserved in `IrGenericParam.default`.
- [ ] Verify supertraits are captured in `IrTrait.super_traits`.

### 5. Custom Trait Definitions — extend `TypeContext`

**Current state**: `TypeContext` in `lowering.rs` already holds `traits: BTreeMap<String, IrTrait>` populated from `IrModule.traits`. It also holds `assoc_types` for resolution. The gap: no lookup by `TraitKind`, no validation that impls satisfy trait requirements, and no built-in trait definitions.

**Design**: Extend `TypeContext` with trait registry methods. `ConstAnalysis` stays in its own struct — `TypeContext` can *hold* a `ConstAnalysis` reference or take it as a parameter, but the constant analysis results are never stored inside `TypeContext` fields.

```rust
// Extended TypeContext in lowering.rs
impl TypeContext {
    /// Look up a trait definition by TraitKind
    pub fn lookup_trait(&self, kind: &TraitKind) -> Option<&IrTrait> {
        let name = match kind {
            TraitKind::Math(m) => format!("{:?}", m),
            TraitKind::Crypto(c) => format!("{:?}", c),
            TraitKind::Custom(n) => n.clone(),
            TraitKind::External { path } => path.join("::"),
            TraitKind::Into(_) => "Into".to_string(),
            TraitKind::AsRef(_) => "AsRef".to_string(),
            TraitKind::Fn(_, _) => "Fn".to_string(),
        };
        self.traits.get(&name)
    }

    /// Get the expected trait items (associated types, methods) for a trait.
    pub fn trait_items(&self, kind: &TraitKind) -> Option<&[IrTraitItem]> {
        self.lookup_trait(kind).map(|t| t.items.as_slice())
    }

    /// Validate that an impl block satisfies all required trait items.
    pub fn validate_impl(&self, imp: &IrImpl) -> Vec<String> {
        // Returns a list of missing/mismatched items
        ...
    }
}
```

**Action items**:
- [ ] Add `lookup_trait()` and `trait_items()` methods to `TypeContext`.
- [ ] Add `validate_impl()` method (returns `Vec<String>` of errors, not hard failure).
- [ ] Register built-in trait definitions (see feature 6) in `TypeContext::from_module()`.
- [ ] Add test: define `LengthDoubler` trait, implement it, verify `lookup_trait` works.
- [ ] Add test: verify `validate_impl` catches a missing associated type.

### 6. Built-in Trait Special Treatment

**Current state**: `MathTrait` and `CryptoTrait` enums recognize built-in traits by name, but their *structure* is implicit—hardcoded knowledge scattered through the codebase.

**Design**: Create canonical `IrTrait` definitions for each built-in trait. These are constructed once and registered into `TypeContext.traits` during `from_module()`. This makes them available to `lookup_trait()`, `validate_impl()`, and backend codegen uniformly.

```rust
// New in ir.rs
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
        // Sub, Mul, Div, Rem: same shape as Add with respective method names
        // BitAnd, BitOr, BitXor, Shl, Shr: same shape as Add
        // Neg, Not: unary — no Rhs generic, no params besides self
        // PartialEq<Rhs = Self> { fn eq(&self, other: &Rhs) -> bool; }
        // Eq: no items, supertrait PartialEq
        // PartialOrd, Ord: similarly
        // Clone { fn clone(&self) -> Self; }
        // Copy: marker, supertrait Clone
        // Default { fn default() -> Self; }  — no receiver!
        // BlockEncrypt { type BlockSize; fn encrypt_block(&self, block: &mut Block<Self>); }
        // Digest { type OutputSize; fn new() -> Self; fn update(&mut self, data: &[u8]); fn finalize(self) -> ...; }
        // ArrayLength<T> — marker with no items (type-level constant)
    ]
}

/// Map a binary operator to its corresponding math trait.
impl SpecBinOp {
    pub fn to_math_trait(&self) -> Option<MathTrait> {
        match self {
            Self::Add => Some(MathTrait::Add),
            Self::Sub => Some(MathTrait::Sub),
            Self::Mul => Some(MathTrait::Mul),
            Self::Div => Some(MathTrait::Div),
            Self::Rem => Some(MathTrait::Rem),
            Self::BitAnd => Some(MathTrait::BitAnd),
            Self::BitOr => Some(MathTrait::BitOr),
            Self::BitXor => Some(MathTrait::BitXor),
            Self::Shl => Some(MathTrait::Shl),
            Self::Shr => Some(MathTrait::Shr),
            Self::Eq => Some(MathTrait::PartialEq),
            Self::Ne => Some(MathTrait::PartialEq), // negated
            Self::Lt | Self::Le | Self::Gt | Self::Ge => Some(MathTrait::PartialOrd),
            _ => None,
        }
    }
}

/// Map a math trait to its canonical binary operator (for backends).
impl MathTrait {
    pub fn to_bin_op(&self) -> Option<SpecBinOp> {
        match self {
            Self::Add => Some(SpecBinOp::Add),
            Self::Sub => Some(SpecBinOp::Sub),
            Self::Mul => Some(SpecBinOp::Mul),
            Self::Div => Some(SpecBinOp::Div),
            Self::Rem => Some(SpecBinOp::Rem),
            Self::BitAnd => Some(SpecBinOp::BitAnd),
            Self::BitOr => Some(SpecBinOp::BitOr),
            Self::BitXor => Some(SpecBinOp::BitXor),
            Self::Shl => Some(SpecBinOp::Shl),
            Self::Shr => Some(SpecBinOp::Shr),
            _ => None,
        }
    }
}
```

**Action items**:
- [ ] Add `builtin_trait_defs()` to `ir.rs`.
- [ ] Add `SpecBinOp::to_math_trait()` and `MathTrait::to_bin_op()` to `ir.rs`.
- [ ] Register built-in defs in `TypeContext::from_module()` (insert into `self.traits` if not already present from user code).
- [ ] Add test: look up `Add` trait definition from `TypeContext`, verify it has `Output` and `add` method.
- [ ] Add test: `SpecBinOp::Add.to_math_trait() == Some(MathTrait::Add)` and back.

---

## Implementation Order

```
Phase 1: Extract constant analysis (enables features 2 + 3, unblocks everything)
  1a. Create src/const_analysis.rs
  1b. Move classify_generic, is_length_bound, etc. from lowering_dyn.rs
  1c. Add classify_assoc_type_bounds() and ConstAnalysis struct
  1d. Refactor lowering_dyn.rs to use const_analysis
  1e. Verify all existing tests pass

Phase 2: IR enrichment (features 1, 6)
  2a. Add Display for AssociatedType
  2b. Fix printer for associated types (names + bounds, not Debug)
  2c. Add builtin_trait_defs() to ir.rs
  2d. Add SpecBinOp ↔ MathTrait mappings

Phase 3: TypeContext extension (feature 5)
  3a. Add lookup_trait(), trait_items() to TypeContext
  3b. Register builtin_trait_defs() in TypeContext::from_module()
  3c. Add validate_impl() method

Phase 4: Testing (features 3, 4 + integration)
  4a. Unit tests for const_analysis (struct generics, trait assoc types)
  4b. Tests for trait-level generics (defaults, supertraits)
  4c. Tests for methods (generic, static, receiver variants)
  4d. Integration test: parse volar-spec, verify LengthDoubler is custom trait
      with OutputSize classified as Length by ConstAnalysis
  4e. Integration test: parse volar-spec, verify Add impls have Output resolved
  4f. Round-trip test: parse → print → re-parse → compare
```

## Architecture Diagram

```
                        ┌──────────────┐
                        │  IrModule    │
                        │  (ir.rs)     │
                        └──────┬───────┘
                               │
                    ┌──────────┼──────────┐
                    │          │          │
                    ▼          ▼          ▼
           ┌───────────┐ ┌──────────┐ ┌──────────────┐
           │ TypeContext│ │ Const    │ │ Lowering     │
           │(lowering) │ │ Analysis │ │ Dyn          │
           │           │ │(separate │ │(lowering_dyn)│
           │• traits   │ │ struct)  │ │              │
           │• assoc_ty │ │          │ │• uses Const  │
           │• lookup() │ │• generic │ │  Analysis    │
           │• validate│ │  kinds   │ │• StructInfo  │
           │           │ │• assoc   │ │              │
           │• builtin  │ │  kinds   │ │              │
           │  defs     │ │          │ │              │
           └───────────┘ └──────────┘ └──────────────┘
                │                │              │
                │                │              │
                ▼                ▼              ▼
           ┌─────────┐    ┌──────────┐   ┌──────────┐
           │ Printer  │    │ C/HDL    │   │ Printer  │
           │(printer) │    │ backends │   │ Dyn      │
           └─────────┘    │ (future) │   └──────────┘
                          └──────────┘
```

Key separation: `ConstAnalysis` is a pure analysis result struct. `TypeContext` holds type resolution state and trait definitions (including builtins). `LoweringDyn` consumes both but owns neither.

## Totality Invariant

All methods in trait definitions and implementations must be **total**. This is already enforced by `convert_expr` rejecting `while`/`loop`. The new infrastructure does not weaken this:
- `IrMethodSig` (trait items) has no body — totality is only enforced at impl time.
- `IrFunction` (impl items) goes through `convert_block` → `convert_expr`, which rejects unbounded loops.
- Type-level constants are *values* (resolved via `ConstAnalysis`), not computations — trivially total.

## Files Changed

| File | Changes |
|------|---------|
| **`src/const_analysis.rs`** | **New.** `GenericKind`, `ConstAnalysis`, extracted classification functions, `classify_assoc_type_bounds()`. |
| `src/ir.rs` | Add `Display for AssociatedType`, `builtin_trait_defs()`, `SpecBinOp::to_math_trait()`, `MathTrait::to_bin_op()`. |
| `src/lowering.rs` | Extend `TypeContext` with `lookup_trait()`, `trait_items()`, `validate_impl()`. Register builtins in `from_module()`. |
| `src/lowering_dyn.rs` | Remove extracted functions, `use crate::const_analysis::*`. `StructInfo` and `LoweringContext` stay here. |
| `src/printer.rs` | Fix associated type printing (use `Display` instead of `Debug`), handle bounds. |
| `src/lib.rs` | Add `pub mod const_analysis;`. |
| `src/printer_dyn.rs` | No changes expected (delegates to `printer.rs`). |
| `tests/traits_and_impls.rs` | **New.** Tests for all 6 features. |
