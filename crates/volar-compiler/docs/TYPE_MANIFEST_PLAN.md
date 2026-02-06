# Type Manifest Files & Cleanup Plan (v2)

## Goals

1. **Type manifest files** (`.volar.d`): enable cross-crate compilation by emitting and consuming type-only descriptions of compiled crates. First target: compile `volar-primitives` and depend on it when compiling `volar-spec`.
2. **Remove stale hardcodes**: `VoleArray` and `ByteBlockEncrypt` are `volar-spec`-defined traits that were hardcoded as `CryptoTrait` enum variants. Remove them.
3. **Refactor the printer**: Introduce a `trait RustBackend` with a `Display`-adapter struct, so the Rust printer is properly encapsulated and can carry extra state (deps, config).

---

## Part A: Remove Stale Hardcodes

### A1. Remove `CryptoTrait::VoleArray` and `CryptoTrait::ByteBlockEncrypt`

**Files changed:** `ir.rs`, `const_analysis.rs`, `lowering_dyn.rs`, `printer.rs`

1. **`ir.rs`** — Remove `VoleArray` and `ByteBlockEncrypt` from `CryptoTrait` enum and `from_path`. Remove their entries from `builtin_trait_defs()`.
2. **`const_analysis.rs`** — In `is_length_bound()`, `VoleArray` was treated as a length bound. After removal, `trait VoleArray<T>: ArrayLength<T>` from volar-spec parses as `TraitKind::Custom("VoleArray")`. We need `is_length_bound` to handle custom traits whose supertrait chain includes `ArrayLength`. **Approach:** `ConstAnalysis::from_module` already has access to the module's trait definitions; add a pre-pass that collects "length alias" custom traits (those whose supertraits include `ArrayLength` or another known length trait). Feed this set into `is_length_bound`. In `is_crypto_bound`, remove `ByteBlockEncrypt`; it becomes `Custom("ByteBlockEncrypt")` and won't match anything special.
3. **`lowering_dyn.rs`** — Replace hardcoded `CryptoTrait::VoleArray`/`ByteBlockEncrypt` checks with `TraitKind::Custom("VoleArray")`/`Custom("ByteBlockEncrypt")` string matches as interim, then delete them once manifests provide marker-trait metadata.
4. **`printer.rs`** — Remove the hardcoded `ByteBlockEncrypt` trait+blanket-impl preamble and the hardcoded `volar_primitives` re-export. These move into the data-driven preamble (Part C).

### A2. Collapse `StructKind` to just `Custom(String)` + `GenericArray`

The named variants (`Delta`, `Q`, `Vope`, `BitVole`, `ABO`, `ABOOpening`, `CommitmentCore`, `Poly`, `PolyInputPool`) are never pattern-matched anywhere — they're purely string tags. Collapsing them simplifies the IR.

**Keep `GenericArray`** as a named variant because it has special semantics in lowering (it's a type-level-sized array, not a user struct).

**Files changed:** `ir.rs` (enum + `from_str` + `Display`)

---

## Part B: Refactor the Printer — `trait RustBackend`

### Motivation

The current printer is a bag of free functions writing to `&mut String`. Problems:
- No place to carry state (dependency info, indentation policy, preamble config)
- Output locked to `String` — can't stream to a file
- The hardcoded preamble in `print_module` mixes output policy with IR rendering
- Can't reuse the rendering logic for manifests vs. full codegen vs. pretty-printing

### Design: `trait RustBackend` + `DisplayRust<T>` adapter

```rust
/// Backend trait for rendering IR nodes to Rust source text.
///
/// Implementors carry whatever extra state they need (deps, config, etc.)
/// and implement `fmt` to write the IR node to a `fmt::Formatter`.
pub trait RustBackend {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

/// Display adapter — wraps any `RustBackend` to implement `core::fmt::Display`.
pub struct DisplayRust<T: RustBackend>(pub T);

impl<T: RustBackend> fmt::Display for DisplayRust<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}
```

Concrete implementors hold a reference to the IR node plus any context:

```rust
/// Renders an IrModule as a complete Rust source file.
pub struct ModuleWriter<'a> {
    pub module: &'a IrModule,
    // future: deps, preamble config, etc.
}

impl<'a> RustBackend for ModuleWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for s in &self.module.structs {
            StructWriter { s }.fmt(f)?;
            writeln!(f)?;
        }
        for t in &self.module.traits {
            TraitWriter { t }.fmt(f)?;
            writeln!(f)?;
        }
        for i in &self.module.impls {
            ImplWriter { i }.fmt(f)?;
            writeln!(f)?;
        }
        for func in &self.module.functions {
            FunctionWriter { f: func, level: 0, is_trait_item: false }.fmt(f)?;
            writeln!(f)?;
        }
        Ok(())
    }
}
```

Each sub-writer is its own struct:

```rust
pub struct StructWriter<'a> { pub s: &'a IrStruct }
pub struct TraitWriter<'a> { pub t: &'a IrTrait }
pub struct ImplWriter<'a> { pub i: &'a IrImpl }
pub struct FunctionWriter<'a> { pub f: &'a IrFunction, pub level: usize, pub is_trait_item: bool }
pub struct TypeWriter<'a> { pub ty: &'a IrType }
pub struct ExprWriter<'a> { pub expr: &'a IrExpr }
pub struct BlockWriter<'a> { pub block: &'a IrBlock, pub level: usize }
pub struct PatternWriter<'a> { pub pat: &'a IrPattern }
// ... etc
```

All implement `RustBackend`, which means they all also get `Display` for free via `DisplayRust<T>`.

### Migration path

1. Convert existing free functions to methods on writer structs
2. Each `write_foo(out: &mut String, node, ...)` becomes `FooWriter { node, ... }.fmt(f)`
3. All `write!(out, ...)` become `write!(f, ...)`; all `.unwrap()` become `?`
4. Keep `pub fn print_module(module: &IrModule) -> String` as a convenience:
   ```rust
   pub fn print_module(module: &IrModule) -> String {
       DisplayRust(ModuleWriter { module }).to_string()
   }
   ```
5. Move the hardcoded preamble into a `DynPreambleWriter` (or into `printer_dyn.rs`)

### Benefits
- Each writer struct can carry extra context (later: dependency info, import tracking)
- `Display` integration: `format!("{}", DisplayRust(TypeWriter { ty: &my_type }))` works anywhere
- Testable: can test individual writers in isolation
- `fmt::Result` propagation eliminates all `.unwrap()` calls
- Future: a `ManifestModuleWriter` can override behavior (e.g., emit `todo!()` bodies)

### Files changed
- `printer.rs` — complete rewrite (same logic, new structure)
- `printer_dyn.rs` — use `ModuleWriter` or `DynModuleWriter`
- No changes to IR types

---

## Part C: Type Manifest Files

### What is a type manifest?

A **type manifest** is a serialized subset of `IrModule` that contains:
- Struct definitions (name, generics, fields, tuple-ness)
- Trait definitions (name, generics, supertraits, items — method *signatures* without bodies)
- Impl blocks (generics, trait ref, self type, items — associated types + method *signatures*)
- Type aliases
- Free function *signatures* (no bodies)
- Crate name + version metadata

It does **not** contain function/method bodies. It's the minimum information needed to type-check and lower code that depends on this crate.

### Format: text-based with binary poison pill

The manifest is **text-based Rust** with `todo!()` method bodies — human-readable, diffable, round-trippable through the existing parser. But it must not be accidentally compiled by `rustc` or by this compiler in normal mode.

**Poison pill**: The very first byte of a `.volar.d` file is `0xFF`, which is invalid UTF-8. This means:
- `rustc` will reject it immediately (Rust source must be valid UTF-8)
- `syn` will reject it (it requires valid UTF-8 input)
- The volar-compiler parser in normal mode will reject it (syn rejects it)
- Only `parse_manifest()` knows to strip the `0xFF` prefix before parsing

**Header** (after the `0xFF` byte):

```rust
//! @volar-manifest crate-name version
//! @volar-deps [dep1, dep2]

// ... normal Rust-like declarations ...
```

**`parse_manifest(raw_bytes: &[u8])`**:
1. Check first byte is `0xFF`; error if not
2. Strip `0xFF` prefix
3. Decode remaining bytes as UTF-8
4. Parse `//! @volar-manifest` header for metadata
5. Parse body with existing `parse_source()` (which calls syn)
6. Return `TypeManifest`

**`emit_manifest(module: &IrModule, crate_name: &str, version: &str) -> Vec<u8>`**:
1. Strip all function/method bodies → `IrBlock { stmts: [], expr: Some(IrExpr::Call { func: "todo!", args: [] }) }`
2. Render header + module body using `ManifestModuleWriter` (a `RustBackend` implementor)
3. Prepend `0xFF` byte
4. Return `Vec<u8>` (not `String`, since it's not valid UTF-8)

### `TypeManifest` struct

```rust
/// Metadata for a compiled crate's type manifest.
#[derive(Debug, Clone)]
pub struct TypeManifest {
    pub crate_name: String,
    pub version: String,
    pub deps: Vec<String>,
    pub module: IrModule,
}
```

### New module: `src/manifest.rs`

Contains:
- `TypeManifest` struct
- `emit_manifest()` — body stripping + serialization
- `parse_manifest()` — deserialization with `0xFF` guard
- `ManifestModuleWriter` — `RustBackend` impl that renders signature-only Rust
- `MANIFEST_MARKER: u8 = 0xFF`

### Integration with TypeContext / LoweringContext

**`TypeContext::from_module_with_deps(module: &IrModule, deps: &[TypeManifest]) -> TypeContext`**:
- Registers builtins first
- Then registers structs/traits/impls from each dependency manifest
- Then registers the module's own definitions (overrides deps on conflict)
- Dependency types are available for `lookup_trait`, `validate_impl`, etc.

**`LoweringContext::new_with_deps(module: &IrModule, deps: &[TypeManifest]) -> LoweringContext`**:
- Populates `struct_info` for dependency structs (so lowering knows their generic structure)
- The lowered output can reference dependency types without needing their source

### Workflow

```
volar-primitives/src/*.rs
    → cargo expand → expanded source
    → parse_source → IrModule
    → emit_manifest("volar-primitives", "0.1.0") → volar-primitives.volar.d (bytes with 0xFF prefix)

volar-spec/src/*.rs
    → parse_sources → IrModule
    → parse_manifest(read_bytes("volar-primitives.volar.d")) → TypeManifest
    → TypeContext::from_module_with_deps(module, &[primitives_manifest])
    → LoweringContext::new_with_deps(module, &[primitives_manifest])
    → lower_module_dyn → print_module → volar_dyn_generated.rs
```

### Impact on printer preamble

The hardcoded preamble in `printer.rs` currently contains:
- `use` for core ops, typenum, cipher, digest
- `ByteBlockEncrypt` trait definition and blanket impl
- `pub use volar_primitives::{...}`
- `ilog2` helper function

After this work:
- `ByteBlockEncrypt` is a custom trait from volar-spec (not hardcoded)
- `volar_primitives` types come from the manifest — the preamble writer generates `use` statements from manifest metadata
- `ilog2` stays as a runtime helper in the preamble or moves to `volar-spec-dyn/src/core_types.rs`

The preamble becomes **data-driven**: `DynPreambleWriter { deps: &[TypeManifest] }` generates `use` statements from what's needed.

---

## Part D: Compile `volar-primitives`

### What it contains

1. **Field types** (from macros): `Bit(bool)`, `Galois(u8)`, `BitsInBytes(u8)`, `Galois64(u64)`, `BitsInBytes64(u64)`, `Tropical<T>(T)`
2. **Trait**: `Invert { fn invert(&self) -> Self; }`
3. **Impls**: `Add`, `Mul`, `Sub`, `BitXor<u8>` for each field type; `Invert for Galois`; generic `Add`/`Mul` for `Tropical<T>`
4. **`backend` module**: `FieldMulBackend` trait + `field_mul` generic function

### Strategy: `cargo expand`

The macros (`u8_field!`, `bool_field!`, `u64_field!`) generate struct definitions and trait impls. Rather than teaching the parser about macros, use `cargo expand -p volar-primitives` to get fully expanded source, then parse that.

### Steps

1. `cargo expand -p volar-primitives` → save to temp file
2. Parse with `parse_source()` — may need minor parser fixes for `#[repr(transparent)]`, `#[derive(...)]` attributes
3. `emit_manifest()` → `volar-primitives.volar.d`
4. Test: load manifest, verify structs/traits/impls are present
5. Test: parse volar-spec with the primitives manifest as a dependency

---

## Execution Order

| Phase | What | Part | Est. |
|-------|------|------|------|
| **1** | Remove `VoleArray`/`ByteBlockEncrypt` hardcodes | A | S |
| **2** | Collapse `StructKind` non-`GenericArray` named variants | A2 | S |
| **3** | Refactor printer: `trait RustBackend` + `DisplayRust<T>` + writer structs | B | L |
| **4** | Implement `manifest.rs`: `TypeManifest`, `emit_manifest`, `parse_manifest`, `0xFF` guard | C core | M |
| **5** | `TypeContext::from_module_with_deps` + `LoweringContext::new_with_deps` | C integration | M |
| **6** | Compile `volar-primitives` via `cargo expand`, emit manifest | D | S |
| **7** | Data-driven preamble, end-to-end: volar-spec + primitives manifest | C+D | M |

### Test Gates

- **After Phase 1**: Existing 48 tests pass; VoleArray/ByteBlockEncrypt in volar-spec parse as `Custom(...)`
- **After Phase 2**: All tests pass; `StructKind` simplified
- **After Phase 3**: Printer output is byte-identical to before; `DisplayRust(TypeWriter { ... })` works; all tests pass
- **After Phase 4**: Round-trip test: emit manifest → parse manifest → IrModules match (modulo bodies). `0xFF` guard prevents accidental normal-mode parse.
- **After Phase 5**: volar-spec parses with primitives manifest; `TypeContext` resolves `Galois`, `Bit`, etc.
- **After Phase 6**: `cargo expand` output parses; manifest emitted and loadable
- **After Phase 7**: `lower_module_dyn` with deps produces output; preamble is data-driven
