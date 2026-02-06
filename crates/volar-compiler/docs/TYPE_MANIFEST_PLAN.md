# Type Manifest Files & Cleanup Plan

## Goals

1. **Type manifest files** (`.volar.d` — like `.d.ts`): enable cross-crate compilation by emitting and consuming type-only descriptions of compiled crates. First target: compile `volar-primitives` and depend on it when compiling `volar-spec`.
2. **Remove stale hardcodes**: `VoleArray` and `ByteBlockEncrypt` are `volar-spec`-defined traits that were hardcoded as `CryptoTrait` enum variants. Remove them.
3. **Refactor the printer**: Replace `write!(String, ...)` with `core::fmt::Write` trait + a `Display` adapter, making the printer usable with any `Write` sink.

---

## Part A: Remove Stale Hardcodes

### A1. Remove `CryptoTrait::VoleArray` and `CryptoTrait::ByteBlockEncrypt`

**Files changed:** `ir.rs`, `const_analysis.rs`, `lowering_dyn.rs`, `printer.rs`

1. **`ir.rs`** — Remove `VoleArray` and `ByteBlockEncrypt` from `CryptoTrait` enum and `from_path`. Remove their entries from `builtin_trait_defs()`.
2. **`const_analysis.rs`** — In `is_length_bound()`, `VoleArray` was treated as a length bound. After removal, if `volar-spec` defines `trait VoleArray<T>: ArrayLength<T>`, parsing will produce `TraitKind::Custom("VoleArray")`. We need `is_length_bound` to check custom trait supertraits — but that requires cross-module analysis we don't have yet. **Interim approach:** keep a lightweight "known length aliases" list (just `["VoleArray"]`) that can be fed externally (from a manifest or from the module's own trait definitions). In `is_crypto_bound`, remove `ByteBlockEncrypt`; it becomes `Custom("ByteBlockEncrypt")` and won't match anything special (correct — it's a user-defined trait).
3. **`lowering_dyn.rs`** — Remove the `ByteBlockEncrypt` blanket-impl skip and `VoleArray` impl skip. These should become data-driven: traits from a dependency manifest that are "marker" or "alias" traits can be marked as skip-in-lowering. For now, replace the hardcoded `CryptoTrait::VoleArray`/`ByteBlockEncrypt` checks with `TraitKind::Custom("VoleArray")`/`Custom("ByteBlockEncrypt")` string matches, then delete them once manifests provide the info.
4. **`printer.rs`** — Remove the hardcoded `ByteBlockEncrypt` trait+blanket-impl preamble and the hardcoded `volar_primitives` re-export. These become part of the manifest-driven import system (Part C).

### A2. Collapse `StructKind` to just `Custom(String)` + `GenericArray`

The named variants (`Delta`, `Q`, `Vope`, `BitVole`, `ABO`, `ABOOpening`, `CommitmentCore`, `Poly`, `PolyInputPool`) are never pattern-matched anywhere — they're purely string tags. Collapsing them simplifies the IR and makes it clear that struct names are just strings.

**Keep `GenericArray`** as a named variant because it has special semantics in the lowering (it's a type-level-sized array, not a user struct).

**Files changed:** `ir.rs` (enum + `from_str` + `Display`)

---

## Part B: Refactor the Printer to use `core::fmt::Write`

### Motivation

The current printer writes to `&mut String` via `write!(out, ...)` where `out: &mut String`. This works because `String` implements `core::fmt::Write`, but:
- The API is locked to `String` — can't write to a file, `Vec<u8>`, or network stream
- `print_module` returns `String`; there's no way to write to a pre-existing buffer
- The hardcoded preamble in `print_module` mixes output policy with IR printing

### Design

1. Create a `IrWriter` trait that extends `core::fmt::Write`:

```rust
/// Trait for writing IR to any fmt::Write sink.
pub trait IrWriter: core::fmt::Write {
    fn write_module(&mut self, module: &IrModule) -> fmt::Result;
    fn write_struct(&mut self, s: &IrStruct) -> fmt::Result;
    fn write_trait(&mut self, t: &IrTrait) -> fmt::Result;
    fn write_impl(&mut self, i: &IrImpl) -> fmt::Result;
    fn write_function(&mut self, f: &IrFunction) -> fmt::Result;
    fn write_type(&mut self, ty: &IrType) -> fmt::Result;
    fn write_expr(&mut self, expr: &IrExpr) -> fmt::Result;
}
```

Wait — this is over-engineered. The printer logic is complex and monolithic. A better approach:

2. **Change all `fn write_*(out: &mut String, ...)` → `fn write_*(out: &mut dyn fmt::Write, ...)`**. This is a mechanical refactor: `&mut String` → `&mut dyn fmt::Write`. Every call site already uses `write!`/`writeln!` which work with `fmt::Write`.

3. **Add a `Display` wrapper**:

```rust
/// Wrapper that makes any IR-printable item implement Display.
pub struct DisplayModule<'a>(pub &'a IrModule);

impl<'a> fmt::Display for DisplayModule<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_module_body(f, self.0)
    }
}
```

4. **Split `print_module` into**:
   - `write_module_body(out: &mut dyn Write, module: &IrModule) -> fmt::Result` — writes structs, traits, impls, functions (no preamble)
   - `print_module(module: &IrModule) -> String` — convenience wrapper that calls `write_module_body` (backward compat)
   - Move the hardcoded preamble out of the printer into the dyn codegen pipeline (`printer_dyn.rs` or a new `codegen_dyn.rs`)

5. **Error handling**: Change `.unwrap()` calls to `?` propagation. All internal write functions return `fmt::Result`.

### Files changed
- `printer.rs` — all functions
- `printer_dyn.rs` — use the new `write_module_body` API

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

### Format

**Text-based**, using the existing printer to emit a `.volar.d` file that looks like valid Rust with `todo!()` method bodies. This means:
- We can re-parse it with the existing parser (round-trip!)
- Human-readable and diffable
- No need for serde or a binary format initially

The file has a header comment identifying it as a manifest:

```rust
//! @volar-manifest volar-primitives 0.1.0
//! @volar-deps []

// Struct definitions
pub struct Bit(pub bool);
pub struct Galois(pub u8);
// ...

// Trait definitions  
pub trait Invert {
    fn invert(&self) -> Self;
}

// Impl blocks (signatures only)
impl Add for Galois {
    type Output = Galois;
    fn add(self, rhs: Galois) -> Galois { todo!() }
}
impl Invert for Galois {
    fn invert(&self) -> Self { todo!() }
}
```

### Manifest Struct

```rust
/// Metadata for a compiled crate's type manifest.
#[derive(Debug, Clone)]
pub struct TypeManifest {
    pub crate_name: String,
    pub version: String,
    pub deps: Vec<String>,      // names of dependency manifests needed
    pub module: IrModule,        // structs, traits, impls, fn sigs (no bodies)
}
```

### Operations

1. **`emit_manifest(module: &IrModule, crate_name: &str, version: &str) -> String`**
   - Strips all function/method bodies (replaces with `IrBlock { stmts: [], expr: Some(todo!()) }`)
   - Writes the header + module using the printer
   
2. **`parse_manifest(source: &str) -> Result<TypeManifest, ...>`**
   - Parses the header comment for metadata
   - Parses the body as normal Rust via the existing parser
   - Returns a `TypeManifest`

3. **`TypeContext::from_module_with_deps(module: &IrModule, deps: &[TypeManifest]) -> TypeContext`**
   - Registers structs/traits/impls from all dependency manifests first
   - Then registers the module's own definitions
   - Dependency types get a `crate_name::` prefix in lookup when needed

4. **`LoweringContext::new_with_deps(module: &IrModule, deps: &[TypeManifest]) -> LoweringContext`**
   - Populates `struct_info` for dependency structs too
   - Allows lowering to know the generic structure of dependency types

### Workflow

```
volar-primitives/src/*.rs
    → parse → IrModule
    → emit_manifest → volar-primitives.volar.d

volar-spec/src/*.rs
    → parse → IrModule
    → parse_manifest("volar-primitives.volar.d") → TypeManifest
    → TypeContext::from_module_with_deps(module, [primitives_manifest])
    → LoweringContext::new_with_deps(module, [primitives_manifest])
    → lower_module_dyn → print_module → volar_dyn_generated.rs
```

### Impact on printer preamble

The hardcoded preamble in `printer.rs` currently has:
- `use` for core ops, typenum, cipher, digest
- `ByteBlockEncrypt` trait definition and blanket impl
- `pub use volar_primitives::{...}`
- `ilog2` helper function

After manifests:
- `ByteBlockEncrypt` comes from manifest (it will be a custom trait in volar-spec)
- `volar_primitives` types come from the manifest — the printer emits `use` statements based on manifest dependencies
- `ilog2` moves to the runtime support module, not hardcoded in the printer

The preamble becomes **data-driven**: `write_dyn_preamble(out, deps: &[TypeManifest])` generates the `use` statements from what's actually needed.

---

## Part D: Compile `volar-primitives`

### What volar-primitives contains

1. **Field types**: `Bit(bool)`, `Galois(u8)`, `BitsInBytes(u8)`, `Galois64(u64)`, `BitsInBytes64(u64)`, `Tropical<T>(T)`
2. **Trait**: `Invert { fn invert(&self) -> Self; }`
3. **Impls**: `Add`, `Mul`, `Sub`, `BitXor<u8>` for each field type; `Invert for Galois`; generic `Add`/`Mul` for `Tropical<T>`
4. **`backend` module**: `FieldMulBackend` trait + `field_mul` generic function

### Challenges

1. **Macros**: `u8_field!`, `bool_field!`, `u64_field!` macros generate the types. The parser needs to either:
   - (a) Expand macros before parsing (syn doesn't do this)
   - (b) Parse the macro-expanded output (`cargo expand`)
   - (c) Parse each macro invocation site and expand manually
   
   **Recommended: (b)** — use `cargo expand` to get the expanded source, then parse it. This is the most reliable approach. We can add a `expand_and_parse` helper.

2. **The `backend::field_mul` function** uses complex trait bounds (`ShlAssign<u32> + ShrAssign<u32> + ...`). These are standard math/operator traits and should parse fine. The function body has a bounded loop (`for _ in 0..(size_of_val(&p) << 3)`) and bitwise operations — all supported by the IR.

3. **`size_of_val`** — a `core::mem` function. The parser would need to handle `core::mem::size_of_val` as an external function call.

### Steps

1. Use `cargo expand -p volar-primitives` to get expanded source
2. Parse with existing parser (may need minor parser fixes for patterns like `repr(transparent)`)
3. Emit manifest: `volar-primitives.volar.d`
4. Test: parse volar-spec with the manifest as a dependency

---

## Execution Order

| Phase | What | Est. Size |
|-------|------|-----------|
| **1** | Remove `VoleArray`/`ByteBlockEncrypt` hardcodes (Part A) | S |
| **2** | Refactor printer to `fmt::Write` (Part B) | M |
| **3** | Implement `TypeManifest` struct + emit/parse (Part C core) | M |
| **4** | `TypeContext::from_module_with_deps` + `LoweringContext` deps (Part C integration) | M |
| **5** | Compile `volar-primitives` via `cargo expand`, emit manifest (Part D) | S |
| **6** | Data-driven preamble, end-to-end test: compile volar-spec with primitives manifest | M |

Each phase has a clear test gate before proceeding to the next.

---

## Test Strategy

- **Phase 1**: Existing tests still pass; `VoleArray`/`ByteBlockEncrypt` in volar-spec sources parse as `Custom(...)` instead of `Crypto(...)` — update assertions
- **Phase 2**: All printer output is byte-identical (regression test); new tests for `Display` wrapper and `fmt::Write` usage
- **Phase 3**: Round-trip test: emit manifest → parse manifest → compare `IrModule` (modulo body erasure)
- **Phase 4**: Parse volar-spec with primitives manifest → `TypeContext` resolves `Galois`, `Bit`, etc. → no "unknown struct" warnings
- **Phase 5**: `cargo expand` output parses cleanly; manifest emitted
- **Phase 6**: `lower_module_dyn` with deps produces compilable Rust code
