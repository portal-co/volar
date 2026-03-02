# volar-compiler

`volar-compiler` (`crates/volar-compiler`) is the compiler that reads
`volar-spec` Rust source and produces dynamic Rust or TypeScript output.

---

## Quick Start

```bash
# Generate dynamic Rust
cargo run --example generate_volar_dyn --features parsing

# Generate TypeScript
cargo run --example generate_volar_ts --features parsing

# Dump the IR (pre-lowering)
cargo run --example generate_volar_ts --features parsing -- --dump-ir

# Dump the dynamic IR (post-lowering)
cargo run --example generate_volar_ts --features parsing -- --dump-ir-dyn
```

---

## Modules

```
volar-compiler/src/
├── ir.rs           Intermediate representation (IrModule and all node types)
├── parser.rs       syn-based parser: Rust AST → IrModule
├── const_analysis.rs  Generic parameter classification (Length vs Type)
├── lowering.rs     TypeContext, OperatorAnalysis, type resolution utilities
├── lowering_dyn.rs Dynamic lowering: type-level lengths → runtime usize witnesses
├── manifest.rs     Type manifest (.volar.d) serialization/deserialization
├── printer.rs      Rust code printer (IrModule → Rust source)
├── printer_ts.rs   TypeScript code printer (IrModule → TypeScript source)
├── deshadow.rs     Rename self-shadowing bindings (let x = f(x) → let x_1 = f(x))
├── dump_ir.rs      Human-readable IR dump for debugging
└── lib.rs          Public API surface
```

---

## Intermediate Representation (`ir.rs`)

The IR is a typed, domain-aware representation of a subset of Rust. All IR
nodes are plain Rust enums/structs — no trait objects, no Box<dyn>.

### IrModule

The top-level container:

```rust
pub struct IrModule {
    pub name: String,
    pub structs:      Vec<IrStruct>,
    pub traits:       Vec<IrTrait>,
    pub impls:        Vec<IrImpl>,
    pub functions:    Vec<IrFunction>,
    pub type_aliases: Vec<IrTypeAlias>,
}
```

### Type System (`IrType`)

| Variant | Description |
|---|---|
| `Primitive(PrimitiveType)` | `bool`, `u8`, `u32`, `u64`, `usize`, `i128`, `Bit`, `Galois`, … |
| `Array { kind, elem, len }` | `GenericArray<T,N>`, `[T;N]`, or `[T]` slice |
| `Vector { elem }` | `Vec<T>` |
| `Struct { kind, type_args }` | Named struct with type arguments |
| `TypeParam(String)` | A generic type parameter (e.g. `T`, `B`) |
| `Tuple(Vec<IrType>)` | `(A, B, C)` |
| `Unit` | `()` |
| `Reference { mutable, elem }` | `&T` or `&mut T` |
| `Projection { base, assoc, .. }` | `<T as Trait>::Assoc` |
| `Existential { bounds }` | `impl Trait` |
| `FnPtr { params, ret }` | `fn(A, B) -> C` |
| `Never` | `!` |
| `Infer` | `_` (type hole) |

`ArrayLength` is represented as a nested enum:

```rust
pub enum ArrayLength {
    Const(usize),           // literal size
    TypeNum(TypeNumConst),  // typenum constant: U0, U1, U2, U8, U16, U32, U64
    TypeParam(String),      // generic parameter N
    Projection { type, field, trait_path }, // <T as Trait>::OutputSize
}
```

### Primitive Types

`PrimitiveType` distinguishes standard Rust scalars from the domain-specific
field element types:

| Type | Kind | Notes |
|---|---|---|
| `Bool`, `U8`, `U32`, `U64`, `Usize`, `I128` | Standard Rust scalars | — |
| `Bit` | GF(2) | 1-bit field element |
| `Galois` | GF(2⁸) | AES S-box field |
| `Galois64` | GF(2⁶⁴) | 64-bit extension |
| `BitsInBytes` | GF(2)⁸ packed | 8-lane SIMD bit field |
| `BitsInBytes64` | GF(2)⁶⁴ packed | 64-lane SIMD bit field |

### Expressions (`IrExpr`)

Key expression variants:

| Variant | Description |
|---|---|
| `Lit(IrLit)` | Integer, float, bool, char, string, bytes |
| `Var(String)` | Variable reference |
| `Path { segments, type_args }` | `Foo::bar::<T>` paths |
| `Binary { op, left, right }` | Arithmetic/logical binary ops |
| `Unary { op, expr }` | Negation, not, deref, ref |
| `MethodCall { receiver, method, … }` | `x.method(args)` |
| `Call { func, args }` | Function call |
| `Field { base, field }` | Field access `x.field` |
| `Index { base, index }` | Indexing `x[i]` |
| `StructExpr { kind, fields, … }` | Struct literal `Foo { a: 1, b: 2 }` |
| `Tuple(Vec<IrExpr>)` | Tuple literal `(a, b)` |
| `Array(Vec<IrExpr>)` | Array literal `[a, b, c]` |
| `Repeat { elem, len }` | `[v; N]` |
| `ArrayGenerate { len, index_var, body }` | `GenericArray::generate(\|i\| …)` |
| `DefaultValue { ty }` | `T::default()` — recursive for array types |
| `LengthOf(ArrayLength)` | Type-level length → runtime `usize` |
| `IterPipeline(IrIterChain)` | Flat iterator pipeline (see below) |
| `RawMap { receiver, elem_var, body }` | `array.map(\|x\| …)` — NOT an iterator |
| `RawZip { left, right, … }` | `array.zip(other, \|a,b\| …)` — NOT an iterator |
| `RawFold { receiver, init, … }` | `array.fold(init, \|acc,x\| …)` — NOT an iterator |
| `BoundedLoop { var, start, end, body }` | `for i in start..end { … }` |
| `IterLoop { pattern, collection, body }` | `for x in collection { … }` |
| `Block(IrBlock)` | Block expression `{ … }` |
| `If { cond, then_branch, else_branch }` | Conditional |
| `Match { expr, arms }` | Pattern matching |
| `Closure { params, body }` | `\|x\| body` |
| `Cast { expr, ty }` | `expr as T` |
| `Return`, `Break`, `Continue` | Control flow |
| `Assign`, `AssignOp` | Assignment |
| `TypenumUsize { ty }` | `<T as Unsigned>::to_usize()` → runtime number |
| `Unreachable` | `unreachable!()` |

### Iterator Pipelines (`IrIterChain`)

Iterator chains are represented as a **flat** structure to make them
backend-neutral:

```rust
pub struct IrIterChain {
    pub source: IterChainSource,
    pub steps:  Vec<IterStep>,
    pub terminal: IterTerminal,
}
```

Sources: `Method { collection, method }` (`.iter()`, `.into_iter()`, etc.),
`Range { start, end, inclusive }`, `Zip { left, right }`.

Steps: `Map`, `Filter`, `FilterMap`, `FlatMap`, `Enumerate`, `Take`, `Skip`, `Chain`.

Terminals: `Collect`, `CollectTyped(IrType)`, `Fold { init, acc_var, elem_var, body }`, `Lazy`.

`RawMap`/`RawZip`/`RawFold` are distinct from `IterPipeline` — they represent
direct element-wise operations on `GenericArray` or `[T;N]` without going
through an iterator. Both Rust and TypeScript backends emit them differently.

---

## Totality

The compiler enforces **totality** — all accepted programs must be provably
terminating. The following are rejected:

- `while` loops (unbounded).
- Bare `loop` constructs.
- Recursive function calls (not currently checked but architecturally unsupported).

Accepted iteration forms:
- `for i in start..end` / `for i in start..=end` → `BoundedLoop`.
- `for x in collection` → `IterLoop` (collection must be a finite array/vec).
- Iterator pipeline with `Collect` or `Fold` terminal → `IterPipeline`.

This mirrors the totality requirement of the Volar IR (see [ir-lowering.md](ir-lowering.md)).

---

## Parser (`parser.rs`)

The parser uses [`syn`](https://docs.rs/syn) to convert Rust source into
`IrModule`. It processes:

- `struct` items → `IrStruct`
- `trait` items → `IrTrait`
- `impl` items → `IrImpl`
- `fn` items → `IrFunction`
- `type` aliases → `IrTypeAlias`
- `use` items → silently ignored

It performs on-the-fly specialization:
- Recognizes known trait paths (`Add`, `BlockEncrypt`, `Digest`, …) and maps
  them to the appropriate `TraitKind`.
- Recognizes known method names (`iter`, `map`, `fold`, `collect`, `remap`, …)
  and maps them to `IterStep`, `MethodKind`, or `VoleMethod`.
- Builds `IrIterChain` directly from `syn` method-call chains without a
  separate normalization pass.

---

## Constant Analysis (`const_analysis.rs`)

`const_analysis` classifies generic parameters as either **Length** parameters
(type-level constants that become runtime `usize` after dynamic lowering) or
**Type** parameters (generic type variables that remain generic).

```rust
pub enum GenericKind { Length, Type }
pub fn classify_generic(param, all_params) -> GenericKind
pub fn classify_generic_with_aliases(param, all_params, length_aliases) -> GenericKind
pub fn is_length_bound(bound) -> bool
pub fn is_fn_bound(bound) -> bool
```

A parameter is classified as `Length` if:
- It has kind `Const` (a Rust `const` generic).
- It is bounded by `Unsigned` (from `typenum`).
- It is bounded by `ArrayLength` (from `generic-array`).
- It is bounded by a custom trait that transitively inherits from `ArrayLength`
  (e.g. `VoleArray`), discovered by fixed-point iteration over the module's
  traits.
- Its name alone resolves to a `Length` parameter via the surrounding
  parameter sets (transitive resolution for `type K: Mul<M>` patterns).

---

## Type Resolution (`lowering.rs`)

`TypeContext` provides type resolution, trait lookup, and impl validation:

```rust
pub struct TypeContext {
    pub structs:     BTreeMap<String, IrStruct>,
    pub traits:      BTreeMap<String, IrTrait>,
    pub trait_impls: Vec<IrImpl>,
    pub assoc_types: BTreeMap<(String, AssociatedType), IrType>,
}
```

`TypeContext::from_module_with_deps(module, deps)` builds the context from:
1. Built-in trait definitions (`builtin_trait_defs()` in `ir.rs`) — Add, Sub,
   Mul, PartialEq, Clone, Default, BlockEncrypt, Digest, etc.
2. Dependency manifests (earlier deps take precedence on conflict).
3. The module's own definitions (override everything).

Key methods:

| Method | Description |
|---|---|
| `lookup_trait(kind)` | Look up an `IrTrait` by `TraitKind` |
| `trait_items(kind)` | Get trait items (associated types, method signatures) |
| `validate_impl(imp)` | Check all required trait items are present; returns errors |
| `substitute(ty, mapping)` | Substitute type parameters with concrete types |

---

## Dynamic Lowering (`lowering_dyn.rs`)

Dynamic lowering transforms a static `IrModule` (with type-level lengths) into
a dynamic `IrModule` (with runtime `usize` witnesses).

Entry point:
```rust
pub fn lower_module_dyn(module: &IrModule) -> IrModule
```

### What it does

**Structs:** Length generics are dropped; `usize` fields are prepended for
each length witness (e.g. `Vope<N, T, K>` → `VopeDyn<T>` with fields `n`,
`k: usize`). `PhantomData<T>` is added if any type params are unused in fields.

**Methods:** A `let n = self.n;` unpacking statement is injected at the top of
every method body so the length witnesses are available as local variables.

**Static methods:** Length parameters become explicit `usize` arguments.

**Types:** `GenericArray<T, N>` → `Vec<T>`. `&[T]` in struct fields → `Vec<T>`.
Fixed arrays (`[T; N]`) and slices (`[T]`) are preserved.

**Struct names:** `Foo` → `FooDyn`.

**Expressions:**
- `N::to_usize()` → `n` (the local witness variable).
- `GenericArray::default()` → `vec![]` (empty Vec).
- Nested `DefaultValue { Array }` → `IterPipeline(0..n).map(_ => default(elem))`.
- `encrypt_block(block)` → wraps argument in `Block::from_mut_slice`.

### LoweringContext

`LoweringContext` caches struct information to avoid re-classifying generics:

```rust
pub struct StructInfo {
    pub kind: StructKind,
    pub length_witnesses: Vec<String>,     // lowercase names of length params
    pub type_params:      Vec<(String, Vec<IrTraitBound>)>,
    pub orig_generics:    Vec<(String, GenericKind, IrGenericParamKind)>,
    pub needs_phantom:    bool,
    pub manual_clone:     bool,
}
```

---

## Type Manifests (`manifest.rs`)

A `.volar.d` manifest captures the *signatures* of a compiled crate (struct,
trait, impl, function signatures — no bodies) for use as a dependency.

**Format:** Text-based Rust with `todo!()` bodies, prefixed by a `0xFF` byte
(invalid UTF-8 poison pill) to prevent accidental processing by `rustc` or `syn`.

```rust
pub const MANIFEST_MARKER: u8 = 0xFF;

pub struct TypeManifest {
    pub crate_name: String,
    pub version:    String,
    pub deps:       Vec<String>,
    pub module:     IrModule,
}
```

**`emit_manifest(module, crate_name, version)`** strips all function bodies
(replacing with `todo!()`) and serializes the signatures.

**`parse_manifest(bytes)`** verifies the `0xFF` prefix, strips it, and parses
the body with the normal `syn`-based parser.

---

## Rust Printer (`printer.rs`)

Renders `IrModule` as Rust source via the `RustBackend` trait:

```rust
pub trait RustBackend {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}
pub struct DisplayRust<T: RustBackend>(pub T);
```

Writer structs: `ModuleWriter`, `StructWriter`, `TraitWriter`, `ImplWriter`,
`FunctionWriter`, `TypeWriter`, `ExprWriter`, `BlockWriter`, `PatternWriter`,
`IterChainWriter`, `GenericsWriter`, `WhereClauseWriter`.

Public API:

```rust
// Render a module as a Rust source string
pub fn print_module(module: &IrModule) -> String

// Render with dependency manifests (for import resolution)
pub fn print_module_with_deps(module: &IrModule, deps: &[TypeManifest]) -> String
```

---

## TypeScript Printer (`printer_ts.rs`)

Renders a dyn-lowered `IrModule` as TypeScript. Structs become classes, impl
methods attach to class bodies. See
[compiler/TYPESCRIPT_BACKEND_PLAN.md](compiler/TYPESCRIPT_BACKEND_PLAN.md)
for the full design.

Notable features:
- **Impl merging**: Multiple `impl` blocks with different type specializations
  on the same struct are merged into one class with runtime dispatch via
  `instanceof`.
- **Witness analysis**: Scans function bodies for uses of type-param
  projections (`B::OutputSize`), constructors (`D::new()`), and defaults
  (`O::default()`), and injects a `ctx` parameter to provide these at runtime.
- **Transitive witness propagation**: If function A calls function B that
  needs witnesses, A inherits B's witness requirements (fixpoint loop).
- **`deshadow` pass**: Renames `let x = f(x)` self-shadows to
  `const x_1 = f(x)` before emission.

Public API:
```rust
pub fn print_module_ts(module: &IrModule) -> String
```

---

## Library API (`lib.rs`)

```rust
// Parse volar-spec source → IrModule
pub fn parse_source(source: &str, name: &str) -> Result<IrModule>
pub fn parse_sources(sources: &[(&str, &str)], module_name: &str) -> Result<IrModule>

// Generate dynamic Rust
pub fn print_module_rust_dyn(module: &IrModule) -> String
pub fn print_module_rust_dyn_with_deps(module: &IrModule, deps: &[TypeManifest]) -> String

// Generate TypeScript
pub fn print_module_typescript(module: &IrModule) -> String
pub fn print_module_typescript_with_deps(module: &IrModule, deps: &[TypeManifest]) -> String
```

---

## Test Suite

| Suite | File | Count |
|---|---|---|
| IR parsing | `tests/parse_volar_spec.rs` | 15 |
| Specialized IR | `tests/specialized_ir.rs` | 7 |
| Traits & impls | `tests/traits_and_impls.rs` | 13 |
| Manifests | `tests/manifest.rs` | 45 |
| Lib unit tests | `src/*.rs` (inline) | 41 |
| **Total** | | **121** |
