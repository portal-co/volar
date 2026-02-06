# TypeScript Backend Plan

## Goal

Add a TypeScript code-generation backend to the volar-compiler, so that
volar-spec specifications can be executed in JavaScript/TypeScript environments
(browsers, Node.js, Deno) in addition to the existing Rust dyn backend.

## Current Architecture

```
volar-spec (Rust source)
    │
    ▼
  parser.rs ──► IrModule (generic, with type-level lengths)
    │
    ▼
  lowering_dyn.rs ──► IrModule (dyn: lengths → runtime usize witnesses)
    │
    ▼
  printer.rs ──► Rust source text (generated.rs)
```

The existing pipeline has two backends baked into one printer:

1. **printer.rs** — emits Rust source; used for the `volar-spec-dyn` crate
2. **manifest.rs** — emits `.volar.d` header files (reuses printer writers)

Both share the `RustBackend` trait, which renders IR nodes via
`fmt::Formatter`. The trait is named `RustBackend` and the writer structs
(`ExprWriter`, `TypeWriter`, etc.) all emit Rust syntax.

### Key observations

- **lowering_dyn.rs** is Rust-specific in several ways:
  - Converts `GenericArray<T, N>` → `Vec<T>` + `usize` witness field
  - Emits Rust-specific patterns (`vec![]`, `.collect::<Vec<_>>()`, `PhantomData`)
  - Injects `use` preambles for Rust crates (`cipher`, `digest`, `typenum`)
  - Preserves Rust generics, trait bounds, and where clauses
- **printer.rs** is entirely Rust syntax
- **ir.rs** is language-agnostic in design, but some IR nodes lean Rust:
  - `IrExpr::TypenumUsize` — `<T as Unsigned>::USIZE` (Rust-specific API)
  - `IrExpr::Path` with turbofish — `::<T>` syntax
  - `IrExpr::Cast` with `as` — Rust cast syntax
  - `IterTerminal::Collect` — Rust `.collect()` idiom
  - `IrExpr::ArrayDefault` / `DefaultValue` — `T::default()` convention

## Design

### 1. New module: `printer_ts.rs`

A parallel printer that walks the same `IrModule` and emits TypeScript. This
is a **separate printer**, not a parameterization of the existing one, because
the structural differences between Rust and TypeScript are too large for a
shared template to remain clean.

**Location:** `crates/volar-compiler/src/printer_ts.rs`

### 2. New lowering pass: `lowering_ts.rs`

A TypeScript-specific lowering pass, analogous to `lowering_dyn.rs`. This
transforms the generic IR into a form suitable for TypeScript emission:

- All `GenericArray<T, N>` and `[T; N]` become `T[]` (TypeScript arrays)
- All `Vec<T>` become `T[]`
- Type-level lengths become runtime `number` parameters
- Rust trait bounds become TypeScript generic constraints or are erased
- `PhantomData` fields are dropped entirely
- `typenum::Unsigned::USIZE` calls become direct number references

**Location:** `crates/volar-compiler/src/lowering_ts.rs`

### 3. Public API additions in `lib.rs`

```rust
/// Generate TypeScript source from the generic IR module.
pub fn print_module_typescript(module: &IrModule) -> String {
    let lowered = lowering_ts::lower_module_ts(module);
    printer_ts::print_module_ts(&lowered)
}

pub fn print_module_typescript_with_deps(
    module: &IrModule,
    deps: &[manifest::TypeManifest],
) -> String {
    let lowered = lowering_ts::lower_module_ts_with_deps(module, deps);
    printer_ts::print_module_ts(&lowered)
}
```

### 4. Generation example: `generate_volar_ts.rs`

Parallel to `generate_volar_dyn.rs`, produces a `.ts` file:

**Location:** `crates/volar-compiler/examples/generate_volar_ts.rs`

### 5. (Optional future) Dedicated output crate: `volar-spec-ts/`

A workspace member that ships the generated `.ts` file and an npm
`package.json`. Out of scope for this phase — can be a follow-up.

---

## TypeScript mapping details

### 3.1. Types

| Rust IR type | TypeScript type |
|---|---|
| `bool` | `boolean` |
| `u8`, `u32`, `u64`, `usize` | `number` |
| `i128` | `bigint` |
| `Bit` | `Bit` (imported class) |
| `Galois` | `Galois` (imported class) |
| `Galois64` | `Galois64` (imported class) |
| `BitsInBytes` | `BitsInBytes` (imported class) |
| `BitsInBytes64` | `BitsInBytes64` (imported class) |
| `Vec<T>`, `GenericArray<T,N>`, `[T; N]` | `T[]` |
| `[T]` (slice) | `readonly T[]` |
| `&T`, `&mut T` | `T` (references erased) |
| `(A, B)` | `[A, B]` (TypeScript tuple) |
| `()` | `void` / `undefined` |
| `Option<T>` | `T \| undefined` |
| `String`, `&str` | `string` |
| `PhantomData<T>` | *(omitted)* |

### 3.2. Expressions

| Rust IR expression | TypeScript emission |
|---|---|
| `IrExpr::Var(v)` | `v` |
| `IrExpr::Lit(Int(n))` | `n` (or `BigInt(n)` if i128) |
| `IrExpr::Binary { op, .. }` | Same infix operators (mostly identical) |
| `IrExpr::Unary { Neg/Not, .. }` | `-expr` / `!expr` |
| `IrExpr::Unary { Deref, .. }` | `expr` (erase) |
| `IrExpr::Unary { Ref/RefMut, .. }` | `expr` (erase) |
| `IrExpr::MethodCall { .. }` | `receiver.method(args)` |
| `IrExpr::Call { func, args }` | `func(args)` |
| `IrExpr::Field { base, field }` | `base.field` |
| `IrExpr::Index { base, index }` | `base[index]` |
| `IrExpr::Cast { expr, ty }` | `(expr as unknown as Ty)` or special cases |
| `IrExpr::Path { segments, .. }` | `segments.join(".")` (no turbofish) |
| `IrExpr::StructExpr { kind, fields }` | `new Kind({ field: val, ... })` or `{ field: val }` |
| `IrExpr::Tuple(elems)` | `[e0, e1, ...]` |
| `IrExpr::Array(elems)` | `[e0, e1, ...]` |
| `IrExpr::Repeat { elem, len }` | `Array.from({length: len}, () => elem)` |
| `IrExpr::BoundedLoop` | `for (let v = start; v < end; v++) { ... }` |
| `IrExpr::IterLoop` | `for (const pat of collection) { ... }` |
| `IrExpr::If` | `if (cond) { ... } else { ... }` |
| `IrExpr::Match` | `switch` or `if/else if` chain |
| `IrExpr::Block` | IIFE `(() => { ... })()` or just `{ ... }` |
| `IrExpr::Closure` | `(params) => body` |
| `IrExpr::Return` | `return expr` |
| `IrExpr::Assign` | `left = right` |
| `IrExpr::AssignOp` | `left += right` (etc.) |
| `IrExpr::Range { start, end }` | Lowered away before TS printer |
| `IrExpr::TypenumUsize` | Direct number variable reference |
| `IrExpr::Unreachable` | `throw new Error("unreachable")` |
| `IrExpr::Try` | `expr` (error handling TBD) |

### 3.3. Iterator pipelines

The `IrIterChain` nodes are the richest translation challenge:

| Rust iterator idiom | TypeScript emission |
|---|---|
| `.iter()` / `.into_iter()` | *(identity — arrays are already iterable)* |
| `.map(\|x\| body)` | `.map((x) => body)` |
| `.filter(\|x\| body)` | `.filter((x) => body)` |
| `.filter_map(\|x\| body)` | `.map(x => body).filter(x => x !== undefined)` |
| `.flat_map(\|x\| body)` | `.flatMap((x) => body)` |
| `.enumerate()` | `.map((val, i) => [i, val])` |
| `.take(n)` | `.slice(0, n)` |
| `.skip(n)` | `.slice(n)` |
| `.chain(other)` | `[...left, ...right]` or `.concat(other)` |
| `.zip(other)` | `left.map((a, i) => [a, other[i]])` |
| `.collect()` | *(identity — already an array after map/filter)* |
| `.fold(init, \|acc, x\| body)` | `.reduce((acc, x) => body, init)` |

Since volar-spec arrays are always finite and materialized (no lazy
evaluation needed for correctness), all iterator chains can be eagerly
evaluated using JavaScript's built-in `Array.prototype` methods.

### 3.4. Structs

Rust structs become TypeScript classes (or interfaces + factory functions):

```typescript
// Rust:  pub struct VopeDyn<T> { pub n: usize, pub k: usize, pub u: Vec<Vec<T>>, pub v: Vec<T> }
// TS:
export class VopeDyn<T> {
  constructor(
    public n: number,
    public k: number,
    public u: T[][],
    public v: T[],
  ) {}
}
```

Tuple structs become classes with positional fields named `_0`, `_1`, etc.

Fields that are `PhantomData<...>` in the Rust source are omitted.

### 3.5. Impl blocks & trait impls

TypeScript has no traits. The approach:

1. **Inherent impls** → methods on the class
2. **Trait impls for operator traits** (Add, Mul, BitXor, etc.) → named
   methods on the class (`.add(rhs)`, `.mul(rhs)`, `.bitxor(rhs)`)
3. **Binary operator expressions** `a + b` where the operands are field
   element types → `a.add(b)` (since JS doesn't support operator overloading)
4. **Trait impls for conversion traits** (Into, AsRef, Clone) → `.into()`,
   `.asRef()`, `.clone()` methods
5. **Trait impls for crypto traits** (BlockEncrypt, Digest) → interface
   declarations that the caller provides

### 3.6. Generics & type parameters

TypeScript generics work differently from Rust:

- **Type parameters** remain as `<T>` in TS
- **Trait bounds** become TypeScript `extends` constraints where possible:
  ```typescript
  // Rust: fn foo<T: Add<Output=T> + Mul<Output=T>>()
  // TS:   function foo<T extends FieldElement>()
  ```
  Where `FieldElement` is a TS interface defining `.add()`, `.mul()`, etc.
- **Length parameters** (typenum) become runtime `number` parameters
  (same strategy as the Rust dyn lowering)
- **Where clauses** are mostly erased (TS doesn't have them)

### 3.7. Domain-specific types (field elements)

The volar-primitives field types (`Bit`, `Galois`, `Galois64`, `BitsInBytes`,
`BitsInBytes64`) need TypeScript implementations. These are small wrapper
classes:

```typescript
export class Galois {
  constructor(public readonly value: number) {}
  add(rhs: Galois): Galois { return new Galois(this.value ^ rhs.value); }
  mul(rhs: Galois): Galois { return new Galois(gf256_mul(this.value, rhs.value)); }
  bitxor(rhs: Galois): Galois { return new Galois(this.value ^ rhs.value); }
  clone(): Galois { return new Galois(this.value); }
  static default(): Galois { return new Galois(0); }
  into<U>(): U { /* type-dependent conversion */ }
}
```

These should live in a separate TS support library (e.g., `volar-primitives.ts`)
that the generated code imports.

### 3.8. Crypto trait interfaces

The generated TS code needs to call `BlockEncrypt` and `Digest` methods.
These become TypeScript interfaces that the user provides implementations for:

```typescript
export interface BlockEncrypt {
  encryptBlock(block: Uint8Array): Uint8Array;
  readonly blockSize: number;
}

export interface Digest {
  update(data: Uint8Array): void;
  finalize(): Uint8Array;
  readonly outputSize: number;
}
```

### 3.9. Helper functions

Functions currently in the Rust preamble that need TS equivalents:

| Rust helper | TypeScript equivalent |
|---|---|
| `ilog2(x)` | `Math.floor(Math.log2(x))` |
| `double_vec::<B>(v)` | Caller-provided `LengthDoubler.double(v)` |
| `commit(...)` | Caller-provided `commit(...)` |
| `x.wrapping_add(y)` | `(x + y) >>> 0` or `(x + y) & 0xFFFFFFFF` |
| `x.wrapping_sub(y)` | `(x - y + 2**32) & 0xFFFFFFFF` |
| `x.clone()` | Copy for primitives; `.clone()` method for classes |
| `x.bitxor(y)` | `x.bitxor(y)` (method call on field elements) |

---

## Implementation phases

### Phase 1: lowering_ts.rs (core transform)

Reuse the strategy from `lowering_dyn.rs` but target TypeScript conventions:

1. Build `StructInfo` the same way (classify generics as length vs type)
2. Transform structs: drop `PhantomData`, convert lengths to `number` fields
3. Transform exprs:
   - `TypenumUsize { ty }` → `Var(witness_name)`
   - `ArrayDefault { elem_ty, len }` → `Array.from(...)` expression
   - `DefaultValue { ty }` → `Type.default()` static call
   - `LengthOf(len)` → numeric variable reference
   - Binary ops on field-element types → method calls (`.add()`, `.bitxor()`)
4. Transform iterator pipelines → array method chains

The lowering pass operates on `IrModule → IrModule` (same IR, different
shape), just like `lowering_dyn.rs`.

### Phase 2: printer_ts.rs (code emitter)

New trait `TsBackend` (or reuse a renamed generic `Backend` trait) with
writer structs:

- `TsModuleWriter` — emits `export class`, `export function`, `export interface`
- `TsTypeWriter` — maps `IrType` → TS type syntax
- `TsExprWriter` — maps `IrExpr` → TS expression syntax
- `TsBlockWriter` — `{ stmt; stmt; expr }` with TS semicolons
- `TsPatternWriter` — destructuring patterns `const [a, b] = ...`
- `TsIterChainWriter` — `.map()`, `.filter()`, `.reduce()`

### Phase 3: Integration & testing

1. Add `generate_volar_ts.rs` example (parallel to `generate_volar_dyn.rs`)
2. Add snapshot tests comparing generated TS to expected output
3. Optional: run generated TS through `tsc --noEmit` in CI to verify
   it type-checks

### Phase 4: Runtime support library

Create `volar-primitives.ts` (or `volar-runtime.ts`) with:
- Field element classes (`Bit`, `Galois`, `Galois64`, `BitsInBytes`,
  `BitsInBytes64`)
- Crypto trait interfaces (`BlockEncrypt`, `Digest`)
- Helper utilities (`ilog2`, wrapping arithmetic)

---

## File inventory

| File | Status | Description |
|---|---|---|
| `src/lowering_ts.rs` | **new** | TS-specific IR lowering pass |
| `src/printer_ts.rs` | **new** | TS code emitter |
| `src/lib.rs` | **modify** | Add `pub mod lowering_ts; pub mod printer_ts;` and public API functions |
| `examples/generate_volar_ts.rs` | **new** | TS generation driver |
| `docs/TYPESCRIPT_BACKEND_PLAN.md` | **new** | This document |
| `tests/typescript_output.rs` | **new** | Snapshot tests for TS output |

External (optional, future phase):
| `crates/volar-spec-ts/` | **new** | npm-publishable package with generated TS |
| `crates/volar-spec-ts/src/volar-primitives.ts` | **new** | Runtime support lib |

---

## Design decisions to discuss

### Q1: Classes vs interfaces + factory functions?

**Option A (classes):** Each struct becomes a `class` with a constructor.
Methods from impl blocks become class methods. Pro: familiar OOP pattern,
natural `new`. Con: no structural typing — you can't pass a plain object
where a class is expected.

**Option B (interfaces + factories):** Each struct becomes an `interface`
and a `create_Foo(...)` factory function. Pro: structural typing, more
"TypeScript-idiomatic". Con: more boilerplate, methods need to be standalone
functions or in a namespace.

**Recommendation:** Option A (classes). The generated code is not meant to
be subclassed or structurally duck-typed; it's a faithful port of the Rust
structs.

### Q2: How to handle operator overloading?

JavaScript has no operator overloading. When the IR contains
`IrExpr::Binary { op: Add, left, right }` and the operands are field
element types, the printer must emit `left.add(right)` instead of
`left + right`.

**Approach:** The TS lowering pass transforms `Binary` nodes on non-primitive
types into `MethodCall` nodes. Primitive numeric types (`u8`, `u32`, etc.)
keep infix operators since TS `number` supports them natively. This requires
type information — the lowering pass needs to know the types of sub-expressions.

**Fallback:** If type info is unavailable, emit a helper like
`fieldAdd(a, b)` that dispatches at runtime. This is less efficient but
always correct.

### Q3: BigInt for u64 / i128?

JavaScript `number` is an IEEE 754 double, which can represent integers
exactly up to 2^53. Values of type `u64` and `i128` can exceed this.

**Option A:** Use `bigint` for `u64` and `i128`. Pro: correct. Con: bigint
is slower, can't mix with `number` without explicit conversion.

**Option B:** Use `number` everywhere, accept precision loss for large values.
Pro: simple. Con: silently wrong for large u64 values.

**Recommendation:** Use `bigint` for `u64` and `i128`. Use `number` for
`u8`, `u32`, `usize`, `bool`. The field element types wrap `number` (u8) or
`bigint` (u64) internally.

### Q4: Reuse the dyn lowering or write from scratch?

`lowering_dyn.rs` already handles the hard parts: classifying generics,
transforming struct layouts, resolving associated types. We could:

**Option A:** Fork `lowering_dyn.rs` into `lowering_ts.rs` and modify.
Pro: fast to get working. Con: duplication.

**Option B:** Extract a shared lowering core with backend-specific hooks.
Pro: DRY. Con: over-engineering if the backends diverge significantly.

**Option C:** Reuse `lowering_dyn.rs` as-is (since it already produces a
"simplified" IR with runtime lengths), then just add a TS printer.
Pro: minimal new code. Con: the dyn-lowered IR still contains Rust-isms
(`vec![]`, `.collect()`, `PhantomData`, `<T>::default()`).

**Recommendation:** Option C as the starting point, with a thin TS-specific
post-pass. The dyn-lowered IR is 90% of the way there — the printer just
needs to render it as TS instead of Rust. The remaining 10% (PhantomData
erasure, operator desugaring, Rust-specific API calls) can be handled in
the printer or a lightweight post-transform.

### Q5: How to handle the crypto / external trait callbacks?

The generated Rust dyn code imports `cipher::BlockEncrypt`, `digest::Digest`,
etc. The TS code can't import Rust crates.

**Approach:** Generate TypeScript interfaces for external traits, and require
the caller to provide implementations:

```typescript
// Generated:
export interface DigestProvider {
  new(): DigestState;
}
export interface DigestState {
  update(data: Uint8Array): void;
  finalize(): Uint8Array;
  readonly outputSize: number;
}

// Usage:
import { createVerifier } from './generated';
const verifier = createVerifier({ digest: Sha256Provider, cipher: Aes128Provider });
```

### Q6: Should the TS output be a single file or multiple modules?

**Recommendation:** Single file for now (matching the Rust dyn approach).
The generated code is ~920 lines of Rust; the TS equivalent will be similar
in size. A single `generated.ts` is simplest to consume.

---

## Risks and mitigations

| Risk | Mitigation |
|---|---|
| JS `number` precision loss for u64 | Use `bigint` for u64/i128 types |
| No operator overloading in JS | Transform binary ops to method calls in lowering |
| Complex Rust generics don't map cleanly to TS | The dyn lowering already monomorphizes most of it; remaining generics are simple `<T>` |
| Closure capture semantics differ | Volar-spec closures are pure (no mutation); direct translation to arrow functions is safe |
| Iterator semantics (lazy vs eager) | All arrays are finite and bounded; eager `.map()/.filter()` is correct |
| `PhantomData` has no TS equivalent | Drop it in lowering (it carries no runtime data) |
| `wrapping_add`/`wrapping_sub` overflow | Emit explicit `& 0xFFFFFFFF` masks for u32, `& 0xFFn` for u64 bigint |

---

## Example: expected output

**Input (Rust IR, post-dyn-lowering):**
```rust
pub struct DeltaDyn<T> {
    pub n: usize,
    pub delta: Vec<T>,
}

impl<T> DeltaDyn<T> {
    pub fn remap<F: FnMut(usize) -> usize>(&self, m: usize, f: F) -> DeltaDyn<T>
    where T: Clone {
        DeltaDyn {
            delta: (0..m).map(|i| self.delta[(f(i) % self.n)].clone()).collect::<Vec<T>>(),
            n: 0,
        }
    }
}
```

**Output (TypeScript):**
```typescript
export class DeltaDyn<T extends Cloneable> {
  constructor(
    public n: number,
    public delta: T[],
  ) {}

  remap(m: number, f: (i: number) => number): DeltaDyn<T> {
    return new DeltaDyn<T>(
      0,
      Array.from({ length: m }, (_, i) => this.delta[f(i) % this.n].clone()),
    );
  }
}
```
