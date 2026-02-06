# TypeScript Backend Plan

## Goal

Add a TypeScript code-generation backend to the volar-compiler, so that
volar-spec specifications can be executed in JavaScript/TypeScript environments
(browsers, Node.js, Deno) in addition to the existing Rust dyn backend.

## Architecture

```
volar-spec (Rust source)
    │
    ▼
  parser.rs ──► IrModule (generic, with type-level lengths)
    │
    ▼
  lowering_dyn.rs ──► IrModule (dyn: lengths → runtime witnesses)
    │                   (genericized: backend-neutral transform)
    ├──────────────────────────┐
    ▼                          ▼
  printer.rs                 printer_ts.rs
    │                          │
    ▼                          ▼
  Rust source (generated.rs) TypeScript source (generated.ts)
```

### Approach: genericize `lowering_dyn.rs`

The existing `lowering_dyn.rs` already does the hard, language-agnostic work:
classifying generics as length vs type parameters, converting type-level
constants to runtime witnesses, transforming struct layouts, resolving
associated type projections, and rewriting iterator pipelines. All of this
is not Rust-specific — it's the universal "static → dynamic" transform.

The few Rust-isms currently baked in (`Vec<T>`, `PhantomData`, `.collect()`,
`<T>::default()`) will be kept as-is in the IR, and the **printer** is
responsible for rendering them in the target language's syntax. This avoids
duplicating the complex lowering logic.

`lowering_dyn.rs` should be renamed conceptually to "the dyn lowering pass"
and any accidental Rust-specific decisions that leak into the IR (rather than
the printer) should be refactored out over time.

---

## Decisions

| # | Topic | Decision |
|---|---|---|
| 1 | Structs | **Classes** — can be optimized later |
| 2 | Operator overloading | **Helper functions** (`fieldAdd(a, b)`) for non-primitive ops; native infix for `number`/`bigint` |
| 3 | Integer types | **`bigint`** for `u64` and `i128` (crypto needs full range); `number` for `u8`, `u32`, `usize`, `bool` |
| 4 | Lowering reuse | **Genericize `lowering_dyn.rs`** — it converts static type-level constants/params to runtime witnesses; both Rust and TS printers consume its output |
| 5 | Crypto traits | **Separate npm package** in `packages/` with TypeScript `interface` declarations for `BlockEncrypt`, `Digest`, `LengthDoubler`, etc. plus field element classes. Compiler generates `import` statements referencing this package. |
| 6 | Output structure | **Single module** — one `generated.ts` file, matching the Rust printer |

---

## TypeScript mapping details

### Types

| Rust IR type | TypeScript type |
|---|---|
| `bool` | `boolean` |
| `u8`, `u32`, `usize` | `number` |
| `u64` | `bigint` |
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
| `()` | `void` |
| `Option<T>` | `T \| undefined` |
| `PhantomData<T>` | *(omitted from struct fields)* |

### Expressions

| Rust IR expression | TypeScript emission |
|---|---|
| `Var(v)` | `v` |
| `Lit(Int(n))` | `n` (or `BigInt(n)` for i128/u64) |
| `Binary { op, .. }` on primitives | `left op right` (infix) |
| `Binary { op, .. }` on field types | `fieldOp(left, right)` (helper) |
| `Unary { Deref/Ref/RefMut, .. }` | `expr` (erase) |
| `MethodCall { .. }` | `receiver.method(args)` |
| `Call { func, args }` | `func(args)` |
| `Field { base, field }` | `base.field` |
| `Index { base, index }` | `base[index]` |
| `Cast { expr, ty }` | `Number(expr)` / `BigInt(expr)` / `expr as Type` |
| `Path { segments, .. }` | `Segments.joined.by.dots` (no turbofish) |
| `StructExpr { kind, fields }` | `new Kind(field1, field2, ...)` |
| `Tuple(elems)` | `[e0, e1, ...]` |
| `Array(elems)` | `[e0, e1, ...]` |
| `Repeat { elem, len }` | `Array.from({length: len}, () => elem)` |
| `BoundedLoop` | `for (let v = start; v < end; v++) { ... }` |
| `IterLoop` | `for (const pat of collection) { ... }` |
| `If` | `if (cond) { ... } else { ... }` |
| `Match` | `if/else if` chain |
| `Block` (expression position) | IIFE `(() => { ... })()` |
| `Closure` | `(params) => body` |
| `Return` | `return expr` |
| `Assign` | `left = right` |
| `AssignOp` | `left += right` (etc.) |
| `TypenumUsize` | Direct number variable reference |
| `Unreachable` | `throw new Error("unreachable")` |
| `ArrayDefault` | `Array.from({length: n}, () => Type.default())` |
| `DefaultValue` | `Type.default()` |
| `LengthOf` | numeric witness variable |

### Iterator pipelines

| Rust iterator idiom | TypeScript emission |
|---|---|
| `.iter()` / `.into_iter()` | *(identity — TS arrays are already iterable)* |
| `.map(\|x\| body)` | `.map((x) => body)` |
| `.filter(\|x\| body)` | `.filter((x) => body)` |
| `.filter_map(\|x\| body)` | `.map(x => body).filter(x => x !== undefined)` |
| `.flat_map(\|x\| body)` | `.flatMap((x) => body)` |
| `.enumerate()` | `.map((val, i) => [i, val] as [number, T])` |
| `.take(n)` | `.slice(0, n)` |
| `.skip(n)` | `.slice(n)` |
| `.chain(other)` | `.concat(other)` |
| `.zip(other)` | `.map((a, i) => [a, other[i]] as [A, B])` |
| `.collect()` | *(identity — already an array)* |
| `.fold(init, \|acc, x\| body)` | `.reduce((acc, x) => body, init)` |

### Structs → Classes

```typescript
// Rust:  pub struct VopeDyn<T> { pub n: usize, pub k: usize, pub u: Vec<Vec<T>>, pub v: Vec<T> }
export class VopeDyn<T> {
  constructor(
    public n: number,
    public k: number,
    public u: T[][],
    public v: T[],
  ) {}
}
```

- `PhantomData` fields omitted
- Tuple structs: fields named `_0`, `_1`, etc.

### Impl blocks

1. **Inherent impls** → methods on the class
2. **Operator trait impls** (Add, Mul, BitXor, etc.) → named methods (`.add()`, `.mul()`, `.bitxor()`)
3. **Conversion traits** (Into, Clone) → `.into()`, `.clone()` methods
4. **Crypto traits** (BlockEncrypt, Digest) → passed as constructor params or method params; interfaces defined in npm package

### Generics

- **Type parameters** → TS `<T>` generics
- **Trait bounds** → erased or simplified to TS `extends` constraints
- **Length parameters** → runtime `number` parameters (handled by dyn lowering)
- **Where clauses** → erased (TS doesn't have them)

---

## Implementation plan

### Phase 1: `printer_ts.rs`

Create `crates/volar-compiler/src/printer_ts.rs` with writer structs:

- `TsModuleWriter` — emits `import`, `export class`, `export function`
- `TsStructWriter` — struct → class with constructor
- `TsImplWriter` — collects methods, attaches to class body
- `TsFunctionWriter` — `export function name<T>(params): RetType { ... }`
- `TsTypeWriter` — `IrType` → TS type syntax
- `TsExprWriter` — `IrExpr` → TS expression syntax
- `TsBlockWriter` — `{ stmt; stmt; return expr; }`
- `TsStmtWriter` — `let`/`const` declarations, expression statements
- `TsPatternWriter` — destructuring: `const [a, b] = ...`, `const {x, y} = ...`
- `TsIterChainWriter` — `.map()`, `.filter()`, `.reduce()`
- `TsPreambleWriter` — `import` statements for the runtime package

Public API:
```rust
pub fn print_module_ts(module: &IrModule) -> String
```

### Phase 2: `generate_volar_ts.rs` example

- Parses volar-spec sources (reuses existing example logic)
- Runs `lowering_dyn::lower_module_dyn_with_deps()`
- Runs `printer_ts::print_module_ts()`
- Writes to `packages/volar-ts/src/generated.ts`

### Phase 3: Integration in `lib.rs`

```rust
pub mod printer_ts;

pub fn print_module_typescript(module: &IrModule) -> String {
    let lowered = lowering_dyn::lower_module_dyn(module);
    printer_ts::print_module_ts(&lowered)
}

pub fn print_module_typescript_with_deps(
    module: &IrModule,
    deps: &[manifest::TypeManifest],
) -> String {
    let lowered = lowering_dyn::lower_module_dyn_with_deps(module, deps);
    printer_ts::print_module_ts(&lowered)
}
```

### Phase 4: npm runtime package

Directory: `packages/volar-runtime/`

Source files needed (I generate these; you handle `package.json`/build config):
- `packages/volar-runtime/src/primitives.ts` — `Bit`, `Galois`, `Galois64`, `BitsInBytes`, `BitsInBytes64` classes
- `packages/volar-runtime/src/interfaces.ts` — `BlockEncrypt`, `Digest`, `LengthDoubler` interfaces
- `packages/volar-runtime/src/helpers.ts` — `ilog2()`, `fieldAdd()`, `fieldMul()`, wrapping arithmetic
- `packages/volar-runtime/src/index.ts` — re-exports

---

## File inventory

| File | Status | Description |
|---|---|---|
| `crates/volar-compiler/src/printer_ts.rs` | **new** | TS code emitter |
| `crates/volar-compiler/src/lib.rs` | **modify** | Add `pub mod printer_ts;` and public API |
| `crates/volar-compiler/examples/generate_volar_ts.rs` | **new** | TS generation driver |
| `crates/volar-compiler/docs/TYPESCRIPT_BACKEND_PLAN.md` | **new** | This document |
| `packages/volar-runtime/src/primitives.ts` | **new** | Field element classes |
| `packages/volar-runtime/src/interfaces.ts` | **new** | Crypto trait interfaces |
| `packages/volar-runtime/src/helpers.ts` | **new** | Runtime helpers |
| `packages/volar-runtime/src/index.ts` | **new** | Package entry point |

---

## Risks and mitigations

| Risk | Mitigation |
|---|---|
| JS `number` precision for u64 | `bigint` for u64/i128 |
| No operator overloading | Helper functions `fieldAdd(a, b)` etc. |
| Complex Rust generics | Dyn lowering already handles; remaining generics are simple `<T>` |
| Closure capture semantics | Volar-spec closures are pure; arrow functions are safe |
| Iterator lazy vs eager | All arrays finite/bounded; eager `Array.prototype` methods correct |
| `PhantomData` | Omitted in struct emission |
| Wrapping arithmetic overflow | Explicit `& 0xFFFFFFFF` masks / BigInt masks |

---

## Example: expected output

**Input (dyn-lowered IR, printed as Rust):**
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
import { type Cloneable } from "volar-runtime";

export class DeltaDyn<T extends Cloneable> {
  constructor(
    public n: number,
    public delta: T[],
  ) {}

  remap(m: number, f: (i: number) => number): DeltaDyn<T> {
    const delta = Array.from({ length: m }, (_, i) => this.delta[f(i) % this.n].clone());
    return new DeltaDyn<T>(0, delta);
  }
}
```
