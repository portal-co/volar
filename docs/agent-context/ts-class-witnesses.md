# TypeScript Class Witnesses

> **Relevant when:** working on `printer_ts.rs`, `ir.rs`, spec type params, or any TS codegen that involves generic functions calling static methods on type-parameter types.

---

## The Problem: Types as Values

Volar spec functions are generic over field/group types:

```rust
fn commit<F: Field>(data: &[F], key: F) -> F {
    F::zero()           // static method on type param
}
```

TypeScript has no concept of passing a type as a value. The naive translation fails:

```typescript
// WRONG — F is a type, not a value; F.zero() is not callable
function commit<F>(data: F[], key: F): F {
    return F.zero();    // TS error: 'F' only refers to a type
}
```

---

## Prior Art: Per-Operation Witnesses

The original witness system solved this by extracting individual operations as closures in a `ctx` parameter:

```typescript
// Generated: Constructor, Default, Projection witnesses
function commit(ctx: { newF: () => any, defaultF: () => any }, data: any[], key: any): any {
    return ctx.defaultF();   // F::zero()
}
```

`WitnessKind` variants:

| Variant | Rust | TypeScript field | TS type |
|---------|------|-----------------|---------|
| `Projection { T, "OutputSize" }` | `T::OutputSize` | `T_OutputSize` | `number` |
| `Constructor { T }` | `T::new()` | `newT` | `() => any` |
| `Default { T }` | `T::default()` | `defaultT` | `() => any` |

**Limitations:**
- Each static method on `T` needs its own witness entry
- Witness types are `() => any` — the TypeScript type system knows nothing about `T`
- `G.generator()`, `G.order()`, `G.add(a, b)` each become separate entries
- No way to express "this witness is the class for `T`"

---

## Class Witnesses: The Solution

A **class witness** passes the entire class constructor as a single witness. One witness covers all static methods and enables `instanceof` checks at runtime.

```typescript
// Generated with class witness
function commit<F extends object>(ctx: { FClass: new (...args: any[]) => F }, data: F[], key: F): F {
    return ctx.FClass.zero();   // F::zero() → ctx.FClass.zero()
}

// Concrete call site
commit({ FClass: Galois }, data, key);
```

### WitnessKind::Class

```rust
pub enum WitnessKind {
    Projection { type_param: String, field: String },
    Constructor { type_param: String },
    Default { type_param: String },
    Class { type_param: String },   // NEW
}
```

### ctx field naming

| Variant | Rust | TypeScript field | TS type |
|---------|------|-----------------|---------|
| `Class { T }` | `T` used as value | `TClass` | `typeof T` (if T is a known concrete class) |

When the concrete class is known (via spec analysis), the type is `typeof Galois`. Otherwise it falls back to `{ new(...): any } & Record<string, (...args: any[]) => any>`.

### Why classes and not object shapes

Object-literal witnesses `{ zero: () => Galois }` work for a single function but:
- Each new static method requires a new witness entry
- `instanceof` doesn't work (`val instanceof ctx.GClass` is invalid when `GClass` is an object)
- Runtimes (V8, SpiderMonkey) optimise class dispatch; closures are opaque
- `undefined` can witness itself as a unit type — it IS the only instance of its type, which is why `None = undefined` works: `undefined instanceof undefined` is identity

`class` instances carry their constructor reference, enabling:
```typescript
val instanceof ctx.GClass   // type narrowing
ctx.GClass.zero()           // static call
new ctx.GClass(...)         // construction
```

---

## Spec Requirement: Explicit Type Parameters

For class witnesses to be emitted correctly, **every type parameter that is used as a value (static call, constructor, `instanceof`) must be explicitly named in the spec signature**. This is enforced by `rustc` — the spec is real Rust, and implicit type params will cause compile errors.

**Correct:**
```rust
fn absorb<F: Field>(x: F) -> F {
    let zero = F::zero();   // F is explicit → Class witness for F
    zero
}
```

**Wrong (implicit via inference):**
```rust
fn absorb(x: impl Field) -> impl Field { ... }   // `impl Trait` has no name → no witness possible
```

The codegen uses the type parameter name directly as the witness key (`FClass`, `GClass`, etc.). If the name is absent, the witness cannot be generated.

---

## How Class Witnesses Are Generated

### 1. Detection (scan pass)

`scan_expr_witnesses` already detects `T::new()` → `Constructor` and `T::default()` → `Default`.

The extended scan also catches **any** `T::method(args)` where `T` is a known type param and `method` is not in the already-handled set:

```rust
IrExpr::Call { func: IrExpr::Path { segments: [T, method] }, .. }
  if declared_generics.contains(T) && method != "new" && method != "default"
  → out.add(WitnessKind::Class { type_param: T })
```

The value-position use of `T` alone (e.g., `G` as an expression) also triggers a `Class` witness:

```rust
IrExpr::Var(T) | IrExpr::Path { segments: [T] }
  if declared_generics.contains(T)
  → out.add(WitnessKind::Class { type_param: T })
```

### 2. Emission (ctx parameter)

`write_ctx_param` emits the `ctx` object type. For a `Class` witness:

```typescript
ctx: { GClass: typeof G }
```

When the class is a known concrete class from the module, the type uses `typeof ClassName`. When unknown (erased type param), it falls back to `{ new(...args: any[]): any } & Record<string, (...args: any[]) => any>`.

### 3. Call-site transformation

In `emit_path` / `TsExprWriter::Call`:

| Rust pattern | Before | After |
|---|---|---|
| `T::new(args)` | `ctx.newT(args)` | `new ctx.TClass(args)` |
| `T::default()` | `ctx.defaultT()` | `ctx.TClass.default()` |
| `T::zero()` | *(unhandled → error)* | `ctx.TClass.zero()` |
| `T` (bare value) | *(error: T is type)* | `ctx.TClass` |

### 4. Generics are preserved

Unlike `Constructor`/`Default` witnesses (typed as `() => any`), class witnesses keep the generic parameter:

```typescript
// Constructor witness (erased)
ctx: { newG: () => any }   // caller has no type info about G

// Class witness (typed)  
ctx: { GClass: typeof Galois64 }   // caller knows G is Galois64
```

The `ctx.TClass` field type is `typeof ConcreteClass` — not `any`. This flows into the return type and argument types, enabling proper type checking on static call results.

---

## File Map

| File | Role |
|------|------|
| `crates/compiler/volar-compiler/src/printer_ts.rs` | Main implementation: `WitnessKind`, `scan_expr_witnesses`, `witness_ctx_field`, `witness_ctx_type`, `write_ctx_param`, `TsExprWriter` call-site translation |
| `crates/compiler/volar-compiler/src/ir.rs` | `WitnessKind` if moved to IR (currently lives in printer) |
| `crates/spec/volar-spec/src/` | Spec files — type params must be explicit for witnesses to work |
