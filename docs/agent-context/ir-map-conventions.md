# IR `#[non_exhaustive]`, `map`, `as_ref`, `as_mut` Conventions

## Why `#[non_exhaustive]` on IR operation enums

All IR operation enums are marked `#[non_exhaustive]`. This means:
- Adding a new variant to an enum **only** forces updates in the defining crate's own exhaustive matches.
- External crates (anything outside the crate that defines the enum) must already have a `_ =>` catch-all arm, so the new variant never breaks external consumers at compile time.
- The invariant: **the cost of adding a variant is O(1)** — just the new variant definition and internal lowering code.

The following enums are non-exhaustive:

| Crate | Enum |
|-------|------|
| `volar-ir-common` | `IrType`, `Stmt<Var, Addr, Ty, Stor>` |
| `volar-ir` | `IRTerminator<Var>`, `IRBlockTargetId` |
| `volar-ir` | `BIrStmt<Var, Stor>`, `BIrTerminator<Var>` |
| `vaffle` | `FuncDecl`, `Terminator<V>`, `Value<V>` |
| `volar-lir` | `LirType` |
| `volar-compiler` | `IrExpr<P>`, `IrStmt<P>`, `IrType`, `IrPattern`, `IrCfgTerminator<P>` |

## Consumer catch-all conventions

The `_ =>` arm to add depends on the pass type:

**Critical lowering passes** — must handle every variant to produce correct output:
```rust
_ => panic!("lower_foo: unhandled XVariant variant — add lowering for this variant"),
```
Use in: `lower_ir_to_boolar`, `lower_lir`, `lower_to_circuit`, `movfuscate`, `substitute_ir`, weaver codegen.

**Text / display / debug-dump passes** — graceful output is acceptable:
```rust
_ => { w.write_str("<unknown-stmt>")?; }  // or
_ => "<unknown-pat>".to_string()
```
Use in: `volar-ir-text`, `volar-lir-text`, `volar-compiler-passes/dump_ir`.

**Analysis / visitor / alias passes** — unknown variants are safely ignored:
```rust
_ => {}
```
Use in: `volar-ir-opt` (store_forward, substitute_ir, biir, ir, common), compiler chunk_fns, lowering collectors.

**Type-inference helpers** — return `None` / sentinel for unknown variants:
```rust
_ => None   // or
_ => TypeId(0)
```
Use in: `stmt_output_type`, `type_bit_width`, `ir_type_bit_width`.

**Interpreter / fuzzer** — panic loudly on unknown variants (evaluation must be complete):
```rust
_ => panic!("eval_foo: unhandled XVariant variant — add evaluation for this variant"),
```

## Generic parameters with defaults

All IR enums were generified so existing code needs no spelling changes:

```rust
// volar-ir-common
pub enum Stmt<Var, Addr = Var, Ty = TypeId, Stor = StorageId> { ... }

// volar-ir
pub enum IRTerminator<Var = IRVarId> { ... }
pub enum BIrStmt<Var = IRVarId, Stor = StorageId> { ... }
pub enum BIrTerminator<Var = IRVarId> { ... }

// vaffle
pub enum Terminator<V = ValueId> { ... }
pub enum Value<V = ValueId> { ... }
```

Existing uses like `Stmt<IRVarId>` or `IRTerminator` continue to compile unchanged. The defaults fill in.

## `Stmt::map` — context-threaded callbacks

```rust
impl<Var, Addr, Ty, Stor> Stmt<Var, Addr, Ty, Stor> {
    pub fn map<Ctx, NV, NA, NT, NS, E>(
        self,
        ctx:      &mut Ctx,
        var_fn:   impl FnMut(&mut Ctx, Var)  -> Result<NV, E>,
        addr_fn:  impl FnMut(&mut Ctx, Addr) -> Result<NA, E>,
        ty_fn:    impl FnMut(&mut Ctx, Ty)   -> Result<NT, E>,
        stor_fn:  impl FnMut(&mut Ctx, Stor) -> Result<NS, E>,
    ) -> Result<Stmt<NV, NA, NT, NS>, E>
    where NV: Ord  // required for Poly.coeffs BTreeMap keys
```

Why `&mut Ctx` + `FnMut(&mut Ctx, T)` rather than plain `FnMut(T)`: when remapping both types and storage IDs in the same pass, both callbacks need access to the same mutable context (e.g., a `TypeTable` and `StorageAllocator` in one struct). Closure capture would require two `&mut` borrows of the same binding, which Rust forbids. Threading `ctx` through the signature avoids this.

### Convenience variants

```rust
// Map only variables (identity on Addr, Ty, Stor)
pub fn map_var<Ctx, NV, E>(
    self,
    ctx: &mut Ctx,
    f: impl FnMut(&mut Ctx, Var) -> Result<NV, E>,
) -> Result<Stmt<NV, Addr, Ty, Stor>, E>
where NV: Ord, Addr: Clone, Ty: Clone, Stor: Clone
```

### `as_ref` / `as_mut`

```rust
pub fn as_ref(&self) -> Stmt<&Var, &Addr, &Ty, &Stor>
where Var: Ord  // O(n log n) for Poly — rebuilds BTreeMap with &Var keys

pub fn as_mut(&mut self) -> Stmt<&mut Var, &mut Addr, &mut Ty, &mut Stor>
where Var: Ord
```

Note: `as_mut` for `Stmt` is implemented but requires `Var: Ord` because `Poly.coeffs` is a `BTreeMap<Vec<Var>, u8>`. Rebuilding with `&mut Var` keys requires cloning or re-inserting, so callers modifying Poly coefficients should prefer `map` over `as_mut`.

## `IRTerminator::map`

```rust
impl<Var> IRTerminator<Var> {
    pub fn map<Ctx, NV, E>(
        self,
        ctx: &mut Ctx,
        go: impl FnMut(&mut Ctx, Var) -> Result<NV, E>,
    ) -> Result<IRTerminator<NV>, E>
    where NV: Ord  // JumpTable uses BTreeMap<Constant, (IRBlockTargetId, Vec<NV>)>

    pub fn as_ref(&self) -> IRTerminator<&Var> where Var: Ord
    pub fn as_mut(&mut self) -> IRTerminator<&mut Var> where Var: Ord
}
```

## `BIrStmt::map` and `BIrTerminator::map`

```rust
impl<Var, Stor> BIrStmt<Var, Stor> {
    pub fn map<Ctx, NV, NS, E>(
        self,
        ctx:      &mut Ctx,
        var_fn:   impl FnMut(&mut Ctx, Var)  -> Result<NV, E>,
        stor_fn:  impl FnMut(&mut Ctx, Stor) -> Result<NS, E>,
    ) -> Result<BIrStmt<NV, NS>, E>
    // No Ord bound needed — BIrStmt has no BTreeMap fields

    pub fn as_ref(&self) -> BIrStmt<&Var, &Stor>
    pub fn as_mut(&mut self) -> BIrStmt<&mut Var, &mut Stor>
}
```

`BIrTerminator<Var>` gets the same single-callback form as `IRTerminator`.

## VAFFLE `Terminator::map` and `Value::map`

Same pattern — single `go` callback for `V`:

```rust
impl<V> Terminator<V> {
    pub fn map<Ctx, NV, E>(self, ctx: &mut Ctx, go: impl FnMut(&mut Ctx, V) -> Result<NV, E>)
        -> Result<Terminator<NV>, E>
    pub fn as_ref(&self) -> Terminator<&V>
    pub fn as_mut(&mut self) -> Terminator<&mut V>
}
```

`Value::Op(Stmt<V>)` delegates to `Stmt::map` with identity callbacks for `Ty` and `Stor`.

## Example: type remapping with `Stmt::map`

```rust
stmt.map(
    remapper,
    |_, v|  Ok::<_, Infallible>(v),      // Var unchanged
    |_, a|  Ok::<_, Infallible>(a),      // Addr unchanged
    |ctx, ty| Ok::<_, Infallible>(ctx.remap(ty)),  // remap TypeId
    |_, s|  Ok::<_, Infallible>(s),      // Stor unchanged
).unwrap()
```

This subsumes `TypeRemapper::remap_stmt_types` (which can be kept as a thin wrapper or removed).
