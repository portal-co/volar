# IR Types, Storage & Poly Semantics

> Load when working on IR, lowering, evaluators, store-forward, or fuzzer generators.

## IR Type Taxonomy

When working with `IRType` (in `volar-ir`), use this taxonomy:

| Variant | Kind | Bit-width | FHE CFG support |
|---|---|---|---|
| `Primitive(Bit)` | 1-bit GF(2) | 1 | Full |
| `Vec(N, Bit)` | packed bitvector | N | Full (`[wire; N]`) |
| `Primitive(_8/_16/_32/_64)` | packed bitvector | 8/16/32/64 | Full (`[wire; W]`) |
| `Primitive(_128/_256)` | packed bitvector | 128/256 | LIR: `unimplemented!`; FHE: `[wire; W]` |
| `Primitive(AES8)` | GF(256) field element | 8 | Deferred |
| `Primitive(Galois64)` | GF(2^64) field element | 64 | Deferred |

- `ir_type_bit_width(ty_id, types)` computes the wire count for any supported type.
- `FheScheme::wire_type_for_ir` / `public_type_for_ir` convert an `IRTypeId` to the appropriate compiler `IrType` for generated code.
- Unsupported types (AES8, Galois64 in FHE CFG path; _128/_256 in LIR) panic with an explicit message — do not silently emit wrong code.

## `Poly` Statement Semantics

`IRStmt::Poly { ty, coeffs, constant }` represents a multilinear polynomial over GF(2) with a typed output:

- **`ty = Bit`**: standard GF(2) gate — all coefficient variables are `Bit`-typed. This is the original and most common case.
- **`ty = T` (bitvector or field element)**: at most one `T`-typed variable per monomial; all other variables in that monomial are `Bit`-typed selectors. The polynomial result has type `T`.

When constructing `Poly` nodes, always supply the `ty` field explicitly. Do not use `ir_stmt_output_ty`'s old fallback (it now returns `Some(*ty)` for `Poly`).

## `IrLoweringConfig`

`volar_ir_config::IrLoweringConfig` configures target-specific parameters for `lower_ir`:

```rust
pub struct IrLoweringConfig {
    pub word_bits: usize,       // native word size (default 64)
    pub pointer_bits: usize,    // pointer size (default 64)
    pub aggregate_byval_limit: usize, // max struct size for by-value ABI (default 128)
    pub native_aggregates: bool,      // use struct-typed LIR values (default false)
}
```

`lower_ir` uses `IrLoweringConfig::default()` for backward compatibility. Pass a custom config via `lower_ir_with_handler` when targeting a different ABI.

## Storage Semantics (Type-Discriminated Slots)

Storage in Volar IR is keyed by `(StorageId, TypeId, address)`. Each such triple is an **independent slot**: writing `_8` to `(S1, addr=0)` does not affect a read of `Bit` from `(S1, addr=0)`, because different `TypeId`s are distinct namespaces within the same `StorageId`.

This design enables:
- **Efficient stack lowering**: a single `StorageId` can represent a stack frame with typed fields at distinct type-slots, without requiring separate `StorageId`s for each field.
- **Storage remapping**: optimization passes (e.g. store-to-load forwarding) can safely forward within a `(StorageId, TypeId)` pair without cross-type interference.

### Invalidation policy

A `StorageWrite` to `(S, T, addr)` invalidates all cached reads for the same `(S, T)` pair regardless of address (conservative on address aliasing), but does NOT invalidate entries for `(S, T')` where `T' != T`. **Do not change this policy.**

### Evaluator conformance

All evaluators must key their storage maps by `(StorageId, TypeId, addr)` (IR, VAFFLE) or the equivalent `(StorageId, bit_width, addr)` (BIR, where all values are single bits). Writing with one type and reading with a different type at the same `StorageId + address` returns the default zero value, not the previously written data.

### BIR multi-bit addresses

`BIrStmt::StorageRead` and `StorageWrite` use `addr: Vec<IRVarId>` — each element is a single-bit BIR variable, and the Vec represents an N-bit address giving 2^N distinct locations per `(StorageId, bit_width)` pair. Bit 0 (index 0) is the least-significant bit.

**IR->BIR lowering** (`lower_ir_to_boolar.rs`): all bits of the IR address variable are passed through to the BIR address vec — `var_bits[&addr.0].iter().copied().collect()`. No truncation.

**BIR evaluator** (`interpreter/biir.rs`): collapses `Vec<IRVarId>` to `u64` via `bits_to_u64` (imported from `interpreter::ir`), then keys the `BIrStorageMap` by `(StorageId, u64)`. This keeps the evaluator simple and supports up to 64-bit addresses.

**Store-forward optimizer** (`store_forward.rs`): `BiirStoreCache` is keyed by `(StorageId, usize, Vec<IRVarId>)`. `Vec<IRVarId>` implements `Ord` lexicographically, so it works as a `BTreeMap` key. Cross-block translation applies the pred->target arg map to **each element** of the addr Vec; if any bit fails to translate, the entire cache entry is dropped.

**FHE weaver** (`fhe.rs`): `emit_read` and `emit_write` take `addr_wires: &[&str]` (one wire name per address bit). `mux_tree_read` uses `addr_wires[level]` at each recursion level (not the same wire at every level). `emit_write` uses a full binary demux tree for N-bit addresses, mirroring `mux_tree_read`.

**Fuzzer generator** (`generators/biir.rs`): generates 1-bit addresses (`addr: vec![IRVarId(bv)]`) as the minimum, with optional 2-bit addresses for additional coverage.

### Relevant files

- `crates/fuzz/volar-fuzz/src/interpreter/ir.rs` — `StorageMap = BTreeMap<(StorageId, TypeId, u64), Vec<bool>>`
- `crates/fuzz/volar-fuzz/src/interpreter/vaffle.rs` — reuses `StorageMap` from `ir.rs`
- `crates/fuzz/volar-fuzz/src/interpreter/biir.rs` — `BIrStorageMap = BTreeMap<(StorageId, u64), bool>`; uses `bits_to_u64` to collapse multi-bit addr
- `crates/ir/volar-ir-opt/src/store_forward.rs` — cache types `IrStoreCache`, `BiirStoreCache` (keyed by `Vec<IRVarId>`), `VaffleCache`
- `crates/compiler/volar-weaver/src/fhe.rs` — `emit_read`/`emit_write` with `addr_wires: &[&str]`; `mux_tree_read` with per-level addr bits
