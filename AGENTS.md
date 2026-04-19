# Volar Agent Context

## Project Mission

Volar's goal is to increase adoption of **program-related cryptography** — zero-knowledge proofs, garbled circuits, multi-party computation, and the primitives that underpin them — by implementing it in auditable Rust, compiling it to other targets (TypeScript, C), and developing it publicly with rigorous reliability tracking.

The current implementation focus is VOLE-based ZK proofs and garbled circuits. These were chosen because they share a clean common substrate (VOLE correlations, boolean circuits, binary extension-field arithmetic) that the compiler and IR were designed around. Other schemes and protocols will follow as the infrastructure matures.

When deciding what to implement or how to design a component, prefer choices that:
- Are useful to the broadest set of program-related cryptography applications (not just the current VOLEitH construction).
- Keep the IR, compiler, and spec layer general enough to support future protocols.
- Follow the reliability system: new cryptographic constructions start at Experimental, not Normal.

## Current Implementation Architecture

The live cryptographic kernel (`volar-spec`) implements two constructions:

**VOLE-based ZK (Quicksilver-style VOLEitH):**
- Subfield VOLE over GF(2) for bit-level operations, authenticated by an extension field GF(2^128) with a global secret Δ.
- Commitment structure: `Vope<N, T, K>` — a vector of N authenticated values as polynomials of degree K in Δ.
- Bit-slicing via `BitsInBytes`/`BitsInBytes64` for SIMD-style parallelization over GF(2).
- Galois Extension Lifting: mapping bit-commitments into GF(2^k) (like AES's GF(256)) via linear basis transformations to perform field-specific operations (e.g., S-Box inversions).
- Quicksilver-style algebraic AND checks: the product of two VOLE polynomials is verified against a claimed result by ensuring the resulting high-degree polynomial vanishes.

**Garbled circuits:**
- Half-gate scheme (Zahur-Rosulek-Evans 2015) over VOLE wire labels.
- `GlobalSecret<N>`, `Garble<N>`, `Eval<N>`, `GarbleTable<N>` types.
- Weaver generates both garbler and evaluator variants from boolean circuits.

## Compiler IR Genericity Invariant

All code that programmatically constructs `IrModule` (in `volar-compiler`) must use typed `IrExpr`/`IrStmt` nodes — never embed raw Rust strings as expression text. Use:
- `IrExpr::MethodCall` for method calls
- `IrExpr::Binary` for binary operators
- `IrExpr::StructExpr` for struct literals
- `IrExpr::Var` for variable references
- `IrExpr::Call` + `IrExpr::Path` for free function calls with turbofish
- `IrType::Struct { kind: Custom("..."), .. }` for named types

If the printer does not handle a node correctly, **fix the printer** rather than working around it with raw strings. Guardrails in the printer (debug_assert on ident characters, etc.) exist to catch injection; respect them.

## Tests for Generated IR

Tests of generated compiler IR must **lower and compile the output** (real backend: `print_module` → `rustc`), not perform syntactic IR analysis. Do not assert on variable names, statement counts, or IR structure unless the test is specifically verifying a hard-to-change structural invariant. The correctness signal is: the generated code compiles and (where possible) runs correctly.

## Garbler + Evaluator Pairing

Any weaving pass targeting a garbled circuit scheme must generate **both** variants:
- **Evaluator side**: takes precomputed `GarbleTable<N>` inputs and evaluates the circuit using `and_via_table`, free-XOR, etc.
- **Garbler side**: takes `GlobalSecret<N>` and input `Garble<N>` wire labels, produces `GarbleTable<N>` for each AND gate.

These variants must be co-designed so their protocols are compatible (same table order, same wire convention).

## Reliability Tags

Files tagged `// @reliability: experimental` contain unreviewed cryptographic code. New code depending on these must not be deployed without separate review. The `@ai: none` or `@ai: assisted` tags indicate the level of AI involvement in that file.

## IR Type Taxonomy

When working with `IRType` (in `volar-ir`), use this taxonomy to decide how to handle each variant:

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

A `StorageWrite` to `(S, T, addr)` invalidates all cached reads for the same `(S, T)` pair regardless of address (conservative on address aliasing), but does NOT invalidate entries for `(S, T')` where `T' != T`.

### Evaluator conformance

All evaluators must key their storage maps by `(StorageId, TypeId, addr)` (IR, VAFFLE) or the equivalent `(StorageId, bit_width, addr)` (BIR, where all values are single bits). Writing with one type and reading with a different type at the same `StorageId + address` returns the default zero value, not the previously written data.

### BIR multi-bit addresses

`BIrStmt::StorageRead` and `StorageWrite` use `addr: Vec<IRVarId>` — each element is a single-bit BIR variable, and the Vec represents an N-bit address giving 2^N distinct locations per `(StorageId, bit_width)` pair. Bit 0 (index 0) is the least-significant bit.

**IR→BIR lowering** (`lower_ir_to_boolar.rs`): all bits of the IR address variable are passed through to the BIR address vec — `var_bits[&addr.0].iter().copied().collect()`. No truncation.

**BIR evaluator** (`interpreter/biir.rs`): collapses `Vec<IRVarId>` to `u64` via `bits_to_u64` (imported from `interpreter::ir`), then keys the `BIrStorageMap` by `(StorageId, u64)`. This keeps the evaluator simple and supports up to 64-bit addresses.

**Store-forward optimizer** (`store_forward.rs`): `BiirStoreCache` is keyed by `(StorageId, usize, Vec<IRVarId>)`. `Vec<IRVarId>` implements `Ord` lexicographically, so it works as a `BTreeMap` key. Cross-block translation applies the pred→target arg map to **each element** of the addr Vec; if any bit fails to translate, the entire cache entry is dropped.

**FHE weaver** (`fhe.rs`): `emit_read` and `emit_write` take `addr_wires: &[&str]` (one wire name per address bit). `mux_tree_read` uses `addr_wires[level]` at each recursion level (not the same wire at every level). `emit_write` uses a full binary demux tree for N-bit addresses, mirroring `mux_tree_read`.

**Fuzzer generator** (`generators/biir.rs`): generates 1-bit addresses (`addr: vec![IRVarId(bv)]`) as the minimum, with optional 2-bit addresses for additional coverage.

### Relevant files

- `crates/fuzz/volar-fuzz/src/interpreter/ir.rs` — `StorageMap = BTreeMap<(StorageId, TypeId, u64), Vec<bool>>`
- `crates/fuzz/volar-fuzz/src/interpreter/vaffle.rs` — reuses `StorageMap` from `ir.rs`
- `crates/fuzz/volar-fuzz/src/interpreter/biir.rs` — `BIrStorageMap = BTreeMap<(StorageId, u64), bool>`; uses `bits_to_u64` to collapse multi-bit addr
- `crates/ir/volar-ir-opt/src/store_forward.rs` — cache types `IrStoreCache`, `BiirStoreCache` (keyed by `Vec<IRVarId>`), `VaffleCache`
- `crates/compiler/volar-weaver/src/fhe.rs` — `emit_read`/`emit_write` with `addr_wires: &[&str]`; `mux_tree_read` with per-level addr bits
