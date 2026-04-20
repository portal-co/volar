# WAFFLE → VAFFLE Lowering

> **Referenced by**: `crates/ir/volar-vaffle-target/src/waffle_lower.rs`
>
> **Status**: @reliability: experimental

---

## Overview

`waffle_lower.rs` lowers a WAFFLE `FunctionBody` (parsed from a WebAssembly
module) into `VaffleTarget` (VAFFLE IR), producing the same GF(2) bit-circuit
representation that `VolarIrTarget` produces from the Volar compiler's IR.
This is the bridge that lets Volar verify or garble arbitrary WASM programs.

**Purpose**: enable program-related cryptography (ZK proofs, garbled circuits)
over WASM-compiled code without writing custom Volar IR.  Rust or C programs
compiled to WASM can be lowered through this path.

---

## Entry Points

```rust
/// Lower all function bodies in a WASM module into `target`.
/// Returns a list of `(name, error)` for skipped (unsupported) functions.
pub fn lower_waffle_module(
    wasm: &WModule,
    target: &mut VaffleTarget,
) -> Vec<(String, UnsupportedOp)>;

/// Lower a single WASM function body.
pub fn lower_waffle_function(
    body: &FunctionBody,
    name: &str,
    wasm: &WModule,
    target: &mut VaffleTarget,
) -> Result<(), UnsupportedOp>;
```

`lower_waffle_module` skips unsupported functions silently, collecting errors.
Call `lower_waffle_function` directly when you need to handle errors per-function.

---

## Supported Operations

### Integer arithmetic

All WASM `I32` and `I64` operations map to `LirTarget` calls, which are then
bit-decomposed by `VaffleTarget`:

| WASM operator | LIR emission |
|---------------|-------------|
| `I32Const` / `I64Const` | `iconst(U32/U64, v)` |
| `I32Add` / `I64Add` | `add(a, b)` |
| `I32Sub` / `I64Sub` | `sub(a, b)` |
| `I32Mul` / `I64Mul` | `mul(a, b)` |
| `I32DivS` / `I64DivS` | `sdiv(a, b)` |
| `I32DivU` / `I64DivU` | `udiv(a, b)` |
| `I32And` / `I64And` | `band(a, b)` |
| `I32Or` / `I64Or` | `bor(a, b)` |
| `I32Xor` / `I64Xor` | `xor(a, b)` |
| `I32Shl` / `I64Shl` | `shl(a, b)` |
| `I32ShrU` / `I64ShrU` | `lshr(a, b)` |
| `I32ShrS` / `I64ShrS` | `ashr(a, b)` |
| `I32Eqz` | `icmp(Eq, v, 0)` |
| `I32Eq` / `I64Eq` | `icmp(Eq, a, b)` |
| `I32Ne` / `I64Ne` | `icmp(Ne, a, b)` |
| `I32LtS` / `I64LtS` | `icmp(Slt, a, b)` |
| `I32LtU` / `I64LtU` | `icmp(Ult, a, b)` |
| `I32GtS` / `I64GtS` | `icmp(Slt, b, a)` |
| `I32GtU` / `I64GtU` | `icmp(Ult, b, a)` |
| `I32LeS` / `I64LeS` | `icmp(Sle, a, b)` |
| `I32LeU` / `I64LeU` | `icmp(Ule, a, b)` |
| `I32GeS` / `I64GeS` | `icmp(Sle, b, a)` |
| `I32GeU` / `I64GeU` | `icmp(Ule, b, a)` |
| `I32WrapI64` | `trunc(U32, v)` |
| `I64ExtendI32S` | `sext(U64, v)` |
| `I64ExtendI32U` | `zext(U64, v)` |
| `Select`, `TypedSelect` | `select(cond, a, b)` |

### Control flow

| WASM construct | VAFFLE output |
|----------------|---------------|
| `Br(block)` | `jump(target_block, args)` |
| `CondBr(cond, then, else)` | `branch(cond, then_block, else_block)` |
| `Return(vals)` | `ret(vals)` |
| `ReturnCall(func, args)` | `ret_call(name, args)` — O(1) stack tail call |
| `Unreachable` / `UB` / `None` | `ret(0)` stub |

### Direct calls

`Operator::Call { function_index }` emits a `VaffleTarget` `call_extern` to
the callee function by name.

### Linear memory

WASM linear memory (heap) is lowered to `StorageId::memory(idx)` with 8-bit
cells (one cell per byte).  Load/store operations read and write individual bytes
and reconstruct multi-byte integers via bit concatenation:

| WASM operator | Byte count | Action |
|---------------|-----------|--------|
| `I32Load` | 4 | Read 4 bytes, combine LSB-first |
| `I32Load8U` | 1 | Read 1 byte, zero-extend to 32 bits |
| `I32Load8S` | 1 | Read 1 byte, sign-extend to 32 bits |
| `I32Load16U` | 2 | Read 2 bytes, zero-extend |
| `I32Load16S` | 2 | Read 2 bytes, sign-extend |
| `I32Store` | 4 | Decompose to 4 bytes, write each |
| `I32Store8` | 1 | Write low byte |
| `I32Store16` | 2 | Write 2 bytes |
| `I64Load` | 8 | Read 8 bytes, combine LSB-first |
| `I64Load8U` | 1 | Read 1 byte, zero-extend to 64 bits |
| `I64Load8S` | 1 | Read 1 byte, sign-extend to 64 bits |
| `I64Load16U` | 2 | Read 2 bytes, zero-extend to 64 bits |
| `I64Load16S` | 2 | Read 2 bytes, sign-extend to 64 bits |
| `I64Load32U` | 4 | Read 4 bytes, zero-extend to 64 bits |
| `I64Load32S` | 4 | Read 4 bytes, sign-extend to 64 bits |
| `I64Store` | 8 | Decompose to 8 bytes, write each |
| `I64Store8` | 1 | Write low byte |
| `I64Store16` | 2 | Write 2 bytes |
| `I64Store32` | 4 | Write 4 bytes |
| `MemorySize` | — | Always returns 0 (fixed compile-time size) |
| `MemoryGrow` | — | Always returns -1 (growth not supported in circuit model) |

The `MemoryArg` offset field is added to the base address at lowering time.

**Note**: memory addresses in WASM are 32-bit, matching `VaffleTarget`'s
`PTR_BITS = 32` stack-pointer width.

---

## Type Mapping

```rust
fn waffle_ty(ty: WType) -> Result<LirType, UnsupportedOp> {
    match ty {
        WType::I32 => Ok(LirType::U32),
        WType::I64 => Ok(LirType::U64),
        other => Err(UnsupportedOp(...)),
    }
}
```

---

## Unsupported Operations

The following WASM features are **not supported** and cause `UnsupportedOp` to
be returned (the function is skipped):

- **Floats** (`F32`, `F64`) — no float circuit exists in VAFFLE
- **Tables and indirect calls** (`CallIndirect`, `TableGet`, `TableSet`, `TableGrow`, `TableSize`, `TableFill`)
- **Globals** (`GlobalGet`, `GlobalSet`)
- **SIMD** (`V128` and all `V128*` operators)
- **Atomics** (`MemoryAtomicNotify`, all `Atomic*` RMW/CAS operators)
- **Reference types** (`RefNull`, `RefIsNull`, `RefFunc`)
- **Bulk memory** (`MemoryCopy`, `MemoryFill`, `MemoryInit`, `DataDrop`, `ElemDrop`)
- **GC proposal** (`StructNew`, `ArrayNew`, `RefCast`, etc.)
- **Multi-value return** and **multi-result `Call`**
- **Missing integer ops**: `rem`, `clz`, `ctz`, `popcnt`, `rotl`, `rotr`, sign-extend variants, `trunc_sat*`
- **Indirect/ref terminators**: `br_table` (`Select`), `ReturnCallIndirect`, `ReturnCallRef`

See [wasm-feature-support.md](wasm-feature-support.md) for a full proposal-level breakdown across both the WAFFLE IR and volar-vaffle-target layers.

---

## Integration with `VaffleTarget`

The lowering uses `VaffleTarget` as a `LirTarget + BitCircuitBuilder`.  All
integer values are bit-decomposed via the same `bc_*` functions used by
`VolarIrTarget`:

- 32-bit WASM values → 32 `Bit`-typed VAFFLE `ValueId`s
- 64-bit WASM values → 64 `Bit`-typed VAFFLE `ValueId`s

The VAFFLE IR can then be used as input to the VOLE or garbled-circuit weavers
via `lower_to_ir` (VAFFLE → Volar IR) or directly to a boolar pass.

---

## Full Pipeline: WASM → Garbled Circuit

```
WASM binary
  ↓  portal_pc_waffle_ir::Module::from_wasm_bytes
WAFFLE Module
  ↓  lower_waffle_module(wasm, &mut target)
VaffleTarget (VAFFLE IR, bit-decomposed)
  ↓  lower_to_ir(&target, name)
IRBlocks (Volar IR, bit-level CFG)
  ↓  weave_garbler / weave_evaluator
IrModule (garbler or evaluator Rust code)
  ↓  print_weaved_module
Rust source
```

---

## Error Handling

```rust
pub struct UnsupportedOp(pub String);
```

Unsupported operators produce an `UnsupportedOp` describing the WASM operator
that triggered the failure.  `lower_waffle_module` collects these and returns
them without panicking, so a module with some unsupported functions can still
be partially lowered.

---

## See Also

- [LIR ABI](lir-abi.md) — how VaffleTarget handles stack allocation and pointers
- [VAFFLE pointer types](#vaffle-pointer-types) — `Value::StackAlloc`, `PtrLoad`, `PtrStore`, `PtrOffset`
- `crates/ir/volar-vaffle-target/src/waffle_lower.rs` — implementation
