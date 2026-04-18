# LIR ABI — Calling Convention and Type Passing

This document specifies the **Application Binary Interface** used by the LIR
subsystem — how values are passed between functions, how aggregates are
represented, and the per-target optimisations that the compiler codegen layer
(`volar-lir-codegen`) applies based on the target's declared ABI policy.

---

## Overview

The LIR ABI has three layers:

| Layer | Crate | Role |
|---|---|---|
| **Policy** | `volar-lir` | `LirAbi` struct + `LirTarget::abi()` method |
| **Codegen** | `volar-lir-codegen` | Compiler IR → LIR lowering; queries `abi()` to choose conventions |
| **Backends** | `volar-c-backend`, `volar-ir-lir-target`, `volar-vaffle-target` | Implement `LirTarget` + declare their ABI |

---

## `LirAbi` — ABI Policy Struct

```rust
pub struct LirAbi {
    /// Maximum flat scalars to pass inline (by value).
    /// Above this, aggregates are passed by pointer via StackAllocExt.
    pub aggregate_byval_limit: usize,

    /// Whether the backend handles aggregate types natively
    /// (e.g. C passes structs by value in the C ABI).
    pub native_aggregates: bool,
}
```

### Predefined Constants

| Constant | `aggregate_byval_limit` | `native_aggregates` | Used by |
|---|---|---|---|
| `LirAbi::CIRCUIT` | `usize::MAX` | `false` | `VolarIrTarget`, `VaffleTarget` |
| `LirAbi::C_NATIVE` | `64` | `true` | `CBackend` |
| `LirAbi::DEFAULT` | `usize::MAX` | `false` | Fallback for unknown backends |

### Query Method

Every `LirTarget` exposes its ABI via:

```rust
fn abi(&self) -> LirAbi { LirAbi::DEFAULT }
```

The codegen layer calls `target.abi()` before lowering to determine:
- Whether to flatten aggregates or pass them natively
- Whether large aggregates should use pointer passing
- What `StackAllocExt` capabilities are available

---

## Per-Target ABI Details

### Circuit Backends (`VolarIrTarget`, `VaffleTarget`)

**ABI**: `LirAbi::CIRCUIT`

All values are decomposed into individual GF(2) bits (one `Bit`-typed SSA
variable per bit, LSB first):

| `LirType` | Representation |
|---|---|
| `Bool` | 1 `Bit` var |
| `U8` / `I8` | 8 `Bit` vars |
| `U16` / `I16` | 16 `Bit` vars |
| `U32` / `I32` | 32 `Bit` vars |
| `U64` / `I64` | 64 `Bit` vars |
| `Arr(T, N)` | `N × bits(T)` `Bit` vars |
| `Struct(id)` | Sum of field widths in `Bit` vars |
| `Native(t)` | 1 native-typed var (not bit-decomposed) |
| `Ptr(T)` | 32 `Bit` vars (`VaffleTarget` only) |

**Function parameters**: Flat `Bit`-typed block parameters in the entry block.

**Function returns**: Flat `Bit`-typed values in the `Return` terminator.

**Extern calls**: Inline callee circuits directly (no actual call boundary;
  VolarIrTarget splices the callee's IRBlocks into the caller's block stream).

**Stack allocation**: `VaffleTarget` supports `StackAllocExt` via
  `StorageId::STACK`.  `VolarIrTarget` does not (returns `None`).

**Aggregate passing**: Always flattened.  `aggregate_byval_limit = usize::MAX`
  means pointer-passing is never triggered.

### C Backend (`CBackend`)

**ABI**: `LirAbi::C_NATIVE`

Values use native C types (`uint8_t`, `uint32_t`, etc.).  Aggregates are
typedef'd structs passed **by value** in the C calling convention:

| `LirType` | C Type |
|---|---|
| `Bool` | `bool` |
| `U8` / `I8` | `uint8_t` / `int8_t` |
| `U32` / `I32` | `uint32_t` / `int32_t` |
| `U64` / `I64` | `uint64_t` / `int64_t` |
| `Arr(T, N)` | `typedef struct { T data[N]; } Arr_T_N;` |
| `Struct(id)` | Named struct typedef |
| `Ptr(T)` | `T*` |

**Function parameters**: One C parameter per logical argument.  Aggregates are
  a single struct-typed parameter, immediately unpacked to scalars in the
  preamble.

**Function returns**: Aggregate return values are packed back into a C struct
  at the `return` statement.

**Extern calls**: `call_extern` packs flat scalars into C aggregates, emits
  the C function call, and unpacks the return value.  An `extern` declaration
  is emitted automatically.

**Stack allocation**: Full `StackAllocExt` support.  `alloca` emits a
  preamble-scoped C array declaration + pointer:

```c
T slot_vN[count];
T* vN = slot_vN;
```

**Aggregate passing threshold**: Aggregates with > 64 flat scalars can be
  passed by pointer via `StackAllocExt` when the codegen layer opts in.

---

## Compiler IR → LIR Optimisations

The `volar-lir-codegen` crate translates `IrModule` (compiler IR) into
`LirTarget` builder calls.  The following ABI-aware optimisations are applied:

### 1. Type-Aware Literal Lowering

Integer literals (`IrLit::Int`) use the binding's declared type when available:

```rust
// IrStmt::Let { ty: Some(Primitive(U8)), init: Lit(42) }
// Old: iconst(U64, 42)  — always U64
// New: iconst(U8, 42)   — uses the Let binding's type annotation
```

This avoids unnecessary widening and keeps the generated code type-correct
for backends that track value types (e.g. C backend emits `uint8_t` vs
`uint64_t`).

When no type annotation is available, the default remains `U64`.

### 2. Return-Type Inference for Function Calls

All function signatures in the module are collected into a `FuncSigInfo` table
before lowering.  When `lower_call` encounters a non-external function call,
it looks up the callee's return type from this table:

```rust
// Old: call_extern("other_fn", &arg_tys, &flat_args, None)
//   → return value discarded
// New: call_extern("other_fn", &arg_tys, &flat_args, Some(LirType::U32))
//   → return value correctly captured and unpacked
```

This fixes a bug where intra-module function calls silently discarded their
return values.

### 3. Type-Correct Negation

The `Neg` unary operator now queries the operand's type via
`value_scalar_type()` instead of hardcoding `U64`:

```rust
// Old: sub(iconst(U64, 0), v)  — wrong if v is U8
// New: sub(iconst(value_type(v), 0), v)  — matches operand width
```

### 4. ABI-Aware Aggregate Passing (Future)

The infrastructure is in place for the codegen layer to query
`target.abi().pass_by_ptr(scalar_count)` and, when `true`, use
`StackAllocExt` to pass large aggregates by pointer.  This path is not yet
activated but is ready for backends that opt in by setting
`aggregate_byval_limit` to a finite value and implementing `StackAllocExt`.

---

## Adding a New Backend

To implement a `LirTarget` with a custom ABI:

1. **Implement `LirTarget`** with all required methods.

2. **Override `abi()`** to return a `LirAbi` that describes your conventions:
   ```rust
   fn abi(&self) -> LirAbi {
       LirAbi {
           aggregate_byval_limit: 32,  // pointer-pass above 32 scalars
           native_aggregates: true,     // backend handles structs/arrays natively
       }
   }
   ```

3. **Optionally implement `StackAllocExt`** and return `Some(self)` from
   `stack_alloc_ext()` if your backend can allocate stack memory and
   dereference pointers.

4. **Override `stack_alloc_ext()`** accordingly:
   ```rust
   fn stack_alloc_ext(&mut self) -> Option<&mut dyn StackAllocExt<Value = Self::Value>> {
       Some(self)  // or None for circuit backends
   }
   ```

---

## VAFFLE Stack-Frame ABI

The VAFFLE IR's `lower_to_ir` pass uses a stack-based call convention for
inter-function calls within the VAFFLE → Volar IR lowering.  This is documented
separately in the `lower_to_ir.rs` module header but summarised here:

**Stack storage**: `StorageId::STACK` with `SP_BITS = 16` address width.

**Frame layout**:
- Bit-typed slots: parameters, return value, spill slots (one per VAFFLE value)
- Block-typed slot: continuation reference (zero Bit-slot cost; separate type lane)

**Call protocol**:
1. Caller spills live values to own frame
2. Caller writes callee args + continuation to new frame at `SP`
3. Caller advances `SP` by callee frame size, jumps to callee entry
4. Callee reads args from `SP - frame_size`
5. Callee returns via `Dyn(continuation)` with retreated `SP` + return values
6. Continuation reloads spilled values and continues

**Stack allocation** (`StackAllocExt`): `VaffleTarget` supports `alloca` via
  `StorageId::STACK` with a per-function bump allocator.  Each `alloca` reserves
  a range of bit-slots in the frame and returns a constant 32-bit address.

---

## Target Capability Matrix

| Capability | `VolarIrTarget` | `VaffleTarget` | `CBackend` |
|---|---|---|---|
| `abi()` | `CIRCUIT` | `CIRCUIT` | `C_NATIVE` |
| `stack_alloc_ext()` | `None` | `Some` | `Some` |
| `native_aggregates` | No | No | Yes |
| `aggregate_byval_limit` | ∞ | ∞ | 64 |
| Value representation | Flat bits | Flat bits | Native C types |
| Extern calls | Inline | Inline + storage-based calls | C function calls |
| Pointer types | Panic | 32-bit address | C pointer |
