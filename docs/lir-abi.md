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
| `LirAbi::CIRCUIT` | `usize::MAX` | `false` | `VolarIrTarget`, `VaffleTarget` (default) |
| `LirAbi::VAFFLE_OPTIMIZED` | `64` | `false` | `VaffleTarget` (with `set_optimized_abi(true)`) |
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

**Aggregate passing**: Always flattened in the default mode
  (`optimized_abi = false`).  See the **VAFFLE Optimized ABI** section below
  for the stack-based passing mode.

### VAFFLE Optimized ABI (`VaffleTarget::with_optimized_abi`)

**ABI**: `LirAbi::VAFFLE_OPTIMIZED` (enabled via runtime flag)

When `VaffleTarget::set_optimized_abi(true)` (or `VaffleTarget::new().with_optimized_abi()`)
is called, parameters whose bit-decomposition exceeds 64 bits are passed
via `StorageId::STACK` instead of as individual block parameters.

**Motivation**: In the default circuit ABI, a 128-bit parameter (e.g.
`[U8; 16]`) becomes 128 separate `Bit`-typed block parameters.  In the
`lower_to_ir` pass, each block parameter is individually spilled and reloaded
at every call site, producing O(params × calls) storage operations.  For
AES-style functions with multiple 128-bit arguments, this is a significant
bottleneck.

**Protocol (caller)**:
1. Allocate a stack slot of N bits via `next_stack_slot` bump.
2. Write each bit of the argument to `StorageId::STACK` at the allocated
   address (ascending byte offsets from the base).
3. Pass the `PTR_BITS`-wide (32-bit) base address as block-parameter bits
   in the `Value::Call` node.

**Protocol (callee)**:
1. Receive `PTR_BITS` block parameters (the stack address).
2. Emit N `StorageRead` operations from `StorageId::STACK` at the
   received address to reconstruct the original bit vector.
3. Return the loaded bits to the LIR codegen layer (which sees the
   same `VaffleValue` shape as in the flat ABI).

**Threshold**: 64 bits.  Parameters ≤ 64 bits (including `Bool`, `U8`,
`U16`, `U32`, `U64`) are always passed directly.  Parameters > 64 bits
(e.g. `Arr(U8, 16)` = 128 bits, AES state) use stack-based passing.

**Compatibility**: Callers and callees **must** use the same ABI setting.
Mixing `optimized_abi = true` callers with `optimized_abi = false` callees
(or vice versa) produces incorrect code.  The flag is a runtime property
of the `VaffleTarget` instance, so mixed-mode is prevented by using a
single target instance for all functions in a module.

**Cost**: The optimization trades block-parameter count (which drives
spill/reload) for storage operations (which are cheaper in the Dyn-based
call convention).  For a function with three 128-bit parameters:

| Metric | Flat ABI | Optimized ABI |
|---|---|---|
| Block params per call | 384 | 96 |
| StorageWrite per call | 0 | 384 |
| StorageRead per entry | 0 | 384 |
| Spill slots (3 calls) | 384 × 3 | 96 × 3 |

The net effect is a 4× reduction in spill/reload traffic (the dominant cost
in the `lower_to_ir` pass).

**Usage**:
```rust
let mut target = VaffleTarget::new().with_optimized_abi();
// or:
let mut target = VaffleTarget::new();
target.set_optimized_abi(true);
```

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

| Capability | `VolarIrTarget` | `VaffleTarget` (default) | `VaffleTarget` (optimized) | `CBackend` |
|---|---|---|---|---|
| `abi()` | `CIRCUIT` | `CIRCUIT` | `VAFFLE_OPTIMIZED` | `C_NATIVE` |
| `stack_alloc_ext()` | `None` | `Some` | `Some` | `Some` |
| `native_aggregates` | No | No | No | Yes |
| `aggregate_byval_limit` | ∞ | ∞ | 64 | 64 |
| Value representation | Flat bits | Flat bits | Flat bits + stack ptrs | Native C types |
| Extern calls | Inline | Inline | Inline + stack-based args | C function calls |
| Pointer types | Panic | 32-bit address | 32-bit address | C pointer |
| `optimized_abi` flag | N/A | `false` | `true` | N/A |
