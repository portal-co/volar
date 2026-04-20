# WebAssembly Feature Support

This document describes which WebAssembly features and proposals are supported
at each layer of the WASM → Volar IR pipeline.

The pipeline has two layers with distinct limitations:

1. **WAFFLE IR** (`portal-pc-waffle-ir`) — the WebAssembly IR library used as
   the frontend parser and SSA representation.
2. **`volar-vaffle-target`** — the crate that lowers WAFFLE IR into Volar's
   bit-circuit IR (`waffle_lower.rs`).  This layer has the tightest constraints
   because it must express everything in terms of GF(2) Poly circuits.

The entry point for the pipeline is `lower_waffle_module` in
`crates/ir/volar-vaffle-target/src/waffle_lower.rs`.

---

## Layer 1: WAFFLE IR

WAFFLE parses standard WebAssembly binaries into an SSA representation.  Its
coverage of the WASM specification is listed below.

### Types

| Type | WAFFLE IR | Notes |
|------|-----------|-------|
| `i32`, `i64` | ✅ Full | |
| `f32`, `f64` | ✅ Full | Parsed; passed through IR |
| `v128` (SIMD) | ✅ Full | Parsed; all SIMD operators present |
| `funcref`, `externref` | ✅ Full | Via `Type::Heap(WithNullable<HeapType>)` |
| GC ref types (`anyref`, `eqref`, `i31ref`, `structref`, `arrayref`, etc.) | ✅ Full | All represented in `HeapType` enum |
| `exnref`, `noexn` | ⚠️ Type only | Types present; no throw/catch operators or terminators |

### Terminators (control flow)

| WASM construct | WAFFLE terminator | Status |
|----------------|-------------------|--------|
| `br`, `br_if` | `Br`, `CondBr` | ✅ Full |
| `br_table` | `Select` | ✅ Full |
| `return` | `Return` | ✅ Full |
| `return_call` | `ReturnCall` | ✅ Full |
| `return_call_indirect` | `ReturnCallIndirect` | ✅ Full |
| `return_call_ref` | `ReturnCallRef` | ✅ Full (null-ref edge case is `todo!()` in interpreter) |
| `unreachable` | `Unreachable` | ✅ Full |
| `throw`, `catch`, `try_table` | — | ❌ Not implemented — no exception terminators exist in the IR |

### Operator categories

| Category | Status | Notes |
|----------|--------|-------|
| MVP integers (`i32`/`i64` arith, bitwise, shifts, rotates, clz/ctz/popcnt, rem, comparisons, conversions) | ✅ Full | All operators present |
| MVP floats (`f32`/`f64` all ops, reinterprets, conversions) | ✅ Full | All operators present |
| SIMD / V128 (~200+ ops) | ✅ Full | All operators present |
| Atomics / threads (notify, wait, fence, RMWs, CAS) | ✅ Full | All operators present |
| Bulk memory (`memory.copy`, `memory.fill`, `memory.init`, `data.drop`, table bulk ops) | ✅ Full | All operators present |
| Reference types (`ref.null`, `ref.is_null`, `ref.func`, `ref.eq`) | ✅ Full | |
| GC proposal (`struct.new/get/set`, `array.new/get/set/len/fill/copy`, `ref.cast`, `ref.test`, `i31.get`, etc.) | ✅ Full | |
| Exception handling (`throw`, `throw_ref`, `catch`, `try_table`) | ❌ Not implemented | No operators or terminators; `exnref` type exists |
| Stack switching (continuation/coroutine proposal) | ❌ Not implemented | Derives from exception handling infrastructure |
| Multi-memory | ✅ Full | `MemoryArg` carries a `Memory` index |
| Memory64 | ⚠️ Partial | `memory64` flag representable; backend behavior unverified |
| Tail calls | ✅ Full | Via `ReturnCall`/`ReturnCallIndirect`/`ReturnCallRef` terminators |
| Multi-value | ✅ Full | Block params, multi-return all supported |

---

## Layer 2: `volar-vaffle-target` frontend

This layer performs the actual bit-decomposition.  Its constraints are
independent of WAFFLE's and are much tighter: only operations that can be
expressed as GF(2) Poly circuits over `I32`/`I64` values are accepted.

Functions that contain any unsupported type or operator return `UnsupportedOp`.
`lower_waffle_module` skips such functions and collects errors; it does not
panic.

### Types

| Type | Status | Notes |
|------|--------|-------|
| `i32` | ✅ Supported | Lowered to `LirType::U32` (32 bits) |
| `i64` | ✅ Supported | Lowered to `LirType::U64` (64 bits) |
| `f32`, `f64` | ❌ Not supported | `UnsupportedOp` — no float circuit |
| `v128` | ❌ Not supported | `UnsupportedOp` |
| Any reference/heap type | ❌ Not supported | `UnsupportedOp` |
| Multi-value return | ❌ Not supported | `UnsupportedOp("multi-value return")` |

### Operators

#### Supported

| Category | Operators |
|----------|-----------|
| Constants | `I32Const`, `I64Const` |
| Arithmetic | `I32/I64`: `add`, `sub`, `mul`, `divS`, `divU` |
| Bitwise | `I32/I64`: `and`, `or`, `xor`, `shl`, `shrS`, `shrU` |
| Comparisons | `I32/I64`: `eqz`, `eq`, `ne`, `ltS`, `ltU`, `gtS`, `gtU`, `leS`, `leU`, `geS`, `geU` |
| Conversions | `I32WrapI64`, `I64ExtendI32S`, `I64ExtendI32U` |
| Select | `Select`, `TypedSelect` (I32 non-zero condition via OR-reduce) |
| Direct call | `Call { function_index }` — single return value only |
| Nop | `Nop` |
| Memory loads | `I32Load`, `I64Load`, `I32Load8U/S`, `I32Load16U/S`, `I64Load8U/S`, `I64Load16U/S`, `I64Load32U/S` |
| Memory stores | `I32Store`, `I64Store`, `I32Store8`, `I32Store16`, `I64Store8`, `I64Store16`, `I64Store32` |
| Memory stubs | `MemorySize` (always 0 pages), `MemoryGrow` (always returns -1) |

#### Not supported

| Category | Examples | Notes |
|----------|----------|-------|
| Integer ops not listed above | `rem`, `clz`, `ctz`, `popcnt`, `rotl`, `rotr`, sign-extend variants (`extend8S`, `extend16S`, `extend32S`), `trunc_sat*` | `UnsupportedOp` |
| Floats | All `F32*`/`F64*` operators, `F32Load`, `F64Load`, `F32Store`, `F64Store` | `UnsupportedOp` |
| SIMD | All `V128*` operators | `UnsupportedOp` |
| Atomics / threads | All `MemoryAtomic*` and `Atomic*` operators | `UnsupportedOp` |
| Indirect / ref calls | `CallIndirect`, `CallRef` | `UnsupportedOp` |
| Multi-result direct call | `Call` with >1 return value | `UnsupportedOp("multi-result Call")` |
| Globals | `GlobalGet`, `GlobalSet` | `UnsupportedOp` |
| Tables | `TableGet`, `TableSet`, `TableGrow`, `TableSize`, `TableFill` | `UnsupportedOp` |
| Bulk memory | `MemoryCopy`, `MemoryFill`, `MemoryInit`, `DataDrop`, `TableCopy`, `ElemDrop`, `TableInit` | `UnsupportedOp` |
| Reference types | `RefNull`, `RefIsNull`, `RefFunc` | `UnsupportedOp` |
| GC proposal | `StructNew`, `ArrayNew`, `RefCast`, `RefTest`, etc. | `UnsupportedOp` |
| `Unreachable` operator | (as an operator, distinct from the `Unreachable` terminator) | `UnsupportedOp` |

### Terminators

| WAFFLE terminator | Status | Notes |
|-------------------|--------|-------|
| `Br` | ✅ Supported | Emits `tgt.jump()` |
| `CondBr` | ✅ Supported | Emits `tgt.branch()`; I32 condition OR-reduced to a bit |
| `Return` | ✅ Supported | Emits `tgt.ret()` |
| `ReturnCall` | ✅ Supported | Emits `tgt.ret_call()` — O(1) stack tail call |
| `Unreachable`, `UB`, `None` | ✅ Stubbed | Emits `ret(0)` or `ret()` |
| `Select` (br_table) | ❌ Not supported | `UnsupportedOp` |
| `ReturnCallIndirect` | ❌ Not supported | `UnsupportedOp` |
| `ReturnCallRef` | ❌ Not supported | `UnsupportedOp` |

### Memory model

Linear memory is lowered to `StorageId::memory(idx)` with one 8-bit cell per
byte.  Multi-byte loads/stores split into individual byte reads/writes
reassembled via bit concatenation (LSB-first).  The memory index from
`MemoryArg` is forwarded, so multi-memory modules are handled correctly as long
as all accessed memories are within the supported type subset.

Memory addresses are 32-bit (`MEM_ADDR_BITS = 32`).  Memory64 (64-bit
addresses) is not supported.

---

## Feature Summary Table

| WASM feature | WAFFLE IR | `volar-vaffle-target` |
|---|---|---|
| `i32` / `i64` | ✅ | ✅ |
| `f32` / `f64` | ✅ | ❌ |
| SIMD (`v128`) | ✅ | ❌ |
| Atomics / threads | ✅ | ❌ |
| Bulk memory | ✅ | ❌ |
| Reference types | ✅ | ❌ |
| GC proposal | ✅ | ❌ |
| Exception handling | ❌ | ❌ |
| Stack switching | ❌ | ❌ |
| Tail calls (direct) | ✅ | ✅ |
| Tail calls (indirect / ref) | ✅ | ❌ |
| Multi-value | ✅ | ❌ |
| Multi-memory | ✅ | ✅ |
| Memory64 | ⚠️ Partial | ❌ |
| Globals | ✅ | ❌ |
| Tables / `call_indirect` | ✅ | ❌ |
| `br_table` | ✅ | ❌ |
| Integer rem / clz / ctz / popcnt / rot | ✅ | ❌ |
| Integer sign-extend variants | ✅ | ❌ |
