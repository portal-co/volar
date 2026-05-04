# Virtualization Pass

`crates/ir/volar-ir-virt` — marked `@reliability: experimental`

The pass converts a multi-block `IRBlocks` (or `BIrBlocks`) into a
**handler-per-unique-skeleton** module plus a bytecode table.  The key
compile-time win is that the backend prints one body per unique handler
rather than one body per original block; with typical patterns this
reduces code volume by an order of magnitude.

---

## Overview

```
IRBlocks (N blocks)
        │
        │  deduplicate_oracle_calls_in_block (CSE)
        ▼
IRBlocks (N blocks, oracle calls merged within each block)
        │
        │  canonicalize_ir_block  ──────────────► IrHandlerKey (structural)
        │                                          BlockImmediates (lifted values)
        ▼
DedupTable<IrHandlerKey>          ──────────────► n_handlers unique bodies
        │
        │  GlobalLayout::from_dedup
        │  RegAlloc::build
        │
        │  emit_output_ir
        ▼
VirtOutput {
    IRBlocks (handler module),
    Option<VirtBytecode> (external table),
}
```

The emitted `IRBlocks` contains a fixed set of infrastructure blocks
followed by the handler bodies.

---

## Dispatcher Loop

### Legacy layout (`direct_dispatch: false`)

```
block 0  SETUP          — writes entry params to registers; seeds bytecode
                          storage (InIr form); jumps to DISPATCHER with (pc=0, done=0)
block 1  DISPATCHER     — JumpCond(done, RETURN, DISPATCH(pc))
block 2  RETURN         — reads return registers; Jmp(Return, values)
block 3  DISPATCH       — reads handler_idx at pc; JumpTable to handler_i(pc)
block 4  handler_0(pc)  ─┐
block 5  handler_1(pc)   │  one per unique handler key
  ...                    │
block N  handler_{n-1}  ─┘
block N+ arm sub-blocks  — one per arm of conditional terminators
```

Hot path per step: `handler_i → DISPATCHER → DISPATCH → handler_j`  (3 hops).

### Direct-dispatch layout (`direct_dispatch: true`)

```
block 0  SETUP          — same body; jumps to INIT_DISPATCH with (pc=0)
block 1  RETURN         — same body
block 2  INIT_DISPATCH  — reads handler_idx at pc=0; JumpTable to handler_i(0)
block 3  handler_0(pc)  ─┐
  ...                    │  one per unique handler key
block M  handler_{n-1}  ─┘
block M+ arm sub-blocks + dispatch sub-blocks
```

Hot path per step: `handler_i → dispatch_sub → handler_j`  (2 hops).

Each arm of a handler's terminator emits one **dispatch sub-block**: it
receives `next_pc`, reads `handler_idx` from bytecode at `next_pc`, and
dispatches via `JumpTable` to the correct successor handler.

BIR direct-dispatch is not implemented.  Inlining the O(n)-deep `CondJmp`
tree at every handler would cause O(n²) block growth; the DISPATCHER /
DISPATCH structure is kept for BIR.

---

## Bytecode Layout

Each original block `B` occupies one **row** of the bytecode storage
(`StorageId::VIRT_BYTECODE` by default).  The row is indexed by `B`'s
original block index (= its program counter value).

### Row structure

```
offset 0           handler_idx  (_32)   — which handler body runs at this pc
offset 1 .. K      per-handler slots    — one StorageId per slot
```

The per-handler slots are defined by `HandlerSchema` and `GlobalLayout`.
Each slot is assigned a unique `StorageId` starting one above the base
bytecode storage id.

### Slot kinds (`SlotKind`)

| Kind | Type | Meaning |
|------|------|---------|
| `ParamSrcReg` | `_32` | Register index from which the handler reads param `i` |
| `ConstValue` | varies | Lifted `Stmt::Const` value or `Stmt::Poly` constant term |
| `TargetPc` | `_32` | Next pc for terminator arm `k` |
| `DoneFlag` | `Bit` | 1 iff terminator arm `k` targets `Return` |
| `ArgDstReg` | `_32` | Register index to which the handler writes arg `j` of arm `k` |

### HandlerSchema and ArmSchema

```rust
struct HandlerSchema {
    slots: Vec<HandlerSlot>,
    param_src_slot: Vec<usize>,           // slot index for each block param
    const_value_slot: Vec<Option<usize>>, // slot index for each Const/Poly stmt
    arms: Vec<ArmSchema>,                 // one per terminator arm
}

struct ArmSchema {
    next_pc_slot: usize,
    done_slot: usize,
    arg_dst_slots: Vec<usize>, // one per arg passed to the target block
}
```

`GlobalLayout` maps `handler_idx × slot_idx` to an absolute `StorageId`,
keeping the bytecode storages dense and collision-free with the register
file.

---

## Register Allocation

Block parameters and return values are routed through typed **register
files** — one `StorageId` per `IRTypeId` that appears as a param or return
type.  The register index is encoded in the `ParamSrcReg` / `ArgDstReg`
slots of each bytecode row.

### Assignment rules

For each type `T`:

- Block `B`'s `k`-th param of type `T` is assigned register index `k`
  (0-based within `B`'s own params of type `T`).
- The total number of `T`-registers needed for params = `max_T_params`
  (the maximum count across all blocks).
- **Return registers** for type `T` are placed starting at index `0`,
  overlapping with the param range.  Return registers are only written in
  the final execution step (the done=1 arm) and read by the RETURN block
  immediately after, so they are never live simultaneously with any param.
  The effective register file depth for `T` = `max(max_T_params, n_T_returns)`.

Register files are assigned `StorageId`s starting strictly above the last
bytecode slot (`GlobalLayout::next_free_storage_after_bytecode`).

---

## Deduplication

### Canonicalization

`canonicalize_ir_block` (canon.rs) transforms each input block into a pair:

- **`IrHandlerKey`** — the structural skeleton: param type list, stmts with
  all immediate fields replaced by placeholders, terminator with all
  concrete block ids replaced by `ZERO_BLOCK_ID`.
- **`BlockImmediates`** — the lifted immediate values: a `Vec<Constant>`
  for every `Stmt::Const` and `Stmt::Poly.constant`, and a `Vec<IRBlockId>`
  for every terminator target.

Two blocks with the same `IrHandlerKey` can share a single handler body;
the actual runtime values are recovered at each step by reading the row's
`ConstValue` and `TargetPc` / `DoneFlag` slots.

### What is lifted vs. structural

| Lifted (per-row immediate) | Structural (part of handler key) |
|---|---|
| `Stmt::Const.value` | `Stmt::Const` presence and type |
| `Stmt::Poly.constant` | `Stmt::Poly.coeffs` and type |
| Terminator block target ids | Terminator shape (Jmp / JumpCond / JumpTable) |

### Oracle call CSE

Before canonicalization, `deduplicate_oracle_calls_in_block` removes
redundant `OracleCall` stmts within each block.  Two calls with the same
name and the same (already-remapped) argument variables are identical; the
second is dropped and its output variable is redirected to the first.  This
shrinks handler keys and enables cross-block dedup between a block with two
identical oracle calls and a block with one.

### DedupTable

```rust
struct DedupTable<K> {
    per_block: Vec<(u32, BlockImmediates)>, // handler_idx + immediates for each original block
    handler_keys: Vec<K>,                   // one entry per unique handler
}
```

`DedupTable::build` inserts each canonical key into a `BTreeMap`; duplicate
keys reuse the existing handler index.

---

## Dispatch Modes

### `DispatchMode::Public` (default)

The DISPATCH block routes control via `IRTerminator::JumpTable` (IR) or a
balanced binary `CondJmp` tree (BIR) on `handler_idx`.  Assumes all party's
view of `handler_idx` agrees; this holds for all Volar programs whose
control-flow is public.

### `DispatchMode::Oblivious`

Every handler runs every step and results are multiplexed with
`is_active · val + (1 - is_active) · fallback` accumulators via
`movfuscate_ir`.  Safe for witness-dependent control flow.  Emitted by
calling `volar_ir_passes::movfuscate_ir` on the output of `virtualize_ir`.

---

## Bytecode Forms

| `BytecodeForm` | Effect |
|---|---|
| `InIr` | SETUP block emits `StorageWrite` for every bytecode row |
| `External` | `VirtBytecode` artifact returned; IR contains no writes |
| `Both` (default) | Both forms emitted simultaneously |

`VirtBytecode` contains a `Vec<BytecodeEntry>` — one row per original block
— suitable for materializing as a `const` array in Rust / TypeScript / C
backends.

---

## DedupPolicy

| Policy | Status | What is lifted |
|---|---|---|
| `ConstantsAndTargets` | Implemented | `Stmt::Const`, `Stmt::Poly.constant`, terminator target ids |
| `Maximal` | Reserved (panics) | Also `Rol.n`, `Ror.n`, `Shuffle.result_bits`, oracle names |

---

## Key Types Reference

| Type | Crate / file | Role |
|---|---|---|
| `VirtualizeConfig` | `lib.rs` | Knobs: dispatch mode, bytecode form, dedup policy, direct dispatch |
| `DedupTable<K>` | `ctx.rs` | Maps original blocks to handler indices + immediates |
| `VirtOutput<M>` | `ctx.rs` | Output module + optional `VirtBytecode` |
| `IrHandlerKey` | `canon.rs` | Canonical (structural) block key for IR |
| `BirHandlerKey` | `canon.rs` | Canonical block key for BIR |
| `BlockImmediates` | `canon.rs` | Lifted constants and jump targets for one block |
| `HandlerSchema` | `ir.rs` | Slot layout for one handler |
| `ArmSchema` | `ir.rs` | Slot layout for one terminator arm |
| `GlobalLayout` | `ir.rs` | Per-handler slot → `StorageId` mapping |
| `RegAlloc` / `RegRef` | `ir.rs` | Per-type register file assignment |
| `VirtBytecode` | `bytecode.rs` | External bytecode table artifact |
| `BytecodeEntry` | `bytecode.rs` | One row of the external bytecode table |

---

## Extension Points and Limitations

**`DedupPolicy::Maximal`** is the natural next step: lift `Rol.n`, `Ror.n`,
`Shuffle.result_bits`, and oracle / action names as immediates, enabling
blocks that differ only in shift distances or oracle names to share handlers.

**BIR direct dispatch** is not implemented because BIR uses a `CondJmp`
tree for dispatch (no `JumpTable`).  Inlining the full O(n)-deep tree at
every handler produces O(n²) blocks — impractical for large circuits.  A
future approach could use a compact binary-encoded dispatch table instead.

**`IRBlockTargetId::Dyn`** is supported.  The source variable must have type
`IRType::Block { params }` at the terminator site; it is `Transmute`d to `_32`
inside the handler body to produce `next_pc`.  Argument destination registers
are derived from the Block type's `params` signature using the standard
per-type sequential rule.
