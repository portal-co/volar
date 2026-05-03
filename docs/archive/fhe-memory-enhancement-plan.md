# FHE Oblivious Memory Enhancement Plan

## Overview

Enhance the FHE oblivious memory access system in `volar-weaver` with four phases:
1. Auto-derive storage sizes from IR analysis
2. Add storage hooks to `FheScheme` trait + TFHE PBS override
3. Loop-based codegen option
4. ORAM (deferred future work)

## Execution Order

```
Phase 1 (auto-derive)  -->  independent
Phase 2a (trait hooks)  -->  independent; enables 2b and 3
Phase 2b (PBS)          -->  depends on 2a + spec-layer PBS function
Phase 3 (loop codegen)  -->  depends on 2a (uses trait methods)
Phase 4 (ORAM)          -->  depends on 2a; deferred
```

Recommended: **1 -> 2a -> 3 -> 2b** (PBS last, riskiest).

---

## Phase 1: Auto-Derive Storage Sizes from IR Analysis

**Goal**: Make `FheStorageSizes` optional by inferring cell counts from the BIR circuit.

**Files**: `crates/compiler/volar-weaver/src/fhe.rs`

**Implementation**:

1. New function `derive_storage_config(circuit: &BIrBlocks) -> FheStorageConfig`:
   - Walk `circuit.0[0].stmts` (single block circuit)
   - For each `StorageRead`/`StorageWrite`, record `(storage.0, bit_width)` -> `max(addr.len())`
   - Cell count = `1 << max_addr_len` (or 1 if addr empty)
   - Return `FheStorageConfig { sizes }`

2. Update `weave_fhe_flat` (~line 471): After movfuscation, if `storage` is `None`, call `derive_storage_config(&circuit)`. If `Some`, use explicit config.

3. Update `weave_fhe` (~line 452): Same logic.

4. Tests: Add test passing `None` for storage config, verify auto-derived config matches explicit.

**Effort**: ~50 lines.

---

## Phase 2a: Trait Methods with Default MUX/Demux

**Goal**: Make storage access strategy customizable per scheme.

**Files**: `crates/compiler/volar-weaver/src/fhe.rs`

**Implementation**:

1. Add to `FheScheme` trait:
   ```rust
   fn emit_oblivious_read<Q: Clone + Default>(
       &self,
       cells: &[String],       // current wire names for all cells
       addr_wires: &[String],  // address bits (LSB first)
       counter: &mut usize,    // unique name counter
   ) -> (Vec<IrStmt<Q>>, String);  // (emitted statements, result wire name)

   fn emit_oblivious_write<Q: Clone + Default>(
       &self,
       cells: &[String],       // current wire names
       addr_wires: &[String],  // address bits
       src_wire: &str,         // value to write
       counter: &mut usize,
   ) -> (Vec<IrStmt<Q>>, Vec<String>);  // (emitted stmts, new cell wire names)
   ```

2. Default implementations: Extract existing `mux_tree_read` and demux logic into these defaults.

3. Refactor `FheStorageCtx`: `emit_read`/`emit_write` delegate to `scheme.emit_oblivious_read()`/`emit_oblivious_write()`.

4. `GrafhenScheme` inherits defaults (no change).

**Effort**: ~150 lines.

---

## Phase 2b: TFHE PBS-Based Oblivious Read

**Goal**: 1 bootstrapping per read instead of O(N).

**Files**: `crates/spec/volar-spec/src/tfhe.rs`, `crates/compiler/volar-weaver/src/fhe.rs`

**Spec layer**:

1. New `tfhe_programmable_bootstrap(ct, lut, bk) -> LweCiphertext`:
   - Same as `tfhe_gate_bootstrapping_and` but caller-supplied LUT polynomial
   - Reuses blind rotation + sample extraction + key switching

2. New `tfhe_oblivious_read(addr, cells, bk) -> LweCiphertext`:
   - Pack cells into LUT polynomial
   - Single PBS call
   - Cost: 1 bootstrapping per value bit

**Weaver layer**:

3. Add `StorageStrategy` enum to `TfheScheme`:
   ```rust
   pub enum StorageStrategy { MuxTree, Pbs }
   ```

4. Override `emit_oblivious_read` in `TfheScheme` for `Pbs` strategy.

5. Write stays MUX/demux (PBS doesn't help writes).

**Effort**: ~350 lines total.

---

## Phase 3: Loop-Based Codegen Option

**Goal**: Generate loop-based oblivious access code instead of unrolled trees.

**Key**: IR already supports `BoundedLoop`, `If`, `Assign`, `Index`, `Block` -- zero IR/printer changes needed.

**Files**: `crates/compiler/volar-weaver/src/fhe.rs`

**Implementation**:

1. `emit_loop_read`: Generate:
   ```rust
   let mut result = zero();
   for i in 0..N {
       let cell_val = storage[i].clone();
       let matches = addr == i;
       result = mux(matches, result, cell_val);
   }
   ```

2. `emit_loop_write`: Generate:
   ```rust
   for i in 0..N {
       let matches = addr == i;
       storage[i] = mux(matches, storage[i], src);
   }
   ```

3. Configuration enum:
   ```rust
   pub enum ObliviousCodegen { Unrolled, Loop }
   ```

4. Guard: Only applicable when generated output supports loops (Rust target: yes; raw circuit: no).

**Effort**: ~100 lines.

---

## Phase 4: ORAM (Deferred)

**Goal**: O(sqrt(N)) amortized for large memory (>256 cells).

**Approach**: Square-root ORAM.
- Stash of O(sqrt(N)) recent entries
- Flush every sqrt(N) accesses with oblivious shuffle
- Read: linear scan stash + amortized main memory scan

**Constraints**:
- State management across invocations
- Oblivious shuffle needs Waksman network O(N log N) or client hint (leaks)
- Only worth it for N > ~256

**Effort**: ~500+ lines. High risk.

---

## Key Architecture Decisions

- `FheStorageSizes` type: `BTreeMap<(u32, usize), usize>` -- `(StorageId.0, bit_width) -> cell_count`
- `FheStorageCtx` tracks cells as `BTreeMap<(u32, usize, usize), String>` -- `(sid, bw, cell_idx) -> wire_name`
- Cell count from addr: `1 << addr.len()` (addr is `Vec<IRVarId>`, each element is 1 bit)
- `effective_addr_width(count)` = `ceil(log2(count))`
- MUX tree cost: `(next_pow2(N) - 1)` AND gates per value bit for read
- Demux+MUX write cost: ~`addr_width * N` AND gates
- PBS read cost: 1 bootstrapping per value bit (up to 2*BIG_N cells)
- VOLE commitment mode: 0 AND gates, uses oracle hints -- NOT portable to FHE

## Relevant Files

- `crates/compiler/volar-weaver/src/fhe.rs` -- main file for all phases
- `crates/spec/volar-spec/src/tfhe.rs` -- Phase 2b spec layer
- `crates/ir/volar-ir/src/boolar.rs` -- BIrStmt definitions
- `crates/compiler/volar-weaver/src/lib.rs` -- re-exports
- `crates/compiler/volar-compiler/src/` -- IrExpr/IrStmt definitions (Phase 3 uses existing nodes)
