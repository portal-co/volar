# Fuzzing Infrastructure

> **Referenced by**: `crates/fuzz/volar-fuzz/`, `fuzz/`
>
> **Status**: @reliability: experimental

---

## Overview

The `volar-fuzz` crate provides a structured fuzzing and property-testing
harness for Volar IR passes.  It is used both as a library for `proptest`-based
property tests (compiled under `#[cfg(test)]`) and as a source of `Arbitrary`
implementations for `cargo-fuzz` (libFuzzer) targets.

```
crates/fuzz/volar-fuzz/src/
├── lib.rs              — crate root + module declarations
├── generators/         — proptest strategies that produce valid IR
│   ├── biir.rs         — BIrBlocks generator
│   └── ir.rs           — IRBlocks generator
├── interpreter/        — concrete evaluators
│   ├── biir.rs         — eval_biir: BIrBlocks concrete evaluation
│   └── ir.rs           — eval_ir: IRBlocks typed evaluation
├── arbitrary/          — Arbitrary impls for cargo-fuzz targets
│   ├── biir.rs         — Arbitrary<BIrBlocks>
│   └── ir.rs           — Arbitrary<IRBlocks>
└── properties/         — proptest property tests
    ├── biir_passes.rs  — Properties A, B, C (movfuscate + lower_to_circuit)
    └── ir_passes.rs    — Property D (lower_ir_to_boolar)

fuzz/
├── Cargo.toml
└── fuzz_targets/
    ├── fuzz_biir_lower_to_circuit.rs   — libFuzzer target
    ├── fuzz_biir_movfuscate.rs         — libFuzzer target
    └── fuzz_lower_ir_to_boolar.rs      — libFuzzer target
```

---

## Generator Design

All generators use a **raw-data → interpretation** approach:

1. **Raw data** — proptest/libFuzzer generates plain integers with no structural
   invariants.
2. **Interpretation** — the interpreter clamps all indices to their valid ranges,
   turning arbitrary bytes into structurally valid IR.

This keeps proptest strategies trivially simple (just generate tuples of
integers) while centralising invariant enforcement in one pure function.
The same interpretation logic is reused for the `Arbitrary` impls.

### `BIrBlocks` generator (`generators/biir.rs`)

Structural invariants guaranteed:

- Variable references always point to previously defined SSA vars (params or
  earlier stmts in the same block).
- Jump targets are either `Return` or **forward** block indices — the generated
  CFG is a DAG, guaranteeing termination of non-movfuscated circuits.
- Jump args match the target block's parameter count.
- All `Return` terminators across all blocks have the same arity (required by
  `movfuscate_biir` and `lower_to_circuit`).
- Only pure gate stmts are generated: `Zero`, `One`, `And`, `Or`, `Xor`, `Not`.
  Oracle, action, RNG, and storage stmts are excluded.

```rust
use volar_fuzz::generators::biir::gen_biir_and_inputs;

// proptest strategy: (BIrBlocks<()>, Vec<bool>)
let strategy = gen_biir_and_inputs();
```

### `IRBlocks` generator (`generators/ir.rs`)

Generates single-block `IRBlocks` with integer-typed statements:
`Add`, `Sub`, `Mul`, `BitXor`, `BitAnd`, `BitOr`, and `Const`.  Multi-bit types
(`_8`, `_16`, `_32`, `_64`) are used.

```rust
use volar_fuzz::generators::ir::gen_ir_and_inputs;

// proptest strategy: (IRBlocks<()>, IRTypes, Vec<Vec<u64>>)
let strategy = gen_ir_and_inputs();
```

---

## Interpreters

### `eval_biir` (`interpreter/biir.rs`)

Evaluates `BIrBlocks<()>` block-by-block, following terminators, returning
the output bit-vector when a `Return` is reached.

```rust
pub fn eval_biir(blocks: &BIrBlocks<()>, inputs: &[bool]) -> Option<Vec<bool>>;
pub fn eval_biir_with_limit(blocks: &BIrBlocks<()>, inputs: &[bool], max_iters: usize) -> Option<Vec<bool>>;
```

Returns `None` if block 0 is re-entered more than `MAX_ITERS` (512) times
without terminating.  This handles movfuscated circuits (which are single
self-looping blocks).

Oracle, action, RNG, and storage stmts panic — the generator never produces
them, so this should not occur during property tests.

### `eval_ir` (`interpreter/ir.rs`)

Evaluates `IRBlocks<()>` with typed multi-bit values:

```rust
pub fn eval_ir(
    blocks: &IRBlocks<()>,
    types: &IRTypes,
    inputs: &[Vec<u64>],  // one Vec<u64> per input var (multi-word for wide types)
) -> Option<Vec<Vec<u64>>>;

// Utility: flatten Vec<Vec<u64>> → Vec<bool> (LSB first)
pub fn bit_flatten(inputs: &[Vec<u64>]) -> Vec<bool>;
pub fn bit_unflatten(bits: &[bool], widths: &[usize]) -> Vec<Vec<u64>>;
pub fn bit_width(ty: &IRType, types: &IRTypes) -> usize;
```

---

## Property Tests

Run with:

```
cargo test -p volar-fuzz
```

### Property A — `movfuscate_biir` preserves semantics

```
eval_biir(movfuscate_biir(cfg), movfuscated_inputs) == eval_biir(cfg, inputs)
```

The movfuscated form is a single self-looping block with a PC prefix.
The test constructs the correct padded input and verifies outputs match.

### Property B — `lower_to_circuit` preserves semantics

```
eval_biir(lower_to_circuit(movfuscate_biir(cfg)), inputs)
    == eval_biir(movfuscate_biir(cfg), inputs)
```

Skipped when the movfuscated circuit doesn't terminate within `LOWER_LIMIT = 16`
re-entries.

### Property C — passes do not panic

Both `movfuscate_biir` and `lower_to_circuit` complete without panicking on any
generated input.

### Property D — `lower_ir_to_boolar` preserves semantics

```
bit_unflatten(eval_biir(lower_ir_to_boolar(ir), bit_flatten(inputs)), output_widths)
    == eval_ir(ir, inputs)
```

Verifies that the `IRBlocks → BIrBlocks` lowering pass is semantics-preserving
for the generated integer-arithmetic circuits.

---

## cargo-fuzz Targets

```bash
cd fuzz
cargo +nightly fuzz run fuzz_biir_lower_to_circuit
cargo +nightly fuzz run fuzz_biir_movfuscate
cargo +nightly fuzz run fuzz_lower_ir_to_boolar
```

Each target uses the `Arbitrary` impl from `volar-fuzz::arbitrary` to produce
structurally valid IR from libFuzzer byte input.  Fuzz targets check:

| Target | Check |
|--------|-------|
| `fuzz_biir_lower_to_circuit` | `lower_to_circuit` does not panic |
| `fuzz_biir_movfuscate` | `movfuscate_biir` does not panic |
| `fuzz_lower_ir_to_boolar` | `lower_ir_to_boolar` does not panic |

Corpus seeds for each target are stored in:
`crates/fuzz/volar-fuzz/corpus/properties/biir_passes.txt`

---

## The `lower_ir_to_boolar` Pass

Added in the same work session as the fuzzing harness, this pass lowers
`IRBlocks` → `BIrBlocks` by bit-decomposing multi-bit typed values:

```rust
pub fn lower_ir_to_boolar(blocks: &IRBlocks, types: &IRTypes) -> BIrBlocks<()>;
```

| IR type | Bit count |
|---------|-----------|
| `Bit` | 1 |
| `_8` / `AES8` | 8 |
| `_16` | 16 |
| `_32` | 32 |
| `_64` / `Galois64` | 64 |
| `_128` | 128 |
| `_256` | 256 |
| `Vec(n, T)` | n × bits(T) |
| `Tuple(Ts)` | Σ bits(Tᵢ) |

Pure bit-permutation ops (`Transmute`, `Rol`, `Ror`, `Merge`, `Splat`, `Shuffle`)
update the variable-to-bits map without emitting any new Boolar stmts.

External primitives (`OracleCall`, `ActionCall`, `Rng`, `StorageRead`,
`StorageWrite`) are passed through as opaque Boolar handles.

`JumpTable` and `Dyn` terminators are not supported (panic).

---

## See Also

- `crates/fuzz/volar-fuzz/` — source of truth for all fuzzing infrastructure
- `fuzz/fuzz_targets/` — cargo-fuzz entry points
- `docs/compiler.md` — IR passes overview
