---
name: volar-ir-tester
description: >
  Run the volar-ir-tester binary to compile a .wasm file to Volar IR,
  measure IR size (block count, statement count, text bytes), and optionally
  write the IR text to a .vir file. Use when benchmarking IR output size,
  verifying the lower_to_ir pipeline on a WASM input, or measuring the
  effect of IR optimizations in lower_to_ir.rs.
compatibility: Requires the volar workspace. Rust toolchain with wasm32 support recommended for generating test inputs.
---

## Usage

```bash
cargo run -p volar-ir-tester -- <input.wasm> [output.vir]
```

- `<input.wasm>`: path to any valid WebAssembly binary
- `[output.vir]`: optional path to write the IR text representation (omit for metrics-only)

## Output

```
blocks:     <N>      # total IR blocks across all functions
stmts:      <N>      # total IR statements (instructions)
text bytes: <N>      # byte length of .vir text
wrote:      <path>   # only printed when output path is given
```

## Measuring optimization impact

Run before and after a change in `lower_to_ir.rs`:

```bash
# baseline
cargo run -p volar-ir-tester -- test.wasm before.vir

# apply changes, then:
cargo run -p volar-ir-tester -- test.wasm after.vir

# compare
diff before.vir after.vir | head -60
```

A successful optimization reduces `stmts` and `text bytes`. For the entry-param optimization, expect removed `storage_read` lines in function entry blocks and removed `storage_write` lines at call sites, replaced by additional jump args.

## Getting a test WASM file

Any valid `.wasm` binary works. Quick options:

```bash
# hand-written WAT → WASM (requires wabt)
wat2wasm test.wat -o test.wasm

# minimal Rust → WASM
rustup target add wasm32-unknown-unknown
cargo build --target wasm32-unknown-unknown --release
# binary at target/wasm32-unknown-unknown/release/*.wasm
```

Minimal WAT with a function that takes parameters:
```wat
(module
  (func $add (param i32 i32) (result i32)
    local.get 0
    local.get 1
    i32.add))
```

## Rebuild after changes

```bash
cargo build -p volar-ir-tester
```
