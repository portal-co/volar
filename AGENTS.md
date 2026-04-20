# Volar Agent Context

## Project Mission

Volar's goal is to increase adoption of **program-related cryptography** — zero-knowledge proofs, garbled circuits, multi-party computation, and the primitives that underpin them — by implementing it in auditable Rust, compiling it to other targets (TypeScript, C), and developing it publicly with rigorous reliability tracking.

The current implementation focus is VOLE-based ZK proofs and garbled circuits. These were chosen because they share a clean common substrate (VOLE correlations, boolean circuits, binary extension-field arithmetic) that the compiler and IR were designed around. Other schemes and protocols will follow as the infrastructure matures.

When deciding what to implement or how to design a component, prefer choices that:
- Are useful to the broadest set of program-related cryptography applications (not just the current VOLEitH construction).
- Keep the IR, compiler, and spec layer general enough to support future protocols.
- Follow the reliability system: new cryptographic constructions start at Experimental, not Normal.

## Crate Constraints

| Crate | `std` | Notes |
|---|---|---|
| `volar-ir-opt` | `#![no_std]` + `extern crate alloc` | Use `alloc::vec`, `alloc::vec::Vec`, `alloc::collections::BTreeMap` |
| `volar-spec` | `#![no_std]` | In `#[cfg(test)]` modules: `extern crate std;` + `use std::vec::Vec;` |
| `volar-fuzz` | `std` | Full standard library available |
| `volar-channel` | `#![no_std]` + `extern crate alloc` | |
| `volar-oram` | `#![no_std]` + `extern crate alloc` | |
| `volar-oram-core` | `#![no_std]`, **zero deps** | Pure total Rust, no alloc |
| `volar-weaver` | `#![no_std]` + `extern crate alloc` | Accesses `volar_ir_common` types through re-exports in `volar_ir` |
| `volar-compiler` | `#![no_std]` + `extern crate alloc` | Optional `feature = "std"` |

## Core Design Rules

1. **Compiler IR Genericity**: All code constructing `IrModule` must use typed `IrExpr`/`IrStmt` nodes — never embed raw Rust strings as expression text. Use `IrExpr::MethodCall`, `IrExpr::Binary`, `IrExpr::StructExpr`, `IrExpr::Var`, `IrExpr::Call` + `IrExpr::Path`, `IrType::Struct { kind: Custom("..."), .. }`. If the printer mishandles a node, **fix the printer** — don't work around it with raw strings. The printer's `debug_assert` guardrails on ident characters exist to catch injection.

2. **Tests for Generated IR**: Tests must **lower and compile the output** (real backend: `print_module` → `rustc`), not perform syntactic IR analysis. Do not assert on variable names, statement counts, or IR structure unless verifying a hard-to-change structural invariant. The correctness signal is: the generated code compiles and runs correctly.

3. **Reliability Tags**: Files tagged `// @reliability: experimental` contain unreviewed cryptographic code. New code depending on these must not be deployed without separate review. `@ai: none` / `@ai: assisted` tags indicate AI involvement.

4. **Catch-all arms**: Use `_ =>` catch-alls on IR type matches to support parallel development.

5. **Deterministic spec**: `volar-spec` must be fully deterministic and NOT use the `rand` crate. A `SpecRng` trait is defined in `lib.rs`.

6. **Never specialize on test cases**: Extend tests instead.

7. **GRAFHEN XOR**: Garbled circuits have truly free composable XOR. Do not change `grafhen_xor`.

8. **CFG vs flat AST**: Cannot convert CFG AST to normal AST — the normal AST is total while the CPS AST doesn't need to be.

9. **`IrExpr::RawMap`**: Use for portable `[T; N]::map` expressions (not `MethodCall` + `Closure`).

10. **Keep witness analysis and deshadowing for CFG modules**: The CFG AST contains copies of spec functions (via `auxiliary_functions`) that have the same shadowing and witness patterns as regular `IrModule` functions.

## Topic Context Files

Load these when working in the relevant area:

| Topic | File | When to load |
|---|---|---|
| IR types, storage, Poly semantics | `docs/agent-context/ir-types-storage.md` | Working on IR, lowering, evaluators, store-forward, fuzzer generators |
| Weaving & multi-backend | `docs/agent-context/weaving.md` | Working on FHE/garbled-circuit weaving, compiler printers (Rust/TS/C), action system, CFG emission |
| ORAM & channel | `docs/agent-context/oram.md` | Working on ORAM crates, channel protocol, ORAM weaver integration |
| Progress tracking | `PROGRESS.md` | Starting a new session, reviewing status, planning next steps |
