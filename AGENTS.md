# Volar Agent Context

> **Before editing any file, read [`docs/agents-guide.md`](docs/agents-guide.md)
> and [`docs/reliability.md`](docs/reliability.md).** Volar gates AI edits by
> capability tier in addition to reliability level. The summary lives in this
> file; the authoritative version is in `docs/reliability.md`.

## Project Mission

Volar's goal is to increase adoption of **program-related cryptography** —
zero-knowledge proofs, garbled circuits, multi-party computation, and the
primitives that underpin them — by implementing it in auditable Rust,
compiling it to other targets (TypeScript, C), and developing it publicly
with rigorous reliability tracking.

The current implementation focus is VOLE-based ZK proofs and garbled
circuits. These were chosen because they share a clean common substrate
(VOLE correlations, boolean circuits, binary extension-field arithmetic)
that the compiler and IR were designed around. Other schemes and protocols
will follow as the infrastructure matures.

When deciding what to implement or how to design a component, prefer
choices that:
- Are useful to the broadest set of program-related cryptography
  applications (not just the current VOLEitH construction).
- Keep the IR, compiler, and spec layer general enough to support future
  protocols.
- Follow the reliability system: new cryptographic constructions start at
  Experimental, not Normal.

## AI Capability Tiers (gating policy)

The reliability level says how trusted the *code* is. The capability tier
says how trusted the *AI agent* must be to modify it.

| Tier | Claude models | GPT models | What this tier may modify |
|---|---|---|---|
| **Tier 1 — Glue** | Any current Claude model | Any current GPT model not listed for Tier 2 or Tier 3, including GPT-5.4-Mini and earlier small/fast variants | Documentation, formatting, mechanical refactors, test scaffolding |
| **Tier 2 — Compiler** | Sonnet 4.6+ or Opus 4.5+ | Full GPT-5.2, GPT-5.3-Codex, full GPT-5.4, GPT-5.5, and later non-mini successors | Compiler, IR, lowering, weavers, backends, fuzzing |
| **Tier 3 — Cryptography** | **Opus 4.6+ only** (Sonnet not permitted at this tier) | **GPT-5.5+ only** | `volar-spec`, `volar-primitives`, `volar-common`, `volar-oram*`, GRAFHEN/TFHE schemes, Hazmat code |

**Crate-default tiers:**

| Path | Default tier |
|---|---|
| `crates/spec/volar-spec/`, `crates/spec/volar-primitives/`, `crates/spec/volar-common/` | **3** |
| `crates/oram/volar-oram-core/`, `crates/oram/volar-oram/` | **3** |
| `crates/compiler/`, `crates/ir/`, `crates/macros/` | 2 |
| `crates/channel/volar-channel/`, `crates/fuzz/volar-fuzz/`, `crates/spec/volar-spec-dyn/`, `crates/spec/volar-dyn/` | 2 |
| `docs/`, `*.md`, `Cargo.toml`/`Cargo.lock` | 1 (Cargo files: 2) |

A file may explicitly raise its required tier with `// @ai-tier: 3` placed
next to its `@reliability` marker. The marker can only raise the
requirement, never lower it.

**If your tier is below the file's required tier, perform a reasoning pass
first** — reason through whether the change is genuinely cryptographic or
merely structural, surface that reasoning to the owner, and wait for an
explicit go/no-go before proceeding or writing a hand-off document. See
[`docs/agents-guide.md` § 5](docs/agents-guide.md#5-when-you-are-blocked).

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

1. **Compiler IR Genericity**: All code constructing `IrModule` must use
   typed `IrExpr`/`IrStmt` nodes — never embed raw Rust strings as
   expression text, and never store pre-rendered strings as IR data fields.
   This applies to every IR struct field, including new additions like
   `IrConst.value`. Use `IrExpr::MethodCall`, `IrExpr::Binary`,
   `IrExpr::StructExpr`, `IrExpr::Var`, `IrExpr::Call` + `IrExpr::Path`,
   `IrType::Struct { kind: Custom("..."), .. }`. If the printer mishandles
   a node, **fix the printer** — don't work around it with raw strings or
   pre-rendered values. The printer's `debug_assert` guardrails on ident
   characters exist to catch injection.

2. **Tests for Generated IR**: Tests must **lower and compile the output**
   (real backend: `print_module` → `rustc`), not perform syntactic IR
   analysis. Do not assert on variable names, statement counts, or IR
   structure unless verifying a hard-to-change structural invariant. The
   correctness signal is: the generated code compiles and runs correctly.

3. **Reliability Tags**: Files tagged `// @reliability: experimental`
   contain unreviewed cryptographic code. New code depending on these must
   not be deployed without separate review. `@ai: none` / `@ai: assisted`
   tags indicate AI involvement. **Tier 3 files** (see above) may only be
   modified by Opus 4.6+ or GPT-5.5+.

4. **Catch-all arms**: Use `_ =>` catch-alls on IR type matches to support
   parallel development.

5. **Deterministic spec**: `volar-spec` must be fully deterministic and
   NOT use the `rand` crate. A `SpecRng` trait is defined in `lib.rs`.

6. **Never specialize on test cases**: Extend tests instead.

7. **GRAFHEN XOR**: Garbled circuits have truly free composable XOR. Do
   not change `grafhen_xor`.

8. **CFG vs flat AST**: Cannot convert CFG AST to normal AST — the normal
   AST is total while the CPS AST doesn't need to be.

9. **`IrExpr::RawMap`**: Use for portable `[T; N]::map` expressions (not
   `MethodCall` + `Closure`).

10. **Keep witness analysis and deshadowing for CFG modules**: The CFG
    AST contains copies of spec functions (via `auxiliary_functions`) that
    have the same shadowing and witness patterns as regular `IrModule`
    functions.

## Topic Context Files

Load these when working in the relevant area:

| Topic | File | When to load |
|---|---|---|
| Reliability + AI tiers | `docs/reliability.md` | Always — first thing before editing |
| Operating procedure for AI agents | `docs/agents-guide.md` | Always — first thing before editing |
| User-facing integration | `docs/integration-guide.md` | When answering questions about app integration |
| IR types, storage, Poly semantics | `docs/agent-context/ir-types-storage.md` | Working on IR, lowering, evaluators, store-forward, fuzzer generators |
| Weaving & multi-backend | `docs/agent-context/weaving.md` | Working on FHE/garbled-circuit weaving, compiler printers (Rust/TS/C), action system, CFG emission |
| ORAM & channel | `docs/agent-context/oram.md` | Working on ORAM crates, channel protocol, ORAM weaver integration |
| Progress tracking | `PROGRESS.md` | Starting a new session, reviewing status, planning next steps |
| Top-level doc index | `docs/README.md` | Looking for a specific subsystem reference |
