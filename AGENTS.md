# Volar Agent Context

> **Before editing any file, read [`docs/agents-guide.md`](docs/agents-guide.md)
> and [`docs/reliability.md`](docs/reliability.md).** The authoritative
> reliability policy is in `docs/reliability.md`.

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
- Follow the pinnedness/stability policy: new cryptographic constructions start
  Unpinned and Very unstable unless stronger evidence and intended use are
  documented.

## Changelog

## Merged-tree update — 2026-07-22

The maintainer removed capability-tier enforcement, model identification, and
sub-threshold review tags. All agents may contribute; correctness claims rest on
reproducible evidence, pinnedness records, stability commitments, and human
decisions where policy requires them. See
[`docs/handoffs/merge-recovery/policy-and-reliability.md`](docs/handoffs/merge-recovery/policy-and-reliability.md).

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

3. **Pinnedness and Stability Tags**: `// @pinnedness:` records evidence
   binding; `// @stability:` records the dependent-facing change expectation.
   They are independent, except Forever requires Proven. Legacy
   `// @reliability:` markers are migration-only and make no positive claim.
   `@ai: none` / `@ai: assisted` tags record AI involvement, not authority.
   Model identity does not gate a contribution; cryptographic correctness still
   requires the evidence in `docs/reliability.md`.

4. **Catch-all arms**: Use `_ =>` catch-alls on IR type matches to support
   parallel development.

5. **Deterministic spec**: `volar-spec` must be fully deterministic and
   NOT use the `rand` crate. A `SpecRng` trait is defined in `lib.rs`.

6. **Never specialize on test cases**: Extend tests instead.

7. **CFG vs flat AST**: Cannot convert CFG AST to normal AST — the normal
   AST is total while the CPS AST doesn't need to be.

8. **`IrExpr::RawMap`**: Use for portable `[T; N]::map` expressions (not
   `MethodCall` + `Closure`).

10. **Keep witness analysis and deshadowing for CFG modules**: The CFG
    AST contains copies of spec functions (via `auxiliary_functions`) that
    have the same shadowing and witness patterns as regular `IrModule`
    functions.

11. **No `Vec` in spec-compiled program structure.** A `Vec` whose length
    is known at weave time (const generic, plan dimension, LUT arity,
    layer count, fixed protocol width) is forbidden in `volar-spec` program
    code and in any IR a weaver emits. Use `[T; N]`,
    `core::array::from_fn`, `hybrid_array::Array`, or a presized inline
    capacity. A `Vec` is allowed only at a documented runtime boundary
    (adapter codecs, host interpreters running a runtime-supplied plan,
    evaluation-key backing stores that exceed stack, test fixtures) and
    only with a `/// @volar-allow-vec: <category>: <reason>` doc comment
    (categories: `runtime-boundary`, `eval-key-store`, `host-interpreter`,
    `test-fixture`). The `volar-codegen` IR linter errors on any other
    `Vec` type, `Collect` pipeline, or `Vec` constructor path. Shape is
    weaver-known; data is not — classify by who knows the length. See
    [`docs/fhe/vec-elimination-and-linter-plan.md`](docs/fhe/vec-elimination-and-linter-plan.md).

13. **Weavers emit IR, not text.** A weaver's deliverable for a Rust
    consumer is the complete woven program as typed IR
    (`IrModule`/`IrCfgModule`), and no weaving or post-processing pass may
    operate on printed text either: every weaver is a pure IR→IR transform,
    with text rendering confined to a separate test-only `print_*` wrapper.
    Producing Rust *source text* of a woven program to be compiled by
    `rustc`, or string-manipulating printed program text as a pass input,
    is forbidden in production code: route the IR through
    `lower_module`/`lower_cfg_module` to an `LirTarget` (C99, WASM, or
    object code via `volar-build`) instead. Rust text printing of woven
    modules is allowed only with a
    `/// @volar-allow-rust-text: <category>: <reason>` doc comment
    (categories: `test-fixture`, `diagnostic`, `ts-target`,
    `migration-in-progress`). Backend text for non-`rustc` consumers
    (C99, WASM, TypeScript) is not a violation. The `weave_text_lint`
    source lint errors on violations in the compiler workspace. See
    [`docs/ir-not-text-weaving-plan.md`](docs/ir-not-text-weaving-plan.md).

14. **ZK / non-ZK proving discipline is a hard boundary**: Proof artifacts
    carry a compile-time discipline (`volar_discipline::Tagged<Z, _>`,
    markers `Zk` / `Transparent`, subtrait `NonZk`). A ZK prover
    (`weave_vole_prover*`, `weave_faest_prover*`) is `Zk`; verifiers, garble,
    noop, and fhe modules are `Transparent`. Folding / regular-SNARK entry
    points are bound `where Z: NonZk`, so a `Zk` artifact cannot reach them.
    **Never** mix the two: do not seal a prover as `Transparent` or a verifier
    as `Zk`, do not `into_inner()` to push a `Zk` module into a non-ZK
    consumer, and do not weaken a `NonZk` bound. This boundary is load-bearing
    — treat changes to it as cryptographically sensitive. See
    [`docs/agent-context/discipline.md`](docs/agent-context/discipline.md).

15. **High-level printers vs. `LirTarget` backends — pick by target-language
    shape, not by convenience.** A backend for a *structured, AST-like*
    target language (no arbitrary `goto`/block-jump control flow — e.g.
    Rust, TypeScript, Solidity) should be a high-level `IrModule`-consuming
    printer (`printer.rs`, `printer_ts.rs`, and any future `printer_*.rs`
    follow the same shape), because `IrModule`'s `If`/`BoundedLoop`/`Match`
    nodes already carry the structure such a target needs — reconstructing
    that structure from a lowered CFG would be pure incidental complexity.
    Reserve the `LirTarget`/SSA-CFG path (`volar-lir-codegen` → `CBackend`,
    `VaffleTarget`, WASM/LLVM backends) for targets that are themselves
    SSA/CFG-shaped or that need `volar-lir-codegen`'s monomorphization
    (`MonoEnv`) and struct-flattening machinery — C (via `goto`), WASM,
    object code. Do not route a structured-source target through
    `LirTarget` merely because a lowering pass (e.g. monomorphization)
    happens to live there already; port or share the specific pass instead
    of adopting the whole low-level pipeline. See
    [`docs/ts-emitter-length-params-and-solidity-backend-plan.md`](docs/ts-emitter-length-params-and-solidity-backend-plan.md)
    §4.3 for the reasoning trail this rule was extracted from.

## Topic Context Files

Load these when working in the relevant area:

| Topic | File | When to load |
|---|---|---|
| Reliability policy | `docs/reliability.md` | Always — first thing before editing |
| Operating procedure for AI agents | `docs/agents-guide.md` | Always — first thing before editing |
| **Full pipeline (multi-pass, weaving feedback)** | **`docs/pipeline.md`** | **Touching any lowering pass, codegen backend, or weaver — the overview.md sketch is not accurate** |
| User-facing integration | `docs/integration-guide.md` | When answering questions about app integration |
| Volar IR / VAFFLE / LIR / C-LLVM-WASM `LirTarget` impls (IR types, lowering, movfuscation, DCE/CSE, virtualization, backend codegen) | [`volar-ir` repo docs](https://github.com/portal-co/volar-ir/tree/main/docs) | Working on `volar-ir`, `vaffle`, `volar-lir`, `volar-c-backend`/`volar-llvm-backend`/`volar-wasm-backend`'s actual codegen, or any of the crates split into that repo. The spec-integration tests for those backends (`volar-c-backend-spec-tests`) and `volar-lir-codegen` stay here — see its `AGENTS.md` for that repo's own topic index |
| Weaving & multi-backend | `docs/agent-context/weaving.md` | Working on FHE/garbled-circuit weaving, compiler printers (Rust/TS/C), action system, CFG emission |
| **ZK / non-ZK proving discipline** | `docs/agent-context/discipline.md` | Touching any weaver, `volar-fold`, the build pipeline, or anything that moves a proof `IrModule` — the load-bearing ZK↔non-ZK boundary |
| Prove-the-verifier (IOP-based) | `docs/prove-the-verifier-iop.md` | Folding the verifier natively into `GF(2^k)`, the Merkle+Fiat–Shamir finalization proof, memory-accumulator boundary |
| AST-to-AST weaving (future track) | `docs/agent-context/ast-to-ast-weaving.md` | Considering bypassing LIR lowering entirely for a new target (e.g. ZK-proven FHE) |
| Higher-K gate degree (future track) | `docs/agent-context/higher-k-gates.md` | Touching `BIrStmt::And`'s degree dispatch, `gate_degree`, or K=3+/FAEST AES pinning |
| TypeScript class witnesses | `docs/agent-context/ts-class-witnesses.md` | Working on TS codegen: `WitnessKind`, `ctx` parameter, type-param-as-value, static method dispatch |
| TS emitter hardening, length-parameterized arrays, Solidity backend (plan) | `docs/ts-emitter-length-params-and-solidity-backend-plan.md` | Touching `printer_ts.rs`, `lowering_dyn.rs`, array/length typing, or starting a Solidity backend |
| ORAM & channel | `docs/agent-context/oram.md` | Working on ORAM crates, channel protocol, ORAM weaver integration |
| **Future FHE/provider plumbing ledger** | **`docs/fhe/future-provider-integration-ledger.md`** | **Adding/changing a generic FHE provider adapter, provider LLVM artifact toolchain, heavy-garbling import, FHE boundary, or any skipped provider test/check; record every deferral there and do not use the legacy `FheScheme`/TFHE path as validation** |
| Progress tracking | `PROGRESS.md` | Starting a new session, reviewing status, planning next steps |
| Top-level doc index | `docs/README.md` | Looking for a specific subsystem reference |

---

## Compression-aware logging

Token compression proxies can sit between this tool and an LLM provider, compressing output before it reaches the model. When a proxy is active, MORE verbose structured output is net-cheaper than terse plaintext.

Environment variables (set before running any binary or test in this workspace):

| Variable | Effect |
|---|---|
| `PORTAL_LOG_JSON=1` | Structured NDJSON output; routes `log::` calls through the sink when subscriber is installed. |
| `PORTAL_LOG_BATCH=1` | Group events by phase into single JSON arrays. |
| `PORTAL_AUTOMINIFY=1` | Minify generated code (C, LLVM IR, Rust, IR text) before embedding in error/assertion messages. |

Logger implementation: `crates/helper/volar-log/`. Do NOT add logging infrastructure to cryptographic crates (`volar-spec`, `volar-primitives`, `volar-common`, `volar-oram*`).

These variables have no effect when unset and do not change program correctness.
