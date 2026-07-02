# Volar Agent Context

> **Before editing any file, read [`docs/agents-guide.md`](docs/agents-guide.md)
> and [`docs/reliability.md`](docs/reliability.md).** Volar uses capability
> tiers as review gates in addition to reliability levels. The summary lives in
> this file; the authoritative version is in `docs/reliability.md`.

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

## AI Capability Tiers (review-gate policy)

The reliability level says how trusted the *code* is. The capability tier
says how trusted the *AI agent* must be to self-review a change to it.

| Tier | Claude models | GPT models | What this tier may author without further review |
|---|---|---|---|
| **Tier 1 — Glue** | Any current Claude model | Any current GPT model not listed for Tier 2 or Tier 3, including GPT-5.4-Mini and earlier small/fast variants | Documentation, formatting, mechanical refactors, test scaffolding |
| **Tier 2 — Compiler** | Sonnet 4.6+ or Opus 4.5+ | Full GPT-5.2, GPT-5.3-Codex, full GPT-5.4, GPT-5.5, and later non-mini successors | Compiler, IR, lowering, weavers, backends, fuzzing |
| **Tier 3 — Cryptography** | **Opus 4.6+ only** (Sonnet not permitted at this tier) | **GPT-5.5+ only** | `volar-spec`, `volar-primitives`, `volar-common`, `volar-oram*`, GRAFHEN/TFHE schemes, Hazmat code |

**Crate-default tiers:**

| Path | Default required tier |
|---|---|
| `crates/spec/volar-spec/`, `crates/spec/volar-primitives/`, `crates/spec/volar-common/` | **3** |
| `crates/oram/volar-oram-core/`, `crates/oram/volar-oram/` | **3** |
| `crates/compiler/`, `crates/ir/`, `crates/macros/` | 2 |
| `crates/channel/volar-channel/`, `crates/fuzz/volar-fuzz/`, `crates/spec/volar-spec-dyn/`, `crates/spec/volar-dyn/` | 2 |
| `docs/`, `*.md`, `Cargo.toml`/`Cargo.lock` | 1 (Cargo files: 2) |

A file may explicitly raise its required tier with `// @ai-tier: 3` placed
next to its `@reliability` marker. The marker can only raise the
requirement, never lower it.

**If your tier is below the file's required tier, you are not blocked.**
Perform a reasoning pass first — reason through whether the change is
genuinely cryptographic or merely structural, surface that reasoning in
your reply, then proceed with the change and tag it as sub-threshold:

```rust
// @ai-author-tier: <your tier>
// @ai-review: pending-tier-<required>
```

Queue the work for review by the required tier, typically at the start of
the next session. See [`docs/agents-guide.md` § 5](docs/agents-guide.md#5-working-below-your-tier--the-sub-threshold-workflow).

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

11. **ZK / non-ZK proving discipline is a hard boundary**: Proof artifacts
    carry a compile-time discipline (`volar_discipline::Tagged<Z, _>`,
    markers `Zk` / `Transparent`, subtrait `NonZk`). A ZK prover
    (`weave_vole_prover*`, `weave_faest_prover*`) is `Zk`; verifiers, garble,
    noop, and fhe modules are `Transparent`. Folding / regular-SNARK entry
    points are bound `where Z: NonZk`, so a `Zk` artifact cannot reach them.
    **Never** mix the two: do not seal a prover as `Transparent` or a verifier
    as `Zk`, do not `into_inner()` to push a `Zk` module into a non-ZK
    consumer, and do not weaken a `NonZk` bound. This boundary is load-bearing
    — treat changes to it as Tier 3 in spirit. See
    [`docs/agent-context/discipline.md`](docs/agent-context/discipline.md).

## Topic Context Files

Load these when working in the relevant area:

| Topic | File | When to load |
|---|---|---|
| Reliability + AI tiers | `docs/reliability.md` | Always — first thing before editing |
| Operating procedure for AI agents | `docs/agents-guide.md` | Always — first thing before editing |
| **Full pipeline (multi-pass, weaving feedback)** | **`docs/pipeline.md`** | **Touching any lowering pass, codegen backend, or weaver — the overview.md sketch is not accurate** |
| User-facing integration | `docs/integration-guide.md` | When answering questions about app integration |
| IR types, storage, Poly semantics | `docs/agent-context/ir-types-storage.md` | Working on IR, lowering, evaluators, store-forward, fuzzer generators |
| IR `map`/`as_ref`/`as_mut` conventions | `docs/agent-context/ir-map-conventions.md` | Adding IR variants, writing IR transformations, understanding `#[non_exhaustive]` catch-all conventions |
| Provenance pipeline | `docs/agent-context/provenance-pipeline.md` | Adding provenance to passes, writing `ProvenanceHandler` impls, understanding why `Default`/`synthetic()` are absent |
| Weaving & multi-backend | `docs/agent-context/weaving.md` | Working on FHE/garbled-circuit weaving, compiler printers (Rust/TS/C), action system, CFG emission |
| **ZK / non-ZK proving discipline** | `docs/agent-context/discipline.md` | Touching any weaver, `volar-fold`, the build pipeline, or anything that moves a proof `IrModule` — the load-bearing ZK↔non-ZK boundary |
| Prove-the-verifier folding | `docs/prove-the-verifier.md` | Folding the verifier into a relaxed-R1CS instance, `volar-fold` reuse, memory-commitment boundaries |
| **GF(2^k) → F_ℓ embedding (open)** | `docs/agent-context/gf2k-to-fell-embedding.md` | Touching `FoldLift`, `NovaFoldSink`, or anything folding the VOLE verifier's field values into `F_ℓ` — unresolved, needs cryptographic review |
| `u128` support in LIR/C backend (deferred) | `docs/agent-context/lir-u128-support.md` | Touching `primitive_to_lir`, the C backend, or spec-linking `u128`-using code (`Scalar`, curve arithmetic) |
| AST-to-AST weaving (future track) | `docs/agent-context/ast-to-ast-weaving.md` | Considering bypassing LIR lowering entirely for a new target (e.g. ZK-proven FHE) |
| Higher-K gate degree (future track) | `docs/agent-context/higher-k-gates.md` | Touching `BIrStmt::And`'s degree dispatch, `gate_degree`, or K=3+/FAEST AES pinning |
| TypeScript class witnesses | `docs/agent-context/ts-class-witnesses.md` | Working on TS codegen: `WitnessKind`, `ctx` parameter, type-param-as-value, static method dispatch |
| ORAM & channel | `docs/agent-context/oram.md` | Working on ORAM crates, channel protocol, ORAM weaver integration |
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

Logger implementation: `crates/helper/volar-log/` (Tier 1 — any agent may modify). Do NOT add logging infrastructure to Tier 3 crates (`volar-spec`, `volar-primitives`, `volar-common`, `volar-oram*`).

These variables have no effect when unset and do not change program correctness.
