# Volar Documentation

**Program-related cryptography, implemented and refined in the open.**

Volar is a Rust workspace and compiler toolchain for zero-knowledge proofs,
garbled circuits, multi-party computation, oblivious RAM, and the field
arithmetic that underpins them. The same cryptographic kernel compiles to
dynamic Rust, TypeScript, and C, so a single specification runs in browsers,
servers, and embedded systems without re-implementation drift.

This directory contains the living documentation. Historical planning
documents live in [`archive/`](archive/README.md).

---

## Where to Start

| If you are… | Read this |
|---|---|
| Integrating Volar into an application | [**integration-guide.md**](integration-guide.md) |
| An AI agent about to modify any file | [**agents-guide.md**](agents-guide.md) → [reliability.md](reliability.md) |
| New to the project | [overview.md](overview.md) → [spec.md](spec.md) |
| Trying to understand a single subsystem | The [Topic Index](#topic-index) below |
| Trying to assess whether some code is safe to deploy | [reliability.md](reliability.md) |

---

## Reliability and AI Capability At a Glance

Every source file carries a `// @reliability:` marker and a `//! @ai:`
marker. AI agents must additionally satisfy a **capability tier** that
matches or exceeds the file's required tier. Both systems are defined in
[reliability.md](reliability.md).

### Reliability levels

| Level | Extension | Compiled | Meaning |
|---|---|---|---|
| **Normal** | `.rs` | Always | Established constructions, proven security |
| **Hazmat** | `.rs` | Always | Proven but requires expert use; misuse breaks security |
| **Experimental** | `.rs` | `volar_experimental` feature | Novel; designed for review, not yet trusted |
| **Insecure** | `.rs.insecure` | Never | Known/suspected broken; research record only |

### AI capability tiers

| Tier | Claude models permitted | What this tier may modify |
|---|---|---|
| **Tier 1 — Glue** | Any current Claude model | Documentation, formatting, mechanical refactors |
| **Tier 2 — Compiler** | Sonnet 4.6+ or Opus 4.5+ | Compiler, IR, lowering, weavers, backends, fuzzing |
| **Tier 3 — Cryptography** | Opus 4.6+ only | Cryptographic spec, primitives, ORAM, Hazmat code |

Lower-tier agents can still contribute productively to higher-tier work by
producing test scaffolding, hand-off documents, and analysis. See
[agents-guide.md](agents-guide.md) for the full operating procedure.

---

## Topic Index

### Foundations

| Document | What it covers |
|---|---|
| [reliability.md](reliability.md) | Reliability levels, AI markers, AI capability tiers, file→tier mapping, promotion/demotion protocol |
| [overview.md](overview.md) | Workspace layout, crate dependency graph, compilation pipeline |
| [insecure.md](insecure.md) | The `.insecure` extension and current insecure files |
| [provenance.md](provenance.md) | Per-statement origin tracking through the IR pipeline |

### Specifications and protocols

| Document | What it covers |
|---|---|
| [spec.md](spec.md) | `volar-spec`: VOLE ZK, garbled circuits, MPC types, byte generation, `volar-common`, `volar-primitives` |
| [vole-weaving.md](vole-weaving.md) | VOLE prover/verifier code generation from boolean circuits (Quicksilver-style) |
| [garbling-pipeline.md](garbling-pipeline.md) | Garbler/evaluator code generation for half-gate garbled circuits |
| [memory-checking.md](memory-checking.md) | Multiset memory checking for VOLE-authenticated storage |

### Compiler and IR

| Document | What it covers |
|---|---|
| [compiler.md](compiler.md) | `volar-compiler`: parser, IR types, manifests, dynamic lowering, Rust/TS printers |
| [ir-lowering.md](ir-lowering.md) | The `volar-ir` low-level circuit IR, movfuscation, Volar IR / Boolar IR |
| [lir.md](lir.md) | LIR target trait, IrModule → LirTarget lowering, monomorphisation |
| [lir-abi.md](lir-abi.md) | LIR ABI policy and per-target conventions |
| [text-format-spec.md](text-format-spec.md) | Stable text formats for serialised IR artefacts |
| [waffle-lowering.md](waffle-lowering.md) | WAFFLE → VAFFLE lowering for WASM-compiled circuits |
| [wasm-feature-support.md](wasm-feature-support.md) | Which WASM features are supported at each pipeline layer |

### FHE / homomorphic evaluation

| Document | What it covers |
|---|---|
| [fhe-weaver.md](fhe-weaver.md) | The generic FHE scheme abstraction and weaver |
| [grafhen.md](grafhen.md) | GRAFHEN construction, ZK architecture, and the IND-CPA break |
| [grafhen-appsec.md](grafhen-appsec.md) | Operational security rules for any GRAFHEN deployment |
| [grafhen-review-plan.md](grafhen-review-plan.md) | Review plan tracking GRAFHEN toward higher reliability |

### ORAM and channel

| Document | What it covers |
|---|---|
| [agent-context/oram.md](agent-context/oram.md) | ORAM and channel design notes for agents working in this area |
| [oram-fuzzing.md](oram-fuzzing.md) | Property tests on the ORAM data structure |

### Fuzzing

| Document | What it covers |
|---|---|
| [fuzzing.md](fuzzing.md) | `volar-fuzz` infrastructure: generators, interpreters, properties, libFuzzer targets |

### Active design plans

| Document | Status |
|---|---|
| [external-primitives-plan.md](external-primitives-plan.md) | Oracles, Actions, and Native RNG — partial implementation; ActionCall is in use |
| [grafhen-review-plan.md](grafhen-review-plan.md) | Active review tracker |

### Agent-context briefings

These short topic briefings are loaded into AI agent contexts when
working on the relevant area. They concentrate the design rules and
trip-wires that show up most often in code review.

| Document | When to load |
|---|---|
| [agent-context/ir-types-storage.md](agent-context/ir-types-storage.md) | IR, lowering, evaluators, store-forward, fuzzer generators |
| [agent-context/weaving.md](agent-context/weaving.md) | FHE/garbled-circuit weaving, compiler printers, action system, CFG emission |
| [agent-context/oram.md](agent-context/oram.md) | ORAM crates, channel protocol, ORAM weaver integration |

### Operating procedures

| Document | Audience |
|---|---|
| [agents-guide.md](agents-guide.md) | AI agents modifying Volar source. Pre-flight checklist, safe refactor patterns, what to do when blocked. |
| [integration-guide.md](integration-guide.md) | Integrators using Volar in applications. Targets, worked examples, reliability hygiene. |

### Archive

[`archive/README.md`](archive/README.md) — completed plans retained for
historical context.

---

## How These Documents Relate

```
                ┌─────────────────────────┐
                │      reliability.md     │  ← single source of truth
                │  (levels + AI tiers)    │     for what is safe to do
                └────────────┬────────────┘
                             │
            ┌────────────────┼────────────────┐
            ▼                ▼                ▼
   agents-guide.md   integration-guide.md   insecure.md
   (for AI agents)   (for app builders)     (current insecure files)

                ┌─────────────────────────┐
                │       overview.md       │  ← architectural starting point
                └────────────┬────────────┘
                             │
   ┌──────────┬──────────────┼──────────────┬──────────────┐
   ▼          ▼              ▼              ▼              ▼
spec.md   compiler.md   vole-weaving.md   lir.md   ir-lowering.md
              │
              ▼
   text-format-spec.md, lir-abi.md, waffle-lowering.md, …

                ┌─────────────────────────┐
                │  agent-context/ briefs  │  ← topic-focused, dense
                └─────────────────────────┘
```

If you are an AI agent and you are unsure where to read first, the
correct order is:

1. [`AGENTS.md`](../AGENTS.md) — always-applied workspace rules.
2. [agents-guide.md](agents-guide.md) — what your tier may do.
3. [reliability.md](reliability.md) — the file/tier mapping.
4. The topic-specific document for the area you are touching.

If you are a human integrator: start with
[integration-guide.md](integration-guide.md).
