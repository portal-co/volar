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
| Touching lowering, a backend, or a weaver | [**pipeline.md**](pipeline.md) — the full multi-pass picture |
| New to the project | [overview.md](overview.md) → [spec.md](spec.md) → [pipeline.md](pipeline.md) |
| Trying to understand a single subsystem | The [Topic Index](#topic-index) below |
| Trying to assess whether some code is safe to deploy | [reliability.md](reliability.md) |

---

## Pinnedness, Stability, and Evidence at a Glance

Pinnedness records the evidence supporting a claim; stability records the
commitment and expected change rate for dependents. They are independent, except
that **Forever** stability requires **Proven** pinnedness. All agents may
contribute; neither these markers nor a model identity establishes correctness.
[reliability.md](reliability.md) defines the required evidence and human
decisions.

| Axis | Values | Meaning |
|---|---|---|
| **Pinnedness** | Unpinned → Paper-pinned → Reviewed → Proven | Evidence from no complete external binding through formal proof connected to the implementation |
| **Stability** | Forever → Stable → Semver → Unstable → Very unstable | Dependent-facing longevity and change expectation; not a security claim |

New cryptographic work begins Unpinned and Very unstable. Paper bindings,
independent review, proof, and every non-default stability commitment require
documented evidence. Legacy `@reliability:` markers are migration-only; Hazmat
is a separate use-safety classification and `.insecure` remains a non-compiled
quarantine. Compiler and backend changes need real compile-and-run evidence
where applicable.


## Topic Index

### Foundations

| Document | What it covers |
|---|---|
| [reliability.md](reliability.md) | Pinnedness, stability, legacy-marker migration, AI markers, and reclassification protocol |
| [overview.md](overview.md) | Workspace layout, crate dependency graph, compilation pipeline |
| [insecure.md](insecure.md) | The `.insecure` extension and current insecure files |
| `volar-ir` repo's `docs/provenance.md` | Per-statement origin tracking through the IR pipeline |
| `volar-ir` repo's `docs/side.md` | Per-value actor/role tracking (ZK witness/statement, FHE plaintext/ciphertext) — provenance's sibling |

### Specifications and protocols

| Document | What it covers |
|---|---|
| [spec.md](spec.md) | `volar-spec`: VOLE ZK, garbled circuits, MPC types, byte generation, `volar-common`, `volar-primitives` |
| [vole-weaving.md](vole-weaving.md) | VOLE prover/verifier code generation from boolean circuits (Quicksilver-style) |
| [prove-the-verifier-iop.md](prove-the-verifier-iop.md) | Prove-the-verifier: native `GF(2^k)` fold + a Merkle+Fiat–Shamir finalization proof (`volar-iop`), including the memory-accumulator boundary, and the ZK↔non-ZK discipline that gates it |
| [garbling-pipeline.md](garbling-pipeline.md) | Garbler/evaluator code generation for half-gate garbled circuits |
| [memory-checking.md](memory-checking.md) | Multiset memory checking for VOLE-authenticated storage |

### Compiler and IR

| Document | What it covers |
|---|---|
| [pipeline.md](pipeline.md) | **Full multi-pass pipeline**: parse → weave → lower → codegen, recursive flows, why fixes belong in lowering not backends |
| [ir-not-text-weaving-plan.md](ir-not-text-weaving-plan.md) | Weavers emit IR not text: the rule, the `weave_text_lint` source lint, and the migration off print→`rustc` |
| [compiler.md](compiler.md) | `volar-compiler`: parser, IR types, manifests, dynamic lowering, Rust/TS printers |
| [`volar-ir` repo docs](https://github.com/portal-co/volar-ir/tree/main/docs) | Volar IR, VAFFLE, movfuscation, circuit lowering, virtualization, DCE/CSE/const-fold, LIR, WAFFLE→VAFFLE lowering, WASM feature support, IR text formats — split out of this repo, see `volar-ir`'s `docs/pipeline.md` |
| [metadata-container-plan.md](metadata-container-plan.md) | Implemented metadata container and propagation ledger; remaining consumer/coverage work |
| [instruction-groups-plan.md](instruction-groups-plan.md) | Implemented instruction-group infrastructure ledger; remaining consumers and generated-backend coverage |

### FHE / homomorphic evaluation

| Document | What it covers |
|---|---|
| [fhe/README.md](fhe/README.md) | FHE / homomorphic evaluation index, legacy TFHE evidence, and design records |
| [fhe/weaver.md](fhe/weaver.md) | The generic FHE scheme abstraction and weaver |

### ORAM and channel

| Document | What it covers |
|---|---|
| [agent-context/oram.md](agent-context/oram.md) | ORAM and channel design notes for agents working in this area |
| [oram-fuzzing.md](oram-fuzzing.md) | Property tests on the ORAM data structure |

### Fuzzing

| Document | What it covers |
|---|---|
| `volar-ir` repo's `docs/fuzzing.md` | `volar-fuzz` infrastructure: generators, interpreters, properties, libFuzzer targets |
| `fuzz/fuzz_targets/fuzz_vole_circuit_completeness.rs` | The one fuzz target that stayed here (also exercises `volar-spec`) |

### Active design plans

| Document | Status |
|---|---|
| [external-primitives-plan.md](external-primitives-plan.md) | Oracles, Actions, and Native RNG — partial implementation; ActionCall is in use |
| [fhe/README.md](fhe/README.md) | FHE / TFHE plans, evidence records, reviews, and generic-weaver material |
| [fhe/tfhe-steady-state-evidence.md](fhe/tfhe-steady-state-evidence.md) | Authoritative Track-S speculative legacy corpus and evidence ledger |
| [spec-static-shapes-plan.md](spec-static-shapes-plan.md) | Static TFHE shapes landed; dynamic and LIR/C validation gaps are recorded |
| [metadata-container-plan.md](metadata-container-plan.md) | Implemented metadata refactor ledger and incomplete consumer/coverage work |
| [instruction-groups-plan.md](instruction-groups-plan.md) | Implemented group infrastructure ledger and incomplete consumer/coverage work |
| `volar-ir` repo's `docs/lir-lowering-monomorphization-plan.md` | Proposed instance-aware lowering; unresolved const `L` blocks widening-ring C evidence |
| [handoffs/merge-recovery/index.md](handoffs/merge-recovery/index.md) | Merged-tree recovery evidence, blockers, and model-neutral next actions |

### Agent-context briefings

These short topic briefings are loaded into AI agent contexts when
working on the relevant area. They concentrate the design rules and
trip-wires that show up most often in code review.

| Document | When to load |
|---|---|
| `volar-ir` repo's `docs/agent-context/ir-types-storage.md` | IR, lowering, evaluators, store-forward, fuzzer generators |
| [agent-context/weaving.md](agent-context/weaving.md) | FHE/garbled-circuit weaving, compiler printers, action system, CFG emission |
| [agent-context/discipline.md](agent-context/discipline.md) | The ZK↔non-ZK proving discipline boundary: weavers, `volar-fold`, build pipeline — binds agents against mixing prover/verifier/fold primitives |
| `volar-ir` repo's `docs/agent-context/side.md` | Per-value side tracking: adding `SideId` to an IR container, `SideHandler` impls, replacing `ZkWitnessConfig`/`*ActionConfig`/`PublicSet`-shaped configs |
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
                │ (pinnedness + stability)│     for what is safe to claim
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
   ┌──────────┬──────────────┼──────────────┐
   ▼          ▼              ▼              ▼
spec.md   compiler.md   vole-weaving.md   volar-ir repo docs (lir.md, ir-lowering.md, …)

                ┌─────────────────────────┐
                │  agent-context/ briefs  │  ← topic-focused, dense
                └─────────────────────────┘
```

If you are an AI agent and you are unsure where to read first, the
correct order is:

1. [`AGENTS.md`](../AGENTS.md) — always-applied workspace rules.
2. [agents-guide.md](agents-guide.md) — evidence-based operating procedure.
3. [reliability.md](reliability.md) — reliability and review evidence.
4. [handoffs/merge-recovery/index.md](handoffs/merge-recovery/index.md) — current merged-tree blockers.
5. The topic-specific document for the area you are touching.

If you are a human integrator: start with
[integration-guide.md](integration-guide.md).


## Merged-tree update — 2026-07-22

Evidence: [policy-and-reliability handoff](handoffs/merge-recovery/policy-and-reliability.md)
and [merge-recovery index](handoffs/merge-recovery/index.md). Capability tiers,
model gating, and Experimental Cargo-feature requirements were removed. This
index now routes contributors by evidence and current handoffs instead.
