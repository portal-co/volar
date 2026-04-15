# Volar Documentation

**Program-related cryptography, implemented and refined in the open.**

Volar is a cryptographic library and compiler toolchain whose goal is to increase adoption of zero-knowledge proofs, garbled circuits, multi-party computation, and related techniques by implementing them in auditable Rust and compiling them to other targets. It provides:

- A specification layer (`volar-spec`) with type-checked cryptographic protocol
  definitions written in a total subset of Rust. Current protocols: VOLE-based ZK
  (Quicksilver-style) and garbled circuits (half-gate scheme).
- A compiler (`volar-compiler`) that parses these specifications and lowers them
  through a purpose-built IR to dynamic Rust and TypeScript, enabling the same
  cryptographic kernel to run in browsers and servers without re-implementation drift.
- Field-element primitives (`volar-primitives`) for GF(2), GF(2⁸), GF(2⁶⁴),
  GF(2¹²⁸), and GF(2²⁵⁶), with Itoh–Tsujii inversion.
- A proof-system IR (`volar-ir`) for representing circuits before and after
  movfuscation — designed to be protocol-agnostic.
- A weaver (`volar-weaver`) that generates garbled-circuit and VOLE ZK proof
  code from boolean circuits.
- A public reliability system for tracking which constructions are established,
  which are experimental, and which are broken — so trust can be assigned accurately.

---

## Code Reliability at a Glance

Every source file carries a `// @reliability:` marker and an `//! @ai:` marker.
See [reliability.md](reliability.md) for the full policy.

| Level | Extension | Compiled | Meaning |
|---|---|---|---|
| **Normal** | `.rs` | ✅ Always | Established constructions, proven security |
| **Hazmat** | `.rs` | ✅ Always | Proven but requires expert use; misuse breaks security |
| **Experimental** | `.rs` | ✅ `volar_experimental` feature | Novel; designed for review, not yet trusted |
| **Insecure** | `.rs.insecure` | ❌ Never | Known/suspected broken; research record only |

---

## Document Index

| Document | Description |
|---|---|
| [reliability.md](reliability.md) | **Code reliability levels** — definitions, markers, AI labels, promotion/demotion protocol |
| [overview.md](overview.md) | Architecture overview — workspace layout, crate dependency graph, compilation pipeline |
| [spec.md](spec.md) | `volar-spec` — ZK, MPC, and garbled circuit specifications |
| [ir-lowering.md](ir-lowering.md) | IR lowering pipeline — `volar-ir`, Volar IR, Boolar IR, movfuscation |
| [compiler.md](compiler.md) | `volar-compiler` — parser, IR, lowering passes, printers |
| [garbling-pipeline.md](garbling-pipeline.md) | Garbling & VOLE pipeline — `volar-weaver`, evaluator/garbler/prover/verifier generation |
| [vole-weaving.md](vole-weaving.md) | VOLE weaving design — Quicksilver AND check, prover/verifier code generation |
| [lir.md](lir.md) | LIR — `LirTarget` trait, C backend, IrModule lowering, monomorphization, loop unrolling |
| [primitives-compiler-plan.md](primitives-compiler-plan.md) | Primitives compiler integration plan (Phase 1/3/4 complete, Phase 2 deferred) |
| [insecure.md](insecure.md) | The `.insecure` file extension — policy and current insecure constructions |

### Compiler sub-documents (migrated from `crates/volar-compiler/docs/`)

| Document | Description |
|---|---|
| [compiler/IR_DOCUMENTATION.md](compiler/IR_DOCUMENTATION.md) | Detailed IR node reference |
| [compiler/ITER_CHAIN_PLAN.md](compiler/ITER_CHAIN_PLAN.md) | Iterator chain refactor (completed) |
| [compiler/TRAIT_IMPL_PLAN.md](compiler/TRAIT_IMPL_PLAN.md) | Custom trait & impl support plan (completed) |
| [compiler/TRAIT_UNIFY_AND_RAW_OPS_PLAN.md](compiler/TRAIT_UNIFY_AND_RAW_OPS_PLAN.md) | Trait unification & RawMap/RawZip/RawFold (completed) |
| [compiler/TYPE_MANIFEST_PLAN.md](compiler/TYPE_MANIFEST_PLAN.md) | Type manifest files (`.volar.d`) plan (completed) |
| [compiler/TYPESCRIPT_BACKEND_PLAN.md](compiler/TYPESCRIPT_BACKEND_PLAN.md) | TypeScript backend design |
| [compiler/TS_BACKEND_PROGRESS.md](compiler/TS_BACKEND_PROGRESS.md) | TypeScript backend progress tracker |
