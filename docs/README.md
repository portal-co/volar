# Volar Documentation

**VOLE-in-the-head computations for a cloud world.**

Volar is a cryptographic library and compiler toolchain built around
Vector Oblivious Linear Evaluation (VOLE). It provides:

- A specification layer (`volar-spec`) with type-checked cryptographic protocol
  definitions written in a total subset of Rust.
- A compiler (`volar-compiler`) that parses these specifications and lowers them
  through a purpose-built IR to dynamic Rust and TypeScript.
- Field-element primitives (`volar-primitives`) for GF(2), GF(2⁸), and GF(2⁶⁴).
- A proof-system IR (`volar-ir`) for representing circuits before and after
  movfuscation.

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
