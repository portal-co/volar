# Merge-recovery handoffs

**Merged-tree base:** `08d1d33` (Phase-1 recovery), following merge tip `70f2ba3`.

Start with [sources.md](sources.md). It inventories historical evidence without
committing any transcript. Historical conversations are evidence only: current
source, this index, and current owner decisions override them.

## Workstreams

1. [Policy and reliability](policy-and-reliability.md)
2. [TFHE GINX validation](tfhe-ginx-validation.md)
3. [Static shapes and monomorphization](static-shapes-and-monomorphization.md)
4. [Metadata and instruction groups](metadata-and-instruction-groups.md)
5. [Real interpreter and virtualisation](real-interpreter-and-virtualisation.md)
6. [AST-to-AST and more proofs](ast-to-ast-and-more-proofs.md)

## Phase 3 documentation reconciliation

The active policy and plan documents now reflect the model-neutral,
evidence-based policy and current merged-tree status. See the dated
merged-tree updates in `AGENTS.md`, `docs/agents-guide.md`,
`docs/reliability.md`, `docs/README.md`, and the plans named in
[`merge-recovery-and-handoffs-plan.md`](../../merge-recovery-and-handoffs-plan.md).
This resolves documentation drift only; the source and environment blockers
below remain open.

## Current recovery evidence

Phase 1 repaired the TFHE test API drift, statement-free provenance entry
points, empty-program fuzz coverage, and a `vole::memory` scalar-loop bug.
The following focused commands passed on 2026-07-22:

```sh
cargo test -p volar-spec
cargo test -p volar-vaffle-target
cargo test -p volar-ir-passes
cargo test -p volar-ir-opt -p volar-ir-virt -p volar-fuzz
```

The next widening ring is blocked by an existing LIR monomorphization failure:
`encrypt_branch` reaches C-backend test lowering with unresolved const parameter
`L`. This is a source/reconciliation issue, not the missing-LLVM environment
blocker. See the static-shapes handoff before changing a backend test or
assuming the historical monomorphization transcript fully bounds the impact.

`cargo fmt --check` also remains blocked by pre-existing unparsable generated
associated types in `crates/spec/volar-spec-dyn/src/generated.rs`; regenerate
through the generator pipeline rather than hand-editing it.
