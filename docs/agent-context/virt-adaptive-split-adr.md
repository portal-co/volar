# ADR: Adaptive split & specialized regions (`volar-ir-virt`)

> ADR for [`virtualize_ir` adaptive split](../virt.md). Compiler context; see
> [reliability.md](../reliability.md) for the pinnedness/stability policy.

## Status

Accepted — v1 implements the narrow core; items below are explicitly deferred.

## Context

Whole-block virtualization deduplicates handlers but leaves two gaps:

1. **Cross-block** shared stmt cores with different prologues/epilogues.
2. **Intra-block** repeated bodies (unrolled loops) that pay outer-dispatcher
   cost per step.

v1 adds two mechanisms with different runtime models:

| Mechanism | Model |
|-----------|--------|
| **SharedCore** | Stepping sub-interpreter over appended opcode rows + `SUB_RETURN` |
| **RerollLoop** | Descriptor row + trip count; read operands N times; implicit return |

Both append rows to the **same** bytecode storage after outer program rows.
Appended rows are seeded via [`build_ir_storage_init_adaptive`](../../crates/ir/volar-ir-virt/src/preinit.rs)
into `pre_init` (same dense lane model as outer rows).

## v1 scope (shipped)

- `AdaptiveSplitConfig` on [`VirtualizeConfig`](../../crates/ir/volar-ir-virt/src/lib.rs),
  default **off**.
- Unified flat bytecode: outer rows `0..N-1`, appended regions after.
- Cross-block SharedCore planner + sub-interpreter infra.
- Intra-block RerollLoop planner + reroll driver blocks.
- **CFG reentry hints** — `cfg_hints` planner uses `IRBranchTarget.reentry` on
  back-edges (including autoderived `BoundedLoop` hints) before structural
  reroll search. See [complexity-hints.md](complexity-hints.md).
- Composite outer handlers (opcodes-before / entry / resume / opcodes-after).
- Public dispatch, IR-only, no commitment extension.
- Equivalence tests with split enabled.

## Deferring strategy

1. Ship the narrow correct core first; default-off toggle preserves today’s behavior.
2. Deferred items are **ADR-gated** — no ad-hoc integration without updating this doc.
3. Each deferred item lists unblock criteria and required equiv tests.
4. Extensibility hooks (e.g. `operand_mode` on reroll metadata) may land in v1 without implementing the optimization.

## Deferred items

| Item | Why deferred | Unblock criteria |
|------|--------------|------------------|
| `direct_dispatch` for sub-tier | Outer DD sufficient initially | Sub-interpreter equiv + block budget |
| `virtualize_ir_committed` + split | Hash must cover all global pcs + reroll descriptors | Committed equiv suite with split on |
| `DispatchMode::Oblivious` + split | `movfuscate_ir` lacks `JumpTable` | Movfuscate JumpTable support |
| BIR adaptive split | No register file / JumpTable on BIR path | BIR register-file mirroring |
| Backend `VirtBytecode` printers | `pre_init` is canonical; printers may consume either form | Separate printer ADR |
| **MUX-tree abstraction for reroll** | See motivation below | RAM↔SSA analysis + MUX cost model |

## Deferred motivation: MUX-tree + RAM for reroll

v1 reroll keeps loop-carried state in the SSA-style register file
([`RegAlloc`](../../crates/ir/volar-ir-virt/src/ir.rs)): each trip re-reads
operand slots. Correct and simple.

Future optimization (not v1):

- Represent reroll operand updates as a **MUX tree** over a small **RAM**
  bank instead of threading every carried value through registers.
- Loop body becomes read/compute/write on RAM indices; trip count unchanged.
- Moves state **from SSA back to RAM** for hot loops, reducing register-file
  pressure and backend spilling.

Requires: cross-trip liveness, RAM index assignment, MUX-tree construction.
Reroll descriptor metadata reserves `operand_mode: RegisterFile | RamMux`
(default `RegisterFile`) for this path.

## References

- [virt.md](../virt.md) — user-facing overview
- [PROGRESS.md](../../PROGRESS.md) — project status
