# Complexity hints (reentry metadata)

> Compiler / IR context. See [virt-adaptive-split-adr.md](virt-adaptive-split-adr.md).

## Purpose

Branch targets may carry **reentry hints**: metadata describing values that must
strictly decrease when control re-enters a block via that edge. This supports
well-founded loop reasoning and adaptive virtualization (CFG reroll planning).

Hints are **non-executable** — evaluators ignore them.

## Types (`volar-ir-common::complexity`)

| Type | Role |
|------|------|
| `ReentryHint` | One or more top-level `MeasureSpec` values (lex product) |
| `MeasureSpec` | Recursive measure: `Strict`, `BitToZero`, `Diff`, `Digits`, `Structural` |
| `StructRef` | Portable compiler struct name for `Structural` hints |

### `Digits` lex order

Compare nested `elements` left-to-right. Valid re-entry iff some index `i`
strictly decreases and all prior elements are equal.

### `Structural`

Compiler struct-typed block param: at least one listed field decreases; all
others equal. Unlisted fields are equal-only.

## Producers

| Source | Hint policy |
|--------|-------------|
| Tree `IrExpr::BoundedLoop` | **Autoderived** in `lower_bounded_loop` only (`ReentryHint::bounded_loop_ascending` on body→header back-edge) |
| CFG `IrCfgJump.reentry` | **Explicit** — weavers / hand annotation |
| All other edges | `None` |

Canonical autoderived ascending loop measure:

```text
Diff(Strict param 1, Strict param 0)   // limit - counter
```

## Propagation

```text
IrCfgJump / LIR BranchTarget → VAFFLE Target.reentry → IRBranchTarget.reentry
```

Critical lowers copy hints unchanged; analysis passes ignore unknown variants.

## Consumers (v1)

- `volar-ir-virt::cfg_hints` — detect back-edges with hints, infer `TripCount`
  for bounded ascending loops, plan `RerollLoop` on the body block before
  structural `find_reroll_body`.
- `AdaptiveSplitConfig::prefer_reentry_hints` (default `true` when adaptive
  split is enabled).

## Deferred

- `JumpTable` per-case hints, `Dyn` targets
- BIR adaptive virt consumption
- Weaver `Structural` annotations
- Proof-carrying validation pass
