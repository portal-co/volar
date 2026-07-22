# Volar Agents Guide

> Operating procedure for AI agents working in this repository. Pair this guide
> with [reliability.md](reliability.md), the authoritative reliability policy.

All agents may contribute. Do not infer correctness from a model identity,
capability label, or an AI marker. Claims about compiler behavior require
reproducible compile-and-run evidence; cryptographic claims require the
appropriate paper binding, review plan, independent evidence where applicable,
and the human decisions named by the relevant policy.

## Before editing

1. Read [`AGENTS.md`](../AGENTS.md), this guide, and
   [reliability.md](reliability.md).
2. Load every topic context named in `AGENTS.md` for the subsystem being
   changed. Read [pipeline.md](pipeline.md) before changing lowering, a backend,
   or a weaver.
3. Preserve the typed-IR, provenance, deterministic-spec, generated-code-test,
   and ZK/non-ZK-discipline rules. When a plan and current source disagree,
   update the plan or handoff before coding.
4. Use the smallest independent reproducer, retain failing coverage, and
   distinguish source failures from environment blockers.

## Evidence and review

- Pinnedness and stability markers describe evidence and dependent-facing
  change expectations; AI markers describe authorship/review history. None is
  a model-gating mechanism.
- New cryptographic work starts Unpinned and Very unstable and needs a
  paper-bound review plan. Non-default pinnedness, Forever stability,
  parameter/security claims, and policy changes remain human decisions under
  [reliability.md](reliability.md).
- Hazmat work retains its safety documentation and call-site justification.
- Generated artifacts are changed only through their generator pipeline.
- A handoff must record the reproducer, evidence, invariants, and next smallest
  safe action without relying on a private transcript or a particular model.

## Related documents

- [reliability.md](reliability.md) — pinnedness, stability, AI markers, and
  reclassification protocol.
- [handoffs/merge-recovery/index.md](handoffs/merge-recovery/index.md) —
  current merged-tree recovery evidence and open blockers.
- [insecure.md](insecure.md) — policy for the `.insecure` file extension.
- [archive/grafhen-review-plan.md](archive/grafhen-review-plan.md) — archived
  example of a review-plan structure for a new construction.

## Merged-tree update — 2026-07-22

Evidence: policy stream `6596629` → `d783cb1` and the
[policy-and-reliability handoff](handoffs/merge-recovery/policy-and-reliability.md).
Capability tiers, model identification, and sub-threshold tags were removed.
The evidence requirements above, pinnedness/stability distinctions, and human
decision points remain in force.
