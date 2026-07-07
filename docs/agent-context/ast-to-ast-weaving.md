# Agent Context: AST-to-AST weaving (future track, not started)

**Load this when:** considering an alternative to the current weave → IR → LIR →
backend pipeline for a *new* target where LIR lowering is the bottleneck.

## The idea

The current weaving pipeline lowers a source AST to `IrModule`, then to LIR, then
to a target backend (C, Rust-source, TypeScript). For targets where LIR lowering
has real performance costs, or where the AST→LIR pass doesn't yet cover enough of
the language to be practical, a **direct AST-to-AST weave** — transforming the
source-level AST into another AST without going through LIR at all — could be a
better-fit alternative for that specific track.

The motivating example raised during scoping: **ZK-proven FHE**, where LIR
lowering costs are real and the AST-to-LIR lowering pass would need to be far more
complete than it is today to support the full FHE surface.

## Status

Not started. Raised while scoping the prove-the-verifier folding work
(`docs/prove-the-verifier-iop.md`) as a related-but-separate idea — that
work explicitly does **not** need this (it goes through `print_module`, which
already exists and is the established "real backend" test pattern per
`AGENTS.md` rule 2, not a new AST-to-AST mechanism).

## What starting this would involve

- Scoping which AST-level transformations actually need to bypass LIR (vs. which
  are fine going through the existing pipeline).
- Deciding whether this is a new backend trait alongside the existing
  `LirTarget`-style abstraction, or a genuinely separate pass structure.
- A concrete first target (ZK-proven FHE is the one motivating example so far) to
  validate the approach against, rather than building it generically up front.

No code exists for this yet; this document exists so the idea isn't lost, not to
prescribe a design.
