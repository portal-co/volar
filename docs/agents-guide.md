# Volar Agents Guide

> Operating procedure for AI agents working in this repository. Pairs with
> [reliability.md](reliability.md), which defines the reliability levels and
> the AI capability tier system this guide assumes.

This guide tells an AI agent **what it is allowed to do** and **how to do it
safely**, especially when the agent's capability tier is lower than the tier
required by the change. The goal is for refactors and infrastructure work to
proceed in parallel with cryptographic work without lower-power agents
silently corrupting load-bearing code.

If you are reading this and you are not sure what tier you are operating at,
the default is **Tier 1**. Read [reliability.md § AI Capability Tiers](reliability.md#ai-capability-tiers)
before continuing.

---

## 1. The Pre-Flight Checklist

Before touching any file, an agent must answer these questions:

1. **What crate is the file in?** Look up the default tier in
   [reliability.md § File-to-Tier Mapping](reliability.md#file-to-tier-mapping).
2. **What is the file's `// @reliability` marker?** Read the first 30 lines.
3. **Does the file have an explicit `// @ai-tier:` marker that raises the
   required tier?**
4. **What is my own tier?** If unknown, assume Tier 1.
5. **Is my tier ≥ the required tier?** If not, see § 5 ("When You Are
   Blocked").
6. **Will my edit cross a reliability boundary?** Edits that move a file
   between Normal/Hazmat/Experimental/Insecure follow the promotion/demotion
   protocols in `reliability.md`. AI agents may only **draft** such changes,
   never finalise them.

Only after all six answers are clear may the agent edit code.

**Borderline cases — the reasoning pass.** Before stopping on a tier
mismatch, the agent must first produce a short reasoning paragraph (inline,
before any hand-off document) that answers:

- Is the proposed change actually *cryptographic* in nature (new protocol
  logic, invariant about secret-randomness, soundness argument), or is it
  mechanical/infrastructural (adding a variant to an enum, wiring a new
  type through existing match arms, fixing a compile error)?
- What specific part of the file's reliability level makes the change
  sensitive: is it the field arithmetic, the protocol transcript, the
  hazmat call-site invariants, or just the crate location?
- Is there any ambiguity about whether a lower-tier model could execute
  this correctly?

Surface this reasoning to the owner and **wait for an explicit go/no-go
before either writing the hand-off or proceeding.** The owner may grant
a session-scoped override, in which case the agent proceeds and marks
the change `@ai: assisted`. If the owner confirms the block, produce the
full hand-off document described in § 5.

---

## 2. The Five Safe Refactor Patterns

These patterns are within Tier 1 capability **regardless of which tier the
file requires**, because they are observably equivalent transformations and
the test suite catches mistakes. Use them freely:

### 2.1 Rename a symbol

Use a workspace-wide rename: every reference must update. The test suite must
pass before and after.

### 2.2 Move a file or split a module

`git mv` plus updating `mod` declarations. No semantic change. The Rust
compiler is the oracle: if the workspace builds and tests pass, the move is
correct.

### 2.3 Reformat / restyle

Apply `rustfmt`, fix lints, normalise whitespace. Never reformat in the same
commit as a substantive change — it makes code review difficult.

### 2.4 Extract a helper

Pulling a block of code into a new private function in the same module, with
no change to its inputs or outputs, is observably equivalent. Verify by
running the test suite. **Do not** extract helpers from Hazmat-tagged code —
the inlined form may be load-bearing for the safety argument; check the
file's `# Safety` section.

### 2.5 Add a test

Adding a new test that exercises existing behaviour is always allowed.
Adding a test whose **expected output you guessed** is not — verify the
expected output by running the existing implementation, never by intuition.
For cryptographic code, the expected output must come from a reference
implementation or from running the existing code, not from a derivation the
agent did itself.

---

## 3. The Tier-2 Operating Rules (Compiler & IR Work)

When working on the compiler, IR, lowering, printers, backends, weavers, or
LIR codegen — i.e. anything in `crates/compiler/`, `crates/ir/`, or
`crates/macros/` — additional rules apply on top of the safe patterns above.
These come from [`AGENTS.md`](../AGENTS.md) and are repeated here for
convenience.

1. **No raw strings as IR expressions.** Always construct typed
   `IrExpr`/`IrStmt` nodes. If the printer mishandles a node, fix the
   printer; do not work around it.
2. **Tests must compile and run the output.** Lower the IR through the real
   backend (`print_module` → `rustc`, `print_module_ts` → `tsc`,
   `print_module_c` → `cc`). Do not assert on variable names, statement
   counts, or IR structure unless verifying a hard-to-change structural
   invariant.
3. **Use `_ =>` catch-alls** on IR type matches so parallel work does not
   conflict.
4. **`volar-spec` must remain deterministic.** No `rand` crate. The spec
   layer uses `SpecRng`.
5. **Do not change `grafhen_xor`.** Garbled circuits depend on free
   composable XOR.
6. **CFG and flat AST are not interconvertible.** The flat AST is total; the
   CFG is not.
7. **Use `IrExpr::RawMap` for `[T; N]::map`-style expressions.** Never
   `MethodCall` + `Closure`.

---

## 4. The Tier-3 Operating Rules (Cryptographic Work)

When working on `crates/spec/`, `crates/spec/volar-primitives/`,
`crates/spec/volar-common/`, `crates/oram/volar-oram-core/`,
`crates/oram/volar-oram/`, the cryptographic schemes inside
`crates/compiler/volar-weaver/` (`grafhen.rs`, `garble`, `vole`), or any file
marked `@reliability: hazmat`:

1. **New cryptographic constructions enter at Experimental.** Never start at
   Normal or Hazmat. Use `// @reliability: experimental` and add the file to
   the *Current Experimental Files* table.
2. **Each new construction needs a review plan.** Write a document analogous
   to [grafhen-review-plan.md](grafhen-review-plan.md): an ordered list of
   internal review, correctness review, and integration review phases, each
   with explicit reviewer requirements. AI cannot complete Phase 3
   (integration review) alone.
3. **Reference implementations are mandatory.** Cite the paper, the section,
   and the equation/algorithm number for every new construction. If you
   diverge from the reference, mark the divergence inline and explain why.
4. **Soundness arguments must be written down.** Even at Experimental, the
   intended argument for why the construction is sound must appear in the
   spec file's module-level doc comment or in a `docs/` companion document.
5. **Hazmat files cannot be `@ai: generated`.** The subtle correctness
   constraints of a Hazmat file require human cryptographic understanding.
   Hazmat files may be `@ai: assisted` only with extreme care.
6. **Promotion is a human action.** Even if every Phase 1–4 reviewer is
   satisfied, the actual rename of `// @reliability: experimental` to
   `// @reliability: normal` (or `hazmat`) must be performed or signed off
   by a human, not an AI agent.

---

## 5. When You Are Blocked

You are blocked when an edit you want to make requires a tier higher than
yours. Examples:

- A Tier 1 agent is asked to refactor `volar-spec/src/garble.rs` (Tier 3).
- A Tier 2 agent is asked to fix a soundness issue in
  `volar-oram/src/lib.rs` (Tier 3).
- Any tier is asked to promote an Experimental file to Normal (requires a
  human).

**Step 1 — Reasoning pass (before anything else).** Write a short paragraph
(directly in your reply, not in a file) that covers:

1. What the proposed change actually does in concrete terms.
2. Whether the sensitivity comes from the change itself (new cryptographic
   logic, soundness argument) or merely from the file's location in a
   high-tier crate (e.g. adding a match arm, wiring a new type).
3. Whether a lower-tier model could execute this correctly — and why or why not.

Then **stop and present this reasoning to the owner.** Do not write the
hand-off document yet; do not proceed with the edit yet.

**Step 2 — Owner decision.** The owner will either:
- **Grant a session-scoped override:** proceed with the edit; mark the change
  `@ai: assisted` and note the override in the commit message.
- **Confirm the block:** produce the full hand-off document below.

**Hand-off document format:**

```
File:           <relative path>
Required tier:  <tier number and reason>
My tier:        <tier number>

Desired change:
  <one-paragraph description>

Why this exceeds my tier:
  <one-paragraph justification, citing reliability.md>

What I have already done that is within my tier:
  <list, e.g. "added failing test", "renamed helper", "wrote docs">

What the higher-tier agent or human should do:
  <ordered list>
```

Hand-off documents are themselves Tier 1 artefacts. A lower-power agent can
always produce a hand-off — and **a thorough hand-off is more valuable than a
half-correct edit at the wrong tier.**

A Tier 1 agent can prepare an entire pull request scaffold for a Tier 3
change: write the failing test, write the doc updates, write the hand-off,
and stop short of the Tier 3 file edit. A Tier 3 agent can then complete it
in a single short pass.

---

## 6. The Tier-Aware Refactor Workflow

A typical "make this safer / cleaner" refactor that touches files at
multiple tiers should be split as follows:

| Phase | Tier required | Output |
|---|---|---|
| 1. Survey | 1 | Notes describing every file the refactor will touch and its required tier. |
| 2. Test scaffolding | 1 or 2 | New tests that pin current behaviour. Must pass before any structural change. |
| 3. Mechanical changes | 1 | Renames, moves, formatting. No semantic change. |
| 4. Compiler / IR changes | 2 | Type-system or lowering changes. |
| 5. Cryptographic changes | 3 | Spec-level edits. |
| 6. Promotion / demotion | Human | Reliability marker changes. |

Each phase is its own commit (or its own pull request). A lower-tier agent
should aim to complete phases 1–4 in their entirety and stop before phase 5.

---

## 7. Common Pitfalls

### Pitfall: "It's just a tiny change"

A one-line change inside `volar-spec/src/vole/prove.rs` is **still a Tier 3
change**, because the file's required tier is 3, not because the change is
big. The tier system gates *files*, not *change sizes*.

### Pitfall: "The tests still pass"

A passing test suite is necessary but not sufficient for cryptographic
correctness. Many cryptographic bugs (timing channels, off-by-one in
challenge generation, missing domain separation) do not manifest in
functional tests. The Tier 3 requirement exists precisely because passing
tests do not prove security.

### Pitfall: "The compiler accepts it"

The Rust compiler accepts many incorrect cryptographic implementations. Type
signatures do not encode security properties. The same applies to TypeScript,
C, and any other backend.

### Pitfall: Re-marking reliability to make a change easier

Lowering a file from `@reliability: hazmat` to `@reliability: normal` to
allow an `@ai: generated` change is **never** correct. If you are tempted,
stop and re-read § 5.

### Pitfall: "I'll just inline this Hazmat function"

Inlining a Hazmat function into a non-Hazmat caller imports its safety
constraint into the caller without the marker. Either propagate the
`@reliability: hazmat` marker, or restructure so the safety constraint
remains encapsulated.

### Pitfall: Editing the `.insecure` file directly

`.insecure` files are research records, not code. Modifying them in any way
that resembles "fixing the bug" is wrong — by policy, an `.insecure` file
is promoted to Experimental by writing a **new** file (typically `_v2.rs`)
with the corrective construction, leaving the `.insecure` file untouched as
a record. See [insecure.md](insecure.md).

---

## 8. Quick Reference: Tier-Bound Tasks

| Task | Minimum tier | Notes |
|---|---|---|
| Read code, summarise behaviour | 1 | Always allowed. |
| Update a doc to match code | 1 | Always allowed. |
| Rename a private symbol workspace-wide | 1 | Tests must pass before and after. |
| Add a new test that re-pins current behaviour | 1 or 2 | Tier 2 if the property tested is cryptographic. |
| Add a compiler pass | 2 | Plus a generator and a property test. |
| Add a backend (C, WASM, …) | 2 | Must round-trip through compile-check tests. |
| Add a new IR variant | 2 | Catch-all arm policy applies. |
| Modify a `volar-spec` protocol | 3 | Must remain `@reliability: experimental` or stricter. |
| Add a new cryptographic scheme | 3 | Plus a review plan; enters at Experimental. |
| Modify ORAM client state | 3 | Stash invariants are subtle. |
| Promote Experimental → Normal | Human | AI may draft, never finalise. |
| Demote Experimental → Insecure | 3 | Must cite the breaking attack with paper reference. |
| Modify a `.insecure` file | 3 | Treat the file as cryptographic claim, not code. |

---

## 9. Related Documents

- [reliability.md](reliability.md) — the authoritative tier system, file
  mappings, and reliability/AI marker policy this guide assumes.
- [`AGENTS.md`](../AGENTS.md) — short, always-applied workspace rules
  including the IR genericity and test policies.
- [integration-guide.md](integration-guide.md) — for *users* integrating
  Volar into applications, not for AI agents modifying Volar itself.
- [insecure.md](insecure.md) — policy for the `.insecure` file extension.
- [grafhen-review-plan.md](grafhen-review-plan.md) — example of a properly
  structured review plan a Tier 3 agent should produce for new
  constructions.
