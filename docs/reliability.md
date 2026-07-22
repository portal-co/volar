# Code Reliability Levels

Volar develops and refines program-related cryptography publicly. A core part
of that mission is being transparent about how much each piece of code can be
trusted. Volar uses a four-level reliability classification for all source code
and documentation.
The reliability level determines:
- Whether the file is compiled as part of any crate.
- What source-level markers must appear.
- What documentation and review requirements apply.
- How AI contributions are labelled (see [AI Markers](#ai-markers)).

---

## The Four Levels

### Level 1 — Normal

**Definition:** Code based on established cryptographic constructions with
published security proofs. Implementations follow the reference design
faithfully, have been reviewed against the specification, and have passing
tests. No novel cryptographic claims are made.

**Examples:** `volar-primitives` (standard field arithmetic), `volar-common`
(standard hash commitments and PRG doubling), `volar-compiler` (a compiler and
transpiler — enables cross-target deployment of spec-layer protocols; no
cryptographic claims of its own), `volar-ir` (circuit IR data structures).

**File extension:** `.rs` (normal Rust source; compiled as part of the crate).

**Required source marker:**
```rust
// @reliability: normal
```
Place at the top of the file, below any copyright/license header but before
`use` declarations.

**Obligations:**
- All public functions must have doc comments explaining their behaviour.
- Tests must exist for all non-trivial logic.
- Deviations from the reference construction must be noted inline.

---

### Level 2 — Hazmat

**Definition:** Code that is cryptographically correct but requires specialized
knowledge to use safely. The construction itself is sound (either based on
established work or verified through the experimental pipeline), but misuse
breaks security properties in non-obvious ways. Named after the hazardous
materials convention: the substance is real and handled by experts, not
discarded.

**Examples:** `volar-spec/src/vole/vope/ai_hazmat.rs` (degree-K VOLE polynomial
multiplication — correct only inside a Quicksilver-style constraint check).

**File extension:** `.rs` (normal Rust source; compiled as part of the crate).

**Required source marker:**
```rust
// @reliability: hazmat
// @hazmat-reason: <one-line explanation of what breaks on misuse>
```

**Obligations:**
- The `@hazmat-reason` must name the specific security property that breaks on
  misuse (e.g. "zero-knowledge" rather than just "security").
- The module-level doc comment must contain a `# Safety` section explaining the
  correct usage context and at minimum one example of incorrect usage.
- Callers inside the crate that call hazmat functions must annotate the call
  site with a `// SAFETY(hazmat): <justification>` comment.

---

### Level 3 — Experimental

**Definition:** Novel constructions designed in this codebase that do not yet
have a published security proof or peer review. The code is compiled and tested,
but is explicitly not yet trusted. The intended lifecycle is:

```
experimental → (peer review / publication) → hazmat or normal
           ↘ (disproven or found insecure) → insecure
```

Experimental code is the only correct destination for new cryptographic
constructions. Writing new cryptography directly at the normal or hazmat level
is prohibited.

**Examples:** `volar-spec/src/garble.rs` (garbled circuit scheme, introduced
under `experiment: garbling`), `volar-spec/src/mpc.rs` (MPC types, introduced
under `experiment: mpc`), `volar-spec/src/byte_gen/prover.rs` and
`byte_gen/verifier.rs` (prover/verifier byte generation — the `cda059c`
commit message `actually unsound, oops` demonstrates the commit history of
active experimental revision).

**File extension:** `.rs` (compiled)
**Required source markers:**
```rust
// @reliability: experimental
// @experimental-status: <one of: design | review-pending | review-in-progress>
// @experimental-since: <git commit hash or date when this was first added>
```

**Obligations:**
- Must compile cleanly with no warnings in its supported build configuration.
- Must have at least one test that exercises the main intended use case, even if
  the test cannot yet verify cryptographic soundness.
- Must have a corresponding entry in this document's [Current Experimental
  Files](#current-experimental-files) table.
- Its callers and documentation must preserve its Experimental status; it may
  not be used to imply a deployment, parameter, or security claim.

---

### Level 4 — Insecure

**Definition:** Code that is known or suspected to be cryptographically broken
and cannot be compiled or used. May have been demoted from experimental after
being disproven, or may have been placed here directly upon initial discovery
of a fundamental flaw. The only two valid next states are:

- **→ experimental:** A complete rework addresses the root flaw (e.g. replacing
  a hash-based construction with an LWE-based one). The old insecure file
  remains as a research record.
- **→ deprecated/removed:** The line of work is abandoned and documented as
  definitively false or out of scope.

**Examples:** `crates/volar-spec/src/xsat.rs.insecure` — demoted from experimental
(`bf2e4b9 experiment: deprecate xsat`) after the hash-based witness encryption
approach was found to be information-theoretically impossible.

**File extension:** `.rs.insecure` — the `.insecure` suffix prevents Rust from
compiling it. **Never** `.rs`.

**Required source marker** (inside the file, as a comment, for documentation purposes):
```rust
// @reliability: insecure
// @insecure-reason: <summary of why this is insecure>
// @insecure-since: <git commit hash that demoted this file>
```

**Obligations:**
- Never appears in a `mod` declaration or `include!` macro.
- Must have a corresponding entry in `docs/insecure.md` explaining in detail
  why it is insecure.
- May only be promoted to experimental via a documented rework (not a rename).

---

## Summary Table

| Level | Extension | Compiled | Security claim |
|---|---|---|---|
| Normal | `.rs` | Always | Established, proven |
| Hazmat | `.rs` | Always | Proven but requires expert use |
| Experimental | `.rs` | Supported build configuration | Novel; designed for review, not yet trusted |
| Insecure | `.rs.insecure` | Never | Known/suspected broken; research record only |


## AI Markers

Files meaningfully shaped by an AI assistant carry a module-level `//! @ai:`
marker. It records authorship and review history, not authority or a substitute
for evidence.

| Marker | Meaning |
|---|---|
| `none` | No AI involvement. |
| `supervised` | AI drafted work that a human reviewed line by line. |
| `assisted` | AI and human collaborated; the human reviewed the intent and output. |
| `generated` | AI produced most content; review was high-level. |
| `unreviewed` | AI-produced content without meaningful review; permitted only in Experimental or Insecure material. |

Normal and Hazmat files must not be marked `unreviewed`; Hazmat files must not
be marked `generated`. Update the marker when it no longer describes the actual
review history.


## Evidence-based contribution and review

All agents may contribute. Capability tiers, model allowlists, model
identification, and sub-threshold source tags are not part of this policy. The
required guardrail is evidence appropriate to the claim:

- Normal code changes need focused tests; compiler and backend changes need
  generated-code compile-and-run evidence where applicable.
- Hazmat changes retain their `@hazmat-reason`, `# Safety` documentation, and
  `SAFETY(hazmat)` call-site justifications.
- Experimental cryptographic work remains explicitly Experimental, has an
  executable main-use-case test and a paper-bound review artifact, and cannot
  justify a security, parameter, noise, or deployment claim without the
  independent/human review named by that artifact.
- New cryptographic constructions enter at Experimental. Promotion from
  Experimental, production parameter/security claims, and reliability-policy
  changes require a human decision.
- Generated files are updated through their generator pipeline. A failure caused
  by a missing environment dependency is recorded separately from a source
  failure.

A review/handoff records the exact reproducer, source evidence, relevant
invariants, unresolved blockers, and next smallest safe action. The
[merge-recovery policy handoff](handoffs/merge-recovery/policy-and-reliability.md)
records the migration that adopted this model-neutral policy.


## Commit Message Convention

Commit prefixes such as `[AI]`, `[AI+human]`, `[human]`, and `experiment:` may
record authorship or reliability history, but they do not establish correctness.
Use the AI marker and the change's test/review evidence to describe the actual
state.


## Merged-tree update — 2026-07-22

Evidence: policy stream `6596629` → `d783cb1` and
[policy-and-reliability handoff](handoffs/merge-recovery/policy-and-reliability.md).
Removed capability-tier enforcement, model gating, sub-threshold tags, and the
obsolete `volar_experimental` feature requirement. Reliability levels, AI
provenance markers, paper binding, and human promotion decisions remain.


## Current Experimental Files

The following compiled files are at the experimental reliability level.


| File | Experimental since | Status | Notes |
|---|---|---|---|
| `crates/volar-spec/src/garble.rs` | `929a03c` (experiment: garbling) | design | Half-gate garbling over VOLE; no security proof yet |
| `crates/volar-spec/src/mpc.rs` | `79ee6d7` (experiment: mpc) | design | MPC party type skeleton; semantics TBD |
| `crates/volar-spec/src/byte_gen/prover.rs` | `263eab1` (fix name) | review-pending | Revised after `cda059c` (actually unsound, oops) |
| `crates/volar-spec/src/byte_gen/verifier.rs` | `263eab1` (fix name) | review-pending | Same revision cycle; `58e8f84` last structural change |

---

## Current Insecure Files

| File | Demoted | Reason summary |
|---|---|---|
| `crates/volar-spec/src/xsat.rs.insecure` | `bf2e4b9` (experiment: deprecate xsat) | Hash-based witness encryption is information-theoretically impossible; see [insecure.md](insecure.md) |

---

## Promotion and Demotion Protocol

### Experimental → Normal or Hazmat

1. A peer review or publication establishes the security of the construction.
2. The `@experimental-status` marker is updated to `review-in-progress` and
   then removed once review is complete.
3. The file is re-marked `@reliability: normal` or `@reliability: hazmat`
   as appropriate.
4. The entry is removed from the [Current Experimental Files](#current-experimental-files)
   table and added to [spec.md](spec.md) or the relevant crate doc.

### Experimental → Insecure

1. A fundamental flaw is discovered (e.g. information-theoretic impossibility,
   specific attack, reduction to a false assumption).
2. The file is renamed from `foo.rs` to `foo.rs.insecure`.
3. The `mod foo;` declaration in `lib.rs` is removed (or commented with a note).
4. The `@reliability: insecure` marker and `@insecure-reason` are added.
5. A detailed entry is added to [insecure.md](insecure.md).
6. The entry in [Current Experimental Files](#current-experimental-files) is
   moved to [Current Insecure Files](#current-insecure-files).

### Insecure → Experimental (rework)

A rework must address the root flaw with a different construction, not merely
patch the existing one. The protocol:

1. Write a new file (e.g. `foo_v2.rs`) at the experimental level explaining
   how it differs from the insecure version and why the flaw does not apply.
2. Add a `@insecure-predecessor: <old file>` marker in the new file.
3. The old `.rs.insecure` file is **not renamed** — it stays as a record.
4. Update `insecure.md` to note the rework and link to the new file.
