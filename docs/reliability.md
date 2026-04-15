# Code Reliability Levels

Volar develops and refines program-related cryptography publicly. A core part
of that mission is being transparent about how much each piece of code can be
trusted. Volar uses a four-tier reliability classification for all source code
and documentation.
The tier determines:
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

**File extension:** `.rs` (compiled), but the module **must** be gated behind
a `volar_experimental` Cargo feature so that downstream users opt in explicitly.
The feature is defined in each crate's `Cargo.toml` and is not enabled by
default.

**Required source markers:**
```rust
// @reliability: experimental
// @experimental-status: <one of: design | review-pending | review-in-progress>
// @experimental-since: <git commit hash or date when this was first added>
```

**Obligations:**
- Must compile cleanly with no warnings when the `volar_experimental` feature
  is enabled.
- Must have at least one test that exercises the main intended use case, even if
  the test cannot yet verify cryptographic soundness.
- Must have a corresponding entry in this document's [Current Experimental
  Files](#current-experimental-files) table.
- Cannot be depended upon by normal or hazmat code except through a
  `#[cfg(feature = "volar_experimental")]` gate.

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

| Level | Extension | Compiled | Feature-gated | Security claim |
|---|---|---|---|---|
| Normal | `.rs` | ✅ Always | ❌ | Established, proven |
| Hazmat | `.rs` | ✅ Always | ❌ | Proven, expert use only |
| Experimental | `.rs` | ✅ With feature | `volar_experimental` | Novel, unproven |
| Insecure | `.rs.insecure` | ❌ Never | N/A | Known/suspected broken |

---

## AI Markers

All files whose content was meaningfully shaped by an AI assistant must carry
an AI marker at module level. This includes files created by AI, files where AI
contributed more than cosmetic changes (reformatting, renaming), and files
reviewed and approved by AI with the human acting as supervisor.

The AI marker takes the form of a module-level doc-comment attribute:

```rust
//! @ai: <level>
```

where `<level>` is one of:

| Marker | Meaning |
|---|---|
| `//! @ai: none` | No AI involvement. All content is human-authored. |
| `//! @ai: supervised` | AI drafted code; human reviewed and approved every significant decision. Changes were made by the human or explicitly directed. Equivalent to code the human could have written without AI. |
| `//! @ai: assisted` | AI and human collaborated with roughly equal decision-making. Human reviewed all output but may not have independently verified every detail. |
| `//! @ai: generated` | AI produced the majority of the content; human reviewed at a high level (structure and intent) but did not independently verify every line. |
| `//! @ai: unreviewed` | AI produced content that has not been meaningfully reviewed by a human. Must not appear in normal or hazmat files. Permitted in experimental (as a signal it needs review) and insecure (as a documentation note). |

**Important:** The AI marker describes the *review process*, not just authorship.
A file that was AI-generated and then carefully line-by-line reviewed and
corrected by a human is `supervised`, not `generated`.

### AI Markers and Reliability Levels

The following combinations are valid:

| Reliability \ AI | none | supervised | assisted | generated | unreviewed |
|---|---|---|---|---|---|
| Normal | ✅ | ✅ | ✅ | ⚠ (must be reviewed) | ❌ |
| Hazmat | ✅ | ✅ | ⚠ (extra care) | ❌ | ❌ |
| Experimental | ✅ | ✅ | ✅ | ✅ | ✅ (signals review needed) |
| Insecure | ✅ | ✅ | ✅ | ✅ | ✅ |

Hazmat files must not be `generated` because the subtle correctness constraints
require human understanding of the cryptographic context, not just structural
approval.

Normal files must not be `unreviewed` because unreviewed code in a normal-reliability
context would silently undermine the security guarantees the level is supposed to
provide.

### Commit Message Convention

The commit history in this repo already uses informal AI labels:
`[AI]`, `[AI+human]`, `[human]`. These map to the formal markers as follows:

| Commit prefix | Equivalent AI marker |
|---|---|
| `[human]` | `none` or `supervised` |
| `[AI+human]` | `assisted` or `supervised` |
| `[AI]` | `generated` or `unreviewed` |
| *(no prefix)* | Determined by content; default assume `assisted` for human-led sessions |
| `experiment: …` | No AI implication; marks experimental reliability level |

---

## Current Experimental Files

The following compiled files are at the experimental reliability level.
All require the `volar_experimental` feature to be enabled.

> **Note:** The `volar_experimental` feature gate is a goal of the reliability
> system; not all files listed below are yet gated behind it in the actual
> `Cargo.toml`. Gating them is a tracked task.

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
4. The `volar_experimental` feature gate is removed from the module.
5. The entry is removed from the [Current Experimental Files](#current-experimental-files)
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
