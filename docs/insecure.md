# The `.insecure` File Extension

## Purpose

Files with the `.insecure` extension contain **cryptographically broken or
unproven constructions** that are kept in the repository for research,
experimentation, or as a record of approaches that were attempted but not
validated. They are **never compiled** as part of any crate and must not be
imported or depended upon by production code.

The extension serves as an explicit, machine-checkable signal:
- It cannot be accidentally included via `mod foo;` (Rust only recognizes `.rs`).
- It is immediately visible to code reviewers, CI scripts, and auditors.
- It avoids the ambiguity of comment-based warnings like `// TODO: insecure`.

---

## Policy

1. **Never compile `.insecure` files.** They must not appear in any `mod`
   declaration or `include!` macro.
2. **Never copy code from an `.insecure` file to a `.rs` file** without a
   full security review and explicit sign-off.
3. **Document why the construction is insecure** in a comment at the top of
   the `.insecure` file.
4. **Leave `.insecure` files in the repository.** They are a research record;
   deleting them loses context. Use `git log` and the comment header to
   understand their history.

---

## Current `.insecure` Files

### `crates/volar-spec/src/xsat.rs.insecure`

**Status:** Experimental. Witness encryption attempt. Security not established.

**What it tries to do:** Implement a form of **witness encryption** for
satisfiability (SAT) problems using VOLE commitments and a hash function.

A `SatProblem<K, N>` encodes N clauses of K literals each. `SealedSatProblem`
is the "encrypted" version where the solution (a satisfying assignment) acts
as the decryption key. The `seal` method generates sealed clauses; `open`
takes a satisfying assignment and returns a digest output.

**Why it is insecure:**

The construction is commented out of `volar-spec/src/lib.rs`
(`// pub mod xsat;`) because:

1. **Soundness is unproven.** The scheme is not derived from a published
   construction with a security proof. The structure loosely resembles
   witness-encryption proposals based on multilinear maps, but those require
   cryptographic assumptions (e.g. indistinguishability obfuscation or multilinear
   maps) that are not available here.

2. **The XOR-tree structure leaks information.** The `seal` function uses a
   sequence of hash-chain steps (`seed = H(seed || waste)`) to build `xor_roots`
   and `cells`. The pattern of which cells are non-zero is correlated with the
   clause structure, potentially leaking information about the satisfying
   assignment.

3. **No simulator argument.** There is no proof that the `SealedSatProblem`
   transcript is simulatable — i.e. that it is indistinguishable from a random
   string to a party that does not know the satisfying assignment.

4. **The `ixor_roots`/`xor_roots` distinction is ad hoc.** The separation
   between "include if negated" vs. "include if positive" roots is not justified
   by any reduction to a known hard problem.

**What is needed for a real witness encryption scheme:**
- A reduction to a cryptographic hardness assumption (e.g. LWE, DCR, or an
  algebraic structure supporting multilinear maps).
- A formal security proof in the random oracle model or the standard model.
- A peer review or publication.

Until these are in place, the file remains `.insecure`.

---

## Future `.insecure` Candidates

The following constructions may be added to `.insecure` files as work
progresses:

- **Preprocessing-phase protocols** that assume honest behavior from a
  semi-honest majority without yet having a full malicious-security argument.
- **Interactive proofs** whose soundness relies on a specific field size before
  the soundness error analysis is written down.
- **Optimized variants** of core protocols where an optimization introduces
  a subtle bias or leakage that has not been ruled out.

---

## Relationship to `ai_hazmat`

The `ai_hazmat.rs` module inside `volar-spec/src/vole/vope/` is **different**
from `.insecure` files. It is a properly compiled `.rs` file whose contents are
*correct* but *require careful use*:

- `mul_generalized` computes the algebraic product of two VOLE polynomials.
- This product is only sound in a Quicksilver-style constraint check.
- Using it outside that context (e.g. as a standalone multiplication) does not
  break field arithmetic, but breaks the zero-knowledge property of the enclosing
  proof.

The `ai_hazmat` name signals "use with expert knowledge" — not "cryptographically
broken". See [spec.md](spec.md#degree-k-multiplication-ai_hazmatrs) for the
correct usage context.
