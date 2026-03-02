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

**Status:** Broken. Hash-based witness encryption for SAT. See detailed
analysis below.

---

## Analysis of `xsat.rs.insecure`

### What it tries to do

`SatProblem<K, N>` encodes N clauses of K literals each (a `K`-CNF formula).
`SealedSatProblem` is a ciphertext where the satisfying assignment is supposed
to act as the decryption key: `seal` encrypts, `open` decrypts.

The `open` method reconstructs a hash output from the sealed structure by
XOR-combining values according to which literals in each clause are satisfied.
If every clause has at least one true literal (i.e. the assignment satisfies the
formula), the XOR accumulation is intended to cancel to the `troot` commitment
and produce a meaningful output. If the formula is not satisfied, the output
should look random.

### Reason 1: Hash-based witness encryption is information-theoretically impossible

**This is not a matter of needing a stronger hash function or a better
construction technique. No hash-based witness encryption scheme can exist.**

Here is why. A witness encryption scheme must satisfy two properties
simultaneously:

- **Correctness:** A party with the witness can efficiently decrypt.
- **Security:** A party without the witness cannot distinguish the ciphertext
  from a random string.

Concretely, fix any specific satisfying assignment `w*`. The ciphertext is a
string of hash outputs. By assumption, `open(ciphertext, w*)` returns a specific
value `v`. But consider an adversary who knows the `SatProblem` and wants to
test whether a candidate assignment `w` satisfies the formula. They run `open`
themselves — it is a public deterministic algorithm. If it returns `v`, they
have decryption; if not, they don't. No security was ever provided: any party
that has the satisfying assignment can simply run `open` themselves, and any
party that does not cannot be prevented from brute-forcing.

More formally: **the `SealedSatProblem` contains no information that is not
already computable from the `SatProblem` alone.** Sealing with a hash does not
hide the formula; it just makes it slightly less convenient to evaluate. A
ciphertext that is entirely determined by a public computation over the formula
structure cannot be semantically secure.

This is distinct from the difficulty of *finding* a satisfying assignment: the
security of a witness encryption scheme would require that even knowing the
formula (and that it is satisfiable), a party without an explicit witness cannot
decrypt. Hash-based constructions cannot achieve this because:

1. The hash is a public, computable function. Given any candidate witness, anyone
   can evaluate it.
2. There is no trapdoor: nothing in the sealed structure is private to the
   encryptor that the adversary could not compute themselves.
3. The XOR-tree structure in `open` is linear in the satisfied literals. An
   adversary with partial knowledge of the assignment can systematically cancel
   terms, recovering information without fully solving the formula.

Real witness encryption requires an assumption that makes it *computationally
hard to evaluate the decryption circuit without a witness*, even when the
circuit is public. This requires either:
- **Multilinear maps** (Garg–Gentry–Halevi 2013): not yet instantiated securely.
- **Indistinguishability obfuscation (iO)**: implies everything, not yet
  practical.
- **Lattice assumptions (LWE/evasive LWE)**: recent results show promise for
  restricted function classes, but not general SAT.

None of these are hash functions.

### Reason 2: The `not` problem is information-theoretically insurmountable for this approach

The deeper structural issue in `xsat.rs.insecure` — and in any witness
encryption scheme built on simple commitments — is that **boolean negation
(`not`) cannot be implemented as a decryption enabler without leaking information
about the witness**.

In the code, `ixor_roots` and `xor_roots` try to handle positive vs. negated
literals:
- A positive literal contributes its root if the variable is `true`.
- A negated literal contributes its root if the variable is `false`.

But to "contribute if false" and "not contribute if true", the scheme must embed
a value that is either the root or trash depending on a hidden bit. This requires
that the sealed structure simultaneously:
- Reveal something when `x = false` (contribute the root).
- Reveal nothing useful when `x = true` (contribute trash).

The problem: the distinction between "root" and "trash" is not hidden. Both are
hash outputs of the same size and distribution. An adversary with the formula can
XOR the cells and roots in all possible combinations and check whether the result
is consistent with the `troot`. This is exactly what `open` does — so an
adversary who guesses the assignment can verify their guess.

More precisely: the `open` algorithm is itself a **decision procedure** for
whether the assignment satisfies the formula. Given any assignment `w`, running
`open` and checking the output against a known-correct output leaks whether `w`
satisfies each clause. This means the ciphertext is not even *one-way* with
respect to the witness — it is a complete oracle.

The information-theoretic core of the problem: a negated literal `¬x`
contributes to a clause *when x is false*. But from the outside, the clause
structure tells you exactly which literals are negated. So the contributer/
non-contributer status of each cell is directly readable from the ciphertext.
No amount of rerandomization with a hash can hide this, because the hash is
deterministic and the adversary knows all inputs except the assignment bits,
which are exactly what they are trying to learn.

This is analogous to the impossibility of computing `OR` in a linear secret
sharing scheme without an additional round of interaction: OR requires a `not`,
and a `not` in a commitment scheme requires that the committer can prove the
absence of a value, which is information-theoretically equivalent to revealing it.

### Reason 3: Specific structural leaks in the current code

Beyond the fundamental impossibility:

- **`seed` is advanced in a predictable pattern.** The `seal` function advances
  `seed` by `H(seed || waste)` for each clause, variable, and branch. The number
  of these advancements is fixed by `K` and `N`, not by the secret assignment.
  An adversary who observes the structure can count the hash chain length and
  recover the intermediate `seed` values by re-deriving from the start.

- **`cells[i.target]` accumulation leaks the assignment.** In `xor_roots`, only
  negated literals accumulate into `cells`. So the set of non-zero `cells` entries
  directly reveals which variables appear negated across all clauses where they
  are false — i.e., it reveals the partial assignment directly.

- **The `take(&mut seal)` in `open` is not cryptographic.** It takes only the
  first satisfied negated literal per clause. This is correct for the intended
  semantics (OR of negated literals), but it means the sealed structure encodes
  the first-satisfied-index for each clause in plaintext, which leaks the assignment.

---

## Draft: Directions for a Real Construction

> ⚠ **DRAFT — NOT REVIEWED.** The following section is speculative design
> thinking, not a security proof or implementation recommendation. It has not
> been reviewed by anyone with expertise in lattice-based cryptography,
> obfuscation, or witness encryption. Do not implement any of this without a
> full cryptographic review.

### Direction A: Lattice-based witness encryption (LWE)

Recent work (Tsabary 2022; Vaikuntanathan–Wee–Wichs 2022; Agrawal–Yamada) has
shown that **evasive LWE** — a strengthening of the standard LWE assumption —
implies witness encryption for restricted circuit classes. The intuition is:

- The formula is encoded as a matrix computation over a lattice.
- A satisfying assignment maps to a "short vector" that unlocks a trapdoor.
- Without the short vector, the ciphertext looks like a random lattice sample.

For a CNF formula specifically, one would:
1. Encode each clause as an LWE instance whose decryption key is a linear
   combination of the variable bits.
2. Combine clauses multiplicatively (AND) by nesting LWE samples — each layer
   of nesting is a multiplication in the lattice.
3. The full ciphertext decrypts iff all clauses decrypt, i.e. iff the assignment
   satisfies all clauses.

The main obstacle is that this requires multilinear maps or the evasive LWE
assumption, neither of which has an efficient, concrete, trusted instantiation
yet. Standard LWE alone is insufficient because it only supports linear
combinations (ADD/XOR), not multiplication (AND).

**For the Volar use case:** VOLE is itself a linear secret sharing scheme over
GF(2^k). If a future version of VOLE supports degree-D multiplication (via
polynomial VOLE / `Vope` with K > 1), it may be possible to encode a CNF formula
of bounded clause width as a bounded-degree polynomial over VOLE, and use the
polynomial evaluation as the decryption check. This would be a form of
"function-binding VOLE" rather than full witness encryption, and its security
would need a careful reduction.

### Direction B: Targeted assumptions for OR gates

The fundamental obstacle to implementing `not`/`or` in a commitment-based scheme
is that:

> **A commitment to a value `x` cannot simultaneously convince someone that
> `¬x` is "correct" without revealing `x`.**

This is information-theoretically unavoidable in the random oracle model with
no structured hardness assumption.

A **targeted assumption** approach would:
1. Accept that `not` of a decryption result is not achievable without
   computational hardness.
2. Introduce a specific, falsifiable assumption tailored to the construction
   that makes it *computationally hard* to distinguish "decryption succeeded"
   from "decryption failed" without the witness — even though the distinction is
   information-theoretically present.

For example, one could conjecture:

> *Targeted XOR-tree assumption:* Given a sealed clause where K literals are
> encoded and exactly one literal is true (i.e. the clause is barely satisfied),
> it is hard to determine *which* literal is true without knowing the satisfying
> assignment, under some hardness assumption.

This is similar in spirit to the "Phi-hiding assumption" used in some
oblivious transfer protocols, but specialized to the XOR structure here.

**Caveats:**
- Targeted assumptions are generally weaker than standard ones because they have
  not been studied by the community, may have unexpected attacks, and provide no
  reduction to well-understood problems.
- Even if such an assumption holds, it does not resolve the information-theoretic
  impossibility for *full* not: if the adversary can evaluate the decryption
  circuit (which they can, since it is public), the assumption only buys
  hardness of a specific distinguishing problem, not full semantic security.
- The `cells` leakage issue (Reason 3 above) would need to be fixed
  independently; a targeted assumption does not patch structural leaks.

### The core tension: OR in a linear scheme

The underlying tension is that CNF formulas require OR gates, and OR requires
NOT, and NOT in a secret-sharing or commitment scheme is the hard part:

- **AND** in a commitment scheme: straightforward. Commit to both inputs, provide
  openings for both, check both. Cost is linear in the number of AND gates.
- **XOR/ADD** in a VOLE scheme: free. Linear operations over VOLE are essentially
  zero-cost.
- **OR**: requires `a OR b = NOT(NOT(a) AND NOT(b))`. To avoid revealing which
  input is false, you need to hide the `NOT` behind a computational assumption.
- **NOT of a decryption event**: requires that the committer can convince the
  verifier that no valid opening exists, which is co-NP-hard in general and
  requires either an inefficient proof or a computational assumption.

Any real construction must either:
1. Reformulate the formula to avoid OR gates (e.g. use AND-of-XOR rather than
   OR-of-literals — but this changes the expressiveness).
2. Rely on a computational assumption strong enough to hide `NOT`.
3. Accept a weaker security notion (e.g. one-wayness rather than semantic
   security, or security against computationally bounded adversaries under
   a specific hardness assumption).

None of these are currently implemented in Volar. The `.insecure` file remains
until a construction with a proper security proof is identified.

---

## Future `.insecure` Candidates

The following constructions may be added to `.insecure` files as work progresses:

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
