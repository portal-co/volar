# GRAFHEN Review Plan

> This plan tracks the path from `@experimental-status: design` toward
> publication-quality review. It is **not** a commitment to any timeline.
> See [grafhen.md](grafhen.md) for the construction and [reliability.md](reliability.md)
> for the promotion protocol.

---

## Current Status

| File | Reliability | AI marker | Status |
|------|-------------|-----------|--------|
| `crates/spec/volar-spec/src/grafhen.rs` | experimental | unreviewed | design |
| `crates/compiler/volar-weaver/src/grafhen.rs` | experimental | unreviewed | design |
| `docs/grafhen.md` | — | — | design |
| `docs/grafhen-appsec.md` | — | — | design |

---

## Phase 0 — Prerequisites (before review begins)

These must be completed before any external reviewer engages.

- [ ] **Feature-gate the module** behind `volar_experimental` in
      `crates/spec/volar-spec/Cargo.toml` and
      `crates/compiler/volar-weaver/Cargo.toml`. The feature must not be
      enabled by default.

- [ ] **Verify AND correctness** with properly derived `and_w1` / `and_w2`
      constants from ePrint 2025/1907. The current test suite uses identity
      constants which bypass algebraic correctness. Add a test that:
      1. Sets up a full GRAFHEN key with recommended parameters.
      2. Verifies `decrypt(AND(enc_a, enc_b)) == a AND b` for all 4 input
         combinations.

- [ ] **Confirm the IND-CPA break applies** to this implementation by running
      the cross-reduction distinguisher from ePrint 2026/700 against
      ciphertexts produced by `grafhen_encrypt`. Document the observed
      distinguishing advantage.

- [ ] **Resolve the `@experimental-since` marker** with a concrete git commit
      hash once this design is merged.

---

## Phase 1 — Internal Code Review

**Goal:** Identify implementation bugs, not cryptographic security.

- [ ] Review `grafhen.rs` (spec) for:
  - Correct left-to-right generator composition in `eval_word_to_perm`.
  - Correct 12-segment concatenation order in `grafhen_and`.
  - Bounds checking: `concat_words` returns `None` on overflow; all callers
    handle this correctly.
  - Memory safety: no out-of-bounds indexing on `data[..len]`.
  - No use of `Vec` or `alloc` in the `no_std` module.

- [ ] Review `grafhen.rs` (weaver) for:
  - Correct gate lowering (XOR → `grafhen_xor`, NOT → `grafhen_not`,
    AND → `grafhen_and`).
  - Or-expansion via De Morgan is identical to other weaver passes.
  - Generated types use the literal WBOUND (not a type parameter name).
  - `IrExpr::Path` segments spell the correct function names from the spec.

- [ ] Run `cargo test` with `volar_experimental` enabled; all tests pass.

- [ ] Run `cargo check` on the generated code (the weaver test does this).

**Reviewer requirement:** At least one human engineer who has read both the
spec crate and the weaver crate in full.

---

## Phase 2 — Cryptographic Correctness Review

**Goal:** Verify that the homomorphic operations are correctly implemented
with respect to ePrint 2025/1907. This is **not** a security review; the
IND-CPA break is accepted.

- [ ] Verify the AND gate construction:
  - Confirm the 12-segment sequence matches the paper exactly.
  - Confirm `and_w1` and `and_w2` are the correct constants per the paper's
    group construction.
  - Verify that `decrypt(AND(enc_a, enc_b)) == a AND b` holds for a real key.

- [ ] Verify the XOR gate:
  - Confirm word concatenation corresponds to group multiplication.
  - Confirm `decrypt(XOR(enc_a, enc_b)) == a XOR b`.

- [ ] Verify decryption:
  - Confirm `eval_word_to_perm` computes the correct group element for
    multi-generator words.
  - Confirm the `π[0] == 0` / `π[0] == 4` check matches the paper's encoding.

- [ ] Document any deviations from the paper with rationale.

**Reviewer requirement:** Someone who has read ePrint 2025/1907 and can verify
the implementation against it. Does not need to be the same person as Phase 1.

---

## Phase 3 — ZK Integration Review

**Goal:** Verify that using GRAFHEN inside a VOLEitH ZK proof achieves the
claimed correctness guarantee.

- [ ] Review the two-layer architecture (ZK layer + GRAFHEN layer):
  - Confirm the ZK proof establishes circuit evaluation correctness.
  - Confirm the oracle-avoidance requirements in `grafhen-appsec.md` are
    sufficient to prevent key recovery under the threat model.
  - Confirm the ZK proof does not accidentally reveal information about
    GRAFHEN's internal structure beyond what is intended.

- [ ] Write an integration test that:
  1. Produces a GRAFHEN homomorphic evaluation of a boolean circuit.
  2. Wraps it in a VOLEitH ZK proof (`weave_vole_prover`).
  3. Verifies the proof (`weave_vole_verifier`).
  4. Checks that the output ciphertext decrypts to the expected value.

- [ ] Review `grafhen-appsec.md` for completeness and accuracy given the
  specific deployment context.

**Reviewer requirement:** Someone familiar with both Quicksilver-style VOLEitH
and with GRAFHEN. This phase cannot be completed by AI alone.

---

## Phase 4 — Promotion Decision

After Phases 1–3 are complete, a promotion decision is made:

| Outcome | Action |
|---------|--------|
| All phases pass; correctness and ZK integration are verified | Update `@experimental-status` to `review-pending`; tag for external review |
| Phase 2 finds the AND construction is incorrect (not just insecure) | Fix and re-run Phase 2; or demote to `insecure` if unfixable |
| Phase 3 finds the ZK integration does not establish correctness | Fix integration or narrow the correctness claim |
| A future attack breaks **correctness** (not just confidentiality) | Demote to `insecure` per the reliability protocol |

**Note:** IND-CPA insecurity does not by itself require demotion to `insecure`.
The file carries a permanent warning. Demotion happens only if the homomorphic
correctness guarantee is broken.

---

## Open Questions

1. **AND constants:** What are the exact values of `and_w1` and `and_w2`
   for the recommended N=11, D=? parameters? These must be derived from
   the paper and validated experimentally.

2. **Word bound:** What is the tightest achievable `WBOUND` for a depth-d
   boolean circuit with the S₁₁ rewriting system? The answer determines
   the memory cost of the ciphertext representation.

3. **Timing channel:** Is there a practical constant-time implementation of
   `eval_word_to_perm` that pads all words to `WBOUND`? What is the
   performance cost?

4. **Key generation:** How are the D secret generators chosen? The paper
   specifies they must generate the full group Sₙ — what is the easiest
   way to verify this property programmatically?
