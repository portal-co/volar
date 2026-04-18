# GRAFHEN: Technical Reference

> **WARNING: IND-CPA BROKEN.** See [§ The IND-CPA Break](#the-ind-cpa-break) below.
> This document exists to support academic study and the ZK-correctness use case.
> GRAFHEN **must not** be used as a confidentiality primitive.

## Overview

GRAFHEN (GRoup-bAsed Fully Homomorphic Encryption without Noise) is a
fully homomorphic encryption scheme over symmetric groups, introduced in
ePrint 2025/1907. It achieves noise-free computation in roughly 7.56 μs/AND,
approximately 3500× faster than lattice-based FHE (e.g., TFHE at ~26.4 ms/AND).

Volar implements the **public-key** variant: any party can encrypt, only the
keyholder decrypts. This enables multi-party computation where an encryptor
does not need the secret key.

**Current status in Volar:** `@reliability: experimental`, `@experimental-status: design`.
The code is compiled but not deployed. Its purpose is ZK correctness proofs
(see [§ ZK Architecture](#zk-architecture)) and academic exploration.

---

## Construction

### Group and Encoding

Plaintexts are bits in F₂. Ciphertexts are **words** over a set of generators
for the symmetric group Sₙ (N ≥ 11 recommended for academic parameters).

The encoding embeds S₆ ⊂ Sₙ and uses two distinguished elements:

| Bit | Permutation (0-indexed) | Condition |
|-----|-------------------------|-----------|
| 0   | identity                | `π[0] = 0` |
| 1   | (0 4)(2 3)              | `π[0] = 4` |

Decryption evaluates the word to a permutation by composing the secret generators
and reads `π[0]`.

### Key Structure

The secret key is D permutations of Sₙ (the generators) plus their inverses:

```rust
pub struct GrafhenKey<const N: usize, const D: usize> {
    pub gens: [[u8; N]; D],      // forward generators
    pub inv_gens: [[u8; N]; D],  // group-theoretic inverses
}
```

Generator index `i < D` maps to `gens[i]`; index `D + i` maps to `inv_gens[i]`.

The public key contains three word constants derived from the group structure:

```rust
pub struct GrafhenPublic<R, const WBOUND: usize> {
    pub enc_one: GrafhenWord<WBOUND>,  // canonical Enc(1)
    pub and_w1: GrafhenWord<WBOUND>,   // AND constant (first)
    pub and_w2: GrafhenWord<WBOUND>,   // AND constant (second)
    pub reducer: R,
}
```

### Homomorphic Operations

| Operation | Construction | Cost |
|-----------|-------------|------|
| Enc(0)    | empty word  | free |
| Enc(1)    | `enc_one`   | key lookup |
| XOR(a,b)  | concatenate words | O(len) |
| NOT(a)    | XOR with `enc_one` | O(len) |
| AND(a,b)  | 12-segment concatenation (see below) | O(len) |

**AND gate** (12-segment concatenation):

```text
result = w1 · a · w1 · w2 · b · w2 · w1 · a · w1 · w2 · b · w2
```

where `w1 = Enc(first_input)`, `w2 = Enc(second_input)`, `a = pk.and_w1`, `b = pk.and_w2`.

The 12 segments break down as: 4× `w1`, 4× `w2`, 2× `and_w1`, 2× `and_w2`.
Pre-reduction word length = `4·len(w1) + 4·len(w2) + 2·len(and_w1) + 2·len(and_w2)`.

### Word Reduction

Without reduction, AND multiplies word length by up to ~12×. The `WordReducer`
trait lets callers plug in a confluent terminating rewriting system (e.g.,
the S₁₁ system described in ePrint 2025/1907) that bounds word length:

```rust
pub trait WordReducer<const WBOUND: usize>: Clone {
    fn reduce(&self, word: &mut GrafhenWord<WBOUND>);
}
```

`NoReduction` is provided for toy circuits where word length is statically bounded.

The `WBOUND` const generic is a caller-specified ceiling on word length. A
runtime `assert!` enforces it after each AND gate; the reducer's job is to
make that bound achievable.

### Public-Key Encryption

```rust
pub fn grafhen_encrypt<R, const WBOUND: usize>(
    bit: bool,
    zero_cipher: &GrafhenWord<WBOUND>,  // fresh Enc(0) from caller
    pk: &GrafhenPublic<R, WBOUND>,
) -> GrafhenWord<WBOUND>
```

The `zero_cipher` must be drawn from a pre-distributed zero-cipher database
by the caller. **Freshness is the caller's responsibility**; reusing a zero
cipher leaks the plaintext even ignoring the IND-CPA break.

---

## ZK Architecture

GRAFHEN in Volar is used as a **ZK correctness layer**, not a confidentiality
primitive. The design proceeds as follows:

### What ZK Proves

The ZK proof (Quicksilver-style VOLEitH, via `weave_vole_prover` /
`weave_vole_verifier`) establishes:

> *Given public ciphertexts `ct_out` and inputs `ct_in_0, ..., ct_in_k`, the
> prover knows plaintexts `x_0, ..., x_k` such that evaluating the boolean
> circuit C on `(x_0, ..., x_k)` produces `x_out`, and the homomorphic
> computation C(ct_in_0, ..., ct_in_k) produces `ct_out`.*

This is a **correctness** guarantee: the circuit was evaluated honestly.
It is **not** a confidentiality guarantee — the ZK proof does not hide inputs.

### Two-Layer Structure

```
                 ┌───────────────────────────────────┐
                 │         ZK Layer (VOLEitH)         │
                 │  proves: C(ct_in) = ct_out          │
                 │  via: weave_vole_prover / verifier  │
                 └──────────────┬────────────────────┘
                                │
                 ┌──────────────▼────────────────────┐
                 │       GRAFHEN Layer               │
                 │  homomorphic evaluation of C       │
                 │  via: weave_grafhen               │
                 │  (correctness only; IND-CPA broken)│
                 └───────────────────────────────────┘
```

Privacy of the **inputs** is the responsibility of the key distribution
mechanism and the oracle-avoidance discipline described in
[grafhen-appsec.md](grafhen-appsec.md). The GRAFHEN scheme itself does not
provide ciphertext privacy.

### What This Buys

- **Computational integrity:** A dishonest evaluator cannot claim a false
  output without being detected by the ZK verifier.
- **Speed:** ~7.56 μs/AND homomorphic computation, plus the ZK overhead.
- **Multi-party encryption:** Any party with the public key can encrypt inputs
  without requiring the secret key or direct interaction with the keyholder.

### What This Does Not Buy

- **Ciphertext confidentiality:** Ciphertexts are distinguishable by the
  cross-reduction attack (see below). Do not rely on GRAFHEN to hide data.
- **Input privacy:** The ZK proof does not hide circuit inputs from the verifier.

---

## The IND-CPA Break

**Reference:** ePrint 2026/700, Geraud–Stewart, "Breaking GRAFHEN."

### Attack Summary

The semidirect product structure of the GRAFHEN ciphertext group produces
**factored ciphertexts**: each ciphertext `w` decomposes as `w = wA · wB`
where the A-part and B-part are independently readable. A cross-reduction
distinguisher:

1. Takes two ciphertexts `c₀` and `c₁` purportedly encrypting the same bit.
2. Applies a polynomial-time cross-reduction procedure to both.
3. Distinguishes `Enc(0)` from `Enc(1)` with approximately 0.48 information-
   theoretic advantage for the recommended parameters (N=11).

### Theorem 2 is Unconditional

The key result in ePrint 2026/700 is Theorem 2, which states:

> *No instantiation of a GRAFHEN-like scheme based on semidirect products of
> symmetric groups Sₙ can achieve IND-CPA security, for any N.*

This is a **structural impossibility**, not a parameter-specific attack.
Increasing N makes the attack *stronger* (higher δ), not weaker. There is no
patch within the semidirect product design space that restores security.

### Academic Dispute

The GRAFHEN authors published a response to ePrint 2026/700. The response
contests some claims but does not refute Theorem 2 as of the last review.
Volar's position: the unconditional structural result governs implementation
policy regardless of the ongoing dispute. The code is `@reliability: experimental`
and carries the IND-CPA broken warning.

### Consequence for Volar

GRAFHEN is retained in Volar at the experimental level because:

1. It is still useful as a ZK correctness layer (the break is about privacy,
   not about computational correctness of the homomorphic operations).
2. It is academically interesting as a fast, noise-free computation substrate.
3. It may be superseded by a repaired construction, at which point this file
   will become a rework predecessor per the reliability protocol.

If a future attack also breaks **correctness** (i.e., the homomorphic AND
gate can produce the wrong group element without detection), GRAFHEN will be
demoted to `@reliability: insecure`.

---

## Weaver Integration

The weaver pass (`crates/compiler/volar-weaver/src/grafhen.rs`) takes a
`BIrBlocks` circuit and emits an `IrModule` that evaluates it homomorphically:

```rust
// Word bound is baked in as a literal (e.g. 64):
pub fn weave_grafhen<P: Clone + Default>(
    circuit: &BIrBlocks<P>,
    name: &str,
    word_bound: usize,
    linkage: Option<&LinkageSystem>,
) -> IrModule
```

Generated function shape:

```rust
fn circuit_name_grafhen<R: WordReducer<64>>(
    pk: &GrafhenPublic<R, 64>,
    input_0: &GrafhenWord<64>,
    input_1: &GrafhenWord<64>,
    // ...
) -> GrafhenWord<64>
```

`WBOUND = 64` is the literal passed as `word_bound`. The caller instantiates
the function with a concrete `R: WordReducer<64>`.

Or-gates are expanded via De Morgan: `Or(a,b) → Not(And(Not(a), Not(b)))`.

---

## Reliability Markers

All GRAFHEN files carry:

```rust
// @reliability: experimental
// @experimental-status: design
// @ai: unreviewed
```

These files must be gated behind `volar_experimental` before any downstream
crate depends on them. See [reliability.md](reliability.md) for the
experimental promotion protocol.
