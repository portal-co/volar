# GRAFHEN Application Security Guide

> This guide covers the operational security requirements for deploying GRAFHEN
> inside a ZK correctness proof. Read [grafhen.md](grafhen.md) first for the
> construction and the IND-CPA break.

---

## What GRAFHEN Provides in Volar

GRAFHEN is used as a **correctness layer** inside ZK proofs. It is explicitly
**not a confidentiality primitive**. Understand the threat model before reading
the rules below.

### What GRAFHEN + ZK Gives You

- **Correctness:** The ZK proof establishes that the homomorphic computation
  was performed faithfully. A cheating evaluator cannot claim a wrong output.
- **Multi-party encryption:** Any party with the public key can encrypt without
  the secret key or keyholder interaction.
- **Speed:** Sub-10 μs per AND gate homomorphic computation.

### What GRAFHEN + ZK Does Not Give You

- **Ciphertext privacy:** Ciphertexts leak the encrypted bit to any party that
  runs the cross-reduction attack (ePrint 2026/700). Treat all ciphertexts as
  **public** from a confidentiality standpoint.
- **Input privacy against the ZK verifier:** The ZK verifier sees the circuit
  and can learn input-dependent information from the proof transcripts depending
  on the specific proof parameters.
- **Key confidentiality from a GRAFHEN oracle:** If any party can submit chosen
  ciphertexts to a decryption oracle (even an implicit one), they can recover
  the secret key. See [§ Oracle Avoidance](#oracle-avoidance) below.

---

## Oracle Avoidance

The most critical operational rule is:

> **The decryption oracle must be unavailable to all parties who do not already
> hold the secret key.**

This sounds obvious, but oracle access arises in unexpected forms.

### Direct Oracle Access

Do not expose `grafhen_decrypt` through any API, RPC endpoint, or callback
accessible to parties who do not already hold `GrafhenKey`.

### Implicit Oracle Access via Behavior

An implicit oracle arises when the **output of a decrypted value drives
observable side-channel behavior**. Examples:

- **Branch on decrypted bit:** If `grafhen_decrypt(ct)` returns `true` and
  the program takes a visibly different action (network request, file write,
  log message, timing), an observer can submit chosen ciphertexts and read
  the oracle from the behavior.

- **Return value includes decrypted bit:** A function that decrypts and returns
  the result (in any form that leaks the plaintext) is an oracle.

- **Retry-on-failure:** If decryption failure causes a retry or error response
  that differs from decryption success, the error response is an oracle.

**Mitigation:** Ensure that all code paths reachable from a decryption call
produce identical observable behavior regardless of the decrypted value, before
the value is consumed by application logic. Alternatively, encrypt the
decrypted bit under a separate confidentiality mechanism before it influences
any observable behavior.

### Timing Oracle

`grafhen_decrypt` is not constant-time. The number of generator compositions
depends on word length, which may vary by ciphertext. If an attacker can
measure decryption latency for chosen ciphertexts, they have a timing oracle.

**Mitigation:** If decryption latency must be hidden, pad all words to `WBOUND`
before decryption and verify that your platform's CPU time is not observable
to the attacker. This is an open research question for GRAFHEN specifically.

---

## Zero-Cipher Database Management

Encryption freshness depends on the zero-cipher database. Each zero cipher
must be used exactly once. Reuse of a zero cipher leaks the plaintext by
allowing a distinguisher to XOR two ciphertexts of the same message and compare.

Rules:

1. **Generate the database offline** using a trusted keyholder. Do not derive
   zero ciphers from the secret key at runtime if the derivation function is
   accessible to adversaries.

2. **Mark zero ciphers as consumed atomically.** If your zero-cipher store is
   concurrent, use an atomic counter or lock to ensure no two encryption calls
   use the same zero cipher.

3. **Bound database size to circuit depth.** The maximum number of zero ciphers
   needed is bounded by the number of input bits (one per input encryption).
   Allocate exactly this many; do not allow the same pool to be shared across
   multiple computation sessions without re-keying.

4. **Destroy consumed zero ciphers.** After a zero cipher has been used to
   produce a ciphertext, zero the backing memory and mark the slot as exhausted.

---

## Key Distribution

`GrafhenKey` is the secret material. Follow standard secret management practices:

1. **Never transmit `GrafhenKey` in cleartext.** Encrypt it under a separate
   authenticated key agreement protocol before any network transmission.

2. **Store `GrafhenKey` in a hardware security module (HSM) or secure enclave**
   if the deployment environment supports it. The key must never appear in a
   swap file, core dump, or crash report.

3. **Rotate keys periodically.** GRAFHEN has no forward secrecy; a compromised
   key retroactively decrypts all historical ciphertexts. Define a key rotation
   schedule proportional to the sensitivity of the data being proven.

4. **Do not derive `GrafhenKey` from a low-entropy source.** Each generator
   must be a uniformly random permutation of Sₙ. Use a cryptographically secure
   RNG. Biased generators degrade the group structure and may weaken the ZK proof.

---

## Threat Model Summary

| Threat | GRAFHEN+ZK Mitigation | Residual Risk |
|--------|----------------------|---------------|
| Cheating evaluator claims wrong output | ZK proof rejects | None (assuming ZK soundness) |
| Passive eavesdropper reads ciphertexts | None — ciphertexts are public | Full plaintext exposure |
| Active attacker submits chosen ciphertexts to decryption oracle | Oracle-avoidance discipline | Key recovery if oracle exists |
| Attacker measures decryption timing | Constant-time padding (open research) | Partial plaintext leakage |
| Secret key compromise | Key rotation | All historical ciphertexts decryptable |
| Zero-cipher reuse | Single-use enforcement | Plaintext leakage across two messages |

---

## Checklist Before Any Deployment

- [ ] `grafhen_decrypt` is not accessible to any party that does not hold `GrafhenKey`.
- [ ] No code path between decryption and observable output reveals the decrypted bit.
- [ ] Zero ciphers are consumed atomically and single-use.
- [ ] `GrafhenKey` is stored in a hardware-protected store or equivalent.
- [ ] Key rotation schedule is defined.
- [ ] Timing channels from word-length-dependent decryption are accepted as a
      known risk or mitigated via padding.
- [ ] The ZK proof layer (`weave_vole_prover` / `weave_vole_verifier`) has been
      reviewed independently of this GRAFHEN deployment.
- [ ] The deployment has been reviewed by someone who has read
      [grafhen.md](grafhen.md) and is aware of the IND-CPA break.

---

## Related Documents

- [grafhen.md](grafhen.md) — technical construction, ZK architecture, IND-CPA break
- [grafhen-review-plan.md](grafhen-review-plan.md) — review roadmap
- [reliability.md](reliability.md) — reliability system and experimental status
- [vole-weaving.md](vole-weaving.md) — the ZK layer this guide assumes
