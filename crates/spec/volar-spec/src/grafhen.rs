// @reliability: experimental
// @experimental-status: design
// @experimental-since: (not yet reviewed)
// @ai: unreviewed
//! GRAFHEN: GRoup-bAsed Fully Homomorphic Encryption without Noise.
//!
//! **WARNING: IND-CPA BROKEN.** See ePrint 2026/700 (Geraud–Stewart).
//! The semidirect product structure of the ciphertext group produces factored
//! ciphertexts whose A-part and B-part are independently readable. A
//! cross-reduction distinguisher runs in polynomial time with ~0.48
//! information-theoretic advantage for the recommended parameters, and
//! **Theorem 2 is unconditional** — it closes all semidirect product repairs.
//!
//! This implementation is suitable only as a **correctness layer** inside
//! ZK proofs (where the proof system guarantees computational integrity, not
//! ciphertext privacy) and for academic study. It MUST NOT be used as a
//! confidentiality primitive in any deployment.
//!
//! Based on ePrint 2025/1907. Plaintexts are bits in F₂ encoded as elements
//! of S₆ embedded in Sₙ (N ≥ 11 recommended for academic parameters).
//!
//! Ciphertexts are **words** over a set of group generators, represented as
//! bounded arrays of generator indices. The `WBOUND` const generic is a
//! caller-specified ceiling on word length; a runtime `assert!` enforces it
//! after each AND gate (the only length-increasing operation).
//!
//! # Public-key variant
//!
//! This module implements the **public-key** variant of GRAFHEN, where any
//! party can encrypt but only the keyholder can decrypt. Encryption requires
//! a fresh zero-ciphertext chosen by the caller from a pre-distributed
//! database; the keyholder's secret generators never leave the trusted party.
//!
//! # Word reduction
//!
//! The `WordReducer` trait is the hook for callers to provide a confluent
//! terminating rewriting system (e.g., the S₁₁ system from the paper) that
//! keeps word lengths bounded. `NoReduction` is provided for tests and toy
//! circuits where word lengths are statically known to stay within `WBOUND`.

// no_std: no Vec, no Box, no alloc.

// ============================================================================
// WordReducer trait
// ============================================================================

/// Caller-provided word-length reduction strategy.
///
/// Implementors apply a confluent terminating rewriting system over generator
/// indices (e.g., the S₁₁ system from ePrint 2025/1907) to bound the length
/// of a word after homomorphic AND operations.
///
/// The weaver enforces `WBOUND` via a runtime `assert!` after each AND gate;
/// this trait is the mechanism by which callers make that bound achievable.
///
/// # Safety contract
///
/// `reduce` must preserve the group element represented by `word`: the
/// permutation computed by evaluating `word` before and after reduction must
/// be identical. Violating this breaks decryption correctness.
pub trait WordReducer<const WBOUND: usize>: Clone {
    fn reduce(&self, word: &mut GrafhenWord<WBOUND>);
}

/// A no-op reducer — does not apply any rewriting system.
///
/// Suitable only for toy circuits where word lengths are statically known
/// to stay within `WBOUND` without reduction.
#[derive(Clone, Copy, Default)]
pub struct NoReduction;

impl<const WBOUND: usize> WordReducer<WBOUND> for NoReduction {
    #[inline]
    fn reduce(&self, _word: &mut GrafhenWord<WBOUND>) {}
}

// ============================================================================
// GrafhenWord
// ============================================================================

/// A bounded word over generator indices `0..2D` for group Sₙ.
///
/// Indices `0..D` refer to forward generators; indices `D..2D` refer to
/// their inverses. The `len` field is the logical length; `data[len..]` is
/// unused and may contain arbitrary bytes.
///
/// An empty word (`len == 0`) represents the identity permutation, i.e. Enc(0).
#[derive(Clone, Copy)]
pub struct GrafhenWord<const WBOUND: usize> {
    pub data: [u8; WBOUND],
    pub len: usize,
}

impl<const WBOUND: usize> GrafhenWord<WBOUND> {
    /// The identity / Enc(0) word (empty word).
    pub const fn identity() -> Self {
        GrafhenWord { data: [0u8; WBOUND], len: 0 }
    }
}

// ============================================================================
// GrafhenKey
// ============================================================================

/// GRAFHEN secret key: D generators of Sₙ stored as permutation arrays.
///
/// `gens[i]` is a permutation of `{0, …, N-1}` represented as a `u8` array.
/// `inv_gens[i]` is the group-theoretic inverse of `gens[i]`, stored
/// explicitly to avoid on-the-fly inversion during decryption.
///
/// Generator index `i` (0 ≤ i < D) maps to `gens[i]`.
/// Generator index `D + i` maps to `inv_gens[i]`.
///
/// **This is secret material.** Keep `GrafhenKey` on the trusted keyholder.
#[derive(Clone)]
pub struct GrafhenKey<const N: usize, const D: usize> {
    pub gens: [[u8; N]; D],
    pub inv_gens: [[u8; N]; D],
}

// ============================================================================
// GrafhenPublic
// ============================================================================

/// GRAFHEN public key.
///
/// Contains the ciphertext constants required for homomorphic operations.
/// The zero-cipher database is **not** stored here; callers must pass a
/// fresh zero-ciphertext to [`grafhen_encrypt`] on each call to ensure
/// ciphertext freshness and prevent replay.
#[derive(Clone)]
pub struct GrafhenPublic<R, const WBOUND: usize>
where
    R: WordReducer<WBOUND>,
{
    /// Canonical encryption of 1: the permutation (0 4)(2 3) expressed as a
    /// word over the key generators (0-indexed).
    pub enc_one: GrafhenWord<WBOUND>,
    /// First AND constant (called `a` in the 12-segment AND construction).
    pub and_w1: GrafhenWord<WBOUND>,
    /// Second AND constant (called `b` in the 12-segment AND construction).
    pub and_w2: GrafhenWord<WBOUND>,
    /// Caller-provided word-length reduction strategy.
    pub reducer: R,
}

// ============================================================================
// Core word operations
// ============================================================================

/// Concatenate two words, returning `None` if combined length exceeds `WBOUND`.
pub fn concat_words<const WBOUND: usize>(
    a: &GrafhenWord<WBOUND>,
    b: &GrafhenWord<WBOUND>,
) -> Option<GrafhenWord<WBOUND>> {
    let new_len = match a.len.checked_add(b.len) {
        Some(n) if n <= WBOUND => n,
        _ => return None,
    };
    let mut result = GrafhenWord::identity();
    result.data[..a.len].copy_from_slice(&a.data[..a.len]);
    result.data[a.len..new_len].copy_from_slice(&b.data[..b.len]);
    result.len = new_len;
    Some(result)
}

// ============================================================================
// Homomorphic operations
// ============================================================================

/// Enc(0) — the identity element (empty word).
#[inline]
pub fn grafhen_zero<const WBOUND: usize>() -> GrafhenWord<WBOUND> {
    GrafhenWord::identity()
}

/// Homomorphic XOR: word concatenation.
///
/// # Panics
/// Panics if the combined word length exceeds `WBOUND`.
pub fn grafhen_xor<const WBOUND: usize>(
    a: &GrafhenWord<WBOUND>,
    b: &GrafhenWord<WBOUND>,
) -> GrafhenWord<WBOUND> {
    concat_words(a, b).expect("grafhen_xor: combined word length exceeds WBOUND")
}

/// Homomorphic NOT: XOR with enc_one.
///
/// # Panics
/// Panics if the combined word length exceeds `WBOUND`.
pub fn grafhen_not<R, const WBOUND: usize>(
    a: &GrafhenWord<WBOUND>,
    pk: &GrafhenPublic<R, WBOUND>,
) -> GrafhenWord<WBOUND>
where
    R: WordReducer<WBOUND>,
{
    grafhen_xor(a, &pk.enc_one)
}

/// Homomorphic AND via 12-segment concatenation.
///
/// Computes the GRAFHEN AND gate:
/// ```text
/// result = w1 · a · w1 · w2 · b · w2 · w1 · a · w1 · w2 · b · w2
/// ```
/// where `w1 = enc_a`, `w2 = enc_b`, `a = pk.and_w1`, `b = pk.and_w2`.
///
/// The result is passed through `pk.reducer` to bound word length.
///
/// # Panics
/// Panics if the pre-reduction length exceeds `WBOUND`, or if the reducer
/// fails to bring the post-reduction length within `WBOUND`.
pub fn grafhen_and<R, const WBOUND: usize>(
    enc_a: &GrafhenWord<WBOUND>,
    enc_b: &GrafhenWord<WBOUND>,
    pk: &GrafhenPublic<R, WBOUND>,
) -> GrafhenWord<WBOUND>
where
    R: WordReducer<WBOUND>,
{
    let w1 = enc_a;
    let w2 = enc_b;
    let a = &pk.and_w1;
    let b = &pk.and_w2;

    // 12-segment concatenation: w1 a w1 w2 b w2 w1 a w1 w2 b w2
    let segs: [&GrafhenWord<WBOUND>; 12] = [w1, a, w1, w2, b, w2, w1, a, w1, w2, b, w2];
    let total_len: usize = segs.iter().map(|s| s.len).sum();
    assert!(
        total_len <= WBOUND,
        "grafhen_and: pre-reduction length {} exceeds WBOUND {}",
        total_len,
        WBOUND
    );

    let mut result = GrafhenWord::identity();
    let mut pos = 0usize;
    for seg in &segs {
        result.data[pos..pos + seg.len].copy_from_slice(&seg.data[..seg.len]);
        pos += seg.len;
    }
    result.len = total_len;

    pk.reducer.reduce(&mut result);
    assert!(
        result.len <= WBOUND,
        "grafhen_and: post-reduction length {} exceeds WBOUND {}; reducer must maintain bound",
        result.len,
        WBOUND
    );
    result
}

// ============================================================================
// Encryption / Decryption
// ============================================================================

/// Encrypt a single bit under the public key.
///
/// `zero_cipher` must be a fresh encryption of 0 (drawn from the
/// pre-distributed zero-cipher database by the caller). It is XOR'd with
/// `pk.enc_one` to produce an encryption of 1, or returned unchanged for 0.
///
/// **Freshness is the caller's responsibility.** Reusing zero ciphers leaks
/// the plaintext (even ignoring the IND-CPA break in GRAFHEN itself).
pub fn grafhen_encrypt<R, const WBOUND: usize>(
    bit: bool,
    zero_cipher: &GrafhenWord<WBOUND>,
    pk: &GrafhenPublic<R, WBOUND>,
) -> GrafhenWord<WBOUND>
where
    R: WordReducer<WBOUND>,
{
    if bit {
        grafhen_xor(zero_cipher, &pk.enc_one)
    } else {
        *zero_cipher
    }
}

/// Evaluate a word to a permutation by composing generators left-to-right.
///
/// Starting from the identity permutation, each generator index `g` in the
/// word is applied by left-composing:
/// ```text
/// perm ← gen_g ∘ perm    (i.e., perm[i] ← gen_g[perm[i]])
/// ```
///
/// Indices `0..D` select `key.gens`; indices `D..2D` select `key.inv_gens`.
///
/// # Panics
/// Panics if any generator index in the word is ≥ 2D.
fn eval_word_to_perm<const N: usize, const D: usize, const WBOUND: usize>(
    key: &GrafhenKey<N, D>,
    word: &GrafhenWord<WBOUND>,
) -> [u8; N] {
    let mut perm: [u8; N] = core::array::from_fn(|i| i as u8);
    for &g in &word.data[..word.len] {
        let g = g as usize;
        assert!(
            g < 2 * D,
            "eval_word_to_perm: generator index {} out of range (D={})",
            g,
            D
        );
        let generator: &[u8; N] = if g < D { &key.gens[g] } else { &key.inv_gens[g - D] };
        // Left-composition: new_perm[i] = generator[perm[i]]
        perm = core::array::from_fn(|i| generator[perm[i] as usize]);
    }
    perm
}

/// Decrypt a GRAFHEN ciphertext.
///
/// Evaluates the word to a permutation π by composing the secret generators,
/// then reads the output bit from `π[0]`:
/// - `π[0] == 0` → `Some(false)` (Enc(0))
/// - `π[0] == 4` → `Some(true)` (Enc(1))
/// - otherwise   → `None` (malformed ciphertext)
///
/// # Panics
/// Panics if any generator index in the word is ≥ 2D.
pub fn grafhen_decrypt<const N: usize, const D: usize, const WBOUND: usize>(
    key: &GrafhenKey<N, D>,
    word: &GrafhenWord<WBOUND>,
) -> Option<bool> {
    let perm = eval_word_to_perm(key, word);
    match perm[0] {
        0 => Some(false),
        4 => Some(true),
        _ => None,
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // Toy parameters: N=6 (S₆ is where the encoding lives), D=1.
    //
    // Generator 0: the permutation (0 4)(2 3) in 0-indexed notation.
    //   gens[0]     = [4, 1, 3, 2, 0, 5]   (maps 0↔4, 2↔3, fixes 1,5)
    //   inv_gens[0] = [4, 1, 3, 2, 0, 5]   (same — this permutation is an involution)
    //
    // enc_one = word [0] (apply generator 0 once):
    //   eval([0])[0] = gens[0][identity[0]] = gens[0][0] = 4 → decrypts to true ✓
    //
    // empty word (identity):
    //   eval([])[0] = identity[0] = 0 → decrypts to false ✓
    //
    // WBOUND = 64 is generous for these toy tests.
    const WBOUND: usize = 64;
    type Key = GrafhenKey<6, 1>;

    fn toy_key() -> Key {
        GrafhenKey {
            gens:     [[4u8, 1, 3, 2, 0, 5]],
            inv_gens: [[4u8, 1, 3, 2, 0, 5]],
        }
    }

    fn toy_enc_one() -> GrafhenWord<WBOUND> {
        let mut w = GrafhenWord::identity();
        w.data[0] = 0; // generator index 0
        w.len = 1;
        w
    }

    fn toy_pk() -> GrafhenPublic<NoReduction, WBOUND> {
        // For mechanical tests, and_w1 and and_w2 are the identity.
        // Correctness of AND gates requires algebraically derived constants
        // per ePrint 2025/1907; see docs/grafhen.md.
        GrafhenPublic {
            enc_one: toy_enc_one(),
            and_w1: GrafhenWord::identity(),
            and_w2: GrafhenWord::identity(),
            reducer: NoReduction,
        }
    }

    // ── Decryption of canonical encodings ────────────────────────────────────

    #[test]
    fn zero_decrypts_false() {
        let key = toy_key();
        let enc0: GrafhenWord<WBOUND> = grafhen_zero();
        assert_eq!(grafhen_decrypt(&key, &enc0), Some(false));
    }

    #[test]
    fn enc_one_decrypts_true() {
        let key = toy_key();
        let enc1 = toy_enc_one();
        assert_eq!(grafhen_decrypt(&key, &enc1), Some(true));
    }

    // ── XOR homomorphism ─────────────────────────────────────────────────────

    #[test]
    fn xor_zero_zero_is_zero() {
        let key = toy_key();
        let a: GrafhenWord<WBOUND> = grafhen_zero();
        let b: GrafhenWord<WBOUND> = grafhen_zero();
        let c = grafhen_xor(&a, &b);
        assert_eq!(grafhen_decrypt(&key, &c), Some(false));
    }

    #[test]
    fn xor_zero_one_is_one() {
        let key = toy_key();
        let a: GrafhenWord<WBOUND> = grafhen_zero();
        let b = toy_enc_one();
        let c = grafhen_xor(&a, &b);
        assert_eq!(grafhen_decrypt(&key, &c), Some(true));
    }

    #[test]
    fn xor_one_zero_is_one() {
        let key = toy_key();
        let a = toy_enc_one();
        let b: GrafhenWord<WBOUND> = grafhen_zero();
        let c = grafhen_xor(&a, &b);
        assert_eq!(grafhen_decrypt(&key, &c), Some(true));
    }

    #[test]
    fn xor_one_one_is_zero() {
        // (0 4)(2 3) composed with itself = identity.
        let key = toy_key();
        let a = toy_enc_one();
        let b = toy_enc_one();
        let c = grafhen_xor(&a, &b);
        assert_eq!(grafhen_decrypt(&key, &c), Some(false));
    }

    // ── NOT ──────────────────────────────────────────────────────────────────

    #[test]
    fn not_zero_is_one() {
        let key = toy_key();
        let pk = toy_pk();
        let a: GrafhenWord<WBOUND> = grafhen_zero();
        let b = grafhen_not(&a, &pk);
        assert_eq!(grafhen_decrypt(&key, &b), Some(true));
    }

    #[test]
    fn not_one_is_zero() {
        let key = toy_key();
        let pk = toy_pk();
        let a = toy_enc_one();
        let b = grafhen_not(&a, &pk);
        assert_eq!(grafhen_decrypt(&key, &b), Some(false));
    }

    // ── Encrypt / Decrypt roundtrip ───────────────────────────────────────────

    #[test]
    fn encrypt_false_decrypts_false() {
        let key = toy_key();
        let pk = toy_pk();
        let zero_cipher: GrafhenWord<WBOUND> = grafhen_zero();
        let ct = grafhen_encrypt(false, &zero_cipher, &pk);
        assert_eq!(grafhen_decrypt(&key, &ct), Some(false));
    }

    #[test]
    fn encrypt_true_decrypts_true() {
        let key = toy_key();
        let pk = toy_pk();
        let zero_cipher: GrafhenWord<WBOUND> = grafhen_zero();
        let ct = grafhen_encrypt(true, &zero_cipher, &pk);
        assert_eq!(grafhen_decrypt(&key, &ct), Some(true));
    }

    // ── AND mechanical (word operations, identity constants) ─────────────────
    // These tests verify that grafhen_and does not panic and that word lengths
    // are within WBOUND. Algebraic correctness of AND requires properly derived
    // and_w1/and_w2 constants per ePrint 2025/1907 (not tested here).

    #[test]
    fn and_does_not_panic_with_identity_constants() {
        let pk = toy_pk(); // and_w1 = and_w2 = identity (len=0)
        let a: GrafhenWord<WBOUND> = grafhen_zero();
        let b: GrafhenWord<WBOUND> = grafhen_zero();
        let _c = grafhen_and(&a, &b, &pk);
        // Total length = 12 * 0 = 0 — no panic.
    }

    #[test]
    fn and_with_unit_inputs_bounded() {
        let pk = toy_pk(); // and_w1 = and_w2 = identity (len=0)
        let a = toy_enc_one(); // len=1
        let b = toy_enc_one(); // len=1
        // Pre-reduction length = 4*1 + 4*1 + 2*0 + 2*0 = 8
        let c = grafhen_and(&a, &b, &pk);
        assert!(c.len <= WBOUND);
    }
}
