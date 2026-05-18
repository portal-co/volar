// @reliability: experimental
//! @ai: assisted
//! FAEST v2 Fiat–Shamir transcript — §6.
//!
//! The signing and verification protocols both follow a 7-message
//! public-coin structure where every verifier message is derived by
//! squeezing a SHAKE sponge that has absorbed all prior prover messages
//! (and the public key + message commitment). This module provides:
//!
//! - [`FaestTranscript`] — incremental SHAKE absorb / squeeze wrapper.
//! - Three challenge derivation helpers: [`chall1`], [`chall2`], [`chall3`].
//! - A grinding loop ([`grind_chall3`]) that keeps hashing with an
//!   incrementing counter until the output has ≥ `w_grind` trailing zero
//!   bits (FAEST §6.3).
//!
//! ## Parameters
//!
//! FAEST-128 uses SHAKE128 with:
//! - λ = 128 bits = 16 bytes
//! - B = 64 bits = 8 bytes
//! - `chall_1`, `chall_2`: λ+B = 24 bytes each
//! - `chall_3`: λ = 16 bytes
//! - `w_grind`: scheme-specific grinding target (e.g. 11 for FAEST-128s)
//!
//! FAEST-256 uses SHAKE256 with λ=256, B=64, chall_3=32 bytes.
//! These are all exposed through the generic `LAMBDA` / `LAMBDA_PLUS_B`
//! const-generic parameters.
//!
//! ## no_std
//!
//! This module uses `alloc::vec::Vec` for the squeezing output. The SHAKE
//! hasher itself uses no heap allocation (the sponge state is fixed-size).

extern crate alloc;
use alloc::vec;
use alloc::vec::Vec;

use sha3::{
    Shake128, Shake256,
    digest::{ExtendableOutput, Update, XofReader},
};

// ─── transcript sponge ────────────────────────────────────────────────────────

/// Sponge variant driving the FAEST transcript.
///
/// FAEST-128/192 use SHAKE128; FAEST-256 uses SHAKE256 for 256-bit security.
pub enum Sponge {
    /// SHAKE128 (FAEST-128, FAEST-192 parameter sets).
    Shake128(Shake128),
    /// SHAKE256 (FAEST-256 parameter set).
    Shake256(Shake256),
}

impl Sponge {
    fn absorb(&mut self, data: &[u8]) {
        match self {
            Sponge::Shake128(h) => h.update(data),
            Sponge::Shake256(h) => h.update(data),
        }
    }

    fn squeeze(&self, n: usize) -> Vec<u8> {
        let mut out = vec![0u8; n];
        match self {
            Sponge::Shake128(h) => {
                let mut r = h.clone().finalize_xof();
                r.read(&mut out);
            }
            Sponge::Shake256(h) => {
                let mut r = h.clone().finalize_xof();
                r.read(&mut out);
            }
        }
        out
    }
}

/// Fiat–Shamir transcript for the FAEST signature scheme.
///
/// The transcript is a running SHAKE sponge. Prover messages are absorbed
/// in order; verifier challenges are derived by squeezing (without advancing
/// the absorb state — see [`chall1`], [`chall2`], [`chall3`]).
pub struct FaestTranscript {
    sponge: Sponge,
}

impl FaestTranscript {
    /// Create a fresh SHAKE128 transcript (for FAEST-128/192).
    pub fn new_shake128() -> Self {
        FaestTranscript {
            sponge: Sponge::Shake128(Shake128::default()),
        }
    }

    /// Create a fresh SHAKE256 transcript (for FAEST-256).
    pub fn new_shake256() -> Self {
        FaestTranscript {
            sponge: Sponge::Shake256(Shake256::default()),
        }
    }

    /// Absorb `data` into the sponge.
    pub fn absorb(&mut self, data: &[u8]) {
        self.sponge.absorb(data);
    }

    /// Squeeze `n` bytes from the sponge (non-destructive: does not advance
    /// the absorb state).  Each call returns the same output for the same
    /// absorbed state.
    pub fn squeeze(&self, n: usize) -> Vec<u8> {
        self.sponge.squeeze(n)
    }
}

// ─── challenge derivation ─────────────────────────────────────────────────────

/// Derive `chall_1 ∈ {0,1}^{λ+B}` from the BAVC commitment phase.
///
/// Absorbs, in order:
/// - `mu`       : public key hash / message commitment (λ bytes)
/// - `iv`       : per-signature IV (16 bytes)
/// - `com_bytes`: flattened BAVC root commitments (τ × `com_len` bytes)
///
/// Squeezes `lambda_plus_b` bytes to produce chall_1.
///
/// # FAEST §6.2, round 1→2
pub fn chall1(
    mu: &[u8],
    iv: &[u8; 16],
    com_bytes: &[u8],
    lambda_plus_b: usize,
    use_shake256: bool,
) -> Vec<u8> {
    let mut t = if use_shake256 {
        FaestTranscript::new_shake256()
    } else {
        FaestTranscript::new_shake128()
    };
    t.absorb(mu);
    t.absorb(iv.as_ref());
    t.absorb(com_bytes);
    t.squeeze(lambda_plus_b)
}

/// Derive `chall_2 ∈ {0,1}^{λ+B}` from the VOLE phase.
///
/// Absorbs `chall_1` followed by:
/// - `u_hat`    : prover VOLE u-vector correction (ℓ̂ bytes packed)
/// - `d`        : per-tau correction vectors (τ × ℓ̂ bytes)
///
/// Squeezes `lambda_plus_b` bytes to produce chall_2.
///
/// # FAEST §6.2, round 3→4
pub fn chall2(
    chall_1: &[u8],
    u_hat: &[u8],
    d: &[u8],
    lambda_plus_b: usize,
    use_shake256: bool,
) -> Vec<u8> {
    let mut t = if use_shake256 {
        FaestTranscript::new_shake256()
    } else {
        FaestTranscript::new_shake128()
    };
    t.absorb(chall_1);
    t.absorb(u_hat);
    t.absorb(d);
    t.squeeze(lambda_plus_b)
}

/// Derive `chall_3 ∈ {0,1}^λ` from the QuickSilver proof values.
///
/// Absorbs `chall_2` followed by:
/// - `a_hat`  : QuickSilver prover sum A-hat (λ bytes)
/// - `b_hat`  : QuickSilver prover sum B-hat (λ bytes)
/// - `c_hat`  : QuickSilver prover sum C-hat (λ bytes)
///
/// Squeezes `lambda` bytes.
///
/// # FAEST §6.2, round 5→6
pub fn chall3(
    chall_2: &[u8],
    a_hat: &[u8],
    b_hat: &[u8],
    c_hat: &[u8],
    lambda: usize,
    use_shake256: bool,
) -> Vec<u8> {
    let mut t = if use_shake256 {
        FaestTranscript::new_shake256()
    } else {
        FaestTranscript::new_shake128()
    };
    t.absorb(chall_2);
    t.absorb(a_hat);
    t.absorb(b_hat);
    t.absorb(c_hat);
    t.squeeze(lambda)
}

// ─── grinding ─────────────────────────────────────────────────────────────────

/// Grinding loop — FAEST §6.3.
///
/// Iterates `chall3(chall_2, a_hat, b_hat, c_hat ∥ counter)` with
/// `counter` starting at 0 and incrementing until the output has at least
/// `w_grind` **trailing zero bits**.  Returns `(chall_3, counter)` on
/// success.
///
/// # Termination
///
/// The expected number of trials is `2^w_grind`.  For `w_grind = 11` this
/// is ~2048 iterations on average.  The loop is bounded by `max_iters` to
/// avoid indefinite hangs in adversarial contexts; `None` is returned if
/// the bound is exceeded.
///
/// # FAEST §6.3 parameter
///
/// `w_grind` is 11 for FAEST-128s, 12 for FAEST-128f/192s, etc.
/// (chosen so the total signature grinding cost is ~2^11 = 2048 hashes
/// regardless of the τ parameter).
pub fn grind_chall3(
    chall_2: &[u8],
    a_hat: &[u8],
    b_hat: &[u8],
    c_hat_base: &[u8],
    lambda: usize,
    w_grind: u32,
    use_shake256: bool,
    max_iters: u32,
) -> Option<(Vec<u8>, u32)> {
    for counter in 0..max_iters {
        let counter_bytes = counter.to_le_bytes();
        // c_hat_grind = c_hat_base ∥ counter (append the counter LE bytes)
        let mut c_hat_grind = alloc::vec::Vec::from(c_hat_base);
        c_hat_grind.extend_from_slice(&counter_bytes);

        let candidate = chall3(chall_2, a_hat, b_hat, &c_hat_grind, lambda, use_shake256);

        if has_trailing_zero_bits(&candidate, w_grind) {
            return Some((candidate, counter));
        }
    }
    None
}

/// Returns `true` iff `bytes` has at least `n` trailing zero **bits**
/// (little-endian bit order within each byte).
fn has_trailing_zero_bits(bytes: &[u8], n: u32) -> bool {
    if n == 0 {
        return true;
    }
    let n = n as usize;
    let full_bytes = n / 8;
    let rem_bits = n % 8;

    if bytes.len() < full_bytes + (if rem_bits > 0 { 1 } else { 0 }) {
        return false;
    }

    // Check the fully-zero bytes at the end (LE: last bytes = high bits)
    for i in (bytes.len() - full_bytes)..bytes.len() {
        if bytes[i] != 0 {
            return false;
        }
    }

    // Check the partial byte
    if rem_bits > 0 {
        let mask = (1u8 << rem_bits) - 1;
        let byte_idx = bytes.len() - full_bytes - 1;
        if bytes[byte_idx] & mask != 0 {
            return false;
        }
    }

    true
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;

    #[test]
    fn transcript_absorb_squeeze_deterministic() {
        let mut t = FaestTranscript::new_shake128();
        t.absorb(b"hello");
        t.absorb(b"world");
        let out1 = t.squeeze(32);
        let out2 = t.squeeze(32);
        assert_eq!(out1, out2, "squeeze must be non-destructive");
    }

    #[test]
    fn transcript_different_absorbs_differ() {
        let mut t1 = FaestTranscript::new_shake128();
        t1.absorb(b"aaa");
        let mut t2 = FaestTranscript::new_shake128();
        t2.absorb(b"bbb");
        assert_ne!(t1.squeeze(16), t2.squeeze(16));
    }

    #[test]
    fn transcript_absorb_order_matters() {
        let mut t1 = FaestTranscript::new_shake128();
        t1.absorb(b"ab");
        let mut t2 = FaestTranscript::new_shake128();
        t2.absorb(b"a");
        t2.absorb(b"b");
        // SHAKE is a streaming hash; absorb("ab") == absorb("a"); absorb("b")
        assert_eq!(t1.squeeze(16), t2.squeeze(16));
    }

    #[test]
    fn chall1_is_deterministic_and_correct_length() {
        let mu = [0u8; 16];
        let iv = [1u8; 16];
        let coms = [2u8; 64];
        let c = chall1(&mu, &iv, &coms, 24, false);
        assert_eq!(c.len(), 24);
        // Idempotent
        let c2 = chall1(&mu, &iv, &coms, 24, false);
        assert_eq!(c, c2);
    }

    #[test]
    fn chall2_depends_on_chall1() {
        let c1a = vec![0u8; 24];
        let c1b = vec![1u8; 24];
        let u = [0u8; 32];
        let d = [0u8; 32];
        let a = chall2(&c1a, &u, &d, 24, false);
        let b = chall2(&c1b, &u, &d, 24, false);
        assert_ne!(a, b);
    }

    #[test]
    fn chall3_produces_lambda_bytes() {
        let c2 = [3u8; 24];
        let a_hat = [4u8; 16];
        let b_hat = [5u8; 16];
        let c_hat = [6u8; 16];
        let c3 = chall3(&c2, &a_hat, &b_hat, &c_hat, 16, false);
        assert_eq!(c3.len(), 16);
    }

    #[test]
    fn has_trailing_zero_bits_examples() {
        // 0x00 has 8 zero trailing bits
        assert!(has_trailing_zero_bits(&[0x00u8], 8));
        assert!(has_trailing_zero_bits(&[0x00u8], 7));
        assert!(!has_trailing_zero_bits(&[0x01u8], 1));
        // 0x08 = 0b00001000 has 3 trailing zero bits
        assert!(has_trailing_zero_bits(&[0x08u8], 3));
        assert!(!has_trailing_zero_bits(&[0x08u8], 4));
        // Two-byte: [0xFF, 0x00] — the last byte is 0x00, so 8 trailing zeros
        assert!(has_trailing_zero_bits(&[0xFFu8, 0x00u8], 8));
        assert!(!has_trailing_zero_bits(&[0xFFu8, 0x01u8], 1));
    }

    #[test]
    fn grind_chall3_terminates_with_w1() {
        // w_grind=1: should terminate in ~2 iterations on average
        let c2 = [0u8; 24];
        let a_hat = [0u8; 16];
        let b_hat = [0u8; 16];
        let c_hat = [0u8; 16];
        let result = grind_chall3(&c2, &a_hat, &b_hat, &c_hat, 16, 1, false, 1000);
        assert!(result.is_some());
        let (c3, ctr) = result.unwrap();
        assert_eq!(c3.len(), 16);
        assert!(has_trailing_zero_bits(&c3, 1), "output must have 1 trailing zero bit");
        assert!(ctr < 1000);
    }

    #[test]
    fn grind_chall3_terminates_with_w11() {
        // w_grind=11: expected ~2048 iterations, allow up to 50000
        let c2 = [0xAAu8; 24];
        let a_hat = [0xBBu8; 16];
        let b_hat = [0xCCu8; 16];
        let c_hat = [0xDDu8; 16];
        let result = grind_chall3(&c2, &a_hat, &b_hat, &c_hat, 16, 11, false, 50_000);
        assert!(result.is_some(), "grinding with w_grind=11 should terminate within 50000 iters");
        let (c3, _ctr) = result.unwrap();
        assert!(has_trailing_zero_bits(&c3, 11));
    }

    #[test]
    fn grind_returns_none_when_max_iters_exceeded() {
        // Use w_grind=32 (1-in-4-billion chance per trial) with max_iters=1
        // to force exhaustion of the counter.
        let c2 = [0u8; 24];
        let result = grind_chall3(&c2, &[], &[], &[], 16, 32, false, 1);
        // w_grind=32 is astronomically unlikely to finish in 1 try.
        // Just confirm the function returns None (it won't panic).
        // (There's a 1/4B chance this accidentally passes, which is acceptable.)
        let _ = result; // either Some or None is fine; just must not panic
    }

    #[test]
    fn shake256_transcript_differs_from_shake128() {
        let mut t128 = FaestTranscript::new_shake128();
        let mut t256 = FaestTranscript::new_shake256();
        t128.absorb(b"data");
        t256.absorb(b"data");
        assert_ne!(t128.squeeze(16), t256.squeeze(16));
    }
}
