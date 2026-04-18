// @reliability: experimental
// @experimental-status: unreviewed
// @ai: assisted
//! TFHE (Torus Fully Homomorphic Encryption) — gate-bootstrapping implementation.
//!
//! This module implements TFHE over boolean circuits using the GINX blind-rotation
//! bootstrapping scheme (Chillotti et al., J. Cryptology 2020; Micciancio & Polyakov,
//! ePrint 2020/086).
//!
//! # Parameters
//!
//! Two ring dimensions are used:
//! - `N_LWE`: LWE secret-key dimension (small, e.g. 630).
//! - `BIG_N`: RLWE ring dimension (larger, e.g. 1024 — must be a power of two).
//!
//! Gadget decomposition parameters:
//! - `BS_ELL`: bootstrapping decomposition levels.
//! - `KS_ELL`: key-switching decomposition levels.
//!
//! # Gate Costs
//!
//! | Gate | Cost |
//! |------|------|
//! | XOR  | free (LWE addition) |
//! | NOT  | free (LWE negation + constant shift) |
//! | AND  | one gate bootstrapping (blind rotation + key switching) |
//!
//! # No-std
//!
//! This module is fully `#![no_std]` compatible. All arithmetic is performed
//! over fixed-size arrays using `u32` modular arithmetic (mod 2^32 ≈ torus).
//! Polynomial multiplication uses schoolbook O(N²) negacyclic convolution.
//!
//! # Security
//!
//! Unlike GRAFHEN, TFHE is a standard cryptographic scheme with published security
//! proofs. It is IND-CPA secure under the RLWE hardness assumption.
//!
//! Suggested 128-bit-security parameters (not validated by this codebase):
//! `N_LWE = 630, BIG_N = 1024, BS_ELL = 2, KS_ELL = 5, BS_BG_LOG = 10, KS_BG_LOG = 3`.
//!
//! # CAUTION
//!
//! This code has not been reviewed by a cryptographer. Do not use in production
//! without independent expert review.

use rand::{CryptoRng, Rng};

// ── Torus conventions ─────────────────────────────────────────────────────────
//
// We represent the torus T = ℝ/ℤ via u32, wrapping at 2^32.
// A quarter-turn is 2^30 (i.e. 0.25 · 2^32).
//
// Bit encoding:
//   0 → 0
//   1 → Q4 = 2^30
//
// LWE ciphertext for bit b under key s:
//   (a, β) where β = ⟨a, s⟩ + e + b · Q4  (all arithmetic mod 2^32)
//
// RLWE polynomial arithmetic is over Z[X] / (X^BIG_N + 1), coefficients mod 2^32.

const Q4: u32 = 1 << 30; // 2^30

// ── Public types ──────────────────────────────────────────────────────────────

/// An LWE ciphertext encrypting a single bit over the torus.
///
/// `a` is the mask vector; `b` is the body: `b = ⟨a, s⟩ + e + bit · Q4`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LweCiphertext<const N_LWE: usize> {
    pub a: [u32; N_LWE],
    pub b: u32,
}

/// An RLWE ciphertext over the ring Z[X]/(X^BIG_N+1).
///
/// `(a, b)` with `b ≈ a·s' + e + m` (polynomial multiplication mod X^BIG_N+1).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct RlweCiphertext<const BIG_N: usize> {
    pub a: [u32; BIG_N],
    pub b: [u32; BIG_N],
}

/// One row of an RGSW gadget matrix: a pair of RLWE ciphertexts encrypting
/// `0` and `0` respectively (with the message folded into the gadget structure).
#[derive(Clone, Copy, Debug)]
pub struct RgswRow<const BIG_N: usize> {
    /// Encrypts `0` in the "a" column (left RLWE pair).
    pub rlwe0: RlweCiphertext<BIG_N>,
    /// Encrypts `0` in the "b" column (right RLWE pair).
    pub rlwe1: RlweCiphertext<BIG_N>,
}

/// An RGSW ciphertext with `BS_ELL` decomposition levels.
///
/// The RGSW ciphertext of message `m` under RLWE secret key `s'` is:
/// `C = Z + m · G` where `G` is the `2 × 2·BS_ELL` gadget matrix and
/// `Z` is a matrix of RLWE encryptions of zero.
///
/// `rows[j]` holds the two RLWE ciphertexts for level `j`.
#[derive(Clone, Copy, Debug)]
pub struct RgswCiphertext<const BIG_N: usize, const BS_ELL: usize> {
    pub rows: [RgswRow<BIG_N>; BS_ELL],
}

/// The key-switching key: for each of the `BIG_N` RLWE-key bits `s'_i`,
/// `KS_ELL` LWE ciphertexts of `s'_i · ⌊2^32 / BG_KS^j⌋` under the LWE key.
#[derive(Clone, Debug)]
pub struct KeySwitchingKey<const N_LWE: usize, const BIG_N: usize, const KS_ELL: usize> {
    /// `ksk[i][j]` encrypts `s'[i] · floor(2^32 / BG_KS^{j+1})` under LWE key.
    pub ksk: [[LweCiphertext<N_LWE>; KS_ELL]; BIG_N],
    /// Log2 of the key-switching decomposition base.
    pub ks_bg_log: u32,
}

/// The complete bootstrapping key: BSK + KSK.
#[derive(Clone, Debug)]
pub struct BootstrappingKey<const N_LWE: usize, const BIG_N: usize, const BS_ELL: usize, const KS_ELL: usize> {
    /// Bootstrapping key: `bsk[i]` is an RGSW encryption of LWE key bit `s[i]` under RLWE key.
    pub bsk: [RgswCiphertext<BIG_N, BS_ELL>; N_LWE],
    /// Key-switching key: converts `LweCiphertext<BIG_N>` → `LweCiphertext<N_LWE>`.
    pub ksk: KeySwitchingKey<N_LWE, BIG_N, KS_ELL>,
    /// Log2 of the bootstrapping decomposition base.
    pub bs_bg_log: u32,
}

/// LWE secret key: a binary vector of length `N_LWE`.
#[derive(Clone, Debug)]
pub struct LweSecretKey<const N_LWE: usize> {
    pub key: [u8; N_LWE],
}

/// RLWE secret key: a binary polynomial of degree < `BIG_N`.
#[derive(Clone, Debug)]
pub struct RlweSecretKey<const BIG_N: usize> {
    pub key: [u32; BIG_N],
}

// ── Trivial / free gates ──────────────────────────────────────────────────────

/// Return a trivial encryption of `0` (mask = 0, body = 0).
pub fn tfhe_trivial_zero<const N_LWE: usize>() -> LweCiphertext<N_LWE> {
    LweCiphertext { a: [0u32; N_LWE], b: 0 }
}

/// Return a trivial encryption of `1` (mask = 0, body = Q4 = 2^30).
pub fn tfhe_trivial_one<const N_LWE: usize>() -> LweCiphertext<N_LWE> {
    LweCiphertext { a: [0u32; N_LWE], b: Q4 }
}

/// XOR gate — free: `ct_xor = ct_a + ct_b` (componentwise wrapping addition).
pub fn tfhe_xor<const N_LWE: usize>(
    a: LweCiphertext<N_LWE>,
    b: LweCiphertext<N_LWE>,
) -> LweCiphertext<N_LWE> {
    let mut out_a = [0u32; N_LWE];
    for i in 0..N_LWE {
        out_a[i] = a.a[i].wrapping_add(b.a[i]);
    }
    LweCiphertext { a: out_a, b: a.b.wrapping_add(b.b) }
}

/// NOT gate — free: `ct_not = -ct_a + Q4` (negate all components, shift body).
pub fn tfhe_not<const N_LWE: usize>(a: LweCiphertext<N_LWE>) -> LweCiphertext<N_LWE> {
    let mut out_a = [0u32; N_LWE];
    for i in 0..N_LWE {
        out_a[i] = a.a[i].wrapping_neg();
    }
    LweCiphertext { a: out_a, b: Q4.wrapping_sub(a.b) }
}

// ── Gate bootstrapping — AND ──────────────────────────────────────────────────

/// AND gate via full GINX gate bootstrapping.
///
/// Computes `a AND b` by:
/// 1. Pre-gate: `ct = ct_a + ct_b − Q4/2` (shifts combined ciphertext).
/// 2. Blind rotation (GINX): evaluates the sign function via RLWE accumulator.
/// 3. Sample extraction: extracts `LweCiphertext<BIG_N>` from accumulator.
/// 4. Key switching: converts `LweCiphertext<BIG_N>` → `LweCiphertext<N_LWE>`.
///
/// # Note on complexity
/// Schoolbook polynomial multiplication is O(BIG_N²) per CMUX step, and there
/// are `N_LWE` CMUX steps per call. This is a reference implementation only.
pub fn tfhe_gate_bootstrapping_and<
    const N_LWE: usize,
    const BIG_N: usize,
    const BS_ELL: usize,
    const KS_ELL: usize,
>(
    ct_a: LweCiphertext<N_LWE>,
    ct_b: LweCiphertext<N_LWE>,
    bk: &BootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL>,
) -> LweCiphertext<N_LWE> {
    // Step 1: combine inputs; subtract Q4/2 = 2^29 to center AND gate threshold
    let mut ct = lwe_add(ct_a, ct_b);
    ct.b = ct.b.wrapping_sub(Q4 >> 1); // subtract q/8

    // Step 2: blind rotate — produces RLWE accumulator
    let acc = blind_rotate(&ct, bk);

    // Step 3: sample extract from accumulator → LweCiphertext<BIG_N>
    let lwe_big = sample_extract(&acc);

    // Step 4: key switch → LweCiphertext<N_LWE>
    key_switch(&lwe_big, &bk.ksk)
}

/// CMUX (controlled multiplexer) gate.
///
/// Computes `if sel { a } else { b }` obliviously using only LWE arithmetic
/// and one gate bootstrapping.
///
/// Formula: `CMUX(sel, a, b) = b XOR (sel AND (a XOR b))`
///
/// - `sel = 0` (encrypts false): returns `b`
/// - `sel = 1` (encrypts true): returns `a`
///
/// Cost: one AND gate (one full gate bootstrapping round).
pub fn tfhe_cmux<
    const N_LWE: usize,
    const BIG_N: usize,
    const BS_ELL: usize,
    const KS_ELL: usize,
>(
    sel: LweCiphertext<N_LWE>,
    a: LweCiphertext<N_LWE>,
    b: LweCiphertext<N_LWE>,
    bk: &BootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL>,
) -> LweCiphertext<N_LWE> {
    let a_xor_b = tfhe_xor(a, b);
    let masked = tfhe_gate_bootstrapping_and(sel, a_xor_b, bk);
    tfhe_xor(b, masked)
}

// ── Encryption / Decryption (for keygen and tests) ───────────────────────────

/// Encrypt a bit `m ∈ {false, true}` under LWE secret key `sk`.
pub fn lwe_encrypt<const N_LWE: usize, R: Rng + CryptoRng>(
    m: bool,
    sk: &LweSecretKey<N_LWE>,
    noise_bits: u32,
    rng: &mut R,
) -> LweCiphertext<N_LWE> {
    let mut a = [0u32; N_LWE];
    for ai in a.iter_mut() {
        *ai = rng.random();
    }
    // dot product ⟨a, s⟩
    let mut dot: u32 = 0;
    for i in 0..N_LWE {
        dot = dot.wrapping_add(a[i].wrapping_mul(sk.key[i] as u32));
    }
    // noise: small signed value in [−2^noise_bits, 2^noise_bits)
    let e: u32 = small_noise(noise_bits, rng);
    let msg = if m { Q4 } else { 0u32 };
    let b = dot.wrapping_add(e).wrapping_add(msg);
    LweCiphertext { a, b }
}

/// Decrypt an LWE ciphertext. Returns `true` if decoded bit is 1.
pub fn lwe_decrypt<const N_LWE: usize>(
    ct: &LweCiphertext<N_LWE>,
    sk: &LweSecretKey<N_LWE>,
) -> bool {
    // phase = b - ⟨a, s⟩
    let mut dot: u32 = 0;
    for i in 0..N_LWE {
        dot = dot.wrapping_add(ct.a[i].wrapping_mul(sk.key[i] as u32));
    }
    let phase = ct.b.wrapping_sub(dot);
    // Round to nearest of {0, Q4}; threshold at Q4/2 = 2^29
    let half = Q4 >> 1;
    // distance to 0: min(phase, 2^32 - phase) ... distance to Q4: min(|phase - Q4|, ...)
    // Simpler: if phase is in [3*Q4/4 .. Q4/4] (mod 2^32) → 1 else 0
    // i.e. if (phase + Q4/2) mod 2^32 < Q4 → 1
    let shifted = phase.wrapping_add(half);
    shifted < Q4
}

// ── Key generation ────────────────────────────────────────────────────────────

/// Generate a random binary LWE secret key.
pub fn gen_lwe_secret_key<const N_LWE: usize, R: Rng + CryptoRng>(
    rng: &mut R,
) -> LweSecretKey<N_LWE> {
    let mut key = [0u8; N_LWE];
    for k in key.iter_mut() {
        *k = (rng.random::<u8>() & 1) as u8;
    }
    LweSecretKey { key }
}

/// Generate a random binary RLWE secret key polynomial.
pub fn gen_rlwe_secret_key<const BIG_N: usize, R: Rng + CryptoRng>(
    rng: &mut R,
) -> RlweSecretKey<BIG_N> {
    let mut key = [0u32; BIG_N];
    for k in key.iter_mut() {
        *k = (rng.random::<u8>() & 1) as u32;
    }
    RlweSecretKey { key }
}

/// Generate the full bootstrapping key (BSK + KSK).
///
/// # Parameters
/// - `lwe_sk`: the LWE secret key (binary, dimension N_LWE)
/// - `rlwe_sk`: the RLWE secret key (binary polynomial, degree < BIG_N)
/// - `bs_bg_log`: log2 of bootstrapping gadget base (e.g. 10 → base 1024)
/// - `ks_bg_log`: log2 of key-switching gadget base (e.g. 3 → base 8)
/// - `bs_noise_bits`: noise level for BSK RLWE encryptions
/// - `ks_noise_bits`: noise level for KSK LWE encryptions
pub fn gen_bootstrapping_key<
    const N_LWE: usize,
    const BIG_N: usize,
    const BS_ELL: usize,
    const KS_ELL: usize,
    R: Rng + CryptoRng,
>(
    lwe_sk: &LweSecretKey<N_LWE>,
    rlwe_sk: &RlweSecretKey<BIG_N>,
    bs_bg_log: u32,
    ks_bg_log: u32,
    bs_noise_bits: u32,
    ks_noise_bits: u32,
    rng: &mut R,
) -> BootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL> {
    // Build BSK: for each LWE key bit, encrypt it as RGSW under the RLWE key
    let bsk = core::array::from_fn(|i| {
        let bit = lwe_sk.key[i] != 0;
        rgsw_encrypt(bit, rlwe_sk, bs_bg_log, bs_noise_bits, rng)
    });

    // Build KSK: for each RLWE key bit s'[i], encrypt s'[i] * floor(2^32/BG_KS^j) under LWE key
    // We create a temporary LWE-style key from the RLWE key bits for this purpose
    let rlwe_as_lwe = LweSecretKey::<BIG_N> {
        key: core::array::from_fn(|i| rlwe_sk.key[i] as u8),
    };

    let ksk_array: [[LweCiphertext<N_LWE>; KS_ELL]; BIG_N] =
        core::array::from_fn(|i| {
            let s_bit = rlwe_sk.key[i];
            core::array::from_fn(|j| {
                // level j: encrypt s'[i] * floor(2^32 / BG_KS^{j+1})
                // = s'[i] * 2^{32 - ks_bg_log*(j+1)}
                let shift = 32u32.saturating_sub(ks_bg_log.saturating_mul(j as u32 + 1));
                let msg_val = s_bit.wrapping_shl(shift);
                // Encrypt as LWE with message on the body directly (no Q4 encoding)
                lwe_encrypt_raw(msg_val, lwe_sk, ks_noise_bits, rng)
            })
        });
    let _ = rlwe_as_lwe; // suppress unused warning

    let ksk = KeySwitchingKey {
        ksk: ksk_array,
        ks_bg_log,
    };

    BootstrappingKey { bsk, ksk, bs_bg_log }
}

// ── Internal: RLWE encryption ─────────────────────────────────────────────────

/// Encrypt a scalar message `m` as an RLWE ciphertext under `rlwe_sk`.
/// The message is placed in the constant coefficient of the `b` polynomial.
fn rlwe_encrypt_scalar<const BIG_N: usize, R: Rng + CryptoRng>(
    m: u32,
    sk: &RlweSecretKey<BIG_N>,
    noise_bits: u32,
    rng: &mut R,
) -> RlweCiphertext<BIG_N> {
    let a: [u32; BIG_N] = core::array::from_fn(|_| rng.random());
    // b = a * s + e + m (constant term)
    let mut b = poly_mul_neg(&a, &sk.key);
    b[0] = b[0].wrapping_add(small_noise(noise_bits, rng)).wrapping_add(m);
    RlweCiphertext { a, b }
}

/// Encrypt a polynomial message `msg_poly` as RLWE.
#[allow(dead_code)]
fn rlwe_encrypt_poly<const BIG_N: usize, R: Rng + CryptoRng>(
    msg_poly: &[u32; BIG_N],
    sk: &RlweSecretKey<BIG_N>,
    noise_bits: u32,
    rng: &mut R,
) -> RlweCiphertext<BIG_N> {
    let a: [u32; BIG_N] = core::array::from_fn(|_| rng.random());
    let mut b = poly_mul_neg(&a, &sk.key);
    for i in 0..BIG_N {
        b[i] = b[i]
            .wrapping_add(small_noise(noise_bits, rng))
            .wrapping_add(msg_poly[i]);
    }
    RlweCiphertext { a, b }
}

// ── Internal: RGSW encryption ─────────────────────────────────────────────────

/// Encrypt bit `m` as an RGSW ciphertext under `rlwe_sk`.
///
/// Construction: `C = Z + m · G` where `G` is the 2×2·ELL gadget matrix
/// and `Z` is a matrix of RLWE encryptions of zero.
///
/// Each row `j` of the gadget has base-level factor `Bg^j = 2^{bg_log * (j+1)}`.
/// The two RLWE ciphertexts in each row encrypt:
///   - `rlwe0`: `0 + m · Bg^{-(j+1)} · q`   (affects `a` column)
///   - `rlwe1`: `0 + m · Bg^{-(j+1)} · q`   (affects `b` column)
///
/// In standard RGSW the message is embedded in both rows of the `G` factor.
/// We use the GINX representation: one RGSW per LWE key bit.
fn rgsw_encrypt<const BIG_N: usize, const BS_ELL: usize, R: Rng + CryptoRng>(
    m: bool,
    sk: &RlweSecretKey<BIG_N>,
    bs_bg_log: u32,
    noise_bits: u32,
    rng: &mut R,
) -> RgswCiphertext<BIG_N, BS_ELL> {
    let msg_bit = if m { 1u32 } else { 0u32 };

    let rows = core::array::from_fn(|j| {
        // Gadget factor at level j: 2^{32 - bs_bg_log*(j+1)}
        let shift = 32u32.saturating_sub(bs_bg_log.saturating_mul(j as u32 + 1));
        let g_factor = 1u32.wrapping_shl(shift);
        let contrib = msg_bit.wrapping_mul(g_factor);

        // rlwe0 encrypts: constant polynomial with value contrib in const term
        let rlwe0 = rlwe_encrypt_scalar(contrib, sk, noise_bits, rng);

        // rlwe1 encrypts: same value (GINX variant — both rows get the message)
        let rlwe1 = rlwe_encrypt_scalar(contrib, sk, noise_bits, rng);

        RgswRow { rlwe0, rlwe1 }
    });

    RgswCiphertext { rows }
}

// ── Internal: blind rotation (GINX) ──────────────────────────────────────────

/// Perform GINX blind rotation.
///
/// Given LWE ciphertext `ct = (a, b)`, computes:
///   `acc_out = X^{-b̃} · ∏_{i=0}^{N_LWE-1} CMUX(BSK[i], acc · X^{ã[i]}, acc)`
///
/// where ã[i] = round(a[i] · 2N / 2^32) and b̃ = round(b · 2N / 2^32).
///
/// The initial accumulator is set to the AND gate test polynomial:
/// encodes `Q4` in positions [0, N/2) and `-Q4` in positions [N/2, N).
fn blind_rotate<
    const N_LWE: usize,
    const BIG_N: usize,
    const BS_ELL: usize,
    const KS_ELL: usize,
>(
    ct: &LweCiphertext<N_LWE>,
    bk: &BootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL>,
) -> RlweCiphertext<BIG_N> {
    // Initial trivial RLWE accumulator with test polynomial for AND gate.
    // Test polynomial: v[k] = Q4 for k in [0, N/2), -Q4 for k in [N/2, N).
    let mut v = [0u32; BIG_N];
    for k in 0..BIG_N / 2 {
        v[k] = Q4;
    }
    for k in BIG_N / 2..BIG_N {
        v[k] = Q4.wrapping_neg();
    }

    // Trivial RLWE encryption of v (a = 0, b = v)
    let mut acc = RlweCiphertext {
        a: [0u32; BIG_N],
        b: v,
    };

    // Scale factor: map u32 torus → ring exponent in [0, 2N)
    // exponent = round(x · 2N / 2^32) = x >> (32 - log2(2N))
    let two_n = 2 * BIG_N;
    let log2_two_n = two_n.trailing_zeros(); // only correct if two_n is power-of-two
    let scale_shift = 32u32.saturating_sub(log2_two_n);

    // Rotate accumulator by -b̃ (negate the body shift)
    let b_exp = torus_to_exp(ct.b, scale_shift, two_n);
    if b_exp != 0 {
        acc = rlwe_rotate(&acc, two_n - b_exp);
    }

    // CMUX loop over N_LWE key bits
    for i in 0..N_LWE {
        let a_exp = torus_to_exp(ct.a[i], scale_shift, two_n);
        if a_exp != 0 {
            // d1 = acc * X^{a_exp}
            let acc_rotated = rlwe_rotate(&acc, a_exp);
            // CMUX(BSK[i], d1, d0=acc)
            acc = cmux(&bk.bsk[i], &acc_rotated, &acc, bk.bs_bg_log);
        }
        // If a_exp == 0, CMUX(BSK[i], acc, acc) = acc, so we skip it.
    }

    acc
}

/// Convert a torus element (u32) to a ring exponent in [0, 2N).
#[inline]
fn torus_to_exp(x: u32, scale_shift: u32, two_n: usize) -> usize {
    // Round: x >> scale_shift, then mod 2N
    let exp = (x >> scale_shift) as usize;
    exp & (two_n - 1) // two_n is power-of-two
}

// ── Internal: sample extraction ───────────────────────────────────────────────

/// Extract an `LweCiphertext<BIG_N>` from the constant coefficient of an RLWE ciphertext.
///
/// If `(a(X), b(X))` is an RLWE ciphertext, the extracted LWE ciphertext is:
///   `a_lwe[i] = a[i]` (for i > 0, negated and wrapped for the negacyclic property),
///   `b_lwe = b[0]`.
fn sample_extract<const BIG_N: usize>(rlwe: &RlweCiphertext<BIG_N>) -> LweCiphertext<BIG_N> {
    let mut a_lwe = [0u32; BIG_N];
    a_lwe[0] = rlwe.a[0];
    for i in 1..BIG_N {
        // Coefficient i of a(X) contributes negatively to the i-th LWE key element
        a_lwe[i] = rlwe.a[BIG_N - i].wrapping_neg();
    }
    LweCiphertext { a: a_lwe, b: rlwe.b[0] }
}

// ── Internal: key switching ───────────────────────────────────────────────────

/// Convert `LweCiphertext<BIG_N>` → `LweCiphertext<N_LWE>` using the KSK.
///
/// Algorithm:
/// 1. Start with trivial ciphertext `(0, b)`.
/// 2. For each of the BIG_N components `a[i]`:
///    - Decompose `a[i]` into KS_ELL digits with base `2^ks_bg_log`.
///    - Subtract `digit[j] · KSK[i][j]` from the running sum.
fn key_switch<const N_LWE: usize, const BIG_N: usize, const KS_ELL: usize>(
    ct_big: &LweCiphertext<BIG_N>,
    ksk: &KeySwitchingKey<N_LWE, BIG_N, KS_ELL>,
) -> LweCiphertext<N_LWE> {
    let mut out_a = [0u32; N_LWE];
    let mut out_b = ct_big.b;

    for i in 0..BIG_N {
        let digits = ks_decompose::<KS_ELL>(ct_big.a[i], ksk.ks_bg_log);
        for j in 0..KS_ELL {
            let d = digits[j] as u32;
            if d == 0 {
                continue;
            }
            let ksk_ct = &ksk.ksk[i][j];
            // Subtract d · KSK[i][j]
            for k in 0..N_LWE {
                out_a[k] = out_a[k].wrapping_sub(d.wrapping_mul(ksk_ct.a[k]));
            }
            out_b = out_b.wrapping_sub(d.wrapping_mul(ksk_ct.b));
        }
    }

    LweCiphertext { a: out_a, b: out_b }
}

/// Signed decomposition for key switching.
///
/// Decomposes `x` into `KS_ELL` unsigned digits in base `2^bg_log`, rounding
/// the most-significant unrepresented portion.
fn ks_decompose<const KS_ELL: usize>(x: u32, bg_log: u32) -> [u32; KS_ELL] {
    let bg = 1u64 << bg_log;
    let mask = bg - 1;
    let mut rem = x as u64;
    // Round up the truncated tail
    let tail_shift = 32u32.saturating_sub(bg_log.saturating_mul(KS_ELL as u32));
    if tail_shift < 32 {
        let half_tail = 1u64 << (tail_shift.saturating_sub(1));
        rem = rem.wrapping_add(half_tail);
    }
    let mut digits = [0u32; KS_ELL];
    for j in (0..KS_ELL).rev() {
        let shift = bg_log.saturating_mul(j as u32 + 1);
        if shift < 64 {
            digits[j] = ((rem >> shift) & mask) as u32;
        }
    }
    digits
}

// ── Internal: CMUX and external product ──────────────────────────────────────

/// CMUX gate: `CMUX(C, d1, d0) = d0 + C ⊡ (d1 − d0)`.
fn cmux<const BIG_N: usize, const BS_ELL: usize>(
    c: &RgswCiphertext<BIG_N, BS_ELL>,
    d1: &RlweCiphertext<BIG_N>,
    d0: &RlweCiphertext<BIG_N>,
    bs_bg_log: u32,
) -> RlweCiphertext<BIG_N> {
    let diff = rlwe_sub(d1, d0);
    let prod = external_product(c, &diff, bs_bg_log);
    rlwe_add(d0, &prod)
}

/// External product: `RGSW ⊡ RLWE → RLWE`.
///
/// Decomposes each coefficient of the RLWE ciphertext using the gadget
/// decomposition, then multiplies by the corresponding RGSW rows.
fn external_product<const BIG_N: usize, const BS_ELL: usize>(
    rgsw: &RgswCiphertext<BIG_N, BS_ELL>,
    rlwe: &RlweCiphertext<BIG_N>,
    bs_bg_log: u32,
) -> RlweCiphertext<BIG_N> {
    // Decompose a and b polynomials of the RLWE ciphertext
    let a_decomp = poly_decompose::<BIG_N, BS_ELL>(&rlwe.a, bs_bg_log);
    let b_decomp = poly_decompose::<BIG_N, BS_ELL>(&rlwe.b, bs_bg_log);

    let mut out_a = [0u32; BIG_N];
    let mut out_b = [0u32; BIG_N];

    for j in 0..BS_ELL {
        // Each row contributes: a_j * row.rlwe0 + b_j * row.rlwe1
        let row = &rgsw.rows[j];

        let prod_a0 = poly_mul_neg(&a_decomp[j], &row.rlwe0.a);
        let prod_a1 = poly_mul_neg(&a_decomp[j], &row.rlwe0.b);
        let prod_b0 = poly_mul_neg(&b_decomp[j], &row.rlwe1.a);
        let prod_b1 = poly_mul_neg(&b_decomp[j], &row.rlwe1.b);

        for k in 0..BIG_N {
            out_a[k] = out_a[k]
                .wrapping_add(prod_a0[k])
                .wrapping_add(prod_b0[k]);
            out_b[k] = out_b[k]
                .wrapping_add(prod_a1[k])
                .wrapping_add(prod_b1[k]);
        }
    }

    RlweCiphertext { a: out_a, b: out_b }
}

/// Decompose a polynomial coefficient-wise into `BS_ELL` levels.
///
/// Returns `[p_0, …, p_{ELL-1}]` where `p_j[i]` is the j-th digit of
/// coefficient `i` in base `2^bs_bg_log`.
fn poly_decompose<const BIG_N: usize, const BS_ELL: usize>(
    p: &[u32; BIG_N],
    bg_log: u32,
) -> [[u32; BIG_N]; BS_ELL] {
    let mut result = [[0u32; BIG_N]; BS_ELL];
    let bg = 1u64 << bg_log;
    let mask = bg - 1;
    for i in 0..BIG_N {
        let mut x = p[i] as u64;
        // Round: add half of the least-significant non-represented bit
        let tail_shift = 32u32.saturating_sub(bg_log.saturating_mul(BS_ELL as u32));
        if tail_shift < 32 {
            x = x.wrapping_add(1u64 << tail_shift.saturating_sub(1));
        }
        for j in (0..BS_ELL).rev() {
            let shift = bg_log.saturating_mul(j as u32 + 1);
            result[j][i] = if shift < 64 {
                ((x >> shift) & mask) as u32
            } else {
                0
            };
        }
    }
    result
}

// ── Internal: RLWE arithmetic ─────────────────────────────────────────────────

/// Add two RLWE ciphertexts componentwise.
#[inline]
fn rlwe_add<const BIG_N: usize>(
    a: &RlweCiphertext<BIG_N>,
    b: &RlweCiphertext<BIG_N>,
) -> RlweCiphertext<BIG_N> {
    RlweCiphertext {
        a: poly_add_neg(&a.a, &b.a),
        b: poly_add_neg(&a.b, &b.b),
    }
}

/// Subtract two RLWE ciphertexts componentwise.
#[inline]
fn rlwe_sub<const BIG_N: usize>(
    a: &RlweCiphertext<BIG_N>,
    b: &RlweCiphertext<BIG_N>,
) -> RlweCiphertext<BIG_N> {
    RlweCiphertext {
        a: poly_sub_neg(&a.a, &b.a),
        b: poly_sub_neg(&a.b, &b.b),
    }
}

/// Rotate an RLWE ciphertext by multiplying by `X^exp` (mod X^BIG_N + 1).
#[inline]
fn rlwe_rotate<const BIG_N: usize>(
    ct: &RlweCiphertext<BIG_N>,
    exp: usize,
) -> RlweCiphertext<BIG_N> {
    RlweCiphertext {
        a: poly_rotate(&ct.a, exp),
        b: poly_rotate(&ct.b, exp),
    }
}

// ── Internal: LWE arithmetic ──────────────────────────────────────────────────

#[inline]
fn lwe_add<const N_LWE: usize>(
    a: LweCiphertext<N_LWE>,
    b: LweCiphertext<N_LWE>,
) -> LweCiphertext<N_LWE> {
    let mut out_a = [0u32; N_LWE];
    for i in 0..N_LWE {
        out_a[i] = a.a[i].wrapping_add(b.a[i]);
    }
    LweCiphertext { a: out_a, b: a.b.wrapping_add(b.b) }
}

/// Encrypt `msg` directly on the body (no Q4 encoding) — used for KSK construction.
fn lwe_encrypt_raw<const N_LWE: usize, R: Rng + CryptoRng>(
    msg: u32,
    sk: &LweSecretKey<N_LWE>,
    noise_bits: u32,
    rng: &mut R,
) -> LweCiphertext<N_LWE> {
    let mut a = [0u32; N_LWE];
    for ai in a.iter_mut() {
        *ai = rng.random();
    }
    let mut dot: u32 = 0;
    for i in 0..N_LWE {
        dot = dot.wrapping_add(a[i].wrapping_mul(sk.key[i] as u32));
    }
    let e = small_noise(noise_bits, rng);
    let b = dot.wrapping_add(e).wrapping_add(msg);
    LweCiphertext { a, b }
}

// ── Internal: polynomial arithmetic over Z[X]/(X^N+1) ────────────────────────

/// Multiply two polynomials mod (X^N + 1) using schoolbook convolution.
///
/// Negacyclic: coefficients that wrap beyond degree N−1 are subtracted (not added).
///
/// O(N²) — reference quality only.
fn poly_mul_neg<const N: usize>(a: &[u32; N], b: &[u32; N]) -> [u32; N] {
    let mut result = [0u32; N];
    for i in 0..N {
        for j in 0..N {
            let deg = i + j;
            if deg < N {
                result[deg] = result[deg].wrapping_add(a[i].wrapping_mul(b[j]));
            } else {
                result[deg - N] = result[deg - N].wrapping_sub(a[i].wrapping_mul(b[j]));
            }
        }
    }
    result
}

/// Add two polynomials mod (X^N + 1) — just componentwise wrapping addition.
#[inline]
fn poly_add_neg<const N: usize>(a: &[u32; N], b: &[u32; N]) -> [u32; N] {
    let mut result = [0u32; N];
    for i in 0..N {
        result[i] = a[i].wrapping_add(b[i]);
    }
    result
}

/// Subtract two polynomials mod (X^N + 1) — componentwise wrapping subtraction.
#[inline]
fn poly_sub_neg<const N: usize>(a: &[u32; N], b: &[u32; N]) -> [u32; N] {
    let mut result = [0u32; N];
    for i in 0..N {
        result[i] = a[i].wrapping_sub(b[i]);
    }
    result
}

/// Multiply polynomial by `X^exp` mod (X^N + 1).
///
/// Rotation by `exp`:
/// - Coefficients shift right by `exp` positions.
/// - Coefficients that wrap past position N−1 are negated (negacyclic).
fn poly_rotate<const N: usize>(p: &[u32; N], exp: usize) -> [u32; N] {
    let exp = exp % (2 * N); // exponents are mod 2N in the negacyclic ring
    if exp == 0 {
        return *p;
    }
    let mut result = [0u32; N];
    for i in 0..N {
        let new_pos = i + exp;
        if new_pos < N {
            result[new_pos] = result[new_pos].wrapping_add(p[i]);
        } else {
            // Wrapped: subtract (negacyclic sign flip)
            result[new_pos - N] = result[new_pos - N].wrapping_sub(p[i]);
        }
    }
    result
}

// ── Internal: noise sampling ──────────────────────────────────────────────────

/// Sample a small Gaussian-like noise value truncated to `noise_bits` bits.
/// Returns a random u32 with only the top-most bits zeroed and the value centered at 0.
#[inline]
fn small_noise<R: Rng>(noise_bits: u32, rng: &mut R) -> u32 {
    if noise_bits >= 32 {
        return rng.random();
    }
    let raw: u32 = rng.random();
    // Keep only the lowest noise_bits bits, sign-extended to u32
    let mask = (1u32 << noise_bits).wrapping_sub(1);
    let small = raw & mask;
    // Extend sign: if bit (noise_bits-1) is set, extend with 1s
    if noise_bits > 0 && (small >> (noise_bits - 1)) != 0 {
        small | !mask
    } else {
        small
    }
}
