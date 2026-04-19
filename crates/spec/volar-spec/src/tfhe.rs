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

use crate::SpecRng;

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

/// Return a trivial encryption of a cleartext boolean `b`.
///
/// Equivalent to `if b { tfhe_trivial_one() } else { tfhe_trivial_zero() }`.
/// Used to promote public (cleartext) values into the ciphertext domain when
/// they must be passed to gates that require `LweCiphertext` operands.
pub fn tfhe_trivial_encrypt<const N_LWE: usize>(b: bool) -> LweCiphertext<N_LWE> {
    if b { tfhe_trivial_one() } else { tfhe_trivial_zero() }
}

/// XOR gate — free: `ct_xor = ct_a + ct_b` (componentwise wrapping addition).
///
/// # Composability warning
///
/// This is a **linear** (non-bootstrapped) gate.  Its output always decrypts
/// correctly, but the output **phase** may not be in the standard `{0, Q4}`
/// range.  In particular, `XOR(true, true)` produces phase `2·Q4 = 0x80000000`
/// instead of `0`.
///
/// **Do not** feed the output of this gate into a bootstrapped gate (AND, OR,
/// CMUX, PBS) unless you are certain both inputs cannot simultaneously be
/// `true`.  For composable boolean operations, use bootstrapped alternatives
/// (e.g. `tfhe_cmux` uses `OR(AND, AND)` internally).
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
    let mut ct_out = key_switch(&lwe_big, &bk.ksk);

    // Step 5: offset — the negacyclic ring naturally produces output in
    // {−Q4/2, Q4/2}.  Shift by +Q4/2 to get composable {0, Q4} encoding.
    ct_out.b = ct_out.b.wrapping_add(Q4 >> 1);

    ct_out
}

// ── Gate bootstrapping — OR ───────────────────────────────────────────────────

/// OR gate via full GINX gate bootstrapping.
///
/// Computes `a OR b` using the same blind-rotation machinery as AND, but with
/// a `+Q4/2` pre-offset instead of `−Q4/2`.
///
/// Phase arithmetic (for standard `{0, Q4}` encoded inputs):
/// - `(F, F)` → `0 + 0 + Q4/2 = Q4/2`         → false ✓
/// - `(F, T)` → `0 + Q4 + Q4/2 = 3·Q4/2`      → true  ✓
/// - `(T, F)` → `Q4 + 0 + Q4/2 = 3·Q4/2`      → true  ✓
/// - `(T, T)` → `Q4 + Q4 + Q4/2 = 5·Q4/2`     → true  ✓
///
/// Cost: one full gate bootstrapping round (same as AND).
pub fn tfhe_gate_bootstrapping_or<
    const N_LWE: usize,
    const BIG_N: usize,
    const BS_ELL: usize,
    const KS_ELL: usize,
>(
    ct_a: LweCiphertext<N_LWE>,
    ct_b: LweCiphertext<N_LWE>,
    bk: &BootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL>,
) -> LweCiphertext<N_LWE> {
    // Step 1: combine inputs; ADD Q4/2 to center OR gate threshold
    let mut ct = lwe_add(ct_a, ct_b);
    ct.b = ct.b.wrapping_add(Q4 >> 1); // add q/8

    // Steps 2–4: blind rotate, sample extract, key switch (same as AND)
    let acc = blind_rotate(&ct, bk);
    let lwe_big = sample_extract(&acc);
    let mut ct_out = key_switch(&lwe_big, &bk.ksk);

    // Step 5: composability offset
    ct_out.b = ct_out.b.wrapping_add(Q4 >> 1);

    ct_out
}

/// CMUX (controlled multiplexer) gate.
///
/// Computes `if sel { a } else { b }` obliviously.
///
/// Formula: `CMUX(sel, a, b) = OR(AND(sel, a), AND(NOT(sel), b))`
///
/// - `sel = 0` (encrypts false): returns `b`
/// - `sel = 1` (encrypts true): returns `a`
///
/// # Why not `b XOR (sel AND (a XOR b))`?
///
/// The free XOR gate produces non-standard phases when both inputs encrypt
/// `true` (phase `2·Q4` instead of `0`).  These non-standard phases cause
/// the AND gate's blind rotation to misclassify the input, making the
/// original formula incorrect for `CMUX(false, true, true)` and similar.
/// The `OR(AND, AND)` decomposition uses only bootstrapped gates on
/// standard-encoded inputs, guaranteeing correctness and composability.
///
/// Cost: three gate bootstrapping rounds (2× AND + 1× OR) plus one free NOT.
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
    let not_sel = tfhe_not(sel);
    let sel_and_a = tfhe_gate_bootstrapping_and(sel, a, bk);
    let nsel_and_b = tfhe_gate_bootstrapping_and(not_sel, b, bk);
    tfhe_gate_bootstrapping_or(sel_and_a, nsel_and_b, bk)
}

// ── Programmable bootstrapping ───────────────────────────────────────────────

/// Programmable bootstrapping — evaluate an arbitrary function encoded as a
/// lookup table (test polynomial) on an encrypted input.
///
/// Given an LWE ciphertext `ct` encrypting value `v`, and a test polynomial
/// `test_poly` encoding function `f`, returns an LWE ciphertext encrypting
/// `f(v)`.
///
/// The function `f` is encoded in the `BIG_N` coefficients of `test_poly`.
/// The blind rotation maps the encrypted value to a rotation exponent via the
/// torus-to-ring scaling, then sample extraction + key switching produce the
/// final LWE ciphertext.
///
/// # Cost
///
/// One full blind rotation (same as an AND gate), regardless of LUT complexity.
///
/// # Use cases
///
/// - Arbitrary boolean functions via LUT.
/// - Read-only table lookups (e.g. AES S-box) where table contents are known
///   at compile time and the index is encrypted.
/// - **NOT** suitable for reading from encrypted storage cells (cell values
///   cannot be packed into a cleartext test polynomial). Use MUX-tree or
///   loop-based oblivious access for encrypted storage.
pub fn tfhe_programmable_bootstrap<
    const N_LWE: usize,
    const BIG_N: usize,
    const BS_ELL: usize,
    const KS_ELL: usize,
>(
    ct: LweCiphertext<N_LWE>,
    test_poly: [u32; BIG_N],
    bk: &BootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL>,
) -> LweCiphertext<N_LWE> {
    // Blind rotate with the caller-supplied test polynomial.
    let acc = blind_rotate_with_poly(&ct, test_poly, bk);

    // Sample extract from accumulator → LweCiphertext<BIG_N>.
    let lwe_big = sample_extract(&acc);

    // Key switch → LweCiphertext<N_LWE>.
    key_switch(&lwe_big, &bk.ksk)
}

/// Read from a cleartext lookup table using programmable bootstrapping.
///
/// `addr_bits` is a slice of LWE ciphertexts, each encrypting a single address
/// bit (bit 0 = LSB).  `lut` is a cleartext boolean lookup table with up to
/// `2 * BIG_N` entries (the maximum addressable by a single blind rotation in
/// the negacyclic ring of degree `BIG_N`).
///
/// The function linearly combines the address-bit ciphertexts into a single
/// multi-bit address ciphertext, builds a test polynomial encoding the LUT
/// values, and performs one programmable bootstrapping.
///
/// # Cost
///
/// One blind rotation per call (regardless of table size), plus free LWE
/// additions for address combination.
///
/// # Panics
///
/// Panics if `lut.len() > 2 * BIG_N` (exceeds negacyclic ring capacity).
///
/// # Limitations
///
/// The lookup table is **cleartext** — this function is suitable for read-only
/// tables (e.g. S-boxes, truth tables) where the contents are public constants.
/// For encrypted mutable storage, use MUX-tree or loop-based oblivious access.
pub fn tfhe_lut_read<
    const N_LWE: usize,
    const BIG_N: usize,
    const BS_ELL: usize,
    const KS_ELL: usize,
>(
    addr_bits: &[LweCiphertext<N_LWE>],
    lut: &[bool],
    bk: &BootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL>,
) -> LweCiphertext<N_LWE> {
    let two_n = 2 * BIG_N;
    assert!(
        lut.len() <= two_n,
        "tfhe_lut_read: LUT size {} exceeds 2*BIG_N = {} (negacyclic ring capacity)",
        lut.len(),
        two_n,
    );

    let k = lut.len().max(1).next_power_of_two(); // padded LUT size

    // ── Special case: constant LUT ───────────────────────────────────────
    //
    // A LUT where all entries are the same is a constant function.
    // The negacyclic ring forces f(x+K/2) = ¬f(x), so a constant function
    // cannot be represented by a single blind rotation.  Return a trivial
    // encryption of the constant value directly.
    if !lut.is_empty() && lut.iter().all(|&v| v == lut[0]) {
        let msg = if lut[0] { Q4 } else { 0u32 };
        return LweCiphertext { a: [0u32; N_LWE], b: msg };
    }

    // ── Build test polynomial (signed range-fill) ────────────────────────
    //
    // The negacyclic ring Z[X]/(X^N+1) represents a test vector v of
    // length 2N via polynomial f of degree N−1:
    //   v[j]   = f[j]    for j ∈ [0, N)
    //   v[j+N] = −f[j]   (negacyclic image)
    //
    // For a K-entry LUT with step = 2N/K, entry `a` occupies test vector
    // positions [a·step, (a+1)·step).  The first K/2 entries map directly
    // to polynomial coefficients [0, N); entries K/2..K are the negacyclic
    // images and satisfy lut[a+K/2] = ¬lut[a].
    //
    // We use signed encoding: true → Q4/2, false → −Q4/2.
    // Each range of `step/2` consecutive polynomial coefficients is filled
    // with the same value, giving step/2 tolerance for rotation error in
    // each direction.
    let half_q4 = Q4 >> 1;
    let step = two_n / k;
    let half_k = k / 2;
    let poly_step = BIG_N / half_k; // coefficients per entry in polynomial

    let mut test_poly = [0u32; BIG_N];
    for j in 0..BIG_N {
        let entry_idx = j / poly_step;
        let val = if entry_idx < lut.len() && lut[entry_idx] {
            half_q4
        } else {
            half_q4.wrapping_neg()
        };
        test_poly[j] = val;
    }

    // ── Combine address bits into a single multi-bit LWE ciphertext ──────
    //
    // Address value = sum_{j} addr_bits[j] · 2^j.
    // Each bit encrypts {0, Q4}.  We rescale so bit j contributes 2^j · Δ
    // to the torus phase, where Δ = 2^32 / K.
    let delta = (1u64 << 32) / (k as u64); // Δ = 2^32 / K

    let mut combined = LweCiphertext { a: [0u32; N_LWE], b: 0 };
    for (j, addr_ct) in addr_bits.iter().enumerate() {
        let target = (1u64 << j) * delta; // 2^j · Δ
        for i in 0..N_LWE {
            let scaled = (addr_ct.a[i] as u64).wrapping_mul(target) / (Q4 as u64);
            combined.a[i] = combined.a[i].wrapping_add(scaled as u32);
        }
        let scaled_b = (addr_ct.b as u64).wrapping_mul(target) / (Q4 as u64);
        combined.b = combined.b.wrapping_add(scaled_b as u32);
    }

    // ── Centering offset ─────────────────────────────────────────────────
    //
    // Without this offset, address `a` maps to phase_exp = a·step, which
    // sits at the LEFT EDGE of the test vector range [a·step, (a+1)·step).
    // Adding Δ/2 shifts the phase to (a+½)·step — the CENTER of the range
    // — giving step/2 tolerance for torus_to_exp rounding in BOTH directions.
    let centering = (delta / 2) as u32;
    combined.b = combined.b.wrapping_add(centering);

    // ── Programmable bootstrap + composability offset ─────────────────────
    //
    // The PBS output is in signed encoding {−Q4/2, Q4/2}.
    // Add Q4/2 to convert to the standard {0, Q4} encoding expected by
    // lwe_decrypt and downstream gates.
    let mut ct_out = tfhe_programmable_bootstrap(combined, test_poly, bk);
    ct_out.b = ct_out.b.wrapping_add(half_q4);
    ct_out
}

// ── Encryption / Decryption (for keygen and tests) ───────────────────────────

/// Encrypt a bit `m ∈ {false, true}` under LWE secret key `sk`.
pub fn lwe_encrypt<const N_LWE: usize, R: SpecRng>(
    m: bool,
    sk: &LweSecretKey<N_LWE>,
    noise_bits: u32,
    rng: &mut R,
) -> LweCiphertext<N_LWE> {
    let mut a = [0u32; N_LWE];
    for ai in a.iter_mut() {
        *ai = rng.next_u32();
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
    // phase = b - ⟨a, s⟩ = e + msg  (where msg ∈ {0, Q4})
    let mut dot: u32 = 0;
    for i in 0..N_LWE {
        dot = dot.wrapping_add(ct.a[i].wrapping_mul(sk.key[i] as u32));
    }
    let phase = ct.b.wrapping_sub(dot);
    // Decision: is phase closer to Q4 (true) or to 0 (false)?
    //
    // Decision region for true (bit = 1): phase ∈ [Q4/2, 3·Q4/2) mod 2^32.
    // Shift by −Q4/2 so that region maps to [0, Q4):
    //   phase − Q4/2 mod 2^32 < Q4  ⟹ bit = 1
    let half = Q4 >> 1; // Q4/2 = 2^29
    let shifted = phase.wrapping_sub(half);
    shifted < Q4
}

// ── Key generation ────────────────────────────────────────────────────────────

/// Generate a random binary LWE secret key.
pub fn gen_lwe_secret_key<const N_LWE: usize, R: SpecRng>(
    rng: &mut R,
) -> LweSecretKey<N_LWE> {
    let mut key = [0u8; N_LWE];
    for k in key.iter_mut() {
        *k = (rng.next_u8() & 1) as u8;
    }
    LweSecretKey { key }
}

/// Generate a random binary RLWE secret key polynomial.
pub fn gen_rlwe_secret_key<const BIG_N: usize, R: SpecRng>(
    rng: &mut R,
) -> RlweSecretKey<BIG_N> {
    let mut key = [0u32; BIG_N];
    for k in key.iter_mut() {
        *k = (rng.next_u8() & 1) as u32;
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
    R: SpecRng,
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
fn rlwe_encrypt_scalar<const BIG_N: usize, R: SpecRng>(
    m: u32,
    sk: &RlweSecretKey<BIG_N>,
    noise_bits: u32,
    rng: &mut R,
) -> RlweCiphertext<BIG_N> {
    let a: [u32; BIG_N] = core::array::from_fn(|_| rng.next_u32());
    // b = a * s + e + m (constant term)
    let mut b = poly_mul_neg(&a, &sk.key);
    b[0] = b[0].wrapping_add(small_noise(noise_bits, rng)).wrapping_add(m);
    RlweCiphertext { a, b }
}

/// Encrypt a polynomial message `msg_poly` as RLWE.
#[allow(dead_code)]
fn rlwe_encrypt_poly<const BIG_N: usize, R: SpecRng>(
    msg_poly: &[u32; BIG_N],
    sk: &RlweSecretKey<BIG_N>,
    noise_bits: u32,
    rng: &mut R,
) -> RlweCiphertext<BIG_N> {
    let a: [u32; BIG_N] = core::array::from_fn(|_| rng.next_u32());
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
fn rgsw_encrypt<const BIG_N: usize, const BS_ELL: usize, R: SpecRng>(
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

        // rlwe0: encrypt zero, then add message contribution to the *a* polynomial
        // constant term.  Per TFHE Definition 3.8, the first ℓ rows of the RGSW
        // gadget matrix have gadget factors in the a-column: (g_j, 0).
        let mut rlwe0 = rlwe_encrypt_scalar(0, sk, noise_bits, rng);
        rlwe0.a[0] = rlwe0.a[0].wrapping_add(contrib);

        // rlwe1: encrypt contrib in the body (b-component) — gadget factor in
        // the b-column: (0, g_j).
        let rlwe1 = rlwe_encrypt_scalar(contrib, sk, noise_bits, rng);

        RgswRow { rlwe0, rlwe1 }
    });

    RgswCiphertext { rows }
}

// ── Internal: blind rotation (GINX) ──────────────────────────────────────────

/// Perform GINX blind rotation with a caller-supplied test polynomial.
///
/// Given LWE ciphertext `ct = (a, b)`, computes:
///   `acc_out = X^{-b̃} · ∏_{i=0}^{N_LWE-1} CMUX(BSK[i], acc · X^{ã[i]}, acc)`
///
/// where ã[i] = round(a[i] · 2N / 2^32) and b̃ = round(b · 2N / 2^32).
///
/// `test_poly` is placed in the body of the trivial RLWE accumulator before
/// rotation begins.  For the AND gate, this is `Q4` in positions `[0, N/2)`,
/// `−Q4` in `[N/2, N)`.  For programmable bootstrapping, the caller encodes
/// the desired lookup function into this polynomial.
fn blind_rotate_with_poly<
    const N_LWE: usize,
    const BIG_N: usize,
    const BS_ELL: usize,
    const KS_ELL: usize,
>(
    ct: &LweCiphertext<N_LWE>,
    test_poly: [u32; BIG_N],
    bk: &BootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL>,
) -> RlweCiphertext<BIG_N> {
    // Trivial RLWE encryption of test_poly (a = 0, b = test_poly)
    let mut acc = RlweCiphertext {
        a: [0u32; BIG_N],
        b: test_poly,
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

/// Build the AND gate test polynomial.
///
/// Uses Q4/2 amplitude so that the blind-rotation output lands in {−Q4/2, Q4/2}.
/// `tfhe_gate_bootstrapping_and` then adds a Q4/2 offset to produce composable
/// {0, Q4} encoding.
///
/// Returns `v[k] = −Q4/2` for `k < BIG_N/2`, `v[k] = Q4/2` for `k ≥ BIG_N/2`.
#[inline]
fn and_test_poly<const BIG_N: usize>() -> [u32; BIG_N] {
    let mut v = [0u32; BIG_N];
    let half_q4 = Q4 >> 1;
    for k in 0..BIG_N / 2 {
        v[k] = half_q4.wrapping_neg(); // −Q4/2
    }
    for k in BIG_N / 2..BIG_N {
        v[k] = half_q4; // Q4/2
    }
    v
}

/// Legacy wrapper: blind rotation with the hardcoded AND gate test polynomial.
fn blind_rotate<
    const N_LWE: usize,
    const BIG_N: usize,
    const BS_ELL: usize,
    const KS_ELL: usize,
>(
    ct: &LweCiphertext<N_LWE>,
    bk: &BootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL>,
) -> RlweCiphertext<BIG_N> {
    blind_rotate_with_poly(ct, and_test_poly(), bk)
}

/// Convert a torus element (u32) to a ring exponent in [0, 2N).
///
/// Per TFHE Algorithm 4 (BlindRotate), the mapping is `round(x · 2N / 2^32)`,
/// i.e. rounding to nearest integer, not truncation.  This halves the maximum
/// per-component quantization error from 1 to 0.5 ring positions.
#[inline]
fn torus_to_exp(x: u32, scale_shift: u32, two_n: usize) -> usize {
    // Add half of the discarded range to round-to-nearest instead of floor.
    let half = if scale_shift > 0 { 1u32 << (scale_shift - 1) } else { 0 };
    let exp = (x.wrapping_add(half) >> scale_shift) as usize;
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
    // Round up the truncated tail (skip when decomposition is exact, i.e. tail_shift == 0)
    let tail_shift = 32u32.saturating_sub(bg_log.saturating_mul(KS_ELL as u32));
    if tail_shift > 0 && tail_shift < 32 {
        let half_tail = 1u64 << (tail_shift - 1);
        rem = rem.wrapping_add(half_tail);
    }
    let mut digits = [0u32; KS_ELL];
    for j in (0..KS_ELL).rev() {
        let shift = 32u32.saturating_sub(bg_log.saturating_mul(j as u32 + 1));
        if shift < 32 {
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
/// coefficient `i` in base `2^bs_bg_log`, extracted from the most significant
/// bits downward.
///
/// Level 0 is the most significant digit: bits `[32 - bg_log, 32)`.
/// Level j extracts bits `[32 - bg_log*(j+1), 32 - bg_log*j)`.
fn poly_decompose<const BIG_N: usize, const BS_ELL: usize>(
    p: &[u32; BIG_N],
    bg_log: u32,
) -> [[u32; BIG_N]; BS_ELL] {
    let bg = 1u64 << bg_log;
    let mask = (bg - 1) as u32;
    let mut result = [[0u32; BIG_N]; BS_ELL];

    for i in 0..BIG_N {
        let x = p[i];
        // Optional rounding: add half of the first non-represented bit
        // to reduce decomposition error.
        let tail_bits = 32u32.saturating_sub(bg_log.saturating_mul(BS_ELL as u32));
        let rounded = if tail_bits > 0 && tail_bits < 32 {
            x.wrapping_add(1u32 << (tail_bits - 1))
        } else {
            x
        };
        for j in 0..BS_ELL {
            // Level j: shift to extract the j-th digit from the top.
            // Digit j occupies bits [32 - bg_log*(j+1), 32 - bg_log*j).
            let shift = 32u32.saturating_sub(bg_log.saturating_mul(j as u32 + 1));
            result[j][i] = if shift < 32 {
                (rounded >> shift) & mask
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
fn lwe_encrypt_raw<const N_LWE: usize, R: SpecRng>(
    msg: u32,
    sk: &LweSecretKey<N_LWE>,
    noise_bits: u32,
    rng: &mut R,
) -> LweCiphertext<N_LWE> {
    let mut a = [0u32; N_LWE];
    for ai in a.iter_mut() {
        *ai = rng.next_u32();
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
            // No wrap — coefficient stays positive.
            result[new_pos] = result[new_pos].wrapping_add(p[i]);
        } else if new_pos < 2 * N {
            // First wrap — negacyclic sign flip.
            result[new_pos - N] = result[new_pos - N].wrapping_sub(p[i]);
        } else {
            // Second wrap — double negation = positive again.
            result[new_pos - 2 * N] = result[new_pos - 2 * N].wrapping_add(p[i]);
        }
    }
    result
}

// ── Internal: noise sampling ──────────────────────────────────────────────────

/// Sample a small Gaussian-like noise value truncated to `noise_bits` bits.
/// Returns a random u32 with only the top-most bits zeroed and the value centered at 0.
#[inline]
fn small_noise<R: SpecRng>(noise_bits: u32, rng: &mut R) -> u32 {
    if noise_bits >= 32 {
        return rng.next_u32();
    }
    let raw: u32 = rng.next_u32();
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

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    // Simple seeded splitmix64-based RNG for test reproducibility.
    struct TestRng(u64);

    impl TestRng {
        fn new(seed: u64) -> Self {
            Self(seed)
        }
    }

    impl SpecRng for TestRng {
        fn next_u32(&mut self) -> u32 {
            // Splitmix64 — better bit quality than raw xorshift for truncated u32.
            self.0 = self.0.wrapping_add(0x9e3779b97f4a7c15);
            let mut z = self.0;
            z = (z ^ (z >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
            z = (z ^ (z >> 27)).wrapping_mul(0x94d049bb133111eb);
            z = z ^ (z >> 31);
            z as u32
        }
    }

    // Parameters sized for correctness testing.
    //
    // Key constraint: BIG_N >> N_LWE so that blind-rotation rounding errors
    // (at most ~N_LWE/2 positions with round-to-nearest) stay well within the
    // test polynomial step size (BIG_N/2 for the AND gate).
    //
    // With BIG_N = 64 and N_LWE = 8 the margin is BIG_N/2 - N_LWE/2 = 28
    // ring positions — more than enough.
    //
    // Decomposition: BS_ELL * BS_BG_LOG = 2 * 16 = 32 bits → exact for u32.
    const T_N_LWE: usize = 8;
    const T_BIG_N: usize = 64;
    const T_BS_ELL: usize = 2;
    const T_KS_ELL: usize = 2;
    const T_BS_BG_LOG: u32 = 16;
    const T_KS_BG_LOG: u32 = 16;
    const T_NOISE_BITS: u32 = 0; // noiseless — spec correctness tests

    type Ct = LweCiphertext<T_N_LWE>;

    fn test_keys(
        seed: u64,
    ) -> (
        LweSecretKey<T_N_LWE>,
        RlweSecretKey<T_BIG_N>,
        BootstrappingKey<T_N_LWE, T_BIG_N, T_BS_ELL, T_KS_ELL>,
    ) {
        let mut rng = TestRng::new(seed);
        let lwe_sk = gen_lwe_secret_key::<T_N_LWE, _>(&mut rng);
        let rlwe_sk = gen_rlwe_secret_key::<T_BIG_N, _>(&mut rng);
        let bk = gen_bootstrapping_key(
            &lwe_sk,
            &rlwe_sk,
            T_BS_BG_LOG,
            T_KS_BG_LOG,
            T_NOISE_BITS,
            T_NOISE_BITS,
            &mut rng,
        );
        (lwe_sk, rlwe_sk, bk)
    }

    fn encrypt(m: bool, sk: &LweSecretKey<T_N_LWE>, seed: u64) -> Ct {
        let mut rng = TestRng::new(seed);
        lwe_encrypt(m, sk, T_NOISE_BITS, &mut rng)
    }

    // ── Basic roundtrip test ─────────────────────────────────────────────

    #[test]
    fn encrypt_decrypt_roundtrip() {
        let (sk, _, _) = test_keys(42);
        for m in [false, true] {
            let ct = encrypt(m, &sk, 50);
            let got = lwe_decrypt(&ct, &sk);
            assert_eq!(got, m, "encrypt({m}) → decrypt = {got}");
        }
    }

    #[test]
    fn debug_cmux_trivial() {
        // Test CMUX directly with a known RGSW encrypting 0 and 1
        let mut rng = TestRng::new(42);
        let rlwe_sk = gen_rlwe_secret_key::<T_BIG_N, _>(&mut rng);

        // d0: trivial RLWE with b[0] = Q4 (encrypts "true")
        let d0 = RlweCiphertext {
            a: [0u32; T_BIG_N],
            b: {
                let mut b = [0u32; T_BIG_N];
                b[0] = Q4;
                b
            },
        };

        // d1: trivial RLWE with b[0] = 0 (encrypts "false")
        let d1 = RlweCiphertext {
            a: [0u32; T_BIG_N],
            b: [0u32; T_BIG_N],
        };

        // RGSW encrypting 0 → CMUX should select d0
        let rgsw0: RgswCiphertext<T_BIG_N, T_BS_ELL> = rgsw_encrypt(false, &rlwe_sk, T_BS_BG_LOG, 0, &mut rng);
        let out0 = cmux(&rgsw0, &d1, &d0, T_BS_BG_LOG);

        // RGSW encrypting 1 → CMUX should select d1
        let rgsw1: RgswCiphertext<T_BIG_N, T_BS_ELL> = rgsw_encrypt(true, &rlwe_sk, T_BS_BG_LOG, 0, &mut rng);
        let out1 = cmux(&rgsw1, &d1, &d0, T_BS_BG_LOG);

        // Decrypt: extract constant coefficient, check it
        // For trivial d0/d1 with a=0, the phase is just b[0].
        // After CMUX, if the RGSW secret key bits interact, we get noise.
        // Since d0/d1 are trivial (a=0), diff = d1-d0 is also trivial,
        // and external_product with a trivial RLWE should produce an RLWE
        // whose decrypted constant coefficient is close to the message.

        // Extract from out0
        let lwe0 = sample_extract(&out0);
        let phase0 = lwe0.b.wrapping_sub({
            let mut dot = 0u32;
            for i in 0..T_BIG_N {
                dot = dot.wrapping_add(lwe0.a[i].wrapping_mul(rlwe_sk.key[i]));
            }
            dot
        });

        let lwe1 = sample_extract(&out1);
        let phase1 = lwe1.b.wrapping_sub({
            let mut dot = 0u32;
            for i in 0..T_BIG_N {
                dot = dot.wrapping_add(lwe1.a[i].wrapping_mul(rlwe_sk.key[i]));
            }
            dot
        });

        // phase0 should be close to Q4 (selected d0)
        // phase1 should be close to 0 (selected d1)
        let err0 = (phase0 as i32).wrapping_sub(Q4 as i32).unsigned_abs();
        let err1 = phase1.min(phase1.wrapping_neg());

        if err0 > Q4 / 4 || err1 > Q4 / 4 {
            panic!(
                "CMUX trivial test:\n  \
                 phase0 (expect {Q4:#010x}) = {phase0:#010x} (err={err0:#010x})\n  \
                 phase1 (expect 0x00000000) = {phase1:#010x} (err={err1:#010x})"
            );
        }
    }

    #[test]
    fn debug_blind_rotate_single_step() {
        // Test blind rotation with just 1 CMUX step
        let mut rng = TestRng::new(42);
        let lwe_sk = gen_lwe_secret_key::<1, _>(&mut rng); // N_LWE = 1
        let rlwe_sk = gen_rlwe_secret_key::<T_BIG_N, _>(&mut rng);
        let bk: BootstrappingKey<1, T_BIG_N, T_BS_ELL, T_KS_ELL> = gen_bootstrapping_key(
            &lwe_sk,
            &rlwe_sk,
            T_BS_BG_LOG,
            T_KS_BG_LOG,
            0, 0,
            &mut rng,
        );

        // Encrypt true: b = dot(a, s) + Q4
        let ct_a = lwe_encrypt(true, &lwe_sk, 0, &mut rng);
        let ct_b = lwe_encrypt(true, &lwe_sk, 0, &mut rng);

        // Verify roundtrip
        assert!(lwe_decrypt(&ct_a, &lwe_sk), "ct_a roundtrip");
        assert!(lwe_decrypt(&ct_b, &lwe_sk), "ct_b roundtrip");

        // AND: combine + subtract Q4/2
        let mut ct = lwe_add(ct_a, ct_b);
        ct.b = ct.b.wrapping_sub(Q4 >> 1);

        // blind rotate with N_LWE=1 → just one CMUX
        let acc = blind_rotate(&ct, &bk);

        // Decrypt acc via sample_extract
        let lwe_big = sample_extract(&acc);
        let mut dot = 0u32;
        for i in 0..T_BIG_N {
            dot = dot.wrapping_add(lwe_big.a[i].wrapping_mul(rlwe_sk.key[i]));
        }
        let phase = lwe_big.b.wrapping_sub(dot);
        // With Q4/2-amplitude test poly, AND(1,1) blind-rotation output is Q4/2
        // (the Q4/2 composability offset is added by tfhe_gate_bootstrapping_and,
        // not by blind_rotate itself).
        let expected = Q4 >> 1;
        let err = (phase as i32).wrapping_sub(expected as i32).unsigned_abs();

        if err > Q4 / 4 {
            panic!(
                "blind_rotate(N_LWE=1, AND(1,1)):\n  \
                 phase = {phase:#010x} (expect ~{expected:#010x})\n  \
                 err = {err:#010x}"
            );
        }
    }

    #[test]
    fn debug_and_gate_noiseless() {
        // Trace AND(true, true) step by step
        let (sk, _, bk) = test_keys(42);

        let ct_a = encrypt(true, &sk, 100);
        let ct_b = encrypt(true, &sk, 200);

        // Verify inputs decrypt correctly
        assert!(lwe_decrypt(&ct_a, &sk), "ct_a should decrypt to true");
        assert!(lwe_decrypt(&ct_b, &sk), "ct_b should decrypt to true");

        // Step 1: combine
        let mut ct = lwe_add(ct_a, ct_b);
        ct.b = ct.b.wrapping_sub(Q4 >> 1);

        // Phase of combined ct
        let mut dot: u32 = 0;
        for i in 0..T_N_LWE {
            dot = dot.wrapping_add(ct.a[i].wrapping_mul(sk.key[i] as u32));
        }
        let phase = ct.b.wrapping_sub(dot);
        // For AND(1,1): phase should be ≈ 2*Q4 - Q4/2 = 3*Q4/2

        // Step 2: blind rotate
        let acc = blind_rotate(&ct, &bk);
        // Coefficient 0 of acc.b should be ≈ Q4/2 for AND(1,1) = true
        // (before the composability offset added by tfhe_gate_bootstrapping_and)
        let b0 = acc.b[0];

        // Step 3: sample extract
        let lwe_big: LweCiphertext<T_BIG_N> = sample_extract(&acc);

        // Step 4: key switch
        let mut ct_out = key_switch(&lwe_big, &bk.ksk);

        // Step 5: add Q4/2 composability offset (same as tfhe_gate_bootstrapping_and)
        ct_out.b = ct_out.b.wrapping_add(Q4 >> 1);

        let got = lwe_decrypt(&ct_out, &sk);

        // Debug output via panic
        if !got {
            panic!(
                "AND(true,true) debug:\n  \
                 phase(combined) = {phase:#010x} (expect ~{:#010x} = 3*Q4/2)\n  \
                 acc.b[0] = {b0:#010x} (expect ~{:#010x} = Q4/2 for 'true')\n  \
                 result = {got}",
                3u32.wrapping_mul(Q4) / 2,
                Q4 >> 1
            );
        }
    }

    // ── AND gate baseline (verify test infrastructure works) ──────────────

    #[test]
    fn and_gate_all_combos() {
        let (sk, _, bk) = test_keys(42);
        for (a, b) in [(false, false), (false, true), (true, false), (true, true)] {
            let ct_a = encrypt(a, &sk, 100);
            let ct_b = encrypt(b, &sk, 200);
            let ct_out = tfhe_gate_bootstrapping_and(ct_a, ct_b, &bk);
            let got = lwe_decrypt(&ct_out, &sk);
            assert_eq!(got, a & b, "AND({a}, {b}) = {got}, expected {}", a & b);
        }
    }

    #[test]
    fn or_gate_all_combos() {
        let (sk, _, bk) = test_keys(42);
        for (a, b) in [(false, false), (false, true), (true, false), (true, true)] {
            let ct_a = encrypt(a, &sk, 100);
            let ct_b = encrypt(b, &sk, 200);
            let ct_out = tfhe_gate_bootstrapping_or(ct_a, ct_b, &bk);
            let got = lwe_decrypt(&ct_out, &sk);
            assert_eq!(got, a | b, "OR({a}, {b}) = {got}, expected {}", a | b);
        }
    }

    // ── Programmable bootstrapping ────────────────────────────────────────

    #[test]
    fn pbs_identity_function() {
        // Build a test polynomial for the identity function using and_test_poly
        // which outputs in {−Q4/2, Q4/2} encoding.  We add a Q4/2 offset after
        // the PBS to get composable {0, Q4} output.
        let (sk, _, bk) = test_keys(42);
        let test_poly = and_test_poly::<T_BIG_N>();

        for m in [false, true] {
            let ct = encrypt(m, &sk, 300);
            let mut ct_out = tfhe_programmable_bootstrap(ct, test_poly, &bk);
            ct_out.b = ct_out.b.wrapping_add(Q4 >> 1); // {-Q4/2, Q4/2} → {0, Q4}
            let got = lwe_decrypt(&ct_out, &sk);
            assert_eq!(got, m, "PBS identity({m}) = {got}");
        }
    }

    #[test]
    fn pbs_not_function() {
        // Build a test polynomial that negates: 0 → 1, 1 → 0.
        // Negate the AND test polynomial, then add Q4/2 offset for composability.
        let (sk, _, bk) = test_keys(42);
        let and_poly = and_test_poly::<T_BIG_N>();
        let mut not_poly = [0u32; T_BIG_N];
        for i in 0..T_BIG_N {
            not_poly[i] = and_poly[i].wrapping_neg();
        }

        for m in [false, true] {
            let ct = encrypt(m, &sk, 400);
            let mut ct_out = tfhe_programmable_bootstrap(ct, not_poly, &bk);
            ct_out.b = ct_out.b.wrapping_add(Q4 >> 1); // {-Q4/2, Q4/2} → {0, Q4}
            let got = lwe_decrypt(&ct_out, &sk);
            assert_eq!(got, !m, "PBS NOT({m}) = {got}");
        }
    }

    // ── LUT read ─────────────────────────────────────────────────────────

    #[test]
    fn lut_read_1bit_addr() {
        // 1-bit address → 2-entry LUT
        let (sk, _, bk) = test_keys(42);

        // LUT: [false, true] → addr 0 = false, addr 1 = true (identity)
        let lut = [false, true];
        for addr_val in 0u8..2 {
            let addr_bit = encrypt(addr_val != 0, &sk, 500 + addr_val as u64);
            let ct_out = tfhe_lut_read(&[addr_bit], &lut, &bk);
            let got = lwe_decrypt(&ct_out, &sk);
            let expected = lut[addr_val as usize];
            assert_eq!(got, expected, "LUT[{addr_val}] = {got}, expected {expected}");
        }

        // LUT: [true, false] → addr 0 = true, addr 1 = false (NOT)
        let lut_not = [true, false];
        for addr_val in 0u8..2 {
            let addr_bit = encrypt(addr_val != 0, &sk, 600 + addr_val as u64);
            let ct_out = tfhe_lut_read(&[addr_bit], &lut_not, &bk);
            let got = lwe_decrypt(&ct_out, &sk);
            let expected = lut_not[addr_val as usize];
            assert_eq!(got, expected, "LUT_NOT[{addr_val}] = {got}, expected {expected}");
        }
    }

    #[test]
    fn lut_read_2bit_addr() {
        // 2-bit address → 4-entry LUT: [true, false, false, true]
        // Negacyclic constraint: LUT[0] != LUT[2] and LUT[1] != LUT[3]
        let (sk, _, bk) = test_keys(42);
        let lut = [true, false, false, true];

        for addr_val in 0u8..4 {
            let bit0 = encrypt((addr_val & 1) != 0, &sk, 700 + addr_val as u64 * 2);
            let bit1 = encrypt((addr_val & 2) != 0, &sk, 701 + addr_val as u64 * 2);
            let ct_out = tfhe_lut_read(&[bit0, bit1], &lut, &bk);
            let got = lwe_decrypt(&ct_out, &sk);
            let expected = lut[addr_val as usize];
            assert_eq!(
                got, expected,
                "LUT[{addr_val}] = {got}, expected {expected} (lut = {lut:?})"
            );
        }
    }

    #[test]
    fn lut_read_all_false() {
        // LUT of all false entries — any address should decrypt to false.
        let (sk, _, bk) = test_keys(42);
        let lut = [false, false];

        for addr_val in 0u8..2 {
            let addr_bit = encrypt(addr_val != 0, &sk, 800 + addr_val as u64);
            let ct_out = tfhe_lut_read(&[addr_bit], &lut, &bk);
            let got = lwe_decrypt(&ct_out, &sk);
            assert!(!got, "all-false LUT[{addr_val}] should be false, got true");
        }
    }

    #[test]
    fn lut_read_alternating() {
        // Negacyclic constraint: LUT[a] and LUT[a + K/2] must have opposite
        // boolean values.  An all-true LUT is therefore impossible with a
        // single PBS.  Test an alternating [true, false] pattern instead.
        let (sk, _, bk) = test_keys(42);
        let lut = [true, false];

        for addr_val in 0u8..2 {
            let addr_bit = encrypt(addr_val != 0, &sk, 900 + addr_val as u64);
            let ct_out = tfhe_lut_read(&[addr_bit], &lut, &bk);
            let got = lwe_decrypt(&ct_out, &sk);
            let expected = lut[addr_val as usize];
            assert_eq!(
                got, expected,
                "alternating LUT[{addr_val}] = {got}, expected {expected}"
            );
        }
    }

    #[test]
    #[should_panic(expected = "LUT size")]
    fn lut_read_panics_on_oversized_lut() {
        let (sk, _, bk) = test_keys(42);
        // BIG_N = 64, so 2*BIG_N = 128. LUT of 129 entries should panic.
        let lut = [false; 129];
        let addr_bit = encrypt(false, &sk, 1000);
        let _ = tfhe_lut_read(&[addr_bit], &lut, &bk);
    }

    // ── Property-based tests ─────────────────────────────────────────────
    //
    // Fuzz TFHE operations by running them on encrypted inputs in parallel
    // with the same operations on plaintext booleans, then comparing
    // decrypted results against the plaintext oracle.

    extern crate std;
    use std::vec::Vec;

    use proptest::prelude::*;

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(256))]

        // ── Individual gate correctness ──────────────────────────────────

        /// AND gate: decrypt(AND(enc(a), enc(b))) == a && b
        #[test]
        fn prop_and_correct(
            a in any::<bool>(),
            b in any::<bool>(),
            key_seed in 0u64..100,
            enc_seed in 0u64..100000,
        ) {
            let (sk, _, bk) = test_keys(key_seed);
            let ct_a = encrypt(a, &sk, enc_seed);
            let ct_b = encrypt(b, &sk, enc_seed.wrapping_add(1));
            let ct_out = tfhe_gate_bootstrapping_and(ct_a, ct_b, &bk);
            let got = lwe_decrypt(&ct_out, &sk);
            prop_assert_eq!(got, a & b, "AND({}, {})", a, b);
        }

        /// XOR gate (free — no bootstrapping): decrypt(XOR(enc(a), enc(b))) == a ^ b
        #[test]
        fn prop_xor_correct(
            a in any::<bool>(),
            b in any::<bool>(),
            key_seed in 0u64..100,
            enc_seed in 0u64..100000,
        ) {
            let (sk, _, _bk) = test_keys(key_seed);
            let ct_a = encrypt(a, &sk, enc_seed);
            let ct_b = encrypt(b, &sk, enc_seed.wrapping_add(1));
            let ct_out = tfhe_xor(ct_a, ct_b);
            let got = lwe_decrypt(&ct_out, &sk);
            prop_assert_eq!(got, a ^ b, "XOR({}, {})", a, b);
        }

        /// NOT gate (free — no bootstrapping): decrypt(NOT(enc(a))) == !a
        #[test]
        fn prop_not_correct(
            a in any::<bool>(),
            key_seed in 0u64..100,
            enc_seed in 0u64..100000,
        ) {
            let (sk, _, _bk) = test_keys(key_seed);
            let ct_a = encrypt(a, &sk, enc_seed);
            let ct_out = tfhe_not(ct_a);
            let got = lwe_decrypt(&ct_out, &sk);
            prop_assert_eq!(got, !a, "NOT({})", a);
        }

        /// OR gate (bootstrapped): decrypt(OR(enc(a), enc(b))) == a || b
        #[test]
        fn prop_or_correct(
            a in any::<bool>(),
            b in any::<bool>(),
            key_seed in 0u64..100,
            enc_seed in 0u64..100000,
        ) {
            let (sk, _, bk) = test_keys(key_seed);
            let ct_a = encrypt(a, &sk, enc_seed);
            let ct_b = encrypt(b, &sk, enc_seed.wrapping_add(1));
            let ct_out = tfhe_gate_bootstrapping_or(ct_a, ct_b, &bk);
            let got = lwe_decrypt(&ct_out, &sk);
            prop_assert_eq!(got, a | b, "OR({}, {})", a, b);
        }

        /// CMUX (encrypted mux): decrypt(CMUX(sel, a, b)) == if sel { a } else { b }
        #[test]
        fn prop_cmux_correct(
            sel in any::<bool>(),
            a in any::<bool>(),
            b in any::<bool>(),
            key_seed in 0u64..100,
            enc_seed in 0u64..100000,
        ) {
            let (sk, _, bk) = test_keys(key_seed);
            let ct_sel = encrypt(sel, &sk, enc_seed);
            let ct_a = encrypt(a, &sk, enc_seed.wrapping_add(1));
            let ct_b = encrypt(b, &sk, enc_seed.wrapping_add(2));
            let ct_out = tfhe_cmux(ct_sel, ct_a, ct_b, &bk);
            let got = lwe_decrypt(&ct_out, &sk);
            let expected = if sel { a } else { b };
            prop_assert_eq!(got, expected, "CMUX({}, {}, {})", sel, a, b);
        }

        // ── Composability (chained gates) ────────────────────────────────
        //
        // These verify that the output of one bootstrapped gate can be
        // correctly fed as input to another gate — the composability
        // property that depends on the Q4/2 offset being correct.

        /// AND(AND(a, b), c) — two bootstrapped AND gates chained
        #[test]
        fn prop_and_chain_2(
            a in any::<bool>(),
            b in any::<bool>(),
            c in any::<bool>(),
            key_seed in 0u64..100,
            enc_seed in 0u64..100000,
        ) {
            let (sk, _, bk) = test_keys(key_seed);
            let ct_a = encrypt(a, &sk, enc_seed);
            let ct_b = encrypt(b, &sk, enc_seed + 1);
            let ct_c = encrypt(c, &sk, enc_seed + 2);
            let ct_ab = tfhe_gate_bootstrapping_and(ct_a, ct_b, &bk);
            let ct_out = tfhe_gate_bootstrapping_and(ct_ab, ct_c, &bk);
            let got = lwe_decrypt(&ct_out, &sk);
            prop_assert_eq!(got, a & b & c, "AND(AND({}, {}), {})", a, b, c);
        }

        /// XOR(AND(a, b), NOT(AND(c, d))) — mixed gate chain
        #[test]
        fn prop_mixed_chain(
            a in any::<bool>(),
            b in any::<bool>(),
            c in any::<bool>(),
            d in any::<bool>(),
            key_seed in 0u64..100,
            enc_seed in 0u64..100000,
        ) {
            let (sk, _, bk) = test_keys(key_seed);
            let ct_a = encrypt(a, &sk, enc_seed);
            let ct_b = encrypt(b, &sk, enc_seed + 1);
            let ct_c = encrypt(c, &sk, enc_seed + 2);
            let ct_d = encrypt(d, &sk, enc_seed + 3);

            let ab = tfhe_gate_bootstrapping_and(ct_a, ct_b, &bk);
            let cd = tfhe_gate_bootstrapping_and(ct_c, ct_d, &bk);
            let not_cd = tfhe_not(cd);
            let result = tfhe_xor(ab, not_cd);

            let got = lwe_decrypt(&result, &sk);
            let expected = (a & b) ^ !(c & d);
            prop_assert_eq!(got, expected,
                "XOR(AND({},{}), NOT(AND({},{})))", a, b, c, d);
        }

        // ── LUT read ────────────────────────────────────────────────────

        /// 1-bit address LUT: negacyclic [v, !v] or constant [v, v]
        #[test]
        fn prop_lut_1bit(
            val0 in any::<bool>(),
            is_constant in any::<bool>(),
            addr in any::<bool>(),
            key_seed in 0u64..100,
            enc_seed in 0u64..100000,
        ) {
            let lut: [bool; 2] = if is_constant {
                [val0, val0] // constant → trivial encryption path
            } else {
                [val0, !val0] // negacyclic-compliant
            };
            let (sk, _, bk) = test_keys(key_seed);
            let ct_addr = encrypt(addr, &sk, enc_seed);
            let ct_out = tfhe_lut_read(&[ct_addr], &lut, &bk);
            let got = lwe_decrypt(&ct_out, &sk);
            let expected = lut[addr as usize];
            prop_assert_eq!(got, expected, "LUT({:?})[{}]", lut, addr as usize);
        }

        /// 2-bit address LUT: negacyclic [a, b, !a, !b]
        #[test]
        fn prop_lut_2bit(
            val0 in any::<bool>(),
            val1 in any::<bool>(),
            addr0 in any::<bool>(),
            addr1 in any::<bool>(),
            key_seed in 0u64..100,
            enc_seed in 0u64..100000,
        ) {
            let lut = [val0, val1, !val0, !val1]; // negacyclic
            let addr_val = (addr0 as usize) + 2 * (addr1 as usize);
            let (sk, _, bk) = test_keys(key_seed);
            let ct_addr0 = encrypt(addr0, &sk, enc_seed);
            let ct_addr1 = encrypt(addr1, &sk, enc_seed + 1);
            let ct_out = tfhe_lut_read(&[ct_addr0, ct_addr1], &lut, &bk);
            let got = lwe_decrypt(&ct_out, &sk);
            let expected = lut[addr_val];
            prop_assert_eq!(got, expected, "LUT({:?})[{}]", lut, addr_val);
        }

        // ── Random circuit ──────────────────────────────────────────────
        //
        // Builds a random DAG of composable gates (AND, OR, NOT) over 2-4
        // encrypted inputs.  After executing 1-5 random operations, decrypts
        // every intermediate value and compares with the plaintext oracle.
        //
        // Note: free XOR is excluded because it produces non-standard phases
        // that break composability (see `tfhe_xor` doc comment).

        #[test]
        fn prop_random_circuit(
            inputs in proptest::collection::vec(any::<bool>(), 2..=4),
            raw_ops in proptest::collection::vec(
                (0u8..3, any::<usize>(), any::<usize>()),
                1..=5,
            ),
            key_seed in 0u64..100,
            enc_seed in 0u64..100000,
        ) {
            let (sk, _, bk) = test_keys(key_seed);

            // Plaintext pool
            let mut plain: Vec<bool> = inputs.clone();
            // Encrypted pool
            let mut enc: Vec<Ct> = inputs
                .iter()
                .enumerate()
                .map(|(i, &v)| encrypt(v, &sk, enc_seed.wrapping_add(i as u64)))
                .collect();

            // Execute operations — each appends a new value to both pools
            for &(op_type, ref_a, ref_b) in &raw_ops {
                let n = plain.len();
                let a = ref_a % n;
                let b = ref_b % n;
                match op_type {
                    0 => {
                        // AND (bootstrapped — composable)
                        plain.push(plain[a] & plain[b]);
                        enc.push(tfhe_gate_bootstrapping_and(enc[a], enc[b], &bk));
                    }
                    1 => {
                        // OR (bootstrapped — composable)
                        plain.push(plain[a] | plain[b]);
                        enc.push(tfhe_gate_bootstrapping_or(enc[a], enc[b], &bk));
                    }
                    _ => {
                        // NOT (free — composable on standard-phase inputs)
                        plain.push(!plain[a]);
                        enc.push(tfhe_not(enc[a]));
                    }
                }
            }

            // Verify every value in the pool
            for (i, (&expected, ct)) in plain.iter().zip(enc.iter()).enumerate() {
                let got = lwe_decrypt(ct, &sk);
                prop_assert_eq!(got, expected, "pool[{}]", i);
            }
        }
    }
}
