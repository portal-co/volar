// @reliability: experimental
// @experimental-status: design
// @experimental-since: bitgc-phase-1
//! @ai: assisted
//! Leveled SWHE (BGV-style) over [`super::ring`] for the BitGC offline phase.
//!
//! This module supplies the homomorphic machinery BitGC's offline phase
//! needs ([LWYY25b], ePrint 2024/1988; [ZLL+26], ePrint 2025/1810):
//! encrypt a seed, expand it homomorphically with a low-depth PRG, and
//! assemble per-gate material with a bounded number of multiplication
//! levels — no bootstrapping.
//!
//! # Structure
//!
//! - BGV invariant: a ciphertext `(c0, c1)` at level `l` decrypts as
//!   `c0 + c1·s ≡ m + p·e (mod Q_l)` where `p` is the plaintext modulus
//!   and `Q_l` is the level's coefficient modulus.
//! - Two levels, matching the artifact's offline→online two-context shape
//!   ([ZLL+26-A]): level 1 ("offline") has `Q_1 = q0·q1` (two RNS limbs);
//!   level 0 ("online") has `Q_0 = q0`. [`Context::mod_switch_down`]
//!   performs the exact BGV switch `c' = (c − δ)/q1` with
//!   `δ ≡ c (mod q1)`, `δ ≡ c (mod p)` (CRT), dropping noise by `~q1`.
//!   Longer chains are out of scope for the reference adapter.
//! - SIMD packing follows Smart–Vercauteren: the plaintext modulus `p`
//!   must be NTT-friendly (`p ≡ 1 mod 2N`), and a message's slots are its
//!   negacyclic-NTT image mod `p`. Homomorphic ring multiplication is
//!   then slot-wise multiplication.
//!
//! # Deliberate boundaries
//!
//! - **Not a reviewed HE library.** Experimental mathematical
//!   implementation for structure/correctness validation, in the spirit of
//!   `tinylabels/ring_lwe.rs`. Constant-time behavior, noise-flooding
//!   policy, distributed decryption, and parameter security (the 128-bit
//!   RLWE-KDM claim) are explicitly deferred — BITGC-LEDGER-03.
//! - Caller supplies randomness ([`RandomSource`]) and error samples
//!   ([`NoiseSource`]). [`ZeroNoise`] is for deterministic tests only.
//! - No wire format; typed in-memory values only.

use alloc::vec::Vec;

use super::ring::{self, Poly, Ring};

/// A source of cryptographically secure random bytes.
pub trait RandomSource {
    /// Fill `output` with independent random bytes.
    fn fill_bytes(&mut self, output: &mut [u8]) -> Result<(), Error>;
}

/// A source of bounded, centered integer errors.
pub trait NoiseSource {
    /// Fill `output` with independent samples of absolute value at most
    /// `bound`.
    fn sample(&mut self, bound: u64, output: &mut [i64]) -> Result<(), Error>;
}

/// Deterministic zero error for semantic tests; never for protected data.
#[derive(Clone, Copy, Debug, Default)]
pub struct ZeroNoise;

impl NoiseSource for ZeroNoise {
    fn sample(&mut self, _bound: u64, output: &mut [i64]) -> Result<(), Error> {
        output.fill(0);
        Ok(())
    }
}

/// SWHE errors.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Error {
    /// Parameters are inconsistent, not NTT-friendly, or out of range.
    InvalidParameters,
    /// A slice has the wrong length.
    LengthMismatch,
    /// Ciphertext/key level mismatch.
    LevelMismatch,
    /// Noise overflow during decryption (message unrecoverable).
    Noise,
    /// Noise source produced an out-of-bound sample.
    NoiseSource,
    /// Caller randomness failed.
    Randomness,
    /// Arithmetic failure (missing inverse/root).
    Arithmetic,
}

impl From<ring::Error> for Error {
    fn from(e: ring::Error) -> Self {
        match e {
            ring::Error::InvalidParameters => Self::InvalidParameters,
            ring::Error::LengthMismatch => Self::LengthMismatch,
            ring::Error::Arithmetic => Self::Arithmetic,
            ring::Error::Randomness => Self::Randomness,
        }
    }
}

/// Leveled parameters. `chain` holds one NTT-friendly prime per level,
/// bottom level first; level `l` has coefficient modulus
/// `Q_l = q0 · … · q_l`. Every chain prime must be `≡ 1 mod 2N` (NTT)
/// **and** `≡ 1 mod p` so the BGV modulus switch preserves messages.
#[derive(Clone, Debug)]
pub struct Parameters {
    /// Power-of-two ring degree `N`.
    pub degree: usize,
    /// NTT-friendly plaintext modulus `p` (prime, `p ≡ 1 mod 2N`), used
    /// for SIMD slot packing. Must be distinct from the chain primes.
    pub plaintext_modulus: u64,
    /// RNS chain, bottom prime first; at most two primes (offline+online).
    pub chain: Vec<u64>,
    /// Bound on sampled error magnitude.
    pub noise_bound: u64,
}

impl Parameters {
    fn validate(&self) -> Result<(), Error> {
        if self.degree < 2
            || !self.degree.is_power_of_two()
            || self.chain.is_empty()
            || self.noise_bound == 0
        {
            return Err(Error::InvalidParameters);
        }
        let n2 = 2 * self.degree as u64;
        let p = self.plaintext_modulus;
        if p < 3 || (p - 1) % n2 != 0 {
            return Err(Error::InvalidParameters);
        }
        let mut seen = alloc::vec::Vec::new();
        for &q in &self.chain {
            // NTT-friendly, distinct, and ≡ 1 (mod p) so the BGV modulus
            // switch preserves messages (`q ≡ 1 mod p` keeps
            // `m + p·e` intact through `c' = (c − δ)/q`).
            if q < 3 || (q - 1) % n2 != 0 || (q - 1) % p != 0 || q == p || seen.contains(&q) {
                return Err(Error::InvalidParameters);
            }
            seen.push(q);
        }
        Ok(())
    }
}

/// An RNS ring element at a level: one slot-form [`Poly`] per live prime.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RnsPoly {
    /// One evaluation-form polynomial per live chain prime, bottom first.
    pub(crate) limbs: Vec<Poly>,
}

impl RnsPoly {
    /// Borrow the per-prime limbs (bottom prime first).
    pub fn limbs(&self) -> &[Poly] {
        &self.limbs
    }
}

/// A secret key: small ternary secret coefficients, convertible per level.
#[derive(Clone, Debug)]
pub struct SecretKey {
    coeffs: Vec<i64>,
}

/// Key-switching key for `s² → s` at one level, gadget base `base`.
#[derive(Clone, Debug)]
pub struct KeySwitchKey {
    level: usize,
    base: u64,
    parts: Vec<(RnsPoly, RnsPoly)>,
}

/// A set of key-switching keys, one per level (`keys[l]` for level `l`).
///
/// [`Context::mul`] consumes multiplication depth top-down: it requires
/// the top-level key and returns a ciphertext at `top − 1`, so a chained
/// evaluation of `L` multiplications uses `keys[max..], keys[max−1], …`
/// in order.
#[derive(Clone, Debug)]
pub struct KeySwitchKeySet {
    keys: Vec<KeySwitchKey>,
    base: u64,
}

impl KeySwitchKeySet {
    /// The top level this set covers.
    pub fn max_level(&self) -> usize {
        self.keys.len() - 1
    }

    /// The key for `level`.
    pub fn at(&self, level: usize) -> Option<&KeySwitchKey> {
        self.keys.get(level)
    }

    /// The gadget base.
    pub const fn base(&self) -> u64 {
        self.base
    }
}

/// A ciphertext: `(c0, c1)` of [`RnsPoly`] at a level.
#[derive(Clone, Debug)]
pub struct Ciphertext {
    level: usize,
    parts: Vec<RnsPoly>,
}

impl Ciphertext {
    /// Level of this ciphertext (0 = online/bottom, 1 = offline/top).
    pub const fn level(&self) -> usize {
        self.level
    }

    /// Borrow the parts `(c0, c1)`.
    pub fn parts(&self) -> &[RnsPoly] {
        &self.parts
    }
}

/// Leveled SWHE context.
#[derive(Clone, Debug)]
pub struct Context {
    parameters: Parameters,
    /// One ring per chain prime.
    level_rings: Vec<Ring>,
    /// Ring over the plaintext modulus, for SIMD slot packing.
    plaintext_ring: Ring,
}

impl Context {
    /// Build the context, validating parameters and all rings.
    pub fn new(parameters: Parameters) -> Result<Self, Error> {
        parameters.validate()?;
        let level_rings = parameters
            .chain
            .iter()
            .map(|&q| {
                Ring::new(ring::Parameters {
                    degree: parameters.degree,
                    modulus: q,
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        let plaintext_ring = Ring::new(ring::Parameters {
            degree: parameters.degree,
            modulus: parameters.plaintext_modulus,
        })?;
        Ok(Self {
            parameters,
            level_rings,
            plaintext_ring,
        })
    }

    /// The parameters.
    pub fn parameters(&self) -> &Parameters {
        &self.parameters
    }

    /// A degree-length slot vector whose every slot is `value mod p`.
    pub fn constant_slots(&self, value: u64) -> Vec<u64> {
        alloc::vec![value % self.parameters.plaintext_modulus; self.parameters.degree]
    }

    /// Maximum level (chain length − 1).
    pub fn max_level(&self) -> usize {
        self.parameters.chain.len() - 1
    }

    fn limbs(&self, level: usize) -> usize {
        level + 1
    }

    fn check_level(&self, level: usize) -> Result<(), Error> {
        if level > self.max_level() {
            return Err(Error::InvalidParameters);
        }
        Ok(())
    }

    fn zero(&self, level: usize) -> RnsPoly {
        RnsPoly {
            limbs: self.level_rings[..=level].iter().map(Ring::zero).collect(),
        }
    }

    fn add_rns(&self, left: &RnsPoly, right: &RnsPoly) -> RnsPoly {
        RnsPoly {
            limbs: left
                .limbs
                .iter()
                .zip(right.limbs.iter())
                .enumerate()
                .map(|(i, (l, r))| self.level_rings[i].add(l, r))
                .collect(),
        }
    }

    fn mul_rns(&self, left: &RnsPoly, right: &RnsPoly) -> RnsPoly {
        RnsPoly {
            limbs: left
                .limbs
                .iter()
                .zip(right.limbs.iter())
                .enumerate()
                .map(|(i, (l, r))| self.level_rings[i].mul(l, r))
                .collect(),
        }
    }

    fn scalar_mul_rns(&self, value: &RnsPoly, scalar: u64) -> RnsPoly {
        RnsPoly {
            limbs: value
                .limbs
                .iter()
                .enumerate()
                .map(|(i, l)| self.level_rings[i].scalar_mul(l, scalar))
                .collect(),
        }
    }

    /// Uniform random RNS element at `level`.
    fn uniform_rns(&self, level: usize, random: &mut impl RandomSource) -> Result<RnsPoly, Error> {
        let mut limbs = Vec::with_capacity(self.limbs(level));
        for ring in &self.level_rings[..=level] {
            let mut slots = alloc::vec![0_u64; self.parameters.degree];
            let modulus = ring.parameters().modulus;
            let mut rng = |out: &mut [u8]| -> Result<(), ring::Error> {
                random.fill_bytes(out).map_err(|_| ring::Error::Randomness)
            };
            ring::sample_uniform(&mut rng, modulus, &mut slots)?;
            limbs.push(ring.from_slots(&slots)?);
        }
        Ok(RnsPoly { limbs })
    }

    /// Bounded centered noise as an RNS element (sampled in coefficient
    /// form, transformed per limb).
    fn noise_rns(&self, level: usize, noise: &mut impl NoiseSource) -> Result<RnsPoly, Error> {
        let mut coeffs_i = alloc::vec![0_i64; self.parameters.degree];
        noise.sample(self.parameters.noise_bound, &mut coeffs_i)?;
        if coeffs_i
            .iter()
            .any(|v| v.unsigned_abs() > self.parameters.noise_bound)
        {
            return Err(Error::NoiseSource);
        }
        let mut limbs = Vec::with_capacity(self.limbs(level));
        for ring in &self.level_rings[..=level] {
            let q = ring.parameters().modulus;
            let coeffs: Vec<u64> = coeffs_i
                .iter()
                .map(|&v| {
                    if v < 0 {
                        q - (v.unsigned_abs() % q)
                    } else {
                        v as u64 % q
                    }
                })
                .collect();
            limbs.push(ring.from_coeffs(&coeffs)?);
        }
        Ok(RnsPoly { limbs })
    }

    /// Embed plaintext slot values (`mod p`) as an RNS element at
    /// `level`. Fails at level 0 if `slots` is all-zero: zero needs no
    /// limbs, so the level would be ambiguous. Use
    /// [`Self::constant_slots`] with a nonzero value, or operate on an
    /// existing level-0 ciphertext, when a zero plaintext is needed.
    fn embed(&self, level: usize, slots: &[u64]) -> Result<RnsPoly, Error> {
        if slots.len() != self.parameters.degree {
            return Err(Error::LengthMismatch);
        }
        if level == 0 && slots.iter().all(|&v| v == 0) {
            return Err(Error::InvalidParameters);
        }
        let p = self.parameters.plaintext_modulus;
        if slots.iter().any(|&v| v >= p) {
            return Err(Error::InvalidParameters);
        }
        // Coefficient-form message mod p via the packing NTT.
        let packed = self.plaintext_ring.from_slots(slots)?;
        let m_coeffs = self.plaintext_ring.to_coeffs(&packed);
        let mut limbs = Vec::with_capacity(self.limbs(level));
        for ring in &self.level_rings[..=level] {
            let q = ring.parameters().modulus;
            let coeffs: Vec<u64> = m_coeffs.iter().map(|&c| c % q).collect();
            limbs.push(ring.from_coeffs(&coeffs)?);
        }
        Ok(RnsPoly { limbs })
    }

    /// Sample a ternary secret key.
    pub fn keygen(&self, random: &mut impl RandomSource) -> Result<SecretKey, Error> {
        let mut coeffs = alloc::vec![0_i64; self.parameters.degree];
        let mut byte = [0u8; 1];
        for c in coeffs.iter_mut() {
            random.fill_bytes(&mut byte[..])?;
            *c = (byte[0] % 3) as i64 - 1;
        }
        Ok(SecretKey { coeffs })
    }

    /// Encrypt plaintext slots (`mod p`) at `level` under `sk`.
    pub fn encrypt(
        &self,
        sk: &SecretKey,
        level: usize,
        slots: &[u64],
        random: &mut impl RandomSource,
        noise: &mut impl NoiseSource,
    ) -> Result<Ciphertext, Error> {
        self.check_level(level)?;
        if sk.coeffs.len() != self.parameters.degree {
            return Err(Error::LengthMismatch);
        }
        let p = self.parameters.plaintext_modulus;
        let m = self.embed(level, slots)?;
        let a = self.uniform_rns(level, random)?;
        let e = self.noise_rns(level, noise)?;
        let s = self.secret_rns(level, sk);
        // c0 = a*s + m + p*e ; c1 = -a
        let mut c0 = self.mul_rns(&a, &s);
        let pe = self.scalar_mul_rns(&e, p);
        self.add_assign_rns(&mut c0, &m);
        self.add_assign_rns(&mut c0, &pe);
        let mut c1 = a;
        for (i, limb) in c1.limbs.iter_mut().enumerate() {
            self.level_rings[i].negate_assign(limb);
        }
        Ok(Ciphertext {
            level,
            parts: alloc::vec![c0, c1],
        })
    }

    fn add_assign_rns(&self, left: &mut RnsPoly, right: &RnsPoly) {
        for (i, (l, r)) in left.limbs.iter_mut().zip(right.limbs.iter()).enumerate() {
            self.level_rings[i].add_assign(l, r);
        }
    }

    fn secret_rns(&self, level: usize, sk: &SecretKey) -> RnsPoly {
        let limbs = self.level_rings[..=level]
            .iter()
            .map(|ring| {
                let q = ring.parameters().modulus;
                let coeffs: Vec<u64> = sk
                    .coeffs
                    .iter()
                    .map(|&v| {
                        if v < 0 {
                            q - (v.unsigned_abs() % q)
                        } else {
                            v as u64 % q
                        }
                    })
                    .collect();
                ring.from_coeffs(&coeffs)
                    .expect("degree checked at keygen use")
            })
            .collect();
        RnsPoly { limbs }
    }

    /// Decrypt: recover plaintext slots (`mod p`).
    pub fn decrypt(&self, sk: &SecretKey, ct: &Ciphertext) -> Result<Vec<u64>, Error> {
        if sk.coeffs.len() != self.parameters.degree {
            return Err(Error::LengthMismatch);
        }
        let p = self.parameters.plaintext_modulus;
        let s = self.secret_rns(ct.level, sk);
        // phase = c0 + c1*s, computed on the bottom limb only. The BGV
        // invariant `phase ≡ m + p·e (mod Q)` holds `mod q0` in
        // particular, so `mod p` reduction recovers `m` whenever the
        // noise budget holds; chain primes are chosen `≡ 1 (mod p)` so
        // modulus switching preserves the invariant (see
        // [`Self::mod_switch_down`]).
        let coeffs = self.level_rings[0].to_coeffs(&{
            let ring0 = &self.level_rings[0];
            let mut limb = ring0.mul(&ct.parts[1].limbs[0], &s.limbs[0]);
            ring0.add_assign(&mut limb, &ct.parts[0].limbs[0]);
            limb
        });
        // Center the phase coefficients (they are `m + p·e` with `e`
        // small and centered) before reducing mod `p`; an uncentered
        // `c % p` misreads negative noise as large positive wrap-around.
        let q0 = self.parameters.chain[0];
        let m_coeffs: Vec<u64> = coeffs
            .iter()
            .map(|&c| ring::center(c, q0).rem_euclid(p as i128) as u64)
            .collect();
        let packed = self.plaintext_ring.from_coeffs(&m_coeffs)?;
        Ok(packed.slots().to_vec())
    }

    /// Homomorphic addition. If the operands sit at different levels the
    /// higher one is modulus-switched down first.
    pub fn add(&self, left: &Ciphertext, right: &Ciphertext) -> Result<Ciphertext, Error> {
        let (left_owned, right_owned);
        let mut l = left;
        let mut r = right;
        if l.level > r.level {
            left_owned = self.switch_to(l, r.level)?;
            l = &left_owned;
        } else if r.level > l.level {
            right_owned = self.switch_to(r, l.level)?;
            r = &right_owned;
        }
        let parts = l
            .parts
            .iter()
            .zip(r.parts.iter())
            .map(|(a, b)| self.add_rns(a, b))
            .collect();
        Ok(Ciphertext {
            level: l.level,
            parts,
        })
    }

    /// Subtract right from left (levels aligned as in [`Self::add`]).
    pub fn sub(&self, left: &Ciphertext, right: &Ciphertext) -> Result<Ciphertext, Error> {
        let (left_owned, right_owned);
        let mut l = left;
        let mut r = right;
        if l.level > r.level {
            left_owned = self.switch_to(l, r.level)?;
            l = &left_owned;
        } else if r.level > l.level {
            right_owned = self.switch_to(r, l.level)?;
            r = &right_owned;
        }
        let mut neg = r.clone();
        for part in neg.parts.iter_mut() {
            for (i, limb) in part.limbs.iter_mut().enumerate() {
                self.level_rings[i].negate_assign(limb);
            }
        }
        self.add(l, &neg)
    }

    /// Switch a ciphertext down to `target` level (no-op if already there).
    pub fn switch_to(&self, ct: &Ciphertext, target: usize) -> Result<Ciphertext, Error> {
        let mut out = ct.clone();
        while out.level > target {
            out = self.mod_switch_down(&out)?;
        }
        Ok(out)
    }

    /// Add plaintext slots to a ciphertext.
    pub fn add_plain(&self, ct: &Ciphertext, slots: &[u64]) -> Result<Ciphertext, Error> {
        let m = self.embed(ct.level, slots)?;
        let mut parts = ct.parts.clone();
        self.add_assign_rns(&mut parts[0], &m);
        Ok(Ciphertext {
            level: ct.level,
            parts,
        })
    }

    /// A noiseless encryption of the constant-one plaintext at the same
    /// level as `like` (`c0 = 1`, `c1 = 0` in every limb).
    ///
    /// Unlike `mul_plain(ct, 0)`-style constructions this works at level
    /// 0, where an all-zero plaintext has no level of its own.
    pub fn encrypt_one_like(&self, like: &Ciphertext) -> Result<Ciphertext, Error> {
        let mut one = self.zero(like.level);
        for (i, limb) in one.limbs.iter_mut().enumerate() {
            *limb = self.level_rings[i].constant(1);
        }
        Ok(Ciphertext {
            level: like.level,
            parts: alloc::vec![one, self.zero(like.level)],
        })
    }

    /// Multiply a ciphertext by plaintext slots (pointwise on slots).
    pub fn mul_plain(&self, ct: &Ciphertext, slots: &[u64]) -> Result<Ciphertext, Error> {
        let m = self.embed(ct.level, slots)?;
        let parts = ct.parts.iter().map(|c| self.mul_rns(c, &m)).collect();
        Ok(Ciphertext {
            level: ct.level,
            parts,
        })
    }

    /// Homomorphic multiplication: tensor, relinearize with the
    /// level-matching key of `ksk`, then modulus-switch the product down
    /// one level to keep noise bounded. Operands at different levels are
    /// aligned first (the higher switches down, as in [`Self::add`]); the
    /// result sits one level below the aligned level, or at level 0 when
    /// already at the bottom.
    pub fn mul(
        &self,
        left: &Ciphertext,
        right: &Ciphertext,
        ksk: &KeySwitchKeySet,
    ) -> Result<Ciphertext, Error> {
        let (left_owned, right_owned);
        let mut l = left;
        let mut r = right;
        if l.level > r.level {
            left_owned = self.switch_to(l, r.level)?;
            l = &left_owned;
        } else if r.level > l.level {
            right_owned = self.switch_to(r, l.level)?;
            r = &right_owned;
        }
        let (left, right) = (l, r);
        let level = left.level;
        let ksk = ksk.at(level).ok_or(Error::LevelMismatch)?;
        if ksk.level != level {
            return Err(Error::LevelMismatch);
        }
        // Tensor.
        let t0 = self.mul_rns(&left.parts[0], &right.parts[0]);
        let mut t1 = self.mul_rns(&left.parts[0], &right.parts[1]);
        let t1b = self.mul_rns(&left.parts[1], &right.parts[0]);
        self.add_assign_rns_mut(&mut t1, &t1b);
        let t2 = self.mul_rns(&left.parts[1], &right.parts[1]);

        // Relinearize t2 with the gadget decomposition of its coefficients.
        let mut c0 = t0;
        let mut c1 = t1;
        let t2_coeffs: Vec<Vec<u64>> = t2
            .limbs
            .iter()
            .enumerate()
            .map(|(i, limb)| self.level_rings[i].to_coeffs(limb))
            .collect();
        let base_bits = ksk.base.trailing_zeros() as usize;
        if !ksk.base.is_power_of_two() || ksk.base < 2 {
            return Err(Error::InvalidParameters);
        }
        // Coverage of the gadget is validated at `key_switch_keygen`; digits
        // beyond 64 base-bits contribute zero.
        let _ = base_bits;
        for (digit_index, (neg_a, b)) in ksk.parts.iter().enumerate() {
            let shift = base_bits * digit_index;
            let mut digit = self.zero(level);
            if shift < 64 {
                for (i, ring_i) in self.level_rings[..=level].iter().enumerate() {
                    let coeffs: Vec<u64> = t2_coeffs[i]
                        .iter()
                        .map(|&c| (c >> shift) & (ksk.base - 1))
                        .collect();
                    digit.limbs[i] = ring_i.from_coeffs(&coeffs)?;
                }
            }
            self.add_assign_rns_mut(&mut c0, &self.mul_rns(&digit, b));
            self.add_assign_rns_mut(&mut c1, &self.mul_rns(&digit, neg_a));
        }
        let product = Ciphertext {
            level,
            parts: alloc::vec![c0, c1],
        };
        // Switch down to keep the noise budget bounded. At level 0 there
        // is nowhere to switch to; correctness then relies on the
        // bottom-level modulus having enough headroom.
        if level == 0 {
            Ok(product)
        } else {
            self.mod_switch_down(&product)
        }
    }

    fn add_assign_rns_mut(&self, left: &mut RnsPoly, right: &RnsPoly) {
        self.add_assign_rns(left, right);
    }

    /// Generate a per-level key-switching key set for `sk` (levels
    /// `0..=max_level`). `base` must be a power of two whose digit count
    /// covers 64 base-bits per level.
    pub fn key_switch_keyset(
        &self,
        sk: &SecretKey,
        base: u64,
        digits: usize,
        random: &mut impl RandomSource,
        noise: &mut impl NoiseSource,
    ) -> Result<KeySwitchKeySet, Error> {
        let mut keys = Vec::with_capacity(self.max_level() + 1);
        for level in 0..=self.max_level() {
            keys.push(self.key_switch_keygen(sk, level, base, digits, random, noise)?);
        }
        Ok(KeySwitchKeySet { keys, base })
    }

    /// Generate a key-switching key `s² → s` at `level`. `base` must be a
    /// power of two with `base^digits` exceeding the level's primes.
    pub fn key_switch_keygen(
        &self,
        sk: &SecretKey,
        level: usize,
        base: u64,
        digits: usize,
        random: &mut impl RandomSource,
        noise: &mut impl NoiseSource,
    ) -> Result<KeySwitchKey, Error> {
        self.check_level(level)?;
        if !base.is_power_of_two() || base < 2 || digits == 0 {
            return Err(Error::InvalidParameters);
        }
        // The gadget must cover every prime in the level's chain: digits
        // past 64 base-bits contribute zero, so require coverage of 64
        // bits (u64 limbs).
        if (base.trailing_zeros() as usize) * digits < 64 {
            return Err(Error::InvalidParameters);
        }
        let p = self.parameters.plaintext_modulus;
        let s = self.secret_rns(level, sk);
        let s2 = self.mul_rns(&s, &s);
        let mut parts = Vec::with_capacity(digits);
        for i in 0..digits {
            let a = self.uniform_rns(level, random)?;
            let e = self.noise_rns(level, noise)?;
            // b = a*s + p*e + base^i * s^2 ; store (neg_a, b)
            let mut b = self.mul_rns(&a, &s);
            let pe = self.scalar_mul_rns(&e, p);
            self.add_assign_rns(&mut b, &pe);
            // base^i * s^2, with base^i reduced per limb (no u64 overflow).
            let mut scaled = s2.clone();
            for (j, limb) in scaled.limbs.iter_mut().enumerate() {
                let q = self.level_rings[j].parameters().modulus;
                let factor = ring::pow_mod(base % q, i, q);
                *limb = self.level_rings[j].scalar_mul(limb, factor);
            }
            self.add_assign_rns(&mut b, &scaled);
            let mut neg_a = a;
            for (j, limb) in neg_a.limbs.iter_mut().enumerate() {
                self.level_rings[j].negate_assign(limb);
            }
            parts.push((neg_a, b));
        }
        Ok(KeySwitchKey { level, base, parts })
    }

    /// Exact BGV modulus switch `level → level − 1`, dropping the top
    /// prime `q_l`. Chain primes are all `≡ 1 (mod p)`, so the switch
    /// preserves the plaintext: `c' = (c − δ)/q_l` per coefficient with
    /// `δ ≡ c (mod q_l)`, `δ ≡ 0 (mod p)` (CRT). Noise drops by a factor
    /// of about `q_l` plus an additive `O(p·N)` term.
    ///
    /// Per-limb RNS implementation: `δ` is computed from the top limb
    /// directly (`c mod q_l` is the top limb's coefficient), and each
    /// remaining limb is corrected with `q_l⁻¹ mod q_i`. No full CRT lift
    /// is performed, so chains of any length work.
    pub fn mod_switch_down(&self, ct: &Ciphertext) -> Result<Ciphertext, Error> {
        if ct.level == 0 || ct.level > self.max_level() {
            return Err(Error::LevelMismatch);
        }
        let p = self.parameters.plaintext_modulus;
        let from = ct.level;
        let q_top = self.parameters.chain[from];
        // inv(p, q_top) and inv(q_top, q_i) per remaining limb
        let inv_p_top = ring::inverse_mod(p % q_top, q_top).ok_or(Error::Arithmetic)?;
        let parts = ct
            .parts
            .iter()
            .map(|part| {
                let top_coeffs = self.level_rings[from].to_coeffs(&part.limbs[from]);
                // δ per coefficient: δ ≡ c (mod q_top), δ ≡ 0 (mod p)
                // δ = p * ((c * p⁻¹) mod q_top)
                let deltas: Vec<u64> = top_coeffs
                    .iter()
                    .map(|&c| ring::mul_mod(ring::mul_mod(c, inv_p_top, q_top), 1, q_top))
                    .collect();
                let mut limbs = Vec::with_capacity(from);
                for (i, ring_i) in self.level_rings[..from].iter().enumerate() {
                    let q_i = ring_i.parameters().modulus;
                    let inv_q_top = ring::inverse_mod(q_top % q_i, q_i).ok_or(Error::Arithmetic)?;
                    let ci = ring_i.to_coeffs(&part.limbs[i]);
                    let p_mod_qi = p % q_i;
                    let out: Vec<u64> = ci
                        .iter()
                        .zip(deltas.iter())
                        .map(|(&c, &d)| {
                            // c' = (c − p·δ) · q_top⁻¹  (mod q_i)
                            let pd = ring::mul_mod(p_mod_qi, d, q_i);
                            let diff = ring::sub_mod(c, pd, q_i);
                            ring::mul_mod(diff, inv_q_top, q_i)
                        })
                        .collect();
                    limbs.push(ring_i.from_coeffs(&out)?);
                }
                Ok(RnsPoly { limbs })
            })
            .collect::<Result<Vec<_>, Error>>()?;
        Ok(Ciphertext {
            level: from - 1,
            parts,
        })
    }
}
