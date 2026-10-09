// @reliability: experimental
// @experimental-status: design
// @experimental-since: bitgc-phase-1
//! @ai: assisted
//! NTT-friendly prime-modulus ring arithmetic for BitGC SIMD SWHE.
//!
//! The ring is `Z_q[X] / (X^degree + 1)` for a prime `q` with
//! `(q - 1) % (2 * degree) == 0`, so a negacyclic NTT exists. Following
//! [ZLL+26] §3, elements are manipulated in **evaluation (slot) form**:
//! pointwise addition and multiplication of slot vectors are the SIMD
//! operations the BitGC constructions are built from. The coefficient
//! form is exposed only through [`Ring::to_coeffs`] /
//! [`Ring::from_coeffs`] for noise addition, modulus switching, and
//! decryption-style rounding.
//!
//! The arithmetic helpers are adapted from the sibling
//! `tinylabels/ring_lwe.rs` implementation (same workspace, same
//! conventions: `u128`-widened `mul_mod`, rejection-sampled uniforms,
//! iterative bit-reversed NTT). Per ADR 0003 this crate imports no
//! external arithmetic library.
//!
//! This is an experimental mathematical implementation: constant-time
//! behavior is not a goal of the spec crate, and no parameter set here is
//! a deployment profile.

use alloc::vec::Vec;

/// Ring parameters: negacyclic degree and prime modulus.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Parameters {
    /// The power-of-two polynomial degree `N` of `Z_q[X]/(X^N + 1)`.
    pub degree: usize,
    /// The prime modulus `q`; must satisfy `(q - 1) % (2N) == 0`.
    pub modulus: u64,
}

impl Parameters {
    fn validate(self) -> Result<(), Error> {
        if self.degree < 2 || !self.degree.is_power_of_two() || self.modulus < 3 {
            return Err(Error::InvalidParameters);
        }
        if (self.modulus - 1) % (2 * self.degree as u64) != 0 {
            return Err(Error::InvalidParameters);
        }
        Ok(())
    }
}

/// A ring-arithmetic error.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Error {
    /// Degree or modulus cannot support the required negacyclic NTT.
    InvalidParameters,
    /// A slice has the wrong length for this ring.
    LengthMismatch,
    /// A required modular inverse or root of unity does not exist.
    Arithmetic,
    /// The caller's random source failed.
    Randomness,
}

/// A ring element in evaluation (slot) form.
///
/// The slot vector is the element's negacyclic-NTT image; pointwise
/// slot operations are ring operations. This is a typed in-memory value,
/// not a wire format.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Poly {
    slots: Vec<u64>,
}

impl Poly {
    /// Borrow the evaluation-form slot vector.
    pub fn slots(&self) -> &[u64] {
        &self.slots
    }
}

/// A ring instance with precomputed NTT tables.
#[derive(Clone, Debug)]
pub struct Ring {
    parameters: Parameters,
    ntt: Ntt,
}

impl Ring {
    /// Build the ring, verifying NTT-friendliness and finding a negacyclic
    /// root of unity.
    pub fn new(parameters: Parameters) -> Result<Self, Error> {
        parameters.validate()?;
        Ok(Self {
            ntt: Ntt::new(parameters.degree, parameters.modulus)?,
            parameters,
        })
    }

    /// Return the ring parameters.
    pub const fn parameters(&self) -> Parameters {
        self.parameters
    }

    /// The zero element.
    pub fn zero(&self) -> Poly {
        Poly {
            slots: alloc::vec![0; self.parameters.degree],
        }
    }

    /// The ring element whose every slot is `value mod q`.
    pub fn constant(&self, value: u64) -> Poly {
        Poly {
            slots: alloc::vec![value % self.parameters.modulus; self.parameters.degree],
        }
    }

    /// Build an element directly from slot values (reduced mod `q`).
    pub fn from_slots(&self, slots: &[u64]) -> Result<Poly, Error> {
        if slots.len() != self.parameters.degree {
            return Err(Error::LengthMismatch);
        }
        let q = self.parameters.modulus;
        Ok(Poly {
            slots: slots.iter().map(|&v| v % q).collect(),
        })
    }

    /// A uniformly random element, rejection-sampled from `random`.
    pub fn uniform(
        &self,
        random: &mut impl FnMut(&mut [u8]) -> Result<(), Error>,
    ) -> Result<Poly, Error> {
        let mut value = self.zero();
        sample_uniform(random, self.parameters.modulus, &mut value.slots)?;
        Ok(value)
    }

    /// Decode to coefficient form (inverse NTT).
    pub fn to_coeffs(&self, value: &Poly) -> Vec<u64> {
        let mut coeffs = value.slots.clone();
        self.ntt.inverse(&mut coeffs);
        coeffs
    }

    /// Encode from coefficient form (forward NTT).
    pub fn from_coeffs(&self, coeffs: &[u64]) -> Result<Poly, Error> {
        if coeffs.len() != self.parameters.degree {
            return Err(Error::LengthMismatch);
        }
        let mut slots = coeffs.to_vec();
        self.ntt.forward(&mut slots);
        Ok(Poly { slots })
    }

    /// Ring addition.
    pub fn add(&self, left: &Poly, right: &Poly) -> Poly {
        let mut out = left.clone();
        self.add_assign(&mut out, right);
        out
    }

    /// Ring subtraction.
    pub fn sub(&self, left: &Poly, right: &Poly) -> Poly {
        let mut out = left.clone();
        self.sub_assign(&mut out, right);
        out
    }

    /// Ring multiplication (negacyclic convolution in coefficient form;
    /// pointwise in slot form).
    pub fn mul(&self, left: &Poly, right: &Poly) -> Poly {
        let q = self.parameters.modulus;
        Poly {
            slots: left
                .slots
                .iter()
                .zip(right.slots.iter())
                .map(|(&l, &r)| mul_mod(l, r, q))
                .collect(),
        }
    }

    /// Multiply by a scalar reduced mod `q`.
    pub fn scalar_mul(&self, value: &Poly, scalar: u64) -> Poly {
        let q = self.parameters.modulus;
        let scalar = scalar % q;
        Poly {
            slots: value.slots.iter().map(|&v| mul_mod(v, scalar, q)).collect(),
        }
    }

    /// In-place ring addition.
    pub fn add_assign(&self, left: &mut Poly, right: &Poly) {
        let q = self.parameters.modulus;
        for (l, &r) in left.slots.iter_mut().zip(right.slots.iter()) {
            *l = add_mod(*l, r, q);
        }
    }

    /// In-place ring subtraction.
    pub fn sub_assign(&self, left: &mut Poly, right: &Poly) {
        let q = self.parameters.modulus;
        for (l, &r) in left.slots.iter_mut().zip(right.slots.iter()) {
            *l = sub_mod(*l, r, q);
        }
    }

    /// Gadget-decompose each coefficient of `value` (read in coefficient
    /// form) into `digits` base-`base` limbs, returning one coefficient-form
    /// vector per digit position (little-endian: `digits[0]` is the units).
    ///
    /// This is the GSW/RLWE' gadget used by the DD primitive: it lets a
    /// party combine a label with a ciphertext's "extended" gadget form.
    /// Requires `base^digits > modulus`.
    pub fn gadget_decompose_coeffs(
        &self,
        coeffs: &[u64],
        base: u64,
        digits: usize,
    ) -> Result<Vec<Vec<u64>>, Error> {
        if coeffs.len() != self.parameters.degree || !base.is_power_of_two() {
            return Err(Error::LengthMismatch);
        }
        let mut out = alloc::vec![alloc::vec![0_u64; self.parameters.degree]; digits];
        let bits = base.trailing_zeros() as usize;
        for (i, &c) in coeffs.iter().enumerate() {
            for d in 0..digits {
                out[d][i] = (c >> (bits * d)) & (base - 1);
            }
        }
        Ok(out)
    }

    /// In-place additive negation.
    pub fn negate_assign(&self, value: &mut Poly) {
        let q = self.parameters.modulus;
        for v in value.slots.iter_mut() {
            if *v != 0 {
                *v = q - *v;
            }
        }
    }
}

pub(crate) fn add_mod(left: u64, right: u64, modulus: u64) -> u64 {
    // u128 widening: correct for all moduli, unlike the carry trick.
    ((left as u128 + right as u128) % modulus as u128) as u64
}

pub(crate) fn sub_mod(left: u64, right: u64, modulus: u64) -> u64 {
    if left >= right {
        left - right
    } else {
        modulus - (right - left)
    }
}

pub(crate) fn mul_mod(left: u64, right: u64, modulus: u64) -> u64 {
    ((left as u128 * right as u128) % modulus as u128) as u64
}

pub(crate) fn pow_mod(base: u64, mut exponent: usize, modulus: u64) -> u64 {
    let mut result = 1_u64;
    let mut base = base % modulus;
    while exponent > 0 {
        if exponent & 1 == 1 {
            result = mul_mod(result, base, modulus);
        }
        base = mul_mod(base, base, modulus);
        exponent >>= 1;
    }
    result
}

pub(crate) fn inverse_mod(value: u64, modulus: u64) -> Option<u64> {
    let (mut t, mut new_t) = (0_i128, 1_i128);
    let (mut r, mut new_r) = (modulus as i128, (value % modulus) as i128);
    while new_r != 0 {
        let quotient = r / new_r;
        (t, new_t) = (new_t, t - quotient * new_t);
        (r, new_r) = (new_r, r - quotient * new_r);
    }
    if r != 1 {
        return None;
    }
    let mut t = t % modulus as i128;
    if t < 0 {
        t += modulus as i128;
    }
    Some(t as u64)
}

/// Centered lift of a residue: map `v mod q` into `(-q/2, q/2]`.
pub(crate) fn center(value: u64, modulus: u64) -> i128 {
    let value = value % modulus;
    if value > modulus / 2 {
        value as i128 - modulus as i128
    } else {
        value as i128
    }
}

pub(crate) fn sample_uniform(
    random: &mut impl FnMut(&mut [u8]) -> Result<(), Error>,
    modulus: u64,
    output: &mut [u64],
) -> Result<(), Error> {
    let threshold = u64::MAX - u64::MAX % modulus;
    for value in output {
        loop {
            let mut bytes = [0_u8; 8];
            random(&mut bytes)?;
            let candidate = u64::from_le_bytes(bytes);
            if candidate < threshold {
                *value = candidate % modulus;
                break;
            }
        }
    }
    Ok(())
}

#[derive(Clone, Debug)]
pub(crate) struct Ntt {
    modulus: u64,
    degree: usize,
    psi: u64,
    omega: u64,
    inverse_psi: u64,
    inverse_omega: u64,
    inverse_degree: u64,
}

impl Ntt {
    pub(crate) fn new(degree: usize, modulus: u64) -> Result<Self, Error> {
        // Necessary condition for a negacyclic NTT of length `degree`:
        // `modulus ≡ 1 (mod 2·degree)`. Fail fast instead of scanning
        // `2..modulus` for a root that cannot exist.
        if degree == 0 || !degree.is_power_of_two() || (modulus - 1) % (2 * degree as u64) != 0 {
            return Err(Error::Arithmetic);
        }
        let psi = find_negacyclic_root(degree, modulus).ok_or(Error::Arithmetic)?;
        let omega = mul_mod(psi, psi, modulus);
        Ok(Self {
            modulus,
            degree,
            psi,
            omega,
            inverse_psi: inverse_mod(psi, modulus).ok_or(Error::Arithmetic)?,
            inverse_omega: inverse_mod(omega, modulus).ok_or(Error::Arithmetic)?,
            inverse_degree: inverse_mod(degree as u64, modulus).ok_or(Error::Arithmetic)?,
        })
    }

    fn forward(&self, values: &mut [u64]) {
        for (index, value) in values.iter_mut().enumerate() {
            *value = mul_mod(*value, pow_mod(self.psi, index, self.modulus), self.modulus);
        }
        self.cyclic(values, self.omega);
    }

    fn inverse(&self, values: &mut [u64]) {
        self.cyclic(values, self.inverse_omega);
        for (index, value) in values.iter_mut().enumerate() {
            *value = mul_mod(*value, self.inverse_degree, self.modulus);
            *value = mul_mod(
                *value,
                pow_mod(self.inverse_psi, index, self.modulus),
                self.modulus,
            );
        }
    }

    fn cyclic(&self, values: &mut [u64], root: u64) {
        debug_assert_eq!(values.len(), self.degree);
        bit_reverse(values);
        let mut length = 2;
        while length <= self.degree {
            let step = pow_mod(root, self.degree / length, self.modulus);
            for start in (0..self.degree).step_by(length) {
                let mut twiddle = 1;
                for offset in 0..length / 2 {
                    let left = values[start + offset];
                    let right = mul_mod(values[start + offset + length / 2], twiddle, self.modulus);
                    values[start + offset] = add_mod(left, right, self.modulus);
                    values[start + offset + length / 2] = sub_mod(left, right, self.modulus);
                    twiddle = mul_mod(twiddle, step, self.modulus);
                }
            }
            length *= 2;
        }
    }
}

fn bit_reverse(values: &mut [u64]) {
    let bits = values.len().ilog2();
    for index in 0..values.len() {
        let reversed = index.reverse_bits() >> (usize::BITS - bits);
        if index < reversed {
            values.swap(index, reversed);
        }
    }
}

pub(crate) fn find_negacyclic_root(degree: usize, modulus: u64) -> Option<u64> {
    let exponent = (modulus - 1) / (2 * degree as u64);
    for candidate in 2..modulus {
        let root = pow_mod(candidate, exponent as usize, modulus);
        if pow_mod(root, degree, modulus) == modulus - 1 {
            return Some(root);
        }
    }
    None
}

/// Negacyclic convolution via NTT: `a · b mod (X^N + 1, q)` where the
/// modulus is NTT-friendly (`q ≡ 1 mod 2N`). `O(N log N)`.
///
/// Equivalent to [`negacyclic_mul_coeffs`] but requires the [`Ntt`]
/// context. Returns coefficients in `[0, q)`.
pub(crate) fn negacyclic_mul_ntt(a: &[u64], b: &[u64], ntt: &Ntt) -> Vec<u64> {
    debug_assert_eq!(a.len(), ntt.degree);
    debug_assert_eq!(b.len(), ntt.degree);
    let mut fa = a.to_vec();
    let mut fb = b.to_vec();
    ntt.forward(&mut fa);
    ntt.forward(&mut fb);
    let q = ntt.modulus;
    for (x, &y) in fa.iter_mut().zip(fb.iter()) {
        *x = mul_mod(*x, y, q);
    }
    ntt.inverse(&mut fa);
    fa
}

/// Schoolbook negacyclic convolution `a · b mod (X^N + 1, p)` on
/// coefficient vectors.
///
/// The NTT path in [`Ring`] requires an NTT-friendly modulus; this helper
/// works for any `p` and is the reference for the BitGC label layer's
/// ring products (DD/HSS terms over `R_p`). `O(N²)`, reference-grade.
pub fn negacyclic_mul_coeffs(a: &[u64], b: &[u64], modulus: u64) -> Vec<u64> {
    assert_eq!(a.len(), b.len(), "negacyclic_mul: degree mismatch");
    let n = a.len();
    let mut out = alloc::vec![0_u64; n];
    for (i, &ai) in a.iter().enumerate() {
        for (j, &bj) in b.iter().enumerate() {
            let k = i + j;
            if k < n {
                out[k] = add_mod(out[k], mul_mod(ai, bj, modulus), modulus);
            } else {
                out[k - n] = sub_mod(out[k - n], mul_mod(ai, bj, modulus), modulus);
            }
        }
    }
    out
}

/// Coefficient-wise modular subtraction on vectors.
pub fn sub_coeffs(a: &[u64], b: &[u64], modulus: u64) -> Vec<u64> {
    a.iter()
        .zip(b)
        .map(|(&x, &y)| sub_mod(x, y, modulus))
        .collect()
}

/// Coefficient-wise modular addition on vectors.
pub fn add_coeffs(a: &[u64], b: &[u64], modulus: u64) -> Vec<u64> {
    a.iter()
        .zip(b)
        .map(|(&x, &y)| add_mod(x, y, modulus))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// NTT-friendly test modulus: prime, `(q - 1) % 16 == 0`.
    const TEST_MODULUS: u64 = 1_073_741_857; // ~2^30

    fn test_ring() -> Ring {
        Ring::new(Parameters {
            degree: 8,
            modulus: TEST_MODULUS,
        })
        .unwrap()
    }

    #[test]
    fn ntt_round_trip() {
        let ring = test_ring();
        let coeffs: Vec<u64> = (0..8)
            .map(|i| (i as u64) * 123_456_789 % TEST_MODULUS)
            .collect();
        let value = ring.from_coeffs(&coeffs).unwrap();
        assert_eq!(ring.to_coeffs(&value), coeffs);
    }

    #[test]
    fn slot_mul_is_negacyclic_convolution() {
        let ring = test_ring();
        let a: Vec<u64> = (0..8).map(|i| (3 * i + 1) as u64).collect();
        let b: Vec<u64> = (0..8).map(|i| (5 * i + 2) as u64).collect();
        let pa = ring.from_coeffs(&a).unwrap();
        let pb = ring.from_coeffs(&b).unwrap();
        let product = ring.mul(&pa, &pb);
        let coeffs = ring.to_coeffs(&product);
        // Reference negacyclic convolution.
        let q = TEST_MODULUS;
        let mut expected = alloc::vec![0_u64; 8];
        for (i, &ai) in a.iter().enumerate() {
            for (j, &bj) in b.iter().enumerate() {
                let k = i + j;
                if k < 8 {
                    expected[k] = add_mod(expected[k], mul_mod(ai, bj, q), q);
                } else {
                    expected[k - 8] = sub_mod(expected[k - 8], mul_mod(ai, bj, q), q);
                }
            }
        }
        assert_eq!(coeffs, expected);
    }

    #[test]
    fn add_sub_negate() {
        let ring = test_ring();
        let a = ring.from_slots(&[1, 2, 3, 4, 5, 6, 7, 8]).unwrap();
        let b = ring.from_slots(&[8, 7, 6, 5, 4, 3, 2, 1]).unwrap();
        let sum = ring.add(&a, &b);
        assert!(sum.slots().iter().all(|&v| v == 9));
        let diff = ring.sub(&sum, &b);
        assert_eq!(diff, a);
        let mut neg = a.clone();
        ring.negate_assign(&mut neg);
        let zero = ring.add(&a, &neg);
        assert!(zero.slots().iter().all(|&v| v == 0));
    }

    #[test]
    fn add_mod_near_overflow() {
        let q = u64::MAX - 58; // odd, exercises the carry path
        assert_eq!(add_mod(q - 1, q - 1, q), q - 2);
        assert_eq!(add_mod(q - 1, 5, q), 4);
        assert_eq!(sub_mod(3, 5, q), q - 2);
    }

    #[test]
    fn inverse_mod_round_trip() {
        let q = TEST_MODULUS;
        for v in [2_u64, 3, 123_457, q - 1] {
            assert_eq!(mul_mod(v, inverse_mod(v, q).unwrap(), q), 1);
        }
        assert_eq!(inverse_mod(0, q), None);
    }
}
