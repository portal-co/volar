// @reliability: experimental
//! @ai: assisted
//! Abstract prime-order group used by the Chou-Orlandi base OT.
//!
//! The trait is deliberately minimal: scalar mul, group law, hashing of
//! elements. Real instantiations should be a curve where the
//! Computational Diffie-Hellman / DDH assumption holds.
//!
//! [`ToyGroup`] is provided **for tests only**. It uses Z*_p over a
//! 31-bit Mersenne prime — protocol-correctness suffices, but DLP is
//! trivial so it MUST NOT be used outside tests.

use digest::Digest;

use crate::SpecRng;

/// Prime-order(-ish) group abstraction for OT protocols.
pub trait Group {
    /// Group element type. Must support equality for protocol checks.
    type Element: Clone + PartialEq;
    /// Scalar type — exponents / discrete-log values.
    type Scalar: Clone;

    /// Canonical generator `g`.
    fn generator() -> Self::Element;
    /// Sample a random scalar in `[0, q)` where `q` is the group order.
    fn random_scalar<R: SpecRng>(rng: &mut R) -> Self::Scalar;
    /// `elt^k` (multiplicative notation) / `k · elt` (additive).
    fn scalar_mul(elt: &Self::Element, k: &Self::Scalar) -> Self::Element;
    /// Group law `a · b` (multiplicative) / `a + b` (additive).
    fn add(a: &Self::Element, b: &Self::Element) -> Self::Element;
    /// Group inverse `a^{-1}` / `-a`.
    fn neg(a: &Self::Element) -> Self::Element;
    /// Absorb the canonical encoding of `elt` into a digest. Used to
    /// derive symmetric keys from group elements (random-oracle model).
    fn write_element<D: Digest>(elt: &Self::Element, h: &mut D);
}

/// Scalar arithmetic (mod the group order) — needed for the Schnorr-style
/// consistency proofs that make the base OT malicious-secure.
///
/// Separate from [`Group`] so existing `Group` impls that never run the
/// malicious-secure proofs don't have to provide scalar arithmetic.
pub trait ScalarOps: Group {
    /// Additive identity `0`.
    fn scalar_zero() -> Self::Scalar;
    /// `a + b` (mod the group order).
    fn scalar_add(a: &Self::Scalar, b: &Self::Scalar) -> Self::Scalar;
    /// `a − b` (mod the group order).
    fn scalar_sub(a: &Self::Scalar, b: &Self::Scalar) -> Self::Scalar;
    /// `a · b` (mod the group order).
    fn scalar_mul_scalar(a: &Self::Scalar, b: &Self::Scalar) -> Self::Scalar;
    /// Scalar equality.
    fn scalar_eq(a: &Self::Scalar, b: &Self::Scalar) -> bool;
    /// Fiat–Shamir challenge: hash a finalized digest to a scalar (mod the
    /// group order). Random-oracle model.
    fn scalar_from_hash<D: Digest>(h: D) -> Self::Scalar;
}

impl ScalarOps for ToyGroup {
    fn scalar_zero() -> u64 {
        0
    }
    fn scalar_add(a: &u64, b: &u64) -> u64 {
        let q = TOY_P - 1;
        (a % q + b % q) % q
    }
    fn scalar_sub(a: &u64, b: &u64) -> u64 {
        let q = TOY_P - 1;
        (a % q + q - b % q) % q
    }
    fn scalar_mul_scalar(a: &u64, b: &u64) -> u64 {
        let q = TOY_P - 1;
        ((a % q) * (b % q)) % q
    }
    fn scalar_eq(a: &u64, b: &u64) -> bool {
        a % (TOY_P - 1) == b % (TOY_P - 1)
    }
    fn scalar_from_hash<D: Digest>(h: D) -> u64 {
        let out = h.finalize();
        let mut le = [0u8; 8];
        le.copy_from_slice(&out[..8]);
        u64::from_le_bytes(le) % (TOY_P - 1)
    }
}

// ============================================================================
// ToyGroup — Z*_p mod (2^31 - 1)
//
// Multiplicative group of integers mod the Mersenne prime p = 2^31 - 1.
// Generator g = 7. Order p - 1 = 2·3²·7·11·31·151·331 (NOT prime — this
// group has small subgroups so DDH does NOT hold).
//
// USE FOR TESTS ONLY. Cryptographically insecure. The OT protocol's
// CORRECTNESS is independent of group security, so this lets us
// unit-test Chou-Orlandi end-to-end without curve arithmetic.
// ============================================================================

/// Modulus for [`ToyGroup`]. Mersenne prime 2^31 − 1 = 2147483647.
pub const TOY_P: u64 = 0x7FFF_FFFF;
/// Generator of [`ToyGroup`].
pub const TOY_G: u64 = 7;

/// Element of [`ToyGroup`] — an integer in `[1, TOY_P)`.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct ToyElement(pub u64);

#[inline]
fn toy_mul(a: u64, b: u64) -> u64 {
    // a, b < 2^31, product < 2^62, fits in u64 without overflow.
    (a * b) % TOY_P
}

fn toy_pow(base: u64, mut exp: u64) -> u64 {
    let mut acc: u64 = 1;
    let mut b = base % TOY_P;
    while exp > 0 {
        if exp & 1 == 1 {
            acc = toy_mul(acc, b);
        }
        b = toy_mul(b, b);
        exp >>= 1;
    }
    acc
}

/// Test-only group. **Insecure**.
pub struct ToyGroup;

impl Group for ToyGroup {
    type Element = ToyElement;
    type Scalar = u64;

    fn generator() -> ToyElement {
        ToyElement(TOY_G)
    }

    fn random_scalar<R: SpecRng>(rng: &mut R) -> u64 {
        // 64-bit value reduced into [0, p-1).
        let lo = rng.next_u32() as u64;
        let hi = rng.next_u32() as u64;
        ((hi << 32) | lo) % (TOY_P - 1)
    }

    fn scalar_mul(elt: &ToyElement, k: &u64) -> ToyElement {
        ToyElement(toy_pow(elt.0, *k))
    }

    fn add(a: &ToyElement, b: &ToyElement) -> ToyElement {
        ToyElement(toy_mul(a.0, b.0))
    }

    fn neg(a: &ToyElement) -> ToyElement {
        // Fermat inverse: a^{p-2} mod p.
        ToyElement(toy_pow(a.0, TOY_P - 2))
    }

    fn write_element<D: Digest>(elt: &ToyElement, h: &mut D) {
        h.update(elt.0.to_le_bytes());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generator_has_nontrivial_order() {
        // g^(p-1) ≡ 1 (mod p) by Fermat's little theorem.
        let g = ToyGroup::generator();
        let one = ToyGroup::scalar_mul(&g, &(TOY_P - 1));
        assert_eq!(one, ToyElement(1));
    }

    #[test]
    fn neg_inverts() {
        let g = ToyGroup::generator();
        let prod = ToyGroup::add(&g, &ToyGroup::neg(&g));
        assert_eq!(prod, ToyElement(1));
    }

    #[test]
    fn dh_correct() {
        // (g^x)^y == (g^y)^x
        let x: u64 = 12345;
        let y: u64 = 67890;
        let g = ToyGroup::generator();
        let gx = ToyGroup::scalar_mul(&g, &x);
        let gy = ToyGroup::scalar_mul(&g, &y);
        let gxy_a = ToyGroup::scalar_mul(&gx, &y);
        let gxy_b = ToyGroup::scalar_mul(&gy, &x);
        assert_eq!(gxy_a, gxy_b);
    }
}
