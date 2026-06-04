// @reliability: experimental
// @ai: assisted
//! The Ed25519 **scalar field** `F_ℓ`, where
//! `ℓ = 2^252 + 27742317777372353535851937790883648493` is the prime order of
//! the Ed25519 group.
//!
//! `volar_spec::curve` provides only the *base* field `Fe25519` (mod `2^255−19`,
//! cheap reduction) and the group; folding R1CS arithmetic lives over the
//! *scalar* field, which has no special-form modulus.  This is a **reference**
//! implementation: modular reduction of the 512-bit product uses obviously-correct
//! bit-by-bit long division (`O(512)` per multiply).  Correct, not fast — a
//! Barrett/Montgomery path is a later optimization (the API is unaffected).

// Arithmetic is exposed as inherent methods (`.add`, `.sub`, `.mul`, `.neg`,
// `.invert`) rather than `core::ops` operators, to avoid method-name ambiguity
// and keep `no_std` callers explicit.

/// `ℓ` as little-endian `u64` limbs.
const L: [u64; 4] = [
    0x5812631a5cf5d3ed,
    0x14def9dea2f79cd6,
    0x0000000000000000,
    0x1000000000000000,
];

/// An element of `F_ℓ`, kept canonical (`0 ≤ x < ℓ`) as little-endian limbs.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub struct Scalar(pub [u64; 4]);

// ── 256-bit limb helpers ─────────────────────────────────────────────────────

/// `a >= b` over 256-bit little-endian limbs.
fn ge_256(a: &[u64; 4], b: &[u64; 4]) -> bool {
    for i in (0..4).rev() {
        if a[i] != b[i] {
            return a[i] > b[i];
        }
    }
    true // equal
}

/// `a - b` (assumes `a >= b`), 256-bit.
fn sub_256(a: &[u64; 4], b: &[u64; 4]) -> [u64; 4] {
    let mut out = [0u64; 4];
    let mut borrow = 0u128;
    for i in 0..4 {
        let cur = (a[i] as u128).wrapping_sub(b[i] as u128).wrapping_sub(borrow);
        out[i] = cur as u64;
        borrow = (cur >> 127) & 1; // 1 if it underflowed
    }
    out
}

/// `a + b` (assumes no overflow past 256 bits), 256-bit.
fn add_256_nooverflow(a: &[u64; 4], b: &[u64; 4]) -> [u64; 4] {
    let mut out = [0u64; 4];
    let mut carry = 0u128;
    for i in 0..4 {
        let cur = a[i] as u128 + b[i] as u128 + carry;
        out[i] = cur as u64;
        carry = cur >> 64;
    }
    out
}

/// `r << 1` over 256 bits (top bit dropped — callers guarantee it is 0).
fn shl1_256(r: &[u64; 4]) -> [u64; 4] {
    let mut out = [0u64; 4];
    let mut carry = 0u64;
    for i in 0..4 {
        out[i] = (r[i] << 1) | carry;
        carry = r[i] >> 63;
    }
    out
}

/// Reduce a 512-bit little-endian value mod `ℓ` via bit-by-bit long division.
fn reduce_512(x: &[u64; 8]) -> [u64; 4] {
    let mut r = [0u64; 4];
    for i in (0..512).rev() {
        let bit = (x[i / 64] >> (i % 64)) & 1;
        // r < ℓ < 2^253 ⇒ r << 1 < 2^254 fits in 256 bits.
        r = shl1_256(&r);
        r[0] |= bit;
        if ge_256(&r, &L) {
            r = sub_256(&r, &L);
        }
    }
    r
}

/// Schoolbook `a * b` → 512-bit little-endian product.
///
/// Row-carry form: each step `t = a[i]·b[j] + res[i+j] + carry` is provably
/// `≤ 2^128 − 1`, so the `u128` accumulator never overflows.
fn mul_512(a: &[u64; 4], b: &[u64; 4]) -> [u64; 8] {
    let mut res = [0u64; 8];
    for i in 0..4 {
        let mut carry: u128 = 0;
        for j in 0..4 {
            let t = (a[i] as u128) * (b[j] as u128) + (res[i + j] as u128) + carry;
            res[i + j] = t as u64;
            carry = t >> 64;
        }
        // `res[i+4]` is untouched before this row, so a plain assign is correct.
        res[i + 4] = carry as u64;
    }
    res
}

impl Scalar {
    pub const ZERO: Scalar = Scalar([0, 0, 0, 0]);
    pub const ONE: Scalar = Scalar([1, 0, 0, 0]);

    /// Reduce an arbitrary 256-bit limb array into canonical form.
    pub fn from_limbs(x: [u64; 4]) -> Scalar {
        let mut r = x;
        while ge_256(&r, &L) {
            r = sub_256(&r, &L);
        }
        Scalar(r)
    }

    pub fn from_u64(x: u64) -> Scalar {
        Scalar::from_limbs([x, 0, 0, 0])
    }

    /// Interpret 32 little-endian bytes as an integer, reduced mod `ℓ`.
    pub fn from_bytes_le(bytes: &[u8; 32]) -> Scalar {
        let mut limbs = [0u64; 4];
        for i in 0..4 {
            let mut w = 0u64;
            for j in 0..8 {
                w |= (bytes[i * 8 + j] as u64) << (8 * j);
            }
            limbs[i] = w;
        }
        Scalar::from_limbs(limbs)
    }

    /// Wide reduction of 64 little-endian bytes (e.g. a hash), mod `ℓ`.
    pub fn from_bytes_wide_le(bytes: &[u8; 64]) -> Scalar {
        let mut limbs = [0u64; 8];
        for i in 0..8 {
            let mut w = 0u64;
            for j in 0..8 {
                w |= (bytes[i * 8 + j] as u64) << (8 * j);
            }
            limbs[i] = w;
        }
        Scalar(reduce_512(&limbs))
    }

    /// Canonical 32-byte little-endian encoding.
    pub fn to_bytes_le(&self) -> [u8; 32] {
        let mut out = [0u8; 32];
        for i in 0..4 {
            for j in 0..8 {
                out[i * 8 + j] = (self.0[i] >> (8 * j)) as u8;
            }
        }
        out
    }

    pub fn is_zero(&self) -> bool {
        self.0 == [0, 0, 0, 0]
    }

    pub fn add(&self, other: &Scalar) -> Scalar {
        // a, b < ℓ < 2^253 ⇒ a + b < 2^254, no 256-bit overflow.
        let s = add_256_nooverflow(&self.0, &other.0);
        if ge_256(&s, &L) {
            Scalar(sub_256(&s, &L))
        } else {
            Scalar(s)
        }
    }

    pub fn neg(&self) -> Scalar {
        if self.is_zero() {
            Scalar::ZERO
        } else {
            Scalar(sub_256(&L, &self.0))
        }
    }

    pub fn sub(&self, other: &Scalar) -> Scalar {
        self.add(&other.neg())
    }

    pub fn mul(&self, other: &Scalar) -> Scalar {
        Scalar(reduce_512(&mul_512(&self.0, &other.0)))
    }

    /// Multiplicative inverse via Fermat's little theorem (`a^(ℓ-2)`).
    pub fn invert(&self) -> Scalar {
        // ℓ - 2
        let l_minus_2 = sub_256(&L, &[2, 0, 0, 0]);
        self.pow(&l_minus_2)
    }

    /// `self^exp` (exp little-endian limbs), square-and-multiply.
    pub fn pow(&self, exp: &[u64; 4]) -> Scalar {
        let mut result = Scalar::ONE;
        let mut base = *self;
        for i in 0..256 {
            if (exp[i / 64] >> (i % 64)) & 1 == 1 {
                result = result.mul(&base);
            }
            base = base.mul(&base);
        }
        result
    }
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;

    #[test]
    fn add_sub_roundtrip() {
        let a = Scalar::from_u64(12345);
        let b = Scalar::from_u64(98765);
        assert_eq!(a.add(&b).sub(&b), a);
        assert_eq!(a.sub(&a), Scalar::ZERO);
    }

    #[test]
    fn neg_is_additive_inverse() {
        let a = Scalar::from_u64(7);
        assert_eq!(a.add(&a.neg()), Scalar::ZERO);
        assert_eq!(Scalar::ZERO.neg(), Scalar::ZERO);
    }

    #[test]
    fn mul_matches_small_ints() {
        let a = Scalar::from_u64(123);
        let b = Scalar::from_u64(456);
        assert_eq!(a.mul(&b), Scalar::from_u64(123 * 456));
    }

    #[test]
    fn distributivity() {
        let a = Scalar::from_u64(0xdead_beef);
        let b = Scalar::from_u64(0xfeed_face);
        let c = Scalar::from_u64(0x1234_5678);
        // a·(b+c) == a·b + a·c
        assert_eq!(a.mul(&b.add(&c)), a.mul(&b).add(&a.mul(&c)));
    }

    #[test]
    fn reduction_wraps_at_l() {
        // ℓ ≡ 0, ℓ+1 ≡ 1.
        let l = Scalar::from_limbs(L);
        assert_eq!(l, Scalar::ZERO);
        let l_plus_1 = Scalar::from_limbs(add_256_nooverflow(&L, &[1, 0, 0, 0]));
        assert_eq!(l_plus_1, Scalar::ONE);
    }

    #[test]
    fn inverse_is_reciprocal() {
        for v in [1u64, 2, 3, 7, 0xabcd_1234, 0xffff_ffff_ffff_ff00] {
            let a = Scalar::from_u64(v);
            assert_eq!(a.mul(&a.invert()), Scalar::ONE, "inv of {v}");
        }
    }

    #[test]
    fn bytes_roundtrip() {
        let a = Scalar::from_u64(0x0123_4567_89ab_cdef);
        let bytes = a.to_bytes_le();
        assert_eq!(Scalar::from_bytes_le(&bytes), a);
    }

    #[test]
    fn wide_reduction_consistent() {
        // A 64-byte value equal to (ℓ + 5) embedded low should reduce to 5.
        let mut wide = [0u8; 64];
        let l_plus_5 = add_256_nooverflow(&L, &[5, 0, 0, 0]);
        for i in 0..4 {
            for j in 0..8 {
                wide[i * 8 + j] = (l_plus_5[i] >> (8 * j)) as u8;
            }
        }
        assert_eq!(Scalar::from_bytes_wide_le(&wide), Scalar::from_u64(5));
    }
}
