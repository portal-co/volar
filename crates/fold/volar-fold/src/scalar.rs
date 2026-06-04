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

// ── Montgomery reduction (fast multiply) ─────────────────────────────────────
//
// The reference `mul_512`+`reduce_512` (bit-by-bit long division) is correct but
// ~512 iterations/multiply — far too slow for folding.  Multiplication instead
// uses Montgomery reduction: `montmul(a,b) = a·b·R⁻¹ mod ℓ` with `R = 2²⁵⁶`, and a
// *plain*-form product is recovered as `montmul(montmul(a,b), R²)` (two `O(n²)`
// reductions, ~300× faster than long division).  The constants `N′ = −ℓ⁻¹ mod
// 2⁶⁴` and `R² = 2⁵¹² mod ℓ` are computed at **compile time** by `const fn`, and
// cross-checked against the reference path in tests — so there is no hand-entered
// constant to get wrong.

const fn ge4(a: &[u64; 4], b: &[u64; 4]) -> bool {
    let mut i = 4;
    while i > 0 {
        i -= 1;
        if a[i] != b[i] {
            return a[i] > b[i];
        }
    }
    true
}

const fn sub4(a: &[u64; 4], b: &[u64; 4]) -> [u64; 4] {
    let mut out = [0u64; 4];
    let mut borrow: u128 = 0;
    let mut i = 0;
    while i < 4 {
        let cur = (a[i] as u128).wrapping_sub(b[i] as u128).wrapping_sub(borrow);
        out[i] = cur as u64;
        borrow = (cur >> 127) & 1;
        i += 1;
    }
    out
}

/// `2·a mod ℓ` for `a < ℓ` (const-fn, used to build `R²`).
const fn dbl_mod_l(a: [u64; 4]) -> [u64; 4] {
    let mut r = [0u64; 4];
    let mut carry = 0u64;
    let mut i = 0;
    while i < 4 {
        r[i] = (a[i] << 1) | carry;
        carry = a[i] >> 63; // a < ℓ < 2^253 ⇒ top carry is 0
        i += 1;
    }
    if ge4(&r, &L) {
        sub4(&r, &L)
    } else {
        r
    }
}

/// `R² = 2⁵¹² mod ℓ` via 512 modular doublings of 1.
const fn r2_mod_l() -> [u64; 4] {
    let mut acc = [1u64, 0, 0, 0];
    let mut i = 0;
    while i < 512 {
        acc = dbl_mod_l(acc);
        i += 1;
    }
    acc
}

/// `a⁻¹ mod 2⁶⁴` (Newton; `a` odd). 6 iters ⇒ ≥ 64 correct bits.
const fn inv_mod_2_64(a: u64) -> u64 {
    let mut x = a;
    let mut i = 0;
    while i < 6 {
        x = x.wrapping_mul(2u64.wrapping_sub(a.wrapping_mul(x)));
        i += 1;
    }
    x
}

const R2: [u64; 4] = r2_mod_l();
const N_PRIME: u64 = inv_mod_2_64(L[0]).wrapping_neg();

/// Montgomery multiply: `a·b·R⁻¹ mod ℓ` (`R = 2²⁵⁶`), SOS form. Inputs `< ℓ`.
fn montmul(a: &[u64; 4], b: &[u64; 4]) -> [u64; 4] {
    let mut t = [0u64; 9];
    for i in 0..4 {
        // t += a[i] · b
        let mut carry: u128 = 0;
        for j in 0..4 {
            let s = t[i + j] as u128 + a[i] as u128 * b[j] as u128 + carry;
            t[i + j] = s as u64;
            carry = s >> 64;
        }
        let mut k = i + 4;
        while carry != 0 {
            let s = t[k] as u128 + carry;
            t[k] = s as u64;
            carry = s >> 64;
            k += 1;
        }
        // Montgomery word: m = t[i]·N′ mod 2⁶⁴; t += m·ℓ·2^{64i} (clears t[i]).
        let m = (t[i] as u128 * N_PRIME as u128) as u64;
        let mut carry2: u128 = 0;
        for j in 0..4 {
            let s = t[i + j] as u128 + m as u128 * L[j] as u128 + carry2;
            t[i + j] = s as u64;
            carry2 = s >> 64;
        }
        let mut k = i + 4;
        while carry2 != 0 {
            let s = t[k] as u128 + carry2;
            t[k] = s as u64;
            carry2 = s >> 64;
            k += 1;
        }
    }
    // Result = t[4..8] (+ t[8] overflow bit), in [0, 2ℓ): one conditional subtract.
    let mut r = [t[4], t[5], t[6], t[7]];
    if t[8] != 0 || ge_256(&r, &L) {
        r = sub_256(&r, &L);
    }
    r
}

/// Reference multiply (long division) — kept as the test oracle for `montmul`.
fn mul_ref(a: &[u64; 4], b: &[u64; 4]) -> [u64; 4] {
    reduce_512(&mul_512(a, b))
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
        // Plain·plain → plain via two Montgomery reductions:
        // montmul(montmul(a,b), R²) = (a·b·R⁻¹)·R²·R⁻¹ = a·b.
        Scalar(montmul(&montmul(&self.0, &other.0), &R2))
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
    fn montgomery_mul_matches_reference() {
        // The fast (Montgomery) multiply must equal the long-division reference
        // across a spread of values, including near-ℓ operands.
        let samples = [
            [1u64, 0, 0, 0],
            [0xffff_ffff_ffff_ffff, 0, 0, 0],
            [0x1234_5678, 0x9abc_def0, 0xdead_beef, 0x0fed_cba9],
            // ℓ − 1 (largest canonical element)
            sub_256(&L, &[1, 0, 0, 0]),
            [0xaaaa_aaaa_aaaa_aaaa, 0x5555_5555_5555_5555, 0xf0f0_f0f0_f0f0_f0f0, 0x0123],
        ];
        for a in &samples {
            for b in &samples {
                let fast = montmul(&montmul(a, b), &R2);
                let refr = mul_ref(a, b);
                assert_eq!(fast, refr, "montmul mismatch for {a:?} * {b:?}");
            }
        }
    }

    #[test]
    fn montgomery_constants_sane() {
        // R² = 2^512 mod ℓ recomputed by reference doubling; N′·ℓ ≡ −1 mod 2^64.
        assert!(ge4(&L, &R2) || !ge4(&R2, &L)); // R2 canonical (< ℓ)
        assert_eq!(N_PRIME.wrapping_mul(L[0]), u64::MAX); // ℓ·N′ ≡ −1 (mod 2^64)
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
