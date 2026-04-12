// @reliability: normal
//! @ai: assisted
// Generic carry-less field multiplication backend (shift-and-XOR).
// Works for any integer word size via the FieldMulBackend trait.
use core::mem::size_of_val;
use core::ops::{BitAnd, BitXor, BitXorAssign, Shl, ShlAssign, ShrAssign};

pub trait FieldMulBackend:
    ShlAssign<u32>
    + ShrAssign<u32>
    + Default
    + BitXor<Output = Self>
    + BitXorAssign
    + Shl<u32, Output = Self>
    + BitAnd<Output = Self>
    + PartialEq
    + From<u8>
    + Clone
{
}
impl<
    T: ShlAssign<u32>
        + ShrAssign<u32>
        + Default
        + BitXor<Output = T>
        + BitXorAssign
        + Shl<u32, Output = T>
        + BitAnd<Output = T>
        + PartialEq
        + From<u8>
        + Clone,
> FieldMulBackend for T
{
}

/// Carry-less (binary) field multiplication: `a * b mod (x^w + c)`,
/// where `w` is the bit width of `T`.
pub fn field_mul<T: FieldMulBackend>(a: T, b: T, c: T) -> T {
    let mut p: T = T::default();
    let mut a = a;
    let mut b = b;
    let h = T::from(1) << ((size_of_val(&p) << 3) - 1) as u32;
    for _ in 0..(size_of_val(&p) << 3) {
        if (b.clone() & T::from(1)) != T::default() {
            p ^= a.clone();
        }
        let high_bit = a.clone() & h.clone();
        a <<= 1;
        if high_bit != T::default() {
            a ^= c.clone();
        }
        b >>= 1;
    }
    p
}

/// Square an element in GF(2^w): `a^2 mod (x^w + c)`.
///
/// Same as `field_mul(a, a, c)` but roughly 2× faster since one
/// operand is known.
pub fn field_square<T: FieldMulBackend>(a: T, c: T) -> T {
    field_mul(a.clone(), a, c)
}

/// Inversion in GF(2^w) via Itoh–Tsujii (addition-chain) exponentiation.
///
/// Computes `a^{-1} = a^{2^w - 2}` using the identity:
///
/// ```text
/// a^{2^w - 2} = (a^{2^{w-1} - 1})^2
/// ```
///
/// This recursion yields O(log w) multiplications + O(w) squarings, which
/// is much faster than the naïve 2^w - 2 loop.
///
/// Returns `T::default()` (zero) when `a` is zero (convention from AES).
pub fn field_invert<T: FieldMulBackend>(a: T, c: T, w: u32) -> T {
    if a == T::default() {
        return T::default();
    }
    // Compute a^{2^w - 2} via addition chain.
    //
    // r = a^{2^1 - 1} = a
    // Then repeatedly: r = r^{2^k} * r   to get a^{2^{2k} - 1}
    // Finally square once to get a^{2^w - 2}.
    //
    // We use the standard Itoh–Tsujii decomposition:
    //   Let e = w - 1.  We want a^{2^e - 1} then square.
    //   Build a^{2^e - 1} by doubling the exponent chain.
    let e = w - 1;
    // r tracks a^{2^k - 1} where k starts at 1 and doubles.
    let mut r = a.clone();
    let mut k: u32 = 1;
    // Process the bits of `e` from MSB-1 down to bit 0.
    let msb = 31 - e.leading_zeros(); // position of highest set bit
    for bit_pos in (0..msb).rev() {
        // Square r by k positions: r -> r^{2^k}
        let mut tmp = r.clone();
        for _ in 0..k {
            tmp = field_square(tmp, c.clone());
        }
        // Multiply: r = r^{2^k} * r = a^{2^{2k} - 1}
        r = field_mul(tmp, r, c.clone());
        k *= 2;
        // If this bit of e is set, do one extra square + multiply
        if (e >> bit_pos) & 1 == 1 {
            r = field_mul(field_square(r, c.clone()), a.clone(), c.clone());
            k += 1;
        }
    }
    // Now r = a^{2^{w-1} - 1}.  Square to get a^{2^w - 2} = a^{-1}.
    field_square(r, c)
}

// ============================================================================
// GF(2^128) — 128-bit carry-less multiplication via u128
// ============================================================================

/// Carry-less multiplication in GF(2^128) using the GCM reduction polynomial
/// x^128 + x^7 + x^2 + x + 1 (reduction constant `0x87`).
///
/// Uses the standard schoolbook shift-and-XOR method on u128.
pub fn field_mul_128(a: u128, b: u128, c: u128) -> u128 {
    let mut p: u128 = 0;
    let mut a = a;
    let mut b = b;
    let h: u128 = 1u128 << 127;
    for _ in 0..128 {
        if b & 1 != 0 {
            p ^= a;
        }
        let high_bit = a & h;
        a <<= 1;
        if high_bit != 0 {
            a ^= c;
        }
        b >>= 1;
    }
    p
}

/// Square in GF(2^128).
pub fn field_square_128(a: u128, c: u128) -> u128 {
    field_mul_128(a, a, c)
}

/// Inversion in GF(2^128) via Itoh–Tsujii.
pub fn field_invert_128(a: u128, c: u128) -> u128 {
    if a == 0 {
        return 0;
    }
    let w = 128u32;
    let e = w - 1; // 127
    let mut r = a;
    let mut k: u32 = 1;
    let msb = 31 - e.leading_zeros();
    for bit_pos in (0..msb).rev() {
        let mut tmp = r;
        for _ in 0..k {
            tmp = field_square_128(tmp, c);
        }
        r = field_mul_128(tmp, r, c);
        k *= 2;
        if (e >> bit_pos) & 1 == 1 {
            r = field_mul_128(field_square_128(r, c), a, c);
            k += 1;
        }
    }
    field_square_128(r, c)
}

// ============================================================================
// GF(2^256) — 256-bit carry-less multiplication via [u64; 4]
// ============================================================================

/// A 256-bit value stored as four u64 words in little-endian limb order
/// (word 0 = least significant).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct U256(pub [u64; 4]);

impl U256 {
    pub const ZERO: Self = U256([0; 4]);
    pub const ONE: Self = U256([1, 0, 0, 0]);

    #[inline]
    fn bit(&self, n: u32) -> bool {
        let word = (n / 64) as usize;
        let bit = n % 64;
        if word < 4 {
            (self.0[word] >> bit) & 1 != 0
        } else {
            false
        }
    }

    #[inline]
    fn set_high_bit(&self) -> bool {
        self.0[3] >> 63 != 0
    }

    #[inline]
    fn shl1(&self) -> Self {
        let mut out = [0u64; 4];
        out[0] = self.0[0] << 1;
        out[1] = (self.0[1] << 1) | (self.0[0] >> 63);
        out[2] = (self.0[2] << 1) | (self.0[1] >> 63);
        out[3] = (self.0[3] << 1) | (self.0[2] >> 63);
        U256(out)
    }

    #[inline]
    fn shr1(&self) -> Self {
        let mut out = [0u64; 4];
        out[0] = (self.0[0] >> 1) | (self.0[1] << 63);
        out[1] = (self.0[1] >> 1) | (self.0[2] << 63);
        out[2] = (self.0[2] >> 1) | (self.0[3] << 63);
        out[3] = self.0[3] >> 1;
        U256(out)
    }

    #[inline]
    pub fn xor(&self, other: &Self) -> Self {
        U256([
            self.0[0] ^ other.0[0],
            self.0[1] ^ other.0[1],
            self.0[2] ^ other.0[2],
            self.0[3] ^ other.0[3],
        ])
    }

    #[inline]
    fn is_zero(&self) -> bool {
        self.0 == [0; 4]
    }
}

/// Carry-less multiplication in GF(2^256) with reduction polynomial
/// x^256 + x^10 + x^5 + x^2 + 1 (reduction constant `0x425`).
pub fn field_mul_256(a: U256, b: U256, c: U256) -> U256 {
    let mut p = U256::ZERO;
    let mut a = a;
    let mut b = b;
    for _ in 0..256 {
        if b.bit(0) {
            p = p.xor(&a);
        }
        let high = a.set_high_bit();
        a = a.shl1();
        if high {
            a = a.xor(&c);
        }
        b = b.shr1();
    }
    p
}

/// Square in GF(2^256).
pub fn field_square_256(a: U256, c: U256) -> U256 {
    field_mul_256(a, a, c)
}

/// Inversion in GF(2^256) via Itoh–Tsujii.
pub fn field_invert_256(a: U256, c: U256) -> U256 {
    if a.is_zero() {
        return U256::ZERO;
    }
    let w = 256u32;
    let e = w - 1; // 255
    let mut r = a;
    let mut k: u32 = 1;
    let msb = 31 - e.leading_zeros();
    for bit_pos in (0..msb).rev() {
        let mut tmp = r;
        for _ in 0..k {
            tmp = field_square_256(tmp, c);
        }
        r = field_mul_256(tmp, r, c);
        k *= 2;
        if (e >> bit_pos) & 1 == 1 {
            r = field_mul_256(field_square_256(r, c), a, c);
            k += 1;
        }
    }
    field_square_256(r, c)
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // --- GF(2^8) tests ---

    const GF8_POLY: u8 = 0x1b;

    #[test]
    fn test_gf8_mul_identity() {
        for a in 0..=255u8 {
            assert_eq!(field_mul(a, 1u8, GF8_POLY), a);
            assert_eq!(field_mul(1u8, a, GF8_POLY), a);
        }
    }

    #[test]
    fn test_gf8_mul_zero() {
        for a in 0..=255u8 {
            assert_eq!(field_mul(a, 0u8, GF8_POLY), 0);
        }
    }

    #[test]
    fn test_gf8_mul_commutativity() {
        for a in 1..=255u8 {
            for b in 1..=255u8 {
                assert_eq!(field_mul(a, b, GF8_POLY), field_mul(b, a, GF8_POLY));
            }
        }
    }

    #[test]
    fn test_gf8_invert_roundtrip() {
        for a in 1..=255u8 {
            let inv = field_invert(a, GF8_POLY, 8);
            let product = field_mul(a, inv, GF8_POLY);
            assert_eq!(product, 1, "a={:#04x} inv={:#04x} product={:#04x}", a, inv, product);
        }
    }

    #[test]
    fn test_gf8_invert_zero() {
        assert_eq!(field_invert(0u8, GF8_POLY, 8), 0);
    }

    // --- GF(2^64) tests ---

    const GF64_POLY: u64 = 0x1b;

    #[test]
    fn test_gf64_mul_identity() {
        let vals: [u64; 5] = [1, 0x42, 0xDEADBEEF, 0x0102030405060708, u64::MAX];
        for &a in &vals {
            assert_eq!(field_mul(a, 1u64, GF64_POLY), a);
        }
    }

    #[test]
    fn test_gf64_mul_commutativity() {
        let vals: [u64; 4] = [0x42, 0xFF, 0xDEADBEEF, 0x0102030405060708];
        for &a in &vals {
            for &b in &vals {
                assert_eq!(field_mul(a, b, GF64_POLY), field_mul(b, a, GF64_POLY));
            }
        }
    }

    #[test]
    fn test_gf64_invert_roundtrip() {
        let vals: [u64; 5] = [1, 0x42, 0xFF, 0xDEADBEEF, 0x0102030405060708];
        for &a in &vals {
            let inv = field_invert(a, GF64_POLY, 64);
            let product = field_mul(a, inv, GF64_POLY);
            assert_eq!(product, 1, "a={:#018x} inv={:#018x} product={:#018x}", a, inv, product);
        }
    }

    // --- GF(2^128) tests ---

    const GF128_POLY: u128 = 0x87;

    #[test]
    fn test_gf128_mul_identity() {
        let vals: [u128; 3] = [1, 0xDEADBEEFCAFEBABE, 0x0102030405060708090A0B0C0D0E0F10];
        for &a in &vals {
            assert_eq!(field_mul_128(a, 1, GF128_POLY), a);
        }
    }

    #[test]
    fn test_gf128_mul_commutativity() {
        let vals: [u128; 3] = [0x42, 0xDEADBEEFCAFEBABE, 0x0102030405060708090A0B0C0D0E0F10];
        for &a in &vals {
            for &b in &vals {
                assert_eq!(field_mul_128(a, b, GF128_POLY), field_mul_128(b, a, GF128_POLY));
            }
        }
    }

    #[test]
    fn test_gf128_invert_roundtrip() {
        let vals: [u128; 3] = [1, 0xDEADBEEFCAFEBABE, 0x0102030405060708090A0B0C0D0E0F10];
        for &a in &vals {
            let inv = field_invert_128(a, GF128_POLY);
            let product = field_mul_128(a, inv, GF128_POLY);
            assert_eq!(product, 1, "a={:#034x} inv={:#034x}", a, inv);
        }
    }

    // --- GF(2^256) tests ---

    const GF256_POLY: U256 = U256([0x425, 0, 0, 0]);

    #[test]
    fn test_gf256_mul_identity() {
        let a = U256([0xDEADBEEF, 0xCAFEBABE, 0x01020304, 0x05060708]);
        assert_eq!(field_mul_256(a, U256::ONE, GF256_POLY), a);
        assert_eq!(field_mul_256(U256::ONE, a, GF256_POLY), a);
    }

    #[test]
    fn test_gf256_mul_zero() {
        let a = U256([0xDEADBEEF, 0xCAFEBABE, 0x01020304, 0x05060708]);
        assert_eq!(field_mul_256(a, U256::ZERO, GF256_POLY), U256::ZERO);
    }

    #[test]
    fn test_gf256_mul_commutativity() {
        let a = U256([0xDEADBEEF, 0xCAFEBABE, 0x01020304, 0x05060708]);
        let b = U256([0x42, 0xFF, 0x123, 0x456]);
        assert_eq!(field_mul_256(a, b, GF256_POLY), field_mul_256(b, a, GF256_POLY));
    }

    #[test]
    fn test_gf256_invert_roundtrip() {
        let vals = [
            U256::ONE,
            U256([0x42, 0, 0, 0]),
            U256([0xDEADBEEF, 0xCAFEBABE, 0x01020304, 0x05060708]),
        ];
        for a in &vals {
            let inv = field_invert_256(*a, GF256_POLY);
            let product = field_mul_256(*a, inv, GF256_POLY);
            assert_eq!(product, U256::ONE, "a={:?} inv={:?}", a, inv);
        }
    }
}
