// @reliability: normal
//! @ai: assisted
// Field element types for GF(2), GF(2^8), GF(2^64), GF(2^128), GF(2^256),
// and packed variants.  Based on standard carry-less multiplication over
// binary extension fields.
#![no_std]

use core::ops::{Add, BitAnd, BitXor, BitXorAssign, Mul, Shl, ShlAssign, ShrAssign, Sub};
pub mod backend;

// ============================================================================
// Macro-generated newtypes
// ============================================================================

macro_rules! u8_field {
    ($($a:ident),*) => {
        $(
            #[repr(transparent)]
            #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
            pub struct $a(pub u8);
            impl BitXor<u8> for $a {
                type Output = Self;
                fn bitxor(self, rhs: u8) -> Self::Output { $a(self.0 ^ rhs) }
            }
        )*
    };
}
macro_rules! bool_field {
    ($($a:ident)*) => {
        $(
            #[repr(transparent)]
            #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
            pub struct $a(pub bool);
            impl BitXor<u8> for $a {
                type Output = Self;
                fn bitxor(self, rhs: u8) -> Self::Output { $a(self.0 ^ (rhs & 1 != 0)) }
            }
        )*
    };
}
macro_rules! u64_field {
    ($($a:ident),*) => {
        $(
            #[repr(transparent)]
            #[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
            pub struct $a(pub u64);
            impl BitXor<u8> for $a {
                type Output = Self;
                fn bitxor(self, rhs: u8) -> Self::Output {
                    $a(u64::from_ne_bytes(self.0.to_ne_bytes().map(|b| b ^ rhs)))
                }
            }
        )*
    };
}

pub trait Invert {
    fn invert(&self) -> Self;
}

bool_field!(Bit);
u8_field!(Galois, BitsInBytes);
u64_field!(Galois64, BitsInBytes64);

// ============================================================================
// BitsInBytes — GF(2)^8 packed (SIMD 8-lane): add = XOR, mul = AND
// ============================================================================

impl Add<BitsInBytes> for BitsInBytes {
    type Output = BitsInBytes;
    fn add(self, rhs: BitsInBytes) -> Self::Output { BitsInBytes(self.0 ^ rhs.0) }
}
impl Mul<BitsInBytes> for BitsInBytes {
    type Output = BitsInBytes;
    fn mul(self, rhs: BitsInBytes) -> Self::Output { BitsInBytes(self.0 & rhs.0) }
}
impl Sub<BitsInBytes> for BitsInBytes {
    type Output = BitsInBytes;
    fn sub(self, rhs: BitsInBytes) -> Self::Output { BitsInBytes(self.0 ^ rhs.0) }
}

// ============================================================================
// BitsInBytes64 — GF(2)^64 packed (SIMD 64-lane): add = XOR, mul = AND
// ============================================================================

impl Add<BitsInBytes64> for BitsInBytes64 {
    type Output = BitsInBytes64;
    fn add(self, rhs: BitsInBytes64) -> Self::Output { BitsInBytes64(self.0 ^ rhs.0) }
}
impl Mul<BitsInBytes64> for BitsInBytes64 {
    type Output = BitsInBytes64;
    fn mul(self, rhs: BitsInBytes64) -> Self::Output { BitsInBytes64(self.0 & rhs.0) }
}
impl Sub<BitsInBytes64> for BitsInBytes64 {
    type Output = BitsInBytes64;
    fn sub(self, rhs: BitsInBytes64) -> Self::Output { BitsInBytes64(self.0 ^ rhs.0) }
}

// ============================================================================
// Galois — GF(2^8) with irreducible x^8 + x^4 + x^3 + x + 1
// ============================================================================

/// Reduction polynomial for GF(2^8): x^8 + x^4 + x^3 + x + 1.
const GF8_POLY: u8 = 0x1b;

impl Add<Galois> for Galois {
    type Output = Galois;
    fn add(self, rhs: Galois) -> Self::Output { Galois(self.0 ^ rhs.0) }
}
impl Mul<Galois> for Galois {
    type Output = Galois;
    fn mul(self, rhs: Galois) -> Self::Output {
        Galois(backend::field_mul(self.0, rhs.0, GF8_POLY))
    }
}
impl Sub<Galois> for Galois {
    type Output = Galois;
    fn sub(self, rhs: Galois) -> Self::Output { Galois(self.0 ^ rhs.0) }
}
impl Invert for Galois {
    /// Inversion in GF(2^8) via Itoh–Tsujii addition chain.
    ///
    /// `a^{-1} = a^{254}` in GF(2^8).  Uses ~11 multiplications + 7 squarings
    /// instead of the naïve 254-iteration loop.
    fn invert(&self) -> Self {
        Galois(backend::field_invert(self.0, GF8_POLY, 8))
    }
}

// ============================================================================
// Galois64 — GF(2^64) with irreducible x^64 + x^4 + x^3 + x + 1
// ============================================================================

/// Reduction polynomial for GF(2^64): x^64 + x^4 + x^3 + x + 1.
const GF64_POLY: u64 = 0x1b;

impl Add<Galois64> for Galois64 {
    type Output = Galois64;
    fn add(self, rhs: Galois64) -> Self::Output { Galois64(self.0 ^ rhs.0) }
}
impl Mul<Galois64> for Galois64 {
    type Output = Galois64;
    fn mul(self, rhs: Galois64) -> Self::Output {
        Galois64(backend::field_mul(self.0, rhs.0, GF64_POLY))
    }
}
impl Sub<Galois64> for Galois64 {
    type Output = Galois64;
    fn sub(self, rhs: Galois64) -> Self::Output { Galois64(self.0 ^ rhs.0) }
}
impl Invert for Galois64 {
    /// Inversion in GF(2^64) via Itoh–Tsujii.
    fn invert(&self) -> Self {
        Galois64(backend::field_invert(self.0, GF64_POLY, 64))
    }
}

// ============================================================================
// Galois128 — GF(2^128) with irreducible x^128 + x^7 + x^2 + x + 1  (GCM)
// ============================================================================

/// GF(2^128) field element.  Backing store is `u128`.
///
/// Uses the GCM standard reduction polynomial: x^128 + x^7 + x^2 + x + 1.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Galois128(pub u128);

/// Reduction polynomial for GF(2^128): x^128 + x^7 + x^2 + x + 1.
const GF128_POLY: u128 = 0x87;

impl Add<Galois128> for Galois128 {
    type Output = Galois128;
    fn add(self, rhs: Galois128) -> Self::Output { Galois128(self.0 ^ rhs.0) }
}
impl Mul<Galois128> for Galois128 {
    type Output = Galois128;
    fn mul(self, rhs: Galois128) -> Self::Output {
        Galois128(backend::field_mul_128(self.0, rhs.0, GF128_POLY))
    }
}
impl Sub<Galois128> for Galois128 {
    type Output = Galois128;
    fn sub(self, rhs: Galois128) -> Self::Output { Galois128(self.0 ^ rhs.0) }
}
impl Invert for Galois128 {
    fn invert(&self) -> Self {
        Galois128(backend::field_invert_128(self.0, GF128_POLY))
    }
}
impl BitXor<u8> for Galois128 {
    type Output = Self;
    fn bitxor(self, rhs: u8) -> Self::Output { Galois128(self.0 ^ rhs as u128) }
}

// ============================================================================
// Galois256 — GF(2^256) with irreducible x^256 + x^10 + x^5 + x^2 + 1
// ============================================================================

/// GF(2^256) field element.  Backing store is `[u64; 4]` (little-endian limbs).
///
/// Uses the reduction polynomial x^256 + x^10 + x^5 + x^2 + 1.
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Galois256(pub backend::U256);

/// Reduction polynomial for GF(2^256).
const GF256_POLY: backend::U256 = backend::U256([0x425, 0, 0, 0]);

impl Add<Galois256> for Galois256 {
    type Output = Galois256;
    fn add(self, rhs: Galois256) -> Self::Output { Galois256(self.0.xor(&rhs.0)) }
}
impl Mul<Galois256> for Galois256 {
    type Output = Galois256;
    fn mul(self, rhs: Galois256) -> Self::Output {
        Galois256(backend::field_mul_256(self.0, rhs.0, GF256_POLY))
    }
}
impl Sub<Galois256> for Galois256 {
    type Output = Galois256;
    fn sub(self, rhs: Galois256) -> Self::Output { Galois256(self.0.xor(&rhs.0)) }
}
impl Invert for Galois256 {
    fn invert(&self) -> Self {
        Galois256(backend::field_invert_256(self.0, GF256_POLY))
    }
}

// ============================================================================
// Tropical semiring
// ============================================================================

#[derive(Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub struct Tropical<T>(pub T);
impl<T: Ord> Add<Tropical<T>> for Tropical<T> {
    type Output = Tropical<T>;
    fn add(self, rhs: Tropical<T>) -> Self::Output { Tropical(self.0.min(rhs.0)) }
}
impl<T: Add<U>, U> Mul<Tropical<U>> for Tropical<T> {
    type Output = Tropical<<T as Add<U>>::Output>;
    fn mul(self, rhs: Tropical<U>) -> Self::Output { Tropical(self.0 + rhs.0) }
}

// ============================================================================
// High-level field element tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_galois_invert_all_nonzero() {
        for a in 1..=255u8 {
            let g = Galois(a);
            let inv = g.invert();
            let product = g * inv;
            assert_eq!(product, Galois(1), "Galois({:#04x}).invert() = Galois({:#04x}), product = {:?}", a, inv.0, product);
        }
    }

    #[test]
    fn test_galois_invert_zero() {
        assert_eq!(Galois(0).invert(), Galois(0));
    }

    #[test]
    fn test_galois64_invert() {
        let vals: [u64; 4] = [1, 0x42, 0xDEADBEEFCAFEBABE, 0x0102030405060708];
        for &a in &vals {
            let g = Galois64(a);
            let inv = g.invert();
            let product = g * inv;
            assert_eq!(product, Galois64(1), "Galois64({:#018x}).invert() failed", a);
        }
    }

    #[test]
    fn test_galois128_invert() {
        let vals: [u128; 3] = [1, 0xDEADBEEFCAFEBABE, 0x0102030405060708090A0B0C0D0E0F10];
        for &a in &vals {
            let g = Galois128(a);
            let inv = g.invert();
            let product = g * inv;
            assert_eq!(product, Galois128(1), "Galois128({:#034x}).invert() failed", a);
        }
    }

    #[test]
    fn test_galois256_invert() {
        let a = Galois256(backend::U256([0xDEADBEEF, 0xCAFEBABE, 0x01020304, 0x05060708]));
        let inv = a.invert();
        let product = a * inv;
        assert_eq!(product, Galois256(backend::U256::ONE), "Galois256 invert failed");
    }

    #[test]
    fn test_galois128_mul_commutativity() {
        let a = Galois128(0xDEADBEEFCAFEBABE);
        let b = Galois128(0x0102030405060708090A0B0C0D0E0F10);
        assert_eq!(a * b, b * a);
    }

    #[test]
    fn test_galois256_mul_commutativity() {
        let a = Galois256(backend::U256([0xDEADBEEF, 0xCAFEBABE, 0x01020304, 0x05060708]));
        let b = Galois256(backend::U256([0x42, 0xFF, 0x123, 0x456]));
        assert_eq!(a * b, b * a);
    }
}
