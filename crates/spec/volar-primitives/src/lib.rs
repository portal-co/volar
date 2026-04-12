// @reliability: normal
//! @ai: assisted
//! Field element types for GF(2), GF(2^8), GF(2^64), GF(2^128), GF(2^256),
//! and packed SIMD-like variants.
//!
//! This file is designed to be parseable by `volar-compiler` in its
//! total-Rust subset.  No macros, no `size_of_val`, no unbounded loops.
//! The generic field backend in `backend.rs` remains available for direct
//! Rust use but is **not** parsed by the compiler.
#![no_std]

use core::ops::{Add, BitXor, Mul, Sub};

/// Re-export the Rust-only generic backend for callers that want it.
pub mod backend;

// ============================================================================
// Invert trait
// ============================================================================

pub trait Invert {
    fn invert(&self) -> Self;
}

// ============================================================================
// Compilable field operations — total-Rust subset
// ============================================================================

/// Carry-less multiplication in GF(2^8).  8-iteration shift-and-XOR.
pub fn gf_mul_u8(a: u8, b: u8, c: u8) -> u8 {
    let mut p: u8 = 0;
    let mut a = a;
    let mut b = b;
    for _ in 0..8 {
        if (b & 1) != 0 {
            p ^= a;
        }
        let high = a & 0x80;
        a <<= 1;
        if high != 0 {
            a ^= c;
        }
        b >>= 1;
    }
    p
}

/// Carry-less multiplication in GF(2^64).  64-iteration shift-and-XOR.
pub fn gf_mul_u64(a: u64, b: u64, c: u64) -> u64 {
    let mut p: u64 = 0;
    let mut a = a;
    let mut b = b;
    let h: u64 = 1u64 << 63;
    for _ in 0..64 {
        if (b & 1) != 0 {
            p ^= a;
        }
        let high = a & h;
        a <<= 1;
        if high != 0 {
            a ^= c;
        }
        b >>= 1;
    }
    p
}

/// Carry-less multiplication in GF(2^128).  128-iteration shift-and-XOR.
pub fn gf_mul_u128(a: u128, b: u128, c: u128) -> u128 {
    let mut p: u128 = 0;
    let mut a = a;
    let mut b = b;
    let h: u128 = 1u128 << 127;
    for _ in 0..128 {
        if (b & 1) != 0 {
            p ^= a;
        }
        let high = a & h;
        a <<= 1;
        if high != 0 {
            a ^= c;
        }
        b >>= 1;
    }
    p
}

/// Itoh–Tsujii inversion in GF(2^8).
///
/// Computes `a^{-1} = a^{254}`.  Returns 0 for input 0.
pub fn gf_invert_u8(a: u8, c: u8) -> u8 {
    if a == 0 { return 0; }
    let e: u32 = 7;  // w - 1
    let msb: u32 = 2; // highest bit position of 7
    let mut r = a;
    let mut k: u32 = 1;
    for bit_pos_rev in 0..msb {
        let bit_pos = msb - 1 - bit_pos_rev;
        let mut tmp = r;
        for _ in 0..k {
            tmp = gf_mul_u8(tmp, tmp, c);
        }
        r = gf_mul_u8(tmp, r, c);
        k *= 2;
        if ((e >> bit_pos) & 1) == 1 {
            r = gf_mul_u8(gf_mul_u8(r, r, c), a, c);
            k += 1;
        }
    }
    gf_mul_u8(r, r, c)
}

/// Itoh–Tsujii inversion in GF(2^64).
///
/// Computes `a^{-1} = a^{2^64 - 2}`.  Returns 0 for input 0.
pub fn gf_invert_u64(a: u64, c: u64) -> u64 {
    if a == 0 { return 0; }
    let e: u32 = 63;
    let msb: u32 = 5;
    let mut r = a;
    let mut k: u32 = 1;
    for bit_pos_rev in 0..msb {
        let bit_pos = msb - 1 - bit_pos_rev;
        let mut tmp = r;
        for _ in 0..k {
            tmp = gf_mul_u64(tmp, tmp, c);
        }
        r = gf_mul_u64(tmp, r, c);
        k *= 2;
        if ((e >> bit_pos) & 1) == 1 {
            r = gf_mul_u64(gf_mul_u64(r, r, c), a, c);
            k += 1;
        }
    }
    gf_mul_u64(r, r, c)
}

/// Itoh–Tsujii inversion in GF(2^128).
///
/// Computes `a^{-1} = a^{2^128 - 2}`.  Returns 0 for input 0.
pub fn gf_invert_u128(a: u128, c: u128) -> u128 {
    if a == 0 { return 0; }
    let e: u32 = 127;
    let msb: u32 = 6;
    let mut r = a;
    let mut k: u32 = 1;
    for bit_pos_rev in 0..msb {
        let bit_pos = msb - 1 - bit_pos_rev;
        let mut tmp = r;
        for _ in 0..k {
            tmp = gf_mul_u128(tmp, tmp, c);
        }
        r = gf_mul_u128(tmp, r, c);
        k *= 2;
        if ((e >> bit_pos) & 1) == 1 {
            r = gf_mul_u128(gf_mul_u128(r, r, c), a, c);
            k += 1;
        }
    }
    gf_mul_u128(r, r, c)
}

// ============================================================================
// U256 — 256-bit value as four u64 limbs (little-endian)
// ============================================================================

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct U256(pub [u64; 4]);

impl U256 {
    pub const ZERO: Self = U256([0; 4]);
    pub const ONE: Self = U256([1, 0, 0, 0]);

    pub fn bit(&self, n: u32) -> bool {
        let word = (n / 64) as usize;
        let bit = n % 64;
        if word < 4 { (self.0[word] >> bit) & 1 != 0 } else { false }
    }

    pub fn high_bit(&self) -> bool {
        self.0[3] >> 63 != 0
    }

    pub fn shl1(&self) -> Self {
        let mut out = [0u64; 4];
        out[0] = self.0[0] << 1;
        out[1] = (self.0[1] << 1) | (self.0[0] >> 63);
        out[2] = (self.0[2] << 1) | (self.0[1] >> 63);
        out[3] = (self.0[3] << 1) | (self.0[2] >> 63);
        U256(out)
    }

    pub fn shr1(&self) -> Self {
        let mut out = [0u64; 4];
        out[0] = (self.0[0] >> 1) | (self.0[1] << 63);
        out[1] = (self.0[1] >> 1) | (self.0[2] << 63);
        out[2] = (self.0[2] >> 1) | (self.0[3] << 63);
        out[3] = self.0[3] >> 1;
        U256(out)
    }

    pub fn xor(&self, other: &Self) -> Self {
        U256([
            self.0[0] ^ other.0[0],
            self.0[1] ^ other.0[1],
            self.0[2] ^ other.0[2],
            self.0[3] ^ other.0[3],
        ])
    }

    pub fn is_zero(&self) -> bool {
        self.0[0] == 0 && self.0[1] == 0 && self.0[2] == 0 && self.0[3] == 0
    }
}

/// Carry-less multiplication in GF(2^256).
pub fn gf_mul_256(a: U256, b: U256, c: U256) -> U256 {
    let mut p = U256::ZERO;
    let mut a = a;
    let mut b = b;
    for _ in 0..256 {
        if b.bit(0) {
            p = p.xor(&a);
        }
        let high = a.high_bit();
        a = a.shl1();
        if high {
            a = a.xor(&c);
        }
        b = b.shr1();
    }
    p
}

/// Itoh–Tsujii inversion in GF(2^256).
pub fn gf_invert_256(a: U256, c: U256) -> U256 {
    if a.is_zero() { return U256::ZERO; }
    let e: u32 = 255;
    let msb: u32 = 7;
    let mut r = a;
    let mut k: u32 = 1;
    for bit_pos_rev in 0..msb {
        let bit_pos = msb - 1 - bit_pos_rev;
        let mut tmp = r;
        for _ in 0..k {
            tmp = gf_mul_256(tmp, tmp, c);
        }
        r = gf_mul_256(tmp, r, c);
        k *= 2;
        if ((e >> bit_pos) & 1) == 1 {
            r = gf_mul_256(gf_mul_256(r, r, c), a, c);
            k += 1;
        }
    }
    gf_mul_256(r, r, c)
}

// ============================================================================
// Reduction polynomial constants
// ============================================================================

/// GF(2^8): x^8 + x^4 + x^3 + x + 1.
const GF8_POLY: u8 = 0x1b;
/// GF(2^64): x^64 + x^4 + x^3 + x + 1.
const GF64_POLY: u64 = 0x1b;
/// GF(2^128): x^128 + x^7 + x^2 + x + 1  (GCM standard).
const GF128_POLY: u128 = 0x87;
/// GF(2^256): x^256 + x^10 + x^5 + x^2 + 1.
const GF256_POLY: U256 = U256([0x425, 0, 0, 0]);

// ============================================================================
// Bit — GF(2)
// ============================================================================

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Bit(pub bool);

impl BitXor<u8> for Bit {
    type Output = Self;
    fn bitxor(self, rhs: u8) -> Self::Output { Bit(self.0 ^ (rhs & 1 != 0)) }
}

// ============================================================================
// Galois — GF(2^8)
// ============================================================================

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Galois(pub u8);

impl BitXor<u8> for Galois {
    type Output = Self;
    fn bitxor(self, rhs: u8) -> Self::Output { Galois(self.0 ^ rhs) }
}
impl Add<Galois> for Galois {
    type Output = Galois;
    fn add(self, rhs: Galois) -> Self::Output { Galois(self.0 ^ rhs.0) }
}
impl Mul<Galois> for Galois {
    type Output = Galois;
    fn mul(self, rhs: Galois) -> Self::Output {
        Galois(gf_mul_u8(self.0, rhs.0, GF8_POLY))
    }
}
impl Sub<Galois> for Galois {
    type Output = Galois;
    fn sub(self, rhs: Galois) -> Self::Output { Galois(self.0 ^ rhs.0) }
}
impl Invert for Galois {
    fn invert(&self) -> Self { Galois(gf_invert_u8(self.0, GF8_POLY)) }
}

// ============================================================================
// BitsInBytes — GF(2)^8 packed (add = XOR, mul = AND)
// ============================================================================

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct BitsInBytes(pub u8);

impl BitXor<u8> for BitsInBytes {
    type Output = Self;
    fn bitxor(self, rhs: u8) -> Self::Output { BitsInBytes(self.0 ^ rhs) }
}
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
// Galois64 — GF(2^64)
// ============================================================================

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Galois64(pub u64);

impl BitXor<u8> for Galois64 {
    type Output = Self;
    fn bitxor(self, rhs: u8) -> Self::Output {
        // Broadcast u8 to all bytes (SIMD-style XOR)
        Galois64(self.0 ^ (rhs as u64 * 0x0101010101010101u64))
    }
}
impl Add<Galois64> for Galois64 {
    type Output = Galois64;
    fn add(self, rhs: Galois64) -> Self::Output { Galois64(self.0 ^ rhs.0) }
}
impl Mul<Galois64> for Galois64 {
    type Output = Galois64;
    fn mul(self, rhs: Galois64) -> Self::Output {
        Galois64(gf_mul_u64(self.0, rhs.0, GF64_POLY))
    }
}
impl Sub<Galois64> for Galois64 {
    type Output = Galois64;
    fn sub(self, rhs: Galois64) -> Self::Output { Galois64(self.0 ^ rhs.0) }
}
impl Invert for Galois64 {
    fn invert(&self) -> Self { Galois64(gf_invert_u64(self.0, GF64_POLY)) }
}

// ============================================================================
// BitsInBytes64 — GF(2)^64 packed (add = XOR, mul = AND)
// ============================================================================

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct BitsInBytes64(pub u64);

impl BitXor<u8> for BitsInBytes64 {
    type Output = Self;
    fn bitxor(self, rhs: u8) -> Self::Output {
        BitsInBytes64(self.0 ^ (rhs as u64 * 0x0101010101010101u64))
    }
}
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
// Galois128 — GF(2^128), GCM polynomial
// ============================================================================

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Galois128(pub u128);

impl BitXor<u8> for Galois128 {
    type Output = Self;
    fn bitxor(self, rhs: u8) -> Self::Output { Galois128(self.0 ^ rhs as u128) }
}
impl Add<Galois128> for Galois128 {
    type Output = Galois128;
    fn add(self, rhs: Galois128) -> Self::Output { Galois128(self.0 ^ rhs.0) }
}
impl Mul<Galois128> for Galois128 {
    type Output = Galois128;
    fn mul(self, rhs: Galois128) -> Self::Output {
        Galois128(gf_mul_u128(self.0, rhs.0, GF128_POLY))
    }
}
impl Sub<Galois128> for Galois128 {
    type Output = Galois128;
    fn sub(self, rhs: Galois128) -> Self::Output { Galois128(self.0 ^ rhs.0) }
}
impl Invert for Galois128 {
    fn invert(&self) -> Self { Galois128(gf_invert_u128(self.0, GF128_POLY)) }
}

// ============================================================================
// Galois256 — GF(2^256)
// ============================================================================

#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Galois256(pub U256);

impl Add<Galois256> for Galois256 {
    type Output = Galois256;
    fn add(self, rhs: Galois256) -> Self::Output { Galois256(self.0.xor(&rhs.0)) }
}
impl Mul<Galois256> for Galois256 {
    type Output = Galois256;
    fn mul(self, rhs: Galois256) -> Self::Output {
        Galois256(gf_mul_256(self.0, rhs.0, GF256_POLY))
    }
}
impl Sub<Galois256> for Galois256 {
    type Output = Galois256;
    fn sub(self, rhs: Galois256) -> Self::Output { Galois256(self.0.xor(&rhs.0)) }
}
impl Invert for Galois256 {
    fn invert(&self) -> Self { Galois256(gf_invert_256(self.0, GF256_POLY)) }
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
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_galois_invert_all_nonzero() {
        for a in 1..=255u8 {
            let g = Galois(a);
            let inv = g.invert();
            assert_eq!(g * inv, Galois(1), "Galois({:#04x}).invert() failed", a);
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
            assert_eq!(g * inv, Galois64(1), "Galois64({:#018x}).invert() failed", a);
        }
    }

    #[test]
    fn test_galois128_invert() {
        let vals: [u128; 3] = [1, 0xDEADBEEFCAFEBABE, 0x0102030405060708090A0B0C0D0E0F10];
        for &a in &vals {
            let g = Galois128(a);
            let inv = g.invert();
            assert_eq!(g * inv, Galois128(1), "Galois128({:#034x}).invert() failed", a);
        }
    }

    #[test]
    fn test_galois256_invert() {
        let a = Galois256(U256([0xDEADBEEF, 0xCAFEBABE, 0x01020304, 0x05060708]));
        let inv = a.invert();
        assert_eq!(a * inv, Galois256(U256::ONE), "Galois256 invert failed");
    }

    #[test]
    fn test_galois_mul_known() {
        // 0x53 * 0xCA = 0x01 in AES field (known test vector)
        assert_eq!(Galois(0x53) * Galois(0xCA), Galois(0x01));
    }

    #[test]
    fn test_galois128_commutativity() {
        let a = Galois128(0xDEADBEEFCAFEBABE);
        let b = Galois128(0x0102030405060708090A0B0C0D0E0F10);
        assert_eq!(a * b, b * a);
    }

    #[test]
    fn test_galois256_commutativity() {
        let a = Galois256(U256([0xDEADBEEF, 0xCAFEBABE, 0x01020304, 0x05060708]));
        let b = Galois256(U256([0x42, 0xFF, 0x123, 0x456]));
        assert_eq!(a * b, b * a);
    }
}
