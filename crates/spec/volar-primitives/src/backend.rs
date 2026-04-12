// @reliability: normal
//! @ai: assisted
//! Generic carry-less field multiplication backend (shift-and-XOR).
//!
//! This module provides a generic `field_mul<T>` for **Rust-only** use.
//! It is NOT parsed by `volar-compiler` (it uses `size_of_val` and unbounded
//! generics).  The compilable specialized versions live in `lib.rs`.
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

/// Square an element in GF(2^w).
pub fn field_square<T: FieldMulBackend>(a: T, c: T) -> T {
    field_mul(a.clone(), a, c)
}

/// Inversion in GF(2^w) via Itoh–Tsujii.
pub fn field_invert<T: FieldMulBackend>(a: T, c: T, w: u32) -> T {
    if a == T::default() {
        return T::default();
    }
    let e = w - 1;
    let mut r = a.clone();
    let mut k: u32 = 1;
    let msb = 31 - e.leading_zeros();
    for bit_pos in (0..msb).rev() {
        let mut tmp = r.clone();
        for _ in 0..k {
            tmp = field_square(tmp, c.clone());
        }
        r = field_mul(tmp, r, c.clone());
        k *= 2;
        if (e >> bit_pos) & 1 == 1 {
            r = field_mul(field_square(r, c.clone()), a.clone(), c.clone());
            k += 1;
        }
    }
    field_square(r, c)
}

// ============================================================================
// Tests — verify generic backend against the compilable versions in lib.rs
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generic_gf8_matches_specialized() {
        for a in 1..=255u8 {
            for b in [1u8, 0x42, 0xFF] {
                let generic = field_mul(a, b, 0x1bu8);
                let specialized = crate::gf_mul_u8(a, b, 0x1b);
                assert_eq!(generic, specialized, "a={:#04x} b={:#04x}", a, b);
            }
        }
    }

    #[test]
    fn test_generic_gf64_matches_specialized() {
        let vals: [u64; 3] = [0x42, 0xDEADBEEF, 0x0102030405060708];
        for &a in &vals {
            for &b in &vals {
                let generic = field_mul(a, b, 0x1bu64);
                let specialized = crate::gf_mul_u64(a, b, 0x1b);
                assert_eq!(generic, specialized, "a={:#018x} b={:#018x}", a, b);
            }
        }
    }

    #[test]
    fn test_generic_gf8_invert() {
        for a in 1..=255u8 {
            let inv = field_invert(a, 0x1bu8, 8);
            let product = field_mul(a, inv, 0x1bu8);
            assert_eq!(product, 1, "a={:#04x}", a);
        }
    }

    #[test]
    fn test_generic_gf64_invert() {
        let vals: [u64; 3] = [1, 0x42, 0xDEADBEEFCAFEBABE];
        for &a in &vals {
            let inv = field_invert(a, 0x1bu64, 64);
            let product = field_mul(a, inv, 0x1bu64);
            assert_eq!(product, 1, "a={:#018x}", a);
        }
    }
}
