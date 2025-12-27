#![no_std]

use core::ops::{Add, BitAnd, BitXor, BitXorAssign, Mul, Shl, ShlAssign, ShrAssign, Sub};
pub mod backend;
macro_rules! u8_field {
    ($($a:ident),*) => {
        $(
            #[repr(transparent)]
            #[derive(Clone,Copy, Debug, PartialEq, Eq,PartialOrd, Ord, Hash,Default)]
            pub struct $a(pub u8);
            impl BitXor<u8> for $a{
                type Output = Self;
                fn bitxor(self, rhs: u8) -> Self::Output {
                    $a(self.0 ^ rhs)
                }
            }

        )*
    };
}
macro_rules! bool_field {
    ($($a:ident)*) => {
        $(
            #[repr(transparent)] 
            #[derive(Clone,Copy, Debug, PartialEq, Eq,PartialOrd, Ord, Hash,Default)] 
            pub struct $a(pub bool);
            impl BitXor<u8> for $a{
                type Output = Self;
                fn bitxor(self, rhs: u8) -> Self::Output {
                    $a(self.0 ^ (rhs & 1 != 0))
                }
            }
    )*
    };
}
macro_rules! u64_field {
    ($($a:ident),*) => {
        $(
            #[repr(transparent)]
            #[derive(Clone,Copy, Debug, PartialEq, Eq,PartialOrd, Ord, Hash,Default)]
            pub struct $a(pub u64);
            impl BitXor<u8> for $a{
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
impl Add<BitsInBytes> for BitsInBytes {
    type Output = BitsInBytes;
    fn add(self, rhs: BitsInBytes) -> Self::Output {
        BitsInBytes(self.0 ^ rhs.0)
    }
}
impl Mul<BitsInBytes> for BitsInBytes {
    type Output = BitsInBytes;
    fn mul(self, rhs: BitsInBytes) -> Self::Output {
        BitsInBytes(self.0 & rhs.0)
    }
}
impl Sub<BitsInBytes> for BitsInBytes {
    type Output = BitsInBytes;
    fn sub(self, rhs: BitsInBytes) -> Self::Output {
        BitsInBytes(self.0 ^ rhs.0)
    }
}
impl Add<BitsInBytes64> for BitsInBytes64 {
    type Output = BitsInBytes64;
    fn add(self, rhs: BitsInBytes64) -> Self::Output {
        BitsInBytes64(self.0 ^ rhs.0)
    }
}
impl Mul<BitsInBytes64> for BitsInBytes64 {
    type Output = BitsInBytes64;
    fn mul(self, rhs: BitsInBytes64) -> Self::Output {
        BitsInBytes64(self.0 & rhs.0)
    }
}
impl Sub<BitsInBytes64> for BitsInBytes64 {
    type Output = BitsInBytes64;
    fn sub(self, rhs: BitsInBytes64) -> Self::Output {
        BitsInBytes64(self.0 ^ rhs.0)
    }
}
impl Add<Galois> for Galois {
    type Output = Galois;
    fn add(self, rhs: Galois) -> Self::Output {
        Galois(self.0 ^ rhs.0)
    }
}

impl Mul<Galois> for Galois {
    type Output = Galois;
    fn mul(self, rhs: Galois) -> Self::Output {
        let mut a = self.0;
        let mut b = rhs.0;
        let c = 0x1b; // x^8 + x^4 + x^3 + x + 1
        let p = backend::field_mul(a, b, c);
        Galois(p)
    }
}
impl Invert for Galois {
    fn invert(&self) -> Self {
        if self.0 == 0 {
            // Follow tradition in AES that since 0 has no inverse, we map it to 0
            return Galois(0);
        }
        let mut a = self.0;
        let mut b = 1u8;
        let c = 0x1b; // x^8 + x^4 + x^3 + x + 1
        for _ in 0..254 {
            b = backend::field_mul(b, a, c);
            a = backend::field_mul(a, a, c);
        }
        Galois(b)
    }
}
impl Sub<Galois> for Galois {
    type Output = Galois;
    fn sub(self, rhs: Galois) -> Self::Output {
        Galois(self.0 ^ rhs.0)
    }
}
impl Add<Galois64> for Galois64 {
    type Output = Galois64;
    fn add(self, rhs: Galois64) -> Self::Output {
        Galois64(self.0 ^ rhs.0)
    }
}
impl Mul<Galois64> for Galois64 {
    type Output = Galois64;
    fn mul(self, rhs: Galois64) -> Self::Output {
        let mut a = self.0;
        let mut b = rhs.0;
        let c = 0x1b; // x^8 + x^4 + x^3 + x + 1
        let p = backend::field_mul(a, b, c);
        Galois64(p)
    }
}
impl Sub<Galois64> for Galois64 {
    type Output = Galois64;
    fn sub(self, rhs: Galois64) -> Self::Output {
        Galois64(self.0 ^ rhs.0)
    }
}
