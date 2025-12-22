use super::*;
use core::ops::{Add, BitXor, Mul, Sub};
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
u8_field!(Galois, Bytes);
impl Add<Bytes> for Bytes {
    type Output = Bytes;
    fn add(self, rhs: Bytes) -> Self::Output {
        Bytes(self.0.wrapping_add(rhs.0))
    }
}
impl Mul<Bytes> for Bytes {
    type Output = Bytes;
    fn mul(self, rhs: Bytes) -> Self::Output {
        Bytes(self.0.wrapping_mul(rhs.0))
    }
}
impl Sub<Bytes> for Bytes {
    type Output = Bytes;
    fn sub(self, rhs: Bytes) -> Self::Output {
        Bytes(self.0.wrapping_sub(rhs.0))
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
        let mut p = 0u8;
        for _ in 0..8 {
            if (b & 1) != 0 {
                p ^= a;
            }
            let high_bit = a & 0x80;
            a <<= 1;
            if high_bit != 0 {
                a ^= 0x1b; // x^8 + x^4 + x^3 + x + 1
            }
            b >>= 1;
        }
        Galois(p)
    }
}
impl Sub<Galois> for Galois {
    type Output = Galois;
    fn sub(self, rhs: Galois) -> Self::Output {
        Galois(self.0 ^ rhs.0)
    }
}
