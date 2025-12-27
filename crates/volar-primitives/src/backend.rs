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
