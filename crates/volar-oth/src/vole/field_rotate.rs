use core::ops::{BitAnd, BitOr, Shl, Shr};

use crate::field::{BitsInBytes, BitsInBytes64};

use super::*;
impl<N: VoleArray<BitsInBytes>> Vole<N, BitsInBytes> {
    pub fn rotate_left_bits(&self, n: usize) -> Self {
        let Vole { u, v } = self;
        Vole {
            u: GenericArray::generate(|i| {
                let BitsInBytes(b) = u[i].clone();
                let BitsInBytes(next) = u[(i + 1) % N::to_usize()].clone();
                BitsInBytes(b.shl(n as u32) | next.shr(8 - n as u32))
            }),
            v: GenericArray::generate(|i| {
                let BitsInBytes(b) = v[i].clone();
                let BitsInBytes(next) = v[(i + 1) % N::to_usize()].clone();
                BitsInBytes(b.shl(n as u32) | next.shr(8 - n as u32))
            }),
        }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self {
        let Vole { u, v } = self;
        Vole {
            u: GenericArray::generate(|i| {
                let BitsInBytes(prev) = u[(i + N::to_usize() - 1) % N::to_usize()].clone();
                let BitsInBytes(b) = u[i].clone();
                BitsInBytes(prev.shl(8 - n as u32) | b.shr(n as u32))
            }),
            v: GenericArray::generate(|i| {
                let BitsInBytes(prev) = v[(i + N::to_usize() - 1) % N::to_usize()].clone();
                let BitsInBytes(b) = v[i].clone();
                BitsInBytes(prev.shl(8 - n as u32) | b.shr(n as u32))
            }),
        }
    }
}
impl<N: VoleArray<BitsInBytes64>> Vole<N, BitsInBytes64> {
    pub fn rotate_left_bits(&self, n: usize) -> Self {
        let Vole { u, v } = self;
        Vole {
            u: GenericArray::generate(|i| {
                let BitsInBytes64(b) = u[i].clone();
                let BitsInBytes64(next) = u[(i + 1) % N::to_usize()].clone();
                BitsInBytes64(b.shl(n as u32) | next.shr(64 - n as u32))
            }),
            v: GenericArray::generate(|i| {
                let BitsInBytes64(b) = v[i].clone();
                let BitsInBytes64(next) = v[(i + 1) % N::to_usize()].clone();
                BitsInBytes64(b.shl(n as u32) | next.shr(64 - n as u32))
            }),
        }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self {
        let Vole { u, v } = self;
        Vole {
            u: GenericArray::generate(|i| {
                let BitsInBytes64(prev) = u[(i + N::to_usize() - 1) % N::to_usize()].clone();
                let BitsInBytes64(b) = u[i].clone();
                BitsInBytes64(prev.shl(64 - n as u32) | b.shr(n as u32))
            }),
            v: GenericArray::generate(|i| {
                let BitsInBytes64(prev) = v[(i + N::to_usize() - 1) % N::to_usize()].clone();
                let BitsInBytes64(b) = v[i].clone();
                BitsInBytes64(prev.shl(64 - n as u32) | b.shr(n as u32))
            }),
        }
    }
}
impl<N: VoleArray<BitsInBytes>> BitAnd for Vole<N, BitsInBytes> {
    type Output = Vole<N, BitsInBytes>;
    fn bitand(self, rhs: Vole<N, BitsInBytes>) -> Self::Output {
        let Vole { u: u1, v: v1 } = self;
        let Vole { u: u2, v: v2 } = rhs;
        Vole {
            u: GenericArray::generate(|i| BitsInBytes(u1[i].clone().0 & u2[i].clone().0)),
            v: GenericArray::generate(|i| BitsInBytes(v1[i].clone().0 & v2[i].clone().0)),
        }
    }
}
impl<N: VoleArray<BitsInBytes64>> BitAnd for Vole<N, BitsInBytes64> {
    type Output = Vole<N, BitsInBytes64>;
    fn bitand(self, rhs: Vole<N, BitsInBytes64>) -> Self::Output {
        let Vole { u: u1, v: v1 } = self;
        let Vole { u: u2, v: v2 } = rhs;
        Vole {
            u: GenericArray::generate(|i| BitsInBytes64(u1[i].clone().0 & u2[i].clone().0)),
            v: GenericArray::generate(|i| BitsInBytes64(v1[i].clone().0 & v2[i].clone().0)),
        }
    }
}
impl<N: VoleArray<BitsInBytes>> BitOr for Vole<N, BitsInBytes> {
    type Output = Vole<N, BitsInBytes>;
    fn bitor(self, rhs: Vole<N, BitsInBytes>) -> Self::Output {
        let Vole { u: u1, v: v1 } = self;
        let Vole { u: u2, v: v2 } = rhs;
        Vole {
            u: GenericArray::generate(|i| BitsInBytes(u1[i].clone().0 | u2[i].clone().0)),
            v: GenericArray::generate(|i| BitsInBytes(v1[i].clone().0 | v2[i].clone().0)),
        }
    }
}
impl<N: VoleArray<BitsInBytes64>> BitOr for Vole<N, BitsInBytes64> {
    type Output = Vole<N, BitsInBytes64>;
    fn bitor(self, rhs: Vole<N, BitsInBytes64>) -> Self::Output {
        let Vole { u: u1, v: v1 } = self;
        let Vole { u: u2, v: v2 } = rhs;
        Vole {
            u: GenericArray::generate(|i| BitsInBytes64(u1[i].clone().0 | u2[i].clone().0)),
            v: GenericArray::generate(|i| BitsInBytes64(v1[i].clone().0 | v2[i].clone().0)),
        }
    }
}
