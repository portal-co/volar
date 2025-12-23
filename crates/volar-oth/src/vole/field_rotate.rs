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
impl<N: ArrayLength<BitsInBytes>> Q<N, BitsInBytes> {
    pub fn rotate_left_bits(&self, n: usize) -> Self {
        let Q { q } = self;
        Q {
            q: GenericArray::generate(|i| {
                let BitsInBytes(b) = q[i].clone();
                let BitsInBytes(next) = q[(i + 1) % N::to_usize()].clone();
                BitsInBytes(b.shl(n as u32) | next.shr(8 - n as u32))
            }),
        }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self {
        let Q { q } = self;
        Q {
            q: GenericArray::generate(|i| {
                let BitsInBytes(prev) = q[(i + N::to_usize() - 1) % N::to_usize()].clone();
                let BitsInBytes(b) = q[i].clone();
                BitsInBytes(prev.shl(8 - n as u32) | b.shr(n as u32))
            }),
        }
    }
}
impl<N: ArrayLength<BitsInBytes64>> Q<N, BitsInBytes64> {
    pub fn rotate_left_bits(&self, n: usize) -> Self {
        let Q { q } = self;
        Q {
            q: GenericArray::generate(|i| {
                let BitsInBytes64(b) = q[i].clone();
                let BitsInBytes64(next) = q[(i + 1) % N::to_usize()].clone();
                BitsInBytes64(b.shl(n as u32) | next.shr(64 - n as u32))
            }),
        }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self {
        let Q { q } = self;
        Q {
            q: GenericArray::generate(|i| {
                let BitsInBytes64(prev) = q[(i + N::to_usize() - 1) % N::to_usize()].clone();
                let BitsInBytes64(b) = q[i].clone();
                BitsInBytes64(prev.shl(64 - n as u32) | b.shr(n as u32))
            }),
        }
    }
}

impl<N: ArrayLength<BitsInBytes>> Delta<N, BitsInBytes> {
    pub fn rotate_left_bits(&self, n: usize) -> Self {
        let Delta { delta } = self;
        Delta {
            delta: GenericArray::generate(|i| {
                let BitsInBytes(b) = delta[i].clone();
                let BitsInBytes(next) = delta[(i + 1) % N::to_usize()].clone();
                BitsInBytes(b.shl(n as u32) | next.shr(8 - n as u32))
            }),
        }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self {
        let Delta { delta } = self;
        Delta {
            delta: GenericArray::generate(|i| {
                let BitsInBytes(prev) = delta[(i + N::to_usize() - 1) % N::to_usize()].clone();
                let BitsInBytes(b) = delta[i].clone();
                BitsInBytes(prev.shl(8 - n as u32) | b.shr(n as u32))
            }),
        }
    }
}
impl<N: ArrayLength<BitsInBytes64>> Delta<N, BitsInBytes64> {
    pub fn rotate_left_bits(&self, n: usize) -> Self {
        let Delta { delta } = self;
        Delta {
            delta: GenericArray::generate(|i| {
                let BitsInBytes64(b) = delta[i].clone();
                let BitsInBytes64(next) = delta[(i + 1) % N::to_usize()].clone();
                BitsInBytes64(b.shl(n as u32) | next.shr(64 - n as u32))
            }),
        }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self {
        let Delta { delta } = self;
        Delta {
            delta: GenericArray::generate(|i| {
                let BitsInBytes64(prev) = delta[(i + N::to_usize() - 1) % N::to_usize()].clone();
                let BitsInBytes64(b) = delta[i].clone();
                BitsInBytes64(prev.shl(64 - n as u32) | b.shr(n as u32))
            }),
        }
    }
}
