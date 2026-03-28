use core::ops::{Shl, Shr};

use crate::field::{Bit, BitsInBytes, BitsInBytes64};

use super::*;
pub mod vope;
impl<N: ArraySize> Q<N, BitsInBytes> {
    pub fn rotate_left_bits(&self, n: usize) -> Self {
        let Q { q } = self;
        Q {
            q: Array::<BitsInBytes, N>::from_fn(|i| {
                let BitsInBytes(b) = q[i].clone();
                let BitsInBytes(next) = q[(i + 1) % N::USIZE].clone();
                BitsInBytes(b.shl(n as u32) | next.shr(8 - n as u32))
            }),
        }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self {
        let Q { q } = self;
        Q {
            q: Array::<BitsInBytes, N>::from_fn(|i| {
                let BitsInBytes(prev) = q[(i + N::USIZE - 1) % N::USIZE].clone();
                let BitsInBytes(b) = q[i].clone();
                BitsInBytes(prev.shl(8 - n as u32) | b.shr(n as u32))
            }),
        }
    }
    pub fn bit(&self, n: u8) -> Q<N, Bit> {
        let Q { q } = self;
        Q {
            q: Array::<Bit, N>::from_fn(|i| {
                let BitsInBytes(b) = q[i].clone();
                Bit((b >> n) & 1 != 0)
            }),
        }
    }
}
impl<N: ArraySize> Q<N, BitsInBytes64> {
    pub fn rotate_left_bits(&self, n: usize) -> Self {
        let Q { q } = self;
        Q {
            q: Array::<BitsInBytes64, N>::from_fn(|i| {
                let BitsInBytes64(b) = q[i].clone();
                let BitsInBytes64(next) = q[(i + 1) % N::USIZE].clone();
                BitsInBytes64(b.shl(n as u32) | next.shr(64 - n as u32))
            }),
        }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self {
        let Q { q } = self;
        Q {
            q: Array::<BitsInBytes64, N>::from_fn(|i| {
                let BitsInBytes64(prev) = q[(i + N::USIZE - 1) % N::USIZE].clone();
                let BitsInBytes64(b) = q[i].clone();
                BitsInBytes64(prev.shl(64 - n as u32) | b.shr(n as u32))
            }),
        }
    }
    pub fn bit(&self, n: u8) -> Q<N, Bit> {
        let Q { q } = self;
        Q {
            q: Array::<Bit, N>::from_fn(|i| {
                let BitsInBytes64(b) = q[i].clone();
                Bit((b >> n) & 1 != 0)
            }),
        }
    }
}

impl<N: ArraySize> Delta<N, BitsInBytes> {
    pub fn rotate_left_bits(&self, n: usize) -> Self {
        let Delta { delta } = self;
        Delta {
            delta: Array::<BitsInBytes, N>::from_fn(|i| {
                let BitsInBytes(b) = delta[i].clone();
                let BitsInBytes(next) = delta[(i + 1) % N::USIZE].clone();
                BitsInBytes(b.shl(n as u32) | next.shr(8 - n as u32))
            }),
        }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self {
        let Delta { delta } = self;
        Delta {
            delta: Array::<BitsInBytes, N>::from_fn(|i| {
                let BitsInBytes(prev) = delta[(i + N::USIZE - 1) % N::USIZE].clone();
                let BitsInBytes(b) = delta[i].clone();
                BitsInBytes(prev.shl(8 - n as u32) | b.shr(n as u32))
            }),
        }
    }
    pub fn bit(&self, n: u8) -> Delta<N, Bit> {
        let Delta { delta } = self;
        Delta {
            delta: Array::<Bit, N>::from_fn(|i| {
                let BitsInBytes(b) = delta[i].clone();
                Bit((b >> n) & 1 != 0)
            }),
        }
    }
}
impl<N: ArraySize> Delta<N, BitsInBytes64> {
    pub fn rotate_left_bits(&self, n: usize) -> Self {
        let Delta { delta } = self;
        Delta {
            delta: Array::<BitsInBytes64, N>::from_fn(|i| {
                let BitsInBytes64(b) = delta[i].clone();
                let BitsInBytes64(next) = delta[(i + 1) % N::USIZE].clone();
                BitsInBytes64(b.shl(n as u32) | next.shr(64 - n as u32))
            }),
        }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self {
        let Delta { delta } = self;
        Delta {
            delta: Array::<BitsInBytes64, N>::from_fn(|i| {
                let BitsInBytes64(prev) = delta[(i + N::USIZE - 1) % N::USIZE].clone();
                let BitsInBytes64(b) = delta[i].clone();
                BitsInBytes64(prev.shl(64 - n as u32) | b.shr(n as u32))
            }),
        }
    }
    pub fn bit(&self, n: u8) -> Delta<N, Bit> {
        let Delta { delta } = self;
        Delta {
            delta: Array::<Bit, N>::from_fn(|i| {
                let BitsInBytes64(b) = delta[i].clone();
                Bit((b >> n) & 1 != 0)
            }),
        }
    }
}
