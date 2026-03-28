use crate::vole::vope::Vope;

use super::*;
impl<N: VoleArray<BitsInBytes>, K: ArraySize> Vope<N, BitsInBytes, K> {
    pub fn rotate_left_bits(&self, n: usize) -> Self {
        let Vope { u, v } = self;
        Vope {
            u: Array::<Array<BitsInBytes, N>, K>::from_fn(|l| {
                Array::<BitsInBytes, N>::from_fn(|i| {
                    let BitsInBytes(b) = u[l][i].clone();
                    let BitsInBytes(next) = u[l][(i + 1) % N::USIZE].clone();
                    BitsInBytes(b.shl(n as u32) | next.shr(8 - n as u32))
                })
            }),
            v: Array::<BitsInBytes, N>::from_fn(|i| {
                let BitsInBytes(b) = v[i].clone();
                let BitsInBytes(next) = v[(i + 1) % N::USIZE].clone();
                BitsInBytes(b.shl(n as u32) | next.shr(8 - n as u32))
            }),
        }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self {
        let Vope { u, v } = self;
        Vope {
            u: Array::<Array<BitsInBytes, N>, K>::from_fn(|l| {
                Array::<BitsInBytes, N>::from_fn(|i| {
                    let BitsInBytes(prev) = u[l][(i + N::USIZE - 1) % N::USIZE].clone();
                    let BitsInBytes(b) = u[l][i].clone();
                    BitsInBytes(prev.shl(8 - n as u32) | b.shr(n as u32))
                })
            }),
            v: Array::<BitsInBytes, N>::from_fn(|i| {
                let BitsInBytes(prev) = v[(i + N::USIZE - 1) % N::USIZE].clone();
                let BitsInBytes(b) = v[i].clone();
                BitsInBytes(prev.shl(8 - n as u32) | b.shr(n as u32))
            }),
        }
    }
    pub fn bit(&self, n: u8) -> Vope<N, Bit, K>
    where
        N: VoleArray<Bit>,
    {
        let Vope { u, v } = self;
        Vope {
            u: Array::<Array<Bit, N>, K>::from_fn(|l| {
                Array::<Bit, N>::from_fn(|i| {
                    let BitsInBytes(b) = u[l][i].clone();
                    Bit((b >> n) & 1 != 0)
                })
            }),
            v: Array::<Bit, N>::from_fn(|i| {
                let BitsInBytes(b) = v[i].clone();
                Bit((b >> n) & 1 != 0)
            }),
        }
    }
}
impl<N: VoleArray<BitsInBytes64>, K: ArraySize> Vope<N, BitsInBytes64, K> {
    pub fn rotate_left_bits(&self, n: usize) -> Self {
        let Vope { u, v } = self;
        Vope {
            u: Array::<Array<BitsInBytes64, N>, K>::from_fn(|l| {
                Array::<BitsInBytes64, N>::from_fn(|i| {
                    let BitsInBytes64(b) = u[l][i].clone();
                    let BitsInBytes64(next) = u[l][(i + 1) % N::USIZE].clone();
                    BitsInBytes64(b.shl(n as u32) | next.shr(64 - n as u32))
                })
            }),
            v: Array::<BitsInBytes64, N>::from_fn(|i| {
                let BitsInBytes64(b) = v[i].clone();
                let BitsInBytes64(next) = v[(i + 1) % N::USIZE].clone();
                BitsInBytes64(b.shl(n as u32) | next.shr(64 - n as u32))
            }),
        }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self {
        let Vope { u, v } = self;
        Vope {
            u: Array::<Array<BitsInBytes64, N>, K>::from_fn(|l| {
                Array::<BitsInBytes64, N>::from_fn(|i| {
                    let BitsInBytes64(prev) = u[l][(i + N::USIZE - 1) % N::USIZE].clone();
                    let BitsInBytes64(b) = u[l][i].clone();
                    BitsInBytes64(prev.shl(64 - n as u32) | b.shr(n as u32))
                })
            }),
            v: Array::<BitsInBytes64, N>::from_fn(|i| {
                let BitsInBytes64(prev) = v[(i + N::USIZE - 1) % N::USIZE].clone();
                let BitsInBytes64(b) = v[i].clone();
                BitsInBytes64(prev.shl(64 - n as u32) | b.shr(n as u32))
            }),
        }
    }
    pub fn bit(&self, n: u8) -> Vope<N, Bit, K>
    where
        N: VoleArray<Bit>,
    {
        let Vope { u, v } = self;
        Vope {
            u: Array::<Array<Bit, N>, K>::from_fn(|l| {
                Array::<Bit, N>::from_fn(|i| {
                    let BitsInBytes64(b) = u[l][i].clone();
                    Bit((b >> n) & 1 != 0)
                })
            }),
            v: Array::<Bit, N>::from_fn(|i| {
                let BitsInBytes64(b) = v[i].clone();
                Bit((b >> n) & 1 != 0)
            }),
        }
    }
}
