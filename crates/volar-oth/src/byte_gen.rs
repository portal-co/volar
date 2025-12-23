use core::{array, ops::Mul};

use cipher::{
    BlockEncrypt, Unsigned,
    consts::{U1, U8},
    typenum::{Logarithm2, Max},
};

use crate::{
    simple::CommitmentCore,
    vole::{ByteVole, Vole, VoleArray},
};

use super::*;
pub trait ByteBlockEncrypt: BlockEncrypt + From<[u8; 32]> {
    fn gen_abo<D: Digest, K: ArrayLength<GenericArray<u8, Self::BlockSize>>>(
        a: GenericArray<u8, Self::BlockSize>,
        rand: &impl AsRef<[u8]>,
    ) -> ABO<Self, D, K> {
        let mut h = D::new();
        let mut per_byte = GenericArray::default();
        for i in 0..K::to_usize() {
            // let id: ByteId = core::array::from_fn(|j| ((i >> j) & 1) != 0);
            let core = (0..K::to_usize().ilog2()).fold(a.clone(), |mut acc, b| {
                if (i >> b) & 1 != 0 {
                    let doubled = double::<Self>(acc);
                    acc = doubled[1].clone();
                } else {
                    let doubled = double::<Self>(acc);
                    acc = doubled[0].clone();
                }
                acc
            });

            h.update(&CommitmentCore::<D>::commit(&core, rand));
            per_byte[i] = core;
        }
        ABO::<Self, D, K> {
            commit: h.finalize(),
            per_byte,
        }
    }
}
impl<T: BlockEncrypt + From<[u8; 32]>> ByteBlockEncrypt for T {}
pub struct ABO<B: ByteBlockEncrypt, D: Digest, K: ArrayLength<GenericArray<u8, B::BlockSize>>> {
    pub commit: GenericArray<u8, D::OutputSize>,
    pub per_byte: GenericArray<GenericArray<u8, B::BlockSize>, K>,
}
pub struct ABOOpening<
    B: ByteBlockEncrypt<BlockSize: Max<D::OutputSize, Output: ArrayLength<u8>>>,
    D: Digest,
    T: ArrayLength<GenericArray<GenericArray<u8, <B::BlockSize as Max<D::OutputSize>>::Output>, U>>
        + ArrayLength<u64>,
    U: ArrayLength<GenericArray<u8, <B::BlockSize as Max<D::OutputSize>>::Output>>,
> {
    pub bad: GenericArray<u64, T>,
    pub openings: GenericArray<
        GenericArray<GenericArray<u8, <B::BlockSize as Max<D::OutputSize>>::Output>, U>,
        T,
    >,
}
impl<
    B: ByteBlockEncrypt<BlockSize: Unsigned + Max<D::OutputSize, Output: ArrayLength<u8>>>,
    D: Digest<OutputSize: Unsigned>,
    T: ArrayLength<GenericArray<GenericArray<u8, <B::BlockSize as Max<D::OutputSize>>::Output>, U>>
        + ArrayLength<u64>,
    U: ArrayLength<GenericArray<u8, <B::BlockSize as Max<D::OutputSize>>::Output>>,
> ABOOpening<B, D, T, U>
{
    pub fn validate(
        &self,
        commit: &GenericArray<u8, D::OutputSize>,
        rand: &impl AsRef<[u8]>,
    ) -> bool {
        let mut h = D::new();
        for i in 0..T::to_usize() {
            for b in 0..U::to_usize() {
                let i2 = i | ((b as usize) << T::to_usize().ilog2());
                if self.bad.contains(&(i2 as u64)) {
                    h.update(&self.openings[i][b][..(D::OutputSize::to_usize())]);
                } else {
                    h.update(&CommitmentCore::<D>::commit(
                        &&self.openings[i][b][..(B::BlockSize::to_usize())],
                        rand,
                    ));
                }
            }
        }
        h.finalize().as_slice() == commit.as_slice()
    }
}
impl<B: ByteBlockEncrypt, D: Digest, K: ArrayLength<GenericArray<u8, B::BlockSize>>> ABO<B, D, K> {
    pub fn to_vole_material<const N: usize>(&self) -> [Vole<B::BlockSize, u8>; N]
    where
        B::BlockSize: VoleArray<u8>,
    {
        core::array::from_fn(|i| {
            let s = &self.per_byte[i / N][..N];
            let u: GenericArray<u8, B::BlockSize> = s
                .chunks(B::BlockSize::to_usize())
                .fold(GenericArray::<u8, B::BlockSize>::default(), |a, b| {
                    a.zip(GenericArray::generate(|i| b[i]), |a, b| a.wrapping_add(b))
                });
            let v: GenericArray<u8, B::BlockSize> = s
                .chunks(B::BlockSize::to_usize())
                .enumerate()
                .fold(GenericArray::<u8, B::BlockSize>::default(), |a, (i, b)| {
                    a.zip(GenericArray::generate(|i| b[i]), |a, b| {
                        a.wrapping_add(b).wrapping_add(i as u8)
                    })
                });
            Vole { u, v }
        })
    }
    pub fn to_vole_material_typenum<N: ArrayLength<Vole<B::BlockSize, u8>>>(
        &self,
    ) -> GenericArray<Vole<B::BlockSize, u8>, N>
    where
        B::BlockSize: VoleArray<u8>,
    {
        GenericArray::generate(|i| {
            let s = &self.per_byte[i / N::to_usize()][..N::to_usize()];
            let u: GenericArray<u8, B::BlockSize> = s
                .chunks(B::BlockSize::to_usize())
                .fold(GenericArray::<u8, B::BlockSize>::default(), |a, b| {
                    a.zip(GenericArray::generate(|i| b[i]), |a, b| a.wrapping_add(b))
                });
            let v: GenericArray<u8, B::BlockSize> = s
                .chunks(B::BlockSize::to_usize())
                .enumerate()
                .fold(GenericArray::<u8, B::BlockSize>::default(), |a, (i, b)| {
                    a.zip(GenericArray::generate(|i| b[i]), |a, b| {
                        a.wrapping_add(b).wrapping_add(i as u8)
                    })
                });
            Vole { u, v }
        })
    }
    pub fn open<
        T: ArrayLength<
                GenericArray<GenericArray<u8, <B::BlockSize as Max<D::OutputSize>>::Output>, U>,
            > + ArrayLength<u64>,
        U: ArrayLength<GenericArray<u8, <B::BlockSize as Max<D::OutputSize>>::Output>>,
    >(
        &self,
        bad: GenericArray<u64, T>,
        rand: &impl AsRef<[u8]>,
    ) -> ABOOpening<B, D, T, U>
    where
        B::BlockSize: Max<D::OutputSize>,
        <B::BlockSize as Max<D::OutputSize>>::Output: ArrayLength<u8>,
        T: Mul<U, Output = K>,
    {
        ABOOpening {
            bad: bad.clone(),
            openings: GenericArray::generate(move |i| {
                let bad = bad.clone();
                GenericArray::generate(move |j| {
                    let i2 = i | ((j as usize) << T::to_usize().ilog2());
                    if bad.contains(&(i2 as u64)) {
                        let h = CommitmentCore::<D>::commit(&self.per_byte[i2], rand);
                        GenericArray::generate(|j| h.as_ref().get(j).cloned().unwrap_or_default())
                    } else {
                        GenericArray::generate(|j| {
                            self.per_byte[i2].get(j).cloned().unwrap_or_default()
                        })
                    }
                })
            }),
        }
    }
}
pub fn double<B: ByteBlockEncrypt>(
    a: GenericArray<u8, B::BlockSize>,
) -> [GenericArray<u8, B::BlockSize>; 2] {
    return core::array::from_fn(|i| {
        let mut out = a.clone();
        let mut b = B::from([(i as u8); 32]);
        b.encrypt_block(&mut out);
        out
    });
}
pub type ByteId = [bool; 5];
