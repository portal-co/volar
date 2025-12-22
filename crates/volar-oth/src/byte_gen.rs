use core::array;

use cipher::{
    BlockEncrypt, Unsigned,
    consts::{U1, U8},
    typenum::Max,
};

use crate::{
    simple::CommitmentCore,
    vole::{ByteVole, Vole, VoleArray},
};

use super::*;
pub trait ByteBlockEncrypt: BlockEncrypt + From<[u8; 32]> {
    fn gen_abo<D: Digest>(
        a: GenericArray<u8, Self::BlockSize>,
        rand: &impl AsRef<[u8]>,
    ) -> ABO<Self, D> {
        let mut h = D::new();
        let mut per_byte =
            core::array::from_fn(|_| core::array::from_fn(|_| GenericArray::default()));
        for i in 0..32 {
            let id: ByteId = core::array::from_fn(|j| ((i >> j) & 1) != 0);
            let core = id.iter().fold(a.clone(), |mut acc, &b| {
                if b {
                    let doubled = double::<Self>(acc);
                    acc = doubled[1].clone();
                } else {
                    let doubled = double::<Self>(acc);
                    acc = doubled[0].clone();
                }
                acc
            });
            let mut per_byte = &mut per_byte[i];
            for b in 0..256 {
                let core = (0..8).fold(core.clone(), |mut acc, j| {
                    if (b >> j) & 1 != 0 {
                        let doubled = double::<Self>(acc);
                        acc = doubled[1].clone();
                    } else {
                        let doubled = double::<Self>(acc);
                        acc = doubled[0].clone();
                    }
                    acc
                });

                h.update(&CommitmentCore::<D>::commit(&core, rand));
                per_byte[b] = core;
            }
        }
        ABO::<Self, D> {
            commit: h.finalize(),
            per_byte,
        }
    }
}
impl<T: BlockEncrypt + From<[u8; 32]>> ByteBlockEncrypt for T {}
pub struct ABO<B: ByteBlockEncrypt, D: Digest> {
    pub commit: GenericArray<u8, D::OutputSize>,
    pub per_byte: [[GenericArray<u8, B::BlockSize>; 256]; 32],
}
pub struct ABOOpening<
    B: ByteBlockEncrypt<BlockSize: Max<D::OutputSize, Output: ArrayLength<u8>>>,
    D: Digest,
> {
    pub bad: [(ByteId, u8); 32],
    pub openings: [[GenericArray<u8, <B::BlockSize as Max<D::OutputSize>>::Output>; 256]; 32],
}
impl<
    B: ByteBlockEncrypt<BlockSize: Unsigned + Max<D::OutputSize, Output: ArrayLength<u8>>>,
    D: Digest<OutputSize: Unsigned>,
> ABOOpening<B, D>
{
    pub fn validate(
        &self,
        commit: &GenericArray<u8, D::OutputSize>,
        rand: &impl AsRef<[u8]>,
    ) -> bool {
        let mut h = D::new();
        for i in 0..32 {
            for b in 0..256 {
                let id: ByteId = core::array::from_fn(|j| (i >> j) & 1 != 0);
                if self.bad.contains(&(id, b as u8)) {
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
impl<B: ByteBlockEncrypt, D: Digest> ABO<B, D> {
    pub fn to_vole_material(&self) -> [Vole<B::BlockSize, u8>; 32]
    where
        B::BlockSize: VoleArray<u8>,
    {
        core::array::from_fn(|i| {
            let s = &self.per_byte[i];
            let u: GenericArray<u8, B::BlockSize> = s
                .iter()
                .cloned()
                .fold(GenericArray::<u8, B::BlockSize>::default(), |a, b| {
                    a.zip(b, |a, b| a.wrapping_add(b))
                });
            let v: GenericArray<u8, B::BlockSize> = s
                .iter()
                .cloned()
                .enumerate()
                .fold(GenericArray::<u8, B::BlockSize>::default(), |a, (i, b)| {
                    a.zip(b, |a, b| a.wrapping_add(b).wrapping_add(i as u8))
                });
            Vole { u, v }
        })
    }
    pub fn open(&self, bad: [(ByteId, u8); 32], rand: &impl AsRef<[u8]>) -> ABOOpening<B, D>
    where
        B::BlockSize: Max<D::OutputSize>,
        <B::BlockSize as Max<D::OutputSize>>::Output: ArrayLength<u8>,
    {
        ABOOpening {
            bad,
            openings: core::array::from_fn(|i| {
                let id: ByteId = core::array::from_fn(|j| (i >> j) & 1 != 0);
                core::array::from_fn(|b| {
                    let b = b as u8;
                    if bad.contains(&(id, b)) {
                        let h = CommitmentCore::<D>::commit(&self.per_byte[i][b as usize], rand);
                        GenericArray::generate(|j| h.as_ref().get(j).cloned().unwrap_or_default())
                    } else {
                        GenericArray::generate(|j| {
                            self.per_byte[i][b as usize]
                                .get(j)
                                .cloned()
                                .unwrap_or_default()
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
