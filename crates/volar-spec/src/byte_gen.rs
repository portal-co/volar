use core::{
    array,
    ops::{BitXor, Deref, Mul},
};

use cipher::{
    BlockEncrypt, Unsigned,
    consts::{U1, U8},
    typenum::{Logarithm2, Max},
};

use super::*;
use crate::{
    simple::CommitmentCore,
    vole::{VoleArray, vope::Vope},
};
use volar_common::hash_commitment::commit;
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

            h.update(&commit::<D>(&core, rand));
            per_byte[i] = core;
        }
        ABO::<Self, D, K> {
            commit: h.finalize(),
            per_byte,
        }
    }
}
impl<T: BlockEncrypt + From<[u8; 32]>> ByteBlockEncrypt for T {}
pub fn create_vole_from_material<B: ByteBlockEncrypt<BlockSize: VoleArray<u8>>>(
    s: &[impl Deref<Target = [u8]>],
) -> Vope<B::BlockSize, u8> {
    let u: GenericArray<u8, B::BlockSize> =
        s.iter()
            .fold(GenericArray::<u8, B::BlockSize>::default(), |a, b| {
                a.zip(
                    GenericArray::<u8, B::BlockSize>::generate(|i| b[i]),
                    |a, b| a.bitxor(b),
                )
            });
    let v: GenericArray<u8, B::BlockSize> =
        s.iter()
            .enumerate()
            .fold(GenericArray::<u8, B::BlockSize>::default(), |a, (i, b)| {
                a.zip(
                    GenericArray    ::generate(|i| b[i]),
                    |a, b| a.bitxor(b).bitxor(i as u8),
                )
            });
    Vope {
        u: GenericArray::generate(|_| u.clone()),
        v,
    }
}
pub fn create_vole_from_material_expanded<
    B: ByteBlockEncrypt<BlockSize: VoleArray<u8>>,
    X: AsRef<[u8]>,
>(
    s: &[impl Deref<Target = [u8]>],
    mut f: impl FnMut(&[u8]) -> X,
) -> Vope<B::BlockSize, u8> {
    let u: GenericArray<u8, B::BlockSize> = s
        .iter()
        .map(|b| f(&b[..(<B::BlockSize as Unsigned>::to_usize())]))
        .fold(GenericArray::<u8, B::BlockSize>::default(), |a, b| {
            a.zip(GenericArray::<u8, B::BlockSize>::generate(|i| b.as_ref()[i]), |a, b| {
                a.bitxor(b)
            })
        });
    let v: GenericArray<u8, B::BlockSize> = s
        .iter()
        .map(|b| f(&b[..(<B::BlockSize as Unsigned>::to_usize())]))
        .enumerate()
        .fold(GenericArray::<u8, B::BlockSize>::default(), |a, (i, b)| {
            a.zip(GenericArray::<u8, B::BlockSize>::generate(|i| b.as_ref()[i]), |a, b| {
                a.bitxor(b).bitxor(i as u8)
            })
        });
    Vope {
        u: GenericArray::<GenericArray<u8, B::BlockSize>, U1>::generate(|_| u.clone()),
        v,
    }
}
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

pub mod prover;
pub mod verifier;
pub fn double<B: ByteBlockEncrypt>(
    a: GenericArray<u8, B::BlockSize>,
) -> [GenericArray<u8, B::BlockSize>; 2] {
    return core::array::from_fn::<GenericArray<u8, B::BlockSize>,2,_>(|i| {
        let mut out = a.clone();
        let mut b = B::from([(i as u8); 32]);
        b.encrypt_block(&mut out);
        out
    });
}
pub type ByteId = [bool; 5];
