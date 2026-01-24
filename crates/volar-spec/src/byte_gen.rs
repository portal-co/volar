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
pub trait ByteBlockEncrypt: Digest {
    fn gen_abo<D: Digest, K: ArrayLength<GenericArray<u8, Self::OutputSize>>>(
        a: GenericArray<u8, Self::OutputSize>,
        rand: &impl AsRef<[u8]>,
    ) -> ABO<Self, D, K> where Self: Sized {
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
impl<T: Digest> ByteBlockEncrypt for T {}
pub fn create_vole_from_material<B: ByteBlockEncrypt<OutputSize: VoleArray<u8>>, X: AsRef<[u8]>>(
    s: &[X],
) -> Vope<B::OutputSize, u8> {
    let u: GenericArray<u8, B::OutputSize> =
        s.iter()
            .fold(GenericArray::<u8, B::OutputSize>::default(), |a, b| {
                a.zip(
                    GenericArray::<u8, B::OutputSize>::generate(|i| b.as_ref()[i]),
                    |a, b| a.bitxor(b),
                )
            });
    let v: GenericArray<u8, B::OutputSize> =
        s.iter()
            .enumerate()
            .fold(GenericArray::<u8, B::OutputSize>::default(), |a, (i, b)| {
                a.zip(
                    GenericArray::<u8, B::OutputSize>::generate(|i| b.as_ref()[i]),
                    |a, b| a.bitxor(b).bitxor(i as u8),
                )
            });
    Vope {
        u: GenericArray::<GenericArray<u8, B::OutputSize>, U1>::generate(|_| u.clone()),
        v,
    }
}
pub fn create_vole_from_material_expanded<
    B: ByteBlockEncrypt<OutputSize: VoleArray<u8>>,
    X: AsRef<[u8]>,
    Y: AsRef<[u8]>,
    F: FnMut(&[u8]) -> X,
>(
    s: &[Y],
    mut f: F,
) -> Vope<B::OutputSize, u8> {
    let u: GenericArray<u8, B::OutputSize> = s
        .iter()
        .map(|b| f(&b.as_ref()[..(<B::OutputSize as Unsigned>::to_usize())]))
        .fold(GenericArray::<u8, B::OutputSize>::default(), |a, b| {
            a.zip(
                GenericArray::<u8, B::OutputSize>::generate(|i| b.as_ref()[i]),
                |a, b| a.bitxor(b),
            )
        });
    let v: GenericArray<u8, B::OutputSize> = s
        .iter()
        .map(|b| f(&b.as_ref()[..(<B::OutputSize as Unsigned>::to_usize())]))
        .enumerate()
        .fold(GenericArray::<u8, B::OutputSize>::default(), |a, (i, b)| {
            a.zip(
                GenericArray::<u8, B::OutputSize>::generate(|i| b.as_ref()[i]),
                |a, b| a.bitxor(b).bitxor(i as u8),
            )
        });
    Vope {
        u: GenericArray::<GenericArray<u8, B::OutputSize>, U1>::generate(|_| u.clone()),
        v,
    }
}
pub struct ABO<B: ByteBlockEncrypt, D: Digest, K: ArrayLength<GenericArray<u8, B::OutputSize>>> {
    pub commit: GenericArray<u8, D::OutputSize>,
    pub per_byte: GenericArray<GenericArray<u8, B::OutputSize>, K>,
}
pub struct ABOOpening<
    B: ByteBlockEncrypt<OutputSize: Max<D::OutputSize, Output: ArrayLength<u8>>>,
    D: Digest,
    T: ArrayLength<
            GenericArray<GenericArray<u8, <B::OutputSize as Max<D::OutputSize>>::Output>, U>,
        > + ArrayLength<u64>,
    U: ArrayLength<GenericArray<u8, <B::OutputSize as Max<D::OutputSize>>::Output>>,
> {
    pub bad: GenericArray<u64, T>,
    pub openings: GenericArray<
        GenericArray<GenericArray<u8, <B::OutputSize as Max<D::OutputSize>>::Output>, U>,
        T,
    >,
}

pub mod prover;
pub mod verifier;
pub fn double<B: ByteBlockEncrypt>(
    a: GenericArray<u8, B::OutputSize>,
) -> [GenericArray<u8, B::OutputSize>; 2] {
    let v = B::digest(&a);
    [v.clone(), v.zip(a, |x, y| x ^ y)]
}
pub type ByteId = [bool; 5];
