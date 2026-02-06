use core::ops::{BitXor, Deref};

use super::*;

impl<B: LengthDoubler, D: Digest, K: ArrayLength<GenericArray<u8, B::OutputSize>>> ABO<B, D, K> {
    pub fn to_vole_material<const N: usize>(&self) -> [Vope<B::OutputSize, u8>; N]
    where
        B::OutputSize: VoleArray<u8>,
    {
        core::array::from_fn(|i| {
            let s = &self.per_byte[(i * N)..][..N];
            create_vole_from_material::<B, _>(s)
        })
    }
    pub fn to_vole_material_typenum<N: ArrayLength<Vope<B::OutputSize, u8>>>(
        &self,
    ) -> GenericArray<Vope<B::OutputSize, u8>, N>
    where
        B::OutputSize: VoleArray<u8>,
    {
        GenericArray::<Vope<B::OutputSize, u8>, N>::generate(|i| {
            let s = &self.per_byte[(i * N::to_usize())..][..N::to_usize()];
            create_vole_from_material::<B, _>(s)
        })
    }
    pub fn to_vole_material_expanded<const N: usize, X: AsRef<[u8]>, F: FnMut(&[u8]) -> X>(
        &self,
        mut f: F,
    ) -> [Vope<B::OutputSize, u8>; N]
    where
        B::OutputSize: VoleArray<u8>,
    {
        core::array::from_fn(|i| {
            let s = &self.per_byte[(i * N)..][..N];
            create_vole_from_material_expanded::<B, X, _, _>(s, &mut f)
        })
    }
    pub fn to_vole_material_typenum_expanded<
        N: ArrayLength<Vope<B::OutputSize, u8>>,
        X: AsRef<[u8]>,
        F: FnMut(&[u8]) -> X,
    >(
        &self,
        mut f: F,
    ) -> GenericArray<Vope<B::OutputSize, u8>, N>
    where
        B::OutputSize: VoleArray<u8>,
    {
        GenericArray::<Vope<B::OutputSize, u8>, N>::generate(|i| {
            let s = &self.per_byte[(i * N::to_usize())..][..N::to_usize()];
            create_vole_from_material_expanded::<B, X, _, _>(s, &mut f)
        })
    }
    pub fn split_bit_typenum<N: ArrayLength<BSplit<B, D>>>(&self) -> GenericArray<BSplit<B, D>, N>
    where
        B::OutputSize: VoleArray<u8>,
        D: Digest<
            OutputSize: Logarithm2<Output: ArrayLength<[GenericArray<u8, B::OutputSize>; 2]>>,
        >,
    {
        GenericArray::<BSplit<B, D>, N>::generate(|i| {
            let s = &self.per_byte[(i * N::to_usize())..][..N::to_usize()];
            BSplit {
                split: GenericArray::<
                    [GenericArray<u8, B::OutputSize>; 2],
                    <D::OutputSize as Logarithm2>::Output,
                >::generate(|j| {
                    core::array::from_fn(|b| {
                        s.iter()
                            .enumerate()
                            .filter_map(|(a, c)| {
                                if a >> j & 1 == b {
                                    Some(c.clone())
                                } else {
                                    None
                                }
                            })
                            .fold(GenericArray::<u8, B::OutputSize>::default(), |a, b| {
                                a.zip(
                                    GenericArray::<u8, B::OutputSize>::generate(|i| b.as_ref()[i]),
                                    |a, b| a.bitxor(b),
                                )
                            })
                    })
                }),
            }
        })
    }
}
