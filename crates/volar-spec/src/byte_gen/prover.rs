use core::ops::{BitXor, Deref};

use super::*;

impl<B: ByteBlockEncrypt, D: Digest, K: ArrayLength<GenericArray<u8, B::OutputSize>>> ABO<B, D, K> {
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
}
