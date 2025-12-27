use core::ops::{BitXor, Deref};

use super::*;

impl<B: ByteBlockEncrypt, D: Digest, K: ArrayLength<GenericArray<u8, B::BlockSize>>> ABO<B, D, K> {
    pub fn to_vole_material<const N: usize>(&self) -> [Vope<B::BlockSize, u8>; N]
    where
        B::BlockSize: VoleArray<u8>,
    {
        core::array::from_fn(|i| {
            let s = &self.per_byte[(i * N)..][..N];
            create_vole_from_material::<B>(s)
        })
    }
    pub fn to_vole_material_typenum<N: ArrayLength<Vope<B::BlockSize, u8>>>(
        &self,
    ) -> GenericArray<Vope<B::BlockSize, u8>, N>
    where
        B::BlockSize: VoleArray<u8>,
    {
        GenericArray::generate(|i| {
            let s = &self.per_byte[(i * N::to_usize())..][..N::to_usize()];
            create_vole_from_material::<B>(s)
        })
    }
    pub fn to_vole_material_expanded<const N: usize, X: AsRef<[u8]>>(
        &self,
        mut f: impl FnMut(&[u8]) -> X,
    ) -> [Vope<B::BlockSize, u8>; N]
    where
        B::BlockSize: VoleArray<u8>,
    {
        core::array::from_fn(|i| {
            let s = &self.per_byte[(i * N)..][..N];
            create_vole_from_material_expanded::<B, X>(s, &mut f)
        })
    }
    pub fn to_vole_material_typenum_expanded<
        N: ArrayLength<Vope<B::BlockSize, u8>>,
        X: AsRef<[u8]>,
    >(
        &self,
        mut f: impl FnMut(&[u8]) -> X,
    ) -> GenericArray<Vope<B::BlockSize, u8>, N>
    where
        B::BlockSize: VoleArray<u8>,
    {
        GenericArray::generate(|i| {
            let s = &self.per_byte[(i * N::to_usize())..][..N::to_usize()];
            create_vole_from_material_expanded::<B, X>(s, &mut f)
        })
    }
}
