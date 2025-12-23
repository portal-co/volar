use core::ops::{BitXor, Deref};

use super::*;
pub fn create_vole_from_material<B: ByteBlockEncrypt<BlockSize: VoleArray<u8>>>(
    s: &[impl Deref<Target = [u8]>],
) -> Vole<B::BlockSize, u8> {
    let u: GenericArray<u8, B::BlockSize> = s
        .iter()
        .fold(GenericArray::<u8, B::BlockSize>::default(), |a, b| {
            a.zip(GenericArray::generate(|i| b[i]), |a, b| a.bitxor(b))
        });
    let v: GenericArray<u8, B::BlockSize> =
        s.iter()
            .enumerate()
            .fold(GenericArray::<u8, B::BlockSize>::default(), |a, (i, b)| {
                a.zip(GenericArray::generate(|i| b[i]), |a, b| {
                    a.bitxor(b).bitxor(i as u8)
                })
            });
    Vole { u, v }
}
pub fn create_vole_from_material_expanded<
    B: ByteBlockEncrypt<BlockSize: VoleArray<u8>>,
    X: AsRef<[u8]>,
>(
    s: &[impl Deref<Target = [u8]>],
    mut f: impl FnMut(&[u8]) -> X,
) -> Vole<B::BlockSize, u8> {
    let u: GenericArray<u8, B::BlockSize> = s
        .iter()
        .map(|b| f(&b[..(B::BlockSize::to_usize())]))
        .fold(GenericArray::<u8, B::BlockSize>::default(), |a, b| {
            a.zip(GenericArray::generate(|i| b.as_ref()[i]), |a, b| {
                a.bitxor(b)
            })
        });
    let v: GenericArray<u8, B::BlockSize> = s
        .iter()
        .map(|b| f(&b[..(B::BlockSize::to_usize())]))
        .enumerate()
        .fold(GenericArray::<u8, B::BlockSize>::default(), |a, (i, b)| {
            a.zip(GenericArray::generate(|i| b.as_ref()[i]), |a, b| {
                a.bitxor(b).bitxor(i as u8)
            })
        });
    Vole { u, v }
}
impl<B: ByteBlockEncrypt, D: Digest, K: ArrayLength<GenericArray<u8, B::BlockSize>>> ABO<B, D, K> {
    pub fn to_vole_material<const N: usize>(&self) -> [Vole<B::BlockSize, u8>; N]
    where
        B::BlockSize: VoleArray<u8>,
    {
        core::array::from_fn(|i| {
            let s = &self.per_byte[(i * N)..][..N];
            create_vole_from_material::<B>(s)
        })
    }
    pub fn to_vole_material_typenum<N: ArrayLength<Vole<B::BlockSize, u8>>>(
        &self,
    ) -> GenericArray<Vole<B::BlockSize, u8>, N>
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
    ) -> [Vole<B::BlockSize, u8>; N]
    where
        B::BlockSize: VoleArray<u8>,
    {
        core::array::from_fn(|i| {
            let s = &self.per_byte[(i * N)..][..N];
            create_vole_from_material_expanded::<B, X>(s, &mut f)
        })
    }
    pub fn to_vole_material_typenum_expanded<
        N: ArrayLength<Vole<B::BlockSize, u8>>,
        X: AsRef<[u8]>,
    >(
        &self,
        mut f: impl FnMut(&[u8]) -> X,
    ) -> GenericArray<Vole<B::BlockSize, u8>, N>
    where
        B::BlockSize: VoleArray<u8>,
    {
        GenericArray::generate(|i| {
            let s = &self.per_byte[(i * N::to_usize())..][..N::to_usize()];
            create_vole_from_material_expanded::<B, X>(s, &mut f)
        })
    }
}
