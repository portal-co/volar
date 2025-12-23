use crate::byte_gen::prover::{create_vole_from_material, create_vole_from_material_expanded};

use super::*;
impl<B: ByteBlockEncrypt, D: Digest, K: ArrayLength<GenericArray<u8, B::BlockSize>>> ABO<B, D, K> {
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
    pub fn to_vole_material<const N: usize>(&self) -> [Vole<B::BlockSize, u8>; N]
    where
        B::BlockSize: VoleArray<u8>,
    {
        core::array::from_fn(|i| {
            let s = &self.openings[i];
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
            let s = &self.openings[i];
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
            let s = &self.openings[i];
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
            let s = &self.openings[i];
            create_vole_from_material_expanded::<B, X>(s, &mut f)
        })
    }
}
