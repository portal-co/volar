use super::*;
impl<B: ByteBlockEncrypt, D: Digest, K: ArrayLength<GenericArray<u8, B::BlockSize>>> ABO<B, D, K> {
    pub fn open<
        T: ArrayLength<
                GenericArray<GenericArray<u8, <B::BlockSize as Max<D::OutputSize>>::Output>, U>,
            > + ArrayLength<u64>,
        U: ArrayLength<GenericArray<u8, <B::BlockSize as Max<D::OutputSize>>::Output>>,
        R: AsRef<[u8]>,
    >(
        &self,
        bad: GenericArray<u64, T>,
        rand: &R,
    ) -> ABOOpening<B, D, T, U>
    where
        B::BlockSize: Max<D::OutputSize>,
        <B::BlockSize as Max<D::OutputSize>>::Output: ArrayLength<u8>,
        T: Mul<U, Output = K>,
    {
        ABOOpening {
            bad: bad.clone(),
            openings: GenericArray::<
                GenericArray<GenericArray<u8, <B::BlockSize as Max<D::OutputSize>>::Output>, U>,
                T,
            >::generate(move |i| {
                let bad = bad.clone();
                GenericArray::<GenericArray<u8, <B::BlockSize as Max<D::OutputSize>>::Output>, U>::generate(move |j| {
                    let i2 = i | ((j as usize) << T::to_usize().ilog2());
                    if bad.contains(&(i2 as u64)) {
                        let h = commit::<D>(&self.per_byte[i2], rand);
                        GenericArray::<u8, <B::BlockSize as Max<D::OutputSize>>::Output>::generate(|j| h.as_ref().get(j).cloned().unwrap_or_default())
                    } else {
                        GenericArray::<u8, <B::BlockSize as Max<D::OutputSize>>::Output>::generate(|j| {
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
    pub fn validate<R: AsRef<[u8]>>(
        &self,
        commit_: &GenericArray<u8, D::OutputSize>,
        rand: &R,
    ) -> bool {
        let mut h = D::new();
        for i in 0..T::to_usize() {
            for b in 0..U::to_usize() {
                let i2 = i | ((b as usize) << T::to_usize().ilog2());
                if self.bad.contains(&(i2 as u64)) {
                    h.update(&self.openings[i][b][..(<D::OutputSize as Unsigned>::to_usize())]);
                } else {
                    h.update(&commit::<D>(
                        &&self.openings[i][b][..(<B::BlockSize as Unsigned>::to_usize())],
                        rand,
                    ));
                }
            }
        }
        h.finalize().as_slice() == commit_.as_slice()
    }
    pub fn to_vole_material<const N: usize>(&self) -> [Vope<B::BlockSize, u8>; N]
    where
        B::BlockSize: VoleArray<u8>,
    {
        core::array::from_fn(|i| {
            let s = &self.openings[i];
            create_vole_from_material::<B, _>(s)
        })
    }
    pub fn to_vole_material_typenum<N: ArrayLength<Vope<B::BlockSize, u8>>>(
        &self,
    ) -> GenericArray<Vope<B::BlockSize, u8>, N>
    where
        B::BlockSize: VoleArray<u8>,
    {
        GenericArray::<Vope<B::BlockSize, u8>, N>::generate(|i| {
            let s = &self.openings[i];
            create_vole_from_material::<B, _>(s)
        })
    }
    pub fn to_vole_material_expanded<const N: usize, X: AsRef<[u8]>, F: FnMut(&[u8]) -> X>(
        &self,
        mut f: F,
    ) -> [Vope<B::BlockSize, u8>; N]
    where
        B::BlockSize: VoleArray<u8>,
    {
        core::array::from_fn(|i| {
            let s = &self.openings[i];
            create_vole_from_material_expanded::<B, X, _, _>(s, &mut f)
        })
    }
    pub fn to_vole_material_typenum_expanded<
        N: ArrayLength<Vope<B::BlockSize, u8>>,
        X: AsRef<[u8]>,
        F: FnMut(&[u8]) -> X,
    >(
        &self,
        mut f: F,
    ) -> GenericArray<Vope<B::BlockSize, u8>, N>
    where
        B::BlockSize: VoleArray<u8>,
    {
        GenericArray::<Vope<B::BlockSize, u8>, N>::generate(|i| {
            let s = &self.openings[i];
            create_vole_from_material_expanded::<B, X, _, _>(s, &mut f)
        })
    }
}
