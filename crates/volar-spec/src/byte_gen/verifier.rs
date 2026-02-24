use super::*;
impl<
    B: LengthDoubler,
    D: Digest,
    K: ArrayLength<GenericArray<u8, B::OutputSize>>,
    N: ArrayLength<GenericArray<GenericArray<u8, B::OutputSize>, K>>,
> ABO<B, D, K, N>
{
    pub fn open<
        T: ArrayLength<
                GenericArray<GenericArray<u8, <B::OutputSize as Max<D::OutputSize>>::Output>, U>,
            > + ArrayLength<u64>,
        U: ArrayLength<GenericArray<u8, <B::OutputSize as Max<D::OutputSize>>::Output>>,
        R: AsRef<[u8]>,
        M: ArrayLength<u8>,
    >(
        &self,
        bad: GenericArray<u64, T>,
        rand: &R,
    ) -> ABOOpening<B, D, T, U, N>
    where
        B::OutputSize: Max<D::OutputSize, Output = M>,
        T: Mul<U, Output = K>,
        N: ArrayLength<
            GenericArray<
                GenericArray<GenericArray<u8, <B::OutputSize as Max<D::OutputSize>>::Output>, U>,
                T,
            >,
        >,
    {
        ABOOpening {
            bad: bad.clone(),
            openings: GenericArray::generate(move |ni| {
                let bad = bad.clone();
                GenericArray::<GenericArray<GenericArray<u8, M>, U>, T>::generate(move |i| {
                    let bad = bad.clone();
                    GenericArray::<GenericArray<u8, M>, U>::generate(move |j| {
                        let i2 = i | ((j as usize) << T::to_usize().ilog2());
                        if bad.contains(&(i2 as u64)) {
                            let h = commit::<D>(&self.per_byte[ni][i2], rand);
                            GenericArray::<u8, M>::generate(|j| {
                                h.as_ref().get(j).cloned().unwrap_or_default()
                            })
                        } else {
                            GenericArray::<u8, M>::generate(|j| {
                                self.per_byte[ni][i2].get(j).cloned().unwrap_or_default()
                            })
                        }
                    })
                })
            }),
        }
    }
}
impl<
    B: LengthDoubler<OutputSize: Unsigned + Max<D::OutputSize, Output: ArrayLength<u8>>>,
    D: Digest<OutputSize: Unsigned>,
    T: ArrayLength<
            GenericArray<GenericArray<u8, <B::OutputSize as Max<D::OutputSize>>::Output>, U>,
        > + ArrayLength<u64>,
    U: ArrayLength<GenericArray<u8, <B::OutputSize as Max<D::OutputSize>>::Output>>,
    NOthers: ArrayLength<
        GenericArray<
            GenericArray<GenericArray<u8, <B::OutputSize as Max<D::OutputSize>>::Output>, U>,
            T,
        >,
    >,
> ABOOpening<B, D, T, U, NOthers>
{
    pub fn validate<'a, R: AsRef<[u8]>>(
        this: GenericArray<&'a Self, NOthers>,
        me: &'a ABO<B, D, <T as Mul<U>>::Output, NOthers>,
        commit_: GenericArray<&'a GenericArray<u8, D::OutputSize>, NOthers>,
        rand: &R,
    ) -> bool
    where
        NOthers: ArrayLength<&'a GenericArray<u8, D::OutputSize>>
            + ArrayLength<&'a Self>
            + ArrayLength<GenericArray<GenericArray<u8, B::OutputSize>, <T as Mul<U>>::Output>>,
        T: Mul<U, Output: ArrayLength<GenericArray<u8, B::OutputSize>>>,
    {
        commit_.iter().enumerate().all(|(ci, commit_)| {
            let mut h = D::new();
            if ci != 0 {
                for i in 0..T::to_usize() {
                    for b in 0..U::to_usize() {
                        let i2 = i | ((b as usize) << T::to_usize().ilog2());
                        h.update(&commit::<D>(
                            &me.per_byte[ci][i2],
                            rand,
                        ));
                    }
                }
            }
            for (idx, this) in this.iter().enumerate() {
                if idx + 1 == ci {
                    continue;
                }
                for i in 0..T::to_usize() {
                    for b in 0..U::to_usize() {
                        let i2 = i | ((b as usize) << T::to_usize().ilog2());
                        if this.bad.contains(&(i2 as u64)) {
                            h.update(
                                &this.openings[ci][i][b]
                                    [..(<D::OutputSize as Unsigned>::to_usize())],
                            );
                        } else {
                            h.update(&commit::<D>(
                                &&this.openings[ci][i][b]
                                    [..(<B::OutputSize as Unsigned>::to_usize())],
                                rand,
                            ));
                        }
                    }
                }
            }
            h.finalize().as_slice() == commit_.as_slice()
        })
    }
    pub fn to_vole_material<const N: usize>(&self, party: usize) -> [Vope<B::OutputSize, u8>; N]
    where
        B::OutputSize: VoleArray<u8>,
    {
        core::array::from_fn(|i| {
            let s = &self.openings[party][i];
            create_vole_from_material::<B, _>(s)
        })
    }
    pub fn to_vole_material_typenum<N: ArrayLength<Vope<B::OutputSize, u8>>>(
        &self,
        party: usize,
    ) -> GenericArray<Vope<B::OutputSize, u8>, N>
    where
        B::OutputSize: VoleArray<u8>,
    {
        GenericArray::<Vope<B::OutputSize, u8>, N>::generate(|i| {
            let s = &self.openings[party][i];
            create_vole_from_material::<B, _>(s)
        })
    }
    pub fn to_vole_material_expanded<const N: usize, X: AsRef<[u8]>, F: FnMut(&[u8]) -> X>(
        &self,
        party: usize,
        mut f: F,
    ) -> [Vope<B::OutputSize, u8>; N]
    where
        B::OutputSize: VoleArray<u8>,
    {
        core::array::from_fn(|i| {
            let s = &self.openings[party][i];
            create_vole_from_material_expanded::<B, X, _, _>(s, &mut f)
        })
    }
    pub fn to_vole_material_typenum_expanded<
        N: ArrayLength<Vope<B::OutputSize, u8>>,
        X: AsRef<[u8]>,
        F: FnMut(&[u8]) -> X,
    >(
        &self,
        party: usize,
        mut f: F,
    ) -> GenericArray<Vope<B::OutputSize, u8>, N>
    where
        B::OutputSize: VoleArray<u8>,
    {
        GenericArray::<Vope<B::OutputSize, u8>, N>::generate(|i| {
            let s = &self.openings[party][i];
            create_vole_from_material_expanded::<B, X, _, _>(s, &mut f)
        })
    }

    pub fn split_bit_typenum<N: ArrayLength<BSplit<B, D>>>(
        &self,
        party: usize,
    ) -> GenericArray<BSplit<B, D>, N>
    where
        B::OutputSize: VoleArray<u8>,
        D: Digest<
            OutputSize: Logarithm2<Output: ArrayLength<[GenericArray<u8, B::OutputSize>; 2]>>,
        >,
    {
        GenericArray::<BSplit<B, D>, N>::generate(|i| {
            let s = &self.openings[party][i];
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
