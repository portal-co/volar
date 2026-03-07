// @reliability: experimental
// @experimental-status: review-pending
// @experimental-since: 263eab1 (fix name)
//! @ai: assisted
// Prover-side byte generation (ABO prover protocol).
// Revised after cda059c (actually unsound, oops). The current construction
// has not been independently verified for soundness.
//
// `open` is ungated: it iterates over all N parties and has no party-selection
// logic, so PartyIndex is not relevant to it.
//
// The vole / split methods use PartyIndex to resolve the target party.
// Without `multi_party` this is satisfied only by U1 (always returns 0);
// with `multi_party` all N: Unsigned satisfy it.
use core::ops::BitXor;

use cipher::{
    ArrayLength, Unsigned,
    consts::U1,
    generic_array::{functional::FunctionalSequence, sequence::GenericSequence},
    typenum::{Logarithm2, Max},
};
use digest::Digest;

use super::*;
use crate::vole::{VoleArray, vope::Vope};
use volar_common::hash_commitment::commit;

// ---------------------------------------------------------------------------
// open — ungated, works for any N
// ---------------------------------------------------------------------------

impl<
    B: LengthDoubler,
    D: Digest,
    K: ArrayLength<GenericArray<u8, B::OutputSize>>,
    N: ArrayLength<GenericArray<GenericArray<u8, B::OutputSize>, K>>,
> ABO<B, D, K, N>
{
    /// Generate an [`ABOOpening`] for each party; bad positions are replaced
    /// with hash commitments, revealing the rest directly.
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

// ---------------------------------------------------------------------------
// Prover vole methods — gated by PartyIndex
// ---------------------------------------------------------------------------

impl<
    B: LengthDoubler,
    D: Digest,
    K: ArrayLength<GenericArray<u8, B::OutputSize>>,
    N: ArrayLength<GenericArray<GenericArray<u8, B::OutputSize>, K>> + PartyIndex,
> ABO<B, D, K, N>
{
    pub fn to_vole_material<const M: usize>(&self, target: usize) -> [Vope<B::OutputSize, u8>; M]
    where
        B::OutputSize: VoleArray<u8>,
    {
        core::array::from_fn(|i| {
            let s = &self.per_byte[N::party_index(target)][(i * M)..][..M];
            create_vole_from_material::<B, _>(s)
        })
    }

    pub fn to_vole_material_typenum<M: ArrayLength<Vope<B::OutputSize, u8>>>(
        &self,
        target: usize,
    ) -> GenericArray<Vope<B::OutputSize, u8>, M>
    where
        B::OutputSize: VoleArray<u8>,
    {
        GenericArray::<Vope<B::OutputSize, u8>, M>::generate(|i| {
            let s = &self.per_byte[N::party_index(target)][(i * M::to_usize())..][..M::to_usize()];
            create_vole_from_material::<B, _>(s)
        })
    }

    pub fn to_vole_material_expanded<const M: usize, X: AsRef<[u8]>, F: FnMut(&[u8]) -> X>(
        &self,
        target: usize,
        mut f: F,
    ) -> [Vope<B::OutputSize, u8>; M]
    where
        B::OutputSize: VoleArray<u8>,
    {
        core::array::from_fn(|i| {
            let s = &self.per_byte[N::party_index(target)][(i * M)..][..M];
            create_vole_from_material_expanded::<B, X, _, _>(s, &mut f)
        })
    }

    pub fn to_vole_material_typenum_expanded<
        M: ArrayLength<Vope<B::OutputSize, u8>>,
        X: AsRef<[u8]>,
        F: FnMut(&[u8]) -> X,
    >(
        &self,
        target: usize,
        mut f: F,
    ) -> GenericArray<Vope<B::OutputSize, u8>, M>
    where
        B::OutputSize: VoleArray<u8>,
    {
        GenericArray::<Vope<B::OutputSize, u8>, M>::generate(|i| {
            let s = &self.per_byte[N::party_index(target)][(i * M::to_usize())..][..M::to_usize()];
            create_vole_from_material_expanded::<B, X, _, _>(s, &mut f)
        })
    }

    pub fn split_bit_typenum<M: ArrayLength<BSplit<B, D>>>(
        &self,
        target: usize,
    ) -> GenericArray<BSplit<B, D>, M>
    where
        B::OutputSize: VoleArray<u8>,
        D: Digest<
            OutputSize: Logarithm2<Output: ArrayLength<[GenericArray<u8, B::OutputSize>; 2]>>,
        >,
    {
        GenericArray::<BSplit<B, D>, M>::generate(|i| {
            let s = &self.per_byte[N::party_index(target)][(i * M::to_usize())..][..M::to_usize()];
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
