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
    consts::U1,
    typenum::{Logarithm2, Max, Unsigned},
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
    K: ArraySize,
    N: ArraySize,
> ABO<B, D, K, N>
{
    /// Generate an [`ABOOpening`] for each party; bad positions are replaced
    /// with hash commitments, revealing the rest directly.
    pub fn open<
        T: ArraySize,
        U: ArraySize,
        R: AsRef<[u8]>,
        M: ArraySize,
    >(
        &self,
        bad: Array<u64, T>,
        rand: &R,
    ) -> ABOOpening<B, D, T, U, N>
    where
        B::OutputSize: Max<D::OutputSize, Output = M>,
        T: Mul<U, Output = K>,
    {
        ABOOpening {
            bad: bad.clone(),
            openings: Array::<Array<Array<Array<u8, M>, U>, T>, N>::from_fn(move |ni| {
                let bad = bad.clone();
                Array::<Array<Array<u8, M>, U>, T>::from_fn(move |i| {
                    let bad = bad.clone();
                    Array::<Array<u8, M>, U>::from_fn(move |j| {
                        let i2 = i | ((j as usize) << T::USIZE.ilog2());
                        if bad.contains(&(i2 as u64)) {
                            let h = commit::<D>(&self.per_byte[ni][i2], rand);
                            Array::<u8, M>::from_fn(|j| h.as_ref().get(j).cloned().unwrap_or_default())
                        } else {
                            Array::<u8, M>::from_fn(|j| {
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
    K: ArraySize,
    N: ArraySize + PartyIndex,
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

    pub fn to_vole_material_typenum<M: ArraySize>(
        &self,
        target: usize,
    ) -> Array<Vope<B::OutputSize, u8>, M>
    where
        B::OutputSize: VoleArray<u8>,
    {
        Array::<Vope<B::OutputSize, u8>, M>::from_fn(|i| {
            let s = &self.per_byte[N::party_index(target)][(i * M::USIZE)..][..M::USIZE];
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
        M: ArraySize,
        X: AsRef<[u8]>,
        F: FnMut(&[u8]) -> X,
    >(
        &self,
        target: usize,
        mut f: F,
    ) -> Array<Vope<B::OutputSize, u8>, M>
    where
        B::OutputSize: VoleArray<u8>,
    {
        Array::<Vope<B::OutputSize, u8>, M>::from_fn(|i| {
            let s = &self.per_byte[N::party_index(target)][(i * M::USIZE)..][..M::USIZE];
            create_vole_from_material_expanded::<B, X, _, _>(s, &mut f)
        })
    }

    pub fn split_bit_typenum<M: ArraySize>(
        &self,
        target: usize,
    ) -> Array<BSplit<B, D>, M>
    where
        B::OutputSize: VoleArray<u8>,
        D: Digest<
            OutputSize: Logarithm2<Output: ArraySize>,
        >,
    {
        Array::<BSplit<B, D>, M>::from_fn(|i| {
            let s = &self.per_byte[N::party_index(target)][(i * M::USIZE)..][..M::USIZE];
            BSplit {
                split: Array::<
                    [Array<u8, B::OutputSize>; 2],
                    <D::OutputSize as Logarithm2>::Output,
                >::from_fn(|j| {
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
                            .fold(Array::<u8, B::OutputSize>::default(), |a, b| {
                                Array::<u8, B::OutputSize>::from_fn(|i| a[i].bitxor(b[i]))
                            })
                    })
                }),
            }
        })
    }
}
