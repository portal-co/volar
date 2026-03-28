// @reliability: experimental
// @experimental-status: review-pending
// @experimental-since: 263eab1 (fix name)
//! @ai: assisted
// Verifier-side byte generation (ABO verifier protocol).
// Last structural change: 58e8f84 (experiment: improve mpc compatibility).
// Revised after cda059c (actually unsound, oops). Not independently verified.
//
// The vole / split methods use PartyIndex (U1 always; all N: Unsigned with
// multi_party).
//
// Two validate implementations, never overlapping:
//
//   ABOOpening<.., U1>::validate — always compiled; simplified 2-party form
//     with no `me` parameter and no cross-party loop.
//
//   ABOOpening<.., NOthers>::validate — compiled only with `multi_party`,
//     restricted to NOthers: IsGreater<U1, Output = B1> so it cannot clash
//     with the U1 impl.  Both are therefore available when `multi_party` is
//     active, satisfying feature additivity.
use core::ops::BitXor;

use cipher::{
    consts::U1,
    typenum::{IsGreater, Logarithm2, Max, Unsigned, B1},
};
use digest::Digest;

use super::*;
use crate::vole::{VoleArray, vope::Vope};
use volar_common::hash_commitment::commit;

// ---------------------------------------------------------------------------
// Verifier vole methods — gated by PartyIndex
// ---------------------------------------------------------------------------

impl<
    B: LengthDoubler<OutputSize: Unsigned + Max<D::OutputSize, Output: ArraySize>>,
    D: Digest<OutputSize: Unsigned>,
    T: ArraySize,
    U: ArraySize,
    N: ArraySize + PartyIndex,
> ABOOpening<B, D, T, U, N>
{
    pub fn to_vole_material<const M: usize>(&self, party: usize) -> [Vope<B::OutputSize, u8>; M]
    where
        B::OutputSize: VoleArray<u8>,
    {
        core::array::from_fn(|i| {
            let s = &self.openings[N::party_index(party)][i];
            create_vole_from_material::<B, _>(s)
        })
    }

    pub fn to_vole_material_typenum<M: ArraySize>(
        &self,
        party: usize,
    ) -> Array<Vope<B::OutputSize, u8>, M>
    where
        B::OutputSize: VoleArray<u8>,
    {
        Array::<Vope<B::OutputSize, u8>, M>::from_fn(|i| {
            let s = &self.openings[N::party_index(party)][i];
            create_vole_from_material::<B, _>(s)
        })
    }

    pub fn to_vole_material_expanded<const M: usize, X: AsRef<[u8]>, F: FnMut(&[u8]) -> X>(
        &self,
        party: usize,
        mut f: F,
    ) -> [Vope<B::OutputSize, u8>; M]
    where
        B::OutputSize: VoleArray<u8>,
    {
        core::array::from_fn(|i| {
            let s = &self.openings[N::party_index(party)][i];
            create_vole_from_material_expanded::<B, X, _, _>(s, &mut f)
        })
    }

    pub fn to_vole_material_typenum_expanded<
        M: ArraySize,
        X: AsRef<[u8]>,
        F: FnMut(&[u8]) -> X,
    >(
        &self,
        party: usize,
        mut f: F,
    ) -> Array<Vope<B::OutputSize, u8>, M>
    where
        B::OutputSize: VoleArray<u8>,
    {
        Array::<Vope<B::OutputSize, u8>, M>::from_fn(|i| {
            let s = &self.openings[N::party_index(party)][i];
            create_vole_from_material_expanded::<B, X, _, _>(s, &mut f)
        })
    }

    pub fn split_bit_typenum<M: ArraySize>(
        &self,
        party: usize,
    ) -> Array<BSplit<B, D>, M>
    where
        B::OutputSize: VoleArray<u8>,
        D: Digest<
            OutputSize: Logarithm2<Output: ArraySize>,
        >,
    {
        Array::<BSplit<B, D>, M>::from_fn(|i| {
            let s = &self.openings[N::party_index(party)][i];
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

// ---------------------------------------------------------------------------
// 2-party validate — ABOOpening<B, D, T, U, U1> — always compiled
//
// Compared to the N-party version below:
//   - takes &self instead of Array<&Self, NOthers>
//   - no `me` parameter (me.per_byte is only accessed for ci > 0, dead for U1)
//   - no cross-party consistency loop; one commit, one pass over T×U positions
// ---------------------------------------------------------------------------

impl<
    B: LengthDoubler<OutputSize: Unsigned + Max<D::OutputSize, Output: ArraySize>>,
    D: Digest<OutputSize: Unsigned>,
    T: ArraySize,
    U: ArraySize,
> ABOOpening<B, D, T, U, U1>
{
    pub fn validate<R: AsRef<[u8]>>(
        &self,
        commit_: &Array<u8, D::OutputSize>,
        rand: &R,
    ) -> bool {
        let mut h = D::new();
        for i in 0..T::USIZE {
            for b in 0..U::USIZE {
                let i2 = i | ((b as usize) << T::USIZE.ilog2());
                if self.bad.contains(&(i2 as u64)) {
                    h.update(
                        &self.openings[0][i][b]
                            [..(<D::OutputSize as Unsigned>::USIZE)],
                    );
                } else {
                    h.update(&commit::<D>(
                        &&self.openings[0][i][b]
                            [..(<B::OutputSize as Unsigned>::USIZE)],
                        rand,
                    ));
                }
            }
        }
        h.finalize().as_slice() == commit_.as_slice()
    }
}

// ---------------------------------------------------------------------------
// N-party validate — ABOOpening<B, D, T, U, NOthers> — requires multi_party
//
// Restricted to NOthers: IsGreater<U1, Output = B1> so this impl never
// overlaps with the U1 impl above; both are usable when multi_party is active.
// ---------------------------------------------------------------------------

#[cfg(feature = "multi_party")]
impl<
    B: LengthDoubler<OutputSize: Unsigned + Max<D::OutputSize, Output: ArraySize>>,
    D: Digest<OutputSize: Unsigned>,
    T: ArraySize,
    U: ArraySize,
    NOthers: ArraySize + IsGreater<U1, Output = B1>,
> ABOOpening<B, D, T, U, NOthers>
{
    pub fn validate<'a, R: AsRef<[u8]>>(
        this: Array<&'a Self, NOthers>,
        me: &'a ABO<B, D, <T as Mul<U>>::Output, NOthers>,
        commit_: Array<&'a Array<u8, D::OutputSize>, NOthers>,
        rand: &R,
    ) -> bool
    where
        T: Mul<U>,
        <T as Mul<U>>::Output: ArraySize,
    {
        commit_.iter().enumerate().all(|(ci, commit_)| {
            let mut h = D::new();
            if ci != 0 {
                for i in 0..T::USIZE {
                    for b in 0..U::USIZE {
                        let i2 = i | ((b as usize) << T::USIZE.ilog2());
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
                for i in 0..T::USIZE {
                    for b in 0..U::USIZE {
                        let i2 = i | ((b as usize) << T::USIZE.ilog2());
                        if this.bad.contains(&(i2 as u64)) {
                            h.update(
                                &this.openings[ci][i][b]
                                    [..(<D::OutputSize as Unsigned>::USIZE)],
                            );
                        } else {
                            h.update(&commit::<D>(
                                &&this.openings[ci][i][b]
                                    [..(<B::OutputSize as Unsigned>::USIZE)],
                                rand,
                            ));
                        }
                    }
                }
            }
            h.finalize().as_slice() == commit_.as_slice()
        })
    }
}
