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
    ArrayLength, Unsigned,
    consts::U1,
    generic_array::{functional::FunctionalSequence, sequence::GenericSequence},
    typenum::{IsGreater, Logarithm2, Max, B1},
};
use digest::Digest;

use super::*;
use crate::vole::{VoleArray, vope::Vope};
use volar_common::hash_commitment::commit;

// ---------------------------------------------------------------------------
// Verifier vole methods — gated by PartyIndex
// ---------------------------------------------------------------------------

impl<
    B: LengthDoubler<OutputSize: Unsigned + Max<D::OutputSize, Output: ArrayLength<u8>>>,
    D: Digest<OutputSize: Unsigned>,
    T: ArrayLength<
            GenericArray<GenericArray<u8, <B::OutputSize as Max<D::OutputSize>>::Output>, U>,
        > + ArrayLength<u64>,
    U: ArrayLength<GenericArray<u8, <B::OutputSize as Max<D::OutputSize>>::Output>>,
    N: ArrayLength<
            GenericArray<
                GenericArray<GenericArray<u8, <B::OutputSize as Max<D::OutputSize>>::Output>, U>,
                T,
            >,
        > + PartyIndex,
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

    pub fn to_vole_material_typenum<M: ArrayLength<Vope<B::OutputSize, u8>>>(
        &self,
        party: usize,
    ) -> GenericArray<Vope<B::OutputSize, u8>, M>
    where
        B::OutputSize: VoleArray<u8>,
    {
        GenericArray::<Vope<B::OutputSize, u8>, M>::generate(|i| {
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
        M: ArrayLength<Vope<B::OutputSize, u8>>,
        X: AsRef<[u8]>,
        F: FnMut(&[u8]) -> X,
    >(
        &self,
        party: usize,
        mut f: F,
    ) -> GenericArray<Vope<B::OutputSize, u8>, M>
    where
        B::OutputSize: VoleArray<u8>,
    {
        GenericArray::<Vope<B::OutputSize, u8>, M>::generate(|i| {
            let s = &self.openings[N::party_index(party)][i];
            create_vole_from_material_expanded::<B, X, _, _>(s, &mut f)
        })
    }

    pub fn split_bit_typenum<M: ArrayLength<BSplit<B, D>>>(
        &self,
        party: usize,
    ) -> GenericArray<BSplit<B, D>, M>
    where
        B::OutputSize: VoleArray<u8>,
        D: Digest<
            OutputSize: Logarithm2<Output: ArrayLength<[GenericArray<u8, B::OutputSize>; 2]>>,
        >,
    {
        GenericArray::<BSplit<B, D>, M>::generate(|i| {
            let s = &self.openings[N::party_index(party)][i];
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

// ---------------------------------------------------------------------------
// 2-party validate — ABOOpening<B, D, T, U, U1> — always compiled
//
// Compared to the N-party version below:
//   - takes &self instead of GenericArray<&Self, NOthers>
//   - no `me` parameter (me.per_byte is only accessed for ci > 0, dead for U1)
//   - no cross-party consistency loop; one commit, one pass over T×U positions
// ---------------------------------------------------------------------------

impl<
    B: LengthDoubler<OutputSize: Unsigned + Max<D::OutputSize, Output: ArrayLength<u8>>>,
    D: Digest<OutputSize: Unsigned>,
    T: ArrayLength<
            GenericArray<GenericArray<u8, <B::OutputSize as Max<D::OutputSize>>::Output>, U>,
        > + ArrayLength<u64>,
    U: ArrayLength<GenericArray<u8, <B::OutputSize as Max<D::OutputSize>>::Output>>,
> ABOOpening<B, D, T, U, U1>
where
    U1: ArrayLength<
        GenericArray<
            GenericArray<GenericArray<u8, <B::OutputSize as Max<D::OutputSize>>::Output>, U>,
            T,
        >,
    >,
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
                    h.update(
                        &self.openings[0][i][b]
                            [..(<D::OutputSize as Unsigned>::to_usize())],
                    );
                } else {
                    h.update(&commit::<D>(
                        &&self.openings[0][i][b]
                            [..(<B::OutputSize as Unsigned>::to_usize())],
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
        > + IsGreater<U1, Output = B1>,
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
}
