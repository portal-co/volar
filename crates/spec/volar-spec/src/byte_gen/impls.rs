// @reliability: experimental
// @experimental-status: review-pending
// @experimental-since: 263eab1 (fix name)
//! @ai: assisted
// 2-party (NOthers = U1) core implementations of the ABO prover/verifier protocol.
// Active when the `multi_party` feature is disabled, allowing the U1 case to be
// studied and managed independently of the general N-party constructions in
// prover.rs / verifier.rs.
use core::ops::{BitXor, Mul};

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
// Prover side + opening generation — ABO<B, D, K, U1>
// ---------------------------------------------------------------------------

/// Prover-side VOLE-material extraction and opening generation for the
/// 2-party case (`N = U1`).  Methods match the signatures in `prover.rs` and
/// `verifier.rs` but drop the `target`/`party: usize` parameter (always 0).
#[cfg(not(feature = "multi_party"))]
impl<
    B: LengthDoubler,
    D: Digest,
    K: ArrayLength<GenericArray<u8, B::OutputSize>>,
> ABO<B, D, K, U1>
where
    U1: ArrayLength<GenericArray<GenericArray<u8, B::OutputSize>, K>>,
{
    /// Generate an `ABOOpening` for the single verifier.  Bad positions are
    /// replaced with hash commitments; all others are revealed directly.
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
    ) -> ABOOpening<B, D, T, U, U1>
    where
        B::OutputSize: Max<D::OutputSize, Output = M>,
        T: Mul<U, Output = K>,
        U1: ArrayLength<
            GenericArray<
                GenericArray<GenericArray<u8, M>, U>,
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

    pub fn to_vole_material<const N: usize>(&self) -> [Vope<B::OutputSize, u8>; N]
    where
        B::OutputSize: VoleArray<u8>,
    {
        core::array::from_fn(|i| {
            let s = &self.per_byte[0][(i * N)..][..N];
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
            let s = &self.per_byte[0][(i * N::to_usize())..][..N::to_usize()];
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
            let s = &self.per_byte[0][(i * N)..][..N];
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
            let s = &self.per_byte[0][(i * N::to_usize())..][..N::to_usize()];
            create_vole_from_material_expanded::<B, X, _, _>(s, &mut f)
        })
    }

    pub fn split_bit_typenum<N: ArrayLength<BSplit<B, D>>>(&self) -> GenericArray<BSplit<B, D>, N>
    where
        B::OutputSize: VoleArray<u8>,
        D: Digest<
            OutputSize: Logarithm2<Output: ArrayLength<[GenericArray<u8, B::OutputSize>; 2]>>,
        >,
    {
        GenericArray::<BSplit<B, D>, N>::generate(|i| {
            let s = &self.per_byte[0][(i * N::to_usize())..][..N::to_usize()];
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
// Verifier side — ABOOpening<B, D, T, U, U1>
// ---------------------------------------------------------------------------

/// Verifier-side validation and VOLE-material extraction for the 2-party case
/// (`NOthers = U1`).
///
/// Compared to the generic `validate` in `verifier.rs`:
/// - The `me` parameter is dropped (it is only accessed for `ci > 0`, which
///   never occurs when `NOthers = U1`).
/// - The cross-party consistency loop collapses to a single pass over the one
///   opening; no party-skipping logic is needed.
#[cfg(not(feature = "multi_party"))]
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
    /// Validate this opening against a single commitment.
    ///
    /// Bad positions must match a hash commitment; all other positions are
    /// hashed directly and accumulated.
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

    pub fn to_vole_material<const N: usize>(&self) -> [Vope<B::OutputSize, u8>; N]
    where
        B::OutputSize: VoleArray<u8>,
    {
        core::array::from_fn(|i| {
            let s = &self.openings[0][i];
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
            let s = &self.openings[0][i];
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
            let s = &self.openings[0][i];
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
            let s = &self.openings[0][i];
            create_vole_from_material_expanded::<B, X, _, _>(s, &mut f)
        })
    }

    pub fn split_bit_typenum<N: ArrayLength<BSplit<B, D>>>(
        &self,
    ) -> GenericArray<BSplit<B, D>, N>
    where
        B::OutputSize: VoleArray<u8>,
        D: Digest<
            OutputSize: Logarithm2<Output: ArrayLength<[GenericArray<u8, B::OutputSize>; 2]>>,
        >,
    {
        GenericArray::<BSplit<B, D>, N>::generate(|i| {
            let s = &self.openings[0][i];
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
