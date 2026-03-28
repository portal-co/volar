use core::ops::{BitXor, Mul};

use cipher::{
    consts::U1,
    typenum::{Logarithm2, Max, Unsigned},
};

use super::*;
use crate::vole::{VoleArray, vope::Vope};
use volar_common::hash_commitment::commit;
pub(crate) use volar_common::length_doubling::LengthDoubler;

/// Resolves a party index into [`ABO::per_byte`] or [`ABOOpening::openings`].
///
/// Without the `multi_party` feature only [`U1`] satisfies this bound,
/// so the shared prover/verifier methods in [`impls`] are restricted to the
/// 2-party case.  With `multi_party` all `N: `[`Unsigned`] satisfy it,
/// enabling the full N-party protocol.
pub trait PartyIndex {
    fn party_index(requested: usize) -> usize;
}

/// 2-party: always index 0; the argument is ignored.
#[cfg(not(feature = "multi_party"))]
impl PartyIndex for U1 {
    #[inline]
    fn party_index(_: usize) -> usize { 0 }
}

/// N-party: return the requested index directly.
#[cfg(feature = "multi_party")]
impl<N: Unsigned> PartyIndex for N {
    #[inline]
    fn party_index(requested: usize) -> usize { requested }
}

pub fn gen_abo<
    B: LengthDoubler,
    D: Digest,
    K: ArraySize,
    N: ArraySize,
>(
    a: Array<u8, B::OutputSize>,
    rand: &impl AsRef<[u8]>,
) -> ABO<B, D, K, N>
where
    B: Sized,
{
    let mut h = D::new();
    let per_byte = Array::<Array<Array<u8, B::OutputSize>, K>, N>::from_fn(|_ni| {
        let mut per_byte = Array::<Array<u8, B::OutputSize>, K>::default();
        for i in 0..K::USIZE {
            let core = (0..K::USIZE.ilog2()).fold(a.clone(), |mut acc, b| {
                if (i >> b) & 1 != 0 {
                    let doubled = B::double(acc);
                    acc = doubled[1].clone();
                } else {
                    let doubled = B::double(acc);
                    acc = doubled[0].clone();
                }
                acc
            });

            h.update(&commit::<D>(&core, rand));
            per_byte[i] = core;
        }
        return per_byte;
    });
    ABO::<B, D, K, N> {
        commit: h.finalize(),
        per_byte,
    }
}
pub fn create_vole_from_material<B: LengthDoubler<OutputSize: VoleArray<u8>>, X: AsRef<[u8]>>(
    s: &[X],
) -> Vope<B::OutputSize, u8> {
    let u: Array<u8, B::OutputSize> =
        s.iter()
            .fold(Array::<u8, B::OutputSize>::default(), |a, b| {
                Array::<u8, B::OutputSize>::from_fn(|i| a[i].bitxor(b.as_ref()[i]))
            });
    let v: Array<u8, B::OutputSize> =
        s.iter()
            .enumerate()
            .fold(Array::<u8, B::OutputSize>::default(), |a, (i, b)| {
                Array::<u8, B::OutputSize>::from_fn(|j| a[j].bitxor(b.as_ref()[j]).bitxor(i as u8))
            });
    Vope {
        u: Array::<Array<u8, B::OutputSize>, U1>::from_fn(|_| u.clone()),
        v,
    }
}
pub fn create_vole_from_material_expanded<
    B: LengthDoubler<OutputSize: VoleArray<u8>>,
    X: AsRef<[u8]>,
    Y: AsRef<[u8]>,
    F: FnMut(&[u8]) -> X,
>(
    s: &[Y],
    mut f: F,
) -> Vope<B::OutputSize, u8> {
    let u: Array<u8, B::OutputSize> = s
        .iter()
        .map(|b| f(&b.as_ref()[..(<B::OutputSize as Unsigned>::USIZE)]))
        .fold(Array::<u8, B::OutputSize>::default(), |a, b| {
            Array::<u8, B::OutputSize>::from_fn(|i| a[i].bitxor(b.as_ref()[i]))
        });
    let v: Array<u8, B::OutputSize> = s
        .iter()
        .map(|b| f(&b.as_ref()[..(<B::OutputSize as Unsigned>::USIZE)]))
        .enumerate()
        .fold(Array::<u8, B::OutputSize>::default(), |a, (i, b)| {
            Array::<u8, B::OutputSize>::from_fn(|j| a[j].bitxor(b.as_ref()[j]).bitxor(i as u8))
        });
    Vope {
        u: Array::<Array<u8, B::OutputSize>, U1>::from_fn(|_| u.clone()),
        v,
    }
}
pub struct ABO<
    B: LengthDoubler,
    D: Digest,
    K: ArraySize,
    N: ArraySize,
> {
    pub commit: Array<u8, D::OutputSize>,
    pub per_byte: Array<Array<Array<u8, B::OutputSize>, K>, N>,
}
pub struct ABOOpening<
    B: LengthDoubler<OutputSize: Max<D::OutputSize, Output: ArraySize>>,
    D: Digest,
    T: ArraySize,
    U: ArraySize,
    N: ArraySize,
> {
    pub bad: Array<u64, T>,
    pub openings: Array<Array<
        Array<Array<u8, <B::OutputSize as Max<D::OutputSize>>::Output>, U>,
        T,
    >, N>,
}

pub mod prover;
pub mod verifier;

pub struct BSplit<
    B: LengthDoubler,
    D: Digest<OutputSize: Logarithm2<Output: ArraySize>>,
> {
    pub split:
        Array<[Array<u8, B::OutputSize>; 2], <D::OutputSize as Logarithm2>::Output>,
}
