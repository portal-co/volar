// @reliability: normal
//! @ai: assisted
// LengthDoubler trait: AES-based PRG that doubles output size at each step.
// Used as the PRF backbone for ABO generation and VOLE randomness.
use core::marker::PhantomData;

use super::*;
pub trait LengthDoubler {
    type OutputSize: ArraySize;
    fn double(a: Array<u8, Self::OutputSize>) -> [Array<u8, Self::OutputSize>; 2];
}
pub trait PuncturableLengthDoubler: LengthDoubler {}
pub struct ViaDigestPuncturableRandomizer<D: Digest> {
    pub digest: PhantomData<D>,
}
impl<D: Digest> LengthDoubler for ViaDigestPuncturableRandomizer<D> {
    type OutputSize = D::OutputSize;
    fn double(a: Array<u8, D::OutputSize>) -> [Array<u8, D::OutputSize>; 2] {
        let v = D::digest(&a);
        [v.clone(), Array::from_fn(|i| v[i] ^ a[i])]
    }
}
impl<D: Digest> PuncturableLengthDoubler for ViaDigestPuncturableRandomizer<D> {}
