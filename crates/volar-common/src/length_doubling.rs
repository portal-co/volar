use core::marker::PhantomData;

use super::*;
pub trait LengthDoubler {
    type OutputSize: ArrayLength<u8>;
    fn double(a: GenericArray<u8, Self::OutputSize>) -> [GenericArray<u8, Self::OutputSize>; 2];
}
pub trait PuncturableLengthDoubler: LengthDoubler{
    
}
pub struct ViaDigestPuncturableRandomizer<D: Digest> {
    pub digest: PhantomData<D>,
}
impl<D: Digest> LengthDoubler for ViaDigestPuncturableRandomizer<D> {
    type OutputSize = D::OutputSize;
    fn double(a: GenericArray<u8, D::OutputSize>) -> [GenericArray<u8, D::OutputSize>; 2] {
        let v = D::digest(&a);
        [v.clone(), v.zip(a, |x, y| x ^ y)]
    }
}
impl<D: Digest> PuncturableLengthDoubler for ViaDigestPuncturableRandomizer<D> {}