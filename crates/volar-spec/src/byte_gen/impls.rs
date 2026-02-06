use super::*;
pub struct ViaDigestPuncturableRandomizer<D: Digest> {
    pub digest: D,
}
impl<D: Digest> LengthDoubler for ViaDigestPuncturableRandomizer<D> {
    type OutputSize = D::OutputSize;
    fn double(a: GenericArray<u8, D::OutputSize>) -> [GenericArray<u8, D::OutputSize>; 2] {
        let v = D::digest(&a);
        [v.clone(), v.zip(a, |x, y| x ^ y)]
    }
}
