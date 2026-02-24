use cipher::{ArrayLength, generic_array::GenericArray};

pub struct AllParties<N: ArrayLength<T>, T> {
    pub other_parties: OtherParties<N, T>,
    pub self_party: T,
}
pub struct OtherParties<N: ArrayLength<T>, T> {
    pub other_parties: GenericArray<T, N>,
}
