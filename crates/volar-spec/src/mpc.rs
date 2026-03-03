// @reliability: experimental
// @experimental-status: design
// @experimental-since: 79ee6d7 (experiment: mpc)
//! @ai: none
// MPC party type skeleton (AllParties, OtherParties).
// Semantics of share combination, consistency checks, and communication
// rounds are not yet defined. Stub only.
use cipher::{ArrayLength, generic_array::GenericArray};

pub struct AllParties<N: ArrayLength<T>, T> {
    pub other_parties: OtherParties<N, T>,
    pub self_party: T,
}
pub struct OtherParties<N: ArrayLength<T>, T> {
    pub other_parties: GenericArray<T, N>,
}
