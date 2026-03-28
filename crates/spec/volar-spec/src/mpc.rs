// @reliability: experimental
// @experimental-status: design
// @experimental-since: 79ee6d7 (experiment: mpc)
//! @ai: none
// MPC party type skeleton (AllParties, OtherParties).
// Semantics of share combination, consistency checks, and communication
// rounds are not yet defined. Stub only.
use hybrid_array::{Array, ArraySize};

pub struct AllParties<N: ArraySize, T> {
    pub other_parties: OtherParties<N, T>,
    pub self_party: T,
}
pub struct OtherParties<N: ArraySize, T> {
    pub other_parties: Array<T, N>,
}
