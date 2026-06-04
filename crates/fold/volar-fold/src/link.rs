// @reliability: experimental
// @ai: assisted
//! The **boundary embedding** — generic over how the binary-field VOLE
//! commitment of the boundary state is linked to its prime-field folding
//! commitment.  **This is the one genuinely open cryptographic component**, so it
//! is abstracted behind the [`BoundaryLink`] trait rather than fixed.
//!
//! ## The problem
//! The boundary state `S_k` (and `S_{k+m}`) is a bit-string that lives in **two**
//! commitment worlds:
//! - **VOLE** (`GF(2^k)`): each bit is authenticated by a MAC/key pair
//!   (`Vope` on the prover, `Q` on the verifier, secret `Δ`).
//! - **Folding** (`F_ℓ`, the Ed25519 scalar field): the bits are packed into a
//!   Pedersen [`commitment`](volar_spec::curve::EdPoint).
//!
//! These fields differ, so there is no *free* linear check tying them.  A
//! [`BoundaryLink`] is a (typically interactive — online is cheap) argument that
//! the two commitments hide the **same bit-string**.  Candidate constructions are
//! documented in `docs/vcb-ivc-folding.md` §"The embedding"; the shipped
//! [`DummyLink`] is a **placeholder that always accepts** so the bridge wires and
//! tests end-to-end — it is **NOT sound** and must be replaced.

use hybrid_array::ArraySize;
use cipher::consts::U1;

use volar_spec::curve::EdPoint;
use volar_spec::vole::{Delta, Q, Vope};

use crate::scalar::Scalar;

/// Prove/verify that a Pedersen commitment over `F_ℓ` and a set of VOLE-committed
/// bits authenticate the **same boundary bit-string**.
///
/// Generic over the VOLE lane count `N` and field `T`.  The associated `Proof`
/// is whatever the concrete embedding sends.
pub trait BoundaryLink<N: ArraySize, T> {
    type Proof;

    /// **Prover** side: given the VOLE-authenticated boundary wires `vole_bits`
    /// and the same bits packed as the folding `state` (with Pedersen
    /// `commitment` = `Commit(state; blind)`), produce a link proof.
    fn prove(
        &self,
        vole_bits: &[Vope<N, T, U1>],
        commitment: &EdPoint,
        state: &[Scalar],
        blind: &Scalar,
    ) -> Self::Proof;

    /// **Verifier** side: check the link against the VOLE keys `vole_keys`
    /// (with secret `delta`) and the folding `commitment`.
    fn verify(
        &self,
        vole_keys: &[Q<N, T>],
        delta: &Delta<N, T>,
        commitment: &EdPoint,
        proof: &Self::Proof,
    ) -> bool;
}

/// A **placeholder** [`BoundaryLink`] that performs no real linking and always
/// accepts.  Lets the folding bridge be exercised end-to-end while the sound
/// embedding remains an isolated task.  **NOT SOUND — do not ship behind a
/// `GapVerdict::Proven`.**
#[derive(Clone, Copy, Debug, Default)]
pub struct DummyLink;

impl<N: ArraySize, T> BoundaryLink<N, T> for DummyLink {
    type Proof = ();

    fn prove(&self, _: &[Vope<N, T, U1>], _: &EdPoint, _: &[Scalar], _: &Scalar) {}

    fn verify(&self, _: &[Q<N, T>], _: &Delta<N, T>, _: &EdPoint, _: &()) -> bool {
        // Placeholder: a real embedding checks bit-string equality across fields.
        true
    }
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use cipher::consts::U2;
    use hybrid_array::Array;

    fn vope(u0: u64) -> Vope<U2, u64, U1> {
        Vope {
            u: Array::<Array<u64, U2>, U1>::from_fn(|_| Array::<u64, U2>::from_fn(|_| u0)),
            v: Array::<u64, U2>::from_fn(|_| 0),
        }
    }
    fn q(a: u64) -> Q<U2, u64> {
        Q { q: Array::<u64, U2>::from_fn(|_| a) }
    }

    #[test]
    fn dummy_link_roundtrips() {
        let link = DummyLink;
        let bits = [vope(1), vope(0)];
        let keys = [q(1), q(0)];
        let delta = Delta { delta: Array::<u64, U2>::from_fn(|_| 9) };
        let commitment = EdPoint::base();
        let state = [Scalar::ONE, Scalar::ZERO];
        let proof = BoundaryLink::<U2, u64>::prove(&link, &bits, &commitment, &state, &Scalar::from_u64(7));
        assert!(BoundaryLink::<U2, u64>::verify(&link, &keys, &delta, &commitment, &proof));
    }
}
