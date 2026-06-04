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

use alloc::vec::Vec;

use hybrid_array::ArraySize;
use cipher::consts::U1;

use volar_spec::curve::EdPoint;
use volar_spec::vole::{Delta, Q, Vope};

use crate::keccak::keccak256_bits;
use crate::keccak_r1cs::keccak256_r1cs_with_digest;
use crate::pedersen::PedersenParams;
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

/// The proof a [`KeccakDigestLink`] sends: the opened boundary bits + Pedersen
/// blinder, and the digest `d = Keccak256(bits)` the two worlds are anchored to.
#[derive(Clone)]
pub struct KeccakLinkProof {
    /// The boundary bit-string (the field-agnostic shared object).
    pub bits: Vec<bool>,
    /// Pedersen blinder opening the folding `commitment`.
    pub blind: Scalar,
    /// Public anchor `d = Keccak256(bits)` (the VOLE side proves a preimage of the
    /// *same* `d`; collision-resistance then forces equal boundaries).
    pub digest: [bool; 256],
}

/// The **dual-preimage Keccak** boundary embedding — *folding leg*.
///
/// `verify` proves that the folding-committed boundary (the Pedersen
/// `commitment`) hashes, **through the in-R1CS Keccak arithmetization**
/// ([`keccak256_r1cs_with_digest`]), to the public digest `proof.digest`:
/// 1. **Pedersen opening** — `commitment == Commit(bits; blind)` binds `bits` to
///    the folding boundary (Pedersen binding; generators are hash-to-curve).
/// 2. **Keccak preimage** — the Keccak R1CS over `bits` bound to `proof.digest`
///    is satisfied, i.e. `Keccak256(bits) = digest` is enforced arithmetically
///    (so the same check is what `native_verify` covers once the block is
///    composed into the folded instance, `docs/vcb-ivc-folding.md` §1.4).
///
/// **Scope (honest).** This is the *folding* leg. The **VOLE leg** — proving the
/// VOLE-committed boundary hashes to the same `digest` — is the woven gadget
/// (`docs/boundary-link-embedding.md` §VOLE side / §1.5); until it lands the
/// `vole_bits`/`vole_keys`/`delta` are unused here. The link is also **non-ZK**
/// (it opens `bits`); the ZK upgrade replaces the openings with woven preimage
/// proofs of the same `digest`. It is strictly sounder than [`DummyLink`]: a
/// boundary whose Pedersen opening is wrong, or that does not hash to `digest`,
/// is **rejected**.
#[derive(Clone)]
pub struct KeccakDigestLink {
    /// The Pedersen parameters the boundary `commitment` was formed under.
    pub params: PedersenParams,
}

impl KeccakDigestLink {
    pub fn new(params: PedersenParams) -> Self {
        KeccakDigestLink { params }
    }

    fn bits_to_scalars(bits: &[bool]) -> Vec<Scalar> {
        bits.iter().map(|&b| if b { Scalar::ONE } else { Scalar::ZERO }).collect()
    }
}

impl<N: ArraySize, T> BoundaryLink<N, T> for KeccakDigestLink {
    type Proof = KeccakLinkProof;

    fn prove(
        &self,
        _vole_bits: &[Vope<N, T, U1>],
        _commitment: &EdPoint,
        state: &[Scalar],
        blind: &Scalar,
    ) -> KeccakLinkProof {
        // The boundary state is a bit-string; open it and anchor to its digest.
        let bits: Vec<bool> = state.iter().map(|s| *s == Scalar::ONE).collect();
        let digest = keccak256_bits(&bits);
        KeccakLinkProof { bits, blind: *blind, digest }
    }

    fn verify(
        &self,
        _vole_keys: &[Q<N, T>],
        _delta: &Delta<N, T>,
        commitment: &EdPoint,
        proof: &KeccakLinkProof,
    ) -> bool {
        // (1) The opened bits must open the folding commitment (Pedersen binding).
        let scalars = Self::bits_to_scalars(&proof.bits);
        if self.params.commit(&scalars, &proof.blind) != *commitment {
            return false;
        }
        // (2) Those same bits must hash (in-circuit) to the anchored digest.
        let inst = keccak256_r1cs_with_digest(&proof.bits, &proof.digest);
        let mut z = inst.witness.clone();
        z.push(Scalar::ONE); // z = [W ‖ u], u = 1
        inst.r1cs.is_satisfied(&z)
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

    fn boundary(bits: &[bool]) -> std::vec::Vec<Scalar> {
        bits.iter().map(|&b| if b { Scalar::ONE } else { Scalar::ZERO }).collect()
    }

    #[test]
    fn keccak_link_accepts_honest_boundary() {
        let params = PedersenParams::setup(8, 11);
        let link = KeccakDigestLink::new(params.clone());
        let bits = [true, false, true, true, false, false, true, false];
        let state = boundary(&bits);
        let blind = Scalar::from_u64(1234);
        let commitment = params.commit(&state, &blind);

        let vbits = [vope(1)];
        let proof = BoundaryLink::<U2, u64>::prove(&link, &vbits, &commitment, &state, &blind);
        // The proof anchors to the true digest of the boundary.
        assert_eq!(proof.digest, keccak256_bits(&bits));

        let keys = [q(7)];
        let delta = Delta { delta: Array::<u64, U2>::from_fn(|_| 9) };
        assert!(
            BoundaryLink::<U2, u64>::verify(&link, &keys, &delta, &commitment, &proof),
            "honest boundary must link"
        );
    }

    #[test]
    fn keccak_link_rejects_wrong_commitment_opening() {
        let params = PedersenParams::setup(8, 11);
        let link = KeccakDigestLink::new(params.clone());
        let bits = [true, false, true, false];
        let state = boundary(&bits);
        let blind = Scalar::from_u64(7);
        let commitment = params.commit(&state, &blind);

        let vbits = [vope(1)];
        let mut proof = BoundaryLink::<U2, u64>::prove(&link, &vbits, &commitment, &state, &blind);
        // Tamper the opened blinder ⇒ the Pedersen opening no longer matches.
        proof.blind = proof.blind.add(&Scalar::ONE);

        let keys = [q(7)];
        let delta = Delta { delta: Array::<u64, U2>::from_fn(|_| 9) };
        assert!(
            !BoundaryLink::<U2, u64>::verify(&link, &keys, &delta, &commitment, &proof),
            "a wrong Pedersen opening must be rejected"
        );
    }

    #[test]
    fn keccak_link_rejects_mismatched_digest() {
        let params = PedersenParams::setup(8, 11);
        let link = KeccakDigestLink::new(params.clone());
        let bits = [false, true, true, false, true];
        let state = boundary(&bits);
        let blind = Scalar::from_u64(99);
        let commitment = params.commit(&state, &blind);

        let vbits = [vope(1)];
        let mut proof = BoundaryLink::<U2, u64>::prove(&link, &vbits, &commitment, &state, &blind);
        // Anchor to a digest that is NOT Keccak(bits): the Keccak R1CS is now
        // unsatisfiable by the honest preimage ⇒ rejected.
        proof.digest[3] = !proof.digest[3];

        let keys = [q(7)];
        let delta = Delta { delta: Array::<u64, U2>::from_fn(|_| 9) };
        assert!(
            !BoundaryLink::<U2, u64>::verify(&link, &keys, &delta, &commitment, &proof),
            "a digest that is not the boundary's hash must be rejected"
        );
    }
}
