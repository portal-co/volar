// @reliability: experimental
// @ai: assisted
//! The **VCB-IVC bridge strategy**: turn a folded gap proof + boundary links into
//! a [`GapVerdict`].
//!
//! The generated hybrid weaver calls only the
//! [`ResilientVoleTransport`](volar_net::ResilientVoleTransport) bridge methods
//! (`prover_bridge` / `verifier_bridge`), so the continuation strategy is
//! swappable without regenerating code.  This module provides the **logic** of
//! the folding strategy as [`FoldingBridge`]; a concrete transport then delegates
//! its streaming methods to an inner transport and its bridge methods here.
//!
//! Protocol on reconnect:
//! 1. Prover folds the `m` gap steps → one relaxed instance (`crate::ivc::prove_gap`),
//!    commits the boundaries `C_in` / `C_out`, and sends them with the opened
//!    folded witness + a [`BoundaryLink`] proof for each boundary.
//! 2. Verifier checks the folded instance natively (`crate::verify::native_verify`)
//!    and both boundary links; on success the gap is [`GapVerdict::Proven`] —
//!    **independent of `m`**, consuming no streaming VOLE correlations.

use hybrid_array::ArraySize;
use cipher::consts::U1;

use volar_net::GapVerdict;
use volar_spec::curve::EdPoint;
use volar_spec::vole::{Delta, Q, Vope};

use crate::ivc::GapProof;
use crate::link::BoundaryLink;
use crate::pedersen::PedersenParams;
use crate::r1cs::{R1CS, RelaxedInstance, RelaxedWitness};
use crate::verify::native_verify;

/// The reconnect message the prover sends the verifier.
pub struct BridgeMessage<P> {
    pub c_in: EdPoint,
    pub c_out: EdPoint,
    pub final_u: RelaxedInstance,
    /// Opened folded witness for the native (no-SNARK) check.
    pub final_w: RelaxedWitness,
    pub link_in: P,
    pub link_out: P,
    pub steps: u32,
}

/// The folding continuation-bridge strategy, parameterised by the (swappable)
/// boundary embedding `L`.
pub struct FoldingBridge<L> {
    pub r1cs: R1CS,
    pub params: PedersenParams,
    pub link: L,
}

impl<L> FoldingBridge<L> {
    pub fn new(r1cs: R1CS, params: PedersenParams, link: L) -> Self {
        FoldingBridge { r1cs, params, link }
    }

    /// **Prover** bridge: assemble the reconnect message from a folded gap proof
    /// and the VOLE-authenticated boundary wires.
    #[allow(clippy::too_many_arguments)]
    pub fn prover_bridge<N: ArraySize, T>(
        &self,
        gap: GapProof,
        vole_bits_in: &[Vope<N, T, U1>],
        state_in: &[crate::scalar::Scalar],
        blind_in: &crate::scalar::Scalar,
        vole_bits_out: &[Vope<N, T, U1>],
        state_out: &[crate::scalar::Scalar],
        blind_out: &crate::scalar::Scalar,
    ) -> BridgeMessage<L::Proof>
    where
        L: BoundaryLink<N, T>,
    {
        let link_in = self.link.prove(vole_bits_in, &gap.c_in, state_in, blind_in);
        let link_out = self.link.prove(vole_bits_out, &gap.c_out, state_out, blind_out);
        BridgeMessage {
            c_in: gap.c_in,
            c_out: gap.c_out,
            final_u: gap.final_u,
            final_w: gap.final_w,
            link_in,
            link_out,
            steps: gap.steps as u32,
        }
    }

    /// **Verifier** bridge: native-verify the folded instance and both boundary
    /// links; return [`GapVerdict::Proven`] iff all checks pass.
    pub fn verifier_bridge<N: ArraySize, T>(
        &self,
        msg: &BridgeMessage<L::Proof>,
        keys_in: &[Q<N, T>],
        keys_out: &[Q<N, T>],
        delta: &Delta<N, T>,
    ) -> GapVerdict
    where
        L: BoundaryLink<N, T>,
    {
        let ok_fold = native_verify(&self.r1cs, &self.params, &msg.final_u, &msg.final_w);
        let ok_in = self.link.verify(keys_in, delta, &msg.c_in, &msg.link_in);
        let ok_out = self.link.verify(keys_out, delta, &msg.c_out, &msg.link_out);
        if ok_fold && ok_in && ok_out {
            GapVerdict::Proven
        } else {
            GapVerdict::Unproven { start: 0, end: msg.steps }
        }
    }
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use crate::ivc::{prove_gap, Step};
    use crate::link::{DummyLink, KeccakDigestLink};
    use crate::scalar::Scalar;
    use cipher::consts::U2;
    use hybrid_array::Array;
    use alloc::vec;
    use alloc::vec::Vec;

    fn mul_gate() -> R1CS {
        R1CS {
            num_cons: 1,
            num_vars: 4,
            a: vec![(0, 0, Scalar::ONE)],
            b: vec![(0, 1, Scalar::ONE)],
            c: vec![(0, 2, Scalar::ONE)],
        }
    }
    fn step(a: u64, b: u64, c: u64, s: u64) -> Step {
        Step {
            w: vec![Scalar::from_u64(a), Scalar::from_u64(b), Scalar::from_u64(c)],
            r_w: Scalar::from_u64(s * 7 + 1),
            r: Scalar::from_u64(s * 13 + 3),
            r_t: Scalar::from_u64(s * 17 + 5),
        }
    }
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
    fn folding_bridge_proves_honest_gap() {
        let r1cs = mul_gate();
        let params = PedersenParams::setup(4, 5);
        let bridge = FoldingBridge::new(r1cs.clone(), params.clone(), DummyLink);

        // m = 6 honest gap steps fold into one instance.
        let steps: Vec<Step> = (1..=6u64).map(|i| step(i, i + 1, i * (i + 1), i)).collect();
        let s_in = vec![Scalar::from_u64(1)];
        let s_out = vec![Scalar::from_u64(42)];
        let gap = prove_gap(&r1cs, &params, &steps, &s_in, &Scalar::from_u64(2), &s_out, &Scalar::from_u64(3));

        let bits_in = [vope(1)];
        let bits_out = [vope(0)];
        let msg = bridge.prover_bridge(gap, &bits_in, &s_in, &Scalar::from_u64(2), &bits_out, &s_out, &Scalar::from_u64(3));

        let keys = [q(7)];
        let delta = Delta { delta: Array::<u64, U2>::from_fn(|_| 9) };
        let verdict = bridge.verifier_bridge(&msg, &keys, &keys, &delta);
        assert!(matches!(verdict, GapVerdict::Proven), "honest gap should be Proven");
    }

    #[test]
    fn folding_bridge_rejects_tampered_fold() {
        let r1cs = mul_gate();
        let params = PedersenParams::setup(4, 5);
        let bridge = FoldingBridge::new(r1cs.clone(), params.clone(), DummyLink);
        let steps: Vec<Step> = (1..=4u64).map(|i| step(i, i + 1, i * (i + 1), i)).collect();
        let s = vec![Scalar::from_u64(1)];
        let gap = prove_gap(&r1cs, &params, &steps, &s, &Scalar::from_u64(2), &s, &Scalar::from_u64(3));
        let bits = [vope(1)];
        let mut msg = bridge.prover_bridge(gap, &bits, &s, &Scalar::from_u64(2), &bits, &s, &Scalar::from_u64(3));
        // Tamper the opened witness ⇒ native_verify fails ⇒ not Proven.
        msg.final_w.w[0] = msg.final_w.w[0].add(&Scalar::ONE);
        let keys = [q(7)];
        let delta = Delta { delta: Array::<u64, U2>::from_fn(|_| 9) };
        let verdict = bridge.verifier_bridge(&msg, &keys, &keys, &delta);
        assert!(matches!(verdict, GapVerdict::Unproven { .. }), "tampered fold must not be Proven");
    }

    fn boundary(bits: &[bool]) -> Vec<Scalar> {
        bits.iter().map(|&b| if b { Scalar::ONE } else { Scalar::ZERO }).collect()
    }

    #[test]
    fn folding_bridge_with_keccak_link_proves_honest_gap() {
        let r1cs = mul_gate();
        let params = PedersenParams::setup(8, 5);
        let bridge = FoldingBridge::new(r1cs.clone(), params.clone(), KeccakDigestLink::new(params.clone()));

        // Honest folded gap.
        let steps: Vec<Step> = (1..=4u64).map(|i| step(i, i + 1, i * (i + 1), i)).collect();
        // Bit-string boundaries (the dual-preimage link hashes these).
        let s_in = boundary(&[true, false, true, true]);
        let s_out = boundary(&[false, false, true, false]);
        let (r_in, r_out) = (Scalar::from_u64(2), Scalar::from_u64(3));
        let gap = prove_gap(&r1cs, &params, &steps, &s_in, &r_in, &s_out, &r_out);

        let bits_in = [vope(1)];
        let bits_out = [vope(0)];
        let msg = bridge.prover_bridge(gap, &bits_in, &s_in, &r_in, &bits_out, &s_out, &r_out);

        let keys = [q(7)];
        let delta = Delta { delta: Array::<u64, U2>::from_fn(|_| 9) };
        let verdict = bridge.verifier_bridge(&msg, &keys, &keys, &delta);
        assert!(matches!(verdict, GapVerdict::Proven), "honest gap with Keccak links should be Proven");
    }

    #[test]
    fn folding_bridge_with_keccak_link_rejects_boundary_opening_mismatch() {
        let r1cs = mul_gate();
        let params = PedersenParams::setup(8, 5);
        let bridge = FoldingBridge::new(r1cs.clone(), params.clone(), KeccakDigestLink::new(params.clone()));

        let steps: Vec<Step> = (1..=4u64).map(|i| step(i, i + 1, i * (i + 1), i)).collect();
        let s_in = boundary(&[true, false, true, true]);
        let s_out = boundary(&[false, false, true, false]);
        let (r_in, r_out) = (Scalar::from_u64(2), Scalar::from_u64(3));
        let gap = prove_gap(&r1cs, &params, &steps, &s_in, &r_in, &s_out, &r_out);

        let bits_in = [vope(1)];
        let bits_out = [vope(0)];
        // Prover lies about the OUT boundary's blinder: c_out was committed with
        // r_out, but the link is handed a different opening ⇒ Pedersen open fails.
        let msg = bridge.prover_bridge(
            gap, &bits_in, &s_in, &r_in, &bits_out, &s_out, &Scalar::from_u64(31337),
        );

        let keys = [q(7)];
        let delta = Delta { delta: Array::<u64, U2>::from_fn(|_| 9) };
        let verdict = bridge.verifier_bridge(&msg, &keys, &keys, &delta);
        assert!(matches!(verdict, GapVerdict::Unproven { .. }), "boundary opening mismatch must not be Proven");
    }
}
