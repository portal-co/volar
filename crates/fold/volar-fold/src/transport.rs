// @reliability: experimental
// @ai: assisted
//! A concrete [`ResilientVoleTransport`] that realizes the VCB-IVC continuation
//! bridge — the adapter the **generated** hybrid weaver actually drives.
//!
//! Generated code is strategy-agnostic: it only calls the
//! [`ResilientVoleTransport`] methods (`prover_bridge` / `verifier_bridge`,
//! `send_opening` / `recv_opening`).  [`FoldingTransport`] implements those by
//! delegating to the [`FoldingBridge`] strategy ([`crate::bridge_adapter`]) — so a
//! reconnect runs `native_verify` on the folded gap + the boundary
//! [`BoundaryLink`] and yields a real [`GapVerdict`], **independent of the gap
//! length**.  Streaming is delegated to an inner transport (or left trivial when
//! only the bridge is exercised).
//!
//! ## Honest scope
//! - The boundary `match` mask the woven prover opens (`send_opening`) is carried
//!   through and surfaced; the *sound* boundary check is the [`BoundaryLink`]
//!   (`KeccakDigestLink` — folding-leg + openings).  The fully **in-circuit**
//!   VOLE-leg match check (the woven `keccak_check_verify` over the boundary
//!   `Q`-wires) needs a verifier-CFG checkpoint that carries the boundary
//!   `Q`-state — see `docs/boundary-link-embedding.md`.
//! - The folding context (the `GapProof` the prover computed during the cleartext
//!   gap, and the verifier's keys/Δ) is supplied to the transport by the harness;
//!   the [`ResumeToken`] from generated code supplies the resumed-boundary VOLE
//!   wires.

use alloc::rc::Rc;
use alloc::vec::Vec;
use core::cell::RefCell;

use cipher::consts::U1;
use hybrid_array::{Array, ArraySize};

use volar_net::{
    GapVerdict, ResilientVoleTransport, ResumeToken, VoleStorageEntry, VoleTransport,
};
use volar_spec::vole::{Delta, Q, Vope};

use crate::bridge_adapter::{BridgeMessage, FoldingBridge};
use crate::ivc::GapProof;
use crate::link::BoundaryLink;
use crate::scalar::Scalar;

/// One side of a gap boundary in the **folding** world: the Pedersen-committed
/// state bits and their blinder (the VOLE wires arrive via the [`ResumeToken`]).
pub struct GapBoundary {
    pub state: Vec<Scalar>,
    pub blind: Scalar,
}

/// The in-memory rendezvous between a prover and verifier [`FoldingTransport`]
/// (a stand-in for the network channel `prover_bridge`/`verifier_bridge` cross).
pub struct BridgeChannel<N: ArraySize, T, P> {
    msg: Option<BridgeMessage<P>>,
    /// The boundary digest-match mask the woven prover opens (`send_opening`).
    opening: Option<Array<T, N>>,
}

impl<N: ArraySize, T, P> Default for BridgeChannel<N, T, P> {
    fn default() -> Self {
        BridgeChannel { msg: None, opening: None }
    }
}

/// Which end of the bridge this transport is.
enum Role<N: ArraySize, T> {
    Prover {
        /// The folded gap proof, computed during the cleartext gap (consumed once).
        gap: Option<GapProof>,
        /// The anchor (`S_k`) boundary's VOLE wires + folding opening.
        in_vole: Vec<Vope<N, T, U1>>,
        in_b: GapBoundary,
        /// The resumed (`S_{k+m}`) boundary's folding opening (VOLE wires from the
        /// [`ResumeToken`]).
        out_b: GapBoundary,
    },
    Verifier {
        keys_in: Vec<Q<N, T>>,
        keys_out: Vec<Q<N, T>>,
        delta: Delta<N, T>,
    },
}

/// A [`ResilientVoleTransport`] whose resumption bridge is the VCB-IVC
/// [`FoldingBridge`].
pub struct FoldingTransport<N: ArraySize, T, L: BoundaryLink<N, T>> {
    bridge: Rc<FoldingBridge<L>>,
    chan: Rc<RefCell<BridgeChannel<N, T, L::Proof>>>,
    role: Role<N, T>,
}

impl<N: ArraySize, T, L: BoundaryLink<N, T>> FoldingTransport<N, T, L> {
    /// The prover end: holds the folded `gap` proof + both boundary openings.
    pub fn prover(
        bridge: Rc<FoldingBridge<L>>,
        chan: Rc<RefCell<BridgeChannel<N, T, L::Proof>>>,
        gap: GapProof,
        in_vole: Vec<Vope<N, T, U1>>,
        in_b: GapBoundary,
        out_b: GapBoundary,
    ) -> Self {
        FoldingTransport { bridge, chan, role: Role::Prover { gap: Some(gap), in_vole, in_b, out_b } }
    }

    /// The verifier end: holds the boundary keys + global secret Δ.
    pub fn verifier(
        bridge: Rc<FoldingBridge<L>>,
        chan: Rc<RefCell<BridgeChannel<N, T, L::Proof>>>,
        keys_in: Vec<Q<N, T>>,
        keys_out: Vec<Q<N, T>>,
        delta: Delta<N, T>,
    ) -> Self {
        FoldingTransport { bridge, chan, role: Role::Verifier { keys_in, keys_out, delta } }
    }
}

// ── VoleTransport: streaming is trivial here (the bridge is the point) ────────
impl<N: ArraySize, T: Clone + Default, L: BoundaryLink<N, T>> VoleTransport<N, T>
    for FoldingTransport<N, T, L>
{
    type Error = core::convert::Infallible;

    fn send_hats(&mut self, _hats: &[Array<T, N>]) -> Result<(), Self::Error> {
        Ok(())
    }
    fn recv_hats(&mut self, _and_count: usize) -> Result<Vec<Array<T, N>>, Self::Error> {
        Ok(Vec::new())
    }
    fn send_verdict(&mut self, _ok: bool) -> Result<(), Self::Error> {
        Ok(())
    }
    fn recv_verdict(&mut self) -> Result<bool, Self::Error> {
        Ok(true)
    }
    fn send_iteration(&mut self, _hats: &[Array<T, N>], _sentinel: bool) -> Result<(), Self::Error> {
        Ok(())
    }
    fn recv_iteration(&mut self, _and_count: usize) -> Result<(Vec<Array<T, N>>, bool), Self::Error> {
        Ok((Vec::new(), true))
    }
    fn send_storage_entries(&mut self, _e: &[VoleStorageEntry<N, T>]) -> Result<(), Self::Error> {
        Ok(())
    }
    fn recv_storage_entries(&mut self, _c: usize) -> Result<Vec<VoleStorageEntry<N, T>>, Self::Error> {
        Ok(Vec::new())
    }

    /// The woven prover opens the boundary digest-match mask here; stash it.
    fn send_opening(&mut self, opening: &Array<T, N>) -> Result<(), Self::Error> {
        self.chan.borrow_mut().opening = Some(opening.clone());
        Ok(())
    }
    /// The verifier side reads the stashed boundary mask (default zero if none).
    fn recv_opening(&mut self) -> Result<Array<T, N>, Self::Error> {
        Ok(self
            .chan
            .borrow_mut()
            .opening
            .take()
            .unwrap_or_else(|| Array::<T, N>::from_fn(|_| T::default())))
    }
}

// ── ResilientVoleTransport: the resumption bridge IS the folding bridge ───────
impl<N: ArraySize, T: Clone + Default, L: BoundaryLink<N, T>> ResilientVoleTransport<N, T>
    for FoldingTransport<N, T, L>
{
    /// On reconnect the prover assembles the folded-gap reconnect message (boundary
    /// commitments + opened folded witness + boundary links) and posts it.  The
    /// resumed-boundary VOLE wires arrive in `token.resume_state`.
    fn prover_bridge(&mut self, token: &ResumeToken<N, T>, _gap_len: u32) -> Result<(), Self::Error> {
        if let Role::Prover { gap, in_vole, in_b, out_b } = &mut self.role {
            let gp = gap.take().expect("prover_bridge: gap proof already consumed");
            let msg = self.bridge.prover_bridge(
                gp,
                in_vole,
                &in_b.state,
                &in_b.blind,
                &token.resume_state,
                &out_b.state,
                &out_b.blind,
            );
            self.chan.borrow_mut().msg = Some(msg);
        }
        Ok(())
    }

    /// The verifier natively verifies the folded gap + both boundary links and
    /// returns the [`GapVerdict`] — `Proven` iff every check passes, **independent
    /// of the gap length**.
    fn verifier_bridge(&mut self, gap_len: u32) -> Result<GapVerdict, Self::Error> {
        if let Role::Verifier { keys_in, keys_out, delta } = &self.role {
            let msg = self.chan.borrow_mut().msg.take();
            if let Some(msg) = msg {
                return Ok(self.bridge.verifier_bridge(&msg, keys_in, keys_out, delta));
            }
        }
        Ok(GapVerdict::Unproven { start: 0, end: gap_len })
    }
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use crate::ivc::{prove_gap, Step};
    use crate::link::KeccakDigestLink;
    use crate::pedersen::PedersenParams;
    use crate::r1cs::R1CS;
    use alloc::vec;
    use cipher::consts::U2;
    use hybrid_array::Array;

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
    fn boundary(bits: &[bool]) -> Vec<Scalar> {
        bits.iter().map(|&x| if x { Scalar::ONE } else { Scalar::ZERO }).collect()
    }

    /// The generated `prover_bridge` → `verifier_bridge` round-trip, through the
    /// `ResilientVoleTransport` trait, yields `Proven` for an honest gap.
    #[test]
    fn folding_transport_proves_honest_gap_via_trait() {
        let r1cs = mul_gate();
        let params = PedersenParams::setup(8, 5);
        let bridge = Rc::new(FoldingBridge::new(
            r1cs.clone(),
            params.clone(),
            KeccakDigestLink::new(params.clone()),
        ));
        let chan = Rc::new(RefCell::new(BridgeChannel::default()));

        let steps: Vec<Step> = (1..=4u64).map(|i| step(i, i + 1, i * (i + 1), i)).collect();
        let s_in = boundary(&[true, false, true, true]);
        let s_out = boundary(&[false, false, true, false]);
        let (r_in, r_out) = (Scalar::from_u64(2), Scalar::from_u64(3));
        let gap = prove_gap(&r1cs, &params, &steps, &s_in, &r_in, &s_out, &r_out);

        let mut prover = FoldingTransport::prover(
            bridge.clone(),
            chan.clone(),
            gap,
            vec![vope(1)],
            GapBoundary { state: s_in.clone(), blind: r_in },
            GapBoundary { state: s_out.clone(), blind: r_out },
        );
        let mut verifier = FoldingTransport::verifier(
            bridge.clone(),
            chan.clone(),
            vec![q(7)],
            vec![q(7)],
            Delta { delta: Array::<u64, U2>::from_fn(|_| 9) },
        );

        // The resumed boundary's VOLE wires ride in the token (generated-code API).
        let token = ResumeToken { resume_state: vec![vope(0)], mem_acc: vec![] };
        prover.prover_bridge(&token, 4).unwrap();
        let verdict = verifier.verifier_bridge(4).unwrap();
        assert!(matches!(verdict, GapVerdict::Proven), "honest gap should bridge to Proven");
    }

    /// A boundary opening that does not match its commitment is rejected.
    #[test]
    fn folding_transport_rejects_boundary_mismatch_via_trait() {
        let r1cs = mul_gate();
        let params = PedersenParams::setup(8, 5);
        let bridge = Rc::new(FoldingBridge::new(
            r1cs.clone(),
            params.clone(),
            KeccakDigestLink::new(params.clone()),
        ));
        let chan = Rc::new(RefCell::new(BridgeChannel::default()));

        let steps: Vec<Step> = (1..=4u64).map(|i| step(i, i + 1, i * (i + 1), i)).collect();
        let s_in = boundary(&[true, false, true, true]);
        let s_out = boundary(&[false, false, true, false]);
        let gap = prove_gap(
            &r1cs, &params, &steps, &s_in, &Scalar::from_u64(2), &s_out, &Scalar::from_u64(3),
        );

        // Prover lies about the OUT boundary blinder ⇒ Pedersen opening fails.
        let mut prover = FoldingTransport::prover(
            bridge.clone(),
            chan.clone(),
            gap,
            vec![vope(1)],
            GapBoundary { state: s_in.clone(), blind: Scalar::from_u64(2) },
            GapBoundary { state: s_out.clone(), blind: Scalar::from_u64(31337) },
        );
        let mut verifier = FoldingTransport::verifier(
            bridge.clone(),
            chan.clone(),
            vec![q(7)],
            vec![q(7)],
            Delta { delta: Array::<u64, U2>::from_fn(|_| 9) },
        );

        let token = ResumeToken { resume_state: vec![vope(0)], mem_acc: vec![] };
        prover.prover_bridge(&token, 4).unwrap();
        let verdict = verifier.verifier_bridge(4).unwrap();
        assert!(matches!(verdict, GapVerdict::Unproven { .. }), "boundary mismatch must not be Proven");
    }
}
