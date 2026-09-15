// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//! 1-out-of-2 payload OT as a role-separated trait.
//!
//! Both Chou-Orlandi ([`super::base`]) and LWE ([`super::lwe`]) implement
//! this surface so IKNP / SoftSpoken can swap the base OT without changing
//! the extension body.
//!
//! Round structure (sender-setup first, then receiver, then payload):
//!
//! ```text
//! 1. Base-OT sender:  sender_setup  → SetupMsg
//! 2. Base-OT receiver: recv_start(SetupMsg, c) → RecvMsg
//! 3. Base-OT sender:  sender_payload(RecvMsg, m0, m1) → PayloadMsg
//! 4. Base-OT receiver: recv_finish(PayloadMsg) → m_c
//! ```

use core::marker::PhantomData;

use digest::Digest;

use super::base::{
    BaseOtReceiver, BaseOtSender, OtReceiverMsg, ot_recv, ot_recv_finish, ot_recv_payload,
    ot_send_finish, ot_send_payload, ot_send_setup,
};
use super::group::Group;
use crate::SpecRng;

/// 1-out-of-2 OT transferring an `L`-byte payload.
pub trait BaseOt<const L: usize> {
    /// Sender state after [`Self::sender_setup`].
    type SenderState;
    /// Receiver state after [`Self::recv_start`].
    type ReceiverState;
    /// First message, sender → receiver.
    type SetupMsg: Clone;
    /// Second message, receiver → sender.
    type RecvMsg;
    /// Third message, sender → receiver (encrypted payloads).
    type PayloadMsg;

    /// Sender samples setup material and emits the first message.
    fn sender_setup<R: SpecRng>(rng: &mut R) -> (Self::SenderState, Self::SetupMsg);

    /// Receiver consumes the setup message and choice bit.
    fn recv_start<R: SpecRng>(
        rng: &mut R,
        setup: &Self::SetupMsg,
        c: bool,
    ) -> (Self::ReceiverState, Self::RecvMsg);

    /// Sender encrypts `(m0, m1)` under the two OT keys.
    fn sender_payload<R: SpecRng>(
        rng: &mut R,
        state: &Self::SenderState,
        recv_msg: &Self::RecvMsg,
        m0: &[u8; L],
        m1: &[u8; L],
    ) -> Self::PayloadMsg;

    /// Receiver decrypts the chosen payload.
    fn recv_finish(state: &Self::ReceiverState, payload: &Self::PayloadMsg) -> [u8; L];
}

/// Chou-Orlandi Simplest OT as a [`BaseOt`] (group `G`, hash `D`).
pub struct ChouOrlandi<G, D> {
    _g: PhantomData<G>,
    _d: PhantomData<D>,
}

impl<G: Group, D: Digest, const L: usize> BaseOt<L> for ChouOrlandi<G, D> {
    type SenderState = BaseOtSender<G, D>;
    type ReceiverState = ChouOrlandiRecv<G, D>;
    type SetupMsg = G::Element;
    type RecvMsg = OtReceiverMsg<G>;
    type PayloadMsg = ([u8; L], [u8; L]);

    fn sender_setup<R: SpecRng>(rng: &mut R) -> (Self::SenderState, Self::SetupMsg) {
        ot_send_setup::<G, D, R>(rng)
    }

    fn recv_start<R: SpecRng>(
        rng: &mut R,
        setup: &Self::SetupMsg,
        c: bool,
    ) -> (Self::ReceiverState, Self::RecvMsg) {
        let (inner, msg) = ot_recv::<G, D, R>(rng, setup.clone(), c);
        (ChouOrlandiRecv { inner }, msg)
    }

    fn sender_payload<R: SpecRng>(
        _rng: &mut R,
        state: &Self::SenderState,
        recv_msg: &Self::RecvMsg,
        m0: &[u8; L],
        m1: &[u8; L],
    ) -> Self::PayloadMsg {
        let (k0, k1) = ot_send_finish::<G, D>(state, recv_msg);
        let mut e0 = [0u8; L];
        let mut e1 = [0u8; L];
        ot_send_payload::<D>(&k0, &k1, m0, m1, &mut e0, &mut e1);
        (e0, e1)
    }

    fn recv_finish(state: &Self::ReceiverState, payload: &Self::PayloadMsg) -> [u8; L] {
        let kc = ot_recv_finish::<G, D>(&state.inner);
        let chosen = if state.inner_choice() {
            &payload.1
        } else {
            &payload.0
        };
        let mut mc = [0u8; L];
        ot_recv_payload::<D>(&kc, chosen, &mut mc);
        mc
    }
}

/// Wrapper so [`BaseOt::recv_finish`] can recover the choice bit.
pub struct ChouOrlandiRecv<G: Group, D: Digest> {
    inner: BaseOtReceiver<G, D>,
}

impl<G: Group, D: Digest> ChouOrlandiRecv<G, D> {
    fn inner_choice(&self) -> bool {
        super::base::ot_recv_choice(&self.inner)
    }
}
