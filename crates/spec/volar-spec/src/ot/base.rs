// @reliability: experimental
//! @ai: assisted
//! Chou-Orlandi "Simplest OT" — 1-out-of-2 base OT.
//!
//! # Protocol (random OT variant)
//!
//! Group `G` of (large) prime order with generator `g`. Hash `H : G → {0,1}^L`
//! modelled as a random oracle.
//!
//! Sender holds nothing; receiver holds choice bit `c ∈ {0,1}`.
//!
//! ```text
//! 1.  Sender:    y ←$ Z_q
//!                S := g^y
//!                T := S^y                   (cached locally)
//!                send S → receiver
//! 2.  Receiver:  x ←$ Z_q
//!                R := if c == 0 { g^x } else { S · g^x }
//!                k_c := H(S^x)
//!                send R → sender
//! 3.  Sender:    k_0 := H(R^y)
//!                k_1 := H((R · S^{-1})^y) = H(R^y · T^{-1})
//! ```
//!
//! Sender output: `(k_0, k_1)`. Receiver output: `k_c`.
//!
//! Correctness: when `c = 0`, `R = g^x` so `R^y = g^{xy} = S^x`. When `c = 1`,
//! `R = S · g^x` so `R^y = S^y · g^{xy} = T · S^x`, and `(R · S^{-1})^y =
//! g^{xy} · T · T^{-1} = g^{xy} = S^x`. Either way the receiver's `S^x`
//! matches the sender's `R^y` for the chosen branch.
//!
//! # Standard 1-of-2 OT
//!
//! Layered on top: sender encrypts `(m_0, m_1)` as `(m_0 ⊕ k_0, m_1 ⊕ k_1)`,
//! receiver recovers `m_c = e_c ⊕ k_c`. See [`ot_send_payload`] /
//! [`ot_recv_payload`].

use core::marker::PhantomData;

use digest::Digest;

use crate::SpecRng;
use super::group::Group;

/// Sender state across a single base OT instance.
pub struct BaseOtSender<G: Group, D: Digest> {
    y: G::Scalar,
    s: G::Element,
    t: G::Element, // S^y
    _d: PhantomData<D>,
}

/// Receiver state across a single base OT instance.
pub struct BaseOtReceiver<G: Group, D: Digest> {
    x: G::Scalar,
    s: G::Element,
    c: bool,
    _d: PhantomData<D>,
}

/// Receiver-side message (`R`).
pub struct OtReceiverMsg<G: Group> {
    pub r: G::Element,
}

/// Step 1: sender samples `y` and emits `S := g^y`.
pub fn ot_send_setup<G: Group, D: Digest, R: SpecRng>(
    rng: &mut R,
) -> (BaseOtSender<G, D>, G::Element) {
    let y = G::random_scalar(rng);
    let g = G::generator();
    let s = G::scalar_mul(&g, &y);
    let t = G::scalar_mul(&s, &y);
    (
        BaseOtSender { y, s: s.clone(), t, _d: PhantomData },
        s,
    )
}

/// Step 2: receiver consumes `S` and choice bit `c`, computes
/// `R = g^x` (c=0) or `S · g^x` (c=1), and prepares its key `k_c = H(S^x)`.
///
/// Returns the receiver's state (carrying `k_c` derivation material) and the
/// `R` message to send to the sender.
pub fn ot_recv<G: Group, D: Digest, R: SpecRng>(
    rng: &mut R,
    s: G::Element,
    c: bool,
) -> (BaseOtReceiver<G, D>, OtReceiverMsg<G>) {
    let x = G::random_scalar(rng);
    let g = G::generator();
    let gx = G::scalar_mul(&g, &x);
    let r = if c { G::add(&s, &gx) } else { gx };
    (
        BaseOtReceiver { x, s, c, _d: PhantomData },
        OtReceiverMsg { r },
    )
}

/// Step 3a: sender derives the two keys `(k_0, k_1)` from the received `R`.
///
/// Each key is `D::OutputSize` bytes (e.g. 32 for SHA-256).
pub fn ot_send_finish<G: Group, D: Digest>(
    state: &BaseOtSender<G, D>,
    msg: &OtReceiverMsg<G>,
) -> (digest::Output<D>, digest::Output<D>) {
    // R^y
    let ry = G::scalar_mul(&msg.r, &state.y);
    // (R · S^{-1})^y = R^y · T^{-1} when written out, but it's clearer to
    // recompute as (R + (-S))^y.
    let s_inv = G::neg(&state.s);
    let r_minus_s = G::add(&msg.r, &s_inv);
    let r_minus_s_y = G::scalar_mul(&r_minus_s, &state.y);

    let mut h0 = D::new();
    G::write_element::<D>(&ry, &mut h0);
    let mut h1 = D::new();
    G::write_element::<D>(&r_minus_s_y, &mut h1);
    (h0.finalize(), h1.finalize())
}

/// Step 3b: receiver derives `k_c = H(S^x)`.
pub fn ot_recv_finish<G: Group, D: Digest>(
    state: &BaseOtReceiver<G, D>,
) -> digest::Output<D> {
    let sx = G::scalar_mul(&state.s, &state.x);
    let mut h = D::new();
    G::write_element::<D>(&sx, &mut h);
    h.finalize()
}

/// Sender's choice bit accessor (constant for a given instance).
pub fn ot_recv_choice<G: Group, D: Digest>(state: &BaseOtReceiver<G, D>) -> bool {
    state.c
}

// ============================================================================
// Payload OT (1-of-2 OT for arbitrary message bytes)
// ============================================================================

/// Standard 1-of-2 OT: sender encrypts `(m_0, m_1)` to keys `(k_0, k_1)`
/// and sends both ciphertexts. Receiver decrypts the chosen one.
///
/// Messages must equal `D::OutputSize` bytes — for longer payloads, expand
/// the OT keys via a KDF before XORing.
pub fn ot_send_payload<D: Digest>(
    k0: &digest::Output<D>,
    k1: &digest::Output<D>,
    m0: &[u8],
    m1: &[u8],
    e0: &mut [u8],
    e1: &mut [u8],
) {
    debug_assert_eq!(m0.len(), e0.len());
    debug_assert_eq!(m1.len(), e1.len());
    debug_assert!(m0.len() <= k0.len());
    debug_assert!(m1.len() <= k1.len());
    for i in 0..m0.len() { e0[i] = m0[i] ^ k0[i]; }
    for i in 0..m1.len() { e1[i] = m1[i] ^ k1[i]; }
}

/// Receiver-side payload decrypt: `m_c = e_c ⊕ k_c`.
pub fn ot_recv_payload<D: Digest>(
    kc: &digest::Output<D>,
    ec: &[u8],
    mc: &mut [u8],
) {
    debug_assert_eq!(ec.len(), mc.len());
    debug_assert!(ec.len() <= kc.len());
    for i in 0..ec.len() { mc[i] = ec[i] ^ kc[i]; }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::group::ToyGroup;
    use sha2::Sha256;

    struct TestRng(u64);
    impl SpecRng for TestRng {
        fn next_u32(&mut self) -> u32 {
            self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
            let mut z = self.0;
            z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
            z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
            (z ^ (z >> 31)) as u32
        }
    }

    /// One full base-OT exchange — receiver's key matches sender's `k_c`.
    fn run_rot(c: bool) -> (digest::Output<Sha256>, digest::Output<Sha256>, digest::Output<Sha256>) {
        let mut rng = TestRng(0xA5A5_A5A5_A5A5_A5A5);
        let (sender, s) = ot_send_setup::<ToyGroup, Sha256, _>(&mut rng);
        let (receiver, msg) = ot_recv::<ToyGroup, Sha256, _>(&mut rng, s, c);
        let (k0, k1) = ot_send_finish::<ToyGroup, Sha256>(&sender, &msg);
        let kc = ot_recv_finish::<ToyGroup, Sha256>(&receiver);
        (k0, k1, kc)
    }

    #[test]
    fn rot_c0_matches_k0() {
        let (k0, _k1, kc) = run_rot(false);
        assert_eq!(kc, k0);
    }

    #[test]
    fn rot_c1_matches_k1() {
        let (_k0, k1, kc) = run_rot(true);
        assert_eq!(kc, k1);
    }

    #[test]
    fn rot_keys_are_distinct() {
        let (k0, k1, _) = run_rot(false);
        assert_ne!(k0, k1);
    }

    #[test]
    fn payload_round_trip_c0() {
        let mut rng = TestRng(0x1234_5678_9ABC_DEF0);
        let (sender, s) = ot_send_setup::<ToyGroup, Sha256, _>(&mut rng);
        let (receiver, msg) = ot_recv::<ToyGroup, Sha256, _>(&mut rng, s, false);
        let (k0, k1) = ot_send_finish::<ToyGroup, Sha256>(&sender, &msg);
        let kc = ot_recv_finish::<ToyGroup, Sha256>(&receiver);

        let m0 = b"hello.sender.side.message.zero!";
        let m1 = b"goodbye.sender.side.message.one";
        let mut e0 = [0u8; 31];
        let mut e1 = [0u8; 31];
        ot_send_payload::<Sha256>(&k0, &k1, m0, m1, &mut e0, &mut e1);

        let mut mc = [0u8; 31];
        ot_recv_payload::<Sha256>(&kc, &e0, &mut mc);
        assert_eq!(&mc, m0);
    }

    #[test]
    fn payload_round_trip_c1() {
        let mut rng = TestRng(0xFEDC_BA98_7654_3210);
        let (sender, s) = ot_send_setup::<ToyGroup, Sha256, _>(&mut rng);
        let (receiver, msg) = ot_recv::<ToyGroup, Sha256, _>(&mut rng, s, true);
        let (k0, k1) = ot_send_finish::<ToyGroup, Sha256>(&sender, &msg);
        let kc = ot_recv_finish::<ToyGroup, Sha256>(&receiver);

        let m0 = b"hello.sender.side.message.zero!";
        let m1 = b"goodbye.sender.side.message.one";
        let mut e0 = [0u8; 31];
        let mut e1 = [0u8; 31];
        ot_send_payload::<Sha256>(&k0, &k1, m0, m1, &mut e0, &mut e1);

        let mut mc = [0u8; 31];
        ot_recv_payload::<Sha256>(&kc, &e1, &mut mc);
        assert_eq!(&mc, m1);
    }
}
