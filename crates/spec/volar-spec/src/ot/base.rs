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

use super::group::{Group, ScalarOps};
use crate::SpecRng;

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
        BaseOtSender {
            y,
            s: s.clone(),
            t,
            _d: PhantomData,
        },
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
        BaseOtReceiver {
            x,
            s,
            c,
            _d: PhantomData,
        },
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
pub fn ot_recv_finish<G: Group, D: Digest>(state: &BaseOtReceiver<G, D>) -> digest::Output<D> {
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
// Malicious-secure base OT: receiver consistency proof (Schnorr OR-proof)
// ============================================================================
//
// Plain Chou-Orlandi is semi-honest: a malicious receiver can set
// `R = S^a · g^x` for known `a` and derive *both* keys (it knows `S^a = T^a`
// and `S^x`, so it reconstructs `R^y` and `(R·S^{-1})^y`). The malicious-secure
// variant makes the receiver prove, in zero knowledge, that `R` is well-formed:
// it knows `x` such that `R = g^x` OR `R·S^{-1} = g^x`. This is a Schnorr
// OR-proof (Cramer–Damgård–Schoenmakers), made non-interactive with
// Fiat–Shamir. A receiver that cannot produce a valid proof is rejected before
// the sender derives keys.

/// The receiver's consistency proof: `(e_0, e_1, z_0, z_1)`.
#[derive(Clone)]
pub struct OtConsistencyProof<S> {
    pub e0: S,
    pub e1: S,
    pub z0: S,
    pub z1: S,
}

fn or_proof_challenge<G: ScalarOps, D: Digest>(
    s: &G::Element,
    r: &G::Element,
    a0: &G::Element,
    a1: &G::Element,
) -> G::Scalar {
    let g = G::generator();
    let mut h = D::new();
    h.update(b"volar-chou-orlandi-or-proof-v1");
    G::write_element::<D>(&g, &mut h);
    G::write_element::<D>(s, &mut h);
    G::write_element::<D>(r, &mut h);
    G::write_element::<D>(a0, &mut h);
    G::write_element::<D>(a1, &mut h);
    G::scalar_from_hash::<D>(h)
}

/// Receiver: generate the consistency proof for a well-formed `R` (knowing `x`
/// and the choice bit `c`, with `R = g^x` when `c = 0` and `R = S·g^x` when
/// `c = 1`).
pub fn ot_recv_prove<G: ScalarOps, D: Digest, R: SpecRng>(
    rng: &mut R,
    s: &G::Element,
    r: &G::Element,
    x: &G::Scalar,
    c: bool,
) -> OtConsistencyProof<G::Scalar> {
    let g = G::generator();
    // Statements: R_0 = R (real when c=0), R_1 = R·S^{-1} (real when c=1).
    let r1_stmt = G::add(r, &G::neg(s));
    // Real branch gets a genuine Schnorr commitment; the simulated branch is
    // computed from a random challenge/response.
    let rr = G::random_mod_order(rng);
    let e_sim = G::random_mod_order(rng);
    let z_sim = G::random_mod_order(rng);
    let a_real = G::scalar_mul(&g, &rr);
    let r_sim_stmt = if c { r } else { &r1_stmt };
    let a_sim = G::add(
        &G::scalar_mul(&g, &z_sim),
        &G::neg(&G::scalar_mul(r_sim_stmt, &e_sim)),
    );
    let (a0, a1) = if c { (a_sim, a_real) } else { (a_real, a_sim) };
    let e = or_proof_challenge::<G, D>(s, r, &a0, &a1);
    let e_real = G::scalar_sub(&e, &e_sim);
    let z_real = G::scalar_add(&rr, &G::scalar_mul_scalar(&e_real, x));
    let (e0, z0, e1, z1) = if c {
        (e_sim, z_sim, e_real, z_real)
    } else {
        (e_real, z_real, e_sim, z_sim)
    };
    OtConsistencyProof { e0, e1, z0, z1 }
}

/// Sender: verify the receiver's consistency proof. Returns `true` iff `R` is
/// well-formed (`R = g^x` or `R = S·g^x` for the receiver's secret `x`).
pub fn ot_send_verify<G: ScalarOps, D: Digest>(
    s: &G::Element,
    r: &G::Element,
    proof: &OtConsistencyProof<G::Scalar>,
) -> bool {
    let g = G::generator();
    let r1_stmt = G::add(r, &G::neg(s));
    // Recompute the commitments: A_b = g^{z_b} · R_b^{-e_b}.
    let a0 = G::add(
        &G::scalar_mul(&g, &proof.z0),
        &G::neg(&G::scalar_mul(r, &proof.e0)),
    );
    let a1 = G::add(
        &G::scalar_mul(&g, &proof.z1),
        &G::neg(&G::scalar_mul(&r1_stmt, &proof.e1)),
    );
    let e = or_proof_challenge::<G, D>(s, r, &a0, &a1);
    G::scalar_eq(&e, &G::scalar_add(&proof.e0, &proof.e1))
}

/// Malicious-secure receiver step: [`ot_recv`] plus the consistency proof.
pub fn ot_recv_malicious<G: ScalarOps, D: Digest, R: SpecRng>(
    rng: &mut R,
    s: G::Element,
    c: bool,
) -> (BaseOtReceiver<G, D>, OtReceiverMsg<G>, OtConsistencyProof<G::Scalar>) {
    let (state, msg) = ot_recv::<G, D, R>(rng, s, c);
    let proof = ot_recv_prove::<G, D, R>(rng, &state.s, &msg.r, &state.x, state.c);
    (state, msg, proof)
}

/// Malicious-secure sender finish: verify the consistency proof, then derive
/// the keys. Returns `None` (reject) if the proof is invalid.
pub fn ot_send_finish_malicious<G: ScalarOps, D: Digest>(
    state: &BaseOtSender<G, D>,
    msg: &OtReceiverMsg<G>,
    proof: &OtConsistencyProof<G::Scalar>,
) -> Option<(digest::Output<D>, digest::Output<D>)> {
    if !ot_send_verify::<G, D>(&state.s, &msg.r, proof) {
        return None;
    }
    Some(ot_send_finish::<G, D>(state, msg))
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
    for i in 0..m0.len() {
        e0[i] = m0[i] ^ k0[i];
    }
    for i in 0..m1.len() {
        e1[i] = m1[i] ^ k1[i];
    }
}

/// Receiver-side payload decrypt: `m_c = e_c ⊕ k_c`.
pub fn ot_recv_payload<D: Digest>(kc: &digest::Output<D>, ec: &[u8], mc: &mut [u8]) {
    debug_assert_eq!(ec.len(), mc.len());
    debug_assert!(ec.len() <= kc.len());
    for i in 0..ec.len() {
        mc[i] = ec[i] ^ kc[i];
    }
}

#[cfg(test)]
mod tests {
    use super::super::group::ToyGroup;
    use super::*;
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
    fn run_rot(
        c: bool,
    ) -> (
        digest::Output<Sha256>,
        digest::Output<Sha256>,
        digest::Output<Sha256>,
    ) {
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

    // --- Malicious-secure base OT (Schnorr OR-proof) ---

    #[test]
    fn malicious_base_ot_honest_receiver_passes() {
        for c in [false, true] {
            let mut rng = TestRng(0x1111_2222_3333_4444);
            let (sender, s) = ot_send_setup::<ToyGroup, Sha256, _>(&mut rng);
            let (receiver, msg, proof) =
                ot_recv_malicious::<ToyGroup, Sha256, _>(&mut rng, s, c);
            let keys = ot_send_finish_malicious::<ToyGroup, Sha256>(&sender, &msg, &proof);
            let (k0, k1) = keys.expect("honest receiver's proof verifies");
            let kc = ot_recv_finish::<ToyGroup, Sha256>(&receiver);
            assert_eq!(kc, if c { k1 } else { k0 });
        }
    }

    #[test]
    fn malicious_base_ot_rejects_malformed_r() {
        // A malicious receiver sets R = S^a · g^x for known a (a = 2), which is
        // neither g^x nor S·g^x — it would learn both keys. It cannot produce a
        // valid consistency proof for the malformed R.
        let mut rng = TestRng(0x5555_6666_7777_8888);
        let (sender, s) = ot_send_setup::<ToyGroup, Sha256, _>(&mut rng);
        let a = ToyGroup::random_scalar(&mut rng);
        let x = ToyGroup::random_scalar(&mut rng);
        // R = S^a · g^x (malformed).
        let sa = ToyGroup::scalar_mul(&s, &a);
        let gx = ToyGroup::scalar_mul(&ToyGroup::generator(), &x);
        let r_bad = ToyGroup::add(&sa, &gx);
        let msg = OtReceiverMsg { r: r_bad };
        // The receiver claims R = g^x (c=0) — but that's false.
        let bad_proof = ot_recv_prove::<ToyGroup, Sha256, _>(&mut rng, &s, &msg.r, &x, false);
        assert!(
            !ot_send_verify::<ToyGroup, Sha256>(&s, &msg.r, &bad_proof),
            "a malformed R cannot produce a valid consistency proof"
        );
        assert!(ot_send_finish_malicious::<ToyGroup, Sha256>(&sender, &msg, &bad_proof).is_none());
    }

    #[test]
    fn malicious_base_ot_ed25519() {
        use crate::curve::Ed25519;
        for c in [false, true] {
            let mut rng = TestRng(0x9999_AAAA_BBBB_CCCC);
            let (sender, s) = ot_send_setup::<Ed25519, Sha256, _>(&mut rng);
            let (receiver, msg, proof) =
                ot_recv_malicious::<Ed25519, Sha256, _>(&mut rng, s, c);
            let keys = ot_send_finish_malicious::<Ed25519, Sha256>(&sender, &msg, &proof);
            let (k0, k1) = keys.expect("honest Ed25519 receiver's proof verifies");
            let kc = ot_recv_finish::<Ed25519, Sha256>(&receiver);
            assert_eq!(kc, if c { k1 } else { k0 }, "choice {c}");
        }
    }

    #[test]
    fn ed25519_scalar_arithmetic_mod_l() {
        use crate::curve::Ed25519;
        use crate::ot::group::ScalarOps;
        // a=3, b=5: add, sub, mul mod ℓ.
        let mut a = [0u8; 32];
        a[0] = 3;
        let mut b = [0u8; 32];
        b[0] = 5;
        let sum = Ed25519::scalar_add(&a, &b);
        assert_eq!(sum[0], 8);
        let diff = Ed25519::scalar_sub(&b, &a);
        assert_eq!(diff[0], 2);
        let prod = Ed25519::scalar_mul_scalar(&a, &b);
        assert_eq!(prod[0], 15);
        // Consistency with the group law: g^{a+b} = g^a · g^b.
        let g = Ed25519::generator();
        let lhs = Ed25519::scalar_mul(&g, &sum);
        let rhs = Ed25519::add(
            &Ed25519::scalar_mul(&g, &a),
            &Ed25519::scalar_mul(&g, &b),
        );
        assert_eq!(lhs.to_affine(), rhs.to_affine(), "g^(a+b) == g^a·g^b");
    }
}
