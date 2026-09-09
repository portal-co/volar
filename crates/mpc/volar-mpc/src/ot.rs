// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//!
//! Oblivious transfer for evaluator-input label delivery.
//!
//! Two layers:
//!
//! - [`LoopbackOt`]: a deterministic in-process [`crate::OtChannel`] double
//!   (sender queues both labels, receiver pops the chosen one). No
//!   cryptographic privacy; used by the single-threaded [`crate::evaluate`]
//!   reference driver and structural tests.
//! - [`CoSender`] / [`CoReceiver`]: the *real* Chou–Orlandi 1-of-2 OT
//!   ([`volar_spec::ot`]) over Ed25519 as explicit step state machines, so a
//!   harness can interleave both roles on one thread ([`crate::run_local`]) or
//!   drive them across two processes over a framed transport. Messages are
//!   byte strings; the session layer moves them.

use alloc::collections::VecDeque;
use alloc::vec::Vec;

use digest::Digest;
use hybrid_array::Array;
use sha2::Sha256;
use volar_spec::SpecRng;
use volar_spec::curve::{Ed25519, EdPoint, Fe25519, fe_from_bytes_le, fe_mul};
use volar_spec::ot::base::{
    BaseOtReceiver, BaseOtSender, OtReceiverMsg, ot_recv, ot_recv_choice, ot_recv_finish,
    ot_send_finish, ot_send_setup,
};
use volar_spec::vole::VoleArray;

use crate::OtChannel;

// ============================================================================
// Deterministic RNG for the in-process harness (NOT a CSPRNG)
// ============================================================================

/// A splitmix64 [`SpecRng`], so the in-process OT pair can supply scheme
/// randomness without real entropy. Used by the lockstep harness and tests;
/// production wiring supplies a real RNG.
pub struct SeedRng(pub u64);

impl SeedRng {
    pub fn new(seed: u64) -> Self {
        Self(seed)
    }
}

impl SpecRng for SeedRng {
    fn next_u32(&mut self) -> u32 {
        self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
        (z ^ (z >> 31)) as u32
    }
}

// ============================================================================
// Serialization (Ed25519 points are sent as 64-byte affine x||y)
// ============================================================================

fn encode_point(p: &EdPoint) -> Vec<u8> {
    let (x, y) = p.to_affine();
    let mut out = Vec::with_capacity(64);
    out.extend_from_slice(&x.to_bytes());
    out.extend_from_slice(&y.to_bytes());
    out
}

fn decode_point(b: &[u8]) -> Option<EdPoint> {
    if b.len() != 64 {
        return None;
    }
    let mut xb = [0u8; 32];
    let mut yb = [0u8; 32];
    xb.copy_from_slice(&b[..32]);
    yb.copy_from_slice(&b[32..]);
    let x = fe_from_bytes_le(&xb);
    let y = fe_from_bytes_le(&yb);
    Some(EdPoint {
        x,
        y,
        z: Fe25519::ONE,
        t: fe_mul(&x, &y),
    })
}

/// Expand an OT key to `n` bytes via SHA-256 counter-mode KDF.
fn kdf_expand(key: &[u8], n: usize) -> Vec<u8> {
    let mut out = Vec::with_capacity(n);
    let mut ctr: u32 = 0;
    while out.len() < n {
        let mut h = Sha256::new();
        h.update(key);
        h.update(ctr.to_le_bytes());
        out.extend_from_slice(&h.finalize());
        ctr += 1;
    }
    out.truncate(n);
    out
}

// ============================================================================
// Chou–Orlandi OT as explicit step state machines
// ============================================================================
//
// Per 1-of-2 OT of an N-byte label pair:
//   sender.setup()    -> msg S   (64 bytes)
//   receiver.setup(S, c) -> msg R (64 bytes)
//   sender.finish(R, labels) -> msg (e0||e1) (2N bytes)
//   receiver.finish(e0||e1) -> N-byte chosen label

/// Malformed peer message.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct OtError;

/// Sender role across one 1-of-2 OT.
pub struct CoSender<N: VoleArray<u8>> {
    state: Option<BaseOtSender<Ed25519, Sha256>>,
    _n: core::marker::PhantomData<N>,
}

impl<N: VoleArray<u8>> CoSender<N> {
    /// Step 1: sample the sender secret and emit `S`.
    pub fn setup<R: SpecRng>(rng: &mut R) -> (Self, Vec<u8>) {
        let (sender, s) = ot_send_setup::<Ed25519, Sha256, _>(rng);
        (
            CoSender {
                state: Some(sender),
                _n: core::marker::PhantomData,
            },
            encode_point(&s),
        )
    }

    /// Step 3: consume `R`, mask the two labels, emit `(e0||e1)`.
    pub fn finish(self, r_bytes: &[u8], labels: [&Array<u8, N>; 2]) -> Result<Vec<u8>, OtError> {
        let sender = self.state.ok_or(OtError)?;
        let r = decode_point(r_bytes).ok_or(OtError)?;
        let (k0, k1) = ot_send_finish::<Ed25519, Sha256>(&sender, &OtReceiverMsg { r });
        let e0k = kdf_expand(&k0, N::USIZE);
        let e1k = kdf_expand(&k1, N::USIZE);
        let mut frame = Vec::with_capacity(2 * N::USIZE);
        for i in 0..N::USIZE {
            frame.push(labels[0].as_slice()[i] ^ e0k[i]);
        }
        for i in 0..N::USIZE {
            frame.push(labels[1].as_slice()[i] ^ e1k[i]);
        }
        Ok(frame)
    }
}

/// Receiver role across one 1-of-2 OT.
pub struct CoReceiver<N: VoleArray<u8>> {
    state: Option<BaseOtReceiver<Ed25519, Sha256>>,
    _n: core::marker::PhantomData<N>,
}

impl<N: VoleArray<u8>> CoReceiver<N> {
    /// Step 2: consume `S`, commit to choice bit `c`, emit `R`.
    pub fn setup<R: SpecRng>(rng: &mut R, s_bytes: &[u8], c: bool) -> Result<(Self, Vec<u8>), OtError> {
        let s = decode_point(s_bytes).ok_or(OtError)?;
        let (receiver, msg) = ot_recv::<Ed25519, Sha256, _>(rng, s, c);
        Ok((
            CoReceiver {
                state: Some(receiver),
                _n: core::marker::PhantomData,
            },
            encode_point(&msg.r),
        ))
    }

    /// Step 4: consume `(e0||e1)`, unmask and return the chosen label.
    pub fn finish(self, frame: &[u8]) -> Result<Array<u8, N>, OtError> {
        let receiver = self.state.ok_or(OtError)?;
        if frame.len() != 2 * N::USIZE {
            return Err(OtError);
        }
        let kc = ot_recv_finish::<Ed25519, Sha256>(&receiver);
        let keystream = kdf_expand(&kc, N::USIZE);
        let c = ot_recv_choice::<Ed25519, Sha256>(&receiver);
        let ec = if c { &frame[N::USIZE..] } else { &frame[..N::USIZE] };
        Ok(Array::<u8, N>::from_fn(|i| ec[i] ^ keystream[i]))
    }
}

/// Run one full OT in-process (test convenience): returns the receiver's
/// recovered label for choice `bit`.
pub fn ot_once<N: VoleArray<u8>>(labels: [&Array<u8, N>; 2], bit: bool, seed: u64) -> Array<u8, N> {
    let mut rng = SeedRng::new(seed);
    let (sender, s_msg) = CoSender::<N>::setup(&mut rng);
    let (receiver, r_msg) = CoReceiver::<N>::setup(&mut rng, &s_msg, bit).expect("receiver setup");
    let frame = sender.finish(&r_msg, labels).expect("sender finish");
    receiver.finish(&frame).expect("receiver finish")
}

// ============================================================================
// LoopbackOt — in-process OtChannel double (no privacy)
// ============================================================================

/// A deterministic in-process 1-of-2 OT for tests.
///
/// The garbler (sender) pushes label pairs; the evaluator (receiver) pops the
/// label for its choice bit. Both halves share one queue, so this is only
/// meaningful inside a single process driving both roles — exactly the
/// [`crate::evaluate`] reference driver shape.
///
/// Privacy: none. The sender "sees" both labels and the receiver could too; it
/// is a test double, not a cryptographic OT. For real privacy use
/// [`CoSender`]/[`CoReceiver`].
#[derive(Default)]
pub struct LoopbackOt<N: VoleArray<u8>> {
    queue: VecDeque<[Array<u8, N>; 2]>,
}

impl<N: VoleArray<u8>> LoopbackOt<N> {
    /// Create an empty loopback OT.
    pub fn new() -> Self {
        Self {
            queue: VecDeque::new(),
        }
    }

    /// Number of unclaimed label pairs (diagnostic for tests).
    pub fn pending(&self) -> usize {
        self.queue.len()
    }
}

impl<N: VoleArray<u8>> OtChannel<N> for LoopbackOt<N> {
    fn send(&mut self, labels: [&Array<u8, N>; 2]) {
        self.queue
            .push_back([labels[0].clone(), labels[1].clone()]);
    }

    fn receive(&mut self, bit: bool) -> Array<u8, N> {
        let pair = self
            .queue
            .pop_front()
            .expect("LoopbackOt: receive called before send");
        pair[bit as usize].clone()
    }
}

/// A recording OT wrapper: passes through to an inner channel while logging
/// every label the sender offered and every choice the receiver made.
///
/// Used by the mutual-privacy test to assert (structurally) that evaluator
/// inputs only ever traverse the channel as OT label pairs — never as
/// plaintext bits — and vice versa for garbler inputs.
pub struct RecordingOt<'a, N: VoleArray<u8>, C: OtChannel<N>> {
    inner: &'a mut C,
    /// Every label pair the sender offered (both labels, in order).
    pub offered: Vec<[Array<u8, N>; 2]>,
    /// Every choice bit the receiver made.
    pub choices: Vec<bool>,
}

impl<'a, N: VoleArray<u8>, C: OtChannel<N>> RecordingOt<'a, N, C> {
    pub fn new(inner: &'a mut C) -> Self {
        Self {
            inner,
            offered: Vec::new(),
            choices: Vec::new(),
        }
    }
}

impl<N: VoleArray<u8>, C: OtChannel<N>> OtChannel<N> for RecordingOt<'_, N, C> {
    fn send(&mut self, labels: [&Array<u8, N>; 2]) {
        self.offered.push([labels[0].clone(), labels[1].clone()]);
        self.inner.send(labels);
    }

    fn receive(&mut self, bit: bool) -> Array<u8, N> {
        self.choices.push(bit);
        self.inner.receive(bit)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use typenum::U16;

    type N = U16;

    fn label(byte: u8) -> Array<u8, N> {
        Array::<u8, N>::from_fn(|i| byte.wrapping_add(i as u8))
    }

    /// The real Chou–Orlandi OT recovers exactly the chosen label, for both
    /// choice bits, across several seeds.
    #[test]
    fn chou_orlandi_recovers_chosen_label() {
        let l0 = label(0x11);
        let l1 = label(0x77);
        for seed in [0u64, 1, 2, 42, 0xDEAD_BEEF] {
            let got0 = ot_once::<N>([&l0, &l1], false, seed);
            let got1 = ot_once::<N>([&l0, &l1], true, seed);
            assert_eq!(got0, l0, "seed {seed} c=0");
            assert_eq!(got1, l1, "seed {seed} c=1");
        }
    }

    /// The two masked labels in the sender's frame differ (the receiver can
    /// only unmask one), and a receiver with the wrong bit gets garbage.
    #[test]
    fn chou_orlandi_hides_other_label() {
        let l0 = label(0x00);
        let l1 = label(0xFF);
        let mut rng = SeedRng::new(7);
        let (sender, s_msg) = CoSender::<N>::setup(&mut rng);
        let (receiver, r_msg) = CoReceiver::<N>::setup(&mut rng, &s_msg, false).unwrap();
        let frame = sender.finish(&r_msg, [&l0, &l1]).unwrap();
        // Frame halves are not the plaintext labels.
        assert_ne!(&frame[..16], l0.as_slice());
        assert_ne!(&frame[16..], l1.as_slice());
        // Chosen label recovers correctly.
        let got = receiver.finish(&frame).unwrap();
        assert_eq!(got, l0);
    }
}
