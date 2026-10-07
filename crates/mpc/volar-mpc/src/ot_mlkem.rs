// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//!
//! Experimental ML-KEM-1024 1-of-2 OT candidate (the `mlkem` feature).
//!
//! This is a custom real/fake-public-key PKE-OT candidate, not the Masny–Rindal
//! protocol and not a reviewed or deployment-ready OT. The receiver places a
//! generated ML-KEM encapsulation key at its choice index and a uniformly
//! sampled valid public key at the other index; unlike generating and dropping
//! a second secret key, the fake-key path creates no second decapsulation key.
//! The candidate relies on public-key pseudorandomness and message hiding for
//! encapsulation under the fake key. FIPS 203 standardizes ML-KEM, not this OT
//! composition; no post-quantum security claim is made here.
//!
//! The sender encapsulates independently under both keys and masks each label
//! with its KEM shared secret. These step state machines ([`MkReceiver`] /
//! [`MkSender`]) support in-process tests and a potential transport adapter;
//! this module does not itself define a framed network protocol.

use alloc::vec::Vec;

use core::convert::Infallible;
use digest::Digest;
use hybrid_array::{Array, typenum::U16};
use ml_kem::{
    Decapsulate, Encapsulate, Generate, KeyExport,
    ml_kem_1024::{Ciphertext, EncapsulationKey},
    {DecapsulationKey1024, SharedKey},
};
use sha2::Sha256;
use volar_spec::SpecRng;
use volar_spec::ot::base_ot::BaseOt;
use volar_spec::vole::VoleArray;

use crate::ot::OtError;

// ============================================================================
// RNG adapter: SpecRng -> rand_core 0.10 CryptoRng
// ============================================================================

/// Adapts a [`SpecRng`] to the `rand_core` 0.10 [`ml_kem`] RNG interface, so
/// the deterministic harness RNG (and any production `SpecRng`) drives ML-KEM
/// key generation and encapsulation.
///
/// **Not** cryptographically stronger than the underlying `SpecRng`: the
/// harness's `SeedRng` is deterministic and must not be used for real
/// randomness.
pub struct SpecRngAsCrypto<'a>(pub &'a mut dyn SpecRng);

impl rand_core::TryRng for SpecRngAsCrypto<'_> {
    type Error = Infallible;
    fn try_next_u32(&mut self) -> Result<u32, Infallible> {
        Ok(self.0.next_u32())
    }
    fn try_next_u64(&mut self) -> Result<u64, Infallible> {
        let hi = self.0.next_u32() as u64;
        let lo = self.0.next_u32() as u64;
        Ok((hi << 32) | lo)
    }
    fn try_fill_bytes(&mut self, dst: &mut [u8]) -> Result<(), Infallible> {
        for chunk in dst.chunks_mut(4) {
            let b = self.0.next_u32().to_le_bytes();
            chunk.copy_from_slice(&b[..chunk.len()]);
        }
        Ok(())
    }
}

impl rand_core::TryCryptoRng for SpecRngAsCrypto<'_> {}

// ============================================================================
// Serialization helpers
// ============================================================================

/// Byte length of an ML-KEM-1024 encapsulation key (1568) and ciphertext
/// (1568). Hard-coded from FIPS 203 to avoid generic-constant plumbing.
pub const EK_BYTES: usize = 1568;
/// ML-KEM-1024 ciphertext byte length.
pub const CT_BYTES: usize = 1568;

// ML-KEM-1024 uses k=4 polynomials of 256 coefficients in Z_3329, encoded
// with 12 bits each; the final 32 bytes of the encapsulation key are rho.
const EK_COEFFICIENTS: usize = 4 * 256;
const EK_T_BYTES: usize = EK_COEFFICIENTS * 12 / 8;
const MLKEM_Q: u16 = 3329;

/// Sample a canonical public-key encoding from the uniform ML-KEM-1024 key
/// space. This is the candidate's fake key: it has no corresponding secret key.
/// The required public-key pseudorandomness/message-hiding argument is not yet
/// paper-bound for this OT composition.
fn sample_fake_ek_bytes(rng: &mut dyn SpecRng) -> Vec<u8> {
    let mut bytes = Vec::with_capacity(EK_BYTES);
    for _ in 0..(EK_COEFFICIENTS / 2) {
        let mut sample = || loop {
            let value = (rng.next_u32() & 0x0fff) as u16;
            if value < MLKEM_Q {
                break value;
            }
        };
        let a = sample();
        let b = sample();
        bytes.push(a as u8);
        bytes.push(((a >> 8) as u8 & 0x0f) | ((b as u8 & 0x0f) << 4));
        bytes.push((b >> 4) as u8);
    }
    debug_assert_eq!(bytes.len(), EK_T_BYTES);
    for _ in 0..32 {
        bytes.push(rng.next_u32() as u8);
    }
    debug_assert_eq!(bytes.len(), EK_BYTES);
    bytes
}

fn ek_to_bytes(ek: &EncapsulationKey) -> Vec<u8> {
    ek.to_bytes().as_slice().to_vec()
}

fn ek_from_bytes(b: &[u8]) -> Option<EncapsulationKey> {
    if b.len() != EK_BYTES {
        return None;
    }
    let arr: &ml_kem::array::Array<u8, _> = b.try_into().ok()?;
    EncapsulationKey::new(arr).ok()
}

fn ct_from_bytes(b: &[u8]) -> Option<Ciphertext> {
    if b.len() != CT_BYTES {
        return None;
    }
    b.try_into().ok()
}

/// Expand a 32-byte ML-KEM shared key to `n` bytes via SHA-256 counter-mode.
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
// Receiver role
// ============================================================================

/// Receiver state across one ML-KEM 1-of-2 OT.
pub struct MkReceiver {
    decaps: DecapsulationKey1024,
    choice: bool,
}

impl MkReceiver {
    /// Step 1: generate one keypair, plant its encapsulation key at `choice`,
    /// sample a fake public key with no secret key at the other index, and emit
    /// both encapsulation keys (2 × [`EK_BYTES`] bytes, index 0 first).
    pub fn setup(rng: &mut dyn SpecRng, choice: bool) -> (Self, Vec<u8>) {
        let (decaps, real_ek) = {
            let mut crng = SpecRngAsCrypto(rng);
            let decaps = DecapsulationKey1024::generate_from_rng(&mut crng);
            let real_ek = decaps.encapsulation_key().clone();
            (decaps, real_ek)
        };
        let decoy_bytes = sample_fake_ek_bytes(rng);
        let decoy_ek = ek_from_bytes(&decoy_bytes)
            .expect("uniform ML-KEM-1024 public-key encoding is canonical");

        let mut out = Vec::with_capacity(2 * EK_BYTES);
        let (k0, k1) = if choice {
            (&decoy_ek, &real_ek)
        } else {
            (&real_ek, &decoy_ek)
        };
        out.extend_from_slice(&ek_to_bytes(k0));
        out.extend_from_slice(&ek_to_bytes(k1));
        (MkReceiver { decaps, choice }, out)
    }

    /// Step 3: consume the sender's `(ct‖masked)` frame (2 ×
    /// (`CT_BYTES` + `N`) bytes), decapsulate the choice-index ciphertext, and
    /// unmask that label.
    pub fn finish<N: VoleArray<u8>>(self, frame: &[u8]) -> Result<Array<u8, N>, OtError> {
        self.finish_ref::<N>(frame)
    }

    fn finish_ref<N: VoleArray<u8>>(&self, frame: &[u8]) -> Result<Array<u8, N>, OtError> {
        let half = CT_BYTES + N::USIZE;
        if frame.len() != 2 * half {
            return Err(OtError);
        }
        let idx = self.choice as usize;
        let seg = &frame[idx * half..(idx + 1) * half];
        let ct = ct_from_bytes(&seg[..CT_BYTES]).ok_or(OtError)?;
        let masked = &seg[CT_BYTES..];
        let shared: SharedKey = self.decaps.decapsulate(&ct);
        let keystream = kdf_expand(shared.as_slice(), N::USIZE);
        Ok(Array::<u8, N>::from_fn(|i| masked[i] ^ keystream[i]))
    }
}

/// ML-KEM-1024 base-OT adapter for the existing semi-honest IKNP/SoftSpoken →
/// Ferret stack. It is an experimental candidate; the `BaseOt` trait does not
/// supply transport framing or make a security claim for this construction.
pub struct MlKem1024BaseOt;

impl BaseOt<16> for MlKem1024BaseOt {
    type SenderState = ();
    type ReceiverState = MkReceiver;
    type SetupMsg = ();
    type RecvMsg = Vec<u8>;
    type PayloadMsg = Vec<u8>;

    fn sender_setup<R: SpecRng>(_rng: &mut R) -> (Self::SenderState, Self::SetupMsg) {
        ((), ())
    }

    fn recv_start<R: SpecRng>(
        rng: &mut R,
        _setup: &Self::SetupMsg,
        choice: bool,
    ) -> (Self::ReceiverState, Self::RecvMsg) {
        MkReceiver::setup(rng, choice)
    }

    fn sender_payload<R: SpecRng>(
        rng: &mut R,
        _state: &Self::SenderState,
        recv_msg: &Self::RecvMsg,
        m0: &[u8; 16],
        m1: &[u8; 16],
    ) -> Self::PayloadMsg {
        let l0 = Array::<u8, U16>::from_fn(|i| m0[i]);
        let l1 = Array::<u8, U16>::from_fn(|i| m1[i]);
        MkSender::finish::<U16>(rng, recv_msg, [&l0, &l1])
            .expect("base-OT receiver sent two valid ML-KEM public keys")
    }

    fn recv_finish(state: &Self::ReceiverState, payload: &Self::PayloadMsg) -> [u8; 16] {
        let chosen = state
            .finish_ref::<U16>(payload)
            .expect("base-OT sender returned a correctly shaped ML-KEM frame");
        core::array::from_fn(|i| chosen[i])
    }
}

// ============================================================================
// Sender role (stateless — one shot)
// ============================================================================

/// Sender role for one ML-KEM 1-of-2 OT. Stateless: `finish` consumes the
/// receiver's two encapsulation keys and the label pair in one call.
pub struct MkSender;

impl MkSender {
    /// Step 2: consume the receiver's two encapsulation keys (2 ×
    /// [`EK_BYTES`] bytes), encapsulate a fresh shared key against each, mask
    /// the two labels, and emit the `(ct‖masked)` frame (2 ×
    /// (`CT_BYTES` + `N`) bytes, index 0 first).
    pub fn finish<N: VoleArray<u8>>(
        rng: &mut dyn SpecRng,
        eks: &[u8],
        labels: [&Array<u8, N>; 2],
    ) -> Result<Vec<u8>, OtError> {
        if eks.len() != 2 * EK_BYTES {
            return Err(OtError);
        }
        let mut crng = SpecRngAsCrypto(rng);
        let mut frame = Vec::with_capacity(2 * (CT_BYTES + N::USIZE));
        for i in 0..2 {
            let ek = ek_from_bytes(&eks[i * EK_BYTES..(i + 1) * EK_BYTES]).ok_or(OtError)?;
            let (ct, shared) = ek.encapsulate_with_rng(&mut crng);
            let keystream = kdf_expand(shared.as_slice(), N::USIZE);
            frame.extend_from_slice(ct.as_slice());
            for j in 0..N::USIZE {
                frame.push(labels[i].as_slice()[j] ^ keystream[j]);
            }
        }
        Ok(frame)
    }
}

/// Run one full ML-KEM OT in-process (test/harness convenience): returns the
/// receiver's recovered label for choice `bit`.
pub fn mk_ot_once<N: VoleArray<u8>>(
    labels: [&Array<u8, N>; 2],
    bit: bool,
    rng: &mut dyn SpecRng,
) -> Array<u8, N> {
    let (receiver, eks) = MkReceiver::setup(rng, bit);
    let frame = MkSender::finish::<N>(rng, &eks, labels).expect("sender finish");
    receiver.finish::<N>(&frame).expect("receiver finish")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ot::SeedRng;
    use hybrid_array::typenum::{U1, U16, Unsigned};
    use volar_spec::field::Galois128;
    use volar_spec::ot::ferret::{FERRET_REG_TOY, params::FerretParams};
    use volar_spec::ot::stack::OtStack;
    use volar_spec::vole::Delta;
    type N = U16;

    fn label(byte: u8) -> Array<u8, N> {
        Array::<u8, N>::from_fn(|i| byte.wrapping_add(i as u8))
    }

    /// The ML-KEM OT recovers exactly the chosen label for both choice bits.
    #[test]
    fn mlkem_recovers_chosen_label() {
        let l0 = label(0x11);
        let l1 = label(0x77);
        let mut rng = SeedRng::new(0xBEEF);
        let got0 = mk_ot_once::<N>([&l0, &l1], false, &mut rng);
        let got1 = mk_ot_once::<N>([&l0, &l1], true, &mut rng);
        assert_eq!(got0, l0);
        assert_eq!(got1, l1);
    }

    /// Uniformly sampled fake keys remain valid canonical ML-KEM public keys.
    #[test]
    fn fake_public_key_is_valid() {
        let mut rng = SeedRng::new(0xFAE);
        let bytes = sample_fake_ek_bytes(&mut rng);
        let key = ek_from_bytes(&bytes).expect("fake key is canonical");
        assert_eq!(key.to_bytes().as_slice(), bytes.as_slice());
    }

    /// The sender's frame does not leak the un-chosen label in plaintext, and
    /// the two encapsulated halves differ.
    #[test]
    fn mlkem_hides_other_label() {
        let l0 = label(0x00);
        let l1 = label(0xFF);
        let mut rng = SeedRng::new(0xF00D);
        let (receiver, eks) = MkReceiver::setup(&mut rng, false);
        let frame = MkSender::finish::<N>(&mut rng, &eks, [&l0, &l1]).unwrap();
        let half = CT_BYTES + N::USIZE;
        // Masked label regions are not the plaintext labels.
        assert_ne!(&frame[CT_BYTES..CT_BYTES + 16], l0.as_slice());
        assert_ne!(&frame[half + CT_BYTES..half + CT_BYTES + 16], l1.as_slice());
        // The two ciphertexts differ (independent encapsulations).
        assert_ne!(&frame[..CT_BYTES], &frame[half..half + CT_BYTES]);
        // Chosen label recovers correctly.
        let got = receiver.finish::<N>(&frame).unwrap();
        assert_eq!(got, l0);
    }

    /// ML-KEM base OT can seed the common SoftSpoken → Ferret pool and the
    /// resulting chosen-bit correlations satisfy their VOLE equation.
    #[test]
    fn mlkem_base_ot_ferret_stack_produces_valid_correlations() {
        let mut rng_s = SeedRng::new(0x1122_3344);
        let mut rng_r = SeedRng::new(0x5566_7788);
        let params: FerretParams = FERRET_REG_TOY;
        let mut stack =
            OtStack::setup_with_base::<Sha256, _, MlKem1024BaseOt>(&mut rng_s, &mut rng_r, params);
        let bits = [true, false, true, false, true, true];
        let committed = stack.commit_bits(&mut rng_s, &bits);
        let delta = Delta {
            delta: Array::<Galois128, U1>::from_fn(|_| {
                Galois128(u128::from_le_bytes(stack.sender.seed.delta))
            }),
        };
        for (i, (vope, q)) in committed.iter().enumerate() {
            assert!(vope.clone() * delta.clone() == *q, "COT row {i}");
        }
    }
}
