// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//!
//! ML-KEM-1024 1-of-2 oblivious transfer (the `mlkem` feature).
//!
//! A post-quantum alternative to the default Chou–Orlandi base OT
//! ([`crate::ot::CoSender`]/[`crate::ot::CoReceiver`]). The construction is a
//! 1-of-N KEM-OT specialized to N=2, the same shape as cirrus's `miniot`:
//!
//! 1. **Receiver** generates a real ML-KEM keypair, plants the encapsulation
//!    key at index `choice ∈ {0,1}`, and a freshly generated *decoy* key
//!    (whose decapsulation key it discards) at the other index; it sends both
//!    encapsulation keys to the sender. The sender cannot tell which index
//!    holds a key the receiver can decapsulate.
//! 2. **Sender** encapsulates a fresh shared key against *both* public keys
//!    and XORs each resulting shared key (KDF-expanded to `N` bytes) with the
//!    corresponding label; it sends both `(ciphertext, masked-label)` pairs.
//! 3. **Receiver** decapsulates only the ciphertext at its choice index and
//!    unmasks that label. The other label stays hidden: its shared key is
//!    encapsulated under a key the receiver cannot decapsulate.
//!
//! Roles are explicit step state machines ([`MkReceiver`] / [`MkSender`]) so
//! a harness can interleave both on one thread or drive them across a framed
//! transport, exactly like the Chou–Orlandi pair.

use alloc::vec::Vec;

use core::convert::Infallible;
use digest::Digest;
use hybrid_array::Array;
use ml_kem::{
    Decapsulate, Encapsulate, Generate, KeyExport,
    ml_kem_1024::{Ciphertext, EncapsulationKey},
    {DecapsulationKey1024, SharedKey},
};
use sha2::Sha256;
use volar_spec::SpecRng;
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
    /// Step 1: generate the keypair, plant the real encapsulation key at
    /// `choice` and a decoy at the other index, and emit both encapsulation
    /// keys (2 × [`EK_BYTES`] bytes, index 0 first).
    pub fn setup(rng: &mut dyn SpecRng, choice: bool) -> (Self, Vec<u8>) {
        let mut crng = SpecRngAsCrypto(rng);
        let decaps = DecapsulationKey1024::generate_from_rng(&mut crng);
        let real_ek = decaps.encapsulation_key().clone();
        // A decoy key the receiver cannot decapsulate: generate and drop its
        // decapsulation half, keeping only the encapsulation key.
        let decoy_decaps = DecapsulationKey1024::generate_from_rng(&mut crng);
        let decoy_ek = decoy_decaps.encapsulation_key().clone();

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
    use hybrid_array::typenum::Unsigned;
    use typenum::U16;

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
}
