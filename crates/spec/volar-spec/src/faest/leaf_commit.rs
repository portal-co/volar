// @reliability: experimental
//! @ai: assisted
//! FAEST leaf commitment (FAEST v2 spec §5.1, Figure 5.3).
//!
//! ## Three variants
//!
//! The FAEST spec lists three leaf-commit constructions; we implement two of
//! the three. Each produces `(sd, com)` from `(r, iv, tweak)` (plus a
//! universal-hash key for the FAEST variant).
//!
//! | Variant | Output `com` size | Assumption | Status |
//! |---|---|---|---|
//! | `BAVC.LeafCommit` (RO-based, generic) | 2λ bits | random oracle | [`RoLeafCommit`] — implemented |
//! | `FAEST-EM.LeafCommit` | 2λ bits | almost-injective PRG (Def 9.13) | [`EmLeafCommit`] — implemented |
//! | `FAEST.LeafCommit` | 3λ bits | universal hashing in GF(2^{3λ}) | NOT YET — needs GF(2^384) primitives |
//!
//! The FAEST-128 parameter set requires the third variant for KAT alignment
//! with the reference. We defer it to M6 alongside the GF(2^384)
//! implementation since the BAVC code is generic over a `LeafCommit` trait
//! and can swap the impl in transparently.
//!
//! ## Trait surface
//!
//! [`LeafCommit`] abstracts both variants. BAVC ([`super::bavc`]) is generic
//! over `L: LeafCommit`. Output sizes are const-generic so the GGM tree's
//! per-leaf hash arrays land at fixed sizes known at compile time.
//!
//! ## Inputs
//!
//! All variants take the same `(r, iv, tweak)` triple:
//!
//! - `r ∈ {0,1}^λ` — leaf randomness (the seed expanded from the GGM tree)
//! - `iv ∈ {0,1}^128` — per-signature IV
//! - `tweak ∈ {0,1}^32` — domain separator (typically the leaf index)
//!
//! `r` length is fixed to 16 bytes (λ=128). The trait stays generic over
//! `LAMBDA_BYTES` for future λ=192, λ=256 work.

use core::marker::PhantomData;

use digest::Digest;

use super::prg::aes_ctr_prg;

/// Pluggable leaf commitment for BAVC.
///
/// `SD_BYTES` and `COM_BYTES` are output sizes in bytes. For the
/// FAEST-128 / FAEST-EM-128 parameter sets, `SD_BYTES = 16`
/// (λ = 128 bits = 16 bytes) and `COM_BYTES = 32` (2λ = 256 bits =
/// 32 bytes).
///
/// FAEST-128's actual leaf commit uses `COM_BYTES = 48` (3λ = 384 bits)
/// under the universal-hash variant — that variant will be a separate
/// impl with `COM_BYTES = 48`, and BAVC will be generic over the constant.
pub trait LeafCommit<const SD_BYTES: usize, const COM_BYTES: usize> {
    /// Produce `(sd, com) = LeafCommit(r, iv, tweak)`. `sd` is the
    /// leaf-level VOLE seed; `com` is the binding commitment that gets
    /// hashed up the tree.
    fn commit(
        r: &[u8; SD_BYTES],
        iv: &[u8; 16],
        tweak: u32,
    ) -> ([u8; SD_BYTES], [u8; COM_BYTES]);
}

/// FAEST §5.1 `BAVC.LeafCommit` (Figure 5.3, top variant) — random-oracle
/// based.
///
/// Hashes `(r ∥ iv ∥ tweak_le_bytes)` with a digest `D` of at least
/// `SD_BYTES + COM_BYTES` output bytes. First `SD_BYTES` bytes are the
/// seed; remaining `COM_BYTES` bytes are the commitment.
///
/// Binding/hiding rests on `D` being a random oracle.
pub struct RoLeafCommit<D: Digest> {
    _d: PhantomData<D>,
}

impl<D: Digest, const SD_BYTES: usize, const COM_BYTES: usize> LeafCommit<SD_BYTES, COM_BYTES>
    for RoLeafCommit<D>
where
    D: Digest,
{
    fn commit(
        r: &[u8; SD_BYTES],
        iv: &[u8; 16],
        tweak: u32,
    ) -> ([u8; SD_BYTES], [u8; COM_BYTES]) {
        // Caller must pick `D` such that D::OutputSize >= SD_BYTES + COM_BYTES.
        // We panic on mismatch rather than smuggling it through the type
        // system because the const generics here can't talk to D's
        // associated typenum.
        let out_size = <D as Digest>::output_size();
        assert!(
            out_size >= SD_BYTES + COM_BYTES,
            "RoLeafCommit: D::output_size() = {} but SD_BYTES + COM_BYTES = {}",
            out_size,
            SD_BYTES + COM_BYTES,
        );

        let mut h = D::new();
        h.update(r);
        h.update(iv);
        h.update(&tweak.to_le_bytes());
        let digest = h.finalize();

        let mut sd = [0u8; SD_BYTES];
        let mut com = [0u8; COM_BYTES];
        sd.copy_from_slice(&digest[..SD_BYTES]);
        com.copy_from_slice(&digest[SD_BYTES..SD_BYTES + COM_BYTES]);
        (sd, com)
    }
}

/// FAEST-EM §5.1 `FAEST-EM.LeafCommit` (Figure 5.3, bottom variant) —
/// PRG-based.
///
/// `sd = r` (no transformation) and `com = AES-CTR(r, iv, tweak; 2λ)`.
/// Binding rests on the almost-injective-PRG assumption (Def 9.13). This
/// is the variant FAEST-EM-128 uses for performance; we get a working
/// BAVC out of it before tackling the GF(2^{3λ}) universal hash needed by
/// plain FAEST.
pub struct EmLeafCommit;

impl<const SD_BYTES: usize, const COM_BYTES: usize> LeafCommit<SD_BYTES, COM_BYTES>
    for EmLeafCommit
{
    fn commit(
        r: &[u8; SD_BYTES],
        iv: &[u8; 16],
        tweak: u32,
    ) -> ([u8; SD_BYTES], [u8; COM_BYTES]) {
        // EM variant requires SD_BYTES == 16 because the PRG keys with a
        // 16-byte AES key. Other parameter sets aren't supported yet.
        assert_eq!(
            SD_BYTES, 16,
            "EmLeafCommit currently only supports SD_BYTES = 16 (λ = 128)"
        );

        let mut seed_buf = [0u8; 16];
        seed_buf.copy_from_slice(&r[..]);
        let com_vec = aes_ctr_prg(&seed_buf, iv, tweak, COM_BYTES);

        let mut sd = [0u8; SD_BYTES];
        sd.copy_from_slice(r);
        let mut com = [0u8; COM_BYTES];
        com.copy_from_slice(&com_vec[..COM_BYTES]);
        (sd, com)
    }
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;

    // Use SHAKE-256 from the dev-dep `sha3` crate for the RO variant
    // tests. Its `Digest` impl produces 32 bytes (truncated extendable
    // output) which fits SD_BYTES + COM_BYTES = 16 + 16 = 32.
    use sha3::Sha3_256;

    #[test]
    fn ro_leaf_commit_is_deterministic() {
        let r = [0u8; 16];
        let iv = [1u8; 16];
        let tweak = 42;
        let (sd_a, com_a) = <RoLeafCommit<Sha3_256> as LeafCommit<16, 16>>::commit(&r, &iv, tweak);
        let (sd_b, com_b) = <RoLeafCommit<Sha3_256> as LeafCommit<16, 16>>::commit(&r, &iv, tweak);
        assert_eq!(sd_a, sd_b);
        assert_eq!(com_a, com_b);
    }

    #[test]
    fn ro_leaf_commit_tweak_matters() {
        let r = [0u8; 16];
        let iv = [1u8; 16];
        let (sd_a, com_a) = <RoLeafCommit<Sha3_256> as LeafCommit<16, 16>>::commit(&r, &iv, 0);
        let (sd_b, com_b) = <RoLeafCommit<Sha3_256> as LeafCommit<16, 16>>::commit(&r, &iv, 1);
        // Different tweak → different digest → both sd and com diverge.
        assert_ne!(sd_a, sd_b);
        assert_ne!(com_a, com_b);
    }

    #[test]
    fn ro_leaf_commit_iv_matters() {
        let r = [0u8; 16];
        let iv_a = [0u8; 16];
        let mut iv_b = [0u8; 16];
        iv_b[0] = 1;
        let (_, com_a) = <RoLeafCommit<Sha3_256> as LeafCommit<16, 16>>::commit(&r, &iv_a, 0);
        let (_, com_b) = <RoLeafCommit<Sha3_256> as LeafCommit<16, 16>>::commit(&r, &iv_b, 0);
        assert_ne!(com_a, com_b);
    }

    #[test]
    fn em_leaf_commit_sd_equals_input_seed() {
        let r = [0x55u8; 16];
        let iv = [0u8; 16];
        let (sd, _) = <EmLeafCommit as LeafCommit<16, 32>>::commit(&r, &iv, 0);
        // EM variant uses sd = r directly (no PRG transformation on the seed).
        assert_eq!(sd, r);
    }

    #[test]
    fn em_leaf_commit_distinct_inputs_distinct_outputs() {
        let r_a = [0u8; 16];
        let r_b = [1u8; 16];
        let iv = [0u8; 16];
        let (_, com_a) = <EmLeafCommit as LeafCommit<16, 32>>::commit(&r_a, &iv, 0);
        let (_, com_b) = <EmLeafCommit as LeafCommit<16, 32>>::commit(&r_b, &iv, 0);
        assert_ne!(com_a, com_b);
    }
}
