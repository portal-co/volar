// @reliability: experimental
//! @ai: assisted
//! SoftSpoken-style OT extension (Roy, EUROCRYPT 2022).
//!
//! Builds on top of [`crate::ot::iknp`] by adding a **consistency-check tag**:
//! both parties hash their view of the OT-extension output (sender's `r0`
//! rows, receiver's reconstructed `r0` from `v` and the choice bits +
//! `Δ_msg`). For honest parties the tags are identical; a transcript-level
//! deviation flips the tag with overwhelming probability.
//!
//! # Protocol shape
//!
//! ```text
//! 1. Run iknp_cot_extend  → (sender_r0, receiver_v)
//! 2. Sender:    tag_S = H( "ssp-v1" || Δ_msg || r0_0 || r0_1 || ... )
//! 3. Receiver:  tag_R = H( "ssp-v1" || Δ_msg || r0'_0 || r0'_1 || ... )
//!    where  r0'_j = v_j ⊕ b_j · Δ_msg
//! 4. Both expose tags; honest exchange satisfies tag_S == tag_R.
//! ```
//!
//! # Caveats
//!
//! - This is a **semi-honest** core (the IKNP body is unchanged) plus a
//!   tag-comparison stub. A malicious receiver who deviates in IKNP and
//!   later "lies" about `b_j` produces an `r0'_j` that doesn't match the
//!   sender's `r0_j` — the tag mismatches. This is *not* a full malicious
//!   security proof: it catches transcript divergence but doesn't yet
//!   bind the receiver's choice bits to the base-OT inputs (that needs the
//!   Roy 2022 §4 random-linear-combination check using a correlation-robust
//!   hash, which we leave as future work).
//!
//! - The `K` parameter (subfield expansion factor) is currently a
//!   no-op — at `K = 1` the protocol is bit-IKNP. Higher `K` would
//!   group `K` bits per base 1-of-2^K OT (reducing base OT count to
//!   `κ/K`). The signature is in place so callers can pin the parameter,
//!   but the implementation falls through to the bit-level extension.
//!   This matches Roy 2022's Theorem 1 reduction at `k=1` and is an
//!   honest stand-in pending the full subfield-VOLE construction.

use digest::Digest;

use crate::SpecRng;
use super::group::Group;
use super::iknp::iknp_cot_extend;

/// Output bundle of one SoftSpoken extension.
pub struct SoftSpokenOut<const M: usize, const L: usize, D: Digest> {
    pub sender_r0: [[u8; L]; M],
    pub receiver_v: [[u8; L]; M],
    /// `H( domain || Δ_msg || r0_j for j ∈ [M] )`.
    pub sender_tag: digest::Output<D>,
    /// `H( domain || Δ_msg || (v_j ⊕ b_j·Δ_msg) for j ∈ [M] )`.
    pub receiver_tag: digest::Output<D>,
}

impl<const M: usize, const L: usize, D: Digest> SoftSpokenOut<M, L, D> {
    /// Tags match ⇒ honest transcript.
    pub fn check(&self) -> bool {
        self.sender_tag == self.receiver_tag
    }
}

const TAG_DOMAIN: &[u8] = b"softspoken-cot-tag-v1";

/// Run a SoftSpoken-style C-OT extension.
///
/// `K` is the subfield expansion factor (see module docs). Currently must
/// be ≥ 1; values > 1 are accepted but the body delegates to the bit-level
/// IKNP, so the base-OT count is `κ` regardless.
pub fn softspoken_cot_extend<G, D, R, const K: usize, const M: usize, const L: usize>(
    rng_s: &mut R,
    rng_r: &mut R,
    receiver_bits: &[bool; M],
    delta_msg: &[u8; L],
) -> SoftSpokenOut<M, L, D>
where
    G: Group,
    D: Digest,
    R: SpecRng,
{
    // K must be at least 1; documented in the module-level caveat.
    debug_assert!(K >= 1, "SoftSpoken expansion factor K must be ≥ 1");

    let (sender_r0, receiver_v) =
        iknp_cot_extend::<G, D, _, M, L>(rng_s, rng_r, receiver_bits, delta_msg);

    // Sender tag: hash of all r0 rows.
    let mut hs = D::new();
    hs.update(TAG_DOMAIN);
    hs.update(delta_msg);
    for row in sender_r0.iter() {
        hs.update(row);
    }
    let sender_tag = hs.finalize();

    // Receiver tag: same shape, derived from v and choice bits.
    let mut hr = D::new();
    hr.update(TAG_DOMAIN);
    hr.update(delta_msg);
    for j in 0..M {
        let mut r0_reconstructed = [0u8; L];
        if receiver_bits[j] {
            for b in 0..L {
                r0_reconstructed[b] = receiver_v[j][b] ^ delta_msg[b];
            }
        } else {
            r0_reconstructed = receiver_v[j];
        }
        hr.update(&r0_reconstructed);
    }
    let receiver_tag = hr.finalize();

    SoftSpokenOut { sender_r0, receiver_v, sender_tag, receiver_tag }
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

    /// Honest run: tags match and the C-OT relation holds.
    #[test]
    fn softspoken_honest_round_trip() {
        const K: usize = 1;
        const M: usize = 16;
        const L: usize = 16;

        let mut rng_s = TestRng(0xDEAD_BEEF_DEAD_BEEF);
        let mut rng_r = TestRng(0xCAFE_F00D_CAFE_F00D);

        let mut bits = [false; M];
        for j in 0..M { bits[j] = (j * 5 + 3) & 1 == 1; }

        let mut delta = [0u8; L];
        for b in 0..L { delta[b] = (b as u8).wrapping_mul(7).wrapping_add(11); }

        let out = softspoken_cot_extend::<ToyGroup, Sha256, _, K, M, L>(
            &mut rng_s, &mut rng_r, &bits, &delta,
        );

        assert!(out.check(), "honest tags must match");
        for j in 0..M {
            for b in 0..L {
                let expected = if bits[j] { out.sender_r0[j][b] ^ delta[b] }
                               else       { out.sender_r0[j][b] };
                assert_eq!(out.receiver_v[j][b], expected,
                           "row {j} byte {b}: C-OT relation broken");
            }
        }
    }

    /// Tampering with sender's r0 must flip the sender tag → mismatch.
    #[test]
    fn softspoken_tampered_sender_row_rejected() {
        const M: usize = 8;
        const L: usize = 8;
        let mut rng_s = TestRng(0x1234_5678_9ABC_DEF0);
        let mut rng_r = TestRng(0xFEDC_BA98_7654_3210);
        let bits = [true, false, true, false, true, false, true, false];
        let delta = [0xAAu8; L];

        let mut out = softspoken_cot_extend::<ToyGroup, Sha256, _, 1, M, L>(
            &mut rng_s, &mut rng_r, &bits, &delta,
        );
        // Forge a sender row.
        out.sender_r0[3][0] ^= 1;
        // Recompute sender tag.
        let mut hs = Sha256::new();
        hs.update(TAG_DOMAIN);
        hs.update(delta);
        for row in out.sender_r0.iter() { hs.update(row); }
        out.sender_tag = hs.finalize();

        assert!(!out.check(), "tampered sender row must mismatch");
    }

    /// Tampering with receiver's v must flip the receiver tag → mismatch.
    #[test]
    fn softspoken_tampered_receiver_row_rejected() {
        const M: usize = 8;
        const L: usize = 8;
        let mut rng_s = TestRng(0x1111_2222_3333_4444);
        let mut rng_r = TestRng(0x5555_6666_7777_8888);
        let bits = [false; M];
        let delta = [0x55u8; L];

        let mut out = softspoken_cot_extend::<ToyGroup, Sha256, _, 1, M, L>(
            &mut rng_s, &mut rng_r, &bits, &delta,
        );
        out.receiver_v[2][1] ^= 1;
        // Recompute receiver tag.
        let mut hr = Sha256::new();
        hr.update(TAG_DOMAIN);
        hr.update(delta);
        for j in 0..M {
            let mut r0r = [0u8; L];
            if bits[j] {
                for b in 0..L { r0r[b] = out.receiver_v[j][b] ^ delta[b]; }
            } else {
                r0r = out.receiver_v[j];
            }
            hr.update(&r0r);
        }
        out.receiver_tag = hr.finalize();

        assert!(!out.check(), "tampered receiver row must mismatch");
    }

    /// At K=2 the API is identical (current impl falls back to K=1).
    #[test]
    fn softspoken_k_param_currently_passthrough() {
        const M: usize = 8;
        const L: usize = 8;
        let mut rng_s_a = TestRng(0xAABB);
        let mut rng_r_a = TestRng(0xCCDD);
        let mut rng_s_b = TestRng(0xAABB);
        let mut rng_r_b = TestRng(0xCCDD);
        let bits = [true; M];
        let delta = [1u8; L];

        let out_k1 = softspoken_cot_extend::<ToyGroup, Sha256, _, 1, M, L>(
            &mut rng_s_a, &mut rng_r_a, &bits, &delta,
        );
        let out_k2 = softspoken_cot_extend::<ToyGroup, Sha256, _, 2, M, L>(
            &mut rng_s_b, &mut rng_r_b, &bits, &delta,
        );
        // Same RNG seeds + same bits + same Δ ⇒ same outputs (K ignored).
        assert_eq!(out_k1.sender_r0, out_k2.sender_r0);
    }
}
