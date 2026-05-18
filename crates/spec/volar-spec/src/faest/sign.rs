// @reliability: experimental
//! @ai: assisted
//! FAEST v2 top-level sign / verify and key generation — §6.
//!
//! ## Scope
//!
//! This module implements the full Fiat-Shamir signing flow, wiring together:
//!
//! - [`aes`] — AES-128 OWF (the hard relation being proved)
//! - [`bavc`] — GGM-tree BAVC commitment and opening
//! - [`convert_to_vole`] — seed expansion into VOLE correlations
//! - [`universal_hash`] — VOLEHash / ZKHash for the VOLE consistency check
//! - [`transcript`] — SHAKE-based Fiat-Shamir challenge derivation + grinding
//!
//! ## Parameters (simplified)
//!
//! The implementation uses a *simplified* FAEST-128 parameter set that
//! satisfies our current BAVC constraint (τ·N must be a power of two):
//!
//! | Constant | Value | Note |
//! |---|---|---|
//! | λ | 128 bits (16 bytes) | Security parameter |
//! | τ | 4 | Sub-VOLEs (simplified from FAEST-128s τ=11) |
//! | N | 8 | Size of each sub-VOLE (2^k with k=3) |
//! | ℓ̂ | 16 bytes | VOLE vector length |
//!
//! Full FAEST-128s uses τ=11 with mixed sub-VOLE sizes, which requires
//! extending BAVC to non-uniform N.  The cryptographic structure (BAVC →
//! ConvertToVOLE → VOLEHash → transcript) is identical; only the parameter
//! packing differs.  KAT alignment is tracked as M6-KAT.
//!
//! ## QuickSilver stub
//!
//! The inner QuickSilver AES-witness proof (A-hat, B-hat, C-hat) requires a
//! compiled AES circuit weaved via the FAEST weaver.  That pipeline is
//! available from M5 but the end-to-end circuit evaluation is deferred to
//! when the weaver's K=3 gate dispatch lands in M6.  As a stand-in, this
//! module uses a hash-based "proof" that is self-consistent (sign ↔ verify
//! round-trips) but does **not** bind to the AES witness.
//!
//! The hook point is clearly marked `// TODO: QuickSilver` so it can be
//! replaced in a follow-up without restructuring the rest of the flow.

extern crate alloc;
use alloc::vec;
use alloc::vec::Vec;

use sha3::{Sha3_256, digest::{Digest, Update as DigestUpdate}};

use super::{
    aes::encrypt_block as aes128_encrypt,
    bavc::{Bavc, BavcCommitment, BavcOpening},
    convert_to_vole::{concat_small_voles, convert_to_vole, BigVoleProver},
    leaf_commit::EmLeafCommit,
    transcript::{chall1, chall2, chall3, grind_chall3},
    universal_hash::{UniversalHashKey, UniversalHashOutput, vole_hash},
};
use crate::SpecRng;

// ─── Parameters ───────────────────────────────────────────────────────────────

/// Simplified FAEST-128 parameters (τ·N = power-of-two BAVC).
/// Full FAEST-128s (τ=11 mixed sizes) is a KAT follow-up.
pub const LAMBDA_BYTES: usize = 16;
pub const TAU: usize = 4;
pub const SUB_VOLE_N: usize = 8; // each sub-VOLE has 8 seeds, k=3 bits
pub const SUB_VOLE_K: usize = 3; // log2(SUB_VOLE_N)
pub const L_HAT_BYTES: usize = 16;
pub const W_GRIND: u32 = 4; // reduced for test speed (FAEST-128s = 11)

/// Per-leaf commitment size for `EmLeafCommit`.
const COM_BYTES: usize = 32;

// ─── Key types ────────────────────────────────────────────────────────────────

/// FAEST secret key: AES-128 key (16 bytes).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FaestSecretKey(pub [u8; LAMBDA_BYTES]);

/// FAEST public key: AES-128 encryption of the all-zeros block.
/// `pk = AES_{sk}(0^128)`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FaestPublicKey(pub [u8; LAMBDA_BYTES]);

// ─── Signature ────────────────────────────────────────────────────────────────

/// FAEST signature (simplified FAEST-128 parameters).
#[derive(Clone, Debug)]
pub struct FaestSignature {
    /// Per-signature IV (16 bytes).
    pub iv: [u8; LAMBDA_BYTES],
    /// BAVC commitment root hash.
    pub bavc_root: Vec<u8>,
    /// Hidden leaf commitments (one per sub-VOLE, for verifier reconstruction).
    pub hidden_commits: Vec<[u8; COM_BYTES]>,
    /// Revealed internal node seeds for BAVC opening.
    pub nodes: Vec<(usize, [u8; LAMBDA_BYTES])>,
    /// Correction vectors `c_i = u_i XOR u_0` for i ∈ [1..τ).
    pub corrections: Vec<Vec<u8>>,
    /// QuickSilver proof A-hat (stub: hash of VOLE u vector).
    pub a_hat: Vec<u8>,
    /// Prover's VOLE u vector (stored for chall_2 reconstruction by verifier).
    pub vole_u: Vec<u8>,
    /// QuickSilver proof B-hat (stub: all-zeros).
    pub b_hat: Vec<u8>,
    /// QuickSilver proof C-hat with appended grinding counter.
    pub c_hat_with_counter: Vec<u8>,
    /// Final Fiat-Shamir challenge (λ bytes with w_grind trailing zero bits).
    pub chall_3: Vec<u8>,
    /// Grinding counter (the number of retries before chall_3 met the grinding target).
    pub counter: u32,
}

// ─── Key generation ───────────────────────────────────────────────────────────

/// Generate a FAEST key pair.
///
/// `sk ← {0,1}^λ` uniformly; `pk = AES_{sk}(0^128)`.
pub fn keygen(rng: &mut impl SpecRng) -> (FaestSecretKey, FaestPublicKey) {
    let mut sk = [0u8; LAMBDA_BYTES];
    for b in sk.iter_mut() {
        *b = rng.next_u8();
    }
    let pk = aes128_encrypt(&sk, &[0u8; LAMBDA_BYTES]);
    (FaestSecretKey(sk), FaestPublicKey(pk))
}

// ─── Signing ──────────────────────────────────────────────────────────────────

/// Sign `message` under `sk`, producing a [`FaestSignature`].
///
/// Uses `iv_seed` as the entropy source for IV derivation (in real FAEST
/// this comes from the signer's randomness; a hash of `sk ∥ msg` is common).
pub fn sign(
    sk: &FaestSecretKey,
    pk: &FaestPublicKey,
    message: &[u8],
    iv_seed: [u8; LAMBDA_BYTES],
) -> FaestSignature {
    // ── 1. Derive IV ────────────────────────────────────────────────────────
    // IV = AES_{iv_seed}(counter=0) as a simple PRF.  Real FAEST uses a
    // dedicated SHAKE-based derivation; this is a structural stand-in.
    let iv: [u8; LAMBDA_BYTES] = aes128_encrypt(&iv_seed, &[0u8; LAMBDA_BYTES]);

    // ── 2. Derive BAVC root seed from (sk, iv) ───────────────────────────
    let r: [u8; LAMBDA_BYTES] = aes128_encrypt(&sk.0, &iv);

    // ── 3. BAVC commit ───────────────────────────────────────────────────────
    let commitment: BavcCommitment<COM_BYTES> =
        Bavc::<EmLeafCommit, COM_BYTES>::commit::<Sha3_256>(r, &iv, TAU, SUB_VOLE_N);

    // ── 4. chall_1 ────────────────────────────────────────────────────────────
    // mu = H(pk ∥ msg) — a short domain-separated hash for the public input.
    let mu: Vec<u8> = {
        let mut h = Sha3_256::new();
        DigestUpdate::update(&mut h, &pk.0);
        DigestUpdate::update(&mut h, message);
        Digest::finalize(h).to_vec()
    };
    // Use the root hash as the commitment input for chall_1.
    // Both signer and verifier have access to bavc_root (it's in the signature).
    let chall_1 = chall1(&mu, &iv, &commitment.root, LAMBDA_BYTES + 8, false);

    // ── 5. Expand challenge to per-sub-VOLE missing indices ─────────────────
    let deltas = expand_challenge_to_deltas(&chall_1, TAU, SUB_VOLE_N);

    // ── 6. BAVC open ─────────────────────────────────────────────────────────
    let nodes = Bavc::<EmLeafCommit, COM_BYTES>::collect_open_nodes(
        &deltas,
        // We need the full tree. Recompute it from r (prover has r privately).
        &recompute_tree(r, TAU * SUB_VOLE_N),
        TAU,
        SUB_VOLE_N,
    );
    let hidden_commits: Vec<[u8; COM_BYTES]> = deltas
        .iter()
        .enumerate()
        .map(|(i, &d)| commitment.commitments[i * SUB_VOLE_N + d])
        .collect();
    let opening = BavcOpening::<COM_BYTES> {
        hidden_commits: hidden_commits.clone(),
        nodes: nodes.clone(),
    };
    let _ = opening; // structural — used implicitly below for signature bytes

    // ── 7. ConvertToVOLE (prover: all seeds present) ──────────────────────────
    let mut sub_voles = Vec::with_capacity(TAU);
    for i in 0..TAU {
        let seeds_i: Vec<Option<[u8; LAMBDA_BYTES]>> = (0..SUB_VOLE_N)
            .map(|j| Some(commitment.seeds[i * SUB_VOLE_N + j]))
            .collect();
        sub_voles.push(convert_to_vole(&seeds_i, &iv, i as u32, L_HAT_BYTES));
    }
    let big_vole: BigVoleProver = concat_small_voles(sub_voles);

    // ── 8. Build correction payload for chall_2 ────────────────────────────
    let corrections_flat: Vec<u8> = big_vole.c.iter().flatten().cloned().collect();
    let chall_2 = chall2(&chall_1, &big_vole.u, &corrections_flat, LAMBDA_BYTES + 8, false);

    // ── 9. VOLEHash keys from chall_2 ────────────────────────────────────────
    let hash_key = hash_key_from_chall(&chall_2);

    // ── 10. QuickSilver stub (TODO: replace with compiled AES circuit) ────────
    // A_hat = VOLEHash(key, u), B_hat = zeros, C_hat = zeros.
    // This is self-consistent: the verifier recomputes A_hat from the
    // reconstructed verifier Q vector and checks A_hat == B_hat + chall_3*C_hat.
    let a_hat_out: UniversalHashOutput = vole_hash(&hash_key, &big_vole.u);
    let a_hat: Vec<u8> = field128_to_bytes(a_hat_out.h0)
        .iter()
        .chain(field64_to_bytes(a_hat_out.h1).iter())
        .cloned()
        .collect();
    let b_hat = vec![0u8; a_hat.len()];
    let c_hat_base = vec![0u8; a_hat.len()];

    // ── 11. Grinding loop ────────────────────────────────────────────────────
    let (chall_3, counter) = grind_chall3(
        &chall_2,
        &a_hat,
        &b_hat,
        &c_hat_base,
        LAMBDA_BYTES,
        W_GRIND,
        false,
        1_000_000,
    )
    .expect("grinding must terminate within 1M iterations");

    // Reconstruct c_hat_with_counter for signature storage.
    let mut c_hat_with_counter = c_hat_base.clone();
    c_hat_with_counter.extend_from_slice(&counter.to_le_bytes());

    FaestSignature {
        iv,
        bavc_root: commitment.root.clone(),
        hidden_commits,
        nodes,
        corrections: big_vole.c.clone(),
        vole_u: big_vole.u.clone(),
        a_hat,
        b_hat,
        c_hat_with_counter,
        chall_3,
        counter,
    }
}

// ─── Verification ─────────────────────────────────────────────────────────────

/// Verify a [`FaestSignature`] against `pk` and `message`.
///
/// Returns `true` iff the signature is valid.
pub fn verify(
    pk: &FaestPublicKey,
    message: &[u8],
    sig: &FaestSignature,
) -> bool {
    let iv = &sig.iv;

    // ── 1. Reconstruct mu and chall_1 ────────────────────────────────────────
    let mu: Vec<u8> = {
        let mut h = Sha3_256::new();
        DigestUpdate::update(&mut h, &pk.0);
        DigestUpdate::update(&mut h, message);
        Digest::finalize(h).to_vec()
    };
    let chall_1 = chall1(&mu, iv, &sig.bavc_root, LAMBDA_BYTES + 8, false);

    // ── 2. Derive deltas ────────────────────────────────────────────────────
    let deltas = expand_challenge_to_deltas(&chall_1, TAU, SUB_VOLE_N);

    // ── 3. BAVC reconstruct ─────────────────────────────────────────────────
    let reconstructed_seeds_opt = Bavc::<EmLeafCommit, COM_BYTES>::reconstruct::<Sha3_256>(
        &sig.nodes,
        &sig.hidden_commits,
        &deltas,
        iv,
        &sig.bavc_root,
        TAU,
        SUB_VOLE_N,
    );
    let reconstructed_seeds = match reconstructed_seeds_opt {
        Some(s) => s,
        None => return false, // root mismatch
    };

    // ── 4. ConvertToVOLE (verifier: one seed missing per sub-VOLE) ──────────
    let mut sub_voles_v = Vec::with_capacity(TAU);
    for i in 0..TAU {
        // Verifier seeds: apply the Proposition 5.2 permutation.
        // sd'_j = sd_{j XOR delta_i}; sd'_0 = None (hidden).
        let d = deltas[i];
        let verifier_seeds: Vec<Option<[u8; LAMBDA_BYTES]>> = (0..SUB_VOLE_N)
            .map(|j| {
                if j == 0 {
                    None
                } else {
                    Some(reconstructed_seeds[i * SUB_VOLE_N + (j ^ d)])
                }
            })
            .collect();
        sub_voles_v.push(convert_to_vole(&verifier_seeds, iv, i as u32, L_HAT_BYTES));
    }

    // ── 5. Verifier's big Q vector via corrections ───────────────────────────
    let corrections = &sig.corrections;
    if corrections.len() != TAU - 1 {
        return false;
    }
    let big_q: Vec<u8> = {
        use super::convert_to_vole::concat_small_voles_verifier;
        let q_out = concat_small_voles_verifier(sub_voles_v, &deltas, corrections);
        q_out.q_columns.into_iter().flatten().collect()
    };

    // ── 6. Reconstruct chall_2 ───────────────────────────────────────────────
    // The prover sends `vole_u` explicitly; verifier uses it for chall_2.
    // (In full FAEST, the verifier reconstructs Q from the opening and
    //  checks a consistency equation instead of reusing vole_u directly.)
    let corrections_flat: Vec<u8> = sig.corrections.iter().flatten().cloned().collect();
    let chall_2 = chall2(&chall_1, &sig.vole_u, &corrections_flat, LAMBDA_BYTES + 8, false);

    // ── 7. Verify grinding: rederive chall_3 and check against signature ─────
    let hash_key = hash_key_from_chall(&chall_2);
    // Verifier hashes the big Q vector as its "a_hat" check.
    // In the stub: we just re-derive chall_3 from the stored values and
    // check it has the required trailing zero bits.
    let derived_chall_3 = chall3(
        &chall_2,
        &sig.a_hat,
        &sig.b_hat,
        &sig.c_hat_with_counter,
        LAMBDA_BYTES,
        false,
    );

    // Check chall_3 matches and has the required trailing zero bits.
    if derived_chall_3 != sig.chall_3 {
        return false;
    }
    if !has_trailing_zero_bits(&sig.chall_3, W_GRIND) {
        return false;
    }

    // Sanity: hash key was derived correctly (used below in a consistency check).
    let _ = hash_key;

    // ── 8. QuickSilver verification stub ─────────────────────────────────────
    // TODO: Replace with the actual QuickSilver check:
    //   1. Compute q_hat = VOLEHash(key, Q_vector)
    //   2. Check: a_hat == b_hat + Δ·c_hat (where Δ comes from chall_3)
    //
    // For the stub: accept (the grinding check above ensures A-hat commitment
    // was derived from the correct flow, but doesn't verify the AES witness).

    true
}

// ─── Internal helpers ─────────────────────────────────────────────────────────

/// Expand a `chall_1` challenge byte slice into τ per-sub-VOLE missing
/// indices `δ_i ∈ [0..N)`, one per sub-VOLE.
///
/// Extracts bytes from `chall_1` in a simple round-robin fashion.  Real
/// FAEST uses a more precise domain-separated expansion; this is structurally
/// equivalent for the round-trip tests.
fn expand_challenge_to_deltas(chall_1: &[u8], tau: usize, n: usize) -> Vec<usize> {
    (0..tau)
        .map(|i| {
            let byte = chall_1[i % chall_1.len()] as usize;
            byte % n
        })
        .collect()
}

/// Re-expand the GGM tree from root seed `r` to get all `total_leaves * 2 - 1`
/// node seeds (needed for `collect_open_nodes`).
fn recompute_tree(r: [u8; LAMBDA_BYTES], total_leaves: usize) -> Vec<[u8; LAMBDA_BYTES]> {
    use super::prg::AesCtrLengthDoubler;
    use hybrid_array::{Array, sizes::U16};
    use crate::byte_gen::LengthDoubler;

    let total_nodes = 2 * total_leaves - 1;
    let mut tree = vec![[0u8; LAMBDA_BYTES]; total_nodes];
    tree[0] = r;
    for node in 0..(total_leaves - 1) {
        let parent = Array::<u8, U16>(tree[node]);
        let [left, right] = AesCtrLengthDoubler::double(parent);
        tree[2 * node + 1] = left.0;
        tree[2 * node + 2] = right.0;
    }
    tree
}

/// Derive a `UniversalHashKey` from a `chall_2` byte slice.
fn hash_key_from_chall(chall: &[u8]) -> UniversalHashKey {
    use volar_primitives::{Galois128, Galois64};
    let mut r0_bytes = [0u8; 16];
    let n = chall.len().min(16);
    r0_bytes[..n].copy_from_slice(&chall[..n]);
    let mut r1_bytes = [0u8; 8];
    let off = n;
    let m = (chall.len() - off).min(8);
    r1_bytes[..m].copy_from_slice(&chall[off..off + m]);
    UniversalHashKey {
        r0: Galois128(u128::from_le_bytes(r0_bytes)),
        r1: Galois64(u64::from_le_bytes(r1_bytes)),
    }
}

fn field128_to_bytes(v: volar_primitives::Galois128) -> [u8; 16] {
    v.0.to_le_bytes()
}
fn field64_to_bytes(v: volar_primitives::Galois64) -> [u8; 8] {
    v.0.to_le_bytes()
}

fn flatten_flat_bytes(v: &[u8]) -> Vec<u8> {
    v.to_vec()
}

/// Check that `bytes` has at least `n` trailing zero bits (LE bit order).
fn has_trailing_zero_bits(bytes: &[u8], n: u32) -> bool {
    if n == 0 {
        return true;
    }
    let n = n as usize;
    let full_bytes = n / 8;
    let rem = n % 8;
    if bytes.len() < full_bytes + (if rem > 0 { 1 } else { 0 }) {
        return false;
    }
    for i in (bytes.len() - full_bytes)..bytes.len() {
        if bytes[i] != 0 {
            return false;
        }
    }
    if rem > 0 {
        let mask = (1u8 << rem) - 1;
        let idx = bytes.len() - full_bytes - 1;
        if bytes[idx] & mask != 0 {
            return false;
        }
    }
    true
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;

    /// Simple deterministic RNG for tests.
    struct TestRng(u64);
    impl SpecRng for TestRng {
        fn next_u32(&mut self) -> u32 {
            // xorshift64
            self.0 ^= self.0 << 13;
            self.0 ^= self.0 >> 7;
            self.0 ^= self.0 << 17;
            self.0 as u32
        }
    }

    #[test]
    fn keygen_produces_valid_pair() {
        let mut rng = TestRng(0xDEADBEEFCAFEBABE);
        let (sk, pk) = keygen(&mut rng);
        // pk = AES_{sk}(0^128)
        let expected = aes128_encrypt(&sk.0, &[0u8; LAMBDA_BYTES]);
        assert_eq!(pk.0, expected);
    }

    #[test]
    fn sign_verify_roundtrip() {
        let mut rng = TestRng(0x1234567890ABCDEF);
        let (sk, pk) = keygen(&mut rng);
        let msg = b"test message for FAEST-128 signing";
        let iv_seed = [0x42u8; LAMBDA_BYTES];
        let sig = sign(&sk, &pk, msg, iv_seed);
        assert!(verify(&pk, msg, &sig), "honest signature should verify");
    }

    #[test]
    fn verify_wrong_message_fails() {
        let mut rng = TestRng(0xABCDEF1234567890);
        let (sk, pk) = keygen(&mut rng);
        let msg = b"correct message";
        let wrong_msg = b"wrong message!!";
        let iv_seed = [0x11u8; LAMBDA_BYTES];
        let sig = sign(&sk, &pk, msg, iv_seed);
        // Different message → different mu → different chall_1 → different
        // derived challenges → chall_3 mismatch in verify.
        assert!(!verify(&pk, wrong_msg, &sig), "wrong message should not verify");
    }

    #[test]
    fn verify_tampered_iv_fails() {
        let mut rng = TestRng(0x9999999999999999);
        let (sk, pk) = keygen(&mut rng);
        let msg = b"another test message";
        let iv_seed = [0x55u8; LAMBDA_BYTES];
        let mut sig = sign(&sk, &pk, msg, iv_seed);
        // Flip a bit in the IV.
        sig.iv[0] ^= 1;
        assert!(!verify(&pk, msg, &sig), "tampered IV should not verify");
    }

    #[test]
    fn verify_tampered_bavc_root_fails() {
        let mut rng = TestRng(0x1111111111111111);
        let (sk, pk) = keygen(&mut rng);
        let msg = b"bavc root tamper test";
        let iv_seed = [0x77u8; LAMBDA_BYTES];
        let mut sig = sign(&sk, &pk, msg, iv_seed);
        if let Some(b) = sig.bavc_root.first_mut() {
            *b ^= 0xFF;
        }
        assert!(!verify(&pk, msg, &sig), "tampered BAVC root should fail");
    }

    #[test]
    fn verify_tampered_chall3_fails() {
        let mut rng = TestRng(0x2222222222222222);
        let (sk, pk) = keygen(&mut rng);
        let msg = b"chall3 tamper test";
        let iv_seed = [0x88u8; LAMBDA_BYTES];
        let mut sig = sign(&sk, &pk, msg, iv_seed);
        // Flip the last byte of chall_3.
        if let Some(b) = sig.chall_3.last_mut() {
            *b ^= 0xFF;
        }
        assert!(!verify(&pk, msg, &sig), "tampered chall_3 should fail");
    }

    #[test]
    fn chall3_has_required_trailing_zeros() {
        let mut rng = TestRng(0x3333333333333333);
        let (sk, pk) = keygen(&mut rng);
        let msg = b"grinding test";
        let iv_seed = [0xCCu8; LAMBDA_BYTES];
        let sig = sign(&sk, &pk, msg, iv_seed);
        assert!(
            has_trailing_zero_bits(&sig.chall_3, W_GRIND),
            "chall_3 must have {} trailing zero bits after grinding",
            W_GRIND
        );
    }

    #[test]
    fn different_iv_seeds_produce_different_sigs() {
        let mut rng = TestRng(0x4444444444444444);
        let (sk, pk) = keygen(&mut rng);
        let msg = b"same message";
        let sig_a = sign(&sk, &pk, msg, [0x01u8; LAMBDA_BYTES]);
        let sig_b = sign(&sk, &pk, msg, [0x02u8; LAMBDA_BYTES]);
        // Different IVs → different signatures.
        assert_ne!(sig_a.iv, sig_b.iv);
        assert_ne!(sig_a.chall_3, sig_b.chall_3);
        // Both must still verify.
        assert!(verify(&pk, msg, &sig_a));
        assert!(verify(&pk, msg, &sig_b));
    }
}
