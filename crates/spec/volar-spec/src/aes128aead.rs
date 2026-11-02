// @reliability: normal
//! @ai: assisted
//! Deterministic AEAD over the FAEST AES-128 (`faest::aes`), mirroring
//! `ascon.rs`'s role for Ascon-128a: the single-party obfuscation pipeline
//! (obfustopia's MPC/ORAM plan) uses this as the `CipherProfile::Aes128`
//! blob/tree encryption profile, conformance-aligned with the `oram_2pc`
//! per-node-pad baseline but upgraded to authenticated encryption.
//!
//! Construction (encrypt-then-MAC with key separation):
//!
//! - `K_e = AES-128_K(0^128)`, `K_m = AES-128_K(0^127 ‖ 1)` — the CTR and
//!   MAC keys are block-cipher derivatives of the embedded key, so a single
//!   128-bit key seeds both roles.
//! - Encryption: CTR mode with the full 128-bit big-endian counter
//!   initialized to the nonce: `ct_i = pt_i ⊕ AES_{K_e}(nonce + i)`.
//! - Authentication: CMAC (NIST SP 800-38B) under `K_m` over
//!   `nonce ‖ ct ‖ be64(bit_len)`, standard `K1`/`K2` subkey doubling.
//!
//! Plaintexts are byte strings; bit-granular blobs are the Ascon profile's
//! domain (AES is byte-oriented).

use alloc::vec::Vec;

use crate::faest::aes::{BLOCK, NK_ROUND_KEYS, encrypt_block_with_keys, key_expansion};

/// Byte length of an AES block / nonce / key / tag.
const BLOCK_LEN: usize = BLOCK;

/// The two role-separated round-key schedules derived from one key.
struct RoleKeys {
    enc: [[u8; BLOCK_LEN]; NK_ROUND_KEYS],
    mac: [[u8; BLOCK_LEN]; NK_ROUND_KEYS],
}

/// Derive `K_e = AES_K(0^128)` and `K_m = AES_K(0^127‖1)` and expand both.
fn derive_roles(key: &[u8; BLOCK_LEN]) -> RoleKeys {
    let zero = [0u8; BLOCK_LEN];
    let mut one = [0u8; BLOCK_LEN];
    one[BLOCK_LEN - 1] = 1;
    let enc_key = encrypt_block_with_keys(&key_expansion(key), &zero);
    let mac_key = encrypt_block_with_keys(&key_expansion(key), &one);
    RoleKeys {
        enc: key_expansion(&enc_key),
        mac: key_expansion(&mac_key),
    }
}

/// GF(2^128) doubling per SP 800-38B §5.3 (`Rb = 0x87`), big-endian.
fn dbl(block: &[u8; BLOCK_LEN]) -> [u8; BLOCK_LEN] {
    let mut out = [0u8; BLOCK_LEN];
    let mut carry = 0u8;
    for i in (0..BLOCK_LEN).rev() {
        let next_carry = block[i] >> 7;
        out[i] = (block[i] << 1) | carry;
        carry = next_carry;
    }
    if carry == 1 {
        out[BLOCK_LEN - 1] ^= 0x87;
    }
    out
}

/// CMAC (SP 800-38B) over `msg` with precomputed round keys.
fn cmac(mac_keys: &[[u8; BLOCK_LEN]; NK_ROUND_KEYS], msg: &[u8]) -> [u8; BLOCK_LEN] {
    let l = encrypt_block_with_keys(mac_keys, &[0u8; BLOCK_LEN]);
    let k1 = dbl(&l);
    let k2 = dbl(&k1);
    let complete = !msg.is_empty() && msg.len() % BLOCK_LEN == 0;
    let n_blocks = if complete {
        msg.len() / BLOCK_LEN
    } else {
        msg.len() / BLOCK_LEN + 1
    };
    let mut state = [0u8; BLOCK_LEN];
    for block_idx in 0..n_blocks {
        let is_last = block_idx + 1 == n_blocks;
        let mut block = [0u8; BLOCK_LEN];
        if is_last {
            if complete {
                block.copy_from_slice(&msg[block_idx * BLOCK_LEN..(block_idx + 1) * BLOCK_LEN]);
                for i in 0..BLOCK_LEN {
                    block[i] ^= k1[i];
                }
            } else {
                let rest = msg.len() - block_idx * BLOCK_LEN;
                block[..rest].copy_from_slice(&msg[block_idx * BLOCK_LEN..]);
                block[rest] = 0x80;
                for i in 0..BLOCK_LEN {
                    block[i] ^= k2[i];
                }
            }
        } else {
            block.copy_from_slice(&msg[block_idx * BLOCK_LEN..(block_idx + 1) * BLOCK_LEN]);
        }
        for i in 0..BLOCK_LEN {
            state[i] ^= block[i];
        }
        state = encrypt_block_with_keys(mac_keys, &state);
    }
    state
}

/// CTR keystream block: `AES_{K_e}(nonce + i)` with the full 128-bit
/// big-endian counter.
fn ctr_block(enc_keys: &[[u8; BLOCK_LEN]; NK_ROUND_KEYS], nonce: &[u8; BLOCK_LEN], i: u64) -> [u8; BLOCK_LEN] {
    let counter = u128::from_be_bytes(*nonce).wrapping_add(i as u128);
    encrypt_block_with_keys(enc_keys, &counter.to_be_bytes())
}

/// Seal `pt` under `key`/`nonce`: returns `(ct, tag)`.
pub fn aes128_etm_seal(key: &[u8; BLOCK_LEN], nonce: &[u8; BLOCK_LEN], pt: &[u8]) -> (Vec<u8>, [u8; BLOCK_LEN]) {
    let roles = derive_roles(key);
    let mut ct = Vec::with_capacity(pt.len());
    for (i, chunk) in pt.chunks(BLOCK_LEN).enumerate() {
        let ks = ctr_block(&roles.enc, nonce, i as u64);
        ct.extend(chunk.iter().zip(ks.iter()).map(|(p, k)| p ^ k));
    }
    let mut msg = Vec::with_capacity(BLOCK_LEN + ct.len() + 8);
    msg.extend_from_slice(nonce);
    msg.extend_from_slice(&ct);
    msg.extend_from_slice(&((pt.len() as u64) * 8).to_be_bytes());
    let tag = cmac(&roles.mac, &msg);
    (ct, tag)
}

/// Open a sealed blob: `Some(pt)` on tag match, `None` on mismatch.
pub fn aes128_etm_open(
    key: &[u8; BLOCK_LEN],
    nonce: &[u8; BLOCK_LEN],
    ct: &[u8],
    tag: &[u8; BLOCK_LEN],
) -> Option<Vec<u8>> {
    let roles = derive_roles(key);
    let mut msg = Vec::with_capacity(BLOCK_LEN + ct.len() + 8);
    msg.extend_from_slice(nonce);
    msg.extend_from_slice(ct);
    msg.extend_from_slice(&((ct.len() as u64) * 8).to_be_bytes());
    let computed = cmac(&roles.mac, &msg);
    if computed != *tag {
        return None;
    }
    let mut pt = Vec::with_capacity(ct.len());
    for (i, chunk) in ct.chunks(BLOCK_LEN).enumerate() {
        let ks = ctr_block(&roles.enc, nonce, i as u64);
        pt.extend(chunk.iter().zip(ks.iter()).map(|(c, k)| c ^ k));
    }
    Some(pt)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::faest::aes::key_expansion;

    fn hex_key() -> [u8; 16] {
        [
            0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf,
            0x4f, 0x3c,
        ]
    }

    /// RFC 4493 §4 example 1: CMAC of the empty message.
    #[test]
    fn cmac_rfc4493_empty() {
        let tag = cmac(&key_expansion(&hex_key()), &[]);
        assert_eq!(
            tag,
            [
                0xbb, 0x1d, 0x69, 0x29, 0xe9, 0x59, 0x37, 0x28, 0x7f, 0xa3, 0x7d, 0x12, 0x9b,
                0x75, 0x67, 0x46
            ]
        );
    }

    /// RFC 4493 §4 example 2: CMAC of one complete block.
    #[test]
    fn cmac_rfc4493_one_block() {
        let msg = [
            0x6b, 0xc1, 0xbe, 0xe2, 0x2e, 0x40, 0x9f, 0x96, 0xe9, 0x3d, 0x7e, 0x11, 0x73, 0x93,
            0x17, 0x2a,
        ];
        let tag = cmac(&key_expansion(&hex_key()), &msg);
        assert_eq!(
            tag,
            [
                0x07, 0x0a, 0x16, 0xb4, 0x6b, 0x4d, 0x41, 0x44, 0xf7, 0x9b, 0xdd, 0x9d, 0xd0,
                0x4a, 0x28, 0x7c
            ]
        );
    }

    /// RFC 4493 §4 example 3: CMAC of 40 bytes (partial last block, K2 path).
    #[test]
    fn cmac_rfc4493_partial_last_block() {
        let mut msg = alloc::vec::Vec::new();
        msg.extend_from_slice(&[
            0x6b, 0xc1, 0xbe, 0xe2, 0x2e, 0x40, 0x9f, 0x96, 0xe9, 0x3d, 0x7e, 0x11, 0x73, 0x93,
            0x17, 0x2a, 0xae, 0x2d, 0x8a, 0x57, 0x1e, 0x03, 0xac, 0x9c, 0x9e, 0xb7, 0x6f, 0xac,
            0x45, 0xaf, 0x8e, 0x51, 0x30, 0xc8, 0x1c, 0x46, 0xa3, 0x5c, 0xe4, 0x11,
        ]);
        let tag = cmac(&key_expansion(&hex_key()), &msg);
        assert_eq!(
            tag,
            [
                0xdf, 0xa6, 0x67, 0x47, 0xde, 0x9a, 0xe6, 0x30, 0x30, 0xca, 0x32, 0x61, 0x14,
                0x97, 0xc8, 0x27
            ]
        );
        msg.clear();
    }

    /// NIST SP 800-38A F.5.1: CTR-AES128 first keystream application.
    #[test]
    fn ctr_sp800_38a_first_block() {
        let nonce = [
            0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd,
            0xfe, 0xff,
        ];
        let roles = derive_roles(&hex_key());
        // The KAT uses the raw key as the CTR key; emulate by calling
        // ctr_block with key_expansion(raw) directly.
        let ks = ctr_block(&key_expansion(&hex_key()), &nonce, 0);
        let pt = [
            0x6b, 0xc1, 0xbe, 0xe2, 0x2e, 0x40, 0x9f, 0x96, 0xe9, 0x3d, 0x7e, 0x11, 0x73, 0x93,
            0x17, 0x2a,
        ];
        let ct: alloc::vec::Vec<u8> = pt.iter().zip(ks.iter()).map(|(p, k)| p ^ k).collect();
        assert_eq!(
            ct,
            alloc::vec![
                0x87, 0x4d, 0x61, 0x91, 0xb6, 0x20, 0xe3, 0x26, 0x1b, 0xef, 0x68, 0x64, 0x99,
                0x0d, 0xb6, 0xce
            ]
        );
        let _ = roles;
    }

    #[test]
    fn seal_open_round_trips_and_rejects_tampering() {
        let key = [0x42u8; 16];
        for pt_len in [0usize, 1, 8, 16, 17, 31, 32, 48, 77, 128] {
            let pt: Vec<u8> = (0..pt_len).map(|i| (i * 7 + 3) as u8).collect();
            let nonce = [pt_len as u8; 16];
            let (ct, tag) = aes128_etm_seal(&key, &nonce, &pt);
            let opened = aes128_etm_open(&key, &nonce, &ct, &tag)
                .expect("honest seal must open");
            assert_eq!(opened, pt, "round trip at {pt_len} bytes");
            // Tamper: flip a ct bit (or a tag bit for empty plaintext).
            if !ct.is_empty() {
                let mut bad = ct.clone();
                bad[0] ^= 1;
                assert!(aes128_etm_open(&key, &nonce, &bad, &tag).is_none());
            }
            let mut bad_tag = tag;
            bad_tag[15] ^= 1;
            assert!(aes128_etm_open(&key, &nonce, &ct, &bad_tag).is_none());
        }
    }
}
