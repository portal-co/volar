//! TLS 1.3 (RFC 8446) key-schedule composition over the SHA-256/HMAC
//! boolar gadgets — the P4c minimal-client milestone.
//!
//! The handshake key schedule is entirely HKDF-Extract (= HMAC) and
//! HKDF-Expand-Label (= HMAC over a compile-time label prefix, the
//! transcript-hash context, and `0x01`), so **no new circuit construction
//! is needed**: every stage is a [`crate::sha_gadget::build_hmac_sha256`]
//! or [`crate::sha_gadget::build_sha256`] circuit with driver-assembled
//! inputs. This module pins the byte layouts (RFC 8446 §7.1 `HkdfLabel`)
//! and the stage geometry so the site integration and the tests share one
//! spelling.
//!
//! Ownership in the MPC client: the shared secret (X25519 output) and
//! every derived secret are threaded garbled values; the transcript
//! context is computed in-circuit over the (plaintext, public) hello
//! messages; the label prefix and trailing `0x01` are public constants.
//! In the two-party driver the HMAC `msg` input is therefore a Feed mix —
//! public prefix, threaded transcript bits, public `0x01` — exactly the
//! `oram_2pc` Feed shape.

use alloc::vec::Vec;

use volar_ir::boolar::BIrBlocks;

use crate::sha_gadget::{build_hmac_sha256, build_sha256};

/// RFC 8446 §7.1 `HkdfLabel` encoding minus the context:
/// `uint16 out_len || u8 label_len || "tls13 " || label || u8 ctx_len`.
/// The context bytes themselves are appended by the driver (they may be
/// threaded in-circuit transcript bits), followed by the HKDF `0x01`.
pub fn hkdf_label_prefix(out_len: u16, label: &[u8], ctx_len: usize) -> Vec<u8> {
    assert!(6 + label.len() <= 255, "HkdfLabel label too long");
    assert!(ctx_len <= 255, "HkdfLabel context too long");
    let mut v = Vec::with_capacity(3 + 6 + label.len() + 1);
    v.extend_from_slice(&out_len.to_be_bytes());
    v.push((6 + label.len()) as u8);
    v.extend_from_slice(b"tls13 ");
    v.extend_from_slice(label);
    v.push(ctx_len as u8);
    v
}

/// HKDF-Expand-Label(Secret, Label, Context) for `out_len <= 32` as a
/// circuit: `HMAC(Secret, prefix || Context || 0x01)` (a single HKDF-Expand
/// block; outputs over 32 bytes are unused by TLS 1.3 with SHA-256).
///
/// Returns `(circuit, prefix)`: the circuit is
/// `build_hmac_sha256(32, prefix.len() + ctx_len + 1)` with inputs
/// `[secret: 256 bits][msg: (prefix.len() + ctx_len + 1) bytes]`, where the
/// driver feeds `prefix || context || [0x01]` as the msg segment. The
/// output is the full 32-byte HMAC; the caller truncates to `out_len`.
pub fn expand_label_circuit(
    out_len: u16,
    label: &[u8],
    ctx_len: usize,
) -> (BIrBlocks<()>, Vec<u8>) {
    assert!(out_len as usize <= 32, "single-block HKDF-Expand only");
    let prefix = hkdf_label_prefix(out_len, label, ctx_len);
    let c = build_hmac_sha256(32, prefix.len() + ctx_len + 1);
    (c, prefix)
}

/// HKDF-Extract(salt, ikm) = HMAC(salt, ikm) — the generic geometry; the
/// driver supplies both segments (either may be threaded secret bits).
pub fn extract_circuit(salt_bytes: usize, ikm_bytes: usize) -> BIrBlocks<()> {
    build_hmac_sha256(salt_bytes, ikm_bytes)
}

/// The transcript hash over handshake messages: plain SHA-256.
pub fn transcript_circuit(msg_bytes: usize) -> BIrBlocks<()> {
    build_sha256(msg_bytes)
}

// -------------------------------------------------------------- records

/// TLS 1.3 §5.2 per-record nonce: the 12-byte static iv XORed with the
/// (big-endian, right-aligned) sequence number. Free-XOR in-circuit (the
/// iv is a threaded secret, the sequence number public) — this helper is
/// the driver-side byte form.
pub fn nonce_xor(iv: &[u8; 12], seq: u64) -> [u8; 12] {
    let mut n = *iv;
    let s = seq.to_be_bytes();
    for i in 0..8 {
        n[4 + i] ^= s[i];
    }
    n
}

/// The record-SEAL circuit for a TLS 1.3 record of `pt_bytes` inner
/// plaintext bytes with the fixed 5-byte record-header AAD:
/// [`crate::aes_gadget::build_aes128_gcm_var(5, pt_bytes)`], params
/// `[key: 128, nonce: 96, aad: 40, pt: 8*pt_bytes]`, outputs
/// `[ct: 8*pt_bytes, tag: 128]`.
pub fn record_seal_circuit(pt_bytes: usize) -> BIrBlocks<()> {
    crate::aes_gadget::build_aes128_gcm_var(5, pt_bytes)
}

/// The record-OPEN circuit for a TLS 1.3 record of `ct_bytes` ciphertext
/// bytes: [`crate::aes_gadget::build_aes128_gcm_decrypt_var(5, ct_bytes)`]
/// (GHASH covers the received ciphertext, not the recovered plaintext),
/// params `[key: 128, nonce: 96, aad: 40, ct: 8*ct_bytes]`, outputs
/// `[pt: 8*ct_bytes, recomputed_tag: 128]` — the tag check compares the
/// recomputed tag against the received tag (native in the concrete
/// driver; an in-circuit compare with a verdict reveal in the MPC).
pub fn record_open_circuit(ct_bytes: usize) -> BIrBlocks<()> {
    crate::aes_gadget::build_aes128_gcm_decrypt_var(5, ct_bytes)
}
