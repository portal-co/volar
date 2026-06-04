// @reliability: experimental
// @ai: assisted
//! Keccak-f[1600] + SHA3-256, used as the **dual-preimage boundary digest** of
//! the VCB-IVC bridge: a public `d = Keccak(S)` that both proof systems prove a
//! preimage of (the boundary bit-string is field-agnostic, so this sidesteps the
//! `GF(2^k) ↔ F_ℓ` cross-field wall). See [`crate::link`] and
//! `docs/boundary-link-embedding.md`.
//!
//! This module is the **reference** (lane-based `u64`) implementation — the
//! oracle the in-R1CS arithmetization ([`r1cs`-side, below]) and the VOLE-side
//! boolean gadget (`volar-weaver`) are cross-checked against. It is anchored to
//! the `sha3` crate in tests.

use alloc::vec::Vec;

// Round constants / rotation offsets / π lane permutation. `pub(crate)` so the
// in-R1CS arithmetization ([`crate::keccak_r1cs`]) reuses the *same* tables — one
// source of truth keeps the reference and the circuit from drifting apart.
pub(crate) const RNDC: [u64; 24] = [
    0x0000_0000_0000_0001, 0x0000_0000_0000_8082, 0x8000_0000_0000_808a, 0x8000_0000_8000_8000,
    0x0000_0000_0000_808b, 0x0000_0000_8000_0001, 0x8000_0000_8000_8081, 0x8000_0000_0000_8009,
    0x0000_0000_0000_008a, 0x0000_0000_0000_0088, 0x0000_0000_8000_8009, 0x0000_0000_8000_000a,
    0x0000_0000_8000_808b, 0x8000_0000_0000_008b, 0x8000_0000_0000_8089, 0x8000_0000_0000_8003,
    0x8000_0000_0000_8002, 0x8000_0000_0000_0080, 0x0000_0000_0000_800a, 0x8000_0000_8000_000a,
    0x8000_0000_8000_8081, 0x8000_0000_0000_8080, 0x0000_0000_8000_0001, 0x8000_0000_8000_8008,
];
pub(crate) const ROTC: [u32; 24] =
    [1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 2, 14, 27, 41, 56, 8, 25, 43, 62, 18, 39, 61, 20, 44];
pub(crate) const PILN: [usize; 24] =
    [10, 7, 11, 17, 18, 3, 5, 16, 8, 21, 24, 4, 15, 23, 19, 13, 12, 2, 20, 14, 22, 9, 6, 1];

/// SHA3-256 rate in bytes (capacity 512 bits).
pub const RATE_BYTES: usize = 136;

/// The Keccak-f[1600] permutation on 25 little-endian lanes.
pub fn keccakf(st: &mut [u64; 25]) {
    for round in 0..24 {
        // θ
        let mut bc = [0u64; 5];
        for i in 0..5 {
            bc[i] = st[i] ^ st[i + 5] ^ st[i + 10] ^ st[i + 15] ^ st[i + 20];
        }
        for i in 0..5 {
            let t = bc[(i + 4) % 5] ^ bc[(i + 1) % 5].rotate_left(1);
            let mut j = 0;
            while j < 25 {
                st[j + i] ^= t;
                j += 5;
            }
        }
        // ρ + π
        let mut t = st[1];
        for i in 0..24 {
            let j = PILN[i];
            let tmp = st[j];
            st[j] = t.rotate_left(ROTC[i]);
            t = tmp;
        }
        // χ
        let mut j = 0;
        while j < 25 {
            let mut row = [0u64; 5];
            for i in 0..5 {
                row[i] = st[j + i];
            }
            for i in 0..5 {
                st[j + i] ^= (!row[(i + 1) % 5]) & row[(i + 2) % 5];
            }
            j += 5;
        }
        // ι
        st[0] ^= RNDC[round];
    }
}

/// SHA3-256 of a byte string (single- or multi-block sponge, `0x06` domain pad).
pub fn sha3_256(input: &[u8]) -> [u8; 32] {
    let mut st = [0u64; 25];
    let mut state_bytes = [0u8; 200];

    let mut offset = 0;
    // Absorb full rate blocks.
    while input.len() - offset >= RATE_BYTES {
        for i in 0..RATE_BYTES {
            state_bytes[i] ^= input[offset + i];
        }
        absorb_permute(&mut st, &mut state_bytes);
        offset += RATE_BYTES;
    }
    // Final block with padding.
    let rem = input.len() - offset;
    let mut block = [0u8; RATE_BYTES];
    block[..rem].copy_from_slice(&input[offset..]);
    block[rem] ^= 0x06; // SHA3 domain separation
    block[RATE_BYTES - 1] ^= 0x80; // pad10*1 final bit
    for i in 0..RATE_BYTES {
        state_bytes[i] ^= block[i];
    }
    // Run the permutation on the absorbed state.
    bytes_to_lanes(&state_bytes, &mut st);
    keccakf(&mut st);
    lanes_to_bytes(&st, &mut state_bytes);

    let mut out = [0u8; 32];
    out.copy_from_slice(&state_bytes[..32]);
    out
}

fn absorb_permute(st: &mut [u64; 25], state_bytes: &mut [u8; 200]) {
    bytes_to_lanes(state_bytes, st);
    keccakf(st);
    lanes_to_bytes(st, state_bytes);
}

fn bytes_to_lanes(b: &[u8; 200], st: &mut [u64; 25]) {
    for i in 0..25 {
        let mut chunk = [0u8; 8];
        chunk.copy_from_slice(&b[i * 8..i * 8 + 8]);
        st[i] = u64::from_le_bytes(chunk);
    }
}

fn lanes_to_bytes(st: &[u64; 25], b: &mut [u8; 200]) {
    for i in 0..25 {
        b[i * 8..i * 8 + 8].copy_from_slice(&st[i].to_le_bytes());
    }
}

/// SHA3-256 over a **bit-vector** input (LSB-first byte packing), returning the
/// 256 digest bits (LSB-first).  This is the function both the folding R1CS and
/// the VOLE boolean gadget reproduce.
pub fn keccak256_bits(input: &[bool]) -> [bool; 256] {
    let nbytes = input.len().div_ceil(8);
    let mut bytes = alloc::vec![0u8; nbytes];
    for (i, &bit) in input.iter().enumerate() {
        if bit {
            bytes[i / 8] |= 1 << (i % 8);
        }
    }
    let digest = sha3_256(&bytes);
    let mut out = [false; 256];
    for i in 0..256 {
        out[i] = (digest[i / 8] >> (i % 8)) & 1 == 1;
    }
    out
}

/// Pack 256 digest bits (LSB-first) back into 32 bytes.
pub fn digest_bits_to_bytes(bits: &[bool; 256]) -> [u8; 32] {
    let mut out = [0u8; 32];
    for i in 0..256 {
        if bits[i] {
            out[i / 8] |= 1 << (i % 8);
        }
    }
    out
}

/// Convenience: digest bits of a `&[bool]` as a `Vec<bool>` (for callers that
/// want owned output).
pub fn keccak256_bits_vec(input: &[bool]) -> Vec<bool> {
    keccak256_bits(input).to_vec()
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use digest::Digest;

    fn sha3_ref(input: &[u8]) -> [u8; 32] {
        let mut h = sha3::Sha3_256::new();
        h.update(input);
        let o = h.finalize();
        let mut out = [0u8; 32];
        out.copy_from_slice(&o);
        out
    }

    #[test]
    fn sha3_256_matches_crate_empty() {
        assert_eq!(sha3_256(&[]), sha3_ref(&[]));
    }

    #[test]
    fn sha3_256_matches_crate_various() {
        for len in [1usize, 7, 8, 33, 135, 136, 137, 200, 300] {
            let msg: std::vec::Vec<u8> = (0..len).map(|i| (i * 31 + 7) as u8).collect();
            assert_eq!(sha3_256(&msg), sha3_ref(&msg), "mismatch at len {len}");
        }
    }

    #[test]
    fn bit_interface_matches_byte_interface() {
        // Byte-aligned bit input must agree with the byte hash.
        let msg = [0xa5u8, 0x5a, 0xffu8, 0x00, 0x13];
        let bits: std::vec::Vec<bool> =
            (0..msg.len() * 8).map(|i| (msg[i / 8] >> (i % 8)) & 1 == 1).collect();
        let from_bits = digest_bits_to_bytes(&keccak256_bits(&bits));
        assert_eq!(from_bits, sha3_256(&msg));
        assert_eq!(from_bits, sha3_ref(&msg));
    }
}
