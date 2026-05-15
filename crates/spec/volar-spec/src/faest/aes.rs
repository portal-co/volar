// @reliability: experimental
//! @ai: assisted
//! AES-128 round function — FAEST OWF for the λ=128 parameter set.
//!
//! Spec source: NIST FIPS 197.
//!
//! This is the **total-Rust** subset: every operation is a fixed-length loop
//! over `[u8; 16]` state with no panicking paths, so the same source compiles
//! to host code (used by [`tests`] for KAT verification against FIPS 197
//! Appendix C.1) and lowers to bit-level circuit form via the existing
//! `volar-ir-passes` boolarisation (used by the FAEST QuickSilver weaver in
//! M4, not yet built).
//!
//! ## Why duplicate AES?
//!
//! Existing block-cipher crates use `unsafe`, dynamic dispatch, or
//! `core::arch::x86_64` intrinsics that the Volar circuit lowering can't
//! see through. The spec needs an AES that is structurally a pure
//! [`u8; 16]`→`[u8; 16`] transformation built out of byte XOR and the
//! `gf_mul_u8` carry-less multiplier from [`crate::field`]. We get one
//! by writing it ourselves.
//!
//! ## What's here
//!
//! - [`SBOX`] / [`RCON`] — the AES S-box and round constants.
//! - [`sub_bytes`] / [`shift_rows`] / [`mix_columns`] / [`add_round_key`]
//!   — the four round-function components.
//! - [`key_expansion`] — derive the 11 round keys for AES-128.
//! - [`encrypt_block`] — single-block AES-128 encryption, the FAEST OWF.
//!
//! ## What's deliberately absent
//!
//! - AES-192 and AES-256. Future parameter sets only. FAEST-128 uses
//!   AES-128.
//! - Decryption. The FAEST OWF only needs forward encryption.
//! - CTR mode. The FAEST PRG (M2 dependency) wraps AES-CTR using
//!   [`encrypt_block`] as the inner permutation — that lives in
//!   [`crate::faest::bavc`] when it gets written.

/// AES S-box (FIPS 197 §5.1.1 Figure 7).
pub const SBOX: [u8; 256] = [
    0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76,
    0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0,
    0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15,
    0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75,
    0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84,
    0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf,
    0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8,
    0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2,
    0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73,
    0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb,
    0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79,
    0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08,
    0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a,
    0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e,
    0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf,
    0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16,
];

/// AES round constants (FIPS 197 §5.2). `RCON[i] = x^i in GF(2^8)`, low
/// byte stored. Index 0 is unused (key schedule starts at round 1).
pub const RCON: [u8; 11] = [0x00, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36];

/// AES-128 number of rounds.
pub const NR: usize = 10;

/// AES-128 block (and key) size in bytes.
pub const BLOCK: usize = 16;

/// Number of round keys produced by [`key_expansion`] (one per round + the
/// initial whitening key).
pub const NK_ROUND_KEYS: usize = NR + 1;

/// GF(2^8) reduction polynomial used by AES: x^8 + x^4 + x^3 + x + 1
/// (low byte 0x1b).
const GF8_AES_POLY: u8 = 0x1b;

/// `a · b` in GF(2^8) under the AES reduction polynomial. Inlined here
/// rather than calling [`crate::field::gf_mul_u8`] so callers reading the
/// spec see the full primitive at a glance; the bodies are identical.
#[inline]
fn gf_mul(a: u8, b: u8) -> u8 {
    volar_primitives::gf_mul_u8(a, b, GF8_AES_POLY)
}

/// SubBytes — apply the S-box to every byte of the state.
pub fn sub_bytes(state: &mut [u8; BLOCK]) {
    for i in 0..BLOCK {
        state[i] = SBOX[state[i] as usize];
    }
}

/// ShiftRows — cyclic byte shift per row of the column-major state.
///
/// The AES state is conceptually a 4×4 byte matrix laid out column-major:
/// `state[r + 4*c]` is the byte at row `r`, column `c`. Row `r` shifts
/// left by `r` bytes (wrap).
pub fn shift_rows(state: &mut [u8; BLOCK]) {
    // Row 1: shift left by 1.
    let t = state[1];
    state[1] = state[5];
    state[5] = state[9];
    state[9] = state[13];
    state[13] = t;

    // Row 2: shift left by 2 (swap pairs).
    let t = state[2];
    state[2] = state[10];
    state[10] = t;
    let t = state[6];
    state[6] = state[14];
    state[14] = t;

    // Row 3: shift left by 3 (= shift right by 1).
    let t = state[15];
    state[15] = state[11];
    state[11] = state[7];
    state[7] = state[3];
    state[3] = t;
}

/// MixColumns — left-multiply each column by the AES MDS matrix in GF(2^8).
///
/// The MDS matrix is `[[2,3,1,1],[1,2,3,1],[1,1,2,3],[3,1,1,2]]` over
/// GF(2^8). For one column `(s0, s1, s2, s3)` the output is
/// `(2·s0 ⊕ 3·s1 ⊕ s2 ⊕ s3, …)`.
pub fn mix_columns(state: &mut [u8; BLOCK]) {
    for c in 0..4 {
        let i = 4 * c;
        let s0 = state[i];
        let s1 = state[i + 1];
        let s2 = state[i + 2];
        let s3 = state[i + 3];
        state[i] = gf_mul(s0, 2) ^ gf_mul(s1, 3) ^ s2 ^ s3;
        state[i + 1] = s0 ^ gf_mul(s1, 2) ^ gf_mul(s2, 3) ^ s3;
        state[i + 2] = s0 ^ s1 ^ gf_mul(s2, 2) ^ gf_mul(s3, 3);
        state[i + 3] = gf_mul(s0, 3) ^ s1 ^ s2 ^ gf_mul(s3, 2);
    }
}

/// AddRoundKey — XOR `state` with `round_key`. Trivially linear, no
/// GF(2^8) multiplications.
pub fn add_round_key(state: &mut [u8; BLOCK], round_key: &[u8; BLOCK]) {
    for i in 0..BLOCK {
        state[i] ^= round_key[i];
    }
}

/// Key expansion for AES-128 (FIPS 197 §5.2).
///
/// Returns 11 round keys: index 0 is the initial whitening key (a copy of
/// `key`), indices 1..=10 are the per-round subkeys.
pub fn key_expansion(key: &[u8; BLOCK]) -> [[u8; BLOCK]; NK_ROUND_KEYS] {
    // Working buffer of 44 4-byte words (Nb·(Nr+1) = 4·11).
    let mut words = [[0u8; 4]; 4 * NK_ROUND_KEYS];
    for i in 0..4 {
        words[i] = [key[4 * i], key[4 * i + 1], key[4 * i + 2], key[4 * i + 3]];
    }

    for i in 4..4 * NK_ROUND_KEYS {
        let mut temp = words[i - 1];
        if i % 4 == 0 {
            // RotWord: rotate left by one byte.
            let t0 = temp[0];
            temp[0] = temp[1];
            temp[1] = temp[2];
            temp[2] = temp[3];
            temp[3] = t0;
            // SubWord: S-box on each byte.
            for b in 0..4 {
                temp[b] = SBOX[temp[b] as usize];
            }
            // XOR with round constant on byte 0.
            temp[0] ^= RCON[i / 4];
        }
        for b in 0..4 {
            words[i][b] = words[i - 4][b] ^ temp[b];
        }
    }

    let mut round_keys = [[0u8; BLOCK]; NK_ROUND_KEYS];
    for r in 0..NK_ROUND_KEYS {
        for c in 0..4 {
            let w = words[4 * r + c];
            round_keys[r][4 * c] = w[0];
            round_keys[r][4 * c + 1] = w[1];
            round_keys[r][4 * c + 2] = w[2];
            round_keys[r][4 * c + 3] = w[3];
        }
    }
    round_keys
}

/// AES-128 single-block encryption — the FAEST OWF for the λ=128
/// parameter set.
///
/// Computes `c = AES128_k(p)`. Pure forward direction: FAEST does not
/// invoke AES decryption anywhere.
pub fn encrypt_block(key: &[u8; BLOCK], plain: &[u8; BLOCK]) -> [u8; BLOCK] {
    let round_keys = key_expansion(key);
    let mut state = *plain;
    add_round_key(&mut state, &round_keys[0]);
    for r in 1..NR {
        sub_bytes(&mut state);
        shift_rows(&mut state);
        mix_columns(&mut state);
        add_round_key(&mut state, &round_keys[r]);
    }
    // Final round skips MixColumns.
    sub_bytes(&mut state);
    shift_rows(&mut state);
    add_round_key(&mut state, &round_keys[NR]);
    state
}

#[cfg(test)]
mod tests {
    use super::*;

    /// FIPS 197 Appendix B — single-block KAT.
    ///
    /// Key:     2b7e1516 28aed2a6 abf71588 09cf4f3c
    /// Plain:   3243f6a8 885a308d 313198a2 e0370734
    /// Cipher:  3925841d 02dc09fb dc118597 196a0b32
    #[test]
    fn fips_197_appendix_b_kat() {
        let key: [u8; 16] = [
            0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf, 0x4f,
            0x3c,
        ];
        let plain: [u8; 16] = [
            0x32, 0x43, 0xf6, 0xa8, 0x88, 0x5a, 0x30, 0x8d, 0x31, 0x31, 0x98, 0xa2, 0xe0, 0x37, 0x07,
            0x34,
        ];
        let expected: [u8; 16] = [
            0x39, 0x25, 0x84, 0x1d, 0x02, 0xdc, 0x09, 0xfb, 0xdc, 0x11, 0x85, 0x97, 0x19, 0x6a, 0x0b,
            0x32,
        ];
        let got = encrypt_block(&key, &plain);
        assert_eq!(got, expected, "FIPS 197 Appendix B KAT mismatch");
    }

    /// FIPS 197 Appendix C.1 — second KAT to nail down a different code
    /// path (key all zeros pattern stress-tests the key schedule's
    /// dependence on RCON).
    ///
    /// Key:     000102030405060708090a0b0c0d0e0f
    /// Plain:   00112233445566778899aabbccddeeff
    /// Cipher:  69c4e0d86a7b0430d8cdb78070b4c55a
    #[test]
    fn fips_197_appendix_c1_kat() {
        let key: [u8; 16] = [
            0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e,
            0x0f,
        ];
        let plain: [u8; 16] = [
            0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee,
            0xff,
        ];
        let expected: [u8; 16] = [
            0x69, 0xc4, 0xe0, 0xd8, 0x6a, 0x7b, 0x04, 0x30, 0xd8, 0xcd, 0xb7, 0x80, 0x70, 0xb4, 0xc5,
            0x5a,
        ];
        let got = encrypt_block(&key, &plain);
        assert_eq!(got, expected, "FIPS 197 Appendix C.1 KAT mismatch");
    }

    /// Key expansion sanity: the initial round key equals the input key.
    #[test]
    fn key_expansion_first_round_is_input_key() {
        let key: [u8; 16] = [
            0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf, 0x4f,
            0x3c,
        ];
        let rks = key_expansion(&key);
        assert_eq!(rks[0], key);
    }

    /// FIPS 197 §A.1 — final round key for the all-zero-pattern key
    /// expansion. Pins the schedule down to the last word.
    #[test]
    fn key_expansion_appendix_a1_final_word() {
        let key: [u8; 16] = [
            0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf, 0x4f,
            0x3c,
        ];
        let rks = key_expansion(&key);
        // FIPS 197 §A.1 row 43 (last word of expanded key):
        // w[43] = b6 63 0c a6
        let last = rks[NR];
        assert_eq!(&last[12..16], &[0xb6, 0x63, 0x0c, 0xa6]);
    }

    /// ShiftRows applied twice equals shift-by-2 per row, equivalent to a
    /// known permutation. Pin it down via a small fixture.
    #[test]
    fn shift_rows_roundtrip() {
        let original: [u8; 16] = [
            0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
        ];
        let mut state = original;
        // Applying ShiftRows 4 times to row r shifts by 4r ≡ 0 (mod 4): identity.
        for _ in 0..4 {
            shift_rows(&mut state);
        }
        assert_eq!(state, original);
    }

    /// MixColumns + InvMixColumns would be a roundtrip; we don't ship inv.
    /// Instead pin a known FIPS 197 §B intermediate value: after the first
    /// SubBytes, ShiftRows on plain=3243…, MixColumns produces the column
    /// values listed in Appendix B's "Start of Round 2" line:
    /// 04 e0 48 28, 66 cb f8 06, 81 19 d3 26, e5 9a 7a 4c.
    #[test]
    fn mix_columns_appendix_b_first_round_pin() {
        // After AddRoundKey(0), SubBytes, ShiftRows on the FIPS test
        // vector, the state is the FIPS 197 §B "After ShiftRows" row:
        // d4 bf 5d 30, e0 b4 52 ae, b8 41 11 f1, 1e 27 98 e5.
        let mut state: [u8; 16] = [
            0xd4, 0xbf, 0x5d, 0x30, 0xe0, 0xb4, 0x52, 0xae, 0xb8, 0x41, 0x11, 0xf1, 0x1e, 0x27, 0x98,
            0xe5,
        ];
        mix_columns(&mut state);
        let expected: [u8; 16] = [
            0x04, 0x66, 0x81, 0xe5, 0xe0, 0xcb, 0x19, 0x9a, 0x48, 0xf8, 0xd3, 0x7a, 0x28, 0x06, 0x26,
            0x4c,
        ];
        assert_eq!(state, expected);
    }
}
