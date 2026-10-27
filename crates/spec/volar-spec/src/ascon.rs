// @reliability: normal
//! @ai: assisted
//! Spec-dialect Ascon (the NIST lightweight-crypto standard, SP 800-232) —
//! the cheap **in-circuit** hash backend for the hash-generic pipeline
//! (`docs/hash-cipher-generic-plan.md`), best for the **recursive-proving** and
//! **memory-accumulator** consumers: the permutation is bit-sliced (the χ
//! S-box is a handful of AND/XOR/NOT per 64-bit column, the linear layer is
//! rotations + xors), so it is far cheaper as a circuit than Keccak's 24-round
//! S-box or Blake2s's ARX carry chains. Written like `keccak.rs`/`blake2s.rs`
//! so the compiler lowers it through LIR to VAFFLE: bounded `for` loops only,
//! `[u64; 5]` state, rotations as shift+or.
//!
//! Differentially pinned against the `ascon-hash` crate's `AsconHash256`
//! (see the tests at the bottom), byte-for-byte. The round function matches
//! `ascon-core`'s refactored algebraic form exactly.

use super::*;

/// The Ascon-Hash256 initial state (the `HashParameters` IV).
pub const ASCON_HASH256_IV: [u64; 5] = [
    0x9b1e5494e934d681,
    0x4bc3a01e333751d2,
    0xae65396c6b34b81a,
    0x3c7fd4a4d56a4db3,
    0x1a5c464906c5976d,
];

/// The Ascon-XOF128 initial state.
pub const ASCON_XOF128_IV: [u64; 5] = [
    0xda82ce768d9447eb,
    0xcc7ce6c75f1ef969,
    0xe7508fd780085631,
    0x0ee0ea53416b58cc,
    0xe0547524db6f0bde,
];

/// The Ascon-p[12] round constants.
pub const ASCON_RC: [u64; 12] = [
    0xf0, 0xe1, 0xd2, 0xc3, 0xb4, 0xa5, 0x96, 0x87, 0x78, 0x69, 0x5a, 0x4b,
];

/// One Ascon round (the `ascon-core` refactored algebraic form). Rotations are
/// shift+or (`rotr64(x, k) = (x >> k) | (x << (64 - k))`).
pub fn ascon_round(x: [u64; 5], c: u64) -> [u64; 5] {
    // S-box layer (with the round constant folded into x2).
    let x0 = x[0] ^ x[4];
    let x2 = x[2] ^ x[1] ^ c;
    let x4 = x[4] ^ x[3];
    let tx0 = x0 ^ (!x[1] & x2);
    let tx1 = x[1] ^ (!x2 & x[3]);
    let tx2 = x2 ^ (!x[3] & x4);
    let tx3 = x[3] ^ (!x4 & x0);
    let tx4 = x4 ^ (!x0 & x[1]);
    let tx1 = tx1 ^ tx0;
    let tx3 = tx3 ^ tx2;
    let tx0 = tx0 ^ tx4;
    // Linear layer (two rotations per word).
    let y0 = tx0 ^ ((tx0 >> 9) | (tx0 << 55));
    let y1 = tx1 ^ ((tx1 >> 22) | (tx1 << 42));
    let y2 = tx2 ^ ((tx2 >> 5) | (tx2 << 59));
    let y3 = tx3 ^ ((tx3 >> 7) | (tx3 << 57));
    let y4 = tx4 ^ ((tx4 >> 34) | (tx4 << 30));
    [
        tx0 ^ ((y0 >> 19) | (y0 << 45)),
        tx1 ^ ((y1 >> 39) | (y1 << 25)),
        !(tx2 ^ ((y2 >> 1) | (y2 << 63))),
        tx3 ^ ((y3 >> 10) | (y3 << 54)),
        tx4 ^ ((y4 >> 7) | (y4 << 57)),
    ]
}

/// The Ascon-p[12] permutation (12 rounds) on the 5-word state.
pub fn ascon_p12(state: &mut [u64; 5]) {
    for round in 0..12 {
        // Element-wise copy back: the dialect's assignment lowering doesn't
        // handle a whole-array `*state = ...` deref-write.
        let new = ascon_round(*state, ASCON_RC[round]);
        for i in 0..5 {
            state[i] = new[i];
        }
    }
}

/// One-shot Ascon-Hash256 of a byte string (native slice form).
pub fn ascon_hash256(input: &[u8]) -> [u8; 32] {
    let mut state = ASCON_HASH256_IV;
    let n = input.len();
    // Absorb all full 8-byte blocks, then the (possibly empty) padded tail.
    let mut i = 0;
    while n - i >= 8 {
        let mut w = [0u8; 8];
        w.copy_from_slice(&input[i..i + 8]);
        state[0] ^= u64::from_le_bytes(w);
        ascon_p12(&mut state);
        i += 8;
    }
    // Last block: the remaining (n - i) bytes (0..7) + the 0x01 padding byte.
    let rem = n - i;
    let mut last = [0u8; 8];
    last[..rem].copy_from_slice(&input[i..]);
    state[0] ^= u64::from_le_bytes(last);
    state[0] ^= 0x01u64 << (8 * rem);
    ascon_p12(&mut state);
    // Squeeze 32 bytes (4 blocks; permute between blocks, not after the last).
    let mut out = [0u8; 32];
    for j in 0..4 {
        out[j * 8..j * 8 + 8].copy_from_slice(&state[0].to_le_bytes());
        if j < 3 {
            ascon_p12(&mut state);
        }
    }
    out
}

/// One-shot Ascon-Hash256 of a fixed-size byte array — the dialect form
/// (concrete `N`). Mirrors [`ascon_hash256`].
pub fn ascon_hash256_array<N: ArraySize>(input: &Array<u8, N>) -> [u8; 32] {
    let mut state = ASCON_HASH256_IV;
    let n = N::USIZE;
    let num_full = n / 8;
    for i in 0..num_full {
        let mut w = [0u8; 8];
        for j in 0..8 {
            w[j] = input[i * 8 + j];
        }
        state[0] ^= u64::from_le_bytes(w);
        ascon_p12(&mut state);
    }
    let rem = n - num_full * 8;
    let mut last = [0u8; 8];
    for j in 0..8 {
        let idx = num_full * 8 + j;
        if idx < n {
            last[j] = input[idx];
        }
    }
    state[0] ^= u64::from_le_bytes(last);
    state[0] ^= 0x01u64 << (8 * rem);
    ascon_p12(&mut state);
    let mut out = [0u8; 32];
    for j in 0..4 {
        out[j * 8..j * 8 + 8].copy_from_slice(&state[0].to_le_bytes());
        if j < 3 {
            ascon_p12(&mut state);
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ascon_hash256_matches_reference() {
        use ascon_hash::Digest;
        // A spread of lengths across the 8-byte rate boundary (incl. 0, a
        // partial block, exactly one block, and multi-block).
        for n in [0usize, 1, 7, 8, 9, 15, 16, 17, 31, 32, 33, 100, 200] {
            let data: alloc::vec::Vec<u8> = (0..n).map(|i| (i % 251) as u8).collect();
            let mut h = ascon_hash::AsconHash256::new();
            h.update(&data);
            let expected: [u8; 32] = h.finalize().into();
            assert_eq!(
                ascon_hash256(&data),
                expected,
                "Ascon-Hash256 mismatch at len {n}"
            );
        }
    }

    #[test]
    fn ascon_array_matches_slice_form() {
        let data: alloc::vec::Vec<u8> = (0..50u32).map(|i| (i % 251) as u8).collect();
        let arr: Array<u8, hybrid_array::typenum::U50> =
            Array::try_from(&data[..]).expect("50 bytes");
        assert_eq!(ascon_hash256_array(&arr), ascon_hash256(&data));
    }
}
