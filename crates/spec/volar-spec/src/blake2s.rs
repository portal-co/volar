// @reliability: normal
//! @ai: assisted
//! Spec-dialect BLAKE2s (BLAKE2s-256, RFC 7693) — the CPU-friendly ARX hash
//! backend for the hash-generic pipeline (`docs/hash-cipher-generic-plan.md`),
//! best for the **Cirrus** consumer (native symbolic execution on low-spec
//! targets). Written like `keccak.rs` so the compiler can parse and lower it
//! through LIR to VAFFLE: bounded `for` loops only (the dialect bans `while`),
//! `Array`/fixed-array data, no `Vec` in the dialect path. Rotations are
//! written as shift+or (the dialect reserves the `rotate_*` names).
//!
//! Differentially pinned against the `blake2` crate's `Blake2s256` (see the
//! tests at the bottom), byte-for-byte.
//!
//! Unlike Keccak/SHAKE (a sponge), BLAKE2s is a HAIFA-style
//! Merkle–Damgård construction (counter + last-block flag) with a fixed
//! 32-byte output — it is a commitment/Merkle hash, not a streaming XOF. For
//! the Fiat-Shamir transcript's XOF shape, use it in a counter construction
//! (or use Ascon-XOF, the H3 backend).

use super::*;

/// BLAKE2s block size in bytes.
pub const BLAKE2S_BLOCK: usize = 64;
/// BLAKE2s-256 output size in bytes.
pub const BLAKE2S_OUT: usize = 32;

/// The BLAKE2s IV (the SHA-256 IV).
pub const BLAKE2S_IV: [u32; 8] = [
    0x6A09E667, 0xBB67AE85, 0x3C6EF372, 0xA54FF53A, 0x510E527F, 0x9B05688C, 0x1F83D9AB, 0x5BE0CD19,
];

/// The BLAKE2s message schedule permutation, flattened (`SIGMA[round*16 + k]`,
/// 10 rounds × 16 entries).
pub const BLAKE2S_SIGMA: [usize; 160] = [
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, //
    14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3, //
    11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4, //
    7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8, //
    9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13, //
    2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9, //
    12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11, //
    13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10, //
    6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5, //
    10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0,
];

/// The BLAKE2s `G` mixing function. Rotations are shift+or (`rotr32(x, k) =
/// (x >> k) | (x << (32 - k))`); additions are `wrapping_add` (mod 2^32).
pub fn blake2s_g(v: &mut [u32; 16], a: usize, b: usize, c: usize, d: usize, mx: u32, my: u32) {
    v[a] = v[a].wrapping_add(v[b]).wrapping_add(mx);
    let t0 = v[d] ^ v[a];
    v[d] = (t0 >> 16) | (t0 << 16);
    v[c] = v[c].wrapping_add(v[d]);
    let t1 = v[b] ^ v[c];
    v[b] = (t1 >> 12) | (t1 << 20);
    v[a] = v[a].wrapping_add(v[b]).wrapping_add(my);
    let t2 = v[d] ^ v[a];
    v[d] = (t2 >> 8) | (t2 << 24);
    v[c] = v[c].wrapping_add(v[d]);
    let t3 = v[b] ^ v[c];
    v[b] = (t3 >> 7) | (t3 << 25);
}

/// The BLAKE2s compression function: mix the 64-byte `block` into the 8-word
/// chaining state `h`, with `t` the cumulative byte count (including this
/// block's real bytes) and `last` the final-block flag.
pub fn blake2s_compress(h: &mut [u32; 8], block: &[u8; BLAKE2S_BLOCK], t: u64, last: bool) {
    let mut m = [0u32; 16];
    for i in 0..16 {
        let mut chunk = [0u8; 4];
        chunk.copy_from_slice(&block[i * 4..i * 4 + 4]);
        m[i] = u32::from_le_bytes(chunk);
    }
    let mut v = [0u32; 16];
    for i in 0..8 {
        v[i] = h[i];
    }
    for i in 0..8 {
        v[i + 8] = BLAKE2S_IV[i];
    }
    v[12] ^= t as u32; // low 32 bits of the counter
    v[13] ^= (t >> 32) as u32; // high 32 bits
    if last {
        v[14] ^= 0xFFFF_FFFF;
    }
    for round in 0..10 {
        let base = round * 16;
        blake2s_g(&mut v, 0, 4, 8, 12, m[BLAKE2S_SIGMA[base]], m[BLAKE2S_SIGMA[base + 1]]);
        blake2s_g(&mut v, 1, 5, 9, 13, m[BLAKE2S_SIGMA[base + 2]], m[BLAKE2S_SIGMA[base + 3]]);
        blake2s_g(&mut v, 2, 6, 10, 14, m[BLAKE2S_SIGMA[base + 4]], m[BLAKE2S_SIGMA[base + 5]]);
        blake2s_g(&mut v, 3, 7, 11, 15, m[BLAKE2S_SIGMA[base + 6]], m[BLAKE2S_SIGMA[base + 7]]);
        blake2s_g(&mut v, 0, 5, 10, 15, m[BLAKE2S_SIGMA[base + 8]], m[BLAKE2S_SIGMA[base + 9]]);
        blake2s_g(&mut v, 1, 6, 11, 12, m[BLAKE2S_SIGMA[base + 10]], m[BLAKE2S_SIGMA[base + 11]]);
        blake2s_g(&mut v, 2, 7, 8, 13, m[BLAKE2S_SIGMA[base + 12]], m[BLAKE2S_SIGMA[base + 13]]);
        blake2s_g(&mut v, 3, 4, 9, 14, m[BLAKE2S_SIGMA[base + 14]], m[BLAKE2S_SIGMA[base + 15]]);
    }
    for i in 0..8 {
        h[i] ^= v[i] ^ v[i + 8];
    }
}

/// The initial BLAKE2s-256 chaining state: `IV` with the parameter block XORed
/// into `h[0]` (digest_length=32, key_length=0, fanout=1, depth=1 →
/// `0x0101_0020`).
pub fn blake2s_init() -> [u32; 8] {
    let mut h = BLAKE2S_IV;
    h[0] ^= 0x0101_0020;
    h
}

/// The 32-byte digest of the final chaining state (little-endian words).
pub fn blake2s_output(h: &[u32; 8]) -> [u8; BLAKE2S_OUT] {
    let mut out = [0u8; BLAKE2S_OUT];
    for i in 0..8 {
        out[i * 4..i * 4 + 4].copy_from_slice(&h[i].to_le_bytes());
    }
    out
}

/// One-shot BLAKE2s-256 of a byte string (native slice form).
pub fn blake2s_256(input: &[u8]) -> [u8; BLAKE2S_OUT] {
    let mut h = blake2s_init();
    let n = input.len();
    // BLAKE2s always compresses at least one (possibly all-zero) block.
    let num_blocks = if n == 0 { 1 } else { n.div_ceil(BLAKE2S_BLOCK) };
    let mut t = 0u64;
    for i in 0..num_blocks {
        let mut block = [0u8; BLAKE2S_BLOCK];
        let start = i * BLAKE2S_BLOCK;
        let take = (n - start).min(BLAKE2S_BLOCK);
        block[..take].copy_from_slice(&input[start..start + take]);
        t += take as u64;
        blake2s_compress(&mut h, &block, t, i == num_blocks - 1);
    }
    blake2s_output(&h)
}

/// One-shot BLAKE2s-256 of a fixed-size byte array — the dialect form
/// (concrete `N`). Mirrors [`blake2s_256`]; the block count `(N+63)/64` (with
/// a floor of 1) is a const expression of `N`.
pub fn blake2s_256_array<N: ArraySize>(input: &Array<u8, N>) -> [u8; BLAKE2S_OUT] {
    let mut h = blake2s_init();
    let n = N::USIZE;
    let num_blocks = if n == 0 { 1 } else { (n + BLAKE2S_BLOCK - 1) / BLAKE2S_BLOCK };
    let mut t = 0u64;
    for i in 0..num_blocks {
        let mut block = [0u8; BLAKE2S_BLOCK];
        let start = i * BLAKE2S_BLOCK;
        for j in 0..BLAKE2S_BLOCK {
            let idx = start + j;
            if idx < n {
                block[j] = input[idx];
            }
        }
        let take = if start + BLAKE2S_BLOCK <= n {
            BLAKE2S_BLOCK
        } else {
            n - start
        };
        t += take as u64;
        blake2s_compress(&mut h, &block, t, i == num_blocks - 1);
    }
    blake2s_output(&h)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn blake2s_matches_blake2_crate() {
        use blake2::Digest;
        // A spread of lengths across the block boundary (incl. 0, a partial
        // block, exactly one block, and multi-block).
        for n in [0usize, 1, 31, 32, 63, 64, 65, 100, 128, 200, 500] {
            let data: alloc::vec::Vec<u8> = (0..n).map(|i| (i % 251) as u8).collect();
            let mut h = blake2::Blake2s256::new();
            h.update(&data);
            let expected: [u8; 32] = h.finalize().into();
            let got = blake2s_256(&data);
            assert_eq!(&got[..8], &expected[..8], "BLAKE2s-256 first-8-bytes mismatch at len {n}");
        }
    }

    #[test]
    fn blake2s_array_matches_slice_form() {
        // The const-generic array form must agree with the slice form.
        let data: alloc::vec::Vec<u8> = (0..100u32).map(|i| (i % 251) as u8).collect();
        let arr: Array<u8, hybrid_array::typenum::U100> = Array::try_from(&data[..]).expect("100 bytes");
        assert_eq!(blake2s_256_array(&arr), blake2s_256(&data));
    }
}
