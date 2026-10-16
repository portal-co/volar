// @reliability: normal
//! @ai: assisted
//! Spec-dialect Keccak: Keccak-f[1600], a streaming sponge, SHA3-256, and
//! SHAKE128 — written so the compiler can parse and lower it through LIR to
//! VAFFLE (no extern hashing boundary), per the WARP plan's M4 requirement
//! (`volar-ir`'s `docs/warp-vole-verifier-zksnark-plan.md`).
//!
//! Ported from `volar-fold`'s `keccak.rs` reference (same tables, same
//! lane layout), restated in the spec subset: bounded `for` loops only (the
//! dialect bans `while`), `Array`/fixed-array data, no `Vec`. The streaming
//! sponge's absorb/squeeze byte schedule is byte-identical to the `sha3`
//! crate — pinned by differential tests against it (SHAKE128 absorb/squeeze
//! and SHA3-256 one-shot and multi-block).
//!
//! Domains: SHA3-256 uses the `0x06` suffix, SHAKE128 the `0x1F` suffix,
//! both with pad10*1 (`0x80` at the last rate byte) — FIPS 202.

use super::*;

/// SHAKE128 rate in bytes.
pub const SHAKE128_RATE: usize = 168;
/// SHA3-256 rate in bytes.
pub const SHA3_256_RATE: usize = 136;

/// Keccak-f round constants (identical to `volar-fold`'s `RNDC`).
pub const KECCAK_RNDC: [u64; 24] = [
    0x0000_0000_0000_0001,
    0x0000_0000_0000_8082,
    0x8000_0000_0000_808a,
    0x8000_0000_8000_8000,
    0x0000_0000_0000_808b,
    0x0000_0000_8000_0001,
    0x8000_0000_8000_8081,
    0x8000_0000_0000_8009,
    0x0000_0000_0000_008a,
    0x0000_0000_0000_0088,
    0x0000_0000_8000_8009,
    0x0000_0000_8000_000a,
    0x0000_0000_8000_808b,
    0x8000_0000_0000_008b,
    0x8000_0000_0000_8089,
    0x8000_0000_0000_8003,
    0x8000_0000_0000_8002,
    0x8000_0000_0000_0080,
    0x0000_0000_0000_800a,
    0x8000_0000_8000_000a,
    0x8000_0000_8000_8081,
    0x8000_0000_0000_8080,
    0x0000_0000_8000_0001,
    0x8000_0000_8000_8008,
];

/// Keccak-f rotation offsets (identical to `volar-fold`'s `ROTC`).
pub const KECCAK_ROTC: [u32; 24] = [
    1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 2, 14, 27, 41, 56, 8, 25, 43, 62, 18, 39, 61, 20, 44,
];

/// Keccak-f π lane permutation (identical to `volar-fold`'s `PILN`).
pub const KECCAK_PILN: [usize; 24] = [
    10, 7, 11, 17, 18, 3, 5, 16, 8, 21, 24, 4, 15, 23, 19, 13, 12, 2, 20, 14, 22, 9, 6, 1,
];

/// The Keccak-f[1600] permutation on 25 little-endian lanes.
pub fn keccak_f1600(st: &mut [u64; 25]) {
    for round in 0..24 {
        // θ
        let mut bc = [0u64; 5];
        for i in 0..5 {
            bc[i] = st[i] ^ st[i + 5] ^ st[i + 10] ^ st[i + 15] ^ st[i + 20];
        }
        for i in 0..5 {
            let t = bc[(i + 4) % 5] ^ bc[(i + 1) % 5].rotate_left(1);
            for jj in 0..5 {
                st[jj * 5 + i] ^= t;
            }
        }
        // ρ + π
        let mut t = st[1];
        for i in 0..24 {
            let j = KECCAK_PILN[i];
            let tmp = st[j];
            st[j] = t.rotate_left(KECCAK_ROTC[i]);
            t = tmp;
        }
        // χ
        for jj in 0..5 {
            let j = jj * 5;
            let mut row = [0u64; 5];
            for i in 0..5 {
                row[i] = st[j + i];
            }
            for i in 0..5 {
                st[j + i] ^= (!row[(i + 1) % 5]) & row[(i + 2) % 5];
            }
        }
        // ι
        st[0] ^= KECCAK_RNDC[round];
    }
}

pub(crate) fn bytes_to_lanes(b: &[u8; 200], st: &mut [u64; 25]) {
    for i in 0..25 {
        let mut chunk = [0u8; 8];
        chunk.copy_from_slice(&b[i * 8..i * 8 + 8]);
        st[i] = u64::from_le_bytes(chunk);
    }
}

pub(crate) fn lanes_to_bytes(st: &[u64; 25], b: &mut [u8; 200]) {
    for i in 0..25 {
        b[i * 8..i * 8 + 8].copy_from_slice(&st[i].to_le_bytes());
    }
}

fn permute_bytes(state_bytes: &mut [u8; 200]) {
    let mut st = [0u64; 25];
    bytes_to_lanes(state_bytes, &mut st);
    keccak_f1600(&mut st);
    lanes_to_bytes(&st, state_bytes);
}

/// A streaming Keccak sponge at a given rate, in the absorb phase.
///
/// Byte-identical to the `sha3` crate's incremental `update`: input bytes
/// XOR into the rate portion (little-endian lanes); the permutation runs
/// each time the rate block fills.
#[derive(Clone, Debug)]
pub struct SpecSponge {
    state: [u8; 200],
    rate: usize,
    pos: usize,
}

impl SpecSponge {
    /// A fresh absorb-phase sponge with the given rate (in bytes).
    pub fn new(rate: usize) -> Self {
        SpecSponge {
            state: [0u8; 200],
            rate,
            pos: 0,
        }
    }

    /// Absorb bytes (incremental, order-preserving).
    pub fn absorb(&mut self, data: &[u8]) {
        for i in 0..data.len() {
            self.state[self.pos] ^= data[i];
            self.pos += 1;
            if self.pos == self.rate {
                permute_bytes(&mut self.state);
                self.pos = 0;
            }
        }
    }

    /// Finalize with the pad10*1 rule and domain `suffix` (which must not
    /// have the top bit set — SHA3's `0x06` and SHAKE's `0x1F` qualify),
    /// returning a squeeze-phase reader. Non-destructive: `self` is cloned.
    pub fn finalize_xof(&self, suffix: u8) -> SpecXofReader {
        let mut state = self.state;
        state[self.pos] ^= suffix;
        state[self.rate - 1] ^= 0x80;
        permute_bytes(&mut state);
        SpecXofReader {
            state,
            rate: self.rate,
            pos: 0,
        }
    }
}

/// A squeeze-phase Keccak reader: reads out the rate bytes, re-permuting
/// each time the rate block is exhausted (FIPS 202 XOF semantics).
#[derive(Clone, Debug)]
pub struct SpecXofReader {
    state: [u8; 200],
    rate: usize,
    pos: usize,
}

impl SpecXofReader {
    /// Read bytes into `out` (streaming; continues across calls).
    pub fn read(&mut self, out: &mut [u8]) {
        for i in 0..out.len() {
            if self.pos == self.rate {
                permute_bytes(&mut self.state);
                self.pos = 0;
            }
            out[i] = self.state[self.pos];
            self.pos += 1;
        }
    }
}

/// One-shot SHA3-256 of a byte string.
pub fn spec_sha3_256(input: &[u8]) -> [u8; 32] {
    let mut sponge = SpecSponge::new(SHA3_256_RATE);
    sponge.absorb(input);
    let mut reader = sponge.finalize_xof(0x06);
    let mut out = [0u8; 32];
    reader.read(&mut out);
    out
}

/// One-shot SHAKE128 of a byte string, `N` output bytes.
pub fn spec_shake128<N: ArraySize>(input: &[u8]) -> Array<u8, N> {
    let mut sponge = SpecSponge::new(SHAKE128_RATE);
    sponge.absorb(input);
    let mut reader = sponge.finalize_xof(0x1F);
    let mut out = Array::<u8, N>::default();
    reader.read(&mut out);
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec::Vec;
    use sha3::Digest;
    use sha3::digest::{ExtendableOutput, FixedOutput, Update, XofReader};

    fn sha3_crate(input: &[u8]) -> [u8; 32] {
        let mut h = sha3::Sha3_256::new();
        Digest::update(&mut h, input);
        h.finalize_fixed().into()
    }

    #[test]
    fn sha3_256_matches_the_sha3_crate() {
        for input in [
            &b""[..],
            &b"abc"[..],
            &[0x42u8; 136][..],  // exactly one rate block
            &[0x42u8; 135][..],  // one byte short
            &[0x42u8; 137][..],  // one byte over
            &[0x07u8; 1000][..], // multi-block
        ] {
            assert_eq!(spec_sha3_256(input), sha3_crate(input));
        }
    }

    #[test]
    fn shake128_matches_the_sha3_crate() {
        for input in [&b""[..], &b"volar"[..], &[0xAAu8; 200][..], &[0x55u8; 1000][..]] {
            let mut crate_reader = {
                let mut h = sha3::Shake128::default();
                Update::update(&mut h, input);
                h.finalize_xof()
            };
            let mut expected = [0u8; 400];
            crate_reader.read(&mut expected);

            let mut sponge = SpecSponge::new(SHAKE128_RATE);
            sponge.absorb(input);
            let mut reader = sponge.finalize_xof(0x1F);
            let mut got = [0u8; 400];
            reader.read(&mut got);
            assert_eq!(got, expected, "SHAKE128 stream must match the sha3 crate");
        }
    }

    #[test]
    fn incremental_absorb_matches_one_shot() {
        // Absorb in odd-sized pieces; the stream must not depend on the split.
        let data: Vec<u8> = (0..500u32).map(|i| (i % 251) as u8).collect();
        let mut one_shot = SpecSponge::new(SHAKE128_RATE);
        one_shot.absorb(&data);
        let mut piecewise = SpecSponge::new(SHAKE128_RATE);
        // Piece boundaries at triangular-stepped offsets.
        let mut off = 0usize;
        for step in 0..data.len() {
            if off >= data.len() {
                break;
            }
            let take = ((step % 7) + 1).min(data.len() - off);
            piecewise.absorb(&data[off..off + take]);
            off += take;
        }
        let mut a = [0u8; 64];
        let mut b = [0u8; 64];
        one_shot.finalize_xof(0x1F).read(&mut a);
        piecewise.finalize_xof(0x1F).read(&mut b);
        assert_eq!(a, b);
    }
}
