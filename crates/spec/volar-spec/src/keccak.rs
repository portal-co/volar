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
            // rotate_left(1) written as shifts (see the ρ+π note below).
            let t = bc[(i + 4) % 5] ^ ((bc[(i + 1) % 5] << 1) | (bc[(i + 1) % 5] >> 63));
            for jj in 0..5 {
                st[jj * 5 + i] ^= t;
            }
        }
        // ρ + π — the rotation is written as shifts+or rather than
        // `u64::rotate_left` because the spec dialect's parser reserves the
        // name `rotate_left` for the VOLE `Delta` method; for k in 1..64
        // (every ROTC entry is nonzero) `(t << k) | (t >> (64 - k))` is
        // exactly `t.rotate_left(k)`.
        let mut t = st[1];
        for i in 0..24 {
            let j = KECCAK_PILN[i];
            let k = KECCAK_ROTC[i];
            let tmp = st[j];
            // `64u32` keeps the subtraction width-consistent with `k: u32`.
            st[j] = (t << k) | (t >> (64u32 - k));
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

/// A fresh absorb-phase sponge with the given rate (in bytes).
pub fn spec_sponge_new(rate: usize) -> SpecSponge {
    SpecSponge {
        state: [0u8; 200],
        rate,
        pos: 0,
    }
}

/// Absorb bytes (incremental, order-preserving). Slice form for native
/// callers; the dialect path uses [`spec_sponge_absorb_array`] (concrete
/// loop bounds).
pub fn spec_sponge_absorb(sponge: &mut SpecSponge, data: &[u8]) {
    // Destructure to locals: the dialect's native loops lower
    // local-array writes, not struct-field array writes.
    let mut state = sponge.state;
    let mut pos = sponge.pos;
    let rate = sponge.rate;
    for i in 0..data.len() {
        state[pos] ^= data[i];
        pos += 1;
        if pos == rate {
            permute_bytes(&mut state);
            pos = 0;
        }
    }
    sponge.state = state;
    sponge.pos = pos;
}

/// Absorb a fixed-size byte array — the dialect form (concrete `N`
/// loop bound).
pub fn spec_sponge_absorb_array<N: ArraySize>(sponge: &mut SpecSponge, data: &Array<u8, N>) {
    let mut state = sponge.state;
    let mut pos = sponge.pos;
    let rate = sponge.rate;
    for i in 0..N::USIZE {
        state[pos] ^= data[i];
        pos += 1;
        if pos == rate {
            permute_bytes(&mut state);
            pos = 0;
        }
    }
    sponge.state = state;
    sponge.pos = pos;
}

/// Finalize with the pad10*1 rule and domain `suffix` (which must not
/// have the top bit set — SHA3's `0x06` and SHAKE's `0x1F` qualify),
/// returning a squeeze-phase reader. Non-destructive: the sponge is cloned.
pub fn spec_sponge_finalize_xof(sponge: &SpecSponge, suffix: u8) -> SpecXofReader {
    let mut state = sponge.state;
    state[sponge.pos] ^= suffix;
    state[sponge.rate - 1] ^= 0x80;
    permute_bytes(&mut state);
    SpecXofReader {
        state,
        rate: sponge.rate,
        pos: 0,
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

/// Read bytes into `out` (streaming; continues across calls). Slice
/// form for native callers; the dialect path uses [`spec_xof_read_array`].
pub fn spec_xof_read(reader: &mut SpecXofReader, out: &mut [u8]) {
    let mut state = reader.state;
    let mut pos = reader.pos;
    let rate = reader.rate;
    for i in 0..out.len() {
        if pos == rate {
            permute_bytes(&mut state);
            pos = 0;
        }
        out[i] = state[pos];
        pos += 1;
    }
    reader.state = state;
    reader.pos = pos;
}

/// Read into a fixed-size array — the dialect form (concrete `N` bound).
pub fn spec_xof_read_array<N: ArraySize>(reader: &mut SpecXofReader, out: &mut Array<u8, N>) {
    let mut state = reader.state;
    let mut pos = reader.pos;
    let rate = reader.rate;
    for i in 0..N::USIZE {
        if pos == rate {
            permute_bytes(&mut state);
            pos = 0;
        }
        out[i] = state[pos];
        pos += 1;
    }
    reader.state = state;
    reader.pos = pos;
}

/// One-shot SHA3-256 of a byte string.
pub fn spec_sha3_256(input: &[u8]) -> [u8; 32] {
    let mut sponge = spec_sponge_new(SHA3_256_RATE);
    spec_sponge_absorb(&mut sponge, input);
    let mut reader = spec_sponge_finalize_xof(&sponge, 0x06);
    let mut out = [0u8; 32];
    spec_xof_read(&mut reader, &mut out);
    out
}

/// One-shot SHAKE128 of a byte string, `N` output bytes.
pub fn spec_shake128<N: ArraySize>(input: &[u8]) -> Array<u8, N> {
    let mut sponge = spec_sponge_new(SHAKE128_RATE);
    spec_sponge_absorb(&mut sponge, input);
    let mut reader = spec_sponge_finalize_xof(&sponge, 0x1F);
    let mut out = Array::<u8, N>::default();
    spec_xof_read(&mut reader, &mut out);
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

            let mut sponge = spec_sponge_new(SHAKE128_RATE);
            spec_sponge_absorb(&mut sponge, input);
            let mut reader = spec_sponge_finalize_xof(&sponge, 0x1F);
            let mut got = [0u8; 400];
            spec_xof_read(&mut reader, &mut got);
            assert_eq!(got, expected, "SHAKE128 stream must match the sha3 crate");
        }
    }

    #[test]
    fn incremental_absorb_matches_one_shot() {
        // Absorb in odd-sized pieces; the stream must not depend on the split.
        let data: Vec<u8> = (0..500u32).map(|i| (i % 251) as u8).collect();
        let mut one_shot = spec_sponge_new(SHAKE128_RATE);
        spec_sponge_absorb(&mut one_shot, &data);
        let mut piecewise = spec_sponge_new(SHAKE128_RATE);
        // Piece boundaries at triangular-stepped offsets.
        let mut off = 0usize;
        for step in 0..data.len() {
            if off >= data.len() {
                break;
            }
            let take = ((step % 7) + 1).min(data.len() - off);
            spec_sponge_absorb(&mut piecewise, &data[off..off + take]);
            off += take;
        }
        let mut a = [0u8; 64];
        let mut b = [0u8; 64];
        spec_xof_read(&mut spec_sponge_finalize_xof(&one_shot, 0x1F), &mut a);
        spec_xof_read(&mut spec_sponge_finalize_xof(&piecewise, 0x1F), &mut b);
        assert_eq!(a, b);
    }
}
