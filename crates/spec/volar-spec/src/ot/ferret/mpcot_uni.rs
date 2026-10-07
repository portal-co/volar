// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//! Uniform-noise MPCOT via Cuckoo hashing (Ferret Fig. 7).
//!
//! Cuckoo parameters follow §7.1: `m = 1.5 t`, `τ = 3`. Hash functions are
//! SHA3-derived (stand-in for the paper's AES-128 permutation). Empty bins
//! puncture the extra SPCOT cell (`|B_j| + 1`).

use alloc::vec::Vec;

use digest::Digest;
use sha3::Sha3_256;

use super::params::FerretParams;
use super::spcot::{
    Block, KAPPA_BITS, SpcotSenderMsg, spcot_choice_bits, spcot_receiver_extend,
    spcot_sender_extend,
};
use crate::SpecRng;

/// Number of Cuckoo hash functions `τ`.
pub const TAU: usize = 3;

/// `m = ceil(1.5 t)`.
pub fn cuckoo_table_size(t: usize) -> usize {
    t.saturating_mul(3).div_ceil(2).max(t + 1)
}

fn hash_i(seed: &[u8; 16], i: usize, x: usize, m: usize) -> usize {
    let mut h = Sha3_256::new();
    h.update(b"ferret-cuckoo-h-v1");
    h.update(seed);
    h.update((i as u64).to_le_bytes());
    h.update((x as u64).to_le_bytes());
    let out = h.finalize();
    let raw = u64::from_le_bytes(out[..8].try_into().unwrap());
    (raw as usize) % m
}

/// Distinct bins `h_i(x)` (duplicate hash collisions counted once).
fn unique_bins(seed: &[u8; 16], x: usize, m: usize) -> Vec<usize> {
    let mut js = Vec::with_capacity(TAU);
    for i in 0..TAU {
        let j = hash_i(seed, i, x, m);
        if !js.contains(&j) {
            js.push(j);
        }
    }
    js
}

/// Insert `t` points into a Cuckoo table of size `m`. Failed inserts are
/// dropped (paper: ignore with probability `2^{-ρ}`).
pub fn cuckoo_insert(seed: &[u8; 16], n: usize, t: usize, points: &[usize]) -> Vec<Option<usize>> {
    let m = cuckoo_table_size(t);
    let mut table = alloc::vec![None; m];
    let max_kicks = 8 * m.max(16);
    for &item in points {
        debug_assert!(item < n);
        let mut x = item;
        for kick in 0..max_kicks {
            let mut placed = false;
            for i in 0..TAU {
                let j = hash_i(seed, i, x, m);
                if table[j].is_none() {
                    table[j] = Some(x);
                    placed = true;
                    break;
                }
            }
            if placed {
                break;
            }
            let i = kick % TAU;
            let j = hash_i(seed, i, x, m);
            x = table[j].replace(x).unwrap();
        }
        // Insertion failure: the last kicked occupant is dropped.
    }
    table
}

/// Bucket `B_j = { x ∈ [n] | ∃ i : h_i(x) = j }`, sorted.
pub fn build_buckets(seed: &[u8; 16], n: usize, m: usize) -> Vec<Vec<usize>> {
    let mut buckets = alloc::vec![Vec::new(); m];
    for x in 0..n {
        for i in 0..TAU {
            let j = hash_i(seed, i, x, m);
            buckets[j].push(x);
        }
    }
    for b in &mut buckets {
        b.sort_unstable();
        b.dedup();
    }
    buckets
}

/// Fig. 7 sender message: one SPCOT per bucket (length `|B_j|+1`).
#[derive(Clone)]
pub struct MpcotUniSenderMsg {
    pub hash_seed: [u8; 16],
    pub blocks: Vec<SpcotSenderMsg>,
}

fn next_pow2(x: usize) -> usize {
    x.next_power_of_two().max(2)
}

/// Uniform MPCOT sender. Pads each bucket+1 up to a power of two for SPCOT.
/// Returns the final (XOR-combined) `s`, the per-bucket SPCOT outputs `s_bins`
/// (needed for the malicious consistency check), and the sender message.
pub fn mpcot_uni_sender<R: SpecRng>(
    rng: &mut R,
    delta: &Block,
    params: FerretParams,
    hash_seed: [u8; 16],
    cot_q_chunks: &[Vec<Block>],
    choices_chunks: &[Vec<bool>],
) -> (Vec<Block>, Vec<Vec<Block>>, MpcotUniSenderMsg) {
    let n = params.n;
    let m = cuckoo_table_size(params.t);
    let buckets = build_buckets(&hash_seed, n, m);
    debug_assert_eq!(cot_q_chunks.len(), m);

    let mut s_bins = Vec::with_capacity(m);
    let mut blocks = Vec::with_capacity(m);
    for j in 0..m {
        let need = buckets[j].len() + 1;
        let splen = next_pow2(need);
        let (v, msg) = spcot_sender_extend(rng, delta, splen, &cot_q_chunks[j], &choices_chunks[j]);
        s_bins.push(v);
        blocks.push(msg);
    }

    let mut s = alloc::vec![[0u8; 16]; n];
    for x in 0..n {
        let mut acc = [0u8; 16];
        for j in unique_bins(&hash_seed, x, m) {
            let pos = buckets[j].iter().position(|&y| y == x).unwrap();
            for b in 0..16 {
                acc[b] ^= s_bins[j][pos][b];
            }
        }
        s[x] = acc;
    }
    (s, s_bins, MpcotUniSenderMsg { hash_seed, blocks })
}

/// Uniform MPCOT receiver. Returns the final (XOR-combined) `r` and the
/// per-bucket SPCOT outputs `r_bins` (needed for the consistency check).
pub fn mpcot_uni_receiver(
    params: FerretParams,
    table: &[Option<usize>],
    cot_t_chunks: &[Vec<Block>],
    msg: &MpcotUniSenderMsg,
) -> (Vec<Block>, Vec<Vec<Block>>) {
    let n = params.n;
    let m = cuckoo_table_size(params.t);
    let buckets = build_buckets(&msg.hash_seed, n, m);
    let mut r_bins = Vec::with_capacity(m);
    for j in 0..m {
        let need = buckets[j].len() + 1;
        let splen = next_pow2(need);
        let p = match table[j] {
            None => buckets[j].len(), // extra cell; padded index still < splen
            Some(val) => buckets[j].iter().position(|&y| y == val).unwrap(),
        };
        let w = spcot_receiver_extend(p, splen, &cot_t_chunks[j], &msg.blocks[j]);
        r_bins.push(w);
    }
    let mut r = alloc::vec![[0u8; 16]; n];
    for x in 0..n {
        let mut acc = [0u8; 16];
        for j in unique_bins(&msg.hash_seed, x, m) {
            let pos = buckets[j].iter().position(|&y| y == x).unwrap();
            for b in 0..16 {
                acc[b] ^= r_bins[j][pos][b];
            }
        }
        r[x] = acc;
    }
    (r, r_bins)
}

/// Per-bucket `(puncture index α, padded SPCOT length)` for the consistency
/// check: bucket `j`'s puncture is the Cuckoo-table value's position (or the
/// extra cell `|B_j|` when the bucket is empty), and the length is
/// `next_pow2(|B_j| + 1)`.
pub fn mpcot_uni_bucket_params(
    params: FerretParams,
    hash_seed: &[u8; 16],
    table: &[Option<usize>],
) -> (Vec<usize>, Vec<usize>) {
    let n = params.n;
    let m = cuckoo_table_size(params.t);
    let buckets = build_buckets(hash_seed, n, m);
    let mut alphas = Vec::with_capacity(m);
    let mut lens = Vec::with_capacity(m);
    for j in 0..m {
        let need = buckets[j].len() + 1;
        let splen = next_pow2(need);
        let p = match table[j] {
            None => buckets[j].len(),
            Some(val) => buckets[j].iter().position(|&y| y == val).unwrap(),
        };
        alphas.push(p);
        lens.push(splen);
    }
    (alphas, lens)
}

/// Batched malicious-security consistency check over the `m` Cuckoo-bucket
/// SPCOT executions (Ferret-Uni Fig. 6 steps 6–9 batched). Each bucket's bins
/// (`s_bins` / `r_bins`) carry that bucket's padded SPCOT output; `alphas` and
/// `lens` come from [`mpcot_uni_bucket_params`].
pub fn mpcot_uni_consistency_check(
    delta: &Block,
    s_bins: &[Vec<Block>],
    r_bins: &[Vec<Block>],
    alphas: &[usize],
    lens: &[usize],
    extra_q: &[Block],
    extra_r: &[bool],
    extra_t: &[Block],
    transcript: &[u8],
) -> bool {
    use super::spcot::{
        spcot_batched_fs_chis, spcot_batched_masked_choice, spcot_batched_receiver_hash_w,
        spcot_batched_sender_hash_v,
    };
    debug_assert_eq!(s_bins.len(), r_bins.len());
    debug_assert_eq!(s_bins.len(), alphas.len());
    debug_assert_eq!(extra_q.len(), KAPPA_BITS);
    debug_assert_eq!(extra_r.len(), KAPPA_BITS);
    debug_assert_eq!(extra_t.len(), KAPPA_BITS);
    let chis = spcot_batched_fs_chis(lens, transcript);
    let x_star_prime = spcot_batched_masked_choice(alphas, extra_r, &chis);
    let vs: Vec<&[Block]> = s_bins.iter().map(|b| b.as_slice()).collect();
    let ws: Vec<&[Block]> = r_bins.iter().map(|b| b.as_slice()).collect();
    let hv = spcot_batched_sender_hash_v(delta, &vs, extra_q, &x_star_prime, &chis);
    let hw = spcot_batched_receiver_hash_w(&ws, extra_t, &chis);
    hv == hw
}

/// Choice bits per bucket for the receiver's Cuckoo table.
pub fn mpcot_uni_choice_bits(
    params: FerretParams,
    hash_seed: &[u8; 16],
    table: &[Option<usize>],
    cot_r_chunks: &[Vec<bool>],
) -> Vec<Vec<bool>> {
    let n = params.n;
    let m = cuckoo_table_size(params.t);
    let buckets = build_buckets(hash_seed, n, m);
    let mut out = Vec::with_capacity(m);
    for j in 0..m {
        let need = buckets[j].len() + 1;
        let splen = next_pow2(need);
        let h = splen.trailing_zeros() as usize;
        let p = match table[j] {
            None => buckets[j].len(),
            Some(val) => buckets[j].iter().position(|&y| y == val).unwrap(),
        };
        out.push(spcot_choice_bits(p, h, &cot_r_chunks[j]));
    }
    out
}

/// SPCOT height per Cuckoo bin: `log2(next_pow2(|B_j|+1))`.
pub fn uni_spcot_heights(hash_seed: &[u8; 16], n: usize, t: usize) -> Vec<usize> {
    let m = cuckoo_table_size(t);
    let buckets = build_buckets(hash_seed, n, m);
    buckets
        .iter()
        .map(|b| next_pow2(b.len() + 1).trailing_zeros() as usize)
        .collect()
}

/// Seed COTs for one Uni iteration: `k + ∑ h_j`.
pub fn uni_seed_cot_count(hash_seed: &[u8; 16], params: FerretParams) -> usize {
    params.k
        + uni_spcot_heights(hash_seed, params.n, params.t)
            .iter()
            .copied()
            .sum::<usize>()
}

/// Seed COTs for one malicious-secure Uni iteration: `k + ∑ h_j + κ` (the κ
/// extra COTs for the batched consistency check).
pub fn uni_seed_cot_count_malicious(hash_seed: &[u8; 16], params: FerretParams) -> usize {
    uni_seed_cot_count(hash_seed, params) + KAPPA_BITS
}

/// Sample `t` distinct uniform points in `[n)` (sorted).
pub fn sample_uniform_points<R: SpecRng>(rng: &mut R, n: usize, t: usize) -> Vec<usize> {
    let mut pts = Vec::with_capacity(t);
    while pts.len() < t {
        let x = (rng.next_u32() as usize) % n;
        if !pts.contains(&x) {
            pts.push(x);
        }
    }
    pts.sort_unstable();
    pts
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ot::ferret::params::FERRET_UNI_TOY;
    use crate::ot::ferret::spcot::KAPPA_BYTES;

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

    fn xor_block(a: &Block, b: &Block) -> Block {
        let mut o = [0u8; KAPPA_BYTES];
        for i in 0..KAPPA_BYTES {
            o[i] = a[i] ^ b[i];
        }
        o
    }

    fn sample_block(rng: &mut TestRng) -> Block {
        let mut b = [0u8; KAPPA_BYTES];
        for chunk in b.chunks_mut(4) {
            chunk.copy_from_slice(&rng.next_u32().to_le_bytes()[..chunk.len()]);
        }
        b
    }

    #[test]
    fn cuckoo_uni_mpcot_relation_toy() {
        let mut rng = TestRng(0x5151_5151);
        let p = FERRET_UNI_TOY;
        let mut delta = sample_block(&mut rng);
        // Avoid all-zero Δ so a missed puncture is visible.
        delta[0] |= 1;
        let mut hash_seed = [0u8; 16];
        for chunk in hash_seed.chunks_mut(4) {
            chunk.copy_from_slice(&rng.next_u32().to_le_bytes()[..chunk.len()]);
        }
        let points = sample_uniform_points(&mut rng, p.n, p.t);
        let table = cuckoo_insert(&hash_seed, p.n, p.t, &points);
        let m = cuckoo_table_size(p.t);
        let buckets = build_buckets(&hash_seed, p.n, m);

        let mut cot_q = Vec::with_capacity(m);
        let mut cot_r = Vec::with_capacity(m);
        let mut cot_t = Vec::with_capacity(m);
        for j in 0..m {
            let splen = next_pow2(buckets[j].len() + 1);
            let h = splen.trailing_zeros() as usize;
            let mut q = Vec::with_capacity(h);
            let mut rbits = Vec::with_capacity(h);
            let mut trows = Vec::with_capacity(h);
            for _ in 0..h {
                let row = sample_block(&mut rng);
                let bit = (rng.next_u32() & 1) == 1;
                let tw = if bit { xor_block(&row, &delta) } else { row };
                q.push(row);
                rbits.push(bit);
                trows.push(tw);
            }
            cot_q.push(q);
            cot_r.push(rbits);
            cot_t.push(trows);
        }
        let choices = mpcot_uni_choice_bits(p, &hash_seed, &table, &cot_r);
        let (s, _s_bins, msg) = mpcot_uni_sender(&mut rng, &delta, p, hash_seed, &cot_q, &choices);
        let (r, _r_bins) = mpcot_uni_receiver(p, &table, &cot_t, &msg);

        let mut expected_e = alloc::vec![false; p.n];
        for slot in &table {
            if let Some(x) = slot {
                expected_e[*x] = true;
            }
        }
        for x in 0..p.n {
            let diff = xor_block(&s[x], &r[x]);
            if expected_e[x] {
                assert_eq!(diff, delta, "puncture {x}");
            } else {
                assert_eq!(diff, [0u8; 16], "off-path {x}");
            }
        }
    }
}
