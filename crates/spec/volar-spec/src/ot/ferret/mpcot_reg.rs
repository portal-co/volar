// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//! Regular-indices MPCOT (Ferret §5, last paragraph).
//!
//! Noise is regular: exactly one puncture in each interval
//! `U_i = [floor(i·n/t), floor((i+1)·n/t))`. Each interval is padded to the
//! next power of two for its SPCOT call; padded leaves are discarded.
//! @volar-allow-vec: runtime-boundary: MPCOT rows, punctures, and batched
//! transcript material have sizes determined by runtime Ferret parameters.

use alloc::vec::Vec;

use super::spcot::{
    Block, KAPPA_BITS, SpcotSenderMsg, spcot_batched_fs_chis, spcot_batched_masked_choice,
    spcot_batched_receiver_hash_w, spcot_batched_sender_hash_v, spcot_choice_bits,
    spcot_receiver_extend, spcot_sender_extend,
};
use crate::SpecRng;

fn interval_bounds(n: usize, t: usize, index: usize) -> (usize, usize) {
    let n = n as u128;
    let t = t as u128;
    let index = index as u128;
    ((index * n / t) as usize, ((index + 1) * n / t) as usize)
}

fn padded_interval_len(n: usize, t: usize) -> usize {
    n.div_ceil(t)
        .checked_next_power_of_two()
        .expect("regular MPCOT interval length overflow")
}

fn sample_below<R: SpecRng>(rng: &mut R, upper: usize) -> usize {
    let upper = u32::try_from(upper).expect("regular MPCOT interval exceeds RNG range");
    let zone = u32::MAX - (u32::MAX % upper);
    loop {
        let value = rng.next_u32();
        if value < zone {
            return (value % upper) as usize;
        }
    }
}

/// Sender message: one SPCOT transcript per interval.
#[derive(Clone)]
pub struct MpcotRegSenderMsg {
    pub blocks: Vec<SpcotSenderMsg>,
}

/// Regular MPCOT sender. `cot_q` is `t · h` sender COT rows, grouped by interval.
pub fn mpcot_reg_sender<R: SpecRng>(
    rng: &mut R,
    delta: &Block,
    n: usize,
    t: usize,
    cot_q: &[Block],
    choices: &[bool],
) -> (Vec<Block>, MpcotRegSenderMsg) {
    debug_assert!(t > 0 && n >= t);
    let splen = padded_interval_len(n, t);
    let h = splen.trailing_zeros() as usize;
    debug_assert_eq!(cot_q.len(), t * h);
    debug_assert_eq!(choices.len(), t * h);

    let mut s = Vec::with_capacity(n);
    let mut blocks = Vec::with_capacity(t);
    for i in 0..t {
        let q = &cot_q[i * h..(i + 1) * h];
        let ch = &choices[i * h..(i + 1) * h];
        let (start, end) = interval_bounds(n, t, i);
        let (v, msg) = spcot_sender_extend(rng, delta, splen, q, ch);
        s.extend(v.into_iter().take(end - start));
        blocks.push(msg);
    }
    (s, MpcotRegSenderMsg { blocks })
}

/// Sender MPCOT outputs before each interval's padded leaves are discarded.
/// Malicious-consistency checks use these complete SPCOT vectors.
pub fn mpcot_reg_sender_padded<R: SpecRng>(
    rng: &mut R,
    delta: &Block,
    n: usize,
    t: usize,
    cot_q: &[Block],
    choices: &[bool],
) -> (Vec<Vec<Block>>, MpcotRegSenderMsg) {
    debug_assert!(t > 0 && n >= t);
    let splen = padded_interval_len(n, t);
    let h = splen.trailing_zeros() as usize;
    debug_assert_eq!(cot_q.len(), t * h);
    debug_assert_eq!(choices.len(), t * h);

    let mut outputs = Vec::with_capacity(t);
    let mut blocks = Vec::with_capacity(t);
    for i in 0..t {
        let q = &cot_q[i * h..(i + 1) * h];
        let ch = &choices[i * h..(i + 1) * h];
        let (v, msg) = spcot_sender_extend(rng, delta, splen, q, ch);
        outputs.push(v);
        blocks.push(msg);
    }
    (outputs, MpcotRegSenderMsg { blocks })
}

/// Regular MPCOT receiver. `alphas[i]` is the puncture's local offset inside
/// interval `i` (global index = `interval_bounds(n, t, i).0 + alphas[i]`).
pub fn mpcot_reg_receiver(
    n: usize,
    t: usize,
    alphas: &[usize],
    cot_t: &[Block],
    msg: &MpcotRegSenderMsg,
) -> Vec<Block> {
    debug_assert_eq!(alphas.len(), t);
    debug_assert!(t > 0 && n >= t);
    let splen = padded_interval_len(n, t);
    let h = splen.trailing_zeros() as usize;
    let mut r = Vec::with_capacity(n);
    for i in 0..t {
        let (start, end) = interval_bounds(n, t, i);
        let t_rows = &cot_t[i * h..(i + 1) * h];
        let w = spcot_receiver_extend(alphas[i], splen, t_rows, &msg.blocks[i]);
        r.extend(w.into_iter().take(end - start));
    }
    r
}

/// Receiver MPCOT outputs before each interval's padded leaves are discarded.
/// Malicious-consistency checks use these complete SPCOT vectors.
pub fn mpcot_reg_receiver_padded(
    n: usize,
    t: usize,
    alphas: &[usize],
    cot_t: &[Block],
    msg: &MpcotRegSenderMsg,
) -> Vec<Vec<Block>> {
    debug_assert_eq!(alphas.len(), t);
    debug_assert!(t > 0 && n >= t);
    let splen = padded_interval_len(n, t);
    let h = splen.trailing_zeros() as usize;
    let mut outputs = Vec::with_capacity(t);
    for i in 0..t {
        let t_rows = &cot_t[i * h..(i + 1) * h];
        outputs.push(spcot_receiver_extend(
            alphas[i],
            splen,
            t_rows,
            &msg.blocks[i],
        ));
    }
    outputs
}

/// Flatten padded MPCOT buckets, discarding leaves outside each regular interval.
pub fn mpcot_reg_flatten_outputs(n: usize, t: usize, buckets: &[Vec<Block>]) -> Vec<Block> {
    debug_assert_eq!(buckets.len(), t);
    let mut output = Vec::with_capacity(n);
    for (index, bucket) in buckets.iter().enumerate() {
        let (start, end) = interval_bounds(n, t, index);
        debug_assert!(bucket.len() >= end - start);
        output.extend(bucket.iter().take(end - start).copied());
    }
    output
}

/// Build receiver choice bits for regular punctures `alphas`.
pub fn mpcot_reg_choice_bits(n: usize, t: usize, alphas: &[usize], cot_r: &[bool]) -> Vec<bool> {
    debug_assert!(t > 0 && n >= t);
    let splen = padded_interval_len(n, t);
    let h = splen.trailing_zeros() as usize;
    let mut out = Vec::with_capacity(t * h);
    for i in 0..t {
        let r = &cot_r[i * h..(i + 1) * h];
        out.extend(spcot_choice_bits(alphas[i], h, r));
    }
    out
}

/// Sample a regular weight-`t` noise vector: one uniform index per interval.
pub fn sample_regular_noise<R: SpecRng>(rng: &mut R, n: usize, t: usize) -> Vec<usize> {
    debug_assert!(t > 0 && n >= t);
    (0..t)
        .map(|i| {
            let (start, end) = interval_bounds(n, t, i);
            sample_below(rng, end - start)
        })
        .collect()
}

/// Batched consistency check over the emitted (unpadded) regular-MPCOT
/// outputs. Prefer [`mpcot_reg_consistency_check_padded`] when the full SPCOT
/// vectors are available, so discarded leaves are checked too.
pub fn mpcot_reg_consistency_check(
    delta: &Block,
    s: &[Block],
    r: &[Block],
    alphas: &[usize],
    extra_q: &[Block],
    extra_r: &[bool],
    extra_t: &[Block],
    transcript: &[u8],
) -> bool {
    let t = alphas.len();
    if t == 0 || s.len() != r.len() || s.len() < t {
        return false;
    }
    let n = s.len();
    let mut vs = Vec::with_capacity(t);
    let mut ws = Vec::with_capacity(t);
    for (index, &alpha) in alphas.iter().enumerate() {
        let (start, end) = interval_bounds(n, t, index);
        if alpha >= end - start {
            return false;
        }
        vs.push(&s[start..end]);
        ws.push(&r[start..end]);
    }
    mpcot_reg_consistency_check_slices(
        delta, &vs, &ws, alphas, extra_q, extra_r, extra_t, transcript,
    )
}

/// Malicious-security batched check over complete, padded SPCOT executions.
/// This checks even leaves discarded from the logical regular-MPCOT output.
pub fn mpcot_reg_consistency_check_padded(
    delta: &Block,
    s: &[Vec<Block>],
    r: &[Vec<Block>],
    alphas: &[usize],
    extra_q: &[Block],
    extra_r: &[bool],
    extra_t: &[Block],
    transcript: &[u8],
) -> bool {
    let Some(first) = s.first() else {
        return false;
    };
    if !first.len().is_power_of_two()
        || s.iter().any(|bucket| bucket.len() != first.len())
        || r.iter().any(|bucket| bucket.len() != first.len())
    {
        return false;
    }
    let vs: Vec<&[Block]> = s.iter().map(Vec::as_slice).collect();
    let ws: Vec<&[Block]> = r.iter().map(Vec::as_slice).collect();
    mpcot_reg_consistency_check_slices(
        delta, &vs, &ws, alphas, extra_q, extra_r, extra_t, transcript,
    )
}

fn mpcot_reg_consistency_check_slices(
    delta: &Block,
    vs: &[&[Block]],
    ws: &[&[Block]],
    alphas: &[usize],
    extra_q: &[Block],
    extra_r: &[bool],
    extra_t: &[Block],
    transcript: &[u8],
) -> bool {
    let t = alphas.len();
    if t == 0
        || vs.len() != t
        || ws.len() != t
        || extra_q.len() != KAPPA_BITS
        || extra_r.len() != KAPPA_BITS
        || extra_t.len() != KAPPA_BITS
    {
        return false;
    }
    let mut lens = Vec::with_capacity(t);
    for (&alpha, (v, w)) in alphas.iter().zip(vs.iter().zip(ws)) {
        if v.len() != w.len() || alpha >= v.len() {
            return false;
        }
        lens.push(v.len());
    }
    let chis = spcot_batched_fs_chis(&lens, transcript);
    let x_star_prime = spcot_batched_masked_choice(alphas, extra_r, &chis);
    let hv = spcot_batched_sender_hash_v(delta, vs, extra_q, &x_star_prime, &chis);
    let hw = spcot_batched_receiver_hash_w(ws, extra_t, &chis);
    hv == hw
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::SpecRng;
    use crate::ot::ferret::spcot::spcot_in_process;

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
        let mut o = [0u8; 16];
        for i in 0..16 {
            o[i] = a[i] ^ b[i];
        }
        o
    }

    #[test]
    fn regular_mpcot_is_t_independent_spcots() {
        // Sanity: concatenating two n=8 SPCOTs matches two punctures.
        let mut rng = TestRng(0x1111);
        let mut delta = [0u8; 16];
        for b in &mut delta {
            *b = rng.next_u32() as u8;
        }
        let (v0, w0) = spcot_in_process(&mut rng, &delta, 8, 3);
        let (v1, w1) = spcot_in_process(&mut rng, &delta, 8, 1);
        for i in 0..8 {
            let expect = if i == 3 { delta } else { [0u8; 16] };
            assert_eq!(xor_block(&v0[i], &w0[i]), expect);
        }
        for i in 0..8 {
            let expect = if i == 1 { delta } else { [0u8; 16] };
            assert_eq!(xor_block(&v1[i], &w1[i]), expect);
        }
    }

    #[test]
    fn regular_mpcot_relation_supports_uneven_padded_intervals() {
        let mut rng = TestRng(0x3737);
        let n = 37;
        let t = 7;
        let h = padded_interval_len(n, t).trailing_zeros() as usize;
        let mut delta = [0u8; 16];
        for b in &mut delta {
            *b = rng.next_u32() as u8;
        }
        delta[0] |= 1;
        let mut cot_q = Vec::new();
        let mut cot_r = Vec::new();
        let mut cot_t = Vec::new();
        for _ in 0..(t * h) {
            let mut row = [0u8; 16];
            for chunk in row.chunks_mut(4) {
                chunk.copy_from_slice(&rng.next_u32().to_le_bytes()[..chunk.len()]);
            }
            let bit = (rng.next_u32() & 1) == 1;
            cot_q.push(row);
            cot_r.push(bit);
            cot_t.push(if bit { xor_block(&row, &delta) } else { row });
        }
        let alphas = sample_regular_noise(&mut rng, n, t);
        let choices = mpcot_reg_choice_bits(n, t, &alphas, &cot_r);
        let (sender_padded, msg) =
            mpcot_reg_sender_padded(&mut rng, &delta, n, t, &cot_q, &choices);
        let receiver_padded = mpcot_reg_receiver_padded(n, t, &alphas, &cot_t, &msg);
        let sender = mpcot_reg_flatten_outputs(n, t, &sender_padded);
        let receiver = mpcot_reg_flatten_outputs(n, t, &receiver_padded);
        assert_eq!(sender.len(), n);
        assert_eq!(receiver.len(), n);
        for i in 0..t {
            let (start, end) = interval_bounds(n, t, i);
            assert!(alphas[i] < end - start);
            for index in start..end {
                let expected = if index == start + alphas[i] {
                    delta
                } else {
                    [0u8; 16]
                };
                assert_eq!(xor_block(&sender[index], &receiver[index]), expected);
            }
        }

        let mut extra_q = Vec::with_capacity(KAPPA_BITS);
        let mut extra_r = Vec::with_capacity(KAPPA_BITS);
        let mut extra_t = Vec::with_capacity(KAPPA_BITS);
        for _ in 0..KAPPA_BITS {
            let mut row = [0u8; 16];
            for chunk in row.chunks_mut(4) {
                chunk.copy_from_slice(&rng.next_u32().to_le_bytes()[..chunk.len()]);
            }
            let bit = (rng.next_u32() & 1) == 1;
            extra_q.push(row);
            extra_r.push(bit);
            extra_t.push(if bit { xor_block(&row, &delta) } else { row });
        }
        assert!(mpcot_reg_consistency_check(
            &delta, &sender, &receiver, &alphas, &extra_q, &extra_r, &extra_t, b"uneven"
        ));
        assert!(mpcot_reg_consistency_check_padded(
            &delta,
            &sender_padded,
            &receiver_padded,
            &alphas,
            &extra_q,
            &extra_r,
            &extra_t,
            b"uneven-padded"
        ));
        let mut bad_padded_receiver = receiver_padded.clone();
        bad_padded_receiver[0][7][0] ^= 1;
        assert!(!mpcot_reg_consistency_check_padded(
            &delta,
            &sender_padded,
            &bad_padded_receiver,
            &alphas,
            &extra_q,
            &extra_r,
            &extra_t,
            b"uneven-padded"
        ));
        let mut bad_receiver = receiver.clone();
        let (start, _) = interval_bounds(n, t, 1);
        bad_receiver[start + alphas[1]][0] ^= 1;
        assert!(!mpcot_reg_consistency_check(
            &delta,
            &sender,
            &bad_receiver,
            &alphas,
            &extra_q,
            &extra_r,
            &extra_t,
            b"uneven"
        ));
    }

    #[test]
    fn regular_mpcot_relation_weight_t() {
        let mut rng = TestRng(0x2222);
        let n = 32;
        let t = 4;
        let splen: usize = n / t;
        let h = splen.trailing_zeros() as usize;
        let mut delta = [0u8; 16];
        for b in &mut delta {
            *b = rng.next_u32() as u8;
        }
        delta[0] |= 1;
        let mut cot_q = Vec::new();
        let mut cot_r = Vec::new();
        let mut cot_t = Vec::new();
        for _ in 0..(t * h) {
            let mut row = [0u8; 16];
            for chunk in row.chunks_mut(4) {
                chunk.copy_from_slice(&rng.next_u32().to_le_bytes()[..chunk.len()]);
            }
            let bit = (rng.next_u32() & 1) == 1;
            cot_q.push(row);
            cot_r.push(bit);
            cot_t.push(if bit { xor_block(&row, &delta) } else { row });
        }
        let alphas = sample_regular_noise(&mut rng, n, t);
        let choices = mpcot_reg_choice_bits(n, t, &alphas, &cot_r);
        let (s, msg) = mpcot_reg_sender(&mut rng, &delta, n, t, &cot_q, &choices);
        let r = mpcot_reg_receiver(n, t, &alphas, &cot_t, &msg);
        for i in 0..t {
            for j in 0..splen {
                let idx = i * splen + j;
                let expect = if j == alphas[i] { delta } else { [0u8; 16] };
                assert_eq!(xor_block(&s[idx], &r[idx]), expect, "i={i} j={j}");
            }
        }
    }

    #[test]
    fn mpcot_reg_batched_consistency_check() {
        let mut rng = TestRng(0x3333);
        let n = 32;
        let t = 4;
        let splen: usize = n / t;
        let h = splen.trailing_zeros() as usize;
        let mut delta = [0u8; 16];
        for b in &mut delta {
            *b = rng.next_u32() as u8;
        }
        delta[0] |= 1;
        // COTs for the SPCOT GGM + the κ extra COTs for the consistency check.
        let mut cot_q = Vec::new();
        let mut cot_r = Vec::new();
        let mut cot_t = Vec::new();
        for _ in 0..(t * h) {
            let mut row = [0u8; 16];
            for chunk in row.chunks_mut(4) {
                chunk.copy_from_slice(&rng.next_u32().to_le_bytes()[..chunk.len()]);
            }
            let bit = (rng.next_u32() & 1) == 1;
            cot_q.push(row);
            cot_r.push(bit);
            cot_t.push(if bit { xor_block(&row, &delta) } else { row });
        }
        let mut eq = Vec::new();
        let mut er = Vec::new();
        let mut et = Vec::new();
        for _ in 0..128 {
            let mut row = [0u8; 16];
            for chunk in row.chunks_mut(4) {
                chunk.copy_from_slice(&rng.next_u32().to_le_bytes()[..chunk.len()]);
            }
            let bit = (rng.next_u32() & 1) == 1;
            eq.push(row);
            er.push(bit);
            et.push(if bit { xor_block(&row, &delta) } else { row });
        }
        let alphas = sample_regular_noise(&mut rng, n, t);
        let choices = mpcot_reg_choice_bits(n, t, &alphas, &cot_r);
        let (s, msg) = mpcot_reg_sender(&mut rng, &delta, n, t, &cot_q, &choices);
        let r = mpcot_reg_receiver(n, t, &alphas, &cot_t, &msg);
        // Honest MPCOT passes the batched consistency check.
        assert!(mpcot_reg_consistency_check(
            &delta, &s, &r, &alphas, &eq, &er, &et, b"mpcot"
        ));
        // A receiver whose w deviates in one SPCOT fails the batched check.
        let mut bad_r = r.clone();
        bad_r[splen + alphas[1]][0] ^= 1;
        assert!(!mpcot_reg_consistency_check(
            &delta, &s, &bad_r, &alphas, &eq, &er, &et, b"mpcot"
        ));
    }
}
