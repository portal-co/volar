// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//! @volar-allow-vec: runtime-boundary: OT/VOLE/FAEST protocol material,
//! transcripts, and batched commitments are runtime-sized host protocol
//! buffers, not weaver-known compiled-program shapes; this module-level
//! exemption applies to the whole file.
//! Regular-indices MPCOT (Ferret §5, last paragraph).
//!
//! Noise is regular: exactly one puncture in each interval
//! `U_i = [i·n/t, (i+1)·n/t)`. Instantiated as `t` independent SPCOT calls
//! of length `n/t`, concatenated.

use alloc::vec::Vec;

use super::spcot::{
    Block, SpcotSenderMsg, spcot_choice_bits, spcot_receiver_extend, spcot_sender_extend,
};
use crate::SpecRng;

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
    debug_assert!(n % t == 0);
    let splen = n / t;
    let h = splen.trailing_zeros() as usize;
    debug_assert_eq!(cot_q.len(), t * h);
    debug_assert_eq!(choices.len(), t * h);

    let mut s = Vec::with_capacity(n);
    let mut blocks = Vec::with_capacity(t);
    for i in 0..t {
        let q = &cot_q[i * h..(i + 1) * h];
        let ch = &choices[i * h..(i + 1) * h];
        let (v, msg) = spcot_sender_extend(rng, delta, splen, q, ch);
        s.extend(v);
        blocks.push(msg);
    }
    (s, MpcotRegSenderMsg { blocks })
}

/// Regular MPCOT receiver. `alphas[i]` is the puncture inside interval `i`
/// (global index = `i·splen + alphas[i]`).
pub fn mpcot_reg_receiver(
    n: usize,
    t: usize,
    alphas: &[usize],
    cot_t: &[Block],
    msg: &MpcotRegSenderMsg,
) -> Vec<Block> {
    debug_assert_eq!(alphas.len(), t);
    let splen = n / t;
    let h = splen.trailing_zeros() as usize;
    let mut r = Vec::with_capacity(n);
    for i in 0..t {
        let t_rows = &cot_t[i * h..(i + 1) * h];
        let w = spcot_receiver_extend(alphas[i], splen, t_rows, &msg.blocks[i]);
        r.extend(w);
    }
    r
}

/// Build receiver choice bits for regular punctures `alphas`.
pub fn mpcot_reg_choice_bits(n: usize, t: usize, alphas: &[usize], cot_r: &[bool]) -> Vec<bool> {
    let splen = n / t;
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
    let splen = n / t;
    (0..t).map(|_| (rng.next_u32() as usize) % splen).collect()
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
}
