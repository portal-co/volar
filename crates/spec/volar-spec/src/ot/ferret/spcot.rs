// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//! Single-point correlated OT (Ferret Fig. 6, ΠSPCOT).
//!
//! Semi-honest path: steps 2–5. Malicious path adds the Fiat–Shamir form of
//! the consistency check (steps 6–9, §4.2). `G` is
//! [`AesCtrLengthDoubler`](crate::faest::prg::AesCtrLengthDoubler).

use alloc::vec::Vec;

use digest::Digest;
use hybrid_array::{Array, sizes::U16};
use sha3::Sha3_256;

use crate::byte_gen::LengthDoubler;
use crate::faest::prg::AesCtrLengthDoubler;
use crate::SpecRng;

/// `κ = 128` bits.
pub const KAPPA_BITS: usize = 128;
/// `κ/8`.
pub const KAPPA_BYTES: usize = 16;

/// A `κ`-bit field element / GGM node.
pub type Block = [u8; KAPPA_BYTES];

/// Sender → receiver: encrypted layer sums and the leaf-sum correction.
#[derive(Clone)]
pub struct SpcotSenderMsg {
    /// `M^i_0, M^i_1` for `i = 1..h` (Fig. 6 step 4).
    pub ms: Vec<[Block; 2]>,
    /// `c = Δ + ∑ v[i]` (Fig. 6 step 4).
    pub c: Block,
    /// Fiat–Shamir `H'(V)` when `malicious`; empty otherwise.
    pub hash_v: Vec<u8>,
}

fn xor_block(a: &Block, b: &Block) -> Block {
    let mut o = [0u8; KAPPA_BYTES];
    for i in 0..KAPPA_BYTES {
        o[i] = a[i] ^ b[i];
    }
    o
}

fn g_double(seed: Block) -> (Block, Block) {
    let [left, right] = AesCtrLengthDoubler::double(Array::<u8, U16>(seed));
    (left.0, right.0)
}

fn crhf(input: &Block, tweak: u64) -> Block {
    let mut h = Sha3_256::new();
    h.update(b"ferret-spcot-h-v1");
    h.update(input);
    h.update(tweak.to_le_bytes());
    let out = h.finalize();
    let mut b = [0u8; KAPPA_BYTES];
    b.copy_from_slice(&out[..KAPPA_BYTES]);
    b
}

fn hash_prime(v: &Block) -> [u8; 32] {
    let mut h = Sha3_256::new();
    h.update(b"ferret-spcot-hp-v1");
    h.update(v);
    let out = h.finalize();
    let mut b = [0u8; 32];
    b.copy_from_slice(&out);
    b
}

fn sample_block<R: SpecRng>(rng: &mut R) -> Block {
    let mut b = [0u8; KAPPA_BYTES];
    for chunk in b.chunks_mut(4) {
        let x = rng.next_u32().to_le_bytes();
        chunk.copy_from_slice(&x[..chunk.len()]);
    }
    b
}

fn bit_msb(alpha: usize, h: usize, i: usize) -> bool {
    // i in 0..h is the depth from the root; bit 0 is the high bit of `alpha`.
    ((alpha >> (h - 1 - i)) & 1) == 1
}

/// Receiver's Fig. 6 step-4 choice bits: `b_i = r_i ⊕ α_i ⊕ 1`.
pub fn spcot_choice_bits(alpha: usize, h: usize, cot_r: &[bool]) -> Vec<bool> {
    debug_assert_eq!(cot_r.len(), h);
    (0..h)
        .map(|i| cot_r[i] ^ bit_msb(alpha, h, i) ^ true)
        .collect()
}

fn expand_full(depth: usize, seed: Block) -> (Vec<Block>, Vec<[Block; 2]>) {
    // `nodes[level][j]` for level 0..=depth, 2^level nodes.
    let mut level: Vec<Block> = alloc::vec![seed];
    let mut sums = Vec::with_capacity(depth);
    for _d in 0..depth {
        let mut next = Vec::with_capacity(level.len() * 2);
        let mut k0 = [0u8; KAPPA_BYTES];
        let mut k1 = [0u8; KAPPA_BYTES];
        for node in &level {
            let (l, r) = g_double(*node);
            for i in 0..KAPPA_BYTES {
                k0[i] ^= l[i];
                k1[i] ^= r[i];
            }
            next.push(l);
            next.push(r);
        }
        sums.push([k0, k1]);
        level = next;
    }
    (level, sums)
}

/// Expand a punctured tree from off-path layer sums (mpz `GgmTree::new_partial`).
fn recover_sibling(layer: &mut [Block], sum: Block, offset: usize, select: bool) {
    let on = offset + select as usize;
    let off = offset + (!select) as usize;
    if on < layer.len() {
        layer[on] = [0u8; KAPPA_BYTES];
    }
    if off < layer.len() {
        layer[off] = [0u8; KAPPA_BYTES];
    }
    let mut value = sum;
    let mut i = (!select) as usize;
    while i < layer.len() {
        value = xor_block(&value, &layer[i]);
        i += 2;
    }
    layer[off] = value;
}

fn expand_partial(depth: usize, sums: &[Block], alpha: usize) -> Vec<Block> {
    debug_assert_eq!(sums.len(), depth);
    let n = 1usize << depth;
    // Level 1 has 2 nodes; we grow until `n` leaves.
    let mut level: Vec<Block> = alloc::vec![[0u8; KAPPA_BYTES]; 2];
    let mut offset = 0usize;
    for d in 0..depth {
        let select = bit_msb(alpha, depth, d);
        recover_sibling(&mut level, sums[d], offset, select);
        if d + 1 == depth {
            break;
        }
        let mut next = alloc::vec![[0u8; KAPPA_BYTES]; level.len() * 2];
        for (j, node) in level.iter().enumerate() {
            let (l, r) = g_double(*node);
            next[2 * j] = l;
            next[2 * j + 1] = r;
        }
        offset = (offset + select as usize) << 1;
        level = next;
    }
    debug_assert_eq!(level.len(), n);
    level
}

/// Sender Fig. 6 steps 3–4. `cot_q[i]` is the sender's COT row `q_i`.
///
/// `choices[i]` is the receiver's `b_i`. Returns leaves `v` and the
/// ciphertext / correction message.
pub fn spcot_sender_extend<R: SpecRng>(
    rng: &mut R,
    delta: &Block,
    n: usize,
    cot_q: &[Block],
    choices: &[bool],
) -> (Vec<Block>, SpcotSenderMsg) {
    debug_assert!(n.is_power_of_two());
    let h = n.trailing_zeros() as usize;
    debug_assert_eq!(cot_q.len(), h);
    debug_assert_eq!(choices.len(), h);

    let seed = sample_block(rng);
    let (leaves, layer_sums) = expand_full(h, seed);

    let mut ms = Vec::with_capacity(h);
    for i in 0..h {
        let b = choices[i];
        let q = cot_q[i];
        let q_xor_delta = xor_block(&q, delta);
        let (k0, k1) = if b {
            (q_xor_delta, q)
        } else {
            (q, q_xor_delta)
        };
        let tweak = i as u64;
        ms.push([
            xor_block(&layer_sums[i][0], &crhf(&k0, tweak)),
            xor_block(&layer_sums[i][1], &crhf(&k1, tweak)),
        ]);
    }

    let mut c = *delta;
    for leaf in &leaves {
        c = xor_block(&c, leaf);
    }

    (
        leaves,
        SpcotSenderMsg {
            ms,
            c,
            hash_v: Vec::new(),
        },
    )
}

/// Receiver Fig. 6 step 5. `cot_t[i]` is `t_i = q_i ⊕ r_i Δ`.
pub fn spcot_receiver_extend(
    alpha: usize,
    n: usize,
    cot_t: &[Block],
    msg: &SpcotSenderMsg,
) -> Vec<Block> {
    debug_assert!(n.is_power_of_two());
    let h = n.trailing_zeros() as usize;
    debug_assert!(alpha < n);
    debug_assert_eq!(cot_t.len(), h);
    debug_assert_eq!(msg.ms.len(), h);

    let mut off_sums = Vec::with_capacity(h);
    for i in 0..h {
        let select = bit_msb(alpha, h, i);
        let t = cot_t[i];
        let tweak = i as u64;
        let ht = crhf(&t, tweak);
        // Off-path: α_i = 0 → decrypt M_1; α_i = 1 → decrypt M_0.
        let m = if select { msg.ms[i][0] } else { msg.ms[i][1] };
        off_sums.push(xor_block(&m, &ht));
    }

    let mut w = expand_partial(h, &off_sums, alpha);
    let mut acc = msg.c;
    for (i, wi) in w.iter().enumerate() {
        if i != alpha {
            acc = xor_block(&acc, wi);
        }
    }
    w[alpha] = acc;
    w
}

/// Honest in-process SPCOT: sample `h` random COTs, run both sides.
pub fn spcot_in_process<R: SpecRng>(
    rng: &mut R,
    delta: &Block,
    n: usize,
    alpha: usize,
) -> (Vec<Block>, Vec<Block>) {
    let h = n.trailing_zeros() as usize;
    let mut cot_q = Vec::with_capacity(h);
    let mut cot_r = Vec::with_capacity(h);
    let mut cot_t = Vec::with_capacity(h);
    for _ in 0..h {
        let q = sample_block(rng);
        let r = (rng.next_u32() & 1) == 1;
        let t = if r { xor_block(&q, delta) } else { q };
        cot_q.push(q);
        cot_r.push(r);
        cot_t.push(t);
    }
    let choices = spcot_choice_bits(alpha, h, &cot_r);
    let (v, msg) = spcot_sender_extend(rng, delta, n, &cot_q, &choices);
    let w = spcot_receiver_extend(alpha, n, &cot_t, &msg);
    (v, w)
}

/// Fiat–Shamir consistency coefficients: `χ_i = χ^{i+1}` in GF(2^128), with χ
/// derived from the execution transcript (Fig. 6 §4.2 Fiat–Shamir form).
pub fn spcot_fs_chis(n: usize, transcript: &[u8]) -> Vec<Block> {
    let mut h = Sha3_256::new();
    h.update(b"ferret-spcot-fs-v1");
    h.update(transcript);
    let seed = h.finalize();
    let mut chi = [0u8; KAPPA_BYTES];
    chi.copy_from_slice(&seed[..KAPPA_BYTES]);
    let mut chi_pow = field_from_block(&chi);
    let mut chis = Vec::with_capacity(n);
    for _ in 0..n {
        chis.push(block_from_field(&chi_pow));
        chi_pow = field_mul(&chi_pow, &field_from_block(&chi));
    }
    chis
}

/// Evaluate the polynomial `∑_i blocks[i]·X^i` in GF(2^128) (X = the
/// indeterminate). This is the consistency-check mask compression.
fn poly_eval_at_x(blocks: &[Block]) -> crate::field::Galois128 {
    let x_elem = crate::field::Galois128(2); // X
    let mut acc = crate::field::Galois128(0);
    let mut x_pow = crate::field::Galois128(1); // X^0
    for b in blocks {
        acc = acc + field_mul(&field_from_block(b), &x_pow);
        x_pow = field_mul(&x_pow, &x_elem);
    }
    acc
}

/// Receiver Fig. 6 step 7: the masked extra-COT choice bits `x*' = x* ⊕ x_ϕ`,
/// where `x_ϕ` is the polynomial-basis bit-decomposition of `ϕ = χ_α` (the
/// receiver's puncture index α stays private). Send `x*'` to the sender.
pub fn spcot_masked_choice(alpha: usize, extra_r: &[bool], chis: &[Block]) -> Vec<bool> {
    debug_assert_eq!(extra_r.len(), KAPPA_BITS);
    let phi_bits = u128::from_le_bytes(chis[alpha]);
    (0..KAPPA_BITS)
        .map(|i| extra_r[i] ^ ((phi_bits >> i) & 1 == 1))
        .collect()
}

/// Sender Fig. 6 step 8: `V = ∑_i χ_i·v[i] + Y` with `Y = ∑_i (q_i ⊕ x*'[i]·Δ)·X^i`.
/// Returns `H'(V)` to send to the receiver.
pub fn spcot_sender_hash_v(
    delta: &Block,
    v: &[Block],
    extra_q: &[Block],
    x_star_prime: &[bool],
    chis: &[Block],
) -> [u8; 32] {
    debug_assert_eq!(extra_q.len(), KAPPA_BITS);
    debug_assert_eq!(x_star_prime.len(), KAPPA_BITS);
    let yp: Vec<Block> = (0..KAPPA_BITS)
        .map(|i| {
            if x_star_prime[i] {
                xor_block(&extra_q[i], delta)
            } else {
                extra_q[i]
            }
        })
        .collect();
    let y = poly_eval_at_x(&yp);
    let mut ip = crate::field::Galois128(0);
    for i in 0..v.len() {
        ip = ip + field_mul(&field_from_block(&chis[i]), &field_from_block(&v[i]));
    }
    hash_prime(&block_from_field(&(ip + y)))
}

/// Receiver Fig. 6 step 9: `W = ∑_i χ_i·w[i] + Z` with `Z = ∑_i t_i·X^i`.
/// The receiver accepts iff `spcot_sender_hash_v(...) == H'(W)`.
pub fn spcot_receiver_hash_w(w: &[Block], extra_t: &[Block], chis: &[Block]) -> [u8; 32] {
    debug_assert_eq!(extra_t.len(), KAPPA_BITS);
    let z = poly_eval_at_x(extra_t);
    let mut ip = crate::field::Galois128(0);
    for i in 0..w.len() {
        ip = ip + field_mul(&field_from_block(&chis[i]), &field_from_block(&w[i]));
    }
    hash_prime(&block_from_field(&(ip + z)))
}

/// In-process Fig. 6 steps 6–9 consistency check (for tests). Both parties'
/// masked hashes must match; the receiver recovers the unique puncture α from
/// `v ≠ w` (there is exactly one for an honest execution). `extra_q`/`extra_r`/
/// `extra_t` are κ consistent extra COTs (`t_i = q_i ⊕ r_i·Δ`).
pub fn spcot_consistency_check(
    delta: &Block,
    v: &[Block],
    w: &[Block],
    extra_q: &[Block],
    extra_r: &[bool],
    extra_t: &[Block],
    transcript: &[u8],
) -> bool {
    debug_assert_eq!(v.len(), w.len());
    debug_assert_eq!(extra_q.len(), KAPPA_BITS);
    debug_assert_eq!(extra_r.len(), KAPPA_BITS);
    debug_assert_eq!(extra_t.len(), KAPPA_BITS);
    let n = v.len();
    let chis = spcot_fs_chis(n, transcript);
    let mut alpha = None;
    for i in 0..n {
        if v[i] != w[i] {
            if alpha.is_some() {
                return false;
            }
            alpha = Some(i);
        }
    }
    let Some(a) = alpha else { return false };
    if xor_block(&v[a], &w[a]) != *delta {
        return false;
    }
    let x_star_prime = spcot_masked_choice(a, extra_r, &chis);
    let hv = spcot_sender_hash_v(delta, v, extra_q, &x_star_prime, &chis);
    let hw = spcot_receiver_hash_w(w, extra_t, &chis);
    hv == hw
}

// --- Batched consistency check (Appendix C): one masking over m SPCOTs. ---

/// Batched FS coefficients: per-execution `χ_i^l`, derived from
/// `transcript ‖ l` so each execution's coefficients are independent.
pub fn spcot_batched_fs_chis(lens: &[usize], transcript: &[u8]) -> Vec<Vec<Block>> {
    lens.iter()
        .enumerate()
        .map(|(l, &n)| {
            let mut t = transcript.to_vec();
            t.extend_from_slice(&(l as u64).to_le_bytes());
            spcot_fs_chis(n, &t)
        })
        .collect()
}

/// Batched masked choice (receiver): `ϕ = ∑_l χ_{α_l}^l`, `x*' = x* ⊕ x_ϕ`.
pub fn spcot_batched_masked_choice(
    alphas: &[usize],
    extra_r: &[bool],
    chis: &[Vec<Block>],
) -> Vec<bool> {
    debug_assert_eq!(extra_r.len(), KAPPA_BITS);
    let mut phi = crate::field::Galois128(0);
    for (l, &a) in alphas.iter().enumerate() {
        phi = phi + field_from_block(&chis[l][a]);
    }
    let phi_bits = phi.0;
    (0..KAPPA_BITS)
        .map(|i| extra_r[i] ^ ((phi_bits >> i) & 1 == 1))
        .collect()
}

/// Batched sender hash: `V = (∑_l∑_i χ_i^l·v^l[i]) + Y`.
pub fn spcot_batched_sender_hash_v(
    delta: &Block,
    vs: &[&[Block]],
    extra_q: &[Block],
    x_star_prime: &[bool],
    chis: &[Vec<Block>],
) -> [u8; 32] {
    let yp: Vec<Block> = (0..KAPPA_BITS)
        .map(|i| {
            if x_star_prime[i] {
                xor_block(&extra_q[i], delta)
            } else {
                extra_q[i]
            }
        })
        .collect();
    let mut acc = poly_eval_at_x(&yp);
    for (l, v) in vs.iter().enumerate() {
        for i in 0..v.len() {
            acc = acc + field_mul(&field_from_block(&chis[l][i]), &field_from_block(&v[i]));
        }
    }
    hash_prime(&block_from_field(&acc))
}

/// Batched receiver hash: `W = (∑_l∑_i χ_i^l·w^l[i]) + Z`.
pub fn spcot_batched_receiver_hash_w(
    ws: &[&[Block]],
    extra_t: &[Block],
    chis: &[Vec<Block>],
) -> [u8; 32] {
    let mut acc = poly_eval_at_x(extra_t);
    for (l, w) in ws.iter().enumerate() {
        for i in 0..w.len() {
            acc = acc + field_mul(&field_from_block(&chis[l][i]), &field_from_block(&w[i]));
        }
    }
    hash_prime(&block_from_field(&acc))
}

fn field_from_block(b: &Block) -> crate::field::Galois128 {
    crate::field::Galois128(u128::from_le_bytes(*b))
}

fn block_from_field(g: &crate::field::Galois128) -> Block {
    g.0.to_le_bytes()
}

fn field_mul(a: &crate::field::Galois128, b: &crate::field::Galois128) -> crate::field::Galois128 {
    *a * *b
}

fn field_mul_block(a: &Block, b: &Block) -> Block {
    block_from_field(&field_mul(&field_from_block(a), &field_from_block(b)))
}

#[cfg(test)]
mod tests {
    use super::*;

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

    #[test]
    fn spcot_relation_holds_for_every_puncture() {
        const N: usize = 16;
        let mut rng = TestRng(0xF00D_CAFE_DEAD_BEEF);
        let delta = sample_block(&mut rng);
        for alpha in 0..N {
            let (v, w) = spcot_in_process(&mut rng, &delta, N, alpha);
            assert_eq!(v.len(), N);
            for i in 0..N {
                if i == alpha {
                    assert_eq!(xor_block(&v[i], &w[i]), delta, "alpha={alpha} i={i}");
                } else {
                    assert_eq!(v[i], w[i], "alpha={alpha} i={i} off-path mismatch");
                }
            }
        }
    }

    /// Sample κ consistent extra COTs: `t_i = q_i ⊕ r_i·Δ`.
    fn sample_extra_cots<R: SpecRng>(rng: &mut R, delta: &Block) -> (Vec<Block>, Vec<bool>, Vec<Block>) {
        let mut q = Vec::with_capacity(KAPPA_BITS);
        let mut r = Vec::with_capacity(KAPPA_BITS);
        let mut t = Vec::with_capacity(KAPPA_BITS);
        for _ in 0..KAPPA_BITS {
            let qi = sample_block(rng);
            let ri = (rng.next_u32() & 1) == 1;
            let ti = if ri { xor_block(&qi, delta) } else { qi };
            q.push(qi);
            r.push(ri);
            t.push(ti);
        }
        (q, r, t)
    }

    #[test]
    fn spcot_consistency_check_accepts_honest() {
        const N: usize = 8;
        let mut rng = TestRng(0x1111_2222);
        let delta = sample_block(&mut rng);
        // Real (nonzero) extra COTs, so the masked algebra is exercised.
        let (extra_q, extra_r, extra_t) = sample_extra_cots(&mut rng, &delta);
        for alpha in 0..N {
            let (v, w) = spcot_in_process(&mut rng, &delta, N, alpha);
            assert!(
                spcot_consistency_check(&delta, &v, &w, &extra_q, &extra_r, &extra_t, b"toy"),
                "alpha={alpha}"
            );
        }
    }

    #[test]
    fn spcot_consistency_check_masked_catches_cheating() {
        const N: usize = 8;
        let mut rng = TestRng(0x2222_3333);
        let delta = sample_block(&mut rng);
        let (extra_q, extra_r, extra_t) = sample_extra_cots(&mut rng, &delta);
        let alpha = 3usize;
        let (v, w) = spcot_in_process(&mut rng, &delta, N, alpha);
        // Honest baseline passes.
        assert!(spcot_consistency_check(
            &delta, &v, &w, &extra_q, &extra_r, &extra_t, b"toy"
        ));

        // (1) A receiver whose w deviates at the puncture (w[α] ≠ v[α] ⊕ Δ).
        let mut bad_w = w.clone();
        bad_w[alpha][0] ^= 1;
        assert!(!spcot_consistency_check(
            &delta, &v, &bad_w, &extra_q, &extra_r, &extra_t, b"toy"
        ));

        // (2) A receiver whose w deviates at an off-puncture index (not the
        // single-point relation).
        let mut bad_w2 = w.clone();
        bad_w2[(alpha + 1) % N][0] ^= 1;
        assert!(!spcot_consistency_check(
            &delta, &v, &bad_w2, &extra_q, &extra_r, &extra_t, b"toy"
        ));

        // (3) Tampered extra COT breaks the masking identity (Y ≠ Z ⊕ Δ·ϕ).
        let mut bad_t = extra_t.clone();
        bad_t[0][0] ^= 1;
        assert!(!spcot_consistency_check(
            &delta, &v, &w, &extra_q, &extra_r, &bad_t, b"toy"
        ));
    }

    #[test]
    fn spcot_two_party_consistency_check_split() {
        const N: usize = 16;
        let mut rng = TestRng(0x3333_4444);
        let delta = sample_block(&mut rng);
        let (extra_q, extra_r, extra_t) = sample_extra_cots(&mut rng, &delta);
        for alpha in 0..N {
            let (v, w) = spcot_in_process(&mut rng, &delta, N, alpha);
            let chis = spcot_fs_chis(N, b"two-party");
            // Receiver (private α): masked choice bits → sender.
            let x_star_prime = spcot_masked_choice(alpha, &extra_r, &chis);
            // Sender: H'(V); receiver: H'(W); must match.
            let hv = spcot_sender_hash_v(&delta, &v, &extra_q, &x_star_prime, &chis);
            let hw = spcot_receiver_hash_w(&w, &extra_t, &chis);
            assert_eq!(hv, hw, "alpha={alpha}");
            // A receiver cheating on α picks the wrong x*' ⇒ hashes diverge.
            let wrong = spcot_masked_choice((alpha + 1) % N, &extra_r, &chis);
            let hv_bad = spcot_sender_hash_v(&delta, &v, &extra_q, &wrong, &chis);
            assert_ne!(hv_bad, hw, "wrong alpha should fail (alpha={alpha})");
        }
    }

    #[test]
    fn spcot_batched_consistency_check() {
        const N: usize = 8;
        const M: usize = 4;
        let mut rng = TestRng(0x4444_5555);
        let delta = sample_block(&mut rng);
        let (extra_q, extra_r, extra_t) = sample_extra_cots(&mut rng, &delta);
        // m SPCOT executions, each with its own puncture.
        let alphas: Vec<usize> = (0..M).map(|_| (rng.next_u32() as usize) % N).collect();
        let mut vs = Vec::with_capacity(M);
        let mut ws = Vec::with_capacity(M);
        for &a in &alphas {
            let (v, w) = spcot_in_process(&mut rng, &delta, N, a);
            vs.push(v);
            ws.push(w);
        }
        let lens = [N; M];
        let chis = spcot_batched_fs_chis(&lens, b"batched");
        let x_star_prime = spcot_batched_masked_choice(&alphas, &extra_r, &chis);
        let v_refs: Vec<&[Block]> = vs.iter().map(|v| &v[..]).collect();
        let w_refs: Vec<&[Block]> = ws.iter().map(|w| &w[..]).collect();
        let hv = spcot_batched_sender_hash_v(&delta, &v_refs, &extra_q, &x_star_prime, &chis);
        let hw = spcot_batched_receiver_hash_w(&w_refs, &extra_t, &chis);
        assert_eq!(hv, hw, "honest batched check passes");
        // A receiver cheating on one execution's w breaks the batched check.
        let mut bad_ws = ws.clone();
        bad_ws[1][alphas[1]][0] ^= 1;
        let bad_refs: Vec<&[Block]> = bad_ws.iter().map(|w| &w[..]).collect();
        let hw_bad = spcot_batched_receiver_hash_w(&bad_refs, &extra_t, &chis);
        assert_ne!(hv, hw_bad, "cheating receiver caught by batched check");
    }

    #[test]
    fn two_thread_spcot_relation() {
        extern crate std;
        use std::sync::mpsc;
        use std::thread;
        const N: usize = 16;
        let mut rng = TestRng(0xABCD);
        let delta = sample_block(&mut rng);
        for alpha in 0..N {
            let h = N.trailing_zeros() as usize;
            let mut cot_q = Vec::new();
            let mut cot_r = Vec::new();
            let mut cot_t = Vec::new();
            for _ in 0..h {
                let q = sample_block(&mut rng);
                let r = (rng.next_u32() & 1) == 1;
                cot_q.push(q);
                cot_r.push(r);
                cot_t.push(if r { xor_block(&q, &delta) } else { q });
            }
            let (tx_c, rx_c) = mpsc::channel();
            let (tx_m, rx_m) = mpsc::channel();
            let cot_q_t = cot_q.clone();
            let delta_t = delta;
            let sender = thread::spawn(move || {
                let mut rng = TestRng(0x51);
                let choices: Vec<bool> = rx_c.recv().unwrap();
                let (v, msg) = spcot_sender_extend(&mut rng, &delta_t, N, &cot_q_t, &choices);
                tx_m.send((v, msg)).unwrap();
            });
            let cot_t_t = cot_t;
            let receiver = thread::spawn(move || {
                let choices = spcot_choice_bits(alpha, h, &cot_r);
                tx_c.send(choices).unwrap();
                let (v, msg) = rx_m.recv().unwrap();
                let w = spcot_receiver_extend(alpha, N, &cot_t_t, &msg);
                (v, w)
            });
            sender.join().unwrap();
            let (v, w) = receiver.join().unwrap();
            for i in 0..N {
                if i == alpha {
                    assert_eq!(xor_block(&v[i], &w[i]), delta);
                } else {
                    assert_eq!(v[i], w[i]);
                }
            }
        }
    }
}
