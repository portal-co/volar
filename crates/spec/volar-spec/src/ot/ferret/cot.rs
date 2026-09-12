// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//! Bootstrapped COT extension ΠCOT (Ferret Fig. 9 + §6.2).
//!
//! Consumes `M = k + t log(n/t)` seed COTs (same global `Δ`), runs regular
//! MPCOT of length `n`, LPN-encodes with the 10-local code, keeps the first
//! `M` output COTs as the next seed, and emits `n − M` fresh random COTs.

use alloc::vec::Vec;

use super::lpn::{encode_bits, encode_blocks};
use super::mpcot_reg::{
    mpcot_reg_choice_bits, mpcot_reg_consistency_check, mpcot_reg_receiver, mpcot_reg_sender,
    sample_regular_noise, MpcotRegSenderMsg,
};
use super::params::FerretParams;
use super::spcot::{Block, KAPPA_BITS};
use crate::SpecRng;

/// One ΠCOT iteration's public transcript (receiver → sender: LPN seed +
/// SPCOT choice bits; sender → receiver: MPCOT ciphertexts).
#[derive(Clone)]
pub struct FerretIterMsg {
    /// 16-byte seed for the 10-local matrix `A`.
    pub lpn_seed: [u8; 16],
    pub choices: Vec<bool>,
    pub mpcot: MpcotRegSenderMsg,
}

/// Sender-side seed: `q` rows (`r0`).
#[derive(Clone)]
pub struct FerretSenderSeed {
    pub delta: Block,
    pub q: Vec<Block>,
}

/// Receiver-side seed: choice bits `u` and MAC rows `t` (`w` in Fig. 9).
#[derive(Clone)]
pub struct FerretReceiverSeed {
    pub u: Vec<bool>,
    pub w: Vec<Block>,
}

/// Output of one bootstrapped iteration.
pub struct FerretExtendOut {
    pub sender_out: Vec<Block>,
    pub recv_x: Vec<bool>,
    pub recv_z: Vec<Block>,
    pub sender_seed: FerretSenderSeed,
    pub receiver_seed: FerretReceiverSeed,
}

fn xor_block(a: &Block, b: &Block) -> Block {
    let mut o = [0u8; 16];
    for i in 0..16 {
        o[i] = a[i] ^ b[i];
    }
    o
}

pub fn sample_seed<R: SpecRng>(rng: &mut R) -> [u8; 16] {
    let mut s = [0u8; 16];
    for chunk in s.chunks_mut(4) {
        chunk.copy_from_slice(&rng.next_u32().to_le_bytes()[..chunk.len()]);
    }
    s
}

/// Receiver-side MPCOT opening (regular noise + LPN seed + SPCOT choices).
#[derive(Clone)]
pub struct FerretPrep {
    pub alphas: Vec<usize>,
    pub e: Vec<bool>,
    pub lpn_seed: [u8; 16],
    pub choices: Vec<bool>,
}

/// Sample regular noise, LPN seed, and SPCOT choice bits (receiver).
pub fn ferret_prepare_receiver<R: SpecRng>(
    rng: &mut R,
    params: FerretParams,
    receiver_seed: &FerretReceiverSeed,
) -> FerretPrep {
    let n = params.n;
    let t = params.t;
    let k = params.k;
    let alphas = sample_regular_noise(rng, n, t);
    let mut e = alloc::vec![false; n];
    for (i, &a) in alphas.iter().enumerate() {
        e[i * params.splen() + a] = true;
    }
    let lpn_seed = sample_seed(rng);
    let cot_r = &receiver_seed.u[k..];
    let choices = mpcot_reg_choice_bits(n, t, &alphas, cot_r);
    FerretPrep {
        alphas,
        e,
        lpn_seed,
        choices,
    }
}

/// Sender MPCOT given the receiver's choice bits.
pub fn ferret_sender_mpcot<R: SpecRng>(
    rng: &mut R,
    params: FerretParams,
    sender_seed: &FerretSenderSeed,
    choices: &[bool],
) -> (Vec<Block>, MpcotRegSenderMsg) {
    let cot_q = &sender_seed.q[params.k..];
    mpcot_reg_sender(
        rng,
        &sender_seed.delta,
        params.n,
        params.t,
        cot_q,
        choices,
    )
}

/// Receiver MPCOT given the stored puncture indices.
pub fn ferret_receiver_mpcot(
    params: FerretParams,
    prep: &FerretPrep,
    receiver_seed: &FerretReceiverSeed,
    mpcot: &MpcotRegSenderMsg,
) -> Vec<Block> {
    let cot_t = &receiver_seed.w[params.k..];
    mpcot_reg_receiver(params.n, params.t, &prep.alphas, cot_t, mpcot)
}

/// LPN-encode MPCOT output and split keep-`M` / emit-`n−M`.
pub fn ferret_finish(
    params: FerretParams,
    sender_seed: &FerretSenderSeed,
    receiver_seed: &FerretReceiverSeed,
    prep: &FerretPrep,
    s: &[Block],
    r: &[Block],
) -> FerretExtendOut {
    ferret_finish_m(
        params,
        sender_seed,
        receiver_seed,
        prep,
        s,
        r,
        params.seed_cot_count(false),
    )
}

/// `ferret_finish` with an explicit keep-`M` (the malicious extension keeps
/// `seed_cot_count(true)` so the next iteration has its extra check COTs).
fn ferret_finish_m(
    params: FerretParams,
    sender_seed: &FerretSenderSeed,
    receiver_seed: &FerretReceiverSeed,
    prep: &FerretPrep,
    s: &[Block],
    r: &[Block],
    m: usize,
) -> FerretExtendOut {
    let n = params.n;
    let k = params.k;
    let v_lpn = &sender_seed.q[..k];
    let u_lpn = &receiver_seed.u[..k];
    let w_lpn = &receiver_seed.w[..k];

    let y_lpn = encode_blocks(&prep.lpn_seed, k, n, v_lpn);
    let x_bits = encode_bits(&prep.lpn_seed, k, n, u_lpn);
    let z_lpn = encode_blocks(&prep.lpn_seed, k, n, w_lpn);

    let mut y = Vec::with_capacity(n);
    let mut x = Vec::with_capacity(n);
    let mut z = Vec::with_capacity(n);
    for j in 0..n {
        y.push(xor_block(&y_lpn[j], &s[j]));
        x.push(x_bits[j] ^ prep.e[j]);
        z.push(xor_block(&z_lpn[j], &r[j]));
    }

    let sender_seed = FerretSenderSeed {
        delta: sender_seed.delta,
        q: y[..m].to_vec(),
    };
    let receiver_seed = FerretReceiverSeed {
        u: x[..m].to_vec(),
        w: z[..m].to_vec(),
    };
    FerretExtendOut {
        sender_out: y[m..].to_vec(),
        recv_x: x[m..].to_vec(),
        recv_z: z[m..].to_vec(),
        sender_seed,
        receiver_seed,
    }
}

/// One Fig. 9 extend iteration with the §6.2 keep-`M` bootstrap.
///
/// Seed COTs: first `k` are the LPN `v/u/w`; the rest are SPCOT FCOT rows
/// (`t · h` of them). Extra malicious-check COTs are not consumed here.
pub fn ferret_extend<R: SpecRng>(
    rng: &mut R,
    params: FerretParams,
    sender_seed: &FerretSenderSeed,
    receiver_seed: &FerretReceiverSeed,
) -> FerretExtendOut {
    let m = params.seed_cot_count(false);
    debug_assert_eq!(sender_seed.q.len(), m);
    debug_assert_eq!(receiver_seed.u.len(), m);
    debug_assert_eq!(receiver_seed.w.len(), m);

    let prep = ferret_prepare_receiver(rng, params, receiver_seed);
    let (s, mpcot) = ferret_sender_mpcot(rng, params, sender_seed, &prep.choices);
    let r = ferret_receiver_mpcot(params, &prep, receiver_seed, &mpcot);
    ferret_finish(params, sender_seed, receiver_seed, &prep, &s, &r)
}

/// Malicious-secure Fig. 9 ΠCOT extend (Ferret-Reg): like [`ferret_extend`], but
/// consumes `seed_cot_count(true)` COTs (the extra κ consistency-check COTs) and
/// runs the batched SPCOT consistency check (Appendix C) over the MPCOT. Returns
/// the extension output plus the check result; the caller must abort on `false`.
///
/// Seed layout: `[k LPN | t·h SPCOT | κ extra]`. `transcript` binds the FS
/// coefficients to this execution (include the MPCOT transcript).
pub fn ferret_extend_malicious<R: SpecRng>(
    rng: &mut R,
    params: FerretParams,
    sender_seed: &FerretSenderSeed,
    receiver_seed: &FerretReceiverSeed,
    transcript: &[u8],
) -> (FerretExtendOut, bool) {
    let m = params.seed_cot_count(true);
    debug_assert_eq!(sender_seed.q.len(), m);
    debug_assert_eq!(receiver_seed.u.len(), m);
    debug_assert_eq!(receiver_seed.w.len(), m);

    let k = params.k;
    let n = params.n;
    let t = params.t;
    let spcot_cots = t * params.log_splen();
    let extra_off = k + spcot_cots;

    // Receiver prep over the SPCOT COT range only.
    let alphas = sample_regular_noise(rng, n, t);
    let mut e = alloc::vec![false; n];
    for (i, &a) in alphas.iter().enumerate() {
        e[i * params.splen() + a] = true;
    }
    let lpn_seed = sample_seed(rng);
    let choices = mpcot_reg_choice_bits(n, t, &alphas, &receiver_seed.u[k..extra_off]);

    // Sender + receiver MPCOT (SPCOT COT range only).
    let (s, mpcot) = mpcot_reg_sender(
        rng,
        &sender_seed.delta,
        n,
        t,
        &sender_seed.q[k..extra_off],
        &choices,
    );
    let r = mpcot_reg_receiver(n, t, &alphas, &receiver_seed.w[k..extra_off], &mpcot);

    // Batched consistency check using the κ extra COTs.
    let extra_q = &sender_seed.q[extra_off..extra_off + KAPPA_BITS];
    let extra_r = &receiver_seed.u[extra_off..extra_off + KAPPA_BITS];
    let extra_t = &receiver_seed.w[extra_off..extra_off + KAPPA_BITS];
    let check = mpcot_reg_consistency_check(
        &sender_seed.delta,
        &s,
        &r,
        &alphas,
        extra_q,
        extra_r,
        extra_t,
        transcript,
    );

    // LPN finish; keep M = seed_cot_count(true) so the next iteration has its
    // extra check COTs.
    let prep = FerretPrep {
        alphas,
        e,
        lpn_seed,
        choices: Vec::new(),
    };
    let out = ferret_finish_m(params, sender_seed, receiver_seed, &prep, &s, &r, m);
    (out, check)
}

fn split_cot_chunks<T: Clone>(flat: &[T], heights: &[usize]) -> Vec<Vec<T>> {
    let mut out = Vec::with_capacity(heights.len());
    let mut off = 0usize;
    for &h in heights {
        out.push(flat[off..off + h].to_vec());
        off += h;
    }
    debug_assert_eq!(off, flat.len());
    out
}

/// Fig. 9 ΠCOT using Fig. 7 Cuckoo MPCOT (Ferret-Uni). `hash_seed` is public
/// and reused across iterations so `M = k + ∑ h_j` is stable.
pub fn ferret_extend_uni<R: SpecRng>(
    rng: &mut R,
    params: FerretParams,
    hash_seed: [u8; 16],
    sender_seed: &FerretSenderSeed,
    receiver_seed: &FerretReceiverSeed,
) -> FerretExtendOut {
    use super::mpcot_uni::{
        mpcot_uni_choice_bits, mpcot_uni_receiver, mpcot_uni_sender, sample_uniform_points,
        uni_seed_cot_count, uni_spcot_heights,
    };

    let m_seed = uni_seed_cot_count(&hash_seed, params);
    debug_assert_eq!(sender_seed.q.len(), m_seed);
    debug_assert_eq!(receiver_seed.u.len(), m_seed);

    let k = params.k;
    let heights = uni_spcot_heights(&hash_seed, params.n, params.t);
    let points = sample_uniform_points(rng, params.n, params.t);
    let table = super::mpcot_uni::cuckoo_insert(&hash_seed, params.n, params.t, &points);
    let mut e = alloc::vec![false; params.n];
    for slot in &table {
        if let Some(x) = slot {
            e[*x] = true;
        }
    }

    let cot_r = split_cot_chunks(&receiver_seed.u[k..], &heights);
    let choices = mpcot_uni_choice_bits(params, &hash_seed, &table, &cot_r);
    let cot_q = split_cot_chunks(&sender_seed.q[k..], &heights);
    let (s, mpcot) = mpcot_uni_sender(
        rng,
        &sender_seed.delta,
        params,
        hash_seed,
        &cot_q,
        &choices,
    );
    let cot_t = split_cot_chunks(&receiver_seed.w[k..], &heights);
    let r = mpcot_uni_receiver(params, &table, &cot_t, &mpcot);

    let prep = FerretPrep {
        alphas: points,
        e,
        lpn_seed: sample_seed(rng),
        choices: Vec::new(),
    };
    let n = params.n;
    let v_lpn = &sender_seed.q[..k];
    let u_lpn = &receiver_seed.u[..k];
    let w_lpn = &receiver_seed.w[..k];
    let y_lpn = encode_blocks(&prep.lpn_seed, k, n, v_lpn);
    let x_bits = encode_bits(&prep.lpn_seed, k, n, u_lpn);
    let z_lpn = encode_blocks(&prep.lpn_seed, k, n, w_lpn);
    let mut y = Vec::with_capacity(n);
    let mut x = Vec::with_capacity(n);
    let mut z = Vec::with_capacity(n);
    for j in 0..n {
        y.push(xor_block(&y_lpn[j], &s[j]));
        x.push(x_bits[j] ^ prep.e[j]);
        z.push(xor_block(&z_lpn[j], &r[j]));
    }
    FerretExtendOut {
        sender_out: y[m_seed..].to_vec(),
        recv_x: x[m_seed..].to_vec(),
        recv_z: z[m_seed..].to_vec(),
        sender_seed: FerretSenderSeed {
            delta: sender_seed.delta,
            q: y[..m_seed].to_vec(),
        },
        receiver_seed: FerretReceiverSeed {
            u: x[..m_seed].to_vec(),
            w: z[..m_seed].to_vec(),
        },
    }
}

/// Sample `m` random COT shares with a fresh `Δ` (one-time setup stand-in).
pub fn sample_seed_cots<R: SpecRng>(
    rng: &mut R,
    m: usize,
) -> (FerretSenderSeed, FerretReceiverSeed) {
    let mut delta = [0u8; 16];
    for chunk in delta.chunks_mut(4) {
        chunk.copy_from_slice(&rng.next_u32().to_le_bytes()[..chunk.len()]);
    }
    let mut q = Vec::with_capacity(m);
    let mut u = Vec::with_capacity(m);
    let mut w = Vec::with_capacity(m);
    for _ in 0..m {
        let mut row = [0u8; 16];
        for chunk in row.chunks_mut(4) {
            chunk.copy_from_slice(&rng.next_u32().to_le_bytes()[..chunk.len()]);
        }
        let bit = (rng.next_u32() & 1) == 1;
        let t = if bit { xor_block(&row, &delta) } else { row };
        q.push(row);
        u.push(bit);
        w.push(t);
    }
    (
        FerretSenderSeed { delta, q },
        FerretReceiverSeed { u, w },
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ot::ferret::params::FERRET_REG_TOY;

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
    fn ferret_reg_toy_iteration_cot_relation() {
        let mut rng = TestRng(0xC0FF_EE00_BEEF);
        let p = FERRET_REG_TOY;
        let m = p.seed_cot_count(false);
        let (ss, rs) = sample_seed_cots(&mut rng, m);
        let delta = ss.delta;
        let out = ferret_extend(&mut rng, p, &ss, &rs);
        assert_eq!(out.sender_out.len(), p.output_cot_count(false));
        for j in 0..out.sender_out.len() {
            let expected = if out.recv_x[j] {
                xor_block(&out.sender_out[j], &delta)
            } else {
                out.sender_out[j]
            };
            assert_eq!(out.recv_z[j], expected, "row {j}");
        }
        // Kept seed also satisfies the relation.
        for j in 0..m {
            let expected = if out.receiver_seed.u[j] {
                xor_block(&out.sender_seed.q[j], &delta)
            } else {
                out.sender_seed.q[j]
            };
            assert_eq!(out.receiver_seed.w[j], expected, "seed row {j}");
        }
    }

    #[test]
    fn ferret_reg_malicious_iteration_consistency_check_passes() {
        let mut rng = TestRng(0xDEAD_0001_5555);
        let p = FERRET_REG_TOY;
        // Malicious seeds carry the extra κ consistency-check COTs.
        let m = p.seed_cot_count(true);
        let (ss, rs) = sample_seed_cots(&mut rng, m);
        let delta = ss.delta;
        let (out, check) = ferret_extend_malicious(&mut rng, p, &ss, &rs, b"toy-transcript");
        assert!(check, "honest malicious-secure iteration passes the check");
        assert_eq!(out.sender_out.len(), p.output_cot_count(true));
        // The emitted COTs still satisfy the correlation.
        for j in 0..out.sender_out.len() {
            let expected = if out.recv_x[j] {
                xor_block(&out.sender_out[j], &delta)
            } else {
                out.sender_out[j]
            };
            assert_eq!(out.recv_z[j], expected, "row {j}");
        }
        // The kept seed (with the extra check COTs) still satisfies the relation,
        // so the next malicious iteration can consume it.
        for j in 0..m {
            let expected = if out.receiver_seed.u[j] {
                xor_block(&out.sender_seed.q[j], &delta)
            } else {
                out.sender_seed.q[j]
            };
            assert_eq!(out.receiver_seed.w[j], expected, "seed row {j}");
        }
    }

    #[test]
    fn ferret_uni_toy_iteration_cot_relation() {
        use crate::ot::ferret::mpcot_uni::uni_seed_cot_count;
        let mut rng = TestRng(0x554E_4946);
        let p = crate::ot::ferret::params::FERRET_UNI_TOY;
        let mut hash_seed = [0u8; 16];
        for chunk in hash_seed.chunks_mut(4) {
            chunk.copy_from_slice(&rng.next_u32().to_le_bytes()[..chunk.len()]);
        }
        let m = uni_seed_cot_count(&hash_seed, p);
        let (ss, rs) = sample_seed_cots(&mut rng, m);
        let delta = ss.delta;
        let out = ferret_extend_uni(&mut rng, p, hash_seed, &ss, &rs);
        assert!(!out.sender_out.is_empty());
        for j in 0..out.sender_out.len() {
            let expected = if out.recv_x[j] {
                xor_block(&out.sender_out[j], &delta)
            } else {
                out.sender_out[j]
            };
            assert_eq!(out.recv_z[j], expected, "uni row {j}");
        }
    }
}
