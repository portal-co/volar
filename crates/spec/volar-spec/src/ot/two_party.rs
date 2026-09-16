// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//! Role-separated LWE → SoftSpoken → Ferret-Reg pool over a byte transport.

use alloc::vec::Vec;

use digest::Digest;
use sha3::Sha3_256;

use super::base_ot::BaseOt;
use super::ferret::FerretParams;
use super::ferret::cot::{
    FerretPrep, FerretReceiverSeed, FerretSenderSeed, ferret_prepare_receiver,
    ferret_receiver_mpcot, ferret_sender_mpcot, sample_seed,
};
use super::ferret::mpcot_reg::{
    mpcot_reg_choice_bits, mpcot_reg_receiver, mpcot_reg_sender, sample_regular_noise,
};
use super::ferret::pool::{CotPoolReceiver, CotPoolSender, bea95_chosen_bit};
use super::ferret::spcot::{
    KAPPA_BITS, spcot_batched_fs_chis, spcot_batched_masked_choice, spcot_batched_receiver_hash_w,
    spcot_batched_sender_hash_v,
};
use super::iknp::{
    IKNP_KAPPA, IKNP_KAPPA_BYTES, iknp_receiver_finish, iknp_receiver_u_cols, iknp_sender_from_u,
    pack_kappa,
};
use super::lwe::{LWE_N, LweBaseOt};
use super::softspoken::TAG_DOMAIN;
use super::wire::{
    TAG_BEA95, TAG_DELTA, TAG_FERRET_CHECK_HV, TAG_FERRET_CHECK_MASK, TAG_FERRET_MPCOT,
    TAG_FERRET_OPEN, TAG_IKNP_CORR, TAG_IKNP_U, TAG_LWE_PAYLOAD, TAG_LWE_RECV, TAG_LWE_SETUP,
    TAG_SSP_R, TAG_SSP_S, decode_bools, decode_ferret_open, decode_iknp_corr, decode_iknp_u,
    decode_lwe_crs, decode_lwe_payload, decode_lwe_recv, decode_mpcot_reg, encode_bools,
    encode_ferret_open, encode_iknp_corr, encode_iknp_u, encode_lwe_crs, encode_lwe_payload,
    encode_lwe_recv, encode_mpcot_reg,
};
use crate::SpecRng;

/// Byte transport used by the two-party stack (mpsc, TCP, …).
pub trait StackIo {
    /// Send a tagged payload.
    fn send(&mut self, tag: u8, payload: &[u8]);
    /// Receive a payload, asserting `expected_tag`.
    fn recv(&mut self, expected_tag: u8) -> Vec<u8>;
}

type DigestImpl = Sha3_256;
type Base = LweBaseOt<LWE_N>;

fn sample_bytes<R: SpecRng, const L: usize>(rng: &mut R) -> [u8; L] {
    let mut b = [0u8; L];
    for chunk in b.chunks_mut(4) {
        chunk.copy_from_slice(&rng.next_u32().to_le_bytes()[..chunk.len()]);
    }
    b
}

fn ssp_sender_tag(delta_msg: &[u8; 16], r0: &[[u8; 16]]) -> digest::Output<DigestImpl> {
    let mut hs = DigestImpl::new();
    hs.update(TAG_DOMAIN);
    hs.update(delta_msg);
    for row in r0 {
        hs.update(row);
    }
    hs.finalize()
}

fn ssp_receiver_tag(
    delta_msg: &[u8; 16],
    bits: &[bool],
    v: &[[u8; 16]],
) -> digest::Output<DigestImpl> {
    let mut hr = DigestImpl::new();
    hr.update(TAG_DOMAIN);
    hr.update(delta_msg);
    for j in 0..bits.len() {
        let mut r0r = [0u8; 16];
        if bits[j] {
            for b in 0..16 {
                r0r[b] = v[j][b] ^ delta_msg[b];
            }
        } else {
            r0r = v[j];
        }
        hr.update(&r0r);
    }
    hr.finalize()
}

/// VOLE-sender / Ferret-sender / IKNP-extension-sender setup.
pub fn stack_setup_sender<R: SpecRng, Io: StackIo>(
    rng: &mut R,
    params: FerretParams,
    io: &mut Io,
) -> CotPoolSender {
    stack_setup_sender_m(rng, params, io, false)
}

/// Malicious-secure sender setup: the seed carries the extra κ consistency-check
/// COTs and refills run the malicious-secure extend.
pub fn stack_setup_sender_malicious<R: SpecRng, Io: StackIo>(
    rng: &mut R,
    params: FerretParams,
    io: &mut Io,
) -> CotPoolSender {
    stack_setup_sender_m(rng, params, io, true)
}

fn stack_setup_sender_m<R: SpecRng, Io: StackIo>(
    rng: &mut R,
    params: FerretParams,
    io: &mut Io,
    malicious: bool,
) -> CotPoolSender {
    let m = params.seed_cot_count(malicious);
    let delta_msg = sample_bytes::<R, 16>(rng);
    io.send(TAG_DELTA, &delta_msg);

    let mut delta_ot = [false; IKNP_KAPPA];
    for i in 0..IKNP_KAPPA {
        delta_ot[i] = (rng.next_u32() & 1) == 1;
    }
    let delta_ot_bytes = pack_kappa(&delta_ot);

    let mut chosen_seeds = [[0u8; IKNP_KAPPA_BYTES]; IKNP_KAPPA];
    for i in 0..IKNP_KAPPA {
        let setup = decode_lwe_crs::<LWE_N>(&io.recv(TAG_LWE_SETUP));
        let (r_state, recv_msg) =
            <Base as BaseOt<IKNP_KAPPA_BYTES>>::recv_start(rng, &setup, delta_ot[i]);
        io.send(TAG_LWE_RECV, &encode_lwe_recv(&recv_msg));
        let payload = decode_lwe_payload(&io.recv(TAG_LWE_PAYLOAD));
        chosen_seeds[i] = <Base as BaseOt<IKNP_KAPPA_BYTES>>::recv_finish(&r_state, &payload);
    }

    let u_msg = decode_iknp_u(&io.recv(TAG_IKNP_U));
    let (sender_r0, corrections) = iknp_sender_from_u::<DigestImpl, 16>(
        m,
        &delta_msg,
        &delta_ot,
        &delta_ot_bytes,
        &chosen_seeds,
        &u_msg,
    );
    io.send(TAG_IKNP_CORR, &encode_iknp_corr(&corrections));

    let tag_s = ssp_sender_tag(&delta_msg, &sender_r0);
    io.send(TAG_SSP_S, tag_s.as_slice());
    let tag_r = io.recv(TAG_SSP_R);
    debug_assert_eq!(tag_s.as_slice(), tag_r.as_slice());

    let mut q = Vec::with_capacity(m);
    for row in sender_r0 {
        q.push(row);
    }
    let mut sender = CotPoolSender {
        params,
        seed: FerretSenderSeed {
            delta: delta_msg,
            q,
        },
        out: alloc::collections::VecDeque::new(),
        raise_n: None,
        malicious,
        refill_count: 0,
    };
    if malicious {
        stack_refill_sender_malicious(rng, &mut sender, io);
    } else {
        stack_refill_sender(rng, &mut sender, io);
    }
    sender
}

/// VOLE-receiver / Ferret-receiver / IKNP-extension-receiver setup.
pub fn stack_setup_receiver<R: SpecRng, Io: StackIo>(
    rng: &mut R,
    params: FerretParams,
    io: &mut Io,
) -> CotPoolReceiver {
    stack_setup_receiver_m(rng, params, io, false)
}

/// Malicious-secure receiver setup (mirrors [`stack_setup_sender_malicious`]).
pub fn stack_setup_receiver_malicious<R: SpecRng, Io: StackIo>(
    rng: &mut R,
    params: FerretParams,
    io: &mut Io,
) -> CotPoolReceiver {
    stack_setup_receiver_m(rng, params, io, true)
}

fn stack_setup_receiver_m<R: SpecRng, Io: StackIo>(
    rng: &mut R,
    params: FerretParams,
    io: &mut Io,
    malicious: bool,
) -> CotPoolReceiver {
    let m = params.seed_cot_count(malicious);
    let delta_raw = io.recv(TAG_DELTA);
    let mut delta_msg = [0u8; 16];
    delta_msg.copy_from_slice(&delta_raw);

    let mut bits = alloc::vec![false; m];
    for b in &mut bits {
        *b = (rng.next_u32() & 1) == 1;
    }
    let mut seeds_0 = [[0u8; IKNP_KAPPA_BYTES]; IKNP_KAPPA];
    let mut seeds_1 = [[0u8; IKNP_KAPPA_BYTES]; IKNP_KAPPA];
    for i in 0..IKNP_KAPPA {
        seeds_0[i] = sample_bytes(rng);
        seeds_1[i] = sample_bytes(rng);
    }

    for i in 0..IKNP_KAPPA {
        let (s_state, setup) = <Base as BaseOt<IKNP_KAPPA_BYTES>>::sender_setup(rng);
        io.send(TAG_LWE_SETUP, &encode_lwe_crs(&setup));
        let recv_msg = decode_lwe_recv::<LWE_N>(&io.recv(TAG_LWE_RECV));
        let payload = <Base as BaseOt<IKNP_KAPPA_BYTES>>::sender_payload(
            rng,
            &s_state,
            &recv_msg,
            &seeds_0[i],
            &seeds_1[i],
        );
        io.send(TAG_LWE_PAYLOAD, &encode_lwe_payload(&payload));
    }

    let (t_cols, u_msg) = iknp_receiver_u_cols::<DigestImpl>(m, &bits, &seeds_0, &seeds_1);
    io.send(TAG_IKNP_U, &encode_iknp_u(&u_msg));
    let corrections = decode_iknp_corr(&io.recv(TAG_IKNP_CORR));
    let receiver_v = iknp_receiver_finish::<DigestImpl, 16>(&bits, &t_cols, &corrections);

    let tag_s = io.recv(TAG_SSP_S);
    let tag_r = ssp_receiver_tag(&delta_msg, &bits, &receiver_v);
    debug_assert_eq!(tag_s.as_slice(), tag_r.as_slice());
    io.send(TAG_SSP_R, tag_r.as_slice());

    let mut w = Vec::with_capacity(m);
    for row in receiver_v {
        w.push(row);
    }
    let mut receiver = CotPoolReceiver {
        params,
        seed: FerretReceiverSeed { u: bits, w },
        out_x: alloc::collections::VecDeque::new(),
        out_z: alloc::collections::VecDeque::new(),
        malicious,
    };
    if malicious {
        // The seed is honest at setup, so the malicious refill check passes.
        debug_assert!(stack_refill_receiver_malicious(rng, &mut receiver, io));
    } else {
        stack_refill_receiver(rng, &mut receiver, io);
    }
    receiver
}

/// One ΠCOT refill on the sender.
pub fn stack_refill_sender<R: SpecRng, Io: StackIo>(
    rng: &mut R,
    sender: &mut CotPoolSender,
    io: &mut Io,
) {
    let raw = io.recv(TAG_FERRET_OPEN);
    let (lpn_seed, choices) = decode_ferret_open(&raw);
    let (s, mpcot) = ferret_sender_mpcot(rng, sender.params, &sender.seed, &choices);
    io.send(TAG_FERRET_MPCOT, &encode_mpcot_reg(&mpcot));
    let out_full = encode_sender_only(sender, lpn_seed, &s, sender.params.seed_cot_count(false));
    sender.seed.q = out_full.seed_q;
    sender.out.extend(out_full.emit);
}

struct SenderLpn {
    seed_q: Vec<[u8; 16]>,
    emit: alloc::collections::VecDeque<[u8; 16]>,
}

fn xor_block(a: &[u8; 16], b: &[u8; 16]) -> [u8; 16] {
    let mut o = [0u8; 16];
    for i in 0..16 {
        o[i] = a[i] ^ b[i];
    }
    o
}

fn encode_sender_only(
    sender: &CotPoolSender,
    lpn_seed: [u8; 16],
    s: &[[u8; 16]],
    m: usize,
) -> SenderLpn {
    use super::ferret::lpn::encode_blocks;
    let k = sender.params.k;
    let n = sender.params.n;
    let y_lpn = encode_blocks(&lpn_seed, k, n, &sender.seed.q[..k]);
    let mut y = Vec::with_capacity(n);
    for j in 0..n {
        y.push(xor_block(&y_lpn[j], &s[j]));
    }
    SenderLpn {
        seed_q: y[..m].to_vec(),
        emit: y[m..].iter().copied().collect(),
    }
}

/// One ΠCOT refill on the receiver.
pub fn stack_refill_receiver<R: SpecRng, Io: StackIo>(
    rng: &mut R,
    receiver: &mut CotPoolReceiver,
    io: &mut Io,
) {
    let prep = ferret_prepare_receiver(rng, receiver.params, &receiver.seed);
    io.send(
        TAG_FERRET_OPEN,
        &encode_ferret_open(&prep.lpn_seed, &prep.choices),
    );
    let mpcot = decode_mpcot_reg(&io.recv(TAG_FERRET_MPCOT));
    let r = ferret_receiver_mpcot(receiver.params, &prep, &receiver.seed, &mpcot);
    let rec = encode_receiver_only(receiver, &prep, &r, receiver.params.seed_cot_count(false));
    receiver.seed = rec.seed;
    receiver.out_x.extend(rec.x);
    receiver.out_z.extend(rec.z);
}

/// Malicious-secure ΠCOT refill on the sender: MPCOT over the SPCOT COT range,
/// then the batched consistency check (the receiver sends its masked extra
/// choice `x*′`; the sender replies `H'(V)`). Keeps `seed_cot_count(true)` COTs
/// so the next iteration has its extra check COTs.
pub fn stack_refill_sender_malicious<R: SpecRng, Io: StackIo>(
    rng: &mut R,
    sender: &mut CotPoolSender,
    io: &mut Io,
) {
    let params = sender.params;
    let k = params.k;
    let t = params.t;
    let splen = params.splen();
    let extra_off = k + t * params.log_splen();
    let raw = io.recv(TAG_FERRET_OPEN);
    let (lpn_seed, choices) = decode_ferret_open(&raw);
    let (s, mpcot) = mpcot_reg_sender(
        rng,
        &sender.seed.delta,
        params.n,
        t,
        &sender.seed.q[k..extra_off],
        &choices,
    );
    let mpcot_bytes = encode_mpcot_reg(&mpcot);
    io.send(TAG_FERRET_MPCOT, &mpcot_bytes);
    // Batched consistency check: recv x*′, send H'(V).
    let x_star_prime = decode_bools(&io.recv(TAG_FERRET_CHECK_MASK)).0;
    let mut transcript = raw.clone();
    transcript.extend_from_slice(&mpcot_bytes);
    let lens = alloc::vec![splen; t];
    let chis = spcot_batched_fs_chis(&lens, &transcript);
    let vs: alloc::vec::Vec<&[super::ferret::spcot::Block]> =
        (0..t).map(|l| &s[l * splen..(l + 1) * splen]).collect();
    let extra_q = &sender.seed.q[extra_off..extra_off + KAPPA_BITS];
    let hv = spcot_batched_sender_hash_v(&sender.seed.delta, &vs, extra_q, &x_star_prime, &chis);
    io.send(TAG_FERRET_CHECK_HV, &hv);
    let m = params.seed_cot_count(true);
    let out_full = encode_sender_only(sender, lpn_seed, &s, m);
    sender.seed.q = out_full.seed_q;
    sender.out.extend(out_full.emit);
    sender.refill_count += 1;
}

/// Malicious-secure ΠCOT refill on the receiver. Runs the MPCOT, then the
/// batched consistency check against the sender's `H'(V)`. Returns `false` (the
/// caller must abort) if the check fails.
pub fn stack_refill_receiver_malicious<R: SpecRng, Io: StackIo>(
    rng: &mut R,
    receiver: &mut CotPoolReceiver,
    io: &mut Io,
) -> bool {
    let params = receiver.params;
    let k = params.k;
    let n = params.n;
    let t = params.t;
    let splen = params.splen();
    let extra_off = k + t * params.log_splen();
    // Prepare over the SPCOT COT range.
    let alphas = sample_regular_noise(rng, n, t);
    let mut e = alloc::vec![false; n];
    for (i, &a) in alphas.iter().enumerate() {
        e[i * splen + a] = true;
    }
    let lpn_seed = sample_seed(rng);
    let choices = mpcot_reg_choice_bits(n, t, &alphas, &receiver.seed.u[k..extra_off]);
    let open_bytes = encode_ferret_open(&lpn_seed, &choices);
    io.send(TAG_FERRET_OPEN, &open_bytes);
    let mpcot_bytes = io.recv(TAG_FERRET_MPCOT);
    let mpcot = decode_mpcot_reg(&mpcot_bytes);
    let r = mpcot_reg_receiver(n, t, &alphas, &receiver.seed.w[k..extra_off], &mpcot);
    // Batched consistency check: send x*′, recv H'(V), verify.
    let mut transcript = open_bytes.clone();
    transcript.extend_from_slice(&mpcot_bytes);
    let lens = alloc::vec![splen; t];
    let chis = spcot_batched_fs_chis(&lens, &transcript);
    let extra_r = &receiver.seed.u[extra_off..extra_off + KAPPA_BITS];
    let extra_t = &receiver.seed.w[extra_off..extra_off + KAPPA_BITS];
    let x_star_prime = spcot_batched_masked_choice(&alphas, extra_r, &chis);
    io.send(TAG_FERRET_CHECK_MASK, &encode_bools(&x_star_prime));
    let hv = io.recv(TAG_FERRET_CHECK_HV);
    let ws: alloc::vec::Vec<&[super::ferret::spcot::Block]> =
        (0..t).map(|l| &r[l * splen..(l + 1) * splen]).collect();
    let hw = spcot_batched_receiver_hash_w(&ws, extra_t, &chis);
    let mut hv_arr = [0u8; 32];
    hv_arr.copy_from_slice(&hv);
    let check = hv_arr == hw;
    if !check {
        return false;
    }
    let prep = FerretPrep {
        alphas,
        e,
        lpn_seed,
        choices,
    };
    let m = params.seed_cot_count(true);
    let rec = encode_receiver_only(receiver, &prep, &r, m);
    receiver.seed = rec.seed;
    receiver.out_x.extend(rec.x);
    receiver.out_z.extend(rec.z);
    true
}

struct RecvLpn {
    seed: FerretReceiverSeed,
    x: Vec<bool>,
    z: Vec<[u8; 16]>,
}

fn encode_receiver_only(
    receiver: &CotPoolReceiver,
    prep: &FerretPrep,
    r: &[[u8; 16]],
    m: usize,
) -> RecvLpn {
    use super::ferret::lpn::{encode_bits, encode_blocks};
    let k = receiver.params.k;
    let n = receiver.params.n;
    let x_bits = encode_bits(&prep.lpn_seed, k, n, &receiver.seed.u[..k]);
    let z_lpn = encode_blocks(&prep.lpn_seed, k, n, &receiver.seed.w[..k]);
    let mut x = Vec::with_capacity(n);
    let mut z = Vec::with_capacity(n);
    for j in 0..n {
        x.push(x_bits[j] ^ prep.e[j]);
        z.push(xor_block(&z_lpn[j], &r[j]));
    }
    RecvLpn {
        seed: FerretReceiverSeed {
            u: x[..m].to_vec(),
            w: z[..m].to_vec(),
        },
        x: x[m..].to_vec(),
        z: z[m..].to_vec(),
    }
}

fn ensure_sender<R: SpecRng, Io: StackIo>(
    rng: &mut R,
    sender: &mut CotPoolSender,
    io: &mut Io,
    need: usize,
) {
    let watermark = sender.params.seed_cot_count(sender.malicious);
    while sender.remaining() < need || sender.remaining().saturating_sub(need) < watermark {
        if sender.malicious {
            stack_refill_sender_malicious(rng, sender, io);
        } else {
            stack_refill_sender(rng, sender, io);
        }
    }
}

fn ensure_receiver<R: SpecRng, Io: StackIo>(
    rng: &mut R,
    receiver: &mut CotPoolReceiver,
    io: &mut Io,
    need: usize,
) {
    let watermark = receiver.params.seed_cot_count(receiver.malicious);
    while receiver.remaining() < need || receiver.remaining().saturating_sub(need) < watermark {
        if receiver.malicious {
            assert!(
                stack_refill_receiver_malicious(rng, receiver, io),
                "malicious Ferret consistency check failed"
            );
        } else {
            stack_refill_receiver(rng, receiver, io);
        }
    }
}

/// Sender: consume one random COT after Bea95 (`d` from the receiver).
pub fn stack_bea95_sender<R: SpecRng, Io: StackIo>(
    rng: &mut R,
    sender: &mut CotPoolSender,
    io: &mut Io,
) -> [u8; 16] {
    ensure_sender(rng, sender, io, 1);
    let r0 = sender.out.pop_front().unwrap();
    let d = io.recv(TAG_BEA95)[0] != 0;
    if d {
        xor_block(&r0, &sender.seed.delta)
    } else {
        r0
    }
}

/// Receiver: Bea95 chosen-bit conversion; sends `d = b ⊕ x`.
pub fn stack_bea95_receiver<R: SpecRng, Io: StackIo>(
    rng: &mut R,
    receiver: &mut CotPoolReceiver,
    io: &mut Io,
    bit: bool,
) -> [u8; 16] {
    ensure_receiver(rng, receiver, io, 1);
    let x = receiver.out_x.pop_front().unwrap();
    let z = receiver.out_z.pop_front().unwrap();
    let (_r0, zc, d) = bea95_chosen_bit(&[0u8; 16], [0u8; 16], x, z, bit);
    let _ = _r0;
    io.send(TAG_BEA95, &[d as u8]);
    zc
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use crate::field::Galois128;
    use crate::ot::ferret::FERRET_REG_TOY;
    use crate::ot::wire::TAG_HAT;
    use crate::vole::prove::vole_and_prover_step;
    use crate::vole::setup::derive_and_q;
    use crate::vole::{Delta, Q, Vope};
    use cipher::consts::U1;
    use hybrid_array::Array;
    use std::sync::mpsc::{self, Receiver, Sender};
    use std::thread;

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

    struct ChanIo {
        tx: Sender<(u8, Vec<u8>)>,
        rx: Receiver<(u8, Vec<u8>)>,
    }
    impl StackIo for ChanIo {
        fn send(&mut self, tag: u8, payload: &[u8]) {
            self.tx.send((tag, payload.to_vec())).unwrap();
        }
        fn recv(&mut self, expected_tag: u8) -> Vec<u8> {
            let (tag, payload) = self.rx.recv().expect("peer closed");
            assert_eq!(tag, expected_tag, "unexpected OT stack tag");
            payload
        }
    }

    fn pair_io() -> (ChanIo, ChanIo) {
        let (s2r_t, s2r_r) = mpsc::channel();
        let (r2s_t, r2s_r) = mpsc::channel();
        (
            ChanIo {
                tx: s2r_t,
                rx: r2s_r,
            },
            ChanIo {
                tx: r2s_t,
                rx: s2r_r,
            },
        )
    }

    fn bit_to_t(b: bool) -> Galois128 {
        Galois128(b as u128)
    }

    fn shares_from_cot(
        r0: [u8; 16],
        z: [u8; 16],
        bit: bool,
    ) -> (Vope<U1, Galois128, U1>, Q<U1, Galois128>) {
        crate::vole::setup::vole_commit_bit_shares(
            Array::<Galois128, U1>::from_fn(|_| Galois128(u128::from_le_bytes(r0))),
            Array::<Galois128, U1>::from_fn(|_| Galois128(u128::from_le_bytes(z))),
            bit_to_t,
            bit,
        )
    }

    #[test]
    fn two_thread_lwe_softspoken_ferret_circuit_loop() {
        let params = FERRET_REG_TOY;
        let (io_s, io_r) = pair_io();
        const ITERS: usize = 64;
        let sender = thread::spawn(move || {
            let mut rng = TestRng(0x1111_2222_3333_4444);
            let mut io = io_s;
            let mut sender = stack_setup_sender(&mut rng, params, &mut io);
            let delta = Delta {
                delta: Array::<Galois128, U1>::from_fn(|_| {
                    Galois128(u128::from_le_bytes(sender.seed.delta))
                }),
            };
            for it in 0..ITERS {
                let a = it % 2 == 0;
                let b = it % 3 == 0;
                let r0_a = stack_bea95_sender(&mut rng, &mut sender, &mut io);
                let r0_b = stack_bea95_sender(&mut rng, &mut sender, &mut io);
                let q_a = Q {
                    q: Array::from_fn(|_| Galois128(u128::from_le_bytes(r0_a))),
                };
                let q_b = Q {
                    q: Array::from_fn(|_| Galois128(u128::from_le_bytes(r0_b))),
                };
                let hat_bytes = io.recv(TAG_HAT);
                let mut hat_block = [0u8; 16];
                hat_block.copy_from_slice(&hat_bytes);
                let hat =
                    Array::<Galois128, U1>::from_fn(|_| Galois128(u128::from_le_bytes(hat_block)));
                let q_and = derive_and_q(&delta, &q_a, &q_b, &hat);
                let vope_bytes = io.recv(TAG_HAT);
                let mut vb = [0u8; 16];
                vb.copy_from_slice(&vope_bytes[..16]);
                let vope_and: Vope<U1, Galois128, U1> = Vope {
                    u: Array::from_fn(|_| Array::from_fn(|_| bit_to_t(a && b))),
                    v: Array::from_fn(|_| Galois128(u128::from_le_bytes(vb))),
                };
                assert!(
                    vope_and.clone() * delta.clone() == q_and,
                    "AND share mismatch iter {it}"
                );
                let _ = (a, b);
            }
            sender.remaining()
        });
        let receiver = thread::spawn(move || {
            let mut rng = TestRng(0xAAAA_BBBB_CCCC_DDDD);
            let mut io = io_r;
            let mut receiver = stack_setup_receiver(&mut rng, params, &mut io);
            for it in 0..ITERS {
                let a = it % 2 == 0;
                let b = it % 3 == 0;
                let z_a = stack_bea95_receiver(&mut rng, &mut receiver, &mut io, a);
                let z_b = stack_bea95_receiver(&mut rng, &mut receiver, &mut io, b);
                let (vope_a, _q_a) = shares_from_cot([0u8; 16], z_a, a);
                let (vope_b, _q_b) = shares_from_cot([0u8; 16], z_b, b);
                let (vope_and, hat) = vole_and_prover_step(vope_a, vope_b);
                io.send(TAG_HAT, &hat[0].0.to_le_bytes());
                io.send(TAG_HAT, &vope_and.v[0].0.to_le_bytes());
            }
        });
        receiver.join().unwrap();
        let remaining = sender.join().unwrap();
        assert!(remaining > 0);
    }

    #[test]
    fn two_thread_pool_refills() {
        let params = FERRET_REG_TOY;
        let (io_s, io_r) = pair_io();
        let need = params.output_cot_count(false) + 20;
        let sender = thread::spawn(move || {
            let mut rng = TestRng(0x0101_0101);
            let mut io = io_s;
            let mut sender = stack_setup_sender(&mut rng, params, &mut io);
            let mut r0s = Vec::new();
            for _ in 0..need {
                r0s.push(stack_bea95_sender(&mut rng, &mut sender, &mut io));
            }
            (r0s, sender.seed.delta)
        });
        let receiver = thread::spawn(move || {
            let mut rng = TestRng(0x0202_0202);
            let mut io = io_r;
            let mut receiver = stack_setup_receiver(&mut rng, params, &mut io);
            let mut zs = Vec::new();
            for j in 0..need {
                zs.push(stack_bea95_receiver(
                    &mut rng,
                    &mut receiver,
                    &mut io,
                    j % 2 == 0,
                ));
            }
            zs
        });
        let zs = receiver.join().unwrap();
        let (r0s, delta) = sender.join().unwrap();
        for j in 0..need {
            let bit = j % 2 == 0;
            let expected = if bit {
                xor_block(&r0s[j], &delta)
            } else {
                r0s[j]
            };
            assert_eq!(zs[j], expected, "bea95 row {j}");
        }
    }

    #[test]
    fn two_thread_malicious_pool_refills() {
        let params = FERRET_REG_TOY;
        let (io_s, io_r) = pair_io();
        let need = params.output_cot_count(true) + 20;
        let sender = thread::spawn(move || {
            let mut rng = TestRng(0x0101_0101);
            let mut io = io_s;
            let mut sender = stack_setup_sender_malicious(&mut rng, params, &mut io);
            assert!(sender.malicious);
            assert_eq!(sender.seed.q.len(), params.seed_cot_count(true));
            let mut r0s = Vec::new();
            for _ in 0..need {
                r0s.push(stack_bea95_sender(&mut rng, &mut sender, &mut io));
            }
            (r0s, sender.seed.delta)
        });
        let receiver = thread::spawn(move || {
            let mut rng = TestRng(0x0202_0202);
            let mut io = io_r;
            let mut receiver = stack_setup_receiver_malicious(&mut rng, params, &mut io);
            assert!(receiver.malicious);
            let mut zs = Vec::new();
            for j in 0..need {
                zs.push(stack_bea95_receiver(
                    &mut rng,
                    &mut receiver,
                    &mut io,
                    j % 2 == 0,
                ));
            }
            zs
        });
        let zs = receiver.join().unwrap();
        let (r0s, delta) = sender.join().unwrap();
        for j in 0..need {
            let bit = j % 2 == 0;
            let expected = if bit {
                xor_block(&r0s[j], &delta)
            } else {
                r0s[j]
            };
            assert_eq!(zs[j], expected, "malicious bea95 row {j}");
        }
    }
}
