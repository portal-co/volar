// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//! @volar-allow-vec: runtime-boundary: OT/VOLE/FAEST protocol material,
//! transcripts, and batched commitments are runtime-sized host protocol
//! buffers, not weaver-known compiled-program shapes; this module-level
//! exemption applies to the whole file.
//! Refillable COT pool (Ferret §6.2 bootstrap) and Bea95 chosen-bit conversion.

use alloc::collections::VecDeque;
use alloc::vec::Vec;

use super::cot::{FerretReceiverSeed, FerretSenderSeed, ferret_extend, sample_seed_cots};
use super::params::FerretParams;
use super::spcot::Block;
use crate::SpecRng;

fn xor_block(a: &Block, b: &Block) -> Block {
    let mut o = [0u8; 16];
    for i in 0..16 {
        o[i] = a[i] ^ b[i];
    }
    o
}

/// Sender's random-COT buffer (`r0` rows) plus the bootstrap seed.
pub struct CotPoolSender {
    pub params: FerretParams,
    pub seed: FerretSenderSeed,
    pub(crate) out: VecDeque<Block>,
    /// If set, the next refill uses this `n` (must stay compatible with `t`).
    pub raise_n: Option<usize>,
}

/// Receiver's random-COT buffer `(x, z)`.
pub struct CotPoolReceiver {
    pub params: FerretParams,
    pub seed: FerretReceiverSeed,
    pub(crate) out_x: VecDeque<bool>,
    pub(crate) out_z: VecDeque<Block>,
}

impl CotPoolSender {
    pub fn remaining(&self) -> usize {
        self.out.len()
    }
}

impl CotPoolReceiver {
    pub fn remaining(&self) -> usize {
        self.out_x.len()
    }
}

/// Allocate a pool from `m` ideal seed COTs (tests / one-time setup stand-in).
pub fn new_pool<R: SpecRng>(rng: &mut R, params: FerretParams) -> (CotPoolSender, CotPoolReceiver) {
    let m = params.seed_cot_count(false);
    let (seed_s, seed_r) = sample_seed_cots(rng, m);
    (
        CotPoolSender {
            params,
            seed: seed_s,
            out: VecDeque::new(),
            raise_n: None,
        },
        CotPoolReceiver {
            params,
            seed: seed_r,
            out_x: VecDeque::new(),
            out_z: VecDeque::new(),
        },
    )
}

/// Run one Ferret iteration and append `n−M` COTs to both pools.
pub fn refill<R: SpecRng>(rng: &mut R, sender: &mut CotPoolSender, receiver: &mut CotPoolReceiver) {
    let mut params = sender.params;
    if let Some(n) = sender.raise_n.take() {
        debug_assert!(n % params.t == 0);
        debug_assert!((n / params.t).is_power_of_two());
        let mut raised = params;
        raised.n = n;
        // New `M` must fit in the current seed (raising `n` grows `log(n/t)`).
        debug_assert!(raised.seed_cot_count(false) <= sender.seed.q.len());
        params.n = n;
        sender.params = params;
        receiver.params = params;
    }
    let out = ferret_extend(rng, params, &sender.seed, &receiver.seed);
    sender.seed = out.sender_seed;
    receiver.seed = out.receiver_seed;
    sender.out.extend(out.sender_out);
    receiver.out_x.extend(out.recv_x);
    receiver.out_z.extend(out.recv_z);
}

fn ensure<R: SpecRng>(
    rng: &mut R,
    sender: &mut CotPoolSender,
    receiver: &mut CotPoolReceiver,
    need: usize,
) {
    let watermark = sender.params.seed_cot_count(false);
    while sender.remaining() < need || sender.remaining().saturating_sub(need) < watermark {
        let before = sender.remaining();
        refill(rng, sender, receiver);
        debug_assert!(sender.remaining() > before, "ΠCOT emitted no output COTs");
    }
}

/// Take `need` random COTs, refilling until the buffer can serve them.
pub fn take_random<R: SpecRng>(
    rng: &mut R,
    sender: &mut CotPoolSender,
    receiver: &mut CotPoolReceiver,
    need: usize,
) -> (Vec<Block>, Vec<bool>, Vec<Block>) {
    ensure(rng, sender, receiver, need);
    let mut r0 = Vec::with_capacity(need);
    let mut x = Vec::with_capacity(need);
    let mut z = Vec::with_capacity(need);
    for _ in 0..need {
        r0.push(sender.out.pop_front().unwrap());
        x.push(receiver.out_x.pop_front().unwrap());
        z.push(receiver.out_z.pop_front().unwrap());
    }
    (r0, x, z)
}

/// Bea95: convert one random COT into a chosen-bit COT.
///
/// Receiver sends `d = b ⊕ x`. Sender XORs `Δ` into `r0` when `d = 1`.
/// Receiver keeps `z`. Then `z = r0' ⊕ b · Δ`.
pub fn bea95_chosen_bit(
    delta: &Block,
    r0: Block,
    x: bool,
    z: Block,
    b: bool,
) -> (Block, Block, bool) {
    let d = b ^ x;
    let r0_chosen = if d { xor_block(&r0, delta) } else { r0 };
    (r0_chosen, z, d)
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
    fn pool_refills_when_drained() {
        let mut rng = TestRng(0xABAB);
        let (mut s, mut r) = new_pool(&mut rng, FERRET_REG_TOY);
        assert_eq!(s.remaining(), 0);
        let need = FERRET_REG_TOY.output_cot_count(false) + 10;
        let (r0, x, z) = take_random(&mut rng, &mut s, &mut r, need);
        assert_eq!(r0.len(), need);
        let delta = s.seed.delta;
        for j in 0..need {
            let expected = if x[j] {
                xor_block(&r0[j], &delta)
            } else {
                r0[j]
            };
            assert_eq!(z[j], expected, "row {j}");
        }
    }

    #[test]
    fn bea95_matches_choice_bit() {
        let delta = [0x11u8; 16];
        let r0 = [0x22u8; 16];
        for x in [false, true] {
            for b in [false, true] {
                let z = if x { xor_block(&r0, &delta) } else { r0 };
                let (r0c, zc, _) = bea95_chosen_bit(&delta, r0, x, z, b);
                let expected = if b { xor_block(&r0c, &delta) } else { r0c };
                assert_eq!(zc, expected, "x={x} b={b}");
            }
        }
    }
}
