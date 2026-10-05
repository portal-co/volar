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

/// Why independently provisioned Ferret seeds cannot initialize a pool.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PoolSeedError {
    /// The regular Ferret parameters are invalid or have no positive output.
    InvalidParameters,
    /// Sender seed COT count did not match the parameter-derived count.
    SenderSeedCount { expected: usize, found: usize },
    /// Receiver choice-bit count did not match the parameter-derived count.
    ReceiverChoiceCount { expected: usize, found: usize },
    /// Receiver seed-value count did not match the parameter-derived count.
    ReceiverSeedCount { expected: usize, found: usize },
    /// A profile transition needed more COTs than the setup pool contains.
    InsufficientCotMaterial { required: usize, available: usize },
    /// Receiver choices and value shares in the ready queue had different lengths.
    ReceiverOutputCountMismatch { choices: usize, values: usize },
}

/// Validate regular Ferret parameters and return the seed COT count.
pub fn checked_seed_cot_count(params: FerretParams) -> Result<usize, PoolSeedError> {
    if params.t == 0 || params.n < params.t || params.k == 0 {
        return Err(PoolSeedError::InvalidParameters);
    }
    let max_interval_len = params.n.div_ceil(params.t);
    let splen = max_interval_len
        .checked_next_power_of_two()
        .ok_or(PoolSeedError::InvalidParameters)?;
    let log_splen = splen.trailing_zeros() as usize;
    let body = params
        .t
        .checked_mul(log_splen)
        .and_then(|extra| params.k.checked_add(extra))
        .ok_or(PoolSeedError::InvalidParameters)?;
    if body >= params.n {
        return Err(PoolSeedError::InvalidParameters);
    }
    Ok(body)
}

impl CotPoolSender {
    /// Initialize one protocol side from an independently produced Ferret seed.
    pub fn from_seed(params: FerretParams, seed: FerretSenderSeed) -> Result<Self, PoolSeedError> {
        let expected = checked_seed_cot_count(params)?;
        if seed.q.len() != expected {
            return Err(PoolSeedError::SenderSeedCount {
                expected,
                found: seed.q.len(),
            });
        }
        Ok(Self {
            params,
            seed,
            out: VecDeque::new(),
            raise_n: None,
        })
    }

    pub fn remaining(&self) -> usize {
        self.out.len()
    }

    /// Change from a one-time setup parameter set to a main-iteration set.
    ///
    /// Call immediately after one setup refill, before consuming any outputs.
    /// The refreshed setup seed is treated as the first part of the generated
    /// `n`-COT sequence; queued outputs follow it. The first target `M` COTs
    /// become the main seed, and the rest remain available in this pool.
    pub fn reprofile(mut self, params: FerretParams) -> Result<Self, PoolSeedError> {
        let old_seed_count = checked_seed_cot_count(self.params)?;
        if self.seed.q.len() != old_seed_count {
            return Err(PoolSeedError::SenderSeedCount {
                expected: old_seed_count,
                found: self.seed.q.len(),
            });
        }
        let required = checked_seed_cot_count(params)?;
        let available = self
            .seed
            .q
            .len()
            .checked_add(self.out.len())
            .ok_or(PoolSeedError::InvalidParameters)?;
        if available < required {
            return Err(PoolSeedError::InsufficientCotMaterial {
                required,
                available,
            });
        }
        let delta = self.seed.delta;
        let mut cots = self.seed.q;
        cots.extend(self.out);
        let out = cots.split_off(required);
        self.params = params;
        self.seed = FerretSenderSeed { delta, q: cots };
        self.out = out.into();
        self.raise_n = None;
        Ok(self)
    }
}

impl CotPoolReceiver {
    /// Initialize one protocol side from independently produced Ferret seeds.
    pub fn from_seed(
        params: FerretParams,
        seed: FerretReceiverSeed,
    ) -> Result<Self, PoolSeedError> {
        let expected = checked_seed_cot_count(params)?;
        if seed.u.len() != expected {
            return Err(PoolSeedError::ReceiverChoiceCount {
                expected,
                found: seed.u.len(),
            });
        }
        if seed.w.len() != expected {
            return Err(PoolSeedError::ReceiverSeedCount {
                expected,
                found: seed.w.len(),
            });
        }
        Ok(Self {
            params,
            seed,
            out_x: VecDeque::new(),
            out_z: VecDeque::new(),
        })
    }

    pub fn remaining(&self) -> usize {
        self.out_x.len()
    }

    /// Change from a one-time setup parameter set to a main-iteration set.
    ///
    /// Call immediately after one setup refill, before consuming any outputs.
    /// The refreshed setup seed is treated as the first part of the generated
    /// `n`-COT sequence; queued outputs follow it. The first target `M` COTs
    /// become the main seed, and the rest remain available in this pool.
    pub fn reprofile(mut self, params: FerretParams) -> Result<Self, PoolSeedError> {
        let old_seed_count = checked_seed_cot_count(self.params)?;
        if self.seed.u.len() != old_seed_count {
            return Err(PoolSeedError::ReceiverChoiceCount {
                expected: old_seed_count,
                found: self.seed.u.len(),
            });
        }
        if self.seed.w.len() != old_seed_count {
            return Err(PoolSeedError::ReceiverSeedCount {
                expected: old_seed_count,
                found: self.seed.w.len(),
            });
        }
        if self.out_x.len() != self.out_z.len() {
            return Err(PoolSeedError::ReceiverOutputCountMismatch {
                choices: self.out_x.len(),
                values: self.out_z.len(),
            });
        }
        let required = checked_seed_cot_count(params)?;
        let available = self
            .seed
            .u
            .len()
            .checked_add(self.out_x.len())
            .ok_or(PoolSeedError::InvalidParameters)?;
        if available < required {
            return Err(PoolSeedError::InsufficientCotMaterial {
                required,
                available,
            });
        }
        let mut choices = self.seed.u;
        choices.extend(self.out_x);
        let mut values = self.seed.w;
        values.extend(self.out_z);
        let output_choices = choices.split_off(required);
        let output_values = values.split_off(required);
        self.params = params;
        self.seed = FerretReceiverSeed {
            u: choices,
            w: values,
        };
        self.out_x = output_choices.into();
        self.out_z = output_values.into();
        Ok(self)
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
    fn reprofile_keeps_cot_relation_for_uneven_setup_intervals() {
        let mut rng = TestRng(0xB007_57A9);
        let setup_params = FerretParams {
            n: 512,
            k: 32,
            t: 7,
        };
        let main_params = FERRET_REG_TOY;
        let (sender_seed, receiver_seed) =
            sample_seed_cots(&mut rng, checked_seed_cot_count(setup_params).unwrap());
        let mut sender = CotPoolSender::from_seed(setup_params, sender_seed).unwrap();
        let mut receiver = CotPoolReceiver::from_seed(setup_params, receiver_seed).unwrap();

        refill(&mut rng, &mut sender, &mut receiver);
        let sender = sender.reprofile(main_params).unwrap();
        let receiver = receiver.reprofile(main_params).unwrap();
        assert_eq!(sender.remaining(), 512 - main_params.seed_cot_count(false));
        assert_eq!(sender.remaining(), receiver.remaining());
        let delta = sender.seed.delta;
        for (q, (&choice, z)) in sender
            .seed
            .q
            .iter()
            .zip(receiver.seed.u.iter().zip(&receiver.seed.w))
        {
            assert_eq!(*z, if choice { xor_block(q, &delta) } else { *q });
        }
        for ((q, &choice), z) in sender
            .out
            .iter()
            .zip(receiver.out_x.iter())
            .zip(receiver.out_z.iter())
        {
            assert_eq!(*z, if choice { xor_block(q, &delta) } else { *q });
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
