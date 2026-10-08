// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//! Refillable COT pool (Ferret §6.2 bootstrap) and Bea95 chosen-bit conversion.
//! @volar-allow-vec: runtime-boundary: Ferret seeds and buffered COT rows are
//! runtime-sized host protocol material.

use alloc::collections::VecDeque;
use alloc::vec::Vec;

use super::cot::{
    FerretReceiverSeed, FerretSenderSeed, ferret_extend, ferret_extend_malicious, sample_seed_cots,
};
use super::params::FerretParams;
use super::spcot::{Block, KAPPA_BITS};
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
    /// Whether refills run the malicious-secure extend (with the SPCOT
    /// consistency check). Semi-honest consumers use `false`.
    pub malicious: bool,
    /// Refill counter, mixed into the malicious consistency-check transcript.
    pub(crate) refill_count: u64,
}

/// Receiver's random-COT buffer `(x, z)`.
pub struct CotPoolReceiver {
    pub params: FerretParams,
    pub seed: FerretReceiverSeed,
    pub(crate) out_x: VecDeque<bool>,
    pub(crate) out_z: VecDeque<Block>,
    pub malicious: bool,
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

/// Validate regular Ferret parameters and return the semi-honest seed COT count.
pub fn checked_seed_cot_count(params: FerretParams) -> Result<usize, PoolSeedError> {
    checked_seed_cot_count_mode(params, false)
}

/// Validate regular Ferret parameters and return the malicious-mode seed COT count.
pub fn checked_seed_cot_count_malicious(params: FerretParams) -> Result<usize, PoolSeedError> {
    checked_seed_cot_count_mode(params, true)
}

fn checked_seed_cot_count_mode(
    params: FerretParams,
    malicious: bool,
) -> Result<usize, PoolSeedError> {
    if params.t == 0 || params.n < params.t || params.k < super::lpn::LOCALITY {
        return Err(PoolSeedError::InvalidParameters);
    }
    let max_interval_len = params.n.div_ceil(params.t);
    let splen = max_interval_len
        .checked_next_power_of_two()
        .ok_or(PoolSeedError::InvalidParameters)?;
    let body = params
        .t
        .checked_mul(splen.trailing_zeros() as usize)
        .and_then(|extra| params.k.checked_add(extra))
        .ok_or(PoolSeedError::InvalidParameters)?;
    let seed_count = if malicious {
        body.checked_add(KAPPA_BITS)
            .ok_or(PoolSeedError::InvalidParameters)?
    } else {
        body
    };
    if seed_count >= params.n {
        return Err(PoolSeedError::InvalidParameters);
    }
    Ok(seed_count)
}

impl CotPoolSender {
    /// Initialize one protocol side from an independently produced Ferret seed.
    pub fn from_seed(params: FerretParams, seed: FerretSenderSeed) -> Result<Self, PoolSeedError> {
        Self::from_seed_mode(params, seed, false)
    }

    /// Initialize a malicious-mode sender from an independently produced Ferret seed.
    pub fn from_seed_malicious(
        params: FerretParams,
        seed: FerretSenderSeed,
    ) -> Result<Self, PoolSeedError> {
        Self::from_seed_mode(params, seed, true)
    }

    fn from_seed_mode(
        params: FerretParams,
        seed: FerretSenderSeed,
        malicious: bool,
    ) -> Result<Self, PoolSeedError> {
        let expected = checked_seed_cot_count_mode(params, malicious)?;
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
            malicious,
            refill_count: 0,
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
        let old_seed_count = checked_seed_cot_count_mode(self.params, self.malicious)?;
        if self.seed.q.len() != old_seed_count {
            return Err(PoolSeedError::SenderSeedCount {
                expected: old_seed_count,
                found: self.seed.q.len(),
            });
        }
        let required = checked_seed_cot_count_mode(params, self.malicious)?;
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
        Self::from_seed_mode(params, seed, false)
    }

    /// Initialize a malicious-mode receiver from independently produced Ferret seeds.
    pub fn from_seed_malicious(
        params: FerretParams,
        seed: FerretReceiverSeed,
    ) -> Result<Self, PoolSeedError> {
        Self::from_seed_mode(params, seed, true)
    }

    fn from_seed_mode(
        params: FerretParams,
        seed: FerretReceiverSeed,
        malicious: bool,
    ) -> Result<Self, PoolSeedError> {
        let expected = checked_seed_cot_count_mode(params, malicious)?;
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
            malicious,
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
        let old_seed_count = checked_seed_cot_count_mode(self.params, self.malicious)?;
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
        let required = checked_seed_cot_count_mode(params, self.malicious)?;
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

/// Allocate a pool from `m` ideal seed COTs (tests / one-time setup stand-in),
/// semi-honest mode. See [`new_pool_malicious`] for the malicious-secure pool.
pub fn new_pool<R: SpecRng>(rng: &mut R, params: FerretParams) -> (CotPoolSender, CotPoolReceiver) {
    new_pool_mode(rng, params, false)
}

/// Allocate a malicious-secure pool: the seed carries the extra κ
/// consistency-check COTs and refills run the malicious-secure extend.
pub fn new_pool_malicious<R: SpecRng>(
    rng: &mut R,
    params: FerretParams,
) -> (CotPoolSender, CotPoolReceiver) {
    new_pool_mode(rng, params, true)
}

fn new_pool_mode<R: SpecRng>(
    rng: &mut R,
    params: FerretParams,
    malicious: bool,
) -> (CotPoolSender, CotPoolReceiver) {
    let m = params.seed_cot_count(malicious);
    let (seed_s, seed_r) = sample_seed_cots(rng, m);
    (
        CotPoolSender {
            params,
            seed: seed_s,
            out: VecDeque::new(),
            raise_n: None,
            malicious,
            refill_count: 0,
        },
        CotPoolReceiver {
            params,
            seed: seed_r,
            out_x: VecDeque::new(),
            out_z: VecDeque::new(),
            malicious,
        },
    )
}

/// Run one Ferret iteration and append output COTs to both pools. Returns the
/// refill's consistency-check result — always `true` for a semi-honest pool; for
/// a malicious pool it is the batched SPCOT consistency check, and the caller
/// must abort the protocol on `false`.
pub fn refill<R: SpecRng>(
    rng: &mut R,
    sender: &mut CotPoolSender,
    receiver: &mut CotPoolReceiver,
) -> bool {
    let malicious = sender.malicious;
    if malicious != receiver.malicious || sender.params != receiver.params {
        return false;
    }
    let mut params = sender.params;
    let current_seed_count = match checked_seed_cot_count_mode(params, malicious) {
        Ok(count) => count,
        Err(_) => return false,
    };
    if sender.seed.q.len() != current_seed_count
        || receiver.seed.u.len() != current_seed_count
        || receiver.seed.w.len() != current_seed_count
        || sender.out.len() != receiver.out_x.len()
        || sender.out.len() != receiver.out_z.len()
    {
        return false;
    }
    if let Some(n) = sender.raise_n.take() {
        let mut raised = params;
        raised.n = n;
        let raised_seed_count = match checked_seed_cot_count_mode(raised, malicious) {
            Ok(count) => count,
            Err(_) => return false,
        };
        // The current seed vectors must still have exactly the target profile's
        // retained length; otherwise the extend's seed layout would be invalid.
        if raised_seed_count != current_seed_count
            || raised_seed_count != sender.seed.q.len()
            || raised_seed_count != receiver.seed.u.len()
            || raised_seed_count != receiver.seed.w.len()
        {
            return false;
        }
        params = raised;
        sender.params = params;
        receiver.params = params;
    }
    if malicious {
        // Bind the consistency check's FS coefficients to this refill.
        let Some(next_refill_count) = sender.refill_count.checked_add(1) else {
            return false;
        };
        let mut transcript = b"volar-ferret-pool-refill-v1".to_vec();
        transcript.extend_from_slice(&sender.refill_count.to_le_bytes());
        let (out, check) =
            ferret_extend_malicious(rng, params, &sender.seed, &receiver.seed, &transcript);
        if !check {
            return false;
        }
        sender.refill_count = next_refill_count;
        sender.seed = out.sender_seed;
        receiver.seed = out.receiver_seed;
        sender.out.extend(out.sender_out);
        receiver.out_x.extend(out.recv_x);
        receiver.out_z.extend(out.recv_z);
        true
    } else {
        let out = ferret_extend(rng, params, &sender.seed, &receiver.seed);
        sender.seed = out.sender_seed;
        receiver.seed = out.receiver_seed;
        sender.out.extend(out.sender_out);
        receiver.out_x.extend(out.recv_x);
        receiver.out_z.extend(out.recv_z);
        true
    }
}

fn ensure<R: SpecRng>(
    rng: &mut R,
    sender: &mut CotPoolSender,
    receiver: &mut CotPoolReceiver,
    need: usize,
) -> bool {
    let watermark = sender.params.seed_cot_count(sender.malicious);
    while sender.remaining() < need || sender.remaining().saturating_sub(need) < watermark {
        let before = sender.remaining();
        if !refill(rng, sender, receiver) {
            return false;
        }
        debug_assert!(sender.remaining() > before, "ΠCOT emitted no output COTs");
    }
    true
}

/// Take `need` random COTs, refilling until the buffer can serve them. Returns
/// `None` if a malicious refill's consistency check failed (the caller must
/// abort the protocol).
pub fn take_random<R: SpecRng>(
    rng: &mut R,
    sender: &mut CotPoolSender,
    receiver: &mut CotPoolReceiver,
    need: usize,
) -> Option<(Vec<Block>, Vec<bool>, Vec<Block>)> {
    if !ensure(rng, sender, receiver, need) {
        return None;
    }
    let mut r0 = Vec::with_capacity(need);
    let mut x = Vec::with_capacity(need);
    let mut z = Vec::with_capacity(need);
    for _ in 0..need {
        r0.push(sender.out.pop_front().unwrap());
        x.push(receiver.out_x.pop_front().unwrap());
        z.push(receiver.out_z.pop_front().unwrap());
    }
    Some((r0, x, z))
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
        let (r0, x, z) = take_random(&mut rng, &mut s, &mut r, need).expect("refill");
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

        assert!(refill(&mut rng, &mut sender, &mut receiver));
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
    fn malicious_pool_refills_and_checks() {
        let mut rng = TestRng(0xB0B0);
        let (mut s, mut r) = new_pool_malicious(&mut rng, FERRET_REG_TOY);
        // Malicious pool seeds carry the extra κ check COTs.
        assert_eq!(s.seed.q.len(), FERRET_REG_TOY.seed_cot_count(true));
        let need = FERRET_REG_TOY.output_cot_count(true) + 10;
        let (r0, x, z) =
            take_random(&mut rng, &mut s, &mut r, need).expect("honest malicious refill passes");
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
    fn malicious_pool_catches_tampered_receiver_seed() {
        let mut rng = TestRng(0xC0C0);
        let (mut s, mut r) = new_pool_malicious(&mut rng, FERRET_REG_TOY);
        // Tamper with the receiver's seed so the COT relation is inconsistent;
        // the malicious consistency check must catch it on the next refill.
        let k = FERRET_REG_TOY.k;
        r.seed.w[k][0] ^= 1;
        assert!(
            !refill(&mut rng, &mut s, &mut r),
            "tampered receiver seed fails the consistency check"
        );
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
