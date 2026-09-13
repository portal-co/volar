// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//! All-in-one OT stack: LWE base OT → SoftSpoken extension → Ferret-Reg pool.
//!
//! In-process generator for tests. Two-thread / TCP drivers live in the
//! stack tests.

use alloc::vec::Vec;

use digest::Digest;

use super::base_ot::BaseOt;
use super::ferret::pool::{
    bea95_chosen_bit, new_pool, take_random, CotPoolReceiver, CotPoolSender,
};
use super::ferret::FerretParams;
use super::iknp::IKNP_KAPPA_BYTES;
use super::lwe::{LweBaseOt, LweBaseOtSecure, LWE_N};
use super::softspoken::softspoken_cot_extend_base;
use crate::field::Galois128;
use crate::vole::setup::{vole_commit_bit_shares, CotSource};
use crate::vole::{Q, Vope};
use crate::SpecRng;
use cipher::consts::U1;
use hybrid_array::Array;

/// In-process LWE + SoftSpoken + Ferret-Reg generator.
pub struct OtStack {
    pub sender: CotPoolSender,
    pub receiver: CotPoolReceiver,
}

impl OtStack {
    /// One-time setup: SoftSpoken (LWE base) produces `M` seed COTs, then
    /// one Ferret iteration fills the output buffer.
    pub fn setup<D, R>(rng_s: &mut R, rng_r: &mut R, params: FerretParams) -> Self
    where
        D: Digest,
        R: SpecRng,
    {
        Self::setup_impl::<D, R, LweBaseOt<LWE_N>>(rng_s, rng_r, params, false)
    }

    /// Malicious-secure one-time setup: identical to [`OtStack::setup`] but the
    /// LWE base OT is the per-coordinate [`LweBaseOtSecure`] (a malicious base
    /// receiver — here the SoftSpoken sender, who picks the base-OT choice bits
    /// — learns nothing from a garbage `pk_0`), the seed is sized for the extra
    /// κ consistency-check COTs, and the pools are flagged `malicious` so every
    /// refill runs the batched SPCOT consistency check.
    pub fn setup_malicious<D, R>(rng_s: &mut R, rng_r: &mut R, params: FerretParams) -> Self
    where
        D: Digest,
        R: SpecRng,
    {
        Self::setup_impl::<D, R, LweBaseOtSecure<LWE_N>>(rng_s, rng_r, params, true)
    }

    fn setup_impl<D, R, B>(rng_s: &mut R, rng_r: &mut R, params: FerretParams, malicious: bool) -> Self
    where
        D: Digest,
        R: SpecRng,
        B: BaseOt<16>,
    {
        let m = params.seed_cot_count(malicious);
        let mut bits = alloc::vec![false; m];
        for b in &mut bits {
            *b = (rng_r.next_u32() & 1) == 1;
        }
        let mut delta_msg = [0u8; 16];
        for chunk in delta_msg.chunks_mut(4) {
            chunk.copy_from_slice(&rng_s.next_u32().to_le_bytes()[..chunk.len()]);
        }
        let out = softspoken_cot_extend_base::<B, D, R, 16>(
            rng_s, rng_r, &bits, &delta_msg,
        );
        debug_assert!(out.check());

        // Map SoftSpoken C-OT rows into Ferret seed format.
        let mut q = Vec::with_capacity(m);
        let mut w = Vec::with_capacity(m);
        for j in 0..m {
            q.push(out.sender_r0[j]);
            w.push(out.receiver_v[j]);
        }
        let mut stack = Self {
            sender: CotPoolSender {
                params,
                seed: crate::ot::ferret::cot::FerretSenderSeed {
                    delta: delta_msg,
                    q,
                },
                out: alloc::collections::VecDeque::new(),
                raise_n: None,
                malicious,
                refill_count: 0,
            },
            receiver: CotPoolReceiver {
                params,
                seed: crate::ot::ferret::cot::FerretReceiverSeed { u: bits, w },
                out_x: alloc::collections::VecDeque::new(),
                out_z: alloc::collections::VecDeque::new(),
                malicious,
            },
        };
        // First refill so callers can take immediately.
        let _ = crate::ot::ferret::pool::refill(rng_s, &mut stack.sender, &mut stack.receiver);
        let _ = rng_r;
        stack
    }

    /// Ideal-seed constructor (skips LWE+SoftSpoken; for Ferret-only tests).
    pub fn from_ideal_seed<R: SpecRng>(rng: &mut R, params: FerretParams) -> Self {
        let (sender, receiver) = new_pool(rng, params);
        Self { sender, receiver }
    }

    /// Take `need` chosen-bit COTs and lift each to a VOLE wire (`Galois128`).
    pub fn commit_bits<R: SpecRng>(
        &mut self,
        rng: &mut R,
        bits: &[bool],
    ) -> Vec<(Vope<U1, Galois128, U1>, Q<U1, Galois128>)> {
        let (r0s, xs, zs) = take_random(rng, &mut self.sender, &mut self.receiver, bits.len())
            .expect("semi-honest pool refill");
        let delta = self.sender.seed.delta;
        let mut out = Vec::with_capacity(bits.len());
        for j in 0..bits.len() {
            let (r0, z, _d) = bea95_chosen_bit(&delta, r0s[j], xs[j], zs[j], bits[j]);
            let r0_t = Array::<Galois128, U1>::from_fn(|_| Galois128(u128::from_le_bytes(r0)));
            let v_t = Array::<Galois128, U1>::from_fn(|_| Galois128(u128::from_le_bytes(z)));
            out.push(vole_commit_bit_shares(r0_t, v_t, bit_to_g128, bits[j]));
        }
        out
    }
}

impl CotSource<U1, Galois128> for OtStack {
    fn cot<R: SpecRng>(
        &mut self,
        rng: &mut R,
        _sample_t: impl Fn(&mut R) -> Galois128,
        bit: bool,
    ) -> (Array<Galois128, U1>, Array<Galois128, U1>) {
        let (r0s, xs, zs) =
            take_random(rng, &mut self.sender, &mut self.receiver, 1).expect("semi-honest pool refill");
        let (r0, z, _d) = bea95_chosen_bit(&self.sender.seed.delta, r0s[0], xs[0], zs[0], bit);
        let r0_t = Array::<Galois128, U1>::from_fn(|_| Galois128(u128::from_le_bytes(r0)));
        let v_t = Array::<Galois128, U1>::from_fn(|_| Galois128(u128::from_le_bytes(z)));
        (r0_t, v_t)
    }
}

fn bit_to_g128(b: bool) -> Galois128 {
    Galois128(b as u128)
}

/// Expose [`IKNP_KAPPA_BYTES`] so callers can name the LWE payload.
pub const STACK_SEED_BYTES: usize = IKNP_KAPPA_BYTES;

/// Marker that the stack's base OT is LWE.
pub fn stack_uses_lwe_base<const N: usize, const L: usize>() -> bool {
    core::mem::size_of::<<LweBaseOt<N> as BaseOt<L>>::SetupMsg>() > 0
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ot::ferret::FERRET_REG_TOY;
    use crate::vole::setup::vole_commit_bit_from;

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

    /// Malicious-secure bootstrap (per-coordinate LWE base OT + consistency
    /// check on refill) produces a working COT pool.
    #[test]
    fn malicious_seed_stack_commit_bits_match_delta() {
        let mut rng = TestRng(0xFEED);
        let mut rng_r = TestRng(0xBEEF);
        let mut stack =
            OtStack::setup_malicious::<sha3::Sha3_256, TestRng>(&mut rng, &mut rng_r, FERRET_REG_TOY);
        let bits = [true, false, true, true];
        let committed = stack.commit_bits(&mut rng, &bits);
        let delta = crate::vole::Delta {
            delta: Array::<Galois128, U1>::from_fn(|_| {
                Galois128(u128::from_le_bytes(stack.sender.seed.delta))
            }),
        };
        for (j, (vope, q)) in committed.iter().enumerate() {
            assert!(vope.clone() * delta.clone() == *q, "malicious bit {j}");
        }
    }

    #[test]
    fn ideal_seed_stack_commit_bits_match_delta() {
        let mut rng = TestRng(0xFEED);
        let mut stack = OtStack::from_ideal_seed(&mut rng, FERRET_REG_TOY);
        let bits = [true, false, true, true];
        let committed = stack.commit_bits(&mut rng, &bits);
        let delta = crate::vole::Delta {
            delta: Array::<Galois128, U1>::from_fn(|_| {
                Galois128(u128::from_le_bytes(stack.sender.seed.delta))
            }),
        };
        for (j, (vope, q)) in committed.iter().enumerate() {
            assert!(vope.clone() * delta.clone() == *q, "bit {j}");
        }
        let (vope, q) = vole_commit_bit_from(
            &mut stack,
            &mut rng,
            |_| Galois128(1),
            bit_to_g128,
            false,
        );
        assert!(vope * delta == q);
    }
}
