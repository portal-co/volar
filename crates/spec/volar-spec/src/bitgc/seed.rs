// @reliability: experimental
// @experimental-status: design
// @experimental-since: bitgc-phase-1
//! @ai: assisted
//! BitGC offline phase: encrypted-seed setup and homomorphic expansion.
//!
//! Structure from [LWYY25b] (ePrint 2024/1988): the garbling consists of
//! (1) a homomorphically encrypted seed, expanded under SWHE to
//! encryptions of many pseudorandom bits, and (2) per-gate stitching
//! information ([`super::common::Stitch`]). This module implements the
//! offline half: key generation, seed encryption, PRG expansion
//! ([`super::prg`]), the modulus switch to the online level, and
//! extraction of per-wire expanded material
//! ([`super::common::ExpandedWire`]) for the online garbling/evaluation
//! drivers in [`super::small`].
//!
//! # Boundaries
//!
//! - Caller supplies randomness/noise (`RandomSource`/`NoiseSource`).
//! - The switch to the online level is the exact BGV switch of
//!   [`super::swhe::Context::mod_switch_down`]; both parties compute it
//!   locally on the expanded ciphertexts (matching the artifact's
//!   two-context shape, [ZLL+26-A]).
//! - No transcript binding or transport here; that is the session
//!   adapter's job (`volar-mpc`, Phase 2.3).

use alloc::vec::Vec;

use super::common::ExpandedWire;
use super::prg::{ExpansionPlan, ExpansionPrg};
use super::swhe::{Ciphertext, Context, Error, KeySwitchKeySet, NoiseSource, RandomSource, SecretKey};

/// Everything the garbler keeps after the offline phase.
#[derive(Clone, Debug)]
pub struct OfflineGarbler {
    /// The SWHE secret key. Garbler-side only; the evaluator never holds
    /// this. Distributed decryption for output opening is a separate
    /// protocol seam, not implemented here.
    pub secret: SecretKey,
    /// Per-level key-switching keys (offline expansion + online gates).
    pub ksks: KeySwitchKeySet,
    /// The expansion plan used.
    pub plan: ExpansionPlan,
    /// Expanded ciphertexts at the top level: index `2*w` encrypts wire
    /// `w`'s zero-label bit `A0`, index `2*w+1` encrypts its
    /// permutation-bit candidate `π`.
    pub expanded: Vec<Ciphertext>,
}

impl OfflineGarbler {
    /// Decrypt wire `w`'s expanded `(A0, π)` bits.
    pub fn wire_bits(&self, ctx: &Context, wire: usize) -> Result<(u64, bool), Error> {
        let degree = ctx.parameters().degree;
        // Stream position -> extraction slot within its PRG output.
        let slot = |index: usize| (index % self.plan.slots_per_seed) % degree;
        let a0 = self
            .expanded
            .get(2 * wire)
            .ok_or(Error::InvalidParameters)?;
        let pi = self
            .expanded
            .get(2 * wire + 1)
            .ok_or(Error::InvalidParameters)?;
        let a0_slots = ctx.decrypt(&self.secret, a0)?;
        let pi_slots = ctx.decrypt(&self.secret, pi)?;
        Ok((
            a0_slots[slot(2 * wire)] % 2,
            pi_slots[slot(2 * wire + 1)] % 2 == 1,
        ))
    }
}

/// Evaluator-side offline state: expanded ciphertexts at the online
/// level, one per wire `(A0, π)` pair.
#[derive(Clone, Debug)]
pub struct OfflineEvaluator {
    /// The expansion plan used.
    pub plan: ExpansionPlan,
    /// Online-level expanded ciphertexts, paired per wire as in
    /// [`OfflineGarbler::expanded`].
    pub wires: Vec<ExpandedWire>,
}

impl OfflineEvaluator {
    /// Expanded material for `wire`.
    pub fn wire(&self, wire: usize) -> Option<&ExpandedWire> {
        self.wires.get(wire)
    }

    /// Number of wires covered.
    pub fn wires(&self) -> usize {
        self.wires.len()
    }
}

/// Run the offline phase for `wire_count` wires.
///
/// - Samples a secret key and both key-switching keys, encrypts
///   `plan.seed_ciphertexts` seed ciphertexts at the top level, and
///   expands each with `prg`.
/// - The garbler's half keeps the top-level expanded ciphertexts (it
///   decrypts on demand for wire bits).
/// - The evaluator's half is the same expansion modulus-switched to the
///   online level — what ships across the transport in a session adapter.
///
/// `ksk_base`/`ksk_digits` select the gadget decomposition for
/// relinearization; `base^digits` must exceed the top-level primes.
/// `online_depth_budget` is the multiplicative depth left for the online
/// gate phase below the expansion (the reference adapter needs 2).
pub fn offline_phase(
    ctx: &Context,
    wire_count: usize,
    prg: &impl ExpansionPrg,
    ksk_base: u64,
    ksk_digits: usize,
    online_depth_budget: usize,
    random: &mut impl RandomSource,
    noise: &mut impl NoiseSource,
) -> Result<(OfflineGarbler, OfflineEvaluator), Error> {
    let required_bits = 2_usize
        .checked_mul(wire_count)
        .ok_or(Error::InvalidParameters)?;
    let plan = ExpansionPlan::plan(required_bits, prg.depth(), prg.slots(), ctx.max_level() + 1)?;
    let top = ctx.max_level();
    let degree = ctx.parameters().degree;
    // Expansion consumes `prg.depth()` levels (its ciphertext
    // multiplications each switch down once), so expanded material lands
    // at `top - prg.depth()` — the online base where gate evaluation
    // begins. `online_depth_budget` is recorded for the plan and checked
    // against the chain length.
    let online_base = top - prg.depth();
    if top < prg.depth() + online_depth_budget {
        return Err(Error::InvalidParameters);
    }

    let secret = ctx.keygen(random)?;
    let ksks = ctx.key_switch_keyset(&secret, ksk_base, ksk_digits, random, noise)?;

    let mut top_expanded: Vec<Ciphertext> = Vec::with_capacity(required_bits);
    for _ in 0..plan.seed_ciphertexts {
        // Sample a random seed (slots beyond the PRG's output count stay 0).
        let mut seed_slots = alloc::vec![0_u64; degree];
        let mut byte = [0u8; 1];
        for slot in seed_slots.iter_mut().take(prg.slots()) {
            random.fill_bytes(&mut byte[..])?;
            *slot = u64::from(byte[0] & 1);
        }
        let ct = ctx.encrypt(&secret, top, &seed_slots, random, noise)?;
        top_expanded.extend(prg.expand(ctx, &ct, &ksks)?);
    }
    top_expanded.truncate(required_bits);

    // Switch to the online base level for the evaluator.
    let mut online: Vec<Ciphertext> = Vec::with_capacity(required_bits);
    for ct in &top_expanded {
        online.push(ctx.switch_to(ct, online_base)?);
    }
    let degree = ctx.parameters().degree;
    let wires = online
        .chunks_exact(2)
        .enumerate()
        .map(|(w, pair)| {
            let slot = |index: usize| (index % plan.slots_per_seed) % degree;
            ExpandedWire {
                ct_a0: pair[0].clone(),
                ct_pi: pair[1].clone(),
                slot_a0: slot(2 * w),
                slot_pi: slot(2 * w + 1),
            }
        })
        .collect();

    Ok((
        OfflineGarbler {
            secret,
            ksks,
            plan,
            expanded: top_expanded,
        },
        OfflineEvaluator { plan, wires },
    ))
}
