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
use super::swhe::{
    Ciphertext, Context, Error, KeySwitchKeySet, NoiseSource, RandomSource, SecretKey,
};

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
}

impl OfflineGarbler {
    /// Decrypt wire `index`'s `(A0, π)` bits from its just-expanded
    /// top-level ciphertext pair, as produced by the expansion stream.
    /// Wire states are deliberately not materialized here: a streaming
    /// deployment cannot hold the full expansion, so the garbler
    /// decrypts each pair as it streams by.
    pub fn wire_state(
        &self,
        ctx: &Context,
        plan: &ExpansionPlan,
        ct_a0: &Ciphertext,
        ct_pi: &Ciphertext,
        index: usize,
    ) -> Result<super::common::WireState, Error> {
        let degree = ctx.parameters().degree;
        let slot = |i: usize| (i % plan.slots_per_seed) % degree;
        let a0 = ctx.decrypt(&self.secret, ct_a0)?[slot(2 * index)] % 2;
        let pi = ctx.decrypt(&self.secret, ct_pi)?[slot(2 * index + 1)] % 2 == 1;
        let p = ctx.parameters().plaintext_modulus;
        Ok(super::common::WireState { a0: a0 % p, pi })
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

/// Offline setup: sample the secret key and the per-level key-switching
/// keys, and compute the expansion plan.
pub fn offline_setup(
    ctx: &Context,
    wire_count: usize,
    prg: &impl ExpansionPrg,
    ksk_base: u64,
    ksk_digits: usize,
    online_depth_budget: usize,
    random: &mut impl RandomSource,
    noise: &mut impl NoiseSource,
) -> Result<OfflineGarbler, Error> {
    let required_bits = 2_usize
        .checked_mul(wire_count)
        .ok_or(Error::InvalidParameters)?;
    let plan = ExpansionPlan::plan(required_bits, prg.depth(), prg.slots(), ctx.max_level() + 1)?;
    let top = ctx.max_level();
    if top < prg.depth() + online_depth_budget {
        return Err(Error::InvalidParameters);
    }
    let secret = ctx.keygen(random)?;
    let ksks = ctx.key_switch_keyset(&secret, ksk_base, ksk_digits, random, noise)?;
    Ok(OfflineGarbler { secret, ksks, plan })
}

/// One unit of the offline expansion stream: the garbler's wire state
/// and the evaluator's online-level expanded material for one wire.
#[derive(Clone, Debug)]
pub struct ExpansionStep {
    /// The garbler's wire state for this step's wire.
    pub wire_state: super::common::WireState,
    /// The evaluator's online-level expanded material.
    pub eval: ExpandedWire,
}

/// Stream the offline expansion wire-by-wire. Both sides consume the
/// stream independently — the garbler to garble gate-by-gate, the
/// evaluator (after a transport) to evaluate — so the full expansion is
/// never materialized on either side.
///
/// This is the exact computation the non-streaming [`offline_phase`]
/// performs, and yields identical wire states and evaluator material.
pub fn stream_expansion(
    ctx: &Context,
    wire_count: usize,
    prg: &impl ExpansionPrg,
    garbler: &OfflineGarbler,
    random: &mut impl RandomSource,
    noise: &mut impl NoiseSource,
) -> Result<Vec<ExpansionStep>, Error> {
    let plan = garbler.plan;
    let top = ctx.max_level();
    let degree = ctx.parameters().degree;
    let online_base = top - prg.depth();

    // Collect the expanded ciphertexts (as `offline_phase` does), then
    // pair them per wire and decrypt each wire's state from its pair.
    let required_bits = 2 * wire_count;
    let mut top_expanded: Vec<Ciphertext> = Vec::with_capacity(required_bits);
    let prg_slots = prg.slots();
    for _ in 0..plan.seed_ciphertexts {
        if top_expanded.len() >= required_bits {
            break;
        }
        let mut seed_slots = alloc::vec![0_u64; degree];
        let mut byte = [0u8; 1];
        for slot in seed_slots.iter_mut().take(prg_slots) {
            random.fill_bytes(&mut byte[..])?;
            *slot = u64::from(byte[0] & 1);
        }
        let ct = ctx.encrypt(&garbler.secret, top, &seed_slots, random, noise)?;
        top_expanded.extend(prg.expand(ctx, &ct, &garbler.ksks)?);
    }
    top_expanded.truncate(required_bits);

    let mut steps = Vec::with_capacity(wire_count);
    for (w, pair) in top_expanded.chunks_exact(2).enumerate() {
        let wire_state = garbler.wire_state(ctx, &plan, &pair[0], &pair[1], w)?;
        steps.push(ExpansionStep {
            wire_state,
            eval: ExpandedWire {
                ct_a0: ctx.switch_to(&pair[0], online_base)?,
                ct_pi: ctx.switch_to(&pair[1], online_base)?,
                slot_a0: (2 * w % plan.slots_per_seed) % degree,
                slot_pi: ((2 * w + 1) % plan.slots_per_seed) % degree,
            },
        });
    }
    Ok(steps)
}

/// Garbler-side decrypted wire states, one per wire, in wire order.
pub type WireStates = Vec<super::common::WireState>;

/// Run the offline phase for `wire_count` wires, materializing both
/// sides (the garbler's decrypted wire states included). This is the
/// non-streaming convenience form; a streaming integration uses
/// [`offline_setup`] + [`stream_expansion`].
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
) -> Result<(OfflineGarbler, OfflineEvaluator, WireStates), Error> {
    let garbler = offline_setup(
        ctx,
        wire_count,
        prg,
        ksk_base,
        ksk_digits,
        online_depth_budget,
        random,
        noise,
    )?;
    let plan = garbler.plan;
    let steps = stream_expansion(ctx, wire_count, prg, &garbler, random, noise)?;
    let mut states = Vec::with_capacity(wire_count);
    let mut wires = Vec::with_capacity(wire_count);
    for step in steps {
        states.push(step.wire_state);
        wires.push(step.eval);
    }
    Ok((garbler, OfflineEvaluator { plan, wires }, states))
}
