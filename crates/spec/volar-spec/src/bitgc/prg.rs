// @reliability: experimental
// @experimental-status: design
// @experimental-since: bitgc-phase-1
//! @ai: assisted
//! Low-depth PRG expansion for the BitGC offline phase.
//!
//! [LWYY25b] (ePrint 2024/1988) garbles with a SWHE-encrypted seed that
//! is homomorphically expanded into encryptions of many pseudorandom
//! bits; [ZLL+26] (ePrint 2025/1810) keeps that structure and makes it
//! SIMD-friendly. The paper's PRG is a *low-depth* arithmetic circuit so
//! that the expansion needs only a bounded number of multiplication
//! levels and no bootstrapping.
//!
//! # Pinning status (BITGC-PIN-03)
//!
//! The paper's exact PRG choice is not yet pinned from the paper text.
//! [`ExpansionPlan`] carries an explicit [`ExpansionPlan::depth`] budget
//! and this module drives expansion with [`ExpansionPrg`]; the shipped
//! [`ReferencePrg`] is a depth-1 reference expander
//! (`s·s` per slot, plus slot-indexed plaintext constants) standing in for the
//! pinned PRG. Replacing it must not change the expansion machinery,
//! only the `ExpansionPrg` implementation.
//!
//! TODO(provider-ledger: BITGC-LEDGER-04) — pin the paper's PRG circuit
//! and depth, and validate the level chain against it.

use alloc::vec::Vec;

use super::swhe::{Ciphertext, Context, Error, KeySwitchKeySet};

/// A low-depth homomorphic PRG evaluated on encrypted seeds.
///
/// One instance covers one SIMD ciphertext: [`Self::slots`] outputs per
/// seed ciphertext, at [`Self::depth`] multiplication levels.
pub trait ExpansionPrg {
    /// Multiplicative depth of the expansion circuit.
    fn depth(&self) -> usize;

    /// Output slot count per seed ciphertext (at most the ring degree).
    fn slots(&self) -> usize;

    /// Expand one encrypted seed into `slots` encrypted pseudorandom
    /// outputs (each a ciphertext at the same level).
    fn expand(
        &self,
        ctx: &Context,
        seed: &Ciphertext,
        ksk: &KeySwitchKeySet,
    ) -> Result<Vec<Ciphertext>, Error>;
}

/// Reference expander: output `i` is `s·s` masked to slot `i`. Seed
/// slots are bits, so slot `i` of output `i` holds `s_i^2 = s_i ∈
/// {0, 1}` — bit-shaped outputs, one per seed slot.
///
/// **Slot discipline:** the reference adapter uses `slots = 1` (one bit
/// per seed ciphertext, in slot 0) so every expanded bit aligns in slot
/// 0 for gate evaluation. Slot-parallel (SIMD) packing of many wires
/// into one ciphertext is the paper's §3 optimization and requires a
/// permutation network to align slots with gate topology; it is a
/// follow-up, not part of this reference adapter (BITGC-PIN-02).
///
/// **Not paper-pinned** (BITGC-PIN-03 / BITGC-LEDGER-04). Depth 1.
#[derive(Clone, Copy, Debug)]
pub struct ReferencePrg {
    slots: usize,
}

impl ReferencePrg {
    /// Expand to `slots` outputs per seed ciphertext.
    pub fn new(slots: usize) -> Self {
        Self { slots }
    }
}

impl ExpansionPrg for ReferencePrg {
    fn depth(&self) -> usize {
        // One ciphertext multiplication level (s·s); the slot masks and
        // constants are plaintext operations.
        1
    }

    fn slots(&self) -> usize {
        self.slots
    }

    fn expand(
        &self,
        ctx: &Context,
        seed: &Ciphertext,
        ksk: &KeySwitchKeySet,
    ) -> Result<Vec<Ciphertext>, Error> {
        let degree = ctx.parameters().degree;
        if self.slots > degree {
            return Err(Error::InvalidParameters);
        }
        // Reference expansion: build `slots` outputs by combining the seed
        // with slot-indexed plaintext constants through one multiplication
        // level (s^2 is the single depth-1 product; combining with c_i is
        // plaintext). Each output is a *slot-extracted* ciphertext: the
        // same ciphertext multiplied by a one-hot slot mask, so downstream
        // consumers see slot i of the expanded value.
        // Reference expansion: output i is `s·s` masked to slot i. Since
        // seed slots are bits, slot i holds `s_i^2 = s_i ∈ {0, 1}` and
        // all other slots are 0 — bit-shaped outputs, as [LWYY25b]'s
        // "encryptions of pseudo-random bits" requires. `s·s` is the
        // single depth-1 product; masks are plaintext.
        let s2 = ctx.mul(seed, seed, ksk)?;
        let mut out = Vec::with_capacity(self.slots);
        for i in 0..self.slots {
            let mut mask = alloc::vec![0_u64; degree];
            mask[i] = 1;
            out.push(ctx.mul_plain(&s2, &mask)?);
        }
        Ok(out)
    }
}

/// Expansion plan for one circuit: how many seed ciphertexts, at what
/// depth, covering how many expanded bits.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ExpansionPlan {
    /// Number of seed ciphertexts.
    pub seed_ciphertexts: usize,
    /// Output slots used per seed ciphertext.
    pub slots_per_seed: usize,
    /// Multiplicative depth budget consumed by expansion.
    pub depth: usize,
    /// Total expanded bits required (`2 × wire count` in BitGC:
    /// a zero-label bit and a permutation-bit candidate per wire).
    pub required_bits: usize,
}

impl ExpansionPlan {
    /// Plan expansion for `required_bits` against a PRG shape and ring
    /// degree. Errors if the depth budget is exceeded.
    pub fn plan(
        required_bits: usize,
        prg_depth: usize,
        slots_per_seed: usize,
        depth_budget: usize,
    ) -> Result<Self, Error> {
        if slots_per_seed == 0 || prg_depth > depth_budget {
            return Err(Error::InvalidParameters);
        }
        let seed_ciphertexts = required_bits.div_ceil(slots_per_seed);
        Ok(Self {
            seed_ciphertexts,
            slots_per_seed,
            depth: prg_depth,
            required_bits,
        })
    }

    /// Total expanded slots (`>= required_bits`).
    pub const fn total_slots(&self) -> usize {
        self.seed_ciphertexts * self.slots_per_seed
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn plan_sizing() {
        let plan = ExpansionPlan::plan(1_000, 2, 128, 3).unwrap();
        assert_eq!(plan.seed_ciphertexts, 8);
        assert_eq!(plan.total_slots(), 1_024);
        assert!(ExpansionPlan::plan(1_000, 4, 128, 3).is_err());
        assert!(ExpansionPlan::plan(1_000, 2, 0, 3).is_err());
    }
}
