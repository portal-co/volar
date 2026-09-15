// @pinnedness: unpinned
// @stability: very-unstable
// @ai: assisted
//! `BootstrapPlan` — the weaver's bootstrap schedule as data, and the
//! reference interpreter for it.
//!
//! The cone-fusion / circuit-bootstrap scheduling logic lives **once** in
//! the weaver (plan §8); its output is this serializable structure. Three
//! consumers see the same plan:
//!
//! 1. the weaver's code generator (typed IR calls into `binfhe_*`),
//! 2. the `volar-dyn` / `volar-spec-dyn` interpreters (plan §8),
//! 3. [`execute_plan`] here — the reference implementation both of the
//!    above differential-test against.
//!
//! The structure is plain `alloc` data (no pointers, no `TypeId`, no
//! string-keyed maps), so it is `rkyv`/serialization friendly and has a
//! deterministic [`BootstrapPlan::plan_hash`].
//!
//! # Wire model
//!
//! Three arenas: Boolean wires (LWE, [`WireId`]), RGSW wires produced by
//! circuit bootstrap ([`RgswId`]), and RLWE content cells ([`CellId`]) on
//! which RGSW multiplexers act (the oblivious-read shape). Inputs occupy
//! the lowest ids of each arena; every op appends exactly one new value.

use alloc::vec;
use alloc::vec::Vec;

use crate::binfhe::circuit_bs::{CircuitBootstrappingKey, circuit_bootstrap};
use crate::binfhe::keys::BootstrappingKey;
use crate::binfhe::lut::table_is_constant;
use crate::binfhe::pbs::binfhe_lut_read_dyn;
use crate::binfhe::lwe::{
    LweCiphertext, binfhe_not, binfhe_trivial, wire_delta,
};
use crate::binfhe::rgsw::{RgswCiphertext, cmux};
use crate::binfhe::rlwe::RlweCiphertext;

/// Boolean wire (LWE) id.
pub type WireId = u32;
/// RGSW wire id (circuit-bootstrap outputs).
pub type RgswId = u32;
/// RLWE content cell id.
pub type CellId = u32;
/// Index into [`BootstrapPlan::luts`].
pub type LutId = u32;

/// One scheduled operation. Wires produced by an op always have the next
/// free id of their arena, in layer order.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PlanOp {
    /// Cleartext constant wire (trivial encryption).
    Const { out: WireId, value: bool },
    /// Free NOT (exact linear op).
    Not { input: WireId, out: WireId },
    /// Multi-input LUT read; `inputs` are LSB-first. One blind rotation.
    Lut { inputs: Vec<WireId>, table: LutId, out: WireId },
    /// Circuit bootstrap: Boolean wire -> RGSW wire.
    CircuitBootstrap { input: WireId, out: RgswId },
    /// Oblivious select between two RLWE cells: `sel ? then : else`.
    RgswMux { sel: RgswId, then_cell: CellId, else_cell: CellId, out: CellId },
}

/// A logical lookup table (address-ordered entries, length `2^k`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LutSpec {
    pub entries: Vec<bool>,
}

/// The profile this schedule was built for (informational; the executable
/// const shape comes from the types used to run it).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ProfileId {
    Toy,
    ToyNoisy,
    Std128,
    Custom,
}

/// Failure-probability accounting recorded by the scheduler (plan §7.5).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FailureBudget {
    /// `log2` of the assumed per-bootstrap failure probability.
    pub per_bootstrap_log2: u32,
    /// `log2` of the whole-circuit failure bound: at least
    /// `per_bootstrap_log2 + ceil_log2(bootstrap_count)`.
    pub total_log2: u32,
}

/// A complete bootstrap schedule.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BootstrapPlan {
    pub profile: ProfileId,
    /// Circuit-wide maximum LUT arity; fixes the wire encoding
    /// `Delta = q / 2^(k_max + 1)`.
    pub k_max: u32,
    pub luts: Vec<LutSpec>,
    /// Topologically ordered layers; ops within a layer are independent.
    pub layers: Vec<Vec<PlanOp>>,
    /// Number of input Boolean wires (ids `0..num_inputs`).
    pub num_inputs: u32,
    /// Number of input RLWE cells (ids `0..num_cells`).
    pub num_cells: u32,
    /// Output Boolean wires.
    pub outputs: Vec<WireId>,
    /// Output RLWE cells.
    pub cell_outputs: Vec<CellId>,
    pub budget: FailureBudget,
}

/// A plan consistency failure.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PlanError {
    /// A LUT table is not a non-empty power of two.
    BadTableShape { table: LutId },
    /// A LUT arity exceeds `k_max`.
    ArityExceedsKMax { table: LutId },
    /// An op references a value that does not exist yet (topological
    /// violation) or is out of range.
    BadReference,
    /// Output list references a non-existent value.
    BadOutput,
    /// The recorded budget is inconsistent with the bootstrap count.
    BudgetInconsistent,
}

impl BootstrapPlan {
    /// Evaluate this schedule over clear Boolean wires and cells.
    ///
    /// This is a semantic oracle for adapters and serialized-plan consumers;
    /// unlike [`execute_plan`], it performs no cryptographic operation. A
    /// circuit-bootstrap result is the selected Boolean value, and an RGSW
    /// mux selects its `then_cell` exactly when that value is true.
    ///
    /// Panics on malformed input or structure. Call [`Self::validate`] first
    /// when invalid plans must be reported rather than rejected.
    pub fn execute_clear(
        &self,
        inputs: &[bool],
        cells: &[bool],
    ) -> (Vec<bool>, Vec<bool>) {
        assert_eq!(inputs.len(), self.num_inputs as usize, "input wire count");
        assert_eq!(cells.len(), self.num_cells as usize, "input cell count");

        let mut wires = inputs.to_vec();
        let mut rgsws = Vec::new();
        let mut cell_arena = cells.to_vec();
        for layer in &self.layers {
            for op in layer {
                match op {
                    PlanOp::Const { out, value } => {
                        assert_eq!(*out as usize, wires.len());
                        wires.push(*value);
                    }
                    PlanOp::Not { input, out } => {
                        assert_eq!(*out as usize, wires.len());
                        wires.push(!wires[*input as usize]);
                    }
                    PlanOp::Lut { inputs, table, out } => {
                        assert_eq!(*out as usize, wires.len());
                        let mut address = 0usize;
                        for (bit, input) in inputs.iter().enumerate() {
                            address |= (wires[*input as usize] as usize) << bit;
                        }
                        wires.push(self.luts[*table as usize].entries[address]);
                    }
                    PlanOp::CircuitBootstrap { input, out } => {
                        assert_eq!(*out as usize, rgsws.len());
                        rgsws.push(wires[*input as usize]);
                    }
                    PlanOp::RgswMux { sel, then_cell, else_cell, out } => {
                        assert_eq!(*out as usize, cell_arena.len());
                        cell_arena.push(if rgsws[*sel as usize] {
                            cell_arena[*then_cell as usize]
                        } else {
                            cell_arena[*else_cell as usize]
                        });
                    }
                }
            }
        }
        (wires, cell_arena)
    }

    /// Count blind rotations scheduled (one per non-constant LUT read, one
    /// per circuit-bootstrap level at evaluation time; here we count the
    /// ops, which is what the budget's `total_log2` refers to).
    pub fn bootstrap_op_count(&self) -> u64 {
        let mut count = 0u64;
        for layer in &self.layers {
            for op in layer {
                match op {
                    PlanOp::Lut { table, .. } => {
                        if !table_is_constant(&self.luts[*table as usize].entries) {
                            count += 1;
                        }
                    }
                    PlanOp::CircuitBootstrap { .. } => count += 1,
                    _ => {}
                }
            }
        }
        count
    }

    /// Structural validation: table shapes, reference validity, layer
    /// topology, output validity, budget consistency.
    pub fn validate(&self) -> Result<(), PlanError> {
        // Table shapes.
        for (i, spec) in self.luts.iter().enumerate() {
            let len = spec.entries.len();
            if len == 0 || !len.is_power_of_two() {
                return Err(PlanError::BadTableShape { table: i as LutId });
            }
            let arity = len.trailing_zeros() as usize;
            if arity > self.k_max as usize {
                return Err(PlanError::ArityExceedsKMax { table: i as LutId });
            }
        }
        // Reference validity + topology: walk ops, tracking arena sizes.
        let mut wires = self.num_inputs;
        let mut rgsws = 0u32;
        let mut cells = self.num_cells;
        for layer in &self.layers {
            for op in layer {
                match op {
                    PlanOp::Const { out, .. } => {
                        if *out != wires {
                            return Err(PlanError::BadReference);
                        }
                        wires += 1;
                    }
                    PlanOp::Not { input, out } => {
                        if *input >= wires || *out != wires {
                            return Err(PlanError::BadReference);
                        }
                        wires += 1;
                    }
                    PlanOp::Lut { inputs, table, out } => {
                        if (*table as usize) >= self.luts.len() {
                            return Err(PlanError::BadReference);
                        }
                        let arity = self.luts[*table as usize].entries.len().trailing_zeros();
                        if inputs.len() != arity as usize
                            || inputs.iter().any(|w| *w >= wires)
                            || *out != wires
                        {
                            return Err(PlanError::BadReference);
                        }
                        wires += 1;
                    }
                    PlanOp::CircuitBootstrap { input, out } => {
                        if *input >= wires || *out != rgsws {
                            return Err(PlanError::BadReference);
                        }
                        rgsws += 1;
                    }
                    PlanOp::RgswMux { sel, then_cell, else_cell, out } => {
                        if *sel >= rgsws
                            || *then_cell >= cells
                            || *else_cell >= cells
                            || *out != cells
                        {
                            return Err(PlanError::BadReference);
                        }
                        cells += 1;
                    }
                }
            }
        }
        if self.outputs.iter().any(|w| *w >= wires)
            || self.cell_outputs.iter().any(|c| *c >= cells)
        {
            return Err(PlanError::BadOutput);
        }
        // Budget consistency: total must cover the scheduled bootstrap ops.
        let count = self.bootstrap_op_count();
        if count > 0 {
            let log2_count = 64 - count.leading_zeros(); // ceil_log2(count)+1 safe upper
            if self.budget.total_log2 < self.budget.per_bootstrap_log2
                || (self.budget.total_log2 - self.budget.per_bootstrap_log2) + 1 < log2_count
            {
                return Err(PlanError::BudgetInconsistent);
            }
        }
        Ok(())
    }

    /// Deterministic FNV-1a (64-bit) hash of the canonical plan encoding.
    /// Identifies the exact scheduled computation in test evidence.
    pub fn plan_hash(&self) -> u64 {
        fn feed(h: &mut u64, bytes: &[u8]) {
            for &b in bytes {
                *h ^= b as u64;
                *h = h.wrapping_mul(0x100000001b3);
            }
        }
        let mut h = 0xcbf29ce484222325u64;
        macro_rules! feed_u32 { ($x:expr) => { feed(&mut h, &($x as u32).to_le_bytes()) } }
        macro_rules! feed_b { ($x:expr) => { feed(&mut h, &[$x]) } }
        feed_b!(self.profile as u8);
        feed_u32!(self.k_max);
        feed_u32!(self.luts.len() as u32);
        for spec in &self.luts {
            feed_u32!(spec.entries.len() as u32);
            for (i, chunk) in spec.entries.chunks(8).enumerate() {
                let mut byte = 0u8;
                for (j, &e) in chunk.iter().enumerate() {
                    byte |= (e as u8) << j;
                }
                feed_b!(byte);
                let _ = i;
            }
        }
        feed_u32!(self.layers.len() as u32);
        for layer in &self.layers {
            feed_u32!(layer.len() as u32);
            for op in layer {
                match op {
                    PlanOp::Const { out, value } => {
                        feed_b!(0);
                        feed_u32!(*out);
                        feed_b!(*value as u8);
                    }
                    PlanOp::Not { input, out } => {
                        feed_b!(1);
                        feed_u32!(*input);
                        feed_u32!(*out);
                    }
                    PlanOp::Lut { inputs, table, out } => {
                        feed_b!(2);
                        feed_u32!(inputs.len() as u32);
                        for w in inputs {
                            feed_u32!(*w);
                        }
                        feed_u32!(*table);
                        feed_u32!(*out);
                    }
                    PlanOp::CircuitBootstrap { input, out } => {
                        feed_b!(3);
                        feed_u32!(*input);
                        feed_u32!(*out);
                    }
                    PlanOp::RgswMux { sel, then_cell, else_cell, out } => {
                        feed_b!(4);
                        feed_u32!(*sel);
                        feed_u32!(*then_cell);
                        feed_u32!(*else_cell);
                        feed_u32!(*out);
                    }
                }
            }
        }
        feed_u32!(self.num_inputs);
        feed_u32!(self.num_cells);
        for w in &self.outputs {
            feed_u32!(*w);
        }
        for c in &self.cell_outputs {
            feed_u32!(*c);
        }
        feed_u32!(self.budget.per_bootstrap_log2);
        feed_u32!(self.budget.total_log2);
        h
    }
}

/// The reference interpreter: execute a validated plan over encrypted
/// inputs.
///
/// `inputs` must contain exactly `plan.num_inputs` Boolean wires (encoded
/// at the plan's `Delta`); `cells` must contain `plan.num_cells` RLWE
/// cells. Returns the full Boolean-wire and cell arenas (outputs are at
/// `plan.outputs` / `plan.cell_outputs`).
///
/// Panics on malformed input or unvalidated structure — call
/// [`BootstrapPlan::validate`] first for a diagnosable error.
pub fn execute_plan<
    const N_LWE: usize,
    const BIG_N: usize,
    const LOG_Q: u32,
    const LOG_Q_LWE: u32,
    const LOG_MOD_KS: u32,
    const BS_ELL: usize,
    const BS_BASE_LOG: u32,
    const KS_ELL: usize,
    const KS_BASE_LOG: u32,
    const PRIV_ELL: usize,
    const PRIV_BASE_LOG: u32,
>(
    plan: &BootstrapPlan,
    inputs: &[LweCiphertext<N_LWE>],
    cells: &[RlweCiphertext<BIG_N>],
    bk: &BootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL>,
    cbk: &CircuitBootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL, PRIV_ELL>,
) -> (Vec<LweCiphertext<N_LWE>>, Vec<RlweCiphertext<BIG_N>>) {
    assert_eq!(inputs.len(), plan.num_inputs as usize, "input wire count");
    assert_eq!(cells.len(), plan.num_cells as usize, "input cell count");
    let delta = wire_delta::<LOG_Q_LWE>(plan.k_max as usize);

    let mut wires: Vec<LweCiphertext<N_LWE>> = inputs.to_vec();
    let mut rgsws: Vec<RgswCiphertext<BIG_N, BS_ELL>> = Vec::new();
    let mut cell_arena: Vec<RlweCiphertext<BIG_N>> = cells.to_vec();

    for layer in &plan.layers {
        for op in layer {
            match op {
                PlanOp::Const { out, value } => {
                    assert_eq!(*out as usize, wires.len());
                    wires.push(binfhe_trivial::<N_LWE, LOG_Q_LWE>(*value, delta));
                }
                PlanOp::Not { input, out } => {
                    assert_eq!(*out as usize, wires.len());
                    wires.push(binfhe_not::<N_LWE, LOG_Q_LWE>(
                        &wires[*input as usize],
                        delta,
                    ));
                }
                PlanOp::Lut { inputs, table, out } => {
                    assert_eq!(*out as usize, wires.len());
                    let spec = &plan.luts[*table as usize];
                    let arity = spec.entries.len().trailing_zeros() as usize;
                    assert_eq!(inputs.len(), arity, "LUT arity");
                    // Shared executor with weaver-generated code.
                    let cts: Vec<LweCiphertext<N_LWE>> = inputs
                        .iter()
                        .map(|w| wires[*w as usize])
                        .collect();
                    wires.push(binfhe_lut_read_dyn::<
                        N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, LOG_MOD_KS,
                        BS_ELL, BS_BASE_LOG, KS_ELL, KS_BASE_LOG,
                    >(&cts, &spec.entries, plan.k_max as usize, bk));
                }
                PlanOp::CircuitBootstrap { input, out } => {
                    assert_eq!(*out as usize, rgsws.len());
                    rgsws.push(circuit_bootstrap::<
                        N_LWE, BIG_N, LOG_Q, LOG_Q_LWE, BS_ELL, BS_BASE_LOG,
                        KS_ELL, PRIV_ELL, PRIV_BASE_LOG,
                    >(&wires[*input as usize], cbk, plan.k_max as usize));
                }
                PlanOp::RgswMux { sel, then_cell, else_cell, out } => {
                    assert_eq!(*out as usize, cell_arena.len());
                    let out_cell = cmux::<BIG_N, LOG_Q, BS_ELL, BS_BASE_LOG>(
                        &rgsws[*sel as usize],
                        &cell_arena[*then_cell as usize],
                        &cell_arena[*else_cell as usize],
                    );
                    cell_arena.push(out_cell);
                }
            }
        }
    }
    (wires, cell_arena)
}

/// Re-export the runtime LUT shape check for plan builders.
pub use crate::binfhe::lut::check_lut_shape as validate_lut_shape;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binfhe::circuit_bs::gen_circuit_bootstrapping_key;
    use crate::binfhe::lwe::{LweSecretKey, gen_lwe_secret_key, lwe_encrypt, lwe_phase};
    use crate::binfhe::params::toy;
    use crate::binfhe::rlwe::{RlweSecretKey, gen_rlwe_secret_key, rlwe_trivial};
    use crate::SpecRng;

    struct TestRng(u64);
    impl TestRng {
        fn new(seed: u64) -> Self {
            Self(seed)
        }
    }
    impl SpecRng for TestRng {
        fn next_u32(&mut self) -> u32 {
            self.0 = self.0.wrapping_add(0x9e3779b97f4a7c15);
            let mut z = self.0;
            z = (z ^ (z >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
            z = (z ^ (z >> 27)).wrapping_mul(0x94d049bb133111eb);
            z = z ^ (z >> 31);
            z as u32
        }
    }

    type ToyCbk = CircuitBootstrappingKey<
        { toy::N_LWE },
        { toy::BIG_N },
        { toy::BS_ELL },
        { toy::KS_ELL },
        { toy::PRIV_ELL },
    >;

    fn toy_keys(seed: u64) -> (LweSecretKey<{ toy::N_LWE }>, RlweSecretKey<{ toy::BIG_N }>, ToyCbk) {
        let mut rng = TestRng::new(seed);
        let lwe_sk = gen_lwe_secret_key(&mut rng);
        let rlwe_sk = gen_rlwe_secret_key(&mut rng);
        let cbk = gen_circuit_bootstrapping_key::<
            { toy::N_LWE }, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE },
            { toy::LOG_MOD_KS }, { toy::BS_ELL }, { toy::BS_BASE_LOG },
            { toy::KS_ELL }, { toy::KS_BASE_LOG }, { toy::PRIV_ELL }, { toy::PRIV_BASE_LOG },
            { toy::CBD_ETA }, _,
        >(&lwe_sk, &rlwe_sk, &mut rng);
        (lwe_sk, rlwe_sk, cbk)
    }

    /// (a AND b) XOR c as a plan: two 2-input LUTs in sequence.
    fn small_plan() -> BootstrapPlan {
        BootstrapPlan {
            profile: ProfileId::Toy,
            k_max: 2,
            luts: vec![
                LutSpec { entries: vec![false, false, false, true] }, // 0: AND
                LutSpec { entries: vec![false, true, true, false] },  // 1: XOR
            ],
            layers: vec![
                vec![PlanOp::Lut { inputs: vec![0, 1], table: 0, out: 3 }],
                vec![
                    PlanOp::Lut { inputs: vec![3, 2], table: 1, out: 4 },
                    PlanOp::Const { out: 5, value: true },
                    PlanOp::Not { input: 5, out: 6 },
                ],
            ],
            num_inputs: 3,
            num_cells: 0,
            outputs: vec![4, 6],
            cell_outputs: vec![],
            budget: FailureBudget { per_bootstrap_log2: 30, total_log2: 32 },
        }
    }

    fn run_toy_plan(
        plan: &BootstrapPlan,
        input_bits: &[bool],
        cells: &[RlweCiphertext<{ toy::BIG_N }>],
        sk: &LweSecretKey<{ toy::N_LWE }>,
        cbk: &ToyCbk,
    ) -> (Vec<LweCiphertext<{ toy::N_LWE }>>, Vec<RlweCiphertext<{ toy::BIG_N }>>) {
        let delta = wire_delta::<{ toy::LOG_Q_LWE }>(plan.k_max as usize);
        let inputs: Vec<_> = input_bits
            .iter()
            .enumerate()
            .map(|(i, &b)| {
                let mut rng = TestRng::new(5000 + i as u64);
                lwe_encrypt::<{ toy::N_LWE }, { toy::LOG_Q_LWE }, 0, _>(b, delta, sk, &mut rng)
            })
            .collect();
        execute_plan::<
            { toy::N_LWE }, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE },
            { toy::LOG_MOD_KS }, { toy::BS_ELL }, { toy::BS_BASE_LOG },
            { toy::KS_ELL }, { toy::KS_BASE_LOG }, { toy::PRIV_ELL }, { toy::PRIV_BASE_LOG },
        >(plan, &inputs, cells, &cbk.bk, cbk)
    }

    #[test]
    fn small_plan_validates_and_executes() {
        let (sk, _, cbk) = toy_keys(0x50A1);
        let plan = small_plan();
        plan.validate().unwrap();
        let delta = wire_delta::<{ toy::LOG_Q_LWE }>(2);
        for a in [false, true] {
            for b in [false, true] {
                for c in [false, true] {
                    let (wires, _) = run_toy_plan(&plan, &[a, b, c], &[], &sk, &cbk);
                    let expected = (a && b) ^ c;
                    assert_eq!(
                        lwe_phase::<{ toy::N_LWE }, { toy::LOG_Q_LWE }>(
                            &wires[plan.outputs[0] as usize],
                            &sk
                        ),
                        if expected { delta } else { 0 },
                        "plan output for ({a},{b},{c})"
                    );
                    // Second output: NOT(const true) = false.
                    assert_eq!(
                        lwe_phase::<{ toy::N_LWE }, { toy::LOG_Q_LWE }>(
                            &wires[plan.outputs[1] as usize],
                            &sk
                        ),
                        0
                    );
                }
            }
        }
    }

    #[test]
    fn plan_execution_matches_direct_gate_calls() {
        // Determinism: plan execution is a deterministic function of the
        // input ciphertexts, so it must equal the direct gate path exactly.
        let (sk, _, cbk) = toy_keys(0x50A2);
        let plan = small_plan();
        let delta = wire_delta::<{ toy::LOG_Q_LWE }>(2);
        let mut rng = TestRng::new(0x50A2);
        let ca = lwe_encrypt::<{ toy::N_LWE }, { toy::LOG_Q_LWE }, 0, _>(true, delta, &sk, &mut rng);
        let cb = lwe_encrypt::<{ toy::N_LWE }, { toy::LOG_Q_LWE }, 0, _>(false, delta, &sk, &mut rng);
        let cc = lwe_encrypt::<{ toy::N_LWE }, { toy::LOG_Q_LWE }, 0, _>(true, delta, &sk, &mut rng);

        // Direct: AND then XOR via the gate wrappers (same tables).
        let and = crate::binfhe::pbs::binfhe_gate_and::<
            { toy::N_LWE }, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE },
            { toy::LOG_MOD_KS }, { toy::BS_ELL }, { toy::BS_BASE_LOG },
            { toy::KS_ELL }, { toy::KS_BASE_LOG }, 2,
        >(ca, cb, &cbk.bk);
        let direct = crate::binfhe::pbs::binfhe_gate_xor::<
            { toy::N_LWE }, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE },
            { toy::LOG_MOD_KS }, { toy::BS_ELL }, { toy::BS_BASE_LOG },
            { toy::KS_ELL }, { toy::KS_BASE_LOG }, 2,
        >(and, cc, &cbk.bk);

        let (wires, _) = run_toy_plan(&plan, &[true, false, true], &[], &sk, &cbk);
        // Note: the plan inputs are re-encrypted inside run_toy_plan with
        // different seeds, so ciphertexts differ; phases must agree.
        assert_eq!(
            lwe_phase::<{ toy::N_LWE }, { toy::LOG_Q_LWE }>(&wires[4], &sk),
            lwe_phase::<{ toy::N_LWE }, { toy::LOG_Q_LWE }>(&direct, &sk),
        );
    }

    #[test]
    fn plan_with_circuit_bootstrap_and_rgsw_mux() {
        let (sk, rlwe_sk, cbk) = toy_keys(0x50A3);
        // Cells: two RLWE contents; sel chooses between them; also carry a
        // plain LUT in the same plan.
        let mut c0 = rlwe_trivial::<{ toy::BIG_N }, 7>(&[0u32; toy::BIG_N]);
        let mut c1 = rlwe_trivial::<{ toy::BIG_N }, 7>(&[0u32; toy::BIG_N]);
        for i in 0..toy::BIG_N {
            c0.b[i] = (i as u32 * 3 + 1) & 0x7F;
            c1.b[i] = (i as u32 * 5 + 2) & 0x7F;
        }
        let plan = BootstrapPlan {
            profile: ProfileId::Toy,
            k_max: 2,
            luts: vec![LutSpec { entries: vec![true, false] }], // NOT as LUT
            layers: vec![
                vec![
                    PlanOp::Lut { inputs: vec![0], table: 0, out: 1 },
                    PlanOp::CircuitBootstrap { input: 0, out: 0 },
                ],
                vec![PlanOp::RgswMux { sel: 0, then_cell: 1, else_cell: 0, out: 2 }],
            ],
            num_inputs: 1,
            num_cells: 2,
            outputs: vec![1],
            cell_outputs: vec![2],
            budget: FailureBudget { per_bootstrap_log2: 30, total_log2: 33 },
        };
        plan.validate().unwrap();
        for m in [false, true] {
            let (wires, cells) = run_toy_plan(&plan, &[m], &[c0, c1], &sk, &cbk);
            let delta = wire_delta::<{ toy::LOG_Q_LWE }>(2);
            assert_eq!(
                lwe_phase::<{ toy::N_LWE }, { toy::LOG_Q_LWE }>(&wires[1], &sk),
                if !m { delta } else { 0 },
                "LUT output for {m}"
            );
            // Selected cell must equal plaintext selection.
            let out = &cells[plan.cell_outputs[0] as usize];
            let expected = if m { &c1 } else { &c0 };
            for i in 0..toy::BIG_N {
                // Phase via the public rlwe_phase (checked elsewhere against
                // an independent convolution).
                let phase = crate::binfhe::rlwe::rlwe_phase::<{ toy::BIG_N }, 7>(out, &rlwe_sk);
                assert_eq!(phase[i], expected.b[i], "cell coeff {i}, m={m}");
            }
        }
    }

    #[test]
    fn validate_rejects_bad_structure() {
        let mut plan = small_plan();
        // Bad table shape.
        plan.luts[0].entries = vec![true; 3];
        assert!(matches!(
            plan.validate(),
            Err(PlanError::BadTableShape { table: 0 })
        ));
        // Arity over k_max.
        plan.luts[0].entries = vec![false; 8];
        assert!(matches!(
            plan.validate(),
            Err(PlanError::ArityExceedsKMax { table: 0 })
        ));
        // Out-of-range reference.
        plan = small_plan();
        plan.layers[0][0] = PlanOp::Lut { inputs: vec![0, 9], table: 0, out: 3 };
        assert!(matches!(plan.validate(), Err(PlanError::BadReference)));
        // Non-sequential output id.
        plan = small_plan();
        plan.layers[0][0] = PlanOp::Lut { inputs: vec![0, 1], table: 0, out: 4 };
        assert!(matches!(plan.validate(), Err(PlanError::BadReference)));
        // Bad output.
        plan = small_plan();
        plan.outputs = vec![42];
        assert!(matches!(plan.validate(), Err(PlanError::BadOutput)));
        // Inconsistent budget.
        plan = small_plan();
        plan.budget.total_log2 = 30;
        assert!(matches!(plan.validate(), Err(PlanError::BudgetInconsistent)));
    }

    #[test]
    fn clear_execution_matches_plan_semantics() {
        let plan = small_plan();
        plan.validate().unwrap();
        for a in [false, true] {
            for b in [false, true] {
                for c in [false, true] {
                    let (wires, cells) = plan.execute_clear(&[a, b, c], &[]);
                    assert_eq!(wires[plan.outputs[0] as usize], (a && b) ^ c);
                    assert!(!wires[plan.outputs[1] as usize]);
                    assert!(cells.is_empty());
                }
            }
        }
    }

    #[test]
    fn clear_execution_circuit_bootstrap_muxes_cells() {
        let plan = BootstrapPlan {
            profile: ProfileId::Toy,
            k_max: 1,
            luts: vec![],
            layers: vec![
                vec![PlanOp::CircuitBootstrap { input: 0, out: 0 }],
                vec![PlanOp::RgswMux { sel: 0, then_cell: 1, else_cell: 0, out: 2 }],
            ],
            num_inputs: 1,
            num_cells: 2,
            outputs: vec![],
            cell_outputs: vec![2],
            budget: FailureBudget { per_bootstrap_log2: 30, total_log2: 30 },
        };
        plan.validate().unwrap();
        assert_eq!(plan.execute_clear(&[false], &[false, true]).1[2], false);
        assert_eq!(plan.execute_clear(&[true], &[false, true]).1[2], true);
    }

    #[test]
    fn plan_hash_is_deterministic_and_sensitive() {
        let a = small_plan();
        let b = small_plan();
        assert_eq!(a.plan_hash(), b.plan_hash());
        let mut c = small_plan();
        c.luts[1].entries = vec![false, false, true, true];
        assert_ne!(a.plan_hash(), c.plan_hash());
        let mut d = small_plan();
        d.k_max = 3;
        assert_ne!(a.plan_hash(), d.plan_hash());
    }
}
