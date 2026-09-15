// @pinnedness: unpinned
// @stability: very-unstable
// @ai: assisted
//! Fixed-shape plaintext/ciphertext boundary for a validated [`BootstrapPlan`].
//!
//! The plan executor intentionally operates only on encrypted wire and cell
//! arenas. This module owns the narrow host-side conversion seam:
//!
//! ```text
//! plaintext bits --encrypt_inputs--> LWE wire arena --execute_plan--> LWE arena
//! LWE output arena --decrypt_outputs--> plaintext bits
//! ```
//!
//! It is generic over the selected `binfhe` profile and has no transport,
//! session, key-store, FHE-provider, or LLVM-artifact dependency. In
//! particular it does not use the historical `FheScheme` / Track-S TFHE path.
//!
//! # Validation order
//!
//! [`PlanBoundary::new`] validates plan structure and wire encoding *before*
//! accepting either plaintext or ciphertext inputs. This binds `k_max`, LUT
//! shape, output IDs, and the failure-budget record to the one delta used for
//! both conversion directions. It does not establish a security or parameter
//! claim; callers must still admit a profile/key epoch/session separately.
//!
//! # Deferred checks
//!
//! See `docs/fhe/future-provider-integration-ledger.md`.

use alloc::vec::Vec;

use crate::SpecRng;
use crate::binfhe::circuit_bs::CircuitBootstrappingKey;
use crate::binfhe::keys::BootstrappingKey;
use crate::binfhe::lwe::{
    LweCiphertext, LweSecretKey, checked_wire_delta, lwe_decrypt, lwe_encrypt,
};
use crate::binfhe::plan::{
    BootstrapPlan, PlanError, PlanWorkspace, PlanWorkspaceError, execute_plan,
    execute_plan_in_workspace,
};
use crate::binfhe::rlwe::RlweCiphertext;

/// Failure to construct or use a plan encryption/decryption boundary.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BoundaryError {
    /// The serialized/constructed plan is not structurally executable.
    InvalidPlan(PlanError),
    /// The plan's `k_max` cannot be represented by this LWE wire modulus.
    InvalidWireEncoding { k_max: u32, log_q_lwe: u32 },
    /// Caller plaintext input count differs from the fixed plan shape.
    InputCount { expected: usize, actual: usize },
    /// Caller encrypted input count differs from the fixed plan shape.
    CiphertextInputCount { expected: usize, actual: usize },
    /// A caller-provided reusable input arena cannot hold the fixed plan shape.
    InputBufferCapacity { required: usize, actual: usize },
    /// The complete returned wire arena cannot contain a declared output.
    WireArenaTooShort { required: usize, actual: usize },
    /// Caller cell input count differs from the fixed plan shape.
    CellCount { expected: usize, actual: usize },
    /// A reusable no-std plan workspace is invalid, undersized, or received
    /// a plan/input shape it cannot execute.
    Workspace(PlanWorkspaceError),
}

/// Validated host-side conversion contract for one bootstrap plan.
///
/// The plan is borrowed rather than copied so one immutable, already-validated
/// artifact drives encryption, evaluation, and decryption. This is the deep
/// boundary module: callers learn a bit/cell shape and receive typed arenas;
/// they do not reconstruct delta or inspect plan internals.
#[derive(Clone, Copy, Debug)]
pub struct PlanBoundary<'a> {
    plan: &'a BootstrapPlan,
    delta: u32,
}

/// Reusable encrypted input arena for [`PlanBoundary`].
///
/// Create it once for a validated boundary, refill it with
/// [`PlanBoundary::encrypt_inputs_into`] for each invocation, and pass its
/// slice to [`PlanBoundary::execute_in_workspace`]. The arena is deliberately
/// separate from the executor workspace because it is the immutable input
/// borrowed while that workspace resets/fills its internal arenas.
pub struct CiphertextInputBuffer<const N_LWE: usize> {
    ciphertexts: Vec<LweCiphertext<N_LWE>>,
}

impl<const N_LWE: usize> CiphertextInputBuffer<N_LWE> {
    /// Borrow the current fixed-shape encrypted input sequence.
    pub fn as_slice(&self) -> &[LweCiphertext<N_LWE>] {
        &self.ciphertexts
    }

    /// Capacity fixed when the buffer was created. A no-std WASM caller can
    /// account for this before entering the module.
    pub fn capacity(&self) -> usize {
        self.ciphertexts.capacity()
    }
}

impl<'a> PlanBoundary<'a> {
    /// Validate `plan` and construct its unique Boolean wire conversion
    /// boundary for modulus `2^LOG_Q_LWE`.
    // TODO(provider-ledger: FHE-PLUMB-PROVIDER-01): bind a selected profile's
    // canonical frame/key-epoch metadata here or in its outer adapter. The
    // current type-level arithmetic boundary has no frame serialization.
    pub fn new<const LOG_Q_LWE: u32>(plan: &'a BootstrapPlan) -> Result<Self, BoundaryError> {
        plan.validate().map_err(BoundaryError::InvalidPlan)?;
        let delta = checked_wire_delta::<LOG_Q_LWE>(plan.k_max as usize).ok_or(
            BoundaryError::InvalidWireEncoding {
                k_max: plan.k_max,
                log_q_lwe: LOG_Q_LWE,
            },
        )?;
        Ok(Self { plan, delta })
    }

    /// The validated schedule shared by both conversion directions.
    pub const fn plan(&self) -> &'a BootstrapPlan {
        self.plan
    }

    /// The one canonical Boolean-wire delta selected by the plan shape.
    pub const fn delta(&self) -> u32 {
        self.delta
    }

    /// Allocate a reusable ciphertext-input arena with this plan's exact
    /// input capacity. This is the one intended setup allocation for a
    /// no-std/WASM module invocation loop.
    pub fn input_buffer<const N_LWE: usize>(&self) -> CiphertextInputBuffer<N_LWE> {
        CiphertextInputBuffer {
            ciphertexts: Vec::with_capacity(self.plan.num_inputs as usize),
        }
    }

    /// Encrypt exactly the plan's Boolean inputs into a caller-owned reusable
    /// buffer. The buffer is cleared first, so no ciphertext input from a
    /// completed plaintext/FHE transition survives into the next call.
    ///
    /// This consumes randomness only for nontrivial inputs. The caller owns
    /// the `SpecRng` lifecycle; reproducible tests can supply a deterministic
    /// stream, while an outer production adapter supplies its own approved
    /// randomness source.
    pub fn encrypt_inputs_into<
        const N_LWE: usize,
        const LOG_Q_LWE: u32,
        const ETA: u32,
        R: SpecRng,
    >(
        &self,
        inputs: &[bool],
        sk: &LweSecretKey<N_LWE>,
        rng: &mut R,
        output: &mut CiphertextInputBuffer<N_LWE>,
    ) -> Result<(), BoundaryError> {
        self.require_input_count(inputs.len())?;
        if output.ciphertexts.capacity() < inputs.len() {
            return Err(BoundaryError::InputBufferCapacity {
                required: inputs.len(),
                actual: output.ciphertexts.capacity(),
            });
        }
        output.ciphertexts.clear();
        output.ciphertexts.extend(
            inputs
                .iter()
                .map(|&bit| lwe_encrypt::<N_LWE, LOG_Q_LWE, ETA, R>(bit, self.delta, sk, rng)),
        );
        Ok(())
    }

    /// Allocating compatibility wrapper around [`Self::encrypt_inputs_into`].
    /// No-std/WASM callers should allocate [`CiphertextInputBuffer`] once and
    /// use the non-allocating refill method instead.
    pub fn encrypt_inputs<const N_LWE: usize, const LOG_Q_LWE: u32, const ETA: u32, R: SpecRng>(
        &self,
        inputs: &[bool],
        sk: &LweSecretKey<N_LWE>,
        rng: &mut R,
    ) -> Result<Vec<LweCiphertext<N_LWE>>, BoundaryError> {
        let mut output = self.input_buffer::<N_LWE>();
        self.encrypt_inputs_into::<N_LWE, LOG_Q_LWE, ETA, R>(inputs, sk, rng, &mut output)?;
        Ok(output.ciphertexts)
    }

    /// Decrypt the plan's Boolean outputs in declared output order.
    ///
    /// `wire_arena` is the complete arena returned by [`execute_plan`], not a
    /// caller-selected list. That prevents an outer adapter from silently
    /// treating a non-output intermediate as an authorized decrypt result.
    pub fn decrypt_outputs<const N_LWE: usize, const LOG_Q_LWE: u32>(
        &self,
        wire_arena: &[LweCiphertext<N_LWE>],
        sk: &LweSecretKey<N_LWE>,
    ) -> Result<Vec<bool>, BoundaryError> {
        let required = self
            .plan
            .outputs
            .iter()
            .copied()
            .max()
            .map(|wire| wire as usize + 1)
            .unwrap_or(0);
        if wire_arena.len() < required {
            return Err(BoundaryError::WireArenaTooShort {
                required,
                actual: wire_arena.len(),
            });
        }
        Ok(self
            .plan
            .outputs
            .iter()
            .map(|&wire| {
                lwe_decrypt::<N_LWE, LOG_Q_LWE>(&wire_arena[wire as usize], sk, self.delta)
            })
            .collect())
    }

    /// Evaluate a fully encrypted plan using the same plan/key material as
    /// the reference executor, after checking both fixed input shapes.
    ///
    /// This deliberately returns encrypted arenas. Only
    /// [`Self::decrypt_outputs`] authorizes conversion of declared Boolean
    /// outputs; RLWE cell output handling remains a provider/ORAM boundary.
    #[allow(clippy::too_many_arguments)]
    pub fn execute<
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
        &self,
        inputs: &[LweCiphertext<N_LWE>],
        cells: &[RlweCiphertext<BIG_N>],
        bk: &BootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL>,
        cbk: &CircuitBootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL, PRIV_ELL>,
    ) -> Result<(Vec<LweCiphertext<N_LWE>>, Vec<RlweCiphertext<BIG_N>>), BoundaryError> {
        self.require_ciphertext_input_count(inputs.len())?;
        self.require_cell_count(cells.len())?;
        Ok(execute_plan::<
            N_LWE,
            BIG_N,
            LOG_Q,
            LOG_Q_LWE,
            LOG_MOD_KS,
            BS_ELL,
            BS_BASE_LOG,
            KS_ELL,
            KS_BASE_LOG,
            PRIV_ELL,
            PRIV_BASE_LOG,
        >(self.plan, inputs, cells, bk, cbk))
    }

    /// Evaluate with a caller-owned bounded `alloc` workspace. Every call
    /// resets plan-local wire/RGSW/cell/LUT arenas before loading these inputs;
    /// no logical global or storage arena is retained across transitions.
    #[allow(clippy::too_many_arguments)]
    pub fn execute_in_workspace<
        'w,
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
        &self,
        inputs: &[LweCiphertext<N_LWE>],
        cells: &[RlweCiphertext<BIG_N>],
        bk: &BootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL>,
        cbk: &CircuitBootstrappingKey<N_LWE, BIG_N, BS_ELL, KS_ELL, PRIV_ELL>,
        workspace: &'w mut PlanWorkspace<N_LWE, BIG_N, BS_ELL>,
    ) -> Result<(&'w [LweCiphertext<N_LWE>], &'w [RlweCiphertext<BIG_N>]), BoundaryError> {
        self.require_ciphertext_input_count(inputs.len())?;
        self.require_cell_count(cells.len())?;
        execute_plan_in_workspace::<
            N_LWE,
            BIG_N,
            LOG_Q,
            LOG_Q_LWE,
            LOG_MOD_KS,
            BS_ELL,
            BS_BASE_LOG,
            KS_ELL,
            KS_BASE_LOG,
            PRIV_ELL,
            PRIV_BASE_LOG,
        >(self.plan, inputs, cells, bk, cbk, workspace)
        .map_err(BoundaryError::Workspace)
    }

    fn require_input_count(&self, actual: usize) -> Result<(), BoundaryError> {
        let expected = self.plan.num_inputs as usize;
        if actual == expected {
            Ok(())
        } else {
            Err(BoundaryError::InputCount { expected, actual })
        }
    }

    fn require_ciphertext_input_count(&self, actual: usize) -> Result<(), BoundaryError> {
        let expected = self.plan.num_inputs as usize;
        if actual == expected {
            Ok(())
        } else {
            Err(BoundaryError::CiphertextInputCount { expected, actual })
        }
    }

    fn require_cell_count(&self, actual: usize) -> Result<(), BoundaryError> {
        let expected = self.plan.num_cells as usize;
        if actual == expected {
            Ok(())
        } else {
            Err(BoundaryError::CellCount { expected, actual })
        }
    }
}

#[cfg(test)]
mod tests {
    use alloc::vec;

    use super::*;
    use crate::SpecRng;
    use crate::binfhe::circuit_bs::gen_circuit_bootstrapping_key;
    use crate::binfhe::keys::gen_bootstrapping_key;
    use crate::binfhe::lwe::gen_lwe_secret_key;
    use crate::binfhe::params::toy;
    use crate::binfhe::plan::{FailureBudget, LutSpec, PlanOp, ProfileId};
    use crate::binfhe::rlwe::gen_rlwe_secret_key;

    struct TestRng(u64);

    impl TestRng {
        const fn new(seed: u64) -> Self {
            Self(seed)
        }
    }

    impl SpecRng for TestRng {
        fn next_u32(&mut self) -> u32 {
            self.0 = self.0.wrapping_add(0x9e3779b97f4a7c15);
            let mut z = self.0;
            z = (z ^ (z >> 30)).wrapping_mul(0xbf58476d1ce4e5b9);
            z = (z ^ (z >> 27)).wrapping_mul(0x94d049bb133111eb);
            (z ^ (z >> 31)) as u32
        }
    }

    fn and_plan() -> BootstrapPlan {
        BootstrapPlan {
            profile: ProfileId::Toy,
            k_max: 2,
            luts: vec![LutSpec {
                entries: vec![false, false, false, true],
            }],
            layers: vec![vec![PlanOp::Lut {
                inputs: vec![0, 1],
                table: 0,
                out: 2,
            }]],
            num_inputs: 2,
            num_cells: 0,
            outputs: vec![2],
            cell_outputs: vec![],
            budget: FailureBudget {
                per_bootstrap_log2: 30,
                total_log2: 30,
            },
        }
    }

    #[test]
    fn boundary_encrypts_executes_and_decrypts_declared_outputs() {
        let plan = and_plan();
        let boundary = PlanBoundary::new::<{ toy::LOG_Q_LWE }>(&plan).unwrap();
        let mut key_rng = TestRng::new(0xB0A0_DA7A);
        let lwe_sk = gen_lwe_secret_key(&mut key_rng);
        let rlwe_sk = gen_rlwe_secret_key(&mut key_rng);
        let bk = gen_bootstrapping_key::<
            { toy::N_LWE },
            { toy::BIG_N },
            { toy::LOG_Q },
            { toy::LOG_Q_LWE },
            { toy::LOG_MOD_KS },
            { toy::BS_ELL },
            { toy::BS_BASE_LOG },
            { toy::KS_ELL },
            { toy::KS_BASE_LOG },
            { toy::CBD_ETA },
            _,
        >(&lwe_sk, &rlwe_sk, &mut key_rng);
        let cbk = gen_circuit_bootstrapping_key::<
            { toy::N_LWE },
            { toy::BIG_N },
            { toy::LOG_Q },
            { toy::LOG_Q_LWE },
            { toy::LOG_MOD_KS },
            { toy::BS_ELL },
            { toy::BS_BASE_LOG },
            { toy::KS_ELL },
            { toy::KS_BASE_LOG },
            { toy::PRIV_ELL },
            { toy::PRIV_BASE_LOG },
            { toy::CBD_ETA },
            _,
        >(&lwe_sk, &rlwe_sk, &mut key_rng);

        for left in [false, true] {
            for right in [false, true] {
                let mut input_rng = TestRng::new((left as u64) << 8 | right as u64);
                let encrypted = boundary
                    .encrypt_inputs::<{ toy::N_LWE }, { toy::LOG_Q_LWE }, { toy::CBD_ETA }, _>(
                        &[left, right],
                        &lwe_sk,
                        &mut input_rng,
                    )
                    .unwrap();
                let (wires, cells) = boundary
                    .execute::<
                        { toy::N_LWE }, { toy::BIG_N }, { toy::LOG_Q },
                        { toy::LOG_Q_LWE }, { toy::LOG_MOD_KS }, { toy::BS_ELL },
                        { toy::BS_BASE_LOG }, { toy::KS_ELL }, { toy::KS_BASE_LOG },
                        { toy::PRIV_ELL }, { toy::PRIV_BASE_LOG },
                    >(&encrypted, &[], &bk, &cbk)
                    .unwrap();
                assert!(cells.is_empty());
                assert_eq!(
                    boundary.decrypt_outputs::<{ toy::N_LWE }, { toy::LOG_Q_LWE }>(&wires, &lwe_sk),
                    Ok(vec![left && right]),
                );
            }
        }
    }

    #[test]
    fn workspace_execution_resets_plan_arenas_between_calls() {
        let plan = and_plan();
        let boundary = PlanBoundary::new::<{ toy::LOG_Q_LWE }>(&plan).unwrap();
        let mut key_rng = TestRng::new(0xA11C_E5ED);
        let lwe_sk = gen_lwe_secret_key(&mut key_rng);
        let rlwe_sk = gen_rlwe_secret_key(&mut key_rng);
        let bk = gen_bootstrapping_key::<
            { toy::N_LWE },
            { toy::BIG_N },
            { toy::LOG_Q },
            { toy::LOG_Q_LWE },
            { toy::LOG_MOD_KS },
            { toy::BS_ELL },
            { toy::BS_BASE_LOG },
            { toy::KS_ELL },
            { toy::KS_BASE_LOG },
            { toy::CBD_ETA },
            _,
        >(&lwe_sk, &rlwe_sk, &mut key_rng);
        let cbk = gen_circuit_bootstrapping_key::<
            { toy::N_LWE },
            { toy::BIG_N },
            { toy::LOG_Q },
            { toy::LOG_Q_LWE },
            { toy::LOG_MOD_KS },
            { toy::BS_ELL },
            { toy::BS_BASE_LOG },
            { toy::KS_ELL },
            { toy::KS_BASE_LOG },
            { toy::PRIV_ELL },
            { toy::PRIV_BASE_LOG },
            { toy::CBD_ETA },
            _,
        >(&lwe_sk, &rlwe_sk, &mut key_rng);
        let mut workspace =
            PlanWorkspace::<{ toy::N_LWE }, { toy::BIG_N }, { toy::BS_ELL }>::new(&plan).unwrap();

        let mut inputs = boundary.input_buffer::<{ toy::N_LWE }>();
        for (left, right) in [(true, false), (true, true)] {
            let mut input_rng = TestRng::new((left as u64) << 4 | right as u64);
            boundary
                .encrypt_inputs_into::<{ toy::N_LWE }, { toy::LOG_Q_LWE }, 0, _>(
                    &[left, right],
                    &lwe_sk,
                    &mut input_rng,
                    &mut inputs,
                )
                .unwrap();
            let (wires, cells) = boundary
                .execute_in_workspace::<
                    { toy::N_LWE }, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE },
                    { toy::LOG_MOD_KS }, { toy::BS_ELL }, { toy::BS_BASE_LOG },
                    { toy::KS_ELL }, { toy::KS_BASE_LOG }, { toy::PRIV_ELL },
                    { toy::PRIV_BASE_LOG },
                >(inputs.as_slice(), &[], &bk, &cbk, &mut workspace)
                .unwrap();
            assert!(cells.is_empty());
            assert_eq!(
                boundary.decrypt_outputs::<{ toy::N_LWE }, { toy::LOG_Q_LWE }>(wires, &lwe_sk),
                Ok(vec![left && right]),
            );
        }
    }

    #[test]
    fn boundary_rejects_invalid_shapes_before_ciphertext_conversion() {
        let mut invalid = and_plan();
        invalid.outputs = vec![9];
        assert!(matches!(
            PlanBoundary::new::<{ toy::LOG_Q_LWE }>(&invalid),
            Err(BoundaryError::InvalidPlan(PlanError::BadOutput)),
        ));

        let plan = and_plan();
        let boundary = PlanBoundary::new::<{ toy::LOG_Q_LWE }>(&plan).unwrap();
        let sk = gen_lwe_secret_key(&mut TestRng::new(9));
        let mut rng = TestRng::new(10);
        assert_eq!(
            boundary.encrypt_inputs::<{ toy::N_LWE }, { toy::LOG_Q_LWE }, 0, _>(
                &[true],
                &sk,
                &mut rng,
            ),
            Err(BoundaryError::InputCount {
                expected: 2,
                actual: 1,
            }),
        );
    }
}
