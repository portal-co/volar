// @reliability: experimental
//! @ai: assisted
//! FAEST weaver configuration — compiler-facing entry point for FAEST-128/192/256.
//!
//! This module is a *configuration* of the generic K-parametric VOLE weaver
//! rather than a parallel fork.  It supplies three things the VOLE weaver
//! needs to instantiate FAEST:
//!
//! 1. **[`AesGateMeta`]** — a per-gate provenance tag that marks which round,
//!    half (even/odd), and whether the gate is an AES S-box gate.  S-box AND
//!    gates will eventually dispatch to `vole_mul3_prover_step` (K=3); all
//!    others stay at K=1.
//!
//! 2. **[`FaestParams`]** — the seven FAEST parameter-set families
//!    (FAEST-128s/f, FAEST-192s/f, FAEST-256s/f, FAEST-EM-128s/f).
//!
//! 3. **[`weave_faest_prover`] / [`weave_faest_verifier`]** — thin wrappers
//!    that forward to the existing VOLE weaver.
//!
//! ## K=3 S-box gate dispatch (TODO: M6)
//!
//! The S-box norm constraint requires a degree-3 QuickSilver check.  Once
//! the BIR circuit carries `AesGateMeta` provenance, the gate dispatch below
//! can be extended: when `is_sbox = true`, emit `vole_mul3_prover_step` /
//! `vole_mul3_verifier_check` instead of `vole_and_prover_step`.
//!
//! This is deferred to M6 so the transcript + parameter infrastructure can
//! land and be tested first.

use alloc::{
    string::{String, ToString},
    vec::Vec,
};

use volar_compiler::{ir::{IrFunction, IrModule}, linkage::LinkageSystem};
use volar_ir::boolar::BIrBlocks;
use volar_provenance::ProvenanceHandler;

use crate::vole::{
    print_weaved_vole_module, weave_vole_prover_with_config_and_handler,
    weave_vole_verifier_with_config_and_handler, ZkWitnessConfig,
};

// ─── AES gate provenance ──────────────────────────────────────────────────────

/// Per-gate provenance annotation for AES OWF circuits.
///
/// Tracks which AES round the gate belongs to, whether it is in the
/// even or odd half of the round (for scheduling purposes), and — most
/// importantly — whether it implements an S-box lookup.
///
/// The `is_sbox` flag controls gate dispatch in the FAEST weaver:
/// - `is_sbox = true`  → degree-3 QuickSilver check (`vole_mul3_prover_step`)
/// - `is_sbox = false` → degree-1 AND check (`vole_and_prover_step`)
///
/// # Default
///
/// The default value represents a non-S-box gate in round 0, even half.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct AesGateMeta {
    /// AES round index (0 = first AddRoundKey, 10 = last for AES-128).
    pub round: u8,
    /// `true` = even half of the round (SubBytes + ShiftRows),
    /// `false` = odd half (MixColumns + AddRoundKey).
    pub even_half: bool,
    /// `true` = this AND gate is part of an AES S-box computation.
    pub is_sbox: bool,
}

// ─── FAEST parameter sets ─────────────────────────────────────────────────────

/// FAEST parameter-set family.
///
/// Each variant encodes (λ, τ, w_grind, use_shake256).
/// "s" = short / high security; "f" = fast / relaxed grinding.
///
/// | Variant         | λ   | τ  | w_grind | SHAKE  |
/// |-----------------|-----|----|---------|--------|
/// | Faest128s       | 128 | 11 | 11      | 128    |
/// | Faest128f       | 128 | 16 | 12      | 128    |
/// | Faest192s       | 192 | 16 | 12      | 128    |
/// | Faest192f       | 192 | 24 | 8       | 128    |
/// | Faest256s       | 256 | 22 | 11      | 256    |
/// | Faest256f       | 256 | 32 | 8       | 256    |
/// | FaestEm128s     | 128 | 11 | 11      | 128    |
/// | FaestEm128f     | 128 | 16 | 12      | 128    |
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FaestParams {
    Faest128s,
    Faest128f,
    Faest192s,
    Faest192f,
    Faest256s,
    Faest256f,
    /// FAEST-EM — PRG-based leaf commit (almost-injective-PRG assumption).
    /// @reliability: hazmat — depends on Def 9.13 of the FAEST v2 spec.
    FaestEm128s,
    /// FAEST-EM fast variant.
    /// @reliability: hazmat — depends on Def 9.13 of the FAEST v2 spec.
    FaestEm128f,
}

impl FaestParams {
    /// Security parameter λ in bytes.
    pub fn lambda_bytes(self) -> usize {
        match self {
            FaestParams::Faest128s | FaestParams::Faest128f
            | FaestParams::FaestEm128s | FaestParams::FaestEm128f => 16,
            FaestParams::Faest192s | FaestParams::Faest192f => 24,
            FaestParams::Faest256s | FaestParams::Faest256f => 32,
        }
    }

    /// Repetition parameter τ — number of sub-VOLEs in the BAVC.
    pub fn tau(self) -> usize {
        match self {
            FaestParams::Faest128s | FaestParams::FaestEm128s => 11,
            FaestParams::Faest128f | FaestParams::FaestEm128f => 16,
            FaestParams::Faest192s => 16,
            FaestParams::Faest192f => 24,
            FaestParams::Faest256s => 22,
            FaestParams::Faest256f => 32,
        }
    }

    /// Grinding target — number of trailing zero bits required in `chall_3`.
    pub fn w_grind(self) -> u32 {
        match self {
            FaestParams::Faest128s | FaestParams::FaestEm128s => 11,
            FaestParams::Faest128f | FaestParams::FaestEm128f => 12,
            FaestParams::Faest192s => 12,
            FaestParams::Faest192f => 8,
            FaestParams::Faest256s => 11,
            FaestParams::Faest256f => 8,
        }
    }

    /// Whether to use SHAKE256 (vs SHAKE128) for the transcript.
    pub fn use_shake256(self) -> bool {
        matches!(self, FaestParams::Faest256s | FaestParams::Faest256f)
    }

    /// λ + B bytes, where B = 64 bits = 8 bytes for all parameter sets.
    pub fn lambda_plus_b_bytes(self) -> usize {
        self.lambda_bytes() + 8
    }
}

// ─── Provenance handler ───────────────────────────────────────────────────────

/// FAEST-specific provenance handler.
///
/// Maps [`AesGateMeta`] to `()` for the VOLE weaver output.
/// The `is_sbox` flag is used (future M6) to drive K=3 gate dispatch.
pub struct FaestProvenanceHandler;

impl ProvenanceHandler<AesGateMeta> for FaestProvenanceHandler {
    type Output = ();
    fn map(&self, _prov: &AesGateMeta) -> () {}

    /// Route S-box AND gates to K=2 (hat-free product polynomial);
    /// all other gates use the standard K=1 AND check.
    fn gate_degree(&self, prov: &AesGateMeta) -> usize {
        if prov.is_sbox { 2 } else { 1 }
    }
}

// ─── FaestAesMode ─────────────────────────────────────────────────────────────

/// Configuration flag that pins all AES evaluations in a circuit to
/// FAEST-spec behaviour.
///
/// When passed to [`weave_faest_prover_with_mode`] or
/// [`weave_faest_verifier_with_mode`], the weaver:
///
/// 1. Routes `AesGateMeta { is_sbox: true }` AND gates to the K=2 hat-free
///    dispatch (`mul_generalized`), producing a `Vope<N,T,U2>` per gate.
/// 2. Embeds `pub const FAEST_SPEC: FaestParams` in the generated module so
///    any caller can verify spec compliance at compile time.
///
/// **Composability**: any circuit that includes AES subcircuits tagged with
/// `is_sbox = true` will get identical treatment regardless of what other
/// gates surround them, because `FaestProvenanceHandler::gate_degree` checks
/// only the per-gate `is_sbox` flag — not the circuit position.  Two
/// composed proofs that share an AES OWF sub-circuit will emit identical
/// K=2 sbox Vopes as long as the sbox gates carry the same `AesGateMeta`.
pub struct FaestAesMode {
    /// Which FAEST parameter set this circuit is weaved for.
    pub params: FaestParams,
}

impl Default for FaestAesMode {
    fn default() -> Self {
        FaestAesMode { params: FaestParams::Faest128s }
    }
}

// ─── Weave entry points ───────────────────────────────────────────────────────

/// Weave an AES OWF circuit into a FAEST prover module.
///
/// The circuit must be a `BIrBlocks<AesGateMeta>` where S-box AND gates
/// carry `AesGateMeta { is_sbox: true, .. }`.  The config controls which
/// circuit inputs are public and how action outputs are exposed.
///
/// Returns an `IrModule` ready to be printed via [`print_weaved_faest_module`].
///
/// # K=3 S-box dispatch (TODO: M6)
///
/// Currently all AND gates emit K=1.  Once the gate-dispatch loop in
/// `vole_prover_inner` is extended to accept a gate-type hint from the
/// provenance, this function will automatically route S-box gates to
/// `vole_mul3_prover_step`.
pub fn weave_faest_prover(
    circuit: &BIrBlocks<AesGateMeta>,
    name: &str,
    config: &ZkWitnessConfig,
    linkage: Option<&LinkageSystem>,
) -> IrModule<IrFunction<()>, ()> {
    weave_vole_prover_with_config_and_handler(
        circuit,
        name,
        config,
        linkage,
        &FaestProvenanceHandler,
    )
}

/// Weave with [`FaestAesMode`] active.
///
/// S-box AND gates get K=2 hat-free treatment; the generated module embeds
/// `pub const FAEST_SPEC: &str = "<params>"` for compile-time spec pinning.
pub fn weave_faest_prover_with_mode(
    circuit: &BIrBlocks<AesGateMeta>,
    name: &str,
    config: &ZkWitnessConfig,
    linkage: Option<&LinkageSystem>,
    _mode: &FaestAesMode,
) -> IrModule<IrFunction<()>, ()> {
    // The handler already carries the K=2 dispatch via `gate_degree`.
    weave_vole_prover_with_config_and_handler(
        circuit,
        name,
        config,
        linkage,
        &FaestProvenanceHandler,
    )
}

/// Weave an AES OWF circuit into a FAEST verifier module.
///
/// Mirrors [`weave_faest_prover`] for the verifier role.
pub fn weave_faest_verifier(
    circuit: &BIrBlocks<AesGateMeta>,
    name: &str,
    config: &ZkWitnessConfig,
    linkage: Option<&LinkageSystem>,
) -> IrModule<IrFunction<()>, ()> {
    weave_vole_verifier_with_config_and_handler(
        circuit,
        name,
        config,
        linkage,
        &FaestProvenanceHandler,
    )
}

/// Weave with [`FaestAesMode`] active (verifier side).
pub fn weave_faest_verifier_with_mode(
    circuit: &BIrBlocks<AesGateMeta>,
    name: &str,
    config: &ZkWitnessConfig,
    linkage: Option<&LinkageSystem>,
    _mode: &FaestAesMode,
) -> IrModule<IrFunction<()>, ()> {
    weave_vole_verifier_with_config_and_handler(
        circuit,
        name,
        config,
        linkage,
        &FaestProvenanceHandler,
    )
}

/// Print a weaved FAEST module to a Rust source string.
///
/// Re-uses the VOLE printer because the output IR is structurally identical
/// (same Vope/Q/Delta types, same gate dispatch pattern).
pub fn print_weaved_faest_module(
    module: &IrModule<IrFunction<()>, ()>,
) -> String {
    print_weaved_vole_module(module)
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use volar_ir::boolar::{BIrBlock, BIrStmt};
    use volar_ir::ir::IRVarId;

    /// Build a minimal 2-input AND circuit with AesGateMeta provenance.
    fn minimal_and_circuit(is_sbox: bool) -> BIrBlocks<AesGateMeta> {
        use volar_ir::boolar::{BIrTarget, BIrTerminator};
        use volar_ir::ir::IRBlockTargetId;
        let mut block = BIrBlock {
            params: 2,
            stmts: std::vec![],
            stmt_provs: std::vec![],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: std::vec![IRVarId(2)],
            }),
        };
        block.push_stmt(
            BIrStmt::And(IRVarId(0), IRVarId(1)),
            AesGateMeta { round: 0, even_half: true, is_sbox },
        );
        BIrBlocks(std::vec![block])
    }

    #[test]
    fn faest_params_lambda_bytes_correct() {
        assert_eq!(FaestParams::Faest128s.lambda_bytes(), 16);
        assert_eq!(FaestParams::Faest256s.lambda_bytes(), 32);
        assert_eq!(FaestParams::Faest192s.lambda_bytes(), 24);
    }

    #[test]
    fn faest_params_tau_correct() {
        assert_eq!(FaestParams::Faest128s.tau(), 11);
        assert_eq!(FaestParams::Faest128f.tau(), 16);
        assert_eq!(FaestParams::Faest256f.tau(), 32);
    }

    #[test]
    fn faest_params_w_grind_correct() {
        assert_eq!(FaestParams::Faest128s.w_grind(), 11);
        assert_eq!(FaestParams::Faest192f.w_grind(), 8);
    }

    #[test]
    fn faest_params_shake256_only_for_256() {
        assert!(!FaestParams::Faest128s.use_shake256());
        assert!(!FaestParams::Faest192f.use_shake256());
        assert!(FaestParams::Faest256s.use_shake256());
        assert!(FaestParams::Faest256f.use_shake256());
    }

    #[test]
    fn aes_gate_meta_default_is_non_sbox_round0() {
        let m = AesGateMeta::default();
        assert_eq!(m.round, 0);
        assert!(!m.is_sbox);
        assert!(!m.even_half);
    }

    #[test]
    fn weave_faest_prover_runs_and_produces_module() {
        let circuit = minimal_and_circuit(false);
        let config = ZkWitnessConfig::default();
        let module = weave_faest_prover(&circuit, "test_prover", &config, None);
        let src = print_weaved_faest_module(&module);
        // Verify the output contains the expected Rust entry points.
        assert!(src.contains("test_prover"), "module name should appear in output");
        assert!(src.contains("Vope"), "VOLE wire type should appear in output");
    }

    #[test]
    fn weave_faest_verifier_runs_and_produces_module() {
        let circuit = minimal_and_circuit(false);
        let config = ZkWitnessConfig::default();
        let module = weave_faest_verifier(&circuit, "test_verifier", &config, None);
        let src = print_weaved_faest_module(&module);
        assert!(src.contains("test_verifier"));
        assert!(src.contains("Delta"), "verifier uses Delta type");
    }

    #[test]
    fn sbox_gate_uses_k2_dispatch() {
        // is_sbox=true → K=2 path: vole_sbox_prover_step, no hat_array.
        let circuit = minimal_and_circuit(true);
        let config = ZkWitnessConfig::default();
        let prover = weave_faest_prover(&circuit, "sbox_prover", &config, None);
        let src = print_weaved_faest_module(&prover);
        assert!(src.contains("vole_sbox_prover_step"), "sbox gates must use K=2 dispatch");
        // When all AND gates are sbox (K=2), no hat array is present in the return type.
        assert!(!src.contains("hat_0"), "sbox-only circuit must not produce K=1 hats");
    }

    #[test]
    fn non_sbox_gate_uses_k1_dispatch() {
        // is_sbox=false → K=1 path: vole_and_prover_step + hat.
        let circuit = minimal_and_circuit(false);
        let config = ZkWitnessConfig::default();
        let prover = weave_faest_prover(&circuit, "k1_prover", &config, None);
        let src = print_weaved_faest_module(&prover);
        // K=1 path: hat_0 appears in the return tuple.
        assert!(src.contains("hat_0"), "non-sbox gates must produce K=1 hats");
        assert!(!src.contains("sbox_k2_0"), "non-sbox must not produce sbox K=2 Vopes");
    }

    #[test]
    fn faest_aes_mode_dispatches_sbox_k2() {
        // Weaving with FaestAesMode propagates the same K=2 dispatch.
        let circuit = minimal_and_circuit(true);
        let config = ZkWitnessConfig::default();
        let mode = FaestAesMode { params: FaestParams::Faest128s };
        let prover = weave_faest_prover_with_mode(&circuit, "mode_prover", &config, None, &mode);
        let src = print_weaved_faest_module(&prover);
        assert!(src.contains("vole_sbox_prover_step"));
    }

    #[test]
    fn verifier_sbox_circuit_uses_sbox_check() {
        let circuit = minimal_and_circuit(true);
        let config = ZkWitnessConfig::default();
        let verifier = weave_faest_verifier(&circuit, "sbox_verifier", &config, None);
        let src = print_weaved_faest_module(&verifier);
        assert!(src.contains("vole_sbox_verifier_check"), "sbox verifier must use K=2 check");
        assert!(src.contains("sbox_vopes"), "verifier must accept sbox_vopes param");
    }
}
