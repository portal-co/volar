// @reliability: experimental
//! @ai: assisted
//! **Prove-the-verifier integration**: the arithmetization frontend and the
//! C-backend pipeline terminal that tie a woven VOLE verifier to the folding
//! core in [`volar_fold::verifier`].
//!
//! This crate is the build-side counterpart to
//! [`volar_fold::verifier`](../volar_fold/verifier/index.html).  It does two
//! things:
//!
//! 1. **Arithmetization frontend** ([`verifier_trace`]): assemble a
//!    [`VerifierTrace`] — the per-AND-gate verifier observations plus the memory
//!    multiset-accumulator boundary — that the folding core consumes.
//! 2. **C-backend terminal** ([`emit_verifier_c`]): lower a woven
//!    `Transparent` verifier `IrModule` to **C source** via
//!    [`volar_c_backend::CBackend`].  C is the executable substrate used here
//!    (the LLVM backend is gated separately and not always available); the
//!    generated verifier is what produces the concrete per-gate observations
//!    that feed the frontend.
//!
//! The fold + native verification themselves live in
//! [`volar_fold::verifier`]; [`prove_and_verify_folded`] wires them together.
//!
//! ## Discipline
//!
//! Everything here is on the non-ZK (`Transparent`) leg: the verifier is proven
//! *without* zero-knowledge because the inner VOLE proof already accounts for
//! it.  [`prove_and_verify_folded`] is bound `where Z: NonZk`, so a
//! [`volar_discipline::Zk`] artifact cannot enter this path — see
//! `docs/agent-context/discipline.md` and `docs/prove-the-verifier.md`.
//!
//! ## Honest scope
//!
//! [`verifier_trace`] models the gate check over the folding scalar field; the
//! binary-field ↔ prime-field embedding of the real `GF(2^k)` check is the
//! documented Tier-3 seam (see `docs/prove-the-verifier.md`).  The per-gate
//! [`GateObservation`]s are the verifier's *runtime* output — produced by
//! executing the [`emit_verifier_c`] verifier on a concrete proof; capturing
//! them from that execution is the remaining build-side hook.

use volar_compiler::ir::{IrFunction, IrModule};
use volar_c_backend::CBackend;
use volar_discipline::{NonZk, Tagged, Transparent};
use volar_fold::pedersen::PedersenParams;
use volar_fold::scalar::Scalar;
use volar_fold::verifier::{prove_verifier, verify_folded, VerifierFold, VerifierStep, VerifierTrace};
use volar_lir_codegen::lower_module_with_opts;
use volar_lir_codegen::mono::MonoEnv;

/// The verifier-side values observed for one AND gate during a verifier run:
/// the MAC shares `K_a, K_b, K_c`, the global secret `Δ`, and the prover-sent
/// opening `V̂`.  An honest gate satisfies `K_a·K_b + V̂ = K_c·Δ`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct GateObservation {
    /// Verifier MAC share of the left input wire.
    pub k_a: u64,
    /// Verifier MAC share of the right input wire.
    pub k_b: u64,
    /// Verifier MAC share of the output wire.
    pub k_c: u64,
    /// Verifier global secret Δ.
    pub delta: u64,
    /// Prover-sent gate opening V̂.
    pub v_hat: u64,
}

/// **Arithmetization frontend.** Assemble a [`VerifierTrace`] from a verifier's
/// per-gate observations and its memory-accumulator boundary
/// (`mem_acc_in` / `mem_acc_out`, the multiset-hash state under
/// `StorageMode::Commitment`).  The result is tagged [`Transparent`] — it is the
/// non-ZK computation that [`prove_and_verify_folded`] folds.
///
/// The folding challenges and Pedersen blinders are derived deterministically
/// from the step index for reproducibility; a production driver derives the
/// public-coin challenges by Fiat–Shamir and samples the blinders at random.
pub fn verifier_trace(
    gates: &[GateObservation],
    mem_acc_in: &[u64],
    mem_acc_out: &[u64],
) -> Tagged<Transparent, VerifierTrace> {
    let steps: Vec<VerifierStep> = gates
        .iter()
        .enumerate()
        .map(|(i, g)| {
            let seed = i as u64 + 1;
            VerifierStep {
                k_a: Scalar::from_u64(g.k_a),
                k_b: Scalar::from_u64(g.k_b),
                k_c: Scalar::from_u64(g.k_c),
                delta: Scalar::from_u64(g.delta),
                v_hat: Scalar::from_u64(g.v_hat),
                r_w: Scalar::from_u64(seed.wrapping_mul(7).wrapping_add(1)),
                r: Scalar::from_u64(seed.wrapping_mul(13).wrapping_add(3)),
                r_t: Scalar::from_u64(seed.wrapping_mul(17).wrapping_add(5)),
            }
        })
        .collect();
    Tagged::seal(VerifierTrace {
        steps,
        mem_acc_in: mem_acc_in.iter().map(|x| Scalar::from_u64(*x)).collect(),
        mem_acc_out: mem_acc_out.iter().map(|x| Scalar::from_u64(*x)).collect(),
        r_in: Scalar::from_u64(2),
        r_out: Scalar::from_u64(3),
    })
}

/// **Pipeline terminal (fold leg).** Fold the whole verifier and check the
/// folded instance natively.  Returns the folded proof and whether it verifies.
///
/// Bound `where Z: NonZk`: a [`volar_discipline::Zk`] artifact cannot be folded
/// here (compile error).  The fold output is [`Transparent`].
pub fn prove_and_verify_folded<Z: NonZk>(
    trace: Tagged<Z, VerifierTrace>,
    params: &PedersenParams,
) -> (Tagged<Transparent, VerifierFold>, bool) {
    let fold = prove_verifier(trace, params);
    let ok = verify_folded(&fold, params);
    (fold, ok)
}

/// **Pipeline terminal (C leg).** Lower a woven [`Transparent`] verifier
/// `IrModule` to **C source** via [`CBackend`] — the executable substrate when
/// the LLVM backend is unavailable.  Same lowering recipe as
/// `volar-c-backend`'s VOLE end-to-end tests
/// (`CBackend::new` → [`lower_module_with_opts`] → `finish`).
///
/// `env` carries the monomorphisation context (build it with
/// [`MonoEnv::new`]).  The discipline tag guarantees we never hand a ZK prover
/// module to the transparent C terminal.
pub fn emit_verifier_c(
    verifier: &Tagged<Transparent, IrModule<IrFunction>>,
    env: &MonoEnv,
) -> String {
    let mut backend = CBackend::new();
    lower_module_with_opts(verifier.inner(), &mut backend, env);
    backend.finish()
}

#[cfg(test)]
mod tests {
    use super::*;
    use volar_compiler::ir::{
        ExternalKind, IrBlock, IrExpr, IrExprKind, IrFunction, IrLit, IrModule, IrType, PrimitiveType,
    };

    /// Honest gate: pick `K_a,K_b,K_c,Δ` and set `V̂ = K_c·Δ − K_a·K_b`.
    fn honest_gate(a: u64, b: u64, c: u64, delta: u64) -> GateObservation {
        // Computed in the field via the trace, but for small values the integer
        // identity holds: choose v_hat so the check passes.
        let lhs = (a as u128) * (b as u128);
        let rhs = (c as u128) * (delta as u128);
        // v_hat = rhs - lhs (mod field) — emulate with the scalar path instead:
        // here we just feed values and let `verifier_trace` build field scalars;
        // pick c·delta ≥ a·b so v_hat is a small non-negative integer.
        assert!(rhs >= lhs, "test gate must have K_c·Δ ≥ K_a·K_b");
        GateObservation { k_a: a, k_b: b, k_c: c, delta, v_hat: (rhs - lhs) as u64 }
    }

    #[test]
    fn whole_verifier_folds_and_verifies() {
        let params = PedersenParams::setup(8, 41);
        let gates: Vec<GateObservation> =
            (1..=10u64).map(|i| honest_gate(i, i + 2, i + 6, i + 1)).collect();
        let trace = verifier_trace(&gates, &[1], &[99]);
        let (fold, ok) = prove_and_verify_folded(trace, &params);
        assert!(ok, "honest verifier run must fold to a natively-verifiable instance");
        assert_eq!(fold.inner().gap.steps, 10);
    }

    #[test]
    fn tampered_gate_is_rejected() {
        let params = PedersenParams::setup(8, 41);
        let mut gates: Vec<GateObservation> =
            (1..=6u64).map(|i| honest_gate(i, i + 2, i + 6, i + 1)).collect();
        gates[3].v_hat += 1; // break the check
        let trace = verifier_trace(&gates, &[1], &[2]);
        let (_, ok) = prove_and_verify_folded(trace, &params);
        assert!(!ok, "a tampered gate observation must fail native verification");
    }

    /// Minimal woven-verifier stand-in: a single function the C backend can
    /// lower without spec linkage.  Exercises [`emit_verifier_c`] end-to-end
    /// (the real verifier→C path is covered by `volar-c-backend`'s vole tests).
    fn minimal_module() -> Tagged<Transparent, IrModule<IrFunction>> {
        let func = IrFunction {
            name: "verifier_ok".into(),
            module_path: vec![],
            generics: vec![],
            receiver: None,
            params: vec![],
            return_type: Some(IrType::Primitive(PrimitiveType::Bool)),
            where_clause: vec![],
            body: IrBlock {
                stmts: vec![],
                expr: Some(Box::new(IrExpr::new(IrExprKind::Lit(IrLit::Bool(true)), (), None))),
            },
            external_kind: ExternalKind::Normal,
        };
        Tagged::seal(IrModule {
            name: "woven_verifier".into(),
            functions: vec![func],
            structs: vec![],
            enums: vec![],
            traits: vec![],
            impls: vec![],
            type_aliases: vec![],
            consts: vec![],
        })
    }

    #[test]
    fn emit_verifier_c_produces_nonempty_source() {
        let module = minimal_module();
        let env = MonoEnv::new("verifier");
        let c_src = emit_verifier_c(&module, &env);
        assert!(!c_src.is_empty(), "C backend must emit source for the verifier module");
        assert!(c_src.contains("verifier_ok"), "emitted C should name the verifier function");
    }
}
