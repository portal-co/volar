// @reliability: experimental
// @ai: assisted
//! PESAT loop emitter: compile movfuscated Volar IR (a self-looping step
//! circuit) directly to per-step PESAT instances over `Galois128`.
//!
//! See `docs/pesat-loop-emitter-plan.md` (volar-ir repo). The emitter reads a
//! step circuit — the single self-looping block produced by
//! `volar_ir_passes::movfuscate::movfuscate_ir` — and emits one
//! `volar_iop::warp::PesatIndex` per executed step plus a native trace
//! (`PesatWitness`), such that the honest trace satisfies the instance
//! (`check_instance`) and any tampered trace is rejected.
//!
//! C2 (the WARP plan's anti-v0 constraint): the constraints are read off the
//! step circuit's own operations, and the emitter's evaluator
//! ([`eval_step_wires`]) shares their semantics exactly — never a re-encoded
//! relation about different variables.

use volar_ir::ir::IRBlock;
use volar_ir_common::TypeTable as IRTypes;
use volar_primitives::Galois128;

pub mod chain;
pub mod emit;
pub mod eval;

pub use chain::{ChainManifest, LinkError, chain_manifest, verify_chain};
pub use emit::{EmitError, StepEmission, build_witness, emit_step_index};
pub use eval::eval_step_wires;

/// One bit embedded into the PESAT field: `false → 0`, `true → 1`.
pub(crate) fn bit_to_field(bit: bool) -> Galois128 {
    Galois128(u128::from(bit))
}
