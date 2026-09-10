// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//!
//! The vc-spec embedder API over Volar's two-party garbled-circuit MPC.
//!
//! An *embedder* invokes an unmodified guest with each argument tagged by
//! [`VcVisibility`], runs the two-party computation, and reveals results to
//! both parties. This type ties together the three pieces this crate
//! provides:
//!
//! 1. [`compile_schedule`](crate::compile_schedule) turns the lowered guest
//!    circuit (`BIrBlocks`) into a [`GateSchedule`];
//! 2. [`partition_from_sides`](crate::partition_from_sides) turns the
//!    per-input-bit side metadata into the session's [`InputOwner`] partition;
//! 3. `volar-mpc`'s session runs the semi-honest garbled evaluation, with the
//!    evaluator's (remote/blind) inputs delivered by 1-of-2 OT.
//!
//! Outcomes follow the spec: a concrete result is [`VcOutcome::Value`], a WASM
//! trap is [`VcOutcome::Trap`], a transport/protocol failure is
//! [`VcOutcome::Abort`], and an illegal embedder op (e.g. `mem_read` of
//! symbolic bytes) is [`VcOutcome::Error`].
//!
//! The crate is transport-agnostic: the in-process path uses `volar-mpc`'s
//! `evaluate` with a supplied OT channel (tests); the `std` feature enables
//! cross-process runs over framed TCP via `run_garbler` / `run_evaluator`.

use alloc::vec::Vec;

use digest::Digest;
use hybrid_array::Array;
use volar_ir::boolar::BIrBlocks;
use volar_mpc::{
    GateSchedule, InputOwner, MpcError, OtChannel, evaluate_multi, garble_schedule,
};
use volar_spec::vole::VoleArray;
use volar_spec::garble::{Garble, GlobalSecret};

use crate::schedule::ScheduleError;

/// vc-spec argument / memory visibility.
///
/// Named from the local party's perspective, symmetric across parties:
/// `Public` is known to both, `Private` is the local party's own (it
/// garbles), `Blind` is the remote party's (delivered by OT). Maps 1:1 onto
/// `volar-vaffle-target`'s `VcVisibility`.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum VcVisibility {
    /// Known to both parties (concrete).
    Public,
    /// Private to the local party (symbolic to the remote party).
    Private,
    /// Private to the remote party (symbolic/blind to the local party).
    Blind,
}

/// The outcome of an embedder operation, per vc-spec.
///
/// The spec distinguishes a *trap* (a valid guest outcome, e.g. an
/// out-of-bounds concrete access) from an *abort* (an implementation-defined
/// failure the parties can distinguish from a trap), and an *error* (an
/// illegal embedder operation such as reading symbolic bytes).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VcOutcome {
    /// A concrete result, revealed to both parties.
    Value(Vec<bool>),
    /// The guest trapped (a valid, concrete outcome).
    Trap,
    /// The session aborted: transport or protocol failure, distinguishable
    /// from a trap. Carries the underlying session error for diagnosis.
    Abort(MpcError),
    /// The embedder operation was illegal (e.g. `mem_read` of symbolic
    /// bytes, or a circuit that cannot be scheduled).
    Error(ScheduleError),
}

/// A two-party vc embedder over the garbled-circuit MPC, in the garbler role.
///
/// The embedder owns the [`GlobalSecret`] (it garbles) plus the deterministic
/// input-label schedule for one circuit shape. It is generic over the label
/// width `N`, input-bit count `I`, and AND-gate count `A` of the circuits it
/// runs — one embedder instance serves one circuit shape.
pub struct VcEmbedder<N: VoleArray<u8>, const I: usize, const A: usize> {
    secret: GlobalSecret<N>,
    input_labels: [Garble<N>; I],
}

impl<N: VoleArray<u8>, const I: usize, const A: usize> VcEmbedder<N, I, A> {
    /// Create an embedder from an explicit secret and per-input false-labels
    /// (deterministic tests; production draws these from a CSPRNG — see
    /// [`Self::secret`]).
    pub fn with_secret(secret: GlobalSecret<N>, input_labels: [Garble<N>; I]) -> Self {
        Self {
            secret,
            input_labels,
        }
    }

    /// Read the secret, e.g. to seed a fresh CSPRNG for input labels.
    pub fn secret(&self) -> &GlobalSecret<N> {
        &self.secret
    }

    /// Compile a guest circuit into its schedule, reporting a schedule error
    /// as a [`VcOutcome::Error`] at the boundary rather than panicking.
    pub fn compile<P: Clone>(circuit: &BIrBlocks<P>) -> Result<GateSchedule, VcOutcome> {
        crate::compile_schedule(circuit).map_err(VcOutcome::Error)
    }

    /// `invoke`: run the two-party computation over a guest circuit.
    ///
    /// `circuit` is the lowered, circuit-fused guest (single block, single
    /// output). `partition` is the per-input-bit owner vector (see
    /// [`partition_from_sides`](crate::partition_from_sides)). `public_bits`,
    /// `private_bits` (local), and `blind_bits` (remote) are the concrete
    /// input values, each slice exactly as long as the number of input bits
    /// of the matching visibility. `ot` delivers the remote party's (blind)
    /// input labels by 1-of-2 OT.
    ///
    /// Returns [`VcOutcome::Value`] with the single revealed output bit on
    /// success, [`VcOutcome::Abort`] on a session/protocol failure, and
    /// [`VcOutcome::Error`] if the circuit cannot be scheduled.
    pub fn invoke<D: Digest, P: Clone>(
        &self,
        circuit: &BIrBlocks<P>,
        partition: &[InputOwner],
        public_bits: &[bool],
        private_bits: &[bool],
        blind_bits: &[bool],
        ot: &mut dyn OtChannel<N>,
    ) -> VcOutcome {
        let schedule = match Self::compile(circuit) {
            Ok(s) => s,
            Err(e) => return e,
        };
        self.invoke_schedule::<D>(&schedule, partition, public_bits, private_bits, blind_bits, ot)
    }

    /// `invoke` on a pre-compiled schedule (skips recompilation when the same
    /// guest is invoked repeatedly with different inputs).
    pub fn invoke_schedule<D: Digest>(
        &self,
        schedule: &GateSchedule,
        partition: &[InputOwner],
        public_bits: &[bool],
        private_bits: &[bool],
        blind_bits: &[bool],
        ot: &mut dyn OtChannel<N>,
    ) -> VcOutcome {
        if schedule.num_inputs != I || schedule.and_count() != A {
            // Circuit shape mismatch against this embedder's const generics.
            return VcOutcome::Error(ScheduleError::NotACircuit);
        }
        let exec = match garble_schedule::<N, D, I, A>(
            schedule,
            self.secret.clone(),
            self.input_labels.clone(),
        ) {
            Ok(e) => e,
            Err(e) => return VcOutcome::Abort(e),
        };
        match evaluate_multi::<N, D, I, A>(
            &exec,
            partition,
            public_bits,
            private_bits,
            blind_bits,
            ot,
        ) {
            Ok(bits) => VcOutcome::Value(bits),
            Err(e) => VcOutcome::Abort(e),
        }
    }
}

/// Re-export the label array type used across the OT channel boundary, so
/// embedder callers can name it without importing `hybrid-array` directly.
pub type Label<N> = Array<u8, N>;
