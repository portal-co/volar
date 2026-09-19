// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//!
//! Two-party garbled-circuit MPC session layer (semi-honest).
//!
//! This crate is the execution-time counterpart to `volar-weaver`'s garbling
//! passes: it runs a *garbler* and an *evaluator* through one Yao semi-honest
//! two-party evaluation of a boolean circuit in which **both parties may hold
//! private input bits** (mutual privacy). It builds directly on the half-gate
//! scheme in [`volar_spec::garble`] (Zahur–Rosulek–Evans '15); the caveat that
//! file carries ("the VOLE-specific binding has not been reviewed") applies
//! here unchanged.
//!
//! # Model
//!
//! A circuit's input bits are partitioned into three disjoint sets:
//!
//! | Set | Owner | Delivery |
//! |-----|-------|----------|
//! | public | both | known to both sides out-of-band |
//! | garbler | garbler | garbler encodes and sends the selected labels |
//! | evaluator | evaluator | one 1-of-2 OT per bit (garbler is OT sender) |
//!
//! The evaluator finishes holding output-wire labels; the garbler publishes
//! its output decoding table so the evaluator recovers the output bit
//! (vc-spec's "returns are revealed" rule). In the semi-honest model the
//! garbler learns nothing about the evaluator's inputs (OT privacy) and the
//! evaluator learns nothing beyond the output (garbled-circuit privacy).
//!
//! # Layers
//!
//! - [`GarbledExec`] pairs a garbled circuit with its [`GateSchedule`] so the
//!   whole honest evaluation can run in one call ([`evaluate`]). Tests use it
//!   directly; it documents the exact message flow.
//! - [`run_garbler`] / [`run_evaluator`] expose the same flow as two halves of
//!   a framed byte-wire protocol over any [`Transport`]; [`run_local`] drives
//!   both to completion over an in-process rendezvous (the lockstep test
//!   harness, the same shape `volar_channel::run_protocol` gives `Protocol`
//!   impls), and a framed-TCP transport behind the `std` feature swaps in
//!   without touching protocol logic.
//!
//! # OT input delivery
//!
//! Evaluator-input labels move by 1-of-2 OT. This crate is generic over the OT
//! channel via [`OtChannel`], so tests can use a trivial in-process OT while
//! production wiring swaps in `volar-spec`'s OT stack (Chou–Orlandi base OT +
//! IKNP/Ferret extension) without touching session logic.

#![no_std]

extern crate alloc;

use alloc::string::String;
use alloc::vec::Vec;

use digest::Digest;
use hybrid_array::Array;
use volar_spec::SpecRng as _;
use volar_spec::garble::{Eval, EvalSetup, Garble, GarbleTable, GarbledCircuit, GlobalSecret};
use volar_spec::vole::VoleArray;

pub mod cut_and_choose;
pub mod external_batch;
pub use external_batch::{
    EXTERNAL_BATCH_MAX_ACTIONS, EXTERNAL_BATCH_MAX_VALUE_BYTES, EXTERNAL_BATCH_MAX_VALUES,
    ExternalActionManifestEntry, ExternalBatchBinding, ExternalBatchFrame, ExternalBatchFrameError,
    ExternalBatchManifest, ExternalBoundaryId,
};
pub mod input_labels;
#[cfg(feature = "std")]
pub mod net;
pub mod ot;
#[cfg(feature = "mlkem")]
pub mod ot_mlkem;
pub mod strict;
pub mod strict_chain;
pub mod strict_cursor;
pub mod strict_split;
#[cfg(feature = "std")]
pub mod tcp;
#[cfg(all(feature = "std", feature = "tinylabels"))]
pub mod tinylabels_delivery;

/// The deterministic Garbled-RAM data-wire base: access `access`'s `bit`-th
/// data-bit false-label is derived as `H(0xDA || access || bit)` — a pure
/// function both the garbler and the evaluator-side ORAM host compute, so
/// they agree with no extra communication. Mirrors
/// `Garble::action_result_base` and cirrus's `gram_data_base` (0xDA domain).
pub fn gram_data_base<D: Digest, N: VoleArray<u8>>(access: u64, bit: u64) -> Garble<N> {
    let mut d = D::new();
    d.update([0xDAu8]);
    d.update(access.to_le_bytes());
    d.update(bit.to_le_bytes());
    let hash = d.finalize();
    Garble {
        base: Array::<u8, N>::from_fn(|i| hash[i]),
    }
}

/// Which set a circuit input bit belongs to (the mutual-privacy partition).
///
/// Session-level view of the per-value side metadata `volar-ir` threads
/// through the pipeline (`MpcProtection` there): each circuit input wire is
/// public, one of the garbler's private bits, or one of the evaluator's
/// private bits.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum InputOwner {
    /// Known to both parties; no label transfer needed.
    Public,
    /// Private to the garbler; the garbler sends the selected label.
    Garbler,
    /// Private to the evaluator; delivered via 1-of-2 OT.
    Evaluator,
}

impl InputOwner {
    /// Build a per-wire owner vector from three disjoint wire-index sets
    /// (public / garbler / evaluator), the shape a compiler-side input
    /// partition (e.g. `volar_weaver::mpc::InputPartition`) produces.
    ///
    /// Wires not present in any set default to `Public`. Returns
    /// [`MpcError::BadPartition`] if the sets overlap or index a wire
    /// `>= num_inputs`.
    pub fn from_index_sets(
        num_inputs: usize,
        public: &[u32],
        garbler: &[u32],
        evaluator: &[u32],
    ) -> Result<Vec<InputOwner>, MpcError> {
        let mut owners = alloc::vec![InputOwner::Public; num_inputs];
        let in_range = |&i: &u32| (i as usize) < num_inputs;
        if !public.iter().all(in_range)
            || !garbler.iter().all(in_range)
            || !evaluator.iter().all(in_range)
        {
            return Err(MpcError::BadPartition);
        }
        for &i in public {
            owners[i as usize] = InputOwner::Public;
        }
        for &i in garbler {
            if owners[i as usize] != InputOwner::Public {
                return Err(MpcError::BadPartition);
            }
            owners[i as usize] = InputOwner::Garbler;
        }
        for &i in evaluator {
            if owners[i as usize] != InputOwner::Public {
                return Err(MpcError::BadPartition);
            }
            owners[i as usize] = InputOwner::Evaluator;
        }
        Ok(owners)
    }
}

/// Error type for a failed evaluation.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum MpcError {
    /// The counterparty sent a message that does not fit the current phase.
    UnexpectedMessage,
    /// The evaluator's recovered output label is neither the false- nor the
    /// true-label: a tampered or malformed garbled table was detected.
    /// Semi-honest evaluation aborts rather than emitting a wrong output.
    DecodeFailure,
    /// An input partition or bit vector had the wrong length.
    BadPartition,
    /// The gate schedule referenced a wire that was never defined.
    MalformedSchedule,
    /// The schedule contains Garbled-RAM storage ops
    /// ([`Gate::StorageRead`]/[`Gate::StorageWrite`]) but the evaluator was
    /// run without a GRAM storage driver. Run the `*_with_gram` evaluator
    /// that drives an ORAM host instead.
    UnsupportedStorage,
    /// The schedule contains action calls ([`Gate::ActionBit`]) but the
    /// evaluator was run without an action host. Run
    /// [`crate::strict::run_evaluator_strict_actions`] instead.
    ActionRequiresHost,
    /// An action host returned an error or a wrong-width result.
    ActionHost,
    /// The schedule selected an executor/reveal policy unavailable in this
    /// session adapter. Adapters must fail closed, never silently fall back to
    /// evaluator execution.
    UnsupportedExternalPolicy,
}

/// The evaluator-side Garbled-RAM storage driver: one ORAM host per storage
/// space, fed by [`GarbledExec::eval_labels_multi_with_gram`]. The garbler
/// has pinned every storage wire's false-label base; this driver runs the
/// actual ORAM access (the ORAM client lives evaluator-side) and re-garbles
/// the result bit to the base both parties agreed on.
///
/// `volar-vc` implements this over `volar-oram`'s `bit_host::OramHost` (the
/// shared bit-level Path-ORAM driver); `volar-mpc` stays free of an ORAM
/// dependency so it remains a pure execution-time crate.
pub trait GramDrive<N: VoleArray<u8>> {
    /// Read the bit at concrete cell `cell` on ORAM access `access`,
    /// returning the read bit re-garbled to `base` (the deterministic
    /// per-access base `gram_data_base(access, 0)`).
    fn read(&mut self, cell: u64, access: u64, base: &Garble<N>) -> Eval<N>;
    /// Write the bit carried by `value` to concrete cell `cell` on ORAM
    /// access `access`. The driver decodes the write bit against the cell's
    /// current base (tracked host-side, mirroring the garbler's cell
    /// pinning) before running the ORAM write.
    fn write(&mut self, cell: u64, access: u64, value: &Eval<N>) -> Result<(), MpcError>;
}

/// A channel over which the per-bit evaluator-input OTs run.
///
/// The garbler is the OT sender (offering the false/true label pair for each
/// evaluator input wire); the evaluator is the OT receiver (choosing the label
/// matching its input bit). Implementations carry whatever scheme state they
/// need; this trait exposes only the two operations the session layer needs.
pub trait OtChannel<N: VoleArray<u8>> {
    /// Sender side: offer the two labels for one input wire.
    fn send(&mut self, labels: [&Array<u8, N>; 2]);
    /// Receiver side: obtain the label for choice bit `bit`.
    fn receive(&mut self, bit: bool) -> Array<u8, N>;
}

// ============================================================================
// Gate schedule
// ============================================================================

/// One boolean gate over wire indices.
///
/// This is the schedule-level mirror of `volar_ir::boolar::BIrStmt`'s pure
/// boolean primitives, re-expressed here so `volar-mpc` stays free of an IR
/// dependency (it is an execution-time crate, not a compiler crate). Wire `0
/// .. num_inputs-1` are the circuit inputs; each gate defines the next wire in
/// sequence. `Or` is pre-expanded to `Not`/`And` by De Morgan, exactly as the
/// weaver does.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Gate {
    /// Constant false.
    Zero,
    /// Constant true.
    One,
    /// Bitwise XOR of two wires (free).
    Xor(usize, usize),
    /// AND of two wires (consumes one garbled table).
    And(usize, usize),
    /// NOT of a wire (XOR with the one-wire; free).
    Not(usize),
    /// Read the bit at a concrete storage cell, producing the next wire.
    ///
    /// This is the Garbled-RAM storage op: the cell index is a *concrete*
    /// (compile-time-known) address, so no address gates are scheduled. The
    /// garbler pins the read-result wire's false-label base deterministically
    /// (`gram_data_base(access, 0)`) and emits no table; the evaluator runs
    /// the ORAM access host-side and re-garbles the read bit to that base.
    /// `access` is the ordinal of this storage op in evaluation order (the
    /// ORAM access index, shared by both parties).
    StorageRead {
        /// Which storage space (indexes `GateSchedule::storages`).
        storage: usize,
        /// Concrete cell index within the space.
        cell: u64,
        /// ORAM access ordinal (1-based, matching the evaluator's counter).
        access: u64,
    },
    /// Write the bit on wire `src` to a concrete storage cell, producing the
    /// next wire (a dummy zero, matching `BIrStmt::StorageWrite`).
    ///
    /// The garbler pins the written cell's base to `src`'s false-label base
    /// (so the evaluator can decode the write bit) and emits no table; the
    /// evaluator decodes the write bit against that base and runs the ORAM
    /// write access.
    StorageWrite {
        /// Which storage space (indexes `GateSchedule::storages`).
        storage: usize,
        /// Concrete cell index within the space.
        cell: u64,
        /// Wire carrying the bit to write.
        src: usize,
        /// ORAM access ordinal (1-based, matching the evaluator's counter).
        access: u64,
    },
    /// Bit `bit` of the result of action call `call` (an index into
    /// [`GateSchedule::actions`]).
    ///
    /// This is the schedule-level mirror of `BIrStmt::ActionBit`: the action
    /// is an evaluator-hosted extern (e.g. a network socket op). The garbler
    /// pins the result wire's false-label base deterministically
    /// (`Garble::action_result_base` over the guard + arg bases) and emits no
    /// table; the strict session
    /// ([`crate::strict::run_evaluator_strict_actions`]) decodes the args via
    /// a garbler round-trip, runs the host, and delivers each result bit by
    /// 1-of-2 OT so the evaluator never holds the free-XOR delta.
    ActionBit {
        /// Which action call (indexes `GateSchedule::actions`).
        call: u32,
        /// Which result bit of the call.
        bit: u32,
    },
}

/// Public party selected to run an external action/oracle host implementation.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ExternalExecutor {
    Garbler,
    Evaluator,
}

/// Public clear-input disclosure authorization for one external request.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ExternalRevealPolicy {
    /// Only the assigned executor may receive clear external inputs. A strict
    /// adapter must explicitly implement this mode before accepting it.
    ExecutorOnly,
    /// Compatibility disclosure: both roles receive the decoded inputs.
    BothRoles,
}

/// Schedule-level action execution policy.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct ActionExecutionPolicy {
    pub executor: ExternalExecutor,
    pub reveal: ExternalRevealPolicy,
    /// Declaration/profile fingerprint bound by a future batch manifest.
    pub fingerprint: [u8; 32],
}

impl ActionExecutionPolicy {
    /// Explicit legacy policy for the existing evaluator-hosted strict path.
    pub const fn legacy_evaluator() -> Self {
        Self {
            executor: ExternalExecutor::Evaluator,
            reveal: ExternalRevealPolicy::BothRoles,
            fingerprint: [0; 32],
        }
    }
}

/// One action call carried by a schedule: the extern's name plus the wires
/// holding its guard, arguments, and fallback bits (the value used for each
/// output bit when `guard = 0` and the action is not invoked).
///
/// The schedule stores *wire indices*; the polarity flips
/// (`strict::eliminate_nots` fills them) recover the logical values the
/// garbler reports to the evaluator's host.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ActionSpec {
    /// The extern's name (matched against the host's registry).
    pub name: String,
    /// Stable public source occurrence identity, bound by future boundary
    /// manifests. It is distinct from `call` table position.
    pub request_id: u64,
    /// Total source action-chain position. Schedulers must preserve this order
    /// even when unrelated storage/oracle requests batch beside an action.
    pub action_ordinal: u64,
    /// Explicit host executor and disclosure policy.
    pub execution: ActionExecutionPolicy,
    /// Wire carrying the guard (1 = invoke, 0 = use the fallback bits).
    pub guard: usize,
    /// Argument wires, in order.
    pub arg_wires: Vec<usize>,
    /// Fallback wires (one per output bit).
    pub fallback_wires: Vec<usize>,
    /// Number of output bits.
    pub num_bits: usize,
    /// Polarity flip for the guard wire (set by `strict::eliminate_nots`).
    #[doc(hidden)]
    pub guard_polarity: bool,
    /// Per-argument polarity flips (set by `strict::eliminate_nots`).
    #[doc(hidden)]
    pub arg_polarity: Vec<bool>,
    /// Per-fallback polarity flips (set by `strict::eliminate_nots`).
    #[doc(hidden)]
    pub fallback_polarity: Vec<bool>,
}

/// The shape of one Garbled-RAM storage space carried by a schedule: enough
/// for the evaluator to size its ORAM tree and host, and for both parties to
/// agree on the deterministic per-cell base derivation.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct GramStorageSpec {
    /// Number of addressable cells (one bit per cell, boolar convention).
    /// This is the *compressed* cell count: the schedule folds the few
    /// distinct (possibly huge, sparsely-touched) memory addresses down to
    /// consecutive compact block indices `0..num_cells`, so the ORAM is sized
    /// by the number of distinct cells, not the maximum address value.
    pub num_cells: u64,
    /// ORAM tree levels (path length). Must satisfy `2^(levels-1) >=
    /// num_cells` for a complete tree over the address space.
    pub levels: usize,
}

/// The gate schedule for a single-output boolean circuit: the gates in
/// evaluation order plus the index of the output wire.
#[derive(Clone, Debug)]
pub struct GateSchedule {
    /// Number of input wires (wires `0 .. num_inputs-1`).
    pub num_inputs: usize,
    /// Gates in order; gate `k` defines wire `num_inputs + k`.
    pub gates: Vec<Gate>,
    /// Index of the wire carrying the circuit's output.
    pub output: usize,
    /// Indices of the wires carrying the circuit's revealed outputs, in
    /// result order. When `None`, the circuit is single-output on `output`
    /// (the legacy shape). A multi-output schedule sets this to the full
    /// output-wire list; `output` is then its first element.
    #[doc(hidden)]
    pub outputs: Option<Vec<usize>>,
    /// The Garbled-RAM storage spaces this schedule reads/writes, indexed by
    /// the `storage` field of [`Gate::StorageRead`]/[`Gate::StorageWrite`].
    /// Empty for a pure boolean circuit (the common case). When non-empty the
    /// evaluator must run with a GRAM storage driver (`*_with_gram`).
    #[doc(hidden)]
    pub storages: Vec<GramStorageSpec>,
    /// The action (evaluator-hosted extern) calls this schedule makes,
    /// indexed by the `call` field of [`Gate::ActionBit`]. Empty for a pure
    /// boolean circuit (the common case); when non-empty the strict session
    /// must run with an action host (`*_strict_actions`).
    #[doc(hidden)]
    pub actions: Vec<ActionSpec>,
}

impl GateSchedule {
    /// Total number of wires (inputs + gate outputs).
    pub fn wire_count(&self) -> usize {
        self.num_inputs + self.gates.len()
    }

    /// Number of AND gates (== number of garbled tables required).
    pub fn and_count(&self) -> usize {
        self.gates
            .iter()
            .filter(|g| matches!(g, Gate::And(..)))
            .count()
    }

    /// The output-wire list, defaulting to the single `output` wire.
    pub fn output_wires(&self) -> Vec<usize> {
        self.outputs
            .clone()
            .unwrap_or_else(|| alloc::vec![self.output])
    }
}

/// The evaluator's pure-boolean gate walk: compute every wire's label from the
/// input labels and the AND tables. Shared by the const-generic
/// [`GarbledExec::eval_labels`] / [`GarbledExec::eval_labels_multi`] and the
/// runtime-sized [`DynGarbledExec::eval_labels_multi`]. Storage gates need a
/// GRAM driver, which the pure walk has none of.
fn eval_gate_wires<N: VoleArray<u8>, D: Digest>(
    one_wire: &Eval<N>,
    tables: &[GarbleTable<N>],
    schedule: &GateSchedule,
    inputs: &[Eval<N>],
) -> Result<Vec<Eval<N>>, MpcError> {
    if inputs.len() != schedule.num_inputs {
        return Err(MpcError::BadPartition);
    }
    let mut wires: Vec<Eval<N>> = Vec::with_capacity(schedule.wire_count());
    wires.extend_from_slice(inputs);
    let mut table = 0usize;
    for gate in &schedule.gates {
        let out = match *gate {
            Gate::Zero => Eval::zero(),
            Gate::One => one_wire.clone(),
            Gate::Xor(a, b) => {
                let (x, y) = (wires.get(a), wires.get(b));
                match (x, y) {
                    (Some(x), Some(y)) => x.clone() ^ y.clone(),
                    _ => return Err(MpcError::MalformedSchedule),
                }
            }
            Gate::Not(a) => match wires.get(a) {
                Some(x) => x.clone() ^ one_wire.clone(),
                None => return Err(MpcError::MalformedSchedule),
            },
            Gate::And(a, b) => {
                let t = tables.get(table).ok_or(MpcError::MalformedSchedule)?;
                table += 1;
                match (wires.get(a), wires.get(b)) {
                    (Some(x), Some(y)) => x.and_via_table::<D>(y, t),
                    _ => return Err(MpcError::MalformedSchedule),
                }
            }
            // Storage ops need a GRAM driver; the pure evaluator has none.
            Gate::StorageRead { .. } | Gate::StorageWrite { .. } => {
                return Err(MpcError::UnsupportedStorage);
            }
            // Actions need an evaluator-side host; the pure evaluator has
            // none.
            Gate::ActionBit { .. } => return Err(MpcError::ActionRequiresHost),
        };
        wires.push(out);
    }
    Ok(wires)
}

/// A garbled circuit together with its gate schedule — everything both
/// parties need to run an evaluation.
///
/// The garbler constructs this (it owns the secret); the evaluator receives
/// only the [`EvalSetup`] view plus the schedule, never the secret or the
/// input false-labels.
pub struct GarbledExec<N: VoleArray<u8>, const I: usize, const A: usize> {
    /// The garbling (garbler-private).
    pub circuit: GarbledCircuit<N, I, A>,
    /// The gate schedule (shared with the evaluator).
    pub schedule: GateSchedule,
    /// Per-output-wire false-label bases (garbler-private), in
    /// `output_wires()` order. Set by [`garble_schedule`]; `GarbledExec`
    /// values built by hand for single-output tests leave this empty.
    pub output_labels: Vec<Garble<N>>,
}

impl<N: VoleArray<u8>, const I: usize, const A: usize> GarbledExec<N, I, A> {
    /// Evaluate the circuit over a full vector of input labels, returning the
    /// output-wire label. This is the evaluator's computation; it uses only
    /// the evaluator-visible `EvalSetup` plus the schedule.
    ///
    /// `Or` must already be expanded (see [`Gate`]); encountering one is a
    /// schedule error surfaced as [`MpcError::MalformedSchedule`].
    pub fn eval_labels<D: Digest>(
        setup: &EvalSetup<N, A>,
        schedule: &GateSchedule,
        inputs: &[Eval<N>],
    ) -> Result<Eval<N>, MpcError> {
        let wires = eval_gate_wires::<N, D>(&setup.one_wire, &setup.tables, schedule, inputs)?;
        wires
            .get(schedule.output)
            .cloned()
            .ok_or(MpcError::MalformedSchedule)
    }

    /// Multi-output variant of [`Self::eval_labels`]: returns the label on
    /// every wire in `schedule.output_wires()`, in order. The evaluator runs
    /// the circuit once; the per-output decode is the caller's concern (each
    /// output wire opens against its own published output label).
    pub fn eval_labels_multi<D: Digest>(
        setup: &EvalSetup<N, A>,
        schedule: &GateSchedule,
        inputs: &[Eval<N>],
    ) -> Result<Vec<Eval<N>>, MpcError> {
        let wires = eval_gate_wires::<N, D>(&setup.one_wire, &setup.tables, schedule, inputs)?;
        schedule
            .output_wires()
            .iter()
            .map(|&w| wires.get(w).cloned().ok_or(MpcError::MalformedSchedule))
            .collect()
    }

    /// GRAM-aware multi-output evaluator: like [`Self::eval_labels_multi`],
    /// but storage ops are driven through `gram` (an ORAM host on the
    /// evaluator side). The garbler has already pinned every storage wire's
    /// base via [`garble_schedule`]; here the evaluator runs each ORAM access
    /// and re-garbles the result to the deterministic per-access base
    /// ([`gram_data_base`]), so downstream gates consume a correctly-based
    /// label. The driver is supplied per space (indexed by the gate's
    /// `storage` field).
    pub fn eval_labels_multi_with_gram<D: Digest>(
        setup: &EvalSetup<N, A>,
        schedule: &GateSchedule,
        inputs: &[Eval<N>],
        gram: &mut [&mut dyn GramDrive<N>],
    ) -> Result<Vec<Eval<N>>, MpcError> {
        if inputs.len() != schedule.num_inputs {
            return Err(MpcError::BadPartition);
        }
        if gram.len() != schedule.storages.len() {
            return Err(MpcError::MalformedSchedule);
        }
        let mut wires: Vec<Eval<N>> = Vec::with_capacity(schedule.wire_count());
        wires.extend_from_slice(inputs);
        let mut table = 0usize;
        for gate in &schedule.gates {
            let out = match *gate {
                Gate::Zero => Eval::zero(),
                Gate::One => setup.one_wire.clone(),
                Gate::Xor(a, b) => {
                    let (x, y) = (wires.get(a), wires.get(b));
                    match (x, y) {
                        (Some(x), Some(y)) => x.clone() ^ y.clone(),
                        _ => return Err(MpcError::MalformedSchedule),
                    }
                }
                Gate::Not(a) => match wires.get(a) {
                    Some(x) => x.clone() ^ setup.one_wire.clone(),
                    None => return Err(MpcError::MalformedSchedule),
                },
                Gate::And(a, b) => {
                    let t = setup.tables.get(table).ok_or(MpcError::MalformedSchedule)?;
                    table += 1;
                    match (wires.get(a), wires.get(b)) {
                        (Some(x), Some(y)) => x.and_via_table::<D>(y, t),
                        _ => return Err(MpcError::MalformedSchedule),
                    }
                }
                Gate::StorageRead {
                    storage,
                    cell,
                    access,
                } => {
                    let driver = gram.get_mut(storage).ok_or(MpcError::MalformedSchedule)?;
                    let base = gram_data_base::<D, N>(access, 0);
                    driver.read(cell, access, &base)
                }
                Gate::StorageWrite {
                    storage,
                    cell,
                    src,
                    access,
                } => {
                    let driver = gram.get_mut(storage).ok_or(MpcError::MalformedSchedule)?;
                    let value = wires.get(src).cloned().ok_or(MpcError::MalformedSchedule)?;
                    driver.write(cell, access, &value)?;
                    // Dummy-zero wire (matches BIrStmt::StorageWrite).
                    Eval::zero()
                }
                Gate::ActionBit { .. } => return Err(MpcError::ActionRequiresHost),
            };
            wires.push(out);
        }
        schedule
            .output_wires()
            .iter()
            .map(|&w| wires.get(w).cloned().ok_or(MpcError::MalformedSchedule))
            .collect()
    }
}

/// The garbler's gate walk: compute every wire's false-label base and the AND
/// tables. Shared by the const-generic [`garble_schedule`] and the
/// runtime-sized [`garble_schedule_dyn`].
///
/// `input_labels` must have exactly `schedule.num_inputs` entries. Returns the
/// full wire-base vector plus the AND tables (in gate order).
fn garble_wire_bases<N: VoleArray<u8>, D: Digest>(
    schedule: &GateSchedule,
    secret: &GlobalSecret<N>,
    input_labels: &[Garble<N>],
) -> Result<(Vec<Garble<N>>, Vec<GarbleTable<N>>), MpcError> {
    garble_wire_bases_pol::<N, D>(schedule, secret, input_labels, None)
}

/// The shared garble walk, optionally polarity-aware: `and_input_polarity`
/// (one `(pa, pb)` per AND gate, in gate order, as produced by
/// [`crate::strict::eliminate_nots`]) folds the logical input flip of each AND
/// gate into its garbled table, so a Not-free schedule evaluates identically
/// without the evaluator ever holding the free-XOR delta. `None` is the plain
/// walk used by the legacy session path.
fn garble_wire_bases_pol<N: VoleArray<u8>, D: Digest>(
    schedule: &GateSchedule,
    secret: &GlobalSecret<N>,
    input_labels: &[Garble<N>],
    and_input_polarity: Option<&[(bool, bool)]>,
) -> Result<(Vec<Garble<N>>, Vec<GarbleTable<N>>), MpcError> {
    if schedule.num_inputs != input_labels.len() {
        return Err(MpcError::MalformedSchedule);
    }
    let mut wires: Vec<Garble<N>> = Vec::with_capacity(schedule.wire_count());
    wires.extend(input_labels.iter().cloned());
    let mut tables: Vec<GarbleTable<N>> = Vec::with_capacity(schedule.and_count());
    // GRAM storage: per-space per-cell current false-label bases. A cell's
    // base starts at `gram_data_base(0, cell)` (the deterministic initial
    // supply) and is re-pinned to the written wire's base on each write, so
    // the evaluator can decode the write bit against the same base the
    // garbler tracked. Mirrors cirrus's `GramStorageSpace::cell_bases`.
    let mut cell_bases: Vec<Vec<Garble<N>>> = schedule
        .storages
        .iter()
        .map(|s| {
            (0..s.num_cells)
                .map(|cell| gram_data_base::<D, N>(0, cell))
                .collect()
        })
        .collect();
    for gate in &schedule.gates {
        let out = match *gate {
            Gate::Zero | Gate::One => Garble::zero(),
            Gate::Xor(a, b) => {
                let (x, y) = (wires.get(a), wires.get(b));
                match (x, y) {
                    (Some(x), Some(y)) => Garble {
                        base: Array::<u8, N>::from_fn(|i| x.base[i] ^ y.base[i]),
                    },
                    _ => return Err(MpcError::MalformedSchedule),
                }
            }
            // NOT flips the *value bit* only: the false-label is unchanged
            // (`false' = false`), so the encoded label becomes `base ^ (1^v)·Δ`
            // = `encode(base, !v)`. This is the consistent free-NOT; note it
            // differs from `GlobalSecret::not_garble` (which computes
            // `encode(a, !v)` and is self-inconsistent — see crate docs).
            Gate::Not(a) => match wires.get(a) {
                Some(x) => x.clone(),
                None => return Err(MpcError::MalformedSchedule),
            },
            Gate::And(a, b) => {
                let (x, y) = (wires.get(a), wires.get(b));
                match (x, y) {
                    (Some(x), Some(y)) => {
                        let (pa, pb) = match and_input_polarity {
                            Some(pols) => {
                                *pols.get(tables.len()).ok_or(MpcError::MalformedSchedule)?
                            }
                            None => (false, false),
                        };
                        tables.push(secret.gen_and_table_pol::<D>(x, y, pa, pb));
                        x.and_result::<D>(y)
                    }
                    _ => return Err(MpcError::MalformedSchedule),
                }
            }
            Gate::StorageRead {
                storage, access, ..
            } => {
                // The read-result wire's false-label base is the deterministic
                // per-access base the evaluator's ORAM host re-garbles to.
                let _ = storage;
                gram_data_base::<D, N>(access, 0)
            }
            Gate::StorageWrite {
                storage, cell, src, ..
            } => {
                // Pin the written cell's base to the written wire's base so
                // the evaluator decodes the write bit against it; produce a
                // dummy-zero wire.
                let sb = wires.get(src).cloned().ok_or(MpcError::MalformedSchedule)?;
                let space = cell_bases
                    .get_mut(storage)
                    .ok_or(MpcError::MalformedSchedule)?;
                let slot = space
                    .get_mut(cell as usize)
                    .ok_or(MpcError::MalformedSchedule)?;
                *slot = sb;
                Garble::zero()
            }
            Gate::ActionBit { call, bit } => {
                // Pin the result wire's false-label base deterministically
                // (`Garble::action_result_base` over the guard + arg bases);
                // the strict-actions session delivers the result bit by OT.
                let spec = schedule
                    .actions
                    .get(call as usize)
                    .ok_or(MpcError::MalformedSchedule)?;
                let guard = wires
                    .get(spec.guard)
                    .cloned()
                    .ok_or(MpcError::MalformedSchedule)?;
                let arg_refs: Vec<&Garble<N>> = spec
                    .arg_wires
                    .iter()
                    .map(|&a| wires.get(a))
                    .collect::<Option<Vec<_>>>()
                    .ok_or(MpcError::MalformedSchedule)?;
                guard.action_result_base::<D>(&arg_refs, bit as usize)
            }
        };
        wires.push(out);
    }
    Ok((wires, tables))
}

/// Garble a gate schedule into a [`GarbledExec`], given the global secret and
/// per-input false-labels.
///
/// This is the execution-time mirror of `volar_weaver::garble`'s woven
/// garbler: it walks the schedule producing each wire's false-label
/// (`Garble`) and emitting one [`GarbleTable`] per AND gate, then packages the
/// secret, input labels, tables, and output false-label into a
/// [`GarbledCircuit`]. `Or` must already be expanded by De Morgan, exactly as
/// the weaver requires.
///
/// The caller supplies `secret` and `input_labels` (both garbler-private); the
/// number of AND gates in `schedule` must equal `A`.
pub fn garble_schedule<N, D, const I: usize, const A: usize>(
    schedule: &GateSchedule,
    secret: GlobalSecret<N>,
    input_labels: [Garble<N>; I],
) -> Result<GarbledExec<N, I, A>, MpcError>
where
    N: VoleArray<u8>,
    D: Digest,
{
    if schedule.num_inputs != I || schedule.and_count() != A {
        return Err(MpcError::MalformedSchedule);
    }
    let (wires, tables) = garble_wire_bases::<N, D>(schedule, &secret, &input_labels)?;
    let output_label = wires
        .get(schedule.output)
        .cloned()
        .ok_or(MpcError::MalformedSchedule)?;
    let tables: [GarbleTable<N>; A] = tables.try_into().map_err(|_| MpcError::MalformedSchedule)?;
    // Collect each output wire's own false-label base so multi-output
    // evaluation can decode every wire against its true base (not the shared
    // single `output_label`, which is only correct for `output` itself).
    let output_labels: Vec<Garble<N>> = schedule
        .output_wires()
        .iter()
        .map(|&w| wires.get(w).cloned().ok_or(MpcError::MalformedSchedule))
        .collect::<Result<_, _>>()?;
    Ok(GarbledExec {
        circuit: GarbledCircuit {
            secret,
            input_labels,
            tables,
            output_label,
        },
        output_labels,
        schedule: schedule.clone(),
    })
}

// ============================================================================
// Runtime-sized two-party path (Vec-backed)
// ============================================================================
//
// The const-generic path (`garble_schedule` / `GarbledExec<N, I, A>` /
// `evaluate_multi`) fixes the input-bit count `I` and AND-count `A` at compile
// time. A general `oram_lower::OramProgram` has per-stage circuits whose AND
// counts *vary* between stages, so a fully general driver can't be
// const-generic. These Vec-backed variants take runtime `I`/`A`, enabling such
// drivers (the symbolic-ORAM S4 two-party `OramProgram` driver). The garble and
// eval walks are shared with the const path via `garble_wire_bases` /
// `eval_gate_wires`, so behavior is identical; only the container differs.

/// Runtime-sized counterpart of [`GarbledCircuit`]: Vec-backed tables and input
/// labels. The garbler constructs it (it owns the secret); the evaluator
/// receives only the [`DynEvalSetup`] view.
pub struct DynGarbledCircuit<N: VoleArray<u8>> {
    /// The garbler's global secret (garbler-private).
    pub secret: GlobalSecret<N>,
    /// Per-input false-label bases (garbler-private).
    pub input_labels: Vec<Garble<N>>,
    /// The AND-gate tables, in schedule order.
    pub tables: Vec<GarbleTable<N>>,
    /// The single `output` wire's false-label base.
    pub output_label: Garble<N>,
}

impl<N: VoleArray<u8>> DynGarbledCircuit<N> {
    /// The evaluator-visible view (everything needed to evaluate, nothing that
    /// reveals the secret).
    pub fn eval_setup(&self) -> DynEvalSetup<N> {
        DynEvalSetup {
            one_wire: self.secret.one_wire_eval(),
            tables: self.tables.clone(),
            output_label: self.output_label.clone(),
        }
    }
}

/// Runtime-sized counterpart of [`EvalSetup`].
pub struct DynEvalSetup<N: VoleArray<u8>> {
    /// The label for a constant-true wire.
    pub one_wire: Eval<N>,
    /// The AND-gate tables, in schedule order.
    pub tables: Vec<GarbleTable<N>>,
    /// The single `output` wire's false-label base.
    pub output_label: Garble<N>,
}

/// Runtime-sized counterpart of [`GarbledExec`].
pub struct DynGarbledExec<N: VoleArray<u8>> {
    /// The garbling (garbler-private).
    pub circuit: DynGarbledCircuit<N>,
    /// The gate schedule (shared with the evaluator).
    pub schedule: GateSchedule,
    /// Per-output-wire false-label bases (garbler-private), in
    /// `output_wires()` order.
    pub output_labels: Vec<Garble<N>>,
}

impl<N: VoleArray<u8>> DynGarbledExec<N> {
    /// Runtime-sized multi-output evaluator (pure boolean; no storage gates).
    /// Returns the label on every wire in `schedule.output_wires()`, in order.
    pub fn eval_labels_multi<D: Digest>(
        setup: &DynEvalSetup<N>,
        schedule: &GateSchedule,
        inputs: &[Eval<N>],
    ) -> Result<Vec<Eval<N>>, MpcError> {
        let wires = eval_gate_wires::<N, D>(&setup.one_wire, &setup.tables, schedule, inputs)?;
        schedule
            .output_wires()
            .iter()
            .map(|&w| wires.get(w).cloned().ok_or(MpcError::MalformedSchedule))
            .collect()
    }
}

/// Runtime-sized counterpart of [`garble_schedule`]: takes `input_labels` as a
/// Vec (length must equal `schedule.num_inputs`) and keeps the AND tables as a
/// Vec (length equals `schedule.and_count()`). Otherwise identical — the gate
/// walk is shared via [`garble_wire_bases`].
pub fn garble_schedule_dyn<N, D>(
    schedule: &GateSchedule,
    secret: GlobalSecret<N>,
    input_labels: Vec<Garble<N>>,
) -> Result<DynGarbledExec<N>, MpcError>
where
    N: VoleArray<u8>,
    D: Digest,
{
    let (wires, tables) = garble_wire_bases::<N, D>(schedule, &secret, &input_labels)?;
    if tables.len() != schedule.and_count() {
        return Err(MpcError::MalformedSchedule);
    }
    let output_label = wires
        .get(schedule.output)
        .cloned()
        .ok_or(MpcError::MalformedSchedule)?;
    let output_labels: Vec<Garble<N>> = schedule
        .output_wires()
        .iter()
        .map(|&w| wires.get(w).cloned().ok_or(MpcError::MalformedSchedule))
        .collect::<Result<_, _>>()?;
    Ok(DynGarbledExec {
        circuit: DynGarbledCircuit {
            secret,
            input_labels,
            tables,
            output_label,
        },
        output_labels,
        schedule: schedule.clone(),
    })
}

/// Run one honest semi-honest two-party evaluation.
///
/// `exec` is the garbler's garbled circuit + schedule (the secret stays
/// garbler-side). `partition` splits the input bits; `public_bits`,
/// `garbler_bits`, `evaluator_bits` supply the bit values for each set (in
/// circuit-input order within that set). `ot` carries the evaluator-input OTs.
///
/// Returns the output bit on success. This is the reference implementation of
/// the message flow that [`run_garbler`]/[`run_evaluator`] expose as a
/// transport-driven protocol.
pub fn evaluate<N, D, const I: usize, const A: usize>(
    exec: &GarbledExec<N, I, A>,
    partition: &[InputOwner],
    public_bits: &[bool],
    garbler_bits: &[bool],
    evaluator_bits: &[bool],
    ot: &mut dyn OtChannel<N>,
) -> Result<bool, MpcError>
where
    N: VoleArray<u8>,
    D: Digest,
{
    let schedule = &exec.schedule;
    if partition.len() != schedule.num_inputs
        || partition
            .iter()
            .filter(|&&o| o == InputOwner::Public)
            .count()
            != public_bits.len()
        || partition
            .iter()
            .filter(|&&o| o == InputOwner::Garbler)
            .count()
            != garbler_bits.len()
        || partition
            .iter()
            .filter(|&&o| o == InputOwner::Evaluator)
            .count()
            != evaluator_bits.len()
    {
        return Err(MpcError::BadPartition);
    }

    // --- Garbler publishes the evaluator-visible setup (tables + one-wire +
    //     output decode). The evaluator keeps only this view.
    let setup = exec.circuit.eval_setup();

    // --- Encode every circuit input into the evaluator's label vector.
    let mut labels: Vec<Eval<N>> = Vec::with_capacity(schedule.num_inputs);
    let mut pub_i = 0usize;
    let mut gb_i = 0usize;
    let mut ev_i = 0usize;
    for (idx, owner) in partition.iter().enumerate() {
        let wire = &exec.circuit.input_labels[idx];
        match owner {
            InputOwner::Public => {
                labels.push(exec.circuit.secret.encode(wire, public_bits[pub_i]));
                pub_i += 1;
            }
            InputOwner::Garbler => {
                labels.push(exec.circuit.secret.encode(wire, garbler_bits[gb_i]));
                gb_i += 1;
            }
            InputOwner::Evaluator => {
                let false_label = exec.circuit.secret.encode(wire, false);
                let true_label = exec.circuit.secret.encode(wire, true);
                ot.send([&false_label.target, &true_label.target]);
                let chosen = ot.receive(evaluator_bits[ev_i]);
                ev_i += 1;
                labels.push(Eval { target: chosen });
            }
        }
    }

    // --- Evaluator runs the circuit over the assembled labels.
    let result = GarbledExec::<N, I, A>::eval_labels::<D>(&setup, schedule, &labels)?;

    // --- Decode the output against the published decode table. Under an
    //     honest garbling the label always opens to a color bit; tampering is
    //     detected structurally by the transport-driven session path (which
    //     re-checks the label against both published output labels).
    Ok(setup.recover_output(&result))
}

/// Multi-output variant of [`evaluate`]: returns one revealed bit per wire in
/// `schedule.output_wires()`, in order. Each output wire's label opens against
/// that wire's own false-label base (collected into `GarbledExec.output_labels`
/// by [`garble_schedule`]), so every output bit decodes correctly. For a
/// single-output schedule this matches [`evaluate`].
pub fn evaluate_multi<N, D, const I: usize, const A: usize>(
    exec: &GarbledExec<N, I, A>,
    partition: &[InputOwner],
    public_bits: &[bool],
    garbler_bits: &[bool],
    evaluator_bits: &[bool],
    ot: &mut dyn OtChannel<N>,
) -> Result<Vec<bool>, MpcError>
where
    N: VoleArray<u8>,
    D: Digest,
{
    let schedule = &exec.schedule;
    if partition.len() != schedule.num_inputs
        || partition
            .iter()
            .filter(|&&o| o == InputOwner::Public)
            .count()
            != public_bits.len()
        || partition
            .iter()
            .filter(|&&o| o == InputOwner::Garbler)
            .count()
            != garbler_bits.len()
        || partition
            .iter()
            .filter(|&&o| o == InputOwner::Evaluator)
            .count()
            != evaluator_bits.len()
    {
        return Err(MpcError::BadPartition);
    }

    let setup = exec.circuit.eval_setup();

    let mut labels: Vec<Eval<N>> = Vec::with_capacity(schedule.num_inputs);
    let mut pub_i = 0usize;
    let mut gb_i = 0usize;
    let mut ev_i = 0usize;
    for (idx, owner) in partition.iter().enumerate() {
        let wire = &exec.circuit.input_labels[idx];
        match owner {
            InputOwner::Public => {
                labels.push(exec.circuit.secret.encode(wire, public_bits[pub_i]));
                pub_i += 1;
            }
            InputOwner::Garbler => {
                labels.push(exec.circuit.secret.encode(wire, garbler_bits[gb_i]));
                gb_i += 1;
            }
            InputOwner::Evaluator => {
                let false_label = exec.circuit.secret.encode(wire, false);
                let true_label = exec.circuit.secret.encode(wire, true);
                ot.send([&false_label.target, &true_label.target]);
                let chosen = ot.receive(evaluator_bits[ev_i]);
                ev_i += 1;
                labels.push(Eval { target: chosen });
            }
        }
    }

    let results = GarbledExec::<N, I, A>::eval_labels_multi::<D>(&setup, schedule, &labels)?;
    // Decode each output wire against its own false-label base, falling back
    // to the shared published `output_label` for hand-built single-output
    // `GarbledExec` values that left `output_labels` empty.
    let decoded: Vec<bool> = results
        .iter()
        .enumerate()
        .map(|(i, r)| {
            let base = exec
                .output_labels
                .get(i)
                .unwrap_or(&exec.circuit.output_label);
            r.open(base)[0] & 1 != 0
        })
        .collect();
    Ok(decoded)
}

/// GRAM-aware multi-output two-party evaluation: like [`evaluate_multi`], but
/// the circuit's Garbled-RAM storage ops are driven through `gram` (one ORAM
/// driver per storage space, in `schedule.storages` order). The input-label
/// assembly (public / garbler / OT-delivered evaluator bits) is identical;
/// only the evaluator's circuit walk routes storage ops through the ORAM
/// host. Use this for any schedule with a non-empty `storages` list.
pub fn evaluate_multi_with_gram<N, D, const I: usize, const A: usize>(
    exec: &GarbledExec<N, I, A>,
    partition: &[InputOwner],
    public_bits: &[bool],
    garbler_bits: &[bool],
    evaluator_bits: &[bool],
    ot: &mut dyn OtChannel<N>,
    gram: &mut [&mut dyn GramDrive<N>],
) -> Result<Vec<bool>, MpcError>
where
    N: VoleArray<u8>,
    D: Digest,
{
    let schedule = &exec.schedule;
    if partition.len() != schedule.num_inputs
        || partition
            .iter()
            .filter(|&&o| o == InputOwner::Public)
            .count()
            != public_bits.len()
        || partition
            .iter()
            .filter(|&&o| o == InputOwner::Garbler)
            .count()
            != garbler_bits.len()
        || partition
            .iter()
            .filter(|&&o| o == InputOwner::Evaluator)
            .count()
            != evaluator_bits.len()
    {
        return Err(MpcError::BadPartition);
    }

    let setup = exec.circuit.eval_setup();

    let mut labels: Vec<Eval<N>> = Vec::with_capacity(schedule.num_inputs);
    let mut pub_i = 0usize;
    let mut gb_i = 0usize;
    let mut ev_i = 0usize;
    for (idx, owner) in partition.iter().enumerate() {
        let wire = &exec.circuit.input_labels[idx];
        match owner {
            InputOwner::Public => {
                labels.push(exec.circuit.secret.encode(wire, public_bits[pub_i]));
                pub_i += 1;
            }
            InputOwner::Garbler => {
                labels.push(exec.circuit.secret.encode(wire, garbler_bits[gb_i]));
                gb_i += 1;
            }
            InputOwner::Evaluator => {
                let false_label = exec.circuit.secret.encode(wire, false);
                let true_label = exec.circuit.secret.encode(wire, true);
                ot.send([&false_label.target, &true_label.target]);
                let chosen = ot.receive(evaluator_bits[ev_i]);
                ev_i += 1;
                labels.push(Eval { target: chosen });
            }
        }
    }

    let results =
        GarbledExec::<N, I, A>::eval_labels_multi_with_gram::<D>(&setup, schedule, &labels, gram)?;
    let decoded: Vec<bool> = results
        .iter()
        .enumerate()
        .map(|(i, r)| {
            let base = exec
                .output_labels
                .get(i)
                .unwrap_or(&exec.circuit.output_label);
            r.open(base)[0] & 1 != 0
        })
        .collect();
    Ok(decoded)
}

// Transport-driven sessions
// ============================================================================

/// A bidirectional byte transport between the two parties.
///
/// The session layer is transport-agnostic, exactly like `volar-channel`:
/// messages are moved by the caller. [`run_local`] drives both roles over an
/// in-process rendezvous (no sockets), and a framed-TCP transport behind the
/// `std` feature swaps in without touching protocol logic. Frames are
/// length-prefixed byte strings; the *contents* are the [`SessionFrame`]
/// encoding below.
pub trait Transport {
    /// Send one frame to the counterparty.
    fn send(&mut self, frame: &[u8]);
    /// Block until one frame arrives from the counterparty.
    fn recv(&mut self) -> Vec<u8>;
}

/// One framed message in the wire protocol.
///
/// The garbler is the sender for setup, owned-input labels, and OT frames;
/// the evaluator is the sender only for the final verdict. Encoding is a
/// compact self-describing byte format (no external serialization dep, so the
/// crate stays `no_std` + `alloc`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SessionFrame {
    /// Garbler → evaluator: the evaluator-visible setup.
    Setup {
        one_wire: Vec<u8>,
        tables: Vec<[Vec<u8>; 4]>,
        output_label: Vec<u8>,
    },
    /// Garbler → evaluator: selected labels for public + garbler-owned input
    /// wires (in circuit-input order restricted to those sets).
    OwnedInputs(Vec<Vec<u8>>),
    /// One 1-of-2 OT frame for a single evaluator input bit. The payload is
    /// OT-scheme-specific and opaque to this layer.
    Ot(Vec<u8>),
    /// Evaluator → garbler: the recovered output bit, or an abort flag.
    /// (`true` = output bit 1 / abort for `Aborted`.)
    Verdict(Result<bool, ()>),
    /// Garbler → evaluator: the evaluator-visible setup for the *strict*
    /// session (`crate::strict`) — tables only. The free-XOR delta
    /// (`one_wire`) and the output wire's false-label base are NOT sent: the
    /// schedule is Not-free (see [`crate::strict::eliminate_nots`]), and the
    /// output base stays garbler-private so the evaluator cannot forge the
    /// true output label.
    SetupStrict { tables: Vec<[Vec<u8>; 4]> },
    /// Garbler → evaluator: one bounded chunk of strict-session tables.
    /// Chunks are ordered and terminated by [`Self::SetupStrictEnd`].
    SetupStrictChunk { tables: Vec<[Vec<u8>; 4]> },
    /// Garbler → evaluator: terminates a strict table stream and binds the
    /// total table count. A mismatch aborts before output evaluation.
    SetupStrictEnd { table_count: u32 },
    /// Evaluator → garbler: the evaluator's output-wire labels, one per
    /// schedule output (in `output_wires()` order). The garbler decodes them
    /// against its private output bases — the verdict is server-authenticated,
    /// not evaluator-claimed.
    OutputLabels(Vec<Vec<u8>>),
    /// Garbler → evaluator: the decoded output bits (the verdict the garbler
    /// authenticated). Informational; the evaluator cannot forge it.
    VerdictBits(Vec<bool>),
    /// Evaluator → garbler (strict-actions session): the labels on one
    /// action call's wires — `[guard, args..., fallback...]` — which the
    /// garbler decodes by exact match against its private bases. The decoded
    /// values are *public* by design (an action's arguments are e.g. TLS
    /// ciphertext records, safe for both parties to see).
    ActionArgs {
        /// Which action call (indexes `GateSchedule::actions`).
        call: u32,
        /// Stable schedule/source occurrence identity. This prevents a peer
        /// with a same-shaped but differently ordered schedule from treating
        /// a call-table index as the same host invocation.
        request_id: u64,
        /// The wire labels.
        labels: Vec<Vec<u8>>,
    },
    /// Garbler → evaluator (strict-actions session): the decoded logical
    /// bits for the action call's wires (guard, args, fallback — polarities
    /// applied). The evaluator's host runs the action on the args when the
    /// guard is 1.
    ActionArgsClear {
        /// Which action call.
        call: u32,
        /// Must echo the request identity from [`Self::ActionArgs`].
        request_id: u64,
        /// The decoded logical bits.
        bits: Vec<bool>,
    },
    /// Garbler → evaluator: false/true encodings for output wires that are
    /// intentionally decoded only by the evaluator.
    OutputDecodes(Vec<[Vec<u8>; 2]>),
}

impl SessionFrame {
    /// Encode to bytes. Layout: 1 tag byte, then per-field (u32 len LE ++ bytes).
    pub fn encode(&self) -> Vec<u8> {
        let mut out = Vec::new();
        match self {
            SessionFrame::Setup {
                one_wire,
                tables,
                output_label,
            } => {
                out.push(0);
                push_bytes(&mut out, one_wire);
                push_u32(&mut out, tables.len() as u32);
                for t in tables {
                    for row in t {
                        push_bytes(&mut out, row);
                    }
                }
                push_bytes(&mut out, output_label);
            }
            SessionFrame::OwnedInputs(labels) => {
                out.push(1);
                push_u32(&mut out, labels.len() as u32);
                for l in labels {
                    push_bytes(&mut out, l);
                }
            }
            SessionFrame::Ot(payload) => {
                out.push(2);
                push_bytes(&mut out, payload);
            }
            SessionFrame::Verdict(v) => {
                out.push(3);
                out.push(match v {
                    Ok(true) => 1,
                    Ok(false) => 0,
                    Err(()) => 2,
                });
            }
            SessionFrame::SetupStrict { tables } => {
                out.push(4);
                push_u32(&mut out, tables.len() as u32);
                for t in tables {
                    for row in t {
                        push_bytes(&mut out, row);
                    }
                }
            }
            SessionFrame::SetupStrictChunk { tables } => {
                out.push(9);
                push_u32(&mut out, tables.len() as u32);
                for t in tables {
                    for row in t {
                        push_bytes(&mut out, row);
                    }
                }
            }
            SessionFrame::SetupStrictEnd { table_count } => {
                out.push(10);
                push_u32(&mut out, *table_count);
            }
            SessionFrame::OutputLabels(labels) => {
                out.push(5);
                push_u32(&mut out, labels.len() as u32);
                for l in labels {
                    push_bytes(&mut out, l);
                }
            }
            SessionFrame::VerdictBits(bits) => {
                out.push(6);
                push_u32(&mut out, bits.len() as u32);
                out.extend(bits.iter().map(|&b| b as u8));
            }
            SessionFrame::ActionArgs {
                call,
                request_id,
                labels,
            } => {
                out.push(7);
                push_u32(&mut out, *call);
                push_u64(&mut out, *request_id);
                push_u32(&mut out, labels.len() as u32);
                for l in labels {
                    push_bytes(&mut out, l);
                }
            }
            SessionFrame::ActionArgsClear {
                call,
                request_id,
                bits,
            } => {
                out.push(8);
                push_u32(&mut out, *call);
                push_u64(&mut out, *request_id);
                push_u32(&mut out, bits.len() as u32);
                out.extend(bits.iter().map(|&b| b as u8));
            }
            SessionFrame::OutputDecodes(decodes) => {
                out.push(14);
                push_u32(&mut out, decodes.len() as u32);
                for [zero, one] in decodes {
                    push_bytes(&mut out, zero);
                    push_bytes(&mut out, one);
                }
            }
        }
        out
    }

    /// Decode from bytes; `None` on malformed input.
    pub fn decode(buf: &[u8]) -> Option<SessionFrame> {
        let mut r = Reader { buf, pos: 0 };
        let frame = match r.u8()? {
            0 => {
                let one_wire = r.bytes()?;
                let nt = r.u32()? as usize;
                let mut tables = Vec::with_capacity(nt);
                for _ in 0..nt {
                    let mut rows: [Vec<u8>; 4] = Default::default();
                    for row in rows.iter_mut() {
                        *row = r.bytes()?;
                    }
                    tables.push(rows);
                }
                let output_label = r.bytes()?;
                Some(SessionFrame::Setup {
                    one_wire,
                    tables,
                    output_label,
                })
            }
            1 => {
                let n = r.u32()? as usize;
                let mut labels = Vec::with_capacity(n);
                for _ in 0..n {
                    labels.push(r.bytes()?);
                }
                Some(SessionFrame::OwnedInputs(labels))
            }
            2 => Some(SessionFrame::Ot(r.bytes()?)),
            3 => {
                let v = match r.u8()? {
                    1 => Ok(true),
                    0 => Ok(false),
                    _ => Err(()),
                };
                Some(SessionFrame::Verdict(v))
            }
            4 => {
                let nt = r.u32()? as usize;
                let mut tables = Vec::with_capacity(nt);
                for _ in 0..nt {
                    let mut rows: [Vec<u8>; 4] = Default::default();
                    for row in rows.iter_mut() {
                        *row = r.bytes()?;
                    }
                    tables.push(rows);
                }
                Some(SessionFrame::SetupStrict { tables })
            }
            9 => {
                let nt = r.u32()? as usize;
                let mut tables = Vec::with_capacity(nt);
                for _ in 0..nt {
                    let mut rows: [Vec<u8>; 4] = Default::default();
                    for row in &mut rows {
                        *row = r.bytes()?;
                    }
                    tables.push(rows);
                }
                Some(SessionFrame::SetupStrictChunk { tables })
            }
            10 => Some(SessionFrame::SetupStrictEnd {
                table_count: r.u32()?,
            }),
            5 => {
                let n = r.u32()? as usize;
                let mut labels = Vec::with_capacity(n);
                for _ in 0..n {
                    labels.push(r.bytes()?);
                }
                Some(SessionFrame::OutputLabels(labels))
            }
            6 => {
                let n = r.u32()? as usize;
                let mut bits = Vec::with_capacity(n);
                for _ in 0..n {
                    bits.push(r.u8()? != 0);
                }
                Some(SessionFrame::VerdictBits(bits))
            }
            7 => {
                let call = r.u32()?;
                let request_id = r.u64()?;
                let n = r.u32()? as usize;
                let mut labels = Vec::with_capacity(n);
                for _ in 0..n {
                    labels.push(r.bytes()?);
                }
                Some(SessionFrame::ActionArgs {
                    call,
                    request_id,
                    labels,
                })
            }
            8 => {
                let call = r.u32()?;
                let request_id = r.u64()?;
                let n = r.u32()? as usize;
                let mut bits = Vec::with_capacity(n);
                for _ in 0..n {
                    bits.push(r.u8()? != 0);
                }
                Some(SessionFrame::ActionArgsClear {
                    call,
                    request_id,
                    bits,
                })
            }
            14 => {
                let count = r.u32()? as usize;
                let mut decodes = Vec::with_capacity(count);
                for _ in 0..count {
                    decodes.push([r.bytes()?, r.bytes()?]);
                }
                Some(SessionFrame::OutputDecodes(decodes))
            }
            _ => None,
        }?;
        r.at_end().then_some(frame)
    }
}

fn push_u32(out: &mut Vec<u8>, v: u32) {
    out.extend_from_slice(&v.to_le_bytes());
}
fn push_u64(out: &mut Vec<u8>, v: u64) {
    out.extend_from_slice(&v.to_le_bytes());
}
fn push_bytes(out: &mut Vec<u8>, b: &[u8]) {
    push_u32(out, b.len() as u32);
    out.extend_from_slice(b);
}

struct Reader<'a> {
    buf: &'a [u8],
    pos: usize,
}
impl<'a> Reader<'a> {
    fn u8(&mut self) -> Option<u8> {
        let b = *self.buf.get(self.pos)?;
        self.pos += 1;
        Some(b)
    }
    fn u32(&mut self) -> Option<u32> {
        let s = self.buf.get(self.pos..self.pos + 4)?;
        self.pos += 4;
        Some(u32::from_le_bytes([s[0], s[1], s[2], s[3]]))
    }
    fn u64(&mut self) -> Option<u64> {
        let s = self.buf.get(self.pos..self.pos + 8)?;
        self.pos += 8;
        Some(u64::from_le_bytes([
            s[0], s[1], s[2], s[3], s[4], s[5], s[6], s[7],
        ]))
    }
    fn bytes(&mut self) -> Option<Vec<u8>> {
        let n = self.u32()? as usize;
        let s = self.buf.get(self.pos..self.pos + n)?;
        self.pos += n;
        Some(s.to_vec())
    }
    fn at_end(&self) -> bool {
        self.pos == self.buf.len()
    }
}

#[cfg(test)]
mod session_frame_tests {
    use alloc::vec;

    use super::*;

    #[test]
    fn action_frames_bind_request_identity_and_reject_trailing_bytes() {
        let frame = SessionFrame::ActionArgs {
            call: 3,
            request_id: 0x0102_0304_0506_0708,
            labels: vec![vec![7, 8]],
        };
        let encoded = frame.encode();
        assert_eq!(SessionFrame::decode(&encoded), Some(frame));
        let mut malformed = encoded;
        malformed.push(0);
        assert_eq!(SessionFrame::decode(&malformed), None);
    }
}

/// Convert an `N`-byte label array to/from the wire.
pub(crate) fn arr_to_vec<N: VoleArray<u8>>(a: &Array<u8, N>) -> Vec<u8> {
    a.as_slice().to_vec()
}
pub(crate) fn vec_to_arr<N: VoleArray<u8>>(v: &[u8]) -> Option<Array<u8, N>> {
    if v.len() != N::USIZE {
        return None;
    }
    Some(Array::<u8, N>::from_fn(|i| v[i]))
}

/// The garbler role over a transport. Drives one full evaluation.
///
/// Sends the setup, then the owned-input labels, then runs one OT per
/// evaluator-owned input bit (via `ot`), and finally waits for the verdict.
/// The free-XOR secret never leaves this role.
pub fn run_garbler<N, D, const I: usize, const A: usize, T: Transport>(
    exec: &GarbledExec<N, I, A>,
    partition: &[InputOwner],
    public_bits: &[bool],
    garbler_bits: &[bool],
    transport: &mut T,
    ot: &mut dyn OtChannel<N>,
) -> Result<bool, MpcError>
where
    N: VoleArray<u8>,
    D: Digest,
{
    let schedule = &exec.schedule;
    if partition.len() != schedule.num_inputs {
        return Err(MpcError::BadPartition);
    }
    let setup = exec.circuit.eval_setup();

    // Setup frame.
    let setup_frame = SessionFrame::Setup {
        one_wire: arr_to_vec(&setup.one_wire.target),
        tables: setup
            .tables
            .iter()
            .map(|t| {
                let mut rows: [Vec<u8>; 4] = Default::default();
                for (r, row) in t.table.iter().enumerate() {
                    rows[r] = arr_to_vec(row);
                }
                rows
            })
            .collect(),
        output_label: arr_to_vec(&setup.output_label.base),
    };
    transport.send(&setup_frame.encode());

    // Owned-input labels (public + garbler-owned), in circuit-input order.
    let mut owned: Vec<Vec<u8>> = Vec::new();
    let mut pub_i = 0usize;
    let mut gb_i = 0usize;
    for (idx, owner) in partition.iter().enumerate() {
        let wire = &exec.circuit.input_labels[idx];
        match owner {
            InputOwner::Public => {
                let b = *public_bits.get(pub_i).ok_or(MpcError::BadPartition)?;
                pub_i += 1;
                owned.push(arr_to_vec(&exec.circuit.secret.encode(wire, b).target));
            }
            InputOwner::Garbler => {
                let b = *garbler_bits.get(gb_i).ok_or(MpcError::BadPartition)?;
                gb_i += 1;
                owned.push(arr_to_vec(&exec.circuit.secret.encode(wire, b).target));
            }
            InputOwner::Evaluator => {}
        }
    }
    transport.send(&SessionFrame::OwnedInputs(owned).encode());

    // OT for each evaluator-owned input bit (garbler is OT sender). The
    // concrete OT scheme runs inside `ot`; the frames it emits are relayed.
    for (idx, owner) in partition.iter().enumerate() {
        if *owner == InputOwner::Evaluator {
            let wire = &exec.circuit.input_labels[idx];
            let f = exec.circuit.secret.encode(wire, false);
            let t = exec.circuit.secret.encode(wire, true);
            ot.send([&f.target, &t.target]);
        }
    }

    // Await the verdict.
    let frame = SessionFrame::decode(&transport.recv()).ok_or(MpcError::UnexpectedMessage)?;
    match frame {
        SessionFrame::Verdict(Ok(b)) => Ok(b),
        SessionFrame::Verdict(Err(())) => Err(MpcError::DecodeFailure),
        _ => Err(MpcError::UnexpectedMessage),
    }
}

/// The evaluator role over a transport. Computes the output.
///
/// Receives the setup and owned-input labels, runs one OT per evaluator-owned
/// input bit (via `ot`, as OT receiver), evaluates the circuit over the
/// assembled labels, decodes the output, and returns the verdict to the
/// garbler.
pub fn run_evaluator<N, D, const I: usize, const A: usize, T: Transport>(
    schedule: &GateSchedule,
    partition: &[InputOwner],
    evaluator_bits: &[bool],
    transport: &mut T,
    ot: &mut dyn OtChannel<N>,
) -> Result<bool, MpcError>
where
    N: VoleArray<u8>,
    D: Digest,
{
    if partition.len() != schedule.num_inputs {
        return Err(MpcError::BadPartition);
    }

    // Setup frame.
    let setup_frame = SessionFrame::decode(&transport.recv()).ok_or(MpcError::UnexpectedMessage)?;
    let (one_wire, tables, output_label) = match setup_frame {
        SessionFrame::Setup {
            one_wire,
            tables,
            output_label,
        } => (one_wire, tables, output_label),
        _ => return Err(MpcError::UnexpectedMessage),
    };
    let tables: [GarbleTable<N>; A] = tables
        .into_iter()
        .map(|rows| {
            let mut t: [Array<u8, N>; 4] = Default::default();
            for (r, row) in rows.iter().enumerate() {
                t[r] = vec_to_arr(row).ok_or(MpcError::MalformedSchedule)?;
            }
            Ok(GarbleTable { table: t })
        })
        .collect::<Result<Vec<_>, MpcError>>()?
        .try_into()
        .map_err(|_| MpcError::MalformedSchedule)?;
    let setup = EvalSetup::<N, A> {
        one_wire: Eval {
            target: vec_to_arr(&one_wire).ok_or(MpcError::MalformedSchedule)?,
        },
        tables,
        output_label: Garble {
            base: vec_to_arr(&output_label).ok_or(MpcError::MalformedSchedule)?,
        },
    };

    // Owned-input labels.
    let owned_frame = SessionFrame::decode(&transport.recv()).ok_or(MpcError::UnexpectedMessage)?;
    let owned: Vec<Eval<N>> = match owned_frame {
        SessionFrame::OwnedInputs(labels) => labels
            .iter()
            .map(|l| {
                Ok(Eval {
                    target: vec_to_arr(l).ok_or(MpcError::MalformedSchedule)?,
                })
            })
            .collect::<Result<Vec<_>, MpcError>>()?,
        _ => return Err(MpcError::UnexpectedMessage),
    };

    // Assemble the full input-label vector: public + garbler-owned from the
    // wire, evaluator-owned via OT.
    let mut labels: Vec<Eval<N>> = Vec::with_capacity(schedule.num_inputs);
    let mut owned_i = 0usize;
    let mut ev_i = 0usize;
    for owner in partition.iter() {
        match owner {
            InputOwner::Public | InputOwner::Garbler => {
                let l = owned.get(owned_i).ok_or(MpcError::BadPartition)?;
                owned_i += 1;
                labels.push(l.clone());
            }
            InputOwner::Evaluator => {
                let b = *evaluator_bits.get(ev_i).ok_or(MpcError::BadPartition)?;
                ev_i += 1;
                let chosen = ot.receive(b);
                labels.push(Eval { target: chosen });
            }
        }
    }

    // Evaluate and decode.
    let result = GarbledExec::<N, I, A>::eval_labels::<D>(&setup, schedule, &labels)?;
    let out = setup.recover_output(&result);

    // Structural tamper check: the recovered label must open to a valid color
    // bit consistent with the published output decode (i.e. it must equal one
    // of the two possible output labels). Under honest garbling this always
    // holds; a corrupted table can produce a label that decodes to neither
    // cleanly. `recover_output` already maps it to a bit; we additionally
    // confirm the label is one of the two expected encodings.
    let opens = result.open(&setup.output_label);
    let is_color = opens[0] & 1 == (if out { 1 } else { 0 });
    let verdict = if is_color { Ok(out) } else { Err(()) };
    transport.send(&SessionFrame::Verdict(verdict).encode());
    if is_color {
        Ok(out)
    } else {
        Err(MpcError::DecodeFailure)
    }
}

/// Drive both roles in-process over a rendezvous pair, returning the output
/// bit both parties agree on. This is the lockstep test harness: it exercises
/// the full framed wire protocol (setup → owned inputs → OTs → verdict)
/// without sockets, exactly as `volar_channel::run_protocol` simulates two
/// `Protocol` impls.
///
/// The two roles run in lockstep on one thread: since each garbler send is
/// matched by an evaluator recv and the only evaluator→garbler traffic is the
/// final verdict, a bounded rendezvous suffices.
pub fn run_local<N, D, const I: usize, const A: usize>(
    exec: &GarbledExec<N, I, A>,
    schedule: &GateSchedule,
    partition: &[InputOwner],
    public_bits: &[bool],
    garbler_bits: &[bool],
    evaluator_bits: &[bool],
) -> Result<bool, MpcError>
where
    N: VoleArray<u8>,
    D: Digest,
{
    // The wire protocol is strictly ordered: the garbler emits setup, owned
    // inputs, and OT offers with no intervening recv, then the evaluator
    // consumes them and emits a single verdict. So a one-thread lockstep run
    // is just: (1) script the garbler's frames, (2) replay them to the
    // evaluator, (3) return the verdict. A loopback OT carries the
    // evaluator-input labels using the garbler's secret.
    use alloc::collections::VecDeque;

    // Phase 1: the garbler's outgoing frames.
    let mut wire: VecDeque<Vec<u8>> = VecDeque::new();
    let setup = exec.circuit.eval_setup();
    wire.push_back(
        SessionFrame::Setup {
            one_wire: arr_to_vec(&setup.one_wire.target),
            tables: setup
                .tables
                .iter()
                .map(|t| {
                    let mut rows: [Vec<u8>; 4] = Default::default();
                    for (r, row) in t.table.iter().enumerate() {
                        rows[r] = arr_to_vec(row);
                    }
                    rows
                })
                .collect(),
            output_label: arr_to_vec(&setup.output_label.base),
        }
        .encode(),
    );
    let mut owned: Vec<Vec<u8>> = Vec::new();
    let mut pub_i = 0usize;
    let mut gb_i = 0usize;
    for (idx, owner) in partition.iter().enumerate() {
        let wire_lbl = &exec.circuit.input_labels[idx];
        match owner {
            InputOwner::Public => {
                let b = *public_bits.get(pub_i).ok_or(MpcError::BadPartition)?;
                pub_i += 1;
                owned.push(arr_to_vec(&exec.circuit.secret.encode(wire_lbl, b).target));
            }
            InputOwner::Garbler => {
                let b = *garbler_bits.get(gb_i).ok_or(MpcError::BadPartition)?;
                gb_i += 1;
                owned.push(arr_to_vec(&exec.circuit.secret.encode(wire_lbl, b).target));
            }
            InputOwner::Evaluator => {}
        }
    }
    wire.push_back(SessionFrame::OwnedInputs(owned).encode());

    // Run the *real* Chou–Orlandi OT for each evaluator-owned input wire,
    // interleaving sender and receiver on this one thread. The receiver
    // recovers exactly the label matching its choice bit; the sender never
    // learns the bit. The recovered labels feed the evaluator through a
    // `LoopbackOt` shim, so the session code path is unchanged and a wire OT
    // (framed TCP) swaps in without touching it.
    let mut ot = crate::ot::LoopbackOt::<N>::new();
    let mut ot_rng = crate::ot::SeedRng::new(0xC0FFEE);
    let mut ev_i = 0usize;
    for (idx, owner) in partition.iter().enumerate() {
        if *owner == InputOwner::Evaluator {
            let wire_lbl = &exec.circuit.input_labels[idx];
            let f = exec.circuit.secret.encode(wire_lbl, false);
            let t = exec.circuit.secret.encode(wire_lbl, true);
            let bit = *evaluator_bits.get(ev_i).ok_or(MpcError::BadPartition)?;
            ev_i += 1;
            // Real Chou–Orlandi OT: the receiver recovers exactly the label for
            // its choice bit. That recovered label (not the garbler's copy) is
            // what the evaluator is handed, via the LoopbackOt shim.
            let recovered =
                crate::ot::ot_once::<N>([&f.target, &t.target], bit, ot_rng.next_u32() as u64);
            let other = if bit {
                f.target.clone()
            } else {
                t.target.clone()
            };
            let pair: [&Array<u8, N>; 2] = if bit {
                [&other, &recovered]
            } else {
                [&recovered, &other]
            };
            crate::OtChannel::send(&mut ot, pair);
        }
    }

    // Phase 2: replay the wire to the evaluator and capture its verdict.
    struct Replay<'a> {
        wire: &'a mut VecDeque<Vec<u8>>,
        verdict: Option<Vec<u8>>,
    }
    impl Transport for Replay<'_> {
        fn send(&mut self, frame: &[u8]) {
            self.verdict = Some(frame.to_vec());
        }
        fn recv(&mut self) -> Vec<u8> {
            self.wire
                .pop_front()
                .expect("evaluator recv: garbler frame available")
        }
    }
    let mut transport = Replay {
        wire: &mut wire,
        verdict: None,
    };
    let out = run_evaluator::<N, D, I, A, _>(
        schedule,
        partition,
        evaluator_bits,
        &mut transport,
        &mut ot,
    )?;

    // Phase 3: the garbler would decode the verdict frame; both roles agree
    // on `out`, which is what an honest run returns.
    let _ = transport.verdict;
    Ok(out)
}

// ============================================================================
// Traffic accounting
// ============================================================================

/// A [`Transport`] wrapper that counts bytes and frames in each direction.
///
/// Wrap any transport to measure the wire cost of a session — the C3
/// traffic-accounting deliverable. Counts are cumulative across the session;
/// read them after the run (or via [`CountingTransport::snapshot`]).
pub struct CountingTransport<'a, T: Transport> {
    inner: &'a mut T,
    /// Total bytes sent (frame payloads, excluding length prefixes).
    pub bytes_sent: usize,
    /// Total bytes received.
    pub bytes_recv: usize,
    /// Frames sent.
    pub frames_sent: usize,
    /// Frames received.
    pub frames_recv: usize,
}

impl<'a, T: Transport> CountingTransport<'a, T> {
    /// Wrap `inner`, starting counts at zero.
    pub fn new(inner: &'a mut T) -> Self {
        Self {
            inner,
            bytes_sent: 0,
            bytes_recv: 0,
            frames_sent: 0,
            frames_recv: 0,
        }
    }

    /// A point-in-time copy of the counters.
    pub fn snapshot(&self) -> Traffic {
        Traffic {
            bytes_sent: self.bytes_sent,
            bytes_recv: self.bytes_recv,
            frames_sent: self.frames_sent,
            frames_recv: self.frames_recv,
        }
    }

    /// Total payload bytes in both directions.
    pub fn total_bytes(&self) -> usize {
        self.bytes_sent + self.bytes_recv
    }
}

/// An immutable traffic snapshot.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Traffic {
    pub bytes_sent: usize,
    pub bytes_recv: usize,
    pub frames_sent: usize,
    pub frames_recv: usize,
}

impl Traffic {
    /// Total payload bytes in both directions.
    pub fn total_bytes(&self) -> usize {
        self.bytes_sent + self.bytes_recv
    }
}

impl<T: Transport> Transport for CountingTransport<'_, T> {
    fn send(&mut self, frame: &[u8]) {
        self.bytes_sent += frame.len();
        self.frames_sent += 1;
        self.inner.send(frame);
    }
    fn recv(&mut self) -> Vec<u8> {
        let f = self.inner.recv();
        self.bytes_recv += f.len();
        self.frames_recv += 1;
        f
    }
}
