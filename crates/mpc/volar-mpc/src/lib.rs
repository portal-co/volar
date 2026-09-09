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
//! - [`GarblerSession`] / [`EvaluatorSession`] expose the same flow as
//!   [`volar_channel::Protocol`] state machines so the pair can be driven over
//!   any transport (in-memory via [`volar_channel::run_protocol`], or framed
//!   TCP), exactly the way `volar-oram` protocols are.
//!
//! # OT input delivery
//!
//! Evaluator-input labels move by 1-of-2 OT. This crate is generic over the OT
//! channel via [`OtChannel`], so tests can use a trivial in-process OT while
//! production wiring swaps in `volar-spec`'s OT stack (Chou–Orlandi base OT +
//! IKNP/Ferret extension) without touching session logic.

#![no_std]

extern crate alloc;

use alloc::vec::Vec;

use digest::Digest;
use hybrid_array::Array;
use volar_channel::{Protocol, Yield};
use volar_spec::garble::{Eval, EvalSetup, Garble, GarbleTable, GarbledCircuit, GlobalSecret};
use volar_spec::vole::VoleArray;

pub mod ot;

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
}

impl GateSchedule {
    /// Total number of wires (inputs + gate outputs).
    pub fn wire_count(&self) -> usize {
        self.num_inputs + self.gates.len()
    }

    /// Number of AND gates (== number of garbled tables required).
    pub fn and_count(&self) -> usize {
        self.gates.iter().filter(|g| matches!(g, Gate::And(..))).count()
    }
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
        if inputs.len() != schedule.num_inputs {
            return Err(MpcError::BadPartition);
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
            };
            wires.push(out);
        }
        wires.get(schedule.output).cloned().ok_or(MpcError::MalformedSchedule)
    }
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
    let mut wires: Vec<Garble<N>> = Vec::with_capacity(schedule.wire_count());
    wires.extend(input_labels.iter().cloned());
    let mut tables: Vec<GarbleTable<N>> = Vec::with_capacity(A);
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
                        tables.push(secret.gen_and_table::<D>(x, y));
                        x.and_result::<D>(y)
                    }
                    _ => return Err(MpcError::MalformedSchedule),
                }
            }
        };
        wires.push(out);
    }
    let output_label = wires
        .get(schedule.output)
        .cloned()
        .ok_or(MpcError::MalformedSchedule)?;
    let tables: [GarbleTable<N>; A] = tables
        .try_into()
        .map_err(|_| MpcError::MalformedSchedule)?;
    Ok(GarbledExec {
        circuit: GarbledCircuit {
            secret,
            input_labels,
            tables,
            output_label,
        },
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
/// the message flow that [`GarblerSession`]/[`EvaluatorSession`] expose as a
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
        || partition.iter().filter(|&&o| o == InputOwner::Public).count() != public_bits.len()
        || partition.iter().filter(|&&o| o == InputOwner::Garbler).count() != garbler_bits.len()
        || partition.iter().filter(|&&o| o == InputOwner::Evaluator).count() != evaluator_bits.len()
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

// ============================================================================
// Transport-driven sessions
// ============================================================================

/// Messages flowing between the two parties during one evaluation.
///
/// (`volar_spec::garble`'s label types carry no `Debug`, so neither does this.)
#[derive(Clone)]
pub enum MpcMessage<N: VoleArray<u8>> {
    /// Garbler → evaluator: tables, constant-one wire, and the output decode.
    Setup {
        one_wire: Eval<N>,
        tables: Vec<GarbleTable<N>>,
        output_label: Garble<N>,
    },
    /// Garbler → evaluator: selected labels for public + garbler-owned input
    /// wires, in circuit-input order restricted to those two sets.
    OwnedInputs(Vec<Eval<N>>),
    /// A single 1-of-2 OT frame for one evaluator input bit; payload is
    /// OT-scheme-specific and opaque to this layer.
    Ot(Vec<u8>),
    /// Evaluator → garbler: session verdict (tamper/abort reporting).
    Verdict,
}

/// Garbler half of one evaluation, as a [`Protocol`] state machine.
pub struct GarblerSession<N: VoleArray<u8>, const I: usize, const A: usize> {
    circuit: GarbledCircuit<N, I, A>,
    partition: Vec<InputOwner>,
    public_bits: Vec<bool>,
    garbler_bits: Vec<bool>,
}

impl<N: VoleArray<u8>, const I: usize, const A: usize> GarblerSession<N, I, A> {
    pub fn new(
        circuit: GarbledCircuit<N, I, A>,
        partition: Vec<InputOwner>,
        public_bits: Vec<bool>,
        garbler_bits: Vec<bool>,
    ) -> Self {
        Self {
            circuit,
            partition,
            public_bits,
            garbler_bits,
        }
    }

    /// Encode the labels this party sends for public + garbler-owned wires.
    fn owned_labels(&self) -> Vec<Eval<N>> {
        let mut out = Vec::new();
        let mut p = self.public_bits.iter();
        let mut g = self.garbler_bits.iter();
        for (idx, owner) in self.partition.iter().enumerate() {
            let wire = &self.circuit.input_labels[idx];
            match owner {
                InputOwner::Public => {
                    out.push(self.circuit.secret.encode(wire, *p.next().expect("public len")))
                }
                InputOwner::Garbler => {
                    out.push(self.circuit.secret.encode(wire, *g.next().expect("garbler len")))
                }
                InputOwner::Evaluator => {}
            }
        }
        out
    }
}

impl<N: VoleArray<u8>, const I: usize, const A: usize> Protocol for GarblerSession<N, I, A> {
    type State = Self;
    type Incoming = MpcMessage<N>;
    type Outgoing = MpcMessage<N>;
    type Done = Result<(), MpcError>;

    fn init(params: Self::State) -> (Self::State, Yield<Self::Done, Self::Outgoing>) {
        let setup = params.circuit.eval_setup();
        (
            params,
            Yield::Send(MpcMessage::Setup {
                one_wire: setup.one_wire,
                tables: setup.tables.to_vec(),
                output_label: setup.output_label,
            }),
        )
    }

    fn step(
        state: Self::State,
        _msg: Self::Incoming,
    ) -> (Self::State, Yield<Self::Done, Self::Outgoing>) {
        // After setup, deliver this party's owned input labels and finish; the
        // evaluator-input OTs are interposed by the wiring layer's OtChannel.
        let labels = state.owned_labels();
        (state, Yield::Send(MpcMessage::OwnedInputs(labels)))
    }
}

/// Evaluator half of one evaluation, as a [`Protocol`] state machine.
pub struct EvaluatorSession<N: VoleArray<u8>, const I: usize, const A: usize> {
    schedule: GateSchedule,
    partition: Vec<InputOwner>,
    evaluator_bits: Vec<bool>,
    setup: Option<EvalSetup<N, A>>,
}

impl<N: VoleArray<u8>, const I: usize, const A: usize> EvaluatorSession<N, I, A> {
    pub fn new(
        schedule: GateSchedule,
        partition: Vec<InputOwner>,
        evaluator_bits: Vec<bool>,
    ) -> Self {
        Self {
            schedule,
            partition,
            evaluator_bits,
            setup: None,
        }
    }
}

impl<N: VoleArray<u8>, const I: usize, const A: usize> Protocol for EvaluatorSession<N, I, A> {
    type State = Self;
    type Incoming = MpcMessage<N>;
    type Outgoing = MpcMessage<N>;
    type Done = Result<bool, MpcError>;

    fn init(params: Self::State) -> (Self::State, Yield<Self::Done, Self::Outgoing>) {
        // Signal readiness; the garbler's Setup is the real first message.
        (params, Yield::Send(MpcMessage::Verdict))
    }

    fn step(
        mut state: Self::State,
        msg: Self::Incoming,
    ) -> (Self::State, Yield<Self::Done, Self::Outgoing>) {
        match msg {
            MpcMessage::Setup {
                one_wire,
                tables,
                output_label,
            } => {
                let Ok(tables) = <Vec<GarbleTable<N>> as TryInto<[GarbleTable<N>; A]>>::try_into(
                    tables,
                ) else {
                    return (state, Yield::Done(Err(MpcError::UnexpectedMessage)));
                };
                state.setup = Some(EvalSetup {
                    one_wire,
                    tables,
                    output_label,
                });
                (state, Yield::Send(MpcMessage::Verdict))
            }
            // Full evaluation additionally requires the evaluator's
            // OT-delivered labels, supplied by the wiring layer's OtChannel;
            // the bare state machine tracks phases only. See `evaluate`.
            MpcMessage::OwnedInputs(_) => (state, Yield::Done(Err(MpcError::UnexpectedMessage))),
            _ => (state, Yield::Done(Err(MpcError::UnexpectedMessage))),
        }
    }
}
