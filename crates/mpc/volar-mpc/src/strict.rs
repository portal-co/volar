// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//!
//! The **strict session**: a garbled-circuit session whose verdict is
//! authenticated by the garbler, for deployments where the evaluator is the
//! potentially-cheating party (e.g. licensing: the server garbles and
//! verifies, the client evaluates and wants the verdict to be `true`).
//!
//! The legacy session ([`crate::run_garbler`]/[`crate::run_evaluator`]) is
//! semi-honest: its setup frame carries `one_wire` (which *is* the free-XOR
//! delta) and the output wire's false-label base, and the evaluator decodes
//! and claims the verdict. An adversarial evaluator can compute
//! `encode(output_base, true) = output_base XOR delta` directly and forge the
//! true output label without evaluating anything.
//!
//! The strict session closes that forgery path:
//!
//! 1. **Not-elimination** ([`eliminate_nots`]): NOT and constant-ONE gates
//!    are compiled away by tracking *polarity* — a compile-time flip per
//!    value, folded into XOR results, AND-table garbling (per-AND input
//!    polarities), and output decode (per-output polarities). The resulting
//!    schedule uses only `Zero`/`Xor`/`And` (plus any storage gates), so the
//!    evaluator never needs the delta.
//! 2. **Minimal setup** ([`SessionFrame::SetupStrict`]): only the garbled
//!    tables are sent — no delta, no output bases.
//! 3. **Garbler-decoded verdict**: the evaluator returns its output *labels*
//!    ([`SessionFrame::OutputLabels`]); the garbler decodes each against its
//!    private output base and that output's polarity, exact-matching against
//!    the two valid encodings (anything else is a forgery), and returns the
//!    authenticated bits ([`SessionFrame::VerdictBits`]).

use alloc::vec::Vec;

use digest::Digest;
use hybrid_array::Array;
use volar_spec::garble::{Garble, GarbleTable, GlobalSecret};
use volar_spec::vole::VoleArray;

use crate::{
    DynEvalSetup, DynGarbledCircuit, DynGarbledExec, Eval, Gate, GateSchedule, InputOwner,
    MpcError, OtChannel, SessionFrame, Transport,
};

/// The result of [`eliminate_nots`]: a Not/One-free schedule plus the
/// polarity bookkeeping the garbler needs to fold the eliminated flips into
/// AND tables and output decode.
#[derive(Clone, Debug)]
pub struct EliminatedNots {
    /// The rewritten schedule (only `Zero`/`Xor`/`And`/storage gates).
    pub schedule: GateSchedule,
    /// Per-AND-gate logical input polarities `(pa, pb)`, in the eliminated
    /// schedule's AND-gate order: the gate's logical inputs are
    /// `raw_a XOR pa` and `raw_b XOR pb`.
    pub and_input_polarity: Vec<(bool, bool)>,
    /// Per-output polarities, in `output_wires()` order: output `o`'s logical
    /// value is `raw XOR output_polarity[o]`.
    pub output_polarity: Vec<bool>,
}

/// Eliminate every `Gate::Not` and `Gate::One` from a schedule by tracking
/// polarity: every old wire is re-expressed as an alias of a canonical
/// (gate-defined) new wire plus a flip bit.
///
/// - `Zero` emits `Zero` (polarity `false`); `One` emits `Zero` with the
///   alias flip `true`.
/// - `Not(a)` emits no gate; the alias points at `a`'s canonical wire with
///   the flip toggled.
/// - `Xor(a, b)` emits over the canonical wires; the emitted wire's own
///   polarity is `flip[a] XOR flip[b]`.
/// - `And(a, b)` emits over the canonical wires and records
///   `(flip[a], flip[b])` in [`EliminatedNots::and_input_polarity`] (the
///   garbler folds them into the table).
///
/// Output wires are remapped through the alias map, with each output's flip
/// recorded in [`EliminatedNots::output_polarity`].
///
/// Fails (`MalformedSchedule`) if a `StorageWrite`'s source wire is flipped
/// (the GRAM driver decodes the raw write bit; structure such circuits so
/// write sources are unflipped).
pub fn eliminate_nots(schedule: &GateSchedule) -> Result<EliminatedNots, MpcError> {
    let n_in = schedule.num_inputs;
    // map[old_wire] = (canonical new wire, flip): the old wire's logical
    // value is `raw[canonical] XOR flip`.
    let mut map: Vec<(usize, bool)> = (0..n_in).map(|i| (i, false)).collect();
    let mut gates: Vec<Gate> = Vec::with_capacity(schedule.gates.len());
    let mut and_input_polarity: Vec<(bool, bool)> = Vec::new();
    for gate in &schedule.gates {
        let mapped: (usize, bool) = match *gate {
            Gate::Zero => {
                gates.push(Gate::Zero);
                (n_in + gates.len() - 1, false)
            }
            Gate::One => {
                gates.push(Gate::Zero);
                (n_in + gates.len() - 1, true)
            }
            Gate::Not(a) => {
                let (w, p) = *map.get(a).ok_or(MpcError::MalformedSchedule)?;
                (w, !p)
            }
            Gate::Xor(a, b) => {
                let (wa, pa) = *map.get(a).ok_or(MpcError::MalformedSchedule)?;
                let (wb, pb) = *map.get(b).ok_or(MpcError::MalformedSchedule)?;
                gates.push(Gate::Xor(wa, wb));
                // The emitted wire's raw value is `raw_a XOR raw_b`, so its
                // own polarity (flip between its logical and raw value) is
                // `pa XOR pb`.
                (n_in + gates.len() - 1, pa ^ pb)
            }
            Gate::And(a, b) => {
                let (wa, pa) = *map.get(a).ok_or(MpcError::MalformedSchedule)?;
                let (wb, pb) = *map.get(b).ok_or(MpcError::MalformedSchedule)?;
                gates.push(Gate::And(wa, wb));
                and_input_polarity.push((pa, pb));
                (n_in + gates.len() - 1, false)
            }
            Gate::StorageRead {
                storage,
                cell,
                access,
            } => {
                gates.push(Gate::StorageRead {
                    storage,
                    cell,
                    access,
                });
                (n_in + gates.len() - 1, false)
            }
            Gate::StorageWrite {
                storage,
                cell,
                src,
                access,
            } => {
                let (ws, ps) = *map.get(src).ok_or(MpcError::MalformedSchedule)?;
                if ps {
                    // The GRAM driver decodes the raw write bit; a flipped
                    // source would silently write the complement.
                    return Err(MpcError::MalformedSchedule);
                }
                gates.push(Gate::StorageWrite {
                    storage,
                    cell,
                    src: ws,
                    access,
                });
                (n_in + gates.len() - 1, false)
            }
            Gate::ActionBit { call, bit } => {
                // The gate itself carries no wires; the spec's wires are
                // remapped (with polarities recorded) below. The emitted
                // wire's raw value IS the delivered result bit (flip false).
                let _ = (call, bit);
                gates.push(Gate::ActionBit { call, bit });
                (n_in + gates.len() - 1, false)
            }
        };
        map.push(mapped);
    }

    let remap = |&o: &usize| -> Result<(usize, bool), MpcError> {
        map.get(o).copied().ok_or(MpcError::MalformedSchedule)
    };
    let (output, _) = remap(&schedule.output)?;
    let out_wires: Vec<usize> = schedule
        .output_wires()
        .iter()
        .map(|o| remap(o).map(|m| m.0))
        .collect::<Result<_, MpcError>>()?;
    let output_polarity: Vec<bool> = schedule
        .output_wires()
        .iter()
        .map(|o| remap(o).map(|m| m.1))
        .collect::<Result<_, MpcError>>()?;
    let outputs = schedule.outputs.as_ref().map(|_| out_wires);
    // Remap the action specs' wires through the alias map, recording the
    // per-wire polarities the garbler uses to recover logical arg values.
    let mut actions: Vec<crate::ActionSpec> = Vec::with_capacity(schedule.actions.len());
    for spec in &schedule.actions {
        let remap_pol = |&w: &usize| -> Result<(usize, bool), MpcError> {
            map.get(w).copied().ok_or(MpcError::MalformedSchedule)
        };
        let (guard, guard_polarity) = remap_pol(&spec.guard)?;
        let arg_pairs: Vec<(usize, bool)> = spec
            .arg_wires
            .iter()
            .map(remap_pol)
            .collect::<Result<_, MpcError>>()?;
        let fb_pairs: Vec<(usize, bool)> = spec
            .fallback_wires
            .iter()
            .map(remap_pol)
            .collect::<Result<_, MpcError>>()?;
        actions.push(crate::ActionSpec {
            name: spec.name.clone(),
            guard,
            arg_wires: arg_pairs.iter().map(|p| p.0).collect(),
            fallback_wires: fb_pairs.iter().map(|p| p.0).collect(),
            num_bits: spec.num_bits,
            guard_polarity,
            arg_polarity: arg_pairs.iter().map(|p| p.1).collect(),
            fallback_polarity: fb_pairs.iter().map(|p| p.1).collect(),
        });
    }
    Ok(EliminatedNots {
        schedule: GateSchedule {
            num_inputs: n_in,
            gates,
            output,
            outputs,
            storages: schedule.storages.clone(),
            actions,
        },
        and_input_polarity,
        output_polarity,
    })
}

/// Polarity-aware runtime-sized garbling over an [`eliminate_nots`] result.
pub fn garble_schedule_strict_dyn<N, D>(
    elim: &EliminatedNots,
    secret: GlobalSecret<N>,
    input_labels: Vec<Garble<N>>,
) -> Result<DynGarbledExec<N>, MpcError>
where
    N: VoleArray<u8>,
    D: Digest,
{
    garble_schedule_strict_dyn_full::<N, D>(elim, secret, input_labels).map(|full| full.exec)
}

/// [`garble_schedule_strict_dyn`] plus the full per-wire false-label bases —
/// the strict-*actions* garbler needs them to decode action-argument labels
/// (by exact match) mid-session.
pub fn garble_schedule_strict_dyn_full<N, D>(
    elim: &EliminatedNots,
    secret: GlobalSecret<N>,
    input_labels: Vec<Garble<N>>,
) -> Result<StrictGarbledFull<N>, MpcError>
where
    N: VoleArray<u8>,
    D: Digest,
{
    let schedule = &elim.schedule;
    if schedule
        .gates
        .iter()
        .any(|g| matches!(g, Gate::Not(_) | Gate::One))
    {
        return Err(MpcError::MalformedSchedule);
    }
    if elim.and_input_polarity.len() != schedule.and_count() {
        return Err(MpcError::MalformedSchedule);
    }
    let (wires, tables) = crate::garble_wire_bases_pol::<N, D>(
        schedule,
        &secret,
        &input_labels,
        Some(&elim.and_input_polarity),
    )?;
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
        .collect::<Result<_, MpcError>>()?;
    Ok(StrictGarbledFull {
        exec: DynGarbledExec {
            circuit: DynGarbledCircuit {
                secret,
                input_labels,
                tables,
                output_label,
            },
            output_labels,
            schedule: schedule.clone(),
        },
        wire_bases: wires,
    })
}

/// The result of [`garble_schedule_strict_dyn_full`]: the garbled exec plus
/// the full per-wire false-label bases (garbler-private).
pub struct StrictGarbledFull<N: VoleArray<u8>> {
    /// The garbled circuit + schedule.
    pub exec: DynGarbledExec<N>,
    /// Every wire's false-label base, in wire order (inputs then gates).
    pub wire_bases: Vec<Garble<N>>,
}

/// The evaluator-hosted extern (action) executor for the strict-actions
/// session: the evaluator runs the action (e.g. a network socket op) on the
/// decoded logical argument bits.
pub trait StrictActionHost {
    /// Execute action `name` on the decoded argument bits, returning the
    /// result bits (the schedule's `ActionSpec::num_bits` wide).
    fn action(&mut self, name: &str, args: &[bool]) -> Result<Vec<bool>, MpcError>;
}

/// The strict garbler role for a schedule carrying actions
/// ([`Gate::ActionBit`]): identical to [`run_garbler_strict`] plus, per
/// action call, a decode round-trip (the evaluator sends the arg labels, the
/// garbler returns the logical bits) and one OT per result bit delivering
/// the host's value against the pinned action-result base.
///
/// The decoded argument values are public by design (e.g. TLS ciphertext
/// records) — never let a secret value reach an action's argument wires
/// unencrypted.
pub fn run_garbler_strict_actions<N, D, T: Transport>(
    full: &StrictGarbledFull<N>,
    elim: &EliminatedNots,
    partition: &[InputOwner],
    public_bits: &[bool],
    garbler_bits: &[bool],
    transport: &mut T,
    ot: &mut dyn OtChannel<N>,
) -> Result<Vec<bool>, MpcError>
where
    N: VoleArray<u8>,
    D: Digest,
{
    let exec = &full.exec;
    let schedule = &exec.schedule;
    if partition.len() != schedule.num_inputs {
        return Err(MpcError::BadPartition);
    }

    // Setup: tables only (no delta, no output bases).
    transport.send(
        &SessionFrame::SetupStrict {
            tables: exec
                .circuit
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
        }
        .encode(),
    );

    // Owned-input labels (public + garbler-owned), in circuit-input order.
    let mut owned: Vec<Vec<u8>> = Vec::new();
    let mut pub_i = 0usize;
    let mut gb_i = 0usize;
    for (idx, owner) in partition.iter().enumerate() {
        let wire = exec
            .circuit
            .input_labels
            .get(idx)
            .ok_or(MpcError::MalformedSchedule)?;
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

    // OT for each evaluator-owned input bit (garbler is OT sender).
    for (idx, owner) in partition.iter().enumerate() {
        if *owner == InputOwner::Evaluator {
            let wire = &exec.circuit.input_labels[idx];
            let f = exec.circuit.secret.encode(wire, false);
            let t = exec.circuit.secret.encode(wire, true);
            ot.send([&f.target, &t.target]);
        }
    }

    // Lockstep action walk: per call, decode the evaluator's arg labels and
    // offer each result bit by OT (both parties derive the same sequence of
    // events from the schedule).
    let mut done_calls: Vec<bool> = alloc::vec![false; schedule.actions.len()];
    for gate in &schedule.gates {
        if let Gate::ActionBit { call, bit: 0 } = *gate {
            let call = call as usize;
            if done_calls.get(call).copied().unwrap_or(false) {
                return Err(MpcError::MalformedSchedule);
            }
            done_calls[call] = true;
            let spec = schedule
                .actions
                .get(call)
                .ok_or(MpcError::MalformedSchedule)?;
            let frame =
                SessionFrame::decode(&transport.recv()).ok_or(MpcError::UnexpectedMessage)?;
            let labels = match frame {
                SessionFrame::ActionArgs { call: c, labels } if c as usize == call => labels,
                _ => return Err(MpcError::UnexpectedMessage),
            };
            let nwires = 1 + spec.arg_wires.len() + spec.fallback_wires.len();
            if labels.len() != nwires {
                return Err(MpcError::UnexpectedMessage);
            }
            let mut decode_one = |i: usize, wire: usize, pol: bool| -> Result<bool, MpcError> {
                let label = vec_to_arr::<N>(&labels[i]).ok_or(MpcError::MalformedSchedule)?;
                let base = full
                    .wire_bases
                    .get(wire)
                    .ok_or(MpcError::MalformedSchedule)?;
                decode_output_label(&exec.circuit.secret, base, pol, &label)
                    .ok_or(MpcError::DecodeFailure)
            };
            let mut bits = Vec::with_capacity(nwires);
            bits.push(decode_one(0, spec.guard, spec.guard_polarity)?);
            for (i, (&w, &p)) in spec
                .arg_wires
                .iter()
                .zip(spec.arg_polarity.iter())
                .enumerate()
            {
                bits.push(decode_one(1 + i, w, p)?);
            }
            for (i, (&w, &p)) in spec
                .fallback_wires
                .iter()
                .zip(spec.fallback_polarity.iter())
                .enumerate()
            {
                bits.push(decode_one(1 + spec.arg_wires.len() + i, w, p)?);
            }
            transport.send(
                &SessionFrame::ActionArgsClear {
                    call: call as u32,
                    bits,
                }
                .encode(),
            );
            // Offer each result bit by OT against the pinned base.
            let guard_base = full
                .wire_bases
                .get(spec.guard)
                .cloned()
                .ok_or(MpcError::MalformedSchedule)?;
            let arg_bases: Vec<Garble<N>> = spec
                .arg_wires
                .iter()
                .map(|&w| {
                    full.wire_bases
                        .get(w)
                        .cloned()
                        .ok_or(MpcError::MalformedSchedule)
                })
                .collect::<Result<_, MpcError>>()?;
            let arg_refs: Vec<&Garble<N>> = arg_bases.iter().collect();
            for i in 0..spec.num_bits {
                let base = guard_base.action_result_base::<D>(&arg_refs, i);
                let f = exec.circuit.secret.encode(&base, false);
                let t = exec.circuit.secret.encode(&base, true);
                ot.send([&f.target, &t.target]);
            }
        }
    }

    // Receive the evaluator's output labels and decode each against its
    // private base + polarity: exact match against one of the two valid
    // encodings, else forgery.
    let frame = SessionFrame::decode(&transport.recv()).ok_or(MpcError::UnexpectedMessage)?;
    let labels = match frame {
        SessionFrame::OutputLabels(labels) => labels,
        _ => return Err(MpcError::UnexpectedMessage),
    };
    let out_wires = schedule.output_wires();
    if labels.len() != out_wires.len() || labels.len() != elim.output_polarity.len() {
        return Err(MpcError::UnexpectedMessage);
    }
    let mut verdict = Vec::with_capacity(out_wires.len());
    for (o, _) in out_wires.iter().enumerate() {
        let base = exec
            .output_labels
            .get(o)
            .ok_or(MpcError::MalformedSchedule)?;
        let label = vec_to_arr::<N>(&labels[o]).ok_or(MpcError::MalformedSchedule)?;
        match decode_output_label(&exec.circuit.secret, base, elim.output_polarity[o], &label) {
            Some(b) => verdict.push(b),
            None => {
                transport.send(&SessionFrame::Verdict(Err(())).encode());
                return Err(MpcError::DecodeFailure);
            }
        }
    }
    transport.send(&SessionFrame::VerdictBits(verdict.clone()).encode());
    Ok(verdict)
}

/// Decode one evaluator output label against its private base and the
/// output's polarity: exact match against one of the two valid encodings.
/// `None` = the label is neither valid encoding (forgery / corruption).
pub fn decode_output_label<N: VoleArray<u8>>(
    secret: &GlobalSecret<N>,
    base: &Garble<N>,
    polarity: bool,
    label: &Array<u8, N>,
) -> Option<bool> {
    if *label == secret.encode(base, polarity).target {
        Some(false)
    } else if *label == secret.encode(base, !polarity).target {
        Some(true)
    } else {
        None
    }
}

fn arr_to_vec<N: VoleArray<u8>>(a: &Array<u8, N>) -> Vec<u8> {
    a.as_slice().to_vec()
}
fn vec_to_arr<N: VoleArray<u8>>(v: &[u8]) -> Option<Array<u8, N>> {
    if v.len() != N::USIZE {
        return None;
    }
    Some(Array::<u8, N>::from_fn(|i| v[i]))
}

/// The strict garbler role: sends only the tables, then the owned-input
/// labels, runs the evaluator-input OTs, receives the output labels, decodes
/// them against its private bases (exact match — anything else is a
/// forgery), and returns the authenticated verdict bits.
pub fn run_garbler_strict<N, D, T: Transport>(
    exec: &DynGarbledExec<N>,
    elim: &EliminatedNots,
    partition: &[InputOwner],
    public_bits: &[bool],
    garbler_bits: &[bool],
    transport: &mut T,
    ot: &mut dyn OtChannel<N>,
) -> Result<Vec<bool>, MpcError>
where
    N: VoleArray<u8>,
    D: Digest,
{
    let schedule = &exec.schedule;
    if partition.len() != schedule.num_inputs {
        return Err(MpcError::BadPartition);
    }

    // Setup: tables only (no delta, no output bases).
    transport.send(
        &SessionFrame::SetupStrict {
            tables: exec
                .circuit
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
        }
        .encode(),
    );

    // Owned-input labels (public + garbler-owned), in circuit-input order.
    let mut owned: Vec<Vec<u8>> = Vec::new();
    let mut pub_i = 0usize;
    let mut gb_i = 0usize;
    for (idx, owner) in partition.iter().enumerate() {
        let wire = exec
            .circuit
            .input_labels
            .get(idx)
            .ok_or(MpcError::MalformedSchedule)?;
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

    // OT for each evaluator-owned input bit (garbler is OT sender).
    for (idx, owner) in partition.iter().enumerate() {
        if *owner == InputOwner::Evaluator {
            let wire = &exec.circuit.input_labels[idx];
            let f = exec.circuit.secret.encode(wire, false);
            let t = exec.circuit.secret.encode(wire, true);
            ot.send([&f.target, &t.target]);
        }
    }

    // Receive the evaluator's output labels and decode each against its
    // private base + polarity: exact match against one of the two valid
    // encodings, else forgery.
    let frame = SessionFrame::decode(&transport.recv()).ok_or(MpcError::UnexpectedMessage)?;
    let labels = match frame {
        SessionFrame::OutputLabels(labels) => labels,
        _ => return Err(MpcError::UnexpectedMessage),
    };
    let out_wires = schedule.output_wires();
    if labels.len() != out_wires.len() || labels.len() != elim.output_polarity.len() {
        return Err(MpcError::UnexpectedMessage);
    }
    let mut verdict = Vec::with_capacity(out_wires.len());
    for (o, _) in out_wires.iter().enumerate() {
        let base = exec
            .output_labels
            .get(o)
            .ok_or(MpcError::MalformedSchedule)?;
        let label = vec_to_arr::<N>(&labels[o]).ok_or(MpcError::MalformedSchedule)?;
        match decode_output_label(&exec.circuit.secret, base, elim.output_polarity[o], &label) {
            Some(b) => verdict.push(b),
            None => {
                transport.send(&SessionFrame::Verdict(Err(())).encode());
                return Err(MpcError::DecodeFailure);
            }
        }
    }
    transport.send(&SessionFrame::VerdictBits(verdict.clone()).encode());
    Ok(verdict)
}

/// The strict evaluator role: receives the tables and owned-input labels,
/// chooses its own input labels via OT, evaluates, and returns its output
/// labels to the garbler (never decoding — it cannot). Returns the
/// garbler-authenticated verdict bits sent back.
pub fn run_evaluator_strict<N, D, T: Transport>(
    schedule: &GateSchedule,
    partition: &[InputOwner],
    evaluator_bits: &[bool],
    transport: &mut T,
    ot: &mut dyn OtChannel<N>,
) -> Result<Vec<bool>, MpcError>
where
    N: VoleArray<u8>,
    D: Digest,
{
    if partition.len() != schedule.num_inputs {
        return Err(MpcError::BadPartition);
    }

    let setup_frame = SessionFrame::decode(&transport.recv()).ok_or(MpcError::UnexpectedMessage)?;
    let tables_raw = match setup_frame {
        SessionFrame::SetupStrict { tables } => tables,
        _ => return Err(MpcError::UnexpectedMessage),
    };
    let tables: Vec<GarbleTable<N>> = tables_raw
        .into_iter()
        .map(|rows| {
            let mut t: [Array<u8, N>; 4] = Default::default();
            for (r, row) in rows.iter().enumerate() {
                t[r] = vec_to_arr(row).ok_or(MpcError::MalformedSchedule)?;
            }
            Ok(GarbleTable { table: t })
        })
        .collect::<Result<Vec<_>, MpcError>>()?;

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

    // Evaluate on the Not-free schedule: no one-wire (delta) is needed.
    let setup = DynEvalSetup {
        one_wire: Eval::zero(),
        tables,
        output_label: Garble::zero(),
    };
    let out_labels = DynGarbledExec::<N>::eval_labels_multi::<D>(&setup, schedule, &labels)?;
    transport.send(
        &SessionFrame::OutputLabels(out_labels.iter().map(|l| arr_to_vec(&l.target)).collect())
            .encode(),
    );

    // Await the garbler-authenticated verdict.
    let frame = SessionFrame::decode(&transport.recv()).ok_or(MpcError::UnexpectedMessage)?;
    match frame {
        SessionFrame::VerdictBits(bits) => Ok(bits),
        SessionFrame::Verdict(Err(())) => Err(MpcError::DecodeFailure),
        _ => Err(MpcError::UnexpectedMessage),
    }
}

/// The strict evaluator role for a schedule carrying actions
/// ([`Gate::ActionBit`]): identical to [`run_evaluator_strict`] plus, per
/// action call, an arg-decode round-trip (the garbler learns the logical
/// argument bits — public by design), a host execution, and one OT per
/// result bit delivering the host's value against the pinned base.
pub fn run_evaluator_strict_actions<N, D, T: Transport>(
    schedule: &GateSchedule,
    partition: &[InputOwner],
    evaluator_bits: &[bool],
    transport: &mut T,
    ot: &mut dyn OtChannel<N>,
    host: &mut dyn StrictActionHost,
    gram: &mut [&mut dyn crate::GramDrive<N>],
) -> Result<Vec<bool>, MpcError>
where
    N: VoleArray<u8>,
    D: Digest,
{
    if partition.len() != schedule.num_inputs {
        return Err(MpcError::BadPartition);
    }
    if gram.len() != schedule.storages.len() {
        return Err(MpcError::MalformedSchedule);
    }

    let setup_frame = SessionFrame::decode(&transport.recv()).ok_or(MpcError::UnexpectedMessage)?;
    let tables_raw = match setup_frame {
        SessionFrame::SetupStrict { tables } => tables,
        _ => return Err(MpcError::UnexpectedMessage),
    };
    let tables: Vec<GarbleTable<N>> = tables_raw
        .into_iter()
        .map(|rows| {
            let mut t: [Array<u8, N>; 4] = Default::default();
            for (r, row) in rows.iter().enumerate() {
                t[r] = vec_to_arr(row).ok_or(MpcError::MalformedSchedule)?;
            }
            Ok(GarbleTable { table: t })
        })
        .collect::<Result<Vec<_>, MpcError>>()?;

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

    // Evaluate on the Not-free schedule, with the action interactions inline.
    let mut wires: Vec<Eval<N>> = Vec::with_capacity(schedule.wire_count());
    wires.extend_from_slice(&labels);
    let mut table = 0usize;
    let mut call_results: Vec<Option<Vec<Eval<N>>>> =
        (0..schedule.actions.len()).map(|_| None).collect();
    for gate in &schedule.gates {
        let out = match *gate {
            Gate::Zero => Eval::zero(),
            Gate::One => return Err(MpcError::MalformedSchedule),
            Gate::Xor(a, b) => {
                let (x, y) = (wires.get(a), wires.get(b));
                match (x, y) {
                    (Some(x), Some(y)) => x.clone() ^ y.clone(),
                    _ => return Err(MpcError::MalformedSchedule),
                }
            }
            Gate::Not(_) => return Err(MpcError::MalformedSchedule),
            Gate::And(a, b) => {
                let t = tables.get(table).ok_or(MpcError::MalformedSchedule)?;
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
                let base = crate::gram_data_base::<D, N>(access, 0);
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
            Gate::ActionBit { call, bit } => {
                let call = call as usize;
                if call_results.get(call).and_then(|r| r.as_ref()).is_none() {
                    let spec = schedule
                        .actions
                        .get(call)
                        .ok_or(MpcError::MalformedSchedule)?;
                    // Send the wire labels [guard, args..., fallback...].
                    let mut arg_labels: Vec<Vec<u8>> =
                        Vec::with_capacity(1 + spec.arg_wires.len() + spec.fallback_wires.len());
                    arg_labels.push(arr_to_vec(
                        &wires
                            .get(spec.guard)
                            .ok_or(MpcError::MalformedSchedule)?
                            .target,
                    ));
                    for &w in spec.arg_wires.iter().chain(spec.fallback_wires.iter()) {
                        arg_labels.push(arr_to_vec(
                            &wires.get(w).ok_or(MpcError::MalformedSchedule)?.target,
                        ));
                    }
                    transport.send(
                        &SessionFrame::ActionArgs {
                            call: call as u32,
                            labels: arg_labels,
                        }
                        .encode(),
                    );
                    let frame = SessionFrame::decode(&transport.recv())
                        .ok_or(MpcError::UnexpectedMessage)?;
                    let bits = match frame {
                        SessionFrame::ActionArgsClear { call: c, bits } if c as usize == call => {
                            bits
                        }
                        _ => return Err(MpcError::UnexpectedMessage),
                    };
                    if bits.len() != 1 + spec.arg_wires.len() + spec.fallback_wires.len() {
                        return Err(MpcError::UnexpectedMessage);
                    }
                    let guard = bits[0];
                    let args = &bits[1..1 + spec.arg_wires.len()];
                    let fallback = &bits[1 + spec.arg_wires.len()..];
                    let result: Vec<bool> = if guard {
                        let r = host.action(&spec.name, args)?;
                        if r.len() != spec.num_bits {
                            return Err(MpcError::ActionHost);
                        }
                        r
                    } else {
                        if fallback.len() != spec.num_bits {
                            return Err(MpcError::MalformedSchedule);
                        }
                        fallback.to_vec()
                    };
                    // Each result bit is delivered by OT against the pinned
                    // action-result base.
                    let mut outs: Vec<Eval<N>> = Vec::with_capacity(spec.num_bits);
                    for &b in result.iter() {
                        outs.push(Eval {
                            target: ot.receive(b),
                        });
                    }
                    call_results[call] = Some(outs);
                }
                let outs = call_results[call]
                    .as_ref()
                    .ok_or(MpcError::MalformedSchedule)?;
                outs.get(bit as usize)
                    .cloned()
                    .ok_or(MpcError::MalformedSchedule)?
            }
        };
        wires.push(out);
    }

    let out_labels: Vec<Eval<N>> = schedule
        .output_wires()
        .iter()
        .map(|&w| wires.get(w).cloned().ok_or(MpcError::MalformedSchedule))
        .collect::<Result<_, MpcError>>()?;
    transport.send(
        &SessionFrame::OutputLabels(out_labels.iter().map(|l| arr_to_vec(&l.target)).collect())
            .encode(),
    );

    // Await the garbler-authenticated verdict.
    let frame = SessionFrame::decode(&transport.recv()).ok_or(MpcError::UnexpectedMessage)?;
    match frame {
        SessionFrame::VerdictBits(bits) => Ok(bits),
        SessionFrame::Verdict(Err(())) => Err(MpcError::DecodeFailure),
        _ => Err(MpcError::UnexpectedMessage),
    }
}
