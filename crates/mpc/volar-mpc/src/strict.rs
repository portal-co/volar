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
    Ok(EliminatedNots {
        schedule: GateSchedule {
            num_inputs: n_in,
            gates,
            output,
            outputs,
            storages: schedule.storages.clone(),
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
        &SessionFrame::OutputLabels(
            out_labels.iter().map(|l| arr_to_vec(&l.target)).collect(),
        )
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
