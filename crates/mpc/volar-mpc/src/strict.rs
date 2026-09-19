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

use crate::strict_cursor::{CursorState, StrictGateCursor};
use crate::{
    DynGarbledCircuit, DynGarbledExec, Eval, EvaluatorBatchExecutor, ExternalBatchAction,
    ExternalBatchActionHost, ExternalBatchBinding, ExternalBatchFrame, ExternalBatchManifest,
    ExternalBatchTranscript, GarblerBatchExecutor, Gate, GateSchedule, InputOwner, MpcError,
    OtChannel, SessionFrame, Transport,
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
            request_id: spec.request_id,
            action_ordinal: spec.action_ordinal,
            execution: spec.execution,
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

impl<N: VoleArray<u8>> StrictGarbledFull<N> {
    /// Construct the canonical action-only external manifest for this strict
    /// schedule and bind it to caller-owned session/circuit digests.
    pub fn external_action_manifest(
        &self,
        boundary: crate::ExternalBoundaryId,
        session_digest: [u8; 32],
        circuit_digest: [u8; 32],
    ) -> Result<(ExternalBatchManifest, ExternalBatchBinding), MpcError> {
        let manifest = ExternalBatchManifest::from_actions(boundary, &self.exec.schedule.actions)?;
        let binding = manifest.bind(session_digest, circuit_digest);
        Ok((manifest, binding))
    }
}

/// The evaluator-hosted extern (action) executor for the strict-actions
/// session: the evaluator runs the action (e.g. a network socket op) on the
/// decoded logical argument bits.
pub trait StrictActionHost {
    /// Execute action `name` on the decoded argument bits, returning the
    /// result bits (the schedule's `ActionSpec::num_bits` wide).
    fn action(&mut self, name: &str, args: &[bool]) -> Result<Vec<bool>, MpcError>;
}

struct LegacyBatchHost<'a> {
    host: &'a mut dyn StrictActionHost,
}

impl ExternalBatchActionHost for LegacyBatchHost<'_> {
    fn action(
        &mut self,
        registration: &ExternalBatchAction,
        args: &[bool],
    ) -> Result<Vec<bool>, MpcError> {
        self.host.action(&registration.name, args)
    }
}

fn batch_action_registrations(
    schedule: &GateSchedule,
) -> Result<Vec<ExternalBatchAction>, MpcError> {
    validate_batch_action_policy(schedule)?;
    schedule
        .actions
        .iter()
        .map(|action| {
            Ok(ExternalBatchAction {
                request_id: action.request_id,
                name: action.name.clone(),
                argument_bits: action.arg_wires.len(),
            })
        })
        .collect()
}

/// Determine the one executor role supported by this strict batch runner.
/// A mixed-executor boundary needs a role-aware dispatcher that can maintain
/// both local host registries in one transcript; reject it rather than routing
/// an action to the wrong process.
fn homogeneous_batch_executor(
    schedule: &GateSchedule,
) -> Result<crate::ExternalExecutor, MpcError> {
    let first = schedule
        .actions
        .first()
        .map(|action| action.execution.executor)
        .unwrap_or(crate::ExternalExecutor::Evaluator);
    if schedule
        .actions
        .iter()
        .all(|action| action.execution.executor == first)
    {
        Ok(first)
    } else {
        Err(MpcError::UnsupportedExternalPolicy)
    }
}

/// Validate the original strict compatibility action mode.
///
/// This remains evaluator-hosted because old `ActionArgs` frames cannot express
/// a garbler executor. The versioned batch runner uses
/// [`validate_batch_action_policy`] instead.
pub fn validate_legacy_action_policy(schedule: &GateSchedule) -> Result<(), MpcError> {
    for action in &schedule.actions {
        validate_legacy_action_spec(action)?;
    }
    Ok(())
}

/// Validate the action modes implemented by the versioned strict batch
/// transport. Both executors require explicitly authorized `BothRoles`
/// disclosure; `ExecutorOnly` stays fail-closed until its distinct transcript
/// direction is implemented.
pub fn validate_batch_action_policy(schedule: &GateSchedule) -> Result<(), MpcError> {
    for action in &schedule.actions {
        if !matches!(
            (action.execution.executor, action.execution.reveal),
            (
                crate::ExternalExecutor::Evaluator,
                crate::ExternalRevealPolicy::BothRoles
            ) | (
                crate::ExternalExecutor::Garbler,
                crate::ExternalRevealPolicy::BothRoles
            )
        ) {
            return Err(MpcError::UnsupportedExternalPolicy);
        }
    }
    Ok(())
}

/// Validate one action before the legacy evaluator-hosted adapter exposes any
/// label-derived input material. `StrictGateCursor` calls this too, so users
/// of its public pause/resume API cannot bypass schedule-entry validation.
pub fn validate_legacy_action_spec(action: &crate::ActionSpec) -> Result<(), MpcError> {
    if action.execution.executor == crate::ExternalExecutor::Evaluator
        && action.execution.reveal == crate::ExternalRevealPolicy::BothRoles
    {
        Ok(())
    } else {
        Err(MpcError::UnsupportedExternalPolicy)
    }
}

pub(crate) fn validate_batch_action_spec(action: &crate::ActionSpec) -> Result<(), MpcError> {
    if matches!(
        (action.execution.executor, action.execution.reveal),
        (
            crate::ExternalExecutor::Evaluator,
            crate::ExternalRevealPolicy::BothRoles
        ) | (
            crate::ExternalExecutor::Garbler,
            crate::ExternalRevealPolicy::BothRoles
        )
    ) {
        Ok(())
    } else {
        Err(MpcError::UnsupportedExternalPolicy)
    }
}

/// Decode one evaluator-provided external-action reveal against the garbler's
/// private wire bases. This is the garbler half of strict label transport.
///
/// The returned bits are authorized only for the explicit legacy evaluator +
/// `BothRoles` policy. The label vector must be exactly
/// `[guard, args..., fallback...]`, in manifest/request order. It never
/// accepts a host-selected action name or a different request occurrence.
fn decode_action_reveal<N: VoleArray<u8>>(
    full: &StrictGarbledFull<N>,
    call: usize,
    request_id: u64,
    labels: &[Vec<u8>],
) -> Result<Vec<bool>, MpcError> {
    let spec = full
        .exec
        .schedule
        .actions
        .get(call)
        .ok_or(MpcError::MalformedSchedule)?;
    if request_id != spec.request_id {
        return Err(MpcError::UnexpectedMessage);
    }
    let expected = 1usize
        .checked_add(spec.arg_wires.len())
        .and_then(|count| count.checked_add(spec.fallback_wires.len()))
        .ok_or(MpcError::MalformedSchedule)?;
    if labels.len() != expected {
        return Err(MpcError::UnexpectedMessage);
    }
    let decode = |index: usize, wire: usize, polarity: bool| -> Result<bool, MpcError> {
        let label = crate::vec_to_arr::<N>(&labels[index]).ok_or(MpcError::MalformedSchedule)?;
        let base = full
            .wire_bases
            .get(wire)
            .ok_or(MpcError::MalformedSchedule)?;
        decode_output_label(&full.exec.circuit.secret, base, polarity, &label)
            .ok_or(MpcError::DecodeFailure)
    };
    let mut bits = Vec::with_capacity(expected);
    bits.push(decode(0, spec.guard, spec.guard_polarity)?);
    for (index, (&wire, &polarity)) in spec
        .arg_wires
        .iter()
        .zip(spec.arg_polarity.iter())
        .enumerate()
    {
        bits.push(decode(1 + index, wire, polarity)?);
    }
    for (index, (&wire, &polarity)) in spec
        .fallback_wires
        .iter()
        .zip(spec.fallback_polarity.iter())
        .enumerate()
    {
        bits.push(decode(1 + spec.arg_wires.len() + index, wire, polarity)?);
    }
    Ok(bits)
}

/// Decode a reveal for the legacy evaluator-hosted strict path.
pub fn decode_legacy_action_reveal<N: VoleArray<u8>>(
    full: &StrictGarbledFull<N>,
    call: usize,
    request_id: u64,
    labels: &[Vec<u8>],
) -> Result<Vec<bool>, MpcError> {
    let spec = full
        .exec
        .schedule
        .actions
        .get(call)
        .ok_or(MpcError::MalformedSchedule)?;
    validate_legacy_action_spec(spec)?;
    decode_action_reveal(full, call, request_id, labels)
}

fn decode_batch_action_reveal<N: VoleArray<u8>>(
    full: &StrictGarbledFull<N>,
    call: usize,
    request_id: u64,
    labels: &[Vec<u8>],
) -> Result<Vec<bool>, MpcError> {
    let spec = full
        .exec
        .schedule
        .actions
        .get(call)
        .ok_or(MpcError::MalformedSchedule)?;
    validate_batch_action_spec(spec)?;
    decode_action_reveal(full, call, request_id, labels)
}

/// Offer the two garbled encodings for every action result wire through OT.
///
/// The garbler never learns the evaluator host's selected result bit. Bases
/// are bound to the action's guard/argument wire bases and output index, so a
/// result label from another request cannot be reinserted at this call site.
pub fn offer_legacy_action_result_labels<N, D>(
    full: &StrictGarbledFull<N>,
    call: usize,
    ot: &mut dyn OtChannel<N>,
) -> Result<(), MpcError>
where
    N: VoleArray<u8>,
    D: Digest,
{
    let spec = full
        .exec
        .schedule
        .actions
        .get(call)
        .ok_or(MpcError::MalformedSchedule)?;
    validate_legacy_action_spec(spec)?;
    let guard_base = full
        .wire_bases
        .get(spec.guard)
        .cloned()
        .ok_or(MpcError::MalformedSchedule)?;
    let arg_bases: Vec<Garble<N>> = spec
        .arg_wires
        .iter()
        .map(|&wire| {
            full.wire_bases
                .get(wire)
                .cloned()
                .ok_or(MpcError::MalformedSchedule)
        })
        .collect::<Result<_, _>>()?;
    let arg_refs: Vec<&Garble<N>> = arg_bases.iter().collect();
    for bit in 0..spec.num_bits {
        let base = guard_base.action_result_base::<D>(&arg_refs, bit);
        let false_label = full.exec.circuit.secret.encode(&base, false);
        let true_label = full.exec.circuit.secret.encode(&base, true);
        ot.send([&false_label.target, &true_label.target]);
    }
    Ok(())
}

fn offer_batch_action_result_labels<N, D>(
    full: &StrictGarbledFull<N>,
    call: usize,
    ot: &mut dyn OtChannel<N>,
) -> Result<(), MpcError>
where
    N: VoleArray<u8>,
    D: Digest,
{
    let spec = full
        .exec
        .schedule
        .actions
        .get(call)
        .ok_or(MpcError::MalformedSchedule)?;
    validate_batch_action_spec(spec)?;
    let guard_base = full
        .wire_bases
        .get(spec.guard)
        .cloned()
        .ok_or(MpcError::MalformedSchedule)?;
    let arg_bases: Vec<Garble<N>> = spec
        .arg_wires
        .iter()
        .map(|&wire| {
            full.wire_bases
                .get(wire)
                .cloned()
                .ok_or(MpcError::MalformedSchedule)
        })
        .collect::<Result<_, _>>()?;
    let arg_refs: Vec<&Garble<N>> = arg_bases.iter().collect();
    for bit in 0..spec.num_bits {
        let base = guard_base.action_result_base::<D>(&arg_refs, bit);
        let false_label = full.exec.circuit.secret.encode(&base, false);
        let true_label = full.exec.circuit.secret.encode(&base, true);
        ot.send([&false_label.target, &true_label.target]);
    }
    Ok(())
}

/// Decode a versioned external-batch reveal into the clear-input frame used by
/// the explicit evaluator-host executor.
///
/// This is the garbler half of batch label transport. It verifies the public
/// manifest/binding/request before exact-match decoding labels, and only then
/// emits a request-bound clear-input envelope. It supports the conservative
/// evaluator + `BothRoles` action profile exclusively.
pub fn decode_external_batch_action_reveal<N: VoleArray<u8>>(
    full: &StrictGarbledFull<N>,
    manifest: &ExternalBatchManifest,
    binding: ExternalBatchBinding,
    call: usize,
    frame: &ExternalBatchFrame,
) -> Result<ExternalBatchFrame, MpcError> {
    manifest
        .validate()
        .map_err(|_| MpcError::MalformedSchedule)?;
    frame
        .validate_request(manifest, binding)
        .map_err(|_| MpcError::UnexpectedMessage)?;
    let ExternalBatchFrame::Reveal {
        binding: received_binding,
        request_id,
        labels,
    } = frame
    else {
        return Err(MpcError::UnexpectedMessage);
    };
    if *received_binding != binding {
        return Err(MpcError::UnexpectedMessage);
    }
    let spec = full
        .exec
        .schedule
        .actions
        .get(call)
        .ok_or(MpcError::MalformedSchedule)?;
    let entry = manifest
        .actions
        .iter()
        .find(|entry| entry.request_id == spec.request_id)
        .ok_or(MpcError::UnexpectedMessage)?;
    if *request_id != spec.request_id
        || entry.execution != spec.execution
        || entry.output_bits != spec.num_bits
    {
        return Err(MpcError::UnexpectedMessage);
    }
    let bits = decode_batch_action_reveal(full, call, *request_id, labels)?;
    Ok(ExternalBatchFrame::ClearInputs {
        binding,
        request_id: *request_id,
        bits,
    })
}

/// Validate an external-batch result then offer its request-bound label pairs
/// through OT for evaluator-side reinsertion.
///
/// The clear result bits are never used by this garbler role: OT delivers only
/// the evaluator-selected labels whose bases are derived from this action's
/// guard/argument bases and output bit positions.
pub fn offer_external_batch_action_result_labels<N, D>(
    full: &StrictGarbledFull<N>,
    manifest: &ExternalBatchManifest,
    binding: ExternalBatchBinding,
    call: usize,
    frame: &ExternalBatchFrame,
    ot: &mut dyn OtChannel<N>,
) -> Result<(), MpcError>
where
    N: VoleArray<u8>,
    D: Digest,
{
    manifest
        .validate()
        .map_err(|_| MpcError::MalformedSchedule)?;
    frame
        .validate_request(manifest, binding)
        .map_err(|_| MpcError::UnexpectedMessage)?;
    let ExternalBatchFrame::Result {
        binding: received_binding,
        request_id,
        ..
    } = frame
    else {
        return Err(MpcError::UnexpectedMessage);
    };
    let spec = full
        .exec
        .schedule
        .actions
        .get(call)
        .ok_or(MpcError::MalformedSchedule)?;
    let entry = manifest
        .actions
        .iter()
        .find(|entry| entry.request_id == spec.request_id)
        .ok_or(MpcError::UnexpectedMessage)?;
    if *received_binding != binding
        || *request_id != spec.request_id
        || entry.execution != spec.execution
        || entry.output_bits != spec.num_bits
    {
        return Err(MpcError::UnexpectedMessage);
    }
    offer_batch_action_result_labels::<N, D>(full, call, ot)
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
/// Run strict actions under the legacy evaluator-hosted policy only.
/// Garbler-hosted actions require the explicit
/// [`run_garbler_strict_actions_garbler_host`] entry point.
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
    validate_legacy_action_policy(&full.exec.schedule)?;
    run_garbler_strict_actions_inner::<N, D, T>(
        full,
        elim,
        partition,
        public_bits,
        garbler_bits,
        transport,
        ot,
        None,
    )
}

/// Run a strict batch whose actions are all explicitly assigned to the
/// garbler under `BothRoles` disclosure. The local host is mandatory.
pub fn run_garbler_strict_actions_garbler_host<N, D, T: Transport>(
    full: &StrictGarbledFull<N>,
    elim: &EliminatedNots,
    partition: &[InputOwner],
    public_bits: &[bool],
    garbler_bits: &[bool],
    transport: &mut T,
    ot: &mut dyn OtChannel<N>,
    host: &mut dyn StrictActionHost,
) -> Result<Vec<bool>, MpcError>
where
    N: VoleArray<u8>,
    D: Digest,
{
    validate_batch_action_policy(&full.exec.schedule)?;
    if homogeneous_batch_executor(&full.exec.schedule)? != crate::ExternalExecutor::Garbler {
        return Err(MpcError::UnsupportedExternalPolicy);
    }
    run_garbler_strict_actions_inner::<N, D, T>(
        full,
        elim,
        partition,
        public_bits,
        garbler_bits,
        transport,
        ot,
        Some(host),
    )
}

fn run_garbler_strict_actions_inner<N, D, T: Transport>(
    full: &StrictGarbledFull<N>,
    elim: &EliminatedNots,
    partition: &[InputOwner],
    public_bits: &[bool],
    garbler_bits: &[bool],
    transport: &mut T,
    ot: &mut dyn OtChannel<N>,
    mut garbler_host: Option<&mut dyn StrictActionHost>,
) -> Result<Vec<bool>, MpcError>
where
    N: VoleArray<u8>,
    D: Digest,
{
    let exec = &full.exec;
    let schedule = &exec.schedule;
    validate_batch_action_policy(schedule)?;
    let executor_role = homogeneous_batch_executor(schedule)?;
    if partition.len() != schedule.num_inputs {
        return Err(MpcError::BadPartition);
    }

    // Tables stream after the owned inputs and OTs below. The evaluator cursor
    // pauses at action/storage gates and resumes from the same table stream.
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

    // Versioned external-batch transport is admitted before any streamed
    // table. The evaluator has completed its input OTs and can now confirm it
    // derived the exact same public action manifest.
    let manifest =
        ExternalBatchManifest::from_actions(crate::ExternalBoundaryId(0), &schedule.actions)?;
    let binding = manifest.bind([0; 32], [0; 32]);
    let peer =
        ExternalBatchFrame::decode(&transport.recv()).map_err(|_| MpcError::UnexpectedMessage)?;
    peer.validate_manifest(&manifest, binding)
        .map_err(|_| MpcError::UnexpectedMessage)?;
    transport.send(
        &ExternalBatchFrame::Manifest {
            binding,
            manifest: manifest.clone(),
        }
        .encode(),
    );
    let mut transcript = ExternalBatchTranscript::new(manifest.clone(), binding);
    let mut garbler_executor = if executor_role == crate::ExternalExecutor::Garbler {
        Some(GarblerBatchExecutor::new(
            manifest.clone(),
            binding,
            batch_action_registrations(schedule)?,
        )?)
    } else {
        None
    };

    for tables in exec.circuit.tables.chunks(STRICT_TABLE_CHUNK) {
        transport.send(
            &SessionFrame::SetupStrictChunk {
                tables: tables
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
    }
    transport.send(
        &SessionFrame::SetupStrictEnd {
            table_count: exec.circuit.tables.len() as u32,
        }
        .encode(),
    );

    for (call, spec) in schedule.actions.iter().enumerate() {
        let reveal = ExternalBatchFrame::decode(&transport.recv())
            .map_err(|_| MpcError::UnexpectedMessage)?;
        transcript
            .accept(&reveal)
            .map_err(|_| MpcError::UnexpectedMessage)?;
        let clear = decode_external_batch_action_reveal(full, &manifest, binding, call, &reveal)?;
        transcript
            .accept(&clear)
            .map_err(|_| MpcError::UnexpectedMessage)?;
        transport.send(&clear.encode());
        let result = match spec.execution.executor {
            crate::ExternalExecutor::Evaluator => ExternalBatchFrame::decode(&transport.recv())
                .map_err(|_| MpcError::UnexpectedMessage)?,
            crate::ExternalExecutor::Garbler => {
                let executor = garbler_executor
                    .as_mut()
                    .ok_or(MpcError::UnsupportedExternalPolicy)?;
                executor.accept_reveal(&reveal)?;
                let host = garbler_host
                    .as_deref_mut()
                    .ok_or(MpcError::UnsupportedExternalPolicy)?;
                let mut batch_host = LegacyBatchHost { host };
                let result = executor.execute_clear_inputs(&clear, &mut batch_host)?;
                transport.send(&result.encode());
                result
            }
        };
        transcript
            .accept(&result)
            .map_err(|_| MpcError::UnexpectedMessage)?;
        offer_external_batch_action_result_labels::<N, D>(
            full, &manifest, binding, call, &result, ot,
        )?;
        let ack = ExternalBatchFrame::decode(&transport.recv())
            .map_err(|_| MpcError::UnexpectedMessage)?;
        transcript
            .accept(&ack)
            .map_err(|_| MpcError::UnexpectedMessage)?;
        if let Some(executor) = garbler_executor.as_mut() {
            executor.accept_reinserted(&ack)?;
        }
        if !matches!(ack, ExternalBatchFrame::Reinserted { .. }) {
            return Err(MpcError::UnexpectedMessage);
        }
    }
    if !transcript.is_complete() {
        return Err(MpcError::MalformedSchedule);
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

    // Stream tables only after all input labels are available. The evaluator
    // consumes each chunk immediately, so it never materializes the full set.
    for tables in exec.circuit.tables.chunks(STRICT_TABLE_CHUNK) {
        transport.send(
            &SessionFrame::SetupStrictChunk {
                tables: tables
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
    }
    transport.send(
        &SessionFrame::SetupStrictEnd {
            table_count: exec.circuit.tables.len() as u32,
        }
        .encode(),
    );

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

/// Number of AND tables per strict streaming frame. This bounds table-frame
/// allocation while keeping transport framing amortized for large TLS rounds.
pub const STRICT_TABLE_CHUNK: usize = 256;

fn decode_table_chunk<N: VoleArray<u8>>(
    raw: Vec<[Vec<u8>; 4]>,
) -> Result<Vec<GarbleTable<N>>, MpcError> {
    raw.into_iter()
        .map(|rows| {
            let mut table: [Array<u8, N>; 4] = Default::default();
            for (row, bytes) in rows.iter().enumerate() {
                table[row] = vec_to_arr(bytes).ok_or(MpcError::MalformedSchedule)?;
            }
            Ok(GarbleTable { table })
        })
        .collect()
}

/// Evaluate a Not-free strict schedule as table chunks arrive. At most one
/// received chunk is retained; wires are the unavoidable live circuit state.
pub(crate) fn eval_strict_table_stream<N: VoleArray<u8>, D: Digest, T: Transport>(
    schedule: &GateSchedule,
    inputs: &[Eval<N>],
    transport: &mut T,
) -> Result<Vec<Eval<N>>, MpcError> {
    if inputs.len() != schedule.num_inputs
        || schedule.gates.iter().any(|g| {
            matches!(
                g,
                Gate::Not(_)
                    | Gate::One
                    | Gate::StorageRead { .. }
                    | Gate::StorageWrite { .. }
                    | Gate::ActionBit { .. }
            )
        })
    {
        return Err(MpcError::MalformedSchedule);
    }
    let mut wires = Vec::with_capacity(schedule.wire_count());
    wires.extend_from_slice(inputs);
    let mut gate_i = 0usize;
    let mut table_i = 0usize;
    let mut seen_tables = 0usize;
    loop {
        let frame = SessionFrame::decode(&transport.recv()).ok_or(MpcError::UnexpectedMessage)?;
        match frame {
            SessionFrame::SetupStrictChunk { tables } => {
                let tables = decode_table_chunk::<N>(tables)?;
                for table in tables {
                    while gate_i < schedule.gates.len()
                        && !matches!(schedule.gates[gate_i], Gate::And(..))
                    {
                        let out = match schedule.gates[gate_i] {
                            Gate::Zero => Eval::zero(),
                            Gate::Xor(a, b) => {
                                wires.get(a).cloned().ok_or(MpcError::MalformedSchedule)?
                                    ^ wires.get(b).cloned().ok_or(MpcError::MalformedSchedule)?
                            }
                            _ => return Err(MpcError::MalformedSchedule),
                        };
                        wires.push(out);
                        gate_i += 1;
                    }
                    let Gate::And(a, b) = schedule
                        .gates
                        .get(gate_i)
                        .copied()
                        .ok_or(MpcError::MalformedSchedule)?
                    else {
                        return Err(MpcError::MalformedSchedule);
                    };
                    let left = wires.get(a).ok_or(MpcError::MalformedSchedule)?;
                    let right = wires.get(b).ok_or(MpcError::MalformedSchedule)?;
                    wires.push(left.and_via_table::<D>(right, &table));
                    gate_i += 1;
                    table_i += 1;
                }
                seen_tables += table_i;
                table_i = 0;
            }
            SessionFrame::SetupStrictEnd { table_count } => {
                if seen_tables != table_count as usize || seen_tables != schedule.and_count() {
                    return Err(MpcError::MalformedSchedule);
                }
                while gate_i < schedule.gates.len() {
                    let out = match schedule.gates[gate_i] {
                        Gate::Zero => Eval::zero(),
                        Gate::Xor(a, b) => {
                            wires.get(a).cloned().ok_or(MpcError::MalformedSchedule)?
                                ^ wires.get(b).cloned().ok_or(MpcError::MalformedSchedule)?
                        }
                        _ => return Err(MpcError::MalformedSchedule),
                    };
                    wires.push(out);
                    gate_i += 1;
                }
                return schedule
                    .output_wires()
                    .iter()
                    .map(|&wire| wires.get(wire).cloned().ok_or(MpcError::MalformedSchedule))
                    .collect();
            }
            _ => return Err(MpcError::UnexpectedMessage),
        }
    }
}

/// Evaluate a strict table stream with a resumable cursor. Unlike
/// [`eval_strict_table_stream`], this supports GRAM and evaluator-hosted
/// actions, pausing only at AND gates until the next table arrives.
/// Evaluate a strict table stream while executing every action boundary over
/// versioned [`ExternalBatchFrame`] transport frames.
///
/// The caller has already computed the public session/circuit binding. The
/// evaluator sends its locally derived manifest first and rejects any garbler
/// manifest mismatch before it transports a label reveal. Each paused action
/// runs through `Reveal → ClearInputs → Result → Reinserted`; result bits are
/// reinserted by OT labels before evaluation resumes.
fn eval_strict_table_stream_batch_cursor<N: VoleArray<u8>, D: Digest, T: Transport>(
    schedule: &GateSchedule,
    inputs: &[Eval<N>],
    manifest: ExternalBatchManifest,
    binding: ExternalBatchBinding,
    transport: &mut T,
    ot: &mut dyn OtChannel<N>,
    host: &mut dyn StrictActionHost,
    gram: &mut [&mut dyn crate::GramDrive<N>],
) -> Result<Vec<Eval<N>>, MpcError> {
    let executor_role = homogeneous_batch_executor(schedule)?;
    let registrations = batch_action_registrations(schedule)?;
    let mut executor = EvaluatorBatchExecutor::new(manifest.clone(), binding, registrations)?;
    transport.send(
        &ExternalBatchFrame::Manifest {
            binding,
            manifest: manifest.clone(),
        }
        .encode(),
    );
    let peer_manifest =
        ExternalBatchFrame::decode(&transport.recv()).map_err(|_| MpcError::UnexpectedMessage)?;
    peer_manifest
        .validate_manifest(&manifest, binding)
        .map_err(|_| MpcError::UnexpectedMessage)?;

    let mut cursor = StrictGateCursor::new(schedule, inputs)?;
    let mut seen = 0usize;
    loop {
        match SessionFrame::decode(&transport.recv()).ok_or(MpcError::UnexpectedMessage)? {
            SessionFrame::SetupStrictChunk { tables } => {
                for table in decode_table_chunk::<N>(tables)? {
                    loop {
                        match cursor.advance::<D, T>(transport, ot, host, gram)? {
                            CursorState::NeedsExternalBoundary { call } => {
                                let reveal =
                                    cursor.action_batch_reveal(&manifest, binding, call)?;
                                executor.accept_reveal(&reveal)?;
                                transport.send(&reveal.encode());
                                let clear = ExternalBatchFrame::decode(&transport.recv())
                                    .map_err(|_| MpcError::UnexpectedMessage)?;
                                let result = match executor_role {
                                    crate::ExternalExecutor::Evaluator => {
                                        let mut batch_host = LegacyBatchHost { host };
                                        let result = executor
                                            .execute_clear_inputs(&clear, &mut batch_host)?;
                                        transport.send(&result.encode());
                                        result
                                    }
                                    crate::ExternalExecutor::Garbler => {
                                        executor.accept_garbler_clear_inputs(&clear)?;
                                        ExternalBatchFrame::decode(&transport.recv())
                                            .map_err(|_| MpcError::UnexpectedMessage)?
                                    }
                                };
                                if executor_role == crate::ExternalExecutor::Garbler {
                                    executor.accept_garbler_result(&result)?;
                                }
                                cursor.reinsert_batch_action_result(
                                    &manifest, binding, call, &result, ot,
                                )?;
                                let ack = ExternalBatchFrame::Reinserted {
                                    binding,
                                    request_id: match &result {
                                        ExternalBatchFrame::Result { request_id, .. } => {
                                            *request_id
                                        }
                                        _ => return Err(MpcError::MalformedSchedule),
                                    },
                                };
                                executor.accept_reinserted(&ack)?;
                                transport.send(&ack.encode());
                            }
                            CursorState::NeedsTable => break,
                            CursorState::Complete => return Err(MpcError::MalformedSchedule),
                        }
                    }
                    cursor.apply_table::<D>(&table)?;
                    seen += 1;
                }
            }
            SessionFrame::SetupStrictEnd { table_count } => {
                if seen != table_count as usize || seen != schedule.and_count() {
                    return Err(MpcError::MalformedSchedule);
                }
                loop {
                    match cursor.advance::<D, T>(transport, ot, host, gram)? {
                        CursorState::NeedsExternalBoundary { call } => {
                            let reveal = cursor.action_batch_reveal(&manifest, binding, call)?;
                            executor.accept_reveal(&reveal)?;
                            transport.send(&reveal.encode());
                            let clear = ExternalBatchFrame::decode(&transport.recv())
                                .map_err(|_| MpcError::UnexpectedMessage)?;
                            let result = match executor_role {
                                crate::ExternalExecutor::Evaluator => {
                                    let mut batch_host = LegacyBatchHost { host };
                                    let result =
                                        executor.execute_clear_inputs(&clear, &mut batch_host)?;
                                    transport.send(&result.encode());
                                    result
                                }
                                crate::ExternalExecutor::Garbler => {
                                    executor.accept_garbler_clear_inputs(&clear)?;
                                    ExternalBatchFrame::decode(&transport.recv())
                                        .map_err(|_| MpcError::UnexpectedMessage)?
                                }
                            };
                            if executor_role == crate::ExternalExecutor::Garbler {
                                executor.accept_garbler_result(&result)?;
                            }
                            cursor.reinsert_batch_action_result(
                                &manifest, binding, call, &result, ot,
                            )?;
                            let ack = ExternalBatchFrame::Reinserted {
                                binding,
                                request_id: match &result {
                                    ExternalBatchFrame::Result { request_id, .. } => *request_id,
                                    _ => return Err(MpcError::MalformedSchedule),
                                },
                            };
                            executor.accept_reinserted(&ack)?;
                            transport.send(&ack.encode());
                        }
                        CursorState::Complete => {
                            if !executor.is_complete() {
                                return Err(MpcError::MalformedSchedule);
                            }
                            return cursor.outputs();
                        }
                        CursorState::NeedsTable => return Err(MpcError::MalformedSchedule),
                    }
                }
            }
            _ => return Err(MpcError::UnexpectedMessage),
        }
    }
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

    // Consume the table stream incrementally. No delta/output base is needed.
    let out_labels = eval_strict_table_stream::<N, D, T>(schedule, &labels, transport)?;
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
    validate_batch_action_policy(schedule)?;
    crate::ExternalBatchManifest::from_actions(crate::ExternalBoundaryId(0), &schedule.actions)?;
    if partition.len() != schedule.num_inputs {
        return Err(MpcError::BadPartition);
    }
    if gram.len() != schedule.storages.len() {
        return Err(MpcError::MalformedSchedule);
    }

    let owned_frame = SessionFrame::decode(&transport.recv()).ok_or(MpcError::UnexpectedMessage)?;
    let owned: Vec<Eval<N>> = match owned_frame {
        SessionFrame::OwnedInputs(labels) => labels
            .iter()
            .map(|label| {
                Ok(Eval {
                    target: vec_to_arr(label).ok_or(MpcError::MalformedSchedule)?,
                })
            })
            .collect::<Result<_, MpcError>>()?,
        _ => return Err(MpcError::UnexpectedMessage),
    };
    let mut labels = Vec::with_capacity(schedule.num_inputs);
    let mut owned_i = 0usize;
    let mut eval_i = 0usize;
    for owner in partition {
        match owner {
            InputOwner::Public | InputOwner::Garbler => {
                labels.push(owned.get(owned_i).cloned().ok_or(MpcError::BadPartition)?);
                owned_i += 1;
            }
            InputOwner::Evaluator => {
                let bit = *evaluator_bits.get(eval_i).ok_or(MpcError::BadPartition)?;
                eval_i += 1;
                labels.push(Eval {
                    target: ot.receive(bit),
                });
            }
        }
    }
    if owned_i != owned.len() || eval_i != evaluator_bits.len() {
        return Err(MpcError::BadPartition);
    }
    let manifest =
        ExternalBatchManifest::from_actions(crate::ExternalBoundaryId(0), &schedule.actions)?;
    let binding = manifest.bind([0; 32], [0; 32]);
    let out_labels = eval_strict_table_stream_batch_cursor::<N, D, T>(
        schedule, &labels, manifest, binding, transport, ot, host, gram,
    )?;
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
