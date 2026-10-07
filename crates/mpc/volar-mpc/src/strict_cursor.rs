//! Resumable evaluator gate cursor for streamed strict garbling.
//!
//! A cursor owns only live evaluator labels and its gate/table positions.  It
//! advances free gates, GRAM operations, and evaluator-hosted actions until it
//! needs exactly one AND table; callers may therefore feed it bounded table
//! chunks without materializing an entire garbling.

use alloc::vec::Vec;

use digest::Digest;
use volar_spec::garble::{Eval, GarbleTable};
use volar_spec::vole::VoleArray;

use crate::strict::StrictActionHost;
use crate::{
    ActionSpec, ExternalBatchBinding, ExternalBatchFrame, ExternalBatchManifest, Gate,
    GateSchedule, GramDrive, MpcError, OtChannel, SessionFrame, Transport, arr_to_vec,
    gram_data_base,
};

/// Result of advancing [`StrictGateCursor`] through free/interactive gates.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CursorState {
    /// The next gate is an AND and requires one streamed garbled table.
    NeedsTable,
    /// Pure gate evaluation has reached an external action boundary. The
    /// caller must execute/reinsert this call, then resume the cursor. This
    /// is deliberately a public call-table index; the strict adapter binds it
    /// to `ActionSpec::request_id` before accepting action frames.
    NeedsExternalBoundary { call: u32 },
    /// All gates have executed.
    Complete,
}

/// Resumable state for strict evaluation, including storage and action gates.
pub struct StrictGateCursor<'a, N: VoleArray<u8>> {
    schedule: &'a GateSchedule,
    wires: Vec<Eval<N>>,
    gate: usize,
    call_results: Vec<Option<Vec<Eval<N>>>>,
}

impl<'a, N: VoleArray<u8>> StrictGateCursor<'a, N> {
    /// Begin at the first gate with evaluator input labels already installed.
    pub fn new(schedule: &'a GateSchedule, inputs: &[Eval<N>]) -> Result<Self, MpcError> {
        if inputs.len() != schedule.num_inputs {
            return Err(MpcError::BadPartition);
        }
        Ok(Self {
            schedule,
            wires: inputs.to_vec(),
            gate: 0,
            call_results: (0..schedule.actions.len()).map(|_| None).collect(),
        })
    }

    /// Advance pure gates until an AND table, an external action boundary, or
    /// completion. Storage remains an existing strict-chain interaction;
    /// actions pause rather than performing transport I/O in this cursor.
    pub fn advance<D: Digest, T: Transport>(
        &mut self,
        transport: &mut T,
        ot: &mut dyn OtChannel<N>,
        host: &mut dyn StrictActionHost,
        gram: &mut [&mut dyn GramDrive<N>],
    ) -> Result<CursorState, MpcError> {
        loop {
            let Some(gate) = self.schedule.gates.get(self.gate).copied() else {
                return Ok(CursorState::Complete);
            };
            if let Gate::ActionBit { call, .. } = gate
                && self
                    .call_results
                    .get(call as usize)
                    .and_then(|result| result.as_ref())
                    .is_none()
            {
                return Ok(CursorState::NeedsExternalBoundary { call });
            }
            let out = match gate {
                Gate::And(..) => return Ok(CursorState::NeedsTable),
                Gate::Zero => Eval::zero(),
                Gate::One | Gate::Not(_) => return Err(MpcError::MalformedSchedule),
                Gate::Xor(a, b) => self.wire(a)? ^ self.wire(b)?,
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
                    access,
                    src,
                } => {
                    let value = self.wire(src)?;
                    gram.get_mut(storage)
                        .ok_or(MpcError::MalformedSchedule)?
                        .write(cell, access, &value)?;
                    Eval::zero()
                }
                Gate::ActionBit { call, bit } => {
                    self.action_bit::<T>(call as usize, bit as usize, transport, ot, host)?
                }
            };
            self.wires.push(out);
            self.gate += 1;
        }
    }

    /// Execute the compatibility evaluator-hosted action at a paused
    /// boundary. This retains the old per-call frames temporarily; the caller
    /// owns the pause/resume point and a future batch adapter can replace this
    /// method without changing cursor gate traversal.
    pub fn execute_legacy_action<T: Transport>(
        &mut self,
        call: u32,
        transport: &mut T,
        ot: &mut dyn OtChannel<N>,
        host: &mut dyn StrictActionHost,
    ) -> Result<(), MpcError> {
        self.action_bit(call as usize, 0, transport, ot, host)
            .map(|_| ())
    }

    /// Prepare evaluator-held labels for one paused action reveal.
    ///
    /// The caller transports these labels to the garbler action adapter. No
    /// logical value is decoded locally; the request ID binds the label vector
    /// to its exact source action occurrence.
    pub fn action_reveal_labels(&self, call: u32) -> Result<(u64, Vec<Vec<u8>>), MpcError> {
        let spec = self
            .schedule
            .actions
            .get(call as usize)
            .ok_or(MpcError::MalformedSchedule)?;
        crate::strict::validate_batch_action_spec(spec)?;
        if self
            .call_results
            .get(call as usize)
            .and_then(|result| result.as_ref())
            .is_some()
        {
            return Err(MpcError::MalformedSchedule);
        }
        let mut labels = Vec::with_capacity(1 + spec.arg_wires.len() + spec.fallback_wires.len());
        labels.push(arr_to_vec(&self.wire(spec.guard)?.target));
        for &wire in spec.arg_wires.iter().chain(spec.fallback_wires.iter()) {
            labels.push(arr_to_vec(&self.wire(wire)?.target));
        }
        Ok((spec.request_id, labels))
    }

    /// Build a versioned batch reveal frame for the next paused action.
    pub fn action_batch_reveal(
        &self,
        manifest: &ExternalBatchManifest,
        binding: ExternalBatchBinding,
        call: u32,
    ) -> Result<ExternalBatchFrame, MpcError> {
        manifest
            .validate()
            .map_err(|_| MpcError::MalformedSchedule)?;
        let (request_id, labels) = self.action_reveal_labels(call)?;
        let spec = self
            .schedule
            .actions
            .get(call as usize)
            .ok_or(MpcError::MalformedSchedule)?;
        let entry = manifest
            .actions
            .iter()
            .find(|entry| entry.request_id == request_id)
            .ok_or(MpcError::UnexpectedMessage)?;
        if entry.execution != spec.execution || entry.output_bits != spec.num_bits {
            return Err(MpcError::UnexpectedMessage);
        }
        Ok(ExternalBatchFrame::Reveal {
            binding,
            request_id,
            labels,
        })
    }

    /// Reinsert one evaluator-selected action result through OT labels.
    ///
    /// The supplied clear bits are used only as OT choices; the cursor stores
    /// the returned labels, never the bits. The corresponding garbler adapter
    /// must have offered result pairs derived from this request's pinned bases.
    pub fn reinsert_action_result(
        &mut self,
        call: u32,
        result_bits: &[bool],
        ot: &mut dyn OtChannel<N>,
    ) -> Result<(), MpcError> {
        let call = call as usize;
        if self
            .call_results
            .get(call)
            .and_then(|result| result.as_ref())
            .is_some()
        {
            return Err(MpcError::MalformedSchedule);
        }
        let spec = self
            .schedule
            .actions
            .get(call)
            .ok_or(MpcError::MalformedSchedule)?;
        crate::strict::validate_batch_action_spec(spec)?;
        if result_bits.len() != spec.num_bits {
            return Err(MpcError::ActionHost);
        }
        self.call_results[call] = Some(
            result_bits
                .iter()
                .map(|bit| Eval {
                    target: ot.receive(*bit),
                })
                .collect(),
        );
        Ok(())
    }

    /// Reinsert a validated versioned batch result through request-bound OT
    /// labels. The caller must have received the result label pairs from the
    /// matching garbler action adapter first.
    pub fn reinsert_batch_action_result(
        &mut self,
        manifest: &ExternalBatchManifest,
        binding: ExternalBatchBinding,
        call: u32,
        frame: &ExternalBatchFrame,
        ot: &mut dyn OtChannel<N>,
    ) -> Result<(), MpcError> {
        manifest
            .validate()
            .map_err(|_| MpcError::MalformedSchedule)?;
        frame
            .validate_request(manifest, binding)
            .map_err(|_| MpcError::UnexpectedMessage)?;
        let ExternalBatchFrame::Result {
            binding: received_binding,
            request_id,
            bits,
        } = frame
        else {
            return Err(MpcError::UnexpectedMessage);
        };
        let spec = self
            .schedule
            .actions
            .get(call as usize)
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
        self.reinsert_action_result(call, bits, ot)
    }

    /// Consume exactly one table for the pending AND gate.
    pub fn apply_table<D: Digest>(&mut self, table: &GarbleTable<N>) -> Result<(), MpcError> {
        let Gate::And(a, b) = self
            .schedule
            .gates
            .get(self.gate)
            .copied()
            .ok_or(MpcError::MalformedSchedule)?
        else {
            return Err(MpcError::MalformedSchedule);
        };
        let out = self.wire(a)?.and_via_table::<D>(&self.wire(b)?, table);
        self.wires.push(out);
        self.gate += 1;
        Ok(())
    }

    /// Return the evaluator labels for the schedule's declared outputs.
    pub fn outputs(&self) -> Result<Vec<Eval<N>>, MpcError> {
        if self.gate != self.schedule.gates.len() {
            return Err(MpcError::MalformedSchedule);
        }
        self.schedule
            .output_wires()
            .iter()
            .map(|&wire| self.wire(wire))
            .collect()
    }

    fn wire(&self, wire: usize) -> Result<Eval<N>, MpcError> {
        self.wires
            .get(wire)
            .cloned()
            .ok_or(MpcError::MalformedSchedule)
    }

    fn action_bit<T: Transport>(
        &mut self,
        call: usize,
        bit: usize,
        transport: &mut T,
        ot: &mut dyn OtChannel<N>,
        host: &mut dyn StrictActionHost,
    ) -> Result<Eval<N>, MpcError> {
        if self
            .call_results
            .get(call)
            .and_then(|r| r.as_ref())
            .is_none()
        {
            let (request_id, labels) = self.action_reveal_labels(call as u32)?;
            transport.send(
                &SessionFrame::ActionArgs {
                    call: call as u32,
                    request_id,
                    labels,
                }
                .encode(),
            );
            let spec: &ActionSpec = self
                .schedule
                .actions
                .get(call)
                .ok_or(MpcError::MalformedSchedule)?;
            let bits =
                match SessionFrame::decode(&transport.recv()).ok_or(MpcError::UnexpectedMessage)? {
                    SessionFrame::ActionArgsClear {
                        call: got,
                        request_id,
                        bits,
                    } if got as usize == call && request_id == spec.request_id => bits,
                    _ => return Err(MpcError::UnexpectedMessage),
                };
            if bits.len() != 1 + spec.arg_wires.len() + spec.fallback_wires.len() {
                return Err(MpcError::UnexpectedMessage);
            }
            let result = if bits[0] {
                let result = host.action(&spec.name, &bits[1..1 + spec.arg_wires.len()])?;
                if result.len() != spec.num_bits {
                    return Err(MpcError::ActionHost);
                }
                result
            } else {
                let fallback = bits[1 + spec.arg_wires.len()..].to_vec();
                if fallback.len() != spec.num_bits {
                    return Err(MpcError::MalformedSchedule);
                }
                fallback
            };
            self.reinsert_action_result(call as u32, &result, ot)?;
        }
        self.call_results[call]
            .as_ref()
            .and_then(|bits| bits.get(bit))
            .cloned()
            .ok_or(MpcError::MalformedSchedule)
    }
}
