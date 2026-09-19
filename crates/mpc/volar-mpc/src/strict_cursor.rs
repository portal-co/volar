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
    ActionSpec, Gate, GateSchedule, GramDrive, MpcError, OtChannel, SessionFrame, Transport,
    arr_to_vec, gram_data_base,
};

/// Result of advancing [`StrictGateCursor`] through free/interactive gates.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CursorState {
    /// The next gate is an AND and requires one streamed garbled table.
    NeedsTable,
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

    /// Advance all gates that do not require an AND table. Storage and action
    /// interactions are performed at their original schedule position.
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
            let spec: &ActionSpec = self
                .schedule
                .actions
                .get(call)
                .ok_or(MpcError::MalformedSchedule)?;
            let mut labels =
                Vec::with_capacity(1 + spec.arg_wires.len() + spec.fallback_wires.len());
            labels.push(arr_to_vec(&self.wire(spec.guard)?.target));
            for &wire in spec.arg_wires.iter().chain(spec.fallback_wires.iter()) {
                labels.push(arr_to_vec(&self.wire(wire)?.target));
            }
            transport.send(
                &SessionFrame::ActionArgs {
                    call: call as u32,
                    request_id: spec.request_id,
                    labels,
                }
                .encode(),
            );
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
            self.call_results[call] = Some(
                result
                    .into_iter()
                    .map(|bit| Eval {
                        target: ot.receive(bit),
                    })
                    .collect(),
            );
        }
        self.call_results[call]
            .as_ref()
            .and_then(|bits| bits.get(bit))
            .cloned()
            .ok_or(MpcError::MalformedSchedule)
    }
}
