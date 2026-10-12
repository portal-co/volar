//! Manifest-bound lockstep scheduling for main-branch MPC.
//!
//! The cursor in this module is deliberately protocol-neutral: it validates
//! the public schedule and the epoch/iteration/frame order, while the existing
//! garbled-circuit and GRAM executors remain responsible for payload semantics.
//! Both protocol roles should own one cursor and feed it the same frames.

use alloc::vec::Vec;
use core::fmt;

use volar_ir_common::{EpochId, IterationId, ManifestError, ScheduleLimits, ScheduleManifest};

use crate::{Gate, GateSchedule, MpcError, SessionFrame, Transport};

/// Public phase carried by a lockstep schedule frame.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MpcSchedulePhase {
    /// Open one consistency epoch.
    EpochOpen,
    /// Materialize one canonical iteration.
    Iteration,
    /// Commit all iterations in the current epoch.
    EpochCommit,
    /// Acknowledge the committed epoch on the opposite role.
    EpochAck,
}

/// The role's position in the public lockstep envelope.
///
/// The garbler emits epoch-open, iteration, and commit frames and receives the
/// evaluator's acknowledgement. The evaluator mirrors that order and sends the
/// acknowledgement. Payload protocol frames run only after the corresponding
/// iteration frame has been exchanged by both roles.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MpcScheduleRole {
    /// The role that initiates epoch and iteration frames.
    Garbler,
    /// The role that validates those frames and acknowledges each epoch.
    Evaluator,
}

/// One public schedule frame exchanged by a lockstep MPC adapter.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MpcScheduleFrame {
    /// Session identity; it prevents cross-session frame reuse.
    pub session_id: u64,
    /// Digest of the canonical [`ScheduleManifest`].
    pub schedule_digest: [u8; 32],
    /// Epoch named by this frame.
    pub epoch: EpochId,
    /// Iteration named by an [`MpcSchedulePhase::Iteration`] frame.
    pub iteration: Option<IterationId>,
    /// Frame protocol phase.
    pub phase: MpcSchedulePhase,
    /// Strictly increasing logical frame ordinal.
    pub message_ordinal: u64,
    /// Adapter-owned bounded payload. The cursor does not interpret it.
    pub payload: Vec<u8>,
}

impl MpcScheduleFrame {
    /// Construct a frame with an owned payload.
    pub fn new(
        session_id: u64,
        schedule_digest: [u8; 32],
        epoch: EpochId,
        iteration: Option<IterationId>,
        phase: MpcSchedulePhase,
        message_ordinal: u64,
        payload: Vec<u8>,
    ) -> Self {
        Self {
            session_id,
            schedule_digest,
            epoch,
            iteration,
            phase,
            message_ordinal,
            payload,
        }
    }

    /// Construct an epoch-open frame.
    pub fn epoch_open(
        session_id: u64,
        schedule_digest: [u8; 32],
        epoch: EpochId,
        message_ordinal: u64,
    ) -> Self {
        Self::new(
            session_id,
            schedule_digest,
            epoch,
            None,
            MpcSchedulePhase::EpochOpen,
            message_ordinal,
            Vec::new(),
        )
    }

    /// Construct an iteration frame.
    pub fn iteration(
        session_id: u64,
        schedule_digest: [u8; 32],
        epoch: EpochId,
        iteration: IterationId,
        message_ordinal: u64,
        payload: Vec<u8>,
    ) -> Self {
        Self::new(
            session_id,
            schedule_digest,
            epoch,
            Some(iteration),
            MpcSchedulePhase::Iteration,
            message_ordinal,
            payload,
        )
    }

    /// Construct an epoch-commit frame.
    pub fn epoch_commit(
        session_id: u64,
        schedule_digest: [u8; 32],
        epoch: EpochId,
        message_ordinal: u64,
    ) -> Self {
        Self::new(
            session_id,
            schedule_digest,
            epoch,
            None,
            MpcSchedulePhase::EpochCommit,
            message_ordinal,
            Vec::new(),
        )
    }

    /// Construct an epoch-ack frame.
    pub fn epoch_ack(
        session_id: u64,
        schedule_digest: [u8; 32],
        epoch: EpochId,
        message_ordinal: u64,
    ) -> Self {
        Self::new(
            session_id,
            schedule_digest,
            epoch,
            None,
            MpcSchedulePhase::EpochAck,
            message_ordinal,
            Vec::new(),
        )
    }
}

/// Fail-closed errors from manifest or lockstep-frame validation.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MpcScheduleError {
    /// The public manifest is malformed or exceeds adapter limits.
    Manifest(ManifestError),
    /// The frame belongs to another session.
    SessionMismatch { expected: u64, found: u64 },
    /// The frame is bound to another schedule.
    DigestMismatch,
    /// The frame phase is not the next phase required by the cursor.
    UnexpectedPhase {
        expected: MpcSchedulePhase,
        found: MpcSchedulePhase,
    },
    /// The frame names another epoch.
    UnexpectedEpoch { expected: EpochId, found: EpochId },
    /// The frame names another iteration (or supplies one when none is valid).
    UnexpectedIteration {
        expected: Option<IterationId>,
        found: Option<IterationId>,
    },
    /// The frame ordinal is not the next logical message ordinal.
    UnexpectedMessageOrdinal { expected: u64, found: u64 },
    /// A gate schedule contains an invalid wire or table reference.
    MalformedGateSchedule,
    /// The cursor was asked to finish before all acknowledgements arrived.
    Incomplete,
    /// A frame was supplied after the final acknowledgement.
    Complete,
}

impl fmt::Display for MpcScheduleError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Manifest(error) => write!(f, "invalid MPC schedule manifest: {error}"),
            Self::SessionMismatch { expected, found } => {
                write!(f, "MPC schedule session {found} does not match {expected}")
            }
            Self::DigestMismatch => f.write_str("MPC schedule digest mismatch"),
            Self::UnexpectedPhase { expected, found } => {
                write!(f, "MPC schedule expected {expected:?}, found {found:?}")
            }
            Self::UnexpectedEpoch { expected, found } => {
                write!(
                    f,
                    "MPC schedule expected epoch {expected:?}, found {found:?}"
                )
            }
            Self::UnexpectedIteration { expected, found } => {
                write!(
                    f,
                    "MPC schedule expected iteration {expected:?}, found {found:?}"
                )
            }
            Self::UnexpectedMessageOrdinal { expected, found } => {
                write!(f, "MPC schedule expected message {expected}, found {found}")
            }
            Self::MalformedGateSchedule => f.write_str("malformed MPC gate schedule"),
            Self::Incomplete => f.write_str("MPC schedule ended before epoch acknowledgement"),
            Self::Complete => f.write_str("MPC schedule cursor is already complete"),
        }
    }
}

impl From<ManifestError> for MpcScheduleError {
    fn from(error: ManifestError) -> Self {
        Self::Manifest(error)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum CursorStage {
    Open,
    Iteration,
    Commit,
    Ack,
    Complete,
}

/// Deterministic lockstep cursor for one MPC protocol role.
#[derive(Clone, Debug)]
pub struct MpcScheduleCursor {
    session_id: u64,
    schedule_digest: [u8; 32],
    manifest: ScheduleManifest,
    epoch_index: usize,
    iteration_index: usize,
    next_message: u64,
    stage: CursorStage,
    accepted_iterations: usize,
}

impl MpcScheduleCursor {
    /// Validate a manifest and create a cursor using default resource limits.
    pub fn new(
        manifest: &ScheduleManifest,
        session_id: u64,
        schedule_digest: [u8; 32],
    ) -> Result<Self, MpcScheduleError> {
        Self::new_with_limits(
            manifest,
            ScheduleLimits::default(),
            session_id,
            schedule_digest,
        )
    }

    /// Validate a manifest and create a cursor with explicit resource limits.
    pub fn new_with_limits(
        manifest: &ScheduleManifest,
        limits: ScheduleLimits,
        session_id: u64,
        schedule_digest: [u8; 32],
    ) -> Result<Self, MpcScheduleError> {
        manifest.validate(limits)?;
        let stage = if manifest.epochs.is_empty() {
            CursorStage::Complete
        } else {
            CursorStage::Open
        };
        Ok(Self {
            session_id,
            schedule_digest,
            manifest: manifest.clone(),
            epoch_index: 0,
            iteration_index: 0,
            next_message: 0,
            stage,
            accepted_iterations: 0,
        })
    }

    /// The next frame shape required by this role, without a payload.
    pub fn next_frame(&self) -> Option<MpcScheduleFrame> {
        let epoch = self.manifest.epochs.get(self.epoch_index)?.ordinal;
        let (phase, iteration) = match self.stage {
            CursorStage::Open => (MpcSchedulePhase::EpochOpen, None),
            CursorStage::Iteration => (
                MpcSchedulePhase::Iteration,
                self.manifest.epochs[self.epoch_index]
                    .iterations
                    .get(self.iteration_index)
                    .map(|iteration| iteration.ordinal),
            ),
            CursorStage::Commit => (MpcSchedulePhase::EpochCommit, None),
            CursorStage::Ack => (MpcSchedulePhase::EpochAck, None),
            CursorStage::Complete => return None,
        };
        Some(MpcScheduleFrame::new(
            self.session_id,
            self.schedule_digest,
            epoch,
            iteration,
            phase,
            self.next_message,
            Vec::new(),
        ))
    }

    /// Accept exactly the next public frame and advance the cursor.
    pub fn accept(&mut self, frame: &MpcScheduleFrame) -> Result<(), MpcScheduleError> {
        if self.stage == CursorStage::Complete {
            return Err(MpcScheduleError::Complete);
        }
        if frame.session_id != self.session_id {
            return Err(MpcScheduleError::SessionMismatch {
                expected: self.session_id,
                found: frame.session_id,
            });
        }
        if frame.schedule_digest != self.schedule_digest {
            return Err(MpcScheduleError::DigestMismatch);
        }
        if frame.message_ordinal != self.next_message {
            return Err(MpcScheduleError::UnexpectedMessageOrdinal {
                expected: self.next_message,
                found: frame.message_ordinal,
            });
        }
        let expected_epoch = self.manifest.epochs[self.epoch_index].ordinal;
        if frame.epoch != expected_epoch {
            return Err(MpcScheduleError::UnexpectedEpoch {
                expected: expected_epoch,
                found: frame.epoch,
            });
        }
        let expected_phase = match self.stage {
            CursorStage::Open => MpcSchedulePhase::EpochOpen,
            CursorStage::Iteration => MpcSchedulePhase::Iteration,
            CursorStage::Commit => MpcSchedulePhase::EpochCommit,
            CursorStage::Ack => MpcSchedulePhase::EpochAck,
            CursorStage::Complete => unreachable!(),
        };
        if frame.phase != expected_phase {
            return Err(MpcScheduleError::UnexpectedPhase {
                expected: expected_phase,
                found: frame.phase,
            });
        }
        match self.stage {
            CursorStage::Open => {
                self.require_iteration(None, frame.iteration)?;
                self.stage = if self.manifest.epochs[self.epoch_index].iterations.is_empty() {
                    CursorStage::Commit
                } else {
                    CursorStage::Iteration
                };
            }
            CursorStage::Iteration => {
                let expected = self.manifest.epochs[self.epoch_index]
                    .iterations
                    .get(self.iteration_index)
                    .map(|iteration| iteration.ordinal);
                self.require_iteration(expected, frame.iteration)?;
                self.iteration_index += 1;
                self.accepted_iterations += 1;
                if self.iteration_index == self.manifest.epochs[self.epoch_index].iterations.len() {
                    self.stage = CursorStage::Commit;
                }
            }
            CursorStage::Commit => {
                self.require_iteration(None, frame.iteration)?;
                self.stage = CursorStage::Ack;
            }
            CursorStage::Ack => {
                self.require_iteration(None, frame.iteration)?;
                self.epoch_index += 1;
                self.iteration_index = 0;
                self.stage = if self.epoch_index == self.manifest.epochs.len() {
                    CursorStage::Complete
                } else {
                    CursorStage::Open
                };
            }
            CursorStage::Complete => unreachable!(),
        }
        self.next_message =
            self.next_message
                .checked_add(1)
                .ok_or(MpcScheduleError::UnexpectedMessageOrdinal {
                    expected: u64::MAX,
                    found: u64::MAX,
                })?;
        Ok(())
    }

    /// Encode, locally accept, and send the next schedule frame.
    pub fn send_next<T: Transport>(
        &mut self,
        transport: &mut T,
    ) -> Result<MpcScheduleFrame, MpcError> {
        let frame = self.next_frame().ok_or(MpcError::MalformedSchedule)?;
        self.accept(&frame)
            .map_err(|_| MpcError::MalformedSchedule)?;
        transport.send(&SessionFrame::Schedule(frame.clone()).encode());
        Ok(frame)
    }

    /// Receive and accept the next schedule frame from the counterparty.
    pub fn receive_next<T: Transport>(
        &mut self,
        transport: &mut T,
    ) -> Result<MpcScheduleFrame, MpcError> {
        let frame = match SessionFrame::decode(&transport.recv()) {
            Some(SessionFrame::Schedule(frame)) => frame,
            _ => return Err(MpcError::UnexpectedMessage),
        };
        self.accept(&frame)
            .map_err(|_| MpcError::UnexpectedMessage)?;
        Ok(frame)
    }

    /// Drive the complete manifest in lockstep, invoking `iteration` once for
    /// every accepted iteration frame.
    ///
    /// The callback receives the canonical, locally validated iteration frame
    /// after that frame has crossed the transport. Returning an error aborts
    /// the schedule before any later iteration or epoch boundary is admitted.
    /// The return value is the number of payload iterations executed.
    pub fn drive<T: Transport>(
        &mut self,
        role: MpcScheduleRole,
        transport: &mut T,
        mut iteration: impl FnMut(&MpcScheduleFrame) -> Result<(), MpcError>,
    ) -> Result<usize, MpcError> {
        let mut executed = 0usize;
        while !self.is_complete() {
            let expected = self.next_frame().ok_or(MpcError::MalformedSchedule)?;
            let frame = match (role, expected.phase) {
                (MpcScheduleRole::Garbler, MpcSchedulePhase::EpochAck) => {
                    self.receive_next(transport)?
                }
                (MpcScheduleRole::Evaluator, MpcSchedulePhase::EpochAck) => {
                    self.send_next(transport)?
                }
                (MpcScheduleRole::Garbler, _) => self.send_next(transport)?,
                (MpcScheduleRole::Evaluator, _) => self.receive_next(transport)?,
            };
            if frame.phase == MpcSchedulePhase::Iteration {
                iteration(&frame)?;
                executed += 1;
            }
        }
        self.finish().map_err(|_| MpcError::MalformedSchedule)?;
        if executed != self.accepted_iterations() {
            return Err(MpcError::MalformedSchedule);
        }
        Ok(executed)
    }

    fn require_iteration(
        &self,
        expected: Option<IterationId>,
        found: Option<IterationId>,
    ) -> Result<(), MpcScheduleError> {
        if expected == found {
            Ok(())
        } else {
            Err(MpcScheduleError::UnexpectedIteration { expected, found })
        }
    }

    /// Require that all epoch acknowledgements have arrived.
    pub fn finish(&self) -> Result<(), MpcScheduleError> {
        if self.is_complete() {
            Ok(())
        } else {
            Err(MpcScheduleError::Incomplete)
        }
    }

    /// Whether this role has acknowledged every manifest epoch.
    pub const fn is_complete(&self) -> bool {
        matches!(self.stage, CursorStage::Complete)
    }

    /// Number of accepted iteration frames.
    pub const fn accepted_iterations(&self) -> usize {
        self.accepted_iterations
    }

    /// The manifest digest this cursor accepts.
    pub const fn schedule_digest(&self) -> [u8; 32] {
        self.schedule_digest
    }
}

/// Validate gate/table/wire references before starting a manifest-bound run.
///
/// This is intentionally separate from the cursor: it checks the local
/// `GateSchedule` shape, while the cursor checks public cross-role order.
pub fn validate_gate_schedule_binding(
    schedule: &GateSchedule,
    manifest: &ScheduleManifest,
) -> Result<(), MpcScheduleError> {
    manifest.validate(ScheduleLimits::default())?;
    if schedule.output >= schedule.wire_count()
        || schedule
            .outputs
            .as_ref()
            .is_some_and(|outputs| outputs.iter().any(|&wire| wire >= schedule.wire_count()))
    {
        return Err(MpcScheduleError::MalformedGateSchedule);
    }
    for (index, gate) in schedule.gates.iter().enumerate() {
        let output = schedule.num_inputs + index;
        let valid_wire = |wire: usize| wire < output;
        let valid = match *gate {
            Gate::Zero | Gate::One => true,
            Gate::Xor(a, b) | Gate::And(a, b) => valid_wire(a) && valid_wire(b),
            Gate::Not(a) => valid_wire(a),
            Gate::StorageRead { storage, .. } => storage < schedule.storages.len(),
            Gate::StorageWrite { storage, src, .. } => {
                storage < schedule.storages.len() && valid_wire(src)
            }
            Gate::ActionBit { call, bit } => schedule
                .actions
                .get(call as usize)
                .is_some_and(|action| (bit as usize) < action.num_bits),
        };
        if !valid {
            return Err(MpcScheduleError::MalformedGateSchedule);
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use alloc::{collections::VecDeque, vec, vec::Vec};

    use super::*;

    #[derive(Default)]
    struct Queue(VecDeque<Vec<u8>>);

    impl Transport for Queue {
        fn send(&mut self, frame: &[u8]) {
            self.0.push_back(frame.to_vec());
        }

        fn recv(&mut self) -> Vec<u8> {
            self.0.pop_front().expect("test queue frame")
        }
    }

    #[derive(Default)]
    struct ScriptedTransport {
        inbound: VecDeque<Vec<u8>>,
        sent: Vec<Vec<u8>>,
    }

    impl Transport for ScriptedTransport {
        fn send(&mut self, frame: &[u8]) {
            self.sent.push(frame.to_vec());
        }

        fn recv(&mut self) -> Vec<u8> {
            self.inbound.pop_front().expect("scripted inbound frame")
        }
    }
    use volar_ir_common::{CommunicationGroup, EpochManifest, IterationManifest, WorkerLane};

    fn manifest() -> ScheduleManifest {
        let mut epoch = EpochManifest::new(EpochId(0), CommunicationGroup(2));
        epoch
            .iterations
            .push(IterationManifest::new(IterationId(0), 10, WorkerLane(0)));
        epoch
            .iterations
            .push(IterationManifest::new(IterationId(1), 11, WorkerLane(1)));
        let mut manifest = ScheduleManifest::new([1; 32], [2; 32], 7, 2, 2);
        manifest.epochs.push(epoch);
        manifest
    }

    fn multi_epoch_manifest() -> ScheduleManifest {
        let mut first = EpochManifest::new(EpochId(0), CommunicationGroup(0));
        first
            .iterations
            .push(IterationManifest::new(IterationId(0), 10, WorkerLane(0)));
        first
            .iterations
            .push(IterationManifest::new(IterationId(1), 11, WorkerLane(1)));
        let mut second = EpochManifest::new(EpochId(1), CommunicationGroup(1));
        second
            .iterations
            .push(IterationManifest::new(IterationId(2), 12, WorkerLane(0)));
        let mut manifest = ScheduleManifest::new([1; 32], [2; 32], 7, 3, 2);
        manifest.epochs.push(first);
        manifest.epochs.push(second);
        manifest
    }

    fn canonical_frames(
        manifest: &ScheduleManifest,
        session_id: u64,
        digest: [u8; 32],
    ) -> Vec<MpcScheduleFrame> {
        let mut cursor = MpcScheduleCursor::new(manifest, session_id, digest).unwrap();
        let mut frames = Vec::new();
        while let Some(frame) = cursor.next_frame() {
            cursor.accept(&frame).unwrap();
            frames.push(frame);
        }
        frames
    }

    fn run_frames(cursor: &mut MpcScheduleCursor) {
        while let Some(expected) = cursor.next_frame() {
            cursor.accept(&expected).unwrap();
        }
        cursor.finish().unwrap();
    }

    #[test]
    fn cursor_can_bind_schedule_frames_to_transport_order() {
        let mut manifest = manifest();
        manifest.epochs[0].iterations.truncate(1);
        let mut sender = MpcScheduleCursor::new(&manifest, 44, [9; 32]).unwrap();
        let mut receiver = MpcScheduleCursor::new(&manifest, 44, [9; 32]).unwrap();
        let mut wire = Queue::default();

        sender.send_next(&mut wire).unwrap();
        receiver.receive_next(&mut wire).unwrap();
        sender.send_next(&mut wire).unwrap();
        receiver.receive_next(&mut wire).unwrap();
        sender.send_next(&mut wire).unwrap();
        receiver.receive_next(&mut wire).unwrap();
        receiver.send_next(&mut wire).unwrap();
        sender.receive_next(&mut wire).unwrap();

        assert!(sender.is_complete());
        assert!(receiver.is_complete());
        sender.finish().unwrap();
        receiver.finish().unwrap();
    }

    #[test]
    fn drive_runs_multi_epoch_iterations_in_manifest_order() {
        let manifest = multi_epoch_manifest();
        let digest = [7; 32];
        let frames = canonical_frames(&manifest, 44, digest);
        let inbound_acks = frames
            .iter()
            .filter(|frame| frame.phase == MpcSchedulePhase::EpochAck)
            .map(|frame| SessionFrame::Schedule(frame.clone()).encode())
            .collect();
        let mut garbler_wire = ScriptedTransport {
            inbound: inbound_acks,
            sent: Vec::new(),
        };
        let mut garbler = MpcScheduleCursor::new(&manifest, 44, digest).unwrap();
        let mut garbler_iterations = Vec::new();
        let executed = garbler
            .drive(MpcScheduleRole::Garbler, &mut garbler_wire, |frame| {
                garbler_iterations.push(frame.iteration.unwrap());
                Ok(())
            })
            .unwrap();
        assert_eq!(executed, 3);
        assert_eq!(
            garbler_iterations,
            vec![IterationId(0), IterationId(1), IterationId(2)]
        );
        let sent: Vec<MpcScheduleFrame> = garbler_wire
            .sent
            .iter()
            .map(|frame| match SessionFrame::decode(frame).unwrap() {
                SessionFrame::Schedule(frame) => frame,
                _ => panic!("driver sent a non-schedule frame"),
            })
            .collect();
        assert_eq!(
            sent,
            frames
                .iter()
                .filter(|frame| frame.phase != MpcSchedulePhase::EpochAck)
                .cloned()
                .collect::<Vec<_>>()
        );

        let inbound_payload = frames
            .iter()
            .filter(|frame| frame.phase != MpcSchedulePhase::EpochAck)
            .map(|frame| SessionFrame::Schedule(frame.clone()).encode())
            .collect();
        let mut evaluator_wire = ScriptedTransport {
            inbound: inbound_payload,
            sent: Vec::new(),
        };
        let mut evaluator = MpcScheduleCursor::new(&manifest, 44, digest).unwrap();
        let mut evaluator_iterations = Vec::new();
        let executed = evaluator
            .drive(MpcScheduleRole::Evaluator, &mut evaluator_wire, |frame| {
                evaluator_iterations.push(frame.iteration.unwrap());
                Ok(())
            })
            .unwrap();
        assert_eq!(executed, 3);
        assert_eq!(evaluator_iterations, garbler_iterations);
        let acknowledgements: Vec<MpcScheduleFrame> = evaluator_wire
            .sent
            .iter()
            .map(|frame| match SessionFrame::decode(frame).unwrap() {
                SessionFrame::Schedule(frame) => frame,
                _ => panic!("driver sent a non-schedule frame"),
            })
            .collect();
        assert_eq!(
            acknowledgements,
            frames
                .iter()
                .filter(|frame| frame.phase == MpcSchedulePhase::EpochAck)
                .cloned()
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn both_roles_accept_identical_epoch_and_iteration_order() {
        let manifest = manifest();
        let mut left = MpcScheduleCursor::new(&manifest, 44, [9; 32]).unwrap();
        let mut right = MpcScheduleCursor::new(&manifest, 44, [9; 32]).unwrap();
        while let Some(frame) = left.next_frame() {
            right.accept(&frame).unwrap();
            left.accept(&frame).unwrap();
        }
        assert_eq!(left.accepted_iterations(), 2);
        assert!(left.is_complete());
        assert!(right.is_complete());
    }

    #[test]
    fn cursor_rejects_skip_duplicate_and_wrong_binding() {
        let manifest = manifest();
        let mut cursor = MpcScheduleCursor::new(&manifest, 44, [9; 32]).unwrap();
        let mut frame = cursor.next_frame().unwrap();
        frame.schedule_digest = [8; 32];
        assert_eq!(cursor.accept(&frame), Err(MpcScheduleError::DigestMismatch));
        frame.schedule_digest = [9; 32];
        frame.message_ordinal = 1;
        assert!(matches!(
            cursor.accept(&frame),
            Err(MpcScheduleError::UnexpectedMessageOrdinal { .. })
        ));
        frame.message_ordinal = 0;
        frame.phase = MpcSchedulePhase::EpochCommit;
        assert!(matches!(
            cursor.accept(&frame),
            Err(MpcScheduleError::UnexpectedPhase { .. })
        ));
        run_frames(&mut cursor);
        assert_eq!(cursor.accept(&frame), Err(MpcScheduleError::Complete));
    }

    #[test]
    fn cursor_requires_epoch_ack() {
        let manifest = manifest();
        let mut cursor = MpcScheduleCursor::new(&manifest, 44, [9; 32]).unwrap();
        for _ in 0..4 {
            let frame = cursor.next_frame().unwrap();
            cursor.accept(&frame).unwrap();
        }
        assert!(!cursor.is_complete());
        assert_eq!(cursor.finish(), Err(MpcScheduleError::Incomplete));
    }

    #[test]
    fn empty_manifest_is_complete() {
        let manifest = ScheduleManifest::new([1; 32], [2; 32], 7, 1, 1);
        let cursor = MpcScheduleCursor::new(&manifest, 44, [9; 32]).unwrap();
        assert!(cursor.is_complete());
        assert_eq!(cursor.next_frame(), None);
        cursor.finish().unwrap();
    }

    #[test]
    fn gate_binding_rejects_invalid_wire_and_accepts_pure_schedule() {
        let schedule = GateSchedule {
            num_inputs: 1,
            gates: vec![Gate::Xor(0, 0)],
            output: 1,
            outputs: None,
            storages: Vec::new(),
            actions: Vec::new(),
        };
        validate_gate_schedule_binding(&schedule, &manifest()).unwrap();
        let malformed = GateSchedule {
            gates: vec![Gate::Not(4)],
            ..schedule
        };
        assert_eq!(
            validate_gate_schedule_binding(&malformed, &manifest()),
            Err(MpcScheduleError::MalformedGateSchedule)
        );
    }
}
