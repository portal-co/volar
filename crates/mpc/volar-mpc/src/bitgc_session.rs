// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//! Bounded, transport-neutral envelope and session state for the proposed
//! BitGC protocol (BITGC-LEDGER-07).
//!
//! Each frame carries the protocol version, role, variant, parameter
//! fingerprint, circuit digest, session ID, and monotonic sequence number.
//! The codec is canonical and rejects unbounded or trailing input before
//! exposing a payload. The state machine rejects role/context mismatches,
//! replay, reordering, and invalid transitions, and makes cancellation
//! terminal.
//!
//! This is framing and local protocol-state validation only. It does not
//! authenticate a peer or transcript, perform OT, garble/evaluate a circuit,
//! or make BitGC available for protected sessions. The admission gate remains
//! closed pending the rest of BITGC-LEDGER-07 and independent review.
//!
//! TODO(provider-ledger: BITGC-LEDGER-07): transcript authentication, OT,
//! authenticated output opening, and reviewed parameters remain release gates.

use alloc::vec::Vec;

use volar_spec::bitgc::Variant;

const MAGIC: &[u8; 8] = b"VBGCF001";
const VERSION: u16 = 1;
const HEADER_BYTES: usize = 8 + 2 + 1 + 1 + 1 + 1 + 32 + 32 + 32 + 8 + 4;

/// Maximum opaque payload accepted in one BitGC frame.
pub const BITGC_FRAME_MAX_PAYLOAD: usize = 64 * 1024;
/// Maximum complete encoded frame, including the fixed header.
pub const BITGC_FRAME_MAX_BYTES: usize = HEADER_BYTES + BITGC_FRAME_MAX_PAYLOAD;

/// Party identity carried in every BitGC frame.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum BitGcRole {
    /// The party holding garbler-only label/PRF material.
    Garbler = 1,
    /// The party evaluating with evaluator-only material.
    Evaluator = 2,
}

impl BitGcRole {
    const fn peer(self) -> Self {
        match self {
            Self::Garbler => Self::Evaluator,
            Self::Evaluator => Self::Garbler,
        }
    }

    fn decode(value: u8) -> Result<Self, FrameError> {
        match value {
            1 => Ok(Self::Garbler),
            2 => Ok(Self::Evaluator),
            _ => Err(FrameError::UnknownRole),
        }
    }
}

/// Immutable public context shared by both parties for one session.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BitGcSessionBinding {
    /// Fresh session identifier.
    pub session_id: [u8; 32],
    /// Selected paper variant. Tiny is rejected until implemented.
    pub variant: Variant,
    /// Digest/fingerprint of the complete parameter profile and codec.
    pub parameter_fingerprint: [u8; 32],
    /// Digest of the ordered circuit/schedule being evaluated.
    pub circuit_digest: [u8; 32],
}

/// Frame kind understood by the Phase 2 envelope.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum BitGcFrameKind {
    /// Initial empty handshake frame; sequence number must be zero.
    Hello = 1,
    /// Opaque bounded protocol material (its semantics are deferred).
    Data = 2,
    /// Empty terminal cancellation frame.
    Cancel = 3,
}

impl BitGcFrameKind {
    fn decode(value: u8) -> Result<Self, FrameError> {
        match value {
            1 => Ok(Self::Hello),
            2 => Ok(Self::Data),
            3 => Ok(Self::Cancel),
            _ => Err(FrameError::UnknownKind),
        }
    }
}

/// A borrowed, canonical BitGC frame. Decoding borrows the payload and does
/// not allocate based on peer-provided lengths.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BitGcFrame<'a> {
    /// Sending party.
    pub role: BitGcRole,
    /// Public session/circuit/profile context.
    pub binding: BitGcSessionBinding,
    /// Monotonic sequence number in the sender-to-receiver direction.
    pub sequence: u64,
    /// Control or opaque-data frame kind.
    pub kind: BitGcFrameKind,
    /// Opaque payload, bounded by [`BITGC_FRAME_MAX_PAYLOAD`].
    pub payload: &'a [u8],
}

impl BitGcFrame<'_> {
    /// Encode this frame in its unique little-endian wire representation.
    pub fn encode(&self) -> Result<Vec<u8>, FrameError> {
        if self.payload.len() > BITGC_FRAME_MAX_PAYLOAD {
            return Err(FrameError::PayloadTooLarge);
        }
        let variant = encode_variant(self.binding.variant)?;
        let payload_len =
            u32::try_from(self.payload.len()).map_err(|_| FrameError::LengthOverflow)?;
        let total_len = HEADER_BYTES
            .checked_add(self.payload.len())
            .ok_or(FrameError::LengthOverflow)?;
        let mut encoded = Vec::new();
        encoded
            .try_reserve_exact(total_len)
            .map_err(|_| FrameError::AllocationFailed)?;
        encoded.extend_from_slice(MAGIC);
        encoded.extend_from_slice(&VERSION.to_le_bytes());
        encoded.push(self.role as u8);
        encoded.push(variant);
        encoded.push(self.kind as u8);
        encoded.push(0); // Reserved; nonzero encodings are noncanonical.
        encoded.extend_from_slice(&self.binding.session_id);
        encoded.extend_from_slice(&self.binding.parameter_fingerprint);
        encoded.extend_from_slice(&self.binding.circuit_digest);
        encoded.extend_from_slice(&self.sequence.to_le_bytes());
        encoded.extend_from_slice(&payload_len.to_le_bytes());
        encoded.extend_from_slice(self.payload);
        Ok(encoded)
    }

    /// Decode exactly one bounded frame, borrowing rather than copying its
    /// payload. Trailing bytes and noncanonical tags are rejected.
    pub fn decode(encoded: &'_ [u8]) -> Result<BitGcFrame<'_>, FrameError> {
        if encoded.len() < HEADER_BYTES {
            return Err(FrameError::Truncated);
        }
        if encoded.len() > BITGC_FRAME_MAX_BYTES {
            return Err(FrameError::FrameTooLarge);
        }
        if &encoded[..MAGIC.len()] != MAGIC {
            return Err(FrameError::BadMagic);
        }
        if u16::from_le_bytes([encoded[8], encoded[9]]) != VERSION {
            return Err(FrameError::UnsupportedVersion);
        }
        let role = BitGcRole::decode(encoded[10])?;
        let variant = decode_variant(encoded[11])?;
        let kind = BitGcFrameKind::decode(encoded[12])?;
        if encoded[13] != 0 {
            return Err(FrameError::NonCanonical);
        }

        let mut session_id = [0; 32];
        session_id.copy_from_slice(&encoded[14..46]);
        let mut parameter_fingerprint = [0; 32];
        parameter_fingerprint.copy_from_slice(&encoded[46..78]);
        let mut circuit_digest = [0; 32];
        circuit_digest.copy_from_slice(&encoded[78..110]);
        let sequence = u64::from_le_bytes([
            encoded[110],
            encoded[111],
            encoded[112],
            encoded[113],
            encoded[114],
            encoded[115],
            encoded[116],
            encoded[117],
        ]);
        let payload_len =
            u32::from_le_bytes([encoded[118], encoded[119], encoded[120], encoded[121]]) as usize;
        if payload_len > BITGC_FRAME_MAX_PAYLOAD {
            return Err(FrameError::PayloadTooLarge);
        }
        let expected_len = HEADER_BYTES
            .checked_add(payload_len)
            .ok_or(FrameError::LengthOverflow)?;
        if encoded.len() != expected_len {
            return Err(FrameError::LengthMismatch);
        }

        Ok(BitGcFrame {
            role,
            binding: BitGcSessionBinding {
                session_id,
                variant,
                parameter_fingerprint,
                circuit_digest,
            },
            sequence,
            kind,
            payload: &encoded[HEADER_BYTES..],
        })
    }
}

fn encode_variant(variant: Variant) -> Result<u8, FrameError> {
    match variant {
        Variant::Small => Ok(1),
        Variant::Fast => Ok(2),
        Variant::Tiny => Err(FrameError::UnsupportedVariant),
    }
}

fn decode_variant(value: u8) -> Result<Variant, FrameError> {
    match value {
        1 => Ok(Variant::Small),
        2 => Ok(Variant::Fast),
        3 => Err(FrameError::UnsupportedVariant),
        _ => Err(FrameError::UnknownVariant),
    }
}

/// Canonical frame parse/encode failure.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FrameError {
    /// Input did not contain a full fixed header.
    Truncated,
    /// The encoded frame exceeds the fixed maximum size.
    FrameTooLarge,
    /// Magic bytes do not identify this protocol.
    BadMagic,
    /// The protocol version is not supported.
    UnsupportedVersion,
    /// The role tag is not assigned.
    UnknownRole,
    /// The variant tag is not assigned.
    UnknownVariant,
    /// Tiny is known but is not available in this protocol implementation.
    UnsupportedVariant,
    /// The frame-kind tag is not assigned.
    UnknownKind,
    /// A reserved field or byte representation is noncanonical.
    NonCanonical,
    /// The declared payload length does not exactly consume the frame.
    LengthMismatch,
    /// The declared or supplied payload exceeds the configured maximum.
    PayloadTooLarge,
    /// A length calculation does not fit the supported integer range.
    LengthOverflow,
    /// The bounded output buffer could not be reserved.
    AllocationFailed,
}

/// Lifecycle state of one local BitGC framing endpoint.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BitGcSessionState {
    /// The local and peer hello frames have not both been exchanged.
    Handshaking,
    /// Both roles exchanged compatible hello frames.
    Active,
    /// A local or peer cancellation was observed; this state is terminal.
    Cancelled,
    /// Malformed input or an invalid transition occurred; terminal fail-closed state.
    Aborted,
}

/// Protocol/state validation failure. Any peer-frame error leaves the session
/// in [`BitGcSessionState::Aborted`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BitGcSessionError {
    /// The canonical frame codec rejected the input.
    Frame(FrameError),
    /// Session identifiers do not match.
    WrongSession,
    /// BitGC variants do not match.
    WrongVariant,
    /// Parameter fingerprints do not match.
    WrongParameters,
    /// Circuit digests do not match.
    WrongCircuit,
    /// The peer sent a frame claiming the local role.
    WrongRole,
    /// The peer sequence is replayed or out of order.
    SequenceMismatch,
    /// A frame kind is invalid in the current state.
    UnexpectedKind,
    /// A control frame has a nonempty payload.
    InvalidControlPayload,
    /// The sequence number cannot advance without wrapping.
    SequenceExhausted,
    /// The session has already been cancelled or aborted.
    Terminal,
}

/// Transport seam for bounded BitGC session frames.
///
/// Implementations must enforce `max_bytes` before allocating or reading a
/// peer frame. Cancellation must interrupt a blocked receive and be
/// idempotent. This trait does not provide peer authentication.
pub trait BitGcFrameTransport {
    /// Transport-specific I/O error.
    type Error;

    /// Send one already bounded encoded frame.
    fn send_frame(&mut self, frame: &[u8]) -> Result<(), Self::Error>;
    /// Receive at most `max_bytes`, rejecting oversized input before allocation.
    fn receive_frame(&mut self, max_bytes: usize) -> Result<Vec<u8>, Self::Error>;
    /// Interrupt pending I/O and close the transport; safe to call repeatedly.
    fn cancel(&mut self) -> Result<(), Self::Error>;
}

/// Error from a transport-driven send/receive seam.
#[derive(Debug)]
pub enum BitGcIoError<E> {
    /// A local protocol transition or peer frame failed validation.
    Protocol(BitGcSessionError),
    /// The transport failed; the session is terminally aborted.
    Transport(E),
}

/// One role's bounded BitGC framing state.
///
/// This state machine binds both directions to one session, variant,
/// parameter fingerprint, and circuit digest. Each direction starts at
/// sequence zero with exactly one empty hello; opaque data is accepted only
/// after both hello frames. A cancel frame is terminal. These checks prevent
/// accidental cross-session replay/reordering but are not an authentication
/// mechanism.
#[derive(Clone, Debug)]
pub struct BitGcSession {
    local_role: BitGcRole,
    binding: BitGcSessionBinding,
    state: BitGcSessionState,
    next_send_sequence: u64,
    next_receive_sequence: u64,
    local_hello_sent: bool,
    peer_hello_received: bool,
}

impl BitGcSession {
    /// Start one framing endpoint. Tiny is rejected until its construction is
    /// implemented; this does not override the separate fail-closed admission
    /// gate for all BitGC profiles.
    pub fn new(
        local_role: BitGcRole,
        binding: BitGcSessionBinding,
    ) -> Result<Self, BitGcSessionError> {
        encode_variant(binding.variant).map_err(BitGcSessionError::Frame)?;
        Ok(Self {
            local_role,
            binding,
            state: BitGcSessionState::Handshaking,
            next_send_sequence: 0,
            next_receive_sequence: 0,
            local_hello_sent: false,
            peer_hello_received: false,
        })
    }

    /// Current lifecycle state.
    pub const fn state(&self) -> BitGcSessionState {
        self.state
    }

    /// The immutable binding this endpoint enforces.
    pub const fn binding(&self) -> BitGcSessionBinding {
        self.binding
    }

    /// Send the one empty sequence-zero hello frame.
    pub fn hello(&mut self) -> Result<Vec<u8>, BitGcSessionError> {
        self.emit(BitGcFrameKind::Hello, &[])
    }

    /// Send one opaque bounded protocol payload after both hello frames.
    pub fn send_data(&mut self, payload: &[u8]) -> Result<Vec<u8>, BitGcSessionError> {
        self.emit(BitGcFrameKind::Data, payload)
    }

    /// Send an empty cancellation frame and make this endpoint terminal.
    pub fn cancel_frame(&mut self) -> Result<Vec<u8>, BitGcSessionError> {
        self.emit(BitGcFrameKind::Cancel, &[])
    }

    /// Validate one peer frame and return its borrowed payload.
    pub fn accept<'a>(&mut self, encoded: &'a [u8]) -> Result<BitGcFrame<'a>, BitGcSessionError> {
        if is_terminal(self.state) {
            return Err(BitGcSessionError::Terminal);
        }
        let frame = match BitGcFrame::decode(encoded) {
            Ok(frame) => frame,
            Err(error) => return self.abort_with(BitGcSessionError::Frame(error)),
        };
        if frame.role != self.local_role.peer() {
            return self.abort_with(BitGcSessionError::WrongRole);
        }
        if frame.binding.session_id != self.binding.session_id {
            return self.abort_with(BitGcSessionError::WrongSession);
        }
        if frame.binding.variant != self.binding.variant {
            return self.abort_with(BitGcSessionError::WrongVariant);
        }
        if frame.binding.parameter_fingerprint != self.binding.parameter_fingerprint {
            return self.abort_with(BitGcSessionError::WrongParameters);
        }
        if frame.binding.circuit_digest != self.binding.circuit_digest {
            return self.abort_with(BitGcSessionError::WrongCircuit);
        }
        if frame.sequence != self.next_receive_sequence {
            return self.abort_with(BitGcSessionError::SequenceMismatch);
        }
        let Some(next_sequence) = self.next_receive_sequence.checked_add(1) else {
            return self.abort_with(BitGcSessionError::SequenceExhausted);
        };

        match frame.kind {
            BitGcFrameKind::Hello => {
                if self.peer_hello_received || frame.sequence != 0 {
                    return self.abort_with(BitGcSessionError::UnexpectedKind);
                }
                if !frame.payload.is_empty() {
                    return self.abort_with(BitGcSessionError::InvalidControlPayload);
                }
                self.peer_hello_received = true;
            }
            BitGcFrameKind::Data => {
                if self.state != BitGcSessionState::Active {
                    return self.abort_with(BitGcSessionError::UnexpectedKind);
                }
            }
            BitGcFrameKind::Cancel => {
                if !frame.payload.is_empty() {
                    return self.abort_with(BitGcSessionError::InvalidControlPayload);
                }
                self.state = BitGcSessionState::Cancelled;
            }
        }
        self.next_receive_sequence = next_sequence;
        self.refresh_active_state();
        Ok(frame)
    }

    /// Send a frame through a fallible transport. Any I/O failure aborts the
    /// protocol state and best-effort cancels the transport.
    pub fn send_over<T: BitGcFrameTransport>(
        &mut self,
        transport: &mut T,
        kind: BitGcFrameKind,
        payload: &[u8],
    ) -> Result<(), BitGcIoError<T::Error>> {
        let encoded = self.emit(kind, payload).map_err(BitGcIoError::Protocol)?;
        if let Err(error) = transport.send_frame(&encoded) {
            self.state = BitGcSessionState::Aborted;
            let _ = transport.cancel();
            return Err(BitGcIoError::Transport(error));
        }
        Ok(())
    }

    /// Receive and validate one bounded frame. The returned encoded bytes can
    /// be decoded again by the caller to borrow its payload. Transport or
    /// protocol errors abort the endpoint and best-effort cancel I/O.
    pub fn receive_over<T: BitGcFrameTransport>(
        &mut self,
        transport: &mut T,
    ) -> Result<Vec<u8>, BitGcIoError<T::Error>> {
        let encoded = match transport.receive_frame(BITGC_FRAME_MAX_BYTES) {
            Ok(encoded) => encoded,
            Err(error) => {
                self.state = BitGcSessionState::Aborted;
                let _ = transport.cancel();
                return Err(BitGcIoError::Transport(error));
            }
        };
        if let Err(error) = self.accept(&encoded) {
            let _ = transport.cancel();
            return Err(BitGcIoError::Protocol(error));
        }
        if self.state == BitGcSessionState::Cancelled {
            if let Err(error) = transport.cancel() {
                self.state = BitGcSessionState::Aborted;
                return Err(BitGcIoError::Transport(error));
            }
        }
        Ok(encoded)
    }

    /// Send cancellation and interrupt the transport. Cancellation is
    /// terminal even if transport shutdown itself fails.
    pub fn cancel_over<T: BitGcFrameTransport>(
        &mut self,
        transport: &mut T,
    ) -> Result<(), BitGcIoError<T::Error>> {
        self.send_over(transport, BitGcFrameKind::Cancel, &[])?;
        transport.cancel().map_err(|error| {
            self.state = BitGcSessionState::Aborted;
            BitGcIoError::Transport(error)
        })
    }

    fn emit(&mut self, kind: BitGcFrameKind, payload: &[u8]) -> Result<Vec<u8>, BitGcSessionError> {
        if is_terminal(self.state) {
            return Err(BitGcSessionError::Terminal);
        }
        let valid = match kind {
            BitGcFrameKind::Hello => {
                !self.local_hello_sent && self.next_send_sequence == 0 && payload.is_empty()
            }
            BitGcFrameKind::Data => self.state == BitGcSessionState::Active,
            BitGcFrameKind::Cancel => payload.is_empty(),
        };
        if !valid {
            let error = if matches!(kind, BitGcFrameKind::Hello | BitGcFrameKind::Cancel)
                && !payload.is_empty()
            {
                BitGcSessionError::InvalidControlPayload
            } else {
                BitGcSessionError::UnexpectedKind
            };
            return self.abort_with(error);
        }
        let Some(next_sequence) = self.next_send_sequence.checked_add(1) else {
            return self.abort_with(BitGcSessionError::SequenceExhausted);
        };
        let frame = BitGcFrame {
            role: self.local_role,
            binding: self.binding,
            sequence: self.next_send_sequence,
            kind,
            payload,
        };
        let encoded = frame.encode().map_err(BitGcSessionError::Frame)?;
        self.next_send_sequence = next_sequence;
        match kind {
            BitGcFrameKind::Hello => self.local_hello_sent = true,
            BitGcFrameKind::Data => {}
            BitGcFrameKind::Cancel => self.state = BitGcSessionState::Cancelled,
        }
        self.refresh_active_state();
        Ok(encoded)
    }

    fn refresh_active_state(&mut self) {
        if self.state == BitGcSessionState::Handshaking
            && self.local_hello_sent
            && self.peer_hello_received
        {
            self.state = BitGcSessionState::Active;
        }
    }

    fn abort_with<T>(&mut self, error: BitGcSessionError) -> Result<T, BitGcSessionError> {
        self.state = BitGcSessionState::Aborted;
        Err(error)
    }
}

fn is_terminal(state: BitGcSessionState) -> bool {
    matches!(
        state,
        BitGcSessionState::Cancelled | BitGcSessionState::Aborted
    )
}

#[cfg(test)]
mod tests {
    use alloc::vec;
    use alloc::vec::Vec;

    use super::*;

    fn binding() -> BitGcSessionBinding {
        BitGcSessionBinding {
            session_id: [1; 32],
            variant: Variant::Small,
            parameter_fingerprint: [2; 32],
            circuit_digest: [3; 32],
        }
    }

    fn pair() -> (BitGcSession, BitGcSession) {
        (
            BitGcSession::new(BitGcRole::Garbler, binding()).unwrap(),
            BitGcSession::new(BitGcRole::Evaluator, binding()).unwrap(),
        )
    }

    fn handshake(garbler: &mut BitGcSession, evaluator: &mut BitGcSession) {
        let g_hello = garbler.hello().unwrap();
        let e_hello = evaluator.hello().unwrap();
        garbler.accept(&e_hello).unwrap();
        evaluator.accept(&g_hello).unwrap();
        assert_eq!(garbler.state(), BitGcSessionState::Active);
        assert_eq!(evaluator.state(), BitGcSessionState::Active);
    }

    #[test]
    fn canonical_frame_roundtrip_binds_every_context_field() {
        let frame = BitGcFrame {
            role: BitGcRole::Evaluator,
            binding: binding(),
            sequence: 42,
            kind: BitGcFrameKind::Data,
            payload: &[9, 8, 7],
        };
        let encoded = frame.encode().unwrap();
        let decoded = BitGcFrame::decode(&encoded).unwrap();
        assert_eq!(decoded, frame);
        assert_eq!(decoded.payload, &[9, 8, 7]);
    }

    #[test]
    fn codec_rejects_truncation_oversize_unknown_tags_and_noncanonical_bytes() {
        let valid = BitGcFrame {
            role: BitGcRole::Garbler,
            binding: binding(),
            sequence: 0,
            kind: BitGcFrameKind::Hello,
            payload: &[],
        }
        .encode()
        .unwrap();
        assert_eq!(
            BitGcFrame::decode(&valid[..HEADER_BYTES - 1]),
            Err(FrameError::Truncated)
        );

        let mut cases = Vec::new();
        let mut bad = valid.clone();
        bad[0] ^= 1;
        cases.push((bad, FrameError::BadMagic));
        let mut bad = valid.clone();
        bad[8] = 2;
        cases.push((bad, FrameError::UnsupportedVersion));
        let mut bad = valid.clone();
        bad[10] = 0;
        cases.push((bad, FrameError::UnknownRole));
        let mut bad = valid.clone();
        bad[11] = 3;
        cases.push((bad, FrameError::UnsupportedVariant));
        let mut bad = valid.clone();
        bad[11] = 99;
        cases.push((bad, FrameError::UnknownVariant));
        let mut bad = valid.clone();
        bad[12] = 0;
        cases.push((bad, FrameError::UnknownKind));
        let mut bad = valid.clone();
        bad[13] = 1;
        cases.push((bad, FrameError::NonCanonical));
        for (encoded, expected) in cases {
            assert_eq!(BitGcFrame::decode(&encoded), Err(expected));
        }

        let oversized_payload = vec![0; BITGC_FRAME_MAX_PAYLOAD + 1];
        let oversized = BitGcFrame {
            payload: &oversized_payload,
            ..valid_frame()
        };
        assert_eq!(oversized.encode(), Err(FrameError::PayloadTooLarge));
        let mut oversized_length = valid.clone();
        oversized_length[118..122]
            .copy_from_slice(&((BITGC_FRAME_MAX_PAYLOAD as u32) + 1).to_le_bytes());
        assert_eq!(
            BitGcFrame::decode(&oversized_length),
            Err(FrameError::PayloadTooLarge)
        );
        assert_eq!(
            BitGcFrame::decode(&vec![0; BITGC_FRAME_MAX_BYTES + 1]),
            Err(FrameError::FrameTooLarge)
        );

        let mut trailing = valid;
        trailing.push(0);
        assert_eq!(
            BitGcFrame::decode(&trailing),
            Err(FrameError::LengthMismatch)
        );
    }

    fn valid_frame<'a>() -> BitGcFrame<'a> {
        BitGcFrame {
            role: BitGcRole::Garbler,
            binding: binding(),
            sequence: 0,
            kind: BitGcFrameKind::Hello,
            payload: &[],
        }
    }

    #[test]
    fn role_pair_exchanges_bounded_data_in_order() {
        let (mut garbler, mut evaluator) = pair();
        handshake(&mut garbler, &mut evaluator);
        let g_data = garbler.send_data(&[1, 2, 3]).unwrap();
        let received = evaluator.accept(&g_data).unwrap();
        assert_eq!(received.payload, &[1, 2, 3]);
        let e_data = evaluator.send_data(&[4]).unwrap();
        assert_eq!(garbler.accept(&e_data).unwrap().payload, &[4]);
    }

    #[test]
    fn replay_reordering_and_wrong_role_are_terminal() {
        let (mut garbler, mut evaluator) = pair();
        handshake(&mut garbler, &mut evaluator);
        let data = garbler.send_data(&[7]).unwrap();
        evaluator.accept(&data).unwrap();
        assert_eq!(
            evaluator.accept(&data),
            Err(BitGcSessionError::SequenceMismatch)
        );
        assert_eq!(evaluator.state(), BitGcSessionState::Aborted);
        assert_eq!(evaluator.accept(&data), Err(BitGcSessionError::Terminal));

        let mut receiver = BitGcSession::new(BitGcRole::Garbler, binding()).unwrap();
        let forged_role = BitGcFrame {
            role: BitGcRole::Garbler,
            ..valid_frame()
        }
        .encode()
        .unwrap();
        assert_eq!(
            receiver.accept(&forged_role),
            Err(BitGcSessionError::WrongRole)
        );
        assert_eq!(receiver.state(), BitGcSessionState::Aborted);
    }

    #[test]
    fn reordered_or_pre_handshake_data_is_rejected() {
        let mut receiver = BitGcSession::new(BitGcRole::Evaluator, binding()).unwrap();
        let early_data = BitGcFrame {
            role: BitGcRole::Garbler,
            binding: binding(),
            sequence: 0,
            kind: BitGcFrameKind::Data,
            payload: &[1],
        }
        .encode()
        .unwrap();
        assert_eq!(
            receiver.accept(&early_data),
            Err(BitGcSessionError::UnexpectedKind)
        );
        assert_eq!(receiver.state(), BitGcSessionState::Aborted);

        let (mut garbler, mut evaluator) = pair();
        handshake(&mut garbler, &mut evaluator);
        let out_of_order = BitGcFrame {
            role: BitGcRole::Garbler,
            binding: binding(),
            sequence: 2,
            kind: BitGcFrameKind::Data,
            payload: &[],
        }
        .encode()
        .unwrap();
        assert_eq!(
            evaluator.accept(&out_of_order),
            Err(BitGcSessionError::SequenceMismatch)
        );
        assert_eq!(evaluator.state(), BitGcSessionState::Aborted);
    }

    #[test]
    fn all_session_binding_mismatches_abort_before_data() {
        let mismatches = [
            (
                BitGcSessionError::WrongSession,
                BitGcSessionBinding {
                    session_id: [9; 32],
                    ..binding()
                },
            ),
            (
                BitGcSessionError::WrongVariant,
                BitGcSessionBinding {
                    variant: Variant::Fast,
                    ..binding()
                },
            ),
            (
                BitGcSessionError::WrongParameters,
                BitGcSessionBinding {
                    parameter_fingerprint: [9; 32],
                    ..binding()
                },
            ),
            (
                BitGcSessionError::WrongCircuit,
                BitGcSessionBinding {
                    circuit_digest: [9; 32],
                    ..binding()
                },
            ),
        ];
        for (expected, peer_binding) in mismatches {
            let mut receiver = BitGcSession::new(BitGcRole::Evaluator, binding()).unwrap();
            let hello = BitGcFrame {
                role: BitGcRole::Garbler,
                binding: peer_binding,
                sequence: 0,
                kind: BitGcFrameKind::Hello,
                payload: &[],
            }
            .encode()
            .unwrap();
            assert_eq!(receiver.accept(&hello), Err(expected));
            assert_eq!(receiver.state(), BitGcSessionState::Aborted);
        }
    }

    #[test]
    fn cancellation_is_terminal_during_handshake_and_active_state() {
        let (mut garbler, mut evaluator) = pair();
        let early_cancel = garbler.cancel_frame().unwrap();
        evaluator.accept(&early_cancel).unwrap();
        assert_eq!(garbler.state(), BitGcSessionState::Cancelled);
        assert_eq!(evaluator.state(), BitGcSessionState::Cancelled);
        assert_eq!(evaluator.hello(), Err(BitGcSessionError::Terminal));

        let (mut garbler, mut evaluator) = pair();
        handshake(&mut garbler, &mut evaluator);
        let cancel = garbler.cancel_frame().unwrap();
        evaluator.accept(&cancel).unwrap();
        assert_eq!(garbler.state(), BitGcSessionState::Cancelled);
        assert_eq!(evaluator.state(), BitGcSessionState::Cancelled);
        assert_eq!(garbler.send_data(&[]), Err(BitGcSessionError::Terminal));
        assert_eq!(evaluator.hello(), Err(BitGcSessionError::Terminal));
    }

    #[test]
    fn session_rejects_malformed_and_over_budget_peer_frames_terminally() {
        let mut malformed_receiver = BitGcSession::new(BitGcRole::Evaluator, binding()).unwrap();
        let mut malformed = valid_frame().encode().unwrap();
        malformed[13] = 1;
        assert_eq!(
            malformed_receiver.accept(&malformed),
            Err(BitGcSessionError::Frame(FrameError::NonCanonical))
        );
        assert_eq!(malformed_receiver.state(), BitGcSessionState::Aborted);

        let mut oversized_receiver = BitGcSession::new(BitGcRole::Evaluator, binding()).unwrap();
        let mut io = TestTransport {
            incoming: Some(vec![0; BITGC_FRAME_MAX_BYTES + 1]),
            sent: Vec::new(),
            fail_send: false,
            cancelled: false,
            receive_limit: 0,
        };
        assert!(matches!(
            oversized_receiver.receive_over(&mut io),
            Err(BitGcIoError::Protocol(BitGcSessionError::Frame(
                FrameError::FrameTooLarge
            )))
        ));
        assert_eq!(oversized_receiver.state(), BitGcSessionState::Aborted);
        assert!(io.cancelled);
    }

    #[test]
    fn tiny_variant_is_rejected_at_session_construction() {
        let tiny = BitGcSessionBinding {
            variant: Variant::Tiny,
            ..binding()
        };
        assert!(matches!(
            BitGcSession::new(BitGcRole::Garbler, tiny),
            Err(BitGcSessionError::Frame(FrameError::UnsupportedVariant))
        ));
    }

    struct TestTransport {
        incoming: Option<Vec<u8>>,
        sent: Vec<Vec<u8>>,
        fail_send: bool,
        cancelled: bool,
        receive_limit: usize,
    }

    impl BitGcFrameTransport for TestTransport {
        type Error = ();

        fn send_frame(&mut self, frame: &[u8]) -> Result<(), Self::Error> {
            if self.fail_send {
                return Err(());
            }
            self.sent.push(frame.to_vec());
            Ok(())
        }

        fn receive_frame(&mut self, max_bytes: usize) -> Result<Vec<u8>, Self::Error> {
            self.receive_limit = max_bytes;
            self.incoming.take().ok_or(())
        }

        fn cancel(&mut self) -> Result<(), Self::Error> {
            self.cancelled = true;
            Ok(())
        }
    }

    #[test]
    fn transport_bounds_receives_and_io_failure_aborts_and_cancels() {
        let (mut garbler, mut evaluator) = pair();
        let g_hello = garbler.hello().unwrap();
        let e_hello = evaluator.hello().unwrap();
        let mut io = TestTransport {
            incoming: Some(g_hello),
            sent: Vec::new(),
            fail_send: false,
            cancelled: false,
            receive_limit: 0,
        };
        evaluator.receive_over(&mut io).unwrap();
        assert_eq!(io.receive_limit, BITGC_FRAME_MAX_BYTES);
        assert_eq!(evaluator.state(), BitGcSessionState::Active);

        let mut failing = TestTransport {
            incoming: None,
            sent: Vec::new(),
            fail_send: true,
            cancelled: false,
            receive_limit: 0,
        };
        let mut failed_sender = BitGcSession::new(BitGcRole::Garbler, binding()).unwrap();
        assert!(matches!(
            failed_sender.send_over(&mut failing, BitGcFrameKind::Hello, &[]),
            Err(BitGcIoError::Transport(()))
        ));
        assert_eq!(failed_sender.state(), BitGcSessionState::Aborted);
        assert!(failing.cancelled);

        // Keep the peer hello used above live to assert it has the opposite role.
        assert_eq!(
            BitGcFrame::decode(&e_hello).unwrap().role,
            BitGcRole::Evaluator
        );
    }
}
