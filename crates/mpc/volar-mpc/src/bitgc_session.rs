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
//! This is an experimental framing/state foundation with bounded
//! Chou–Orlandi OT and output-label hash matching. Callers may opt into keyed
//! frame authentication with [`BitGcSession::new_authenticated`] or the
//! PSK-authenticated [`BitGcSession::new_psk_authenticated`] constructor and
//! keyed OT envelopes. Session-driven input transfer maintains one shared
//! context transcript across hello/data frames and OT messages; frame
//! authentication tags use that same common state. The canonical digest
//! helpers bind the circuit/profile shape; they do not make BitGC available
//! for protected sessions. The admission gate remains closed pending Cirrus
//! integration and independent review.
//!
//! TODO(provider-ledger: BITGC-LEDGER-07): forward-secret external key
//! exchange where required, Cirrus integration, and reviewed parameters
//! remain release gates.

use alloc::vec::Vec;
use digest::Digest;
use hybrid_array::Array;
use sha2::Sha256;
use volar_spec::SpecRng;
use volar_spec::bitgc::Variant;
use volar_spec::bitgc::common::{Circuit, Gate};
use volar_spec::bitgc::pinned::{
    DriverError, GarblerWireLabels, LabelPoly, PrfKey, RoleDdGarbler, RoleMaterialCodec,
    RoleMaterialError, decode_role_tables, encode_role_tables, evaluate_role_circuit_with_labels,
    garble_role_circuit,
};
use volar_spec::vole::VoleArray;

use crate::OtChannel;

const MAGIC: &[u8; 8] = b"VBGCF001";
const VERSION: u16 = 1;
const HEADER_BYTES: usize = 8 + 2 + 1 + 1 + 1 + 1 + 32 + 32 + 32 + 8 + 4;

/// Maximum opaque payload accepted in one BitGC frame.
pub const BITGC_FRAME_MAX_PAYLOAD: usize = 64 * 1024;
/// Maximum complete encoded frame, including the fixed header.
pub const BITGC_FRAME_MAX_BYTES: usize = HEADER_BYTES + BITGC_FRAME_MAX_PAYLOAD;
/// Maximum canonical byte length for one serialized BitGC input label.
pub const BITGC_OT_MAX_INPUT_BYTES: usize = 1024 * 1024;
const INPUT_CHUNK_TAG: u8 = 0x10;
const INPUT_CHUNK_HEADER_BYTES: usize = 1 + 8 + 4 + 4 + 4;
const OUTPUT_COMMIT_TAG: u8 = 0x20;
const OUTPUT_COMMIT_BYTES: usize = 1 + 4 + 4 + 8 + 32 + 32;
const BITGC_OUTPUT_MAX_COUNT: usize = 1 << 20;
/// Garbler-to-evaluator evaluator-material chunk (Phase 4 delivery).
const MATERIAL_CHUNK_TAG: u8 = 0x30;
const MATERIAL_CHUNK_HEADER_BYTES: usize = 1 + 8 + 4 + 4;
/// Maximum canonical byte length of one evaluator-material package.
pub const BITGC_MATERIAL_MAX_BYTES: usize = 16 * 1024 * 1024;
/// Garbler-to-evaluator direct delivery of one garbler-owned input label.
const GARBLER_INPUT_TAG: u8 = 0x40;
const GARBLER_INPUT_HEADER_BYTES: usize = 1 + 8 + 8;
const AUTH_TAG_BYTES: usize = 32;
const AUTH_BLOCK_BYTES: usize = 64;
const AUTH_DOMAIN: &[u8] = b"Volar BitGC frame auth v1\0";
const CIRCUIT_DIGEST_DOMAIN: &[u8] = b"Volar BitGC circuit digest v1\0";
const PARAMETER_DIGEST_DOMAIN: &[u8] = b"Volar BitGC parameter fingerprint v1\0";
const PSK_HELLO_MAGIC: &[u8; 8] = b"VBGPK001";
const PSK_HELLO_BYTES: usize = 8 + 32 + AUTH_TAG_BYTES;
const PSK_HELLO_DOMAIN: &[u8] = b"Volar BitGC PSK hello v1\0";
const PSK_SESSION_KEY_DOMAIN: &[u8] = b"Volar BitGC PSK session key v1\0";
const FRAME_DIGEST_DOMAIN: &[u8] = b"Volar BitGC frame digest v1\0";
const FRAME_OT_CONTEXT_DOMAIN: &[u8] = b"Volar BitGC frame/OT context v1\0";
const UNIFIED_TRANSCRIPT_DOMAIN: &[u8] = b"Volar BitGC unified transcript v1\0";

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

/// Failure while deriving a canonical session binding digest.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BitGcDigestError {
    /// The circuit is malformed or contains a non-topological wire.
    InvalidCircuit,
    /// The requested variant is not available to this protocol.
    UnsupportedVariant,
    /// A platform-sized count or index cannot be represented canonically.
    LengthOverflow,
    /// The parameter profile is outside the canonical BitGC shape.
    InvalidParameters,
}

/// Derive the circuit digest from the canonical, dependency-preserving circuit
/// encoding. Callers must use this instead of hashing a compiler-specific
/// serialization or supplying an unrelated digest.
pub fn canonical_circuit_digest(circuit: &Circuit) -> Result<[u8; 32], BitGcDigestError> {
    circuit
        .validate()
        .map_err(|_| BitGcDigestError::InvalidCircuit)?;
    let inputs = u64::try_from(circuit.inputs).map_err(|_| BitGcDigestError::LengthOverflow)?;
    let gates = u64::try_from(circuit.gates.len()).map_err(|_| BitGcDigestError::LengthOverflow)?;
    let outputs =
        u64::try_from(circuit.outputs.len()).map_err(|_| BitGcDigestError::LengthOverflow)?;
    let mut hash = Sha256::new();
    hash.update(CIRCUIT_DIGEST_DOMAIN);
    hash.update(inputs.to_le_bytes());
    hash.update(gates.to_le_bytes());
    for gate in &circuit.gates {
        let (tag, a, b) = match *gate {
            Gate::And { a, b } => (1u8, a, b),
            Gate::Xor { a, b } => (2u8, a, b),
            Gate::Not { a } => (3u8, a, a),
        };
        hash.update([tag]);
        hash.update(
            u64::try_from(a)
                .map_err(|_| BitGcDigestError::LengthOverflow)?
                .to_le_bytes(),
        );
        hash.update(
            u64::try_from(b)
                .map_err(|_| BitGcDigestError::LengthOverflow)?
                .to_le_bytes(),
        );
    }
    hash.update(outputs.to_le_bytes());
    for &output in &circuit.outputs {
        hash.update(
            u64::try_from(output)
                .map_err(|_| BitGcDigestError::LengthOverflow)?
                .to_le_bytes(),
        );
    }
    let digest = hash.finalize();
    let mut result = [0; 32];
    result.copy_from_slice(&digest);
    Ok(result)
}

/// Derive the parameter fingerprint from the exact profile inputs used by the
/// session. The modulus chain is encoded in order, so changing a limb or its
/// level cannot silently reuse a session binding.
pub fn canonical_parameter_fingerprint(
    variant: Variant,
    plaintext_modulus: u64,
    ring_degree: usize,
    modulus_chain: &[u64],
) -> Result<[u8; 32], BitGcDigestError> {
    let variant = encode_variant(variant).map_err(|_| BitGcDigestError::UnsupportedVariant)?;
    if plaintext_modulus <= 2
        || plaintext_modulus & 1 == 0
        || ring_degree == 0
        || modulus_chain.is_empty()
        || modulus_chain
            .iter()
            .any(|&modulus| modulus <= 2 || modulus & 1 == 0)
    {
        return Err(BitGcDigestError::InvalidParameters);
    }
    let degree = u64::try_from(ring_degree).map_err(|_| BitGcDigestError::LengthOverflow)?;
    let levels =
        u64::try_from(modulus_chain.len()).map_err(|_| BitGcDigestError::LengthOverflow)?;
    let mut hash = Sha256::new();
    hash.update(PARAMETER_DIGEST_DOMAIN);
    hash.update([variant]);
    hash.update(plaintext_modulus.to_le_bytes());
    hash.update(degree.to_le_bytes());
    hash.update(levels.to_le_bytes());
    for &modulus in modulus_chain {
        hash.update(modulus.to_le_bytes());
    }
    let digest = hash.finalize();
    let mut result = [0; 32];
    result.copy_from_slice(&digest);
    Ok(result)
}

/// Build a binding with canonical circuit and parameter digests.
pub fn canonical_session_binding(
    session_id: [u8; 32],
    variant: Variant,
    plaintext_modulus: u64,
    ring_degree: usize,
    modulus_chain: &[u64],
    circuit: &Circuit,
) -> Result<BitGcSessionBinding, BitGcDigestError> {
    Ok(BitGcSessionBinding {
        session_id,
        variant,
        parameter_fingerprint: canonical_parameter_fingerprint(
            variant,
            plaintext_modulus,
            ring_degree,
            modulus_chain,
        )?,
        circuit_digest: canonical_circuit_digest(circuit)?,
    })
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
    /// Local or received label material is malformed or outside its profile.
    InvalidInputMaterial,
    /// An input chunk descriptor is malformed or out of its declared order.
    InvalidInputDescriptor,
    /// An output commitment record does not match the expected output/wire.
    InvalidOutputCommitment,
    /// An evaluator-material chunk is malformed or out of its declared order.
    InvalidMaterial,
    /// A garbler-input delivery frame is malformed or out of order.
    InvalidGarblerInput,
    /// The evaluator's active output label matches neither committed label.
    OutputLabelMismatch,
    /// An authenticated session frame did not carry the required tag.
    MissingAuthentication,
    /// An authenticated session frame failed its keyed transcript check.
    AuthenticationFailed,
    /// The underlying 1-of-2 OT operation failed.
    OtFailure,
    /// Cancelling the underlying OT operation failed.
    OtCancellationFailure,
    /// The sequence number cannot advance without wrapping.
    SequenceExhausted,
    /// The session has already been cancelled or aborted.
    Terminal,
}

/// Ownership of one circuit input in the shared protocol.
///
/// Garbler-owned inputs are delivered directly (the garbler knows the bit);
/// evaluator-owned inputs are transferred by OT so the garbler learns
/// nothing about them.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BitGcInputOwner {
    /// The garbler supplies this input bit; its active label is sent
    /// directly in one Data frame.
    Garbler,
    /// The evaluator supplies this input bit; its label arrives by OT.
    Evaluator,
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

/// Fallible 1-of-2 OT seam for evaluator-input label chunks.
///
/// The channel must be dedicated to this session (or independently bind each
/// OT to the same session/input/chunk context). `cancel` must interrupt any
/// blocked OT operation. The old [`crate::OtChannel`] trait is infallible, so
/// it can only be used through the explicitly named compatibility adapter
/// below; production adapters should preserve their transport errors here.
pub trait BitGcInputOt<N: VoleArray<u8>> {
    /// OT-specific protocol or transport error.
    type Error;

    /// Offer one fixed-size false/true message pair.
    fn send_pair(&mut self, labels: [&Array<u8, N>; 2]) -> Result<(), Self::Error>;
    /// Receive exactly the selected fixed-size message.
    fn receive_choice(&mut self, choice: bool) -> Result<Array<u8, N>, Self::Error>;
    /// Bind the next OT invocation to the current unified frame/OT transcript.
    /// Local compatibility adapters may leave this as the default no-op.
    fn bind_context(&mut self, _context: &[u8; 32]) {}
    /// Return the current authenticated transcript checkpoint when available.
    fn transcript(&self) -> Option<[u8; 32]> {
        None
    }
    /// Interrupt pending OT work; safe to call repeatedly.
    fn cancel(&mut self) -> Result<(), Self::Error>;
}

/// Bounded, fallible message channel for one Chou–Orlandi OT session.
/// Implementations must enforce `max_bytes` before allocating peer data.
pub trait BitGcOtTransport {
    /// Transport error.
    type Error;

    /// Send one bounded OT message.
    fn send_ot_message(&mut self, message: &[u8]) -> Result<(), Self::Error>;
    /// Receive one OT message, enforcing the supplied limit before allocation.
    fn receive_ot_message(&mut self, max_bytes: usize) -> Result<Vec<u8>, Self::Error>;
    /// Bind the next OT message sequence to the current unified frame/OT
    /// transcript.
    fn bind_context(&mut self, _context: &[u8; 32]) {}
    /// Return the current authenticated transcript checkpoint when available.
    fn transcript(&self) -> Option<[u8; 32]> {
        None
    }
    /// Interrupt pending OT I/O; safe to call repeatedly.
    fn cancel(&mut self) -> Result<(), Self::Error>;
}

const OT_MAGIC: &[u8; 8] = b"VBGOT001";
const OT_HEADER_BYTES: usize = 8 + 1 + 8 + 4;
const OT_ENVELOPE_OVERHEAD: usize = OT_HEADER_BYTES + AUTH_TAG_BYTES;

/// Error from the keyed OT envelope.
#[derive(Debug, PartialEq, Eq)]
pub enum AuthenticatedOtError<E> {
    /// The wrapped OT transport failed.
    Transport(E),
    /// The envelope was malformed, replayed, or exceeded its bound.
    InvalidMessage,
    /// The envelope could not be allocated locally.
    AllocationFailed,
}

/// Adds a keyed, ordered envelope to the dedicated OT message transport.
///
/// This authenticates and binds each OT message to the public session binding,
/// role, shared frame/OT context transcript, and one ordered sequence shared
/// by both OT directions. It is intended to be used together with
/// [`BitGcSession::new_authenticated`]. The frame and OT transports remain
/// separate channels, but session-driven input transfer seeds OT from the
/// common transcript checkpoint.
pub struct AuthenticatedBitGcOtTransport<T> {
    role: BitGcRole,
    binding: BitGcSessionBinding,
    key: [u8; 32],
    transport: T,
    next_sequence: u64,
    transcript: [u8; 32],
    context: [u8; 32],
}

impl<T> AuthenticatedBitGcOtTransport<T> {
    /// Wrap a dedicated OT transport with the authenticated session envelope.
    pub fn new(role: BitGcRole, binding: BitGcSessionBinding, key: [u8; 32], transport: T) -> Self {
        Self {
            role,
            binding,
            key,
            transport,
            next_sequence: 0,
            transcript: initial_ot_transcript(&binding),
            context: [0; 32],
        }
    }
}

impl<T: BitGcOtTransport> BitGcOtTransport for AuthenticatedBitGcOtTransport<T> {
    type Error = AuthenticatedOtError<T::Error>;

    fn send_ot_message(&mut self, message: &[u8]) -> Result<(), Self::Error> {
        if message.len() > BITGC_OT_MAX_INPUT_BYTES {
            return Err(AuthenticatedOtError::InvalidMessage);
        }
        let sequence = self.next_sequence;
        let next = sequence
            .checked_add(1)
            .ok_or(AuthenticatedOtError::InvalidMessage)?;
        let length =
            u32::try_from(message.len()).map_err(|_| AuthenticatedOtError::InvalidMessage)?;
        let mut envelope = Vec::new();
        envelope
            .try_reserve_exact(OT_ENVELOPE_OVERHEAD + message.len())
            .map_err(|_| AuthenticatedOtError::AllocationFailed)?;
        envelope.extend_from_slice(OT_MAGIC);
        envelope.push(self.role as u8);
        envelope.extend_from_slice(&sequence.to_le_bytes());
        envelope.extend_from_slice(&length.to_le_bytes());
        envelope.extend_from_slice(message);
        let tag = ot_auth_tag(
            &self.key,
            &self.binding,
            self.role,
            &self.transcript,
            &self.context,
            sequence,
            message,
        );
        envelope.extend_from_slice(&tag);
        self.transport
            .send_ot_message(&envelope)
            .map_err(AuthenticatedOtError::Transport)?;
        self.transcript = ot_transcript_after(&self.transcript, &envelope);
        self.next_sequence = next;
        Ok(())
    }

    fn receive_ot_message(&mut self, max_bytes: usize) -> Result<Vec<u8>, Self::Error> {
        if max_bytes > BITGC_OT_MAX_INPUT_BYTES {
            return Err(AuthenticatedOtError::InvalidMessage);
        }
        let limit = max_bytes
            .checked_add(OT_ENVELOPE_OVERHEAD)
            .ok_or(AuthenticatedOtError::InvalidMessage)?;
        let envelope = self
            .transport
            .receive_ot_message(limit)
            .map_err(AuthenticatedOtError::Transport)?;
        if envelope.len() < OT_ENVELOPE_OVERHEAD
            || envelope[..OT_MAGIC.len()] != OT_MAGIC[..]
            || envelope[8] != self.role.peer() as u8
        {
            return Err(AuthenticatedOtError::InvalidMessage);
        }
        let sequence = u64::from_le_bytes(
            envelope[9..17]
                .try_into()
                .map_err(|_| AuthenticatedOtError::InvalidMessage)?,
        );
        if sequence != self.next_sequence {
            return Err(AuthenticatedOtError::InvalidMessage);
        }
        let length = u32::from_le_bytes(
            envelope[17..21]
                .try_into()
                .map_err(|_| AuthenticatedOtError::InvalidMessage)?,
        ) as usize;
        if length > max_bytes || envelope.len() != OT_HEADER_BYTES + length + AUTH_TAG_BYTES {
            return Err(AuthenticatedOtError::InvalidMessage);
        }
        let payload_end = OT_HEADER_BYTES + length;
        let payload = &envelope[OT_HEADER_BYTES..payload_end];
        let supplied: &[u8; AUTH_TAG_BYTES] = envelope[payload_end..]
            .try_into()
            .map_err(|_| AuthenticatedOtError::InvalidMessage)?;
        let expected = ot_auth_tag(
            &self.key,
            &self.binding,
            self.role.peer(),
            &self.transcript,
            &self.context,
            sequence,
            payload,
        );
        if !constant_time_eq(&expected, supplied) {
            return Err(AuthenticatedOtError::InvalidMessage);
        }
        let mut message = Vec::new();
        message
            .try_reserve_exact(length)
            .map_err(|_| AuthenticatedOtError::AllocationFailed)?;
        message.extend_from_slice(payload);
        self.transcript = ot_transcript_after(&self.transcript, &envelope);
        self.next_sequence = self
            .next_sequence
            .checked_add(1)
            .ok_or(AuthenticatedOtError::InvalidMessage)?;
        Ok(message)
    }

    fn bind_context(&mut self, context: &[u8; 32]) {
        self.context = *context;
        self.transcript = *context;
    }

    fn transcript(&self) -> Option<[u8; 32]> {
        Some(self.transcript)
    }

    fn cancel(&mut self) -> Result<(), Self::Error> {
        self.transport
            .cancel()
            .map_err(AuthenticatedOtError::Transport)
    }
}

/// Failure while running one Chou–Orlandi OT invocation.
#[derive(Debug)]
pub enum ChouOrlandiOtError<E> {
    /// The bounded message transport failed.
    Transport(E),
    /// The role, message size, or OT message encoding was invalid.
    InvalidMessage,
    /// This endpoint's role does not match the requested OT operation.
    WrongRole,
}

/// Chou–Orlandi 1-of-2 OT over a dedicated bounded message transport.
///
/// Construct a garbler endpoint with `BitGcRole::Garbler` and an evaluator
/// endpoint with `BitGcRole::Evaluator`. One instance handles the ordered OT
/// stream for a session. When its transport supports it, `bind_context`
/// attaches each OT invocation to the preceding session descriptor frame.
pub struct ChouOrlandiBitGcOt<N, T, R>
where
    N: VoleArray<u8>,
    T: BitGcOtTransport,
    R: volar_spec::SpecRng,
{
    role: BitGcRole,
    transport: T,
    rng: R,
    context: [u8; 32],
    _n: core::marker::PhantomData<N>,
}

impl<N, T, R> ChouOrlandiBitGcOt<N, T, R>
where
    N: VoleArray<u8>,
    T: BitGcOtTransport,
    R: volar_spec::SpecRng,
{
    /// Create an OT endpoint. Its transport must be dedicated to the same
    /// session and consumed in the same chunk order as the session frames.
    pub fn new(role: BitGcRole, transport: T, rng: R) -> Self {
        Self {
            role,
            transport,
            rng,
            context: [0; 32],
            _n: core::marker::PhantomData,
        }
    }
}

impl<N, T, R> BitGcInputOt<N> for ChouOrlandiBitGcOt<N, T, R>
where
    N: VoleArray<u8>,
    T: BitGcOtTransport,
    R: volar_spec::SpecRng,
{
    type Error = ChouOrlandiOtError<T::Error>;

    fn bind_context(&mut self, context: &[u8; 32]) {
        self.context = *context;
        self.transport.bind_context(context);
    }

    fn send_pair(&mut self, labels: [&Array<u8, N>; 2]) -> Result<(), Self::Error> {
        if self.role != BitGcRole::Garbler {
            return Err(ChouOrlandiOtError::WrongRole);
        }
        let (sender, setup) = crate::ot::CoSender::<N>::setup(&mut self.rng);
        self.transport
            .send_ot_message(&setup)
            .map_err(ChouOrlandiOtError::Transport)?;
        let response = self
            .transport
            .receive_ot_message(64)
            .map_err(ChouOrlandiOtError::Transport)?;
        if response.len() != 64 {
            return Err(ChouOrlandiOtError::InvalidMessage);
        }
        let encrypted = sender
            .finish(&response, labels)
            .map_err(|_| ChouOrlandiOtError::InvalidMessage)?;
        self.transport
            .send_ot_message(&encrypted)
            .map_err(ChouOrlandiOtError::Transport)
    }

    fn receive_choice(&mut self, choice: bool) -> Result<Array<u8, N>, Self::Error> {
        if self.role != BitGcRole::Evaluator {
            return Err(ChouOrlandiOtError::WrongRole);
        }
        let setup = self
            .transport
            .receive_ot_message(64)
            .map_err(ChouOrlandiOtError::Transport)?;
        if setup.len() != 64 {
            return Err(ChouOrlandiOtError::InvalidMessage);
        }
        let (receiver, response) = crate::ot::CoReceiver::<N>::setup(&mut self.rng, &setup, choice)
            .map_err(|_| ChouOrlandiOtError::InvalidMessage)?;
        self.transport
            .send_ot_message(&response)
            .map_err(ChouOrlandiOtError::Transport)?;
        let max_bytes = N::USIZE
            .checked_mul(2)
            .ok_or(ChouOrlandiOtError::InvalidMessage)?;
        let encrypted = self
            .transport
            .receive_ot_message(max_bytes)
            .map_err(ChouOrlandiOtError::Transport)?;
        receiver
            .finish(&encrypted)
            .map_err(|_| ChouOrlandiOtError::InvalidMessage)
    }

    fn transcript(&self) -> Option<[u8; 32]> {
        self.transport.transcript()
    }

    fn cancel(&mut self) -> Result<(), Self::Error> {
        self.transport
            .cancel()
            .map_err(ChouOrlandiOtError::Transport)
    }
}

/// Compatibility wrapper for existing infallible [`crate::OtChannel`]
/// implementations. It does not make an infallible channel suitable for
/// production; its owner must separately guarantee transport cancellation.
pub struct InfallibleBitGcOt<'a, N: VoleArray<u8>> {
    channel: &'a mut dyn OtChannel<N>,
}

impl<'a, N: VoleArray<u8>> InfallibleBitGcOt<'a, N> {
    /// Wrap an existing `OtChannel` for deterministic/local compatibility.
    pub fn new(channel: &'a mut dyn OtChannel<N>) -> Self {
        Self { channel }
    }
}

impl<N: VoleArray<u8>> BitGcInputOt<N> for InfallibleBitGcOt<'_, N> {
    type Error = core::convert::Infallible;

    fn send_pair(&mut self, labels: [&Array<u8, N>; 2]) -> Result<(), Self::Error> {
        self.channel.send(labels);
        Ok(())
    }

    fn receive_choice(&mut self, choice: bool) -> Result<Array<u8, N>, Self::Error> {
        Ok(self.channel.receive(choice))
    }

    fn cancel(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
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
/// after both hello frames. A cancel frame is terminal. [`Self::new`] provides
/// structural checks only; [`Self::new_authenticated`] additionally verifies
/// a keyed transcript tag on every frame.
#[derive(Clone, Debug)]
pub struct BitGcSession {
    local_role: BitGcRole,
    binding: BitGcSessionBinding,
    state: BitGcSessionState,
    next_send_sequence: u64,
    next_receive_sequence: u64,
    local_hello_sent: bool,
    peer_hello_received: bool,
    auth_key: Option<[u8; 32]>,
    psk_key: Option<[u8; 32]>,
    local_nonce: Option<[u8; 32]>,
    unified_transcript: [u8; 32],
    garbler_hello_digest: [u8; 32],
    evaluator_hello_digest: [u8; 32],
    last_sent_frame_digest: [u8; 32],
    last_received_frame_digest: [u8; 32],
}

impl BitGcSession {
    /// Start one framing endpoint. Tiny is rejected until its construction is
    /// implemented; this does not override the separate fail-closed admission
    /// gate for all BitGC profiles.
    pub fn new(
        local_role: BitGcRole,
        binding: BitGcSessionBinding,
    ) -> Result<Self, BitGcSessionError> {
        Self::new_inner(local_role, binding, None, None, None)
    }

    /// Start a session whose frames are authenticated by an externally
    /// established symmetric session key. The key is deliberately not
    /// derived from public binding fields; callers must obtain it from an
    /// authenticated channel/key exchange before entering BitGC.
    pub fn new_authenticated(
        local_role: BitGcRole,
        binding: BitGcSessionBinding,
        session_key: [u8; 32],
    ) -> Result<Self, BitGcSessionError> {
        Self::new_inner(local_role, binding, Some(session_key), None, None)
    }

    /// Start a session authenticated by a pre-shared key (PSK).
    ///
    /// The caller supplies a fresh, unpredictable 32-byte nonce. Each role
    /// sends a bounded PSK-authenticated hello; after both hellos the session
    /// key is derived from the PSK, binding, and role-ordered nonces. This
    /// provides authentication and key separation, but not forward secrecy;
    /// applications needing that property must use an external key exchange.
    pub fn new_psk_authenticated(
        local_role: BitGcRole,
        binding: BitGcSessionBinding,
        pre_shared_key: [u8; 32],
        local_nonce: [u8; 32],
    ) -> Result<Self, BitGcSessionError> {
        Self::new_inner(
            local_role,
            binding,
            None,
            Some(pre_shared_key),
            Some(local_nonce),
        )
    }

    /// Wrap a dedicated OT transport with this session's externally
    /// established authentication key and binding.
    ///
    /// Keeping construction on the authenticated session prevents callers
    /// from accidentally using a different key or public binding for OT.
    pub fn authenticated_ot_transport<T: BitGcOtTransport>(
        &self,
        transport: T,
    ) -> Result<AuthenticatedBitGcOtTransport<T>, BitGcSessionError> {
        let Some(key) = self.auth_key else {
            return Err(BitGcSessionError::MissingAuthentication);
        };
        Ok(AuthenticatedBitGcOtTransport::new(
            self.local_role,
            self.binding,
            key,
            transport,
        ))
    }

    fn psk_hello_payload(&self, key: &[u8; 32]) -> Result<Vec<u8>, BitGcSessionError> {
        let nonce = self
            .local_nonce
            .ok_or(BitGcSessionError::AuthenticationFailed)?;
        let tag = psk_hello_tag(key, &self.binding, self.local_role, &nonce);
        let mut payload = Vec::new();
        payload
            .try_reserve_exact(PSK_HELLO_BYTES)
            .map_err(|_| BitGcSessionError::Frame(FrameError::AllocationFailed))?;
        payload.extend_from_slice(PSK_HELLO_MAGIC);
        payload.extend_from_slice(&nonce);
        payload.extend_from_slice(&tag);
        Ok(payload)
    }

    fn accept_psk_hello(
        &mut self,
        peer_role: BitGcRole,
        payload: &[u8],
    ) -> Result<(), BitGcSessionError> {
        if payload.len() != PSK_HELLO_BYTES || &payload[..8] != PSK_HELLO_MAGIC {
            return Err(BitGcSessionError::AuthenticationFailed);
        }
        let nonce: &[u8; 32] = payload[8..40]
            .try_into()
            .map_err(|_| BitGcSessionError::AuthenticationFailed)?;
        let supplied: &[u8; AUTH_TAG_BYTES] = payload[40..]
            .try_into()
            .map_err(|_| BitGcSessionError::AuthenticationFailed)?;
        let key = self
            .psk_key
            .as_ref()
            .ok_or(BitGcSessionError::AuthenticationFailed)?;
        let expected = psk_hello_tag(key, &self.binding, peer_role, nonce);
        if !constant_time_eq(&expected, supplied) {
            return Err(BitGcSessionError::AuthenticationFailed);
        }
        let local_nonce = self
            .local_nonce
            .ok_or(BitGcSessionError::AuthenticationFailed)?;
        let (garbler_nonce, evaluator_nonce) = match self.local_role {
            BitGcRole::Garbler => (local_nonce, *nonce),
            BitGcRole::Evaluator => (*nonce, local_nonce),
        };
        self.auth_key = Some(derive_psk_session_key(
            key,
            &self.binding,
            &garbler_nonce,
            &evaluator_nonce,
        ));
        Ok(())
    }

    fn set_hello_digest(&mut self, role: BitGcRole, digest: [u8; 32]) {
        match role {
            BitGcRole::Garbler => self.garbler_hello_digest = digest,
            BitGcRole::Evaluator => self.evaluator_hello_digest = digest,
        }
    }

    fn frame_ot_context(&self) -> [u8; 32] {
        self.unified_transcript
    }

    fn new_inner(
        local_role: BitGcRole,
        binding: BitGcSessionBinding,
        auth_key: Option<[u8; 32]>,
        psk_key: Option<[u8; 32]>,
        local_nonce: Option<[u8; 32]>,
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
            auth_key,
            psk_key,
            local_nonce,
            unified_transcript: initial_unified_transcript(&binding),
            garbler_hello_digest: [0; 32],
            evaluator_hello_digest: [0; 32],
            last_sent_frame_digest: [0; 32],
            last_received_frame_digest: [0; 32],
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

    /// Send the sequence-zero hello frame.
    ///
    /// PSK-authenticated sessions carry a bounded nonce/MAC payload; plain
    /// and externally keyed sessions retain the empty legacy hello.
    pub fn hello(&mut self) -> Result<Vec<u8>, BitGcSessionError> {
        let payload = match self.psk_key {
            Some(key) => self.psk_hello_payload(&key)?,
            None => Vec::new(),
        };
        self.emit(BitGcFrameKind::Hello, &payload)
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
        let payload = match self.authenticated_payload(&frame) {
            Ok(payload) => payload,
            Err(error) => return self.abort_with(error),
        };
        let Some(next_sequence) = self.next_receive_sequence.checked_add(1) else {
            return self.abort_with(BitGcSessionError::SequenceExhausted);
        };

        match frame.kind {
            BitGcFrameKind::Hello => {
                if self.peer_hello_received || frame.sequence != 0 {
                    return self.abort_with(BitGcSessionError::UnexpectedKind);
                }
                if self.psk_key.is_some() {
                    if let Err(error) = self.accept_psk_hello(frame.role, payload) {
                        return self.abort_with(error);
                    }
                } else if !payload.is_empty() {
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
                if !payload.is_empty() {
                    return self.abort_with(BitGcSessionError::InvalidControlPayload);
                }
                self.state = BitGcSessionState::Cancelled;
            }
        }
        self.next_receive_sequence = next_sequence;
        self.last_received_frame_digest = frame_digest(encoded);
        if matches!(frame.kind, BitGcFrameKind::Hello) {
            self.set_hello_digest(frame.role, self.last_received_frame_digest);
            if self.local_hello_sent {
                self.unified_transcript = unified_handshake_transcript(
                    &self.binding,
                    self.garbler_hello_digest,
                    self.evaluator_hello_digest,
                );
            }
        } else {
            self.unified_transcript =
                unified_transcript_after(&self.unified_transcript, self.last_received_frame_digest);
        }
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
        let encoded = match self.emit(kind, payload) {
            Ok(encoded) => encoded,
            Err(error) => {
                let _ = transport.cancel();
                return Err(BitGcIoError::Protocol(error));
            }
        };
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

    /// Exchange hello frames over a bidirectional transport: send the local
    /// hello, then receive and validate the peer hello. Failures abort the
    /// session and best-effort cancel the transport.
    pub fn handshake_over<T: BitGcFrameTransport>(
        &mut self,
        transport: &mut T,
    ) -> Result<(), BitGcIoError<T::Error>> {
        let encoded = match self.hello() {
            Ok(encoded) => encoded,
            Err(error) => {
                let _ = transport.cancel();
                return Err(BitGcIoError::Protocol(error));
            }
        };
        if let Err(error) = transport.send_frame(&encoded) {
            self.state = BitGcSessionState::Aborted;
            let _ = transport.cancel();
            return Err(BitGcIoError::Transport(error));
        }
        self.receive_over(transport)?;
        Ok(())
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

    /// Transfer one evaluator-owned input's false/true labels by 1-of-2 OT.
    ///
    /// `ring_degree` is the profile's exact polynomial dimension. Labels are
    /// serialized as canonical little-endian coefficients plus a masked-bit
    /// byte, then split into `N::USIZE`-byte OT chunks. Each OT is
    /// preceded by a session-bound frame naming the input and chunk ordinal.
    /// The choice bit is used only by the receiver's OT operation and is never
    /// serialized into a BitGC frame.
    pub fn send_evaluator_input_over<N, C, T>(
        &mut self,
        transport: &mut T,
        ot: &mut C,
        input_index: u64,
        plaintext_modulus: u64,
        ring_degree: usize,
        label_pair: &[(LabelPoly, bool); 2],
    ) -> Result<(), BitGcIoError<T::Error>>
    where
        N: VoleArray<u8>,
        C: BitGcInputOt<N>,
        T: BitGcFrameTransport,
    {
        if self.local_role != BitGcRole::Garbler {
            return self.abort_ot_over(transport, ot, BitGcSessionError::WrongRole);
        }
        let branch0 = match encode_input_branch(
            &label_pair[0].0,
            label_pair[0].1,
            plaintext_modulus,
            ring_degree,
        ) {
            Ok(bytes) => bytes,
            Err(error) => return self.abort_ot_over(transport, ot, error),
        };
        let branch1 = match encode_input_branch(
            &label_pair[1].0,
            label_pair[1].1,
            plaintext_modulus,
            ring_degree,
        ) {
            Ok(bytes) => bytes,
            Err(error) => return self.abort_ot_over(transport, ot, error),
        };
        if branch0.len() != branch1.len() || N::USIZE == 0 {
            return self.abort_ot_over(transport, ot, BitGcSessionError::InvalidInputMaterial);
        }
        let chunk_count = match chunk_count(branch0.len(), N::USIZE) {
            Some(count) if count <= u32::MAX as usize => count,
            _ => return self.abort_ot_over(transport, ot, BitGcSessionError::InvalidInputMaterial),
        };
        let total_bytes = branch0.len() as u32;
        let chunk_count_u32 = chunk_count as u32;

        for chunk_index in 0..chunk_count {
            let descriptor = encode_input_chunk_descriptor(
                input_index,
                chunk_index as u32,
                chunk_count_u32,
                total_bytes,
            );
            if let Err(error) = self.send_over(transport, BitGcFrameKind::Data, &descriptor) {
                return self.cancel_ot_after_io_error(transport, ot, error);
            }
            let context = self.frame_ot_context();
            ot.bind_context(&context);
            let start = chunk_index * N::USIZE;
            let block0 =
                Array::<u8, N>::from_fn(|index| branch0.get(start + index).copied().unwrap_or(0));
            let block1 =
                Array::<u8, N>::from_fn(|index| branch1.get(start + index).copied().unwrap_or(0));
            if ot.send_pair([&block0, &block1]).is_err() {
                return self.abort_ot_over(transport, ot, BitGcSessionError::OtFailure);
            }
            if let Some(transcript) = ot.transcript() {
                self.unified_transcript = transcript;
            }
        }
        Ok(())
    }

    /// Receive one evaluator-owned input label through the matching 1-of-2
    /// OT. `choice` is local-only and never enters the session frame.
    pub fn receive_evaluator_input_over<N, C, T>(
        &mut self,
        transport: &mut T,
        ot: &mut C,
        expected_input_index: u64,
        plaintext_modulus: u64,
        ring_degree: usize,
        choice: bool,
    ) -> Result<(LabelPoly, bool), BitGcIoError<T::Error>>
    where
        N: VoleArray<u8>,
        C: BitGcInputOt<N>,
        T: BitGcFrameTransport,
    {
        if self.local_role != BitGcRole::Evaluator {
            return self.abort_ot_over(transport, ot, BitGcSessionError::WrongRole);
        }
        if plaintext_modulus <= 2 || plaintext_modulus & 1 == 0 || ring_degree == 0 || N::USIZE == 0
        {
            return self.abort_ot_over(transport, ot, BitGcSessionError::InvalidInputMaterial);
        }
        let (first, first_context) = match self.receive_input_chunk_descriptor(transport) {
            Ok(descriptor) => descriptor,
            Err(error) => return self.cancel_ot_after_io_error(transport, ot, error),
        };
        if first.input_index != expected_input_index || first.chunk_index != 0 {
            return self.abort_ot_over(transport, ot, BitGcSessionError::InvalidInputDescriptor);
        }
        let total_bytes = first.total_bytes as usize;
        let expected_chunks = match chunk_count(total_bytes, N::USIZE) {
            Some(count) if count <= u32::MAX as usize => count,
            _ => {
                return self.abort_ot_over(
                    transport,
                    ot,
                    BitGcSessionError::InvalidInputDescriptor,
                );
            }
        };
        if total_bytes < 9
            || total_bytes > BITGC_OT_MAX_INPUT_BYTES
            || ring_degree
                .checked_mul(8)
                .and_then(|bytes| bytes.checked_add(1))
                != Some(total_bytes)
            || (total_bytes - 1) % 8 != 0
            || first.chunk_count as usize != expected_chunks
            || expected_chunks == 0
        {
            return self.abort_ot_over(transport, ot, BitGcSessionError::InvalidInputDescriptor);
        }
        let mut encoded_label = Vec::new();
        if encoded_label.try_reserve_exact(total_bytes).is_err() {
            return self.abort_ot_over(transport, ot, BitGcSessionError::InvalidInputMaterial);
        }

        for chunk_index in 0..expected_chunks {
            let (descriptor, context) = if chunk_index == 0 {
                (first, first_context)
            } else {
                match self.receive_input_chunk_descriptor(transport) {
                    Ok(descriptor) => descriptor,
                    Err(error) => return self.cancel_ot_after_io_error(transport, ot, error),
                }
            };
            if descriptor.input_index != expected_input_index
                || descriptor.chunk_index as usize != chunk_index
                || descriptor.chunk_count as usize != expected_chunks
                || descriptor.total_bytes as usize != total_bytes
            {
                return self.abort_ot_over(
                    transport,
                    ot,
                    BitGcSessionError::InvalidInputDescriptor,
                );
            }
            ot.bind_context(&context);
            let block = match ot.receive_choice(choice) {
                Ok(block) => block,
                Err(_) => return self.abort_ot_over(transport, ot, BitGcSessionError::OtFailure),
            };
            if let Some(transcript) = ot.transcript() {
                self.unified_transcript = transcript;
            }
            let remaining = total_bytes - encoded_label.len();
            let take = core::cmp::min(N::USIZE, remaining);
            if block.as_slice()[take..].iter().any(|&byte| byte != 0) {
                return self.abort_ot_over(transport, ot, BitGcSessionError::InvalidInputMaterial);
            }
            encoded_label.extend_from_slice(&block.as_slice()[..take]);
        }
        match decode_input_branch(&encoded_label, plaintext_modulus, ring_degree) {
            Ok(label) => Ok(label),
            Err(error) => self.abort_ot_over(transport, ot, error),
        }
    }

    /// Send domain-separated hashes of both clear-value output labels. The
    /// label pair itself remains garbler-side; only the ordered hashes are
    /// sent. This provides output-label consistency opening, not transport
    /// authentication or active security. `ring_degree` must match the fixed
    /// profile dimension.
    pub fn send_output_commitments_over<T: BitGcFrameTransport>(
        &mut self,
        transport: &mut T,
        garbler_labels: &GarblerWireLabels,
        plaintext_modulus: u64,
        ring_degree: usize,
    ) -> Result<(), BitGcIoError<T::Error>> {
        if self.local_role != BitGcRole::Garbler {
            return self.abort_over(transport, BitGcSessionError::WrongRole);
        }
        if plaintext_modulus <= 2
            || plaintext_modulus & 1 == 0
            || ring_degree == 0
            || garbler_labels.output_count() > BITGC_OUTPUT_MAX_COUNT
        {
            return self.abort_over(transport, BitGcSessionError::InvalidOutputCommitment);
        }
        let output_count_u32 = match u32::try_from(garbler_labels.output_count()) {
            Ok(count) => count,
            Err(_) => {
                return self.abort_over(transport, BitGcSessionError::InvalidOutputCommitment);
            }
        };
        for output_index in 0..garbler_labels.output_count() {
            let (wire, pair) = match garbler_labels.output_label_pair(output_index) {
                Ok(output) => output,
                Err(_) => {
                    return self.abort_over(transport, BitGcSessionError::InvalidOutputCommitment);
                }
            };
            let output_index_u32 = match u32::try_from(output_index) {
                Ok(index) => index,
                Err(_) => {
                    return self.abort_over(transport, BitGcSessionError::InvalidOutputCommitment);
                }
            };
            let wire_u64 = match u64::try_from(wire) {
                Ok(wire) => wire,
                Err(_) => {
                    return self.abort_over(transport, BitGcSessionError::InvalidOutputCommitment);
                }
            };
            let hash0 = match output_label_hash(
                &self.binding,
                output_index as u64,
                wire_u64,
                &pair[0].0,
                plaintext_modulus,
                ring_degree,
            ) {
                Ok(hash) => hash,
                Err(error) => return self.abort_over(transport, error),
            };
            let hash1 = match output_label_hash(
                &self.binding,
                output_index as u64,
                wire_u64,
                &pair[1].0,
                plaintext_modulus,
                ring_degree,
            ) {
                Ok(hash) => hash,
                Err(error) => return self.abort_over(transport, error),
            };
            let mut payload = [0; OUTPUT_COMMIT_BYTES];
            payload[0] = OUTPUT_COMMIT_TAG;
            payload[1..5].copy_from_slice(&output_count_u32.to_le_bytes());
            payload[5..9].copy_from_slice(&output_index_u32.to_le_bytes());
            payload[9..17].copy_from_slice(&wire_u64.to_le_bytes());
            payload[17..49].copy_from_slice(&hash0);
            payload[49..81].copy_from_slice(&hash1);
            self.send_over(transport, BitGcFrameKind::Data, &payload)?;
        }
        Ok(())
    }

    /// Match evaluator output labels against the garbler's committed pair and
    /// return clear output bits without opening permutation bits.
    pub fn receive_committed_outputs_over<T: BitGcFrameTransport>(
        &mut self,
        transport: &mut T,
        outputs: &[(LabelPoly, bool)],
        output_wires: &[usize],
        plaintext_modulus: u64,
        ring_degree: usize,
    ) -> Result<Vec<bool>, BitGcIoError<T::Error>> {
        if self.local_role != BitGcRole::Evaluator {
            return self.abort_over(transport, BitGcSessionError::WrongRole);
        }
        if plaintext_modulus <= 2
            || plaintext_modulus & 1 == 0
            || ring_degree == 0
            || outputs.len() != output_wires.len()
            || outputs.len() > BITGC_OUTPUT_MAX_COUNT
        {
            return self.abort_over(transport, BitGcSessionError::InvalidOutputCommitment);
        }
        let mut clear_outputs = Vec::new();
        if clear_outputs.try_reserve_exact(outputs.len()).is_err() {
            return self.abort_over(transport, BitGcSessionError::InvalidOutputCommitment);
        }
        for (output_index, ((label, _masked), &wire)) in
            outputs.iter().zip(output_wires).enumerate()
        {
            let encoded = self.receive_over(transport)?;
            let frame = match BitGcFrame::decode(&encoded) {
                Ok(frame) if frame.kind == BitGcFrameKind::Data => frame,
                _ => return self.abort_over(transport, BitGcSessionError::InvalidOutputCommitment),
            };
            let payload = match self.payload_without_auth(&frame) {
                Ok(payload) => payload,
                Err(error) => return self.abort_over(transport, error),
            };
            if payload.len() != OUTPUT_COMMIT_BYTES || payload[0] != OUTPUT_COMMIT_TAG {
                return self.abort_over(transport, BitGcSessionError::InvalidOutputCommitment);
            }
            let committed_count =
                u32::from_le_bytes([payload[1], payload[2], payload[3], payload[4]]);
            let committed_index =
                u32::from_le_bytes([payload[5], payload[6], payload[7], payload[8]]);
            let committed_wire = u64::from_le_bytes([
                payload[9],
                payload[10],
                payload[11],
                payload[12],
                payload[13],
                payload[14],
                payload[15],
                payload[16],
            ]);
            let expected_wire = match u64::try_from(wire) {
                Ok(wire) => wire,
                Err(_) => {
                    return self.abort_over(transport, BitGcSessionError::InvalidOutputCommitment);
                }
            };
            if committed_count as usize != outputs.len()
                || committed_index as usize != output_index
                || committed_wire != expected_wire
            {
                return self.abort_over(transport, BitGcSessionError::InvalidOutputCommitment);
            }
            let label_hash = match output_label_hash(
                &self.binding,
                output_index as u64,
                committed_wire,
                label,
                plaintext_modulus,
                ring_degree,
            ) {
                Ok(hash) => hash,
                Err(error) => return self.abort_over(transport, error),
            };
            let hash0: &[u8; 32] = match payload[17..49].try_into() {
                Ok(hash) => hash,
                Err(_) => {
                    return self.abort_over(transport, BitGcSessionError::InvalidOutputCommitment);
                }
            };
            let hash1: &[u8; 32] = match payload[49..81].try_into() {
                Ok(hash) => hash,
                Err(_) => {
                    return self.abort_over(transport, BitGcSessionError::InvalidOutputCommitment);
                }
            };
            let matches0 = constant_time_eq(&label_hash, hash0);
            let matches1 = constant_time_eq(&label_hash, hash1);
            if matches0 == matches1 {
                return self.abort_over(transport, BitGcSessionError::OutputLabelMismatch);
            }
            clear_outputs.push(matches1);
        }
        Ok(clear_outputs)
    }

    /// Deliver one evaluator-material package (evaluator setup material
    /// followed by canonical [`RoleEvaluatorTables`][tables] bytes) to the
    /// evaluator as bounded Data frames.
    ///
    /// [tables]: volar_spec::bitgc::pinned::RoleEvaluatorTables
    ///
    /// Chunks are `[tag][u64 total_bytes][u32 chunk_index][u32 chunk_count][chunk]`
    /// frames sent after hello and before input OTs; because they are Data
    /// frames, they advance the unified frame/OT transcript, so the material
    /// is transcript-bound (and authenticated when the session has a key).
    /// The garbler role sends, the evaluator role receives; any mismatch,
    /// reorder, or bound violation aborts both the frame transport and the
    /// session.
    pub fn send_material_over<T: BitGcFrameTransport>(
        &mut self,
        transport: &mut T,
        material: &[u8],
    ) -> Result<(), BitGcIoError<T::Error>> {
        if self.local_role != BitGcRole::Garbler {
            return self.abort_over(transport, BitGcSessionError::WrongRole);
        }
        if material.is_empty() || material.len() > BITGC_MATERIAL_MAX_BYTES {
            return self.abort_over(transport, BitGcSessionError::InvalidMaterial);
        }
        let chunk_capacity = BITGC_FRAME_MAX_PAYLOAD
            .saturating_sub(MATERIAL_CHUNK_HEADER_BYTES)
            .saturating_sub(AUTH_TAG_BYTES);
        let chunk_count = match chunk_count(material.len(), chunk_capacity) {
            Some(count) if count <= u32::MAX as usize => count as u32,
            _ => return self.abort_over(transport, BitGcSessionError::InvalidMaterial),
        };
        let total_bytes = material.len() as u64;
        for chunk_index in 0..chunk_count {
            let start = chunk_index as usize * chunk_capacity;
            let end = core::cmp::min(start + chunk_capacity, material.len());
            let chunk = &material[start..end];
            let mut payload = Vec::new();
            if payload
                .try_reserve_exact(MATERIAL_CHUNK_HEADER_BYTES + chunk.len())
                .is_err()
            {
                return self.abort_over(
                    transport,
                    BitGcSessionError::Frame(FrameError::AllocationFailed),
                );
            }
            payload.push(MATERIAL_CHUNK_TAG);
            payload.extend_from_slice(&total_bytes.to_le_bytes());
            payload.extend_from_slice(&chunk_index.to_le_bytes());
            payload.extend_from_slice(&chunk_count.to_le_bytes());
            payload.extend_from_slice(chunk);
            self.send_over(transport, BitGcFrameKind::Data, &payload)?;
        }
        Ok(())
    }

    /// Receive one evaluator-material package delivered by
    /// [`Self::send_material_over`], enforcing order, count, total, and the
    /// [`BITGC_MATERIAL_MAX_BYTES`] bound before allocation.
    pub fn receive_material_over<T: BitGcFrameTransport>(
        &mut self,
        transport: &mut T,
    ) -> Result<Vec<u8>, BitGcIoError<T::Error>> {
        if self.local_role != BitGcRole::Evaluator {
            return self.abort_over(transport, BitGcSessionError::WrongRole);
        }
        let mut material: Option<(Vec<u8>, u64, u32)> = None;
        let mut next_index = 0u32;
        loop {
            let encoded = self.receive_over(transport)?;
            let frame = match BitGcFrame::decode(&encoded) {
                Ok(frame) if frame.kind == BitGcFrameKind::Data => frame,
                _ => return self.abort_over(transport, BitGcSessionError::InvalidMaterial),
            };
            let payload = match self.payload_without_auth(&frame) {
                Ok(payload) => payload,
                Err(error) => return self.abort_over(transport, error),
            };
            if payload.len() < MATERIAL_CHUNK_HEADER_BYTES
                || payload[0] != MATERIAL_CHUNK_TAG
                || payload.len() > MATERIAL_CHUNK_HEADER_BYTES + BITGC_FRAME_MAX_PAYLOAD
            {
                return self.abort_over(transport, BitGcSessionError::InvalidMaterial);
            }
            let total_bytes = u64::from_le_bytes(
                payload[1..9]
                    .try_into()
                    .map_err(|_| BitGcIoError::Protocol(BitGcSessionError::InvalidMaterial))?,
            );
            let chunk_index = u32::from_le_bytes(
                payload[9..13]
                    .try_into()
                    .map_err(|_| BitGcIoError::Protocol(BitGcSessionError::InvalidMaterial))?,
            );
            let chunk_count = u32::from_le_bytes(
                payload[13..17]
                    .try_into()
                    .map_err(|_| BitGcIoError::Protocol(BitGcSessionError::InvalidMaterial))?,
            );
            let body = &payload[MATERIAL_CHUNK_HEADER_BYTES..];
            if chunk_count == 0 || chunk_index >= chunk_count || total_bytes == 0 {
                return self.abort_over(transport, BitGcSessionError::InvalidMaterial);
            }
            if total_bytes > BITGC_MATERIAL_MAX_BYTES as u64 {
                return self.abort_over(transport, BitGcSessionError::InvalidMaterial);
            }
            if chunk_index != next_index {
                return self.abort_over(transport, BitGcSessionError::InvalidMaterial);
            }
            if material.is_none() {
                let total = total_bytes as usize;
                let mut buffer = Vec::new();
                if buffer.try_reserve_exact(total).is_err() {
                    return self.abort_over(
                        transport,
                        BitGcSessionError::Frame(FrameError::AllocationFailed),
                    );
                }
                material = Some((buffer, total_bytes, chunk_count));
            }
            let (buffer, expected_total, expected_count) = match material.as_mut() {
                Some(state) => state,
                None => return self.abort_over(transport, BitGcSessionError::InvalidMaterial),
            };
            if *expected_total != total_bytes || *expected_count != chunk_count {
                return self.abort_over(transport, BitGcSessionError::InvalidMaterial);
            }
            buffer.extend_from_slice(body);
            next_index = next_index.saturating_add(1);
            if next_index == chunk_count {
                let (buffer, expected_total, _) = match material.take() {
                    Some(state) => state,
                    None => {
                        return self.abort_over(transport, BitGcSessionError::InvalidMaterial);
                    }
                };
                if buffer.len() as u64 != expected_total {
                    return self.abort_over(transport, BitGcSessionError::InvalidMaterial);
                }
                return Ok(buffer);
            }
        }
    }

    /// Deliver one garbler-owned input's active label directly to the
    /// evaluator.
    ///
    /// Garbler-owned inputs need no OT: the garbler knows the bit, so it
    /// sends the active label and its masked bit in one Data frame. The
    /// evaluator uses the label as the wire's evaluation input. Frames carry
    /// `[tag][u64 input_index][u64 byte_len][label bytes][u8 masked]`.
    pub fn send_garbler_input_over<T: BitGcFrameTransport>(
        &mut self,
        transport: &mut T,
        input_index: u64,
        plaintext_modulus: u64,
        ring_degree: usize,
        label: &(LabelPoly, bool),
    ) -> Result<(), BitGcIoError<T::Error>> {
        if self.local_role != BitGcRole::Garbler {
            return self.abort_over(transport, BitGcSessionError::WrongRole);
        }
        let encoded = match encode_input_branch(&label.0, label.1, plaintext_modulus, ring_degree) {
            Ok(encoded) => encoded,
            Err(error) => return self.abort_over(transport, error),
        };
        let mut payload = Vec::new();
        if payload
            .try_reserve_exact(GARBLER_INPUT_HEADER_BYTES + encoded.len())
            .is_err()
        {
            return self.abort_over(
                transport,
                BitGcSessionError::Frame(FrameError::AllocationFailed),
            );
        }
        payload.push(GARBLER_INPUT_TAG);
        payload.extend_from_slice(&input_index.to_le_bytes());
        payload.extend_from_slice(&(encoded.len() as u64).to_le_bytes());
        payload.extend_from_slice(&encoded);
        self.send_over(transport, BitGcFrameKind::Data, &payload)
    }

    /// Receive one garbler-owned input label delivered by
    /// [`Self::send_garbler_input_over`], binding it to the expected input
    /// index and profile shape.
    pub fn receive_garbler_input_over<T: BitGcFrameTransport>(
        &mut self,
        transport: &mut T,
        expected_input_index: u64,
        plaintext_modulus: u64,
        ring_degree: usize,
    ) -> Result<(LabelPoly, bool), BitGcIoError<T::Error>> {
        if self.local_role != BitGcRole::Evaluator {
            return self.abort_over(transport, BitGcSessionError::WrongRole);
        }
        let encoded = self.receive_over(transport)?;
        let frame = match BitGcFrame::decode(&encoded) {
            Ok(frame) if frame.kind == BitGcFrameKind::Data => frame,
            _ => return self.abort_over(transport, BitGcSessionError::InvalidGarblerInput),
        };
        let payload = match self.payload_without_auth(&frame) {
            Ok(payload) => payload,
            Err(error) => return self.abort_over(transport, error),
        };
        if payload.len() < GARBLER_INPUT_HEADER_BYTES || payload[0] != GARBLER_INPUT_TAG {
            return self.abort_over(transport, BitGcSessionError::InvalidGarblerInput);
        }
        let input_index = u64::from_le_bytes(
            payload[1..9]
                .try_into()
                .map_err(|_| BitGcIoError::Protocol(BitGcSessionError::InvalidGarblerInput))?,
        );
        let byte_len = u64::from_le_bytes(
            payload[9..17]
                .try_into()
                .map_err(|_| BitGcIoError::Protocol(BitGcSessionError::InvalidGarblerInput))?,
        );
        let body = &payload[GARBLER_INPUT_HEADER_BYTES..];
        if input_index != expected_input_index || body.len() as u64 != byte_len {
            return self.abort_over(transport, BitGcSessionError::InvalidGarblerInput);
        }
        match decode_input_branch(body, plaintext_modulus, ring_degree) {
            Ok(label) => Ok(label),
            Err(error) => self.abort_over(transport, error),
        }
    }

    fn receive_input_chunk_descriptor<T: BitGcFrameTransport>(
        &mut self,
        transport: &mut T,
    ) -> Result<(InputChunkDescriptor, [u8; 32]), BitGcIoError<T::Error>> {
        let encoded = self.receive_over(transport)?;
        let frame = match BitGcFrame::decode(&encoded) {
            Ok(frame) if frame.kind == BitGcFrameKind::Data => frame,
            _ => return self.abort_over(transport, BitGcSessionError::InvalidInputDescriptor),
        };
        let payload = match self.payload_without_auth(&frame) {
            Ok(payload) => payload,
            Err(error) => return self.abort_over(transport, error),
        };
        match decode_input_chunk_descriptor(payload) {
            Some(descriptor) => Ok((descriptor, self.frame_ot_context())),
            None => self.abort_over(transport, BitGcSessionError::InvalidInputDescriptor),
        }
    }

    fn abort_ot_over<N, C, T, R>(
        &mut self,
        transport: &mut T,
        ot: &mut C,
        error: BitGcSessionError,
    ) -> Result<R, BitGcIoError<T::Error>>
    where
        N: VoleArray<u8>,
        C: BitGcInputOt<N>,
        T: BitGcFrameTransport,
    {
        self.state = BitGcSessionState::Aborted;
        let ot_cancel_failed = ot.cancel().is_err();
        if let Err(transport_error) = transport.cancel() {
            return Err(BitGcIoError::Transport(transport_error));
        }
        if ot_cancel_failed {
            return Err(BitGcIoError::Protocol(
                BitGcSessionError::OtCancellationFailure,
            ));
        }
        Err(BitGcIoError::Protocol(error))
    }

    fn cancel_ot_after_io_error<N, C, T, R>(
        &mut self,
        transport: &mut T,
        ot: &mut C,
        error: BitGcIoError<T::Error>,
    ) -> Result<R, BitGcIoError<T::Error>>
    where
        N: VoleArray<u8>,
        C: BitGcInputOt<N>,
        T: BitGcFrameTransport,
    {
        self.state = BitGcSessionState::Aborted;
        let ot_cancel_failed = ot.cancel().is_err();
        if let Err(transport_error) = transport.cancel() {
            return Err(BitGcIoError::Transport(transport_error));
        }
        if ot_cancel_failed {
            return Err(BitGcIoError::Protocol(
                BitGcSessionError::OtCancellationFailure,
            ));
        }
        Err(error)
    }

    fn abort_over<T: BitGcFrameTransport, R>(
        &mut self,
        transport: &mut T,
        error: BitGcSessionError,
    ) -> Result<R, BitGcIoError<T::Error>> {
        self.state = BitGcSessionState::Aborted;
        if let Err(transport_error) = transport.cancel() {
            return Err(BitGcIoError::Transport(transport_error));
        }
        Err(BitGcIoError::Protocol(error))
    }

    fn emit(&mut self, kind: BitGcFrameKind, payload: &[u8]) -> Result<Vec<u8>, BitGcSessionError> {
        if is_terminal(self.state) {
            return Err(BitGcSessionError::Terminal);
        }
        let valid = match kind {
            BitGcFrameKind::Hello => {
                !self.local_hello_sent
                    && self.next_send_sequence == 0
                    && ((self.psk_key.is_some() && payload.len() == PSK_HELLO_BYTES)
                        || (self.psk_key.is_none() && payload.is_empty()))
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
        let sequence = self.next_send_sequence;
        let mut authenticated = Vec::new();
        let authenticate = self.auth_key.is_some()
            && !(matches!(kind, BitGcFrameKind::Hello) && self.psk_key.is_some());
        let wire_payload = if let Some(key) = self.auth_key.as_ref().filter(|_| authenticate) {
            if payload.len() > BITGC_FRAME_MAX_PAYLOAD - AUTH_TAG_BYTES {
                return self.abort_with(BitGcSessionError::Frame(FrameError::PayloadTooLarge));
            }
            if authenticated
                .try_reserve_exact(payload.len() + AUTH_TAG_BYTES)
                .is_err()
            {
                return self.abort_with(BitGcSessionError::Frame(FrameError::AllocationFailed));
            }
            authenticated.extend_from_slice(payload);
            let tag = match frame_auth_tag(
                key,
                &self.unified_transcript,
                self.local_role,
                &self.binding,
                sequence,
                kind,
                payload,
            ) {
                Ok(tag) => tag,
                Err(()) => return self.abort_with(BitGcSessionError::AuthenticationFailed),
            };
            authenticated.extend_from_slice(&tag);
            authenticated.as_slice()
        } else {
            payload
        };
        let frame = BitGcFrame {
            role: self.local_role,
            binding: self.binding,
            sequence,
            kind,
            payload: wire_payload,
        };
        let encoded = match frame.encode() {
            Ok(encoded) => encoded,
            Err(error) => return self.abort_with(BitGcSessionError::Frame(error)),
        };
        self.next_send_sequence = next_sequence;
        self.last_sent_frame_digest = frame_digest(&encoded);
        if matches!(kind, BitGcFrameKind::Hello) {
            self.set_hello_digest(self.local_role, self.last_sent_frame_digest);
            if self.peer_hello_received {
                self.unified_transcript = unified_handshake_transcript(
                    &self.binding,
                    self.garbler_hello_digest,
                    self.evaluator_hello_digest,
                );
            }
        } else {
            self.unified_transcript =
                unified_transcript_after(&self.unified_transcript, self.last_sent_frame_digest);
        }
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

    fn authenticated_payload<'a>(
        &self,
        frame: &BitGcFrame<'a>,
    ) -> Result<&'a [u8], BitGcSessionError> {
        if matches!(frame.kind, BitGcFrameKind::Hello) && self.psk_key.is_some() {
            return Ok(frame.payload);
        }
        let Some(key) = self.auth_key.as_ref() else {
            return Ok(frame.payload);
        };
        if frame.payload.len() < AUTH_TAG_BYTES {
            return Err(BitGcSessionError::MissingAuthentication);
        }
        let split = frame.payload.len() - AUTH_TAG_BYTES;
        let payload = &frame.payload[..split];
        let supplied: &[u8; AUTH_TAG_BYTES] = frame.payload[split..]
            .try_into()
            .map_err(|_| BitGcSessionError::MissingAuthentication)?;
        let expected = frame_auth_tag(
            key,
            &self.unified_transcript,
            frame.role,
            &frame.binding,
            frame.sequence,
            frame.kind,
            payload,
        )
        .map_err(|_| BitGcSessionError::AuthenticationFailed)?;
        if !constant_time_eq(&expected, supplied) {
            return Err(BitGcSessionError::AuthenticationFailed);
        }
        Ok(payload)
    }

    /// Return the application payload after [`Self::accept`] has verified the
    /// frame's binding, sequence, and authentication tag.
    pub fn payload_without_auth<'a>(
        &self,
        frame: &BitGcFrame<'a>,
    ) -> Result<&'a [u8], BitGcSessionError> {
        if self.auth_key.is_some() {
            let split = frame
                .payload
                .len()
                .checked_sub(AUTH_TAG_BYTES)
                .ok_or(BitGcSessionError::MissingAuthentication)?;
            Ok(&frame.payload[..split])
        } else {
            Ok(frame.payload)
        }
    }

    fn abort_with<T>(&mut self, error: BitGcSessionError) -> Result<T, BitGcSessionError> {
        self.state = BitGcSessionState::Aborted;
        Err(error)
    }
}

#[derive(Clone, Copy)]
struct InputChunkDescriptor {
    input_index: u64,
    chunk_index: u32,
    chunk_count: u32,
    total_bytes: u32,
}

fn chunk_count(total_bytes: usize, chunk_bytes: usize) -> Option<usize> {
    if chunk_bytes == 0 {
        return None;
    }
    total_bytes
        .checked_add(chunk_bytes - 1)
        .map(|rounded| rounded / chunk_bytes)
}

fn encode_input_chunk_descriptor(
    input_index: u64,
    chunk_index: u32,
    chunk_count: u32,
    total_bytes: u32,
) -> [u8; INPUT_CHUNK_HEADER_BYTES] {
    let mut payload = [0; INPUT_CHUNK_HEADER_BYTES];
    payload[0] = INPUT_CHUNK_TAG;
    payload[1..9].copy_from_slice(&input_index.to_le_bytes());
    payload[9..13].copy_from_slice(&chunk_index.to_le_bytes());
    payload[13..17].copy_from_slice(&chunk_count.to_le_bytes());
    payload[17..21].copy_from_slice(&total_bytes.to_le_bytes());
    payload
}

fn decode_input_chunk_descriptor(payload: &[u8]) -> Option<InputChunkDescriptor> {
    if payload.len() != INPUT_CHUNK_HEADER_BYTES || payload[0] != INPUT_CHUNK_TAG {
        return None;
    }
    Some(InputChunkDescriptor {
        input_index: u64::from_le_bytes(payload[1..9].try_into().ok()?),
        chunk_index: u32::from_le_bytes(payload[9..13].try_into().ok()?),
        chunk_count: u32::from_le_bytes(payload[13..17].try_into().ok()?),
        total_bytes: u32::from_le_bytes(payload[17..21].try_into().ok()?),
    })
}

fn frame_digest(encoded: &[u8]) -> [u8; 32] {
    let mut hash = Sha256::new();
    hash.update(FRAME_DIGEST_DOMAIN);
    hash.update(encoded);
    let digest = hash.finalize();
    let mut result = [0; 32];
    result.copy_from_slice(&digest);
    result
}

fn initial_unified_transcript(binding: &BitGcSessionBinding) -> [u8; 32] {
    let mut hash = Sha256::new();
    hash.update(UNIFIED_TRANSCRIPT_DOMAIN);
    hash.update(binding.session_id);
    hash.update([binding.variant as u8]);
    hash.update(binding.parameter_fingerprint);
    hash.update(binding.circuit_digest);
    let digest = hash.finalize();
    let mut result = [0; 32];
    result.copy_from_slice(&digest);
    result
}

fn unified_handshake_transcript(
    binding: &BitGcSessionBinding,
    garbler_hello_digest: [u8; 32],
    evaluator_hello_digest: [u8; 32],
) -> [u8; 32] {
    let mut hash = Sha256::new();
    hash.update(FRAME_OT_CONTEXT_DOMAIN);
    hash.update(binding.session_id);
    hash.update([binding.variant as u8]);
    hash.update(binding.parameter_fingerprint);
    hash.update(binding.circuit_digest);
    hash.update(garbler_hello_digest);
    hash.update(evaluator_hello_digest);
    let digest = hash.finalize();
    let mut result = [0; 32];
    result.copy_from_slice(&digest);
    result
}

fn unified_transcript_after(previous: &[u8; 32], encoded_frame_digest: [u8; 32]) -> [u8; 32] {
    let mut hash = Sha256::new();
    hash.update(UNIFIED_TRANSCRIPT_DOMAIN);
    hash.update(previous);
    hash.update(encoded_frame_digest);
    let digest = hash.finalize();
    let mut result = [0; 32];
    result.copy_from_slice(&digest);
    result
}

fn encode_input_branch(
    label: &LabelPoly,
    masked: bool,
    plaintext_modulus: u64,
    ring_degree: usize,
) -> Result<Vec<u8>, BitGcSessionError> {
    if plaintext_modulus <= 2
        || plaintext_modulus & 1 == 0
        || ring_degree == 0
        || label.len() != ring_degree
        || label
            .iter()
            .any(|&coefficient| coefficient >= plaintext_modulus)
    {
        return Err(BitGcSessionError::InvalidInputMaterial);
    }
    let total_bytes = label
        .len()
        .checked_mul(8)
        .and_then(|bytes| bytes.checked_add(1))
        .ok_or(BitGcSessionError::InvalidInputMaterial)?;
    if total_bytes > BITGC_OT_MAX_INPUT_BYTES {
        return Err(BitGcSessionError::InvalidInputMaterial);
    }
    let mut encoded = Vec::new();
    encoded
        .try_reserve_exact(total_bytes)
        .map_err(|_| BitGcSessionError::InvalidInputMaterial)?;
    for coefficient in label {
        encoded.extend_from_slice(&coefficient.to_le_bytes());
    }
    encoded.push(u8::from(masked));
    Ok(encoded)
}

fn decode_input_branch(
    encoded: &[u8],
    plaintext_modulus: u64,
    ring_degree: usize,
) -> Result<(LabelPoly, bool), BitGcSessionError> {
    if plaintext_modulus <= 2
        || plaintext_modulus & 1 == 0
        || ring_degree == 0
        || encoded.len() < 9
        || encoded.len() > BITGC_OT_MAX_INPUT_BYTES
        || (encoded.len() - 1) % 8 != 0
        || (encoded.len() - 1) / 8 != ring_degree
        || encoded[encoded.len() - 1] > 1
    {
        return Err(BitGcSessionError::InvalidInputMaterial);
    }
    let degree = (encoded.len() - 1) / 8;
    let mut label = Vec::new();
    label
        .try_reserve_exact(degree)
        .map_err(|_| BitGcSessionError::InvalidInputMaterial)?;
    for bytes in encoded[..encoded.len() - 1].chunks_exact(8) {
        let coefficient = u64::from_le_bytes(
            bytes
                .try_into()
                .map_err(|_| BitGcSessionError::InvalidInputMaterial)?,
        );
        if coefficient >= plaintext_modulus {
            return Err(BitGcSessionError::InvalidInputMaterial);
        }
        label.push(coefficient);
    }
    Ok((label, encoded[encoded.len() - 1] == 1))
}

fn frame_auth_tag(
    key: &[u8; 32],
    transcript: &[u8; 32],
    role: BitGcRole,
    binding: &BitGcSessionBinding,
    sequence: u64,
    kind: BitGcFrameKind,
    payload: &[u8],
) -> Result<[u8; AUTH_TAG_BYTES], ()> {
    let variant = encode_variant(binding.variant).map_err(|_| ())?;
    let sequence = sequence.to_le_bytes();
    let parts = [
        AUTH_DOMAIN,
        transcript.as_slice(),
        &[role as u8],
        &[variant],
        binding.session_id.as_slice(),
        binding.parameter_fingerprint.as_slice(),
        binding.circuit_digest.as_slice(),
        sequence.as_slice(),
        &[kind as u8],
        payload,
    ];
    Ok(hmac_sha256(key, &parts))
}

fn initial_ot_transcript(binding: &BitGcSessionBinding) -> [u8; 32] {
    let mut hash = Sha256::new();
    hash.update(b"Volar BitGC OT transcript v1\0");
    hash.update(binding.session_id);
    hash.update([binding.variant as u8]);
    hash.update(binding.parameter_fingerprint);
    hash.update(binding.circuit_digest);
    let digest = hash.finalize();
    let mut result = [0; 32];
    result.copy_from_slice(&digest);
    result
}

fn ot_transcript_after(previous: &[u8; 32], envelope: &[u8]) -> [u8; 32] {
    let mut hash = Sha256::new();
    hash.update(b"Volar BitGC OT message v1\0");
    hash.update(previous);
    hash.update(envelope);
    let digest = hash.finalize();
    let mut result = [0; 32];
    result.copy_from_slice(&digest);
    result
}

fn ot_auth_tag(
    key: &[u8; 32],
    binding: &BitGcSessionBinding,
    sender_role: BitGcRole,
    transcript: &[u8; 32],
    context: &[u8; 32],
    sequence: u64,
    payload: &[u8],
) -> [u8; AUTH_TAG_BYTES] {
    let sequence = sequence.to_le_bytes();
    let variant = [binding.variant as u8];
    let sender = [sender_role as u8];
    hmac_sha256(
        key,
        &[
            b"Volar BitGC OT v1\0",
            binding.session_id.as_slice(),
            &variant,
            binding.parameter_fingerprint.as_slice(),
            binding.circuit_digest.as_slice(),
            transcript.as_slice(),
            context.as_slice(),
            &sender,
            &sequence,
            payload,
        ],
    )
}

fn psk_hello_tag(
    key: &[u8; 32],
    binding: &BitGcSessionBinding,
    role: BitGcRole,
    nonce: &[u8; 32],
) -> [u8; AUTH_TAG_BYTES] {
    let variant = [binding.variant as u8];
    let role = [role as u8];
    hmac_sha256(
        key,
        &[
            PSK_HELLO_DOMAIN,
            binding.session_id.as_slice(),
            &variant,
            binding.parameter_fingerprint.as_slice(),
            binding.circuit_digest.as_slice(),
            &role,
            nonce.as_slice(),
        ],
    )
}

fn derive_psk_session_key(
    key: &[u8; 32],
    binding: &BitGcSessionBinding,
    garbler_nonce: &[u8; 32],
    evaluator_nonce: &[u8; 32],
) -> [u8; 32] {
    let variant = [binding.variant as u8];
    hmac_sha256(
        key,
        &[
            PSK_SESSION_KEY_DOMAIN,
            binding.session_id.as_slice(),
            &variant,
            binding.parameter_fingerprint.as_slice(),
            binding.circuit_digest.as_slice(),
            garbler_nonce.as_slice(),
            evaluator_nonce.as_slice(),
        ],
    )
}

fn hmac_sha256(key: &[u8; 32], parts: &[&[u8]]) -> [u8; AUTH_TAG_BYTES] {
    let mut ipad = [0x36; AUTH_BLOCK_BYTES];
    let mut opad = [0x5c; AUTH_BLOCK_BYTES];
    for (index, &byte) in key.iter().enumerate() {
        ipad[index] ^= byte;
        opad[index] ^= byte;
    }
    let mut inner = Sha256::new();
    inner.update(ipad);
    for part in parts {
        inner.update(part);
    }
    let inner_digest = inner.finalize();
    let mut outer = Sha256::new();
    outer.update(opad);
    outer.update(inner_digest);
    let digest = outer.finalize();
    let mut result = [0; AUTH_TAG_BYTES];
    result.copy_from_slice(&digest);
    result
}

fn output_label_hash(
    binding: &BitGcSessionBinding,
    output_index: u64,
    wire_index: u64,
    label: &LabelPoly,
    plaintext_modulus: u64,
    ring_degree: usize,
) -> Result<[u8; 32], BitGcSessionError> {
    if plaintext_modulus <= 2
        || plaintext_modulus & 1 == 0
        || ring_degree == 0
        || label.len() != ring_degree
        || label.len().checked_mul(8).is_none_or(|bytes| {
            bytes
                .checked_add(1)
                .is_none_or(|bytes| bytes > BITGC_OT_MAX_INPUT_BYTES)
        })
        || label
            .iter()
            .any(|&coefficient| coefficient >= plaintext_modulus)
    {
        return Err(BitGcSessionError::InvalidInputMaterial);
    }
    let variant = encode_variant(binding.variant).map_err(BitGcSessionError::Frame)?;
    let mut hash = Sha256::new();
    hash.update(b"Volar BitGC output label v1\0");
    hash.update([variant]);
    hash.update(binding.session_id);
    hash.update(binding.parameter_fingerprint);
    hash.update(binding.circuit_digest);
    hash.update(plaintext_modulus.to_le_bytes());
    hash.update(output_index.to_le_bytes());
    hash.update(wire_index.to_le_bytes());
    hash.update((label.len() as u64).to_le_bytes());
    for coefficient in label {
        hash.update(coefficient.to_le_bytes());
    }
    let digest = hash.finalize();
    let mut output = [0; 32];
    output.copy_from_slice(&digest);
    Ok(output)
}

fn constant_time_eq(left: &[u8; 32], right: &[u8; 32]) -> bool {
    let mut difference = 0u8;
    for index in 0..32 {
        difference |= left[index] ^ right[index];
    }
    difference == 0
}

/// Error from the Phase 4 role-separated circuit runner.
///
/// The runner is a narrow integration seam for the role-separated pinned
/// drivers. It is not active security, a production profile, or protected-
/// session admission.
#[derive(Debug, PartialEq, Eq)]
pub enum BitGcCircuitRunnerError<E> {
    /// Canonical binding construction rejected the circuit/profile shape.
    Binding(BitGcDigestError),
    /// A local session-state operation failed before a transport seam was
    /// entered.
    SessionState(BitGcSessionError),
    /// The shared frame/OT transport seam failed.
    Session(E),
    /// The circuit driver rejected its inputs or material.
    Driver(DriverError),
    /// Canonical evaluator-material or tables bytes failed to decode.
    Material(RoleMaterialError),
    /// A garbler/evaluator ownership mismatch is structurally impossible.
    WrongRole,
}

impl<E> core::fmt::Display for BitGcCircuitRunnerError<E>
where
    E: core::fmt::Debug,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Binding(error) => write!(f, "canonical BitGC binding failed: {error:?}"),
            Self::SessionState(error) => write!(f, "BitGC session state failed: {error:?}"),
            Self::Session(error) => write!(f, "BitGC session failed: {error:?}"),
            Self::Driver(error) => write!(f, "BitGC circuit driver failed: {error:?}"),
            Self::Material(error) => {
                write!(f, "BitGC evaluator material decode failed: {error:?}")
            }
            Self::WrongRole => f.write_str("BitGC runner role mismatch"),
        }
    }
}

#[cfg(feature = "std")]
impl<E> std::error::Error for BitGcCircuitRunnerError<E> where E: core::fmt::Debug {}

/// Transfer one evaluator-owned input label over an in-process loopback
/// frame/OT pair from garbler to evaluator.
///
/// This helper is an integration/test seam: it drives both roles through one
/// local transport object. Real deployments run the per-role methods
/// ([`BitGcSession::send_evaluator_input_over`] /
/// [`BitGcSession::receive_evaluator_input_over`]) on separate processes over
/// real transports, as the cross-process test demonstrates.
pub fn transfer_evaluator_input<N, O, T>(
    garbler_session: &mut BitGcSession,
    evaluator_session: &mut BitGcSession,
    frames: &mut T,
    ot: &mut O,
    input_index: u64,
    plaintext_modulus: u64,
    ring_degree: usize,
    label_pair: &[(LabelPoly, bool); 2],
    choice: bool,
) -> Result<(LabelPoly, bool), BitGcCircuitRunnerError<BitGcIoError<T::Error>>>
where
    N: VoleArray<u8>,
    O: BitGcInputOt<N>,
    T: BitGcFrameTransport,
{
    garbler_session
        .send_evaluator_input_over::<N, _, _>(
            frames,
            ot,
            input_index,
            plaintext_modulus,
            ring_degree,
            label_pair,
        )
        .map_err(BitGcCircuitRunnerError::Session)?;
    evaluator_session
        .receive_evaluator_input_over::<N, _, _>(
            frames,
            ot,
            input_index,
            plaintext_modulus,
            ring_degree,
            choice,
        )
        .map_err(BitGcCircuitRunnerError::Session)
}

/// Open output-label commitments from garbler to evaluator over an
/// in-process loopback frame transport and return the clear output bits.
///
/// Like [`transfer_evaluator_input`], this drives both roles locally; real
/// deployments use the per-role session methods on separate transports.
pub fn open_committed_outputs<T: BitGcFrameTransport>(
    garbler_session: &mut BitGcSession,
    evaluator_session: &mut BitGcSession,
    frames: &mut T,
    garbler_labels: &GarblerWireLabels,
    outputs: &[(LabelPoly, bool)],
    output_wires: &[usize],
    plaintext_modulus: u64,
    ring_degree: usize,
) -> Result<Vec<bool>, BitGcCircuitRunnerError<BitGcIoError<T::Error>>> {
    garbler_session
        .send_output_commitments_over(frames, garbler_labels, plaintext_modulus, ring_degree)
        .map_err(BitGcCircuitRunnerError::Session)?;
    evaluator_session
        .receive_committed_outputs_over(
            frames,
            outputs,
            output_wires,
            plaintext_modulus,
            ring_degree,
        )
        .map_err(BitGcCircuitRunnerError::Session)
}

/// Run one role-separated BitGC circuit in-process over a loopback frame
/// transport and OT.
///
/// The evaluator endpoint is constructed with only the evaluator-only
/// material produced by [`garble_role_circuit`]; garbler Δ and the PRF key
/// remain outside evaluator-facing arguments. All inputs are transferred by
/// OT using the evaluator's choices, and output labels open through the
/// session's hash-match commitments. This runner is an in-process
/// integration/correctness seam for the Phase 4 shared protocol: it does
/// not model concurrent processes, does not grant active security, and does
/// not admit BitGC for protected data.
///
/// TODO(provider-ledger: BITGC-LEDGER-07): keep this out of protected-data
/// profiles until independent protocol/parameter review passes.
pub fn run_role_separated_circuit_local<G, N, T, O, R>(
    variant: Variant,
    circuit: &Circuit,
    dd: &G,
    prf: &PrfKey,
    prg_bits: &[bool],
    random: &mut R,
    pis: &[bool],
    modulus_chain: &[u64],
    session_id: [u8; 32],
    evaluator_choices: &[bool],
    frames: &mut T,
    ot: &mut O,
) -> Result<Vec<bool>, BitGcCircuitRunnerError<BitGcIoError<T::Error>>>
where
    G: RoleDdGarbler,
    N: VoleArray<u8>,
    T: BitGcFrameTransport,
    O: BitGcInputOt<N>,
    R: SpecRng,
{
    circuit
        .validate()
        .map_err(|_| BitGcCircuitRunnerError::Binding(BitGcDigestError::InvalidCircuit))?;
    if evaluator_choices.len() != circuit.inputs {
        return Err(BitGcCircuitRunnerError::Driver(
            volar_spec::bitgc::pinned::DriverError::BadInputCount,
        ));
    }
    let (garbler_labels, tables, evaluator_material) =
        garble_role_circuit(variant, circuit, dd, pis, prf, prg_bits, random)
            .map_err(BitGcCircuitRunnerError::Driver)?;
    let binding = canonical_session_binding(
        session_id,
        variant,
        dd.p(),
        dd.degree(),
        modulus_chain,
        circuit,
    )
    .map_err(BitGcCircuitRunnerError::Binding)?;
    let mut garbler = BitGcSession::new_authenticated(BitGcRole::Garbler, binding, [0x42; 32])
        .map_err(BitGcCircuitRunnerError::SessionState)?;
    let mut evaluator = BitGcSession::new_authenticated(BitGcRole::Evaluator, binding, [0x42; 32])
        .map_err(BitGcCircuitRunnerError::SessionState)?;

    let garbler_hello = garbler
        .hello()
        .map_err(BitGcCircuitRunnerError::SessionState)?;
    let evaluator_hello = evaluator
        .hello()
        .map_err(BitGcCircuitRunnerError::SessionState)?;
    garbler
        .accept(&evaluator_hello)
        .map_err(BitGcCircuitRunnerError::SessionState)?;
    evaluator
        .accept(&garbler_hello)
        .map_err(BitGcCircuitRunnerError::SessionState)?;

    let mut inputs: Vec<(LabelPoly, bool)> = Vec::with_capacity(circuit.inputs);
    for input_index in 0..circuit.inputs {
        let label_pair = garbler_labels
            .input_label_pair(input_index)
            .map_err(BitGcCircuitRunnerError::Driver)?;
        let transferred = transfer_evaluator_input::<N, O, T>(
            &mut garbler,
            &mut evaluator,
            frames,
            ot,
            input_index as u64,
            dd.p(),
            dd.degree(),
            &label_pair,
            evaluator_choices[input_index],
        )?;
        inputs.push(transferred);
    }

    let outputs =
        evaluate_role_circuit_with_labels(variant, circuit, &evaluator_material, &tables, &inputs)
            .map_err(BitGcCircuitRunnerError::Driver)?;
    open_committed_outputs(
        &mut garbler,
        &mut evaluator,
        frames,
        &garbler_labels,
        &outputs,
        &circuit.outputs,
        dd.p(),
        dd.degree(),
    )
}

/// Run the garbler side of one role-separated BitGC circuit over real
/// caller-owned frame and OT transports.
///
/// After the hello handshake, the garbler delivers the evaluator setup
/// material and canonical [`RoleEvaluatorTables`][tables] through
/// [`BitGcSession::send_material_over`], delivers garbler-owned inputs
/// through [`BitGcSession::send_garbler_input_over`], transfers each
/// evaluator-owned input by OT through
/// [`BitGcSession::send_evaluator_input_over`], and finally sends the
/// output-label commitments. The garbler learns nothing back except
/// transport success.
///
/// [tables]: volar_spec::bitgc::pinned::RoleEvaluatorTables
///
/// This is a correctness/integration seam only: it does not admit BitGC for
/// protected data, and its transcript authentication is exactly what the
/// underlying [`BitGcSession`] was constructed with.
///
/// TODO(provider-ledger: BITGC-LEDGER-07): keep out of protected-data
/// profiles until independent protocol/parameter review passes.
#[allow(clippy::too_many_arguments)]
pub fn run_garbler_over<G, N, TF, TO, R>(
    session: &mut BitGcSession,
    circuit: &Circuit,
    dd: &G,
    prf: &PrfKey,
    prg_bits: &[bool],
    random: &mut R,
    pis: &[bool],
    modulus_chain: &[u64],
    input_owners: &[BitGcInputOwner],
    garbler_inputs: &[bool],
    frames: &mut TF,
    ot: &mut TO,
) -> Result<(), BitGcCircuitRunnerError<BitGcIoError<TF::Error>>>
where
    G: RoleDdGarbler,
    G::Evaluator: RoleMaterialCodec,
    N: VoleArray<u8>,
    TF: BitGcFrameTransport,
    TO: BitGcInputOt<N>,
    R: SpecRng,
{
    let variant = session.binding().variant;
    if session.state() != BitGcSessionState::Handshaking {
        return Err(BitGcCircuitRunnerError::SessionState(
            BitGcSessionError::UnexpectedKind,
        ));
    }
    circuit
        .validate()
        .map_err(|_| BitGcCircuitRunnerError::Binding(BitGcDigestError::InvalidCircuit))?;
    if input_owners.len() != circuit.inputs
        || garbler_inputs.len()
            != input_owners
                .iter()
                .filter(|owner| matches!(owner, BitGcInputOwner::Garbler))
                .count()
    {
        return Err(BitGcCircuitRunnerError::Driver(DriverError::BadInputCount));
    }
    let expected_binding = canonical_session_binding(
        session.binding().session_id,
        variant,
        dd.p(),
        dd.degree(),
        modulus_chain,
        circuit,
    )
    .map_err(BitGcCircuitRunnerError::Binding)?;
    if session.binding() != expected_binding {
        return Err(BitGcCircuitRunnerError::Binding(
            BitGcDigestError::InvalidParameters,
        ));
    }
    let (garbler_labels, tables, evaluator_material) =
        garble_role_circuit(variant, circuit, dd, pis, prf, prg_bits, random)
            .map_err(BitGcCircuitRunnerError::Driver)?;
    session
        .handshake_over(frames)
        .map_err(BitGcCircuitRunnerError::Session)?;

    let tables_bytes = encode_role_tables(&evaluator_material, &tables)
        .map_err(BitGcCircuitRunnerError::Material)?;
    let setup_bytes = evaluator_material.encode_evaluator_material();
    let mut material = Vec::new();
    material
        .try_reserve_exact(8 + setup_bytes.len() + tables_bytes.len())
        .map_err(|_| BitGcCircuitRunnerError::Material(RoleMaterialError::CountOverflow))?;
    material.extend_from_slice(&(setup_bytes.len() as u64).to_le_bytes());
    material.extend_from_slice(&setup_bytes);
    material.extend_from_slice(&tables_bytes);
    session
        .send_material_over(frames, &material)
        .map_err(BitGcCircuitRunnerError::Session)?;

    let mut garbler_index = 0usize;
    for (input_index, owner) in input_owners.iter().enumerate() {
        match owner {
            BitGcInputOwner::Garbler => {
                let value = garbler_inputs[garbler_index];
                garbler_index += 1;
                let pair = garbler_labels
                    .input_label_pair(input_index)
                    .map_err(BitGcCircuitRunnerError::Driver)?;
                let selected = pair[usize::from(value)].clone();
                session
                    .send_garbler_input_over(
                        frames,
                        input_index as u64,
                        dd.p(),
                        dd.degree(),
                        &selected,
                    )
                    .map_err(BitGcCircuitRunnerError::Session)?;
            }
            BitGcInputOwner::Evaluator => {
                let pair = garbler_labels
                    .input_label_pair(input_index)
                    .map_err(BitGcCircuitRunnerError::Driver)?;
                session
                    .send_evaluator_input_over::<N, _, _>(
                        frames,
                        ot,
                        input_index as u64,
                        dd.p(),
                        dd.degree(),
                        &pair,
                    )
                    .map_err(BitGcCircuitRunnerError::Session)?;
            }
        }
    }

    session
        .send_output_commitments_over(frames, &garbler_labels, dd.p(), dd.degree())
        .map_err(BitGcCircuitRunnerError::Session)
}

/// Run the evaluator side of one role-separated BitGC circuit over real
/// caller-owned frame and OT transports.
///
/// After the hello handshake, the evaluator receives the material package
/// (setup material + canonical tables) through
/// [`BitGcSession::receive_material_over`], decodes it against the declared
/// `E` profile, receives garbler-owned inputs directly and evaluator-owned
/// inputs by OT with its local choices, evaluates the circuit with the
/// decoded evaluator-only material, and opens the output commitments.
///
/// No garbler Δ, PRF key, or secret key material ever enters this function.
/// This is a correctness/integration seam only: it does not admit BitGC for
/// protected data.
///
/// TODO(provider-ledger: BITGC-LEDGER-07): keep out of protected-data
/// profiles until independent protocol/parameter review passes.
pub fn run_evaluator_over<E, N, TF, TO>(
    session: &mut BitGcSession,
    circuit: &Circuit,
    modulus_chain: &[u64],
    input_owners: &[BitGcInputOwner],
    evaluator_choices: &[bool],
    frames: &mut TF,
    ot: &mut TO,
) -> Result<Vec<bool>, BitGcCircuitRunnerError<BitGcIoError<TF::Error>>>
where
    E: RoleMaterialCodec,
    N: VoleArray<u8>,
    TF: BitGcFrameTransport,
    TO: BitGcInputOt<N>,
{
    let variant = session.binding().variant;
    if session.state() != BitGcSessionState::Handshaking {
        return Err(BitGcCircuitRunnerError::SessionState(
            BitGcSessionError::UnexpectedKind,
        ));
    }
    circuit
        .validate()
        .map_err(|_| BitGcCircuitRunnerError::Binding(BitGcDigestError::InvalidCircuit))?;
    if input_owners.len() != circuit.inputs
        || evaluator_choices.len()
            != input_owners
                .iter()
                .filter(|owner| matches!(owner, BitGcInputOwner::Evaluator))
                .count()
    {
        return Err(BitGcCircuitRunnerError::Driver(DriverError::BadInputCount));
    }
    session
        .handshake_over(frames)
        .map_err(BitGcCircuitRunnerError::Session)?;

    let material = session
        .receive_material_over(frames)
        .map_err(BitGcCircuitRunnerError::Session)?;
    if material.len() < 8 {
        return Err(BitGcCircuitRunnerError::Material(
            RoleMaterialError::LengthMismatch,
        ));
    }
    let setup_len = u64::from_le_bytes(
        material[..8]
            .try_into()
            .map_err(|_| BitGcCircuitRunnerError::Material(RoleMaterialError::LengthMismatch))?,
    ) as usize;
    if material.len() < 8 + setup_len {
        return Err(BitGcCircuitRunnerError::Material(
            RoleMaterialError::LengthMismatch,
        ));
    }
    let evaluator_material = E::decode_evaluator_material(&material[8..8 + setup_len]).ok_or(
        BitGcCircuitRunnerError::Material(RoleMaterialError::NonCanonical),
    )?;
    let tables = decode_role_tables(&evaluator_material, variant, &material[8 + setup_len..])
        .map_err(BitGcCircuitRunnerError::Material)?;
    let p = evaluator_material.p();
    let degree = evaluator_material.degree();
    // The decoded profile must reproduce the session's parameter
    // fingerprint, so a garbler cannot silently swap profiles mid-session.
    let expected_fingerprint = canonical_parameter_fingerprint(variant, p, degree, modulus_chain)
        .map_err(BitGcCircuitRunnerError::Binding)?;
    if expected_fingerprint != session.binding().parameter_fingerprint {
        return Err(BitGcCircuitRunnerError::Binding(
            BitGcDigestError::InvalidParameters,
        ));
    }

    let mut inputs: Vec<(LabelPoly, bool)> = Vec::with_capacity(circuit.inputs);
    let mut choice_index = 0usize;
    for (input_index, owner) in input_owners.iter().enumerate() {
        match owner {
            BitGcInputOwner::Garbler => {
                let label = session
                    .receive_garbler_input_over(frames, input_index as u64, p, degree)
                    .map_err(BitGcCircuitRunnerError::Session)?;
                inputs.push(label);
            }
            BitGcInputOwner::Evaluator => {
                let choice = evaluator_choices[choice_index];
                choice_index += 1;
                let label = session
                    .receive_evaluator_input_over::<N, _, _>(
                        frames,
                        ot,
                        input_index as u64,
                        p,
                        degree,
                        choice,
                    )
                    .map_err(BitGcCircuitRunnerError::Session)?;
                inputs.push(label);
            }
        }
    }

    let outputs =
        evaluate_role_circuit_with_labels(variant, circuit, &evaluator_material, &tables, &inputs)
            .map_err(BitGcCircuitRunnerError::Driver)?;
    session
        .receive_committed_outputs_over(frames, &outputs, &circuit.outputs, p, degree)
        .map_err(BitGcCircuitRunnerError::Session)
}

fn is_terminal(state: BitGcSessionState) -> bool {
    matches!(
        state,
        BitGcSessionState::Cancelled | BitGcSessionState::Aborted
    )
}

#[cfg(test)]
mod tests {
    use alloc::string::ToString;
    use alloc::vec;
    use alloc::vec::Vec;
    extern crate std;
    use std::io::{Read, Write};
    use std::net::{Shutdown, TcpListener, TcpStream};
    use std::process::Command;
    use std::sync::mpsc::{self, Receiver, Sender};
    use std::thread;
    use std::time::Duration;
    use typenum::U16;
    use volar_spec::bitgc::common::Circuit;
    use volar_spec::bitgc::lattice_dd_rns::LatticeRnsDd;
    use volar_spec::bitgc::pinned::{
        PrfKey, evaluate_role_circuit_with_labels, garble_role_circuit,
    };

    use super::*;
    use crate::ot::{LoopbackOt, SeedRng};

    fn role_runner_fixture(
        variant: Variant,
        seed: u64,
    ) -> (
        Circuit,
        LatticeRnsDd,
        PrfKey,
        Vec<bool>,
        Vec<bool>,
        Vec<u64>,
    ) {
        let mut rng = SeedRng::new(seed);
        let circuit = Circuit {
            inputs: 3,
            gates: vec![Gate::And { a: 0, b: 1 }, Gate::Xor { a: 3, b: 2 }],
            outputs: vec![4],
        };
        let p = 97;
        let dd = LatticeRnsDd::new(8, p, 1649, 1153, &mut rng).unwrap();
        let prf = PrfKey([0x11, 0x22, 0x33, 0x44]);
        let pis: Vec<bool> = (0..circuit.wires())
            .map(|_| (rng.next_u32() & 1) == 1)
            .collect();
        let prg_count = if variant == Variant::Fast { 4 } else { 0 };
        let prg_bits: Vec<bool> = (0..prg_count).map(|_| (rng.next_u32() & 1) == 1).collect();
        (circuit, dd, prf, prg_bits, pis, vec![1649, 1153])
    }

    /// In-process loopback frame transport: sent frames are immediately
    /// readable by the other role in order.
    #[derive(Default)]
    struct LoopbackFrames {
        queue: Vec<Vec<u8>>,
    }

    impl BitGcFrameTransport for LoopbackFrames {
        type Error = ();

        fn send_frame(&mut self, frame: &[u8]) -> Result<(), Self::Error> {
            self.queue.push(frame.to_vec());
            Ok(())
        }

        fn receive_frame(&mut self, max_bytes: usize) -> Result<Vec<u8>, Self::Error> {
            let frame = self.queue.first().ok_or(())?;
            if frame.len() > max_bytes {
                return Err(());
            }
            Ok(self.queue.remove(0))
        }

        fn cancel(&mut self) -> Result<(), Self::Error> {
            Ok(())
        }
    }

    #[test]
    fn role_runner_covers_small_and_fast_choices() {
        for variant in [Variant::Small, Variant::Fast] {
            for pattern in 0..8u8 {
                let choices = [pattern & 1 == 1, pattern & 2 == 2, pattern & 4 == 4];
                let (circuit, dd, prf, prg_bits, pis, modulus_chain) =
                    role_runner_fixture(variant, 0x5012_3456 + variant as u64);
                let mut rng = SeedRng::new(0x5012_3456 + variant as u64 + 0x1000);
                let (garbler_labels, tables, evaluator_material) =
                    garble_role_circuit(variant, &circuit, &dd, &pis, &prf, &prg_bits, &mut rng)
                        .unwrap();
                let binding = canonical_session_binding(
                    [0x51; 32],
                    variant,
                    dd.p(),
                    dd.degree(),
                    &modulus_chain,
                    &circuit,
                )
                .unwrap();
                let mut garbler =
                    BitGcSession::new_authenticated(BitGcRole::Garbler, binding, [0x42; 32])
                        .unwrap();
                let mut evaluator =
                    BitGcSession::new_authenticated(BitGcRole::Evaluator, binding, [0x42; 32])
                        .unwrap();
                handshake(&mut garbler, &mut evaluator);

                let mut frames = LoopbackFrames::default();
                let mut loopback = LoopbackOt::<U16>::new();
                let mut ot = InfallibleBitGcOt::new(&mut loopback);
                let mut inputs = Vec::new();
                for (input_index, choice) in choices.iter().enumerate() {
                    let pair = garbler_labels.input_label_pair(input_index).unwrap();
                    inputs.push(
                        transfer_evaluator_input::<U16, _, _>(
                            &mut garbler,
                            &mut evaluator,
                            &mut frames,
                            &mut ot,
                            input_index as u64,
                            dd.p(),
                            dd.degree(),
                            &pair,
                            *choice,
                        )
                        .unwrap(),
                    );
                }
                let outputs = evaluate_role_circuit_with_labels(
                    variant,
                    &circuit,
                    &evaluator_material,
                    &tables,
                    &inputs,
                )
                .unwrap();
                let got = open_committed_outputs(
                    &mut garbler,
                    &mut evaluator,
                    &mut frames,
                    &garbler_labels,
                    &outputs,
                    &circuit.outputs,
                    dd.p(),
                    dd.degree(),
                )
                .unwrap();
                let want = vec![(choices[0] && choices[1]) ^ choices[2]];
                assert_eq!(got, want, "{variant:?} choices={choices:?}");
            }
        }
    }

    #[test]
    fn role_runner_rejects_bad_input_count() {
        let (circuit, dd, prf, prg_bits, pis, modulus_chain) =
            role_runner_fixture(Variant::Small, 0x7012_3456);
        let mut rng = SeedRng::new(0x7012_3456);
        let short_choices = [false, true];
        let mut frames = LoopbackFrames::default();
        let mut loopback = LoopbackOt::<U16>::new();
        let mut ot = InfallibleBitGcOt::new(&mut loopback);
        let error = run_role_separated_circuit_local::<_, U16, _, _, _>(
            Variant::Small,
            &circuit,
            &dd,
            &prf,
            &prg_bits,
            &mut rng,
            &pis,
            &modulus_chain,
            [0x53; 32],
            &short_choices,
            &mut frames,
            &mut ot,
        );
        assert!(matches!(
            error,
            Err(BitGcCircuitRunnerError::Driver(DriverError::BadInputCount))
        ));
    }

    #[test]
    fn role_runner_rejects_tiny_and_nontopological_circuit() {
        let (mut circuit, dd, prf, prg_bits, pis, modulus_chain) =
            role_runner_fixture(Variant::Small, 0x7012_3457);
        let mut rng = SeedRng::new(0x7012_3457);
        let choices = [false, true, true];
        let mut frames = LoopbackFrames::default();
        let mut loopback = LoopbackOt::<U16>::new();
        let mut ot = InfallibleBitGcOt::new(&mut loopback);
        let error = run_role_separated_circuit_local::<_, U16, _, _, _>(
            Variant::Tiny,
            &circuit,
            &dd,
            &prf,
            &prg_bits,
            &mut rng,
            &pis,
            &modulus_chain,
            [0x52; 32],
            &choices,
            &mut frames,
            &mut ot,
        );
        assert!(matches!(
            error,
            Err(BitGcCircuitRunnerError::Driver(
                DriverError::UnsupportedVariant
            ))
        ));

        // Gate 0's output is wire 3; an operand equal to its own output
        // violates topological order.
        circuit.gates[0] = Gate::And { a: 3, b: 1 };
        let error = run_role_separated_circuit_local::<_, U16, _, _, _>(
            Variant::Small,
            &circuit,
            &dd,
            &prf,
            &prg_bits,
            &mut rng,
            &pis,
            &modulus_chain,
            [0x54; 32],
            &choices,
            &mut frames,
            &mut ot,
        );
        assert!(matches!(
            error,
            Err(BitGcCircuitRunnerError::Binding(
                BitGcDigestError::InvalidCircuit
            ))
        ));
    }

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

    fn authenticated_pair() -> (BitGcSession, BitGcSession) {
        let key = [0x5a; 32];
        (
            BitGcSession::new_authenticated(BitGcRole::Garbler, binding(), key).unwrap(),
            BitGcSession::new_authenticated(BitGcRole::Evaluator, binding(), key).unwrap(),
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

    fn run_single_input_session(
        choice: bool,
        tamper_output: bool,
        authenticated: bool,
    ) -> Result<Vec<bool>, BitGcIoError<()>> {
        let p = 97;
        let mut rng = SeedRng::new(0x5048_4153 + u64::from(choice));
        let dd = LatticeRnsDd::new(8, p, 1649, 1153, &mut rng).unwrap();
        let circuit = Circuit {
            inputs: 1,
            gates: Vec::new(),
            outputs: vec![0],
        };
        let prf = PrfKey([0x11, 0x22, 0x33, 0x44]);
        let (garbler_labels, tables, evaluator_material) =
            garble_role_circuit(Variant::Small, &circuit, &dd, &[false], &prf, &[], &mut rng)
                .unwrap();
        let input_pair = garbler_labels.input_label_pair(0).unwrap();
        let (mut garbler, mut evaluator) = if authenticated {
            authenticated_pair()
        } else {
            pair()
        };
        handshake(&mut garbler, &mut evaluator);

        let mut loopback_ot = LoopbackOt::<U16>::new();
        let mut ot = InfallibleBitGcOt::new(&mut loopback_ot);
        let mut garbler_transport = TestTransport::new();
        garbler.send_evaluator_input_over::<U16, _, _>(
            &mut garbler_transport,
            &mut ot,
            0,
            p,
            8,
            &input_pair,
        )?;
        let mut evaluator_transport = TestTransport {
            incoming: garbler_transport.sent.clone(),
            ..TestTransport::new()
        };
        let evaluator_input = evaluator.receive_evaluator_input_over::<U16, _, _>(
            &mut evaluator_transport,
            &mut ot,
            0,
            p,
            8,
            choice,
        )?;
        let mut outputs = evaluate_role_circuit_with_labels(
            Variant::Small,
            &circuit,
            &evaluator_material,
            &tables,
            &[evaluator_input],
        )
        .unwrap();
        if tamper_output {
            outputs[0].0[0] = (outputs[0].0[0] + 1) % p;
        }
        let output_start = garbler_transport.sent.len();
        garbler.send_output_commitments_over(&mut garbler_transport, &garbler_labels, p, 8)?;
        evaluator_transport
            .incoming
            .extend(garbler_transport.sent[output_start..].iter().cloned());
        evaluator.receive_committed_outputs_over(
            &mut evaluator_transport,
            &outputs,
            &circuit.outputs,
            p,
            8,
        )
    }

    #[test]
    fn evaluator_input_ot_and_hash_match_opening_cover_both_bits() {
        for choice in [false, true] {
            assert_eq!(
                run_single_input_session(choice, false, false).unwrap(),
                [choice]
            );
        }
        assert!(matches!(
            run_single_input_session(true, true, false),
            Err(BitGcIoError::Protocol(
                BitGcSessionError::OutputLabelMismatch
            ))
        ));
        assert_eq!(run_single_input_session(true, false, true).unwrap(), [true]);
    }

    #[test]
    fn input_ot_descriptor_binds_wire_and_aborts_on_mismatch() {
        let (mut garbler, mut evaluator) = pair();
        handshake(&mut garbler, &mut evaluator);
        let label_pair = [(vec![1, 2], false), (vec![3, 4], true)];
        let mut loopback_ot = LoopbackOt::<U16>::new();
        let mut ot = InfallibleBitGcOt::new(&mut loopback_ot);
        let mut garbler_transport = TestTransport::new();
        garbler
            .send_evaluator_input_over::<U16, _, _>(
                &mut garbler_transport,
                &mut ot,
                5,
                97,
                2,
                &label_pair,
            )
            .unwrap();
        let mut evaluator_transport = TestTransport {
            incoming: garbler_transport.sent.clone(),
            ..TestTransport::new()
        };
        assert!(matches!(
            evaluator.receive_evaluator_input_over::<U16, _, _>(
                &mut evaluator_transport,
                &mut ot,
                6,
                97,
                2,
                false,
            ),
            Err(BitGcIoError::Protocol(
                BitGcSessionError::InvalidInputDescriptor
            ))
        ));
        assert_eq!(evaluator.state(), BitGcSessionState::Aborted);
        assert!(evaluator_transport.cancelled);
    }

    struct FailingInputOt {
        fail_send: bool,
        fail_receive: bool,
        cancelled: bool,
    }

    impl BitGcInputOt<U16> for FailingInputOt {
        type Error = ();

        fn send_pair(&mut self, _labels: [&Array<u8, U16>; 2]) -> Result<(), Self::Error> {
            if self.fail_send { Err(()) } else { Ok(()) }
        }

        fn receive_choice(&mut self, _choice: bool) -> Result<Array<u8, U16>, Self::Error> {
            if self.fail_receive {
                Err(())
            } else {
                Ok(Array::from_fn(|_| 0))
            }
        }

        fn cancel(&mut self) -> Result<(), Self::Error> {
            self.cancelled = true;
            Ok(())
        }
    }

    #[test]
    fn ot_failures_abort_both_ends_and_cancel_frame_and_ot_transports() {
        let (mut garbler, mut evaluator) = pair();
        handshake(&mut garbler, &mut evaluator);
        let labels = [(vec![1, 2], false), (vec![3, 4], true)];
        let mut failed_sender_ot = FailingInputOt {
            fail_send: true,
            fail_receive: false,
            cancelled: false,
        };
        let mut garbler_transport = TestTransport::new();
        assert!(matches!(
            garbler.send_evaluator_input_over::<U16, _, _>(
                &mut garbler_transport,
                &mut failed_sender_ot,
                0,
                97,
                2,
                &labels,
            ),
            Err(BitGcIoError::Protocol(BitGcSessionError::OtFailure))
        ));
        assert_eq!(garbler.state(), BitGcSessionState::Aborted);
        assert!(garbler_transport.cancelled);
        assert!(failed_sender_ot.cancelled);

        let (mut garbler, mut evaluator) = pair();
        handshake(&mut garbler, &mut evaluator);
        let mut loopback = LoopbackOt::<U16>::new();
        let mut adapter = InfallibleBitGcOt::new(&mut loopback);
        let mut garbler_transport = TestTransport::new();
        garbler
            .send_evaluator_input_over::<U16, _, _>(
                &mut garbler_transport,
                &mut adapter,
                0,
                97,
                2,
                &labels,
            )
            .unwrap();
        let mut evaluator_transport = TestTransport {
            incoming: garbler_transport.sent.clone(),
            ..TestTransport::new()
        };
        let mut failed_receiver_ot = FailingInputOt {
            fail_send: false,
            fail_receive: true,
            cancelled: false,
        };
        assert!(matches!(
            evaluator.receive_evaluator_input_over::<U16, _, _>(
                &mut evaluator_transport,
                &mut failed_receiver_ot,
                0,
                97,
                2,
                true,
            ),
            Err(BitGcIoError::Protocol(BitGcSessionError::OtFailure))
        ));
        assert_eq!(evaluator.state(), BitGcSessionState::Aborted);
        assert!(evaluator_transport.cancelled);
        assert!(failed_receiver_ot.cancelled);
    }

    #[test]
    fn input_profile_degree_and_oversized_outbound_frames_fail_closed() {
        let (mut garbler, mut evaluator) = pair();
        handshake(&mut garbler, &mut evaluator);

        let mut loopback_ot = LoopbackOt::<U16>::new();
        let mut ot = InfallibleBitGcOt::new(&mut loopback_ot);
        let labels = [(vec![1, 2], false), (vec![3, 4], true)];
        let mut transport = TestTransport::new();
        assert!(matches!(
            garbler.send_evaluator_input_over::<U16, _, _>(
                &mut transport,
                &mut ot,
                0,
                97,
                3,
                &labels,
            ),
            Err(BitGcIoError::Protocol(
                BitGcSessionError::InvalidInputMaterial
            ))
        ));
        assert_eq!(garbler.state(), BitGcSessionState::Aborted);
        assert!(transport.cancelled);

        let (mut garbler, mut evaluator) = pair();
        handshake(&mut garbler, &mut evaluator);
        let mut transport = TestTransport::new();
        let oversized = vec![0; BITGC_FRAME_MAX_PAYLOAD + 1];
        assert!(matches!(
            garbler.send_over(&mut transport, BitGcFrameKind::Data, &oversized),
            Err(BitGcIoError::Protocol(BitGcSessionError::Frame(
                FrameError::PayloadTooLarge
            )))
        ));
        assert_eq!(garbler.state(), BitGcSessionState::Aborted);
        assert!(transport.cancelled);
        assert_eq!(evaluator.state(), BitGcSessionState::Active);
    }

    struct Pipe {
        send: Sender<Vec<u8>>,
        receive: Receiver<Vec<u8>>,
        cancelled: bool,
    }

    fn pipe_pair() -> (Pipe, Pipe) {
        let (left_send, right_receive) = mpsc::channel();
        let (right_send, left_receive) = mpsc::channel();
        (
            Pipe {
                send: left_send,
                receive: left_receive,
                cancelled: false,
            },
            Pipe {
                send: right_send,
                receive: right_receive,
                cancelled: false,
            },
        )
    }

    impl BitGcFrameTransport for Pipe {
        type Error = ();

        fn send_frame(&mut self, frame: &[u8]) -> Result<(), Self::Error> {
            self.send.send(frame.to_vec()).map_err(|_| ())
        }

        fn receive_frame(&mut self, max_bytes: usize) -> Result<Vec<u8>, Self::Error> {
            let frame = self.receive.recv().map_err(|_| ())?;
            if frame.len() > max_bytes {
                return Err(());
            }
            Ok(frame)
        }

        fn cancel(&mut self) -> Result<(), Self::Error> {
            self.cancelled = true;
            Ok(())
        }
    }

    impl BitGcOtTransport for Pipe {
        type Error = ();

        fn send_ot_message(&mut self, message: &[u8]) -> Result<(), Self::Error> {
            self.send.send(message.to_vec()).map_err(|_| ())
        }

        fn receive_ot_message(&mut self, max_bytes: usize) -> Result<Vec<u8>, Self::Error> {
            let message = self.receive.recv().map_err(|_| ())?;
            if message.len() > max_bytes {
                return Err(());
            }
            Ok(message)
        }

        fn cancel(&mut self) -> Result<(), Self::Error> {
            self.cancelled = true;
            Ok(())
        }
    }

    struct CrossProcessTransport(TcpStream);

    impl CrossProcessTransport {
        fn write_blob(&mut self, bytes: &[u8]) -> std::io::Result<()> {
            let length = u32::try_from(bytes.len()).map_err(|_| {
                std::io::Error::new(std::io::ErrorKind::InvalidInput, "message too large")
            })?;
            self.0.write_all(&length.to_le_bytes())?;
            self.0.write_all(bytes)
        }

        fn read_blob(&mut self, max_bytes: usize) -> std::io::Result<Vec<u8>> {
            let mut length = [0; 4];
            self.0.read_exact(&mut length)?;
            let length = u32::from_le_bytes(length) as usize;
            if length > max_bytes {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "message exceeds bound",
                ));
            }
            let mut bytes = vec![0; length];
            self.0.read_exact(&mut bytes)?;
            Ok(bytes)
        }
    }

    impl BitGcFrameTransport for CrossProcessTransport {
        type Error = std::io::Error;

        fn send_frame(&mut self, frame: &[u8]) -> Result<(), Self::Error> {
            self.write_blob(frame)
        }

        fn receive_frame(&mut self, max_bytes: usize) -> Result<Vec<u8>, Self::Error> {
            self.read_blob(max_bytes)
        }

        fn cancel(&mut self) -> Result<(), Self::Error> {
            self.0.shutdown(Shutdown::Both)
        }
    }

    impl BitGcOtTransport for CrossProcessTransport {
        type Error = std::io::Error;

        fn send_ot_message(&mut self, message: &[u8]) -> Result<(), Self::Error> {
            self.write_blob(message)
        }

        fn receive_ot_message(&mut self, max_bytes: usize) -> Result<Vec<u8>, Self::Error> {
            self.read_blob(max_bytes)
        }

        fn cancel(&mut self) -> Result<(), Self::Error> {
            self.0.shutdown(Shutdown::Both)
        }
    }

    #[test]
    fn cross_process_worker() {
        let Ok(frame_address) = std::env::var("VOLAR_BITGC_FRAME_ADDRESS") else {
            return;
        };
        let ot_address = std::env::var("VOLAR_BITGC_OT_ADDRESS").unwrap();
        let frame_stream = TcpStream::connect(frame_address).unwrap();
        let ot_stream = TcpStream::connect(ot_address).unwrap();
        frame_stream
            .set_read_timeout(Some(Duration::from_secs(10)))
            .unwrap();
        ot_stream
            .set_read_timeout(Some(Duration::from_secs(10)))
            .unwrap();
        let mut frames = CrossProcessTransport(frame_stream);
        let mut evaluator =
            BitGcSession::new_authenticated(BitGcRole::Evaluator, binding(), [0x6b; 32]).unwrap();
        let ot_transport = evaluator
            .authenticated_ot_transport(CrossProcessTransport(ot_stream))
            .unwrap();
        let hello = evaluator.hello().unwrap();
        frames.send_frame(&hello).unwrap();
        let peer_hello = frames.receive_frame(BITGC_FRAME_MAX_BYTES).unwrap();
        evaluator.accept(&peer_hello).unwrap();
        assert_eq!(evaluator.state(), BitGcSessionState::Active);

        let data = frames.receive_frame(BITGC_FRAME_MAX_BYTES).unwrap();
        let frame = evaluator.accept(&data).unwrap();
        assert_eq!(evaluator.payload_without_auth(&frame).unwrap(), &[7, 8, 9]);
        let response = evaluator.send_data(&[10, 11]).unwrap();
        frames.send_frame(&response).unwrap();

        let mut receiver = ChouOrlandiBitGcOt::<U16, _, _>::new(
            BitGcRole::Evaluator,
            ot_transport,
            SeedRng::new(0x4352_4f53),
        );
        let (selected, masked) = evaluator
            .receive_evaluator_input_over::<U16, _, _>(&mut frames, &mut receiver, 4, 97, 2, true)
            .unwrap();
        assert_eq!(selected, vec![41, 42]);
        assert!(masked);
    }

    #[test]
    fn authenticated_frame_and_ot_roundtrip_across_processes() {
        let frame_listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let ot_listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let frame_address = frame_listener.local_addr().unwrap();
        let ot_address = ot_listener.local_addr().unwrap();
        let mut child = Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "bitgc_session::tests::cross_process_worker",
                "--nocapture",
            ])
            .env("VOLAR_BITGC_FRAME_ADDRESS", frame_address.to_string())
            .env("VOLAR_BITGC_OT_ADDRESS", ot_address.to_string())
            .spawn()
            .unwrap();

        let (frame_stream, _) = frame_listener.accept().unwrap();
        let (ot_stream, _) = ot_listener.accept().unwrap();
        frame_stream
            .set_read_timeout(Some(Duration::from_secs(10)))
            .unwrap();
        ot_stream
            .set_write_timeout(Some(Duration::from_secs(10)))
            .unwrap();
        let mut frames = CrossProcessTransport(frame_stream);
        let mut garbler =
            BitGcSession::new_authenticated(BitGcRole::Garbler, binding(), [0x6b; 32]).unwrap();
        let hello = garbler.hello().unwrap();
        frames.send_frame(&hello).unwrap();
        let peer_hello = frames.receive_frame(BITGC_FRAME_MAX_BYTES).unwrap();
        garbler.accept(&peer_hello).unwrap();
        assert_eq!(garbler.state(), BitGcSessionState::Active);
        let data = garbler.send_data(&[7, 8, 9]).unwrap();
        frames.send_frame(&data).unwrap();
        let response = frames.receive_frame(BITGC_FRAME_MAX_BYTES).unwrap();
        let frame = garbler.accept(&response).unwrap();
        assert_eq!(garbler.payload_without_auth(&frame).unwrap(), &[10, 11]);

        let ot_transport = garbler
            .authenticated_ot_transport(CrossProcessTransport(ot_stream))
            .unwrap();
        let mut sender = ChouOrlandiBitGcOt::<U16, _, _>::new(
            BitGcRole::Garbler,
            ot_transport,
            SeedRng::new(0x5345_4e44),
        );
        let labels = [(vec![31, 32], false), (vec![41, 42], true)];
        garbler
            .send_evaluator_input_over::<U16, _, _>(&mut frames, &mut sender, 4, 97, 2, &labels)
            .unwrap();
        assert!(child.wait().unwrap().success());
    }

    #[test]
    fn chou_orlandi_ot_transfers_only_the_selected_label_across_threads() {
        for choice in [false, true] {
            let (mut garbler_session, mut evaluator_session) = pair();
            handshake(&mut garbler_session, &mut evaluator_session);
            let (mut garbler_frames, mut evaluator_frames) = pipe_pair();
            let (garbler_ot_transport, evaluator_ot_transport) = pipe_pair();
            let labels = [(vec![11, 12], false), (vec![21, 22], true)];
            let sender = ChouOrlandiBitGcOt::<U16, _, _>::new(
                BitGcRole::Garbler,
                garbler_ot_transport,
                SeedRng::new(0x5345_4e44),
            );
            let receiver = ChouOrlandiBitGcOt::<U16, _, _>::new(
                BitGcRole::Evaluator,
                evaluator_ot_transport,
                SeedRng::new(0x5245_4356),
            );

            let send = thread::spawn(move || {
                let mut ot = sender;
                garbler_session.send_evaluator_input_over::<U16, _, _>(
                    &mut garbler_frames,
                    &mut ot,
                    4,
                    97,
                    2,
                    &labels,
                )
            });
            let receive = thread::spawn(move || {
                let mut ot = receiver;
                evaluator_session.receive_evaluator_input_over::<U16, _, _>(
                    &mut evaluator_frames,
                    &mut ot,
                    4,
                    97,
                    2,
                    choice,
                )
            });
            send.join().unwrap().unwrap();
            let (label, masked) = receive.join().unwrap().unwrap();
            assert_eq!(label, if choice { vec![21, 22] } else { vec![11, 12] });
            assert_eq!(masked, choice);
        }
    }

    #[test]
    fn authenticated_chou_orlandi_ot_binds_messages_to_session_and_role() {
        let key = [0x42; 32];
        let (garbler_ot_transport, evaluator_ot_transport) = pipe_pair();
        let sender_transport = AuthenticatedBitGcOtTransport::new(
            BitGcRole::Garbler,
            binding(),
            key,
            garbler_ot_transport,
        );
        let receiver_transport = AuthenticatedBitGcOtTransport::new(
            BitGcRole::Evaluator,
            binding(),
            key,
            evaluator_ot_transport,
        );
        let sender = ChouOrlandiBitGcOt::<U16, _, _>::new(
            BitGcRole::Garbler,
            sender_transport,
            SeedRng::new(0x5345_4e44),
        );
        let receiver = ChouOrlandiBitGcOt::<U16, _, _>::new(
            BitGcRole::Evaluator,
            receiver_transport,
            SeedRng::new(0x5245_4356),
        );
        let sender_thread = thread::spawn(move || {
            let mut ot = sender;
            let blocks = [
                Array::<u8, U16>::from_fn(|i| match i {
                    0 => 31,
                    1 => 32,
                    _ => 0,
                }),
                Array::<u8, U16>::from_fn(|i| match i {
                    0 => 41,
                    1 => 42,
                    _ => 0,
                }),
            ];
            ot.send_pair([&blocks[0], &blocks[1]])
        });
        let receiver_thread = thread::spawn(move || {
            let mut ot = receiver;
            ot.receive_choice(true)
        });
        sender_thread.join().unwrap().unwrap();
        let received = receiver_thread.join().unwrap().unwrap();
        assert_eq!(&received.as_slice()[..2], &[41, 42]);
        assert!(received.as_slice()[2..].iter().all(|&byte| byte == 0));
    }

    #[test]
    fn keyed_ot_rejects_descriptor_context_mismatch() {
        let (sender_pipe, receiver_pipe) = pipe_pair();
        let mut sender = AuthenticatedBitGcOtTransport::new(
            BitGcRole::Garbler,
            binding(),
            [0x42; 32],
            sender_pipe,
        );
        let mut receiver = AuthenticatedBitGcOtTransport::new(
            BitGcRole::Evaluator,
            binding(),
            [0x42; 32],
            receiver_pipe,
        );
        sender.bind_context(&[1; 32]);
        receiver.bind_context(&[2; 32]);
        sender.send_ot_message(&[9; 16]).unwrap();
        assert_eq!(
            receiver.receive_ot_message(16),
            Err(AuthenticatedOtError::InvalidMessage)
        );
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
    fn canonical_digests_bind_circuit_and_parameter_shape() {
        let circuit = Circuit {
            inputs: 2,
            gates: vec![Gate::Xor { a: 0, b: 1 }],
            outputs: vec![2],
        };
        let digest = canonical_circuit_digest(&circuit).unwrap();
        let changed = Circuit {
            gates: vec![Gate::And { a: 0, b: 1 }],
            ..circuit.clone()
        };
        assert_ne!(digest, canonical_circuit_digest(&changed).unwrap());
        let profile =
            canonical_parameter_fingerprint(Variant::Small, 97, 8, &[1649, 1153]).unwrap();
        let changed_profile =
            canonical_parameter_fingerprint(Variant::Small, 97, 8, &[1649, 1157]).unwrap();
        assert_ne!(profile, changed_profile);
        let binding =
            canonical_session_binding([7; 32], Variant::Small, 97, 8, &[1649, 1153], &circuit)
                .unwrap();
        assert_eq!(binding.session_id, [7; 32]);
        assert_eq!(binding.parameter_fingerprint, profile);
        assert_eq!(binding.circuit_digest, digest);
    }

    #[test]
    fn canonical_digest_rejects_non_topological_circuit() {
        let circuit = Circuit {
            inputs: 1,
            gates: vec![Gate::Xor { a: 0, b: 1 }],
            outputs: vec![1],
        };
        assert_eq!(
            canonical_circuit_digest(&circuit),
            Err(BitGcDigestError::InvalidCircuit)
        );
    }

    #[test]
    fn hmac_sha256_matches_known_answer() {
        let key = [0x5a; 32];
        let tag = hmac_sha256(&key, &[b"Hi There"]);
        assert_eq!(
            tag,
            [
                0xd2, 0xab, 0x63, 0xe8, 0x59, 0xb5, 0x0a, 0x5d, 0xec, 0xd7, 0x54, 0x85, 0xc0, 0xca,
                0x30, 0x20, 0x34, 0x4e, 0x8a, 0x84, 0x87, 0xfb, 0x79, 0xf8, 0x91, 0xf0, 0x4c, 0x3a,
                0xf8, 0x3e, 0x8b, 0x60,
            ]
        );
    }

    #[test]
    fn authenticated_frames_bind_key_and_transcript() {
        let (mut garbler, mut evaluator) = authenticated_pair();
        handshake(&mut garbler, &mut evaluator);
        let frame = garbler.send_data(&[1, 2, 3]).unwrap();
        evaluator.accept(&frame).unwrap();
        let mut tampered = garbler.send_data(&[4, 5, 6]).unwrap();
        let last = tampered.len() - 1;
        tampered[last] ^= 1;
        assert_eq!(
            evaluator.accept(&tampered),
            Err(BitGcSessionError::AuthenticationFailed)
        );
        assert_eq!(evaluator.state(), BitGcSessionState::Aborted);

        let mut wrong_key =
            BitGcSession::new_authenticated(BitGcRole::Evaluator, binding(), [0x51; 32]).unwrap();
        let hello = BitGcSession::new_authenticated(BitGcRole::Garbler, binding(), [0x5a; 32])
            .unwrap()
            .hello()
            .unwrap();
        assert_eq!(
            wrong_key.accept(&hello),
            Err(BitGcSessionError::AuthenticationFailed)
        );
    }

    #[test]
    fn psk_handshake_derives_authenticated_session_key() {
        let mut garbler = BitGcSession::new_psk_authenticated(
            BitGcRole::Garbler,
            binding(),
            [0x33; 32],
            [0x44; 32],
        )
        .unwrap();
        let mut evaluator = BitGcSession::new_psk_authenticated(
            BitGcRole::Evaluator,
            binding(),
            [0x33; 32],
            [0x55; 32],
        )
        .unwrap();
        let evaluator_hello = evaluator.hello().unwrap();
        garbler.accept(&evaluator_hello).unwrap();
        let garbler_hello = garbler.hello().unwrap();
        assert_eq!(garbler_hello.len(), HEADER_BYTES + PSK_HELLO_BYTES);
        evaluator.accept(&garbler_hello).unwrap();
        assert_eq!(garbler.state(), BitGcSessionState::Active);
        assert_eq!(evaluator.state(), BitGcSessionState::Active);
        let data = garbler.send_data(&[6, 7, 8]).unwrap();
        let frame = evaluator.accept(&data).unwrap();
        assert_eq!(evaluator.payload_without_auth(&frame).unwrap(), &[6, 7, 8]);

        let mut wrong_key = BitGcSession::new_psk_authenticated(
            BitGcRole::Evaluator,
            binding(),
            [0x34; 32],
            [0x55; 32],
        )
        .unwrap();
        assert_eq!(
            wrong_key.accept(&garbler_hello),
            Err(BitGcSessionError::AuthenticationFailed)
        );
        assert_eq!(wrong_key.state(), BitGcSessionState::Aborted);
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
            incoming: vec![vec![0; BITGC_FRAME_MAX_BYTES + 1]],
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
        incoming: Vec<Vec<u8>>,
        sent: Vec<Vec<u8>>,
        fail_send: bool,
        cancelled: bool,
        receive_limit: usize,
    }

    impl TestTransport {
        fn new() -> Self {
            Self {
                incoming: Vec::new(),
                sent: Vec::new(),
                fail_send: false,
                cancelled: false,
                receive_limit: 0,
            }
        }
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
            if self.incoming.is_empty() {
                Err(())
            } else {
                Ok(self.incoming.remove(0))
            }
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
            incoming: vec![g_hello],
            sent: Vec::new(),
            fail_send: false,
            cancelled: false,
            receive_limit: 0,
        };
        evaluator.receive_over(&mut io).unwrap();
        assert_eq!(io.receive_limit, BITGC_FRAME_MAX_BYTES);
        assert_eq!(evaluator.state(), BitGcSessionState::Active);

        let mut failing = TestTransport {
            incoming: Vec::new(),
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

    #[test]
    fn material_delivery_roundtrip_and_bounds() {
        let (mut garbler, mut evaluator) = pair();
        handshake(&mut garbler, &mut evaluator);
        let (mut garbler_frames, mut evaluator_frames) = pipe_pair();
        let material: Vec<u8> = (0..100_000u32).map(|i| (i % 251) as u8).collect();
        let send_material = material.clone();
        let send = thread::spawn(move || {
            garbler
                .send_material_over(&mut garbler_frames, &send_material)
                .map(|_| garbler)
        });
        let receive = thread::spawn(move || {
            evaluator
                .receive_material_over(&mut evaluator_frames)
                .map(|bytes| (bytes, evaluator))
        });
        let garbler = send.join().unwrap().unwrap();
        let (received, evaluator) = receive.join().unwrap().unwrap();
        assert_eq!(received, material);
        assert_eq!(garbler.state(), BitGcSessionState::Active);
        assert_eq!(evaluator.state(), BitGcSessionState::Active);

        // Oversized material is rejected before sending.
        let mut garbler2 = BitGcSession::new(BitGcRole::Garbler, binding()).unwrap();
        let mut io = TestTransport::new();
        let oversized = vec![0u8; BITGC_MATERIAL_MAX_BYTES + 1];
        assert!(matches!(
            garbler2.send_material_over(&mut io, &oversized),
            Err(BitGcIoError::Protocol(BitGcSessionError::InvalidMaterial))
        ));
        assert_eq!(garbler2.state(), BitGcSessionState::Aborted);

        // A reordered chunk index aborts the evaluator.
        let (mut garbler3, mut evaluator3) = pair();
        handshake(&mut garbler3, &mut evaluator3);
        let (mut g_frames, mut e_frames) = pipe_pair();
        let mut payload = Vec::new();
        payload.push(MATERIAL_CHUNK_TAG);
        payload.extend_from_slice(&100u64.to_le_bytes());
        payload.extend_from_slice(&5u32.to_le_bytes());
        payload.extend_from_slice(&10u32.to_le_bytes());
        payload.extend_from_slice(&[7u8; 10]);
        let send = thread::spawn(move || {
            garbler3.send_over(&mut g_frames, BitGcFrameKind::Data, &payload)
        });
        let receive = thread::spawn(move || evaluator3.receive_material_over(&mut e_frames));
        send.join().unwrap().unwrap();
        assert!(matches!(
            receive.join().unwrap(),
            Err(BitGcIoError::Protocol(BitGcSessionError::InvalidMaterial))
        ));
    }

    #[test]
    fn garbler_input_direct_delivery_roundtrip() {
        let (mut garbler, mut evaluator) = pair();
        handshake(&mut garbler, &mut evaluator);
        let (mut garbler_frames, mut evaluator_frames) = pipe_pair();
        let label = (vec![3u64, 1, 4, 1, 5, 9, 2, 6], true);
        let send_label = label.clone();
        let send = thread::spawn(move || {
            garbler.send_garbler_input_over(&mut garbler_frames, 7, 97, 8, &send_label)
        });
        let receive = thread::spawn(move || {
            evaluator.receive_garbler_input_over(&mut evaluator_frames, 7, 97, 8)
        });
        send.join().unwrap().unwrap();
        let (got_label, got_masked) = receive.join().unwrap().unwrap();
        assert_eq!(got_label, label.0);
        assert_eq!(got_masked, label.1);

        // Wrong expected index aborts the evaluator.
        let (mut garbler, mut evaluator) = pair();
        handshake(&mut garbler, &mut evaluator);
        let (mut garbler_frames, mut evaluator_frames) = pipe_pair();
        let send = thread::spawn(move || {
            garbler.send_garbler_input_over(&mut garbler_frames, 7, 97, 8, &label)
        });
        let receive = thread::spawn(move || {
            evaluator.receive_garbler_input_over(&mut evaluator_frames, 8, 97, 8)
        });
        send.join().unwrap().unwrap();
        assert!(matches!(
            receive.join().unwrap(),
            Err(BitGcIoError::Protocol(
                BitGcSessionError::InvalidGarblerInput
            ))
        ));
    }

    #[test]
    fn per_role_runners_drive_circuit_over_pipes() {
        for variant in [Variant::Small, Variant::Fast] {
            let (circuit, dd, prf, prg_bits, pis, modulus_chain) =
                role_runner_fixture(variant, 0x9004_0001 + variant as u64);
            let mut rng = SeedRng::new(0x9004_0002 + variant as u64);
            let owners = vec![
                BitGcInputOwner::Garbler,
                BitGcInputOwner::Evaluator,
                BitGcInputOwner::Evaluator,
            ];
            let garbler_inputs = [true];
            let evaluator_choices = [true, false];
            let want = vec![(true && true) ^ false];
            let key = [0x77; 32];

            let (garbler_frames, evaluator_frames) = pipe_pair();
            let (garbler_ot_transport, evaluator_ot_transport) = pipe_pair();

            let garbler_binding =
                canonical_session_binding([0x9a; 32], variant, 97, 8, &modulus_chain, &circuit)
                    .unwrap();
            let evaluator_binding = garbler_binding;

            let garbler_circuit = circuit.clone();
            let garbler_modulus_chain = modulus_chain.clone();
            let garbler_owners = owners.clone();
            let garbler_handle = thread::spawn(move || {
                let circuit = garbler_circuit;
                let modulus_chain = garbler_modulus_chain;
                let owners = garbler_owners;
                let mut session =
                    BitGcSession::new_authenticated(BitGcRole::Garbler, garbler_binding, key)
                        .unwrap();
                let auth_transport = session
                    .authenticated_ot_transport(garbler_ot_transport)
                    .unwrap();
                let mut ot = ChouOrlandiBitGcOt::<U16, _, _>::new(
                    BitGcRole::Garbler,
                    auth_transport,
                    SeedRng::new(0x4752_424c),
                );
                let mut frames = garbler_frames;
                run_garbler_over::<_, U16, _, _, _>(
                    &mut session,
                    &circuit,
                    &dd,
                    &prf,
                    &prg_bits,
                    &mut rng,
                    &pis,
                    &modulus_chain,
                    &owners,
                    &garbler_inputs,
                    &mut frames,
                    &mut ot,
                )
            });

            let evaluator_handle = thread::spawn(move || {
                let mut session =
                    BitGcSession::new_authenticated(BitGcRole::Evaluator, evaluator_binding, key)
                        .unwrap();
                let auth_transport = session
                    .authenticated_ot_transport(evaluator_ot_transport)
                    .unwrap();
                let mut ot = ChouOrlandiBitGcOt::<U16, _, _>::new(
                    BitGcRole::Evaluator,
                    auth_transport,
                    SeedRng::new(0x4556_414c),
                );
                let mut frames = evaluator_frames;
                run_evaluator_over::<
                    volar_spec::bitgc::lattice_dd_rns::LatticeRnsEvaluator,
                    U16,
                    _,
                    _,
                >(
                    &mut session,
                    &circuit,
                    &modulus_chain,
                    &owners,
                    &evaluator_choices,
                    &mut frames,
                    &mut ot,
                )
            });

            garbler_handle.join().unwrap().unwrap();
            let got = evaluator_handle.join().unwrap().unwrap();
            assert_eq!(got, want, "{variant:?}");
        }
    }
}
