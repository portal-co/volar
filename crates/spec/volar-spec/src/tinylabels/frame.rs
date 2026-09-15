//! @volar-allow-vec: runtime-boundary: OT/VOLE/FAEST protocol material,
//! transcripts, and batched commitments are runtime-sized host protocol
//! buffers, not weaver-known compiled-program shapes; this module-level
//! exemption applies to the whole file.
//! Canonical bounded TinyLabels stage-frame envelope.
//!
//! This module binds a stage payload to one parameter profile, session,
//! manifest, and monotonic use counter. It intentionally does not define the
//! inner Ring-LWE polynomial encoding: that encoding must be added with its
//! own canonical representation and interoperability vectors, rather than
//! treating an in-memory NTT limb layout as a wire format.
//!
//! The envelope provides unambiguous framing and bounded parsing. Integrity
//! and peer authentication remain responsibilities of the enclosing
//! transcript-authenticated transport.

extern crate alloc;

use alloc::vec::Vec;

const MAGIC: &[u8; 8] = b"VTLBL001";
const VERSION: u16 = 1;
const HEADER_BYTES: usize = 8 + 2 + 1 + 32 + 32 + 32 + 8 + 4;

/// The public phase carried by a TinyLabels frame.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(u8)]
pub enum Stage {
    /// Construction public parameters.
    PublicParameters = 1,
    /// Reusable `enc1` ciphertext.
    ReusableCiphertext = 2,
    /// Per-use `enc2` ciphertext.
    PerUseCiphertext = 3,
    /// Selection/key material.
    SelectionKey = 4,
    /// Explicit successful completion marker.
    Complete = 5,
    /// Explicit protocol failure marker.
    Error = 6,
}

impl TryFrom<u8> for Stage {
    type Error = FrameError;

    fn try_from(value: u8) -> Result<Self, FrameError> {
        match value {
            1 => Ok(Self::PublicParameters),
            2 => Ok(Self::ReusableCiphertext),
            3 => Ok(Self::PerUseCiphertext),
            4 => Ok(Self::SelectionKey),
            5 => Ok(Self::Complete),
            6 => Ok(Self::Error),
            _ => Err(FrameError::UnknownStage),
        }
    }
}

/// Public binding fields for one TinyLabels frame.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct FrameBinding {
    /// Digest/fingerprint of the complete parameter profile and field codec.
    pub parameter_fingerprint: [u8; 32],
    /// Session identifier supplied by the enclosing transcript protocol.
    pub session_id: [u8; 32],
    /// Digest of the ordered external-input manifest.
    pub manifest_digest: [u8; 32],
    /// Monotonically increasing use number within `session_id`.
    pub use_counter: u64,
}

/// One bounded TinyLabels stage frame.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Frame {
    /// Public construction stage.
    pub stage: Stage,
    /// Public context that makes cross-session/stage replay detectable.
    pub binding: FrameBinding,
    /// Canonically encoded stage payload, interpreted only by that stage's
    /// future polynomial codec.
    pub payload: Vec<u8>,
}

/// Canonical frame parse/encoding error.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum FrameError {
    /// Input did not contain a complete fixed header.
    Truncated,
    /// Magic bytes or version do not identify this frame format.
    UnsupportedFormat,
    /// The stage tag is not assigned by this version.
    UnknownStage,
    /// Declared payload exceeds the caller's bounded admission limit.
    PayloadTooLarge,
    /// The declared payload length does not exactly consume the input.
    LengthMismatch,
}

impl Frame {
    /// Serialize this frame in one canonical byte layout.
    pub fn encode(&self) -> Vec<u8> {
        let mut out = Vec::with_capacity(HEADER_BYTES + self.payload.len());
        out.extend_from_slice(MAGIC);
        out.extend_from_slice(&VERSION.to_le_bytes());
        out.push(self.stage as u8);
        out.extend_from_slice(&self.binding.parameter_fingerprint);
        out.extend_from_slice(&self.binding.session_id);
        out.extend_from_slice(&self.binding.manifest_digest);
        out.extend_from_slice(&self.binding.use_counter.to_le_bytes());
        out.extend_from_slice(&(self.payload.len() as u32).to_le_bytes());
        out.extend_from_slice(&self.payload);
        out
    }

    /// Parse a frame, rejecting trailing data and payloads above `max_payload`.
    /// The caller must select the limit before allocating from peer-controlled
    /// input, using its explicit resource-admission policy.
    pub fn decode(input: &[u8], max_payload: usize) -> Result<Self, FrameError> {
        if input.len() < HEADER_BYTES {
            return Err(FrameError::Truncated);
        }
        if &input[..8] != MAGIC || u16::from_le_bytes([input[8], input[9]]) != VERSION {
            return Err(FrameError::UnsupportedFormat);
        }
        let stage = Stage::try_from(input[10])?;
        let mut offset = 11;
        let mut take_32 = || {
            let mut value = [0u8; 32];
            value.copy_from_slice(&input[offset..offset + 32]);
            offset += 32;
            value
        };
        let parameter_fingerprint = take_32();
        let session_id = take_32();
        let manifest_digest = take_32();
        let use_counter = u64::from_le_bytes(input[offset..offset + 8].try_into().unwrap());
        offset += 8;
        let payload_len =
            u32::from_le_bytes(input[offset..offset + 4].try_into().unwrap()) as usize;
        offset += 4;
        if payload_len > max_payload {
            return Err(FrameError::PayloadTooLarge);
        }
        if input.len() != offset.saturating_add(payload_len) {
            return Err(FrameError::LengthMismatch);
        }
        Ok(Self {
            stage,
            binding: FrameBinding {
                parameter_fingerprint,
                session_id,
                manifest_digest,
                use_counter,
            },
            payload: input[offset..].to_vec(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::{Frame, FrameBinding, FrameError, Stage};

    fn frame() -> Frame {
        Frame {
            stage: Stage::ReusableCiphertext,
            binding: FrameBinding {
                parameter_fingerprint: [1; 32],
                session_id: [2; 32],
                manifest_digest: [3; 32],
                use_counter: 9,
            },
            payload: alloc::vec![4, 5, 6],
        }
    }

    #[test]
    fn frame_round_trips_with_all_bindings() {
        let frame = frame();
        assert_eq!(Frame::decode(&frame.encode(), 3), Ok(frame));
    }

    #[test]
    fn frame_rejects_unbounded_or_noncanonical_input() {
        let mut encoded = frame().encode();
        assert_eq!(Frame::decode(&encoded, 2), Err(FrameError::PayloadTooLarge));
        encoded.push(0);
        assert_eq!(Frame::decode(&encoded, 4), Err(FrameError::LengthMismatch));
        assert_eq!(Frame::decode(&encoded[..8], 4), Err(FrameError::Truncated));
    }
}
