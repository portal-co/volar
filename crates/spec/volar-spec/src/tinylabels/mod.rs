#![warn(missing_docs)]

//! Shared offline/online input-label batching and experimental TinyLabels Ring-LWE.
//!
//! This module owns the construction-neutral input-label seam. Session and
//! interpreter adapters remain in their respective parent repositories. An offline [`LabelBatch`] validates the global free-XOR
//! offset and the label pair for every input wire. At runtime it writes the
//! selected labels into a caller-provided buffer for a garbled-circuit
//! evaluator.
//!
//! [`LabelBatch::select`] is intentionally a local reference adapter. It is
//! useful for correctness tests and for defining the allocation-free embedded
//! interface, but it is **not** a TinyLabels/Ring-LWE protocol and makes no
//! privacy or integrity claim. [`ring_lwe`] now implements the paper's staged
//! Ring-LWE field-element batch-select construction, but it deliberately does
//! not turn raw labels into field elements: that canonical mapping, the
//! required CSPRNG and noise sampler, and a framed transport need independent
//! protocol review. It deliberately has no dependency on a particular
//! garbling-table format, transport, strict session, or interpreter.
//!
//! The [`PAPER_PROFILE`] constants record the reported 128-bit-security
//! benchmark shape from ePrint 2024/2048. They are compatibility targets, not
//! enough to make a deployment-security claim: label encoding, noise sampling,
//! parameter review, and transcript formats still require validation.

/// Experimental Ring-LWE batch selection following TinyLabels Construction 3.
///
/// This module is intentionally separate from the local [`LabelBatch`] adapter.
/// It implements the construction's arithmetic and staged `setup`, `enc1`,
/// `enc2`, `keygen`, and `dec` flow, but deliberately leaves the caller in
/// charge of a reviewed CSPRNG, the paper's discrete-noise distribution, and a
/// canonical mapping between garbling labels and field elements. See
/// [`ring_lwe::BatchSelect`] for the security and interoperability boundaries.
pub mod ring_lwe;

/// The claimed security target for the current deployment profile.
pub const SECURITY_BITS: usize = 128;

/// A reported TinyLabels parameter/profile shape from the paper.
///
/// The values are descriptive only. They must not be treated as a deployment
/// parameter set until label encoding, noise sampling, and transcript handling
/// have interoperable tests and an independent security review.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct PaperProfile {
    /// The target security level in bits.
    pub security_bits: usize,
    /// The Ring-LWE polynomial degree.
    pub ring_degree: usize,
    /// The reported modulus width.
    pub modulus_bits: usize,
    /// The reported batch size of selected messages.
    pub batch_messages: usize,
}

/// The profile reported for the paper's selected benchmark.
pub const PAPER_PROFILE: PaperProfile = PaperProfile {
    security_bits: SECURITY_BITS,
    ring_degree: 4_096,
    modulus_bits: 109,
    batch_messages: 699_050,
};

/// The two raw labels allocated for one Boolean input wire.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct LabelPair<const N: usize> {
    /// The label selected when this input bit is false.
    pub zero: [u8; N],
    /// The label selected when this input bit is true.
    pub one: [u8; N],
}

/// An invalid offline input-label batch.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BatchError {
    /// A free-XOR offset must have a set low bit.
    EvenOffset,
    /// A label pair does not differ by the batch's free-XOR offset.
    MismatchedPair {
        /// The zero-based input-wire index of the invalid pair.
        index: usize,
    },
    /// The runtime choices and output buffer do not have the batch length.
    LengthMismatch,
}

/// Validated offline input-label material.
///
/// It borrows label pairs so an embedded caller can keep them in flash or in a
/// protocol-specific offline store. [`LabelBatch::select`] does not allocate
/// and can write directly into a network-frame or evaluator-input buffer.
pub struct LabelBatch<'a, const N: usize> {
    pairs: &'a [LabelPair<N>],
}

impl<'a, const N: usize> LabelBatch<'a, N> {
    /// Validate `pairs` against their common free-XOR `offset`.
    ///
    /// `offset` must be nonempty and have a set low bit. Every `one` label
    /// must equal its `zero` label XOR `offset`.
    pub fn new(pairs: &'a [LabelPair<N>], offset: [u8; N]) -> Result<Self, BatchError> {
        if N == 0 || offset[0] & 1 == 0 {
            return Err(BatchError::EvenOffset);
        }
        for (index, pair) in pairs.iter().enumerate() {
            if pair.one != core::array::from_fn(|byte| pair.zero[byte] ^ offset[byte]) {
                return Err(BatchError::MismatchedPair { index });
            }
        }
        Ok(Self { pairs })
    }

    /// Return the number of input wires in this batch.
    pub const fn len(&self) -> usize {
        self.pairs.len()
    }

    /// Return whether this batch has no input wires.
    pub const fn is_empty(&self) -> bool {
        self.pairs.is_empty()
    }

    /// Select labels for `choices` into `output` without allocating.
    ///
    /// This reference adapter runs at the garbler, which is permitted to know
    /// the choices. Only the selected raw labels belong in the evaluator's
    /// online input stream. A future TinyLabels encoder will keep this
    /// interface while changing how that stream is derived and transferred.
    pub fn select(&self, choices: &[bool], output: &mut [[u8; N]]) -> Result<(), BatchError> {
        if choices.len() != self.pairs.len() || output.len() != self.pairs.len() {
            return Err(BatchError::LengthMismatch);
        }
        for ((pair, choice), selected) in self
            .pairs
            .iter()
            .zip(choices.iter().copied())
            .zip(output.iter_mut())
        {
            *selected = if choice { pair.one } else { pair.zero };
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::{BatchError, LabelBatch, LabelPair, PAPER_PROFILE, SECURITY_BITS};

    const OFFSET: [u8; 16] = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];

    fn pair(zero: u8) -> LabelPair<16> {
        let zero = [zero; 16];
        LabelPair {
            zero,
            one: core::array::from_fn(|byte| zero[byte] ^ OFFSET[byte]),
        }
    }

    #[test]
    fn local_reference_adapter_selects_only_the_requested_labels() {
        let pairs = [pair(0x20), pair(0x40), pair(0x60), pair(0x80)];
        let batch = LabelBatch::new(&pairs, OFFSET).expect("the pairs share the offset");
        let mut selected = [[0; 16]; 4];

        batch
            .select(&[false, true, true, false], &mut selected)
            .expect("the provided buffers match the validated batch");

        assert_eq!(
            selected,
            [pairs[0].zero, pairs[1].one, pairs[2].one, pairs[3].zero]
        );
    }

    #[test]
    fn validation_rejects_a_nonfree_xor_pair_and_wrong_runtime_lengths() {
        assert!(matches!(
            LabelBatch::new(&[pair(0)], [0; 16]),
            Err(BatchError::EvenOffset)
        ));
        let invalid = LabelPair {
            zero: [0; 16],
            one: [0; 16],
        };
        assert!(matches!(
            LabelBatch::new(&[invalid], OFFSET),
            Err(BatchError::MismatchedPair { index: 0 })
        ));

        let pairs = [pair(0), pair(2)];
        let batch = LabelBatch::new(&pairs, OFFSET).unwrap();
        let mut output = [[0; 16]; 1];
        assert_eq!(
            batch.select(&[true, false], &mut output),
            Err(BatchError::LengthMismatch)
        );
    }

    #[test]
    fn reported_profile_keeps_the_current_security_target_visible() {
        assert_eq!(PAPER_PROFILE.security_bits, SECURITY_BITS);
        assert_eq!(PAPER_PROFILE.ring_degree, 4_096);
        assert_eq!(PAPER_PROFILE.modulus_bits, 109);
        assert_eq!(PAPER_PROFILE.batch_messages, 699_050);
    }
}
