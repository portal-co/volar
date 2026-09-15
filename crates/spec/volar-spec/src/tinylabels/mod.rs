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
//! Ring-LWE field-element batch-select construction. It provides a canonical
//! 16-byte-label encoding, but the required CSPRNG and noise sampler plus a
//! framed transport still need independent protocol review. It deliberately
//! has no dependency on a particular
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

/// Canonical encoding error for a 16-byte garbling label.
///
/// The TinyLabels reference profile uses a 50-bit plaintext field. Three
/// elements therefore encode one 128-bit label without reducing its entropy:
/// the first two elements carry six little-endian bytes each and the third
/// carries four. Values outside that exact image are rejected on decode rather
/// than silently folded modulo the field.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LabelEncodingError {
    /// A field element is not below the TinyLabels plaintext modulus.
    FieldElementOutOfRange {
        /// Element position in the three-element encoding.
        index: usize,
    },
    /// A field element is in the field but not in this label encoding's exact
    /// 48-bit/48-bit/32-bit image.
    NonCanonicalElement {
        /// Element position in the three-element encoding.
        index: usize,
    },
}

/// Encode a 16-byte wire label as three canonical TinyLabels plaintexts.
///
/// This is an injective byte representation, not a label shortening or a
/// change to free-XOR. Decoding the result with [`decode_label_16`] returns the
/// original bytes exactly. Batch selection may operate on modular differences
/// of these elements; it must decode the selected output before treating it as
/// a garbling label.
pub fn encode_label_16(label: [u8; 16]) -> [u64; 3] {
    let first = u64::from_le_bytes([
        label[0], label[1], label[2], label[3], label[4], label[5], 0, 0,
    ]);
    let second = u64::from_le_bytes([
        label[6], label[7], label[8], label[9], label[10], label[11], 0, 0,
    ]);
    let third = u32::from_le_bytes([label[12], label[13], label[14], label[15]]) as u64;
    [first, second, third]
}

/// Decode the exact three-field-element image produced by [`encode_label_16`].
///
/// Rejecting field-valid but noncanonical values prevents two wire encodings
/// from becoming one label at the serialization seam.
pub fn decode_label_16(elements: [u64; 3]) -> Result<[u8; 16], LabelEncodingError> {
    const WIDTHS: [u32; 3] = [48, 48, 32];
    let modulus = ring_lwe::REFERENCE_PLAINTEXT_MODULUS;
    let mut label = [0u8; 16];
    let mut offset = 0usize;
    for (index, (&element, &width)) in elements.iter().zip(WIDTHS.iter()).enumerate() {
        if element >= modulus {
            return Err(LabelEncodingError::FieldElementOutOfRange { index });
        }
        if element >= (1u64 << width) {
            return Err(LabelEncodingError::NonCanonicalElement { index });
        }
        let bytes = element.to_le_bytes();
        let count = (width / 8) as usize;
        label[offset..offset + count].copy_from_slice(&bytes[..count]);
        offset += count;
    }
    Ok(label)
}

/// Encoded TinyLabels messages for a batch of 16-byte wire-label pairs.
///
/// `differences` is `K1 - K0 (mod p)` for [`ring_lwe::BatchSelect::enc1`].
/// `zeroes` is `K0` for [`ring_lwe::BatchSelect::enc2`]. A caller expands one
/// Boolean choice per wire with [`Self::expanded_choices`] before
/// `keygen`/`dec`, then converts the selected plaintexts back with
/// [`Self::decode_selected`]. This preserves the complete 128-bit label; it
/// is input-delivery preparation, never label shortening or material storage.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct EncodedLabelBatch {
    differences: alloc::vec::Vec<u64>,
    zeroes: alloc::vec::Vec<u64>,
}

impl EncodedLabelBatch {
    /// Encode validated free-XOR label pairs into Construction-3 message
    /// vectors. The common `offset` is checked before converting any bytes.
    pub fn from_pairs(pairs: &[LabelPair<16>], offset: [u8; 16]) -> Result<Self, BatchError> {
        let batch = LabelBatch::new(pairs, offset)?;
        let mut differences = alloc::vec::Vec::with_capacity(pairs.len() * 3);
        let mut zeroes = alloc::vec::Vec::with_capacity(pairs.len() * 3);
        let modulus = ring_lwe::REFERENCE_PLAINTEXT_MODULUS;
        for pair in batch.pairs {
            let zero = encode_label_16(pair.zero);
            let one = encode_label_16(pair.one);
            for (one, zero) in one.into_iter().zip(zero) {
                differences.push(if one >= zero {
                    one - zero
                } else {
                    modulus - (zero - one)
                });
                zeroes.push(zero);
            }
        }
        Ok(Self {
            differences,
            zeroes,
        })
    }

    /// Borrow the `K1 - K0` message vector in wire/limb order.
    pub fn differences(&self) -> &[u64] {
        &self.differences
    }

    /// Borrow the `K0` message vector in wire/limb order.
    pub fn zeroes(&self) -> &[u64] {
        &self.zeroes
    }

    /// Number of complete 16-byte label pairs represented by this batch.
    pub fn label_count(&self) -> usize {
        self.zeroes.len() / 3
    }

    /// Pad this message vector to the exact field-slot count required by one
    /// [`ring_lwe::BatchSelect`] instance. Padding is public zero data and is
    /// never decoded as a wire label.
    pub fn pad_to_slots(&self, slots: usize) -> Result<PaddedLabelBatch, LabelBatchPaddingError> {
        let used = self.zeroes.len();
        if slots < used {
            return Err(LabelBatchPaddingError::TooFewSlots { slots, used });
        }
        let mut differences = self.differences.clone();
        let mut zeroes = self.zeroes.clone();
        differences.resize(slots, 0);
        zeroes.resize(slots, 0);
        Ok(PaddedLabelBatch {
            label_count: self.label_count(),
            differences,
            zeroes,
        })
    }

    /// Expand one choice per wire into the three-element field representation.
    pub fn expanded_choices(choices: &[bool]) -> alloc::vec::Vec<bool> {
        let mut out = alloc::vec::Vec::with_capacity(choices.len() * 3);
        for &choice in choices {
            out.extend_from_slice(&[choice; 3]);
        }
        out
    }

    /// Decode selected field elements into exact 16-byte labels.
    pub fn decode_selected(
        selected: &[u64],
    ) -> Result<alloc::vec::Vec<[u8; 16]>, LabelBatchDecodeError> {
        if selected.len() % 3 != 0 {
            return Err(LabelBatchDecodeError::LengthMismatch);
        }
        selected
            .chunks_exact(3)
            .map(|chunk| {
                decode_label_16([chunk[0], chunk[1], chunk[2]])
                    .map_err(LabelBatchDecodeError::NonCanonicalLabel)
            })
            .collect()
    }
}

/// An encoded label batch padded to a concrete TinyLabels field-slot count.
///
/// The final `slots - label_count * 3` entries are public zero padding. This
/// object makes it impossible to accidentally interpret those entries as
/// labels after a batch-select decryption.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PaddedLabelBatch {
    label_count: usize,
    differences: alloc::vec::Vec<u64>,
    zeroes: alloc::vec::Vec<u64>,
}

impl PaddedLabelBatch {
    /// Exact number of field slots in this selection instance.
    pub fn slots(&self) -> usize {
        self.zeroes.len()
    }

    /// Number of non-padding labels in this batch.
    pub fn label_count(&self) -> usize {
        self.label_count
    }

    /// `K1 - K0` field messages including public zero padding.
    pub fn differences(&self) -> &[u64] {
        &self.differences
    }

    /// `K0` field messages including public zero padding.
    pub fn zeroes(&self) -> &[u64] {
        &self.zeroes
    }

    /// Expand choices and append false selections for public padding slots.
    pub fn expanded_choices(
        &self,
        choices: &[bool],
    ) -> Result<alloc::vec::Vec<bool>, LabelBatchPaddingError> {
        if choices.len() != self.label_count {
            return Err(LabelBatchPaddingError::ChoiceLengthMismatch {
                expected: self.label_count,
                actual: choices.len(),
            });
        }
        let mut out = EncodedLabelBatch::expanded_choices(choices);
        out.resize(self.slots(), false);
        Ok(out)
    }

    /// Recover only the non-padding labels from selected TinyLabels outputs.
    pub fn decode_selected(
        &self,
        selected: &[u64],
    ) -> Result<alloc::vec::Vec<[u8; 16]>, LabelBatchDecodeError> {
        if selected.len() != self.slots() {
            return Err(LabelBatchDecodeError::LengthMismatch);
        }
        EncodedLabelBatch::decode_selected(&selected[..self.label_count * 3])
    }
}

/// Error while preparing a field-slot-aligned TinyLabels batch.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LabelBatchPaddingError {
    /// The selected Ring-LWE profile cannot hold all three-field-element labels.
    TooFewSlots {
        /// Available field slots in the profile.
        slots: usize,
        /// Required slots before public padding.
        used: usize,
    },
    /// The caller did not provide one Boolean choice for every original label.
    ChoiceLengthMismatch {
        /// Number of labels in this padded batch.
        expected: usize,
        /// Number of provided choices.
        actual: usize,
    },
}

/// Error while decoding a selected vector of TinyLabels plaintexts.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LabelBatchDecodeError {
    /// The field-element vector cannot contain an integral number of labels.
    LengthMismatch,
    /// One three-element group is not a canonical encoded wire label.
    NonCanonicalLabel(LabelEncodingError),
}

/// The two raw labels allocated for one Boolean input wire.
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
    use super::{
        BatchError, EncodedLabelBatch, LabelBatch, LabelBatchDecodeError, LabelEncodingError,
        LabelPair, PAPER_PROFILE, SECURITY_BITS, decode_label_16, encode_label_16,
    };

    const OFFSET: [u8; 16] = [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];

    struct TestRandom(u64);

    impl super::ring_lwe::RandomSource for TestRandom {
        fn fill_bytes(&mut self, output: &mut [u8]) -> Result<(), super::ring_lwe::Error> {
            for byte in output {
                self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
                *byte = (self.0 >> 24) as u8;
            }
            Ok(())
        }
    }

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
    fn encoded_batch_expands_choices_and_reconstructs_selected_labels() {
        let pairs = [pair(0x20), pair(0x40), pair(0x60)];
        let encoded = EncodedLabelBatch::from_pairs(&pairs, OFFSET).expect("valid free-XOR pairs");
        assert_eq!(encoded.differences().len(), 9);
        assert_eq!(encoded.zeroes().len(), 9);
        assert_eq!(
            EncodedLabelBatch::expanded_choices(&[false, true, false]),
            alloc::vec![false, false, false, true, true, true, false, false, false]
        );

        let choices = [false, true, false];
        let mut selected = alloc::vec::Vec::with_capacity(9);
        for (wire, &choice) in choices.iter().enumerate() {
            for limb in 0..3 {
                let index = wire * 3 + limb;
                let zero = encoded.zeroes()[index];
                selected.push(if choice {
                    (zero + encoded.differences()[index])
                        % super::ring_lwe::REFERENCE_PLAINTEXT_MODULUS
                } else {
                    zero
                });
            }
        }
        assert_eq!(
            EncodedLabelBatch::decode_selected(&selected),
            Ok(alloc::vec![pairs[0].zero, pairs[1].one, pairs[2].zero])
        );
        assert_eq!(
            EncodedLabelBatch::decode_selected(&[0, 1]),
            Err(LabelBatchDecodeError::LengthMismatch)
        );
    }

    #[test]
    fn padded_batch_runs_the_staged_selector_and_discards_public_padding() {
        let pairs = [pair(0x20), pair(0x40), pair(0x60)];
        let encoded = EncodedLabelBatch::from_pairs(&pairs, OFFSET).expect("valid pairs");
        let padded = encoded
            .pad_to_slots(16)
            .expect("the test ring has sixteen slots");
        let choices = [false, true, false];
        let expanded = padded
            .expanded_choices(&choices)
            .expect("one choice per label");
        assert_eq!(expanded.len(), 16);
        assert!(expanded[9..].iter().all(|choice| !choice));

        let mut random = TestRandom(0xD1CE_BA5E);
        let mut noise = super::ring_lwe::ZeroNoise;
        let selector = super::ring_lwe::BatchSelect::setup(
            super::ring_lwe::Parameters::scaled_reference(8, 2),
            &mut random,
        )
        .expect("test selector");
        let first = selector
            .enc1(padded.differences(), &mut random, &mut noise)
            .expect("reusable difference ciphertext");
        let second = selector
            .enc2(padded.zeroes(), &mut random, &mut noise)
            .expect("per-use zero ciphertext");
        let key = selector
            .keygen(&first, &second, &expanded)
            .expect("choice key");
        let selected = selector
            .dec(&first, &second, &key, &expanded)
            .expect("selected field messages");
        assert_eq!(
            padded.decode_selected(&selected),
            Ok(alloc::vec![pairs[0].zero, pairs[1].one, pairs[2].zero])
        );
        assert_eq!(
            padded.expanded_choices(&[true]),
            Err(super::LabelBatchPaddingError::ChoiceLengthMismatch {
                expected: 3,
                actual: 1,
            })
        );
    }

    #[test]
    fn label_encoding_is_exact_injective_and_rejects_noncanonical_field_values() {
        let label = [
            0x00, 0xff, 0x42, 0x13, 0x99, 0x80, 0x7e, 0x51, 0x01, 0xa5, 0xfe, 0x11, 0x88, 0, 0x33,
            0xcc,
        ];
        let encoded = encode_label_16(label);
        assert_eq!(decode_label_16(encoded), Ok(label));
        assert_eq!(
            decode_label_16([1 << 48, 0, 0]),
            Err(LabelEncodingError::NonCanonicalElement { index: 0 })
        );
        assert_eq!(
            decode_label_16([super::ring_lwe::REFERENCE_PLAINTEXT_MODULUS, 0, 0]),
            Err(LabelEncodingError::FieldElementOutOfRange { index: 0 })
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
