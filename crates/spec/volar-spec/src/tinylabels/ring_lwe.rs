//! @volar-allow-vec: runtime-boundary: OT/VOLE/FAEST protocol material,
//! transcripts, and batched commitments are runtime-sized host protocol
//! buffers, not weaver-known compiled-program shapes; this module-level
//! exemption applies to the whole file.
//! Ring-LWE batch selection from TinyLabels Construction 3.
//!
//! The implementation follows the staged construction in Dietz, Li, and Lin,
//! [*TinyLabels: How to Compress Garbled Circuit Input Labels,
//! Efficiently*](https://eprint.iacr.org/2024/2048.pdf), Construction 3. It
//! combines the paper's LEnc tree with the LHE linear homomorphic encryption
//! layer. The parameter values in `REFERENCE_PARAMETERS` are taken from the
//! authors' EUROCRYPT 2025 artifact's pinned
//! [`batchselect.h`](https://github.com/MarianDietz/tinylabels/blob/46107cce5e1c10f3906da07c6c3666d3f40eb734/native/tinylabels/batchselect.h).
//!
//! ## Deliberate boundaries
//!
//! This is an experimental mathematical implementation, not a production
//! protocol. In particular, callers must supply a cryptographically secure
//! random source and an independently reviewed implementation of the paper's
//! clipped discrete-Gaussian noise distribution through `RandomSource` and
//! `NoiseSource`. `ZeroNoise` exists only to make deterministic
//! correctness tests possible and must never be used for protected data.
//!
//! The public values here are in-memory typed values, not a wire format. The
//! authors' `*.bin` files are unversioned host-endian dumps of SEAL's internal
//! NTT representation; they are not a portable protocol serialization. Nor
//! does this module define the required canonical conversion from a 128-bit
//! free-XOR garbling label to the paper's three field elements. Those choices
//! remain outside the stable garbling-table formats.
//!
//! The pinned authors' reference is a useful functional benchmark but is not a
//! byte-level interoperability target: its README disclaims production use,
//! and its `Lenc::enc` path allocates `r` without sampling it even though
//! Construction 1 requires random `r`. This module deliberately samples that
//! vector through `RandomSource`. The audit detail and source links are
//! recorded in `RESEARCH.md` beside this crate.

use alloc::vec;
use alloc::vec::Vec;

/// The plaintext modulus used by the pinned authors' profile.
pub const REFERENCE_PLAINTEXT_MODULUS: u64 = 1_125_899_906_826_241;

/// The second Ring-LWE modulus factor used by the pinned authors' profile.
pub const REFERENCE_DELTA: u64 = 576_460_752_303_415_297;

/// The standard deviation of the reference's small error distribution.
pub const SMALL_NOISE_STANDARD_DEVIATION: u64 = 4;

/// The absolute bound of the reference's small error distribution.
pub const SMALL_NOISE_MAX_DEVIATION: u64 = 512;

/// The standard deviation of the reference's large error distribution.
pub const LARGE_NOISE_STANDARD_DEVIATION: u64 = 1_000;

/// The absolute bound of the reference's large error distribution.
pub const LARGE_NOISE_MAX_DEVIATION: u64 = 128_000;

/// Ring and gadget parameters for one batch-select instance.
///
/// The ring is `Z_(p * delta)[X] / (X^degree + 1)`. `width` packed
/// polynomials provide `degree * width` scalar slots. `gadget_base` and
/// `gadget_digits` must provide a base decomposition larger than `p * delta`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Parameters {
    /// The power-of-two polynomial degree.
    pub degree: usize,
    /// The power-of-two number of packed polynomials in a batch.
    pub width: usize,
    /// The plaintext modulus `p`.
    pub plaintext_modulus: u64,
    /// The second modulus factor `delta`, so the Ring-LWE modulus is `p * delta`.
    pub delta: u64,
    /// The base `g` used for gadget decomposition.
    pub gadget_base: u64,
    /// The number of base-`g` gadget digits.
    pub gadget_digits: usize,
}

/// The degree-4096 reference profile used by the authors' artifact.
///
/// It has 2^21 scalar slots. A 128-bit garbling label occupies three field
/// elements in the paper's evaluation, yielding 699,050 labels per batch.
pub const REFERENCE_PARAMETERS: Parameters = Parameters {
    degree: 4_096,
    width: 512,
    plaintext_modulus: REFERENCE_PLAINTEXT_MODULUS,
    delta: REFERENCE_DELTA,
    gadget_base: 1 << 28,
    gadget_digits: 4,
};

impl Parameters {
    /// Return a scaled test profile with the reference moduli and gadget.
    ///
    /// `degree` and `width` must be powers of two, and `degree` must divide
    /// 4096 so the reference moduli retain their required negacyclic NTT root.
    /// This helper is intended for semantic tests and measurements, not for a
    /// claim about the security of the scaled profile.
    pub const fn scaled_reference(degree: usize, width: usize) -> Self {
        Self {
            degree,
            width,
            ..REFERENCE_PARAMETERS
        }
    }

    /// Return the number of scalar field elements in this batch.
    pub fn slots(self) -> Result<usize, Error> {
        self.degree
            .checked_mul(self.width)
            .ok_or(Error::InvalidParameters)
    }

    fn levels(self) -> Result<usize, Error> {
        if self.width < 2 || !self.width.is_power_of_two() {
            return Err(Error::InvalidParameters);
        }
        Ok(self.width.ilog2() as usize)
    }

    fn validate(self) -> Result<(), Error> {
        if self.degree < 2
            || !self.degree.is_power_of_two()
            || self.width < 2
            || !self.width.is_power_of_two()
            || self.plaintext_modulus < 3
            || self.delta < 3
            || self.gadget_base < 2
            || self.gadget_digits == 0
            || (self.plaintext_modulus - 1) % (2 * self.degree as u64) != 0
            || (self.delta - 1) % (2 * self.degree as u64) != 0
        {
            return Err(Error::InvalidParameters);
        }
        self.slots()?;

        let modulus = self.plaintext_modulus as u128 * self.delta as u128;
        let mut capacity = 1_u128;
        for _ in 0..self.gadget_digits {
            capacity = capacity
                .checked_mul(self.gadget_base as u128)
                .ok_or(Error::InvalidParameters)?;
        }
        if capacity <= modulus {
            return Err(Error::InvalidParameters);
        }
        Ok(())
    }
}

/// A batch-select construction error.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Error {
    /// Parameters cannot support the required NTT, packing, or gadget range.
    InvalidParameters,
    /// An input, choice, or output slice has the wrong number of scalar slots.
    LengthMismatch,
    /// A scalar input is not an element of the plaintext field.
    NonCanonicalPlaintext,
    /// The caller's random source failed.
    Randomness,
    /// The caller's error sampler failed or yielded a value outside its bound.
    Noise,
    /// A required modular inverse or root of unity could not be constructed.
    Arithmetic,
}

/// A source of cryptographically secure random bytes.
///
/// Each `fill_bytes` invocation must be independent and unpredictable to the
/// recipient. A deterministic source is appropriate only for test vectors.
pub trait RandomSource {
    /// Fill `output` with independent random bytes.
    fn fill_bytes(&mut self, output: &mut [u8]) -> Result<(), Error>;
}

/// A source of bounded, centered integer Ring-LWE errors.
///
/// The selected profile requests the clipped distributions published by the
/// paper: [`SMALL_NOISE_STANDARD_DEVIATION`] / [`SMALL_NOISE_MAX_DEVIATION`]
/// for LEnc and LHE `enc1`, and
/// [`LARGE_NOISE_STANDARD_DEVIATION`] / [`LARGE_NOISE_MAX_DEVIATION`] for
/// `enc2`. Implementations must sample each output independently. This trait
/// makes that cryptographic policy explicit instead of silently replacing it
/// with an unrelated distribution.
pub trait NoiseSource {
    /// Fill `output` with samples whose absolute values do not exceed `bound`.
    fn sample(
        &mut self,
        standard_deviation: u64,
        bound: u64,
        output: &mut [i64],
    ) -> Result<(), Error>;
}

/// Deterministic zero error for semantic tests.
///
/// This type removes the Ring-LWE hiding property and must never be used with
/// secrets, production labels, or performance/security measurements.
#[derive(Clone, Copy, Debug, Default)]
pub struct ZeroNoise;

impl NoiseSource for ZeroNoise {
    fn sample(
        &mut self,
        _standard_deviation: u64,
        _bound: u64,
        output: &mut [i64],
    ) -> Result<(), Error> {
        output.fill(0);
        Ok(())
    }
}

/// Opaque two-limb RNS polynomial in the construction's NTT representation.
///
/// The first limb is modulo [`Parameters::plaintext_modulus`] and the second
/// is modulo [`Parameters::delta`]. It is exposed only as typed staging data;
/// callers cannot rely on its representation as a wire format.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Polynomial {
    first: Vec<u64>,
    second: Vec<u64>,
}

impl Polynomial {
    /// Return the polynomial degree.
    pub fn degree(&self) -> usize {
        self.first.len()
    }

    /// Borrow the two RNS limbs in internal NTT order.
    ///
    /// This makes bounded streaming adapters possible, but it is deliberately
    /// not a protocol serialization. Frames must bind the parameters,
    /// endianness, stage, element index, and integrity protection before these
    /// values cross a process or network boundary.
    pub fn rns_limbs(&self) -> (&[u64], &[u64]) {
        (&self.first, &self.second)
    }
}

/// Public parameters for the paper's LHE and LEnc layers.
///
/// This in-memory holder is deliberately a host/reference adapter. Its full
/// reference profile occupies far more memory than the target microcontroller;
/// an integrator must frame and stream the polynomials rather than serialize
/// this type directly.
#[derive(Clone, Debug)]
pub struct PublicParameters {
    parameters: Parameters,
    lhe_a: Vec<Polynomial>,
    lenc_b: Vec<Polynomial>,
}

impl PublicParameters {
    /// Return the parameters bound to these public polynomials.
    pub const fn parameters(&self) -> Parameters {
        self.parameters
    }

    /// Iterate over the public LHE polynomials in their construction order.
    pub fn lhe_polynomials(&self) -> impl ExactSizeIterator<Item = &Polynomial> {
        self.lhe_a.iter()
    }

    /// Iterate over the public LEnc polynomials in their construction order.
    pub fn lenc_polynomials(&self) -> impl ExactSizeIterator<Item = &Polynomial> {
        self.lenc_b.iter()
    }
}

/// Reusable first ciphertext plus sender state retained for key generation.
#[derive(Clone, Debug)]
pub struct FirstCiphertext {
    lhe_state: Vec<Polynomial>,
    lhe_ciphertext: Vec<Polynomial>,
    lenc_ciphertext: Vec<Polynomial>,
}

impl FirstCiphertext {
    /// Iterate over the recipient-visible LHE ciphertext polynomials.
    ///
    /// Sender state used only by `keygen` is intentionally not exposed here.
    pub fn lhe_polynomials(&self) -> impl ExactSizeIterator<Item = &Polynomial> {
        self.lhe_ciphertext.iter()
    }

    /// Iterate over the recipient-visible LEnc ciphertext polynomials.
    pub fn lenc_polynomials(&self) -> impl ExactSizeIterator<Item = &Polynomial> {
        self.lenc_ciphertext.iter()
    }
}

/// Per-evaluation second ciphertext plus sender state retained for key generation.
#[derive(Clone, Debug)]
pub struct SecondCiphertext {
    lhe_state: Polynomial,
    lhe_ciphertext: Vec<Polynomial>,
}

impl SecondCiphertext {
    /// Iterate over the recipient-visible LHE ciphertext polynomials.
    ///
    /// Sender state used only by `keygen` is intentionally not exposed here.
    pub fn polynomials(&self) -> impl ExactSizeIterator<Item = &Polynomial> {
        self.lhe_ciphertext.iter()
    }
}

/// An input-dependent decryption key.
#[derive(Clone, Debug)]
pub struct SelectionKey {
    key: Polynomial,
}

impl SelectionKey {
    /// Borrow the recipient-visible input-dependent key polynomial.
    pub fn polynomial(&self) -> &Polynomial {
        &self.key
    }
}

/// A staged implementation of TinyLabels batch-select.
///
/// `setup` creates [`PublicParameters`]. The sender uses [`Self::enc1`] once
/// for the reusable label-difference vector and [`Self::enc2`] for every
/// label-zero vector. Once the selection bits are known, it makes a
/// [`SelectionKey`] with [`Self::keygen`]. The recipient calls [`Self::dec`]
/// with public ciphertexts, that key, and the same choices to recover the
/// coordinate-wise `l1 * choice + l2 (mod p)` result.
///
/// The stages intentionally accept and return typed values rather than an
/// unauthenticated byte stream. Canonical framing, label encoding, ownership
/// of choices, and transport remain protocol-level work.
#[derive(Clone, Debug)]
pub struct BatchSelect {
    ring: Ring,
    public: PublicParameters,
}

impl BatchSelect {
    /// Sample the public LHE and LEnc parameters for `parameters`.
    pub fn setup(parameters: Parameters, random: &mut impl RandomSource) -> Result<Self, Error> {
        let ring = Ring::new(parameters)?;
        let mut lhe_a = Vec::with_capacity(parameters.width);
        for _ in 0..parameters.width {
            lhe_a.push(ring.uniform(random)?);
        }
        let mut lenc_b = Vec::with_capacity(2 * parameters.gadget_digits);
        for _ in 0..2 * parameters.gadget_digits {
            lenc_b.push(ring.uniform(random)?);
        }
        Ok(Self {
            ring,
            public: PublicParameters {
                parameters,
                lhe_a,
                lenc_b,
            },
        })
    }

    /// Return the construction's public parameters.
    pub fn public_parameters(&self) -> &PublicParameters {
        &self.public
    }

    /// Encrypt a reusable vector of label differences, `l1`.
    ///
    /// Each element must be canonical modulo `p`; callers that use 128-bit
    /// labels must first perform the still-to-be-specified canonical label
    /// encoding into field elements.
    pub fn enc1(
        &self,
        l1: &[u64],
        random: &mut impl RandomSource,
        noise: &mut impl NoiseSource,
    ) -> Result<FirstCiphertext, Error> {
        let messages = self.ring.encode_messages(l1)?;
        let (random_vector, lenc_ciphertext) = self.lenc_enc(&messages, random, noise)?;
        let (lhe_state, lhe_ciphertext) = self.lhe_enc1(&random_vector, random, noise)?;
        Ok(FirstCiphertext {
            lhe_state,
            lhe_ciphertext,
            lenc_ciphertext,
        })
    }

    /// Encrypt a per-evaluation vector of zero labels, `l2`.
    pub fn enc2(
        &self,
        l2: &[u64],
        random: &mut impl RandomSource,
        noise: &mut impl NoiseSource,
    ) -> Result<SecondCiphertext, Error> {
        let mut messages = self.ring.encode_messages(l2)?;
        for message in &mut messages {
            self.ring.add_noise(
                message,
                noise,
                LARGE_NOISE_STANDARD_DEVIATION,
                LARGE_NOISE_MAX_DEVIATION,
            )?;
        }
        let (lhe_state, lhe_ciphertext) = self.lhe_enc2(&messages, random, noise)?;
        Ok(SecondCiphertext {
            lhe_state,
            lhe_ciphertext,
        })
    }

    /// Derive the recipient key for `choices` from sender-retained states.
    pub fn keygen(
        &self,
        first: &FirstCiphertext,
        second: &SecondCiphertext,
        choices: &[bool],
    ) -> Result<SelectionKey, Error> {
        self.check_first(first)?;
        self.check_second(second)?;
        let choice = self.ring.encode_choices(choices)?;
        let tree = self.lenc_digest(&choice)?;
        let digits = self.ring.decompose(&tree.digest)?;
        let mut key = second.lhe_state.clone();
        for (state, digit) in first.lhe_state.iter().zip(digits.iter()) {
            key.add_assign(&self.ring, &state.product(&self.ring, digit));
        }
        Ok(SelectionKey { key })
    }

    /// Decrypt the selected field elements for `choices`.
    ///
    /// The result is the coordinate-wise `l1 * choices + l2` modulo the
    /// plaintext modulus, assuming the supplied ciphertexts and key came from
    /// the preceding stages with matching public parameters.
    pub fn dec(
        &self,
        first: &FirstCiphertext,
        second: &SecondCiphertext,
        key: &SelectionKey,
        choices: &[bool],
    ) -> Result<Vec<u64>, Error> {
        self.check_first(first)?;
        self.check_second(second)?;
        if key.key.degree() != self.ring.parameters.degree {
            return Err(Error::LengthMismatch);
        }
        let choice = self.ring.encode_choices(choices)?;
        let tree = self.lenc_digest(&choice)?;
        let digits = self.ring.decompose(&tree.digest)?;
        let mut result = Vec::with_capacity(self.ring.parameters.width);
        for index in 0..self.ring.parameters.width {
            let row = &first.lhe_ciphertext[index * self.ring.parameters.gadget_digits
                ..(index + 1) * self.ring.parameters.gadget_digits];
            let mut value = inner_product(&self.ring, row, &digits)?;
            value.add_assign(&self.ring, &second.lhe_ciphertext[index]);
            value.sub_assign(
                &self.ring,
                &self.public.lhe_a[index].product(&self.ring, &key.key),
            );
            result.push(value);
        }
        let correction = self.lenc_eval(&first.lenc_ciphertext, &tree)?;
        for (value, delta) in result.iter_mut().zip(correction.iter()) {
            value.sub_assign(&self.ring, delta);
        }
        self.ring.decode_messages(&result)
    }

    fn check_first(&self, first: &FirstCiphertext) -> Result<(), Error> {
        let p = self.ring.parameters;
        if first.lhe_state.len() != p.gadget_digits
            || first.lhe_ciphertext.len() != p.width * p.gadget_digits
            || first.lenc_ciphertext.len() != p.levels()? * p.width * 2 * p.gadget_digits
            || first
                .lhe_state
                .iter()
                .chain(first.lhe_ciphertext.iter())
                .chain(first.lenc_ciphertext.iter())
                .any(|poly| poly.degree() != p.degree)
        {
            return Err(Error::LengthMismatch);
        }
        Ok(())
    }

    fn check_second(&self, second: &SecondCiphertext) -> Result<(), Error> {
        let p = self.ring.parameters;
        if second.lhe_ciphertext.len() != p.width
            || second.lhe_state.degree() != p.degree
            || second
                .lhe_ciphertext
                .iter()
                .any(|poly| poly.degree() != p.degree)
        {
            return Err(Error::LengthMismatch);
        }
        Ok(())
    }

    fn lhe_enc1(
        &self,
        messages: &[Polynomial],
        random: &mut impl RandomSource,
        noise: &mut impl NoiseSource,
    ) -> Result<(Vec<Polynomial>, Vec<Polynomial>), Error> {
        if messages.len() != self.ring.parameters.width {
            return Err(Error::LengthMismatch);
        }
        let p = self.ring.parameters;
        let mut state = Vec::with_capacity(p.gadget_digits);
        for _ in 0..p.gadget_digits {
            state.push(self.ring.uniform(random)?);
        }
        let mut ciphertext = Vec::with_capacity(p.width * p.gadget_digits);
        for (index, message) in messages.iter().enumerate() {
            for digit in 0..p.gadget_digits {
                let mut value = self.public.lhe_a[index].product(&self.ring, &state[digit]);
                value.add_assign(
                    &self.ring,
                    &message.scaled(&self.ring, p.gadget_base, digit),
                );
                self.ring.add_noise(
                    &mut value,
                    noise,
                    SMALL_NOISE_STANDARD_DEVIATION,
                    SMALL_NOISE_MAX_DEVIATION,
                )?;
                ciphertext.push(value);
            }
        }
        Ok((state, ciphertext))
    }

    fn lhe_enc2(
        &self,
        messages: &[Polynomial],
        random: &mut impl RandomSource,
        noise: &mut impl NoiseSource,
    ) -> Result<(Polynomial, Vec<Polynomial>), Error> {
        if messages.len() != self.ring.parameters.width {
            return Err(Error::LengthMismatch);
        }
        let state = self.ring.uniform(random)?;
        let mut ciphertext = Vec::with_capacity(self.ring.parameters.width);
        for (a, message) in self.public.lhe_a.iter().zip(messages.iter()) {
            let mut value = a.product(&self.ring, &state);
            value.add_assign(&self.ring, message);
            self.ring.add_noise(
                &mut value,
                noise,
                LARGE_NOISE_STANDARD_DEVIATION,
                LARGE_NOISE_MAX_DEVIATION,
            )?;
            ciphertext.push(value);
        }
        Ok((state, ciphertext))
    }

    fn lenc_enc(
        &self,
        message: &[Polynomial],
        random: &mut impl RandomSource,
        noise: &mut impl NoiseSource,
    ) -> Result<(Vec<Polynomial>, Vec<Polynomial>), Error> {
        let p = self.ring.parameters;
        if message.len() != p.width {
            return Err(Error::LengthMismatch);
        }
        let levels = p.levels()?;
        let mut random_vector = Vec::with_capacity(levels * p.width);
        for _ in 0..levels * p.width {
            random_vector.push(self.ring.uniform(random)?);
        }

        let mut ciphertext = Vec::with_capacity(levels * p.width * 2 * p.gadget_digits);
        for level in 0..levels {
            for row in 0..p.width {
                for public in &self.public.lenc_b {
                    ciphertext
                        .push(random_vector[level * p.width + row].product(&self.ring, public));
                }
                let half = if row & (1 << (levels - level - 1)) == 0 {
                    0
                } else {
                    p.gadget_digits
                };
                let next = if level + 1 == levels {
                    &message[row]
                } else {
                    &random_vector[(level + 1) * p.width + row]
                };
                let base = (level * p.width + row) * 2 * p.gadget_digits + half;
                for digit in 0..p.gadget_digits {
                    let extra = next.scaled(&self.ring, p.gadget_base, digit);
                    ciphertext[base + digit].add_assign(&self.ring, &extra);
                }
            }
        }
        for value in &mut ciphertext {
            self.ring.add_noise(
                value,
                noise,
                SMALL_NOISE_STANDARD_DEVIATION,
                SMALL_NOISE_MAX_DEVIATION,
            )?;
        }
        Ok((random_vector, ciphertext))
    }

    fn lenc_digest(&self, choice: &[Polynomial]) -> Result<Tree, Error> {
        let p = self.ring.parameters;
        if choice.len() != p.width {
            return Err(Error::LengthMismatch);
        }
        let mut tree = vec![self.ring.zero(); (2 * p.width - 1) * p.gadget_digits];
        for (index, value) in choice.iter().enumerate() {
            let digits = self.ring.decompose(value)?;
            let base = (p.width - 1 + index) * p.gadget_digits;
            tree[base..base + p.gadget_digits].clone_from_slice(&digits);
        }

        let mut digest = self.ring.zero();
        for node in (0..p.width - 1).rev() {
            let children = (2 * node + 1) * p.gadget_digits;
            let mut parent = inner_product(
                &self.ring,
                &self.public.lenc_b,
                &tree[children..children + 2 * p.gadget_digits],
            )?;
            parent.negate_assign(&self.ring);
            if node == 0 {
                digest = parent;
            } else {
                let digits = self.ring.decompose(&parent)?;
                let base = node * p.gadget_digits;
                tree[base..base + p.gadget_digits].clone_from_slice(&digits);
            }
        }
        Ok(Tree { tree, digest })
    }

    fn lenc_eval(&self, ciphertext: &[Polynomial], tree: &Tree) -> Result<Vec<Polynomial>, Error> {
        let p = self.ring.parameters;
        let levels = p.levels()?;
        if ciphertext.len() != levels * p.width * 2 * p.gadget_digits
            || tree.tree.len() != (2 * p.width - 1) * p.gadget_digits
        {
            return Err(Error::LengthMismatch);
        }
        let mut delta = Vec::with_capacity(p.width);
        for row in 0..p.width {
            let mut value = inner_product(
                &self.ring,
                &ciphertext[row * 2 * p.gadget_digits..(row + 1) * 2 * p.gadget_digits],
                &tree.tree[p.gadget_digits..3 * p.gadget_digits],
            )?;
            for level in 1..levels {
                let ciphertext_base = (level * p.width + row) * 2 * p.gadget_digits;
                let tree_base =
                    (((row >> (levels - level)) + (1 << level) - 1) * 2 + 1) * p.gadget_digits;
                let term = inner_product(
                    &self.ring,
                    &ciphertext[ciphertext_base..ciphertext_base + 2 * p.gadget_digits],
                    &tree.tree[tree_base..tree_base + 2 * p.gadget_digits],
                )?;
                value.add_assign(&self.ring, &term);
            }
            value.negate_assign(&self.ring);
            delta.push(value);
        }
        Ok(delta)
    }
}

#[derive(Clone, Debug)]
struct Tree {
    tree: Vec<Polynomial>,
    digest: Polynomial,
}

#[derive(Clone, Debug)]
struct Ring {
    parameters: Parameters,
    first_ntt: Ntt,
    second_ntt: Ntt,
    inverse_plaintext_mod_delta: u64,
    inverse_delta_mod_plaintext: u64,
}

impl Ring {
    fn new(parameters: Parameters) -> Result<Self, Error> {
        parameters.validate()?;
        Ok(Self {
            first_ntt: Ntt::new(parameters.degree, parameters.plaintext_modulus)?,
            second_ntt: Ntt::new(parameters.degree, parameters.delta)?,
            inverse_plaintext_mod_delta: inverse_mod(
                parameters.plaintext_modulus % parameters.delta,
                parameters.delta,
            )
            .ok_or(Error::Arithmetic)?,
            inverse_delta_mod_plaintext: inverse_mod(
                parameters.delta % parameters.plaintext_modulus,
                parameters.plaintext_modulus,
            )
            .ok_or(Error::Arithmetic)?,
            parameters,
        })
    }

    fn zero(&self) -> Polynomial {
        Polynomial {
            first: vec![0; self.parameters.degree],
            second: vec![0; self.parameters.degree],
        }
    }

    fn uniform(&self, random: &mut impl RandomSource) -> Result<Polynomial, Error> {
        let mut value = self.zero();
        sample_uniform(random, self.parameters.plaintext_modulus, &mut value.first)?;
        sample_uniform(random, self.parameters.delta, &mut value.second)?;
        Ok(value)
    }

    fn encode_messages(&self, input: &[u64]) -> Result<Vec<Polynomial>, Error> {
        if input.len() != self.parameters.slots()? {
            return Err(Error::LengthMismatch);
        }
        if input
            .iter()
            .any(|&value| value >= self.parameters.plaintext_modulus)
        {
            return Err(Error::NonCanonicalPlaintext);
        }
        let mut output = Vec::with_capacity(self.parameters.width);
        for chunk in input.chunks_exact(self.parameters.degree) {
            let mut value = self.zero();
            for (slot, &message) in value.first.iter_mut().zip(chunk.iter()) {
                *slot = mul_mod(
                    message,
                    self.parameters.delta % self.parameters.plaintext_modulus,
                    self.parameters.plaintext_modulus,
                );
            }
            output.push(value);
        }
        Ok(output)
    }

    fn encode_choices(&self, input: &[bool]) -> Result<Vec<Polynomial>, Error> {
        if input.len() != self.parameters.slots()? {
            return Err(Error::LengthMismatch);
        }
        let mut output = Vec::with_capacity(self.parameters.width);
        for chunk in input.chunks_exact(self.parameters.degree) {
            let mut value = self.zero();
            for ((first, second), &choice) in value
                .first
                .iter_mut()
                .zip(value.second.iter_mut())
                .zip(chunk.iter())
            {
                let choice = u64::from(choice);
                *first = choice;
                *second = choice;
            }
            // The paper treats the first RNS limb as already packed in NTT
            // slots. To represent that same packed element in the second
            // modulus, recover its coefficient representation with the first
            // modulus's inverse NTT, then transform those coefficients with
            // the second modulus's NTT. This is the `plain_ntt` -> `small_ntt`
            // conversion in the pinned reference's `keygen` and `dec`.
            self.first_ntt.inverse(&mut value.second);
            self.second_ntt.forward(&mut value.second);
            output.push(value);
        }
        Ok(output)
    }

    fn add_noise(
        &self,
        value: &mut Polynomial,
        noise: &mut impl NoiseSource,
        standard_deviation: u64,
        bound: u64,
    ) -> Result<(), Error> {
        let mut coefficients = vec![0_i64; self.parameters.degree];
        noise.sample(standard_deviation, bound, &mut coefficients)?;
        if coefficients
            .iter()
            .any(|sample| sample.unsigned_abs() > bound)
        {
            return Err(Error::Noise);
        }
        let mut first = Vec::with_capacity(self.parameters.degree);
        let mut second = Vec::with_capacity(self.parameters.degree);
        for sample in coefficients {
            first.push(signed_to_mod(sample, self.parameters.plaintext_modulus));
            second.push(signed_to_mod(sample, self.parameters.delta));
        }
        self.first_ntt.forward(&mut first);
        self.second_ntt.forward(&mut second);
        for (destination, error) in value.first.iter_mut().zip(first) {
            *destination = add_mod(*destination, error, self.parameters.plaintext_modulus);
        }
        for (destination, error) in value.second.iter_mut().zip(second) {
            *destination = add_mod(*destination, error, self.parameters.delta);
        }
        Ok(())
    }

    fn decompose(&self, value: &Polynomial) -> Result<Vec<Polynomial>, Error> {
        if value.degree() != self.parameters.degree {
            return Err(Error::LengthMismatch);
        }
        let mut first = value.first.clone();
        let mut second = value.second.clone();
        self.first_ntt.inverse(&mut first);
        self.second_ntt.inverse(&mut second);
        let mut digits: Vec<Polynomial> = (0..self.parameters.gadget_digits)
            .map(|_| self.zero())
            .collect();
        for index in 0..self.parameters.degree {
            let mut combined = self.combine(first[index], second[index]);
            for digit in &mut digits {
                let part = (combined % self.parameters.gadget_base as u128) as u64;
                digit.first[index] = part % self.parameters.plaintext_modulus;
                digit.second[index] = part % self.parameters.delta;
                combined /= self.parameters.gadget_base as u128;
            }
            if combined != 0 {
                return Err(Error::Arithmetic);
            }
        }
        for digit in &mut digits {
            self.first_ntt.forward(&mut digit.first);
            self.second_ntt.forward(&mut digit.second);
        }
        Ok(digits)
    }

    fn decode_messages(&self, values: &[Polynomial]) -> Result<Vec<u64>, Error> {
        if values.len() != self.parameters.width
            || values
                .iter()
                .any(|value| value.degree() != self.parameters.degree)
        {
            return Err(Error::LengthMismatch);
        }
        let mut output = Vec::with_capacity(self.parameters.slots()?);
        for value in values {
            let mut noise = value.second.clone();
            self.second_ntt.inverse(&mut noise);
            let mut correction = Vec::with_capacity(self.parameters.degree);
            for error in noise {
                let signed = if error > self.parameters.delta / 2 {
                    let magnitude = self.parameters.delta - error;
                    if magnitude >= self.parameters.plaintext_modulus {
                        return Err(Error::Noise);
                    }
                    if magnitude == 0 {
                        0
                    } else {
                        self.parameters.plaintext_modulus - magnitude
                    }
                } else {
                    if error >= self.parameters.plaintext_modulus {
                        return Err(Error::Noise);
                    }
                    error
                };
                correction.push(signed);
            }
            self.first_ntt.forward(&mut correction);
            for (message, correction) in value.first.iter().zip(correction) {
                let no_error = sub_mod(*message, correction, self.parameters.plaintext_modulus);
                output.push(mul_mod(
                    no_error,
                    self.inverse_delta_mod_plaintext,
                    self.parameters.plaintext_modulus,
                ));
            }
        }
        Ok(output)
    }

    fn combine(&self, first: u64, second: u64) -> u128 {
        let first_mod_delta = first % self.parameters.delta;
        let offset = mul_mod(
            sub_mod(second, first_mod_delta, self.parameters.delta),
            self.inverse_plaintext_mod_delta,
            self.parameters.delta,
        );
        first as u128 + self.parameters.plaintext_modulus as u128 * offset as u128
    }
}

impl Polynomial {
    fn product(&self, ring: &Ring, other: &Self) -> Self {
        let mut output = ring.zero();
        for ((output, left), right) in output
            .first
            .iter_mut()
            .zip(self.first.iter())
            .zip(other.first.iter())
        {
            *output = mul_mod(*left, *right, ring.parameters.plaintext_modulus);
        }
        for ((output, left), right) in output
            .second
            .iter_mut()
            .zip(self.second.iter())
            .zip(other.second.iter())
        {
            *output = mul_mod(*left, *right, ring.parameters.delta);
        }
        output
    }

    fn add_assign(&mut self, ring: &Ring, other: &Self) {
        for (left, right) in self.first.iter_mut().zip(other.first.iter()) {
            *left = add_mod(*left, *right, ring.parameters.plaintext_modulus);
        }
        for (left, right) in self.second.iter_mut().zip(other.second.iter()) {
            *left = add_mod(*left, *right, ring.parameters.delta);
        }
    }

    fn sub_assign(&mut self, ring: &Ring, other: &Self) {
        for (left, right) in self.first.iter_mut().zip(other.first.iter()) {
            *left = sub_mod(*left, *right, ring.parameters.plaintext_modulus);
        }
        for (left, right) in self.second.iter_mut().zip(other.second.iter()) {
            *left = sub_mod(*left, *right, ring.parameters.delta);
        }
    }

    fn negate_assign(&mut self, ring: &Ring) {
        for value in &mut self.first {
            if *value != 0 {
                *value = ring.parameters.plaintext_modulus - *value;
            }
        }
        for value in &mut self.second {
            if *value != 0 {
                *value = ring.parameters.delta - *value;
            }
        }
    }

    fn scaled(&self, ring: &Ring, base: u64, power: usize) -> Self {
        let first = pow_mod(
            base % ring.parameters.plaintext_modulus,
            power,
            ring.parameters.plaintext_modulus,
        );
        let second = pow_mod(base % ring.parameters.delta, power, ring.parameters.delta);
        let mut output = self.clone();
        for value in &mut output.first {
            *value = mul_mod(*value, first, ring.parameters.plaintext_modulus);
        }
        for value in &mut output.second {
            *value = mul_mod(*value, second, ring.parameters.delta);
        }
        output
    }
}

fn inner_product(
    ring: &Ring,
    left: &[Polynomial],
    right: &[Polynomial],
) -> Result<Polynomial, Error> {
    if left.len() != right.len()
        || left
            .iter()
            .chain(right.iter())
            .any(|value| value.degree() != ring.parameters.degree)
    {
        return Err(Error::LengthMismatch);
    }
    let mut output = ring.zero();
    for (left, right) in left.iter().zip(right.iter()) {
        output.add_assign(ring, &left.product(ring, right));
    }
    Ok(output)
}

#[derive(Clone, Debug)]
struct Ntt {
    modulus: u64,
    degree: usize,
    psi: u64,
    omega: u64,
    inverse_psi: u64,
    inverse_omega: u64,
    inverse_degree: u64,
}

impl Ntt {
    fn new(degree: usize, modulus: u64) -> Result<Self, Error> {
        let psi = find_negacyclic_root(degree, modulus).ok_or(Error::Arithmetic)?;
        let omega = mul_mod(psi, psi, modulus);
        Ok(Self {
            modulus,
            degree,
            psi,
            omega,
            inverse_psi: inverse_mod(psi, modulus).ok_or(Error::Arithmetic)?,
            inverse_omega: inverse_mod(omega, modulus).ok_or(Error::Arithmetic)?,
            inverse_degree: inverse_mod(degree as u64, modulus).ok_or(Error::Arithmetic)?,
        })
    }

    fn forward(&self, values: &mut [u64]) {
        for (index, value) in values.iter_mut().enumerate() {
            *value = mul_mod(*value, pow_mod(self.psi, index, self.modulus), self.modulus);
        }
        self.cyclic(values, self.omega);
    }

    fn inverse(&self, values: &mut [u64]) {
        self.cyclic(values, self.inverse_omega);
        for (index, value) in values.iter_mut().enumerate() {
            *value = mul_mod(*value, self.inverse_degree, self.modulus);
            *value = mul_mod(
                *value,
                pow_mod(self.inverse_psi, index, self.modulus),
                self.modulus,
            );
        }
    }

    fn cyclic(&self, values: &mut [u64], root: u64) {
        debug_assert_eq!(values.len(), self.degree);
        bit_reverse(values);
        let mut length = 2;
        while length <= self.degree {
            let step = pow_mod(root, self.degree / length, self.modulus);
            for start in (0..self.degree).step_by(length) {
                let mut twiddle = 1;
                for offset in 0..length / 2 {
                    let left = values[start + offset];
                    let right = mul_mod(values[start + offset + length / 2], twiddle, self.modulus);
                    values[start + offset] = add_mod(left, right, self.modulus);
                    values[start + offset + length / 2] = sub_mod(left, right, self.modulus);
                    twiddle = mul_mod(twiddle, step, self.modulus);
                }
            }
            length *= 2;
        }
    }
}

fn bit_reverse(values: &mut [u64]) {
    let bits = values.len().ilog2();
    for index in 0..values.len() {
        let reversed = index.reverse_bits() >> (usize::BITS - bits);
        if index < reversed {
            values.swap(index, reversed);
        }
    }
}

fn find_negacyclic_root(degree: usize, modulus: u64) -> Option<u64> {
    let exponent = (modulus - 1) / (2 * degree as u64);
    for candidate in 2..modulus {
        let root = pow_mod(candidate, exponent as usize, modulus);
        if pow_mod(root, degree, modulus) == modulus - 1 {
            return Some(root);
        }
    }
    None
}

fn sample_uniform(
    random: &mut impl RandomSource,
    modulus: u64,
    output: &mut [u64],
) -> Result<(), Error> {
    let threshold = u64::MAX - u64::MAX % modulus;
    for value in output {
        loop {
            let mut bytes = [0_u8; 8];
            random.fill_bytes(&mut bytes)?;
            let candidate = u64::from_le_bytes(bytes);
            if candidate < threshold {
                *value = candidate % modulus;
                break;
            }
        }
    }
    Ok(())
}

fn signed_to_mod(value: i64, modulus: u64) -> u64 {
    if value < 0 {
        let magnitude = value.unsigned_abs() % modulus;
        if magnitude == 0 {
            0
        } else {
            modulus - magnitude
        }
    } else {
        value as u64 % modulus
    }
}

fn add_mod(left: u64, right: u64, modulus: u64) -> u64 {
    let sum = left + right;
    if sum >= modulus { sum - modulus } else { sum }
}

fn sub_mod(left: u64, right: u64, modulus: u64) -> u64 {
    if left >= right {
        left - right
    } else {
        modulus - (right - left)
    }
}

fn mul_mod(left: u64, right: u64, modulus: u64) -> u64 {
    ((left as u128 * right as u128) % modulus as u128) as u64
}

fn pow_mod(mut value: u64, mut exponent: usize, modulus: u64) -> u64 {
    let mut result = 1;
    while exponent != 0 {
        if exponent & 1 != 0 {
            result = mul_mod(result, value, modulus);
        }
        value = mul_mod(value, value, modulus);
        exponent >>= 1;
    }
    result
}

fn inverse_mod(value: u64, modulus: u64) -> Option<u64> {
    if value == 0 || modulus < 2 {
        return None;
    }
    let mut old_r = value as i128;
    let mut r = modulus as i128;
    let mut old_s = 1_i128;
    let mut s = 0_i128;
    while r != 0 {
        let quotient = old_r / r;
        (old_r, r) = (r, old_r - quotient * r);
        (old_s, s) = (s, old_s - quotient * s);
    }
    if old_r != 1 {
        return None;
    }
    Some(old_s.rem_euclid(modulus as i128) as u64)
}

#[cfg(test)]
mod tests {
    use super::{
        BatchSelect, Error, NoiseSource, Ntt, Parameters, REFERENCE_DELTA, REFERENCE_PARAMETERS,
        REFERENCE_PLAINTEXT_MODULUS, RandomSource, Ring, SMALL_NOISE_MAX_DEVIATION,
        SMALL_NOISE_STANDARD_DEVIATION, ZeroNoise, add_mod, mul_mod,
    };

    #[derive(Clone, Copy)]
    struct TestRandom(u64);

    impl RandomSource for TestRandom {
        fn fill_bytes(&mut self, output: &mut [u8]) -> Result<(), Error> {
            for byte in output {
                self.0 ^= self.0 << 7;
                self.0 ^= self.0 >> 9;
                self.0 ^= self.0 << 8;
                *byte = self.0 as u8;
            }
            Ok(())
        }
    }

    struct ExcessNoise;

    impl NoiseSource for ExcessNoise {
        fn sample(
            &mut self,
            _standard_deviation: u64,
            bound: u64,
            output: &mut [i64],
        ) -> Result<(), Error> {
            output.fill(bound as i64 + 1);
            Ok(())
        }
    }

    #[test]
    fn reference_profile_pins_the_paper_and_artifact_parameters() {
        assert_eq!(REFERENCE_PARAMETERS.degree, 4_096);
        assert_eq!(REFERENCE_PARAMETERS.width, 512);
        assert_eq!(
            REFERENCE_PARAMETERS.plaintext_modulus,
            REFERENCE_PLAINTEXT_MODULUS
        );
        assert_eq!(REFERENCE_PARAMETERS.delta, REFERENCE_DELTA);
        assert_eq!(REFERENCE_PARAMETERS.gadget_base, 1 << 28);
        assert_eq!(REFERENCE_PARAMETERS.gadget_digits, 4);
        assert_eq!(REFERENCE_PARAMETERS.slots(), Ok(2_097_152));
        assert!(Ring::new(REFERENCE_PARAMETERS).is_ok());
    }

    #[test]
    fn negacyclic_ntt_round_trips_and_multiplies() {
        let ntt = Ntt::new(8, REFERENCE_PLAINTEXT_MODULUS).unwrap();
        let left = [2, 5, 1, 8, 0, 3, 4, 7];
        let right = [6, 2, 9, 1, 5, 0, 3, 4];
        let mut transformed_left = left;
        let mut transformed_right = right;
        ntt.forward(&mut transformed_left);
        ntt.forward(&mut transformed_right);
        for (left, right) in transformed_left.iter_mut().zip(transformed_right) {
            *left = mul_mod(*left, right, REFERENCE_PLAINTEXT_MODULUS);
        }
        ntt.inverse(&mut transformed_left);

        let mut expected = [0; 8];
        for (left_index, left) in left.iter().enumerate() {
            for (right_index, right) in right.iter().enumerate() {
                let product = mul_mod(*left, *right, REFERENCE_PLAINTEXT_MODULUS);
                let index = left_index + right_index;
                if index < expected.len() {
                    expected[index] =
                        add_mod(expected[index], product, REFERENCE_PLAINTEXT_MODULUS);
                } else {
                    expected[index - expected.len()] = super::sub_mod(
                        expected[index - expected.len()],
                        product,
                        REFERENCE_PLAINTEXT_MODULUS,
                    );
                }
            }
        }
        assert_eq!(transformed_left, expected);
    }

    #[test]
    fn reference_degree_ntt_round_trips_a_nontrivial_polynomial() {
        let ntt = Ntt::new(REFERENCE_PARAMETERS.degree, REFERENCE_PLAINTEXT_MODULUS).unwrap();
        let expected = (0..REFERENCE_PARAMETERS.degree)
            .map(|index| ((index * 13 + 7) as u64) % REFERENCE_PLAINTEXT_MODULUS)
            .collect::<alloc::vec::Vec<_>>();
        let mut transformed = expected.clone();
        ntt.forward(&mut transformed);
        ntt.inverse(&mut transformed);
        assert_eq!(transformed, expected);
    }

    #[test]
    fn zero_noise_staged_construction_selects_each_plaintext_slot() {
        let parameters = Parameters::scaled_reference(8, 2);
        let mut random = TestRandom(0x8b79_4f22_d1c0_9a35);
        let mut noise = ZeroNoise;
        let batch = BatchSelect::setup(parameters, &mut random).unwrap();
        let left = [9, 2, 17, 4, 21, 6, 25, 8, 29, 10, 33, 12, 37, 14, 41, 16];
        let right = [3, 20, 5, 24, 7, 28, 11, 32, 13, 36, 15, 40, 19, 44, 23, 48];
        let choices = [
            false, true, false, true, true, false, true, false, true, false, true, false, false,
            true, false, true,
        ];
        let first = batch.enc1(&left, &mut random, &mut noise).unwrap();
        let second = batch.enc2(&right, &mut random, &mut noise).unwrap();
        let key = batch.keygen(&first, &second, &choices).unwrap();
        let choice = batch.ring.encode_choices(&choices).unwrap();
        let tree = batch.lenc_digest(&choice).unwrap();
        let digits = batch.ring.decompose(&tree.digest).unwrap();
        let mut raw = alloc::vec::Vec::new();
        for index in 0..parameters.width {
            let mut value = super::inner_product(
                &batch.ring,
                &first.lhe_ciphertext
                    [index * parameters.gadget_digits..(index + 1) * parameters.gadget_digits],
                &digits,
            )
            .unwrap();
            value.add_assign(&batch.ring, &second.lhe_ciphertext[index]);
            value.sub_assign(
                &batch.ring,
                &batch.public.lhe_a[index].product(&batch.ring, &key.key),
            );
            raw.push(value);
        }
        let correction = batch.lenc_eval(&first.lenc_ciphertext, &tree).unwrap();
        for (value, correction) in raw.iter_mut().zip(correction.iter()) {
            value.sub_assign(&batch.ring, correction);
        }
        let output = batch.dec(&first, &second, &key, &choices).unwrap();
        let expected: alloc::vec::Vec<_> = left
            .iter()
            .zip(right)
            .zip(choices)
            .map(|((&left, right), choice)| if choice { left + right } else { right })
            .collect();
        assert_eq!(raw, batch.ring.encode_messages(&expected).unwrap());
        assert_eq!(output, expected);
    }

    #[test]
    fn lenc_evaluation_cancels_the_lhe_random_vector_at_zero_noise() {
        let parameters = Parameters::scaled_reference(8, 2);
        let mut random = TestRandom(0x41e8_76b2_0c9d_1551);
        let mut noise = ZeroNoise;
        let batch = BatchSelect::setup(parameters, &mut random).unwrap();
        let values = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];
        let choices = [
            true, false, true, false, false, true, false, true, true, true, false, false, true,
            false, true, false,
        ];
        let messages = batch.ring.encode_messages(&values).unwrap();
        let choice = batch.ring.encode_choices(&choices).unwrap();
        let (random_vector, ciphertext) =
            batch.lenc_enc(&messages, &mut random, &mut noise).unwrap();
        let tree = batch.lenc_digest(&choice).unwrap();
        let delta = batch.lenc_eval(&ciphertext, &tree).unwrap();
        for index in 0..parameters.width {
            let mut expected = random_vector[index].product(&batch.ring, &tree.digest);
            expected.sub_assign(
                &batch.ring,
                &messages[index].product(&batch.ring, &choice[index]),
            );
            assert_eq!(delta[index], expected);
        }
    }

    #[test]
    fn lhe_layer_recovers_the_linear_digest_product_at_zero_noise() {
        let parameters = Parameters::scaled_reference(8, 2);
        let mut random = TestRandom(0x03b2_1379_d0a1_c50d);
        let mut noise = ZeroNoise;
        let batch = BatchSelect::setup(parameters, &mut random).unwrap();
        let choices = [
            false, true, true, false, false, false, true, true, true, false, true, false, true,
            true, false, false,
        ];
        let choice = batch.ring.encode_choices(&choices).unwrap();
        let tree = batch.lenc_digest(&choice).unwrap();
        let messages = (0..parameters.width)
            .map(|_| batch.ring.uniform(&mut random).unwrap())
            .collect::<alloc::vec::Vec<_>>();
        let (s1, ct1) = batch.lhe_enc1(&messages, &mut random, &mut noise).unwrap();
        let (s2, ct2) = batch.lhe_enc2(&messages, &mut random, &mut noise).unwrap();
        let digits = batch.ring.decompose(&tree.digest).unwrap();
        let mut key = s2;
        for (state, digit) in s1.iter().zip(digits.iter()) {
            key.add_assign(&batch.ring, &state.product(&batch.ring, digit));
        }
        for index in 0..parameters.width {
            let mut result = super::inner_product(
                &batch.ring,
                &ct1[index * parameters.gadget_digits..(index + 1) * parameters.gadget_digits],
                &digits,
            )
            .unwrap();
            result.add_assign(&batch.ring, &ct2[index]);
            result.sub_assign(
                &batch.ring,
                &batch.public.lhe_a[index].product(&batch.ring, &key),
            );
            let mut expected = messages[index].product(&batch.ring, &tree.digest);
            expected.add_assign(&batch.ring, &messages[index]);
            assert_eq!(result, expected);
        }
    }

    #[test]
    fn delta_message_encoding_round_trips_without_noise() {
        let parameters = Parameters::scaled_reference(8, 2);
        let mut random = TestRandom(0x7134_8a5e_1b27_4e09);
        let batch = BatchSelect::setup(parameters, &mut random).unwrap();
        let input = [
            0, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987,
        ];
        let encoded = batch.ring.encode_messages(&input).unwrap();
        assert_eq!(batch.ring.decode_messages(&encoded).unwrap(), input);
    }

    #[test]
    fn error_sampler_cannot_exceed_the_requested_clipping_bound() {
        let ring = Ring::new(Parameters::scaled_reference(8, 2)).unwrap();
        let mut polynomial = ring.zero();
        assert_eq!(
            ring.add_noise(
                &mut polynomial,
                &mut ExcessNoise,
                SMALL_NOISE_STANDARD_DEVIATION,
                SMALL_NOISE_MAX_DEVIATION,
            ),
            Err(Error::Noise)
        );
    }

    #[test]
    fn combined_lenc_and_lhe_equations_cancel_at_zero_noise() {
        let parameters = Parameters::scaled_reference(8, 2);
        let mut random = TestRandom(0x7715_f0d3_29a5_18cb);
        let mut noise = ZeroNoise;
        let batch = BatchSelect::setup(parameters, &mut random).unwrap();
        let first_input = [2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32];
        let second_input = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31];
        let choices = [
            true, false, true, false, true, false, true, false, false, true, false, true, false,
            true, false, true,
        ];
        let first_message = batch.ring.encode_messages(&first_input).unwrap();
        let second_message = batch.ring.encode_messages(&second_input).unwrap();
        let choices_encoded = batch.ring.encode_choices(&choices).unwrap();
        let (random_vector, lenc_ciphertext) = batch
            .lenc_enc(&first_message, &mut random, &mut noise)
            .unwrap();
        let (s1, ct1) = batch
            .lhe_enc1(&random_vector, &mut random, &mut noise)
            .unwrap();
        let (s2, ct2) = batch
            .lhe_enc2(&second_message, &mut random, &mut noise)
            .unwrap();
        let tree = batch.lenc_digest(&choices_encoded).unwrap();
        let digits = batch.ring.decompose(&tree.digest).unwrap();
        let mut key = s2;
        for (state, digit) in s1.iter().zip(digits.iter()) {
            key.add_assign(&batch.ring, &state.product(&batch.ring, digit));
        }
        let correction = batch.lenc_eval(&lenc_ciphertext, &tree).unwrap();
        let expected = first_input
            .iter()
            .zip(second_input)
            .zip(choices)
            .map(|((&first, second), choice)| if choice { first + second } else { second })
            .collect::<alloc::vec::Vec<_>>();
        let expected = batch.ring.encode_messages(&expected).unwrap();
        for index in 0..parameters.width {
            let mut output = super::inner_product(
                &batch.ring,
                &ct1[index * parameters.gadget_digits..(index + 1) * parameters.gadget_digits],
                &digits,
            )
            .unwrap();
            output.add_assign(&batch.ring, &ct2[index]);
            output.sub_assign(
                &batch.ring,
                &batch.public.lhe_a[index].product(&batch.ring, &key),
            );
            let mut before_correction = random_vector[index].product(&batch.ring, &tree.digest);
            before_correction.add_assign(&batch.ring, &second_message[index]);
            assert_eq!(output, before_correction);
            let mut expected_correction = random_vector[index].product(&batch.ring, &tree.digest);
            expected_correction.sub_assign(
                &batch.ring,
                &first_message[index].product(&batch.ring, &choices_encoded[index]),
            );
            assert_eq!(correction[index], expected_correction);
            output.sub_assign(&batch.ring, &correction[index]);
            assert_eq!(output, expected[index]);
        }
    }

    #[test]
    fn bad_input_lengths_and_noncanonical_plaintexts_fail_before_encryption() {
        let parameters = Parameters::scaled_reference(8, 2);
        let mut random = TestRandom(9);
        let mut noise = ZeroNoise;
        let batch = BatchSelect::setup(parameters, &mut random).unwrap();
        assert!(matches!(
            batch.enc1(&[0; 15], &mut random, &mut noise),
            Err(Error::LengthMismatch)
        ));
        assert!(matches!(
            batch.enc2(&[REFERENCE_PLAINTEXT_MODULUS; 16], &mut random, &mut noise,),
            Err(Error::NonCanonicalPlaintext)
        ));
    }
}
