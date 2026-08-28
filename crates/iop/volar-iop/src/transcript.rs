// @reliability: experimental
// @ai: assisted
//! Generic Fiat–Shamir transcript — a running SHAKE128 sponge with
//! `absorb`/`squeeze`, modeled directly on the shape of
//! [`volar_spec`'s FAEST transcript](../../spec/volar-spec/src/faest/transcript.rs)
//! (`FaestTranscript`: incremental absorb, non-destructive squeeze) but
//! generic rather than hardcoded to FAEST's specific `chall1`/`chall2`/
//! `chall3` absorb order. Serves both this crate's Phase 1 (optional
//! per-gate fold-challenge derivation, `crate::fold`) and Phase 2
//! (the finalization IOP's challenges/query indices, `crate::ligero`) — see
//! `docs/prove-the-verifier-iop.md`.
//!
//! **Soundness note (carry into review):** a transcript is only as sound as
//! its absorb ordering — omitting a value from the absorb sequence before
//! deriving a challenge that should have depended on it is a classic,
//! easy-to-introduce soundness bug. Every caller of [`IopTranscript`] in
//! this crate documents exactly what it absorbs and in what order; keep
//! that discipline for any new caller.

use alloc::vec;
use alloc::vec::Vec;

use sha3::{
    Shake128,
    digest::{ExtendableOutput, Update, XofReader},
};

use crate::field::Field;

/// A running SHAKE128 sponge. `absorb` is streaming/incremental (O(1)
/// state — the sponge's internal state, not the absorbed history, is what's
/// kept); `squeeze` is non-destructive (repeated squeezes without an
/// intervening absorb return deterministic, independent output — mirroring
/// `FaestTranscript::squeeze`'s own documented behavior).
#[derive(Clone)]
pub struct IopTranscript {
    sponge: Shake128,
}

impl Default for IopTranscript {
    fn default() -> Self {
        Self::new()
    }
}

impl IopTranscript {
    pub fn new() -> Self {
        IopTranscript {
            sponge: Shake128::default(),
        }
    }

    /// Absorb a domain-separation label (call once, first, per logical
    /// sub-protocol — e.g. `b"volar-iop-ligero-v1"` — so this transcript's
    /// output can never collide with an unrelated protocol's).
    pub fn domain_sep(mut self, label: &[u8]) -> Self {
        self.sponge.update(label);
        self
    }

    pub fn absorb(&mut self, data: &[u8]) {
        self.sponge.update(data);
    }

    /// Absorb a length-prefixed byte string — use this (not bare
    /// [`absorb`](Self::absorb)) whenever the surrounding data's boundaries
    /// aren't otherwise fixed, so `absorb(a); absorb(b)` can never be
    /// confused with `absorb(a || b)` for a different `a`/`b` split.
    pub fn absorb_len_prefixed(&mut self, data: &[u8]) {
        self.absorb(&(data.len() as u64).to_le_bytes());
        self.absorb(data);
    }

    pub fn squeeze_bytes(&self, n: usize) -> Vec<u8> {
        let mut out = vec![0u8; n];
        let mut reader = self.sponge.clone().finalize_xof();
        reader.read(&mut out);
        out
    }

    /// Squeeze a field element: enough bytes for `F`'s byte length, taken as
    /// the element's canonical little-endian-tower encoding
    /// ([`Field::to_bytes`]'s layout). Not reduced further — every byte
    /// pattern of the right length is a valid tower-field element (there's
    /// no modulus to reduce against), so this is uniform over `F` already.
    pub fn squeeze_field_element<F: Field>(&self) -> F
    where
        F: FromBytes,
    {
        let nbytes = (F::BITS / 8) as usize;
        F::from_bytes(&self.squeeze_bytes(nbytes))
    }

    /// Squeeze `count` **pairwise-distinct** indices uniformly in
    /// `0..domain_size` (`domain_size` must be a power of two —
    /// rejection sampling would be needed otherwise; this design only ever
    /// calls this with power-of-two domains, § `crate::ligero`).
    ///
    /// Distinctness matters, not just uniformity: `crate::ligero`'s
    /// proximity check interpolates a degree-`<K` polynomial from `K` of
    /// these indices' points — a repeated index would make two
    /// interpolation points coincide, corrupting the Lagrange denominator
    /// (`crate::field::Field::inv`'s zero-returns-zero convention would
    /// silently produce a wrong result rather than panic). Rejection-sample
    /// (bounded retries, re-deriving from an incrementing counter) until
    /// `count` distinct values are found — sound as long as
    /// `count < domain_size`, which every caller here satisfies.
    pub fn squeeze_indices(&mut self, count: usize, domain_size: usize) -> Vec<usize> {
        assert!(
            domain_size.is_power_of_two(),
            "squeeze_indices: domain_size must be a power of two"
        );
        assert!(
            count < domain_size,
            "squeeze_indices: count must be < domain_size to guarantee distinctness"
        );
        let bits = domain_size.trailing_zeros();
        let bytes_needed = ((bits as usize) + 7) / 8;
        let mut out: Vec<usize> = Vec::with_capacity(count);
        let mut counter: u64 = 0;
        let max_tries = (domain_size as u64) * 64 + 1024; // generous bound; failure would be a transcript bug, not bad luck
        while out.len() < count {
            assert!(
                counter < max_tries,
                "squeeze_indices: exceeded max retries — check domain_size/count"
            );
            let mut sub = self.sponge.clone();
            sub.update(&counter.to_le_bytes());
            counter += 1;
            let mut reader = sub.finalize_xof();
            let mut buf = vec![0u8; bytes_needed.max(1)];
            reader.read(&mut buf);
            let mut v: u64 = 0;
            for b in buf.iter().rev() {
                v = (v << 8) | (*b as u64);
            }
            let idx = (v as usize) & (domain_size - 1);
            if !out.contains(&idx) {
                out.push(idx);
            }
        }
        out
    }
}

/// Reconstruct a `Field` value from its [`Field::to_bytes`] encoding —
/// separate from `Field` itself so `Field` stays usable in `alloc`-only
/// contexts that never need to deserialize (kept minimal on purpose).
pub trait FromBytes: Field {
    fn from_bytes(bytes: &[u8]) -> Self;

    /// Embed a small integer as a distinct field element (bit pattern in the
    /// low bytes, zero-padded above) — used wherever a set of *distinct*
    /// evaluation points is needed (e.g. Reed–Solomon encoding,
    /// `crate::ligero`) but no particular algebraic meaning of "n" is
    /// required, only that `n != m => from_u64(n) != from_u64(m)` (true here
    /// since the embedding is injective for any `n` fitting in the low
    /// bytes).
    fn from_u64(n: u64) -> Self {
        let nbytes = (Self::BITS / 8) as usize;
        let mut b = alloc::vec![0u8; nbytes];
        let take = 8.min(nbytes);
        b[..take].copy_from_slice(&n.to_le_bytes()[..take]);
        Self::from_bytes(&b)
    }
}

impl FromBytes for volar_primitives::Galois {
    fn from_bytes(bytes: &[u8]) -> Self {
        volar_primitives::Galois(bytes[0])
    }
}

impl<F: Field + FromBytes, B: crate::field::BetaOf<F>> FromBytes for crate::field::Ext<F, B> {
    fn from_bytes(bytes: &[u8]) -> Self {
        let half = bytes.len() / 2;
        crate::field::Ext::new(F::from_bytes(&bytes[..half]), F::from_bytes(&bytes[half..]))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::field::Gf128;

    #[test]
    fn squeeze_is_non_destructive() {
        let mut t = IopTranscript::new();
        t.absorb(b"hello");
        let a = t.squeeze_bytes(32);
        let b = t.squeeze_bytes(32);
        assert_eq!(a, b);
    }

    #[test]
    fn different_absorbs_differ() {
        let mut t1 = IopTranscript::new();
        t1.absorb(b"aaa");
        let mut t2 = IopTranscript::new();
        t2.absorb(b"bbb");
        assert_ne!(t1.squeeze_bytes(16), t2.squeeze_bytes(16));
    }

    #[test]
    fn absorb_len_prefixed_prevents_boundary_confusion() {
        let mut t1 = IopTranscript::new();
        t1.absorb_len_prefixed(b"a");
        t1.absorb_len_prefixed(b"b");
        let mut t2 = IopTranscript::new();
        t2.absorb_len_prefixed(b"ab");
        assert_ne!(t1.squeeze_bytes(16), t2.squeeze_bytes(16));
    }

    #[test]
    fn domain_sep_changes_output() {
        let mut t1 = IopTranscript::new().domain_sep(b"proto-a");
        t1.absorb(b"x");
        let mut t2 = IopTranscript::new().domain_sep(b"proto-b");
        t2.absorb(b"x");
        assert_ne!(t1.squeeze_bytes(16), t2.squeeze_bytes(16));
    }

    #[test]
    fn squeeze_field_element_is_deterministic_and_right_length() {
        let mut t = IopTranscript::new();
        t.absorb(b"seed");
        let a: Gf128 = t.squeeze_field_element();
        let b: Gf128 = t.squeeze_field_element();
        assert_eq!(a, b);
        assert_eq!(a.to_bytes().len(), 16);
    }

    #[test]
    fn squeeze_indices_are_in_range_and_deterministic() {
        let mut t1 = IopTranscript::new();
        t1.absorb(b"root");
        let idxs1 = t1.squeeze_indices(10, 64);
        let mut t2 = IopTranscript::new();
        t2.absorb(b"root");
        let idxs2 = t2.squeeze_indices(10, 64);
        assert_eq!(idxs1, idxs2);
        assert!(idxs1.iter().all(|i| *i < 64));
    }
}
