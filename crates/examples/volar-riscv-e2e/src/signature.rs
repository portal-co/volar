//! Pluggable signature-verification scheme for the RISC-V program being
//! run. Milestone 1 ships only [`NoSignature`], which contributes zero
//! extra prover witness and zero extra verifier checks -- the woven
//! circuit is exactly the interpreter's own IR, unmodified.
//!
//! Milestone 4 is expected to add a `Faest` variant, but **not** by
//! implementing circuit-construction code against this trait: by
//! compiling the external `faest` crate directly into the Milestone-2
//! Rust interpreter source (as an ordinary `wasm32-unknown-unknown`
//! dependency) and calling it from ordinary Rust control flow, so the
//! *same* `lower_waffle_module` -> weave -> prove/verify pipeline handles
//! it uniformly, with no bespoke FAEST circuit code anywhere. This trait
//! exists only so Milestone 1's driver has one swappable hook rather than
//! being coupled to "no signature" by construction.

/// One pluggable signature-verification scheme for the program's binary.
pub trait SignatureCircuit {
    /// Extra prover-side witness bytes this scheme needs alongside the
    /// program/RAM image (e.g. a signature + public key). Empty for
    /// [`NoSignature`].
    fn extra_witness(&self) -> Vec<u8> {
        Vec::new()
    }

    /// Extra verifier-side check beyond the woven circuit's own checks
    /// (e.g. "the embedded signature verified"). Always `true` for
    /// [`NoSignature`] -- there is nothing extra to check.
    fn extra_verifier_check(&self) -> bool {
        true
    }
}

/// No signature verification at all -- the program is trusted as-is.
/// Contributes zero gates: the woven circuit for a run driven with
/// `NoSignature` is bit-for-bit identical to one driven with no signature
/// slot at all.
pub struct NoSignature;

impl SignatureCircuit for NoSignature {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_signature_contributes_zero_witness_and_always_checks_out() {
        let sig = NoSignature;
        assert!(sig.extra_witness().is_empty());
        assert!(sig.extra_verifier_check());
    }
}
