#![no_std]

use digest::Digest;
pub use hybrid_array::{Array, ArraySize};

pub use volar_common::hash_commitment as simple;
pub mod byte_gen;
pub mod garble;
pub mod vole;
pub mod curve;
pub mod mpc;
pub mod lwe;
// pub mod xsat;
pub mod grafhen;
pub mod tfhe;
pub use volar_primitives as field;

// ── Deterministic RNG trait ──────────────────────────────────────────────────
//
// `volar-spec` is a specification crate: all operations must be fully
// deterministic for a given RNG state.  Instead of depending on the `rand`
// crate we define a minimal trait that callers implement with whatever
// source of randomness (or deterministic seed) they prefer.

/// Minimal random-number-generator trait for `volar-spec`.
///
/// Implementors provide `next_u32` — all other randomness in the spec is
/// derived from it.  This keeps the spec crate dependency-free from `rand`
/// while still allowing callers to plug in any RNG they like.
pub trait SpecRng {
    /// Return the next pseudo-random `u32`.
    fn next_u32(&mut self) -> u32;

    /// Return the next pseudo-random `u8` (default: truncate `next_u32`).
    #[inline]
    fn next_u8(&mut self) -> u8 {
        self.next_u32() as u8
    }
}
