// @reliability: experimental
//! @ai: assisted
//! Multiset memory checking for VOLE-authenticated storage.
//!
//! See `docs/memory-checking.md` for the full protocol specification,
//! soundness proof, and integration guide.
//!
//! # Overview
//!
//! This module provides a generic state machine ([`MemoryCheckState`]) that
//! tracks memory consistency via two multiset hash accumulators.  Every
//! init / write / read / drain operation encodes a `(addr, value, timestamp)`
//! tuple and absorbs it into the appropriate accumulator.  At the end of the
//! proof, the two accumulators must match.
//!
//! The encoding uses a random verifier challenge `r ∈ T`:
//!
//! ```text
//! encode(addr, value, timestamp) = addr · r  +  value · r²  +  timestamp · r³
//! ```
//!
//! All multiplications are by public constants (powers of `r`), so the
//! encoding is **free** in the VOLE framework — zero AND gates.
//!
//! # Genericity
//!
//! The hasher trait [`MemoryHasher`] abstracts over how encoded elements are
//! accumulated.  The default implementation [`AdditiveHasher`] simply sums
//! all encodings (multiset equality via random linear combination).
//! Alternative hashers (multiplicative / polynomial) can be plugged in for
//! different security/performance trade-offs.

use core::ops::{Add, Mul};

use hybrid_array::{Array, ArraySize};

// ============================================================================
// Hasher trait
// ============================================================================

/// Abstraction over how encoded memory tuples are accumulated.
///
/// Implementations must be commutative and associative so that the
/// accumulation order does not matter (multiset, not sequence).
pub trait MemoryHasher<T> {
    /// Internal accumulator state.
    type State: Clone;

    /// Create a fresh (identity) accumulator.
    fn new_state() -> Self::State;

    /// Absorb one encoded tuple into the accumulator.
    fn absorb(state: &mut Self::State, encoded: T);

    /// Check whether two accumulators represent the same multiset.
    fn finalize_eq(produce: &Self::State, consume: &Self::State) -> bool;
}

// ============================================================================
// Additive hasher (default)
// ============================================================================

/// Additive multiset hasher: `H = Σ encode(op_i)`.
///
/// Two multisets are equal iff their sums over random encodings match.
/// Security: Schwartz-Zippel over |T| (see `docs/memory-checking.md`).
pub struct AdditiveHasher;

impl<T> MemoryHasher<T> for AdditiveHasher
where
    T: Clone + Default + Add<Output = T> + PartialEq,
{
    type State = T;

    fn new_state() -> T {
        T::default()
    }

    fn absorb(state: &mut T, encoded: T) {
        let old = core::mem::take(state);
        *state = old + encoded;
    }

    fn finalize_eq(produce: &T, consume: &T) -> bool {
        *produce == *consume
    }
}

// ============================================================================
// Memory check state machine
// ============================================================================

/// Powers of the random challenge `r` pre-computed for encoding.
///
/// `r1 = r`, `r2 = r²`, `r3 = r³`.
#[derive(Clone)]
pub struct ChallengeKey<T> {
    pub r1: T,
    pub r2: T,
    pub r3: T,
}

impl<T: Clone + Mul<Output = T>> ChallengeKey<T> {
    /// Derive `(r, r², r³)` from a single challenge value.
    pub fn from_challenge(r: T) -> Self {
        let r2 = r.clone() * r.clone();
        let r3 = r2.clone() * r.clone();
        ChallengeKey { r1: r, r2, r3 }
    }
}

/// State machine for multiset memory checking.
///
/// Generic over:
/// - `T`: the field element type (extension field, e.g. `u128` treated as
///   GF(2^128), or a Vope / Q type for VOLE-authenticated values).
/// - `H`: the hasher used to accumulate encodings.
///
/// Both prover and verifier instantiate this with the same `H`; they differ
/// only in `T` (Vope vs Q).
///
/// # Usage
///
/// ```text
/// let mut state = MemoryCheckState::<T, AdditiveHasher>::new(key);
///
/// // Initialise storage cells.
/// state.init(addr_zero, value_zero);
///
/// // Circuit execution: writes and reads.
/// state.write(addr, new_val, 1, old_val, 0);
/// state.read(addr, val, 2, 1);
///
/// // Drain all surviving cells.
/// state.drain(addr, final_val, last_ts);
///
/// // Check consistency.
/// assert!(state.verify());
/// ```
#[derive(Clone)]
pub struct MemoryCheckState<T, H: MemoryHasher<T> = AdditiveHasher> {
    key: ChallengeKey<T>,
    produce: H::State,
    consume: H::State,
}

impl<T, H> MemoryCheckState<T, H>
where
    T: Clone + Default + Add<Output = T> + Mul<Output = T>,
    H: MemoryHasher<T>,
{
    /// Create a new memory check state from a challenge key.
    pub fn new(key: ChallengeKey<T>) -> Self {
        MemoryCheckState {
            key,
            produce: H::new_state(),
            consume: H::new_state(),
        }
    }

    /// Encode `(addr, value, timestamp)` as `addr·r + value·r² + ts·r³`.
    ///
    /// `addr` and `value` are field elements (possibly VOLE-authenticated).
    /// `timestamp` is a public integer, encoded by scaling `r³` by `ts`.
    pub fn encode(&self, addr: T, value: T, timestamp: u64) -> T {
        let a = addr * self.key.r1.clone();
        let v = value * self.key.r2.clone();
        // timestamp · r³ : scale r³ by the public integer.
        let t = self.scale_by_u64(self.key.r3.clone(), timestamp);
        a + v + t
    }

    /// Initialise a storage cell: produce entry `(addr, zero, 0)`.
    ///
    /// Call once per address before any write/read.
    pub fn init(&mut self, addr: T, zero_value: T) {
        let enc = self.encode(addr, zero_value, 0);
        H::absorb(&mut self.produce, enc);
    }

    /// Record a write that overwrites `(addr, old_value, old_ts)` with
    /// `(addr, new_value, timestamp)`.
    ///
    /// The caller (prover) supplies `old_value` and `old_ts` as public
    /// hints; if they are wrong the multiset check will fail.
    pub fn write(
        &mut self,
        addr: T,
        new_value: T,
        timestamp: u64,
        old_value: T,
        old_timestamp: u64,
    ) where
        T: Clone,
    {
        // Produce the new entry.
        let enc_new = self.encode(addr.clone(), new_value, timestamp);
        H::absorb(&mut self.produce, enc_new);
        // Consume the old entry.
        let enc_old = self.encode(addr, old_value, old_timestamp);
        H::absorb(&mut self.consume, enc_old);
    }

    /// Record a read of `value` at `addr` at time `timestamp`, matching
    /// the write at `write_timestamp`.
    ///
    /// In commitment mode the `value` is an oracle hint from the prover.
    /// `write_timestamp` is a public hint identifying which write this
    /// read corresponds to.
    pub fn read(
        &mut self,
        addr: T,
        value: T,
        timestamp: u64,
        write_timestamp: u64,
    ) where
        T: Clone,
    {
        // Produce: the read re-produces the entry with the read's own
        // timestamp so the entry remains available for future reads.
        let enc_produce = self.encode(addr.clone(), value.clone(), timestamp);
        H::absorb(&mut self.produce, enc_produce);
        // Consume: consume the entry from the write (or previous read)
        // that established this value.
        let enc_consume = self.encode(addr, value, write_timestamp);
        H::absorb(&mut self.consume, enc_consume);
    }

    /// Drain a surviving cell at the end of the proof.
    ///
    /// Call once per address.  `final_value` and `final_timestamp` must
    /// match the last write (or the init if never written).
    pub fn drain(&mut self, addr: T, final_value: T, final_timestamp: u64) {
        let enc = self.encode(addr, final_value, final_timestamp);
        H::absorb(&mut self.consume, enc);
    }

    /// Check multiset equality: `H_produce == H_consume`.
    pub fn verify(&self) -> bool
    where
        H: MemoryHasher<T, State: PartialEq>,
    {
        H::finalize_eq(&self.produce, &self.consume)
    }

    // -- internal helpers ---------------------------------------------------

    /// Multiply a field element by a public u64 via repeated doubling.
    ///
    /// This is `x · n` in the field, where `n` is a public integer.
    /// In GF(2^k) with characteristic 2, `2x = 0`, so `n·x = (n mod 2)·x`.
    /// For general fields (or composite values like Vope), this does
    /// schoolbook scalar multiplication.
    fn scale_by_u64(&self, x: T, n: u64) -> T {
        if n == 0 {
            return T::default();
        }
        if n == 1 {
            return x;
        }
        // For general fields: double-and-add over the 64 bits of n.
        // We iterate all 64 bit positions; positions beyond the MSB contribute 0.
        let mut acc = T::default();
        let mut base = x;
        for bit in 0u64..64u64 {
            if (n >> bit) & 1 == 1 {
                acc = acc + base.clone();
            }
            base = base.clone() + base; // double
        }
        acc
    }
}

// ============================================================================
// N-lane (SIMD) convenience
// ============================================================================

/// Create N independent [`MemoryCheckState`]s, one per VOLE lane.
///
/// This is the natural instantiation for the VOLE prover/verifier: each
/// lane `i ∈ 0..N` runs an independent multiset check with challenge
/// `r[i]`.
pub fn memory_check_per_lane<N, T>(
    challenges: Array<T, N>,
) -> Array<MemoryCheckState<T, AdditiveHasher>, N>
where
    N: ArraySize,
    T: Clone + Default + Add<Output = T> + Mul<Output = T> + PartialEq,
{
    Array::from_fn(|i| {
        let key = ChallengeKey::from_challenge(challenges[i].clone());
        MemoryCheckState::new(key)
    })
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;

    // Use u64 as a simple "field" for testing (wrapping arithmetic).
    // This is NOT a real field (no proper inverse), but sufficient to
    // exercise the state machine logic.

    #[test]
    fn test_init_drain_balanced() {
        let key = ChallengeKey::from_challenge(7u64);
        let mut state = MemoryCheckState::<u64, AdditiveHasher>::new(key);

        // Init cell at addr 0 with value 0.
        state.init(0, 0);
        // Drain immediately (no writes).
        state.drain(0, 0, 0);

        assert!(state.verify(), "init + drain should balance");
    }

    #[test]
    fn test_write_then_drain() {
        let key = ChallengeKey::from_challenge(13u64);
        let mut state = MemoryCheckState::<u64, AdditiveHasher>::new(key);

        state.init(5, 0);                       // addr=5, value=0, t=0
        state.write(5, 42, 1, 0, 0);            // overwrite 0→42 at t=1
        state.drain(5, 42, 1);                   // final state: (5, 42, 1)

        assert!(state.verify());
    }

    #[test]
    fn test_write_read_drain() {
        let key = ChallengeKey::from_challenge(17u64);
        let mut state = MemoryCheckState::<u64, AdditiveHasher>::new(key);

        state.init(3, 0);                        // (3, 0, 0)
        state.write(3, 99, 1, 0, 0);             // overwrite: (3, 0, 0) → (3, 99, 1)
        state.read(3, 99, 2, 1);                  // read: consume (3,99,1), produce (3,99,2)
        state.drain(3, 99, 2);                    // drain: consume (3, 99, 2)

        assert!(state.verify());
    }

    #[test]
    fn test_wrong_read_fails() {
        let key = ChallengeKey::from_challenge(19u64);
        let mut state = MemoryCheckState::<u64, AdditiveHasher>::new(key);

        state.init(3, 0);
        state.write(3, 99, 1, 0, 0);
        state.read(3, 77, 2, 1);                  // WRONG value: 77 instead of 99
        state.drain(3, 77, 2);                     // drain with the wrong value

        // The multiset won't balance because the consumed entry (3,99,1)
        // was never produced (we produced (3,77,2) instead).
        assert!(!state.verify(), "wrong read value should be detected");
    }

    #[test]
    fn test_wrong_write_timestamp_fails() {
        let key = ChallengeKey::from_challenge(23u64);
        let mut state = MemoryCheckState::<u64, AdditiveHasher>::new(key);

        state.init(1, 0);
        state.write(1, 10, 1, 0, 0);
        state.write(1, 20, 2, 10, 0);             // WRONG old_ts: 0 instead of 1
        state.drain(1, 20, 2);

        assert!(!state.verify(), "wrong old timestamp should be detected");
    }

    #[test]
    fn test_multiple_cells() {
        let key = ChallengeKey::from_challenge(31u64);
        let mut state = MemoryCheckState::<u64, AdditiveHasher>::new(key);

        // Two independent cells.
        state.init(0, 0);
        state.init(1, 0);

        state.write(0, 100, 1, 0, 0);
        state.write(1, 200, 2, 0, 0);
        state.read(0, 100, 3, 1);
        state.read(1, 200, 4, 2);

        state.drain(0, 100, 3);
        state.drain(1, 200, 4);

        assert!(state.verify());
    }

    #[test]
    fn test_multiple_writes_same_cell() {
        let key = ChallengeKey::from_challenge(37u64);
        let mut state = MemoryCheckState::<u64, AdditiveHasher>::new(key);

        state.init(0, 0);
        state.write(0, 10, 1, 0, 0);             // 0 → 10
        state.write(0, 20, 2, 10, 1);            // 10 → 20
        state.write(0, 30, 3, 20, 2);            // 20 → 30
        state.read(0, 30, 4, 3);                  // read current value
        state.drain(0, 30, 4);

        assert!(state.verify());
    }

    #[test]
    fn test_multiple_reads_between_writes() {
        let key = ChallengeKey::from_challenge(41u64);
        let mut state = MemoryCheckState::<u64, AdditiveHasher>::new(key);

        state.init(0, 0);
        state.write(0, 55, 1, 0, 0);
        // Two consecutive reads — each consumes the previous entry's timestamp
        // and re-produces with its own.
        state.read(0, 55, 2, 1);     // consume t=1, produce t=2
        state.read(0, 55, 3, 2);     // consume t=2, produce t=3
        state.drain(0, 55, 3);

        assert!(state.verify());
    }
}
