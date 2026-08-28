// @reliability: experimental
// @ai: assisted
//! Native `GF(2^k)` extension field for the IOP-based prove-the-verifier
//! path — see `docs/prove-the-verifier-iop.md`.
//!
//! The VOLE verifier's per-gate check lives in `GF(2^8)`
//! ([`volar_primitives::Galois`]), which has only 256 elements — far too
//! small a challenge space for negligible Fiat–Shamir/query soundness
//! error. Rather than embed into an unrelated prime field (a real, generally-
//! open cryptographic question for any construction that needs to bridge a
//! binary field into a prime field), this module builds a **binary tower
//! field** on top of `Galois` by repeated quadratic extension — the
//! standard construction behind binary-field SNARKs (Wiedemann towers):
//! each level is `F_{i+1} = F_i[x]/(x^2 + x + β_i)`.
//!
//! ## Why quadratic-tower, not a single degree-`m` extension
//!
//! A single `GF(2^{8m})` built via one degree-`m` irreducible polynomial
//! over `GF(2^8)` would need an irreducibility test for an arbitrary-degree
//! polynomial (real algorithmic work, real correctness risk). A **quadratic**
//! extension in characteristic 2 has a much simpler, textbook irreducibility
//! criterion (Lidl & Niederreiter, *Finite Fields*, Thm 3.78): `x^2+x+β` is
//! irreducible over a field `F` of characteristic 2 iff the trace
//! `Tr_{F/GF(2)}(β) = β + β^2 + β^4 + ... + β^{2^{d-1}}` (`d = dim_GF(2) F`)
//! equals `1`. Stacking quadratic extensions reaches any `GF(2^{8·2^t})`
//! while only ever needing that one simple criterion at each step.
//!
//! ## Constants: how `BETA` was derived
//!
//! Each level's `BETA` (an element of the *previous* level, embedded as
//! `(BETA, 0)` to define `x^2 = x + BETA` in the *new* level) was found by
//! brute-force/pool search for a trace-1 element, then the whole tower was
//! checked (`scripts/verify_gf_tower.py` — see that file) against three
//! independent properties for 20 random elements per level:
//! `a^(2^dim - 1) = 1` (multiplicative-group order, i.e. no zero divisors),
//! distributivity, and the defining relation `x^2 = x + BETA` — all passed
//! at every level from `GF(2^16)` through `GF(2^128)`. This is standard
//! practice for this repo's derived constants (cf.
//! `docs/hash-to-curve-generators.md`) but — like every cryptographic
//! primitive in this crate — **still Tier 3 / needs cryptographic review**
//! before production trust.
//!
//! `β_i` (used to build level `i+1`) turned out to follow a clean pattern:
//! `β_0 = 0x20` (a `Galois`/`GF(2^8)` element), and `β_i = (0, β_{i-1})` for
//! `i ≥ 1` — i.e. "the previous level's `β` placed in the high half, zero in
//! the low half" — which is what `Ext::BETA` below encodes directly.

use volar_primitives::Galois;

/// Shared interface every tower level implements — mirrors
/// `volar_fold::scalar::Scalar`'s method-call convention (`.add()`/`.mul()`/
/// `.sub()`/`.neg()`) rather than `core::ops` operator overloads, so Phase 1
/// fold code (`crate::fold`) reads side-by-side with `volar-fold`'s
/// `nifs.rs` almost verbatim.
pub trait Field: Copy + Clone + PartialEq + Eq + core::fmt::Debug + Default {
    /// Dimension over `GF(2)` — `8` for the base field, doubling per level.
    const BITS: u32;
    const ZERO: Self;
    const ONE: Self;

    fn add(&self, rhs: &Self) -> Self;
    fn sub(&self, rhs: &Self) -> Self {
        // characteristic 2: subtraction is addition.
        self.add(rhs)
    }
    fn mul(&self, rhs: &Self) -> Self;
    fn neg(&self) -> Self {
        *self
    }
    fn square(&self) -> Self {
        self.mul(self)
    }
    fn is_zero(&self) -> bool {
        *self == Self::ZERO
    }

    /// Multiplicative inverse via Fermat's little theorem generalized to
    /// `GF(2^BITS)`: `a^{-1} = a^{2^BITS - 2}` (since `a^{2^BITS-1} = 1` for
    /// nonzero `a`). Computed via the square-and-multiply addition chain for
    /// exponent `2^BITS-2` (binary `1^{BITS-1}0`): `BITS-1` "square then
    /// multiply by self" steps followed by one final square — this needs no
    /// big-integer exponent representation, so it's uniform across every
    /// tower level. **Returns `ZERO` for `ZERO`** (mathematically
    /// undefined) rather than panicking — callers that divide must ensure a
    /// nonzero denominator themselves (e.g. Lagrange interpolation over
    /// *distinct* points, `crate::ligero`, guarantees this by construction).
    fn inv(&self) -> Self {
        let mut result = Self::ONE;
        for _ in 0..(Self::BITS - 1) {
            result = result.square().mul(self);
        }
        result.square()
    }

    /// `LSB`-first byte encoding — used by the Merkle/transcript layers, and
    /// as this level's own `Add`-preserving embedding into the next level up
    /// (`(lo, hi)` pairs are always little-endian: `lo` holds the embedded
    /// lower-degree half).
    fn to_bytes(&self) -> alloc::vec::Vec<u8>;
}

impl Field for Galois {
    const BITS: u32 = 8;
    const ZERO: Self = Galois(0);
    const ONE: Self = Galois(1);

    fn add(&self, rhs: &Self) -> Self {
        Galois(self.0 ^ rhs.0)
    }
    fn mul(&self, rhs: &Self) -> Self {
        (*self) * (*rhs)
    }
    fn to_bytes(&self) -> alloc::vec::Vec<u8> {
        alloc::vec![self.0]
    }
}

/// One quadratic-extension tower level: `F[x]/(x^2 + x + BETA)`, elements
/// `lo + hi·x`. `BETA` is an `F`-element (see the module doc for how it was
/// derived and verified).
///
/// Multiplication: `(lo1+hi1·x)(lo2+hi2·x) = lo1·lo2 + hi1·hi2·BETA +
/// (lo1·hi2 + hi1·lo2 + hi1·hi2)·x`, using `x^2 = x + BETA`.
// Derives are written out by hand (not `#[derive(...)]`) because the derive
// macros add a `B: Trait` bound on the (phantom, zero-sized) marker
// parameter even though no `B` value is ever stored — `#[derive(Default)]`
// on a `PhantomData<B>` field would otherwise wrongly require `B: Default`.
pub struct Ext<F: Field, B: BetaOf<F>> {
    pub lo: F,
    pub hi: F,
    _beta: core::marker::PhantomData<B>,
}

impl<F: Field, B: BetaOf<F>> Clone for Ext<F, B> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<F: Field, B: BetaOf<F>> Copy for Ext<F, B> {}
impl<F: Field, B: BetaOf<F>> PartialEq for Ext<F, B> {
    fn eq(&self, other: &Self) -> bool {
        self.lo == other.lo && self.hi == other.hi
    }
}
impl<F: Field, B: BetaOf<F>> Eq for Ext<F, B> {}
impl<F: Field, B: BetaOf<F>> core::fmt::Debug for Ext<F, B> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("Ext")
            .field("lo", &self.lo)
            .field("hi", &self.hi)
            .finish()
    }
}
impl<F: Field, B: BetaOf<F>> Default for Ext<F, B> {
    fn default() -> Self {
        Ext::new(F::default(), F::default())
    }
}

/// Supplies the verified `BETA` constant for one tower level — a separate
/// trait (rather than an associated const directly on `Ext`) purely so each
/// level can name a distinct zero-sized marker type carrying its own
/// verified constant, without needing const-generic non-primitive
/// parameters (not stable/ergonomic for this).
pub trait BetaOf<F: Field>: Copy + Clone + core::fmt::Debug {
    fn beta() -> F;
}

impl<F: Field, B: BetaOf<F>> Ext<F, B> {
    pub const fn new(lo: F, hi: F) -> Self {
        Ext {
            lo,
            hi,
            _beta: core::marker::PhantomData,
        }
    }
}

impl<F: Field, B: BetaOf<F>> Field for Ext<F, B> {
    const BITS: u32 = F::BITS * 2;
    const ZERO: Self = Ext::new(F::ZERO, F::ZERO);
    const ONE: Self = Ext::new(F::ONE, F::ZERO);

    fn add(&self, rhs: &Self) -> Self {
        Ext::new(self.lo.add(&rhs.lo), self.hi.add(&rhs.hi))
    }
    fn mul(&self, rhs: &Self) -> Self {
        let ll = self.lo.mul(&rhs.lo);
        let hh = self.hi.mul(&rhs.hi);
        let cross = self.lo.mul(&rhs.hi).add(&self.hi.mul(&rhs.lo));
        let lo = ll.add(&hh.mul(&B::beta()));
        let hi = cross.add(&hh);
        Ext::new(lo, hi)
    }
    fn to_bytes(&self) -> alloc::vec::Vec<u8> {
        let mut v = self.lo.to_bytes();
        v.extend(self.hi.to_bytes());
        v
    }
}

// Deliberately no `core::ops::{Add,Mul,Sub}` overloads here: this crate
// uses the `Field` trait's method-call convention (`.add(&x)`/`.mul(&x)`,
// mirroring `volar_fold::scalar::Scalar`) exclusively, and defining both
// would make `x.add(&y)` ambiguous between the two traits.

// ============================================================================
// The concrete tower: GF(2^8) -> GF(2^16) -> GF(2^32) -> GF(2^64) -> GF(2^128)
// ============================================================================

/// `β_0 = 0x20`, the verified trace-1 `GF(2^8)` element used to build
/// `Gf16 = GF(2^16)`.
#[derive(Clone, Copy, Debug)]
pub struct Beta8;
impl BetaOf<Galois> for Beta8 {
    fn beta() -> Galois {
        Galois(0x20)
    }
}

/// `GF(2^16)`.
pub type Gf16 = Ext<Galois, Beta8>;

/// `β_1 = (0, β_0)` as a `Gf16` element — the tower's own pattern (§ module
/// doc): "the previous level's `β`, shifted into the high half".
#[derive(Clone, Copy, Debug)]
pub struct Beta16;
impl BetaOf<Gf16> for Beta16 {
    fn beta() -> Gf16 {
        Gf16::new(Galois::ZERO, Beta8::beta())
    }
}

/// `GF(2^32)`.
pub type Gf32 = Ext<Gf16, Beta16>;

/// `β_2 = (0, β_1)` as a `Gf32` element.
#[derive(Clone, Copy, Debug)]
pub struct Beta32;
impl BetaOf<Gf32> for Beta32 {
    fn beta() -> Gf32 {
        Gf32::new(Gf16::ZERO, Beta16::beta())
    }
}

/// `GF(2^64)`.
pub type Gf64 = Ext<Gf32, Beta32>;

/// `β_3 = (0, β_2)` as a `Gf64` element.
#[derive(Clone, Copy, Debug)]
pub struct Beta64;
impl BetaOf<Gf64> for Beta64 {
    fn beta() -> Gf64 {
        Gf64::new(Gf32::ZERO, Beta32::beta())
    }
}

/// `GF(2^128)` — the recommended default challenge/accumulator field
/// (λ≈128-bit target, matching this repo's existing FAEST-transcript
/// convention, `crates/spec/volar-spec/src/faest/transcript.rs`).
pub type Gf128 = Ext<Gf64, Beta64>;

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use std::vec::Vec;

    fn rand_bytes(n: usize, seed: &mut u64) -> Vec<u8> {
        (0..n)
            .map(|_| {
                // xorshift64 — deterministic, no external RNG dependency needed for tests.
                *seed ^= *seed << 13;
                *seed ^= *seed >> 7;
                *seed ^= *seed << 17;
                (*seed & 0xff) as u8
            })
            .collect()
    }

    fn gf128_from_bytes(bytes: &[u8]) -> Gf128 {
        // Rebuild a Gf128 from 16 raw bytes via repeated halving — mirrors
        // `to_bytes`'s layout (lo half first, then hi half, recursively).
        fn build_ext<F: Field, B: BetaOf<F>>(
            bytes: &[u8],
            from_half: impl Fn(&[u8]) -> F,
        ) -> Ext<F, B> {
            let half = bytes.len() / 2;
            Ext::new(from_half(&bytes[..half]), from_half(&bytes[half..]))
        }
        fn g8(b: &[u8]) -> Galois {
            Galois(b[0])
        }
        fn g16(b: &[u8]) -> Gf16 {
            build_ext(b, g8)
        }
        fn g32(b: &[u8]) -> Gf32 {
            build_ext(b, g16)
        }
        fn g64(b: &[u8]) -> Gf64 {
            build_ext(b, g32)
        }
        build_ext(bytes, g64)
    }

    fn field_pow(a: Gf128, mut e: u128) -> Gf128 {
        let mut result = Gf128::ONE;
        let mut base = a;
        while e > 0 {
            if e & 1 == 1 {
                result = result.mul(&base);
            }
            base = base.square();
            e >>= 1;
        }
        result
    }

    #[test]
    fn bits_and_byte_length_match_across_levels() {
        assert_eq!(Galois::BITS, 8);
        assert_eq!(Gf16::BITS, 16);
        assert_eq!(Gf32::BITS, 32);
        assert_eq!(Gf64::BITS, 64);
        assert_eq!(Gf128::BITS, 128);
        assert_eq!(Gf128::ZERO.to_bytes().len(), 16);
    }

    #[test]
    fn defining_relation_x_squared_eq_x_plus_beta() {
        // x = (0, 1) at each level; check x^2 = x + BETA.
        let x16 = Gf16::new(Galois::ZERO, Galois::ONE);
        assert_eq!(
            x16.mul(&x16),
            x16.add(&Gf16::new(Beta8::beta(), Galois::ZERO))
        );

        let x128 = Gf128::new(Gf64::ZERO, Gf64::ONE);
        assert_eq!(
            x128.mul(&x128),
            x128.add(&Gf128::new(Beta64::beta(), Gf64::ZERO))
        );
    }

    #[test]
    fn multiplicative_group_order_holds_for_random_elements() {
        // a^(2^128 - 1) == 1 for random nonzero a — evidence of no zero
        // divisors (a reducible/incorrect tower construction would
        // generically fail this for random elements).
        let mut seed = 0xdeadbeefcafeu64;
        for _ in 0..20 {
            let bytes = loop {
                let b = rand_bytes(16, &mut seed);
                if b.iter().any(|x| *x != 0) {
                    break b;
                }
            };
            let a = gf128_from_bytes(&bytes);
            let order = u128::MAX; // 2^128 - 1
            assert_eq!(
                field_pow(a, order),
                Gf128::ONE,
                "a^(2^128-1) must be 1 for {a:?}"
            );
        }
    }

    #[test]
    fn distributivity_holds() {
        let mut seed = 0x1357_9bdf_2468_aceu64;
        for _ in 0..20 {
            let a = gf128_from_bytes(&rand_bytes(16, &mut seed));
            let b = gf128_from_bytes(&rand_bytes(16, &mut seed));
            let c = gf128_from_bytes(&rand_bytes(16, &mut seed));
            assert_eq!(a.add(&b).mul(&c), a.mul(&c).add(&b.mul(&c)));
        }
    }

    #[test]
    fn inv_is_multiplicative_inverse_for_nonzero_elements() {
        let mut seed = 999u64;
        for _ in 0..20 {
            let a = loop {
                let cand = gf128_from_bytes(&rand_bytes(16, &mut seed));
                if !cand.is_zero() {
                    break cand;
                }
            };
            assert_eq!(a.mul(&a.inv()), Gf128::ONE, "a * a^-1 must be 1 for {a:?}");
        }
    }

    #[test]
    fn inv_of_zero_is_zero_by_convention() {
        assert_eq!(Gf128::ZERO.inv(), Gf128::ZERO);
    }

    #[test]
    fn zero_and_one_identities() {
        let mut seed = 42u64;
        let a = gf128_from_bytes(&rand_bytes(16, &mut seed));
        assert_eq!(a.add(&Gf128::ZERO), a);
        assert_eq!(a.mul(&Gf128::ONE), a);
        assert_eq!(a.mul(&Gf128::ZERO), Gf128::ZERO);
    }

    #[test]
    fn to_bytes_round_trips_through_gf128_from_bytes() {
        let mut seed = 7u64;
        let bytes = rand_bytes(16, &mut seed);
        let a = gf128_from_bytes(&bytes);
        assert_eq!(a.to_bytes(), bytes);
    }
}
