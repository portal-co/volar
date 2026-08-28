// @reliability: normal
//! @ai: assisted
//! Compile-time **proving-discipline** typestate for Volar IR pipelines.
//!
//! A woven IR artifact carries one of two disciplines:
//!
//! | Marker | `IS_ZK` | Meaning |
//! |--------|---------|---------|
//! | [`Zk`] | `true`  | Carries zero-knowledge proving secrets: blinding MACs (`Vope.u`), coupling to the verifier-only global secret Δ, and a shared Fiat–Shamir transcript. |
//! | [`Transparent`] | `false` | Non-ZK: the verifier-as-a-computation, a regular (non-ZK) SNARK, or a folding instance. Carries no ZK blinding. |
//!
//! # Why a typestate
//!
//! Mixing a ZK prover with a non-ZK prover is unsafe — revealing the prover's
//! MAC blinding, leaking Δ, or replaying a Fiat–Shamir transcript across
//! independent proofs all break soundness or zero-knowledge.  [`Tagged<Z, T>`]
//! makes the discipline part of the *type* of an artifact, so a folding /
//! regular-SNARK combinator can bound on [`NonZk`] and a `Zk` artifact is
//! rejected **at compile time**.  [`DynTagged`] is the runtime-checked fallback
//! for boundaries where the discipline is only known dynamically.
//!
//! # Design invariant: discipline is never invented
//!
//! [`Tagged::seal`] is the single construction point and should be called only
//! at an audited boundary (a weaver's return, a fold entry).  Discipline is then
//! *preserved* by [`Tagged::map`] and only *changed* by an explicit, reviewed
//! re-seal — never silently.
//!
//! # Example
//!
//! ```
//! use volar_discipline::{Tagged, Zk, Transparent, NonZk, Discipline, DynTagged};
//!
//! // A ZK prover seals its module as `Zk`.
//! let prover: Tagged<Zk, u32> = Tagged::seal(7);
//!
//! // A folding entry only accepts non-ZK artifacts.
//! fn fold<Z: NonZk, T>(x: Tagged<Z, T>) -> T { x.into_inner() }
//! let verifier: Tagged<Transparent, u32> = Tagged::seal(9);
//! assert_eq!(fold(verifier), 9);
//! // fold(prover); // <- compile error: `Zk: NonZk` is not satisfied
//!
//! // Dynamic boundary: recover the static tag with a runtime check.
//! let dynv = DynTagged::new(9u32, Discipline::Transparent);
//! let back: Tagged<Transparent, u32> = dynv.require().unwrap();
//! assert_eq!(back.into_inner(), 9);
//! ```

#![no_std]

use core::fmt;
use core::marker::PhantomData;

/// Runtime mirror of the compile-time discipline markers.
///
/// Used for error reporting and at [`DynTagged`] boundaries where the
/// discipline is not known statically.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum Discipline {
    /// Zero-knowledge: carries proving secrets that must not be mixed with
    /// non-ZK artifacts.
    Zk,
    /// Non-ZK: verifier-as-computation, regular SNARK, or folding instance.
    Transparent,
}

/// Compile-time marker for the proving discipline an IR artifact carries.
///
/// Implemented only by the zero-sized [`Zk`] and [`Transparent`] markers; not
/// intended to be implemented downstream.
pub trait ZkDiscipline: Copy + 'static {
    /// Whether this discipline carries zero-knowledge proving secrets.
    const IS_ZK: bool;

    /// The runtime mirror of this discipline.
    fn runtime() -> Discipline;
}

/// Zero-knowledge discipline: carries blinding MACs, Δ-coupling, and a shared
/// Fiat–Shamir transcript.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct Zk;

/// Non-ZK discipline: the verifier-as-a-computation, a regular (non-ZK) SNARK,
/// or a folding instance.  Carries no zero-knowledge proving secrets.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct Transparent;

impl ZkDiscipline for Zk {
    const IS_ZK: bool = true;
    #[inline]
    fn runtime() -> Discipline {
        Discipline::Zk
    }
}

impl ZkDiscipline for Transparent {
    const IS_ZK: bool = false;
    #[inline]
    fn runtime() -> Discipline {
        Discipline::Transparent
    }
}

/// Subtrait implemented **only** for [`Transparent`].
///
/// Folding and regular-SNARK APIs bound `where Z: NonZk` so that a [`Zk`]
/// artifact cannot reach them: the bound `Zk: NonZk` is unsatisfied and the
/// call fails to compile.
pub trait NonZk: ZkDiscipline {}
impl NonZk for Transparent {}

/// A value `T` tagged with a compile-time proving discipline `Z`.
///
/// Constructed at an audited boundary via [`Tagged::seal`], transformed
/// discipline-preservingly via [`Tagged::map`], and unwrapped via
/// [`Tagged::into_inner`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Tagged<Z: ZkDiscipline, T> {
    inner: T,
    _marker: PhantomData<Z>,
}

impl<Z: ZkDiscipline, T> Tagged<Z, T> {
    /// Seal `inner` under discipline `Z`.
    ///
    /// This is the single audited construction point — call it only where the
    /// discipline of `inner` is established (a weaver's return, a fold entry).
    #[inline]
    pub fn seal(inner: T) -> Self {
        Tagged {
            inner,
            _marker: PhantomData,
        }
    }

    /// Consume the wrapper, returning the inner value.  The escape hatch for
    /// code that genuinely needs the raw artifact.
    #[inline]
    pub fn into_inner(self) -> T {
        self.inner
    }

    /// Borrow the inner value without changing discipline.
    #[inline]
    pub fn inner(&self) -> &T {
        &self.inner
    }

    /// Mutably borrow the inner value without changing discipline.
    #[inline]
    pub fn inner_mut(&mut self) -> &mut T {
        &mut self.inner
    }

    /// Apply a transformation, preserving discipline `Z`.
    #[inline]
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Tagged<Z, U> {
        Tagged {
            inner: f(self.inner),
            _marker: PhantomData,
        }
    }

    /// The runtime mirror of this artifact's discipline.
    #[inline]
    pub fn discipline(&self) -> Discipline {
        Z::runtime()
    }
}

/// A value `T` whose discipline is known only at runtime.
///
/// Recover a static [`Tagged`] via [`DynTagged::require`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DynTagged<T> {
    inner: T,
    disc: Discipline,
}

impl<T> DynTagged<T> {
    /// Tag `inner` with a runtime-known discipline.
    #[inline]
    pub fn new(inner: T, disc: Discipline) -> Self {
        DynTagged { inner, disc }
    }

    /// The runtime discipline.
    #[inline]
    pub fn discipline(&self) -> Discipline {
        self.disc
    }

    /// Borrow the inner value.
    #[inline]
    pub fn inner(&self) -> &T {
        &self.inner
    }

    /// Discard the discipline and return the inner value.
    #[inline]
    pub fn into_inner(self) -> T {
        self.inner
    }

    /// Recover a statically-tagged [`Tagged<Z, T>`], checking that the runtime
    /// discipline matches `Z`.
    ///
    /// Returns [`DisciplineError`] (carrying the expected and found
    /// disciplines) on mismatch.
    #[inline]
    pub fn require<Z: ZkDiscipline>(self) -> Result<Tagged<Z, T>, DisciplineError> {
        let expected = Z::runtime();
        if self.disc == expected {
            Ok(Tagged::seal(self.inner))
        } else {
            Err(DisciplineError {
                expected,
                found: self.disc,
            })
        }
    }
}

/// A runtime discipline mismatch surfaced by [`DynTagged::require`].
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct DisciplineError {
    /// The discipline the caller required.
    pub expected: Discipline,
    /// The discipline actually carried by the artifact.
    pub found: Discipline,
}

impl fmt::Display for DisciplineError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "proving-discipline mismatch: expected {:?}, found {:?} \
             (mixing ZK and non-ZK artifacts is unsafe)",
            self.expected, self.found
        )
    }
}

impl core::error::Error for DisciplineError {}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn markers_report_is_zk() {
        assert!(Zk::IS_ZK);
        assert!(!Transparent::IS_ZK);
        assert_eq!(Zk::runtime(), Discipline::Zk);
        assert_eq!(Transparent::runtime(), Discipline::Transparent);
    }

    #[test]
    fn seal_into_inner_round_trip() {
        let t: Tagged<Zk, u32> = Tagged::seal(42);
        assert_eq!(t.discipline(), Discipline::Zk);
        assert_eq!(t.into_inner(), 42);
    }

    #[test]
    fn map_preserves_discipline() {
        let t: Tagged<Transparent, u32> = Tagged::seal(3);
        let u: Tagged<Transparent, u64> = t.map(|x| x as u64 * 2);
        assert_eq!(u.discipline(), Discipline::Transparent);
        assert_eq!(u.into_inner(), 6);
    }

    #[test]
    fn inner_borrows() {
        let mut t: Tagged<Zk, u32> = Tagged::seal(1);
        assert_eq!(*t.inner(), 1);
        *t.inner_mut() += 9;
        assert_eq!(t.into_inner(), 10);
    }

    #[test]
    fn dyn_require_success() {
        let d = DynTagged::new(7u32, Discipline::Transparent);
        assert_eq!(d.discipline(), Discipline::Transparent);
        let t: Tagged<Transparent, u32> = d.require().unwrap();
        assert_eq!(t.into_inner(), 7);
    }

    #[test]
    fn dyn_require_mismatch_reports_both() {
        let d = DynTagged::new(7u32, Discipline::Zk);
        let err = d.require::<Transparent>().unwrap_err();
        assert_eq!(err.expected, Discipline::Transparent);
        assert_eq!(err.found, Discipline::Zk);
    }

    #[test]
    fn nonzk_bound_accepts_transparent() {
        fn fold<Z: NonZk, T>(x: Tagged<Z, T>) -> T {
            x.into_inner()
        }
        let v: Tagged<Transparent, u32> = Tagged::seal(5);
        assert_eq!(fold(v), 5);
        // fold::<Zk, _>(Tagged::seal(0)); // would fail: `Zk: NonZk` unsatisfied
    }
}
