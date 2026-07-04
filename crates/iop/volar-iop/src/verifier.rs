// @reliability: experimental
// @ai: assisted
//! Public entry points tying Phase 1 ([`crate::fold`]) and Phase 2
//! ([`crate::ligero`]) together, discipline-gated exactly like
//! `volar_fold::verifier::prove_verifier`/`verify_folded`
//! (`crates/fold/volar-fold/src/verifier.rs:203,220`) — and, precisely
//! named, the real implementation of that module's stubbed
//! `compress_with_snark` seam for a native-`GF(2^k)` fold. See
//! `docs/prove-the-verifier-iop.md`.

use volar_discipline::{NonZk, Tagged, Transparent};

use crate::field::{Field, Gf128};
use crate::fold::IopAccumulator;
use crate::ligero::LigeroProof;
use crate::transcript::FromBytes;

/// Phase 1's output: the small, fixed-size final `(W, E, u)` — this crate's
/// analogue of `volar_fold::verifier::VerifierFold`, native to `GF(2^k)`.
pub struct IopVerifierFold<F: Field = Gf128> {
    pub w: alloc::vec::Vec<F>,
    pub e: alloc::vec::Vec<F>,
    pub u: F,
}

/// Build [`IopVerifierFold`] from a finished [`IopAccumulator`] — panics if
/// no gate was ever folded in (matching
/// `volar_fold::ivc::prove_gap`'s "panics on an empty trace" convention).
pub fn finish_fold<F: Field>(acc: &IopAccumulator<F>) -> IopVerifierFold<F> {
    let (w, e, u) = acc.witness().expect("finish_fold: accumulator has no folded gates");
    IopVerifierFold { w: w.to_vec(), e: e.to_vec(), u: *u }
}

/// The finalization proof, discipline-tagged [`Transparent`] — matching
/// `VerifierFold`'s own tag (the verifier-as-computation carries no ZK
/// secret; see `docs/agent-context/discipline.md`).
pub type IopProof<F = Gf128> = LigeroProof<F>;

/// **Phase 2 entry point.** Fold the whole verifier (Phase 1, already done
/// by the caller via [`crate::fold::fold_gate`]) into `acc`, then produce the
/// finalization proof.
///
/// Bound `where Z: NonZk`: a [`volar_discipline::Zk`]-tagged accumulator
/// cannot reach this function — compile error, mirroring
/// `volar_fold::verifier::prove_verifier`'s own bound exactly (mixing ZK and
/// non-ZK proving is unsafe; see `docs/agent-context/discipline.md`).
pub fn prove_verifier_iop<Z: NonZk, F: Field + FromBytes>(
    acc: Tagged<Z, IopAccumulator<F>>,
) -> Tagged<Transparent, IopProof<F>> {
    let folded = finish_fold(acc.inner());
    Tagged::seal(crate::ligero::prove(&folded.w, &folded.e, folded.u))
}

/// **Terminal check.** Takes a [`Transparent`] proof by construction — the
/// type system guarantees a ZK artifact never reaches here, same as
/// `volar_fold::verifier::verify_folded`.
pub fn verify_iop<F: Field + FromBytes>(proof: &Tagged<Transparent, IopProof<F>>) -> bool {
    crate::ligero::verify(proof.inner())
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use crate::field::Gf128;
    use crate::fold::fold_gate;
    use volar_discipline::{Discipline, Transparent};

    fn honest_gate(a: u64, b: u64, d: u64) -> (Gf128, Gf128, Gf128, Gf128, Gf128) {
        let ka = Gf128::from_u64(a);
        let kb = Gf128::from_u64(b);
        let delta = Gf128::from_u64(d);
        let kc = ka.mul(&kb).mul(&delta.inv());
        (ka, kb, kc, delta, Gf128::ZERO)
    }

    #[test]
    fn honest_fold_and_prove_verifies() {
        let mut acc = IopAccumulator::fresh();
        for i in 1u64..=4 {
            let (ka, kb, kc, delta, v_hat) = honest_gate(i, i + 1, i + 2);
            let r = Gf128::from_u64(1000 + i);
            acc = fold_gate(acc, ka, kb, kc, delta, v_hat, r);
        }
        let tagged: Tagged<Transparent, _> = Tagged::seal(acc);
        let proof = prove_verifier_iop(tagged);
        assert!(verify_iop(&proof));
        assert_eq!(proof.discipline(), Discipline::Transparent);
    }

    #[test]
    fn dishonest_gate_in_chain_is_rejected() {
        let mut acc = IopAccumulator::fresh();
        for i in 1u64..=4 {
            let (ka, kb, kc, delta, mut v_hat) = honest_gate(i, i + 1, i + 2);
            if i == 3 {
                v_hat = v_hat.add(&Gf128::ONE);
            }
            let r = Gf128::from_u64(1000 + i);
            acc = fold_gate(acc, ka, kb, kc, delta, v_hat, r);
        }
        let tagged: Tagged<Transparent, _> = Tagged::seal(acc);
        let proof = prove_verifier_iop(tagged);
        assert!(!verify_iop(&proof), "a dishonest gate anywhere in the chain must be caught");
    }
}

/// Compile-time proof that a ZK artifact cannot reach [`prove_verifier_iop`]
/// (mixing ZK and non-ZK proving is rejected by the `Z: NonZk` bound) —
/// mirrors `volar_fold::verifier`'s own `compile_fail` doctest exactly.
///
/// ```compile_fail
/// use volar_iop::{prove_verifier_iop, IopAccumulator};
/// use volar_iop::field::Gf128;
/// use volar_discipline::{Tagged, Zk};
///
/// let acc: Tagged<Zk, IopAccumulator<Gf128>> = Tagged::seal(IopAccumulator::fresh());
/// // `Zk: NonZk` is unsatisfied → this does not compile:
/// let _ = prove_verifier_iop(acc);
/// ```
#[cfg(doctest)]
struct ZkCannotReachIopProof;
