// @reliability: experimental
// @ai: assisted
//! Public entry points tying Phase 1 ([`crate::fold`]) and Phase 2
//! ([`crate::ligero`]) together, discipline-gated exactly like a
//! Nova-style `prove_verifier`/`verify_folded` split would be — and,
//! precisely named, the real implementation of a `compress_with_snark`-
//! shaped seam for a native-`GF(2^k)` fold. See
//! `docs/prove-the-verifier-iop.md`.

use volar_discipline::{NonZk, Tagged, Transparent};

use crate::field::{Field, Gf128};
use crate::fold::IopAccumulator;
use crate::ligero::LigeroProof;
use crate::transcript::FromBytes;

/// Phase 1's output plus the memory-accumulator boundary: the small,
/// fixed-size final `(W, E, u)` together with `mem_acc_in`/`mem_acc_out`
/// (each a `Vec<F>` of any length — mirrors the shape a caller's own
/// memory-consistency bookkeeping produces; empty slices for a circuit with
/// no committed storage). This crate's analogue of a Nova-style
/// `VerifierFold`, native to `GF(2^k)`.
pub struct IopVerifierFold<F: Field = Gf128> {
    pub w: alloc::vec::Vec<F>,
    pub e: alloc::vec::Vec<F>,
    pub u: F,
    pub mem_acc_in: alloc::vec::Vec<F>,
    pub mem_acc_out: alloc::vec::Vec<F>,
}

/// Build [`IopVerifierFold`] from a finished [`IopAccumulator`] plus the
/// memory-accumulator boundary, supplied wholesale by the caller — mirrors
/// how a memory boundary is handled on any prove-the-verifier backend: it
/// is never re-derived gate-by-gate, only committed once at finalization
/// (see `docs/prove-the-verifier-iop.md`'s memory-accumulator section for
/// why that's sound: the multiset-hash check itself is already free/checked
/// elsewhere; this is just the boundary attestation).
///
/// Panics if no gate was ever folded in.
pub fn finish_fold<F: Field>(
    acc: &IopAccumulator<F>,
    mem_acc_in: &[F],
    mem_acc_out: &[F],
) -> IopVerifierFold<F> {
    let (w, e, u) = acc
        .witness()
        .expect("finish_fold: accumulator has no folded gates");
    IopVerifierFold {
        w: w.to_vec(),
        e: e.to_vec(),
        u: *u,
        mem_acc_in: mem_acc_in.to_vec(),
        mem_acc_out: mem_acc_out.to_vec(),
    }
}

/// The finalization proof, discipline-tagged [`Transparent`] (the
/// verifier-as-computation carries no ZK secret; see
/// `docs/agent-context/discipline.md`).
pub type IopProof<F = Gf128> = LigeroProof<F>;

/// **Phase 2 entry point.** Fold the whole verifier (Phase 1, already done
/// by the caller via [`crate::fold::fold_gate`]) into `acc`, then produce the
/// finalization proof — including the memory-accumulator boundary
/// `(mem_acc_in, mem_acc_out)`, supplied wholesale (pass empty slices for a
/// circuit with no committed storage).
///
/// Bound `where Z: NonZk`: a [`volar_discipline::Zk`]-tagged accumulator
/// cannot reach this function — compile error (mixing ZK and non-ZK
/// proving is unsafe; see `docs/agent-context/discipline.md`).
pub fn prove_verifier_iop<Z: NonZk, F: Field + FromBytes>(
    acc: Tagged<Z, IopAccumulator<F>>,
    mem_acc_in: &[F],
    mem_acc_out: &[F],
) -> Tagged<Transparent, IopProof<F>> {
    let folded = finish_fold(acc.inner(), mem_acc_in, mem_acc_out);
    Tagged::seal(crate::ligero::prove(
        &folded.w,
        &folded.e,
        folded.u,
        &folded.mem_acc_in,
        &folded.mem_acc_out,
    ))
}

/// **Terminal check.** Takes a [`Transparent`] proof by construction — the
/// type system guarantees a ZK artifact never reaches here.
///
/// `expected_mem_acc`, if given, additionally checks the proof's recovered
/// `(mem_acc_in, mem_acc_out)` match the caller's own expectation (e.g. the
/// values a continuation/linking mechanism independently committed to) —
/// pass `None` to check only the AND-gate relation, matching the weaker
/// Nova-path behavior of committing the boundary without ever opening it.
pub fn verify_iop<F: Field + FromBytes>(
    proof: &Tagged<Transparent, IopProof<F>>,
    expected_mem_acc: Option<(&[F], &[F])>,
) -> bool {
    crate::ligero::verify(proof.inner(), expected_mem_acc)
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

    fn fold_honest_chain() -> IopAccumulator<Gf128> {
        let mut acc = IopAccumulator::fresh();
        for i in 1u64..=4 {
            let (ka, kb, kc, delta, v_hat) = honest_gate(i, i + 1, i + 2);
            let r = Gf128::from_u64(1000 + i);
            acc = fold_gate(acc, ka, kb, kc, delta, v_hat, r);
        }
        acc
    }

    #[test]
    fn honest_fold_and_prove_verifies_with_no_memory_boundary() {
        let acc = fold_honest_chain();
        let tagged: Tagged<Transparent, _> = Tagged::seal(acc);
        let proof = prove_verifier_iop(tagged, &[], &[]);
        assert!(verify_iop(&proof, None));
        assert_eq!(proof.discipline(), Discipline::Transparent);
    }

    #[test]
    fn honest_fold_and_prove_verifies_with_memory_boundary() {
        let acc = fold_honest_chain();
        let tagged: Tagged<Transparent, _> = Tagged::seal(acc);
        let mem_in = [Gf128::from_u64(1), Gf128::from_u64(2)];
        let mem_out = [Gf128::from_u64(9), Gf128::from_u64(9)];
        let proof = prove_verifier_iop(tagged, &mem_in, &mem_out);
        assert!(
            verify_iop(&proof, None),
            "no expectation given: should still verify"
        );
        assert!(
            verify_iop(&proof, Some((&mem_in, &mem_out))),
            "matching expected mem_acc: should verify"
        );
    }

    #[test]
    fn memory_boundary_mismatch_against_caller_expectation_is_rejected() {
        let acc = fold_honest_chain();
        let tagged: Tagged<Transparent, _> = Tagged::seal(acc);
        let mem_in = [Gf128::from_u64(1)];
        let mem_out = [Gf128::from_u64(9)];
        let proof = prove_verifier_iop(tagged, &mem_in, &mem_out);
        let wrong = [Gf128::from_u64(123)];
        assert!(
            !verify_iop(&proof, Some((&mem_in, &wrong))),
            "a caller-supplied expectation that doesn't match the proof's mem_acc_out must be rejected \
             — this is a genuine improvement over the Nova path, whose c_in/c_out are never opened/checked"
        );
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
        let proof = prove_verifier_iop(tagged, &[], &[]);
        assert!(
            !verify_iop(&proof, None),
            "a dishonest gate anywhere in the chain must be caught"
        );
    }
}

/// Compile-time proof that a ZK artifact cannot reach [`prove_verifier_iop`]
/// (mixing ZK and non-ZK proving is rejected by the `Z: NonZk` bound).
///
/// ```compile_fail
/// use volar_iop::{prove_verifier_iop, IopAccumulator};
/// use volar_iop::field::Gf128;
/// use volar_discipline::{Tagged, Zk};
///
/// let acc: Tagged<Zk, IopAccumulator<Gf128>> = Tagged::seal(IopAccumulator::fresh());
/// // `Zk: NonZk` is unsatisfied → this does not compile:
/// let _ = prove_verifier_iop(acc, &[], &[]);
/// ```
#[cfg(doctest)]
struct ZkCannotReachIopProof;
