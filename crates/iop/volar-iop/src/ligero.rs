// @reliability: experimental
// @ai: assisted
//! **Phase 2**: the finalization IOP — a small, one-shot, Merkle+Fiat–Shamir
//! argument that Phase 1's final accumulator `(W, E, u)` satisfies
//! `and_check_r1cs`'s relaxed relation, together with the memory-accumulator
//! boundary (`mem_acc_in`/`mem_acc_out`) carried alongside it. See
//! `docs/prove-the-verifier-iop.md` for the full design and honest scope.
//!
//! ## Why this is simpler than a "real" Ligero
//!
//! Classic Ligero (Ames–Bencsath–Feigenbaum et al., 2017) hides the witness
//! and proves a *quadratic* constraint about the hidden values using only a
//! few revealed codeword positions — real machinery, needed because the
//! witness must stay secret. **This proof does not need to hide anything**
//! (there is no secret left to hide, only soundness to preserve — true of
//! the whole prove-the-verifier problem). So this module reveals
//! `(W, E, u, mem_acc_in, mem_acc_out)` directly and checks the relation on
//! the revealed values — the genuinely new IOP part is a standard
//! **Reed–Solomon codeword proximity test**: the prover commits an RS
//! encoding of the message via Merkle tree, Fiat–Shamir picks random query
//! positions, and the verifier checks the opened positions are consistent
//! with *some* degree-`<k` polynomial (by interpolating from some queries
//! and cross-checking the rest) before trusting the recovered values. This
//! is the standard, well-understood "encode + spot-check" IOP of proximity
//! underlying Ligero/Aurora/FRI alike — simplified here to a single round
//! because the message never needs folding-in-half (that machinery —
//! BaseFold/FRI's recursive halving — only pays for itself on a *growing*
//! object, which Phase 1 already prevents).
//!
//! ## Message shape is generic, not hardcoded
//!
//! The message is `W (7) ++ E (3) ++ [u] ++ mem_acc_in (any length) ++
//! mem_acc_out (any length)` — `K` (message length), `N` (codeword length),
//! and `Q` (query count) are all **computed from the actual message
//! length**, not fixed constants, specifically so a future extension (more
//! rows for an oracle/action statement, a different-length memory
//! accumulator) doesn't need this module's machinery redesigned — only a
//! longer message vector.
//!
//! **Tier 3 / needs cryptographic review**: the concrete `N`/`Q` sizing
//! formulas below are a reasonable first choice, not a derived-and-proven
//! soundness bound — flag before production trust, same as this crate's
//! other constants.

use alloc::vec::Vec;

use crate::field::Field;
use crate::merkle::{self, AuthPath, MerkleTree};
use crate::transcript::{FromBytes, IopTranscript};

/// Redundant query margin beyond the `K` anchor queries (see [`sizes`]).
const QUERY_MARGIN: usize = 10;

/// Derive `(K, N, Q)` from the message length `k`: `N` is the next power of
/// two `≥ 2k` (matches this module's original fixed `11 -> 32` sizing
/// exactly), `Q = k + QUERY_MARGIN` (capped below `N`).
fn sizes(k: usize) -> (usize, usize, usize) {
    let n = (2 * k).next_power_of_two();
    let q = (k + QUERY_MARGIN).min(n - 1);
    (k, n, q)
}

/// Evaluate the unique degree-`<k` polynomial through `(points[i], values[i])`
/// (Lagrange form) at `at`. `points` must be pairwise distinct (guaranteed by
/// construction here — see [`eval_points`]).
fn lagrange_eval<F: Field>(points: &[F], values: &[F], at: F) -> F {
    let mut acc = F::ZERO;
    for i in 0..points.len() {
        let mut term = values[i];
        for j in 0..points.len() {
            if i == j {
                continue;
            }
            let num = at.sub(&points[j]);
            let den = points[i].sub(&points[j]);
            term = term.mul(&num).mul(&den.inv());
        }
        acc = acc.add(&term);
    }
    acc
}

/// `n` pairwise-distinct evaluation points `0, 1, ..., n-1` embedded via
/// [`FromBytes::from_u64`].
fn eval_points<F: Field + FromBytes>(n: usize) -> Vec<F> {
    (0..n as u64).map(F::from_u64).collect()
}

/// Systematically Reed–Solomon-encode `v` (length `k`) over the first `k`
/// [`eval_points`]: interpolate the unique degree-`<k` polynomial through
/// `(points[0..k], v)`, then evaluate it at *every* point in `0..n` — the
/// first `k` evaluations reproduce `v` exactly (systematic), the rest are
/// the redundant symbols the proximity test spot-checks.
fn rs_encode<F: Field + FromBytes>(v: &[F], n: usize) -> Vec<F> {
    let points = eval_points::<F>(n);
    let msg_points = &points[..v.len()];
    points.iter().map(|p| lagrange_eval(msg_points, v, *p)).collect()
}

/// The finalization proof: the Merkle root, the queried codeword positions
/// (values + Merkle paths), and the revealed message
/// `(W, E, u, mem_acc_in, mem_acc_out)`.
#[derive(Clone)]
pub struct LigeroProof<F: Field> {
    pub root: merkle::Digest32,
    pub query_indices: Vec<usize>,
    pub query_values: Vec<F>,
    pub query_paths: Vec<AuthPath>,
    pub w: Vec<F>,
    pub e: Vec<F>,
    pub u: F,
    pub mem_acc_in: Vec<F>,
    pub mem_acc_out: Vec<F>,
}

fn field_to_leaf<F: Field>(x: &F) -> Vec<u8> {
    x.to_bytes()
}

fn build_message<F: Field>(w: &[F], e: &[F], u: F, mem_acc_in: &[F], mem_acc_out: &[F]) -> Vec<F> {
    let mut v = w.to_vec();
    v.extend_from_slice(e);
    v.push(u);
    v.extend_from_slice(mem_acc_in);
    v.extend_from_slice(mem_acc_out);
    v
}

/// **Prover.** Build the finalization proof for a satisfying
/// `(W, E, u)` plus the memory-accumulator boundary `(mem_acc_in,
/// mem_acc_out)` (pass empty slices if a circuit has no committed storage —
/// mirrors `VerifierTrace`'s own `Vec<Scalar>` shape, no assumed length).
///
/// Panics if `w.len() != 7` or `e.len() != 3` (mismatched with
/// `crate::fold::and_check_r1cs`'s shape) — a caller error, not a runtime
/// data condition.
pub fn prove<F: Field + FromBytes>(w: &[F], e: &[F], u: F, mem_acc_in: &[F], mem_acc_out: &[F]) -> LigeroProof<F> {
    assert_eq!(w.len(), 7, "prove: W must have 7 slots (and_check_r1cs shape)");
    assert_eq!(e.len(), 3, "prove: E must have 3 slots (and_check_r1cs shape)");

    let v = build_message(w, e, u, mem_acc_in, mem_acc_out);
    let (_k, n, q) = sizes(v.len());
    let codeword = rs_encode(&v, n);
    let leaves: Vec<Vec<u8>> = codeword.iter().map(field_to_leaf).collect();
    let tree = MerkleTree::commit(&leaves);
    let root = tree.root();

    let mut t = IopTranscript::new().domain_sep(b"volar-iop-ligero-v1");
    t.absorb(&root);
    let query_indices = t.squeeze_indices(q, n);

    let query_values: Vec<F> = query_indices.iter().map(|&i| codeword[i]).collect();
    let query_paths: Vec<AuthPath> = query_indices.iter().map(|&i| tree.open(i)).collect();

    LigeroProof {
        root,
        query_indices,
        query_values,
        query_paths,
        w: w.to_vec(),
        e: e.to_vec(),
        u,
        mem_acc_in: mem_acc_in.to_vec(),
        mem_acc_out: mem_acc_out.to_vec(),
    }
}

/// **Verifier.** Re-derives the Fiat–Shamir query indices from the proof's
/// own root (so a cheating prover cannot choose favorable queries), checks
/// every queried Merkle path, checks the queried codeword is consistent
/// with a single degree-`<k` polynomial (the RS proximity test), checks
/// that polynomial's message-position evaluations match the claimed
/// `(W, E, u, mem_acc_in, mem_acc_out)`, checks `and_check_r1cs`'s relaxed
/// relation on them, and — if `expected_mem_acc` is given — checks the
/// recovered memory boundary matches it (an improvement over the Nova
/// path's own `c_in`/`c_out`, which are committed but never opened/checked
/// by `verify_folded`; see `docs/prove-the-verifier-iop.md`).
pub fn verify<F: Field + FromBytes>(proof: &LigeroProof<F>, expected_mem_acc: Option<(&[F], &[F])>) -> bool {
    if proof.w.len() != 7 || proof.e.len() != 3 {
        return false;
    }
    if let Some((exp_in, exp_out)) = expected_mem_acc {
        if proof.mem_acc_in != exp_in || proof.mem_acc_out != exp_out {
            return false;
        }
    }

    let mut claimed = proof.w.clone();
    claimed.extend_from_slice(&proof.e);
    claimed.push(proof.u);
    claimed.extend_from_slice(&proof.mem_acc_in);
    claimed.extend_from_slice(&proof.mem_acc_out);
    let (k, n, q) = sizes(claimed.len());
    if proof.query_indices.len() != q {
        return false;
    }

    let mut t = IopTranscript::new().domain_sep(b"volar-iop-ligero-v1");
    t.absorb(&proof.root);
    let expected_indices = t.squeeze_indices(q, n);
    if expected_indices != proof.query_indices {
        return false; // prover didn't use the honestly-derived queries
    }

    for i in 0..q {
        let leaf = field_to_leaf(&proof.query_values[i]);
        if !merkle::verify(&proof.root, &leaf, proof.query_indices[i], &proof.query_paths[i]) {
            return false;
        }
    }

    // RS proximity test: interpolate from the first k queries, cross-check
    // the rest.
    let points = eval_points::<F>(n);
    let anchor_points: Vec<F> = proof.query_indices[..k].iter().map(|&i| points[i]).collect();
    let anchor_values: Vec<F> = proof.query_values[..k].to_vec();
    for i in k..q {
        let at = points[proof.query_indices[i]];
        let expected = lagrange_eval(&anchor_points, &anchor_values, at);
        if expected != proof.query_values[i] {
            return false;
        }
    }

    // Recover the message from the interpolated polynomial at the
    // systematic message points, and cross-check against the claimed
    // values the prover sent alongside the proof.
    let msg_points = &points[..k];
    let mut recovered = Vec::with_capacity(k);
    for mp in msg_points {
        recovered.push(lagrange_eval(&anchor_points, &anchor_values, *mp));
    }
    if recovered != claimed {
        return false;
    }

    let r1cs = crate::fold::and_check_r1cs::<F>();
    r1cs.is_satisfied_relaxed(&proof.w, &proof.e, &proof.u)
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;
    use crate::field::Gf128;
    use crate::fold::{and_check_r1cs, gate_witness};

    fn honest_wu() -> (Vec<Gf128>, Vec<Gf128>, Gf128) {
        // A single honest AND-gate step, fresh (E = 0, u = 1).
        let ka = Gf128::from_u64(3);
        let kb = Gf128::from_u64(5);
        let delta = Gf128::from_u64(7);
        let kc = ka.mul(&kb).mul(&delta.inv()); // pick k_c so v_hat can be 0
        let v_hat = Gf128::ZERO;
        let w = gate_witness(ka, kb, kc, delta, v_hat);
        (w, vec![Gf128::ZERO; 3], Gf128::ONE)
    }

    #[test]
    fn honest_accumulator_proof_verifies_with_no_memory_boundary() {
        let (w, e, u) = honest_wu();
        let r1cs = and_check_r1cs::<Gf128>();
        assert!(r1cs.is_satisfied_relaxed(&w, &e, &u), "sanity: honest witness must satisfy relation");
        let proof = prove(&w, &e, u, &[], &[]);
        assert!(verify(&proof, None), "honest accumulator's finalization proof must verify");
    }

    #[test]
    fn honest_accumulator_proof_verifies_with_memory_boundary() {
        let (w, e, u) = honest_wu();
        let mem_in = vec![Gf128::from_u64(1), Gf128::from_u64(2)];
        let mem_out = vec![Gf128::from_u64(9), Gf128::from_u64(9)];
        let proof = prove(&w, &e, u, &mem_in, &mem_out);
        assert!(verify(&proof, None), "honest accumulator + memory boundary must verify without an expectation");
        assert!(
            verify(&proof, Some((&mem_in, &mem_out))),
            "honest accumulator + memory boundary must verify against the matching expectation"
        );
    }

    #[test]
    fn mismatched_expected_memory_boundary_is_rejected() {
        let (w, e, u) = honest_wu();
        let mem_in = vec![Gf128::from_u64(1)];
        let mem_out = vec![Gf128::from_u64(9)];
        let proof = prove(&w, &e, u, &mem_in, &mem_out);
        let wrong_out = vec![Gf128::from_u64(42)];
        assert!(
            !verify(&proof, Some((&mem_in, &wrong_out))),
            "a caller-supplied expected mem_acc_out that doesn't match must be rejected"
        );
    }

    #[test]
    fn tampered_memory_boundary_in_proof_is_rejected() {
        let (w, e, u) = honest_wu();
        let mem_in = vec![Gf128::from_u64(1)];
        let mem_out = vec![Gf128::from_u64(9)];
        let mut proof = prove(&w, &e, u, &mem_in, &mem_out);
        proof.mem_acc_out[0] = proof.mem_acc_out[0].add(&Gf128::ONE);
        assert!(!verify(&proof, None), "tampering the claimed mem_acc_out without redoing the codeword must fail");
    }

    #[test]
    fn tampered_witness_in_proof_is_rejected() {
        let (w, e, u) = honest_wu();
        let mut proof = prove(&w, &e, u, &[], &[]);
        proof.w[0] = proof.w[0].add(&Gf128::ONE);
        // Tampering the claimed W without redoing the codeword/queries must
        // fail the recovered-vs-claimed cross-check.
        assert!(!verify(&proof, None));
    }

    #[test]
    fn tampered_codeword_value_is_rejected() {
        let (w, e, u) = honest_wu();
        let mut proof = prove(&w, &e, u, &[], &[]);
        // Flip one queried codeword value (simulating a prover who lies
        // about an opened position) without fixing the Merkle path.
        proof.query_values[0] = proof.query_values[0].add(&Gf128::ONE);
        assert!(!verify(&proof, None), "tampering an opened codeword value must fail Merkle verification");
    }

    #[test]
    fn tampered_merkle_root_is_rejected() {
        let (w, e, u) = honest_wu();
        let mut proof = prove(&w, &e, u, &[], &[]);
        proof.root[0] ^= 0xff;
        assert!(!verify(&proof, None));
    }

    #[test]
    fn wrong_query_indices_are_rejected() {
        let (w, e, u) = honest_wu();
        let mut proof = prove(&w, &e, u, &[], &[]);
        proof.query_indices.swap(0, 1);
        assert!(!verify(&proof, None), "queries not matching the Fiat-Shamir-derived order must be rejected");
    }

    #[test]
    fn relation_violation_with_consistent_codeword_is_rejected() {
        // A prover who commits a *consistent* RS codeword for a witness
        // that does NOT satisfy and_check_r1cs must still be rejected (the
        // proximity test alone doesn't imply relation satisfaction — the
        // final relation check must run on the recovered values too).
        let ka = Gf128::from_u64(3);
        let kb = Gf128::from_u64(5);
        let delta = Gf128::from_u64(7);
        let kc = Gf128::from_u64(9); // wrong on purpose
        let v_hat = Gf128::ZERO;
        let w = gate_witness(ka, kb, kc, delta, v_hat);
        let e = vec![Gf128::ZERO; 3];
        let u = Gf128::ONE;
        assert!(!and_check_r1cs::<Gf128>().is_satisfied_relaxed(&w, &e, &u));
        let proof = prove(&w, &e, u, &[], &[]);
        assert!(!verify(&proof, None), "an internally-consistent but relation-violating witness must be rejected");
    }

    #[test]
    fn and_gate_slots_matches_documented_shape() {
        // Sanity-pin the AND-gate accumulator's shape (W:7 + E:3 + u:1 = 11)
        // against what and_check_r1cs actually produces.
        let (w, e, _u) = honest_wu();
        assert_eq!(w.len() + e.len() + 1, 11);
    }
}
