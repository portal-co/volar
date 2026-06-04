// @reliability: experimental
// @ai: assisted
//! Sparse **R1CS** and **relaxed R1CS** over the scalar field `F_ℓ`.
//!
//! A constraint system has matrices `A, B, C ∈ F^{m×n}` (sparse triples) and is
//! satisfied by an assignment `z ∈ F^n` iff `(A·z) ∘ (B·z) = C·z` (Hadamard
//! product).  Following Nova, the assignment is laid out as `z = [W ‖ u]`: the
//! private witness `W` (length `n−1`) followed by the relaxation scalar `u`
//! (column `n−1`), which plays the role of the constant `1` in a plain instance.
//!
//! The **relaxed** relation introduces an error vector `E ∈ F^m`:
//! `(A·z) ∘ (B·z) = u·(C·z) + E`.  A fresh (plain) instance has `u = 1`, `E = 0`.
//! Folding (see [`crate::nifs`]) preserves this relation.

use alloc::vec::Vec;

use crate::scalar::Scalar;

/// A sparse R1CS constraint system over `F_ℓ`.
#[derive(Clone)]
pub struct R1CS {
    pub num_cons: usize,
    /// Total columns of `z` (= `|W| + 1`; the last column is `u`).
    pub num_vars: usize,
    pub a: Vec<(usize, usize, Scalar)>,
    pub b: Vec<(usize, usize, Scalar)>,
    pub c: Vec<(usize, usize, Scalar)>,
}

/// Sparse matrix–vector product `M·z` → length `num_cons`.
fn matvec(mat: &[(usize, usize, Scalar)], z: &[Scalar], num_cons: usize) -> Vec<Scalar> {
    let mut out = alloc::vec![Scalar::ZERO; num_cons];
    for (row, col, val) in mat {
        out[*row] = out[*row].add(&val.mul(&z[*col]));
    }
    out
}

/// A relaxed-R1CS **instance** (public): commitments to `W` and `E`, plus `u`.
#[derive(Clone)]
pub struct RelaxedInstance {
    pub comm_w: volar_spec::curve::EdPoint,
    pub comm_e: volar_spec::curve::EdPoint,
    pub u: Scalar,
}

/// A relaxed-R1CS **witness** (private): the assignment `W`, error `E`, and the
/// commitment blinders.
#[derive(Clone)]
pub struct RelaxedWitness {
    pub w: Vec<Scalar>,
    pub e: Vec<Scalar>,
    pub r_w: Scalar,
    pub r_e: Scalar,
}

impl R1CS {
    /// `z = [W ‖ u]`.
    pub fn full_z(&self, w: &[Scalar], u: &Scalar) -> Vec<Scalar> {
        assert_eq!(w.len(), self.num_vars - 1, "witness length must be num_vars - 1");
        let mut z = w.to_vec();
        z.push(*u);
        z
    }

    /// `(A·z, B·z, C·z)`.
    pub fn eval_abc(&self, z: &[Scalar]) -> (Vec<Scalar>, Vec<Scalar>, Vec<Scalar>) {
        (
            matvec(&self.a, z, self.num_cons),
            matvec(&self.b, z, self.num_cons),
            matvec(&self.c, z, self.num_cons),
        )
    }

    /// Plain satisfaction `(A·z) ∘ (B·z) = C·z` (for `z` with the `u`-column = 1).
    pub fn is_satisfied(&self, z: &[Scalar]) -> bool {
        let (az, bz, cz) = self.eval_abc(z);
        (0..self.num_cons).all(|i| az[i].mul(&bz[i]) == cz[i])
    }

    /// Relaxed (algebraic) satisfaction `(A·z) ∘ (B·z) = u·(C·z) + E`, with
    /// `z = [W ‖ u]`.  (Commitment consistency is checked separately, in
    /// [`crate::verify`].)
    pub fn is_satisfied_relaxed(&self, w: &[Scalar], e: &[Scalar], u: &Scalar) -> bool {
        if e.len() != self.num_cons {
            return false;
        }
        let z = self.full_z(w, u);
        let (az, bz, cz) = self.eval_abc(&z);
        (0..self.num_cons).all(|i| az[i].mul(&bz[i]) == u.mul(&cz[i]).add(&e[i]))
    }
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use alloc::vec;

    /// One multiplication gate: `w0 · w1 = w2`.
    /// z = [w0, w1, w2, u];  A picks w0, B picks w1, C picks w2.
    fn mul_gate() -> R1CS {
        R1CS {
            num_cons: 1,
            num_vars: 4,
            a: vec![(0, 0, Scalar::ONE)],
            b: vec![(0, 1, Scalar::ONE)],
            c: vec![(0, 2, Scalar::ONE)],
        }
    }

    #[test]
    fn plain_satisfied() {
        let r = mul_gate();
        // 3 · 4 = 12
        let z = vec![Scalar::from_u64(3), Scalar::from_u64(4), Scalar::from_u64(12), Scalar::ONE];
        assert!(r.is_satisfied(&z));
        let bad = vec![Scalar::from_u64(3), Scalar::from_u64(4), Scalar::from_u64(13), Scalar::ONE];
        assert!(!r.is_satisfied(&bad));
    }

    #[test]
    fn relaxed_fresh_matches_plain() {
        let r = mul_gate();
        let w = vec![Scalar::from_u64(3), Scalar::from_u64(4), Scalar::from_u64(12)];
        let e = vec![Scalar::ZERO];
        assert!(r.is_satisfied_relaxed(&w, &e, &Scalar::ONE));
    }

    #[test]
    fn relaxed_with_u_and_error() {
        // With u = 2: A z ∘ B z = w0·w1 (unaffected by u); u·Cz = 2·w2; so
        // E must absorb w0·w1 − 2·w2.  Pick w0=3,w1=4 ⇒ 12; w2=12 ⇒ E = 12−24 = −12.
        let r = mul_gate();
        let w = vec![Scalar::from_u64(3), Scalar::from_u64(4), Scalar::from_u64(12)];
        let u = Scalar::from_u64(2);
        let e = vec![Scalar::from_u64(12).sub(&Scalar::from_u64(24))];
        assert!(r.is_satisfied_relaxed(&w, &e, &u));
    }
}
