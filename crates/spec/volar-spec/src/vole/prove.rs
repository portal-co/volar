// @reliability: experimental
//! @ai: assisted
//! Quicksilver-style VOLE AND gate primitives used by the weaver-generated
//! prover and verifier circuits.
//!
//! # Security
//!
//! These functions implement the online phase of the Quicksilver protocol
//! (Yang et al., CCS 2021 / VOLEitH).  They assume:
//! - VOLE correlations are set up before calling — prover holds `(x_w, M_w)`,
//!   verifier holds `K_w = M_w + x_w · Δ` for every input wire `w`.
//! - The extension field type `T` is a field where `0 ≠ 1` and multiplication
//!   is the field product (e.g. GF(2^64) or GF(2^128)).
//!
//! **Soundness** depends on `Δ` being uniformly random and secret from the prover.

use super::*;
use cipher::consts::{U1, U2, U3};

/// Prover step for a single AND gate `c = a AND b`.
///
/// Given VOLE-committed wires `vope_a` and `vope_b`, produces:
/// - `vope_c` : the committed output wire (degree-1 Vope), and
/// - `hat`    : the `V̂` value that must be sent to the verifier.
///
/// # Protocol
///
/// A Vope `{ u, v }` encodes a wire with bit value `u[0]` (lifted to `T`)
/// and mask `v`, satisfying `K_w = v + u[0] · Δ` (verifier's share).
///
/// For AND:
/// - `u_c = u_a * u_b`  (element-wise; encodes the AND of the bits)
/// - `v_c = v_a * u_b + u_a * v_b`  (cross-term; consistent with VOLE)
/// - `hat = v_a * v_b`  (constant-product; sent to verifier as V̂)
///
/// The verifier checks `K_a * K_b + hat == K_c * Δ`.
pub fn vole_and_prover_step<N, T>(
    vope_a: Vope<N, T, U1>,
    vope_b: Vope<N, T, U1>,
) -> (Vope<N, T, U1>, Array<T, N>)
where
    N: VoleArray<T>,
    T: Clone + Add<Output = T> + Mul<Output = T> + Default,
{
    let u_c_inner = Array::<T, N>::from_fn(|i| {
        vope_a.u[0][i].clone() * vope_b.u[0][i].clone()
    });
    let u_c = Array::<Array<T, N>, U1>::from_fn(|_| u_c_inner.clone());
    let v_c = Array::<T, N>::from_fn(|i| {
        vope_a.v[i].clone() * vope_b.u[0][i].clone()
            + vope_b.v[i].clone() * vope_a.u[0][i].clone()
    });
    let hat = Array::<T, N>::from_fn(|i| {
        vope_a.v[i].clone() * vope_b.v[i].clone()
    });
    (Vope { u: u_c, v: v_c }, hat)
}

/// Verifier step for a single AND gate `c = a AND b`.
///
/// Given verifier VOLE shares `q_a`, `q_b`, `q_and` (share for the output
/// wire `c`), and the prover-sent `hat`, checks the Quicksilver constraint:
///
/// ```text
/// K_a · K_b + V̂  ==  K_c · Δ    (element-wise in T, for all N lanes)
/// ```
///
/// Returns `(Q<N, T>, bool)` — the output wire's verifier share and whether
/// the check passed on every lane.  The returned Q is a clone of `q_and`
/// (propagated forward so the caller can derive dependent gate shares).
pub fn vole_and_verifier_check<N, T>(
    delta: &Delta<N, T>,
    q_a: &Q<N, T>,
    q_b: &Q<N, T>,
    q_and: &Q<N, T>,
    hat: &Array<T, N>,
) -> (Q<N, T>, bool)
where
    N: ArraySize,
    T: Clone + Add<Output = T> + Mul<Output = T> + PartialEq + Default,
{
    let mut ok = true;
    for i in 0..N::USIZE {
        let lhs = q_a.q[i].clone() * q_b.q[i].clone() + hat[i].clone();
        let rhs = q_and.q[i].clone() * delta.delta[i].clone();
        ok = ok && (lhs == rhs);
    }
    (Q { q: q_and.q.clone() }, ok)
}

/// Prover step for a K=2 S-box AND gate: `c = a AND b`.
///
/// Returns `(vope_c_k1, vope_c_k2)`:
/// - `vope_c_k1`: K=1 wire to use in downstream gates (same wire value as K=1 AND).
/// - `vope_c_k2`: K=2 hat-free product commitment sent to the verifier.
///
/// The K=2 Vope satisfies `K_{c}(Δ) = K_a(Δ)·K_b(Δ)` exactly — no hat
/// correction is needed.  The verifier reconstructs `Q_c = K_c(Δ)` by
/// evaluating the K=2 Vope at Δ and checks `Q_a·Q_b == Q_c`.
pub fn vole_sbox_prover_step<N, T>(
    vope_a: Vope<N, T, U1>,
    vope_b: Vope<N, T, U1>,
) -> (Vope<N, T, U1>, Vope<N, T, U2>)
where
    N: VoleArray<T>,
    T: Add<Output = T> + Mul<Output = T> + Default + Clone,
{
    // K=2 product: v_k2 = v_a·v_b, u_k2[0] = v_a·u_b+u_a·v_b, u_k2[1] = u_a·u_b
    let k2: Vope<N, T, U2> = vope_a.mul_generalized(&vope_b);

    // Extract K=1 downstream wire: v_k1 = u_k2[0], u_k1[0] = u_k2[1]
    let k1 = Vope::<N, T, U1> {
        u: Array::<Array<T, N>, U1>::from_fn(|_| k2.u[1].clone()),
        v: k2.u[0].clone(),
    };
    (k1, k2)
}

/// Verifier check for a K=2 S-box AND gate.
///
/// Given the prover-supplied K=2 Vope, evaluates it at `Δ` to get `Q_c` and
/// checks `Q_a · Q_b == Q_c`.  No hat correction is needed.
///
/// Returns `(Q_c, ok)`.
pub fn vole_sbox_verifier_check<N, T>(
    delta: &Delta<N, T>,
    q_a: &Q<N, T>,
    q_b: &Q<N, T>,
    vope_k2: Vope<N, T, U2>,
) -> (Q<N, T>, bool)
where
    N: VoleArray<T>,
    T: Clone + Add<Output = T> + Mul<Output = T> + PartialEq + Default + Into<T>,
{
    let q_c = vope_k2 * delta.clone(); // eval K=2 Vope at Δ → Q
    let mut ok = true;
    for i in 0..N::USIZE {
        ok = ok && (q_a.q[i].clone() * q_b.q[i].clone() == q_c.q[i]);
    }
    (q_c, ok)
}

/// Prover step for a degree-3 product gate: `c = a · b · d`.
///
/// Produces a `Vope<N, T, U3>` whose polynomial in Δ equals
/// `K_a(Δ) · K_b(Δ) · K_d(Δ)`.  The prover sends this K=3 Vope's
/// coefficients to the verifier, who reconstructs `Q_abd = K_abd(Δ)` and
/// checks it against `Q_a · Q_b · Q_d` (see [`vole_mul3_verifier_check`]).
///
/// # FAEST usage
///
/// FAEST uses this for the AES S-box norm constraint: the prover commits
/// three intermediate field values and the verifier checks their product
/// equals the claimed S-box output, without revealing the individual wires.
///
/// # Safety
///
/// `// SAFETY(hazmat)`: uses `mul_generalized` exclusively within a
/// constraint-check context.  The returned K=3 Vope is immediately
/// evaluated by the verifier and is not used as a forward wire.
pub fn vole_mul3_prover_step<N, T>(
    vope_a: &Vope<N, T, U1>,
    vope_b: &Vope<N, T, U1>,
    vope_d: &Vope<N, T, U1>,
) -> Vope<N, T, U3>
where
    N: VoleArray<T>,
    T: Add<Output = T> + Mul<Output = T> + Default + Clone,
{
    let ab: Vope<N, T, U2> = vope_a.mul_generalized(vope_b);
    ab.mul_generalized(vope_d)
}

/// Verifier check for a degree-3 product gate: `c = a · b · d`.
///
/// The verifier:
/// 1. Evaluates the prover-provided K=3 Vope at `Δ` to obtain `Q_abd`.
/// 2. Computes `Q_a · Q_b · Q_d` element-wise.
/// 3. Checks equality on every lane.
///
/// Returns `(Q_abd, ok)` where `ok = true` iff all lanes pass.
pub fn vole_mul3_verifier_check<N, T>(
    delta: &Delta<N, T>,
    q_a: &Q<N, T>,
    q_b: &Q<N, T>,
    q_d: &Q<N, T>,
    vope_abd: Vope<N, T, U3>,
) -> (Q<N, T>, bool)
where
    N: VoleArray<T>,
    T: Clone + Add<Output = T> + Mul<Output = T> + PartialEq + Default + Into<T>,
{
    let q_abd = vope_abd * delta.clone();
    let mut ok = true;
    for i in 0..N::USIZE {
        let lhs = q_a.q[i].clone() * q_b.q[i].clone() * q_d.q[i].clone();
        ok = ok && (lhs == q_abd.q[i]);
    }
    (q_abd, ok)
}

#[cfg(test)]
mod tests {
    extern crate std;
    use super::*;
    use cipher::consts::U16;
    use volar_primitives::Galois128;

    type N16 = U16;

    fn make_vope(u0: u128, v: u128) -> Vope<N16, Galois128, U1> {
        Vope {
            u: Array::<Array<Galois128, N16>, U1>::from_fn(|_| {
                Array::<Galois128, N16>::from_fn(|_| Galois128(u0))
            }),
            v: Array::<Galois128, N16>::from_fn(|_| Galois128(v)),
        }
    }

    fn make_delta(d: u128) -> Delta<N16, Galois128> {
        Delta {
            delta: Array::<Galois128, N16>::from_fn(|_| Galois128(d)),
        }
    }

    fn make_q(q: u128) -> Q<N16, Galois128> {
        Q {
            q: Array::<Galois128, N16>::from_fn(|_| Galois128(q)),
        }
    }

    #[test]
    fn vole_mul3_honest_accepts() {
        // Pick small field elements so we can track the expected products.
        // Use Galois128 with trivial values (actual GF(2^128) mul, so 2*2 = 4 here is NOT true).
        // Use u=1,v=0 so K_a(Δ) = Δ, K_b(Δ) = Δ, K_d(Δ) = Δ, product = Δ³.

        let vope_a = make_vope(1, 0); // K_a(Δ) = Δ
        let vope_b = make_vope(1, 0); // K_b(Δ) = Δ
        let vope_d = make_vope(1, 0); // K_d(Δ) = Δ

        let delta_val = Galois128(3);
        let delta = make_delta(3);

        // Product Vope: K_abd(Δ) = Δ³
        let vope_abd = vole_mul3_prover_step(&vope_a, &vope_b, &vope_d);

        // Q shares for verifier (K_a(Δ) = 0 + 1*Δ = Δ)
        let q_a = make_q(delta_val.0);
        let q_b = make_q(delta_val.0);
        let q_d = make_q(delta_val.0);

        let (q_abd, ok) = vole_mul3_verifier_check(&delta, &q_a, &q_b, &q_d, vope_abd);
        assert!(ok, "honest degree-3 product check should accept");

        // Also verify Q_abd = Δ³
        let delta3 = delta_val * delta_val * delta_val;
        assert_eq!(q_abd.q[0], delta3);
    }

    #[test]
    fn vole_mul3_tampered_rejects() {
        let vope_a = make_vope(1, 0);
        let vope_b = make_vope(1, 0);
        let vope_d = make_vope(1, 0);

        let delta = make_delta(3);
        let delta_val = Galois128(3);

        let vope_abd = vole_mul3_prover_step(&vope_a, &vope_b, &vope_d);

        // Verifier has a wrong Q_d (bit-flipped)
        let q_a = make_q(delta_val.0);
        let q_b = make_q(delta_val.0);
        let q_d_bad = make_q(delta_val.0 ^ 1); // tampered

        let (_, ok) = vole_mul3_verifier_check(&delta, &q_a, &q_b, &q_d_bad, vope_abd);
        assert!(!ok, "tampered Q_d should cause the check to fail");
    }

    #[test]
    fn vole_mul3_with_nonzero_v_accepts() {
        // a = v_a + u_a*Δ, etc. with non-trivial v.
        // K_a(Δ) = 5 + 2*Δ, K_b(Δ) = 7 + 3*Δ, K_d(Δ) = 11 + 4*Δ
        let vope_a = make_vope(2, 5);
        let vope_b = make_vope(3, 7);
        let vope_d = make_vope(4, 11);

        let delta_val = Galois128(13);
        let delta = make_delta(13);

        let vope_abd = vole_mul3_prover_step(&vope_a, &vope_b, &vope_d);

        // Compute Q shares manually: Q_x = v_x + u_x*Δ
        let ka = Galois128(5) + Galois128(2) * delta_val;
        let kb = Galois128(7) + Galois128(3) * delta_val;
        let kd = Galois128(11) + Galois128(4) * delta_val;

        let q_a = make_q(ka.0);
        let q_b = make_q(kb.0);
        let q_d = make_q(kd.0);

        let (_, ok) = vole_mul3_verifier_check(&delta, &q_a, &q_b, &q_d, vope_abd);
        assert!(ok, "honest proof with non-trivial v should accept");
    }
}
