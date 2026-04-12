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
use cipher::consts::U1;

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
