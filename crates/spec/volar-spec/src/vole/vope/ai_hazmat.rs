// @reliability: hazmat
// @hazmat-reason: multiplying VOLE polynomials is only sound inside a
//   Quicksilver-style constraint check; using this outside that context
//   breaks the zero-knowledge property of the enclosing proof.
//! @ai: assisted
//! # Safety
//!
//! `mul_generalized` computes the degree-(K+K₂) product of two `Vope`s via
//! SIMD convolution over their coefficient arrays.
//!
//! **Correct usage:** Call this only when constructing a Quicksilver
//! multiplication check gate, where the product's consistency with the VOLE
//! correlation is immediately verified by the verifier. The product itself
//! is not a valid standalone VOLE commitment.
//!
//! **Incorrect usage:** Using the returned `Vope` as a wire value in a
//! circuit, or passing it to `* delta` without the surrounding check,
//! produces a value that is algebraically correct but cryptographically
//! unsound — the verifier cannot distinguish a cheating prover from an
//! honest one.
//!
//! // SAFETY(hazmat): all call sites in this crate invoke mul_generalized
//! // exclusively within a constraint-check context.
use super::*;
impl<N, T, K> Vope<N, T, K>
where
    N: VoleArray<T>,
    T: Add<Output = T> + Mul<Output = T> + Default + Clone, // T is the extension field (e.g. GF(2^128))
    K: ArraySize,
{
    /// Multiplies this Vope with another, returning a Vope of degree K1 + K2.
    /// This represents the algebraic product in the context of Delta.
    pub fn mul_generalized<K2: Add<K>>(&self, other: &Vope<N, T, K2>) -> Vope<N, T, K2::Output>
    where
        K2: ArraySize,
        K2::Output: ArraySize, // KRes must be K + K2
    {
        // 1. Initialize result with zeros
        let mut res_u = Array::<Array<T, N>, K2::Output>::default();
        let mut res_v = Array::<T, N>::default();

        // 2. Perform SIMD Convolution
        // Degree of self is K, degree of other is K2. Max index is K+K2.
        for i in 0..=(K::USIZE) {
            for j in 0..=(K2::USIZE) {
                let k = i + j;
                let a_coeff = if i == 0 { &self.v } else { &self.u[i - 1] };
                let b_coeff = if j == 0 { &other.v } else { &other.u[j - 1] };

                if k == 0 {
                    // Resulting offset (degree 0)
                    for lane in 0..N::USIZE {
                        res_v[lane] =
                            res_v[lane].clone() + a_coeff[lane].clone() * b_coeff[lane].clone();
                    }
                } else {
                    // Resulting higher degree coefficients
                    for lane in 0..N::USIZE {
                        res_u[k - 1][lane] = res_u[k - 1][lane].clone()
                            + a_coeff[lane].clone() * b_coeff[lane].clone();
                    }
                }
            }
        }

        Vope { u: res_u, v: res_v }
    }
}
