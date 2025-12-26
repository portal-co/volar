use super::*;
impl<N, T, K> Vope<N, T, K> 
where 
    N: VoleArray<T>, 
    T: Add<Output = T> + Mul<Output = T> + Default + Copy, // T is the extension field (e.g. GF(2^128))
    K: ArrayLength<GenericArray<T, N>>
{
    /// Multiplies this Vope with another, returning a Vope of degree K1 + K2.
    /// This represents the algebraic product in the context of Delta.
    pub fn mul_generalized<K2, KRes>(
        &self, 
        other: &Vope<N, T, K2>
    ) -> Vope<N, T, KRes>
    where
        K2: ArrayLength<GenericArray<T, N>>,
        KRes: ArrayLength<GenericArray<T, N>>, // KRes must be K + K2
    {
        // 1. Initialize result with zeros
        let mut res_u = GenericArray::<GenericArray<T, N>, KRes>::default();
        let mut res_v = GenericArray::<T, N>::default();



        macro_rules! get_coeff {
            ($v:expr, $u:expr, $idx:expr) => {
                if $idx == 0 {
                    $v
                } else {
                    &$u[$idx - 1]
                }
            };
        }

        // 2. Perform SIMD Convolution
        // Degree of self is K, degree of other is K2. Max index is K+K2.
        for i in 0..=(K::to_usize()) {
            for j in 0..=(K2::to_usize()) {
                let k = i + j;
                let a_coeff = get_coeff!(&self.v, &self.u, i);
                let b_coeff = get_coeff!(&other.v, &other.u, j);

                if k == 0 {
                    // Resulting offset (degree 0)
                    for lane in 0..N::to_usize() {
                        res_v[lane] = res_v[lane] + a_coeff[lane] * b_coeff[lane];
                    }
                } else {
                    // Resulting higher degree coefficients
                    for lane in 0..N::to_usize() {
                        res_u[k - 1][lane] = res_u[k - 1][lane] + a_coeff[lane] * b_coeff[lane];
                    }
                }
            }
        }

        Vope { u: res_u, v: res_v }
    }
}