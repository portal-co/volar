use super::*;

impl<T> VopeDyn<T>
where
    T: Add<Output = T> + Mul<Output = T> + Default + Clone,
{
    /// Dynamic equivalent of `mul_generalized` from the spec: convolve two VopeDyn
    /// values producing degree K1 + K2.
    pub fn mul_generalized_dyn(&self, other: &VopeDyn<T>) -> VopeDyn<T> {
        // let degree K = self.u.len(), K2 = other.u.len();
        let k1 = self.u.len();
        let k2 = other.u.len();
        let n = self.v.len();
        // result degree up to k1 + k2
        let res_k = k1 + k2;

        // initialize res_u as res_k x n of defaults
        let mut res_u: Vec<Vec<T>> = Vec::with_capacity(res_k);
        for _ in 0..res_k {
            res_u.push(vec![T::default(); n]);
        }
        let mut res_v: Vec<T> = vec![T::default(); n];

        // helper: index 0 maps to `v`, otherwise to `u[index-1]` — handled inline below.

        // perform convolution-like loops
        for i in 0..=k1 {
            for j in 0..=k2 {
                let k = i + j;
                // a_coeff: if i==0 -> self.v else self.u[i-1]
                // b_coeff similarly
                if k == 0 {
                    // degree 0: accumulate into res_v
                    if i == 0 && j == 0 {
                        for lane in 0..n {
                            res_v[lane] = res_v[lane].clone()
                                + self.v[lane].clone() * other.v[lane].clone();
                        }
                    } else if i == 0 {
                        let b_coeff = &other.u[j - 1];
                        for lane in 0..n {
                            res_v[lane] = res_v[lane].clone()
                                + self.v[lane].clone() * b_coeff[lane].clone();
                        }
                    } else if j == 0 {
                        let a_coeff = &self.u[i - 1];
                        for lane in 0..n {
                            res_v[lane] = res_v[lane].clone()
                                + a_coeff[lane].clone() * other.v[lane].clone();
                        }
                    }
                } else {
                    // higher degree coefficient goes into res_u[k-1]
                    if i == 0 {
                        let b_coeff = &other.u[j - 1];
                        for lane in 0..n {
                            res_u[k - 1][lane] = res_u[k - 1][lane].clone()
                                + self.v[lane].clone() * b_coeff[lane].clone();
                        }
                    } else if j == 0 {
                        let a_coeff = &self.u[i - 1];
                        for lane in 0..n {
                            res_u[k - 1][lane] = res_u[k - 1][lane].clone()
                                + a_coeff[lane].clone() * other.v[lane].clone();
                        }
                    } else {
                        let a_coeff = &self.u[i - 1];
                        let b_coeff = &other.u[j - 1];
                        for lane in 0..n {
                            res_u[k - 1][lane] = res_u[k - 1][lane].clone()
                                + a_coeff[lane].clone() * b_coeff[lane].clone();
                        }
                    }
                }
            }
        }

        VopeDyn { u: res_u, v: res_v }
    }
}
