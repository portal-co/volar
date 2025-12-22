use super::*;
pub struct Poly<N: ArrayLength<T>, T> {
    pub c0: T,
    pub c1: GenericArray<T, N>,
}
impl<N: ArrayLength<T>, T> Poly<N, T> {
    pub fn apply<M, O: Mul<O, Output = O> + Add<O, Output = O> + Default + Clone>(
        &self,
        voles: [GenericArray<Vole<M, T>, N>; 2],
    ) -> Vole<M, O>
    where
        N: ArrayLength<Vole<M, T>>,
        T: Into<O> + Clone,
        M: VoleArray<T> + VoleArray<O>,
    {
        let v = GenericArray::generate(|i| {
            // core::array::from_fn(|j| {
            let mut sum = O::default();
            for k in 0..N::to_usize() {
                let a: O = voles[0][k].v[i].clone().into();
                let b: O = self.c1[k].clone().into();
                let c: O = voles[1][k].v[i].clone().into();
                sum = sum + a * b * c;
            }
            let c0: O = self.c0.clone().into();
            sum + c0
            // })
        });
        let u = GenericArray::generate(|i| {
            // core::array::from_fn(|j| {
            let mut sum = O::default();
            for k in 0..N::to_usize() {
                for (ai, bi) in [(0, 1), (1, 0)] {
                    let a: O = voles[ai][k].v[i].clone().into();
                    let b: O = self.c1[k].clone().into();
                    let c: O = voles[bi][k].u[i].clone().into();
                    sum = sum + a * b * c;
                }
            }
            sum
            // })
        });
        return Vole { u, v };
    }
}
