use core::ops::Div;

use cipher::{Unsigned, consts::U2};

use super::*;

pub struct Poly<N: ArrayLength<T>, T> {
    pub c0: T,
    pub c1: GenericArray<T, N>,
}
pub struct PolyInputPool<'a, T, N: ArrayLength<usize>, X: ArrayLength<GenericArray<usize, N>>> {
    pub inputs: &'a [T],
    pub indices: GenericArray<GenericArray<usize, N>, X>,
}
impl<N: ArrayLength<T>, T> Poly<N, T> {
    pub fn get_qs_pool<
        Q: Clone + Mul<A, Output = A>,
        A: Add<A, Output = A>,
        M: ArrayLength<Q> + ArrayLength<A>,
        X: ArrayLength<GenericArray<usize, N>>,
    >(
        &self,
        root: Delta<M, Q>,
        inputs: PolyInputPool<super::Q<M, Q>, N, X>,
        reduction: usize,
    ) -> super::Q<M, A>
    where
        N: ArrayLength<usize>,
        T: Clone + Into<A>,
    {
        super::Q {
            q: GenericArray::generate(|i| {
                let mut sum: A = self.c0.clone().into();
                for _ in 0..N::to_usize() {
                    sum = root.delta[i].clone() * sum;
                }
                for j in 0..N::to_usize() {
                    let mut b: A = self.c1[j].clone().into();
                    for i2 in inputs.indices.iter() {
                        for _ in 0..reduction {
                            b = inputs.inputs[i2[j]].q[i].clone() * b;
                        }
                    }
                    sum = sum + b;
                }
                sum
            }),
        }
    }
    pub fn get_qs<
        Q: Clone + Mul<A, Output = A>,
        A: Add<A, Output = A>,
        M: ArrayLength<Q> + ArrayLength<A>,
        X: ArrayLength<GenericArray<super::Q<M, Q>, N>>,
    >(
        &self,
        root: Delta<M, Q>,
        inputs: GenericArray<GenericArray<super::Q<M, Q>, N>, X>,
        reduction: usize,
    ) -> super::Q<M, A>
    where
        N: ArrayLength<super::Q<M, Q>>,
        T: Clone + Into<A>,
    {
        super::Q {
            q: GenericArray::generate(|i| {
                let mut sum: A = self.c0.clone().into();
                for _ in 0..N::to_usize() {
                    sum = root.delta[i].clone() * sum;
                }
                for j in 0..N::to_usize() {
                    let mut b: A = self.c1[j].clone().into();
                    for i2 in inputs.iter() {
                        for _ in 0..reduction {
                            b = i2[j].q[i].clone() * b;
                        }
                    }
                    sum = sum + b;
                }
                sum
            }),
        }
    }
    pub fn apply_pool<
        M,
        O: Mul<O, Output = O> + Add<O, Output = O> + Default + Clone,
        X: ArrayLength<GenericArray<usize, N>>,
        X2: ArrayLength<GenericArray<T, M>> + Div<S, Output = XS>,
        XS: ArrayLength<GenericArray<O, M>>,
        S: Unsigned,
    >(
        &self,
        voles: &PolyInputPool<Vope<M, T, X2>, N, X>,
    ) -> Vope<M, O, XS>
    where
        N: ArrayLength<usize>,
        T: Into<O> + Clone,
        M: VoleArray<T> + VoleArray<O>,
    {
        let v = GenericArray::generate(|i| {
            // core::array::from_fn(|j| {
            let mut sum = O::default();
            for k in 0..N::to_usize() {
                let mut b: O = self.c1[k].clone().into();
                for v in &voles.indices {
                    b = b * voles.inputs[v[k]].v[i].clone().into();
                }
                sum = sum + b;
            }
            let c0: O = self.c0.clone().into();
            sum + c0
            // })
        });
        let u = GenericArray::generate(|l| {
            GenericArray::generate(|i| {
                // core::array::from_fn(|j| {
                let mut sum = O::default();
                for k in 0..N::to_usize() {
                    for n in 0..X::to_usize() {
                        let mut b: O = self.c1[k].clone().into();
                        for m in 0..S::to_usize() {
                            let l = l * S::to_usize() + m;
                            for (idx, v) in voles.indices.iter().enumerate() {
                                b = b * if idx == n {
                                    voles.inputs[v[k]].u[l][i].clone().into()
                                } else {
                                    voles.inputs[v[k]].v[i].clone().into()
                                };
                            }
                        }
                        sum = sum + b;
                    }
                }
                sum
                // })
            })
        });
        return Vope { u, v };
    }
    pub fn apply<
        M,
        O: Mul<O, Output = O> + Add<O, Output = O> + Default + Clone,
        X: ArrayLength<GenericArray<Vope<M, T, X2>, N>>,
        X2: ArrayLength<GenericArray<T, M>> + Div<S, Output = XS>,
        XS: ArrayLength<GenericArray<O, M>>,
        S: Unsigned,
    >(
        &self,
        voles: GenericArray<GenericArray<Vope<M, T, X2>, N>, X>,
    ) -> Vope<M, O, XS>
    where
        N: ArrayLength<Vope<M, T, X2>>,
        T: Into<O> + Clone,
        M: VoleArray<T> + VoleArray<O>,
    {
        let v = GenericArray::generate(|i| {
            // core::array::from_fn(|j| {
            let mut sum = O::default();
            for k in 0..N::to_usize() {
                let mut b: O = self.c1[k].clone().into();
                for v in &voles {
                    b = b * v[k].v[i].clone().into();
                }
                sum = sum + b;
            }
            let c0: O = self.c0.clone().into();
            sum + c0
            // })
        });
        let u = GenericArray::generate(|l| {
            GenericArray::generate(|i| {
                // core::array::from_fn(|j| {
                let mut sum = O::default();
                for k in 0..N::to_usize() {
                    for n in 0..X::to_usize() {
                        let mut b: O = self.c1[k].clone().into();
                        for m in 0..S::to_usize() {
                            let l = l * S::to_usize() + m;
                            for (idx, v) in voles.iter().enumerate() {
                                b = b * if idx == n {
                                    v[k].u[l][i].clone().into()
                                } else {
                                    v[k].v[i].clone().into()
                                };
                            }
                        }
                        sum = sum + b;
                    }
                }
                sum
                // })
            })
        });
        return Vope { u, v };
    }
}
