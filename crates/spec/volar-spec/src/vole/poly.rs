use core::ops::Div;

use cipher::typenum::Unsigned;

use super::*;

pub struct Poly<N: ArraySize, T> {
    pub c0: T,
    pub c1: Array<T, N>,
}
pub struct PolyInputPool<'a, T, N: ArraySize, X: ArraySize> {
    pub inputs: &'a [T],
    pub indices: Array<Array<usize, N>, X>,
}
impl<N: ArraySize, T> Poly<N, T> {
    pub fn get_qs_pool<
        Q: Clone + Mul<A, Output = A>,
        A: Add<A, Output = A>,
        M: ArraySize,
        X: ArraySize,
    >(
        &self,
        root: Delta<M, Q>,
        inputs: PolyInputPool<super::Q<M, Q>, N, X>,
        reduction: usize,
    ) -> super::Q<M, A>
    where
        T: Clone + Into<A>,
    {
        super::Q {
            q: Array::<A, M>::from_fn(|i| {
                let mut sum: A = self.c0.clone().into();
                for _ in 0..N::USIZE {
                    sum = root.delta[i].clone() * sum;
                }
                for j in 0..N::USIZE {
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
        M: ArraySize,
        X: ArraySize,
    >(
        &self,
        root: Delta<M, Q>,
        inputs: Array<Array<super::Q<M, Q>, N>, X>,
        reduction: usize,
    ) -> super::Q<M, A>
    where
        T: Clone + Into<A>,
    {
        super::Q {
            q: Array::<A, M>::from_fn(|i| {
                let mut sum: A = self.c0.clone().into();
                for _ in 0..N::USIZE {
                    sum = root.delta[i].clone() * sum;
                }
                for j in 0..N::USIZE {
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
        X: ArraySize,
        X2: ArraySize + Div<S, Output = XS>,
        XS: ArraySize,
        S: Unsigned,
    >(
        &self,
        voles: &PolyInputPool<Vope<M, T, X2>, N, X>,
    ) -> Vope<M, O, XS>
    where
        T: Into<O> + Clone,
        M: VoleArray<T> + VoleArray<O>,
    {
        let v = Array::<O, M>::from_fn(|i| {
            let mut sum = O::default();
            for k in 0..N::USIZE {
                let mut b: O = self.c1[k].clone().into();
                for v in &voles.indices {
                    b = b * voles.inputs[v[k]].v[i].clone().into();
                }
                sum = sum + b;
            }
            let c0: O = self.c0.clone().into();
            sum + c0
        });
        let u = Array::<Array<O, M>, XS>::from_fn(|l| {
            Array::<O, M>::from_fn(|i| {
                let mut sum = O::default();
                for k in 0..N::USIZE {
                    for n in 0..X::USIZE {
                        let mut b: O = self.c1[k].clone().into();
                        for m in 0..S::USIZE {
                            let l = l * S::USIZE + m;
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
            })
        });
        return Vope { u, v };
    }
    pub fn apply<
        M,
        O: Mul<O, Output = O> + Add<O, Output = O> + Default + Clone,
        X: ArraySize,
        X2: ArraySize + Div<S, Output = XS>,
        XS: ArraySize,
        S: Unsigned,
    >(
        &self,
        voles: Array<Array<Vope<M, T, X2>, N>, X>,
    ) -> Vope<M, O, XS>
    where
        T: Into<O> + Clone,
        M: VoleArray<T> + VoleArray<O>,
    {
        let v = Array::<O, M>::from_fn(|i| {
            let mut sum = O::default();
            for k in 0..N::USIZE {
                let mut b: O = self.c1[k].clone().into();
                for v in &voles {
                    b = b * v[k].v[i].clone().into();
                }
                sum = sum + b;
            }
            let c0: O = self.c0.clone().into();
            sum + c0
        });
        let u = Array::<Array<O, M>, XS>::from_fn(|l| {
            Array::<O, M>::from_fn(|i| {
                let mut sum = O::default();
                for k in 0..N::USIZE {
                    for n in 0..X::USIZE {
                        let mut b: O = self.c1[k].clone().into();
                        for m in 0..S::USIZE {
                            let l = l * S::USIZE + m;
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
            })
        });
        return Vope { u, v };
    }
}
