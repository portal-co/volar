use super::*;
use alloc::vec::Vec;
use core::ops::{Add, Mul, Div};

pub struct PolyDyn<T> {
    pub c0: T,
    pub c1: Vec<T>,
}

pub struct PolyInputPool<'a, T> {
    pub inputs: &'a [VopeDyn<T>],
    pub indices: Vec<Vec<usize>>,
}

impl<T: Clone + Default> PolyDyn<T> {
    pub fn get_qs_pool<Q, A>(
        &self,
        root: DeltaDyn<Q>,
        inputs: PolyInputPool<'_, Q>,
        reduction: usize,
    ) -> QDyn<A>
    where
        Q: Clone + Mul<A, Output = A>,
        A: Add<A, Output = A> + Default + Clone,
        T: Into<A> + Clone,
    {
        let n = root.delta.len();
        let mut out = Vec::with_capacity(n);
        for i in 0..n {
            let mut sum: A = self.c0.clone().into();
            for _ in 0..self.c1.len() {
                sum = root.delta[i].clone() * sum.clone();
            }
            for j in 0..self.c1.len() {
                let mut b: A = self.c1[j].clone().into();
                for idxs in &inputs.indices {
                    for _ in 0..reduction {
                        b = inputs.inputs[idxs[j]].v[i].clone().into() * b;
                    }
                }
                sum = sum + b;
            }
            out.push(sum);
        }
        QDyn { q: out }
    }
    pub fn get_qs<Q, A>(
        &self,
        root: DeltaDyn<Q>,
        inputs: &[VopeDyn<Q>],
        reduction: usize,
    ) -> QDyn<A>
    where
        Q: Clone + Mul<A, Output = A>,
        A: Add<A, Output = A> + Default + Clone,
        T: Into<A> + Clone,
    {
        let n = root.delta.len();
        let mut out = Vec::with_capacity(n);
        for i in 0..n {
            let mut sum: A = self.c0.clone().into();
            for _ in 0..self.c1.len() {
                sum = root.delta[i].clone() * sum.clone();
            }
            for j in 0..self.c1.len() {
                let mut b: A = self.c1[j].clone().into();
                for v in inputs.iter() {
                    for _ in 0..reduction {
                        b = v.u[j].iter().map(|row| row[i].clone()).next().unwrap().clone().into() * b;
                    }
                }
                sum = sum + b;
            }
            out.push(sum);
        }
        QDyn { q: out }
    }
    pub fn apply_pool<M, O>(
        &self,
        voles: &PolyInputPool<'_, VopeDyn<T>>,
        s: usize,
    ) -> VopeDyn<O>
    where
        O: Mul<O, Output = O> + Add<O, Output = O> + Default + Clone,
        T: Into<O> + Clone,
    {
        let n = voles.inputs[0].v.len();
        let mut v = Vec::with_capacity(n);
        for i in 0..n {
            let mut sum: O = O::default();
            for k in 0..self.c1.len() {
                let mut b: O = self.c1[k].clone().into();
                for v_idx in &voles.indices {
                    b = b * voles.inputs[v_idx[k]].v[i].clone().into();
                }
                sum = sum + b;
            }
            let c0: O = self.c0.clone().into();
            v.push(sum + c0);
        }
        let mut u: Vec<Vec<O>> = Vec::new();
        for l in 0..(voles.inputs[0].u.len() * s) {
            let mut row = Vec::with_capacity(n);
            for i in 0..n {
                let mut sum: O = O::default();
                for k in 0..self.c1.len() {
                    for n_idx in 0..voles.indices.len() {
                        let mut b: O = self.c1[k].clone().into();
                        for m in 0..s {
                            let l_idx = l * s + m;
                            for (idx, v) in voles.indices.iter().enumerate() {
                                let val = if idx == n_idx {
                                    voles.inputs[v[k]].u[l_idx][i].clone().into()
                                } else {
                                    voles.inputs[v[k]].v[i].clone().into()
                                };
                                b = b * val;
                            }
                        }
                        sum = sum + b;
                    }
                }
                row.push(sum);
            }
            u.push(row);
        }
        VopeDyn { u, v }
    }
    pub fn apply<M, O>(
        &self,
        voles: &[VopeDyn<T>],
        s: usize,
    ) -> VopeDyn<O>
    where
        O: Mul<O, Output = O> + Add<O, Output = O> + Default + Clone,
        T: Into<O> + Clone,
    {
        let n = voles[0].v.len();
        let mut v = Vec::with_capacity(n);
        for i in 0..n {
            let mut sum: O = O::default();
            for k in 0..self.c1.len() {
                let mut b: O = self.c1[k].clone().into();
                for vol in voles.iter() {
                    b = b * vol.v[i].clone().into();
                }
                sum = sum + b;
            }
            let c0: O = self.c0.clone().into();
            v.push(sum + c0);
        }
        let mut u: Vec<Vec<O>> = Vec::new();
        for l in 0..(voles[0].u.len() * s) {
            let mut row = Vec::with_capacity(n);
            for i in 0..n {
                let mut sum: O = O::default();
                for k in 0..self.c1.len() {
                    for n_idx in 0..voles.len() {
                        let mut b: O = self.c1[k].clone().into();
                        for m in 0..s {
                            let l_idx = l * s + m;
                            for (idx, v) in voles.iter().enumerate() {
                                let val = if idx == n_idx {
                                    v.u[l_idx][i].clone().into()
                                } else {
                                    v.v[i].clone().into()
                                };
                                b = b * val;
                            }
                        }
                        sum = sum + b;
                    }
                }
                row.push(sum);
            }
            u.push(row);
        }
        VopeDyn { u, v }
    }
}
