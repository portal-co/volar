use super::*;
use alloc::vec::Vec;
use core::ops::{Add, Mul};

pub struct PolyDyn<T> {
    pub c0: T,
    pub c1: Vec<T>,
}

pub struct PolyInputPool<'a, T> {
    pub inputs: &'a [VopeDyn<T>],
    pub indices: Vec<Vec<usize>>,
}

impl<T> PolyDyn<T>
where
    T: Clone + Default + Add<T, Output = T> + Mul<T, Output = T>,
{
    pub fn get_qs_pool(&self, root: DeltaDyn<T>, inputs: PolyInputPool<'_, T>, reduction: usize) -> QDyn<T> {
        let n = root.delta.len();
        let mut out = Vec::with_capacity(n);
        for i in 0..n {
            let mut sum: T = self.c0.clone();
            for _ in 0..self.c1.len() {
                sum = root.delta[i].clone() * sum.clone();
            }
            for j in 0..self.c1.len() {
                let mut b: T = self.c1[j].clone();
                for idxs in &inputs.indices {
                    for _ in 0..reduction {
                        b = inputs.inputs[idxs[j]].v[i].clone() * b.clone();
                    }
                }
                sum = sum + b;
            }
            out.push(sum);
        }
        QDyn { q: out }
    }

    pub fn get_qs(&self, root: DeltaDyn<T>, inputs: &[VopeDyn<T>], reduction: usize) -> QDyn<T> {
        let n = root.delta.len();
        let mut out = Vec::with_capacity(n);
        for i in 0..n {
            let mut sum: T = self.c0.clone();
            for _ in 0..self.c1.len() {
                sum = root.delta[i].clone() * sum.clone();
            }
            for j in 0..self.c1.len() {
                let mut b: T = self.c1[j].clone();
                for v in inputs.iter() {
                    for _ in 0..reduction {
                        b = v.u[j][i].clone() * b.clone();
                    }
                }
                sum = sum + b;
            }
            out.push(sum);
        }
        QDyn { q: out }
    }

    pub fn apply_pool(&self, voles: &PolyInputPool<'_, T>, s: usize) -> VopeDyn<T> {
        let n = voles.inputs[0].v.len();
        let mut v = Vec::with_capacity(n);
        for i in 0..n {
            let mut sum: T = T::default();
            for k in 0..self.c1.len() {
                let mut b: T = self.c1[k].clone();
                for v_idx in &voles.indices {
                    b = b * voles.inputs[v_idx[k]].v[i].clone();
                }
                sum = sum + b;
            }
            let c0: T = self.c0.clone();
            v.push(sum + c0);
        }
        let mut u: Vec<Vec<T>> = Vec::new();
        for l in 0..(voles.inputs[0].u.len() * s) {
            let mut row = Vec::with_capacity(n);
            for i in 0..n {
                let mut sum: T = T::default();
                for k in 0..self.c1.len() {
                    for n_idx in 0..voles.indices.len() {
                        let mut b: T = self.c1[k].clone();
                        for m in 0..s {
                            let l_idx = l * s + m;
                            for (idx, v) in voles.indices.iter().enumerate() {
                                let val = if idx == n_idx {
                                    voles.inputs[v[k]].u[l_idx][i].clone()
                                } else {
                                    voles.inputs[v[k]].v[i].clone()
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

    pub fn apply(&self, voles: &[VopeDyn<T>], s: usize) -> VopeDyn<T> {
        let n = voles[0].v.len();
        let mut v = Vec::with_capacity(n);
        for i in 0..n {
            let mut sum: T = T::default();
            for k in 0..self.c1.len() {
                let mut b: T = self.c1[k].clone();
                for vol in voles.iter() {
                    b = b * vol.v[i].clone();
                }
                sum = sum + b;
            }
            let c0: T = self.c0.clone();
            v.push(sum + c0);
        }
        let mut u: Vec<Vec<T>> = Vec::new();
        for l in 0..(voles[0].u.len() * s) {
            let mut row = Vec::with_capacity(n);
            for i in 0..n {
                let mut sum: T = T::default();
                for k in 0..self.c1.len() {
                    for n_idx in 0..voles.len() {
                        let mut b: T = self.c1[k].clone();
                        for m in 0..s {
                            let l_idx = l * s + m;
                            for (idx, v) in voles.iter().enumerate() {
                                let val = if idx == n_idx {
                                    v.u[l_idx][i].clone()
                                } else {
                                    v.v[i].clone()
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
