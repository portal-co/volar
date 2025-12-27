//! Dynamic equivalents for `volar-spec/src/vole.rs` and `volar-spec/src/vole/vope.rs`.
use super::*;

use alloc::vec::Vec;
use core::ops::{Add, Mul};

/// Dynamic Delta equivalent of `Delta<N, T>` (from `volar-spec/src/vole.rs`).
/// Original: `pub struct Delta<N: ArrayLength<T>, T> { pub delta: GenericArray<T, N> }`
pub struct DeltaDyn<T> {
    pub delta: Vec<T>,
}

/// Dynamic Q equivalent of `Q<N, T>` (from `volar-spec/src/vole.rs`).
/// Original: `pub struct Q<N: ArrayLength<T>, T> { pub q: GenericArray<T, N> }`
pub struct QDyn<T> {
    pub q: Vec<T>,
}

impl<T: Clone> DeltaDyn<T> {
    pub fn remap(&self, mut f: impl FnMut(usize) -> usize) -> DeltaDyn<T> {
        let n = self.delta.len();
        let mut out = Vec::with_capacity(n);
        for i in 0..n {
            let idx = f(i) % n;
            out.push(self.delta[idx].clone());
        }
        DeltaDyn { delta: out }
    }
    pub fn rotate_left(&self, n: usize) -> Self {
        self.remap(|a| a.wrapping_sub(n))
    }
    pub fn rotate_right(&self, n: usize) -> Self {
        self.remap(|a| a.wrapping_add(n))
    }
    /// Elementwise multiply `val` and `self.delta`, returning a `QDyn`.
    /// Mirrors `Delta::static` from the spec but works with `Vec`.
    pub fn r#static<U, Out>(&self, val: &[U]) -> QDyn<Out>
    where
        T: Clone,
        U: Clone + Mul<T, Output = Out>,
        Out: Clone,
    {
        let n = self.delta.len();
        assert_eq!(val.len(), n);
        let mut q = Vec::with_capacity(n);
        for i in 0..n {
            q.push(val[i].clone() * self.delta[i].clone());
        }
        QDyn { q }
    }
}

impl<T: Clone> QDyn<T> {
    pub fn remap(&self, mut f: impl FnMut(usize) -> usize) -> QDyn<T> {
        let n = self.q.len();
        let mut out = Vec::with_capacity(n);
        for i in 0..n {
            let idx = f(i) % n;
            out.push(self.q[idx].clone());
        }
        QDyn { q: out }
    }
    pub fn rotate_left(&self, n: usize) -> Self {
        self.remap(|a| a.wrapping_sub(n))
    }
    pub fn rotate_right(&self, n: usize) -> Self {
        self.remap(|a| a.wrapping_add(n))
    }
}

/// Dynamic Vope equivalent.
/// Original: `pub struct Vope<N: VoleArray<T>, T, K: ArrayLength<GenericArray<T, N>> = U1> { u, v }`
pub struct VopeDyn<T> {
    /// `u` is K x N (represented as Vec of Vec). In the common case K==N this
    /// mirrors the spec's `GenericArray<GenericArray<T, N>, K>`.
    pub u: Vec<Vec<T>>,
    /// `v` is length N
    pub v: Vec<T>,
}

impl<T: Clone> VopeDyn<T> {
    /// Create a constant-style Vope where `u` rows are left empty and `v` is provided.
    /// Mirrors `impl Vope::constant` where `u` is generated with unreachable().
    pub fn constant(v: Vec<T>, k: usize) -> Self  where T: Default{
        let n = v.len();
        let mut u = Vec::with_capacity(k);
        for _ in 0..k {
            u.push(vec![T::default(); n]);
        }
        VopeDyn { u, v }
    }
    /// Remap indices of inner arrays similarly to `Vope::remap`.
    pub fn remap(&self, mut f: impl FnMut(usize) -> usize) -> VopeDyn<T> {
        let n = self.v.len();
        let k = self.u.len();
        let mut u_out = Vec::with_capacity(k);
        for l in 0..k {
            let mut row = Vec::with_capacity(n);
            for i in 0..n {
                row.push(self.u[l][f(i) % n].clone());
            }
            u_out.push(row);
        }
        let mut v_out = Vec::with_capacity(n);
        for i in 0..n {
            v_out.push(self.v[f(i) % n].clone());
        }
        VopeDyn { u: u_out, v: v_out }
    }
    pub fn rotate_left(&self, n: usize) -> Self {
        self.remap(|a| a.wrapping_sub(n))
    }
    pub fn rotate_right(&self, n: usize) -> Self {
        self.remap(|a| a.wrapping_add(n))
    }
    /// Expand K to L by selecting u rows repeatedly (mirrors `expand`).
    pub fn expand(&self, new_k: usize) -> VopeDyn<T> {
        let n = self.v.len();
        let mut u_out = Vec::with_capacity(new_k);
        for l in 0..new_k {
            let src = &self.u[l % self.u.len()];
            let mut row = Vec::with_capacity(n);
            for i in 0..n {
                row.push(src[i].clone());
            }
            u_out.push(row);
        }
        VopeDyn { u: u_out, v: self.v.clone() }
    }
}
