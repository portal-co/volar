//! Dynamic equivalents for `volar-spec/src/vole.rs` and `volar-spec/src/vole/vope.rs`.
use super::*;

use alloc::vec::Vec;
use core::ops::{Add, Mul};

use cipher::consts::{U1, U8};



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
// `VopeDyn` implementation is moved to `spec_dyn/vole/vope.rs` to mirror the
// module layout in `volar-spec`.
pub mod vope;
pub mod field_rotate;
pub mod impls;
pub mod poly;
pub use vope::VopeDyn;
