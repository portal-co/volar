// @reliability: normal
//! @ai: assisted
// Core VOLE types: Delta (verifier global secret), Q (verifier share),
// VoleArray trait, and rotation/remap operations.
// Based on the standard VOLE relation: u·Δ + v = q over binary extension fields.
use core::ops::{Add, BitXor, Mul};

use cipher::consts::U1;

use super::*;
pub mod field_rotate;
mod impls;
pub mod memory;
pub mod poly;
pub mod setup;

pub trait VoleArray<T>: ArraySize {}
impl<T, X: ArraySize> VoleArray<T> for X {}

/// Debug-mode guard for the pool-based regalloc design (cross-function
/// value pooling): panics if a shared pool slot is read before it's
/// been written -- a no-op in release builds (`cfg!(debug_assertions)`
/// is a compile-time constant, so the release build doesn't even carry
/// the branch). Woven code calls this once per pooled read, immediately
/// before the actual read, wrapped around it in a block expression --
/// see `vole.rs`'s own `WireRepr::Pooled` doc for the full design.
#[inline(always)]
pub fn debug_check_pool_written(written: bool, slot: usize) {
    if cfg!(debug_assertions) && !written {
        panic!("read from unwritten pool slot [{slot}]");
    }
}

pub struct Delta<N: ArraySize, T> {
    pub delta: Array<T, N>,
}
pub struct Q<N: ArraySize, T> {
    pub q: Array<T, N>,
}
impl<N: ArraySize, T: Default> Default for Q<N, T> {
    fn default() -> Self {
        Q { q: Default::default() }
    }
}
impl<N: ArraySize, T> Delta<N, T> {
    pub fn remap<M: ArraySize, F: FnMut(usize) -> usize>(&self, mut f: F) -> Delta<M, T>
    where
        T: Clone,
    {
        let Self { delta } = self;
        Delta {
            delta: Array::<T, M>::from_fn(|i| delta[f(i) % N::USIZE].clone()),
        }
    }
    pub fn rotate_left(&self, n: usize) -> Self
    where
        T: Clone,
    {
        self.remap(|a| a.wrapping_sub(n))
    }
    pub fn rotate_right(&self, n: usize) -> Self
    where
        T: Clone,
    {
        self.remap(|a| a.wrapping_add(n))
    }
    pub fn r#static<U: Mul<T, Output = O> + Clone, O>(&self, val: Array<U, N>) -> Q<N, O>
    where
        T: Clone,
    {
        Q {
            q: Array::<O, N>::from_fn(|i| val[i].clone() * self.delta[i].clone()),
        }
    }
}
impl<N: ArraySize, T> Q<N, T> {
    pub fn remap<M: ArraySize, F: FnMut(usize) -> usize>(&self, mut f: F) -> Q<M, T>
    where
        T: Clone,
    {
        let Self { q } = self;
        Q {
            q: Array::<T, M>::from_fn(|i| q[f(i) % N::USIZE].clone()),
        }
    }
    pub fn rotate_left(&self, n: usize) -> Self
    where
        T: Clone,
    {
        self.remap(|a| a.wrapping_sub(n))
    }
    pub fn rotate_right(&self, n: usize) -> Self
    where
        T: Clone,
    {
        self.remap(|a| a.wrapping_add(n))
    }
}
pub mod vope;
pub use vope::Vope;
pub mod garble;
pub mod prove;
pub mod bridge;
