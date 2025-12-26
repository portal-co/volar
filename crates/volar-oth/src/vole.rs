use core::{
    mem::MaybeUninit,
    ops::{Add, BitXor, Mul, Sub},
};

use cipher::consts::{U1, U8};
use rand::distr::Distribution;

use super::*;
pub mod field_rotate;
mod impls;
pub mod poly;

pub trait VoleArray<T>: ArrayLength<T> {}
impl<T, X: ArrayLength<T>> VoleArray<T> for X {}

pub struct Delta<N: ArrayLength<T>, T> {
    pub delta: GenericArray<T, N>,
}
pub struct Q<N: ArrayLength<T>, T> {
    pub q: GenericArray<T, N>,
}
impl<N: ArrayLength<T>, T> Delta<N, T> {
    pub fn remap<M: ArrayLength<T>>(&self, mut f: impl FnMut(usize) -> usize) -> Delta<M, T>
    where
        T: Clone,
    {
        let Self { delta } = self;
        Delta {
            delta: GenericArray::generate(|i| delta[f(i) % N::to_usize()].clone()),
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
    pub fn r#static<U: Mul<T> + Clone>(&self, val: GenericArray<U, N>) -> Q<N, U::Output>
    where
        T: Clone,
        N: ArrayLength<U::Output> + ArrayLength<U>,
    {
        Q {
            q: GenericArray::generate(|i| val[i].clone() * self.delta[i].clone()),
        }
    }
}
impl<N: ArrayLength<T>, T> Q<N, T> {
    pub fn remap<M: ArrayLength<T>>(&self, mut f: impl FnMut(usize) -> usize) -> Q<M, T>
    where
        T: Clone,
    {
        let Self { q } = self;
        Q {
            q: GenericArray::generate(|i| q[f(i) % N::to_usize()].clone()),
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