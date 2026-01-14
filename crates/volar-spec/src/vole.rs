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
    pub fn remap<M: ArrayLength<T>, F: FnMut(usize) -> usize>(&self, mut f: F) -> Delta<M, T>
    where
        T: Clone,
    {
        let Self { delta } = self;
        Delta {
            delta: GenericArray::<T, M>::generate(|i| delta[f(i) % N::to_usize()].clone()),
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
    pub fn r#static<U: Mul<T, Output = O>, O>(&self, val: GenericArray<U, N>) -> Q<N, O>
    where
        T: Clone,
        N: ArrayLength<O> + ArrayLength<U>,
    {
        Q {
            q: GenericArray::<O, N>::generate(|i| val[i].clone() * self.delta[i].clone()),
        }
    }
}
impl<N: ArrayLength<T>, T> Q<N, T> {
    pub fn remap<M: ArrayLength<T>, F: FnMut(usize) -> usize>(&self, mut f: F) -> Q<M, T>
    where
        T: Clone,
    {
        let Self { q } = self;
        Q {
            q: GenericArray::<T, M>::generate(|i| q[f(i) % N::to_usize()].clone()),
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
