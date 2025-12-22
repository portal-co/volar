use core::{
    mem::MaybeUninit,
    ops::{Add, BitXor, Mul, Sub},
};

use cipher::consts::{U1, U8};
use rand::distr::Distribution;

use super::*;
pub mod poly;
pub type ByteVole<T> = Vole<U1, T>;
pub trait VoleArray<T>: ArrayLength<T> + ArrayLength<MaybeUninit<T>> {}
impl<T, X: ArrayLength<T> + ArrayLength<MaybeUninit<T>>> VoleArray<T> for X {}
pub struct Vole<N: VoleArray<T>, T> {
    pub u: GenericArray<T, N>,
    pub v: GenericArray<T, N>,
}
impl<N: VoleArray<T> + VoleArray<U> + VoleArray<T::Output>, T: Add<U> + Clone, U: Clone>
    Add<Vole<N, U>> for Vole<N, T>
{
    type Output = Vole<N, T::Output>;
    fn add(self, rhs: Vole<N, U>) -> Self::Output {
        let mut result_v: GenericArray<MaybeUninit<T::Output>, N> =
            GenericArray::generate(|_| MaybeUninit::uninit());
        for i in 0..N::to_usize() {
            result_v[i] = MaybeUninit::new(self.v[i].clone() + rhs.v[i].clone());
        }
        Vole {
            u: self.u.zip(rhs.u, |a, b| a + b),
            v: result_v.map(|x| unsafe { x.assume_init() }),
        }
    }
}
impl<N: VoleArray<T> + VoleArray<T::Output> + VoleArray<U>, T: BitXor<U>, U>
    BitXor<GenericArray<U, N>> for Vole<N, T>
where
    T: Into<T::Output>,
{
    type Output = Vole<N, T::Output>;
    fn bitxor(self, rhs: GenericArray<U, N>) -> Self::Output {
        Vole {
            u: self.u.zip(rhs, |a, b| a ^ b),
            v: self.v.map(|a| a.into()),
        }
    }
}
impl<N: VoleArray<T>, T> Vole<N, T> {
    pub fn glue<'a, M: VoleArray<T>>(
        a: impl Iterator<Item = Vole<M, T>> + Clone + 'a,
    ) -> (Self, impl Iterator<Item = GenericArray<T, M>> + Clone + 'a)
    where
        T: Clone + Sub<T, Output = T> + 'a,
    {
        let first = a.clone().next().unwrap();
        let corrections = a
            .clone()
            .map(move |a2| a2.u.clone().zip(first.u.clone(), |a, b| a - b));
        (
            Self {
                u: GenericArray::from_iter(a.clone().flat_map(|a| a.u)),
                v: GenericArray::from_iter(a.clone().flat_map(|a| a.v)),
            },
            corrections,
        )
    }
    pub fn map<U>(self, mut f: impl FnMut(T) -> U) -> Vole<N, U>
    where
        N: VoleArray<U>,
    {
        let Self { u, v } = self;
        Vole {
            u: u.map(&mut f),
            v: v.map(&mut f),
        }
    }
    pub fn q<U: Default + Clone + Mul<T, Output = M>, M: Add<T, Output = O>, O>(
        self,
        q: GenericArray<U, N>,
    ) -> GenericArray<O, N>
    where
        T: Clone,
        N: ArrayLength<U> + ArrayLength<O>,
    {
        GenericArray::generate(|i| {
            let m: M = (q[i].clone() * self.u[i].clone());
            m + self.v[i].clone()
        })
    }
}
