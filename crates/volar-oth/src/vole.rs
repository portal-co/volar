use core::{
    mem::MaybeUninit,
    ops::{Add, Mul},
};

use cipher::consts::{U1, U8};
use rand::distr::Distribution;

use super::*;
pub type ByteVole<T> = Vole<U1, T>;
pub trait VoleArray<T>:
    ArrayLength<[T; 8]> + ArrayLength<MaybeUninit<[T; 8]>> + ArrayLength<u8>
{
}
impl<T, X: ArrayLength<[T; 8]> + ArrayLength<MaybeUninit<[T; 8]>> + ArrayLength<u8>> VoleArray<T>
    for X
{
}
pub struct Vole<N: VoleArray<T>, T> {
    pub u: GenericArray<u8, N>,
    pub v: GenericArray<[T; 8], N>,
}
impl<N: VoleArray<T> + VoleArray<U> + VoleArray<T::Output>, T: Add<U> + Clone, U: Clone>
    Add<Vole<N, U>> for Vole<N, T>
{
    type Output = Vole<N, T::Output>;
    fn add(self, rhs: Vole<N, U>) -> Self::Output {
        let mut result_v: GenericArray<MaybeUninit<[T::Output; 8]>, N> =
            GenericArray::generate(|_| MaybeUninit::uninit());
        for i in 0..N::to_usize() {
            result_v[i] = MaybeUninit::new(core::array::from_fn(|j| {
                self.v[i][j].clone() + rhs.v[i][j].clone()
            }));
        }
        Vole {
            u: self.u.zip(rhs.u, |a, b| a ^ b),
            v: result_v.map(|x| unsafe { x.assume_init() }),
        }
    }
}
impl<N: VoleArray<T>, T> Vole<N, T> {
    pub fn glue<'a, M: VoleArray<T>>(
        a: impl Iterator<Item = Vole<M, T>> + Clone + 'a,
    ) -> (Self, impl Iterator<Item = GenericArray<u8, M>> + Clone + 'a)
    where
        T: Clone,
    {
        let first = a.clone().next().unwrap();
        let corrections = a
            .clone()
            .map(move |a2| a2.u.clone().zip(first.u.clone(), |a, b| a.wrapping_sub(b)));
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
            u,
            v: v.map(|a| a.map(&mut f)),
        }
    }
    pub fn q<U: Default + Clone + Add<T>>(
        self,
        q: GenericArray<[U; 8], N>,
    ) -> GenericArray<[U::Output; 8], N>
    where
        T: Clone,
        N: ArrayLength<[U; 8]>
            + ArrayLength<MaybeUninit<[U::Output; 8]>>
            + ArrayLength<[U::Output; 8]>,
    {
        let mut result: GenericArray<MaybeUninit<[U::Output; 8]>, N> =
            GenericArray::generate(|_| MaybeUninit::uninit());
        for i in 0..N::to_usize() {
            result[i] = MaybeUninit::new(core::array::from_fn(|j| if self.u[i] << j & 1 != 0{
                q[i][j].clone()
            }else{
                U::default()
            } + self.v[i][j].clone()))
        }
        result.map(|x| unsafe { x.assume_init() })
    }
}
impl<N: VoleArray<T>, T> Distribution<Vole<N, T>> for rand::distr::StandardUniform
where
    rand::distr::StandardUniform: Distribution<u8>,
    rand::distr::StandardUniform: Distribution<[T; 8]>,
{
    fn sample<R: rand::Rng + ?Sized>(&self, rng: &mut R) -> Vole<N, T> {
        let mut u = GenericArray::<u8, N>::default();
        let mut v: GenericArray<MaybeUninit<[T; 8]>, N> =
            GenericArray::generate(|_| MaybeUninit::uninit());
        for i in 0..N::to_usize() {
            u[i] = rng.r#gen();
            v[i] = MaybeUninit::new(rng.r#gen());
        }
        Vole {
            u,
            v: v.map(|x| unsafe { x.assume_init() }),
        }
    }
}
