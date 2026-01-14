use cipher::consts::U0;

use crate::field::Bit;

use super::*;
pub mod ai_hazmat;
pub struct Vope<N: VoleArray<T>, T, K: ArrayLength<GenericArray<T, N>> = U1> {
    ///Multiplication-based randomizer
    pub u: GenericArray<GenericArray<T, N>, K>,
    ///Fixed offset
    pub v: GenericArray<T, N>,
}
impl<N: VoleArray<T>, T> Vope<N, T, U0> {
    pub fn constant(v: GenericArray<T, N>) -> Self {
        Vope {
            u: GenericArray::<GenericArray<T, N>, U0>::generate(|_| unreachable!()),
            v,
        }
    }
}
impl<
    N: VoleArray<T> + VoleArray<U> + VoleArray<O>,
    T: Add<U, Output = O> + Clone,
    U: Clone,
    O,
    K: ArrayLength<GenericArray<U, N>>
        + ArrayLength<GenericArray<O, N>>
        + ArrayLength<GenericArray<T, N>>,
> Add<Vope<N, U, K>> for Vope<N, T, K>
{
    type Output = Vope<N, O, K>;
    fn add(self, rhs: Vope<N, U, K>) -> Self::Output {
        Vope {
            u: self.u.zip(rhs.u, |a, b| a.zip(b, |a, b| a + b)),
            v: self.v.zip(rhs.v, |a, b| a + b),
        }
    }
}
impl<
    N: VoleArray<T> + VoleArray<O> + VoleArray<U> + Mul<K, Output: ArrayLength<U>>,
    T: BitXor<U, Output = O> + Clone,
    U: Clone,
    O,
    K: ArrayLength<GenericArray<U, N>>
        + ArrayLength<GenericArray<O, N>>
        + ArrayLength<GenericArray<T, N>>,
> BitXor<GenericArray<U, <N as Mul<K>>::Output>> for Vope<N, T, K>
where
    T: Into<O>,
{
    type Output = Vope<N, O, K>;
    fn bitxor(self, rhs: GenericArray<U, <N as Mul<K>>::Output>) -> Self::Output {
        Vope {
            u: GenericArray::<GenericArray<O, N>, K>::generate(|i| {
                GenericArray::<O, N>::generate(|j| {
                    let o: O = (self.u[i][j].clone()).bitxor(rhs[i * K::to_usize() + j].clone());
                    o
                })
            }),
            v: self.v.map(|a| a.into()),
        }
    }
}
impl<
    N: VoleArray<T> + VoleArray<O> + VoleArray<U>,
    T: Mul<U, Output = O> + Into<O> + Clone,
    U: Mul<U, Output = U> + Clone,
    K: ArrayLength<GenericArray<T, N>>,
    O: Add<O, Output = O>,
> Mul<Delta<N, U>> for Vope<N, T, K>
{
    type Output = Q<N, O>;
    fn mul(self, rhs: Delta<N, U>) -> Self::Output {
        Q {
            q: self
                .u
                .iter()
                .enumerate()
                .fold(self.v.clone().map(|a| a.into()), |a, (i, b)| {
                    a.zip(b, |a, b| {
                        let mut x = rhs.delta[i].clone();
                        for _ in 0..i {
                            x = x * rhs.delta[i].clone();
                        }
                        let m: O = b.clone() * x;
                        m + a
                    })
                }),
        }
    }
}

impl<N: VoleArray<T>, T, K: ArrayLength<GenericArray<T, N>>> Vope<N, T, K> {
    pub fn expand<L: ArrayLength<GenericArray<T, N>>>(&self) -> Vope<N, T, L>
    where
        T: Clone + Default,
    {
        let Self { u, v } = self;
        Vope {
            u: GenericArray::<GenericArray<T, N>, L>::generate(|l| {
                GenericArray::<T, N>::generate(|i| u.get(l).map_or(T::default(), |a| a[i].clone()))
            }),
            v: v.clone(),
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
    pub fn remap<M: VoleArray<T>, F: FnMut(usize) -> usize>(&self, mut f: F) -> Vope<M, T, K>
    where
        T: Clone,
        K: ArrayLength<GenericArray<T, M>>,
    {
        let Self { u, v } = self;
        Vope {
            u: GenericArray::<GenericArray<T, M>, K>::generate(|l| {
                GenericArray::<T, M>::generate(|i| u[l][f(i) % N::to_usize()].clone())
            }),
            v: GenericArray::<T, M>::generate(|i| v[f(i) % N::to_usize()].clone()),
        }
    }
}
impl<N, K> Vope<N, Bit, K>
where
    N: VoleArray<Bit>,
    K: ArrayLength<GenericArray<Bit, N>>,
{
    pub fn scale<T>(self, f: impl Fn(bool) -> T) -> Vope<N, T, K>
    where
        N: VoleArray<T>,
        K: ArrayLength<GenericArray<T, N>>,
    {
        let Vope { u, v } = self;
        Vope {
            u: GenericArray::<GenericArray<T, N>, K>::generate(|l| {
                GenericArray::<T, N>::generate(|i| {
                    let Bit(b) = u[l][i].clone();
                    f(b)
                })
            }),
            v: GenericArray::<T, N>::generate(|i| {
                let Bit(b) = v[i].clone();
                f(b)
            }),
        }
    }
}
