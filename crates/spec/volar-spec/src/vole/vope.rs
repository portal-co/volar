// @reliability: normal
//! @ai: assisted
// Vope<N, T, K>: polynomial VOLE — a degree-K polynomial in Δ over vectors of length N.
// Evaluation at Δ yields a Q<N, T>. See vope/ai_hazmat.rs for degree-K multiplication.
use cipher::consts::U0;

use crate::field::Bit;

use super::*;
pub mod ai_hazmat;
pub struct Vope<N: VoleArray<T>, T, K: ArraySize = U1> {
    ///Multiplication-based randomizer
    pub u: Array<Array<T, N>, K>,
    ///Fixed offset
    pub v: Array<T, N>,
}
impl<N: VoleArray<T>, T> Vope<N, T, U0> {
    pub fn constant(v: Array<T, N>) -> Self {
        Vope {
            u: Array::<Array<T, N>, U0>::from_fn(|_| unreachable!()),
            v,
        }
    }
}
impl<
    N: VoleArray<T> + VoleArray<U> + VoleArray<O>,
    T: Add<U, Output = O> + Clone,
    U: Clone,
    O,
    K: ArraySize,
> Add<Vope<N, U, K>> for Vope<N, T, K>
{
    type Output = Vope<N, O, K>;
    fn add(self, rhs: Vope<N, U, K>) -> Self::Output {
        Vope {
            u: Array::<Array<O, N>, K>::from_fn(|l| {
                Array::<O, N>::from_fn(|i| self.u[l][i].clone() + rhs.u[l][i].clone())
            }),
            v: Array::<O, N>::from_fn(|i| self.v[i].clone() + rhs.v[i].clone()),
        }
    }
}
impl<
    N: VoleArray<T> + VoleArray<O> + VoleArray<U> + Mul<K, Output: ArraySize>,
    T: BitXor<U, Output = O> + Clone,
    U: Clone,
    O,
    K: ArraySize,
> BitXor<Array<U, <N as Mul<K>>::Output>> for Vope<N, T, K>
where
    T: Into<O>,
{
    type Output = Vope<N, O, K>;
    fn bitxor(self, rhs: Array<U, <N as Mul<K>>::Output>) -> Self::Output {
        Vope {
            u: Array::<Array<O, N>, K>::from_fn(|i| {
                Array::<O, N>::from_fn(|j| {
                    let o: O = (self.u[i][j].clone()).bitxor(rhs[i * K::USIZE + j].clone());
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
    K: ArraySize,
    O: Add<O, Output = O> + Clone,
> Mul<Delta<N, U>> for Vope<N, T, K>
{
    type Output = Q<N, O>;
    fn mul(self, rhs: Delta<N, U>) -> Self::Output {
        Q {
            q: self
                .u
                .iter()
                .enumerate()
                .fold(self.v.map(|a| a.into()), |a, (i, b)| {
                    Array::<O, N>::from_fn(|j| {
                        let mut x = rhs.delta[i].clone();
                        for _ in 0..i {
                            x = x * rhs.delta[i].clone();
                        }
                        let m: O = b[j].clone() * x;
                        m + a[j].clone()
                    })
                }),
        }
    }
}

impl<N: VoleArray<T>, T, K: ArraySize> Vope<N, T, K> {
    pub fn expand<L: ArraySize>(&self) -> Vope<N, T, L>
    where
        T: Clone + Default,
    {
        let Self { u, v } = self;
        Vope {
            u: Array::<Array<T, N>, L>::from_fn(|l| {
                Array::<T, N>::from_fn(|i| u.get(l).map_or(T::default(), |a| a[i].clone()))
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
    {
        let Self { u, v } = self;
        Vope {
            u: Array::<Array<T, M>, K>::from_fn(|l| {
                Array::<T, M>::from_fn(|i| u[l][f(i) % N::USIZE].clone())
            }),
            v: Array::<T, M>::from_fn(|i| v[f(i) % N::USIZE].clone()),
        }
    }
}
impl<N, K> Vope<N, Bit, K>
where
    N: VoleArray<Bit>,
    K: ArraySize,
{
    pub fn scale<T>(self, f: impl Fn(bool) -> T) -> Vope<N, T, K>
    where
        N: VoleArray<T>,
    {
        let Vope { u, v } = self;
        Vope {
            u: Array::<Array<T, N>, K>::from_fn(|l| {
                Array::<T, N>::from_fn(|i| {
                    let Bit(b) = u[l][i].clone();
                    f(b)
                })
            }),
            v: Array::<T, N>::from_fn(|i| {
                let Bit(b) = v[i].clone();
                f(b)
            }),
        }
    }
}
