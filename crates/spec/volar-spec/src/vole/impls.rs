use crate::vole::vope::Vope;

use super::*;
impl<N: VoleArray<T>, T: Clone, K: ArraySize> Clone for Vope<N, T, K> {
    fn clone(&self) -> Self {
        let Vope { u, v } = self;
        Vope {
            u: Array::<Array<T, N>, K>::from_fn(|l| Array::<T, N>::from_fn(|i| u[l][i].clone())),
            v: Array::<T, N>::from_fn(|i| v[i].clone()),
        }
    }
}
impl<N: ArraySize, T: Clone> Clone for Q<N, T> {
    fn clone(&self) -> Self {
        let Q { q } = self;
        Q {
            q: Array::<T, N>::from_fn(|i| q[i].clone()),
        }
    }
}
impl<N: ArraySize, T: Clone> Clone for Delta<N, T> {
    fn clone(&self) -> Self {
        let Delta { delta } = self;
        Delta {
            delta: Array::<T, N>::from_fn(|i| delta[i].clone()),
        }
    }
}
impl<N: VoleArray<T>, T: PartialEq, K: ArraySize> PartialEq for Vope<N, T, K> {
    fn eq(&self, other: &Self) -> bool {
        let Vope { u: u1, v: v1 } = self;
        let Vope { u: u2, v: v2 } = other;
        for l in 0..K::USIZE {
            for i in 0..N::USIZE {
                if u1[l][i] != u2[l][i] {
                    return false;
                }
            }
        }
        for i in 0..N::USIZE {
            if v1[i] != v2[i] {
                return false;
            }
        }
        true
    }
}
impl<N: ArraySize, T: PartialEq> PartialEq for Q<N, T> {
    fn eq(&self, other: &Self) -> bool {
        let Q { q: q1 } = self;
        let Q { q: q2 } = other;
        for i in 0..N::USIZE {
            if q1[i] != q2[i] {
                return false;
            }
        }
        true
    }
}
impl<N: ArraySize, T: PartialEq> PartialEq for Delta<N, T> {
    fn eq(&self, other: &Self) -> bool {
        let Delta { delta: d1 } = self;
        let Delta { delta: d2 } = other;
        for i in 0..N::USIZE {
            if d1[i] != d2[i] {
                return false;
            }
        }
        true
    }
}
impl<N: VoleArray<T>, T: Eq, K: ArraySize> Eq for Vope<N, T, K> {}
impl<N: ArraySize, T: Eq> Eq for Q<N, T> {}
impl<N: ArraySize, T: Eq> Eq for Delta<N, T> {}
