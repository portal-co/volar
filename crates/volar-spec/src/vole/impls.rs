use crate::vole::vope::Vope;

use super::*;
impl<N: VoleArray<T>, T: Clone, K: ArrayLength<GenericArray<T, N>>> Clone for Vope<N, T, K> {
    fn clone(&self) -> Self {
        let Vope { u, v } = self;
        Vope {
            u: GenericArray::generate(|l| GenericArray::generate(|i| u[l][i].clone())),
            v: GenericArray::generate(|i| v[i].clone()),
        }
    }
}
impl<N: ArrayLength<T>, T: Clone> Clone for Q<N, T> {
    fn clone(&self) -> Self {
        let Q { q } = self;
        Q {
            q: GenericArray::generate(|i| q[i].clone()),
        }
    }
}
impl<N: ArrayLength<T>, T: Clone> Clone for Delta<N, T> {
    fn clone(&self) -> Self {
        let Delta { delta } = self;
        Delta {
            delta: GenericArray::generate(|i| delta[i].clone()),
        }
    }
}
impl<N: VoleArray<T>, T: PartialEq, K: ArrayLength<GenericArray<T, N>>> PartialEq
    for Vope<N, T, K>
{
    fn eq(&self, other: &Self) -> bool {
        let Vope { u: u1, v: v1 } = self;
        let Vope { u: u2, v: v2 } = other;
        for l in 0..K::to_usize() {
            for i in 0..N::to_usize() {
                if u1[l][i] != u2[l][i] {
                    return false;
                }
            }
        }
        for i in 0..N::to_usize() {
            if v1[i] != v2[i] {
                return false;
            }
        }
        true
    }
}
impl<N: ArrayLength<T>, T: PartialEq> PartialEq for Q<N, T> {
    fn eq(&self, other: &Self) -> bool {
        let Q { q: q1 } = self;
        let Q { q: q2 } = other;
        for i in 0..N::to_usize() {
            if q1[i] != q2[i] {
                return false;
            }
        }
        true
    }
}
impl<N: ArrayLength<T>, T: PartialEq> PartialEq for Delta<N, T> {
    fn eq(&self, other: &Self) -> bool {
        let Delta { delta: d1 } = self;
        let Delta { delta: d2 } = other;
        for i in 0..N::to_usize() {
            if d1[i] != d2[i] {
                return false;
            }
        }
        true
    }
}
impl<N: VoleArray<T>, T: Eq, K: ArrayLength<GenericArray<T, N>>> Eq for Vope<N, T, K> {}
impl<N: ArrayLength<T>, T: Eq> Eq for Q<N, T> {}
impl<N: ArrayLength<T>, T: Eq> Eq for Delta<N, T> {}
