use super::*;
impl<N: VoleArray<T>, T: Clone> Clone for Vole<N, T> {
    fn clone(&self) -> Self {
        let Vole { u, v } = self;
        Vole {
            u: GenericArray::generate(|i| u[i].clone()),
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
impl<N: VoleArray<T>, T: PartialEq> PartialEq for Vole<N, T> {
    fn eq(&self, other: &Self) -> bool {
        let Vole { u: u1, v: v1 } = self;
        let Vole { u: u2, v: v2 } = other;
        for i in 0..N::to_usize() {
            if u1[i] != u2[i] || v1[i] != v2[i] {
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
impl<N: VoleArray<T>, T: Eq> Eq for Vole<N, T> {}
impl<N: ArrayLength<T>, T: Eq> Eq for Q<N, T> {}
impl<N: ArrayLength<T>, T: Eq> Eq for Delta<N, T> {}
