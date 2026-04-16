use core::ops::{Add, Mul};

use cipher::Array;
use hybrid_array::ArraySize;

pub struct LweSample<T, U, N: ArraySize, M: ArraySize> {
    pub matrix: Array<Array<T, N>, M>,
    pub b: Array<U, M>,
}
impl<T: Clone, U: Clone, N: ArraySize, M: ArraySize> LweSample<T, U, N, M> {
    pub fn new(matrix: Array<Array<T, N>, M>, b: Array<U, M>) -> Self {
        Self { matrix, b }
    }
    pub fn sample<S: Clone, A: Add<P, Output = U> + Add<A, Output = A> + Default, P: Clone>(
        matrix: Array<Array<T, N>, M>,
        s: Array<S, N>,
        e: Array<P, M>,
    ) -> Self
    where
        T: Mul<S, Output = A>,
    {
        Self {
            b: Array::<U, M>::from_fn(|i| {
                s.iter()
                    .enumerate()
                    .map(|(a, b)| matrix[i][a].clone() * b.clone())
                    .fold(A::default(), |a, b| a + b)
                    + e[i].clone()
            }),
            matrix,
        }
    }
}
