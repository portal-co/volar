use super::*;
pub struct BitVole<N: VoleArray<T> + VoleArray<Bit>,T>{
    ///Multiplication-based randomizer
    pub u: Array<Bit,N>,
    ///Fixed offset
    pub v: Array<T, N>,
}