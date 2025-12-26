use super::*;
pub struct BitVole<N: VoleArray<T> + VoleArray<Bit>,T>{
    ///Multiplication-based randomizer
    pub u: GenericArray<Bit,N>,
    ///Fixed offset
    pub v: GenericArray<T, N>,
}