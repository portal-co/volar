//! Core dynamic type definitions
//! 
//! These are manually written reference implementations of the dynamic types.

use alloc::vec::Vec;

/// Compute integer log2
#[inline]
pub fn ilog2(x: usize) -> u32 {
    usize::BITS - x.leading_zeros() - 1
}

/// Dynamic version of a Delta type - the length is stored at runtime
#[derive(Clone, Debug, Default)]
pub struct DeltaDyn<T> {
    pub n: usize,
    pub delta: Vec<T>,
}

/// Dynamic version of Q type
#[derive(Clone, Debug, Default)]
pub struct QDyn<T> {
    pub n: usize,
    pub q: Vec<T>,
}

/// Dynamic version of Poly type
#[derive(Clone, Debug, Default)]
pub struct PolyDyn<T> {
    pub m: usize,  // number of coefficients
    pub c0: T,
    pub c1: Vec<T>,
}

/// Dynamic version of BitVole
#[derive(Clone, Debug, Default)]
pub struct BitVoleDyn<T> {
    pub n: usize,
    pub v: Vec<T>,
}

/// Dynamic version of Vope - stores multiple length witnesses
#[derive(Clone, Debug, Default)]
pub struct VopeDyn<T> {
    pub n: usize,  // outer dimension
    pub t: usize,  // inner dimension per layer  
    pub k: usize,  // number of layers
    pub u: Vec<Vec<T>>,
    pub v: Vec<T>,
}

/// Pool for polynomial inputs
pub struct PolyInputPoolDyn<'a, T> {
    pub t: usize,
    pub n: usize,
    pub x: usize,
    pub inputs: &'a [T],
    pub indices: Vec<Vec<usize>>,
}

// Helper trait for types that can provide their length
pub trait HasLen {
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool { self.len() == 0 }
}

impl<T> HasLen for DeltaDyn<T> {
    fn len(&self) -> usize { self.n }
}

impl<T> HasLen for QDyn<T> {
    fn len(&self) -> usize { self.n }
}

impl<T> HasLen for VopeDyn<T> {
    fn len(&self) -> usize { self.n }
}
