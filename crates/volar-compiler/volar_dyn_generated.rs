//! Auto-generated dynamic types from volar-spec
//! Type-level lengths have been converted to runtime usize witnesses

#![allow(unused_variables, dead_code, unused_mut, unused_imports, non_snake_case)]

extern crate alloc;
use alloc::vec::Vec;
use alloc::vec;
use core::ops::{Add, Sub, Mul, Div, BitAnd, BitOr, BitXor, Shl, Shr};
use typenum::Unsigned;
use cipher::BlockEncrypt;
use digest::Digest;
use volar_common::hash_commitment::commit;


/// Block cipher that can encrypt blocks and be created from a 32-byte key
pub trait ByteBlockEncrypt: BlockEncrypt + From<[u8; 32]> {}
impl<T: BlockEncrypt + From<[u8; 32]>> ByteBlockEncrypt for T {}

// Primitive field types from volar-primitives
pub use volar_primitives::{Bit, BitsInBytes, BitsInBytes64, Galois, Galois64};

/// Compute integer log2
#[inline]
pub fn ilog2(x: usize) -> u32 {
    usize::BITS - x.leading_zeros() - 1
}

#[derive(Debug, Default)]
pub struct DeltaDyn<T> {
    pub n: usize,
    pub delta: Vec<T>,
}

#[derive(Debug, Default)]
pub struct QDyn<T> {
    pub n: usize,
    pub q: Vec<T>,
}

#[derive(Clone, Debug, Default)]
pub struct ABODyn<B: ByteBlockEncrypt, D: Digest> {
    pub k: usize,
    pub commit: Vec<u8>,
    pub per_byte: Vec<Vec<u8>>,
    _phantom: core::marker::PhantomData<(B, D)>,
}

#[derive(Clone, Debug, Default)]
pub struct ABOOpeningDyn<B: ByteBlockEncrypt, D: Digest> {
    pub t: usize,
    pub u: usize,
    pub bad: Vec<u64>,
    pub openings: Vec<Vec<Vec<u8>>>,
    _phantom: core::marker::PhantomData<(B, D)>,
}

#[derive(Clone, Debug, Default)]
pub struct PolyDyn<T> {
    pub n: usize,
    pub c0: T,
    pub c1: Vec<T>,
}

#[derive(Clone, Debug)]
pub struct PolyInputPoolDyn<'a, T> {
    pub n: usize,
    pub x: usize,
    pub inputs: &'a [T],
    pub indices: Vec<Vec<usize>>,
}

#[derive(Clone, Debug, Default)]
pub struct BitVoleDyn<T> {
    pub n: usize,
    pub u: Vec<Bit>,
    pub v: Vec<T>,
}

#[derive(Debug, Default)]
pub struct VopeDyn<T> {
    pub n: usize,
    pub k: usize,
    pub u: Vec<Vec<T>>,
    pub v: Vec<T>,
}


impl<T> DeltaDyn<T>// UNCONSTRAINED GENERICS at generated.rs line 90: T
 {
    pub fn remap<F: FnMut(usize) -> usize>(&self, m: usize, mut f: F) -> DeltaDyn<T> where T: Clone {
        let n = self.n;
        let Self { delta: delta, .. } = self;
        DeltaDyn { delta: (0..m).map(|i| delta[(f(i) % n)].clone()).collect::<Vec<_>>(), n: m }
    }
    pub fn rotate_left(&self, mut n: usize) -> Self where T: Clone {
        let n = self.n;
        self.remap(n, |a: usize| a.wrapping_sub(n))
    }
    pub fn rotate_right(&self, mut n: usize) -> Self where T: Clone {
        let n = self.n;
        self.remap(n, |a: usize| a.wrapping_add(n))
    }
    pub fn r#static<U: Mul<T, Output = O> + Clone, O>(&self, mut val: Vec<U>) -> QDyn<O> where T: Clone// UNCONSTRAINED GENERICS at generated.rs line 105: O
 {
        let n = self.n;
        QDyn { q: (0..n).map(|i| (val[i].clone() * self.delta[i].clone())).collect::<Vec<_>>(), n: n }
    }
}

impl<T> QDyn<T>// UNCONSTRAINED GENERICS at generated.rs line 112: T
 {
    pub fn remap<F: FnMut(usize) -> usize>(&self, m: usize, mut f: F) -> QDyn<T> where T: Clone {
        let n = self.n;
        let Self { q: q, .. } = self;
        QDyn { q: (0..m).map(|i| q[(f(i) % n)].clone()).collect::<Vec<_>>(), n: m }
    }
    pub fn rotate_left(&self, mut n: usize) -> Self where T: Clone {
        let n = self.n;
        self.remap(n, |a: usize| a.wrapping_sub(n))
    }
    pub fn rotate_right(&self, mut n: usize) -> Self where T: Clone {
        let n = self.n;
        self.remap(n, |a: usize| a.wrapping_add(n))
    }
}


impl<T> PolyDyn<T>// UNCONSTRAINED GENERICS at generated.rs line 130: T
 {
    pub fn get_qs_pool<Q: Clone + Mul<A, Output = A>, A: Add<A, Output = A>>(&self, m: usize, x: usize, mut root: DeltaDyn<Q>, mut inputs: PolyInputPoolDyn<QDyn<Q>>, mut reduction: usize) -> QDyn<A> where T: Clone + Into<A> {
        let n = self.n;
        QDyn { q: (0..m).map(|i| {
    let mut sum: A = self.c0.clone().into();
    for _ in 0..n {
    sum = (root.delta[i].clone() * sum);
};
    for j in 0..n {
    let mut b: A = self.c1[j].clone().into();
    for i2 in inputs.indices.iter() {
    for _ in 0..reduction {
    b = (inputs.inputs[i2[j]].q[i].clone() * b);
}
};
    sum = (sum + b);
};
    sum
}).collect::<Vec<_>>(), n: m }
    }
    pub fn get_qs<Q: Clone + Mul<A, Output = A>, A: Add<A, Output = A>>(&self, m: usize, x: usize, mut root: DeltaDyn<Q>, mut inputs: Vec<Vec<QDyn<Q>>>, mut reduction: usize) -> QDyn<A> where T: Clone + Into<A> {
        let n = self.n;
        QDyn { q: (0..m).map(|i| {
    let mut sum: A = self.c0.clone().into();
    for _ in 0..n {
    sum = (root.delta[i].clone() * sum);
};
    for j in 0..n {
    let mut b: A = self.c1[j].clone().into();
    for i2 in inputs.iter() {
    for _ in 0..reduction {
    b = (i2[j].q[i].clone() * b);
}
};
    sum = (sum + b);
};
    sum
}).collect::<Vec<_>>(), n: m }
    }
    pub fn apply_pool<O: Mul<O, Output = O> + Add<O, Output = O> + Default + Clone>(&self, m: usize, x: usize, x2: usize, xs: usize, s: usize, mut voles: &PolyInputPoolDyn<VopeDyn<T>>) -> VopeDyn<O> where T: Into<O> + Clone {
        let n = self.n;
        let v = (0..m).map(|i| {
    let mut sum = O::default();
    for k in 0..n {
    let mut b: O = self.c1[k].clone().into();
    for v in &voles.indices {
    b = (b * voles.inputs[v[k]].v[i].clone().into());
};
    sum = (sum + b);
};
    let c0: O = self.c0.clone().into();
    (sum + c0)
}).collect::<Vec<_>>();
        let u = (0..xs).map(|l| {
    (0..m).map(|i| {
    let mut sum = O::default();
    for k in 0..n {
    for n in 0..x {
    let mut b: O = self.c1[k].clone().into();
    for m in 0..s {
    let l = ((l * s) + m);
    for (idx, v) in voles.indices.iter().enumerate() {
    b = (b * if (idx == n) {
    voles.inputs[v[k]].u[l][i].clone().into()
} else {
    voles.inputs[v[k]].v[i].clone().into()
});
}
};
    sum = (sum + b);
}
};
    sum
}).collect::<Vec<_>>()
}).collect::<Vec<_>>();
        return VopeDyn { u: u, v: v, n: m, k: xs };
    }
    pub fn apply<O: Mul<O, Output = O> + Add<O, Output = O> + Default + Clone>(&self, m: usize, x: usize, x2: usize, xs: usize, s: usize, mut voles: Vec<Vec<VopeDyn<T>>>) -> VopeDyn<O> where T: Into<O> + Clone {
        let n = self.n;
        let v = (0..m).map(|i| {
    let mut sum = O::default();
    for k in 0..n {
    let mut b: O = self.c1[k].clone().into();
    for v in &voles {
    b = (b * v[k].v[i].clone().into());
};
    sum = (sum + b);
};
    let c0: O = self.c0.clone().into();
    (sum + c0)
}).collect::<Vec<_>>();
        let u = (0..xs).map(|l| {
    (0..m).map(|i| {
    let mut sum = O::default();
    for k in 0..n {
    for n in 0..x {
    let mut b: O = self.c1[k].clone().into();
    for m in 0..s {
    let l = ((l * s) + m);
    for (idx, v) in voles.iter().enumerate() {
    b = (b * if (idx == n) {
    v[k].u[l][i].clone().into()
} else {
    v[k].v[i].clone().into()
});
}
};
    sum = (sum + b);
}
};
    sum
}).collect::<Vec<_>>()
}).collect::<Vec<_>>();
        return VopeDyn { u: u, v: v, n: m, k: xs };
    }
}

impl<T: Add<Output = T> + Mul<Output = T> + Default + Clone + Add<Output = T> + Mul<Output = T> + Default + Clone> VopeDyn<T> where T: Add<Output = T> + Mul<Output = T> + Default + Clone {
    pub fn mul_generalized(&self, k2: usize, mut other: &VopeDyn<T>) -> VopeDyn<T> {
        let n = self.n;
        let k = self.k;
        let mut res_u = Vec::<Vec<T>>::new();
        let mut res_v = Vec::<T>::new();
        for i in 0..=k {
    for j in 0..=k2 {
    let k = (i + j);
    let a_coeff = (if i == 0 { &self.v } else { &self.u[i - 1] });
    let b_coeff = (if j == 0 { &other.v } else { &other.u[j - 1] });
    if (k == 0) {
    for lane in 0..n {
    res_v[lane] = (res_v[lane].clone() + (a_coeff[lane].clone() * b_coeff[lane].clone()));
}
} else {
    for lane in 0..n {
    res_u[(k - 1)][lane] = (res_u[(k - 1)][lane].clone() + (a_coeff[lane].clone() * b_coeff[lane].clone()));
}
}
}
};
        VopeDyn { u: res_u, v: res_v, n: n, k: k2 }
    }
}

impl VopeDyn<BitsInBytes> {
    pub fn rotate_left_bits(&self, mut n: usize) -> Self {
        let n = self.n;
        let k = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n).map(|i| {
    let BitsInBytes(b) = u[l][i].clone();
    let BitsInBytes(next) = u[l][((i + 1) % n)].clone();
    BitsInBytes((b.shl((n as u32)) | next.shr((8 - (n as u32)))))
}).collect::<Vec<_>>()
}).collect::<Vec<_>>(), v: (0..n).map(|i| {
    let BitsInBytes(b) = v[i].clone();
    let BitsInBytes(next) = v[((i + 1) % n)].clone();
    BitsInBytes((b.shl((n as u32)) | next.shr((8 - (n as u32)))))
}).collect::<Vec<_>>(), n: n, k: 1 }
    }
    pub fn rotate_right_bits(&self, mut n: usize) -> Self {
        let n = self.n;
        let k = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n).map(|i| {
    let BitsInBytes(prev) = u[l][(((i + n) - 1) % n)].clone();
    let BitsInBytes(b) = u[l][i].clone();
    BitsInBytes((prev.shl((8 - (n as u32))) | b.shr((n as u32))))
}).collect::<Vec<_>>()
}).collect::<Vec<_>>(), v: (0..n).map(|i| {
    let BitsInBytes(prev) = v[(((i + n) - 1) % n)].clone();
    let BitsInBytes(b) = v[i].clone();
    BitsInBytes((prev.shl((8 - (n as u32))) | b.shr((n as u32))))
}).collect::<Vec<_>>(), n: n, k: 1 }
    }
    pub fn bit(&self, mut n: u8) -> VopeDyn<Bit> {
        let n = self.n;
        let k = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n).map(|i| {
    let BitsInBytes(b) = u[l][i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect::<Vec<_>>()
}).collect::<Vec<_>>(), v: (0..n).map(|i| {
    let BitsInBytes(b) = v[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect::<Vec<_>>(), n: n, k: k }
    }
}

impl VopeDyn<BitsInBytes64> {
    pub fn rotate_left_bits(&self, mut n: usize) -> Self {
        let n = self.n;
        let k = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n).map(|i| {
    let BitsInBytes64(b) = u[l][i].clone();
    let BitsInBytes64(next) = u[l][((i + 1) % n)].clone();
    BitsInBytes64((b.shl((n as u32)) | next.shr((64 - (n as u32)))))
}).collect::<Vec<_>>()
}).collect::<Vec<_>>(), v: (0..n).map(|i| {
    let BitsInBytes64(b) = v[i].clone();
    let BitsInBytes64(next) = v[((i + 1) % n)].clone();
    BitsInBytes64((b.shl((n as u32)) | next.shr((64 - (n as u32)))))
}).collect::<Vec<_>>(), n: n, k: 1 }
    }
    pub fn rotate_right_bits(&self, mut n: usize) -> Self {
        let n = self.n;
        let k = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n).map(|i| {
    let BitsInBytes64(prev) = u[l][(((i + n) - 1) % n)].clone();
    let BitsInBytes64(b) = u[l][i].clone();
    BitsInBytes64((prev.shl((64 - (n as u32))) | b.shr((n as u32))))
}).collect::<Vec<_>>()
}).collect::<Vec<_>>(), v: (0..n).map(|i| {
    let BitsInBytes64(prev) = v[(((i + n) - 1) % n)].clone();
    let BitsInBytes64(b) = v[i].clone();
    BitsInBytes64((prev.shl((64 - (n as u32))) | b.shr((n as u32))))
}).collect::<Vec<_>>(), n: n, k: 1 }
    }
    pub fn bit(&self, mut n: u8) -> VopeDyn<Bit> {
        let n = self.n;
        let k = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n).map(|i| {
    let BitsInBytes64(b) = u[l][i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect::<Vec<_>>()
}).collect::<Vec<_>>(), v: (0..n).map(|i| {
    let BitsInBytes64(b) = v[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect::<Vec<_>>(), n: n, k: k }
    }
}

impl<T: Clone> Clone for VopeDyn<T> {
    fn clone(&self) -> Self {
        let n = self.n;
        let k = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n).map(|i| u[l][i].clone()).collect::<Vec<_>>()
}).collect::<Vec<_>>(), v: (0..n).map(|i| v[i].clone()).collect::<Vec<_>>(), n: n, k: 1 }
    }
}

impl<T: Clone> Clone for QDyn<T> {
    fn clone(&self) -> Self {
        let n = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n).map(|i| q[i].clone()).collect::<Vec<_>>(), n: n }
    }
}

impl<T: Clone> Clone for DeltaDyn<T> {
    fn clone(&self) -> Self {
        let n = self.n;
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n).map(|i| delta[i].clone()).collect::<Vec<_>>(), n: n }
    }
}

impl<T: PartialEq> PartialEq for VopeDyn<T> {
    fn eq(&self, mut other: &Self) -> bool {
        let n = self.n;
        let k = self.k;
        let VopeDyn { u: u1, v: v1, .. } = self;
        let VopeDyn { u: u2, v: v2, .. } = other;
        for l in 0..k {
    for i in 0..n {
    if (u1[l][i] != u2[l][i]) {
    return false;
}
}
};
        for i in 0..n {
    if (v1[i] != v2[i]) {
    return false;
}
};
        true
    }
}

impl<T: PartialEq> PartialEq for QDyn<T> {
    fn eq(&self, mut other: &Self) -> bool {
        let n = self.n;
        let QDyn { q: q1, .. } = self;
        let QDyn { q: q2, .. } = other;
        for i in 0..n {
    if (q1[i] != q2[i]) {
    return false;
}
};
        true
    }
}

impl<T: PartialEq> PartialEq for DeltaDyn<T> {
    fn eq(&self, mut other: &Self) -> bool {
        let n = self.n;
        let DeltaDyn { delta: d1, .. } = self;
        let DeltaDyn { delta: d2, .. } = other;
        for i in 0..n {
    if (d1[i] != d2[i]) {
    return false;
}
};
        true
    }
}

impl<T: Eq> Eq for VopeDyn<T> {
}

impl<T: Eq> Eq for QDyn<T> {
}

impl<T: Eq> Eq for DeltaDyn<T> {
}

impl QDyn<BitsInBytes> {
    pub fn rotate_left_bits(&self, mut n: usize) -> Self {
        let n = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n).map(|i| {
    let BitsInBytes(b) = q[i].clone();
    let BitsInBytes(next) = q[((i + 1) % n)].clone();
    BitsInBytes((b.shl((n as u32)) | next.shr((8 - (n as u32)))))
}).collect::<Vec<_>>(), n: n }
    }
    pub fn rotate_right_bits(&self, mut n: usize) -> Self {
        let n = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n).map(|i| {
    let BitsInBytes(prev) = q[(((i + n) - 1) % n)].clone();
    let BitsInBytes(b) = q[i].clone();
    BitsInBytes((prev.shl((8 - (n as u32))) | b.shr((n as u32))))
}).collect::<Vec<_>>(), n: n }
    }
    pub fn bit(&self, mut n: u8) -> QDyn<Bit> {
        let n = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n).map(|i| {
    let BitsInBytes(b) = q[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect::<Vec<_>>(), n: n }
    }
}

impl QDyn<BitsInBytes64> {
    pub fn rotate_left_bits(&self, mut n: usize) -> Self {
        let n = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n).map(|i| {
    let BitsInBytes64(b) = q[i].clone();
    let BitsInBytes64(next) = q[((i + 1) % n)].clone();
    BitsInBytes64((b.shl((n as u32)) | next.shr((64 - (n as u32)))))
}).collect::<Vec<_>>(), n: n }
    }
    pub fn rotate_right_bits(&self, mut n: usize) -> Self {
        let n = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n).map(|i| {
    let BitsInBytes64(prev) = q[(((i + n) - 1) % n)].clone();
    let BitsInBytes64(b) = q[i].clone();
    BitsInBytes64((prev.shl((64 - (n as u32))) | b.shr((n as u32))))
}).collect::<Vec<_>>(), n: n }
    }
    pub fn bit(&self, mut n: u8) -> QDyn<Bit> {
        let n = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n).map(|i| {
    let BitsInBytes64(b) = q[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect::<Vec<_>>(), n: n }
    }
}

impl DeltaDyn<BitsInBytes> {
    pub fn rotate_left_bits(&self, mut n: usize) -> Self {
        let n = self.n;
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let BitsInBytes(b) = delta[i].clone();
    let BitsInBytes(next) = delta[((i + 1) % n)].clone();
    BitsInBytes((b.shl((n as u32)) | next.shr((8 - (n as u32)))))
}).collect::<Vec<_>>(), n: n }
    }
    pub fn rotate_right_bits(&self, mut n: usize) -> Self {
        let n = self.n;
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let BitsInBytes(prev) = delta[(((i + n) - 1) % n)].clone();
    let BitsInBytes(b) = delta[i].clone();
    BitsInBytes((prev.shl((8 - (n as u32))) | b.shr((n as u32))))
}).collect::<Vec<_>>(), n: n }
    }
    pub fn bit(&self, mut n: u8) -> DeltaDyn<Bit> {
        let n = self.n;
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let BitsInBytes(b) = delta[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect::<Vec<_>>(), n: n }
    }
}

impl DeltaDyn<BitsInBytes64> {
    pub fn rotate_left_bits(&self, mut n: usize) -> Self {
        let n = self.n;
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let BitsInBytes64(b) = delta[i].clone();
    let BitsInBytes64(next) = delta[((i + 1) % n)].clone();
    BitsInBytes64((b.shl((n as u32)) | next.shr((64 - (n as u32)))))
}).collect::<Vec<_>>(), n: n }
    }
    pub fn rotate_right_bits(&self, mut n: usize) -> Self {
        let n = self.n;
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let BitsInBytes64(prev) = delta[(((i + n) - 1) % n)].clone();
    let BitsInBytes64(b) = delta[i].clone();
    BitsInBytes64((prev.shl((64 - (n as u32))) | b.shr((n as u32))))
}).collect::<Vec<_>>(), n: n }
    }
    pub fn bit(&self, mut n: u8) -> DeltaDyn<Bit> {
        let n = self.n;
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let BitsInBytes64(b) = delta[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect::<Vec<_>>(), n: n }
    }
}

impl<T> VopeDyn<T>// UNCONSTRAINED GENERICS at generated.rs line 574: T
 {
    pub fn constant(n: usize, mut v: Vec<T>) -> Self {
        VopeDyn { u: (0..0).map(|_| unreachable!()).collect::<Vec<_>>(), v: v, n: n, k: 1 }
    }
}

impl<T: Add<U, Output = O> + Clone, U: Clone, O> Add<VopeDyn<U>> for VopeDyn<T>// UNCONSTRAINED GENERICS at generated.rs line 581: O
 {
    type Output = VopeDyn<O>;
    fn add(self, mut rhs: VopeDyn<U>) -> Self::Output {
        let n = self.n;
        let k = self.k;
        VopeDyn { u: self.u.into_iter().zip(rhs.u.into_iter()).map(|(a, b)| a.into_iter().zip(b.into_iter()).map(|(a, b)| (a + b)).collect::<Vec<_>>()).collect::<Vec<_>>(), v: self.v.into_iter().zip(rhs.v.into_iter()).map(|(a, b)| (a + b)).collect::<Vec<_>>(), n: n, k: 1 }
    }
}

impl<T: BitXor<U, Output = O> + Clone + Into<O> + Into<O>, U: Clone, O> BitXor<Vec<U>> for VopeDyn<T> where T: Into<O>// UNCONSTRAINED GENERICS at generated.rs line 591: O
 {
    type Output = VopeDyn<O>;
    fn bitxor(self, mut rhs: Vec<U>) -> Self::Output {
        let n = self.n;
        let k = self.k;
        VopeDyn { u: (0..k).map(|i| {
    (0..n).map(|j| {
    let o: O = self.u[i][j].clone().bitxor(rhs[((i * k) + j)].clone());
    o
}).collect::<Vec<_>>()
}).collect::<Vec<_>>(), v: self.v.into_iter().map(|a| a.into()).collect::<Vec<_>>(), n: n, k: 1 }
    }
}

impl<T: Mul<U, Output = O> + Into<O> + Clone, U: Mul<U, Output = U> + Clone, O: Add<O, Output = O>> Mul<DeltaDyn<U>> for VopeDyn<T> {
    type Output = QDyn<O>;
    fn mul(self, mut rhs: DeltaDyn<U>) -> Self::Output {
        let n = self.n;
        let k = self.k;
        QDyn { q: self.u.iter().enumerate().fold(self.v.clone().into_iter().map(|a| a.into()).collect::<Vec<_>>(), |a, (i, b)| {
    a.into_iter().zip(b.into_iter()).map(|(a, b)| {
    let mut x = rhs.delta[i].clone();
    for _ in 0..i {
    x = (x * rhs.delta[i].clone());
};
    let m: O = (b.clone() * x);
    (m + a)
}).collect::<Vec<_>>()
}), n: n }
    }
}

impl<T> VopeDyn<T>// UNCONSTRAINED GENERICS at generated.rs line 624: T
 {
    pub fn expand(&self, l: usize) -> VopeDyn<T> where T: Clone + Default {
        let n = self.n;
        let k = self.k;
        let Self { u: u, v: v, .. } = self;
        VopeDyn { u: (0..l).map(|l| {
    (0..n).map(|i| u.get(l).map_or(T::default(), |a| a[i].clone())).collect::<Vec<_>>()
}).collect::<Vec<_>>(), v: v.clone(), n: n, k: l }
    }
    pub fn rotate_left(&self, mut n: usize) -> Self where T: Clone {
        let n = self.n;
        let k = self.k;
        self.remap(n, |a: usize| a.wrapping_sub(n))
    }
    pub fn rotate_right(&self, mut n: usize) -> Self where T: Clone {
        let n = self.n;
        let k = self.k;
        self.remap(n, |a: usize| a.wrapping_add(n))
    }
    pub fn remap<F: FnMut(usize) -> usize>(&self, m: usize, mut f: F) -> VopeDyn<T> where T: Clone {
        let n = self.n;
        let k = self.k;
        let Self { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..m).map(|i| u[l][(f(i) % n)].clone()).collect::<Vec<_>>()
}).collect::<Vec<_>>(), v: (0..m).map(|i| v[(f(i) % n)].clone()).collect::<Vec<_>>(), n: m, k: k }
    }
}

impl VopeDyn<Bit> {
    pub fn scale<T>(self, mut f: impl FnMut(bool) -> T) -> VopeDyn<T>// UNCONSTRAINED GENERICS at generated.rs line 655: T
 {
        let n = self.n;
        let k = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n).map(|i| {
    let Bit(b) = u[l][i].clone();
    f(b)
}).collect::<Vec<_>>()
}).collect::<Vec<_>>(), v: (0..n).map(|i| {
    let Bit(b) = v[i].clone();
    f(b)
}).collect::<Vec<_>>(), n: n, k: k }
    }
}

impl<B: ByteBlockEncrypt, D: Digest> ABODyn<B, D> {
    pub fn open<R: AsRef<[u8]>>(&self, t: usize, u: usize, mut bad: Vec<u64>, mut rand: &R) -> ABOOpeningDyn<B, D> {
        let k = self.k;
        ABOOpeningDyn { bad: bad.clone(), openings: (0..t).map(|i| {
    let bad = bad.clone();
    (0..u).map(|j| {
    let i2 = (i | ((j as usize) << ilog2(t)));
    if bad.contains(&(i2 as u64)) {
    let h = commit::<D>(&self.per_byte[i2], rand);
    (0..self::output).map(|j| h.as_ref().get(j).cloned().unwrap_or_default()).collect::<Vec<_>>()
} else {
    (0..self::output).map(|j| {
    self.per_byte[i2].get(j).cloned().unwrap_or_default()
}).collect::<Vec<_>>()
}
}).collect::<Vec<_>>()
}).collect::<Vec<_>>(), t: t, u: u, _phantom: core::marker::PhantomData }
    }
}

impl<B: ByteBlockEncrypt, D: Digest> ABOOpeningDyn<B, D> {
    pub fn validate<R: AsRef<[u8]>>(&self, mut commit_: &Vec<u8>, mut rand: &R) -> bool {
        let t = self.t;
        let u = self.u;
        let mut h = D::new();
        for i in 0..t {
    for b in 0..u {
    let i2 = (i | ((b as usize) << ilog2(t)));
    if self.bad.contains(&(i2 as u64)) {
    h.update(&self.openings[i][b][(0 .. <D::OutputSize as typenum::Unsigned>::USIZE)]);
} else {
    h.update(&commit::<D>(&&self.openings[i][b][(0 .. <B::BlockSize as typenum::Unsigned>::USIZE)], rand));
}
}
};
        (h.finalize().as_slice() == commit_.as_slice())
    }
    pub fn to_vole_material(&self, n: usize) -> Vec<VopeDyn<u8>> {
        let t = self.t;
        let u = self.u;
        (0..n).map(|i| {
    let s = &self.openings[i];
    create_vole_from_material::<B, _>(s)
}).collect::<Vec<_>>()
    }
    pub fn to_vole_material_typenum(&self, n: usize) -> Vec<VopeDyn<u8>> {
        let t = self.t;
        let u = self.u;
        (0..n).map(|i| {
    let s = &self.openings[i];
    create_vole_from_material::<B, _>(s)
}).collect::<Vec<_>>()
    }
    pub fn to_vole_material_expanded<X: AsRef<[u8]>, F: FnMut(&[u8]) -> X>(&self, n: usize, mut f: F) -> Vec<VopeDyn<u8>> {
        let t = self.t;
        let u = self.u;
        (0..n).map(|i| {
    let s = &self.openings[i];
    create_vole_from_material_expanded::<B, _, _, _>(s, &mut f)
}).collect::<Vec<_>>()
    }
    pub fn to_vole_material_typenum_expanded<X: AsRef<[u8]>, F: FnMut(&[u8]) -> X>(&self, n: usize, mut f: F) -> Vec<VopeDyn<u8>> {
        let t = self.t;
        let u = self.u;
        (0..n).map(|i| {
    let s = &self.openings[i];
    create_vole_from_material_expanded::<B, _, _, _>(s, &mut f)
}).collect::<Vec<_>>()
    }
}

impl<B: ByteBlockEncrypt, D: Digest> ABODyn<B, D> {
    pub fn to_vole_material(&self, n: usize) -> Vec<VopeDyn<u8>> {
        let k = self.k;
        (0..n).map(|i| {
    let s = &self.per_byte[((i * n) .. 0)][(0 .. n)];
    create_vole_from_material::<B, _>(s)
}).collect::<Vec<_>>()
    }
    pub fn to_vole_material_typenum(&self, n: usize) -> Vec<VopeDyn<u8>> {
        let k = self.k;
        (0..n).map(|i| {
    let s = &self.per_byte[((i * n) .. 0)][(0 .. n)];
    create_vole_from_material::<B, _>(s)
}).collect::<Vec<_>>()
    }
    pub fn to_vole_material_expanded<X: AsRef<[u8]>, F: FnMut(&[u8]) -> X>(&self, n: usize, mut f: F) -> Vec<VopeDyn<u8>> {
        let k = self.k;
        (0..n).map(|i| {
    let s = &self.per_byte[((i * n) .. 0)][(0 .. n)];
    create_vole_from_material_expanded::<B, _, _, _>(s, &mut f)
}).collect::<Vec<_>>()
    }
    pub fn to_vole_material_typenum_expanded<X: AsRef<[u8]>, F: FnMut(&[u8]) -> X>(&self, n: usize, mut f: F) -> Vec<VopeDyn<u8>> {
        let k = self.k;
        (0..n).map(|i| {
    let s = &self.per_byte[((i * n) .. 0)][(0 .. n)];
    create_vole_from_material_expanded::<B, _, _, _>(s, &mut f)
}).collect::<Vec<_>>()
    }
}

pub fn create_vole_from_material<B: ByteBlockEncrypt, X: AsRef<[u8]>>(mut s: &[X]) -> VopeDyn<u8> {
    let u: Vec<u8> = s.iter().fold(Vec::<u8>::new(), |a, b| {
    a.into_iter().zip((0..<B::BlockSize as typenum::Unsigned>::USIZE).map(|i| b.as_ref()[i]).collect::<Vec<_>>().into_iter()).map(|(a, b)| a.bitxor(b)).collect::<Vec<_>>()
});
    let v: Vec<u8> = s.iter().enumerate().fold(Vec::<u8>::new(), |a, (i, b)| {
    a.into_iter().zip((0..<B::BlockSize as typenum::Unsigned>::USIZE).map(|i| b.as_ref()[i]).collect::<Vec<_>>().into_iter()).map(|(a, b)| a.bitxor(b).bitxor((i as u8))).collect::<Vec<_>>()
});
    VopeDyn { u: (0..1).map(|_| u.clone()).collect::<Vec<_>>(), v: v, n: <<B as cipher::BlockSizeUser>::BlockSize as typenum::Unsigned>::USIZE, k: 1 }
}

pub fn create_vole_from_material_expanded<B: ByteBlockEncrypt, X: AsRef<[u8]>, Y: AsRef<[u8]>, F: FnMut(&[u8]) -> X>(mut s: &[Y], mut f: F) -> VopeDyn<u8> {
    let u: Vec<u8> = s.iter().into_iter().map(|b| f(&b.as_ref()[(0 .. <B::BlockSize as typenum::Unsigned>::USIZE)])).collect::<Vec<_>>().into_iter().fold(Vec::<u8>::new(), |a, b| {
    a.into_iter().zip((0..<B::BlockSize as typenum::Unsigned>::USIZE).map(|i| b.as_ref()[i]).collect::<Vec<_>>().into_iter()).map(|(a, b)| a.bitxor(b)).collect::<Vec<_>>()
});
    let v: Vec<u8> = s.iter().into_iter().map(|b| f(&b.as_ref()[(0 .. <B::BlockSize as typenum::Unsigned>::USIZE)])).collect::<Vec<_>>().into_iter().enumerate().fold(Vec::<u8>::new(), |a, (i, b)| {
    a.into_iter().zip((0..<B::BlockSize as typenum::Unsigned>::USIZE).map(|i| b.as_ref()[i]).collect::<Vec<_>>().into_iter()).map(|(a, b)| a.bitxor(b).bitxor((i as u8))).collect::<Vec<_>>()
});
    VopeDyn { u: (0..1).map(|_| u.clone()).collect::<Vec<_>>(), v: v, n: <<B as cipher::BlockSizeUser>::BlockSize as typenum::Unsigned>::USIZE, k: 1 }
}

pub fn double<B: ByteBlockEncrypt>(mut a: Vec<u8>) -> Vec<Vec<u8>> {
    return (0..2).map(|i| {
    let mut out = a.clone();
    let mut b = B::from([(i as u8); 32]);
    b.encrypt_block(cipher::Block::<B>::from_mut_slice(&mut out));
    out
}).collect::<Vec<_>>();
}

