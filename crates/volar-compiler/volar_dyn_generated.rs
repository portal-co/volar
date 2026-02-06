//! Auto-generated dynamic types from volar-spec
//! Type-level lengths have been converted to runtime usize witnesses

#![allow(unused_variables, dead_code, unused_mut, unused_imports, non_snake_case, unused_parens)]
extern crate alloc;
use alloc::vec::Vec;
use alloc::vec;
use core::ops::{Add, Sub, Mul, Div, BitAnd, BitOr, BitXor, Shl, Shr};
use core::marker::PhantomData;
use typenum::Unsigned;
use cipher::{BlockEncrypt, Block};
use digest::Digest;
use volar_common::hash_commitment::commit;
use volar_primitives::{Bit, BitsInBytes, BitsInBytes64, Galois, Galois64};

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

#[derive(Debug, Default)]
pub struct ABODyn<B: LengthDoubler, D: Digest> {
    pub k: usize,
    pub commit: Vec<u8>,
    pub per_byte: Vec<Vec<u8>>,
    pub _phantom: PhantomData<(B, D)>,
}

#[derive(Debug, Default)]
pub struct ABOOpeningDyn<B: LengthDoubler, D: Digest> {
    pub t: usize,
    pub u: usize,
    pub bad: Vec<u64>,
    pub openings: Vec<Vec<Vec<u8>>>,
    pub _phantom: PhantomData<(B, D)>,
}

#[derive(Debug, Default)]
pub struct BSplitDyn<B: LengthDoubler, D: Digest> {
    pub split: Vec<Vec<Vec<u8>>>,
    pub _phantom: PhantomData<(B, D)>,
}

#[derive(Debug, Default)]
pub struct PolyDyn<T> {
    pub n: usize,
    pub c0: T,
    pub c1: Vec<T>,
}

#[derive(Debug, Default)]
pub struct PolyInputPoolDyn<T> {
    pub n: usize,
    pub x: usize,
    pub inputs: Vec<T>,
    pub indices: Vec<Vec<usize>>,
}

#[derive(Debug, Default)]
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

#[derive(Debug, Default)]
pub struct ViaDigestPuncturableRandomizerDyn<D: Digest> {
    pub digest: D,
}

pub trait LengthDoubler {
    type OutputSize;
    fn double(a: Vec<u8>) -> Vec<Vec<u8>>;
}

pub trait PuncturableLengthDoubler: LengthDoubler {
}

impl <T> DeltaDyn<T> {
    pub fn remap<F: FnMut(usize) -> usize>(&self, mut m: usize, mut f: F) -> DeltaDyn<T> where T: Clone
    {
        let n: usize = self.n;
        let Self { delta: delta, .. } = self;
        DeltaDyn { delta: (0..m).map(|i| delta[(f(i) % n)].clone()).collect::<Vec<_>>(), n: 0 }
    }
    pub fn rotate_left(&self, mut n: usize) -> Self where T: Clone
    {
        let n: usize = self.n;
        self.remap(n, |a| a.wrapping_sub(n))
    }
    pub fn rotate_right(&self, mut n: usize) -> Self where T: Clone
    {
        let n: usize = self.n;
        self.remap(n, |a| a.wrapping_add(n))
    }
    pub fn r#static<U: Mul<T, Output = O> + Clone, O>(&self, mut val: Vec<U>) -> QDyn<O> where T: Clone
    {
        let n: usize = self.n;
        QDyn { q: (0..n).map(|i| (val[i].clone() * self.delta[i].clone())).collect::<Vec<_>>(), n: 0 }
    }
}

impl <T> QDyn<T> {
    pub fn remap<F: FnMut(usize) -> usize>(&self, mut m: usize, mut f: F) -> QDyn<T> where T: Clone
    {
        let n: usize = self.n;
        let Self { q: q, .. } = self;
        QDyn { q: (0..m).map(|i| q[(f(i) % n)].clone()).collect::<Vec<_>>(), n: 0 }
    }
    pub fn rotate_left(&self, mut n: usize) -> Self where T: Clone
    {
        let n: usize = self.n;
        self.remap(n, |a| a.wrapping_sub(n))
    }
    pub fn rotate_right(&self, mut n: usize) -> Self where T: Clone
    {
        let n: usize = self.n;
        self.remap(n, |a| a.wrapping_add(n))
    }
}

impl <T> PolyDyn<T> {
    pub fn get_qs_pool<Q: Clone + Mul<A, Output = A>, A: Add<A, Output = A>>(&self, mut m: usize, mut x: usize, mut root: DeltaDyn<Q>, mut inputs: PolyInputPoolDyn<QDyn<Q>>, mut reduction: usize) -> QDyn<A> where T: Clone + Into<A>
    {
        let n: usize = self.n;
        QDyn { q: (0..m).map(|i| {
    let mut sum: A = self.c0.clone().into();
    for _ in 0.. n{
    sum = (root.delta[i].clone() * sum);
};
    for j in 0.. n{
    let mut b: A = self.c1[j].clone().into();
    for i2 in inputs.indices.iter(){
    for _ in 0.. reduction{
    b = (inputs.inputs[i2[j]].q[i].clone() * b);
}
};
    sum = (sum + b);
};
    sum
}).collect::<Vec<_>>(), n: 0 }
    }
    pub fn get_qs<Q: Clone + Mul<A, Output = A>, A: Add<A, Output = A>>(&self, mut m: usize, mut x: usize, mut root: DeltaDyn<Q>, mut inputs: Vec<Vec<QDyn<Q>>>, mut reduction: usize) -> QDyn<A> where T: Clone + Into<A>
    {
        let n: usize = self.n;
        QDyn { q: (0..m).map(|i| {
    let mut sum: A = self.c0.clone().into();
    for _ in 0.. n{
    sum = (root.delta[i].clone() * sum);
};
    for j in 0.. n{
    let mut b: A = self.c1[j].clone().into();
    for i2 in inputs.iter(){
    for _ in 0.. reduction{
    b = (i2[j].q[i].clone() * b);
}
};
    sum = (sum + b);
};
    sum
}).collect::<Vec<_>>(), n: 0 }
    }
    pub fn apply_pool<M, O: Mul<O, Output = O> + Add<O, Output = O> + Default + Clone>(&self, mut m: usize, mut x: usize, mut x2: usize, mut xs: usize, mut s: usize, mut voles: &PolyInputPoolDyn<VopeDyn<T>>) -> VopeDyn<O> where T: Into<O> + Clone
    {
        let n: usize = self.n;
        let v = (0..m).map(|i| {
    let mut sum = O::default();
    for k in 0.. n{
    let mut b: O = self.c1[k].clone().into();
    for v in compile_error!("Unsupported expression in printer: Unary { op: Ref, expr: Field { base: Var('voles'), field: 'indices' } }"){
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
    for k in 0.. n{
    for n in 0.. x{
    let mut b: O = self.c1[k].clone().into();
    for m in 0.. s{
    let l = ((l * s) + m);
    for (idx, v) in voles.indices.iter().enumerate(){
    b = (b * if (idx == n){
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
        return VopeDyn { u: u, v: v, n: 0, k: 1 };
    }
    pub fn apply<M, O: Mul<O, Output = O> + Add<O, Output = O> + Default + Clone>(&self, mut m: usize, mut x: usize, mut x2: usize, mut xs: usize, mut s: usize, mut voles: Vec<Vec<VopeDyn<T>>>) -> VopeDyn<O> where T: Into<O> + Clone
    {
        let n: usize = self.n;
        let v = (0..m).map(|i| {
    let mut sum = O::default();
    for k in 0.. n{
    let mut b: O = self.c1[k].clone().into();
    for v in compile_error!("Unsupported expression in printer: Unary { op: Ref, expr: Var('voles') }"){
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
    for k in 0.. n{
    for n in 0.. x{
    let mut b: O = self.c1[k].clone().into();
    for m in 0.. s{
    let l = ((l * s) + m);
    for (idx, v) in voles.iter().enumerate(){
    b = (b * if (idx == n){
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
        return VopeDyn { u: u, v: v, n: 0, k: 1 };
    }
}

impl <T: Add<Output = T> + Mul<Output = T> + Default + Clone> VopeDyn<T> where T: Add<Output = T> + Mul<Output = T> + Default + Clone {
    pub fn mul_generalized(&self, mut k2: usize, mut other: &VopeDyn<T>) -> VopeDyn<T>
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let mut res_u = vec![Vec<T>::default(); k2_output];
        let mut res_v = vec![T::default(); n];
        for i in 0..= k{
    for j in 0..= k2{
    let k = (i + j);
    let a_coeff = if (i == 0){
    compile_error!("Unsupported expression in printer: Unary { op: Ref, expr: Field { base: Var('self'), field: 'v' } }")
} else {
    compile_error!("Unsupported expression in printer: Unary { op: Ref, expr: Index { base: Field { base: Var('self'), field: 'u' }, index: Binary { op: Sub, left: Var('i'), right: Lit(Int(1)) } } }")
};
    let b_coeff = if (j == 0){
    compile_error!("Unsupported expression in printer: Unary { op: Ref, expr: Field { base: Var('other'), field: 'v' } }")
} else {
    compile_error!("Unsupported expression in printer: Unary { op: Ref, expr: Index { base: Field { base: Var('other'), field: 'u' }, index: Binary { op: Sub, left: Var('j'), right: Lit(Int(1)) } } }")
};
    if (k == 0){
    for lane in 0.. n{
    res_v[lane] = (res_v[lane].clone() + (a_coeff[lane].clone() * b_coeff[lane].clone()));
}
} else {
    for lane in 0.. n{
    res_u[(k - 1)][lane] = (res_u[(k - 1)][lane].clone() + (a_coeff[lane].clone() * b_coeff[lane].clone()));
}
}
}
};
        VopeDyn { u: res_u, v: res_v, n: 0, k: 1 }
    }
}

impl  VopeDyn<BitsInBytes> {
    pub fn rotate_left_bits(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        let k: usize = self.k;
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
}).collect::<Vec<_>>(), n: 0, k: 1 }
    }
    pub fn rotate_right_bits(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        let k: usize = self.k;
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
}).collect::<Vec<_>>(), n: 0, k: 1 }
    }
    pub fn bit(&self, mut n: u8) -> VopeDyn<Bit>
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n).map(|i| {
    let BitsInBytes(b) = u[l][i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect::<Vec<_>>()
}).collect::<Vec<_>>(), v: (0..n).map(|i| {
    let BitsInBytes(b) = v[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect::<Vec<_>>(), n: 0, k: 1 }
    }
}

impl  VopeDyn<BitsInBytes64> {
    pub fn rotate_left_bits(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        let k: usize = self.k;
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
}).collect::<Vec<_>>(), n: 0, k: 1 }
    }
    pub fn rotate_right_bits(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        let k: usize = self.k;
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
}).collect::<Vec<_>>(), n: 0, k: 1 }
    }
    pub fn bit(&self, mut n: u8) -> VopeDyn<Bit>
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n).map(|i| {
    let BitsInBytes64(b) = u[l][i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect::<Vec<_>>()
}).collect::<Vec<_>>(), v: (0..n).map(|i| {
    let BitsInBytes64(b) = v[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect::<Vec<_>>(), n: 0, k: 1 }
    }
}

impl <T: Clone> Clone for VopeDyn<T> {
    fn clone(&self) -> Self
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n).map(|i| u[l][i].clone()).collect::<Vec<_>>()
}).collect::<Vec<_>>(), v: (0..n).map(|i| v[i].clone()).collect::<Vec<_>>(), n: 0, k: 1 }
    }
}

impl <T: Clone> Clone for QDyn<T> {
    fn clone(&self) -> Self
    {
        let n: usize = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n).map(|i| q[i].clone()).collect::<Vec<_>>(), n: 0 }
    }
}

impl <T: Clone> Clone for DeltaDyn<T> {
    fn clone(&self) -> Self
    {
        let n: usize = self.n;
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n).map(|i| delta[i].clone()).collect::<Vec<_>>(), n: 0 }
    }
}

impl <T: PartialEq> PartialEq for VopeDyn<T> {
    fn eq(&self, mut other: &Self) -> bool
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let VopeDyn { u: u1, v: v1, .. } = self;
        let VopeDyn { u: u2, v: v2, .. } = other;
        for l in 0.. k{
    for i in 0.. n{
    if (u1[l][i] != u2[l][i]){
    return false;
}
}
};
        for i in 0.. n{
    if (v1[i] != v2[i]){
    return false;
}
};
        true
    }
}

impl <T: PartialEq> PartialEq for QDyn<T> {
    fn eq(&self, mut other: &Self) -> bool
    {
        let n: usize = self.n;
        let QDyn { q: q1, .. } = self;
        let QDyn { q: q2, .. } = other;
        for i in 0.. n{
    if (q1[i] != q2[i]){
    return false;
}
};
        true
    }
}

impl <T: PartialEq> PartialEq for DeltaDyn<T> {
    fn eq(&self, mut other: &Self) -> bool
    {
        let n: usize = self.n;
        let DeltaDyn { delta: d1, .. } = self;
        let DeltaDyn { delta: d2, .. } = other;
        for i in 0.. n{
    if (d1[i] != d2[i]){
    return false;
}
};
        true
    }
}

impl <T: Eq> Eq for VopeDyn<T> {
}

impl <T: Eq> Eq for QDyn<T> {
}

impl <T: Eq> Eq for DeltaDyn<T> {
}

impl  QDyn<BitsInBytes> {
    pub fn rotate_left_bits(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n).map(|i| {
    let BitsInBytes(b) = q[i].clone();
    let BitsInBytes(next) = q[((i + 1) % n)].clone();
    BitsInBytes((b.shl((n as u32)) | next.shr((8 - (n as u32)))))
}).collect::<Vec<_>>(), n: 0 }
    }
    pub fn rotate_right_bits(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n).map(|i| {
    let BitsInBytes(prev) = q[(((i + n) - 1) % n)].clone();
    let BitsInBytes(b) = q[i].clone();
    BitsInBytes((prev.shl((8 - (n as u32))) | b.shr((n as u32))))
}).collect::<Vec<_>>(), n: 0 }
    }
    pub fn bit(&self, mut n: u8) -> QDyn<Bit>
    {
        let n: usize = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n).map(|i| {
    let BitsInBytes(b) = q[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect::<Vec<_>>(), n: 0 }
    }
}

impl  QDyn<BitsInBytes64> {
    pub fn rotate_left_bits(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n).map(|i| {
    let BitsInBytes64(b) = q[i].clone();
    let BitsInBytes64(next) = q[((i + 1) % n)].clone();
    BitsInBytes64((b.shl((n as u32)) | next.shr((64 - (n as u32)))))
}).collect::<Vec<_>>(), n: 0 }
    }
    pub fn rotate_right_bits(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n).map(|i| {
    let BitsInBytes64(prev) = q[(((i + n) - 1) % n)].clone();
    let BitsInBytes64(b) = q[i].clone();
    BitsInBytes64((prev.shl((64 - (n as u32))) | b.shr((n as u32))))
}).collect::<Vec<_>>(), n: 0 }
    }
    pub fn bit(&self, mut n: u8) -> QDyn<Bit>
    {
        let n: usize = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n).map(|i| {
    let BitsInBytes64(b) = q[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect::<Vec<_>>(), n: 0 }
    }
}

impl  DeltaDyn<BitsInBytes> {
    pub fn rotate_left_bits(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let BitsInBytes(b) = delta[i].clone();
    let BitsInBytes(next) = delta[((i + 1) % n)].clone();
    BitsInBytes((b.shl((n as u32)) | next.shr((8 - (n as u32)))))
}).collect::<Vec<_>>(), n: 0 }
    }
    pub fn rotate_right_bits(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let BitsInBytes(prev) = delta[(((i + n) - 1) % n)].clone();
    let BitsInBytes(b) = delta[i].clone();
    BitsInBytes((prev.shl((8 - (n as u32))) | b.shr((n as u32))))
}).collect::<Vec<_>>(), n: 0 }
    }
    pub fn bit(&self, mut n: u8) -> DeltaDyn<Bit>
    {
        let n: usize = self.n;
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let BitsInBytes(b) = delta[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect::<Vec<_>>(), n: 0 }
    }
}

impl  DeltaDyn<BitsInBytes64> {
    pub fn rotate_left_bits(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let BitsInBytes64(b) = delta[i].clone();
    let BitsInBytes64(next) = delta[((i + 1) % n)].clone();
    BitsInBytes64((b.shl((n as u32)) | next.shr((64 - (n as u32)))))
}).collect::<Vec<_>>(), n: 0 }
    }
    pub fn rotate_right_bits(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let BitsInBytes64(prev) = delta[(((i + n) - 1) % n)].clone();
    let BitsInBytes64(b) = delta[i].clone();
    BitsInBytes64((prev.shl((64 - (n as u32))) | b.shr((n as u32))))
}).collect::<Vec<_>>(), n: 0 }
    }
    pub fn bit(&self, mut n: u8) -> DeltaDyn<Bit>
    {
        let n: usize = self.n;
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let BitsInBytes64(b) = delta[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect::<Vec<_>>(), n: 0 }
    }
}

impl <T> VopeDyn<T> {
    pub fn constant(mut n: usize, mut v: Vec<T>) -> Self
    {
        let k: usize = 0;
        VopeDyn { u: (0..0).map(|_| unreachable!()).collect::<Vec<_>>(), v: v, n: 0, k: 1 }
    }
}

impl <T: Add<U, Output = O> + Clone, U: Clone, O> Add<VopeDyn<U>> for VopeDyn<T> {
    type Output = VopeDyn<O>;
    fn add(self, mut rhs: VopeDyn<U>) -> Self::Output
    {
        let n: usize = self.n;
        let k: usize = self.k;
        VopeDyn { u: self.u.zip(rhs.u, |a, b| a.zip(b, |a, b| (a + b))), v: self.v.zip(rhs.v, |a, b| (a + b)), n: 0, k: 1 }
    }
}

impl <T: BitXor<U, Output = O> + Clone + Into<O>, U: Clone, O> BitXor<Vec<U>> for VopeDyn<T> where T: Into<O> {
    type Output = VopeDyn<O>;
    fn bitxor(self, mut rhs: Vec<U>) -> Self::Output
    {
        let n: usize = self.n;
        let k: usize = self.k;
        VopeDyn { u: (0..k).map(|i| {
    (0..n).map(|j| {
    let o: O = self.u[i][j].clone().bitxor(rhs[((i * k) + j)].clone());
    o
}).collect::<Vec<_>>()
}).collect::<Vec<_>>(), v: self.v.map(|a| a.into()), n: 0, k: 1 }
    }
}

impl <T: Mul<U, Output = O> + Into<O> + Clone, U: Mul<U, Output = U> + Clone, O: Add<O, Output = O>> Mul<DeltaDyn<U>> for VopeDyn<T> {
    type Output = QDyn<O>;
    fn mul(self, mut rhs: DeltaDyn<U>) -> Self::Output
    {
        let n: usize = self.n;
        let k: usize = self.k;
        QDyn { q: self.u.iter().enumerate().fold(self.v.clone().map(|a| a.into()), |a, _| {
    a.zip(b, |a, b| {
    let mut x = rhs.delta[i].clone();
    for _ in 0.. i{
    x = (x * rhs.delta[i].clone());
};
    let m: O = (b.clone() * x);
    (m + a)
})
}), n: 0 }
    }
}

impl <T> VopeDyn<T> {
    pub fn expand(&self, mut l: usize) -> VopeDyn<T> where T: Clone + Default
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let Self { u: u, v: v, .. } = self;
        VopeDyn { u: (0..l).map(|l| {
    (0..n).map(|i| u.get(l).map_or(T::default(), |a| a[i].clone())).collect::<Vec<_>>()
}).collect::<Vec<_>>(), v: v.clone(), n: 0, k: 1 }
    }
    pub fn rotate_left(&self, mut n: usize) -> Self where T: Clone
    {
        let n: usize = self.n;
        let k: usize = self.k;
        self.remap(n, |a| a.wrapping_sub(n))
    }
    pub fn rotate_right(&self, mut n: usize) -> Self where T: Clone
    {
        let n: usize = self.n;
        let k: usize = self.k;
        self.remap(n, |a| a.wrapping_add(n))
    }
    pub fn remap<F: FnMut(usize) -> usize>(&self, mut m: usize, mut f: F) -> VopeDyn<T> where T: Clone
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let Self { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..m).map(|i| u[l][(f(i) % n)].clone()).collect::<Vec<_>>()
}).collect::<Vec<_>>(), v: (0..m).map(|i| v[(f(i) % n)].clone()).collect::<Vec<_>>(), n: 0, k: 1 }
    }
}

impl  VopeDyn<Bit> {
    pub fn scale<T>(self, mut f: impl FnMut(bool) -> T) -> VopeDyn<T>
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n).map(|i| {
    let Bit(b) = u[l][i].clone();
    f(b)
}).collect::<Vec<_>>()
}).collect::<Vec<_>>(), v: (0..n).map(|i| {
    let Bit(b) = v[i].clone();
    f(b)
}).collect::<Vec<_>>(), n: 0, k: 1 }
    }
}

impl <D: Digest> LengthDoubler for ViaDigestPuncturableRandomizerDyn<D> {
    type OutputSize = <D as _>::OutputSize;
    fn double(mut a: Vec<u8>) -> Vec<Vec<u8>>
    {
        let v = D::digest(compile_error!("Unsupported expression in printer: Unary { op: Ref, expr: Var('a') }"));
        vec![v.clone(), v.zip(a, |x, y| (x ^ y))]
    }
}

impl <D: Digest> PuncturableLengthDoubler for ViaDigestPuncturableRandomizerDyn<D> {
}

impl <B: LengthDoubler, D: Digest> ABODyn<B, D> {
    pub fn open<R: AsRef<[u8]>>(&self, mut t: usize, mut u: usize, mut m: usize, mut bad: Vec<u64>, mut rand: &R) -> ABOOpeningDyn<B, D>
    {
        let k: usize = self.k;
        ABOOpeningDyn { bad: bad.clone(), openings: (0..t).map(|i| {
    let bad = bad.clone();
    (0..u).map(|j| {
    let i2 = (i | ((j as usize) << t.ilog2()));
    if bad.contains(compile_error!("Unsupported expression in printer: Unary { op: Ref, expr: Cast { expr: Var('i2'), ty: Primitive(U64) } }")){
    let h = commit::<D>(compile_error!("Unsupported expression in printer: Unary { op: Ref, expr: Index { base: Field { base: Var('self'), field: 'per_byte' }, index: Var('i2') } }"), rand);
    (0..m).map(|j| {
    h.as_ref().get(j).cloned().unwrap_or_default()
}).collect::<Vec<_>>()
} else {
    (0..m).map(|j| {
    self.per_byte[i2].get(j).cloned().unwrap_or_default()
}).collect::<Vec<_>>()
}
}).collect::<Vec<_>>()
}).collect::<Vec<_>>(), t: 0, u: 0, _phantom: PhantomData }
    }
}

impl <B: LengthDoubler, D: Digest> ABOOpeningDyn<B, D> {
    pub fn validate<R: AsRef<[u8]>>(&self, mut commit_: &Vec<u8>, mut rand: &R) -> bool
    {
        let t: usize = self.t;
        let u: usize = self.u;
        let mut h = D::new();
        for i in 0.. t{
    for b in 0.. u{
    let i2 = (i | ((b as usize) << t.ilog2()));
    if self.bad.contains(compile_error!("Unsupported expression in printer: Unary { op: Ref, expr: Cast { expr: Var('i2'), ty: Primitive(U64) } }")){
    h.update(compile_error!("Unsupported expression in printer: Unary { op: Ref, expr: Index { base: Index { base: Index { base: Field { base: Var('self'), field: 'openings' }, index: Var('i') }, index: Var('b') }, index: Range { start: None, end: Some(Var('d_outputsize')), inclusive: false } } }"));
} else {
    h.update(compile_error!("Unsupported expression in printer: Unary { op: Ref, expr: Call { func: Path { segments: ['commit'], type_args: [TypeParam('D')] }, args: [Unary { op: Ref, expr: Unary { op: Ref, expr: Index { base: Index { base: Index { base: Field { base: Var('self'), field: 'openings' }, index: Var('i') }, index: Var('b') }, index: Range { start: None, end: Some(Var('b_outputsize')), inclusive: false } } } }, Var('rand')] } }"));
}
}
};
        (h.finalize().as_slice() == commit_.as_slice())
    }
    pub fn to_vole_material(&self, mut n: usize) -> Vec<VopeDyn<u8>>
    {
        let t: usize = self.t;
        let u: usize = self.u;
        (0..n).map(|i| {
    let s = compile_error!("Unsupported expression in printer: Unary { op: Ref, expr: Index { base: Field { base: Var('self'), field: 'openings' }, index: Var('i') } }");
    create_vole_from_material::<B, _>(s)
}).collect::<Vec<_>>()
    }
    pub fn to_vole_material_typenum(&self, mut n: usize) -> Vec<VopeDyn<u8>>
    {
        let t: usize = self.t;
        let u: usize = self.u;
        (0..n).map(|i| {
    let s = compile_error!("Unsupported expression in printer: Unary { op: Ref, expr: Index { base: Field { base: Var('self'), field: 'openings' }, index: Var('i') } }");
    create_vole_from_material::<B, _>(s)
}).collect::<Vec<_>>()
    }
    pub fn to_vole_material_expanded<X: AsRef<[u8]>, F: FnMut(&[u8]) -> X>(&self, mut n: usize, mut f: F) -> Vec<VopeDyn<u8>>
    {
        let t: usize = self.t;
        let u: usize = self.u;
        (0..n).map(|i| {
    let s = compile_error!("Unsupported expression in printer: Unary { op: Ref, expr: Index { base: Field { base: Var('self'), field: 'openings' }, index: Var('i') } }");
    create_vole_from_material_expanded::<B, _, _, _>(s, compile_error!("Unsupported expression in printer: Unary { op: RefMut, expr: Var('f') }"))
}).collect::<Vec<_>>()
    }
    pub fn to_vole_material_typenum_expanded<X: AsRef<[u8]>, F: FnMut(&[u8]) -> X>(&self, mut n: usize, mut f: F) -> Vec<VopeDyn<u8>>
    {
        let t: usize = self.t;
        let u: usize = self.u;
        (0..n).map(|i| {
    let s = compile_error!("Unsupported expression in printer: Unary { op: Ref, expr: Index { base: Field { base: Var('self'), field: 'openings' }, index: Var('i') } }");
    create_vole_from_material_expanded::<B, _, _, _>(s, compile_error!("Unsupported expression in printer: Unary { op: RefMut, expr: Var('f') }"))
}).collect::<Vec<_>>()
    }
    pub fn split_bit_typenum(&self, mut n: usize) -> Vec<BSplitDyn<B, D>> where D: Digest
    {
        let t: usize = self.t;
        let u: usize = self.u;
        (0..n).map(|i| {
    let s = compile_error!("Unsupported expression in printer: Unary { op: Ref, expr: Index { base: Field { base: Var('self'), field: 'openings' }, index: Var('i') } }");
    BSplitDyn { split: (0..self_output).map(|j| {
    (0..n).map(|b| {
    s.iter().enumerate().filter_map(|_| {
    if (((a >> j) & 1) == b){
    Some(c.clone())
} else {
    None
}
}).fold(vec![u8::default(); b_outputsize], |a, b| {
    a.zip((0..b_outputsize).map(|i| b.as_ref()[i]).collect::<Vec<_>>(), |a, b| a.bitxor(b))
})
}).collect::<Vec<_>>()
}).collect::<Vec<_>>(), _phantom: PhantomData }
}).collect::<Vec<_>>()
    }
}

impl <B: LengthDoubler, D: Digest> ABODyn<B, D> {
    pub fn to_vole_material(&self, mut n: usize) -> Vec<VopeDyn<u8>>
    {
        let k: usize = self.k;
        (0..n).map(|i| {
    let s = compile_error!("Unsupported expression in printer: Unary { op: Ref, expr: Index { base: Index { base: Field { base: Var('self'), field: 'per_byte' }, index: Range { start: Some(Binary { op: Mul, left: Var('i'), right: Var('n') }), end: None, inclusive: false } }, index: Range { start: None, end: Some(Var('n')), inclusive: false } } }");
    create_vole_from_material::<B, _>(s)
}).collect::<Vec<_>>()
    }
    pub fn to_vole_material_typenum(&self, mut n: usize) -> Vec<VopeDyn<u8>>
    {
        let k: usize = self.k;
        (0..n).map(|i| {
    let s = compile_error!("Unsupported expression in printer: Unary { op: Ref, expr: Index { base: Index { base: Field { base: Var('self'), field: 'per_byte' }, index: Range { start: Some(Binary { op: Mul, left: Var('i'), right: Var('n') }), end: None, inclusive: false } }, index: Range { start: None, end: Some(Var('n')), inclusive: false } } }");
    create_vole_from_material::<B, _>(s)
}).collect::<Vec<_>>()
    }
    pub fn to_vole_material_expanded<X: AsRef<[u8]>, F: FnMut(&[u8]) -> X>(&self, mut n: usize, mut f: F) -> Vec<VopeDyn<u8>>
    {
        let k: usize = self.k;
        (0..n).map(|i| {
    let s = compile_error!("Unsupported expression in printer: Unary { op: Ref, expr: Index { base: Index { base: Field { base: Var('self'), field: 'per_byte' }, index: Range { start: Some(Binary { op: Mul, left: Var('i'), right: Var('n') }), end: None, inclusive: false } }, index: Range { start: None, end: Some(Var('n')), inclusive: false } } }");
    create_vole_from_material_expanded::<B, _, _, _>(s, compile_error!("Unsupported expression in printer: Unary { op: RefMut, expr: Var('f') }"))
}).collect::<Vec<_>>()
    }
    pub fn to_vole_material_typenum_expanded<X: AsRef<[u8]>, F: FnMut(&[u8]) -> X>(&self, mut n: usize, mut f: F) -> Vec<VopeDyn<u8>>
    {
        let k: usize = self.k;
        (0..n).map(|i| {
    let s = compile_error!("Unsupported expression in printer: Unary { op: Ref, expr: Index { base: Index { base: Field { base: Var('self'), field: 'per_byte' }, index: Range { start: Some(Binary { op: Mul, left: Var('i'), right: Var('n') }), end: None, inclusive: false } }, index: Range { start: None, end: Some(Var('n')), inclusive: false } } }");
    create_vole_from_material_expanded::<B, _, _, _>(s, compile_error!("Unsupported expression in printer: Unary { op: RefMut, expr: Var('f') }"))
}).collect::<Vec<_>>()
    }
    pub fn split_bit_typenum(&self, mut n: usize) -> Vec<BSplitDyn<B, D>> where D: Digest
    {
        let k: usize = self.k;
        (0..n).map(|i| {
    let s = compile_error!("Unsupported expression in printer: Unary { op: Ref, expr: Index { base: Index { base: Field { base: Var('self'), field: 'per_byte' }, index: Range { start: Some(Binary { op: Mul, left: Var('i'), right: Var('n') }), end: None, inclusive: false } }, index: Range { start: None, end: Some(Var('n')), inclusive: false } } }");
    BSplitDyn { split: (0..self_output).map(|j| {
    (0..n).map(|b| {
    s.iter().enumerate().filter_map(|_| {
    if (((a >> j) & 1) == b){
    Some(c.clone())
} else {
    None
}
}).fold(vec![u8::default(); b_outputsize], |a, b| {
    a.zip((0..b_outputsize).map(|i| b.as_ref()[i]).collect::<Vec<_>>(), |a, b| a.bitxor(b))
})
}).collect::<Vec<_>>()
}).collect::<Vec<_>>(), _phantom: PhantomData }
}).collect::<Vec<_>>()
    }
}

pub fn gen_abo<B: LengthDoubler, D: Digest>(mut k: usize, mut a: Vec<u8>, mut rand: &impl AsRef<[u8]>) -> ABODyn<B, D> where B: Sized
{
    let mut h = D::new();
    let mut per_byte = vec![Default::default(); n];
    for i in 0.. k{
    let core = (0..k.ilog2()).fold(a.clone(), |acc, b| {
    if (((i >> b) & 1) != 0){
    let doubled = B::double(acc);
    acc = doubled[1].clone();
} else {
    let doubled = B::double(acc);
    acc = doubled[0].clone();
};
    acc
});
    h.update(compile_error!("Unsupported expression in printer: Unary { op: Ref, expr: Call { func: Path { segments: ['commit'], type_args: [TypeParam('D')] }, args: [Unary { op: Ref, expr: Var('core') }, Var('rand')] } }"));
    per_byte[i] = core;
};
    ABODyn { commit: h.finalize(), per_byte: per_byte, k: 0, _phantom: PhantomData }
}

pub fn create_vole_from_material<B: LengthDoubler, X: AsRef<[u8]>>(mut s: &[X]) -> VopeDyn<u8>
{
    let u: Vec<u8> = s.iter().fold(vec![u8::default(); b_outputsize], |a, b| {
    a.zip((0..b_outputsize).map(|i| b.as_ref()[i]).collect::<Vec<_>>(), |a, b| a.bitxor(b))
});
    let v: Vec<u8> = s.iter().enumerate().fold(vec![u8::default(); b_outputsize], |a, _| {
    a.zip((0..b_outputsize).map(|i| b.as_ref()[i]).collect::<Vec<_>>(), |a, b| a.bitxor(b).bitxor((i as u8)))
});
    VopeDyn { u: (0..1).map(|_| u.clone()).collect::<Vec<_>>(), v: v, n: 0, k: 1 }
}

pub fn create_vole_from_material_expanded<B: LengthDoubler, X: AsRef<[u8]>, Y: AsRef<[u8]>, F: FnMut(&[u8]) -> X>(mut s: &[Y], mut f: F) -> VopeDyn<u8>
{
    let u: Vec<u8> = s.iter().map(|b| f(compile_error!("Unsupported expression in printer: Unary { op: Ref, expr: Index { base: MethodCall { receiver: Var('b'), method: Std('as_ref'), type_args: [], args: [] }, index: Range { start: None, end: Some(Var('b_outputsize')), inclusive: false } } }"))).fold(vec![u8::default(); b_outputsize], |a, b| {
    a.zip((0..b_outputsize).map(|i| b.as_ref()[i]).collect::<Vec<_>>(), |a, b| a.bitxor(b))
});
    let v: Vec<u8> = s.iter().map(|b| f(compile_error!("Unsupported expression in printer: Unary { op: Ref, expr: Index { base: MethodCall { receiver: Var('b'), method: Std('as_ref'), type_args: [], args: [] }, index: Range { start: None, end: Some(Var('b_outputsize')), inclusive: false } } }"))).enumerate().fold(vec![u8::default(); b_outputsize], |a, _| {
    a.zip((0..b_outputsize).map(|i| b.as_ref()[i]).collect::<Vec<_>>(), |a, b| a.bitxor(b).bitxor((i as u8)))
});
    VopeDyn { u: (0..1).map(|_| u.clone()).collect::<Vec<_>>(), v: v, n: 0, k: 1 }
}

