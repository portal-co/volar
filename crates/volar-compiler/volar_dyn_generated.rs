//! Auto-generated dynamic types from volar-spec
//! Type-level lengths have been converted to runtime usize witnesses

#![allow(unused_variables, dead_code, unused_mut, unused_imports, non_snake_case, unused_parens)]
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

#[derive(Clone, Debug, Default)]
pub struct DeltaDyn<T> {
    pub n: usize,
    pub delta: Vec<T>,
}

#[derive(Clone, Debug, Default)]
pub struct QDyn<T> {
    pub n: usize,
    pub q: Vec<T>,
}

#[derive(Clone, Debug, Default)]
pub struct ABODyn<B: ByteBlockEncrypt, D: Digest> {
    pub k: usize,
    pub commit: Vec<u8>,
    pub per_byte: Vec<Vec<u8>>,
}

#[derive(Clone, Debug, Default)]
pub struct ABOOpeningDyn<B: ByteBlockEncrypt, D: Digest> {
    pub t: usize,
    pub u: usize,
    pub bad: Vec<u64>,
    pub openings: Vec<Vec<Vec<u8>>>,
}

#[derive(Clone, Debug, Default)]
pub struct PolyDyn<T> {
    pub n: usize,
    pub c0: T,
    pub c1: Vec<T>,
}

#[derive(Clone, Debug, Default)]
pub struct PolyInputPoolDyn<'a, 'a, T> {
    pub n: usize,
    pub x: usize,
    pub inputs: &[T],
    pub indices: Vec<Vec<usize>>,
}

#[derive(Clone, Debug, Default)]
pub struct BitVoleDyn<T> {
    pub n: usize,
    pub u: Vec<Bit>,
    pub v: Vec<T>,
}

#[derive(Clone, Debug, Default)]
pub struct VopeDyn<T> {
    pub n: usize,
    pub k: usize,
    pub u: Vec<Vec<T>>,
    pub v: Vec<T>,
}

impl <T> VoleArray for X {
}

impl <T> DeltaDyn<T> {
    pub fn remap(&self, mut m: usize, mut f: usize, mut f: F) -> DeltaDyn<T>
    {
        let n: usize = self.n;
        let Self { delta: delta } = self;
        DeltaDyn { delta: (0..m).map(|i| delta[(f(i) % N::to_usize())].clone()).collect::<Vec<_>>(), n: n }
    }
    pub fn rotate_left(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        self.remap(n, |a| a.wrapping_sub(n))
    }
    pub fn rotate_right(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        self.remap(n, |a| a.wrapping_add(n))
    }
    pub fn r#static(&self, mut u: usize, mut o: usize, mut val: Vec<U>) -> QDyn<O>
    {
        let n: usize = self.n;
        QDyn { q: (0..n).map(|i| (val[i].clone() * self.delta[i].clone())).collect::<Vec<_>>(), n: n }
    }
}

impl <T> QDyn<T> {
    pub fn remap(&self, mut m: usize, mut f: usize, mut f: F) -> QDyn<T>
    {
        let n: usize = self.n;
        let Self { q: q } = self;
        QDyn { q: (0..m).map(|i| q[(f(i) % N::to_usize())].clone()).collect::<Vec<_>>(), n: n }
    }
    pub fn rotate_left(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        self.remap(n, |a| a.wrapping_sub(n))
    }
    pub fn rotate_right(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        self.remap(n, |a| a.wrapping_add(n))
    }
}

impl <T: BlockEncrypt + From<[u8; 32]>> ByteBlockEncrypt for T {
}

impl <T> PolyDyn<T> {
    pub fn get_qs_pool(&self, mut q: usize, mut a: usize, mut m: usize, mut x: usize, mut root: DeltaDyn<Q>, mut inputs: PolyInputPoolDyn<QDyn<Q>>, mut reduction: usize) -> QDyn<A>
    {
        let n: usize = self.n;
        QDyn { q: (0..m).map(|i| {
    let sum: A = self.c0.clone().into();
    for _ in 0.. N::to_usize(){
    sum = (root.delta[i].clone() * sum);
};
    for j in 0.. N::to_usize(){
    let b: A = self.c1[j].clone().into();
    for i2 in inputs.indices.iter(){
    for _ in 0.. reduction{
    b = (inputs.inputs[i2[j]].q[i].clone() * b);
}
};
    sum = (sum + b);
};
    sum
}).collect::<Vec<_>>(), n: n }
    }
    pub fn get_qs(&self, mut q: usize, mut a: usize, mut m: usize, mut x: usize, mut root: DeltaDyn<Q>, mut inputs: Vec<Vec<QDyn<Q>>>, mut reduction: usize) -> QDyn<A>
    {
        let n: usize = self.n;
        QDyn { q: (0..m).map(|i| {
    let sum: A = self.c0.clone().into();
    for _ in 0.. N::to_usize(){
    sum = (root.delta[i].clone() * sum);
};
    for j in 0.. N::to_usize(){
    let b: A = self.c1[j].clone().into();
    for i2 in inputs.iter(){
    for _ in 0.. reduction{
    b = (i2[j].q[i].clone() * b);
}
};
    sum = (sum + b);
};
    sum
}).collect::<Vec<_>>(), n: n }
    }
    pub fn apply_pool(&self, mut m: usize, mut o: usize, mut x: usize, mut x2: usize, mut xs: usize, mut s: usize, mut voles: &PolyInputPoolDyn<VopeDyn<T>>) -> VopeDyn<O>
    {
        let n: usize = self.n;
        let v = (0..m).map(|i| {
    let sum = O::default();
    for k in 0.. N::to_usize(){
    let b: O = self.c1[k].clone().into();
    for v in &voles.indices{
    b = (b * voles.inputs[v[k]].v[i].clone().into());
};
    sum = (sum + b);
};
    let c0: O = self.c0.clone().into();
    (sum + c0)
}).collect::<Vec<_>>();
        let u = (0..xs).map(|l| {
    (0..m).map(|i| {
    let sum = O::default();
    for k in 0.. N::to_usize(){
    for n in 0.. X::to_usize(){
    let b: O = self.c1[k].clone().into();
    for m in 0.. S::to_usize(){
    let l = ((l * S::to_usize()) + m);
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
        return Vope { u: u, v: v };
    }
    pub fn apply(&self, mut m: usize, mut o: usize, mut x: usize, mut x2: usize, mut xs: usize, mut s: usize, mut voles: Vec<Vec<VopeDyn<T>>>) -> VopeDyn<O>
    {
        let n: usize = self.n;
        let v = (0..m).map(|i| {
    let sum = O::default();
    for k in 0.. N::to_usize(){
    let b: O = self.c1[k].clone().into();
    for v in &voles{
    b = (b * v[k].v[i].clone().into());
};
    sum = (sum + b);
};
    let c0: O = self.c0.clone().into();
    (sum + c0)
}).collect::<Vec<_>>();
        let u = (0..xs).map(|l| {
    (0..m).map(|i| {
    let sum = O::default();
    for k in 0.. N::to_usize(){
    for n in 0.. X::to_usize(){
    let b: O = self.c1[k].clone().into();
    for m in 0.. S::to_usize(){
    let l = ((l * S::to_usize()) + m);
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
        return Vope { u: u, v: v };
    }
}

impl <T> VopeDyn<T> {
    pub fn mul_generalized(&self, mut k2: usize, mut other: &VopeDyn<T>) -> VopeDyn<T>
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let res_u = Vec::new::<Vec<T>, <K2 as _>::Output>();
        let res_v = Vec::new::<T, N>();
        for i in 0..= K::to_usize(){
    for j in 0..= K2::to_usize(){
    let k = (i + j);
    let a_coeff = get_coeff!(& self . v , & self . u , i);
    let b_coeff = get_coeff!(& other . v , & other . u , j);
    if (k == 0){
    for lane in 0.. N::to_usize(){
    res_v[lane] = (res_v[lane].clone() + (a_coeff[lane].clone() * b_coeff[lane].clone()));
}
} else {
    for lane in 0.. N::to_usize(){
    res_u[(k - 1)][lane] = (res_u[(k - 1)][lane].clone() + (a_coeff[lane].clone() * b_coeff[lane].clone()));
}
}
}
};
        VopeDyn { u: res_u, v: res_v, n: n, k: k }
    }
}

impl  VopeDyn<BitsInBytes> {
    pub fn rotate_left_bits(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let Vope { u: u, v: v } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n).map(|i| {
    let BitsInBytes(b) = u[l][i].clone();
    let BitsInBytes(next) = u[l][((i + 1) % N::to_usize())].clone();
    BitsInBytes((b.shl((n as u32)) | next.shr((8 - (n as u32)))))
}).collect::<Vec<_>>()
}).collect::<Vec<_>>(), v: (0..n).map(|i| {
    let BitsInBytes(b) = v[i].clone();
    let BitsInBytes(next) = v[((i + 1) % N::to_usize())].clone();
    BitsInBytes((b.shl((n as u32)) | next.shr((8 - (n as u32)))))
}).collect::<Vec<_>>(), n: n, k: k }
    }
    pub fn rotate_right_bits(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let Vope { u: u, v: v } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n).map(|i| {
    let BitsInBytes(prev) = u[l][(((i + N::to_usize()) - 1) % N::to_usize())].clone();
    let BitsInBytes(b) = u[l][i].clone();
    BitsInBytes((prev.shl((8 - (n as u32))) | b.shr((n as u32))))
}).collect::<Vec<_>>()
}).collect::<Vec<_>>(), v: (0..n).map(|i| {
    let BitsInBytes(prev) = v[(((i + N::to_usize()) - 1) % N::to_usize())].clone();
    let BitsInBytes(b) = v[i].clone();
    BitsInBytes((prev.shl((8 - (n as u32))) | b.shr((n as u32))))
}).collect::<Vec<_>>(), n: n, k: k }
    }
    pub fn bit(&self, mut n: u8) -> VopeDyn<Bit>
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let Vope { u: u, v: v } = self;
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

impl  VopeDyn<BitsInBytes64> {
    pub fn rotate_left_bits(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let Vope { u: u, v: v } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n).map(|i| {
    let BitsInBytes64(b) = u[l][i].clone();
    let BitsInBytes64(next) = u[l][((i + 1) % N::to_usize())].clone();
    BitsInBytes64((b.shl((n as u32)) | next.shr((64 - (n as u32)))))
}).collect::<Vec<_>>()
}).collect::<Vec<_>>(), v: (0..n).map(|i| {
    let BitsInBytes64(b) = v[i].clone();
    let BitsInBytes64(next) = v[((i + 1) % N::to_usize())].clone();
    BitsInBytes64((b.shl((n as u32)) | next.shr((64 - (n as u32)))))
}).collect::<Vec<_>>(), n: n, k: k }
    }
    pub fn rotate_right_bits(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let Vope { u: u, v: v } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n).map(|i| {
    let BitsInBytes64(prev) = u[l][(((i + N::to_usize()) - 1) % N::to_usize())].clone();
    let BitsInBytes64(b) = u[l][i].clone();
    BitsInBytes64((prev.shl((64 - (n as u32))) | b.shr((n as u32))))
}).collect::<Vec<_>>()
}).collect::<Vec<_>>(), v: (0..n).map(|i| {
    let BitsInBytes64(prev) = v[(((i + N::to_usize()) - 1) % N::to_usize())].clone();
    let BitsInBytes64(b) = v[i].clone();
    BitsInBytes64((prev.shl((64 - (n as u32))) | b.shr((n as u32))))
}).collect::<Vec<_>>(), n: n, k: k }
    }
    pub fn bit(&self, mut n: u8) -> VopeDyn<Bit>
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let Vope { u: u, v: v } = self;
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

impl <T: Clone> Clone for VopeDyn<T> {
    fn clone(&self) -> Self
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let Vope { u: u, v: v } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n).map(|i| u[l][i].clone()).collect::<Vec<_>>()
}).collect::<Vec<_>>(), v: (0..n).map(|i| v[i].clone()).collect::<Vec<_>>(), n: n, k: k }
    }
}

impl <T: Clone> Clone for QDyn<T> {
    fn clone(&self) -> Self
    {
        let n: usize = self.n;
        let Q { q: q } = self;
        QDyn { q: (0..n).map(|i| q[i].clone()).collect::<Vec<_>>(), n: n }
    }
}

impl <T: Clone> Clone for DeltaDyn<T> {
    fn clone(&self) -> Self
    {
        let n: usize = self.n;
        let Delta { delta: delta } = self;
        DeltaDyn { delta: (0..n).map(|i| delta[i].clone()).collect::<Vec<_>>(), n: n }
    }
}

impl <T: PartialEq> PartialEq for VopeDyn<T> {
    fn eq(&self, mut other: &Self) -> bool
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let Vope { u: u1, v: v1 } = self;
        let Vope { u: u2, v: v2 } = other;
        for l in 0.. K::to_usize(){
    for i in 0.. N::to_usize(){
    if (u1[l][i] != u2[l][i]){
    return false;
}
}
};
        for i in 0.. N::to_usize(){
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
        let Q { q: q1 } = self;
        let Q { q: q2 } = other;
        for i in 0.. N::to_usize(){
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
        let Delta { delta: d1 } = self;
        let Delta { delta: d2 } = other;
        for i in 0.. N::to_usize(){
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
        let Q { q: q } = self;
        QDyn { q: (0..n).map(|i| {
    let BitsInBytes(b) = q[i].clone();
    let BitsInBytes(next) = q[((i + 1) % N::to_usize())].clone();
    BitsInBytes((b.shl((n as u32)) | next.shr((8 - (n as u32)))))
}).collect::<Vec<_>>(), n: n }
    }
    pub fn rotate_right_bits(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        let Q { q: q } = self;
        QDyn { q: (0..n).map(|i| {
    let BitsInBytes(prev) = q[(((i + N::to_usize()) - 1) % N::to_usize())].clone();
    let BitsInBytes(b) = q[i].clone();
    BitsInBytes((prev.shl((8 - (n as u32))) | b.shr((n as u32))))
}).collect::<Vec<_>>(), n: n }
    }
    pub fn bit(&self, mut n: u8) -> QDyn<Bit>
    {
        let n: usize = self.n;
        let Q { q: q } = self;
        QDyn { q: (0..n).map(|i| {
    let BitsInBytes(b) = q[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect::<Vec<_>>(), n: n }
    }
}

impl  QDyn<BitsInBytes64> {
    pub fn rotate_left_bits(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        let Q { q: q } = self;
        QDyn { q: (0..n).map(|i| {
    let BitsInBytes64(b) = q[i].clone();
    let BitsInBytes64(next) = q[((i + 1) % N::to_usize())].clone();
    BitsInBytes64((b.shl((n as u32)) | next.shr((64 - (n as u32)))))
}).collect::<Vec<_>>(), n: n }
    }
    pub fn rotate_right_bits(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        let Q { q: q } = self;
        QDyn { q: (0..n).map(|i| {
    let BitsInBytes64(prev) = q[(((i + N::to_usize()) - 1) % N::to_usize())].clone();
    let BitsInBytes64(b) = q[i].clone();
    BitsInBytes64((prev.shl((64 - (n as u32))) | b.shr((n as u32))))
}).collect::<Vec<_>>(), n: n }
    }
    pub fn bit(&self, mut n: u8) -> QDyn<Bit>
    {
        let n: usize = self.n;
        let Q { q: q } = self;
        QDyn { q: (0..n).map(|i| {
    let BitsInBytes64(b) = q[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect::<Vec<_>>(), n: n }
    }
}

impl  DeltaDyn<BitsInBytes> {
    pub fn rotate_left_bits(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        let Delta { delta: delta } = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let BitsInBytes(b) = delta[i].clone();
    let BitsInBytes(next) = delta[((i + 1) % N::to_usize())].clone();
    BitsInBytes((b.shl((n as u32)) | next.shr((8 - (n as u32)))))
}).collect::<Vec<_>>(), n: n }
    }
    pub fn rotate_right_bits(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        let Delta { delta: delta } = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let BitsInBytes(prev) = delta[(((i + N::to_usize()) - 1) % N::to_usize())].clone();
    let BitsInBytes(b) = delta[i].clone();
    BitsInBytes((prev.shl((8 - (n as u32))) | b.shr((n as u32))))
}).collect::<Vec<_>>(), n: n }
    }
    pub fn bit(&self, mut n: u8) -> DeltaDyn<Bit>
    {
        let n: usize = self.n;
        let Delta { delta: delta } = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let BitsInBytes(b) = delta[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect::<Vec<_>>(), n: n }
    }
}

impl  DeltaDyn<BitsInBytes64> {
    pub fn rotate_left_bits(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        let Delta { delta: delta } = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let BitsInBytes64(b) = delta[i].clone();
    let BitsInBytes64(next) = delta[((i + 1) % N::to_usize())].clone();
    BitsInBytes64((b.shl((n as u32)) | next.shr((64 - (n as u32)))))
}).collect::<Vec<_>>(), n: n }
    }
    pub fn rotate_right_bits(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        let Delta { delta: delta } = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let BitsInBytes64(prev) = delta[(((i + N::to_usize()) - 1) % N::to_usize())].clone();
    let BitsInBytes64(b) = delta[i].clone();
    BitsInBytes64((prev.shl((64 - (n as u32))) | b.shr((n as u32))))
}).collect::<Vec<_>>(), n: n }
    }
    pub fn bit(&self, mut n: u8) -> DeltaDyn<Bit>
    {
        let n: usize = self.n;
        let Delta { delta: delta } = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let BitsInBytes64(b) = delta[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect::<Vec<_>>(), n: n }
    }
}

impl <T> VopeDyn<T> {
    pub fn constant(mut n: usize, mut v: Vec<T>) -> Self
    {
        VopeDyn { u: (0..u0).map(|_| unreachable!()).collect::<Vec<_>>(), v: v, n: n, k: k }
    }
}

impl <T: Add<U, Output = O> + Clone> Add for VopeDyn<T> {
    type Output = VopeDyn<O>;
    fn add(self, mut rhs: VopeDyn<U>) -> <Self as _>::Output
    {
        let n: usize = self.n;
        let k: usize = self.k;
        VopeDyn { u: self.u.into_iter().zip(rhs.u).map(|(a, b)| a.into_iter().zip(b).map(|(a, b)| (a + b)).collect::<Vec<_>>()).collect::<Vec<_>>(), v: self.v.into_iter().zip(rhs.v).map(|(a, b)| (a + b)).collect::<Vec<_>>(), n: n, k: k }
    }
}

impl <T: BitXor<U, Output = O> + Clone> BitXor for VopeDyn<T> {
    type Output = VopeDyn<O>;
    fn bitxor(self, mut rhs: Vec<U>) -> <Self as _>::Output
    {
        let n: usize = self.n;
        let k: usize = self.k;
        VopeDyn { u: (0..k).map(|i| {
    (0..n).map(|j| {
    let o: O = self.u[i][j].clone().bitxor(rhs[((i * K::to_usize()) + j)].clone());
    o
}).collect::<Vec<_>>()
}).collect::<Vec<_>>(), v: self.v.into_iter().map(|a| a.into()).collect::<Vec<_>>(), n: n, k: k }
    }
}

impl <T: Mul<U, Output = O> + Into<O> + Clone> Mul for VopeDyn<T> {
    type Output = QDyn<O>;
    fn mul(self, mut rhs: DeltaDyn<U>) -> <Self as _>::Output
    {
        let n: usize = self.n;
        let k: usize = self.k;
        QDyn { q: self.u.iter().enumerate().fold(self.v.clone().into_iter().map(|a| a.into()).collect::<Vec<_>>(), |a, (i, b)| {
    a.into_iter().zip(b).map(|(a, b)| {
    let x = rhs.delta[i].clone();
    for _ in 0.. i{
    x = (x * rhs.delta[i].clone());
};
    let m: O = (b.clone() * x);
    (m + a)
}).collect::<Vec<_>>()
}), n: n }
    }
}

impl <T> VopeDyn<T> {
    pub fn expand(&self, mut l: usize) -> VopeDyn<T>
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let Self { u: u, v: v } = self;
        VopeDyn { u: (0..l).map(|l| {
    (0..n).map(|i| u.get(l).map_or(T::default(), |a| a[i].clone())).collect::<Vec<_>>()
}).collect::<Vec<_>>(), v: v.clone(), n: n, k: k }
    }
    pub fn rotate_left(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        let k: usize = self.k;
        self.remap(n, |a| a.wrapping_sub(n))
    }
    pub fn rotate_right(&self, mut n: usize) -> Self
    {
        let n: usize = self.n;
        let k: usize = self.k;
        self.remap(n, |a| a.wrapping_add(n))
    }
    pub fn remap(&self, mut m: usize, mut f: usize, mut f: F) -> VopeDyn<T>
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let Self { u: u, v: v } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..m).map(|i| u[l][(f(i) % N::to_usize())].clone()).collect::<Vec<_>>()
}).collect::<Vec<_>>(), v: (0..m).map(|i| v[(f(i) % N::to_usize())].clone()).collect::<Vec<_>>(), n: n, k: k }
    }
}

impl  VopeDyn<Bit> {
    pub fn scale(self, mut f: impl FnMut(bool) -> T) -> VopeDyn<T>
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let Vope { u: u, v: v } = self;
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

impl <B: ByteBlockEncrypt, D: Digest> ABODyn<B, D> {
    pub fn open(&self, mut t: usize, mut u: usize, mut r: usize, mut bad: Vec<u64>, mut rand: &R) -> ABOOpeningDyn<B, D> where <B as _>::BlockSize: Max<<D as _>::OutputSize>
    {
        let k: usize = self.k;
        ABOOpeningDyn { bad: bad.clone(), openings: (0..t).map(|i| {
    let bad = bad.clone();
    (0..u).map(|j| {
    let i2 = (i | ((j as usize) << T::to_usize().ilog2()));
    if bad.contains(&(i2 as u64)){
    let h = commit(&self.per_byte[i2], rand);
    (0..self::output).map(|j| h.as_ref().get(j).cloned().unwrap_or_default()).collect::<Vec<_>>()
} else {
    (0..self::output).map(|j| {
    self.per_byte[i2].get(j).cloned().unwrap_or_default()
}).collect::<Vec<_>>()
}
}).collect::<Vec<_>>()
}).collect::<Vec<_>>(), t: t, u: u }
    }
}

impl <B: ByteBlockEncrypt, D: Digest> ABOOpeningDyn<B, D> {
    pub fn validate(&self, mut r: usize, mut commit_: &Vec<u8>, mut rand: &R) -> bool
    {
        let t: usize = self.t;
        let u: usize = self.u;
        let h = D::new();
        for i in 0.. T::to_usize(){
    for b in 0.. U::to_usize(){
    let i2 = (i | ((b as usize) << T::to_usize().ilog2()));
    if self.bad.contains(&(i2 as u64)){
    h.update(&self.openings[i][b][(0 + D::OutputSize::to_usize())]);
} else {
    h.update(&commit(&&self.openings[i][b][(0 + B::BlockSize::to_usize())], rand));
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
    let s = &self.openings[i];
    create_vole_from_material(s)
}).collect::<Vec<_>>()
    }
    pub fn to_vole_material_typenum(&self, mut n: usize) -> Vec<VopeDyn<u8>>
    {
        let t: usize = self.t;
        let u: usize = self.u;
        (0..n).map(|i| {
    let s = &self.openings[i];
    create_vole_from_material(s)
}).collect::<Vec<_>>()
    }
    pub fn to_vole_material_expanded(&self, mut n: usize, mut x: usize, mut f: usize, mut f: F) -> Vec<VopeDyn<u8>>
    {
        let t: usize = self.t;
        let u: usize = self.u;
        (0..n).map(|i| {
    let s = &self.openings[i];
    create_vole_from_material_expanded(s, &mut f)
}).collect::<Vec<_>>()
    }
    pub fn to_vole_material_typenum_expanded(&self, mut n: usize, mut x: usize, mut f: usize, mut f: F) -> Vec<VopeDyn<u8>>
    {
        let t: usize = self.t;
        let u: usize = self.u;
        (0..n).map(|i| {
    let s = &self.openings[i];
    create_vole_from_material_expanded(s, &mut f)
}).collect::<Vec<_>>()
    }
}

impl <B: ByteBlockEncrypt, D: Digest> ABODyn<B, D> {
    pub fn to_vole_material(&self, mut n: usize) -> Vec<VopeDyn<u8>>
    {
        let k: usize = self.k;
        (0..n).map(|i| {
    let s = &self.per_byte[((i * N) + 0)][(0 + N)];
    create_vole_from_material(s)
}).collect::<Vec<_>>()
    }
    pub fn to_vole_material_typenum(&self, mut n: usize) -> Vec<VopeDyn<u8>>
    {
        let k: usize = self.k;
        (0..n).map(|i| {
    let s = &self.per_byte[((i * N::to_usize()) + 0)][(0 + N::to_usize())];
    create_vole_from_material(s)
}).collect::<Vec<_>>()
    }
    pub fn to_vole_material_expanded(&self, mut n: usize, mut x: usize, mut f: usize, mut f: F) -> Vec<VopeDyn<u8>>
    {
        let k: usize = self.k;
        (0..n).map(|i| {
    let s = &self.per_byte[((i * N) + 0)][(0 + N)];
    create_vole_from_material_expanded(s, &mut f)
}).collect::<Vec<_>>()
    }
    pub fn to_vole_material_typenum_expanded(&self, mut n: usize, mut x: usize, mut f: usize, mut f: F) -> Vec<VopeDyn<u8>>
    {
        let k: usize = self.k;
        (0..n).map(|i| {
    let s = &self.per_byte[((i * N::to_usize()) + 0)][(0 + N::to_usize())];
    create_vole_from_material_expanded(s, &mut f)
}).collect::<Vec<_>>()
    }
}

pub fn create_vole_from_material(mut x: usize, mut s: &[X]) -> VopeDyn<u8>
{
    let u: Vec<u8> = s.iter().fold(Vec::new::<u8, <B as _>::BlockSize>(), |a, b| {
    a.into_iter().zip((0..b::blocksize).map(|i| b.as_ref()[i]).collect::<Vec<_>>()).map(|(a, b)| a.bitxor(b)).collect::<Vec<_>>()
});
    let v: Vec<u8> = s.iter().enumerate().fold(Vec::new::<u8, <B as _>::BlockSize>(), |a, (i, b)| {
    a.into_iter().zip((0..b::blocksize).map(|i| b.as_ref()[i]).collect::<Vec<_>>()).map(|(a, b)| a.bitxor(b).bitxor((i as u8))).collect::<Vec<_>>()
});
    VopeDyn { u: (0..u1).map(|_| u.clone()).collect::<Vec<_>>(), v: v, n: n, k: k }
}

pub fn create_vole_from_material_expanded(mut x: usize, mut y: usize, mut f: usize, mut s: &[Y], mut f: F) -> VopeDyn<u8>
{
    let u: Vec<u8> = s.iter().into_iter().map(|b| f(&b.as_ref()[(0 + B::BlockSize::to_usize())])).collect::<Vec<_>>().fold(Vec::new::<u8, <B as _>::BlockSize>(), |a, b| {
    a.into_iter().zip((0..b::blocksize).map(|i| b.as_ref()[i]).collect::<Vec<_>>()).map(|(a, b)| a.bitxor(b)).collect::<Vec<_>>()
});
    let v: Vec<u8> = s.iter().into_iter().map(|b| f(&b.as_ref()[(0 + B::BlockSize::to_usize())])).collect::<Vec<_>>().enumerate().fold(Vec::new::<u8, <B as _>::BlockSize>(), |a, (i, b)| {
    a.into_iter().zip((0..b::blocksize).map(|i| b.as_ref()[i]).collect::<Vec<_>>()).map(|(a, b)| a.bitxor(b).bitxor((i as u8))).collect::<Vec<_>>()
});
    VopeDyn { u: (0..u1).map(|_| u.clone()).collect::<Vec<_>>(), v: v, n: n, k: k }
}

pub fn double(mut a: Vec<u8>) -> Vec<Vec<u8>>
{
    return (0..2).map(|i| {
    let out = a.clone();
    let b = B::from(vec![(i as u8); 32]);
    b.encryptblock(&mut out);
    out
}).collect::<Vec<_>>();
}

