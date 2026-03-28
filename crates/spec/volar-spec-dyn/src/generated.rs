//! Auto-generated dynamic types from volar-spec
//! Type-level lengths have been converted to runtime usize witnesses

#![allow(unused_variables, dead_code, unused_mut, unused_imports, non_snake_case, unused_parens)]
extern crate alloc;
use alloc::vec::Vec;
use alloc::vec;
use core::ops::{Add, Sub, Mul, Div, BitAnd, BitOr, BitXor, Shl, Shr};
use core::marker::PhantomData;
use typenum::Unsigned;
use digest::Digest;
use volar_common::hash_commitment::commit;
use volar_common::length_doubling::LengthDoubler;
use volar_primitives::{Bit, BitsInBytes, BitsInBytes64, Galois, Galois64};
use hybrid_array::Array;

/// Compute integer log2
#[inline]
pub fn ilog2(x: usize) -> u32 {
    usize::BITS - x.leading_zeros() - 1
}

/// Bridge: call LengthDoubler::double on a Vec<u8>, converting to/from Array
#[inline]
pub fn double_vec<B: LengthDoubler>(v: Vec<u8>) -> [Vec<u8>; 2] {
    let arr = Array::try_from(v.as_slice()).expect("double_vec: length mismatch");
    let [a, b] = B::double(arr);
    [a.to_vec(), b.to_vec()]
}

#[derive(Debug, Default)]
pub struct EvalDyn {
    pub n: usize,
    pub target: Vec<u8>,
}

#[derive(Debug, Default)]
pub struct GarbleDyn {
    pub n: usize,
    pub base: Vec<u8>,
}

#[derive(Debug, Default)]
pub struct GarbleTableDyn {
    pub n: usize,
    pub table: Vec<Vec<u8>>,
}

#[derive(Debug, Default)]
pub struct GlobalSecretDyn {
    pub n: usize,
    pub secret: Vec<u8>,
}

#[derive(Debug, Default)]
pub struct AllPartiesDyn<T> {
    pub n: usize,
    pub other_parties: OtherPartiesDyn<T>,
    pub self_party: T,
}

#[derive(Debug, Default)]
pub struct OtherPartiesDyn<T> {
    pub n: usize,
    pub other_parties: Vec<T>,
}

#[derive(Debug, Default)]
pub struct BitVoleDyn<T> {
    pub n: usize,
    pub u: Vec<Bit>,
    pub v: Vec<T>,
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
pub struct VopeDyn<T> {
    pub n: usize,
    pub k: usize,
    pub u: Vec<Vec<T>>,
    pub v: Vec<T>,
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

impl <B: LengthDoubler, D: Digest> ABODyn<B, D, K, NParties> {
    pub fn to_vole_material(&self, mut n: usize, mut target: usize) -> Vec<VopeDyn<u8>>
    {
        (0..n).map(|i| {
    let s = &self.per_byte[target][(i * n)..][..n];
    create_vole_from_material::<B, _>(s)
}).collect::<Vec<_>>()
    }
    pub fn to_vole_material_typenum(&self, mut n: usize, mut target: usize) -> Vec<VopeDyn<u8>>
    {
        (0..n).map(|i| {
    let s = &self.per_byte[target][(i * n)..][..n];
    create_vole_from_material::<B, _>(s)
}).collect::<Vec<VopeDyn<u8>>>()
    }
    pub fn to_vole_material_expanded<X: AsRef<[u8]>, F: FnMut(&[u8]) -> X>(&self, mut n: usize, mut target: usize, mut f: F) -> Vec<VopeDyn<u8>>
    {
        (0..n).map(|i| {
    let s = &self.per_byte[target][(i * n)..][..n];
    create_vole_from_material_expanded::<B, _, _, _>(s, &mut f)
}).collect::<Vec<_>>()
    }
    pub fn to_vole_material_typenum_expanded<X: AsRef<[u8]>, F: FnMut(&[u8]) -> X>(&self, mut n: usize, mut target: usize, mut f: F) -> Vec<VopeDyn<u8>>
    {
        (0..n).map(|i| {
    let s = &self.per_byte[target][(i * n)..][..n];
    create_vole_from_material_expanded::<B, _, _, _>(s, &mut f)
}).collect::<Vec<VopeDyn<u8>>>()
    }
    pub fn split_bit_typenum(&self, mut n: usize, mut target: usize) -> Vec<BSplitDyn<B, D>> where D: Digest
    {
        (0..n).map(|i| {
    let s = &self.per_byte[target][(i * n)..][..n];
    BSplit { split: (0..ilog2(<<D>::OutputSize as Unsigned>::to_usize())).map(|j| {
    (0..n).map(|b| {
    s.iter().enumerate().filter_map(|(a, c)| {
    if (((a >> j) & 1) == b){
    Some(c.clone())
} else {
    None
}
}).fold((0..<<B>::OutputSize as Unsigned>::to_usize()).map(|_| 0).collect::<Vec<u8>>(), |mut a, b| {
    a.into_iter().zip((0..<<B>::OutputSize as Unsigned>::to_usize()).map(|i| AsRef::<[u8]>::as_ref(&b)[i]).collect::<Vec<u8>>().into_iter()).map(|(a, b)| a.bitxor(b)).collect::<Vec<_>>()
})
}).collect::<Vec<_>>()
}).collect::<Vec<Vec<Vec<u8>>>>() }
}).collect::<Vec<BSplitDyn<B, D>>>()
    }
}

impl  GarbleDyn {
    pub fn share(&self, mut target: &Vec<u8>) -> EvalDyn
    {
        let n: usize = self.n;
        EvalDyn { target: self.base.clone().into_iter().zip(target.clone().into_iter()).map(|(a, b)| (a ^ b)).collect::<Vec<_>>(), n: 0 }
    }
    pub fn to_share(&self, mut o: usize) -> GarbleDyn
    {
        let n: usize = self.n;
        GarbleDyn { base: (0..o).map(|i| {
    let mut v = 0;
    for j in 0.. 8{
    let bit = (self.base[((i * 8) + j)] & 1);
    (v + (bit << j));
};
    v
}).collect::<Vec<u8>>(), n: 0 }
    }
}

impl  EvalDyn {
    pub fn open(&self, mut garble: &GarbleDyn) -> Vec<u8>
    {
        let n: usize = self.n;
        self.target.clone().into_iter().zip(garble.base.clone().into_iter()).map(|(a, b)| (a ^ b)).collect::<Vec<_>>()
    }
    pub fn to_share(&self, mut o: usize) -> EvalDyn
    {
        let n: usize = self.n;
        EvalDyn { target: (0..o).map(|i| {
    let mut v = 0;
    for j in 0.. 8{
    let bit = (self.target[((i * 8) + j)] & 1);
    (v + (bit << j));
};
    v
}).collect::<Vec<u8>>(), n: 0 }
    }
    pub fn and_via_table<D: Digest<OutputSize = N>>(&self, mut other: &EvalDyn, mut table: &GarbleTableDyn) -> EvalDyn
    {
        let n: usize = self.n;
        let index = (if ((self.target[0] & 1) == 1){
    1
} else {
    0
} | if ((other.target[0] & 1) == 1){
    2
} else {
    0
});
        EvalDyn { target: {
    let mut d = D::new();
    d.update(&self.target);
    d.update(&other.target);
    d.finalize().to_vec()
}.into_iter().zip(table.table[index].clone().into_iter()).map(|(a, b)| (a ^ b)).collect::<Vec<_>>(), n: 0 }
    }
}

impl  GlobalSecretDyn {
    pub fn new(mut n: usize, mut secret: Vec<u8>) -> Self
    {
        (secret[0] + 1);
        Self { secret: secret }
    }
    pub fn secret(&self) -> Vec<u8>
    {
        let n: usize = self.n;
        self.secret.clone()
    }
    pub fn encode(&self, mut garble: &GarbleDyn, mut value: bool) -> EvalDyn
    {
        let n: usize = self.n;
        EvalDyn { target: self.secret.clone().into_iter().zip(garble.base.clone().into_iter()).map(|(a, b)| if value{
    (a ^ b)
} else {
    b
}).collect::<Vec<_>>(), n: 0 }
    }
    pub fn gen_and_table<D: Digest<OutputSize = N>>(&self, mut a: &GarbleDyn, mut b: &GarbleDyn) -> GarbleTableDyn
    {
        let n: usize = self.n;
        let mut table = (0..n).map(|_| (0..n).map(|_| 0).collect::<Vec<u8>>()).collect::<Vec<_>>();
        for i in 0.. 4{
    let av = ((i & 1) != 0);
    let bv = ((i & 2) != 0);
    let mut d = D::new();
    d.update(&self.encode(a, av).target);
    d.update(&self.encode(b, bv).target);
    let target = d.finalize().to_vec();
    let index = (if ((a.base[0] & 1) != 1){
    1
} else {
    0
} | if ((b.base[0] & 1) != 1){
    2
} else {
    0
});
    table[index] = table[index].clone().into_iter().zip(target.into_iter()).map(|(a, b)| (a ^ b)).collect::<Vec<_>>();
};
        GarbleTableDyn { table: table, n: 0 }
    }
}

impl  BitXor<EvalDyn> for EvalDyn {
    type Output = EvalDyn;
    fn bitxor(self, mut rhs: EvalDyn) -> Self::Output
    {
        let n: usize = self.n;
        return EvalDyn { target: self.target.into_iter().zip(rhs.target.into_iter()).map(|(a, b)| (a ^ b)).collect::<Vec<_>>(), n: 0 };
    }
}

impl  VopeDyn<BitsInBytes> {
    pub fn rotate_left_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n_param).map(|i| {
    let BitsInBytes(b) = u[l][i].clone();
    let BitsInBytes(next) = u[l][((i + 1) % n_param)].clone();
    BitsInBytes((b.shl((n_param as u32)) | next.shr((8 - (n_param as u32)))))
}).collect::<Vec<BitsInBytes>>()
}).collect::<Vec<Vec<BitsInBytes>>>(), v: (0..n_param).map(|i| {
    let BitsInBytes(b) = v[i].clone();
    let BitsInBytes(next) = v[((i + 1) % n_param)].clone();
    BitsInBytes((b.shl((n_param as u32)) | next.shr((8 - (n_param as u32)))))
}).collect::<Vec<BitsInBytes>>(), n: 0, k: 1 }
    }
    pub fn rotate_right_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n_param).map(|i| {
    let BitsInBytes(prev) = u[l][(((i + n_param) - 1) % n_param)].clone();
    let BitsInBytes(b) = u[l][i].clone();
    BitsInBytes((prev.shl((8 - (n_param as u32))) | b.shr((n_param as u32))))
}).collect::<Vec<BitsInBytes>>()
}).collect::<Vec<Vec<BitsInBytes>>>(), v: (0..n_param).map(|i| {
    let BitsInBytes(prev) = v[(((i + n_param) - 1) % n_param)].clone();
    let BitsInBytes(b) = v[i].clone();
    BitsInBytes((prev.shl((8 - (n_param as u32))) | b.shr((n_param as u32))))
}).collect::<Vec<BitsInBytes>>(), n: 0, k: 1 }
    }
    pub fn bit(&self, mut n_param: u8) -> VopeDyn<Bit>
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n_param).map(|i| {
    let BitsInBytes(b) = u[l][i].clone();
    Bit((((b >> n_param) & 1) != 0))
}).collect::<Vec<Bit>>()
}).collect::<Vec<Vec<Bit>>>(), v: (0..n_param).map(|i| {
    let BitsInBytes(b) = v[i].clone();
    Bit((((b >> n_param) & 1) != 0))
}).collect::<Vec<Bit>>(), n: 0, k: 1 }
    }
}

impl  VopeDyn<BitsInBytes64> {
    pub fn rotate_left_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n_param).map(|i| {
    let BitsInBytes64(b) = u[l][i].clone();
    let BitsInBytes64(next) = u[l][((i + 1) % n_param)].clone();
    BitsInBytes64((b.shl((n_param as u32)) | next.shr((64 - (n_param as u32)))))
}).collect::<Vec<BitsInBytes64>>()
}).collect::<Vec<Vec<BitsInBytes64>>>(), v: (0..n_param).map(|i| {
    let BitsInBytes64(b) = v[i].clone();
    let BitsInBytes64(next) = v[((i + 1) % n_param)].clone();
    BitsInBytes64((b.shl((n_param as u32)) | next.shr((64 - (n_param as u32)))))
}).collect::<Vec<BitsInBytes64>>(), n: 0, k: 1 }
    }
    pub fn rotate_right_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n_param).map(|i| {
    let BitsInBytes64(prev) = u[l][(((i + n_param) - 1) % n_param)].clone();
    let BitsInBytes64(b) = u[l][i].clone();
    BitsInBytes64((prev.shl((64 - (n_param as u32))) | b.shr((n_param as u32))))
}).collect::<Vec<BitsInBytes64>>()
}).collect::<Vec<Vec<BitsInBytes64>>>(), v: (0..n_param).map(|i| {
    let BitsInBytes64(prev) = v[(((i + n_param) - 1) % n_param)].clone();
    let BitsInBytes64(b) = v[i].clone();
    BitsInBytes64((prev.shl((64 - (n_param as u32))) | b.shr((n_param as u32))))
}).collect::<Vec<BitsInBytes64>>(), n: 0, k: 1 }
    }
    pub fn bit(&self, mut n_param: u8) -> VopeDyn<Bit>
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n_param).map(|i| {
    let BitsInBytes64(b) = u[l][i].clone();
    Bit((((b >> n_param) & 1) != 0))
}).collect::<Vec<Bit>>()
}).collect::<Vec<Vec<Bit>>>(), v: (0..n_param).map(|i| {
    let BitsInBytes64(b) = v[i].clone();
    Bit((((b >> n_param) & 1) != 0))
}).collect::<Vec<Bit>>(), n: 0, k: 1 }
    }
}

impl  QDyn<BitsInBytes> {
    pub fn rotate_left_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n_param).map(|i| {
    let BitsInBytes(b) = q[i].clone();
    let BitsInBytes(next) = q[((i + 1) % n_param)].clone();
    BitsInBytes((b.shl((n_param as u32)) | next.shr((8 - (n_param as u32)))))
}).collect::<Vec<BitsInBytes>>(), n: 0 }
    }
    pub fn rotate_right_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n_param).map(|i| {
    let BitsInBytes(prev) = q[(((i + n_param) - 1) % n_param)].clone();
    let BitsInBytes(b) = q[i].clone();
    BitsInBytes((prev.shl((8 - (n_param as u32))) | b.shr((n_param as u32))))
}).collect::<Vec<BitsInBytes>>(), n: 0 }
    }
    pub fn bit(&self, mut n_param: u8) -> QDyn<Bit>
    {
        let n: usize = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n_param).map(|i| {
    let BitsInBytes(b) = q[i].clone();
    Bit((((b >> n_param) & 1) != 0))
}).collect::<Vec<Bit>>(), n: 0 }
    }
}

impl  QDyn<BitsInBytes64> {
    pub fn rotate_left_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n_param).map(|i| {
    let BitsInBytes64(b) = q[i].clone();
    let BitsInBytes64(next) = q[((i + 1) % n_param)].clone();
    BitsInBytes64((b.shl((n_param as u32)) | next.shr((64 - (n_param as u32)))))
}).collect::<Vec<BitsInBytes64>>(), n: 0 }
    }
    pub fn rotate_right_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n_param).map(|i| {
    let BitsInBytes64(prev) = q[(((i + n_param) - 1) % n_param)].clone();
    let BitsInBytes64(b) = q[i].clone();
    BitsInBytes64((prev.shl((64 - (n_param as u32))) | b.shr((n_param as u32))))
}).collect::<Vec<BitsInBytes64>>(), n: 0 }
    }
    pub fn bit(&self, mut n_param: u8) -> QDyn<Bit>
    {
        let n: usize = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n_param).map(|i| {
    let BitsInBytes64(b) = q[i].clone();
    Bit((((b >> n_param) & 1) != 0))
}).collect::<Vec<Bit>>(), n: 0 }
    }
}

impl  DeltaDyn<BitsInBytes> {
    pub fn rotate_left_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n_param).map(|i| {
    let BitsInBytes(b) = delta[i].clone();
    let BitsInBytes(next) = delta[((i + 1) % n_param)].clone();
    BitsInBytes((b.shl((n_param as u32)) | next.shr((8 - (n_param as u32)))))
}).collect::<Vec<BitsInBytes>>(), n: 0 }
    }
    pub fn rotate_right_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n_param).map(|i| {
    let BitsInBytes(prev) = delta[(((i + n_param) - 1) % n_param)].clone();
    let BitsInBytes(b) = delta[i].clone();
    BitsInBytes((prev.shl((8 - (n_param as u32))) | b.shr((n_param as u32))))
}).collect::<Vec<BitsInBytes>>(), n: 0 }
    }
    pub fn bit(&self, mut n_param: u8) -> DeltaDyn<Bit>
    {
        let n: usize = self.n;
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n_param).map(|i| {
    let BitsInBytes(b) = delta[i].clone();
    Bit((((b >> n_param) & 1) != 0))
}).collect::<Vec<Bit>>(), n: 0 }
    }
}

impl  DeltaDyn<BitsInBytes64> {
    pub fn rotate_left_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n_param).map(|i| {
    let BitsInBytes64(b) = delta[i].clone();
    let BitsInBytes64(next) = delta[((i + 1) % n_param)].clone();
    BitsInBytes64((b.shl((n_param as u32)) | next.shr((64 - (n_param as u32)))))
}).collect::<Vec<BitsInBytes64>>(), n: 0 }
    }
    pub fn rotate_right_bits(&self, mut n_param: usize) -> Self
    {
        let n: usize = self.n;
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n_param).map(|i| {
    let BitsInBytes64(prev) = delta[(((i + n_param) - 1) % n_param)].clone();
    let BitsInBytes64(b) = delta[i].clone();
    BitsInBytes64((prev.shl((64 - (n_param as u32))) | b.shr((n_param as u32))))
}).collect::<Vec<BitsInBytes64>>(), n: 0 }
    }
    pub fn bit(&self, mut n_param: u8) -> DeltaDyn<Bit>
    {
        let n: usize = self.n;
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n_param).map(|i| {
    let BitsInBytes64(b) = delta[i].clone();
    Bit((((b >> n_param) & 1) != 0))
}).collect::<Vec<Bit>>(), n: 0 }
    }
}

impl <T: Clone> Clone for VopeDyn<T> {
    fn clone(&self) -> Self
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n).map(|i| u[l][i].clone()).collect::<Vec<T>>()
}).collect::<Vec<Vec<T>>>(), v: (0..n).map(|i| v[i].clone()).collect::<Vec<T>>(), n: 0, k: 1 }
    }
}

impl <T: Clone> Clone for QDyn<T> {
    fn clone(&self) -> Self
    {
        let n: usize = self.n;
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n).map(|i| q[i].clone()).collect::<Vec<T>>(), n: 0 }
    }
}

impl <T: Clone> Clone for DeltaDyn<T> {
    fn clone(&self) -> Self
    {
        let n: usize = self.n;
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n).map(|i| delta[i].clone()).collect::<Vec<T>>(), n: 0 }
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
}).collect::<Vec<A>>(), n: 0 }
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
}).collect::<Vec<A>>(), n: 0 }
    }
    pub fn apply_pool<M, O: Mul<O, Output = O> + Add<O, Output = O> + Default + Clone>(&self, mut m: usize, mut x: usize, mut x2: usize, mut xs: usize, mut s: usize, mut voles: &PolyInputPoolDyn<VopeDyn<T>>) -> VopeDyn<O> where T: Into<O> + Clone
    {
        let n: usize = self.n;
        let v = (0..m).map(|i| {
    let mut sum = O::default();
    for k in 0.. n{
    let mut b: O = self.c1[k].clone().into();
    for v in &voles.indices{
    b = (b * voles.inputs[v[k]].v[i].clone().into());
};
    sum = (sum + b);
};
    let c0: O = self.c0.clone().into();
    (sum + c0)
}).collect::<Vec<O>>();
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
}).collect::<Vec<O>>()
}).collect::<Vec<Vec<O>>>();
        return VopeDyn { u: u, v: v, n: 0, k: 1 };
    }
    pub fn apply<M, O: Mul<O, Output = O> + Add<O, Output = O> + Default + Clone>(&self, mut m: usize, mut x: usize, mut x2: usize, mut xs: usize, mut s: usize, mut voles: Vec<Vec<VopeDyn<T>>>) -> VopeDyn<O> where T: Into<O> + Clone
    {
        let n: usize = self.n;
        let v = (0..m).map(|i| {
    let mut sum = O::default();
    for k in 0.. n{
    let mut b: O = self.c1[k].clone().into();
    for v in &voles{
    b = (b * v[k].v[i].clone().into());
};
    sum = (sum + b);
};
    let c0: O = self.c0.clone().into();
    (sum + c0)
}).collect::<Vec<O>>();
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
}).collect::<Vec<O>>()
}).collect::<Vec<Vec<O>>>();
        return VopeDyn { u: u, v: v, n: 0, k: 1 };
    }
}

impl <T: Add<Output = T> + Mul<Output = T> + Default + Clone> VopeDyn<T> where T: Add<Output = T> + Mul<Output = T> + Default + Clone {
    pub fn mul_generalized(&self, mut k2: usize, mut other: &VopeDyn<T>) -> VopeDyn<T>
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let mut res_u = (0..(k2 + k)).map(|_| <Vec<T>>::default()).collect::<Vec<Vec<T>>>();
        let mut res_v = (0..n).map(|_| <T>::default()).collect::<Vec<T>>();
        for i in 0..= k{
    for j in 0..= k2{
    let k = (i + j);
    let a_coeff = if (i == 0){
    &self.v
} else {
    &self.u[(i - 1)]
};
    let b_coeff = if (j == 0){
    &other.v
} else {
    &other.u[(j - 1)]
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

impl <T> VopeDyn<T> {
    pub fn constant(mut n: usize, mut v: Vec<T>) -> Self
    {
        let k: usize = 0;
        VopeDyn { u: (0..0).map(|_| unreachable!()).collect::<Vec<Vec<T>>>(), v: v, n: 0, k: 1 }
    }
}

impl <T: Add<U, Output = O> + Clone, U: Clone, O> Add<VopeDyn<U>> for VopeDyn<T> {
    type Output = VopeDyn<O>;
    fn add(self, mut rhs: VopeDyn<U>) -> Self::Output
    {
        let n: usize = self.n;
        let k: usize = self.k;
        VopeDyn { u: self.u.into_iter().zip(rhs.u.into_iter()).map(|(a, b)| a.into_iter().zip(b.into_iter()).map(|(a, b)| (a + b)).collect::<Vec<_>>()).collect::<Vec<_>>(), v: self.v.into_iter().zip(rhs.v.into_iter()).map(|(a, b)| (a + b)).collect::<Vec<_>>(), n: 0, k: 1 }
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
}).collect::<Vec<O>>()
}).collect::<Vec<Vec<O>>>(), v: self.v.into_iter().map(|a| a.into()).collect::<Vec<_>>(), n: 0, k: 1 }
    }
}

impl <T: Mul<U, Output = O> + Into<O> + Clone, U: Mul<U, Output = U> + Clone, O: Add<O, Output = O>> Mul<DeltaDyn<U>> for VopeDyn<T> {
    type Output = QDyn<O>;
    fn mul(self, mut rhs: DeltaDyn<U>) -> Self::Output
    {
        let n: usize = self.n;
        let k: usize = self.k;
        QDyn { q: self.u.iter().enumerate().fold(self.v.clone().into_iter().map(|a| a.into()).collect::<Vec<_>>(), |mut a, (i, b)| {
    a.into_iter().zip(b.into_iter()).map(|(a, b)| {
    let mut x = rhs.delta[i].clone();
    for _ in 0.. i{
    x = (x * rhs.delta[i].clone());
};
    let m: O = (b.clone() * x);
    (m + a)
}).collect::<Vec<_>>()
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
    (0..n).map(|i| u.get(l).map_or(T::default(), |a| a[i].clone())).collect::<Vec<T>>()
}).collect::<Vec<Vec<T>>>(), v: v.clone(), n: 0, k: 1 }
    }
    pub fn rotate_left(&self, mut n_param: usize) -> Self where T: Clone
    {
        let n: usize = self.n;
        let k: usize = self.k;
        self.remap(self.n, |a| a.wrapping_sub(n_param))
    }
    pub fn rotate_right(&self, mut n_param: usize) -> Self where T: Clone
    {
        let n: usize = self.n;
        let k: usize = self.k;
        self.remap(self.n, |a| a.wrapping_add(n_param))
    }
    pub fn remap<F: FnMut(usize) -> usize>(&self, mut m: usize, mut f: F) -> VopeDyn<T> where T: Clone
    {
        let n: usize = self.n;
        let k: usize = self.k;
        let Self { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..m).map(|i| u[l][(f(i) % n)].clone()).collect::<Vec<T>>()
}).collect::<Vec<Vec<T>>>(), v: (0..m).map(|i| v[(f(i) % n)].clone()).collect::<Vec<T>>(), n: 0, k: 1 }
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
}).collect::<Vec<T>>()
}).collect::<Vec<Vec<T>>>(), v: (0..n).map(|i| {
    let Bit(b) = v[i].clone();
    f(b)
}).collect::<Vec<T>>(), n: 0, k: 1 }
    }
}

impl <T> DeltaDyn<T> {
    pub fn remap<F: FnMut(usize) -> usize>(&self, mut m: usize, mut f: F) -> DeltaDyn<T> where T: Clone
    {
        let n: usize = self.n;
        let Self { delta: delta, .. } = self;
        DeltaDyn { delta: (0..m).map(|i| delta[(f(i) % n)].clone()).collect::<Vec<T>>(), n: 0 }
    }
    pub fn rotate_left(&self, mut n_param: usize) -> Self where T: Clone
    {
        let n: usize = self.n;
        self.remap(self.n, |a| a.wrapping_sub(n_param))
    }
    pub fn rotate_right(&self, mut n_param: usize) -> Self where T: Clone
    {
        let n: usize = self.n;
        self.remap(self.n, |a| a.wrapping_add(n_param))
    }
    pub fn r#static<U: Mul<T, Output = O> + Clone, O>(&self, mut val: Vec<U>) -> QDyn<O> where T: Clone
    {
        let n: usize = self.n;
        QDyn { q: (0..n).map(|i| (val[i].clone() * self.delta[i].clone())).collect::<Vec<O>>(), n: 0 }
    }
}

impl <T> QDyn<T> {
    pub fn remap<F: FnMut(usize) -> usize>(&self, mut m: usize, mut f: F) -> QDyn<T> where T: Clone
    {
        let n: usize = self.n;
        let Self { q: q, .. } = self;
        QDyn { q: (0..m).map(|i| q[(f(i) % n)].clone()).collect::<Vec<T>>(), n: 0 }
    }
    pub fn rotate_left(&self, mut n_param: usize) -> Self where T: Clone
    {
        let n: usize = self.n;
        self.remap(self.n, |a| a.wrapping_sub(n_param))
    }
    pub fn rotate_right(&self, mut n_param: usize) -> Self where T: Clone
    {
        let n: usize = self.n;
        self.remap(self.n, |a| a.wrapping_add(n_param))
    }
}

