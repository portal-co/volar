#![allow(unused_variables, dead_code, unused_mut, unused_imports)]
use alloc::vec::Vec;
use alloc::vec;
use core::ops::{Add, Sub, Mul, Div, Rem, BitAnd, BitOr, BitXor, Neg, Not};

#[derive(Clone, Copy, Default, Debug, PartialEq)] pub struct Bit(pub bool);
impl core::ops::BitXor for Bit { type Output = Self; fn bitxor(self, rhs: Self) -> Self { Bit(self.0 ^ rhs.0) } }
pub type Galois = u8;
pub type Galois64 = u64;
#[derive(Clone, Copy, Default, Debug, PartialEq)] pub struct BitsInBytes(pub u8);
#[derive(Clone, Copy, Default, Debug, PartialEq)] pub struct BitsInBytes64(pub u64);
impl core::ops::BitXor for BitsInBytes { type Output = Self; fn bitxor(self, rhs: Self) -> Self { BitsInBytes(self.0 ^ rhs.0) } }
impl core::ops::BitXor for BitsInBytes64 { type Output = Self; fn bitxor(self, rhs: Self) -> Self { BitsInBytes64(self.0 ^ rhs.0) } }
impl BitsInBytes { pub fn shl(self, n: u32) -> u8 { self.0 << n } pub fn shr(self, n: u32) -> u8 { self.0 >> n } }
impl BitsInBytes64 { pub fn shl(self, n: u32) -> u64 { self.0 << n } pub fn shr(self, n: u32) -> u64 { self.0 >> n } }
impl core::ops::Shl<u32> for BitsInBytes { type Output = Self; fn shl(self, rhs: u32) -> Self { BitsInBytes(self.0 << rhs) } }
impl core::ops::Shr<u32> for BitsInBytes { type Output = Self; fn shr(self, rhs: u32) -> Self { BitsInBytes(self.0 >> rhs) } }
impl core::ops::Shl<u32> for BitsInBytes64 { type Output = Self; fn shl(self, rhs: u32) -> Self { BitsInBytes64(self.0 << rhs) } }
impl core::ops::Shr<u32> for BitsInBytes64 { type Output = Self; fn shr(self, rhs: u32) -> Self { BitsInBytes64(self.0 >> rhs) } }
impl core::ops::Shl<u32> for Bit { type Output = Self; fn shl(self, rhs: u32) -> Self { self } }
impl core::ops::Shr<u32> for Bit { type Output = Self; fn shr(self, rhs: u32) -> Self { self } }
impl core::ops::BitAnd<usize> for BitsInBytes { type Output = usize; fn bitand(self, rhs: usize) -> usize { (self.0 as usize) & rhs } }
impl core::ops::BitAnd<usize> for BitsInBytes64 { type Output = usize; fn bitand(self, rhs: usize) -> usize { (self.0 as usize) & rhs } }
impl core::ops::Shr<usize> for BitsInBytes { type Output = usize; fn shr(self, rhs: usize) -> usize { (self.0 as usize) >> rhs } }
impl core::ops::Shr<usize> for BitsInBytes64 { type Output = usize; fn shr(self, rhs: usize) -> usize { (self.0 as usize) >> rhs } }
pub struct GenericArray;
impl GenericArray { pub fn default<T: Default>() -> Vec<T> { Vec::new() } pub fn generate<T, F: FnMut(usize) -> T>(n: usize, mut f: F) -> Vec<T> { (0..n).map(f).collect() } }
pub struct CommitmentCore;
impl CommitmentCore { pub fn commit<T>(_: T, _: &u64) -> Vec<u8> { Vec::new() } }
pub fn ilog2(x: usize) -> u32 { (usize::BITS - x.leading_zeros() - 1) }
pub trait New { fn new() -> Self; }
impl New for u8 { fn new() -> Self { 0 } }
impl New for u64 { fn new() -> Self { 0 } }
impl New for Bit { fn new() -> Self { Bit(false) } }
impl New for BitsInBytes { fn new() -> Self { BitsInBytes(0) } }
impl New for BitsInBytes64 { fn new() -> Self { BitsInBytes64(0) } }
pub trait DefaultNew: Default { fn new() -> Self { Self::default() } }
impl<T: Default> DefaultNew for T {}
pub trait ToUsize { fn to_usize(&self) -> usize; }
impl ToUsize for usize { fn to_usize(&self) -> usize { *self } }
pub struct BlockSizeDyn;
pub struct OutputSizeDyn;
impl BlockSizeDyn { pub fn to_usize(&self) -> usize { 16 } }
impl OutputSizeDyn { pub fn to_usize(&self) -> usize { 16 } }
impl Default for BlockSizeDyn { fn default() -> Self { BlockSizeDyn } }
impl Default for OutputSizeDyn { fn default() -> Self { OutputSizeDyn } }
pub struct B; impl B { pub fn from<T>(_: T) -> usize { 0 } pub const BlockSize: BlockSizeDyn = BlockSizeDyn; }
pub struct D; impl D { pub fn new() -> Self { D } pub fn to_usize(&self) -> usize { 0 } pub fn finalize(&self) -> Vec<u8> { Vec::new() } pub fn update(&mut self, _: &[u8]) {} pub const OutputSize: OutputSizeDyn = OutputSizeDyn; }
pub trait DefaultVal { fn default_val() -> Self; }
impl DefaultVal for u8 { fn default_val() -> Self { 0 } }
impl DefaultVal for u64 { fn default_val() -> Self { 0 } }
impl DefaultVal for Bit { fn default_val() -> Self { Bit(false) } }
impl DefaultVal for BitsInBytes { fn default_val() -> Self { BitsInBytes(0) } }
impl DefaultVal for BitsInBytes64 { fn default_val() -> Self { BitsInBytes64(0) } }
impl<T: DefaultVal> DefaultVal for Vec<T> { fn default_val() -> Self { Vec::new() } }
impl<T> core::ops::Add<T> for PolyDyn<T> where T: core::ops::Add<Output=T> + Clone { type Output = Self; fn add(self, rhs: T) -> Self { todo!() } }
impl<T> core::ops::Mul<T> for PolyDyn<T> where T: core::ops::Mul<Output=T> + Clone { type Output = Self; fn mul(self, rhs: T) -> Self { todo!() } }
impl<T> core::ops::Add<Self> for VopeDyn<T> where T: core::ops::Add<Output=T> + Clone { type Output = Self; fn add(self, rhs: Self) -> Self { todo!() } }
impl<T> core::ops::Sub<Self> for VopeDyn<T> where T: core::ops::Sub<Output=T> + Clone { type Output = Self; fn sub(self, rhs: Self) -> Self { todo!() } }
impl<T> core::ops::Mul<T> for VopeDyn<T> where T: core::ops::Mul<Output=T> + Clone { type Output = Self; fn mul(self, rhs: T) -> Self { todo!() } }
pub type OutputDyn = Vec<u8>;
pub type Q = u8;
pub type A = u8;
pub type Delta = u8;
pub struct PolyInputPoolDyn<'a, T> {
    pub t: usize,
    pub n: usize,
    pub x: usize,
    pub inputs: &'a Vec<T>,
    pub indices: Vec<Vec<usize>>,
}
#[derive(Clone, Debug, Default)]
pub struct DeltaDyn {
    pub n: usize,
    pub t: usize,
    pub delta: Vec<T>,
}

#[derive(Clone, Debug, Default)]
pub struct QDyn {
    pub n: usize,
    pub t: usize,
    pub q: Vec<T>,
}

#[derive(Clone, Debug, Default)]
pub struct ABODyn {
    pub b: usize,
    pub d: usize,
    pub k: usize,
    pub commit: Vec<u8>,
    pub per_byte: Vec<Vec<u8>>,
}

#[derive(Clone, Debug, Default)]
pub struct ABOOpeningDyn {
    pub b: usize,
    pub d: usize,
    pub t: usize,
    pub u: usize,
    pub bad: Vec<u64>,
    pub openings: Vec<Vec<Vec<u8>>>,
}

#[derive(Clone, Debug, Default)]
pub struct PolyDyn {
    pub n: usize,
    pub t: usize,
    pub c0: T,
    pub c1: Vec<T>,
}

#[derive(Clone, Debug, Default)]
pub struct BitVoleDyn {
    pub n: usize,
    pub t: usize,
    pub u: Vec<Bit>,
    pub v: Vec<T>,
}

#[derive(Clone, Debug, Default)]
pub struct VopeDyn {
    pub n: usize,
    pub t: usize,
    pub k: usize,
    pub u: Vec<Vec<T>>,
    pub v: Vec<T>,
}


impl DeltaDyn {
    pub fn remap(&self, m_len: usize, f: _) -> DeltaDyn
    {
        let n = self.n;
        let t = self.t;
        let m = m_len;
        let _ = self;
        DeltaDyn { delta: (0..n).map(|i| self.delta[(f(i) % n())].clone()).collect(), n: n, t: t }
    }
    pub fn rotate_left(&self, n: usize) -> Self
    {
        let n = self.n;
        let t = self.t;
        self.remap(|a| a.wrapping_sub(n))
    }
    pub fn rotate_right(&self, n: usize) -> Self
    {
        let n = self.n;
        let t = self.t;
        self.remap(|a| a.wrapping_add(n))
    }
    pub fn r#static(&self, u_len: usize, val: Vec<U>) -> QDyn<OutputDyn>
    {
        let n = self.n;
        let t = self.t;
        let u = u_len;
        QDyn { q: (0..n).map(|i| (val[i].clone() * self.delta[i].clone())).collect(), n: n, t: t }
    }
}

impl QDyn {
    pub fn remap(&self, m_len: usize, f: _) -> QDyn
    {
        let n = self.n;
        let t = self.t;
        let m = m_len;
        let _ = self;
        QDyn { q: (0..n).map(|i| self.q[(f(i) % n())].clone()).collect(), n: n, t: t }
    }
    pub fn rotate_left(&self, n: usize) -> Self
    {
        let n = self.n;
        let t = self.t;
        self.remap(|a| a.wrapping_sub(n))
    }
    pub fn rotate_right(&self, n: usize) -> Self
    {
        let n = self.n;
        let t = self.t;
        self.remap(|a| a.wrapping_add(n))
    }
}


impl PolyDyn {
    pub fn get_qs_pool(&self, m_len: usize, x_len: usize, root: DeltaDyn<Q>, inputs: PolyInputPoolDyn<'_<QDyn<Q>>, reduction: usize) -> QDyn<A>
    {
        let n = self.n;
        let t = self.t;
        let m = m_len;
        let x = x_len;
        QDyn { q: (0..n).map(|i| {
    let _ = self.c0.clone().into();
    for _ in 0.. n() {
    sum = (root.delta[i].clone() * sum);
};
    for j in 0.. n() {
    let _ = self.c1[j].clone().into();
    todo!();
    sum = (sum + b);
};
    sum
}).collect(), n: n, t: t }
    }
    pub fn get_qs(&self, m_len: usize, x_len: usize, root: DeltaDyn<Q>, inputs: Vec<Vec<QDyn<Q>>>, reduction: usize) -> QDyn<A>
    {
        let n = self.n;
        let t = self.t;
        let m = m_len;
        let x = x_len;
        QDyn { q: (0..n).map(|i| {
    let _ = self.c0.clone().into();
    for _ in 0.. n() {
    sum = (root.delta[i].clone() * sum);
};
    for j in 0.. n() {
    let _ = self.c1[j].clone().into();
    todo!();
    sum = (sum + b);
};
    sum
}).collect(), n: n, t: t }
    }
    pub fn apply_pool(&self, m_len: usize, x_len: usize, s_len: usize, voles: &PolyInputPoolDyn<'_<VopeDyn<X2>>) -> VopeDyn<O, XS>
    {
        let n = self.n;
        let t = self.t;
        let m = m_len;
        let x = x_len;
        let s = s_len;
        let mut v = (0..n).map(|i| {
    let mut sum = O::default();
    for k in 0.. n() {
    let _ = self.c1[k].clone().into();
    todo!();
    sum = (sum + b);
};
    let _ = self.c0.clone().into();
    (sum + self.c0)
}).collect();
        let mut u = (0..n).map(|l| {
    (0..n).map(|i| {
    let mut sum = O::default();
    for k in 0.. n() {
    for n in 0.. x() {
    let _ = self.c1[k].clone().into();
    for m in 0.. s() {
    let mut l = ((l * s()) + m);
    todo!()
};
    sum = (sum + b);
}
};
    sum
}).collect()
}).collect();
        return VopeDyn { u: self.u, v: self.v, n: n, t: t, k: k };
    }
    pub fn apply(&self, m_len: usize, x_len: usize, s_len: usize, voles: Vec<Vec<VopeDyn<X2>>>) -> VopeDyn<O, XS>
    {
        let n = self.n;
        let t = self.t;
        let m = m_len;
        let x = x_len;
        let s = s_len;
        let mut v = (0..n).map(|i| {
    let mut sum = O::default();
    for k in 0.. n() {
    let _ = self.c1[k].clone().into();
    todo!();
    sum = (sum + b);
};
    let _ = self.c0.clone().into();
    (sum + self.c0)
}).collect();
        let mut u = (0..n).map(|l| {
    (0..n).map(|i| {
    let mut sum = O::default();
    for k in 0.. n() {
    for n in 0.. x() {
    let _ = self.c1[k].clone().into();
    for m in 0.. s() {
    let mut l = ((l * s()) + m);
    todo!()
};
    sum = (sum + b);
}
};
    sum
}).collect()
}).collect();
        return VopeDyn { u: self.u, v: self.v, n: n, t: t, k: k };
    }
}

impl VopeDyn {
    pub fn mul_generalized(&self, k2_len: usize, other: &VopeDyn) -> VopeDyn<OutputDyn>
    {
        let n = self.n;
        let t = self.t;
        let k = self.k;
        let k2 = k2_len;
        let mut res_u = GenericArray::default();
        let mut res_v = GenericArray::default();
        for i in 0..= k() {
    for j in 0..= k2() {
    let mut k = (i + j);
    let mut a_coeff = todo!();
    let mut b_coeff = todo!();
    if (k == 0) {
    for lane in 0.. n() {
    res_v[lane] = (res_v[lane].clone() + (a_coeff[lane].clone() * b_coeff[lane].clone()));
}
} else {
    for lane in 0.. n() {
    res_u[(k - 1)][lane] = (res_u[(k - 1)][lane].clone() + (a_coeff[lane].clone() * b_coeff[lane].clone()));
}
}
}
};
        VopeDyn { u: res_u, v: res_v, n: n, t: t, k: k }
    }
}

impl VopeDyn {
    pub fn rotate_left_bits(&self, n: usize) -> Self
    {
        let n = self.n;
        let t = self.t;
        let k = self.k;
        let _ = self;
        VopeDyn { u: (0..n).map(|l| (0..n).map(|i| {
    let _ = self.u[l][i].clone();
    let _ = self.u[l][((i + 1) % n())].clone();
    BitsInBytes((b.shl(n as u32) | next.shr((8 - n as u32))))
}).collect()).collect(), v: (0..n).map(|i| {
    let _ = self.v[i].clone();
    let _ = self.v[((i + 1) % n())].clone();
    BitsInBytes((b.shl(n as u32) | next.shr((8 - n as u32))))
}).collect(), n: n, t: t, k: k }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self
    {
        let n = self.n;
        let t = self.t;
        let k = self.k;
        let _ = self;
        VopeDyn { u: (0..n).map(|l| (0..n).map(|i| {
    let _ = self.u[l][(((i + n()) - 1) % n())].clone();
    let _ = self.u[l][i].clone();
    BitsInBytes((prev.shl((8 - n as u32)) | b.shr(n as u32)))
}).collect()).collect(), v: (0..n).map(|i| {
    let _ = self.v[(((i + n()) - 1) % n())].clone();
    let _ = self.v[i].clone();
    BitsInBytes((prev.shl((8 - n as u32)) | b.shr(n as u32)))
}).collect(), n: n, t: t, k: k }
    }
    pub fn bit(&self, n: u8) -> VopeDyn<Bit>
    {
        let n = self.n;
        let t = self.t;
        let k = self.k;
        let _ = self;
        VopeDyn { u: (0..n).map(|l| (0..n).map(|i| {
    let _ = self.u[l][i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect()).collect(), v: (0..n).map(|i| {
    let _ = self.v[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect(), n: n, t: t, k: k }
    }
}

impl VopeDyn {
    pub fn rotate_left_bits(&self, n: usize) -> Self
    {
        let n = self.n;
        let t = self.t;
        let k = self.k;
        let _ = self;
        VopeDyn { u: (0..n).map(|l| (0..n).map(|i| {
    let _ = self.u[l][i].clone();
    let _ = self.u[l][((i + 1) % n())].clone();
    BitsInBytes64((b.shl(n as u32) | next.shr((64 - n as u32))))
}).collect()).collect(), v: (0..n).map(|i| {
    let _ = self.v[i].clone();
    let _ = self.v[((i + 1) % n())].clone();
    BitsInBytes64((b.shl(n as u32) | next.shr((64 - n as u32))))
}).collect(), n: n, t: t, k: k }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self
    {
        let n = self.n;
        let t = self.t;
        let k = self.k;
        let _ = self;
        VopeDyn { u: (0..n).map(|l| (0..n).map(|i| {
    let _ = self.u[l][(((i + n()) - 1) % n())].clone();
    let _ = self.u[l][i].clone();
    BitsInBytes64((prev.shl((64 - n as u32)) | b.shr(n as u32)))
}).collect()).collect(), v: (0..n).map(|i| {
    let _ = self.v[(((i + n()) - 1) % n())].clone();
    let _ = self.v[i].clone();
    BitsInBytes64((prev.shl((64 - n as u32)) | b.shr(n as u32)))
}).collect(), n: n, t: t, k: k }
    }
    pub fn bit(&self, n: u8) -> VopeDyn<Bit>
    {
        let n = self.n;
        let t = self.t;
        let k = self.k;
        let _ = self;
        VopeDyn { u: (0..n).map(|l| (0..n).map(|i| {
    let _ = self.u[l][i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect()).collect(), v: (0..n).map(|i| {
    let _ = self.v[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect(), n: n, t: t, k: k }
    }
}

impl VopeDyn {
    pub fn clone(&self) -> Self
    {
        let n = self.n;
        let t = self.t;
        let k = self.k;
        let _ = self;
        VopeDyn { u: (0..n).map(|l| (0..n).map(|i| self.u[l][i].clone()).collect()).collect(), v: (0..n).map(|i| self.v[i].clone()).collect(), n: n, t: t, k: k }
    }
}

impl QDyn {
    pub fn clone(&self) -> Self
    {
        let n = self.n;
        let t = self.t;
        let _ = self;
        QDyn { q: (0..n).map(|i| self.q[i].clone()).collect(), n: n, t: t }
    }
}

impl DeltaDyn {
    pub fn clone(&self) -> Self
    {
        let n = self.n;
        let t = self.t;
        let _ = self;
        DeltaDyn { delta: (0..n).map(|i| self.delta[i].clone()).collect(), n: n, t: t }
    }
}

impl VopeDyn {
    pub fn eq(&self, other: &Self) -> bool
    {
        let n = self.n;
        let t = self.t;
        let k = self.k;
        let _ = self;
        let _ = other;
        for l in 0.. k() {
    for i in 0.. n() {
    if (u1[l][i] != u2[l][i]) {
    return false;
}
}
};
        for i in 0.. n() {
    if (v1[i] != v2[i]) {
    return false;
}
};
        true
    }
}

impl QDyn {
    pub fn eq(&self, other: &Self) -> bool
    {
        let n = self.n;
        let t = self.t;
        let _ = self;
        let _ = other;
        for i in 0.. n() {
    if (q1[i] != q2[i]) {
    return false;
}
};
        true
    }
}

impl DeltaDyn {
    pub fn eq(&self, other: &Self) -> bool
    {
        let n = self.n;
        let t = self.t;
        let _ = self;
        let _ = other;
        for i in 0.. n() {
    if (d1[i] != d2[i]) {
    return false;
}
};
        true
    }
}

impl VopeDyn {
}

impl QDyn {
}

impl DeltaDyn {
}

impl QDyn {
    pub fn rotate_left_bits(&self, n: usize) -> Self
    {
        let n = self.n;
        let t = self.t;
        let _ = self;
        QDyn { q: (0..n).map(|i| {
    let _ = self.q[i].clone();
    let _ = self.q[((i + 1) % n())].clone();
    BitsInBytes((b.shl(n as u32) | next.shr((8 - n as u32))))
}).collect(), n: n, t: t }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self
    {
        let n = self.n;
        let t = self.t;
        let _ = self;
        QDyn { q: (0..n).map(|i| {
    let _ = self.q[(((i + n()) - 1) % n())].clone();
    let _ = self.q[i].clone();
    BitsInBytes((prev.shl((8 - n as u32)) | b.shr(n as u32)))
}).collect(), n: n, t: t }
    }
    pub fn bit(&self, n: u8) -> QDyn<Bit>
    {
        let n = self.n;
        let t = self.t;
        let _ = self;
        QDyn { q: (0..n).map(|i| {
    let _ = self.q[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect(), n: n, t: t }
    }
}

impl QDyn {
    pub fn rotate_left_bits(&self, n: usize) -> Self
    {
        let n = self.n;
        let t = self.t;
        let _ = self;
        QDyn { q: (0..n).map(|i| {
    let _ = self.q[i].clone();
    let _ = self.q[((i + 1) % n())].clone();
    BitsInBytes64((b.shl(n as u32) | next.shr((64 - n as u32))))
}).collect(), n: n, t: t }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self
    {
        let n = self.n;
        let t = self.t;
        let _ = self;
        QDyn { q: (0..n).map(|i| {
    let _ = self.q[(((i + n()) - 1) % n())].clone();
    let _ = self.q[i].clone();
    BitsInBytes64((prev.shl((64 - n as u32)) | b.shr(n as u32)))
}).collect(), n: n, t: t }
    }
    pub fn bit(&self, n: u8) -> QDyn<Bit>
    {
        let n = self.n;
        let t = self.t;
        let _ = self;
        QDyn { q: (0..n).map(|i| {
    let _ = self.q[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect(), n: n, t: t }
    }
}

impl DeltaDyn {
    pub fn rotate_left_bits(&self, n: usize) -> Self
    {
        let n = self.n;
        let t = self.t;
        let _ = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let _ = self.delta[i].clone();
    let _ = self.delta[((i + 1) % n())].clone();
    BitsInBytes((b.shl(n as u32) | next.shr((8 - n as u32))))
}).collect(), n: n, t: t }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self
    {
        let n = self.n;
        let t = self.t;
        let _ = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let _ = self.delta[(((i + n()) - 1) % n())].clone();
    let _ = self.delta[i].clone();
    BitsInBytes((prev.shl((8 - n as u32)) | b.shr(n as u32)))
}).collect(), n: n, t: t }
    }
    pub fn bit(&self, n: u8) -> DeltaDyn<Bit>
    {
        let n = self.n;
        let t = self.t;
        let _ = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let _ = self.delta[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect(), n: n, t: t }
    }
}

impl DeltaDyn {
    pub fn rotate_left_bits(&self, n: usize) -> Self
    {
        let n = self.n;
        let t = self.t;
        let _ = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let _ = self.delta[i].clone();
    let _ = self.delta[((i + 1) % n())].clone();
    BitsInBytes64((b.shl(n as u32) | next.shr((64 - n as u32))))
}).collect(), n: n, t: t }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self
    {
        let n = self.n;
        let t = self.t;
        let _ = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let _ = self.delta[(((i + n()) - 1) % n())].clone();
    let _ = self.delta[i].clone();
    BitsInBytes64((prev.shl((64 - n as u32)) | b.shr(n as u32)))
}).collect(), n: n, t: t }
    }
    pub fn bit(&self, n: u8) -> DeltaDyn<Bit>
    {
        let n = self.n;
        let t = self.t;
        let _ = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let _ = self.delta[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect(), n: n, t: t }
    }
}

impl VopeDyn {
    pub fn constant(v: Vec<T>) -> Self
    {
        VopeDyn { u: (0..n).map(|_| todo!()).collect(), v: self.v, n: n, t: t, k: k }
    }
}

impl VopeDyn {
    pub fn add(self, rhs: VopeDyn) -> OutputDyn
    {
        let n = self.n;
        let t = self.t;
        let k = self.k;
        VopeDyn { u: self.u.iter().zip(rhs.u.iter()).map(|(a, b)| a.iter().zip(b.iter()).map(|(a, b)| (a + b)).collect()).collect(), v: self.v.iter().zip(rhs.v.iter()).map(|(a, b)| (a + b)).collect(), n: n, t: t, k: k }
    }
}

impl<O> VopeDyn<O> {
    pub fn bitxor(self, rhs: Vec<U>) -> OutputDyn
    {
        let n = self.n;
        let t = self.t;
        let k = self.k;
        VopeDyn { u: (0..n).map(|i| {
    (0..n).map(|j| {
    let _ = self.u[i][j].clone().bitxor(rhs[((i * k()) + j)].clone());
    o
}).collect()
}).collect(), v: self.v.iter().map(|a| a.into()).collect(), n: n, t: t, k: k }
    }
}

impl<O> VopeDyn<O> {
    pub fn mul(self, rhs: DeltaDyn) -> OutputDyn
    {
        let n = self.n;
        let t = self.t;
        let k = self.k;
        QDyn { q: self.u.iter().enumerate().fold(self.v.clone().iter().map(|a| a.into()).collect(), |a, _| {
    a.iter().zip(b.iter()).map(|(a, b)| {
    let mut x = rhs.delta[i].clone();
    for _ in 0.. i {
    x = (x * rhs.delta[i].clone());
};
    let _ = (b.clone() * x);
    (m + a)
}).collect()
}), n: n, t: t }
    }
}

impl VopeDyn {
    pub fn expand(&self, l_len: usize) -> VopeDyn
    {
        let n = self.n;
        let t = self.t;
        let k = self.k;
        let l = l_len;
        let _ = self;
        VopeDyn { u: (0..n).map(|l| {
    (0..n).map(|i| self.u.get(l).map_or(T::default(), |a| a[i].clone())).collect()
}).collect(), v: self.v.clone(), n: n, t: t, k: k }
    }
    pub fn rotate_left(&self, n: usize) -> Self
    {
        let n = self.n;
        let t = self.t;
        let k = self.k;
        self.remap(|a| a.wrapping_sub(n))
    }
    pub fn rotate_right(&self, n: usize) -> Self
    {
        let n = self.n;
        let t = self.t;
        let k = self.k;
        self.remap(|a| a.wrapping_add(n))
    }
    pub fn remap(&self, m_len: usize, f: _) -> VopeDyn
    {
        let n = self.n;
        let t = self.t;
        let k = self.k;
        let m = m_len;
        let _ = self;
        VopeDyn { u: (0..n).map(|l| {
    (0..n).map(|i| self.u[l][(f(i) % n())].clone()).collect()
}).collect(), v: (0..n).map(|i| self.v[(f(i) % n())].clone()).collect(), n: n, t: t, k: k }
    }
}

impl VopeDyn {
    pub fn scale(self, t_len: usize, f: _) -> VopeDyn
    {
        let n = self.n;
        let t = self.t;
        let k = self.k;
        let t = t_len;
        let _ = self;
        VopeDyn { u: (0..n).map(|l| {
    (0..n).map(|i| {
    let _ = self.u[l][i].clone();
    f(b)
}).collect()
}).collect(), v: (0..n).map(|i| {
    let _ = self.v[i].clone();
    f(b)
}).collect(), n: n, t: t, k: k }
    }
}

impl ABODyn {
    pub fn open(&self, t_len: usize, u_len: usize, bad: Vec<u64>, rand: &_) -> ABOOpeningDyn
    {
        let b = self.b;
        let d = self.d;
        let k = self.k;
        let t = t_len;
        let u = u_len;
        ABOOpeningDyn { bad: self.bad.clone(), openings: (0..n).map(|i| {
    let mut bad = self.bad.clone();
    (0..n).map(|j| {
    let mut i2 = (i | ((j as usize) << ilog2(t())));
    if self.bad.contains(&i2 as u64) {
    let mut h = CommitmentCore::commit(&self.per_byte[i2], rand);
    (0..n).map(|j| h.as_ref().get(j).cloned().unwrap_or_default()).collect()
} else {
    (0..n).map(|j| {
    self.per_byte[i2].get(j).cloned().unwrap_or_default()
}).collect()
}
}).collect()
}).collect(), b: b, d: d, t: t, u: u }
    }
}

impl ABOOpeningDyn {
    pub fn validate(&self, commit: &Vec<u8>, rand: &_) -> bool
    {
        let b = self.b;
        let d = self.d;
        let t = self.t;
        let u = self.u;
        let mut h = D::new();
        for i in 0.. t() {
    for b in 0.. u() {
    let mut i2 = (i | ((b as usize) << ilog2(t())));
    if self.bad.contains(&i2 as u64) {
    h.update(&self.openings[i][b][(0 + D::OutputSize::to_usize())]);
} else {
    h.update(&CommitmentCore::commit(&&self.openings[i][b][(0 + B::BlockSize::to_usize())], rand));
}
}
};
        (h.finalize().as_slice() == self.commit.as_slice())
    }
    pub fn to_vole_material(&self, n_len: usize) -> Vec<VopeDyn<BlockSizeDyn, u8>>
    {
        let b = self.b;
        let d = self.d;
        let t = self.t;
        let u = self.u;
        let n = n_len;
        core::array::from_fn(|i| {
    let mut s = &self.openings[i];
    create_vole_from_material(s)
})
    }
    pub fn to_vole_material_typenum(&self, n_len: usize) -> Vec<VopeDyn<BlockSizeDyn, u8>>
    {
        let b = self.b;
        let d = self.d;
        let t = self.t;
        let u = self.u;
        let n = n_len;
        (0..n).map(|i| {
    let mut s = &self.openings[i];
    create_vole_from_material(s)
}).collect()
    }
    pub fn to_vole_material_expanded(&self, n_len: usize, x_len: usize, f: _) -> Vec<VopeDyn<BlockSizeDyn, u8>>
    {
        let b = self.b;
        let d = self.d;
        let t = self.t;
        let u = self.u;
        let n = n_len;
        let x = x_len;
        core::array::from_fn(|i| {
    let mut s = &self.openings[i];
    create_vole_from_material_expanded(s, &mut f)
})
    }
    pub fn to_vole_material_typenum_expanded(&self, n_len: usize, x_len: usize, f: _) -> Vec<VopeDyn<BlockSizeDyn, u8>>
    {
        let b = self.b;
        let d = self.d;
        let t = self.t;
        let u = self.u;
        let n = n_len;
        let x = x_len;
        (0..n).map(|i| {
    let mut s = &self.openings[i];
    create_vole_from_material_expanded(s, &mut f)
}).collect()
    }
}

impl ABODyn {
    pub fn to_vole_material(&self, n_len: usize) -> Vec<VopeDyn<BlockSizeDyn, u8>>
    {
        let b = self.b;
        let d = self.d;
        let k = self.k;
        let n = n_len;
        core::array::from_fn(|i| {
    let mut s = &self.per_byte[((i * n) + 0)][(0 + n)];
    create_vole_from_material(s)
})
    }
    pub fn to_vole_material_typenum(&self, n_len: usize) -> Vec<VopeDyn<BlockSizeDyn, u8>>
    {
        let b = self.b;
        let d = self.d;
        let k = self.k;
        let n = n_len;
        (0..n).map(|i| {
    let mut s = &self.per_byte[((i * n()) + 0)][(0 + n())];
    create_vole_from_material(s)
}).collect()
    }
    pub fn to_vole_material_expanded(&self, n_len: usize, x_len: usize, f: _) -> Vec<VopeDyn<BlockSizeDyn, u8>>
    {
        let b = self.b;
        let d = self.d;
        let k = self.k;
        let n = n_len;
        let x = x_len;
        core::array::from_fn(|i| {
    let mut s = &self.per_byte[((i * n) + 0)][(0 + n)];
    create_vole_from_material_expanded(s, &mut f)
})
    }
    pub fn to_vole_material_typenum_expanded(&self, n_len: usize, x_len: usize, f: _) -> Vec<VopeDyn<BlockSizeDyn, u8>>
    {
        let b = self.b;
        let d = self.d;
        let k = self.k;
        let n = n_len;
        let x = x_len;
        (0..n).map(|i| {
    let mut s = &self.per_byte[((i * n()) + 0)][(0 + n())];
    create_vole_from_material_expanded(s, &mut f)
}).collect()
    }
}

pub fn create_vole_from_material(b_len: usize, s: &Vec<_>) -> VopeDyn<BlockSizeDyn, u8>
{
    let b = b_len;
    let _ = s.iter().fold(GenericArray::default(), |a, b| {
    a.iter().zip((0..n).map(|i| b[i]).collect().iter()).map(|(a, b)| a.bitxor(b)).collect()
});
    let _ = s.iter().enumerate().fold(GenericArray::default(), |a, _| {
    a.iter().zip((0..n).map(|i| b[i]).collect().iter()).map(|(a, b)| {
    a.bitxor(b).bitxor(i as u8)
}).collect()
});
    VopeDyn { u: (0..n).map(|_| self.u.clone()).collect(), v: self.v, n: n, t: t, k: k }
}

pub fn create_vole_from_material_expanded(b_len: usize, x_len: usize, s: &Vec<_>, f: _) -> VopeDyn<BlockSizeDyn, u8>
{
    let b = b_len;
    let x = x_len;
    let _ = s.iter().iter().map(|b| f(&b[(0 + B::BlockSize::to_usize())])).collect().fold(GenericArray::default(), |a, b| {
    a.iter().zip((0..n).map(|i| b.as_ref()[i]).collect().iter()).map(|(a, b)| {
    a.bitxor(b)
}).collect()
});
    let _ = s.iter().iter().map(|b| f(&b[(0 + B::BlockSize::to_usize())])).collect().enumerate().fold(GenericArray::default(), |a, _| {
    a.iter().zip((0..n).map(|i| b.as_ref()[i]).collect().iter()).map(|(a, b)| {
    a.bitxor(b).bitxor(i as u8)
}).collect()
});
    VopeDyn { u: (0..n).map(|_| self.u.clone()).collect(), v: self.v, n: n, t: t, k: k }
}

pub fn double(b_len: usize, a: Vec<u8>) -> Vec<Vec<u8>>
{
    let b = b_len;
    return core::array::from_fn(|i| {
    let mut out = a.clone();
    let mut b = B::from(todo!());
    b.encryptblock(&mut out);
    out
});
}

