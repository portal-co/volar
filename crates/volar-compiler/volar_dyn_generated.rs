use alloc::vec::Vec;
use core::ops::{Add, Sub, Mul, Div, Rem, BitAnd, BitOr, BitXor, Neg, Not};

pub struct DeltaDyn<T> {
    pub n: usize,
    pub delta: Vec<T>,
}

pub struct QDyn<T> {
    pub n: usize,
    pub q: Vec<T>,
}

pub struct ABODyn {
    pub b: usize,
    pub d: usize,
    pub k: usize,
    pub commit: Vec<u8>,
    pub per_byte: Vec<Vec<u8>>,
}

pub struct ABOOpeningDyn {
    pub b: usize,
    pub d: usize,
    pub t: usize,
    pub u: usize,
    pub bad: Vec<u64>,
    pub openings: Vec<Vec<Vec<u8>>>,
}

pub struct PolyDyn<T> {
    pub n: usize,
    pub c0: T,
    pub c1: Vec<T>,
}

pub struct PolyInputPoolDyn<T> {
    pub n: usize,
    pub x: usize,
    pub inputs: &Vec<T>,
    pub indices: Vec<Vec<usize>>,
}

pub struct BitVoleDyn<T> {
    pub n: usize,
    pub u: Vec<Bit>,
    pub v: Vec<T>,
}

pub struct VopeDyn<T> {
    pub n: usize,
    pub k: usize,
    pub u: Vec<Vec<T>>,
    pub v: Vec<T>,
}


impl<T> DeltaDyn<T> {
    pub fn remap(&self, m_len: usize, f: _) -> DeltaDyn<M, T>
    {
        let _ = self;
        DeltaDyn { delta: (0..n).map(|i| delta[(f(i) % N::to_usize())].clone()).collect() }
    }
    pub fn rotate_left(&self, n: usize) -> Self
    {
        self.remap(|a| a.wrapping_sub(n))
    }
    pub fn rotate_right(&self, n: usize) -> Self
    {
        self.remap(|a| a.wrapping_add(n))
    }
    pub fn r#static(&self, u_len: usize, val: Vec<U>) -> QDyn<N, OutputDyn>
    {
        assert_eq!(val.len(), n);
        QDyn { q: (0..n).map(|i| (val[i].clone() * self.delta[i].clone())).collect() }
    }
}

impl<T> QDyn<T> {
    pub fn remap(&self, m_len: usize, f: _) -> QDyn<M, T>
    {
        let _ = self;
        QDyn { q: (0..n).map(|i| q[(f(i) % N::to_usize())].clone()).collect() }
    }
    pub fn rotate_left(&self, n: usize) -> Self
    {
        self.remap(|a| a.wrapping_sub(n))
    }
    pub fn rotate_right(&self, n: usize) -> Self
    {
        self.remap(|a| a.wrapping_add(n))
    }
}


impl<T> PolyDyn<T> {
    pub fn get_qs_pool(&self, m_len: usize, , x_len: usize, root: DeltaDyn<M, Q>, inputs: PolyInputPoolDyn<QDyn<M, Q>, N, X>, reduction: usize) -> QDyn<M, A>
    {
        QDyn { q: (0..n).map(|i| {
    let _ = self.c0.clone().into();
    for _ in 0.. N::to_usize(){
    sum = (root.delta[i].clone() * sum);
};
    for j in 0.. N::to_usize(){
    let _ = self.c1[j].clone().into();
    todo!();
    sum = (sum + b);
};
    sum
}).collect() }
    }
    pub fn get_qs(&self, m_len: usize, , x_len: usize, root: DeltaDyn<M, Q>, inputs: Vec<Vec<QDyn<M, Q>>>, reduction: usize) -> QDyn<M, A>
    {
        assert_eq!(inputs.len(), x);
        QDyn { q: (0..n).map(|i| {
    let _ = self.c0.clone().into();
    for _ in 0.. N::to_usize(){
    sum = (root.delta[i].clone() * sum);
};
    for j in 0.. N::to_usize(){
    let _ = self.c1[j].clone().into();
    todo!();
    sum = (sum + b);
};
    sum
}).collect() }
    }
    pub fn apply_pool(&self, m_len: usize, , x_len: usize, voles: &PolyInputPoolDyn<VopeDyn<M, T, X2>, N, X>) -> VopeDyn<M, O, XS>
    {
        let v = (0..n).map(|i| {
    let sum = O::default();
    for k in 0.. N::to_usize(){
    let _ = self.c1[k].clone().into();
    todo!();
    sum = (sum + b);
};
    let _ = self.c0.clone().into();
    (sum + c0)
}).collect();
        let u = (0..n).map(|l| {
    (0..n).map(|i| {
    let sum = O::default();
    for k in 0.. N::to_usize(){
    for n in 0.. X::to_usize(){
    let _ = self.c1[k].clone().into();
    for m in 0.. S::to_usize(){
    let l = ((l * S::to_usize()) + m);
    todo!()
};
    sum = (sum + b);
}
};
    sum
}).collect()
}).collect();
        return VopeDyn { u: u, v: v };
    }
    pub fn apply(&self, m_len: usize, , x_len: usize, voles: Vec<Vec<VopeDyn<M, T, X2>>>) -> VopeDyn<M, O, XS>
    {
        assert_eq!(voles.len(), x);
        let v = (0..n).map(|i| {
    let sum = O::default();
    for k in 0.. N::to_usize(){
    let _ = self.c1[k].clone().into();
    todo!();
    sum = (sum + b);
};
    let _ = self.c0.clone().into();
    (sum + c0)
}).collect();
        let u = (0..n).map(|l| {
    (0..n).map(|i| {
    let sum = O::default();
    for k in 0.. N::to_usize(){
    for n in 0.. X::to_usize(){
    let _ = self.c1[k].clone().into();
    for m in 0.. S::to_usize(){
    let l = ((l * S::to_usize()) + m);
    todo!()
};
    sum = (sum + b);
}
};
    sum
}).collect()
}).collect();
        return VopeDyn { u: u, v: v };
    }
}

impl<T> VopeDyn<T> {
    pub fn mul_generalized(&self, other: &VopeDyn<N, T, K2>) -> VopeDyn<N, T, OutputDyn>
    {
        let res_u = GenericArray::default();
        let res_v = GenericArray::default();
        for i in 0..= K::to_usize(){
    for j in 0..= K2::to_usize(){
    let k = (i + j);
    let a_coeff = todo!();
    let b_coeff = todo!();
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
        VopeDyn { u: res_u, v: res_v }
    }
}

impl VopeDyn {
    pub fn rotate_left_bits(&self, n: usize) -> Self
    {
        let _ = self;
        VopeDyn { u: (0..n).map(|l| (0..n).map(|i| {
    let _ = u[l][i].clone();
    let _ = u[l][((i + 1) % N::to_usize())].clone();
    BitsInBytes((b.shl(n as u32) | next.shr((8 - n as u32))))
}).collect()).collect(), v: (0..n).map(|i| {
    let _ = v[i].clone();
    let _ = v[((i + 1) % N::to_usize())].clone();
    BitsInBytes((b.shl(n as u32) | next.shr((8 - n as u32))))
}).collect() }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self
    {
        let _ = self;
        VopeDyn { u: (0..n).map(|l| (0..n).map(|i| {
    let _ = u[l][(((i + N::to_usize()) - 1) % N::to_usize())].clone();
    let _ = u[l][i].clone();
    BitsInBytes((prev.shl((8 - n as u32)) | b.shr(n as u32)))
}).collect()).collect(), v: (0..n).map(|i| {
    let _ = v[(((i + N::to_usize()) - 1) % N::to_usize())].clone();
    let _ = v[i].clone();
    BitsInBytes((prev.shl((8 - n as u32)) | b.shr(n as u32)))
}).collect() }
    }
    pub fn bit(&self, n: u8) -> VopeDyn<N, Bit, K>
    {
        let _ = self;
        VopeDyn { u: (0..n).map(|l| (0..n).map(|i| {
    let _ = u[l][i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect()).collect(), v: (0..n).map(|i| {
    let _ = v[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect() }
    }
}

impl VopeDyn {
    pub fn rotate_left_bits(&self, n: usize) -> Self
    {
        let _ = self;
        VopeDyn { u: (0..n).map(|l| (0..n).map(|i| {
    let _ = u[l][i].clone();
    let _ = u[l][((i + 1) % N::to_usize())].clone();
    BitsInBytes64((b.shl(n as u32) | next.shr((64 - n as u32))))
}).collect()).collect(), v: (0..n).map(|i| {
    let _ = v[i].clone();
    let _ = v[((i + 1) % N::to_usize())].clone();
    BitsInBytes64((b.shl(n as u32) | next.shr((64 - n as u32))))
}).collect() }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self
    {
        let _ = self;
        VopeDyn { u: (0..n).map(|l| (0..n).map(|i| {
    let _ = u[l][(((i + N::to_usize()) - 1) % N::to_usize())].clone();
    let _ = u[l][i].clone();
    BitsInBytes64((prev.shl((64 - n as u32)) | b.shr(n as u32)))
}).collect()).collect(), v: (0..n).map(|i| {
    let _ = v[(((i + N::to_usize()) - 1) % N::to_usize())].clone();
    let _ = v[i].clone();
    BitsInBytes64((prev.shl((64 - n as u32)) | b.shr(n as u32)))
}).collect() }
    }
    pub fn bit(&self, n: u8) -> VopeDyn<N, Bit, K>
    {
        let _ = self;
        VopeDyn { u: (0..n).map(|l| (0..n).map(|i| {
    let _ = u[l][i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect()).collect(), v: (0..n).map(|i| {
    let _ = v[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect() }
    }
}

impl<T> VopeDyn<T> {
    pub fn clone(&self) -> Self
    {
        let _ = self;
        VopeDyn { u: (0..n).map(|l| (0..n).map(|i| u[l][i].clone()).collect()).collect(), v: (0..n).map(|i| v[i].clone()).collect() }
    }
}

impl<T> QDyn<T> {
    pub fn clone(&self) -> Self
    {
        let _ = self;
        QDyn { q: (0..n).map(|i| q[i].clone()).collect() }
    }
}

impl<T> DeltaDyn<T> {
    pub fn clone(&self) -> Self
    {
        let _ = self;
        DeltaDyn { delta: (0..n).map(|i| delta[i].clone()).collect() }
    }
}

impl<T> VopeDyn<T> {
    pub fn eq(&self, other: &Self) -> bool
    {
        let _ = self;
        let _ = other;
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

impl<T> QDyn<T> {
    pub fn eq(&self, other: &Self) -> bool
    {
        let _ = self;
        let _ = other;
        for i in 0.. N::to_usize(){
    if (q1[i] != q2[i]){
    return false;
}
};
        true
    }
}

impl<T> DeltaDyn<T> {
    pub fn eq(&self, other: &Self) -> bool
    {
        let _ = self;
        let _ = other;
        for i in 0.. N::to_usize(){
    if (d1[i] != d2[i]){
    return false;
}
};
        true
    }
}

impl<T> VopeDyn<T> {
}

impl<T> QDyn<T> {
}

impl<T> DeltaDyn<T> {
}

impl QDyn {
    pub fn rotate_left_bits(&self, n: usize) -> Self
    {
        let _ = self;
        QDyn { q: (0..n).map(|i| {
    let _ = q[i].clone();
    let _ = q[((i + 1) % N::to_usize())].clone();
    BitsInBytes((b.shl(n as u32) | next.shr((8 - n as u32))))
}).collect() }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self
    {
        let _ = self;
        QDyn { q: (0..n).map(|i| {
    let _ = q[(((i + N::to_usize()) - 1) % N::to_usize())].clone();
    let _ = q[i].clone();
    BitsInBytes((prev.shl((8 - n as u32)) | b.shr(n as u32)))
}).collect() }
    }
    pub fn bit(&self, n: u8) -> QDyn<N, Bit>
    {
        let _ = self;
        QDyn { q: (0..n).map(|i| {
    let _ = q[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect() }
    }
}

impl QDyn {
    pub fn rotate_left_bits(&self, n: usize) -> Self
    {
        let _ = self;
        QDyn { q: (0..n).map(|i| {
    let _ = q[i].clone();
    let _ = q[((i + 1) % N::to_usize())].clone();
    BitsInBytes64((b.shl(n as u32) | next.shr((64 - n as u32))))
}).collect() }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self
    {
        let _ = self;
        QDyn { q: (0..n).map(|i| {
    let _ = q[(((i + N::to_usize()) - 1) % N::to_usize())].clone();
    let _ = q[i].clone();
    BitsInBytes64((prev.shl((64 - n as u32)) | b.shr(n as u32)))
}).collect() }
    }
    pub fn bit(&self, n: u8) -> QDyn<N, Bit>
    {
        let _ = self;
        QDyn { q: (0..n).map(|i| {
    let _ = q[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect() }
    }
}

impl DeltaDyn {
    pub fn rotate_left_bits(&self, n: usize) -> Self
    {
        let _ = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let _ = delta[i].clone();
    let _ = delta[((i + 1) % N::to_usize())].clone();
    BitsInBytes((b.shl(n as u32) | next.shr((8 - n as u32))))
}).collect() }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self
    {
        let _ = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let _ = delta[(((i + N::to_usize()) - 1) % N::to_usize())].clone();
    let _ = delta[i].clone();
    BitsInBytes((prev.shl((8 - n as u32)) | b.shr(n as u32)))
}).collect() }
    }
    pub fn bit(&self, n: u8) -> DeltaDyn<N, Bit>
    {
        let _ = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let _ = delta[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect() }
    }
}

impl DeltaDyn {
    pub fn rotate_left_bits(&self, n: usize) -> Self
    {
        let _ = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let _ = delta[i].clone();
    let _ = delta[((i + 1) % N::to_usize())].clone();
    BitsInBytes64((b.shl(n as u32) | next.shr((64 - n as u32))))
}).collect() }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self
    {
        let _ = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let _ = delta[(((i + N::to_usize()) - 1) % N::to_usize())].clone();
    let _ = delta[i].clone();
    BitsInBytes64((prev.shl((64 - n as u32)) | b.shr(n as u32)))
}).collect() }
    }
    pub fn bit(&self, n: u8) -> DeltaDyn<N, Bit>
    {
        let _ = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let _ = delta[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect() }
    }
}

impl<T> VopeDyn<T> {
    pub fn constant(v: Vec<T>) -> Self
    {
        assert_eq!(v.len(), n);
        VopeDyn { u: (0..n).map(|_| todo!()).collect(), v: v }
    }
}

impl<T> VopeDyn<T> {
    pub fn add(self, rhs: VopeDyn<N, U, K>) -> OutputDyn
    {
        VopeDyn { u: self.u.iter().zip(rhs.u.iter()).map(|(a, b)| a.iter().zip(b.iter()).map(|(a, b)| (a + b)).collect()).collect(), v: self.v.iter().zip(rhs.v.iter()).map(|(a, b)| (a + b)).collect() }
    }
}

impl<T, O> VopeDyn<T, O> {
    pub fn bitxor(self, rhs: Vec<U>) -> OutputDyn
    {
        assert_eq!(rhs.len(), output);
        VopeDyn { u: (0..n).map(|i| {
    (0..n).map(|j| {
    let _ = self.u[i][j].clone().bitxor(rhs[((i * K::to_usize()) + j)].clone());
    o
}).collect()
}).collect(), v: self.v.iter().map(|a| a.into()).collect() }
    }
}

impl<T, O> VopeDyn<T, O> {
    pub fn mul(self, rhs: DeltaDyn<N, U>) -> OutputDyn
    {
        QDyn { q: self.u.iter().enumerate().fold(self.v.clone().iter().map(|a| a.into()).collect(), |a, _| {
    a.iter().zip(b.iter()).map(|(a, b)| {
    let x = rhs.delta[i].clone();
    for _ in 0.. i{
    x = (x * rhs.delta[i].clone());
};
    let _ = (b.clone() * x);
    (m + a)
}).collect()
}) }
    }
}

impl<T> VopeDyn<T> {
    pub fn expand(&self, l_len: usize) -> VopeDyn<N, T, L>
    {
        let _ = self;
        VopeDyn { u: (0..n).map(|l| {
    (0..n).map(|i| u.get(l).map_or(T::default(), |a| a[i].clone())).collect()
}).collect(), v: v.clone() }
    }
    pub fn rotate_left(&self, n: usize) -> Self
    {
        self.remap(|a| a.wrapping_sub(n))
    }
    pub fn rotate_right(&self, n: usize) -> Self
    {
        self.remap(|a| a.wrapping_add(n))
    }
    pub fn remap(&self, m_len: usize, f: _) -> VopeDyn<M, T, K>
    {
        let _ = self;
        VopeDyn { u: (0..n).map(|l| {
    (0..n).map(|i| u[l][(f(i) % N::to_usize())].clone()).collect()
}).collect(), v: (0..n).map(|i| v[(f(i) % N::to_usize())].clone()).collect() }
    }
}

impl VopeDyn {
    pub fn scale(self, f: _) -> VopeDyn<N, T, K>
    {
        let _ = self;
        VopeDyn { u: (0..n).map(|l| {
    (0..n).map(|i| {
    let _ = u[l][i].clone();
    f(b)
}).collect()
}).collect(), v: (0..n).map(|i| {
    let _ = v[i].clone();
    f(b)
}).collect() }
    }
}

impl ABODyn {
    pub fn open(&self, u_len: usize, bad: Vec<u64>, rand: &_) -> ABOOpeningDyn<B, D, T, U>
    {
        assert_eq!(bad.len(), t);
        ABOOpeningDyn { bad: bad.clone(), openings: (0..n).map(|i| {
    let bad = bad.clone();
    (0..n).map(|j| {
    let i2 = (i | (j as usize << T::to_usize().ilog2()));
    if bad.contains(&i2 as u64){
    let h = CommitmentCore::commit(&self.per_byte[i2], rand);
    (0..n).map(|j| h.as_ref().get(j).cloned().unwrap_or_default()).collect()
} else {
    (0..n).map(|j| {
    self.per_byte[i2].get(j).cloned().unwrap_or_default()
}).collect()
}
}).collect()
}).collect() }
    }
}

impl<T> ABOOpeningDyn<T> {
    pub fn validate(&self, commit: &Vec<u8>, rand: &_) -> bool
    {
        let h = D::new();
        for i in 0.. T::to_usize(){
    for b in 0.. U::to_usize(){
    let i2 = (i | (b as usize << T::to_usize().ilog2()));
    if self.bad.contains(&i2 as u64){
    h.update(&self.openings[i][b][(0 ??? D::OutputSize::to_usize())]);
} else {
    h.update(&CommitmentCore::commit(&&self.openings[i][b][(0 ??? B::BlockSize::to_usize())], rand));
}
}
};
        (h.finalize().as_slice() == commit.as_slice())
    }
    pub fn to_vole_material(&self, n_len: usize) -> Vec<VopeDyn<BlockSizeDyn, u8>>
    {
        core::array::from_fn(|i| {
    let s = &self.openings[i];
    create_vole_from_material(s)
})
    }
    pub fn to_vole_material_typenum(&self, n_len: usize) -> Vec<VopeDyn<BlockSizeDyn, u8>>
    {
        (0..n).map(|i| {
    let s = &self.openings[i];
    create_vole_from_material(s)
}).collect()
    }
    pub fn to_vole_material_expanded(&self, n_len: usize, , x_len: usize, f: _) -> Vec<VopeDyn<BlockSizeDyn, u8>>
    {
        core::array::from_fn(|i| {
    let s = &self.openings[i];
    create_vole_from_material_expanded(s, &mut f)
})
    }
    pub fn to_vole_material_typenum_expanded(&self, n_len: usize, , x_len: usize, f: _) -> Vec<VopeDyn<BlockSizeDyn, u8>>
    {
        (0..n).map(|i| {
    let s = &self.openings[i];
    create_vole_from_material_expanded(s, &mut f)
}).collect()
    }
}

impl ABODyn {
    pub fn to_vole_material(&self, n_len: usize) -> Vec<VopeDyn<BlockSizeDyn, u8>>
    {
        core::array::from_fn(|i| {
    let s = &self.per_byte[((i * N) ??? 0)][(0 ??? N)];
    create_vole_from_material(s)
})
    }
    pub fn to_vole_material_typenum(&self, n_len: usize) -> Vec<VopeDyn<BlockSizeDyn, u8>>
    {
        (0..n).map(|i| {
    let s = &self.per_byte[((i * N::to_usize()) ??? 0)][(0 ??? N::to_usize())];
    create_vole_from_material(s)
}).collect()
    }
    pub fn to_vole_material_expanded(&self, n_len: usize, , x_len: usize, f: _) -> Vec<VopeDyn<BlockSizeDyn, u8>>
    {
        core::array::from_fn(|i| {
    let s = &self.per_byte[((i * N) ??? 0)][(0 ??? N)];
    create_vole_from_material_expanded(s, &mut f)
})
    }
    pub fn to_vole_material_typenum_expanded(&self, n_len: usize, , x_len: usize, f: _) -> Vec<VopeDyn<BlockSizeDyn, u8>>
    {
        (0..n).map(|i| {
    let s = &self.per_byte[((i * N::to_usize()) ??? 0)][(0 ??? N::to_usize())];
    create_vole_from_material_expanded(s, &mut f)
}).collect()
    }
}

pub fn create_vole_from_material(b_len: usize, s: &Vec<_>) -> VopeDyn<BlockSizeDyn, u8>
{
    let _ = s.iter().fold(GenericArray::default(), |a, b| {
    a.iter().zip((0..n).map(|i| b[i]).collect().iter()).map(|(a, b)| a.bitxor(b)).collect()
});
    let _ = s.iter().enumerate().fold(GenericArray::default(), |a, _| {
    a.iter().zip((0..n).map(|i| b[i]).collect().iter()).map(|(a, b)| {
    a.bitxor(b).bitxor(i as u8)
}).collect()
});
    VopeDyn { u: (0..n).map(|_| u.clone()).collect(), v: v }
}

pub fn create_vole_from_material_expanded(b_len: usize, , x_len: usize, s: &Vec<_>, f: _) -> VopeDyn<BlockSizeDyn, u8>
{
    let _ = s.iter().iter().map(|b| f(&b[(0 ??? B::BlockSize::to_usize())])).collect().fold(GenericArray::default(), |a, b| {
    a.iter().zip((0..n).map(|i| b.as_ref()[i]).collect().iter()).map(|(a, b)| {
    a.bitxor(b)
}).collect()
});
    let _ = s.iter().iter().map(|b| f(&b[(0 ??? B::BlockSize::to_usize())])).collect().enumerate().fold(GenericArray::default(), |a, _| {
    a.iter().zip((0..n).map(|i| b.as_ref()[i]).collect().iter()).map(|(a, b)| {
    a.bitxor(b).bitxor(i as u8)
}).collect()
});
    VopeDyn { u: (0..n).map(|_| u.clone()).collect(), v: v }
}

pub fn double(b_len: usize, a: Vec<u8>) -> Vec<Vec<u8>>
{
    assert_eq!(a.len(), blocksize);
    return core::array::from_fn(|i| {
    let out = a.clone();
    let b = B::from(todo!());
    b.encryptblock(&mut out);
    out
});
}

