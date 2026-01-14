use alloc::vec::Vec;
use core::ops::{Add, Sub, Mul, Div, Rem, BitAnd, BitOr, BitXor, Neg, Not};

pub struct DeltaDyn<N, T> {
    pub delta: Vec<T>,
}

pub struct QDyn<N, T> {
    pub q: Vec<T>,
}

pub struct ABODyn<B, D, K> {
    pub commit: Vec<u8>,
    pub per_byte: Vec<Vec<u8>>,
}

pub struct ABOOpeningDyn<B, D, T, U> {
    pub bad: Vec<u64>,
    pub openings: Vec<Vec<Vec<u8>>>,
}

pub struct PolyDyn<N, T> {
    pub c0: T,
    pub c1: Vec<T>,
}

pub struct PolyInputPoolDyn<T, N, X> {
    pub inputs: &Vec<T>,
    pub indices: Vec<Vec<usize>>,
}

pub struct BitVoleDyn<N, T> {
    pub u: Vec<Bit>,
    pub v: Vec<T>,
}

pub struct VopeDyn<N, T, K> {
    pub u: Vec<Vec<T>>,
    pub v: Vec<T>,
}


impl<N, T> DeltaDyn<N, T> {
    pub fn remap(&self, f: _) -> DeltaDyn<M, T>
    {
        let _ = self;
        todo!()
    }
    pub fn rotate_left(&self, n: usize) -> Self
    {
        self.remap(todo!())
    }
    pub fn rotate_right(&self, n: usize) -> Self
    {
        self.remap(todo!())
    }
    pub fn r#static(&self, val: Vec<U>) -> QDyn<N, OutputDyn>
    {
        todo!()
    }
}

impl<N, T> QDyn<N, T> {
    pub fn remap(&self, f: _) -> QDyn<M, T>
    {
        let _ = self;
        todo!()
    }
    pub fn rotate_left(&self, n: usize) -> Self
    {
        self.remap(todo!())
    }
    pub fn rotate_right(&self, n: usize) -> Self
    {
        self.remap(todo!())
    }
}


impl<N, T> PolyDyn<N, T> {
    pub fn get_qs_pool(&self, root: DeltaDyn<M, Q>, inputs: PolyInputPoolDyn<QDyn<M, Q>, N, X>, reduction: usize) -> QDyn<M, A>
    {
        todo!()
    }
    pub fn get_qs(&self, root: DeltaDyn<M, Q>, inputs: Vec<Vec<QDyn<M, Q>>>, reduction: usize) -> QDyn<M, A>
    {
        todo!()
    }
    pub fn apply_pool(&self, voles: &PolyInputPoolDyn<VopeDyn<M, T, X2>, N, X>) -> VopeDyn<M, O, XS>
    {
        let v = (0..n).map(|i| {
    let sum = todo!()();
    for k in 0.. todo!()(){
    let _ = self.c1[k].clone().into();
    todo!();
    todo!();
};
    let _ = self.c0.clone().into();
    (sum + c0)
}).collect();
        let u = (0..n).map(|l| {
    (0..n).map(|i| {
    let sum = todo!()();
    for k in 0.. todo!()(){
    for n in 0.. todo!()(){
    let _ = self.c1[k].clone().into();
    for m in 0.. todo!()(){
    let l = ((l * todo!()()) + m);
    todo!()
};
    todo!();
}
};
    sum
}).collect()
}).collect();
        todo!();
    }
    pub fn apply(&self, voles: Vec<Vec<VopeDyn<M, T, X2>>>) -> VopeDyn<M, O, XS>
    {
        let v = (0..n).map(|i| {
    let sum = todo!()();
    for k in 0.. todo!()(){
    let _ = self.c1[k].clone().into();
    todo!();
    todo!();
};
    let _ = self.c0.clone().into();
    (sum + c0)
}).collect();
        let u = (0..n).map(|l| {
    (0..n).map(|i| {
    let sum = todo!()();
    for k in 0.. todo!()(){
    for n in 0.. todo!()(){
    let _ = self.c1[k].clone().into();
    for m in 0.. todo!()(){
    let l = ((l * todo!()()) + m);
    todo!()
};
    todo!();
}
};
    sum
}).collect()
}).collect();
        todo!();
    }
}

impl<N, T, K> VopeDyn<N, T, K> {
    pub fn mul_generalized(&self, other: &VopeDyn<N, T, K2>) -> VopeDyn<N, T, OutputDyn>
    {
        let res_u = todo!()();
        let res_v = todo!()();
        for i in 0..= todo!()(){
    for j in 0..= todo!()(){
    let k = (i + j);
    let a_coeff = todo!();
    let b_coeff = todo!();
    todo!()
}
};
        todo!()
    }
}

impl<N, K> VopeDyn<N, K> {
    pub fn rotate_left_bits(&self, n: usize) -> Self
    {
        let _ = self;
        todo!()
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self
    {
        let _ = self;
        todo!()
    }
    pub fn bit(&self, n: u8) -> VopeDyn<N, Bit, K>
    {
        let _ = self;
        todo!()
    }
}

impl<N, K> VopeDyn<N, K> {
    pub fn rotate_left_bits(&self, n: usize) -> Self
    {
        let _ = self;
        todo!()
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self
    {
        let _ = self;
        todo!()
    }
    pub fn bit(&self, n: u8) -> VopeDyn<N, Bit, K>
    {
        let _ = self;
        todo!()
    }
}

impl<N, T, K> VopeDyn<N, T, K> {
    pub fn clone(&self) -> Self
    {
        let _ = self;
        todo!()
    }
}

impl<N, T> QDyn<N, T> {
    pub fn clone(&self) -> Self
    {
        let _ = self;
        todo!()
    }
}

impl<N, T> DeltaDyn<N, T> {
    pub fn clone(&self) -> Self
    {
        let _ = self;
        todo!()
    }
}

impl<N, T, K> VopeDyn<N, T, K> {
    pub fn eq(&self, other: &Self) -> bool
    {
        let _ = self;
        let _ = other;
        for l in 0.. todo!()(){
    for i in 0.. todo!()(){
    todo!()
}
};
        for i in 0.. todo!()(){
    todo!()
};
        true
    }
}

impl<N, T> QDyn<N, T> {
    pub fn eq(&self, other: &Self) -> bool
    {
        let _ = self;
        let _ = other;
        for i in 0.. todo!()(){
    todo!()
};
        true
    }
}

impl<N, T> DeltaDyn<N, T> {
    pub fn eq(&self, other: &Self) -> bool
    {
        let _ = self;
        let _ = other;
        for i in 0.. todo!()(){
    todo!()
};
        true
    }
}

impl<N, T, K> VopeDyn<N, T, K> {
}

impl<N, T> QDyn<N, T> {
}

impl<N, T> DeltaDyn<N, T> {
}

impl<N> QDyn<N> {
    pub fn rotate_left_bits(&self, n: usize) -> Self
    {
        let _ = self;
        todo!()
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self
    {
        let _ = self;
        todo!()
    }
    pub fn bit(&self, n: u8) -> QDyn<N, Bit>
    {
        let _ = self;
        todo!()
    }
}

impl<N> QDyn<N> {
    pub fn rotate_left_bits(&self, n: usize) -> Self
    {
        let _ = self;
        todo!()
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self
    {
        let _ = self;
        todo!()
    }
    pub fn bit(&self, n: u8) -> QDyn<N, Bit>
    {
        let _ = self;
        todo!()
    }
}

impl<N> DeltaDyn<N> {
    pub fn rotate_left_bits(&self, n: usize) -> Self
    {
        let _ = self;
        todo!()
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self
    {
        let _ = self;
        todo!()
    }
    pub fn bit(&self, n: u8) -> DeltaDyn<N, Bit>
    {
        let _ = self;
        todo!()
    }
}

impl<N> DeltaDyn<N> {
    pub fn rotate_left_bits(&self, n: usize) -> Self
    {
        let _ = self;
        todo!()
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self
    {
        let _ = self;
        todo!()
    }
    pub fn bit(&self, n: u8) -> DeltaDyn<N, Bit>
    {
        let _ = self;
        todo!()
    }
}

impl<N, T> VopeDyn<N, T> {
    pub fn constant(v: Vec<T>) -> Self
    {
        todo!()
    }
}

impl<N, T, U, K> VopeDyn<N, T, U, K> {
    pub fn add(self, rhs: VopeDyn<N, U, K>) -> OutputDyn
    {
        todo!()
    }
}

impl<N, T, U, O, K> VopeDyn<N, T, U, O, K> {
    pub fn bitxor(self, rhs: Vec<U>) -> OutputDyn
    {
        todo!()
    }
}

impl<N, T, U, K, O> VopeDyn<N, T, U, K, O> {
    pub fn mul(self, rhs: DeltaDyn<N, U>) -> OutputDyn
    {
        todo!()
    }
}

impl<N, T, K> VopeDyn<N, T, K> {
    pub fn expand(&self) -> VopeDyn<N, T, L>
    {
        let _ = self;
        todo!()
    }
    pub fn rotate_left(&self, n: usize) -> Self
    {
        self.remap(todo!())
    }
    pub fn rotate_right(&self, n: usize) -> Self
    {
        self.remap(todo!())
    }
    pub fn remap(&self, f: _) -> VopeDyn<M, T, K>
    {
        let _ = self;
        todo!()
    }
}

impl<N, K> VopeDyn<N, K> {
    pub fn scale(self, f: _) -> VopeDyn<N, T, K>
    {
        let _ = self;
        todo!()
    }
}

impl<B, D, K> ABODyn<B, D, K> {
    pub fn open(&self, bad: Vec<u64>, rand: &_) -> ABOOpeningDyn<B, D, T, U>
    {
        todo!()
    }
}

impl<B, D, T, U> ABOOpeningDyn<B, D, T, U> {
    pub fn validate(&self, commit: &Vec<u8>, rand: &_) -> bool
    {
        let h = todo!()();
        for i in 0.. todo!()(){
    for b in 0.. todo!()(){
    let i2 = (i | (todo!() << todo!()().ilog2()));
    todo!()
}
};
        (h.finalize().as_slice() == commit.as_slice())
    }
    pub fn to_vole_material(&self) -> Vec<VopeDyn<BlockSizeDyn, u8>>
    {
        todo!()(todo!())
    }
    pub fn to_vole_material_typenum(&self) -> Vec<VopeDyn<BlockSizeDyn, u8>>
    {
        (0..n).map(|i| {
    let s = &self.openings[i];
    create_vole_from_material(s)
}).collect()
    }
    pub fn to_vole_material_expanded(&self, f: _) -> Vec<VopeDyn<BlockSizeDyn, u8>>
    {
        todo!()(todo!())
    }
    pub fn to_vole_material_typenum_expanded(&self, f: _) -> Vec<VopeDyn<BlockSizeDyn, u8>>
    {
        (0..n).map(|i| {
    let s = &self.openings[i];
    create_vole_from_material_expanded(s, &mut f)
}).collect()
    }
}

impl<B, D, K> ABODyn<B, D, K> {
    pub fn to_vole_material(&self) -> Vec<VopeDyn<BlockSizeDyn, u8>>
    {
        todo!()(todo!())
    }
    pub fn to_vole_material_typenum(&self) -> Vec<VopeDyn<BlockSizeDyn, u8>>
    {
        (0..n).map(|i| {
    let s = &self.per_byte[((i * todo!()()) ??? 0)][(0 ??? todo!()())];
    create_vole_from_material(s)
}).collect()
    }
    pub fn to_vole_material_expanded(&self, f: _) -> Vec<VopeDyn<BlockSizeDyn, u8>>
    {
        todo!()(todo!())
    }
    pub fn to_vole_material_typenum_expanded(&self, f: _) -> Vec<VopeDyn<BlockSizeDyn, u8>>
    {
        (0..n).map(|i| {
    let s = &self.per_byte[((i * todo!()()) ??? 0)][(0 ??? todo!()())];
    create_vole_from_material_expanded(s, &mut f)
}).collect()
    }
}

pub fn create_vole_from_material(s: &Vec<_>) -> VopeDyn<BlockSizeDyn, u8>
{
    let _ = s.iter().fold(todo!()(), todo!());
    let _ = s.iter().enumerate().fold(todo!()(), todo!());
    todo!()
}

pub fn create_vole_from_material_expanded(s: &Vec<_>, f: _) -> VopeDyn<BlockSizeDyn, u8>
{
    let _ = s.iter().iter().map(|b| f(&b[(0 ??? todo!()())])).collect().fold(todo!()(), todo!());
    let _ = s.iter().iter().map(|b| f(&b[(0 ??? todo!()())])).collect().enumerate().fold(todo!()(), todo!());
    todo!()
}

pub fn double(a: Vec<u8>) -> Vec<Vec<u8>>
{
    todo!();
}

