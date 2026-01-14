//! Auto-generated dynamic types from volar-spec
//! Type-level lengths have been converted to runtime usize witnesses

#![allow(unused_variables, dead_code, unused_mut, unused_imports, non_snake_case)]

extern crate alloc;
use alloc::vec::Vec;
use alloc::vec;
use core::ops::{Add, Sub, Mul, Div, BitAnd, BitOr, BitXor, Shl, Shr};

// Primitive field types from volar-primitives
pub use volar_primitives::{Bit, BitsInBytes, BitsInBytes64, Galois, Galois64};

/// Compute integer log2
#[inline]
pub fn ilog2(x: usize) -> u32 {
    usize::BITS - x.leading_zeros() - 1
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
    pub inputs: &'a Vec<T>,
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

impl<T> PolyDyn<T>// UNCONSTRAINED GENERICS at generated.rs line 51: T
 {
    pub fn get_qs_pool<Q: Clone + Mul<compile_error!("Unknown type parameter: A"), Output = compile_error!("Unknown type parameter: A")>, A: Add<A, Output = A>> where T: Clone + Into<A>(&self, m: usize, x: usize, root: DeltaDyn<Q>, inputs: PolyInputPoolDyn<QDyn<Q>>, reduction: usize) -> QDyn<A> {
        let n = self.n;
        QDyn { q: (0..m).map(|i| {
    let _ = self.c0.clone().into();
    for _ in 0..n {
    sum = (root.delta[i].clone() * sum);
};
    for j in 0..n {
    let _ = self.c1[j].clone().into();
    for i2 in inputs.indices.iter() {
    for _ in 0..reduction {
    b = (inputs.inputs[i2[j]].q[i].clone() * b);
}
};
    sum = (sum + b);
};
    sum
}).collect() }
    }
    pub fn get_qs<Q: Clone + Mul<compile_error!("Unknown type parameter: A"), Output = compile_error!("Unknown type parameter: A")>, A: Add<A, Output = A>> where T: Clone + Into<A>(&self, m: usize, x: usize, root: DeltaDyn<Q>, inputs: Vec<Vec<QDyn<Q>>>, reduction: usize) -> QDyn<A> {
        let n = self.n;
        QDyn { q: (0..m).map(|i| {
    let _ = self.c0.clone().into();
    for _ in 0..n {
    sum = (root.delta[i].clone() * sum);
};
    for j in 0..n {
    let _ = self.c1[j].clone().into();
    for i2 in inputs.iter() {
    for _ in 0..reduction {
    b = (i2[j].q[i].clone() * b);
}
};
    sum = (sum + b);
};
    sum
}).collect() }
    }
    pub fn apply_pool<M, O: Mul<O, Output = O> + Add<O, Output = O> + Default + Clone> where T: Into<O> + Clone, M: VoleArray<T> + VoleArray<O>(&self, x: usize, x2: usize, xs: usize, s: usize, voles: &'a PolyInputPoolDyn<VopeDyn<M, T>>) -> VopeDyn<M, O> {
        let n = self.n;
        let v = (0..m).map(|i| {
    let mut sum = O::new();
    for k in 0..n {
    let _ = self.c1[k].clone().into();
    for v in &voles.indices {
    b = (b * voles.inputs[v[k]].v[i].clone().into());
};
    sum = (sum + b);
};
    let _ = self.c0.clone().into();
    (sum + c0)
}).collect();
        let u = (0..xs).map(|l| {
    (0..m).map(|i| {
    let mut sum = O::new();
    for k in 0..n {
    for n in 0..x {
    let _ = self.c1[k].clone().into();
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
}).collect()
}).collect();
        return VopeDyn { u: u, v: v, n: n, k: k };
    }
    pub fn apply<M, O: Mul<O, Output = O> + Add<O, Output = O> + Default + Clone> where T: Into<O> + Clone, M: VoleArray<T> + VoleArray<O>(&self, x: usize, x2: usize, xs: usize, s: usize, voles: Vec<Vec<VopeDyn<M, T>>>) -> VopeDyn<M, O> {
        let n = self.n;
        let v = (0..m).map(|i| {
    let mut sum = O::new();
    for k in 0..n {
    let _ = self.c1[k].clone().into();
    for v in &voles {
    b = (b * v[k].v[i].clone().into());
};
    sum = (sum + b);
};
    let _ = self.c0.clone().into();
    (sum + c0)
}).collect();
        let u = (0..xs).map(|l| {
    (0..m).map(|i| {
    let mut sum = O::new();
    for k in 0..n {
    for n in 0..x {
    let _ = self.c1[k].clone().into();
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
}).collect()
}).collect();
        return VopeDyn { u: u, v: v, n: n, k: k };
    }
}

impl<T: Add<Output = T> + Mul<Output = T> + Default + Clone> VopeDyn<T> where N: VoleArray<T>, T: Add<Output = T> + Mul<Output = T> + Default + Clone, K: ArrayLength<Vec<T>> {
    pub fn mul_generalized<K2: Add<K>> where K2: ArrayLength<Vec<T>>, OutputDyn: ArrayLength<Vec<T>>(&self, other: &'a VopeDyn<N, T, K2>) -> VopeDyn<N, T, OutputDyn> {
        let n = self.n;
        let k = self.k;
        let mut res_u = Vec::new();
        let mut res_v = Vec::new();
        for i in 0..=k {
    for j in 0..=k2 {
    let k = (i + j);
    let a_coeff = compile_error!("Unsupported expression Macro { name: "get_coeff", tokens: "& self . v , & self . u , i" }");
    let b_coeff = compile_error!("Unsupported expression Macro { name: "get_coeff", tokens: "& other . v , & other . u , j" }");
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
        VopeDyn { u: res_u, v: res_v, n: n, k: k }
    }
}

impl VopeDyn<BitsInBytes> {
    pub fn rotate_left_bits(&self, n: usize) -> Self {
        let n = self.n;
        let k = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n).map(|i| {
    let BitsInBytes(b) = u[l][i].clone();
    let BitsInBytes(next) = u[l][((i + 1) % n)].clone();
    BitsInBytes((b.shl((n as u32)) | next.shr((8 - (n as u32)))))
}).collect()
}).collect(), v: (0..n).map(|i| {
    let BitsInBytes(b) = v[i].clone();
    let BitsInBytes(next) = v[((i + 1) % n)].clone();
    BitsInBytes((b.shl((n as u32)) | next.shr((8 - (n as u32)))))
}).collect(), n: n, k: k }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self {
        let n = self.n;
        let k = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n).map(|i| {
    let BitsInBytes(prev) = u[l][(((i + n) - 1) % n)].clone();
    let BitsInBytes(b) = u[l][i].clone();
    BitsInBytes((prev.shl((8 - (n as u32))) | b.shr((n as u32))))
}).collect()
}).collect(), v: (0..n).map(|i| {
    let BitsInBytes(prev) = v[(((i + n) - 1) % n)].clone();
    let BitsInBytes(b) = v[i].clone();
    BitsInBytes((prev.shl((8 - (n as u32))) | b.shr((n as u32))))
}).collect(), n: n, k: k }
    }
    pub fn bit(&self, n: u8) -> VopeDyn<Bit> {
        let n = self.n;
        let k = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n).map(|i| {
    let BitsInBytes(b) = u[l][i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect()
}).collect(), v: (0..n).map(|i| {
    let BitsInBytes(b) = v[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect(), n: n, k: k }
    }
}

impl VopeDyn<BitsInBytes64> {
    pub fn rotate_left_bits(&self, n: usize) -> Self {
        let n = self.n;
        let k = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n).map(|i| {
    let BitsInBytes64(b) = u[l][i].clone();
    let BitsInBytes64(next) = u[l][((i + 1) % n)].clone();
    BitsInBytes64((b.shl((n as u32)) | next.shr((64 - (n as u32)))))
}).collect()
}).collect(), v: (0..n).map(|i| {
    let BitsInBytes64(b) = v[i].clone();
    let BitsInBytes64(next) = v[((i + 1) % n)].clone();
    BitsInBytes64((b.shl((n as u32)) | next.shr((64 - (n as u32)))))
}).collect(), n: n, k: k }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self {
        let n = self.n;
        let k = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n).map(|i| {
    let BitsInBytes64(prev) = u[l][(((i + n) - 1) % n)].clone();
    let BitsInBytes64(b) = u[l][i].clone();
    BitsInBytes64((prev.shl((64 - (n as u32))) | b.shr((n as u32))))
}).collect()
}).collect(), v: (0..n).map(|i| {
    let BitsInBytes64(prev) = v[(((i + n) - 1) % n)].clone();
    let BitsInBytes64(b) = v[i].clone();
    BitsInBytes64((prev.shl((64 - (n as u32))) | b.shr((n as u32))))
}).collect(), n: n, k: k }
    }
    pub fn bit(&self, n: u8) -> VopeDyn<Bit> {
        let n = self.n;
        let k = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n).map(|i| {
    let BitsInBytes64(b) = u[l][i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect()
}).collect(), v: (0..n).map(|i| {
    let BitsInBytes64(b) = v[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect(), n: n, k: k }
    }
}

impl<T: Clone> Clone for VopeDyn<T> {
    fn clone(&self) -> Self {
        let n = self.n;
        let k = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n).map(|i| u[l][i].clone()).collect()
}).collect(), v: (0..n).map(|i| v[i].clone()).collect(), n: n, k: k }
    }
}

impl<T: Clone> Clone for QDyn<T> {
    fn clone(&self) -> Self {
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n).map(|i| q[i].clone()).collect() }
    }
}

impl<T: Clone> Clone for DeltaDyn<T> {
    fn clone(&self) -> Self {
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n).map(|i| delta[i].clone()).collect() }
    }
}

impl<T: PartialEq> PartialEq for VopeDyn<T> {
    fn eq(&self, other: &'a Self) -> bool {
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
    fn eq(&self, other: &'a Self) -> bool {
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
    fn eq(&self, other: &'a Self) -> bool {
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
    pub fn rotate_left_bits(&self, n: usize) -> Self {
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n).map(|i| {
    let BitsInBytes(b) = q[i].clone();
    let BitsInBytes(next) = q[((i + 1) % n)].clone();
    BitsInBytes((b.shl((n as u32)) | next.shr((8 - (n as u32)))))
}).collect() }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self {
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n).map(|i| {
    let BitsInBytes(prev) = q[(((i + n) - 1) % n)].clone();
    let BitsInBytes(b) = q[i].clone();
    BitsInBytes((prev.shl((8 - (n as u32))) | b.shr((n as u32))))
}).collect() }
    }
    pub fn bit(&self, n: u8) -> QDyn<Bit> {
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n).map(|i| {
    let BitsInBytes(b) = q[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect() }
    }
}

impl QDyn<BitsInBytes64> {
    pub fn rotate_left_bits(&self, n: usize) -> Self {
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n).map(|i| {
    let BitsInBytes64(b) = q[i].clone();
    let BitsInBytes64(next) = q[((i + 1) % n)].clone();
    BitsInBytes64((b.shl((n as u32)) | next.shr((64 - (n as u32)))))
}).collect() }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self {
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n).map(|i| {
    let BitsInBytes64(prev) = q[(((i + n) - 1) % n)].clone();
    let BitsInBytes64(b) = q[i].clone();
    BitsInBytes64((prev.shl((64 - (n as u32))) | b.shr((n as u32))))
}).collect() }
    }
    pub fn bit(&self, n: u8) -> QDyn<Bit> {
        let QDyn { q: q, .. } = self;
        QDyn { q: (0..n).map(|i| {
    let BitsInBytes64(b) = q[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect() }
    }
}

impl DeltaDyn<BitsInBytes> {
    pub fn rotate_left_bits(&self, n: usize) -> Self {
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let BitsInBytes(b) = delta[i].clone();
    let BitsInBytes(next) = delta[((i + 1) % n)].clone();
    BitsInBytes((b.shl((n as u32)) | next.shr((8 - (n as u32)))))
}).collect() }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self {
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let BitsInBytes(prev) = delta[(((i + n) - 1) % n)].clone();
    let BitsInBytes(b) = delta[i].clone();
    BitsInBytes((prev.shl((8 - (n as u32))) | b.shr((n as u32))))
}).collect() }
    }
    pub fn bit(&self, n: u8) -> DeltaDyn<Bit> {
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let BitsInBytes(b) = delta[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect() }
    }
}

impl DeltaDyn<BitsInBytes64> {
    pub fn rotate_left_bits(&self, n: usize) -> Self {
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let BitsInBytes64(b) = delta[i].clone();
    let BitsInBytes64(next) = delta[((i + 1) % n)].clone();
    BitsInBytes64((b.shl((n as u32)) | next.shr((64 - (n as u32)))))
}).collect() }
    }
    pub fn rotate_right_bits(&self, n: usize) -> Self {
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let BitsInBytes64(prev) = delta[(((i + n) - 1) % n)].clone();
    let BitsInBytes64(b) = delta[i].clone();
    BitsInBytes64((prev.shl((64 - (n as u32))) | b.shr((n as u32))))
}).collect() }
    }
    pub fn bit(&self, n: u8) -> DeltaDyn<Bit> {
        let DeltaDyn { delta: delta, .. } = self;
        DeltaDyn { delta: (0..n).map(|i| {
    let BitsInBytes64(b) = delta[i].clone();
    Bit((((b >> n) & 1) != 0))
}).collect() }
    }
}

impl<T> VopeDyn<T>// UNCONSTRAINED GENERICS at generated.rs line 479: T
 {
    pub fn constant(v: Vec<T>) -> Self {
        VopeDyn { u: (0..n).map(|_| compile_error!("Unsupported expression Macro { name: "unreachable", tokens: "" }")).collect(), v: v, n: n, k: k }
    }
}

impl<T: Add<U> + Clone, U: Clone> Add<VopeDyn<U>> for VopeDyn<T> {
    type Output = VopeDyn<OutputDyn>;
    fn add(self, rhs: VopeDyn<U>) -> Self::Output {
        let n = self.n;
        let k = self.k;
        VopeDyn { u: self.u.iter().zip(rhs.u.iter()).map(|(a, b)| a.iter().zip(b.iter()).map(|(a, b)| (a + b)).collect()).collect(), v: self.v.iter().zip(rhs.v.iter()).map(|(a, b)| (a + b)).collect(), n: n, k: k }
    }
}

impl<T: BitXor<U, Output = O> + Clone + Into<O>, U: Clone, O> BitXor<Vec<U>> for VopeDyn<T> where T: Into<O>// UNCONSTRAINED GENERICS at generated.rs line 495: O
 {
    type Output = VopeDyn<O>;
    fn bitxor(self, rhs: Vec<U>) -> Self::Output {
        let n = self.n;
        let k = self.k;
        VopeDyn { u: (0..k).map(|i| {
    (0..n).map(|j| {
    let _ = self.u[i][j].clone().bitxor(rhs[((i * k) + j)].clone());
    o
}).collect()
}).collect(), v: self.v.iter().map(|a| a.into()).collect(), n: n, k: k }
    }
}

impl<T: Mul<U, Output = O> + Into<O> + Clone, U: Mul<U, Output = U> + Clone, O: Add<O, Output = O>> Mul<DeltaDyn<U>> for VopeDyn<T> {
    type Output = QDyn<O>;
    fn mul(self, rhs: DeltaDyn<U>) -> Self::Output {
        let n = self.n;
        let k = self.k;
        QDyn { q: self.u.iter().enumerate().fold(self.v.clone().iter().map(|a| a.into()).collect(), |a, (i, b)| {
    a.iter().zip(b.iter()).map(|(a, b)| {
    let mut x = rhs.delta[i].clone();
    for _ in 0..i {
    x = (x * rhs.delta[i].clone());
};
    let _ = (b.clone() * x);
    (m + a)
}).collect()
}) }
    }
}

impl<T> VopeDyn<T>// UNCONSTRAINED GENERICS at generated.rs line 528: T
 {
    pub fn expand where T: Clone + Default(&self, l: usize) -> VopeDyn<T> {
        let n = self.n;
        let k = self.k;
        let Self { u: u, v: v } = self;
        VopeDyn { u: (0..l).map(|l| {
    (0..n).map(|i| u.get(l).map_or(T::new(), |a| a[i].clone())).collect()
}).collect(), v: v.clone(), n: n, k: k }
    }
    pub fn rotate_left where T: Clone(&self, n: usize) -> Self {
        let n = self.n;
        let k = self.k;
        self.remap(|a| a.wrapping_sub(n))
    }
    pub fn rotate_right where T: Clone(&self, n: usize) -> Self {
        let n = self.n;
        let k = self.k;
        self.remap(|a| a.wrapping_add(n))
    }
    pub fn remap where T: Clone(&self, m: usize, f: compile_error!("Unsupported type Existential { bounds: [IrTraitBound { trait_kind: Expand(Primitive(Usize)), type_args: [], assoc_bindings: [] }] }")) -> VopeDyn<T> {
        let n = self.n;
        let k = self.k;
        let Self { u: u, v: v } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..m).map(|i| u[l][(f(i) % n)].clone()).collect()
}).collect(), v: (0..m).map(|i| v[(f(i) % n)].clone()).collect(), n: n, k: k }
    }
}

impl VopeDyn<Bit> where N: VoleArray<Bit>, K: ArrayLength<Vec<Bit>> {
    pub fn scale<T> where N: VoleArray<T>, K: ArrayLength<Vec<T>>// UNCONSTRAINED GENERICS at generated.rs line 559: T
(self, f: compile_error!("Unsupported type Existential { bounds: [IrTraitBound { trait_kind: Custom("Fn"), type_args: [], assoc_bindings: [] }] }")) -> VopeDyn<N, T, K> {
        let n = self.n;
        let k = self.k;
        let VopeDyn { u: u, v: v, .. } = self;
        VopeDyn { u: (0..k).map(|l| {
    (0..n).map(|i| {
    let Bit(b) = u[l][i].clone();
    f(b)
}).collect()
}).collect(), v: (0..n).map(|i| {
    let Bit(b) = v[i].clone();
    f(b)
}).collect(), n: n, k: k }
    }
}

