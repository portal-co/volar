//! Dynamic (heap-allocated) versions of key pieces from `crates/volar-spec`.
//!
//! This file re-implements selected types and helpers from the original
//! `volar-spec` crate using dynamic allocation (`Vec`) instead of
//! `GenericArray`/const generics. The intent is to keep the same
//! semantics while removing the compile-time size requirements.
//!
//! Original sources cited (kept here for clarity):
//! - crates/volar-spec/src/vole.rs
//! - crates/volar-spec/src/byte_gen.rs
//!
use alloc::vec;
use alloc::vec::Vec;
use core::ops::Mul;

/// Dynamic Delta equivalent of `Delta<N, T>` (from `volar-spec/src/vole.rs`).
pub struct DeltaDyn<T> {
    pub delta: Vec<T>,
}

/// Dynamic Q equivalent of `Q<N, T>` (from `volar-spec/src/vole.rs`).
pub struct QDyn<T> {
    pub q: Vec<T>,
}

impl<T: Clone> DeltaDyn<T> {
    pub fn remap(&self, mut f: impl FnMut(usize) -> usize) -> DeltaDyn<T> {
        let n = self.delta.len();
        let mut out = Vec::with_capacity(n);
        for i in 0..n {
            let idx = f(i) % n;
            out.push(self.delta[idx].clone());
        }
        DeltaDyn { delta: out }
    }
    pub fn rotate_left(&self, n: usize) -> Self {
        self.remap(|a| a.wrapping_sub(n))
    }
    pub fn rotate_right(&self, n: usize) -> Self {
        self.remap(|a| a.wrapping_add(n))
    }
    /// Elementwise multiply `val` and `self.delta`, returning a `QDyn`.
    /// This mirrors `Delta::static` from the spec but works with `Vec`.
    pub fn r#static<U, Out>(&self, val: &[U]) -> QDyn<Out>
    where
        T: Clone,
        U: Clone + Mul<T, Output = Out>,
        Out: Clone,
    {
        let n = self.delta.len();
        assert_eq!(val.len(), n);
        let mut q = Vec::with_capacity(n);
        for i in 0..n {
            q.push(val[i].clone() * self.delta[i].clone());
        }
        QDyn { q }
    }
}

impl<T: Clone> QDyn<T> {
    pub fn remap(&self, mut f: impl FnMut(usize) -> usize) -> QDyn<T> {
        let n = self.q.len();
        let mut out = Vec::with_capacity(n);
        for i in 0..n {
            let idx = f(i) % n;
            out.push(self.q[idx].clone());
        }
        QDyn { q: out }
    }
    pub fn rotate_left(&self, n: usize) -> Self {
        self.remap(|a| a.wrapping_sub(n))
    }
    pub fn rotate_right(&self, n: usize) -> Self {
        self.remap(|a| a.wrapping_add(n))
    }
}

/// Dynamic Vope equivalent with dynamic block size.
/// The original `Vope` holds `u: GenericArray<GenericArray<u8, B::BlockSize>, B::BlockSize>`
/// and `v: GenericArray<u8, B::BlockSize>`. Here we use `Vec<Vec<u8>>` and `Vec<u8>`.
pub struct VopeDyn {
    /// `u` is length `block_size`, each entry is a `Vec<u8>` of length `block_size`.
    pub u: Vec<Vec<u8>>,
    /// `v` is a single vector of length `block_size`.
    pub v: Vec<u8>,
}

use core::ops::Deref;

/// Create a dynamic `VopeDyn` from a slice of byte-like inputs, mirroring
/// `create_vole_from_material` in `byte_gen.rs` but using `Vec`.
pub fn create_vole_from_material_dyn(s: &[impl Deref<Target = [u8]>], block_size: usize) -> VopeDyn {
    // compute `u_core` as xor of the first `block_size` bytes of each entry
    let mut u_core = vec![0u8; block_size];
    for b in s {
        let slice = &b[..core::cmp::min(block_size, b.len())];
        for i in 0..block_size {
            let vb = if i < slice.len() { slice[i] } else { 0 };
            u_core[i] ^= vb;
        }
    }

    // compute `v` similar to original code: xor plus index
    let mut v = vec![0u8; block_size];
    for (idx, b) in s.iter().enumerate() {
        let slice = &b[..core::cmp::min(block_size, b.len())];
        for i in 0..block_size {
            let vb = if i < slice.len() { slice[i] } else { 0 };
            v[i] = v[i].bitxor(vb).bitxor(idx as u8);
        }
    }

    // u: replicate u_core `block_size` times (mirrors GenericArray::generate(|_| u.clone()))
    let mut u = Vec::with_capacity(block_size);
    for _ in 0..block_size {
        u.push(u_core.clone());
    }

    VopeDyn { u, v }
}

/// Dynamic ABO equivalent (commit is a byte vector; per_byte is a vector of blocks).
pub struct ABODyn {
    pub commit: Vec<u8>,
    pub per_byte: Vec<Vec<u8>>,
}

/// Dynamic version of `double` from `byte_gen.rs`.
///
/// `a` must be a block-sized slice. `mut encrypt` is a closure that given a key
/// (32-byte array) and a mutable output buffer of length `block_size` will write
/// the encrypted block. The function returns two blocks (for i=0 and i=1) like
/// the original `double` which called the block cipher with two different
/// `From<[u8;32]>`-constructed keys.
pub fn double_dyn(
    a: &[u8],
    mut encrypt: impl FnMut([u8; 32], &mut [u8]),
) -> [Vec<u8>; 2] {
    let block_size = a.len();
    let mut out0 = vec![0u8; block_size];
    let mut out1 = vec![0u8; block_size];
    let mut tmp = a.to_vec();

    let mut key0 = [0u8; 32];
    key0.copy_from_slice(&[0u8; 32]);
    encrypt(key0, &mut out0);

    let mut key1 = [0u8; 32];
    key1.copy_from_slice(&[1u8; 32]);
    encrypt(key1, &mut out1);

    [out0, out1]
}

// small helper: xor for u8 (kept so code reads similarly to original)
trait BitXorU8 {
    fn bitxor(self, rhs: u8) -> u8;
}
impl BitXorU8 for u8 {
    fn bitxor(self, rhs: u8) -> u8 {
        self ^ rhs
    }
}
