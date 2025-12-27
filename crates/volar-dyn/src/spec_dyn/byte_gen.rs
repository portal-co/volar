//! Dynamic equivalents for `volar-spec/src/byte_gen.rs` (create_vole, ABO, double, ...)
use super::*;

use alloc::vec::Vec;
use core::ops::Deref;

use super::vole::VopeDyn;

/// Create a dynamic `VopeDyn` from a slice of byte-like inputs, mirroring
/// `create_vole_from_material` in `byte_gen.rs` but using `Vec`.
pub fn create_vole_from_material_dyn(s: &[impl Deref<Target = [u8]>], block_size: usize) -> VopeDyn<u8> {
    let mut u_core = vec![0u8; block_size];
    for b in s {
        let slice = &b[..core::cmp::min(block_size, b.len())];
        for i in 0..block_size {
            let vb = if i < slice.len() { slice[i] } else { 0 };
            u_core[i] ^= vb;
        }
    }

    let mut v = vec![0u8; block_size];
    for (idx, b) in s.iter().enumerate() {
        let slice = &b[..core::cmp::min(block_size, b.len())];
        for i in 0..block_size {
            let vb = if i < slice.len() { slice[i] } else { 0 };
            v[i] = v[i] ^ vb ^ (idx as u8);
        }
    }

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
pub fn double_dyn(
    a: &[u8],
    mut encrypt: impl FnMut([u8; 32], &mut [u8]),
) -> [Vec<u8>; 2] {
    let block_size = a.len();
    let mut out0 = vec![0u8; block_size];
    let mut out1 = vec![0u8; block_size];

    let key0 = [0u8; 32];
    encrypt(key0, &mut out0);

    let key1 = [1u8; 32];
    encrypt(key1, &mut out1);

    [out0, out1]
}

/// Expanded variant that applies a function `f` to each input before folding.
pub fn create_vole_from_material_expanded_dyn< X: AsRef<[u8]> , F: FnMut(&[u8]) -> X>(
    s: &[impl Deref<Target = [u8]>],
    block_size: usize,
    mut f: F,
) -> VopeDyn<u8> {
    let mut u_core = vec![0u8; block_size];
    for b in s {
        let out = f(&b[..core::cmp::min(block_size, b.len())]);
        let slice = out.as_ref();
        for i in 0..block_size {
            let vb = if i < slice.len() { slice[i] } else { 0 };
            u_core[i] ^= vb;
        }
    }

    let mut v = vec![0u8; block_size];
    for (idx, b) in s.iter().enumerate() {
        let out = f(&b[..core::cmp::min(block_size, b.len())]);
        let slice = out.as_ref();
        for i in 0..block_size {
            let vb = if i < slice.len() { slice[i] } else { 0 };
            v[i] = v[i] ^ vb ^ (idx as u8);
        }
    }

    let mut u = Vec::with_capacity(block_size);
    for _ in 0..block_size {
        u.push(u_core.clone());
    }

    VopeDyn { u, v }
}
