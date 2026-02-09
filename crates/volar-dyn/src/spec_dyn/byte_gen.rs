//! Dynamic equivalents for `volar-spec/src/byte_gen.rs` (create_vole, ABO, double, ...)
use super::*;

use alloc::vec::Vec;
use core::ops::Deref;

use cipher::BlockEncrypt;
use digest::Digest;

use volar_common::hash_commitment::{CommitmentCore, commit};

use super::vole::VopeDyn;

/// Create a dynamic `VopeDyn` from a slice of byte-like inputs, mirroring
/// `create_vole_from_material` in `byte_gen.rs` but using `Vec`.
pub fn create_vole_from_material_dyn(
    s: &[impl Deref<Target = [u8]>],
    block_size: usize,
) -> VopeDyn<u8> {
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

/// Lightweight trait for block-cipher-like types usable by the dynamic
/// reimplementation. This mirrors the role of `BlockEncrypt + From<[u8;32]>`
/// used in the `volar-spec` crate but is sized for dynamic buffers.
pub trait ByteBlockEncryptDyn {
    fn from_key(key: [u8; 32]) -> Self;
    fn encrypt_block(&mut self, block: &mut [u8]);
}

/// Dynamic version of `double` from `byte_gen.rs`.
pub fn double_dyn<B: ByteBlockEncryptDyn>(a: &[u8]) -> [Vec<u8>; 2] {
    let block_size = a.len();
    let mut out0 = a.to_vec();
    let mut out1 = a.to_vec();

    let mut b0 = B::from_key([0u8; 32]);
    b0.encrypt_block(&mut out0);

    let mut b1 = B::from_key([1u8; 32]);
    b1.encrypt_block(&mut out1);

    [out0, out1]
}

/// Dynamic equivalent of `ByteBlockEncrypt::gen_abo` from `volar-spec`.
pub fn gen_abo_dyn<B: ByteBlockEncryptDyn, D: Digest>(
    a: &[u8],
    rand: &impl AsRef<[u8]>,
    k: usize,
) -> ABODyn {
    let mut h = D::new();
    let mut per_byte: Vec<Vec<u8>> = Vec::with_capacity(k);

    // compute number of bits needed to index `k` entries
    let bits = (usize::BITS as usize) - (k.leading_zeros() as usize);

    for i in 0..k {
        let mut acc = a.to_vec();
        for b in 0..bits {
            let doubled = double_dyn::<B>(&acc);
            if ((i >> b) & 1) != 0 {
                acc = doubled[1].clone();
            } else {
                acc = doubled[0].clone();
            }
        }
        let core = acc;
        let c = commit::<D>(&core, rand);
        h.update(c.as_ref());
        per_byte.push(core);
    }

    let commit = h.finalize();
    ABODyn {
        commit: commit.as_slice().to_vec(),
        per_byte,
    }
}

/// Expanded variant that applies a function `f` to each input before folding.
pub fn create_vole_from_material_expanded_dyn<X: AsRef<[u8]>, F: FnMut(&[u8]) -> X>(
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
