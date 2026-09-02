// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//! 10-local linear code for primal-LPN (Ferret §6.2 / BCGI18).
//!
//! Each of the `n` columns of `A ∈ F_2^{k×n}` has Hamming weight 10. The
//! matrix is derived from a public seed via SHA3 (random-oracle model);
//! parties never send `A`.

use alloc::vec::Vec;

use digest::Digest;
use sha3::Sha3_256;

use super::spcot::Block;

/// Number of ones per column (BCGI18 / SGRR19 / Ferret).
pub const LOCALITY: usize = 10;

/// Column `j`'s 10 row indices in `[0, k)`.
pub fn column_rows(seed: &[u8; 16], k: usize, j: usize) -> [usize; LOCALITY] {
    let mut rows = [0usize; LOCALITY];
    let mut fill = 0usize;
    let mut counter = 0u32;
    while fill < LOCALITY {
        let mut h = Sha3_256::new();
        h.update(b"ferret-lpn-10local-v1");
        h.update(seed);
        h.update((j as u64).to_le_bytes());
        h.update(counter.to_le_bytes());
        let out = h.finalize();
        for chunk in out.chunks_exact(4) {
            if fill >= LOCALITY {
                break;
            }
            let raw = u32::from_le_bytes(chunk.try_into().unwrap()) as usize;
            let row = raw % k;
            if !rows[..fill].contains(&row) {
                rows[fill] = row;
                fill += 1;
            }
        }
        counter += 1;
    }
    rows
}

/// `y = v A` over `F_{2^κ}` (`A` binary, `v` a `k`-vector of blocks).
pub fn encode_blocks(seed: &[u8; 16], k: usize, n: usize, v: &[Block]) -> Vec<Block> {
    debug_assert_eq!(v.len(), k);
    let mut y = alloc::vec![[0u8; 16]; n];
    for j in 0..n {
        let rows = column_rows(seed, k, j);
        let mut acc = [0u8; 16];
        for row in rows {
            for b in 0..16 {
                acc[b] ^= v[row][b];
            }
        }
        y[j] = acc;
    }
    y
}

/// `x = u A` over `F_2` (`u` a `k`-bit vector).
pub fn encode_bits(seed: &[u8; 16], k: usize, n: usize, u: &[bool]) -> Vec<bool> {
    debug_assert_eq!(u.len(), k);
    let mut x = alloc::vec![false; n];
    for j in 0..n {
        let rows = column_rows(seed, k, j);
        let mut acc = false;
        for row in rows {
            acc ^= u[row];
        }
        x[j] = acc;
    }
    x
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn columns_have_distinct_rows() {
        let seed = [7u8; 16];
        for j in 0..32 {
            let rows = column_rows(&seed, 16, j);
            let mut sorted = rows;
            sorted.sort();
            for w in sorted.windows(2) {
                assert_ne!(w[0], w[1], "column {j} repeated row");
            }
            for r in rows {
                assert!(r < 16);
            }
        }
    }
}
