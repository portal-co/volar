// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//! @volar-allow-vec: runtime-boundary: LPN encode output is a
//! runtime-sized host protocol buffer, not a weaver-known compiled-program
//! shape; this module-level exemption applies to the whole file.
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
    assert!(k >= LOCALITY, "10-local LPN requires k >= 10");
    column_rows_for_valid_k(seed, k, j)
}

fn column_rows_for_valid_k(seed: &[u8; 16], k: usize, j: usize) -> [usize; LOCALITY] {
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
    assert!(k >= LOCALITY, "10-local LPN requires k >= 10");
    debug_assert_eq!(v.len(), k);
    let mut y = alloc::vec![[0u8; 16]; n];
    for j in 0..n {
        let rows = column_rows_for_valid_k(seed, k, j);
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
    assert!(k >= LOCALITY, "10-local LPN requires k >= 10");
    debug_assert_eq!(u.len(), k);
    let mut x = alloc::vec![false; n];
    for j in 0..n {
        let rows = column_rows_for_valid_k(seed, k, j);
        let mut acc = false;
        for row in rows {
            acc ^= u[row];
        }
        x[j] = acc;
    }
    x
}

/// Compute Ferret's three LPN-encoded COT outputs in one column pass, folding
/// the MPCOT outputs and regular noise directly into the results. Reusing the
/// same generated columns avoids repeating the SHA3 expansion three times and
/// avoids retaining the three intermediate encoded vectors.
pub fn encode_ferret_outputs(
    seed: &[u8; 16],
    k: usize,
    n: usize,
    v: &[Block],
    u: &[bool],
    w: &[Block],
    s: &[Block],
    e: &[bool],
    r: &[Block],
) -> (Vec<Block>, Vec<bool>, Vec<Block>) {
    assert!(k >= LOCALITY, "10-local LPN requires k >= 10");
    debug_assert_eq!(v.len(), k);
    debug_assert_eq!(u.len(), k);
    debug_assert_eq!(w.len(), k);
    debug_assert_eq!(s.len(), n);
    debug_assert_eq!(e.len(), n);
    debug_assert_eq!(r.len(), n);

    let mut y = Vec::with_capacity(n);
    let mut x = Vec::with_capacity(n);
    let mut z = Vec::with_capacity(n);
    for j in 0..n {
        let rows = column_rows_for_valid_k(seed, k, j);
        let mut y_col = [0u8; 16];
        let mut x_bit = false;
        let mut z_col = [0u8; 16];
        for row in rows {
            x_bit ^= u[row];
            for b in 0..16 {
                y_col[b] ^= v[row][b];
                z_col[b] ^= w[row][b];
            }
        }
        for b in 0..16 {
            y_col[b] ^= s[j][b];
            z_col[b] ^= r[j][b];
        }
        y.push(y_col);
        x.push(x_bit ^ e[j]);
        z.push(z_col);
    }
    (y, x, z)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fused_ferret_encoding_matches_independent_encoders() {
        let seed = [0xA5; 16];
        let k = 16;
        let n = 24;
        let v: Vec<Block> = (0..k).map(|i| [i as u8; 16]).collect();
        let u: Vec<bool> = (0..k).map(|i| i % 3 == 0).collect();
        let w: Vec<Block> = (0..k).map(|i| [(i * 7) as u8; 16]).collect();
        let s: Vec<Block> = (0..n).map(|i| [(i * 11) as u8; 16]).collect();
        let e: Vec<bool> = (0..n).map(|i| i % 2 == 0).collect();
        let r: Vec<Block> = (0..n).map(|i| [(i * 13) as u8; 16]).collect();

        let (y, x, z) = encode_ferret_outputs(&seed, k, n, &v, &u, &w, &s, &e, &r);
        let y_lpn = encode_blocks(&seed, k, n, &v);
        let x_lpn = encode_bits(&seed, k, n, &u);
        let z_lpn = encode_blocks(&seed, k, n, &w);
        for j in 0..n {
            let mut expected_y = y_lpn[j];
            let mut expected_z = z_lpn[j];
            for b in 0..16 {
                expected_y[b] ^= s[j][b];
                expected_z[b] ^= r[j][b];
            }
            assert_eq!(y[j], expected_y);
            assert_eq!(x[j], x_lpn[j] ^ e[j]);
            assert_eq!(z[j], expected_z);
        }
    }

    #[test]
    #[should_panic(expected = "10-local LPN requires k >= 10")]
    fn column_generation_rejects_too_small_lpn_dimension() {
        let _ = column_rows(&[0; 16], LOCALITY - 1, 0);
    }

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
