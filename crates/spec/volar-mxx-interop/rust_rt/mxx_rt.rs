// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: generated
//! Exact ring arithmetic helpers for mxx-lowered spec elements.
//!
//! This file is the single source of truth for the runtime support of
//! programs lowered by `volar-mxx-interop`. It is plain spec-subset Rust:
//! `volar-mxx-interop` parses it into spec IR and inlines the functions a
//! lowered program needs, so a generated module is self-contained.
//!
//! # Representation
//!
//! A matrix with `R` rows and `C` columns over the negacyclic ring
//! `Z_q[X]/(X^N + 1)` with CRT modulus `q = prod_t moduli[t]` (`D` towers) is
//! `[[[[u64; N]; D]; C]; R]`: a row-major matrix of per-tower coefficient
//! arrays. Every operation is coefficientwise per tower, reduced mod
//! `moduli[t]`. This is the reference ("exact") semantics — schoolbook
//! convolution, no NTT — matching mxx's Lean `Mxx.Primitives.ExactMatrix`
//! model per tower.

/// `c[i] = (a[i] + b[i]) mod q`, for canonical `a[i], b[i] < q`.
pub fn poly_add<const N: usize>(a: [u64; N], b: [u64; N], q: u64) -> [u64; N] {
    let mut out = [0u64; N];
    for i in 0..N {
        out[i] = ((a[i] as u128 + b[i] as u128) % q as u128) as u64;
    }
    out
}

/// `c[i] = (a[i] - b[i]) mod q`, for canonical `a[i], b[i] < q`.
pub fn poly_sub<const N: usize>(a: [u64; N], b: [u64; N], q: u64) -> [u64; N] {
    let mut out = [0u64; N];
    for i in 0..N {
        out[i] = ((a[i] as u128 + q as u128 - b[i] as u128) % q as u128) as u64;
    }
    out
}

/// `c[i] = (-a[i]) mod q`.
pub fn poly_neg<const N: usize>(a: [u64; N], q: u64) -> [u64; N] {
    let mut out = [0u64; N];
    for i in 0..N {
        out[i] = ((q as u128 - a[i] as u128) % q as u128) as u64;
    }
    out
}

/// `c[i] = (a[i] * s) mod q`.
pub fn poly_scale<const N: usize>(a: [u64; N], s: u64, q: u64) -> [u64; N] {
    let mut out = [0u64; N];
    for i in 0..N {
        out[i] = ((a[i] as u128 * s as u128) % q as u128) as u64;
    }
    out
}

/// Negacyclic convolution `c = a * b` in `Z_q[X]/(X^N + 1)`, schoolbook.
///
/// Every product term is reduced mod `q` before accumulation, so no
/// intermediate exceeds `2q`; tower moduli up to 63 bits are exact in
/// `u128` term arithmetic.
pub fn poly_mul<const N: usize>(a: [u64; N], b: [u64; N], q: u64) -> [u64; N] {
    let q128 = q as u128;
    let mut acc = [0u128; N];
    for i in 0..N {
        for j in 0..N {
            let t = (a[i] as u128 * b[j] as u128) % q128;
            let k = i + j;
            if k < N {
                acc[k] = (acc[k] + t) % q128;
            } else {
                // X^N = -1: wrapped terms subtract.
                acc[k - N] = (acc[k - N] + q128 - t) % q128;
            }
        }
    }
    let mut out = [0u64; N];
    for i in 0..N {
        out[i] = acc[i] as u64;
    }
    out
}

/// Coefficientwise matrix add: `out[i][j] = a[i][j] + b[i][j]` per tower.
pub fn mat_add<const R: usize, const C: usize, const N: usize, const D: usize>(
    a: [[[[u64; N]; D]; C]; R],
    b: [[[[u64; N]; D]; C]; R],
    moduli: [u64; D],
) -> [[[[u64; N]; D]; C]; R] {
    let mut out = [[[[0u64; N]; D]; C]; R];
    for i in 0..R {
        for j in 0..C {
            for t in 0..D {
                out[i][j][t] = poly_add(a[i][j][t], b[i][j][t], moduli[t]);
            }
        }
    }
    out
}

/// Coefficientwise matrix subtract.
pub fn mat_sub<const R: usize, const C: usize, const N: usize, const D: usize>(
    a: [[[[u64; N]; D]; C]; R],
    b: [[[[u64; N]; D]; C]; R],
    moduli: [u64; D],
) -> [[[[u64; N]; D]; C]; R] {
    let mut out = [[[[0u64; N]; D]; C]; R];
    for i in 0..R {
        for j in 0..C {
            for t in 0..D {
                out[i][j][t] = poly_sub(a[i][j][t], b[i][j][t], moduli[t]);
            }
        }
    }
    out
}

/// Coefficientwise matrix negation.
pub fn mat_neg<const R: usize, const C: usize, const N: usize, const D: usize>(
    a: [[[[u64; N]; D]; C]; R],
    moduli: [u64; D],
) -> [[[[u64; N]; D]; C]; R] {
    let mut out = [[[[0u64; N]; D]; C]; R];
    for i in 0..R {
        for j in 0..C {
            for t in 0..D {
                out[i][j][t] = poly_neg(a[i][j][t], moduli[t]);
            }
        }
    }
    out
}

/// Coefficientwise matrix scale by a per-tower scalar table.
pub fn mat_scale<const R: usize, const C: usize, const N: usize, const D: usize>(
    a: [[[[u64; N]; D]; C]; R],
    s: [u64; D],
    moduli: [u64; D],
) -> [[[[u64; N]; D]; C]; R] {
    let mut out = [[[[0u64; N]; D]; C]; R];
    for i in 0..R {
        for j in 0..C {
            for t in 0..D {
                out[i][j][t] = poly_scale(a[i][j][t], s[t], moduli[t]);
            }
        }
    }
    out
}

/// Ring matrix product: `out[i][j] = sum_k a[i][k] * b[k][j]` (negacyclic
/// per entry, per tower).
pub fn mat_mul<const R: usize, const K: usize, const C: usize, const N: usize, const D: usize>(
    a: [[[[u64; N]; D]; K]; R],
    b: [[[[u64; N]; D]; C]; K],
    moduli: [u64; D],
) -> [[[[u64; N]; D]; C]; R] {
    let mut out = [[[[0u64; N]; D]; C]; R];
    for i in 0..R {
        for j in 0..C {
            for k in 0..K {
                for t in 0..D {
                    let term = poly_mul(a[i][k][t], b[k][j][t], moduli[t]);
                    out[i][j][t] = poly_add(out[i][j][t], term, moduli[t]);
                }
            }
        }
    }
    out
}

/// Matrix transpose.
pub fn mat_transpose<const R: usize, const C: usize, const N: usize, const D: usize>(
    a: [[[[u64; N]; D]; C]; R],
) -> [[[[u64; N]; D]; R]; C] {
    let mut out = [[[[0u64; N]; D]; R]; C];
    for i in 0..R {
        for j in 0..C {
            out[j][i] = a[i][j];
        }
    }
    out
}

/// The `R x R` identity matrix: constant polynomial 1 on the diagonal.
pub fn mat_identity<const R: usize, const N: usize, const D: usize>() -> [[[[u64; N]; D]; R]; R] {
    let mut out = [[[[0u64; N]; D]; R]; R];
    for i in 0..R {
        for t in 0..D {
            out[i][i][t][0] = 1u64;
        }
    }
    out
}
