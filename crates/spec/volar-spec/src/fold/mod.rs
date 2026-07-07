// @reliability: experimental
// @ai-tier: 3
//! @ai: assisted
//! Nova **NIFS fold** primitives for the verifier-gate R1CS, in the linkable
//! spec subset.
//!
//! These mirror `crates/fold/volar-fold/src/{r1cs,nifs}.rs` (the runtime
//! **oracle**) but are written generically over the fold scalar field `S` (the
//! `F_ℓ` analog) with operator bounds, exactly as [`crate::vole::prove`] is
//! generic over the VOLE field `T`.  The concrete `F_ℓ` arithmetic is supplied
//! by the instantiating type; the weaver links these and lowers them to the
//! target like any other spec function.
//!
//! Everything here is **specialised to the `and_check` R1CS** — the verifier
//! gate `K_a·K_b + V̂ = K_c·Δ` — which is fixed-size (3 constraints, witness
//! `W = [K_a, K_b, K_c, Δ, V̂, P₁, P₂]`, `z = [W ‖ u]` of length 8). Fixed sizes
//! keep it in the total/bounded spec subset.
//!
//! The Pedersen commitment fold over curve points lives with the weaver (it uses
//! the non-generic [`crate::curve`] group ops); this module is the scalar-field
//! core that is cross-tested against the oracle.

use core::ops::{Add, Mul, Sub};

/// Witness length for the verifier-gate R1CS (`W`, excluding the `u` column).
pub const AND_VARS: usize = 7;
/// Number of constraints in the verifier-gate R1CS.
pub const AND_CONS: usize = 3;

// Column indices in `z = [W ‖ u]`.
const K_A: usize = 0;
const K_B: usize = 1;
const K_C: usize = 2;
const DELTA: usize = 3;
const V_HAT: usize = 4;
const P1: usize = 5;
const P2: usize = 6;
const U: usize = 7;

/// `z = [W ‖ u]` for the fixed verifier-gate R1CS.
fn full_z<S: Clone>(w: &[S; AND_VARS], u: &S) -> [S; 8] {
    [
        w[0].clone(), w[1].clone(), w[2].clone(), w[3].clone(),
        w[4].clone(), w[5].clone(), w[6].clone(), u.clone(),
    ]
}

/// `(A·z, B·z, C·z)` for the fixed `and_check` R1CS.
///
/// `A`: row0 = `K_a`, row1 = `K_c`, row2 = `P₁ + V̂ − P₂`.
/// `B`: row0 = `K_b`, row1 = `Δ`,  row2 = `u`.
/// `C`: row0 = `P₁`, row1 = `P₂`,  row2 = `0`.
pub fn eval_abc<S>(z: &[S; 8]) -> ([S; AND_CONS], [S; AND_CONS], [S; AND_CONS])
where
    S: Clone + Add<Output = S> + Sub<Output = S> + Default,
{
    let az = [
        z[K_A].clone(),
        z[K_C].clone(),
        z[P1].clone() + z[V_HAT].clone() - z[P2].clone(),
    ];
    let bz = [z[K_B].clone(), z[DELTA].clone(), z[U].clone()];
    let cz = [z[P1].clone(), z[P2].clone(), S::default()];
    (az, bz, cz)
}

/// Relaxed satisfaction `(A·z) ∘ (B·z) = u·(C·z) + E` for the verifier gate.
pub fn is_satisfied_relaxed<S>(w: &[S; AND_VARS], e: &[S; AND_CONS], u: &S) -> bool
where
    S: Clone + Add<Output = S> + Sub<Output = S> + Mul<Output = S> + Default + PartialEq,
{
    let z = full_z(w, u);
    let (az, bz, cz) = eval_abc(&z);
    let mut ok = true;
    for i in 0..AND_CONS {
        let lhs = az[i].clone() * bz[i].clone();
        let rhs = u.clone() * cz[i].clone() + e[i].clone();
        ok = ok && (lhs == rhs);
    }
    ok
}

/// Build the satisfying witness `W = [K_a,K_b,K_c,Δ,V̂,P₁,P₂]` for one verifier
/// gate from its observed values, with `P₁ = K_a·K_b`, `P₂ = K_c·Δ`.
pub fn gate_witness<S>(k_a: S, k_b: S, k_c: S, delta: S, v_hat: S) -> [S; AND_VARS]
where
    S: Clone + Mul<Output = S>,
{
    let p1 = k_a.clone() * k_b.clone();
    let p2 = k_c.clone() * delta.clone();
    [k_a, k_b, k_c, delta, v_hat, p1, p2]
}

/// The cross term `T = A z1 ∘ B z2 + A z2 ∘ B z1 − u1·(C z2) − u2·(C z1)`.
pub fn cross_term<S>(w1: &[S; AND_VARS], u1: &S, w2: &[S; AND_VARS], u2: &S) -> [S; AND_CONS]
where
    S: Clone + Add<Output = S> + Sub<Output = S> + Mul<Output = S> + Default,
{
    let z1 = full_z(w1, u1);
    let z2 = full_z(w2, u2);
    let (az1, bz1, cz1) = eval_abc(&z1);
    let (az2, bz2, cz2) = eval_abc(&z2);
    let mut t = [S::default(), S::default(), S::default()];
    for i in 0..AND_CONS {
        let cross = az1[i].clone() * bz2[i].clone() + az2[i].clone() * bz1[i].clone();
        let sub = u1.clone() * cz2[i].clone() + u2.clone() * cz1[i].clone();
        t[i] = cross - sub;
    }
    t
}

/// Fold two relaxed witnesses with challenge `r`: `W = W1 + r·W2`,
/// `E = E1 + r·T + r²·E2`.  Matches the oracle's witness fold in `nifs::prove_fold`.
pub fn fold_witness<S>(
    w1: &[S; AND_VARS],
    e1: &[S; AND_CONS],
    w2: &[S; AND_VARS],
    e2: &[S; AND_CONS],
    t: &[S; AND_CONS],
    r: &S,
) -> ([S; AND_VARS], [S; AND_CONS])
where
    S: Clone + Add<Output = S> + Mul<Output = S> + Default,
{
    let r2 = r.clone() * r.clone();
    let mut w = [
        S::default(), S::default(), S::default(), S::default(),
        S::default(), S::default(), S::default(),
    ];
    for i in 0..AND_VARS {
        w[i] = w1[i].clone() + r.clone() * w2[i].clone();
    }
    let mut e = [S::default(), S::default(), S::default()];
    for i in 0..AND_CONS {
        e[i] = e1[i].clone() + r.clone() * t[i].clone() + r2.clone() * e2[i].clone();
    }
    (w, e)
}

/// Fold the relaxation scalar: `u = u1 + r·u2`.
pub fn fold_u<S>(u1: &S, u2: &S, r: &S) -> S
where
    S: Clone + Add<Output = S> + Mul<Output = S>,
{
    u1.clone() + r.clone() * u2.clone()
}

/// Fold a commitment blinder: `ρ = ρ1 + r·ρ2`.
pub fn fold_blinder<S>(rho1: &S, rho2: &S, r: &S) -> S
where
    S: Clone + Add<Output = S> + Mul<Output = S>,
{
    rho1.clone() + r.clone() * rho2.clone()
}

/// Fold the error blinder: `ρ_E = ρ_E1 + r·ρ_T + r²·ρ_E2`.
pub fn fold_error_blinder<S>(re1: &S, rt: &S, re2: &S, r: &S) -> S
where
    S: Clone + Add<Output = S> + Mul<Output = S>,
{
    let r2 = r.clone() * r.clone();
    re1.clone() + r.clone() * rt.clone() + r2 * re2.clone()
}

// ── Commitment (instance) fold over the curve ────────────────────────────────
//
// The NIFS *instance* fold combines the Pedersen commitments. It uses the group
// ops from [`crate::curve`] with the challenge supplied as a little-endian byte
// scalar (`r`, `r²`) — the same representation `ed_scalar_mul` consumes — so this
// fold reproduces the oracle's `nifs::verify_fold` without needing the `F_ℓ`
// scalar type here.

use crate::curve::{ed_add, ed_scalar_mul, EdPoint};

/// Fold the witness commitment: `comm_W = comm_W1 + r·comm_W2`.
pub fn fold_commit_w(comm_w1: &EdPoint, comm_w2: &EdPoint, r: &[u8; 32]) -> EdPoint {
    ed_add(comm_w1, &ed_scalar_mul(comm_w2, r))
}

/// Fold the error commitment:
/// `comm_E = comm_E1 + r·comm_T + r²·comm_E2`.
pub fn fold_commit_e(
    comm_e1: &EdPoint,
    comm_t: &EdPoint,
    comm_e2: &EdPoint,
    r: &[u8; 32],
    r2: &[u8; 32],
) -> EdPoint {
    ed_add(
        &ed_add(comm_e1, &ed_scalar_mul(comm_t, r)),
        &ed_scalar_mul(comm_e2, r2),
    )
}

/// Pedersen vector commitment `Σ_i x_i·g_i + ρ·H`, with the message scalars
/// `x` and blinder `ρ` as little-endian byte scalars.  Reproduces the oracle's
/// `PedersenParams::commit` (its Pippenger MSM computes the same sum).  Used by
/// the `fresh` step to commit a gate's witness/error before folding.
///
/// `x.len()` must be `≤ gens.len()`; only the first `x.len()` generators are used.
pub fn pedersen_commit(gens: &[EdPoint], h: &EdPoint, x: &[[u8; 32]], blind: &[u8; 32]) -> EdPoint {
    let mut acc = ed_scalar_mul(h, blind);
    let mut i = 0usize;
    while i < x.len() {
        acc = ed_add(&acc, &ed_scalar_mul(&gens[i], &x[i]));
        i += 1;
    }
    acc
}
