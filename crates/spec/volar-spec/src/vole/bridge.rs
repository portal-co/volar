// @reliability: experimental
//! @ai: assisted
//! VOLE Continuation Bridge primitives.
//!
//! These functions bind committed state across a *gap* (network outage) or a
//! *skip* (fast-forwarded loop iterations), so a resumed proof segment stays
//! linked to the pre-gap / pre-skip commitments without leaving the VOLE
//! framework.  See `docs/vole-continuation-bridge.md`.
//!
//! Two mechanisms, both **free** in VOLE (no AND gates):
//!
//! - **XOR-key re-commitment** ([`vole_rekey_prover`] /
//!   [`vole_rekey_verifier_check`]): re-randomise a committed wire by adding a
//!   fresh one-time-pad `key` wire.  Because VOLE `Add` is bitwise XOR in
//!   `GF(2^k)`, the re-keyed wire is linearly bound to the original; the
//!   verifier check is the same additive consistency used by XOR/NOT gates
//!   (no `Δ` needed).  Used by the non-bridge dynamic-skip path.
//!
//! - **Additive multiset-hash accumulation** ([`mem_acc_absorb`]): fold one
//!   `(addr, value, ts)` memory tuple into a running accumulator
//!   `acc + addr·r + value·r² + ts·r³` (mirrors `vole::memory`'s `encode`).
//!   The accumulator is a single field element, so it can be carried across a
//!   gap as one VOLE-authenticated wire — memory stays succinct in cell count.

use super::*;
use cipher::consts::U1;

/// Prover side of an XOR-key re-commitment: `rekeyed = wire + key`.
///
/// `key` is a fresh one-time-pad VOLE wire.  In `GF(2^k)` the VOLE `Add` is
/// bitwise XOR, so this re-randomises the commitment (hiding the original mask)
/// while remaining linearly bound to `wire`.  The verifier confirms the binding
/// with [`vole_rekey_verifier_check`].
pub fn vole_rekey_prover<N, T>(wire: Vope<N, T, U1>, key: Vope<N, T, U1>) -> Vope<N, T, U1>
where
    N: VoleArray<T>,
    T: Clone + Add<Output = T> + Default,
    Vope<N, T, U1>: Add<Output = Vope<N, T, U1>>,
{
    wire + key
}

/// Verifier side of an XOR-key re-commitment.
///
/// Checks the re-keyed share equals the sum of the original and the key shares
/// lane-wise: `q_rekeyed[i] == q_wire[i] + q_key[i]`.  This is exactly the
/// linear (XOR) gate check — `Δ` is not involved, so it is free.
pub fn vole_rekey_verifier_check<N, T>(
    q_wire: &Q<N, T>,
    q_key: &Q<N, T>,
    q_rekeyed: &Q<N, T>,
) -> bool
where
    N: ArraySize,
    T: Clone + Add<Output = T> + PartialEq,
{
    let mut ok = true;
    for i in 0..N::USIZE {
        let expect = q_wire.q[i].clone() + q_key.q[i].clone();
        ok = ok && (q_rekeyed.q[i].clone() == expect);
    }
    ok
}

/// Absorb one `(addr, value, ts)` memory tuple into a multiset-hash accumulator.
///
/// Returns `acc + addr·r1 + value·r2 + ts·r3` where `(r1, r2, r3) = (r, r², r³)`
/// are the public challenge powers.  All multiplications are by public
/// constants, so this is free in VOLE.  Matches the `encode` formula in
/// `vole::memory` and is commutative/associative, so accumulation order does
/// not matter (multiset semantics).
///
/// `T` may be a plain field element (prover/verifier offline) or a
/// VOLE-authenticated value (`Vope`/`Q`), since the operation is purely linear.
pub fn mem_acc_absorb<T>(acc: T, r1: T, r2: T, r3: T, addr: T, value: T, ts: T) -> T
where
    T: Clone + Add<Output = T> + Mul<Output = T>,
{
    acc + addr * r1 + value * r2 + ts * r3
}

/// Scale a degree-1 VOPE wire by a **public** field constant `c`.
///
/// Multiplies both the `u[0]` and `v` lanes by `c`; degree-preserving and free
/// (public scalar → no secret-dependent multiplication, unlike the hazmat
/// `Vope * Vope`).  This is the in-circuit analogue of multiplying a committed
/// value by a public challenge power.
pub fn vope_scale_const<N, T>(w: &Vope<N, T, U1>, c: &T) -> Vope<N, T, U1>
where
    N: VoleArray<T>,
    T: Clone + Mul<Output = T> + Default,
{
    Vope {
        u: Array::<Array<T, N>, U1>::from_fn(|_| {
            Array::<T, N>::from_fn(|i| w.u[0][i].clone() * c.clone())
        }),
        v: Array::<T, N>::from_fn(|i| w.v[i].clone() * c.clone()),
    }
}

/// In-circuit multiset-hash absorb on VOPE wires.
///
/// Returns `acc + addr·r1 + value·r2 + ts·r3` where `(r1, r2, r3)` are the
/// public challenge powers (cf. [`mem_acc_absorb`], which operates on plain
/// field elements).  The committed `addr`/`value`/`ts` wires are scaled by the
/// public powers via [`vope_scale_const`] and summed — all free in VOLE.  Carry
/// the resulting accumulator as loop-state so memory stays O(1) across a gap.
pub fn mem_acc_absorb_vope<N, T>(
    acc: Vope<N, T, U1>,
    addr: &Vope<N, T, U1>,
    value: &Vope<N, T, U1>,
    ts: &Vope<N, T, U1>,
    r1: &T,
    r2: &T,
    r3: &T,
) -> Vope<N, T, U1>
where
    N: VoleArray<T>,
    T: Clone + Add<Output = T> + Mul<Output = T> + Default,
    Vope<N, T, U1>: Add<Output = Vope<N, T, U1>>,
{
    acc + vope_scale_const(addr, r1) + vope_scale_const(value, r2) + vope_scale_const(ts, r3)
}

#[cfg(test)]
mod tests {
    use super::*;
    use cipher::consts::U2;
    use hybrid_array::Array;

    fn q<const A: u64, const B: u64>() -> Q<U2, u64> {
        Q { q: Array::<u64, U2>::from_fn(|i| if i == 0 { A } else { B }) }
    }

    #[test]
    fn rekey_verifier_accepts_consistent() {
        let q_wire = q::<5, 9>();
        let q_key = q::<3, 4>();
        // honest rekeyed share = wire + key lane-wise
        let q_rekeyed = Q {
            q: Array::<u64, U2>::from_fn(|i| q_wire.q[i] + q_key.q[i]),
        };
        assert!(vole_rekey_verifier_check(&q_wire, &q_key, &q_rekeyed));
    }

    #[test]
    fn rekey_verifier_rejects_tampered() {
        let q_wire = q::<5, 9>();
        let q_key = q::<3, 4>();
        let mut q_rekeyed = Q {
            q: Array::<u64, U2>::from_fn(|i| q_wire.q[i] + q_key.q[i]),
        };
        q_rekeyed.q[0] = q_rekeyed.q[0].wrapping_add(1); // tamper
        assert!(!vole_rekey_verifier_check(&q_wire, &q_key, &q_rekeyed));
    }

    #[test]
    fn mem_acc_is_order_independent() {
        // Multiset property: absorbing two tuples in either order matches.
        let (r1, r2, r3) = (7u64, 49, 343);
        let a = mem_acc_absorb(0, r1, r2, r3, 2, 11, 1);
        let ab = mem_acc_absorb(a, r1, r2, r3, 5, 13, 2);
        let b = mem_acc_absorb(0, r1, r2, r3, 5, 13, 2);
        let ba = mem_acc_absorb(b, r1, r2, r3, 2, 11, 1);
        assert_eq!(ab, ba);
    }

    /// Build a degree-1 VOPE wire with `u[0] = [u0, u0]`, `v = [v0, v0]`.
    fn vope(u0: u64, v0: u64) -> Vope<U2, u64, U1> {
        Vope {
            u: Array::<Array<u64, U2>, U1>::from_fn(|_| Array::<u64, U2>::from_fn(|_| u0)),
            v: Array::<u64, U2>::from_fn(|_| v0),
        }
    }

    #[test]
    fn scale_const_scales_both_lanes() {
        let w = vope(3, 5);
        let s = vope_scale_const(&w, &4u64);
        assert_eq!(s.u[0][0], 12);
        assert_eq!(s.u[0][1], 12);
        assert_eq!(s.v[0], 20);
        assert_eq!(s.v[1], 20);
    }

    #[test]
    fn mem_acc_vope_matches_scalar_formula() {
        // The VOPE accumulator's u[0] lane should track the plain-field formula
        // acc + addr·r1 + value·r2 + ts·r3 on the encoded bit values.
        let (r1, r2, r3) = (7u64, 49u64, 343u64);
        let acc = vope(0, 0);
        let addr = vope(1, 0); // addr bit = 1
        let value = vope(11, 0);
        let ts = vope(2, 0);
        let out = mem_acc_absorb_vope(acc, &addr, &value, &ts, &r1, &r2, &r3);
        let expect = 1 * r1 + 11 * r2 + 2 * r3;
        assert_eq!(out.u[0][0], expect);
    }
}
