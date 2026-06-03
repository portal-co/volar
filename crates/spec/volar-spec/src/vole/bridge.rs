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

/// Bit-pack committed timestamp bits into a field value: `Σ bits[i] · pow2[i]`.
///
/// Links a committed bit-vector timestamp (used by the [`crate::vole::bridge`]
/// less-than gadget for ordering) to the single field element the multiset
/// `encode` consumes — a free linear combination (public `pow2` powers).  Use
/// the same `pow2` on prover and verifier.
pub fn vope_bitpack<N, T>(bits: &[Vope<N, T, U1>], pow2: &[T]) -> Vope<N, T, U1>
where
    N: VoleArray<T>,
    T: Clone + Add<Output = T> + Mul<Output = T> + Default,
    Vope<N, T, U1>: Add<Output = Vope<N, T, U1>>,
{
    assert_eq!(bits.len(), pow2.len(), "vope_bitpack: length mismatch");
    let mut acc = Vope {
        u: Array::<Array<T, N>, U1>::from_fn(|_| Array::<T, N>::from_fn(|_| T::default())),
        v: Array::<T, N>::from_fn(|_| T::default()),
    };
    for (b, p) in bits.iter().zip(pow2.iter()) {
        acc = acc + vope_scale_const(b, p);
    }
    acc
}

// ── Verifier-side (Q) mirrors ───────────────────────────────────────────────

/// Scale a verifier `Q` share by a **public** field constant `c` (mirror of
/// [`vope_scale_const`]).
pub fn q_scale_const<N, T>(q: &Q<N, T>, c: &T) -> Q<N, T>
where
    N: ArraySize,
    T: Clone + Mul<Output = T>,
{
    Q { q: Array::<T, N>::from_fn(|i| q.q[i].clone() * c.clone()) }
}

/// In-circuit multiset-hash absorb on verifier `Q` shares (mirror of
/// [`mem_acc_absorb_vope`]): `acc + addr·r1 + value·r2 + ts·r3`.
pub fn mem_acc_absorb_q<N, T>(
    acc: Q<N, T>,
    addr: &Q<N, T>,
    value: &Q<N, T>,
    ts: &Q<N, T>,
    r1: &T,
    r2: &T,
    r3: &T,
) -> Q<N, T>
where
    N: ArraySize,
    T: Clone + Add<Output = T> + Mul<Output = T>,
{
    let a = q_scale_const(addr, r1);
    let v = q_scale_const(value, r2);
    let t = q_scale_const(ts, r3);
    Q {
        q: Array::<T, N>::from_fn(|i| {
            acc.q[i].clone() + a.q[i].clone() + v.q[i].clone() + t.q[i].clone()
        }),
    }
}

/// Verifier mirror of [`vope_bitpack`]: `Σ bits[i] · pow2[i]` over `Q` shares.
pub fn q_bitpack<N, T>(bits: &[Q<N, T>], pow2: &[T]) -> Q<N, T>
where
    N: ArraySize,
    T: Clone + Add<Output = T> + Mul<Output = T> + Default,
{
    assert_eq!(bits.len(), pow2.len(), "q_bitpack: length mismatch");
    let mut acc = Q { q: Array::<T, N>::from_fn(|_| T::default()) };
    for (b, p) in bits.iter().zip(pow2.iter()) {
        let scaled = q_scale_const(b, p);
        acc = Q { q: Array::<T, N>::from_fn(|i| acc.q[i].clone() + scaled.q[i].clone()) };
    }
    acc
}

/// Prover side of the memory-consistency **drain check**: open the mask of the
/// committed difference `mem_prod − mem_cons`.
///
/// For a degree-1 VOPE the prover's MAC of a wire is its `v`-component; the
/// difference's MAC is `mem_prod.v + mem_cons.v` (subtraction = XOR in
/// `GF(2^k)`).  The prover sends this array; the verifier checks it with
/// [`mem_drain_check`].
pub fn mem_drain_open<N, T>(prod: &Vope<N, T, U1>, cons: &Vope<N, T, U1>) -> Array<T, N>
where
    N: VoleArray<T>,
    T: Clone + Add<Output = T>,
{
    Array::<T, N>::from_fn(|i| prod.v[i].clone() + cons.v[i].clone())
}

/// Verifier side of the memory-consistency drain check.
///
/// Checks `K_prod + K_cons == opening` lane-wise, where `opening` is the
/// prover-sent mask from [`mem_drain_open`].  Because `K = M + x·Δ`, this holds
/// iff the committed difference `x = 0` (i.e. the produce/consume multisets
/// match) — a cheating prover with `x ≠ 0` would need `x·Δ = 0`, impossible for
/// non-zero `Δ`.  Returns `true` if memory was consistent.
pub fn mem_drain_check<N, T>(prod_q: &Q<N, T>, cons_q: &Q<N, T>, opening: &Array<T, N>) -> bool
where
    N: ArraySize,
    T: Clone + Add<Output = T> + PartialEq,
{
    let mut ok = true;
    for i in 0..N::USIZE {
        let k_diff = prod_q.q[i].clone() + cons_q.q[i].clone();
        ok = ok && (k_diff == opening[i].clone());
    }
    ok
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;
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
    fn bitpack_packs_le_bits() {
        // bits = [1,0,1] (=5), pow2 = [1,2,4]  ->  field value 5 in lane u[0].
        let bits = vec![vope(1, 0), vope(0, 0), vope(1, 0)];
        let pow2 = vec![1u64, 2, 4];
        let packed = vope_bitpack(&bits, &pow2);
        assert_eq!(packed.u[0][0], 5);
        // Q mirror.
        let qbits = vec![q::<1, 1>(), q::<0, 0>(), q::<1, 1>()];
        let qpacked = q_bitpack(&qbits, &pow2);
        assert_eq!(qpacked.q[0], 5);
    }

    #[test]
    fn q_scale_const_scales_lanes() {
        let s = q_scale_const(&q::<3, 5>(), &4u64);
        assert_eq!(s.q[0], 12);
        assert_eq!(s.q[1], 20);
    }

    #[test]
    fn mem_acc_q_matches_formula() {
        let (r1, r2, r3) = (7u64, 49, 343);
        let acc = q::<0, 0>();
        let out = mem_acc_absorb_q(acc, &q::<1, 1>(), &q::<11, 11>(), &q::<2, 2>(), &r1, &r2, &r3);
        assert_eq!(out.q[0], 1 * r1 + 11 * r2 + 2 * r3);
    }

    #[test]
    fn drain_open_xors_masks() {
        let open = mem_drain_open(&vope(0, 2), &vope(0, 5));
        assert_eq!(open[0], 7);
        assert_eq!(open[1], 7);
    }

    #[test]
    fn drain_check_accepts_and_rejects() {
        // K_prod + K_cons == opening  (3+5 == 8) lane-wise.
        let prod_q = q::<3, 3>();
        let cons_q = q::<5, 5>();
        let good = Array::<u64, U2>::from_fn(|_| 8);
        assert!(mem_drain_check(&prod_q, &cons_q, &good));
        let mut bad = good.clone();
        bad[0] = 9;
        assert!(!mem_drain_check(&prod_q, &cons_q, &bad));
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
