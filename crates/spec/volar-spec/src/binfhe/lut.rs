// @pinnedness: unpinned
// @stability: very-unstable
// @ai: assisted
//! Validated multi-input lookup tables for programmable bootstrapping.
//!
//! A `Lut` encodes a Boolean function of `ADDR_BITS` address bits into one
//! negacyclic test polynomial. The construction (plan §3, §7):
//!
//! - All wires in a circuit share the encoding `Delta = q / 2^(K_MAX+1)`
//!   ([`wire_delta`](crate::binfhe::lwe::wire_delta)).
//! - The selector combines address wires with exact integer weights
//!   `2^j` (ciphertext-linear), so the clear phase is
//!   `addr * Delta` with `addr = sum_j b_j 2^j`, then adds the half-bin
//!   centering offset `Delta/2`.
//! - Bin width is `W = BIG_N >> K_MAX` test-polynomial coefficients
//!   (`Delta` ring positions, since `q = 2N`). Bin `a` occupies
//!   `[a*W, (a+1)*W)`. Because `addr < 2^ADDR_BITS <= 2^K_MAX`, the selector
//!   phase stays in `[0, N/2 + Delta/2) ⊂ [0, N)`: it never crosses the
//!   negacyclic sign boundary, so **arbitrary** tables are representable —
//!   no negacyclic-complement restriction, and no fractional-weight
//!   selector (the legacy module's `ADDR_BITS <= 2` limitation).
//! - Table values are stored pre-scaled to the ring modulus
//!   (`{0, Delta_out} * (Q/q)`), so an in-budget read produces the exact
//!   canonical output encoding; output noise comes only from the evaluation
//!   key, not from the table.
//!
//! A constant table is represented explicitly and "read" as a trivial
//! ciphertext without bootstrapping (exact, noiseless, free).
//!
//! # Noise requirement
//!
//! A read is correct iff the selector phase error stays below `Delta/2`.
//! Bootstrapped wires carry only evaluation-key noise; freshly encrypted
//! circuit inputs carry the keygen noise, amplified by the selector weights
//! (`sum_j 2^j = 2^k - 1`). The failure-budget accounting in
//! [`crate::binfhe::plan`] is responsible for keeping each fused cone
//! within budget for the active profile.

use crate::binfhe::torus;

/// A failure to encode a logical table for one programmable bootstrap.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LutError {
    /// `ADDR_BITS` must be at least 1 and `TABLE_LEN` must equal
    /// `2^ADDR_BITS`.
    AddressShapeInvalid,
    /// `ADDR_BITS` exceeds the circuit's declared maximum arity `K_MAX`.
    ArityExceedsCircuitMax,
    /// The profile/modulus chain cannot represent `K_MAX`-ary tables:
    /// need `LOG_Q_LWE >= K_MAX + 2`, `2^K_MAX <= BIG_N`, `q = 2N`, and
    /// `LOG_Q_LWE <= LOG_Q <= 32`.
    ShapeUnsupported,
}

/// A validated Boolean lookup table with its precomputed test polynomial.
///
/// Const-generic parameters: `ADDR_BITS`/`TABLE_LEN` describe the logical
/// table; `BIG_N`, `LOG_Q`, `LOG_Q_LWE` describe the profile's ring and
/// moduli; `K_MAX` is the circuit-wide maximum arity fixing the wire
/// encoding. All are part of the type, so a table built for one profile or
/// encoding cannot be read against another.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Lut<
    const ADDR_BITS: usize,
    const TABLE_LEN: usize,
    const BIG_N: usize,
    const LOG_Q: u32,
    const LOG_Q_LWE: u32,
    const K_MAX: usize,
> {
    logical: [bool; TABLE_LEN],
    test_poly: [u32; BIG_N],
    is_constant: bool,
}

impl<
    const ADDR_BITS: usize,
    const TABLE_LEN: usize,
    const BIG_N: usize,
    const LOG_Q: u32,
    const LOG_Q_LWE: u32,
    const K_MAX: usize,
> Lut<ADDR_BITS, TABLE_LEN, BIG_N, LOG_Q, LOG_Q_LWE, K_MAX>
{
    /// Construct and validate a table. `const`, so a fixed table selected
    /// by the weaver can be materialized at compile time.
    pub const fn new(logical: [bool; TABLE_LEN]) -> Result<Self, LutError> {
        // Shape checks.
        if ADDR_BITS == 0 || ADDR_BITS >= usize::BITS as usize {
            return Err(LutError::AddressShapeInvalid);
        }
        if TABLE_LEN != 1usize << ADDR_BITS {
            return Err(LutError::AddressShapeInvalid);
        }
        if ADDR_BITS > K_MAX {
            return Err(LutError::ArityExceedsCircuitMax);
        }
        if (K_MAX as u32) + 2 > LOG_Q_LWE
            || !BIG_N.is_power_of_two()
            || (1usize << K_MAX) > BIG_N
            || (1usize << LOG_Q_LWE) != 2 * BIG_N
            || LOG_Q_LWE > LOG_Q
            || LOG_Q > 32
        {
            return Err(LutError::ShapeUnsupported);
        }

        let mut is_constant = true;
        let mut i = 1;
        while i < TABLE_LEN {
            if logical[i] != logical[0] {
                is_constant = false;
                break;
            }
            i += 1;
        }

        // Output value at ring-modulus scale: Delta_out * (Q / q).
        let delta_out: u32 = 1u32 << (LOG_Q_LWE - 1 - K_MAX as u32);
        let value = torus::reduce::<LOG_Q>(delta_out << (LOG_Q - LOG_Q_LWE));
        let width = BIG_N >> K_MAX; // coefficients per bin, >= 1
        let used = if is_constant { 0 } else { TABLE_LEN * width }; // <= BIG_N since ADDR_BITS <= K_MAX
        let mut test_poly = [0u32; BIG_N];
        let mut p = 0;
        while p < used {
            test_poly[p] = if logical[p / width] { value } else { 0 };
            p += 1;
        }
        // Positions [used, BIG_N) stay zero: an in-budget selector phase
        // never reads them (phase < TABLE_LEN * width + width / 2).

        Ok(Self {
            logical,
            test_poly,
            is_constant,
        })
    }

    /// The logical table entries, address-ordered (bit 0 = LSB).
    pub const fn entries(&self) -> &[bool; TABLE_LEN] {
        &self.logical
    }

    /// The precomputed test polynomial (ring-modulus scale).
    pub const fn test_polynomial(&self) -> &[u32; BIG_N] {
        &self.test_poly
    }

    /// Whether the table is constant (read as a trivial ciphertext).
    pub const fn is_constant(&self) -> bool {
        self.is_constant
    }

    /// The constant value; only meaningful when [`Self::is_constant`].
    pub const fn constant_value(&self) -> bool {
        self.logical[0]
    }

    /// The output wire delta produced by reads of this table.
    pub const fn output_delta(&self) -> u32 {
        1u32 << (LOG_Q_LWE - 1 - K_MAX as u32)
    }
}

/// Standard two-input gates as associated constants, indexed by
/// `addr = a + 2*b` (first operand is the least-significant address bit).
impl<
    const BIG_N: usize,
    const LOG_Q: u32,
    const LOG_Q_LWE: u32,
    const K_MAX: usize,
> Lut<2, 4, BIG_N, LOG_Q, LOG_Q_LWE, K_MAX>
{
    /// AND: `[false, false, false, true]`.
    pub const AND: Result<Self, LutError> = Self::new([false, false, false, true]);
    /// OR: `[false, true, true, true]`.
    pub const OR: Result<Self, LutError> = Self::new([false, true, true, true]);
    /// XOR: `[false, true, true, false]`.
    pub const XOR: Result<Self, LutError> = Self::new([false, true, true, false]);
    /// NAND: `[true, true, true, false]`.
    pub const NAND: Result<Self, LutError> = Self::new([true, true, true, false]);
    /// NOR: `[true, false, false, false]`.
    pub const NOR: Result<Self, LutError> = Self::new([true, false, false, false]);
    /// XNOR: `[true, false, false, true]`.
    pub const XNOR: Result<Self, LutError> = Self::new([true, false, false, true]);
}

/// Standard one-input tables, indexed by `addr = a`.
impl<
    const BIG_N: usize,
    const LOG_Q: u32,
    const LOG_Q_LWE: u32,
    const K_MAX: usize,
> Lut<1, 2, BIG_N, LOG_Q, LOG_Q_LWE, K_MAX>
{
    /// Identity: `[false, true]`.
    pub const IDENTITY: Result<Self, LutError> = Self::new([false, true]);
    /// NOT (as a bootstrapped refresh): `[true, false]`.
    pub const NOT: Result<Self, LutError> = Self::new([true, false]);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::binfhe::params::toy;

    type ToyLut2 = Lut<2, 4, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE }, 2>;

    #[test]
    fn standard_gate_tables_construct_at_compile_time() {
        let and = ToyLut2::AND.unwrap();
        assert_eq!(and.entries(), &[false, false, false, true]);
        assert!(!and.is_constant());
        // Bin width W = 64 >> 2 = 16 coefficients; bin 3 occupies [48, 64).
        // Delta_out(q=128, K=2) = 16; toy has Q = q, so no upscaling.
        let tp = and.test_polynomial();
        assert!(tp[..48].iter().all(|&v| v == 0));
        assert!(tp[48..].iter().all(|&v| v == 16));
    }

    #[test]
    fn constant_tables_are_detected() {
        let c = ToyLut2::new([true, true, true, true]).unwrap();
        assert!(c.is_constant());
        assert!(c.constant_value());
        assert!(c.test_polynomial().iter().all(|&v| v == 0));
    }

    #[test]
    fn invalid_shapes_are_rejected() {
        // Table length must be 2^ADDR_BITS.
        assert!(matches!(
            Lut::<2, 3, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE }, 2>::new(
                [false, true, false]
            ),
            Err(LutError::AddressShapeInvalid)
        ));
        // Arity over the circuit max.
        assert!(matches!(
            Lut::<3, 8, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE }, 2>::new(
                [false; 8]
            ),
            Err(LutError::ArityExceedsCircuitMax)
        ));
        // K_MAX over the profile cap (LOG_Q_LWE = 7 -> K_MAX <= 5).
        assert!(matches!(
            Lut::<6, 64, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE }, 6>::new(
                [false; 64]
            ),
            Err(LutError::ShapeUnsupported)
        ));
        // Non-power-of-two ring.
        assert!(matches!(
            Lut::<1, 2, 48, 8, 7, 2>::new([false, true]),
            Err(LutError::ShapeUnsupported)
        ));
    }

    #[test]
    fn multi_input_table_bins_are_exact() {
        // 4-address-bit table on toy (K_MAX = 4 -> W = 4 coefficients,
        // Delta_out = 8 at q, 16 at Q).
        let mut table = [false; 16];
        for (i, e) in table.iter_mut().enumerate() {
            *e = i % 3 == 0;
        }
        let lut =
            Lut::<4, 16, { toy::BIG_N }, { toy::LOG_Q }, { toy::LOG_Q_LWE }, 4>::new(table)
                .unwrap();
        // Delta_out(q=128, K=4) = 128/2^5 = 4; toy has Q = q (no upscale).
        let tp = lut.test_polynomial();
        for addr in 0..16usize {
            let bin_value = tp[addr * 4];
            let expected = if table[addr] { 4 } else { 0 };
            assert_eq!(bin_value, expected, "bin {addr}");
            // Whole bin is uniform.
            assert!(tp[addr * 4..(addr + 1) * 4].iter().all(|&v| v == bin_value));
        }
    }
}
