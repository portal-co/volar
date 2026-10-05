// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//! Ferret LPN parameter sets.
//!
//! Table 2 of ePrint 2020/924 (Ferret-Reg / Ferret-Uni, targeting 128-bit
//! attack cost) is recorded as named constants. Production-size constants are
//! checked arithmetically; full-size protocol runs are intentionally not unit
//! tested. [`FERRET_REG_TOY`] / [`FERRET_UNI_TOY`] are insecure and exist only
//! for correctness tests.

/// Primal-LPN dimensions for one Ferret ΠCOT iteration (Fig. 9).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FerretParams {
    /// Output length `n` of the LPN code (and of one MPCOT).
    pub n: usize,
    /// LPN dimension `k` (columns of `A` are in `F_2^k`).
    pub k: usize,
    /// Hamming weight of the noise / number of SPCOT punctures.
    pub t: usize,
}

impl FerretParams {
    /// Padded SPCOT block length for the largest regular-noise interval.
    ///
    /// The `t` intervals partition `[0,n)` at `floor(i*n/t)`. When `n/t` is
    /// not a power of two (including the official Ferret-Reg setup set), each
    /// interval is padded to the next power of two and unused leaves are
    /// discarded by regular MPCOT.
    pub fn splen(&self) -> usize {
        debug_assert!(self.t > 0 && self.n >= self.t);
        let max_interval_len = self.n.div_ceil(self.t);
        max_interval_len
            .checked_next_power_of_two()
            .expect("Ferret SPCOT interval length overflow")
    }

    /// `h = ceil(log2(ceil(n/t)))`.
    pub fn log_splen(&self) -> usize {
        let s = self.splen();
        debug_assert!(s.is_power_of_two());
        s.trailing_zeros() as usize
    }

    /// Bounds of a regular-noise interval, partitioning `[0,n)` at
    /// `floor(i*n/t)`. Returns `None` for an invalid interval index/parameter.
    pub fn interval_bounds(&self, index: usize) -> Option<(usize, usize)> {
        if self.t == 0 || index >= self.t {
            return None;
        }
        let n = self.n as u128;
        let t = self.t as u128;
        let start = (index as u128 * n / t) as usize;
        let end = ((index as u128 + 1) * n / t) as usize;
        Some((start, end))
    }

    /// Seed COTs consumed per regular iteration: `k + t·h`, where
    /// `h = ceil(log2(ceil(n/t)))`, plus `κ` if the SPCOT consistency check
    /// is enabled (Fig. 6 steps 6–9).
    pub fn seed_cot_count(&self, malicious: bool) -> usize {
        let body = self.k + self.t * self.log_splen();
        if malicious {
            body + super::KAPPA_BITS
        } else {
            body
        }
    }

    /// COTs emitted per iteration after keeping the next seed (`n − M`).
    pub fn output_cot_count(&self, malicious: bool) -> usize {
        self.n.saturating_sub(self.seed_cot_count(malicious))
    }
}

/// Insecure toy Ferret-Reg: `n=256`, `k=32`, `t=4`, `splen=64`.
pub const FERRET_REG_TOY: FerretParams = FerretParams {
    n: 256,
    k: 32,
    t: 4,
};

/// Table 2 Ferret-Reg one-time setup `(n0, k0, t0)`. Its 1,269 regular-noise
/// intervals have 480 or 481 entries and are padded to SPCOT length 512.
pub const FERRET_REG_SETUP: FerretParams = FerretParams {
    n: 609_728,
    k: 36_288,
    t: 1_269,
};

/// Table 2 Ferret-Reg main iteration. Not for tests.
pub const FERRET_REG_MAIN: FerretParams = FerretParams {
    n: 10_805_248,
    k: 589_760,
    t: 1_319,
};

/// Insecure toy Ferret-Uni (same `n,k,t` as Reg toy; Cuckoo uses `m=1.5 t`).
pub const FERRET_UNI_TOY: FerretParams = FerretParams {
    n: 256,
    k: 32,
    t: 4,
};

/// Table 2 Ferret-Uni setup. Not for tests.
pub const FERRET_UNI_SETUP: FerretParams = FerretParams {
    n: 616_092,
    k: 37_248,
    t: 1_254,
};

/// Table 2 Ferret-Uni main iteration. Not for tests.
pub const FERRET_UNI_MAIN: FerretParams = FerretParams {
    n: 10_616_092,
    k: 588_160,
    t: 1_324,
};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn paper_reg_setup_and_main_dimensions_and_seed_counts() {
        assert_eq!(FERRET_REG_SETUP.splen(), 512);
        assert_eq!(FERRET_REG_SETUP.interval_bounds(0), Some((0, 480)));
        assert_eq!(FERRET_REG_SETUP.interval_bounds(1), Some((480, 960)));
        assert_eq!(
            FERRET_REG_SETUP.interval_bounds(1_268),
            Some((609_247, 609_728))
        );
        assert_eq!(FERRET_REG_SETUP.seed_cot_count(false), 36_288 + 1_269 * 9);
        assert_eq!(FERRET_REG_SETUP.seed_cot_count(true), 47_837);
        assert_eq!(FERRET_REG_SETUP.output_cot_count(false), 609_728 - 47_709);

        assert_eq!(FERRET_REG_MAIN.n / FERRET_REG_MAIN.t, 8192);
        assert_eq!(FERRET_REG_MAIN.splen(), 8192);
        assert_eq!(FERRET_REG_MAIN.seed_cot_count(false), 606_907);
        assert_eq!(FERRET_REG_MAIN.seed_cot_count(true), 607_035);
        assert_eq!(FERRET_REG_TOY.seed_cot_count(false), 32 + 4 * 6);
        assert_eq!(FERRET_REG_TOY.output_cot_count(false), 256 - 56);
    }
}
