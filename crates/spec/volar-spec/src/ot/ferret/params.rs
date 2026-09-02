// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//! Ferret LPN parameter sets.
//!
//! Table 2 of ePrint 2020/924 (Ferret-Reg / Ferret-Uni, 128-bit attack-cost
//! target) is recorded as named constants. **Those sizes are not used in
//! unit tests.** [`FERRET_REG_TOY`] / [`FERRET_UNI_TOY`] are insecure and
//! exist only for correctness tests.

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
    /// SPCOT block length `n/t`. Must be a power of two for regular MPCOT.
    pub fn splen(&self) -> usize {
        debug_assert!(self.t > 0 && self.n % self.t == 0);
        self.n / self.t
    }

    /// `h = log2(n/t)`.
    pub fn log_splen(&self) -> usize {
        let s = self.splen();
        debug_assert!(s.is_power_of_two());
        s.trailing_zeros() as usize
    }

    /// Seed COTs consumed per iteration: `k + t·log(n/t)`, plus `κ` if
    /// the SPCOT consistency check is enabled (Fig. 6 steps 6–9).
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

/// Table 2 Ferret-Reg one-time setup `(n0, k0, t0)`. Not for tests.
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
    fn table2_reg_main_splen_is_power_of_two() {
        assert_eq!(FERRET_REG_MAIN.n / FERRET_REG_MAIN.t, 8192);
        assert_eq!(FERRET_REG_MAIN.splen(), 8192);
        assert_eq!(FERRET_REG_TOY.seed_cot_count(false), 32 + 4 * 6);
        assert_eq!(FERRET_REG_TOY.output_cot_count(false), 256 - 56);
        // Setup n/t is not a power of two in Table 2; do not call splen().
        assert_eq!(FERRET_REG_SETUP.n, 609_728);
        assert_eq!(FERRET_REG_SETUP.k, 36_288);
        assert_eq!(FERRET_REG_SETUP.t, 1_269);
    }
}
