// @reliability: experimental
// @ai: assisted
//! Shared R1CS constraint-builder gadget kit: linear combinations (`Lc`) over
//! the witness plus a `Builder` that accumulates constraint rows *and* the
//! satisfying assignment together (so the R1CS and its witness cannot
//! disagree).
//!
//! Extracted from [`crate::keccak_r1cs`] so [`crate::gf2k`] can reuse it
//! verbatim — same gadgets, same row shapes, no behavior change from the
//! original `keccak_r1cs.rs` copy.

use alloc::vec::Vec;

use crate::r1cs::R1CS;
use crate::scalar::Scalar;

/// Sentinel column for an `Lc`'s constant term; patched to the `u`/one column
/// (the last column of `z`) at [`Builder::finish`].
pub(crate) const ONE_COL: usize = usize::MAX;

pub(crate) fn bit_scalar(b: bool) -> Scalar {
    if b {
        Scalar::ONE
    } else {
        Scalar::ZERO
    }
}

/// A linear combination `Σ coeff_i·var_i + constant·1` over the witness.
#[derive(Clone)]
pub(crate) struct Lc {
    pub(crate) terms: Vec<(usize, Scalar)>,
    pub(crate) constant: Scalar,
}

impl Lc {
    pub(crate) fn zero() -> Lc {
        Lc { terms: Vec::new(), constant: Scalar::ZERO }
    }
    pub(crate) fn one() -> Lc {
        Lc { terms: Vec::new(), constant: Scalar::ONE }
    }
    pub(crate) fn from_var(v: usize) -> Lc {
        Lc { terms: alloc::vec![(v, Scalar::ONE)], constant: Scalar::ZERO }
    }
    pub(crate) fn is_static_zero(&self) -> bool {
        self.terms.is_empty() && self.constant == Scalar::ZERO
    }
    pub(crate) fn add(&self, other: &Lc) -> Lc {
        let mut terms = self.terms.clone();
        terms.extend_from_slice(&other.terms);
        Lc { terms, constant: self.constant.add(&other.constant) }
    }
    pub(crate) fn sub(&self, other: &Lc) -> Lc {
        let mut terms = self.terms.clone();
        terms.extend(other.terms.iter().map(|(v, c)| (*v, c.neg())));
        Lc { terms, constant: self.constant.sub(&other.constant) }
    }
    pub(crate) fn scale(&self, s: &Scalar) -> Lc {
        Lc {
            terms: self.terms.iter().map(|(v, c)| (*v, c.mul(s))).collect(),
            constant: self.constant.mul(s),
        }
    }
}

/// Constraint accumulator that *also* carries the satisfying assignment, so the
/// R1CS and its witness are built together (and cannot disagree).
pub(crate) struct Builder {
    a: Vec<(usize, usize, Scalar)>,
    b: Vec<(usize, usize, Scalar)>,
    c: Vec<(usize, usize, Scalar)>,
    num_cons: usize,
    /// Witness values, indexed by variable.
    w: Vec<Scalar>,
    inv2: Scalar,
}

impl Builder {
    pub(crate) fn new() -> Builder {
        Builder {
            a: Vec::new(),
            b: Vec::new(),
            c: Vec::new(),
            num_cons: 0,
            w: Vec::new(),
            inv2: Scalar::from_u64(2).invert(),
        }
    }

    pub(crate) fn alloc(&mut self, val: Scalar) -> usize {
        self.w.push(val);
        self.w.len() - 1
    }

    /// Value of an `Lc` under the current assignment (the one-column = 1).
    pub(crate) fn eval(&self, lc: &Lc) -> Scalar {
        let mut acc = lc.constant;
        for (v, c) in &lc.terms {
            acc = acc.add(&c.mul(&self.w[*v]));
        }
        acc
    }

    pub(crate) fn eval_bit(&self, lc: &Lc) -> bool {
        self.eval(lc) == Scalar::ONE
    }

    fn push_row(mat: &mut Vec<(usize, usize, Scalar)>, row: usize, lc: &Lc) {
        for (v, c) in &lc.terms {
            mat.push((row, *v, *c));
        }
        if lc.constant != Scalar::ZERO {
            mat.push((row, ONE_COL, lc.constant));
        }
    }

    /// Emit the constraint `(a)·(b) = (c)`.
    pub(crate) fn enforce(&mut self, a: &Lc, b: &Lc, c: &Lc) {
        let row = self.num_cons;
        Self::push_row(&mut self.a, row, a);
        Self::push_row(&mut self.b, row, b);
        Self::push_row(&mut self.c, row, c);
        self.num_cons += 1;
    }

    /// Emit the constraint `(lc)·(1) = 0` — a single linear row, same shape as
    /// the `and_check_r1cs`-style output-binding rows (`A = lc`, `B = 1`, `C = 0`).
    pub(crate) fn enforce_linear(&mut self, lc: &Lc) {
        self.enforce(lc, &Lc::one(), &Lc::zero());
    }

    /// A fresh **input** bit: a witness variable constrained boolean (`b² = b`).
    pub(crate) fn input_bit(&mut self, bit: bool) -> Lc {
        let v = self.alloc(bit_scalar(bit));
        let lc = Lc::from_var(v);
        self.enforce(&lc, &lc, &lc); // v·v = v ⇒ v ∈ {0,1}
        lc
    }

    pub(crate) fn not(&self, a: &Lc) -> Lc {
        Lc::one().sub(a)
    }

    /// `p = a ∧ b` — one constraint, fresh boolean var (boolean by `a,b` boolean).
    pub(crate) fn and(&mut self, a: &Lc, b: &Lc) -> Lc {
        if a.is_static_zero() || b.is_static_zero() {
            return Lc::zero();
        }
        let val = bit_scalar(self.eval_bit(a) && self.eval_bit(b));
        let p = self.alloc(val);
        let out = Lc::from_var(p);
        self.enforce(a, b, &out);
        out
    }

    /// `r = a ⊕ b` via `(a)·(b) = ½·(a + b − r)` — one constraint, fresh var.
    pub(crate) fn xor(&mut self, a: &Lc, b: &Lc) -> Lc {
        if a.is_static_zero() {
            return b.clone();
        }
        if b.is_static_zero() {
            return a.clone();
        }
        let val = bit_scalar(self.eval_bit(a) ^ self.eval_bit(b));
        let r = self.alloc(val);
        let r_lc = Lc::from_var(r);
        let c = a.add(b).sub(&r_lc).scale(&self.inv2);
        self.enforce(a, b, &c);
        r_lc
    }

    /// XOR with a public constant bit — free (`⊕0` identity, `⊕1` is NOT).
    pub(crate) fn xor_const(&self, a: &Lc, bit: bool) -> Lc {
        if bit {
            self.not(a)
        } else {
            a.clone()
        }
    }

    /// Finalize into an [`R1CS`]; returns `(r1cs, witness W)` with `z = [W ‖ 1]`.
    pub(crate) fn finish(mut self) -> (R1CS, Vec<Scalar>) {
        let one_col = self.w.len();
        let num_vars = one_col + 1;
        for mat in [&mut self.a, &mut self.b, &mut self.c] {
            for entry in mat.iter_mut() {
                if entry.1 == ONE_COL {
                    entry.1 = one_col;
                }
            }
        }
        let r1cs = R1CS { num_cons: self.num_cons, num_vars, a: self.a, b: self.b, c: self.c };
        (r1cs, self.w)
    }
}
