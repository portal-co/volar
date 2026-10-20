// @reliability: experimental
// @ai: assisted
//! Step-circuit → PESAT emission. One `PesatIndex` per movfuscated step block
//! (the constraint set is the step relation); the per-step `PesatWitness` comes
//! from [`crate::eval_step_wires`]. Every variable is a GF(2) bit embedded in
//! `Galois128`, with an explicit booleanity constraint (`v·v + v = 0`) per wire.

use volar_ir::ir::{IRBlock, IRVarId};
use volar_ir_common::{Constant, Stmt, TypeId, TypeTable as IRTypes};
use volar_iop::warp::{PesatConstraint, PesatIndex, PesatTerm};
use volar_primitives::Galois128;

use crate::eval::{const_bits, stmt_out_width, width_of};

/// Emission failure: the step block used an op outside this emitter's
/// bit-level scope, or a shape the PESAT encoding cannot express.
#[derive(Debug)]
pub enum EmitError {
    /// A statement op outside the emitter's scope (storage, oracle, action,
    /// rng — the memory boundary is a later milestone, E3).
    UnsupportedStmt {
        /// The `core::mem::discriminant` of the offending `Stmt` variant.
        discriminant: core::mem::Discriminant<Stmt<IRVarId>>,
    },
    /// A field-typed `Poly` (field multiplication is a different term shape).
    Unsupported(&'static str),
    /// A type with no GF(2) bit width.
    NonDataType(TypeId),
    /// An input count/width mismatch.
    Arity {
        /// Expected count/width.
        expected: usize,
        /// Supplied count/width.
        got: usize,
    },
    /// A referenced variable has no computed value.
    UndefinedVar(IRVarId),
}

impl core::fmt::Display for EmitError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::UnsupportedStmt { .. } => write!(f, "unsupported statement in step circuit"),
            Self::Unsupported(what) => write!(f, "unsupported: {what}"),
            Self::NonDataType(ty) => write!(f, "non-data type {ty:?}"),
            Self::Arity { expected, got } => write!(f, "arity mismatch: expected {expected}, got {got}"),
            Self::UndefinedVar(v) => write!(f, "undefined var {}", v.0),
        }
    }
}

impl std::error::Error for EmitError {}

/// The result of emitting one step block: the PESAT index plus the variable
/// layout needed to build a matching witness.
#[derive(Debug)]
pub struct StepEmission {
    /// The step relation as a PESAT instance (padded to a power-of-two
    /// constraint count with always-satisfied empty constraints).
    pub index: PesatIndex<Galois128>,
    /// Number of public variables (the block's parameter bits — the step's
    /// `(pc, state)` input boundary).
    pub public_len: usize,
    /// Number of witness variables (the block's statement-output bits).
    pub witness_len: usize,
    /// `var_base[v]` = the first PESAT variable index of IR var `v`'s bits.
    pub var_base: Vec<usize>,
    /// Bit width of each IR var (parallel to `var_base`).
    pub var_width: Vec<usize>,
}

const ZERO: Galois128 = Galois128(0);
const ONE: Galois128 = Galois128(1);

/// A constraint-under-construction: a sum of `coeff · ∏ vars` terms.
struct ConstraintBuilder {
    terms: Vec<PesatTerm<Galois128>>,
}

impl ConstraintBuilder {
    fn new() -> Self {
        Self { terms: Vec::new() }
    }
    /// Add `coeff · ∏ vars` (dropping zero-coeff terms).
    fn term(&mut self, coeff: Galois128, vars: Vec<usize>) {
        if coeff != ZERO {
            self.terms.push(PesatTerm { coeff, vars });
        }
    }
    fn build(self) -> PesatConstraint<Galois128> {
        PesatConstraint { terms: self.terms }
    }
}

/// Map an IR var + bit position to its PESAT variable index.
struct VarMap {
    var_base: Vec<usize>,
    var_width: Vec<usize>,
}

impl VarMap {
    fn idx(&self, var: IRVarId, bit: usize) -> Result<usize, EmitError> {
        let base = self
            .var_base
            .get(var.0 as usize)
            .ok_or(EmitError::UndefinedVar(var))?;
        Ok(base + bit)
    }
    fn width(&self, var: IRVarId) -> Result<usize, EmitError> {
        self.var_width
            .get(var.0 as usize)
            .copied()
            .ok_or(EmitError::UndefinedVar(var))
    }
}

/// Emit the PESAT index for a movfuscated step block. The variable layout is
/// `public = param bits` then `witness = statement-output bits`, in order.
pub fn emit_step_index(
    block: &IRBlock<()>,
    types: &IRTypes,
) -> Result<StepEmission, EmitError> {
    // Assign variable indices: params (public) then statement outputs
    // (witness), each var occupying `width` consecutive PESAT variables.
    let n_params = block.params.len();
    let mut var_base = Vec::with_capacity(n_params + block.stmts.len());
    let mut var_width = Vec::with_capacity(n_params + block.stmts.len());
    let mut next = 0usize;
    for param in &block.params {
        let w = width_of(*param, types)?;
        var_base.push(next);
        var_width.push(w);
        next += w;
    }
    let public_len = next;
    for node in &block.stmts {
        let w = stmt_out_width(&node.kind, types)?;
        var_base.push(next);
        var_width.push(w);
        next += w;
    }
    let witness_len = next - public_len;
    let map = VarMap { var_base, var_width };

    let mut constraints: Vec<PesatConstraint<Galois128>> = Vec::new();

    // Booleanity: every wire is a bit — `v·v + v = 0` (v ∈ {0,1} in char 2).
    for v in 0..next {
        let mut c = ConstraintBuilder::new();
        c.term(ONE, vec![v, v]);
        c.term(ONE, vec![v]);
        constraints.push(c.build());
    }

    // Relational constraints, one per output bit of each statement.
    for (i, node) in block.stmts.iter().enumerate() {
        let rv = IRVarId((n_params + i) as u32);
        emit_stmt_constraints(&node.kind, rv, &map, types, &mut constraints)?;
    }

    // Pad to a power-of-two constraint count with empty (always-satisfied)
    // constraints — the PESAT index requires M a power of two.
    let target = constraints.len().next_power_of_two().max(1);
    while constraints.len() < target {
        constraints.push(PesatConstraint::default());
    }

    let degree_bound = constraints
        .iter()
        .map(|c| {
            c.terms
                .iter()
                .map(|t| t.vars.len())
                .max()
                .unwrap_or(0)
        })
        .max()
        .unwrap_or(0);

    let index = PesatIndex {
        constraints,
        public_len,
        witness_len,
        degree_bound,
    };
    Ok(StepEmission {
        index,
        public_len,
        witness_len,
        var_base: map.var_base,
        var_width: map.var_width,
    })
}

/// Emit the relational constraints for one statement whose result var is `rv`.
fn emit_stmt_constraints(
    stmt: &Stmt<IRVarId>,
    rv: IRVarId,
    map: &VarMap,
    types: &IRTypes,
    constraints: &mut Vec<PesatConstraint<Galois128>>,
) -> Result<(), EmitError> {
    let out_w = map.width(rv)?;
    // `out[j] + rhs = 0` (char 2): out equals the described value.
    let mut eq = |bit: usize, build: &mut dyn FnMut(&mut ConstraintBuilder)| -> Result<(), EmitError> {
        let mut c = ConstraintBuilder::new();
        c.term(ONE, vec![map.idx(rv, bit)?]);
        build(&mut c);
        constraints.push(c.build());
        Ok(())
    };
    match stmt {
        Stmt::Const(c, ty) => {
            let w = width_of(*ty, types)?;
            let bits = const_bits(c, w);
            for j in 0..w {
                eq(j, &mut |cb| {
                    cb.term(if bits[j] { ONE } else { ZERO }, vec![]);
                })?;
            }
        }
        Stmt::Poly {
            ty,
            coeffs,
            constant,
        } => {
            if volar_ir_common::contains_ext_field(*ty, types)
                || volar_ir_common::contains_prime_field(*ty, types)
            {
                return Err(EmitError::Unsupported("field-typed Poly"));
            }
            let w = width_of(*ty, types)?;
            let const_b = const_bits(constant, w);
            for lane in 0..w {
                eq(lane, &mut |cb| {
                    cb.term(if const_b[lane] { ONE } else { ZERO }, vec![]);
                    for (mono, coeff) in coeffs {
                        if coeff & 1 == 0 {
                            continue;
                        }
                        // lane_product: width-1 factor broadcasts; a narrower
                        // factor contributes 0 past its width, killing the
                        // monomial for this lane.
                        let mut vars = Vec::with_capacity(mono.len());
                        let mut killed = false;
                        for v in mono {
                            let vw = map.width(*v).unwrap_or(0);
                            let bit = if vw == 1 { 0 } else { lane };
                            if vw != 1 && lane >= vw {
                                killed = true;
                                break;
                            }
                            vars.push(map.idx(*v, bit).unwrap_or(0));
                        }
                        if !killed {
                            cb.term(ONE, vars);
                        }
                    }
                })?;
            }
        }
        Stmt::Merge { parts, .. } => {
            // out = concat of part bits (LSB-first), truncated/padded.
            let mut j = 0usize;
            'outer: for part in parts {
                let pw = map.width(*part)?;
                for k in 0..pw {
                    if j >= out_w {
                        break 'outer;
                    }
                    let src = map.idx(*part, k)?;
                    eq(j, &mut |cb| {
                        cb.term(ONE, vec![src]);
                    })?;
                    j += 1;
                }
            }
            while j < out_w {
                // zero-padded tail: out[j] = 0.
                eq(j, &mut |_cb| {})?;
                j += 1;
            }
        }
        Stmt::Splat { src, .. } => {
            let src = map.idx(*src, 0)?;
            for j in 0..out_w {
                eq(j, &mut |cb| {
                    cb.term(ONE, vec![src]);
                })?;
            }
        }
        Stmt::Shuffle { result_bits, .. } => {
            for i in 0..out_w {
                let Some((bit_idx, var)) = result_bits.get(i) else {
                    // Past the mapping: out is 0.
                    eq(i, &mut |_cb| {})?;
                    continue;
                };
                let src = map.idx(*var, *bit_idx as usize)?;
                eq(i, &mut |cb| {
                    cb.term(ONE, vec![src]);
                })?;
            }
        }
        Stmt::Transmute { src, .. } => {
            let sw = map.width(*src)?;
            for j in 0..out_w {
                if j < sw {
                    let s = map.idx(*src, j)?;
                    eq(j, &mut |cb| {
                        cb.term(ONE, vec![s]);
                    })?;
                } else {
                    eq(j, &mut |_cb| {})?;
                }
            }
        }
        Stmt::Rol { src, n, .. } => {
            let w = out_w.max(1);
            for j in 0..out_w {
                let s = map.idx(*src, (j + w - (n % w)) % w)?;
                eq(j, &mut |cb| {
                    cb.term(ONE, vec![s]);
                })?;
            }
        }
        Stmt::Ror { src, n, .. } => {
            let w = out_w.max(1);
            for j in 0..out_w {
                let s = map.idx(*src, (j + (n % w)) % w)?;
                eq(j, &mut |cb| {
                    cb.term(ONE, vec![s]);
                })?;
            }
        }
        other => {
            return Err(EmitError::UnsupportedStmt {
                discriminant: core::mem::discriminant(other),
            })
        }
    }
    Ok(())
}

/// Build the witness for one step from its param inputs and the full wire
/// trace (from [`crate::eval_step_wires`]), embedding each bit into the field.
pub fn build_witness(
    emission: &StepEmission,
    wires: &[Vec<bool>],
) -> volar_iop::warp::PesatWitness<Galois128> {
    let mut all = Vec::with_capacity(emission.public_len + emission.witness_len);
    for bits in wires {
        all.extend(bits.iter().map(|&b| crate::bit_to_field(b)));
    }
    volar_iop::warp::PesatWitness {
        public: all[..emission.public_len].to_vec(),
        witness: all[emission.public_len..].to_vec(),
    }
}

