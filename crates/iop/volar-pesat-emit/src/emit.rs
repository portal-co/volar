// @reliability: experimental
// @ai: assisted
//! Step-circuit → PESAT emission. One `PesatIndex` per movfuscated step block
//! (the constraint set is the step relation); the per-step `PesatWitness` comes
//! from [`crate::eval_step_wires`]. Every bit wire is a GF(2) bit embedded in
//! `Galois128` with an explicit booleanity constraint (`v·v + v = 0`).
//!
//! ## Storage boundary (plan §3.3, E3)
//!
//! A step that touches memory carries a **memory accumulator** across the
//! chain boundary instead of the whole memory image: `mem_prod` absorbs every
//! write, `mem_cons` every read, via the additive multiset-hash shape
//! `acc' = acc + r0 + addr·r1 + value·r2 + ts·r3` (the
//! `volar_spec::vole::bridge::mem_acc_absorb` shape). `addr`/`value` pack their
//! bit-wires into a field element with public powers of two, so the absorption
//! is a **degree-1** PESAT constraint; the accumulator variables are
//! field-valued (no booleanity). The `(mem_prod, mem_cons)` pair joins the
//! `(pc, state)` linking boundary.
//!
//! **Honest soundness note** (carried from the storage_loop weaver): this is
//! the *carry* mechanism. Full read-consistency additionally needs a distinct
//! last-write timestamp per access and a final drain of live cells; here `ts`
//! is one per-step counter (the step index), matching the storage_loop
//! simplification. That bookkeeping layers on top without changing the chain's
//! shape.

use volar_ir::ir::{IRBlock, IRVarId};
use volar_ir_common::{Constant, Stmt, TypeId, TypeTable as IRTypes};
use volar_iop::transcript::{IopTranscript, Sponge, SpongeTranscript};
use volar_iop::warp::{PesatConstraint, PesatIndex, PesatTerm};
use volar_primitives::Galois128;

use crate::eval::{const_bits, stmt_out_width, width_of};

/// Emission failure: the step block used an op outside this emitter's scope.
#[derive(Debug)]
pub enum EmitError {
    /// A statement op outside the emitter's scope (oracle, action, rng).
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
            Self::Arity { expected, got } => {
                write!(f, "arity mismatch: expected {expected}, got {got}")
            }
            Self::UndefinedVar(v) => write!(f, "undefined var {}", v.0),
        }
    }
}

impl std::error::Error for EmitError {}

/// The result of emitting one step block: the PESAT index plus the variable
/// layout needed to build a matching witness and link the chain.
#[derive(Debug)]
pub struct StepEmission {
    /// The step relation as a PESAT instance (padded to a power-of-two
    /// constraint count with always-satisfied empty constraints).
    pub index: PesatIndex<Galois128>,
    /// Number of public variables (the step's `(pc, state)` boundary-in, plus
    /// the memory accumulators and timestamp when storage is present).
    pub public_len: usize,
    /// Number of witness variables (the block's statement-output bits, plus
    /// the accumulator chain and boundary-out).
    pub witness_len: usize,
    /// `var_base[v]` = the first PESAT variable index of IR var `v`'s bits.
    pub var_base: Vec<usize>,
    /// Bit width of each IR var (parallel to `var_base`).
    pub var_width: Vec<usize>,
    /// The linked boundary: public (boundary-in) PESAT indices.
    pub boundary_in: Vec<usize>,
    /// The linked boundary: witness (boundary-out) PESAT indices (absolute,
    /// into `x || w`).
    pub boundary_out: Vec<usize>,
    /// The per-step timestamp public variable (present iff storage is used).
    pub ts_public: Option<usize>,
    /// Whether the step block contains any storage op.
    pub has_storage: bool,
}

const ZERO: Galois128 = Galois128(0);
const ONE: Galois128 = Galois128(1);

/// `2^j` in `Galois128` (the field is char 2; `2 = x`).
fn pow2(j: usize) -> Galois128 {
    let mut acc = ONE;
    let two = Galois128(2);
    for _ in 0..j {
        acc = acc * two;
    }
    acc
}

/// Derive the four memory-accumulator challenges `(r0, r1, r2, r3)`.
///
/// **Fiat–Shamir binding (H5)**: the challenges are squeezed from a transcript
/// that has absorbed the step block's canonical (Debug) serialization — the
/// *program digest*. This replaces the E3 splitmix placeholder, binding the
/// memory accumulator to the program so a different step circuit yields
/// different challenges. The hash backend is generic (H1's `Sponge`); the
/// default is SHAKE128, and Ascon-XOF is the cheap in-circuit option for the
/// recursive verifier (see `docs/hash-cipher-generic-plan.md`).
pub fn derive_challenges(block: &IRBlock<()>) -> [Galois128; 4] {
    let mut t = IopTranscript::new().domain_sep(b"volar-pesat-emit-mem-acc-v1");
    t.absorb_len_prefixed(format!("{:?}", block).as_bytes());
    squeeze_four(&mut t)
}

/// [`derive_challenges`] with an explicit hash backend `S` (which the recursive
/// verifier must use in-circuit, matching the native prover's choice).
pub fn derive_challenges_with<S: Sponge>(block: &IRBlock<()>) -> [Galois128; 4] {
    let mut t = SpongeTranscript::<S>::new().domain_sep(b"volar-pesat-emit-mem-acc-v1");
    t.absorb_len_prefixed(format!("{:?}", block).as_bytes());
    squeeze_four(&mut t)
}

/// Squeeze four `Galois128` challenges (4 × 16 bytes, little-endian).
fn squeeze_four<S: Sponge>(t: &mut SpongeTranscript<S>) -> [Galois128; 4] {
    let bytes = t.squeeze_bytes(64);
    let mut out = [Galois128(0); 4];
    for i in 0..4 {
        out[i] = Galois128(u128::from_le_bytes(
            bytes[i * 16..i * 16 + 16].try_into().expect("16 bytes"),
        ));
    }
    out
}

/// A constraint-under-construction: a sum of `coeff · ∏ vars` terms.
struct ConstraintBuilder {
    terms: Vec<PesatTerm<Galois128>>,
}

impl ConstraintBuilder {
    fn new() -> Self {
        Self { terms: Vec::new() }
    }
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

/// Emit the PESAT index for a movfuscated step block.
pub fn emit_step_index(
    block: &IRBlock<()>,
    types: &IRTypes,
) -> Result<StepEmission, EmitError> {
    let n_params = block.params.len();
    let has_storage = block
        .stmts
        .iter()
        .any(|n| matches!(n.kind, Stmt::StorageRead { .. } | Stmt::StorageWrite { .. }));

    // ---- Layout -----------------------------------------------------------
    // Public: param bit vars, then (when storage) the field vars
    // [mem_prod_in, mem_cons_in, ts]. Witness: statement-output bits, then
    // (when storage) the accumulator chain vars and [mem_prod_out,
    // mem_cons_out].
    let mut var_base = Vec::with_capacity(n_params + block.stmts.len());
    let mut var_width = Vec::with_capacity(n_params + block.stmts.len());
    let mut next = 0usize;
    for param in &block.params {
        let w = width_of(*param, types)?;
        var_base.push(next);
        var_width.push(w);
        next += w;
    }
    let param_bits = next;
    let (mem_prod_in, mem_cons_in, ts_public) = if has_storage {
        let (mpi, mci, ts) = (next, next + 1, next + 2);
        next += 3;
        (mpi, mci, Some(ts))
    } else {
        (0, 0, None)
    };
    let public_len = next;

    // Statement outputs (witness bit vars).
    let mut stmt_bit_count = 0usize;
    for node in &block.stmts {
        let w = stmt_out_width(&node.kind, types)?;
        var_base.push(next);
        var_width.push(w);
        next += w;
        stmt_bit_count += w;
    }
    let stmt_end = next;

    // Witness field vars for the accumulator chain (when storage): one per
    // storage access (in statement order) plus the two boundary-out vars.
    let n_accesses = block
        .stmts
        .iter()
        .filter(|n| matches!(n.kind, Stmt::StorageRead { .. } | Stmt::StorageWrite { .. }))
        .count();
    let acc_chain_base = stmt_end;
    let (mem_prod_out, mem_cons_out) = if has_storage {
        let (mpo, mco) = (acc_chain_base + n_accesses, acc_chain_base + n_accesses + 1);
        next = mco + 1;
        (mpo, mco)
    } else {
        (0, 0)
    };
    // The witness region pads to a power of two: the WARP linear code
    // (IdentityCode: message_len == codeword_len == witness_len) requires a
    // power-of-two witness length. The padding variables are unconstrained
    // and always zero in the honest witness.
    let witness_len = (next - public_len).next_power_of_two().max(1);

    let map = VarMap { var_base, var_width };

    // ---- Constraints ------------------------------------------------------
    let mut constraints: Vec<PesatConstraint<Galois128>> = Vec::new();

    // Booleanity: every BIT wire is a bit — `v·v + v = 0`. The field-valued
    // accumulator / timestamp vars are exempt (they range over the whole
    // field, not {0,1}).
    let field_vars = |v: usize| -> bool {
        has_storage
            && (v == mem_prod_in
                || v == mem_cons_in
                || Some(v) == ts_public
                || (acc_chain_base..=mem_cons_out).contains(&v))
    };
    for v in 0..param_bits {
        if field_vars(v) {
            continue;
        }
        let mut c = ConstraintBuilder::new();
        c.term(ONE, vec![v, v]);
        c.term(ONE, vec![v]);
        constraints.push(c.build());
    }
    for v in public_len..stmt_end {
        let mut c = ConstraintBuilder::new();
        c.term(ONE, vec![v, v]);
        c.term(ONE, vec![v]);
        constraints.push(c.build());
    }

    // Relational constraints per statement; storage ops absorb into the
    // accumulator chain.
    let [r0, r1, r2, r3] = if has_storage {
        derive_challenges(block)
    } else {
        [ZERO, ZERO, ZERO, ZERO]
    };
    let mut cur_prod = mem_prod_in;
    let mut cur_cons = mem_cons_in;
    let mut access_ordinal = 0usize;
    for (i, node) in block.stmts.iter().enumerate() {
        let rv = IRVarId((n_params + i) as u32);
        match &node.kind {
            Stmt::StorageWrite {
                src, ty, addr, ..
            } => {
                let new_acc = acc_chain_base + access_ordinal;
                access_ordinal += 1;
                let ts_idx = ts_public.expect("storage implies ts");
                let value_bits = map.width(*src)?;
                let addr_bits = map.width(*addr)?;
                let mut c = ConstraintBuilder::new();
                // new_acc + cur_prod + r0 + r1·addr + r2·value + r3·ts = 0
                c.term(ONE, vec![new_acc]);
                c.term(ONE, vec![cur_prod]);
                c.term(r0, vec![]);
                for j in 0..addr_bits {
                    c.term(r1 * pow2(j), vec![map.idx(*addr, j)?]);
                }
                for j in 0..value_bits.min(width_of(*ty, types)?) {
                    c.term(r2 * pow2(j), vec![map.idx(*src, j)?]);
                }
                c.term(r3, vec![ts_idx]);
                constraints.push(c.build());
                cur_prod = new_acc;
            }
            Stmt::StorageRead { ty, addr, .. } => {
                let new_acc = acc_chain_base + access_ordinal;
                access_ordinal += 1;
                let ts_idx = ts_public.expect("storage implies ts");
                let addr_bits = map.width(*addr)?;
                let read_bits = map.width(rv)?; // the read result wire
                let mut c = ConstraintBuilder::new();
                // new_acc + cur_cons + r0 + r1·addr + r2·read + r3·ts = 0
                c.term(ONE, vec![new_acc]);
                c.term(ONE, vec![cur_cons]);
                c.term(r0, vec![]);
                for j in 0..addr_bits {
                    c.term(r1 * pow2(j), vec![map.idx(*addr, j)?]);
                }
                for j in 0..read_bits.min(width_of(*ty, types)?) {
                    c.term(r2 * pow2(j), vec![map.idx(rv, j)?]);
                }
                c.term(r3, vec![ts_idx]);
                constraints.push(c.build());
                cur_cons = new_acc;
            }
            other => emit_stmt_constraints(other, rv, &map, types, &mut constraints)?,
        }
    }

    if has_storage {
        // Boundary-out accumulators: equal to the last access's chain value
        // (or the boundary-in value when the step has no writes/reads).
        let mut eq = |out: usize, prev: usize| {
            let mut c = ConstraintBuilder::new();
            c.term(ONE, vec![out]);
            c.term(ONE, vec![prev]);
            constraints.push(c.build());
        };
        eq(mem_prod_out, cur_prod);
        eq(mem_cons_out, cur_cons);
    }

    // ---- Boundary ---------------------------------------------------------
    // boundary-in: params (bit vars) + the two accumulator ins (linked); the
    // timestamp is a per-step public input, not linked.
    let mut boundary_in: Vec<usize> = (0..param_bits).collect();
    if has_storage {
        boundary_in.push(mem_prod_in);
        boundary_in.push(mem_cons_in);
    }
    // boundary-out: the (pc', state') return args (positions 1..=n_params,
    // skipping the done flag), then the two accumulator outs.
    let ret_args: &[IRVarId] = match &block.terminator {
        volar_ir::ir::IRTerminator::Jmp { target }
            if target.dest == volar_ir::ir::IRBlockTargetId::Return =>
        {
            &target.args
        }
        other => panic!("step block must return; got {other:?}"),
    };
    let mut boundary_out: Vec<usize> = Vec::new();
    for arg in ret_args.iter().skip(1).take(n_params) {
        let base = map.var_base[arg.0 as usize];
        let w = map.var_width[arg.0 as usize];
        boundary_out.extend(base..base + w);
    }
    if has_storage {
        boundary_out.push(mem_prod_out);
        boundary_out.push(mem_cons_out);
    }
    assert_eq!(
        boundary_in.len(),
        boundary_out.len(),
        "boundary-in and boundary-out widths must match"
    );

    // ---- Pad + validate shape ---------------------------------------------
    let target = constraints.len().next_power_of_two().max(1);
    while constraints.len() < target {
        constraints.push(PesatConstraint::default());
    }
    let degree_bound = constraints
        .iter()
        .map(|c| c.terms.iter().map(|t| t.vars.len()).max().unwrap_or(0))
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
        boundary_in,
        boundary_out,
        ts_public,
        has_storage,
    })
}

/// Emit the relational constraints for one non-storage statement whose result
/// var is `rv`.
fn emit_stmt_constraints(
    stmt: &Stmt<IRVarId>,
    rv: IRVarId,
    map: &VarMap,
    types: &IRTypes,
    constraints: &mut Vec<PesatConstraint<Galois128>>,
) -> Result<(), EmitError> {
    let out_w = map.width(rv)?;
    let mut eq = |bit: usize,
                  build: &mut dyn FnMut(&mut ConstraintBuilder)|
     -> Result<(), EmitError> {
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

/// The emitted instance flattened to flat arrays, for driving the
/// const-generic in-circuit satisfaction checker (E5). The const-generic
/// `WarpIndex<M, TERMS, DEG>` is built in-circuit from these arrays.
#[derive(Clone, Debug)]
pub struct FlattenedIndex {
    /// Constraint count (power of two).
    pub m: usize,
    /// Term capacity per constraint (max terms over all constraints).
    pub terms: usize,
    /// Variable capacity per term (the degree bound).
    pub deg: usize,
    /// `coeffs[c*terms + t]` = the coefficient of term `t` in constraint `c`.
    pub coeffs: Vec<Galois128>,
    /// `n_vars[c*terms + t]` = that term's variable count.
    pub n_vars: Vec<u32>,
    /// `vars[(c*terms + t)*deg + j]` = that term's j-th variable index.
    pub vars: Vec<usize>,
    /// `n_terms[c]` = constraint `c`'s term count.
    pub n_terms: Vec<u32>,
    /// Public variable count.
    pub public_len: usize,
    /// Witness variable count.
    pub witness_len: usize,
}

/// Flatten an emitted instance for the const-generic in-circuit driver.
pub fn flatten_index(emission: &StepEmission) -> FlattenedIndex {
    let index = &emission.index;
    let m = index.constraints.len();
    let terms = index
        .constraints
        .iter()
        .map(|c| c.terms.len())
        .max()
        .unwrap_or(0)
        .max(1);
    let deg = index.degree_bound.max(1);
    let mut coeffs = Vec::with_capacity(m * terms);
    let mut n_vars = Vec::with_capacity(m * terms);
    let mut vars = Vec::with_capacity(m * terms * deg);
    let mut n_terms = Vec::with_capacity(m);
    for constraint in &index.constraints {
        n_terms.push(constraint.terms.len() as u32);
        for t in 0..terms {
            match constraint.terms.get(t) {
                Some(term) => {
                    coeffs.push(term.coeff);
                    n_vars.push(term.vars.len() as u32);
                    for j in 0..deg {
                        vars.push(term.vars.get(j).copied().unwrap_or(0));
                    }
                }
                None => {
                    coeffs.push(ZERO);
                    n_vars.push(0);
                    for _ in 0..deg {
                        vars.push(0);
                    }
                }
            }
        }
    }
    FlattenedIndex {
        m,
        terms,
        deg,
        coeffs,
        n_vars,
        vars,
        n_terms,
        public_len: index.public_len,
        witness_len: index.witness_len,
    }
}

/// Build the witness for one step from its param inputs, the full wire trace
/// (from [`crate::eval_step_wires`]), and the per-step memory-accumulator /
/// timestamp field values (computed by the caller threading the chain).
pub fn build_witness(
    emission: &StepEmission,
    wires: &[Vec<bool>],
    mem_prod_in: Galois128,
    mem_cons_in: Galois128,
    ts: Galois128,
    acc_chain: &[Galois128],
    mem_prod_out: Galois128,
    mem_cons_out: Galois128,
) -> volar_iop::warp::PesatWitness<Galois128> {
    let mut public: Vec<Galois128> = Vec::with_capacity(emission.public_len);
    let mut witness: Vec<Galois128> = Vec::with_capacity(emission.witness_len);
    // Public: param bits (from the wire trace's param region) then the field
    // vars (when storage).
    let mut wire_bits: Vec<bool> = Vec::new();
    for b in wires {
        wire_bits.extend_from_slice(b);
    }
    // The wire trace covers params + stmt outputs; the param bits are the
    // public prefix.
    let n_field_public = if emission.has_storage { 3 } else { 0 };
    let param_bits = emission.public_len - n_field_public;
    for j in 0..param_bits {
        public.push(crate::bit_to_field(wire_bits[j]));
    }
    if emission.has_storage {
        public.push(mem_prod_in);
        public.push(mem_cons_in);
        public.push(ts);
    }
    // Witness: stmt-output bits, then the accumulator chain vars, then the
    // two boundary-out accumulators.
    for j in param_bits..wire_bits.len() {
        witness.push(crate::bit_to_field(wire_bits[j]));
    }
    if emission.has_storage {
        witness.extend_from_slice(acc_chain);
        witness.push(mem_prod_out);
        witness.push(mem_cons_out);
    }
    // Pad the witness region to the (power-of-two) witness length with zeros.
    while witness.len() < emission.witness_len {
        witness.push(ZERO);
    }
    volar_iop::warp::PesatWitness { public, witness }
}
