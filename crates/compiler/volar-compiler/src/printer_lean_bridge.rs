//! Lean *bridge* emitter: for each spec construct covered by the native
//! Lean printer (`printer_lean`), emit the transport definitions and the
//! Aeneas-extraction-equivalence theorems connecting the two.
//!
//! The output is a Lean 4 file fragment (the consumer — voof's
//! `scripts/prepare-native.sh` — prepends the import block for its package
//! layout) containing, per covered spec item:
//!
//! - for structs whose fields are all shim `array::Array<u8, N>`: a
//!   transport `<Struct>ToNative (n : Nat) (x : volar_spec.<...>.<S> N) :
//!   <S> n` mapping the Aeneas extraction to the native emission;
//! - for inherent methods whose bodies match a recognized pattern, the
//!   commutation theorem `transport ∘ extracted = native ∘ transport`,
//!   with a fully-generated proof:
//!   * `from_fn` pointwise-xor constructors (e.g. `Garble.share`)
//!     returning a struct;
//!   * `from_fn` pointwise-xor methods returning a bare array (e.g.
//!     `Eval.open`);
//!   * `Default::default` constructors (e.g. `Garble.zero`);
//! - everything else is listed in a trailing `-- NOT-BRIDGED` comment so
//!   coverage gaps are visible in the generated artifact.
//!
//! The proofs reference the curated support lemmas in voof's
//! `VolarSpecGarble.Verification.ProofObligations` (shim specs: `vecL`,
//! `from_fn_spec`, `array_default_spec`, `xorBytes`, `spec_ok_elim`) and
//! `VolarSpecBridge.Prelude` (scalar transports: `u8ToUInt8`, ...). Proof
//! scripts are fixed templates instantiated with the extracted names; if
//! the extraction's shape changes, the theorems fail to compile — loudly.
//!
//! Aeneas naming conventions relied upon (hax-lean backend):
//! - `volar_spec::<mod path>::<item>` ↦ `volar_spec.<module path>.<item>`;
//! - inherent method `S::m` ↦ `volar_spec...<S>.<m>`, or
//!   `...<S>.impl.<m>` when `m` clashes with a field name of `S`;
//! - a `from_fn` closure for `<S>.<m>` ↦
//!   `...<S>.<m>.closure.Insts.CoreOpsFunctionFnMutTupleUsizeU8.call_mut`
//!   (the `TupleUsizeU8` mangling is specific to `(usize,) → u8` closures;
//!   garble's are all of that shape).

use std::collections::BTreeSet;
use std::fmt::Write as _;

use crate::ir::{
    IrExpr, IrExprKind, IrFunction, IrImpl, IrModule, IrStruct, IrType, PrimitiveType, SpecBinOp,
    StructKind,
};
use crate::printer_lean::ident;
use crate::CompilerError;

#[cfg(not(feature = "std"))]
use alloc::{
    format,
    string::{String, ToString},
    vec::Vec,
};
#[cfg(feature = "std")]
use std::{format, string::String, string::ToString, vec, vec::Vec};

type Result<T> = std::result::Result<T, CompilerError>;

/// Emit nothing; the caller records a NOT-BRIDGED comment.
fn skip(what: &str) -> Result<Option<String>> {
    Ok(Some(what.to_string()))
}

macro_rules! w {
    ($out:expr, $($arg:tt)*) => { let _ = write!($out, $($arg)*); };
}

fn kind_name_of(k: &StructKind) -> Option<&str> {
    match k {
        StructKind::Custom(n) => Some(n.as_str()),
        _ => None,
    }
}

fn struct_name(s: &IrStruct) -> String {
    kind_name_of(&s.kind).unwrap_or("<unknown>").to_string()
}

/// The Aeneas-extracted name of a spec item: `volar_spec.<mod>.<name>`.
///
/// The IR does not record which Rust submodule an item came from
/// (`module_path` is crate-level), so the caller supplies the module
/// segment explicitly (`aeneas_module`, e.g. "garble").
fn aeneas_path(module: &IrModule<IrFunction>, module_path: &[String], name: &str) -> String {
    let mut s = module.name.clone();
    for p in module_path {
        s.push('.');
        s.push_str(p);
    }
    s.push('.');
    s.push_str(name);
    s
}

/// The Aeneas module segment this bridge targets (the Rust submodule that
/// the hax scenario extracts, e.g. "garble").
#[derive(Clone, Copy)]
pub struct AeneasModule<'a>(pub &'a str);

impl<'a> AeneasModule<'a> {
    fn path(&self, module: &IrModule<IrFunction>, name: &str) -> String {
        format!("{}.{}.{}", module.name, self.0, name)
    }
}

/// The Aeneas-extracted name of an inherent method: `impl.` is inserted when
/// the method name clashes with a field of the struct.
fn aeneas_method(
    amod: &AeneasModule,
    module: &IrModule<IrFunction>,
    self_ty: &str,
    name: &str,
    fields: &[String],
) -> String {
    let base = amod.path(module, self_ty);
    if fields.iter().any(|f| f == name) {
        format!("{base}.impl.{name}")
    } else {
        format!("{base}.{name}")
    }
}

/// The native (printer_lean) name of a method inside its struct namespace.
fn native_method(name: &str, fields: &[String]) -> String {
    if fields.iter().any(|f| f == name) {
        ident(&format!("{name}_m"))
    } else {
        ident(name)
    }
}

/// One side of a pointwise binary op: where the operand lives.
#[derive(Clone)]
enum Operand {
    /// `self.<field>` — a field of the receiver struct.
    SelfField(String),
    /// `<param>` — a bare shim-array parameter.
    BareArray(String),
    /// `<param>.<field>` — a field of a struct parameter.
    StructField(String, String),
}

impl Operand {
    /// The extracted list-of-bytes expression.
    fn list(&self) -> String {
        match self {
            Operand::SelfField(f) => format!("(vecL self.{f}.data)"),
            Operand::BareArray(p) => format!("(vecL {p}.data)"),
            Operand::StructField(p, f) => format!("(vecL {p}.{f}.data)"),
        }
    }

    /// The root variable (the captured closure value component).
    fn root(&self) -> String {
        match self {
            Operand::SelfField(_) => "self".to_string(),
            Operand::BareArray(p) => p.clone(),
            Operand::StructField(p, _) => p.clone(),
        }
    }

    /// The coercion form the index-chain simp reduces to.
    fn val_list(&self) -> String {
        match self {
            Operand::SelfField(f) => format!("(self.{f}.data.val)"),
            Operand::BareArray(p) => format!("({p}.data.val)"),
            Operand::StructField(p, f) => format!("({p}.{f}.data.val)"),
        }
    }
}

struct Pointwise {
    #[allow(dead_code)] // v1 supports BitXor only; kept for the check
    op: SpecBinOp,
    index_var: String,
    left: Operand,
    right: Operand,
}

/// Recognized method-body patterns.
enum MethodPattern {
    /// `Ret { rf: Array::from_fn(|i| lhs[i] ^ rhs[i]) }` — e.g. `share`.
    PointwiseCtor { ret: String, rfield: String, pw: Pointwise },
    /// `Array::from_fn(|i| lhs[i] ^ rhs[i])` returned bare — e.g. `Eval.open`.
    PointwiseArray { pw: Pointwise },
    /// `Ret { rf: Array::from_fn(|i| if c { lhs[i] ^ rhs[i] } else { rhs[i] }) }`
    /// — e.g. `GlobalSecret.encode`.
    PointwiseCondCtor { ret: String, rfield: String, cond: String, pw: Pointwise },
    /// `S { f: Default::default(), .. }` — e.g. `Garble.zero`.
    DefaultCtor,
    /// `let mut d = D::new(); d.update(&e1); …; let hash = d.finalize();`
    /// `Ret { rf: from_fn(|i| hash[i]) }` — e.g. `Garble.and_result`.
    DigestCtor {
        ret: String,
        rfield: String,
        /// the update operands, in order: (root, field) for `&x.f` args
        updates: Vec<(String, String)>,
    },
    /// `self.f.clone()` returned bare — e.g. `GlobalSecret.secret`.
    CloneField { field: String },
    /// `p[0] |= 1; Self { f: p }` — mutate-index-zero-then-construct, e.g.
    /// `GlobalSecret.new`.
    MutateOrZeroCtor { field: String, param: String },
    /// `let index = (if s.f[0]&1==1 {1} else {0}) | (if o.g[0]&1==1 {2} else {0});`
    /// `let hash = { let mut d = D::new(); d.update(&x.f); …; d.finalize() };`
    /// `Ret { rf: from_fn(|i| hash[i] ^ t.table[index][i]) }` — e.g.
    /// `Eval.and_via_table`.
    TableDigestCtor {
        ret: String,
        rfield: String,
        /// the two bit-tested operands, in weight order (weight 1, then 2):
        /// (root, field)
        tests: [(String, String); 2],
        /// the update operands, in order: (root, field) for `&x.f` args
        updates: Vec<(String, String)>,
        /// the table parameter and its row field: (param, field)
        table: (String, String),
    },
}

fn as_operand(e: &IrExpr, iv: &str) -> Option<Operand> {
    if let IrExprKind::Index { base, index } = &e.kind {
        if !matches!(&index.kind, IrExprKind::Var(v) if v == iv) {
            return None;
        }
        return match &base.kind {
            IrExprKind::Field { base, field } => match &base.kind {
                IrExprKind::Var(v) if v == "self" => Some(Operand::SelfField(field.clone())),
                IrExprKind::Var(v) => Some(Operand::StructField(v.clone(), field.clone())),
                _ => None,
            },
            IrExprKind::Var(v) => Some(Operand::BareArray(v.clone())),
            _ => None,
        };
    }
    None
}

fn as_pointwise(e: &IrExpr) -> Option<Pointwise> {
    if let IrExprKind::ArrayGenerate { elem_ty, index_var, body, .. } = &e.kind {
        if !matches!(elem_ty.as_deref(), Some(IrType::Primitive(PrimitiveType::U8))) {
            return None;
        }
        if let IrExprKind::Binary { op: SpecBinOp::BitXor, left, right } = &body.kind {
            return Some(Pointwise {
                op: SpecBinOp::BitXor,
                index_var: index_var.clone(),
                left: as_operand(left, index_var)?,
                right: as_operand(right, index_var)?,
            });
        }
    }
    None
}

/// `let mut d = D::new(); d.update(&x.f); …; let hash = d.finalize();
/// Ret { rf: from_fn(|i| hash[i]) }` — the digest-accumulator shape.
fn as_digest_ctor(f: &IrFunction) -> Option<MethodPattern> {
    let stmts = &f.body.stmts;
    if stmts.len() < 3 {
        return None;
    }
    // first: let mut d := D::new()
    let crate::ir::IrStmtKind::Let { pattern, init: Some(init), .. } = &stmts[0].kind else {
        return None;
    };
    let crate::ir::IrPattern::Ident { name: dvar, .. } = pattern else {
        return None;
    };
    let IrExprKind::Call { func, args } = &init.kind else {
        return None;
    };
    if !args.is_empty() {
        return None;
    }
    if !matches!(&func.kind, IrExprKind::Path { segments, .. }
        if segments.len() == 2 && segments[1] == "new")
    {
        return None;
    }
    // middle: Semi(d.update(&x.f))*
    let mut updates = Vec::new();
    let mut i = 1;
    while i + 1 < stmts.len() {
        let crate::ir::IrStmtKind::Semi(inner) = &stmts[i].kind else {
            return None;
        };
        let IrExprKind::MethodCall { receiver, method, args, .. } = &inner.kind else {
            return None;
        };
        if !matches!(method, crate::ir::MethodKind::Known(crate::ir::StdMethod::Update)) {
            return None;
        }
        if !matches!(&receiver.kind, IrExprKind::Var(v) if v == dvar) {
            return None;
        }
        if args.len() != 1 {
            return None;
        }
        let IrExprKind::Unary { op: crate::ir::SpecUnaryOp::Ref, expr } = &args[0].kind else {
            return None;
        };
        let IrExprKind::Field { base, field } = &expr.kind else {
            return None;
        };
        let IrExprKind::Var(root) = &base.kind else {
            return None;
        };
        updates.push((root.clone(), field.clone()));
        i += 1;
    }
    // last stmt: let hash := d.finalize()
    let crate::ir::IrStmtKind::Let { pattern, init: Some(init), .. } = &stmts[stmts.len() - 1].kind
    else {
        return None;
    };
    let crate::ir::IrPattern::Ident { name: hvar, .. } = pattern else {
        return None;
    };
    let IrExprKind::MethodCall { receiver, method, args, .. } = &init.kind else {
        return None;
    };
    if !matches!(method, crate::ir::MethodKind::Known(crate::ir::StdMethod::Finalize)) {
        return None;
    }
    if !matches!(&receiver.kind, IrExprKind::Var(v) if v == dvar) || !args.is_empty() {
        return None;
    }
    // tail: Ret { rf := from_fn(|i| hash[i]) }
    let tail = f.body.expr.as_ref()?;
    let IrExprKind::StructExpr { kind, fields, rest: None, .. } = &tail.kind else {
        return None;
    };
    if fields.len() != 1 {
        return None;
    }
    let IrExprKind::ArrayGenerate { index_var, body, .. } = &fields[0].1.kind else {
        return None;
    };
    let IrExprKind::Index { base, index } = &body.kind else {
        return None;
    };
    if !matches!(&base.kind, IrExprKind::Var(v) if v == hvar)
        || !matches!(&index.kind, IrExprKind::Var(v) if v == index_var)
    {
        return None;
    }
    Some(MethodPattern::DigestCtor {
        ret: kind_name_of(kind)?.to_string(),
        rfield: fields[0].0.clone(),
        updates,
    })
}

/// `if c { lhs[i] ^ rhs[i] } else { rhs[i] }` — the encode shape.
fn as_pointwise_cond(e: &IrExpr) -> Option<(String, Pointwise)> {
    if let IrExprKind::ArrayGenerate { elem_ty, index_var, body, .. } = &e.kind {
        if !matches!(elem_ty.as_deref(), Some(IrType::Primitive(PrimitiveType::U8))) {
            return None;
        }
        let body = match &body.kind {
            IrExprKind::Block(b) if b.stmts.is_empty() => b.expr.as_ref()?,
            _ => body,
        };
        if let IrExprKind::If { cond, then_branch, else_branch } = &body.kind {
            let cvar = match &cond.kind {
                IrExprKind::Var(v) => v.clone(),
                _ => return None,
            };
            let (then_e, else_e) = (then_branch.expr.as_ref()?, else_branch.as_ref()?);
            let else_e = match &else_e.kind {
                IrExprKind::Block(b) if b.stmts.is_empty() => b.expr.as_ref()?,
                _ => else_e,
            };
            if let IrExprKind::Binary { op: SpecBinOp::BitXor, left, right } = &then_e.kind {
                let left = as_operand(left, index_var)?;
                let right = as_operand(right, index_var)?;
                // else branch must be exactly the right operand
                let else_op = as_operand(else_e, index_var)?;
                if format!("{:?}", else_op.list()) != format!("{:?}", right.list()) {
                    return None;
                }
                return Some((cvar, Pointwise {
                    op: SpecBinOp::BitXor,
                    index_var: index_var.clone(),
                    left,
                    right,
                }));
            }
        }
    }
    None
}

/// `if x.f[0] & 1 == 1 { w } else { 0 }` — one bit-test arm of the
/// and_via_table index computation, with `w` the expected weight.
fn as_bit_test_if(e: &IrExpr, weight: i128) -> Option<(String, String)> {
    let IrExprKind::If { cond, then_branch, else_branch } = &e.kind else { return None };
    let IrExprKind::Binary { op: SpecBinOp::Eq, left: masked, right: one1 } = &cond.kind else {
        return None
    };
    if !matches!(&one1.kind, IrExprKind::Lit(crate::ir::IrLit::Int(1))) {
        return None;
    }
    let IrExprKind::Binary { op: SpecBinOp::BitAnd, left: idx, right: one2 } = &masked.kind
    else {
        return None
    };
    if !matches!(&one2.kind, IrExprKind::Lit(crate::ir::IrLit::Int(1))) {
        return None;
    }
    let IrExprKind::Index { base, index } = &idx.kind else { return None };
    if !matches!(&index.kind, IrExprKind::Lit(crate::ir::IrLit::Int(0))) {
        return None;
    }
    let IrExprKind::Field { base, field } = &base.kind else { return None };
    let IrExprKind::Var(root) = &base.kind else { return None };
    let then_e = then_branch.expr.as_ref()?;
    if !matches!(&then_e.kind, IrExprKind::Lit(crate::ir::IrLit::Int(w)) if *w == weight) {
        return None;
    }
    let else_e = else_branch.as_ref()?;
    let else_e = match &else_e.kind {
        IrExprKind::Block(b) if b.stmts.is_empty() => b.expr.as_ref()?,
        _ => else_e,
    };
    if !matches!(&else_e.kind, IrExprKind::Lit(crate::ir::IrLit::Int(0))) {
        return None;
    }
    Some((root.clone(), field.clone()))
}

/// `let index = (bit-test or); let hash = { digest block };`
/// `Ret { rf: from_fn(|i| hash[i] ^ t.table[index][i]) }` — the
/// `Eval.and_via_table` shape.
fn as_table_digest_ctor(f: &IrFunction) -> Option<MethodPattern> {
    let stmts = &f.body.stmts;
    if stmts.len() != 2 {
        return None;
    }
    // stmt 0: let index = (if ... {1} else {0}) | (if ... {2} else {0})
    let crate::ir::IrStmtKind::Let { pattern, init: Some(init), .. } = &stmts[0].kind else {
        return None
    };
    let crate::ir::IrPattern::Ident { name: index_var, .. } = pattern else { return None };
    let IrExprKind::Binary { op: SpecBinOp::BitOr, left: if1, right: if2 } = &init.kind else {
        return None
    };
    let t1 = as_bit_test_if(if1, 1)?;
    let t2 = as_bit_test_if(if2, 2)?;
    // stmt 1: let hash = { let mut d = D::new(); d.update(&x.f)*; d.finalize() }
    let crate::ir::IrStmtKind::Let { pattern, init: Some(init), .. } = &stmts[1].kind else {
        return None
    };
    let crate::ir::IrPattern::Ident { name: hash_var, .. } = pattern else { return None };
    let IrExprKind::Block(blk) = &init.kind else { return None };
    let bstmts = &blk.stmts;
    if bstmts.is_empty() {
        return None;
    }
    let crate::ir::IrStmtKind::Let { pattern, init: Some(init), .. } = &bstmts[0].kind else {
        return None
    };
    let crate::ir::IrPattern::Ident { name: dvar, .. } = pattern else { return None };
    let IrExprKind::Call { func, args } = &init.kind else { return None };
    if !args.is_empty() {
        return None;
    }
    if !matches!(&func.kind, IrExprKind::Path { segments, .. }
        if segments.len() == 2 && segments[1] == "new")
    {
        return None;
    }
    let mut updates = Vec::new();
    for s in &bstmts[1..] {
        let crate::ir::IrStmtKind::Semi(inner) = &s.kind else { return None };
        let IrExprKind::MethodCall { receiver, method, args, .. } = &inner.kind else {
            return None
        };
        if !matches!(method, crate::ir::MethodKind::Known(crate::ir::StdMethod::Update)) {
            return None;
        }
        if !matches!(&receiver.kind, IrExprKind::Var(v) if v == dvar) {
            return None;
        }
        if args.len() != 1 {
            return None;
        }
        let IrExprKind::Unary { op: crate::ir::SpecUnaryOp::Ref, expr } = &args[0].kind else {
            return None
        };
        let IrExprKind::Field { base, field } = &expr.kind else { return None };
        let IrExprKind::Var(root) = &base.kind else { return None };
        updates.push((root.clone(), field.clone()));
    }
    let fin = blk.expr.as_ref()?;
    let IrExprKind::MethodCall { receiver, method, args, .. } = &fin.kind else { return None };
    if !matches!(method, crate::ir::MethodKind::Known(crate::ir::StdMethod::Finalize)) {
        return None;
    }
    if !matches!(&receiver.kind, IrExprKind::Var(v) if v == dvar) || !args.is_empty() {
        return None;
    }
    // tail: Ret { rf := from_fn(|i| hash[i] ^ table.tfield[index][i]) }
    let tail = f.body.expr.as_ref()?;
    let IrExprKind::StructExpr { kind, fields, rest: None, .. } = &tail.kind else { return None };
    if fields.len() != 1 {
        return None;
    }
    let IrExprKind::ArrayGenerate { index_var: iv, body, .. } = &fields[0].1.kind else {
        return None
    };
    let IrExprKind::Binary { op: SpecBinOp::BitXor, left, right } = &body.kind else {
        return None
    };
    let IrExprKind::Index { base: hb, index: hi } = &left.kind else { return None };
    if !matches!(&hb.kind, IrExprKind::Var(v) if v == hash_var) {
        return None;
    }
    if !matches!(&hi.kind, IrExprKind::Var(v) if v == iv) {
        return None;
    }
    let IrExprKind::Index { base: rowidx, index: ri } = &right.kind else { return None };
    if !matches!(&ri.kind, IrExprKind::Var(v) if v == iv) {
        return None;
    }
    let IrExprKind::Index { base: tfield_base, index: selidx } = &rowidx.kind else { return None };
    if !matches!(&selidx.kind, IrExprKind::Var(v) if v == index_var) {
        return None;
    }
    let IrExprKind::Field { base: tv_base, field: tfield } = &tfield_base.kind else { return None };
    let IrExprKind::Var(tv) = &tv_base.kind else { return None };
    Some(MethodPattern::TableDigestCtor {
        ret: kind_name_of(kind)?.to_string(),
        rfield: fields[0].0.clone(),
        tests: [t1, t2],
        updates,
        table: (tv.clone(), tfield.clone()),
    })
}

/// A shim-array-of-u8 field type (extracted side): `array.Array u8 N`.
fn is_shim_u8_array(ty: &IrType) -> bool {
    matches!(
        ty,
        IrType::Array { elem, .. } if matches!(elem.as_ref(), IrType::Primitive(PrimitiveType::U8))
    )
}

/// A fixed-size core array of shim u8 arrays (e.g. `GarbleTable.table`).
fn is_fixed_shim_u8_array(ty: &IrType) -> bool {
    matches!(
        ty,
        IrType::Array { kind: crate::ir::ArrayKind::FixedArray, elem, .. } if is_shim_u8_array(elem)
    )
}

/// `p[0] |= 1; Self { f: p }` — the GlobalSecret.new shape.
fn as_mutate_or_zero_ctor(f: &IrFunction) -> Option<MethodPattern> {
    if f.body.stmts.len() != 1 {
        return None;
    }
    let crate::ir::IrStmtKind::Semi(inner) = &f.body.stmts[0].kind else { return None };
    let IrExprKind::AssignOp { op: SpecBinOp::BitOr, left, right } = &inner.kind else {
        return None
    };
    if !matches!(&right.kind, IrExprKind::Lit(crate::ir::IrLit::Int(1))) {
        return None;
    }
    let IrExprKind::Index { base, index } = &left.kind else { return None };
    if !matches!(&index.kind, IrExprKind::Lit(crate::ir::IrLit::Int(0))) {
        return None;
    }
    let IrExprKind::Var(pv) = &base.kind else { return None };
    if pv == "self" {
        return None;
    }
    let tail = f.body.expr.as_ref()?;
    let IrExprKind::StructExpr { fields, rest: None, .. } = &tail.kind else { return None };
    if fields.len() != 1 || !matches!(&fields[0].1.kind, IrExprKind::Var(v) if v == pv) {
        return None;
    }
    Some(MethodPattern::MutateOrZeroCtor { field: fields[0].0.clone(), param: pv.clone() })
}

/// Classify a method body (a single tail expression, or a single
/// `Semi(Return(expr))` statement).
fn classify(f: &IrFunction) -> Option<MethodPattern> {
    if let Some(p) = as_mutate_or_zero_ctor(f) {
        return Some(p);
    }
    if let Some(p) = as_table_digest_ctor(f) {
        return Some(p);
    }
    if let Some(p) = as_digest_ctor(f) {
        return Some(p);
    }
    let e = if f.body.stmts.is_empty() {
        f.body.expr.as_ref()?
    } else if f.body.stmts.len() == 1 && f.body.expr.is_none() {
        match &f.body.stmts[0].kind {
            crate::ir::IrStmtKind::Semi(inner) => match &inner.kind {
                IrExprKind::Return(Some(e)) => e,
                _ => return None,
            },
            _ => return None,
        }
    } else {
        return None;
    };
    // `self.f.clone()` — the CloneField pattern
    if let IrExprKind::MethodCall { receiver, method, args, .. } = &e.kind {
        if matches!(method, crate::ir::MethodKind::Known(crate::ir::StdMethod::Clone))
            && args.is_empty()
        {
            if let IrExprKind::Field { base, field } = &receiver.kind {
                if matches!(&base.kind, IrExprKind::Var(v) if v == "self") {
                    return Some(MethodPattern::CloneField { field: field.clone() });
                }
            }
        }
    }
    match &e.kind {
        IrExprKind::StructExpr { kind, fields, rest, .. } => {
            let _ret = kind_name_of(kind)?;
            if rest.is_some() || fields.is_empty() {
                return None;
            }
            if fields.len() == 1 {
                if let Some(pw) = as_pointwise(&fields[0].1) {
                    return Some(MethodPattern::PointwiseCtor {
                        ret: _ret.to_string(),
                        rfield: fields[0].0.clone(),
                        pw,
                    });
                }
                if let Some((cond, pw)) = as_pointwise_cond(&fields[0].1) {
                    return Some(MethodPattern::PointwiseCondCtor {
                        ret: _ret.to_string(),
                        rfield: fields[0].0.clone(),
                        cond,
                        pw,
                    });
                }
            }
            if fields.iter().all(|(_, v)| matches!(&v.kind, IrExprKind::DefaultValue { ty } if matches!(ty, Some(t) if is_shim_u8_array(t)))) {
                return Some(MethodPattern::DefaultCtor);
            }
            None
        }
        _ => as_pointwise(e).map(|pw| MethodPattern::PointwiseArray { pw }),
    }
}

/// The single non-self parameter of a method, with its struct type if any.
fn sole_param(f: &IrFunction) -> Option<(String, Option<String>)> {
    if f.params.len() != 1 {
        return None;
    }
    let p = &f.params[0];
    let inner = match &p.ty {
        IrType::Reference { elem, .. } => elem.as_ref(),
        t => t,
    };
    let ty = match inner {
        IrType::Struct { kind, .. } => Some(kind_name_of(kind)?.to_string()),
        IrType::Array { elem, .. } if matches!(elem.as_ref(), IrType::Primitive(PrimitiveType::U8)) => None,
        _ => return None,
    };
    Some((p.name.clone(), ty))
}

/// Emit the closure-spec theorem for a pointwise from_fn closure.
#[allow(clippy::too_many_arguments)]
fn emit_closure_spec(
    out: &mut String,
    thm: &str,
    a_closure: &str,
    self_ty: &str,
    self_path: &str,
    param: &str,
    param_ty: Option<&str>, // struct type of the param, if any
    param_path: Option<&str>,
    pw: &Pointwise,
    capture_override: Option<String>,
) {
    let lhs = pw.left.list();
    let rhs = pw.right.list();
    let param_decl = match param_ty {
        Some(_) => format!("({param} : {} N)", param_path.unwrap()),
        None => format!("({param} : volar_spec.array.Array Std.U8 N)"),
    };
    // Aeneas captures closure values in first-use order in the body
    // (or by field path for value-receiver trait-impl closures).
    let (cap0, cap1) = {
        let (r0, r1) = (pw.left.root(), pw.right.root());
        if r0 == "self" { ("self".to_string(), param.to_string()) } else { (param.to_string(), "self".to_string()) }
    };
    let capture = capture_override.unwrap_or_else(|| format!("({cap0}, {cap1})"));
    w!(out, "\n/-- (generated) closure spec: the extracted `{a_closure}.call_mut` is the\n    pointwise xor of the two operands. -/\n");
    w!(out, "theorem {thm} {{N : Type}} (d : vole.VoleArray N Std.U8)\n");
    w!(out, "    (self : {self_path} N) {param_decl} (i : Std.Usize)\n");
    w!(out, "    (hs : i.val < {lhs}.length) (ha : i.val < {rhs}.length) :\n");
    w!(out, "    {a_closure}.call_mut d {capture} i =\n");
    w!(out, "      ok (xorBytes {lhs} {rhs} i.val, {capture}) := by\n");
    // primed hypotheses at the coercion form the index chain reduces to
    w!(out, "  have hs' : i.val < {}.length := hs\n", pw.left.val_list());
    w!(out, "  have ha' : i.val < {}.length := ha\n", pw.right.val_list());
    w!(out, "  simp only [{a_closure}.call_mut,\n");
    w!(out, "    array.Array.Insts.CoreOpsIndexIndexUsizeT.index,\n");
    w!(out, "    alloc.vec.Vec.Insts.CoreOpsIndexIndex.index,\n");
    w!(out, "    core.Slice.Insts.CoreOpsIndexIndex.index,\n");
    w!(out, "    alloc.vec.Vec.Insts.CoreOpsDerefDerefSlice.deref, alloc.vec.Vec.as_slice,\n");
    w!(out, "    rust_primitives.sequence.seq_to_slice,\n");
    w!(out, "    core.Usize.Insts.CoreSliceIndexSliceIndexSliceT.get,\n");
    w!(out, "    rust_primitives.slice.slice_length,\n");
    w!(out, "    rust_primitives.slice.slice_index, Slice.index_usize,\n");
    w!(out, "    UScalar.lt_equiv, Slice.len_val, Slice.length, Slice.getElem?_Usize_eq,\n");
    w!(out, "    eq_true hs', eq_true ha',\n");
    w!(out, "    decide_true, ↓reduceIte, getElem?_pos, spec_ok, bind_tc_ok, lift]\n");
    w!(out, "  simp [xorBytes, vecL, hs, ha, hs', ha']\n");
    w!(out, "  rfl\n");
    let _ = self_ty;
}

/// Emit the commutation theorem for a pointwise-constructor method
/// (`Ret { rf: from_fn(...) }`).
#[allow(clippy::too_many_arguments)]
fn emit_pointwise_ctor(
    out: &mut String,
    amod: &AeneasModule,
    module: &IrModule<IrFunction>,
    imp: &IrImpl,
    fields: &[String],
    f: &IrFunction,
    ret: &str,
    rfield: &str,
    pw: &Pointwise,
    bridged: &BTreeSet<String>,
) -> Result<Option<String>> {
    let self_ty = struct_name_of_impl(imp)?;
    let Some((param, param_ty)) = sole_param(f) else {
        return skip("params");
    };
    // one side must be a self field, the other the parameter
    let sfield = match (&pw.left, &pw.right) {
        (Operand::SelfField(_), Operand::BareArray(_) | Operand::StructField(_, _)) => (),
        (Operand::BareArray(_) | Operand::StructField(_, _), Operand::SelfField(_)) => (),
        _ => return skip("operands not self/parameter"),
    };
    let a_method = aeneas_method(amod, module, &self_ty, &f.name, fields);
    let a_closure = format!("{a_method}.closure.Insts.CoreOpsFunctionFnMutTupleUsizeU8");
    let a_self = amod.path(module, &self_ty);
    let n_method = native_method(&f.name, fields);

    let closure_thm = format!("{self_ty}_{}_closure_spec", f.name);
    let param_path = param_ty.as_deref().map(|pt| amod.path(module, pt));
    emit_closure_spec(
        out, &closure_thm, &a_closure, &self_ty, &a_self, &param, param_ty.as_deref(),
        param_path.as_deref(), pw, None,
    );

    let lhs = pw.left.list();
    let rhs = pw.right.list();
    // native transports
    let self_t = format!("({self_ty}ToNative m.val self)");
    let arg_op = if matches!(pw.left, Operand::SelfField(_)) { &pw.right } else { &pw.left };
    let arg_t = match arg_op {
        Operand::BareArray(p) => format!("(arrToNative {p})"),
        Operand::StructField(p, _) => {
            let pt = param_ty.as_deref().unwrap();
            if !bridged.contains(pt) {
                return skip("param struct not bridged");
            }
            format!("({pt}ToNative m.val {p})")
        }
        _ => return skip("bad right operand"),
    };
    w!(out, "\n/-- (generated) `{a_method}` commutes with the transport. -/\n");
    w!(out, "theorem {self_ty}_{}_commutes {{N : Type}} (d : vole.VoleArray N Std.U8)\n", f.name);
    w!(out, "    (m : Std.Usize)\n");
    w!(out, "    (hsize : d.arrayArraySizeInst.typenummarker_traitsUnsignedInst.USIZE = ok m)\n");
    w!(out, "    (self : {a_self} N) ({param} : {}) ", match param_ty {
        Some(_) => param_path.clone().unwrap() + " N",
        None => "volar_spec.array.Array Std.U8 N".to_string(),
    });
    w!(out, "\n    (hsl : {lhs}.length = m.val) (hal : {rhs}.length = m.val) :\n");
    w!(out, "    (do let r ← {a_method} d self {param}; ok ({ret}ToNative m.val r)) =\n");
    w!(out, "    ok ({self_ty}.{n_method} {self_t} {arg_t} : {ret} m.val) := by\n");
    let capture = default_capture(pw, &param);
    w!(out, "  have hspec : {a_method} d self {param}\n");
    w!(out, "      ⦃ r => vecL r.{rfield}.data = List.ofFn (fun j : Fin m.val =>\n");
    w!(out, "          xorBytes {lhs} {rhs} j.val) ⦄ := by\n");
    w!(out, "    simp only [{a_method}]\n");
    w!(out, "    apply spec_bind (from_fn_spec d.arrayArraySizeInst\n");
    w!(out, "      ({a_closure} d) m hsize {capture}\n");
    w!(out, "      (fun j => xorBytes {lhs} {rhs} j)\n");
    w!(out, "      (by\n");
    w!(out, "        intro i hi\n");
    w!(out, "        exact {closure_thm} d self {param} i (by rw [hsl]; exact hi) (by rw [hal]; exact hi)))\n");
    w!(out, "    intro a ha\n");
    w!(out, "    rw [spec_ok]\n");
    w!(out, "    exact ha\n");
    w!(out, "  obtain ⟨r0, hr, hpost⟩ := spec_ok_elim hspec\n");
    w!(out, "  simp only [Bind.bind, Aeneas.Std.bind]\n");
    w!(out, "  rw [hr]\n");
    w!(out, "  simp only []\n");
    w!(out, "  congr 1\n");
    let pt_unfold = match &pw.right {
        Operand::StructField(_, _) => format!(", {}ToNative", param_ty.as_deref().unwrap()),
        _ => String::new(),
    };
    w!(out, "  simp only [{ret}ToNative, {self_ty}ToNative{pt_unfold}, {self_ty}.{n_method}, arrToNative]\n");
    w!(out, "  congr 1\n");
    emit_pointwise_tail(out, &lhs, &rhs);
    Ok(None)
}

/// The pointwise list-equality tail shared by both commutation templates.
fn emit_pointwise_tail(out: &mut String, lhs: &str, rhs: &str) {
    w!(out, "  rw [hpost, List.map_ofFn]\n");
    w!(out, "  apply List.ext_getElem\n");
    w!(out, "  · simp\n");
    w!(out, "  · intro i h1 h2\n");
    w!(out, "    rw [List.getElem_ofFn, List.getElem_ofFn]\n");
    w!(out, "    have hi : i < m.val := by simpa using h1\n");
    w!(out, "    have hb : i < {lhs}.length := by rw [hsl]; exact hi\n");
    w!(out, "    have ht : i < {rhs}.length := by rw [hal]; exact hi\n");
    w!(out, "    simp only [Function.comp_apply, xorBytes]\n");
    w!(out, "    rw [dif_pos ⟨hb, ht⟩]\n");
    w!(out, "    rw [List.getElem!_eq_getElem?_getD, List.getElem!_eq_getElem?_getD]\n");
    w!(out, "    rw [List.getElem?_map, List.getElem?_map]\n");
    w!(out, "    rw [List.getElem?_eq_getElem hb, List.getElem?_eq_getElem ht]\n");
    w!(out, "    simp [u8ToUInt8_xor]\n");
}

/// Emit the commutation theorem for a bare-array-returning pointwise method
/// (e.g. `Eval.open`).
#[allow(clippy::too_many_arguments)]
fn emit_pointwise_array(
    out: &mut String,
    amod: &AeneasModule,
    module: &IrModule<IrFunction>,
    imp: &IrImpl,
    fields: &[String],
    f: &IrFunction,
    pw: &Pointwise,
    bridged: &BTreeSet<String>,
) -> Result<Option<String>> {
    let self_ty = struct_name_of_impl(imp)?;
    let Some((param, param_ty)) = sole_param(f) else {
        return skip("params");
    };
    match (&pw.left, &pw.right) {
        (Operand::SelfField(_), Operand::BareArray(_) | Operand::StructField(_, _)) => (),
        (Operand::BareArray(_) | Operand::StructField(_, _), Operand::SelfField(_)) => (),
        _ => return skip("operands not self/parameter"),
    }
    let a_method = aeneas_method(amod, module, &self_ty, &f.name, fields);
    let a_closure = format!("{a_method}.closure.Insts.CoreOpsFunctionFnMutTupleUsizeU8");
    let a_self = amod.path(module, &self_ty);
    let n_method = native_method(&f.name, fields);

    let closure_thm = format!("{self_ty}_{}_closure_spec", f.name);
    let param_path = param_ty.as_deref().map(|pt| amod.path(module, pt));
    emit_closure_spec(
        out, &closure_thm, &a_closure, &self_ty, &a_self, &param, param_ty.as_deref(),
        param_path.as_deref(), pw, None,
    );

    let lhs = pw.left.list();
    let rhs = pw.right.list();
    let self_t = format!("({self_ty}ToNative m.val self)");
    let arg_op = if matches!(pw.left, Operand::SelfField(_)) { &pw.right } else { &pw.left };
    let arg_t = match arg_op {
        Operand::BareArray(p) => format!("(arrToNative {p})"),
        Operand::StructField(p, _) => {
            let pt = param_ty.as_deref().unwrap();
            if !bridged.contains(pt) {
                return skip("param struct not bridged");
            }
            format!("({pt}ToNative m.val {p})")
        }
        _ => return skip("bad right operand"),
    };
    w!(out, "\n/-- (generated) `{a_method}` commutes with the transport. -/\n");
    w!(out, "theorem {self_ty}_{}_commutes {{N : Type}} (d : vole.VoleArray N Std.U8)\n", f.name);
    w!(out, "    (m : Std.Usize)\n");
    w!(out, "    (hsize : d.arrayArraySizeInst.typenummarker_traitsUnsignedInst.USIZE = ok m)\n");
    w!(out, "    (self : {a_self} N) ({param} : {}) ", match param_ty {
        Some(_) => param_path.clone().unwrap() + " N",
        None => "volar_spec.array.Array Std.U8 N".to_string(),
    });
    w!(out, "\n    (hsl : {lhs}.length = m.val) (hal : {rhs}.length = m.val) :\n");
    w!(out, "    (do let a ← {a_method} d self {param}; ok (arrToNative a)) =\n");
    w!(out, "    ok ({self_ty}.{n_method} {self_t} {arg_t}) := by\n");
    let capture = default_capture(pw, &param);
    w!(out, "  have hspec : {a_method} d self {param}\n");
    w!(out, "      ⦃ a => vecL a.data = List.ofFn (fun j : Fin m.val =>\n");
    w!(out, "          xorBytes {lhs} {rhs} j.val) ⦄ := by\n");
    w!(out, "    simp only [{a_method}]\n");
    w!(out, "    exact from_fn_spec d.arrayArraySizeInst\n");
    w!(out, "      ({a_closure} d) m hsize {capture}\n");
    w!(out, "      (fun j => xorBytes {lhs} {rhs} j)\n");
    w!(out, "      (by\n");
    w!(out, "        intro i hi\n");
    w!(out, "        exact {closure_thm} d self {param} i (by rw [hsl]; exact hi) (by rw [hal]; exact hi))\n");
    w!(out, "  obtain ⟨a0, hr, hpost⟩ := spec_ok_elim hspec\n");
    w!(out, "  simp only [Bind.bind, Aeneas.Std.bind]\n");
    w!(out, "  rw [hr]\n");
    w!(out, "  simp only []\n");
    w!(out, "  congr 1\n");
    let pt_unfold = match &pw.right {
        Operand::StructField(_, _) => format!(", {}ToNative", param_ty.as_deref().unwrap()),
        _ => String::new(),
    };
    w!(out, "  simp only [{self_ty}ToNative{pt_unfold}, {self_ty}.{n_method}, arrToNative]\n");
    // native side: the result list is `List.ofFn fun i => lhs[i]! ^^^ rhs[i]!`
    emit_pointwise_tail(out, &lhs, &rhs);
    Ok(None)
}

/// Closure capture pair in first-use order (Aeneas's convention for the
/// inherent-method closures).
fn default_capture(pw: &Pointwise, param: &str) -> String {
    if pw.left.root() == "self" {
        format!("(self, {param})")
    } else {
        format!("({param}, self)")
    }
}

/// Emit the closure spec + commutation for a `BitXor` trait impl method
/// (`Eval.bitxor`): the Aeneas method lives at
/// `<S>.Insts.CoreOpsBitBitXor{S}{S}.bitxor`, and its value-receiver closure
/// captures the *fields* (precise capture), not the structs.
fn emit_bitxor(
    out: &mut String,
    amod: &AeneasModule,
    module: &IrModule<IrFunction>,
    imp: &IrImpl,
    fields: &[String],
    f: &IrFunction,
    ret: &str,
    rfield: &str,
    pw: &Pointwise,
    bridged: &BTreeSet<String>,
) -> Result<Option<String>> {
    let self_ty = struct_name_of_impl(imp)?;
    let (param, param_ty) = match sole_param(f) {
        Some(x) => x,
        None => return skip("bitxor: params"),
    };
    let Some(pt) = param_ty.as_deref() else {
        return skip("bitxor: rhs not a struct");
    };
    if pt != self_ty || !bridged.contains(&self_ty) {
        return skip("bitxor: rhs not the self struct");
    }
    let sf = match &pw.left {
        Operand::SelfField(f) => f.clone(),
        _ => return skip("bitxor: left not a self field"),
    };
    let of = match &pw.right {
        Operand::StructField(p, f) if *p == param => f.clone(),
        _ => return skip("bitxor: right not a param field"),
    };
    let a_self = amod.path(module, &self_ty);
    let a_method = format!("{a_self}.Insts.CoreOpsBitBitXor{self_ty}{self_ty}.bitxor");
    let a_closure = format!(
        "{}.bitxor.closure.Insts.CoreOpsFunctionFnMutTupleUsizeU8",
        amod.path(module, &format!("BitXor{self_ty}{self_ty}{self_ty}"))
    );

    let closure_thm = format!("{self_ty}_{}_closure_spec", f.name);
    let lhs = pw.left.list();
    let rhs = pw.right.list();
    let capture = format!("(self.{sf}, {param}.{of})");
    emit_closure_spec(
        out, &closure_thm, &a_closure, &self_ty, &a_self, &param, Some(&self_ty),
        Some(&a_self), pw, Some(capture.clone()),
    );

    w!(out, "\n/-- (generated) `{a_method}` commutes with the transport. -/\n");
    w!(out, "theorem {self_ty}_{}_commutes {{N : Type}} (d : vole.VoleArray N Std.U8)\n", f.name);
    w!(out, "    (m : Std.Usize)\n");
    w!(out, "    (hsize : d.arrayArraySizeInst.typenummarker_traitsUnsignedInst.USIZE = ok m)\n");
    w!(out, "    (self : {a_self} N) ({param} : {a_self} N)\n");
    w!(out, "    (hsl : {lhs}.length = m.val) (hal : {rhs}.length = m.val) :\n");
    w!(out, "    (do let r ← {a_method} d self {param}; ok ({ret}ToNative m.val r)) =\n");
    w!(out, "    ok ({self_ty}.bitxor ({self_ty}ToNative m.val self) ({self_ty}ToNative m.val {param}) : {ret} m.val) := by\n");
    w!(out, "  have hspec : {a_method} d self {param}\n");
    w!(out, "      ⦃ r => vecL r.{rfield}.data = List.ofFn (fun j : Fin m.val =>\n");
    w!(out, "          xorBytes {lhs} {rhs} j.val) ⦄ := by\n");
    w!(out, "    simp only [{a_method}]\n");
    w!(out, "    apply spec_bind (from_fn_spec d.arrayArraySizeInst\n");
    w!(out, "      ({a_closure} d) m hsize {capture}\n");
    w!(out, "      (fun j => xorBytes {lhs} {rhs} j)\n");
    w!(out, "      (by\n");
    w!(out, "        intro i hi\n");
    w!(out, "        exact {closure_thm} d self {param} i (by rw [hsl]; exact hi) (by rw [hal]; exact hi)))\n");
    w!(out, "    intro a ha\n");
    w!(out, "    rw [spec_ok]\n");
    w!(out, "    exact ha\n");
    w!(out, "  obtain ⟨r0, hr, hpost⟩ := spec_ok_elim hspec\n");
    w!(out, "  simp only [Bind.bind, Aeneas.Std.bind]\n");
    w!(out, "  rw [hr]\n");
    w!(out, "  simp only []\n");
    w!(out, "  congr 1\n");
    w!(out, "  simp only [{ret}ToNative, {self_ty}ToNative, {self_ty}.bitxor, arrToNative]\n");
    w!(out, "  congr 1\n");
    emit_pointwise_tail(out, &lhs, &rhs);
    Ok(None)
}

/// Emit the closure spec + commutation for a digest-constructor method:
/// `let mut d = D::new(); d.update(&x.f); …; let hash = d.finalize();
/// Ret { rf: from_fn(|i| hash[i]) }`.
///
/// The commutation is parameterized by the *peeled* digest chain: the
/// caller supplies each intermediate state, and the requirement that the
/// dict realizes the native `hash` on the concatenation of the update
/// inputs. (For a correct streaming hash, chunk boundaries do not matter,
/// so this is the specification of "the dict is hash-of-concatenation".)
#[allow(clippy::too_many_arguments)]
fn emit_digest_ctor(
    out: &mut String,
    amod: &AeneasModule,
    module: &IrModule<IrFunction>,
    imp: &IrImpl,
    fields: &[String],
    f: &IrFunction,
    ret: &str,
    rfield: &str,
    updates: &[(String, String)],
) -> Result<Option<String>> {
    let self_ty = struct_name_of_impl(imp)?;
    let a_self = amod.path(module, &self_ty);
    let a_method = aeneas_method(amod, module, &self_ty, &f.name, fields);
    let a_closure = format!("{a_method}.closure.Insts.CoreOpsFunctionFnMutTupleUsizeU8");
    let n_method = native_method(&f.name, fields);
    let closure_thm = format!("{self_ty}_{}_closure_spec", f.name);

    // binders + length hypotheses for the update operands (self, then params)
    let mut binders: Vec<String> = Vec::new();
    let mut len_hyps: Vec<String> = Vec::new();
    for (root, field) in updates {
        if root == "self" {
            len_hyps.push(format!("(h{root}l : (vecL self.{field}.data).length = m.val)"));
            continue;
        }
        let Some(p) = f.params.iter().find(|p| &p.name == root) else {
            return skip("digest-ctor: update operand not a param");
        };
        let inner = match &p.ty {
            IrType::Reference { elem, .. } => elem.as_ref(),
            t => t,
        };
        match inner {
            IrType::Struct { kind, .. } => {
                let pt = kind_name_of(kind).ok_or_else(|| {
                    CompilerError::Unsupported("digest-ctor: param kind".into())
                })?;
                binders.push(format!("({root} : {} N)", amod.path(module, pt)));
            }
            _ => return skip("digest-ctor: update operand not a struct"),
        }
        len_hyps.push(format!("(h{root}l : (vecL {root}.{field}.data).length = m.val)"));
    }

    // closure spec
    w!(out, "\n/-- (generated) closure spec: the extracted `{a_closure}.call_mut` reads\n    the finalized array. -/\n");
    w!(out, "theorem {closure_thm} {{N : Type}} {{D : Type}}\n");
    w!(out, "    (d : vole.VoleArray N Std.U8) (dict : volar_spec.SpecDigest D N)\n");
    w!(out, "    (h : volar_spec.array.Array Std.U8 N) (i : Std.Usize)\n");
    w!(out, "    (hl : i.val < (vecL h.data).length) :\n");
    w!(out, "    {a_closure}.call_mut d dict h i =\n");
    w!(out, "      ok ((vecL h.data).getD i.val default, h) := by\n");
    w!(out, "  have hl' : i.val < h.data.val.length := hl\n");
    w!(out, "  simp only [{a_closure}.call_mut,\n");
    w!(out, "    array.Array.Insts.CoreOpsIndexIndexUsizeT.index,\n");
    w!(out, "    alloc.vec.Vec.Insts.CoreOpsIndexIndex.index,\n");
    w!(out, "    core.Slice.Insts.CoreOpsIndexIndex.index,\n");
    w!(out, "    alloc.vec.Vec.Insts.CoreOpsDerefDerefSlice.deref, alloc.vec.Vec.as_slice,\n");
    w!(out, "    rust_primitives.sequence.seq_to_slice,\n");
    w!(out, "    core.Usize.Insts.CoreSliceIndexSliceIndexSliceT.get,\n");
    w!(out, "    rust_primitives.slice.slice_length,\n");
    w!(out, "    rust_primitives.slice.slice_index, Slice.index_usize,\n");
    w!(out, "    UScalar.lt_equiv, Slice.len_val, Slice.length, Slice.getElem?_Usize_eq,\n");
    w!(out, "    eq_true hl',\n");
    w!(out, "    decide_true, ↓reduceIte, getElem?_pos, spec_ok, bind_tc_ok, lift]\n");
    w!(out, "  simp [vecL, List.getD_eq_getElem?_getD, getElem?_pos, hl']\n");

    // the peeled digest chain hypotheses
    let asref = "(core.Shared0T.Insts.CoreConvertAsRef (array.Array.Insts.CoreConvertAsRefSlice Std.U8 d.arrayArraySizeInst))";
    let mut chain_hyps: Vec<String> = vec!["(d0 : D) (hnew : dict.new = ok d0)".to_string()];
    let concat_parts: Vec<String> = updates
        .iter()
        .map(|(root, field)| format!("(vecL {root}.{field}.data).map u8ToUInt8"))
        .collect();
    for (idx, (root, field)) in updates.iter().enumerate() {
        chain_hyps.push(format!(
            "(d{} : D) (hup{} : dict.update {asref} d{} {root}.{field} = ok d{})",
            idx + 1,
            idx + 1,
            idx,
            idx + 1
        ));
    }
    let n = updates.len();

    w!(out, "\n/-- (generated) `{a_method}` commutes with the transport, given the dict\n    realizes `hash` on the concatenated update inputs (streaming-hash spec). -/\n");
    w!(out, "theorem {self_ty}_{}_commutes {{N : Type}} {{D : Type}}\n", f.name);
    w!(out, "    (d : vole.VoleArray N Std.U8) (dict : volar_spec.SpecDigest D N)\n");
    w!(out, "    (m : Std.Usize)\n");
    w!(out, "    (hsize : d.arrayArraySizeInst.typenummarker_traitsUnsignedInst.USIZE = ok m)\n");
    w!(out, "    (self : {a_self} N) {}\n", binders.join(" "));
    for h in &len_hyps {
        w!(out, "    {h}\n");
    }
    w!(out, "    (hash : List UInt8 → List UInt8)\n");
    w!(out, "    {}", chain_hyps.join("\n    ").replace("\\n", "\n"));
    w!(out, "\n    (r : volar_spec.array.Array Std.U8 N) (hfin : dict.finalize d{n} = ok r)\n");
    w!(out, "    (hr : arrToNative r = hash ({}))\n", concat_parts.join(" ++ "));
    w!(out, "    (hrlen : (vecL r.data).length = m.val) :\n");
    let mut native_args = format!("({self_ty}ToNative m.val self)");
    for (root, field) in updates {
        if root != "self" {
            let p = f.params.iter().find(|p| &p.name == root).unwrap();
            let inner = match &p.ty {
                IrType::Reference { elem, .. } => elem.as_ref(),
                t => t,
            };
            let pt = match inner {
                IrType::Struct { kind, .. } => kind_name_of(kind).unwrap(),
                _ => unreachable!(),
            };
            let _ = field;
            native_args.push_str(&format!(" ({pt}ToNative m.val {root})"));
        }
    }
    w!(out, "    (do let r ← {a_method} d dict self {}\n", f.params.iter().map(|p| p.name.clone()).collect::<Vec<_>>().join(" "));
    w!(out, "        ok ({ret}ToNative m.val r)) =\n");
    w!(out, "    ok ({self_ty}.{n_method} {native_args} hash) := by\n");
    w!(out, "  obtain ⟨v, hv, hpost⟩ := spec_ok_elim (from_fn_spec d.arrayArraySizeInst\n");
    w!(out, "    ({a_closure} d dict) m hsize r\n");
    w!(out, "    (fun j => (vecL r.data).getD j default)\n");
    w!(out, "    (by intro i hi; exact {closure_thm} d dict r i (by rw [hrlen]; exact hi)))\n");
    w!(out, "  simp only [{a_method}]\n");
    w!(out, "  rw [hnew]\n  simp only [Bind.bind, Aeneas.Std.bind]\n");
    for idx in 0..n {
        w!(out, "  rw [hup{}]\n  simp only [Bind.bind, Aeneas.Std.bind]\n", idx + 1);
    }
    w!(out, "  rw [hfin]\n  simp only [Bind.bind, Aeneas.Std.bind]\n");
    w!(out, "  rw [hv]\n  simp only [Bind.bind, Aeneas.Std.bind]\n");
    w!(out, "  congr 1\n");
    w!(out, "  simp only [{ret}ToNative, {self_ty}ToNative, arrToNative]\n");
    // native side: reduce the Id.run digest accumulation
    let mut native_others: Vec<String> = Vec::new();
    for (root, _) in updates {
        if root != "self" {
            let p = f.params.iter().find(|p| &p.name == root).unwrap();
            let inner = match &p.ty {
                IrType::Reference { elem, .. } => elem.as_ref(),
                t => t,
            };
            if let IrType::Struct { kind, .. } = inner {
                native_others.push(format!("{}ToNative", kind_name_of(kind).unwrap()));
            }
        }
    }
    w!(out, "  simp only [{self_ty}.{n_method}{}, DigestSt.new, DigestSt.update, DigestSt.finalize]\n",
        if native_others.is_empty() { String::new() } else { format!(", {}", native_others.join(", ")) });
    w!(out, "  simp [Id.run, pure]\n");
    w!(out, "  congr 1\n");
    w!(out, "  rw [hpost, List.map_ofFn]\n");
    w!(out, "  rw [← hr]\n");
    w!(out, "  simp only [arrToNative]\n");
    w!(out, "  apply List.ext_getElem\n");
    w!(out, "  · simp\n");
    w!(out, "  · intro i h1 h2\n");
    w!(out, "    rw [List.getElem_ofFn, List.getElem_ofFn]\n");
    w!(out, "    simp only [Function.comp_apply]\n");
    w!(out, "    have hi : i < m.val := by simpa using h1\n");
    w!(out, "    have hb : i < (vecL r.data).length := by rw [hrlen]; exact hi\n");
    w!(out, "    rw [List.getD_eq_getElem?_getD]\n");
    w!(out, "    rw [List.getElem?_map, List.getElem?_eq_getElem hb]\n");
    w!(out, "    simp\n");
    Ok(None)
}

/// Emit the commutation for a clone-of-field method (`self.f.clone()`) —
/// e.g. `GlobalSecret.secret`. Uses the curated `array_u8_clone_eq` (voof
/// VolarSpecGarble.Verification.ProofObligations).
fn emit_clone_field(
    out: &mut String,
    amod: &AeneasModule,
    module: &IrModule<IrFunction>,
    imp: &IrImpl,
    fields: &[String],
    f: &IrFunction,
    field: &str,
) -> Result<Option<String>> {
    let self_ty = struct_name_of_impl(imp)?;
    let a_self = amod.path(module, &self_ty);
    let a_method = aeneas_method(amod, module, &self_ty, &f.name, fields);
    let n_method = native_method(&f.name, fields);
    w!(out, "\n/-- (generated) `{a_method}` commutes with the transport (clone is the\n    identity on U8 shim arrays). -/\n");
    w!(out, "theorem {self_ty}_{}_commutes {{N : Type}} (d : vole.VoleArray N Std.U8)\n", f.name);
    w!(out, "    (m : Std.Usize) (self : {a_self} N) :\n");
    w!(out, "    (do let r ← {a_method} d self\n");
    w!(out, "        ok (arrToNative r)) =\n");
    w!(out, "    ok ({self_ty}.{n_method} ({self_ty}ToNative m.val self)) := by\n");
    w!(out, "  simp only [{a_method}]\n");
    w!(out, "  rw [array_u8_clone_eq d self.{field}]\n");
    w!(out, "  rfl\n");
    Ok(None)
}

/// Emit the commutation for a mutate-index-zero-then-construct method
/// (`p[0] |= 1; Self { f: p }`) — e.g. `GlobalSecret.new`. Uses the curated
/// `shim_index_mut_zero` (voof ProofObligations) and `map_set_u8` /
/// `u8ToUInt8_or` (voof VolarSpecBridge.Prelude).
fn emit_mutate_or_zero_ctor(
    out: &mut String,
    amod: &AeneasModule,
    module: &IrModule<IrFunction>,
    imp: &IrImpl,
    fields: &[String],
    f: &IrFunction,
    field: &str,
    param: &str,
) -> Result<Option<String>> {
    let self_ty = struct_name_of_impl(imp)?;
    let a_self = amod.path(module, &self_ty);
    let a_method = aeneas_method(amod, module, &self_ty, &f.name, fields);
    let n_method = native_method(&f.name, fields);
    w!(out, "\n/-- (generated) `{a_method}` commutes with the transport (set-index-zero-or-one\n    then construct). -/\n");
    w!(out, "theorem {self_ty}_{}_commutes {{N : Type}} (d : vole.VoleArray N Std.U8)\n", f.name);
    w!(out, "    (m : Std.Usize) ({param} : volar_spec.array.Array Std.U8 N)\n");
    w!(out, "    (hs0 : 0 < (vecL {param}.data).length) :\n");
    w!(out, "    (do let r ← {a_method} d {param}\n");
    w!(out, "        ok ({self_ty}ToNative m.val r)) =\n");
    w!(out, "    ok ({self_ty}.{n_method} (arrToNative {param})) := by\n");
    w!(out, "  simp only [{a_method}]\n");
    w!(out, "  rw [shim_index_mut_zero d {param} hs0]\n");
    w!(out, "  simp only [bind_tc_ok, lift]\n");
    w!(out, "  show (ok ({self_ty}ToNative m.val\n");
    w!(out, "        ({{ {field} := ({{ data := (⟨{param}.data.val.set 0 (((vecL {param}.data)[0]'hs0) ||| 1#u8), (by rw [List.length_set]; exact {param}.data.property)⟩ :\n");
    w!(out, "          alloc.vec.Vec Std.U8), _n := {param}._n }} : volar_spec.array.Array Std.U8 N) }} : {a_self} N)) : RustM _) =\n");
    w!(out, "    ok ({self_ty}.{n_method} (arrToNative {param}))\n");
    w!(out, "  show (ok ({{ {field} := List.map u8ToUInt8 ({param}.data.val.set 0 (((vecL {param}.data)[0]'hs0) ||| 1#u8)) }} :\n");
    w!(out, "        {self_ty} m.val) : RustM _) =\n");
    w!(out, "    ok ({self_ty}.{n_method} (arrToNative {param}))\n");
    w!(out, "  rw [map_set_u8, u8ToUInt8_or, u8ToUInt8_one]\n");
    w!(out, "  simp only [{self_ty}.{n_method}, arrToNative]\n");
    w!(out, "  simp [Id.run, pure]\n");
    w!(out, "  congr 1\n");
    w!(out, "  congr 1\n");
    w!(out, "  rw [List.getElem?_eq_getElem hs0]\n");
    w!(out, "  simp\n");
    Ok(None)
}

/// Emit the closure spec + commutation for a table-selected digest
/// constructor (`let index = (bit-tests); …digest…;
/// Ret { rf: from_fn(|i| hash[i] ^ t.table[index][i]) }`) — e.g.
/// `Eval.and_via_table`. The proof template is the hand-verified voof probe
/// (table_sel_prefix/table_sel_index_* and shim_index_zero are curated in
/// voof's VolarSpecBridge.Prelude).
#[allow(clippy::too_many_arguments)]
fn emit_table_digest_ctor(
    out: &mut String,
    amod: &AeneasModule,
    module: &IrModule<IrFunction>,
    imp: &IrImpl,
    fields: &[String],
    f: &IrFunction,
    ret: &str,
    rfield: &str,
    tests: &[(String, String); 2],
    updates: &[(String, String)],
    table: &(String, String),
) -> Result<Option<String>> {
    let _ = rfield;
    let self_ty = struct_name_of_impl(imp)?;
    let a_self = amod.path(module, &self_ty);
    let a_method = aeneas_method(amod, module, &self_ty, &f.name, fields);
    let a_closure = format!("{a_method}.closure.Insts.CoreOpsFunctionFnMutTupleUsizeU8");
    let n_method = native_method(&f.name, fields);
    let closure_thm = format!("{self_ty}_{}_closure_spec", f.name);
    let (tname, tfield) = table;
    // the table param's struct type (for the transport + binder)
    let Some(tparam) = f.params.iter().find(|p| p.name == *tname) else {
        return skip("table-digest-ctor: table not a param");
    };
    let tinner = match &tparam.ty {
        IrType::Reference { elem, .. } => elem.as_ref(),
        t => t,
    };
    let IrType::Struct { kind: tkind, .. } = tinner else {
        return skip("table-digest-ctor: table param not a struct");
    };
    let Some(table_ty) = kind_name_of(tkind) else {
        return skip("table-digest-ctor: table param kind");
    };
    let a_table = amod.path(module, table_ty);

    // param binders + per-param native transports (params in order)
    let mut binders: Vec<String> = Vec::new();
    for p in &f.params {
        let inner = match &p.ty {
            IrType::Reference { elem, .. } => elem.as_ref(),
            t => t,
        };
        match inner {
            IrType::Struct { kind, .. } => {
                let pt = kind_name_of(kind)
                    .ok_or_else(|| CompilerError::Unsupported("table-digest: param kind".into()))?;
                binders.push(format!("({} : {} N)", p.name, amod.path(module, pt)));
            }
            _ => return skip("table-digest-ctor: param not a struct"),
        }
    }

    // length hypotheses: union of test roots and update roots, first field wins
    let mut len_hyps: Vec<String> = Vec::new();
    let mut seen: Vec<String> = Vec::new();
    for (root, field) in tests.iter().chain(updates.iter()) {
        if root == "self" {
            if !seen.contains(root) {
                seen.push(root.clone());
                len_hyps.push(format!("(h{root}l : (vecL self.{field}.data).length = m.val)"));
            }
            continue;
        }
        if !seen.contains(root) {
            seen.push(root.clone());
            len_hyps.push(format!("(h{root}l : (vecL {root}.{field}.data).length = m.val)"));
        }
    }

    // closure spec
    w!(out, "\n/-- (generated) closure spec: the extracted `{a_closure}.call_mut` is the\n    pointwise xor of the hash and the selected table row. -/\n");
    w!(out, "theorem {closure_thm} {{N : Type}} {{D : Type}}\n");
    w!(out, "    (d : vole.VoleArray N Std.U8) (dict : volar_spec.SpecDigest D N)\n");
    w!(out, "    (hash : volar_spec.array.Array Std.U8 N) (table : {a_table} N)\n");
    w!(out, "    (index : Std.Usize) (row : volar_spec.array.Array Std.U8 N)\n");
    w!(out, "    (hrow : table.{tfield}.val[index.val]? = some row) (i : Std.Usize)\n");
    w!(out, "    (hh : i.val < (vecL hash.data).length)\n");
    w!(out, "    (ht : i.val < (vecL row.data).length) :\n");
    w!(out, "    {a_closure}.call_mut d dict (hash, table, index) i =\n");
    w!(out, "      ok (xorBytes (vecL hash.data) (vecL row.data) i.val, (hash, table, index)) := by\n");
    w!(out, "  have hh' : i.val < hash.data.val.length := hh\n");
    w!(out, "  have ht' : i.val < row.data.val.length := ht\n");
    w!(out, "  simp only [{a_closure}.call_mut,\n");
    w!(out, "    array.Array.Insts.CoreOpsIndexIndexUsizeT.index,\n");
    w!(out, "    alloc.vec.Vec.Insts.CoreOpsIndexIndex.index,\n");
    w!(out, "    core.Slice.Insts.CoreOpsIndexIndex.index,\n");
    w!(out, "    alloc.vec.Vec.Insts.CoreOpsDerefDerefSlice.deref, alloc.vec.Vec.as_slice,\n");
    w!(out, "    rust_primitives.sequence.seq_to_slice,\n");
    w!(out, "    core.Usize.Insts.CoreSliceIndexSliceIndexSliceT.get,\n");
    w!(out, "    rust_primitives.slice.slice_length,\n");
    w!(out, "    rust_primitives.slice.slice_index, Slice.index_usize,\n");
    w!(out, "    Aeneas.Std.Array.index_usize, Aeneas.Std.Array.getElem?_Usize_eq, hrow,\n");
    w!(out, "    UScalar.lt_equiv, Slice.len_val, Slice.length, Slice.getElem?_Usize_eq,\n");
    w!(out, "    eq_true hh', eq_true ht',\n");
    w!(out, "    decide_true, ↓reduceIte, getElem?_pos, spec_ok, bind_tc_ok, lift]\n");
    w!(out, "  simp [xorBytes, vecL, hh, ht, hh', ht', hrow]\n");
    w!(out, "  rfl\n");

    // the peeled digest chain hypotheses
    let asref = "(core.Shared0T.Insts.CoreConvertAsRef (array.Array.Insts.CoreConvertAsRefSlice Std.U8 d.arrayArraySizeInst))";
    let mut chain_hyps: Vec<String> = vec!["(d0 : D) (hnew : dict.new = ok d0)".to_string()];
    let concat_parts: Vec<String> = updates
        .iter()
        .map(|(root, field)| format!("(vecL {root}.{field}.data).map u8ToUInt8"))
        .collect();
    for (idx, (root, field)) in updates.iter().enumerate() {
        chain_hyps.push(format!(
            "(d{} : D) (hup{} : dict.update {asref} d{} {root}.{field} = ok d{})",
            idx + 1,
            idx + 1,
            idx,
            idx + 1
        ));
    }
    let n = updates.len();

    // native args: self + params in order, each via its transport
    let mut native_args = format!("({self_ty}ToNative m.val self)");
    for p in &f.params {
        let inner = match &p.ty {
            IrType::Reference { elem, .. } => elem.as_ref(),
            t => t,
        };
        if let IrType::Struct { kind, .. } = inner {
            native_args.push_str(&format!(" ({}ToNative m.val {})", kind_name_of(kind).unwrap(), p.name));
        }
    }

    let (t0, t1) = (&tests[0], &tests[1]);
    w!(out, "\n/-- (generated) `{a_method}` commutes with the transport, given the dict\n    realizes `hash` on the concatenated update inputs (streaming-hash spec). -/\n");
    w!(out, "theorem {self_ty}_{}_commutes {{N : Type}} {{D : Type}}\n", f.name);
    w!(out, "    (d : vole.VoleArray N Std.U8) (dict : volar_spec.SpecDigest D N)\n");
    w!(out, "    (m : Std.Usize)\n");
    w!(out, "    (hsize : d.arrayArraySizeInst.typenummarker_traitsUnsignedInst.USIZE = ok m)\n");
    w!(out, "    (self : {a_self} N) {}\n", binders.join(" "));
    for h in &len_hyps {
        w!(out, "    {h}\n");
    }
    w!(out, "    (hm0 : 0 < m.val)\n");
    w!(out, "    (htl : ∀ k, k < 4 → ∀ row, table.{tfield}.val[k]? = some row →\n");
    w!(out, "      (vecL row.data).length = m.val)\n");
    w!(out, "    (hash : List UInt8 → List UInt8)\n");
    w!(out, "    {}\n", chain_hyps.join("\n    "));
    w!(out, "    (r : volar_spec.array.Array Std.U8 N) (hfin : dict.finalize d{n} = ok r)\n");
    w!(out, "    (hr : arrToNative r = hash ({}))\n", concat_parts.join(" ++ "));
    w!(out, "    (hrlen : (vecL r.data).length = m.val) :\n");
    w!(out, "    (do let res ← {a_method} d dict self {}\n", f.params.iter().map(|p| p.name.clone()).collect::<Vec<_>>().join(" "));
    w!(out, "        ok ({ret}ToNative m.val res)) =\n");
    w!(out, "    ok ({self_ty}.{n_method} {native_args} hash) := by\n");
    w!(out, "  have hlen4 : table.{tfield}.val.length = 4 := by\n");
    w!(out, "    have := table.{tfield}.property; simpa using this\n");
    w!(out, "  have h01l : 0 < (vecL {}.{}.data).length := by rw [h{}l]; exact hm0\n", t0.0, t0.1, t0.0);
    w!(out, "  have h02l : 0 < (vecL {}.{}.data).length := by rw [h{}l]; exact hm0\n", t1.0, t1.1, t1.0);
    w!(out, "  simp only [{a_method}]\n");
    w!(out, "  rw [shim_index_zero d {}.{} h01l]\n", t0.0, t0.1);
    w!(out, "  simp only [bind_tc_ok, lift]\n");
    w!(out, "  set b1 := (vecL {}.{}.data)[0]'h01l &&& 1#u8 with hb1def\n", t0.0, t0.1);
    w!(out, "  rw [shim_index_zero d {}.{} h02l]\n", t1.0, t1.1);
    w!(out, "  simp only [bind_tc_ok, lift]\n");
    w!(out, "  set b2 := (vecL {}.{}.data)[0]'h02l &&& 1#u8 with hb2def\n", t1.0, t1.1);
    w!(out, "  rw [table_sel_prefix b1 b2]\n");
    w!(out, "  set E : Std.Usize :=\n");
    w!(out, "    (if b1 = 1#u8 then 1#usize else 0#usize) ||| (if b2 = 1#u8 then 2#usize else 0#usize)\n");
    w!(out, "    with hEdef\n");
    w!(out, "  have hidxv : E.val = (if b1 = 1#u8 then 1 else 0) ||| (if b2 = 1#u8 then 2 else 0) :=\n");
    w!(out, "    table_sel_index_val b1 b2\n");
    w!(out, "  have hidx4 : E.val < 4 := table_sel_index_lt4 b1 b2\n");
    w!(out, "  rw [hnew]; simp only [Bind.bind, Aeneas.Std.bind]\n");
    for idx in 0..n {
        w!(out, "  rw [hup{}]; simp only [Bind.bind, Aeneas.Std.bind]\n", idx + 1);
    }
    w!(out, "  rw [hfin]; simp only [Bind.bind, Aeneas.Std.bind]\n");
    w!(out, "  set row : volar_spec.array.Array Std.U8 N :=\n");
    w!(out, "    table.{tfield}.val[E.val]'(by rw [hlen4]; exact hidx4) with hrowdef\n");
    w!(out, "  have hrow : table.{tfield}.val[E.val]? = some row := getElem?_pos _ _ _\n");
    w!(out, "  have hrowl : (vecL row.data).length = m.val := htl E.val hidx4 row hrow\n");
    w!(out, "  obtain ⟨v, hv, hpost⟩ := spec_ok_elim (from_fn_spec d.arrayArraySizeInst\n");
    w!(out, "    ({a_closure} d dict) m hsize\n");
    w!(out, "    (r, {tname}, E)\n");
    w!(out, "    (fun j => xorBytes (vecL r.data) (vecL row.data) j)\n");
    w!(out, "    (by intro j hj\n");
    w!(out, "        have hh : j.val < (vecL r.data).length := by rw [hrlen]; exact hj\n");
    w!(out, "        have ht : j.val < (vecL row.data).length := by rw [hrowl]; exact hj\n");
    w!(out, "        exact {closure_thm} d dict r {tname} E row hrow j hh ht))\n");
    w!(out, "  rw [hv]\n");
    w!(out, "  simp only [Bind.bind, Aeneas.Std.bind]\n");
    w!(out, "  congr 1\n");
    w!(out, "  simp only [{self_ty}.{n_method}, {self_ty}ToNative, {table_ty}ToNative, arrFixedToNative,\n");
    w!(out, "    arrToNative, DigestSt.new, DigestSt.update, DigestSt.finalize]\n");
    w!(out, "  simp [Id.run, pure]\n");
    w!(out, "  congr 1\n");
    w!(out, "  rw [hpost, List.map_ofFn]\n");
    w!(out, "  rw [← hr]\n");
    w!(out, "  simp only [arrToNative]\n");
    w!(out, "  apply List.ext_getElem\n");
    w!(out, "  · simp\n");
    w!(out, "  · intro i h1 h2\n");
    w!(out, "    rw [List.getElem_ofFn, List.getElem_ofFn]\n");
    w!(out, "    simp only [Function.comp_apply]\n");
    w!(out, "    have hi : i < m.val := by simpa using h1\n");
    w!(out, "    have hb1 : (Option.map u8ToUInt8 ((vecL {}.{}.data)[0]?)).getD default &&& (1 : UInt8)\n", t0.0, t0.1);
    w!(out, "        = u8ToUInt8 b1 := by\n");
    w!(out, "      rw [List.getElem?_eq_getElem h01l]\n");
    w!(out, "      simp only [Option.map_some, Option.getD_some]\n");
    w!(out, "      rw [← u8ToUInt8_one, ← u8ToUInt8_and, hb1def]\n");
    w!(out, "    have hb2 : (Option.map u8ToUInt8 ((vecL {}.{}.data)[0]?)).getD default &&& (1 : UInt8)\n", t1.0, t1.1);
    w!(out, "        = u8ToUInt8 b2 := by\n");
    w!(out, "      rw [List.getElem?_eq_getElem h02l]\n");
    w!(out, "      simp only [Option.map_some, Option.getD_some]\n");
    w!(out, "      rw [← u8ToUInt8_one, ← u8ToUInt8_and, hb2def]\n");
    w!(out, "    have hiff1 : ((Option.map u8ToUInt8 ((vecL {}.{}.data)[0]?)).getD default &&& (1 : UInt8)) = 1\n", t0.0, t0.1);
    w!(out, "        ↔ b1 = 1#u8 := by\n");
    w!(out, "      rw [hb1]; constructor\n");
    w!(out, "      · intro h; apply u8ToUInt8_injective; rw [h, u8ToUInt8_one]\n");
    w!(out, "      · intro h; rw [h, u8ToUInt8_one]\n");
    w!(out, "    have hiff2 : ((Option.map u8ToUInt8 ((vecL {}.{}.data)[0]?)).getD default &&& (1 : UInt8)) = 1\n", t1.0, t1.1);
    w!(out, "        ↔ b2 = 1#u8 := by\n");
    w!(out, "      rw [hb2]; constructor\n");
    w!(out, "      · intro h; apply u8ToUInt8_injective; rw [h, u8ToUInt8_one]\n");
    w!(out, "      · intro h; rw [h, u8ToUInt8_one]\n");
    w!(out, "    rw [if_congr hiff1 rfl rfl, if_congr hiff2 rfl rfl, ← hidxv]\n");
    w!(out, "    rw [hrow]\n");
    w!(out, "    simp only [Option.map_some, Option.getD_some]\n");
    w!(out, "    have hbr : i < (vecL r.data).length := by rw [hrlen]; exact hi\n");
    w!(out, "    have hbrow : i < (vecL row.data).length := by rw [hrowl]; exact hi\n");
    w!(out, "    rw [List.getElem?_map, List.getElem?_eq_getElem hbr]\n");
    w!(out, "    simp only [arrToNative]\n");
    w!(out, "    rw [List.getElem?_map, List.getElem?_eq_getElem hbrow]\n");
    w!(out, "    simp only [Option.map_some, Option.getD_some]\n");
    w!(out, "    simp [xorBytes, hbr, hbrow, u8ToUInt8_xor]\n");
    Ok(None)
}

/// Emit the closure spec + commutation for a conditional pointwise
/// constructor (`Ret { rf: from_fn(|i| if c { lhs[i] ^ rhs[i] } else rhs[i]) }`).
#[allow(clippy::too_many_arguments)]
fn emit_pointwise_cond_ctor(
    out: &mut String,
    amod: &AeneasModule,
    module: &IrModule<IrFunction>,
    imp: &IrImpl,
    fields: &[String],
    f: &IrFunction,
    ret: &str,
    rfield: &str,
    cond: &str,
    pw: &Pointwise,
    bridged: &BTreeSet<String>,
) -> Result<Option<String>> {
    let self_ty = struct_name_of_impl(imp)?;
    // two non-self params: the cond (Bool) and the array/struct
    let arr_param = pw
        .right
        .root();
    if arr_param == "self" {
        return skip("cond-ctor: right operand is self");
    }
    let cond_param = if cond == arr_param { None } else { Some(cond.to_string()) };
    let Some(cond_param) = cond_param else {
        return skip("cond-ctor: cond is the array param");
    };
    // locate param types
    let mut arr_ty: Option<Option<String>> = None;
    let mut cond_ty_ok = false;
    for p in &f.params {
        let inner = match &p.ty {
            IrType::Reference { elem, .. } => elem.as_ref(),
            t => t,
        };
        if p.name == arr_param {
            arr_ty = Some(match inner {
                IrType::Struct { kind, .. } => kind_name_of(kind).map(|s| s.to_string()),
                IrType::Array { elem, .. }
                    if matches!(elem.as_ref(), IrType::Primitive(PrimitiveType::U8)) =>
                {
                    None
                }
                _ => return skip("cond-ctor: bad array param type"),
            });
        } else if p.name == cond_param {
            cond_ty_ok = matches!(inner, IrType::Primitive(PrimitiveType::Bool));
        }
    }
    if !cond_ty_ok {
        return skip("cond-ctor: cond param not Bool");
    }
    let Some(arr_ty) = arr_ty else {
        return skip("cond-ctor: array param not found");
    };
    let a_method = aeneas_method(amod, module, &self_ty, &f.name, fields);
    let a_closure = format!("{a_method}.closure.Insts.CoreOpsFunctionFnMutTupleUsizeU8");
    let a_self = amod.path(module, &self_ty);
    let n_method = native_method(&f.name, fields);

    let lhs = pw.left.list(); // self field (xor operand 1)
    let rhs = pw.right.list(); // param (xor operand 2 and else-branch)
    // capture order: first-use order — the cond var, then the operands' roots
    let mut caps: Vec<String> = vec![cond_param.clone()];
    for r in [pw.left.root(), pw.right.root()] {
        if !caps.contains(&r) {
            caps.push(r);
        }
    }
    let capture = format!("({})", caps.join(", "));
    let closure_thm = format!("{self_ty}_{}_closure_spec", f.name);
    let arr_decl = match &arr_ty {
        Some(pt) => format!("({arr_param} : {} N)", amod.path(module, pt)),
        None => format!("({arr_param} : volar_spec.array.Array Std.U8 N)"),
    };
    // closure spec
    w!(out, "\n/-- (generated) closure spec: the extracted `{a_closure}.call_mut` is the\n    conditional pointwise xor. -/\n");
    w!(out, "theorem {closure_thm} {{N : Type}} (d : vole.VoleArray N Std.U8)\n");
    w!(out, "    ({cond_param} : Bool) (self : {a_self} N) {arr_decl} (i : Std.Usize)\n");
    w!(out, "    (hs : i.val < {lhs}.length) (ha : i.val < {rhs}.length) :\n");
    w!(out, "    {a_closure}.call_mut d {capture} i =\n");
    w!(out, "      ok ((if {cond_param} = true then xorBytes {lhs} {rhs} i.val\n");
    w!(out, "           else {rhs}.getD i.val default), {capture}) := by\n");
    w!(out, "  have hs' : i.val < {}.length := hs\n", pw.left.val_list());
    w!(out, "  have ha' : i.val < {}.length := ha\n", pw.right.val_list());
    w!(out, "  simp only [{a_closure}.call_mut,\n");
    w!(out, "    array.Array.Insts.CoreOpsIndexIndexUsizeT.index,\n");
    w!(out, "    alloc.vec.Vec.Insts.CoreOpsIndexIndex.index,\n");
    w!(out, "    core.Slice.Insts.CoreOpsIndexIndex.index,\n");
    w!(out, "    alloc.vec.Vec.Insts.CoreOpsDerefDerefSlice.deref, alloc.vec.Vec.as_slice,\n");
    w!(out, "    rust_primitives.sequence.seq_to_slice,\n");
    w!(out, "    core.Usize.Insts.CoreSliceIndexSliceIndexSliceT.get,\n");
    w!(out, "    rust_primitives.slice.slice_length,\n");
    w!(out, "    rust_primitives.slice.slice_index, Slice.index_usize,\n");
    w!(out, "    UScalar.lt_equiv, Slice.len_val, Slice.length, Slice.getElem?_Usize_eq,\n");
    w!(out, "    eq_true hs', eq_true ha',\n");
    w!(out, "    decide_true, ↓reduceIte, getElem?_pos, spec_ok, bind_tc_ok, lift]\n");
    w!(out, "  cases {cond_param} <;> simp [xorBytes, vecL, hs, ha, hs', ha',\n");
    w!(out, "    List.getD_eq_getElem?_getD, getElem?_pos]\n");
    w!(out, "  · rfl\n  · rfl\n");

    // commutation
    let self_t = format!("({self_ty}ToNative m.val self)");
    let arg_t = match &pw.right {
        Operand::StructField(p, _) => {
            let pt = arr_ty.as_deref().unwrap();
            if !bridged.contains(pt) {
                return skip("cond-ctor: param struct not bridged");
            }
            format!("({pt}ToNative m.val {p})")
        }
        Operand::BareArray(p) => format!("(arrToNative {p})"),
        _ => return skip("cond-ctor: bad right operand"),
    };
    w!(out, "\n/-- (generated) `{a_method}` commutes with the transport. -/\n");
    w!(out, "theorem {self_ty}_{}_commutes {{N : Type}} (d : vole.VoleArray N Std.U8)\n", f.name);
    w!(out, "    (m : Std.Usize)\n");
    w!(out, "    (hsize : d.arrayArraySizeInst.typenummarker_traitsUnsignedInst.USIZE = ok m)\n");
    w!(out, "    (self : {a_self} N) {arr_decl} ({cond_param} : Bool)\n");
    w!(out, "    (hsl : {lhs}.length = m.val) (hal : {rhs}.length = m.val) :\n");
    w!(out, "    (do let r ← {a_method} d self {arr_param} {cond_param}; ok ({ret}ToNative m.val r)) =\n");
    w!(out, "    ok ({self_ty}.{n_method} {self_t} {arg_t} {cond_param} : {ret} m.val) := by\n");
    w!(out, "  have hspec : {a_method} d self {arr_param} {cond_param}\n");
    w!(out, "      ⦃ r => vecL r.{rfield}.data = List.ofFn (fun j : Fin m.val =>\n");
    w!(out, "          if {cond_param} = true then xorBytes {lhs} {rhs} j.val\n");
    w!(out, "          else {rhs}.getD (↑j) default) ⦄ := by\n");
    w!(out, "    simp only [{a_method}]\n");
    w!(out, "    apply spec_bind (from_fn_spec d.arrayArraySizeInst\n");
    w!(out, "      ({a_closure} d) m hsize {capture}\n");
    w!(out, "      (fun j => if {cond_param} = true then xorBytes {lhs} {rhs} j\n");
    w!(out, "                else {rhs}.getD j default)\n");
    w!(out, "      (by\n");
    w!(out, "        intro i hi\n");
    w!(out, "        exact {closure_thm} d {cond_param} self {arr_param} i (by rw [hsl]; exact hi) (by rw [hal]; exact hi)))\n");
    w!(out, "    intro a ha\n");
    w!(out, "    rw [spec_ok]\n");
    w!(out, "    exact ha\n");
    w!(out, "  obtain ⟨r0, hr, hpost⟩ := spec_ok_elim hspec\n");
    w!(out, "  simp only [Bind.bind, Aeneas.Std.bind]\n");
    w!(out, "  rw [hr]\n");
    w!(out, "  simp only []\n");
    w!(out, "  congr 1\n");
    let pt_unfold = match &arr_ty {
        Some(pt) => format!(", {pt}ToNative"),
        None => String::new(),
    };
    w!(out, "  simp only [{ret}ToNative, {self_ty}ToNative{pt_unfold}, {self_ty}.{n_method}, arrToNative]\n");
    w!(out, "  congr 1\n");
    w!(out, "  rw [hpost, List.map_ofFn]\n");
    w!(out, "  apply List.ext_getElem\n");
    w!(out, "  · simp\n");
    w!(out, "  · intro i h1 h2\n");
    w!(out, "    rw [List.getElem_ofFn, List.getElem_ofFn]\n");
    w!(out, "    have hi : i < m.val := by simpa using h1\n");
    w!(out, "    have hb : i < {lhs}.length := by rw [hsl]; exact hi\n");
    w!(out, "    have ht : i < {rhs}.length := by rw [hal]; exact hi\n");
    w!(out, "    cases {cond_param}\n");
    w!(out, "    · simp only [Function.comp_apply, Bool.false_eq_true, ↓reduceIte]\n");
    w!(out, "      rw [List.getD_eq_getElem?_getD, List.getElem!_eq_getElem?_getD, List.getElem?_map,\n");
    w!(out, "        List.getElem?_eq_getElem ht]\n");
    w!(out, "      simp [Option.getD_some, Option.map_some]\n");
    w!(out, "    · simp only [Function.comp_apply, eq_self_iff_true, ↓reduceIte, xorBytes]\n");
    w!(out, "      rw [dif_pos ⟨hb, ht⟩]\n");
    w!(out, "      rw [List.getElem!_eq_getElem?_getD, List.getElem!_eq_getElem?_getD]\n");
    w!(out, "      rw [List.getElem?_map, List.getElem?_map]\n");
    w!(out, "      rw [List.getElem?_eq_getElem hb, List.getElem?_eq_getElem ht]\n");
    w!(out, "      simp [u8ToUInt8_xor]\n");
    Ok(None)
}

/// Emit the commutation theorem for a default-constructor method (`zero`).
fn emit_default_ctor(
    out: &mut String,
    amod: &AeneasModule,
    module: &IrModule<IrFunction>,
    imp: &IrImpl,
    fields: &[String],
    f: &IrFunction,
) -> Result<Option<String>> {
    if !f.params.is_empty() {
        return skip("params");
    }
    let self_ty = struct_name_of_impl(imp)?;
    let a_method = aeneas_method(amod, module, &self_ty, &f.name, fields);
    let n_method = native_method(&f.name, fields);
    // single-field structs only (each field is a default array)
    if fields.len() != 1 {
        return skip("multi-field");
    }
    let field = &fields[0];
    w!(out, "\n/-- (generated) `{a_method}` commutes with the transport. -/\n");
    w!(out, "theorem {self_ty}_{}_commutes {{N : Type}} (d : vole.VoleArray N Std.U8)\n", f.name);
    w!(out, "    (m : Std.Usize)\n");
    w!(out, "    (hsize : d.arrayArraySizeInst.typenummarker_traitsUnsignedInst.USIZE = ok m) :\n");
    w!(out, "    (do let r ← {a_method} d; ok ({self_ty}ToNative m.val r)) =\n");
    w!(out, "    ok ({self_ty}.{n_method} : {self_ty} m.val) := by\n");
    w!(out, "  have hspec : {a_method} d\n");
    w!(out, "      ⦃ r => vecL r.{field}.data = List.replicate m.val default ⦄ := by\n");
    w!(out, "    simp only [{a_method}]\n");
    w!(out, "    apply spec_bind (array_default_spec _ _ _ m hsize default core_u8_default)\n");
    w!(out, "    intro a ha\n");
    w!(out, "    rw [spec_ok]\n");
    w!(out, "    exact ha\n");
    w!(out, "  obtain ⟨r0, hr, hpost⟩ := spec_ok_elim hspec\n");
    w!(out, "  simp only [Bind.bind, Aeneas.Std.bind]\n");
    w!(out, "  rw [hr]\n");
    w!(out, "  simp only []\n");
    w!(out, "  congr 1\n");
    w!(out, "  simp only [{self_ty}ToNative, arrToNative, {self_ty}.{n_method}]\n");
    w!(out, "  congr 1\n");
    w!(out, "  rw [hpost, List.map_replicate, u8ToUInt8_default]\n");
    Ok(None)
}

fn struct_name_of_impl(imp: &IrImpl) -> Result<String> {
    match &imp.self_ty {
        IrType::Struct { kind, .. } => kind_name_of(kind)
            .map(|s| s.to_string())
            .ok_or_else(|| CompilerError::Unsupported("bridge: impl self kind".into())),
        _ => Err(CompilerError::Unsupported("bridge: impl self type not a struct".into())),
    }
}

fn struct_module_path(module: &IrModule<IrFunction>, name: &str) -> Vec<String> {
    module
        .structs
        .iter()
        .find(|s| struct_name(s) == name)
        .map(|s| s.module_path.clone())
        .unwrap_or_default()
}

/// Emit the full bridge module.
pub fn print_module_lean_bridge(
    module: &IrModule<IrFunction>,
    only: &BTreeSet<String>,
    aeneas_module: &str,
) -> Result<String> {
    let amod = AeneasModule(aeneas_module);
    let mut out = String::new();
    w!(out, "-- THIS FILE WAS AUTOMATICALLY GENERATED BY volar-compiler (lean bridge) — DO NOT EDIT.\n");
    w!(out, "-- Regenerate with `cargo xtask gen-specs`. The import block is prepended by the\n");
    w!(out, "-- downstream consumer (voof's scripts/prepare-native.sh).\n\n");
    w!(out, "open CoreModels Aeneas\n");
    w!(out, "open Aeneas.Std hiding namespace core alloc\n");
    w!(out, "open RustM ControlFlow Error\n");
    w!(out, "open volar_spec\n");
    w!(out, "open Aeneas.Std.WP\n");

    let mut bridged: BTreeSet<String> = BTreeSet::new();
    let mut skipped: Vec<String> = Vec::new();
    let mut struct_fields: Vec<(String, Vec<String>)> = Vec::new();

    // ── Transports ─────────────────────────────────────────────────────
    w!(out, "\n-- Transport definitions\n");
    for s in &module.structs {
        let name = struct_name(s);
        if !only.contains(&name) {
            continue;
        }
        let fields: Vec<String> = s.fields.iter().map(|f| f.name.clone()).collect();
        struct_fields.push((name.clone(), fields.clone()));
        if s.generics.len() == 1
            && !s.fields.is_empty()
            && s.fields.iter().all(|f| is_shim_u8_array(&f.ty) || is_fixed_shim_u8_array(&f.ty))
        {
            let aename = amod.path(module, &name);
            w!(out, "\n/-- (generated) transport: extracted `{aename}` → native `{name}`. -/\n");
            w!(out, "def {name}ToNative (n : Nat) (x : {aename} N) : {name} n :=\n");
            let parts: Vec<String> = s
                .fields
                .iter()
                .map(|f| {
                    if is_shim_u8_array(&f.ty) {
                        format!("{} := arrToNative x.{}", f.name, f.name)
                    } else {
                        format!("{} := arrFixedToNative x.{}", f.name, f.name)
                    }
                })
                .collect();
            w!(out, "  {{ {} }}\n", parts.join(", "));
            bridged.insert(name);
        } else {
            skipped.push(format!("struct {name} (fields not all shim u8-arrays)"));
        }
    }

    // ── Methods ────────────────────────────────────────────────────────
    w!(out, "\n-- Commutation theorems\n");
    for imp in &module.impls {
        let Ok(self_name) = struct_name_of_impl(imp) else {
            continue; // not a spec struct impl at all; outside our scope
        };
        if !only.contains(&self_name) {
            continue; // a different spec module; not this bridge
        }
        if !bridged.contains(&self_name) {
            skipped.push(format!("impl {self_name} (struct not bridged)"));
            continue;
        }
        let is_bitxor = imp.trait_.as_ref().is_some_and(|t| {
            matches!(&t.kind, crate::ir::TraitKind::Math(crate::ir::MathTrait::BitXor))
        });
        if imp.trait_.is_some() && !is_bitxor {
            skipped.push(format!("impl {self_name} (trait impl)"));
            continue;
        }
        let fields = &struct_fields
            .iter()
            .find(|(n, _)| *n == self_name)
            .unwrap()
            .1;
        for item in &imp.items {
            let crate::ir::IrImplItem::Method(f) = item else { continue };
            let r = match classify(f) {
                Some(MethodPattern::PointwiseCtor { ret, rfield, pw }) => {
                    if is_bitxor {
                        emit_bitxor(&mut out, &amod, module, imp, fields, f, &ret, &rfield, &pw, &bridged)?
                    } else {
                        emit_pointwise_ctor(
                            &mut out, &amod, module, imp, fields, f, &ret, &rfield, &pw, &bridged,
                        )?
                    }
                }
                Some(MethodPattern::PointwiseArray { pw }) => {
                    emit_pointwise_array(&mut out, &amod, module, imp, fields, f, &pw, &bridged)?
                }
                Some(MethodPattern::PointwiseCondCtor { ret, rfield, cond, pw }) => {
                    emit_pointwise_cond_ctor(
                        &mut out, &amod, module, imp, fields, f, &ret, &rfield, &cond, &pw,
                        &bridged,
                    )?
                }
                Some(MethodPattern::DefaultCtor) => {
                    emit_default_ctor(&mut out, &amod, module, imp, fields, f)?
                }
                Some(MethodPattern::DigestCtor { ret, rfield, updates }) => {
                    emit_digest_ctor(&mut out, &amod, module, imp, fields, f, &ret, &rfield, &updates)?
                }
                Some(MethodPattern::TableDigestCtor { ret, rfield, tests, updates, table }) => {
                    emit_table_digest_ctor(
                        &mut out, &amod, module, imp, fields, f, &ret, &rfield, &tests, &updates,
                        &table,
                    )?
                }
                Some(MethodPattern::CloneField { field }) => {
                    emit_clone_field(&mut out, &amod, module, imp, fields, f, &field)?
                }
                Some(MethodPattern::MutateOrZeroCtor { field, param }) => {
                    emit_mutate_or_zero_ctor(&mut out, &amod, module, imp, fields, f, &field, &param)?
                }
                None => Some("body pattern".to_string()),
            };
            if let Some(reason) = r {
                skipped.push(format!("method {self_name}.{} ({reason})", f.name));
            }
        }
    }

    for f in &module.functions {
        if only.contains(&f.name) {
            skipped.push(format!("function {} (free functions not bridged in v1)", f.name));
        }
    }

    if !skipped.is_empty() {
        w!(out, "\n-- NOT-BRIDGED (v1):\n");
        for s in skipped {
            w!(out, "--   - {s}\n");
        }
    }
    Ok(out)
}
