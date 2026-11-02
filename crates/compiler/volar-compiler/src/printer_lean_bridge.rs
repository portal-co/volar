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
use std::{format, string::String, string::ToString, vec::Vec};

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
    /// `S { f: Default::default(), .. }` — e.g. `Garble.zero`.
    DefaultCtor,
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

/// A shim-array-of-u8 field type (extracted side): `array.Array u8 N`.
fn is_shim_u8_array(ty: &IrType) -> bool {
    matches!(
        ty,
        IrType::Array { elem, .. } if matches!(elem.as_ref(), IrType::Primitive(PrimitiveType::U8))
    )
}

/// Classify a method body (must be a single tail expression).
fn classify(f: &IrFunction) -> Option<MethodPattern> {
    if !f.body.stmts.is_empty() {
        return None;
    }
    let e = f.body.expr.as_ref()?;
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
) {
    let lhs = pw.left.list();
    let rhs = pw.right.list();
    let param_decl = match param_ty {
        Some(_) => format!("({param} : {} N)", param_path.unwrap()),
        None => format!("({param} : volar_spec.array.Array Std.U8 N)"),
    };
    w!(out, "\n/-- (generated) closure spec: the extracted `{a_closure}.call_mut` is the\n    pointwise xor of the two operands. -/\n");
    w!(out, "theorem {thm} {{N : Type}} (d : vole.VoleArray N Std.U8)\n");
    w!(out, "    (self : {self_path} N) {param_decl} (i : Std.Usize)\n");
    w!(out, "    (hs : i.val < {lhs}.length) (ha : i.val < {rhs}.length) :\n");
    w!(out, "    {a_closure}.call_mut d (self, {param}) i =\n");
    w!(out, "      ok (xorBytes {lhs} {rhs} i.val, (self, {param})) := by\n");
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
    // right operand must involve the parameter; left must be a self field
    let sfield = match &pw.left {
        Operand::SelfField(f) => f.clone(),
        _ => return skip("left operand not a self field"),
    };
    let a_method = aeneas_method(amod, module, &self_ty, &f.name, fields);
    let a_closure = format!("{a_method}.closure.Insts.CoreOpsFunctionFnMutTupleUsizeU8");
    let a_self = amod.path(module, &self_ty);
    let n_method = native_method(&f.name, fields);

    let closure_thm = format!("{self_ty}_{}_closure_spec", f.name);
    let param_path = param_ty.as_deref().map(|pt| amod.path(module, pt));
    emit_closure_spec(
        out, &closure_thm, &a_closure, &self_ty, &a_self, &param, param_ty.as_deref(),
        param_path.as_deref(), pw,
    );

    let lhs = pw.left.list();
    let rhs = pw.right.list();
    // native transports
    let self_t = format!("({self_ty}ToNative m.val self)");
    let arg_t = match &pw.right {
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
    w!(out, "  have hspec : {a_method} d self {param}\n");
    w!(out, "      ⦃ r => vecL r.{rfield}.data = List.ofFn (fun j : Fin m.val =>\n");
    w!(out, "          xorBytes {lhs} {rhs} j.val) ⦄ := by\n");
    w!(out, "    simp only [{a_method}]\n");
    w!(out, "    apply spec_bind (from_fn_spec d.arrayArraySizeInst\n");
    w!(out, "      ({a_closure} d) m hsize (self, {param})\n");
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
    let sfield = match &pw.left {
        Operand::SelfField(f) => f.clone(),
        _ => return skip("left operand not a self field"),
    };
    let _ = sfield;
    let a_method = aeneas_method(amod, module, &self_ty, &f.name, fields);
    let a_closure = format!("{a_method}.closure.Insts.CoreOpsFunctionFnMutTupleUsizeU8");
    let a_self = amod.path(module, &self_ty);
    let n_method = native_method(&f.name, fields);

    let closure_thm = format!("{self_ty}_{}_closure_spec", f.name);
    let param_path = param_ty.as_deref().map(|pt| amod.path(module, pt));
    emit_closure_spec(
        out, &closure_thm, &a_closure, &self_ty, &a_self, &param, param_ty.as_deref(),
        param_path.as_deref(), pw,
    );

    let lhs = pw.left.list();
    let rhs = pw.right.list();
    let self_t = format!("({self_ty}ToNative m.val self)");
    let arg_t = match &pw.right {
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
    w!(out, "  have hspec : {a_method} d self {param}\n");
    w!(out, "      ⦃ a => vecL a.data = List.ofFn (fun j : Fin m.val =>\n");
    w!(out, "          xorBytes {lhs} {rhs} j.val) ⦄ := by\n");
    w!(out, "    simp only [{a_method}]\n");
    w!(out, "    exact from_fn_spec d.arrayArraySizeInst\n");
    w!(out, "      ({a_closure} d) m hsize (self, {param})\n");
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
            && s.fields.iter().all(|f| is_shim_u8_array(&f.ty))
        {
            let aename = amod.path(module, &name);
            w!(out, "\n/-- (generated) transport: extracted `{aename}` → native `{name}`. -/\n");
            w!(out, "def {name}ToNative (n : Nat) (x : {aename} N) : {name} n :=\n");
            let parts: Vec<String> = fields
                .iter()
                .map(|f| format!("{f} := arrToNative x.{f}"))
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
            skipped.push("impl (non-struct self type)".to_string());
            continue;
        };
        if !bridged.contains(&self_name) {
            skipped.push(format!("impl {self_name} (struct not bridged)"));
            continue;
        }
        if imp.trait_.is_some() {
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
                    emit_pointwise_ctor(
                        &mut out, &amod, module, imp, fields, f, &ret, &rfield, &pw, &bridged,
                    )?
                }
                Some(MethodPattern::PointwiseArray { pw }) => {
                    emit_pointwise_array(&mut out, &amod, module, imp, fields, f, &pw, &bridged)?
                }
                Some(MethodPattern::DefaultCtor) => {
                    emit_default_ctor(&mut out, &amod, module, imp, fields, f)?
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
