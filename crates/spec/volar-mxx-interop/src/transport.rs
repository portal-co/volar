// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: generated
//! The Phase-2 transport generator: emits a Lean module proving that a
//! generated spec element's native Lean function (from `printer_lean`,
//! exact-Nat mode) refines the mxx stage relation of the same frozen graph
//! (from `mxx_ir_core::lean::export`).
//!
//! ## Recognized shape (v1)
//!
//! - Ring dimension 1 (constant polys), a single CRT tower. Multi-tower
//!   needs CRT recomposition lemmas; ring dim > 1 needs the general
//!   negacyclic convolution correspondence — both are Phase-2 follow-ups.
//! - Helper calls limited to `mat_add` / `mat_mul`, with mat_mul inner
//!   dimension `K ≤ 3` (the proof's `Fin.sum_univ_*` table).
//! - Single named output, deterministic graph (empty witness — no
//!   samplers), no compile parameters.
//!
//! Anything outside this shape produces a `-- NOT-BRIDGED` inventory
//! instead of a theorem: loud, never silently wrong.

use mxx_ir_core::graph::{FrozenGraphScopeId, Graph};
use mxx_ir_core::node::NodeKind;
use mxx_ir_core::types::{ConcreteWireType, WireRef};
use mxx_ir_core::validate::ValidatedGraph;
use volar_compiler::{IrExprKind, IrModule, IrFunction};

use crate::InteropError;
use crate::generate::MXX_PIN;
use crate::programs::ElementSpec;

/// Fixed prelude: the lift definition and the dimension-independent lemmas
/// (all proven; shared by every recognized element).
const TRANSPORT_PRELUDE: &str = r#"import MxxIR
import MxxRuntime
import {element}.Native
import {element}.Stage

/-! Transport (generated): the native spec function refines the mxx stage
    relation. Wire names match both generators (`w_<node>_<port>`).

    The native helpers operate on nested `List Nat` (per-CRT-tower
    coefficient lists); the mxx relation speaks of `ExactMatrix q 1 r c`
    (mathlib matrices of `ZMod q`-valued constant polys, ring dimension 1).
    `liftMat` is the transport between them. Out-of-range reads take the
    `getElem!` defaults on both sides, so no length hypotheses are needed. -/

namespace {ns}

open Mxx.Primitives

/-- Lift a native matrix (rows→cols→towers→coeffs nested `Nat` residue
    lists) to mxx's exact model, at ring dimension 1 and a single CRT tower
    (constant polys in `ZMod q`). -/
noncomputable def liftMat {q : Nat} (m : List (List (List (List Nat)))) (r c : Nat) :
    ExactMatrix q 1 r c :=
  fun i j => algebraMap (ZMod q) _ (((m[i.val]!)[j.val]!)[0]!)[0]!

/-- `getElem!` over `List.ofFn`. -/
theorem getElem!_ofFn {α : Type} [Inhabited α] {n : Nat} (f : Fin n → α) (i : Nat) :
    (List.ofFn f)[i]! = if h : i < n then f ⟨i, h⟩ else default := by
  rw [List.getElem!_eq_getElem?_getD, List.getElem?_ofFn]
  split <;> rfl

/-- Cast of a reduced sum into `ZMod q`. -/
theorem zmod_cast_add_mod {q : Nat} [NeZero q] (x y : Nat) :
    (((x + y) % q : Nat) : ZMod q) = (x : ZMod q) + y := by
  rw [ZMod.natCast_mod, Nat.cast_add]

/-- Cast of a reduced product into `ZMod q`. -/
theorem zmod_cast_mul_mod {q : Nat} [NeZero q] (x y : Nat) :
    (((x * y) % q : Nat) : ZMod q) = (x : ZMod q) * y := by
  rw [ZMod.natCast_mod, Nat.cast_mul]

/-- The lifted head coefficient of `poly_add` at ring dimension 1. -/
theorem cast_poly_add_head {q : Nat} [NeZero q] (a b : List Nat) :
    (((poly_add (n := 1) a b q)[0]! : Nat) : ZMod q) = (a[0]! : ZMod q) + b[0]! := by
  simp only [poly_add, getElem!_ofFn, Nat.zero_lt_one, dif_pos]
  exact zmod_cast_add_mod _ _

/-- The lifted head coefficient of `poly_mul` at ring dimension 1
    (single coefficient: the convolution degenerates to one product). -/
theorem cast_poly_mul_head {q : Nat} [NeZero q] (a b : List Nat) :
    (((poly_mul (n := 1) a b q)[0]! : Nat) : ZMod q) = (a[0]! : ZMod q) * b[0]! := by
  simp only [poly_mul, getElem!_ofFn, Nat.zero_lt_one, dif_pos]
  -- the range' fold over [0] computes to the single i = 0 ≤ 0 term
  rw [show List.range' 0 (1 - 0) = [0] from rfl]
  simp only [List.foldl_cons, List.foldl_nil, Nat.le_refl, if_true]
  -- ↑(((0 + (a*b) % q) % q) % q) = ↑a * ↑b
  simp only [Nat.zero_add, Nat.mod_mod]
  exact zmod_cast_mul_mod _ _
"#;

/// A helper call found in the lowered element function.
#[derive(Clone, Debug)]
struct HelperCall {
    /// Which helper (`mat_add`, `mat_mul`, ...).
    helper: String,
    /// Const-generic arguments in order (e.g. [R, K, C, N, D] for mat_mul).
    const_args: Vec<usize>,
}

/// The analyzed shape of a lowered element.
struct ElementShape {
    /// Input names (from the mxx graph) with (rows, columns).
    inputs: Vec<(String, usize, usize)>,
    /// Output (rows, columns).
    output: (usize, usize),
    /// Helper calls in let order.
    calls: Vec<HelperCall>,
}

fn lean_ident(name: &str) -> String {
    let s: String = name
        .chars()
        .map(|c| if c.is_ascii_alphanumeric() || c == '_' { c } else { '_' })
        .collect();
    if s.chars().next().is_some_and(|c| c.is_ascii_digit()) {
        format!("x{s}")
    } else {
        s
    }
}

/// Analyze the validated graph + lowered module into the recognized
/// transport shape, or report why not.
fn analyze(
    graph: &Graph,
    validated: &ValidatedGraph,
    module: &IrModule<IrFunction>,
    fn_name: &str,
) -> Result<ElementShape, Vec<String>> {
    let mut gaps: Vec<String> = Vec::new();

    let root_id = FrozenGraphScopeId::Root;
    let Some(vscope) = validated.scope(&root_id) else {
        return Err(vec!["no root scope in validated graph".into()]);
    };
    let wire_ty = |w: &WireRef| vscope.wire_types.get(w);

    // Ring checks: every matrix wire must be ring-dim 1, single tower, and
    // all on one modulus.
    let mut moduli: Vec<Vec<u64>> = Vec::new();
    for (w, ty) in &vscope.wire_types {
        match ty {
            ConcreteWireType::Matrix(m) => {
                if m.ring.ring_dimension() != 1 {
                    gaps.push(format!("wire {w:?}: ring dimension {} ≠ 1", m.ring.ring_dimension()));
                }
                if m.ring.crt_depth() != 1 {
                    gaps.push(format!("wire {w:?}: CRT depth {} ≠ 1", m.ring.crt_depth()));
                }
                let ms = m.ring.crt_moduli().to_vec();
                if !moduli.contains(&ms) {
                    moduli.push(ms);
                }
            }
            _ => gaps.push(format!("wire {w:?}: non-matrix type {ty:?} not supported in transport v1")),
        }
    }
    if moduli.len() > 1 {
        gaps.push(format!("multiple rings: {moduli:?}"));
    }

    // Inputs: root-scope Input nodes in execution order.
    let scope = graph.root_scope();
    let mut inputs = Vec::new();
    for handle in &vscope.execution_order {
        if let NodeKind::Input { name, .. } = handle.kind() {
            let node_id = scope.node_id(handle).expect("frozen node id");
            let wire = WireRef { node: node_id, port: mxx_ir_core::types::Port(0) };
            match wire_ty(&wire) {
                Some(ConcreteWireType::Matrix(m)) => {
                    inputs.push((lean_ident(name), m.rows, m.columns));
                }
                _ => gaps.push(format!("input {name}: non-matrix input not supported in transport v1")),
            }
        }
    }

    // Outputs: exactly one, matrix.
    let mut output = (0, 0);
    if graph.outputs().len() != 1 {
        gaps.push(format!("{} outputs; transport v1 requires exactly one", graph.outputs().len()));
    } else if let Some((_name, root)) = graph.outputs().iter().next() {
        match wire_ty(&root.value) {
            Some(ConcreteWireType::Matrix(m)) => output = (m.rows, m.columns),
            _ => gaps.push("output: non-matrix output not supported in transport v1".into()),
        }
    }

    // Helper calls from the lowered function's lets, in order.
    let Some(f) = module.functions.iter().find(|f| f.name == fn_name) else {
        return Err(vec![format!("lowered module has no function {fn_name}")]);
    };
    let mut calls = Vec::new();
    for s in &f.body.stmts {
        let volar_compiler::IrStmtKind::Let { init: Some(init), .. } = &s.kind else {
            gaps.push("non-let statement in lowered element".into());
            continue;
        };
        match &init.kind {
            IrExprKind::Call { func, .. } => {
                let (name, type_args) = match &func.kind {
                    IrExprKind::Path { segments, type_args } => {
                        (segments.last().cloned().unwrap_or_default(), type_args.clone())
                    }
                    IrExprKind::Var(v) => (v.clone(), vec![]),
                    other => {
                        gaps.push(format!("call via {:?} not supported", std::mem::discriminant(other)));
                        continue;
                    }
                };
                let const_args: Option<Vec<usize>> = type_args
                    .iter()
                    .map(|a| match a {
                        volar_compiler::IrType::TypeParam(s) => s.parse::<usize>().ok(),
                        _ => None,
                    })
                    .collect();
                let Some(const_args) = const_args else {
                    gaps.push(format!("call to {name}: non-numeric const args"));
                    continue;
                };
                if name == "mat_mul" && const_args.get(1).is_some_and(|k| *k > 3) {
                    gaps.push(format!("mat_mul with K={} > 3 (Fin.sum_univ table)", const_args[1]));
                }
                if !matches!(name.as_str(), "mat_add" | "mat_mul") {
                    gaps.push(format!("helper {name} not covered by transport v1 (mat_add, mat_mul)"));
                }
                calls.push(HelperCall { helper: name, const_args });
            }
            other => gaps.push(format!(
                "non-call let initializer {:?} not supported in transport v1",
                std::mem::discriminant(other)
            )),
        }
    }

    if gaps.is_empty() {
        Ok(ElementShape { inputs, output, calls })
    } else {
        Err(gaps)
    }
}

const SUM_UNIV: [(&str, &str); 3] = [
    ("one", "[0]"),
    ("two", "[0, 1]"),
    ("three", "[0, 1, 2]"),
];

const FIN_VALS: [&str; 3] = ["Fin.val_zero", "Fin.val_one", "Fin.val_two"];

/// Lemma name for a helper call shape.
fn lemma_name(call: &HelperCall) -> String {
    match call.helper.as_str() {
        // mat_add const args: [R, C, N, D]
        "mat_add" => format!("liftMat_mat_add_r{}_c{}", call.const_args[0], call.const_args[1]),
        // mat_mul const args: [R, K, C, N, D]
        "mat_mul" => format!(
            "liftMat_mat_mul_r{}_k{}_c{}",
            call.const_args[0], call.const_args[1], call.const_args[2]
        ),
        other => unreachable!("analyze() rejects {other}"),
    }
}

/// Generate the transport Lean module for one element.
///
/// Returns either the module source or a `-- NOT-BRIDGED` inventory naming
/// every gap (the v1 recognized shape is documented at the module level).
pub fn generate_transport(
    element: &ElementSpec,
    graph: &Graph,
    validated: &ValidatedGraph,
    module: &IrModule<IrFunction>,
) -> String {
    let ns = format!("Mxx{}Transport", camel(&element.element));
    let header = format!(
        "-- @generated by `cargo xtask gen-specs` (volar-mxx-interop), mxx @ {MXX_PIN}.\n\
         -- DO NOT EDIT; regenerate. Transport proofs for element `{}`.\n\n",
        element.element
    );

    let shape = match analyze(graph, validated, module, element.function) {
        Ok(s) => s,
        Err(gaps) => {
            let mut out = header;
            out.push_str(&format!(
                "-- NOT-BRIDGED: element `{}` is outside the v1 transport shape.\n",
                element.element
            ));
            for gap in gaps {
                out.push_str(&format!("--   - {gap}\n"));
            }
            return out;
        }
    };

    let mut out = header;
    out.push_str(&TRANSPORT_PRELUDE.replace("{element}", &element.element).replace("{ns}", &ns));
    out.push('\n');

    // Per-call-shape correspondence lemmas (deduplicated).
    let mut emitted: Vec<String> = Vec::new();
    for call in &shape.calls {
        let name = lemma_name(call);
        if emitted.contains(&name) {
            continue;
        }
        emitted.push(name.clone());
        match call.helper.as_str() {
            "mat_add" => {
                let (r, c) = (call.const_args[0], call.const_args[1]);
                out.push_str(&format!(
                    "\n/-- `mat_add` correspondence at {r}×{c} (ring dim 1, tower [q]). -/\n\
                     theorem {name} {{q : Nat}} [NeZero q] (a b : List (List (List (List Nat)))) :\n    \
                     liftMat (mat_add (r := {r}) (c := {c}) (n := 1) (d := 1) a b [q]) {r} {c} =\n      \
                     (liftMat a {r} {c} + liftMat b {r} {c} : ExactMatrix q 1 {r} {c}) := by\n  \
                     funext i j\n  \
                     rw [Matrix.add_apply]\n  \
                     simp only [liftMat, mat_add, getElem!_ofFn, Fin.is_lt, Nat.zero_lt_one, dif_pos]\n  \
                     rw [show [q][0]! = q from rfl, cast_poly_add_head, map_add]\n"
                ));
            }
            "mat_mul" => {
                let (r, k, c) = (call.const_args[0], call.const_args[1], call.const_args[2]);
                let (sum_univ, range_list) = SUM_UNIV[k - 1];
                let fin_vals: String = FIN_VALS[..k].join(", ");
                out.push_str(&format!(
                    "\n/-- `mat_mul` correspondence at {r}×{k} times {k}×{c} (ring dim 1, tower [q]). -/\n\
                     theorem {name} {{q : Nat}} [NeZero q] (a b : List (List (List (List Nat)))) :\n    \
                     liftMat (mat_mul (r := {r}) (k := {k}) (c := {c}) (n := 1) (d := 1) a b [q]) {r} {c} =\n      \
                     (liftMat a {r} {k} * liftMat b {k} {c} : ExactMatrix q 1 {r} {c}) := by\n  \
                     funext i j\n  \
                     rw [Matrix.mul_apply, Fin.sum_univ_{sum_univ}]\n  \
                     simp only [liftMat, mat_mul, getElem!_ofFn, Fin.is_lt, Nat.zero_lt_one, dif_pos]\n  \
                     rw [show [q][0]! = q from rfl]\n  \
                     rw [show List.range' 0 ({k} - 0) = {range_list} from rfl]\n  \
                     simp only [List.foldl_cons, List.foldl_nil]\n  \
                     simp only [cast_poly_add_head, cast_poly_mul_head]\n  \
                     simp only [List.getElem!_eq_getElem?_getD, List.getElem?_replicate, Nat.zero_lt_one,\n    \
                       if_true, Option.getD_some, Nat.cast_zero, map_zero, zero_add, {fin_vals}]\n  \
                     simp only [← map_mul, ← map_add]\n"
                ));
            }
            other => unreachable!("analyze() rejects {other}"),
        }
    }

    // The refinement theorem.
    let stage_ns = format!("Stage_mxx_{}", element.element);
    let input_binders: String = shape
        .inputs
        .iter()
        .map(|(name, _, _)| format!("({name} : List (List (List (List Nat)))) "))
        .collect();
    // mxx's Stage inputs are `A × B × C × Unit`; Lean's flat tuple
    // notation `(x, y, z, ())` is exactly that right-nested form.
    let input_tuple: String = {
        let flat: Vec<String> = shape
            .inputs
            .iter()
            .map(|(name, r, c)| format!("liftMat {name} {r} {c}"))
            .collect();
        format!("({}, ())", flat.join(", "))
    };
    let (or, oc) = shape.output;
    let rw_chain: String = {
        // outermost (last let) first
        let mut names: Vec<String> = shape.calls.iter().map(lemma_name).collect();
        names.reverse();
        names.join(", ")
    };
    out.push_str(&format!(
        "\n/-- The element's native function refines the mxx stage relation:\n    \
         feeding the lifted inputs, the mxx relation accepts the lifted output. -/\n\
         theorem {}_refines {{q : Nat}} [NeZero q]\n    {input_binders}:\n    \
         {stage_ns}.generatedRoot ⟨()⟩\n      {input_tuple}\n      (liftMat ({} {}) {} {}) := by\n  \
         refine ⟨⟨⟩, ?_⟩\n  \
         simp only [{stage_ns}.generatedRoot.body, {stage_ns}.generatedRoot.constraints_0,\n    \
           {}, MXX_RING_0, MxxRuntime.matrixAdd, MxxRuntime.matrixMul]\n  \
         rw [{rw_chain}]\n\nend {ns}\n",
        element.function,
        element.function,
        shape.inputs.iter().map(|(n, _, _)| n.as_str()).collect::<Vec<_>>().join(" "),
        or,
        oc,
        element.function,
    ));
    out
}

/// Snake/camel-case a name for the Lean namespace.
fn camel(name: &str) -> String {
    let mut out = String::new();
    let mut up = true;
    for c in name.chars() {
        if c == '_' || c == '-' {
            up = true;
        } else if up {
            out.extend(c.to_uppercase());
            up = false;
        } else {
            out.push(c);
        }
    }
    out
}
