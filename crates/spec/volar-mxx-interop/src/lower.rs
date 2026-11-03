// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: generated
//! The lowerer: validated frozen mxx graph → `IrModule` spec IR.
//!
//! One `let` per frozen node, in validated execution order, mirroring the
//! wire naming of mxx's own Lean export (`w_<node>_<port>`) so the Lean
//! transport layer (Phase 2 of `docs/mxx-cutover-plan.md`) can align the
//! two without a name-mapping side channel.

use std::collections::BTreeSet;

use mxx_ir_core::expr::IntExpr;
use mxx_ir_core::graph::{Graph, GraphScope};
use mxx_ir_core::node::{ConstantMatrix, NodeKind};
use mxx_ir_core::types::{ConcreteMatrixType, ConcreteWireType, NodeId, WireRef};
use mxx_ir_core::validate::ValidatedGraph;
use num_bigint::BigInt;
use volar_compiler::{
    ArrayKind, ArrayLength, IrBlock, IrConst, IrExpr, IrExprKind, IrFunction, IrLit, IrModule,
    IrParam, IrPattern, IrStmt, IrStmtKind, IrType, PrimitiveType, SpecBinOp,
};

use crate::InteropError;

/// How a lowered module obtains the `mxx_rt` helper functions it calls.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum HelperMode {
    /// Parse the bundled `rust_rt/mxx_rt.rs` and inline the referenced
    /// helpers into the module, making it self-contained (used by the
    /// interop crate's own compile-and-run tests).
    Inline,
    /// Emit calls through an external path prefix (e.g. `["crate", "mxx_rt"]`
    /// for generated files living inside `volar-spec`, where the helpers
    /// exist once as reviewed spec source).
    External {
        /// Path segments prepended to every helper call.
        path: Vec<String>,
    },
}

/// Options for [`lower_validated_to_module`].
#[derive(Clone, Debug)]
pub struct LowerOptions {
    /// Name of the emitted top-level function.
    pub function_name: String,
    /// Helper provisioning mode.
    pub helpers: HelperMode,
    /// Name given to the emitted module.
    pub module_name: String,
}

/// A distinct concrete ring encountered while lowering, keyed by value.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
struct RingKey {
    /// CRT tower moduli.
    moduli: Vec<u64>,
    /// Negacyclic ring dimension.
    dimension: u32,
}

/// State for one lowering run.
struct Lowerer<'a> {
    validated: &'a ValidatedGraph,
    /// Distinct rings in first-use order; index = `MXX_RING_<i>` const.
    rings: Vec<RingKey>,
    /// Helper function names the lowered program calls (drives `Inline`).
    used_helpers: BTreeSet<String>,
}

/// `w_<node>_<port>` — the same wire naming mxx's Lean export uses.
fn wire_var(wire: &WireRef) -> String {
    format!("w_{}_{}", wire.node.0, wire.port.0)
}

fn expr<P: Clone + Default>(kind: IrExprKind<P>) -> IrExpr<P> {
    IrExpr::new(kind, P::default(), None)
}

fn stmt(kind: IrStmtKind) -> IrStmt {
    IrStmt::new(kind, (), None)
}

fn var(name: &str) -> IrExpr {
    expr(IrExprKind::Var(name.to_string()))
}

fn u64_ty() -> IrType {
    IrType::Primitive(PrimitiveType::U64)
}

fn fixed_array(elem: IrType, len: usize) -> IrType {
    IrType::Array {
        kind: ArrayKind::FixedArray,
        elem: Box::new(elem),
        len: ArrayLength::Const(len),
    }
}

/// The Rust type of a concrete mxx matrix: `[[[[u64; N]; D]; C]; R]`.
fn matrix_ir_type(matrix: &ConcreteMatrixType) -> IrType {
    let n = matrix.ring.ring_dimension() as usize;
    let d = matrix.ring.crt_depth();
    fixed_array(
        fixed_array(fixed_array(fixed_array(u64_ty(), n), d), matrix.columns),
        matrix.rows,
    )
}

/// A `usize` const-generic argument, represented the same way the parser
/// represents literal const generic arguments (`IrType::TypeParam("8")`).
fn const_arg(value: usize) -> IrType {
    IrType::TypeParam(value.to_string())
}

impl<'a> Lowerer<'a> {
    fn new(validated: &'a ValidatedGraph) -> Self {
        Self {
            validated,
            rings: Vec::new(),
            used_helpers: BTreeSet::new(),
        }
    }

    /// Register a concrete ring, returning its `MXX_RING_<i>` index.
    fn ring_index(&mut self, matrix: &ConcreteMatrixType) -> usize {
        let key = RingKey {
            moduli: matrix.ring.crt_moduli().to_vec(),
            dimension: matrix.ring.ring_dimension(),
        };
        if let Some(i) = self.rings.iter().position(|k| *k == key) {
            return i;
        }
        self.rings.push(key);
        self.rings.len() - 1
    }

    fn concrete_type(&self, scope: &GraphScope, wire: &WireRef) -> Result<&ConcreteWireType, InteropError> {
        let vscope = self
            .validated
            .scope(scope.id())
            .ok_or_else(|| InteropError::MissingWireType(format!("{wire:?}")))?;
        vscope
            .wire_types
            .get(wire)
            .ok_or_else(|| InteropError::MissingWireType(format!("{wire:?}")))
    }

    fn lower_wire_type(&mut self, scope: &GraphScope, wire: &WireRef) -> Result<IrType, InteropError> {
        let ty = self.concrete_type(scope, wire)?.clone();
        match &ty {
            ConcreteWireType::Matrix(matrix) => {
                self.ring_index(matrix);
                Ok(matrix_ir_type(matrix))
            }
            ConcreteWireType::Int | ConcreteWireType::ConstantInt => Ok(IrType::Primitive(PrimitiveType::I128)),
            ConcreteWireType::Bool | ConcreteWireType::ConstantBool => Ok(IrType::Primitive(PrimitiveType::Bool)),
            other => Err(InteropError::Unsupported {
                scope: format!("{:?}", scope.id()),
                node: wire.node.0,
                kind: format!("{other:?}"),
                detail: "wire type has no spec representation yet (bytes, reals, \
                           trapdoors, bounded matrices, families are Phase-1+)"
                    .into(),
            }),
        }
    }

    /// Evaluate a compile-time `IntExpr` under the graph's bindings.
    fn eval_int(&self, expr: &IntExpr) -> Result<BigInt, InteropError> {
        expr.evaluate(&self.validated.bindings)
            .map_err(|e| InteropError::Expr(format!("{e:?}")))
    }

    fn int_lit(&self, value: &BigInt) -> Result<IrExpr, InteropError> {
        let v: i128 = value
            .try_into()
            .map_err(|_| InteropError::OversizedInt(value.to_string()))?;
        Ok(expr(IrExprKind::Lit(IrLit::Int(v))))
    }

    /// A call to an `mxx_rt` helper, recorded for `HelperMode::Inline`.
    fn helper_call(
        &mut self,
        mode: &HelperMode,
        name: &str,
        const_args: &[usize],
        args: Vec<IrExpr>,
    ) -> IrExpr {
        self.used_helpers.insert(name.to_string());
        let segments = match mode {
            HelperMode::Inline => vec![name.to_string()],
            HelperMode::External { path } => {
                let mut p = path.clone();
                p.push(name.to_string());
                p
            }
        };
        expr(IrExprKind::Call {
            func: Box::new(expr(IrExprKind::Path {
                segments,
                type_args: const_args.iter().map(|a| const_arg(*a)).collect(),
            })),
            args,
        })
    }

    fn ring_const_expr(&self, index: usize) -> IrExpr {
        expr(IrExprKind::Path {
            segments: vec![format!("MXX_RING_{index}")],
            type_args: vec![],
        })
    }

    fn unsupported<T>(&self, scope: &GraphScope, node: NodeId, kind: &NodeKind, detail: &str) -> Result<T, InteropError> {
        Err(InteropError::Unsupported {
            scope: format!("{:?}", scope.id()),
            node: node.0,
            kind: format!("{kind:?}"),
            detail: detail.into(),
        })
    }

    /// Lower one node to zero or more statements. Returns `Ok(None)` for
    /// nodes that need no statement (root inputs are function params).
    fn lower_node(
        &mut self,
        scope: &GraphScope,
        node: NodeId,
        kind: &NodeKind,
        args: &[WireRef],
        mode: &HelperMode,
    ) -> Result<Option<IrStmt>, InteropError> {
        let out_wire = WireRef {
            node,
            port: mxx_ir_core::types::Port(0),
        };
        let out_name = wire_var(&out_wire);
        let arg_var = |i: usize| -> Result<String, InteropError> {
            args.get(i)
                .map(wire_var)
                .ok_or_else(|| InteropError::MissingWireType(format!("node {node:?} argument {i}")))
        };
        let let_stmt = |ty: IrType, init: IrExpr| {
            stmt(IrStmtKind::Let {
                pattern: IrPattern::ident(out_name.clone()),
                ty: Some(ty),
                init: Some(init),
            })
        };

        match kind {
            NodeKind::Input { .. } => Ok(None),
            NodeKind::ConstantInt(value) => {
                let ty = self.lower_wire_type(scope, &out_wire)?;
                let lit = self.int_lit(value)?;
                Ok(Some(let_stmt(ty, lit)))
            }
            NodeKind::ConstantBool(value) => {
                let ty = self.lower_wire_type(scope, &out_wire)?;
                Ok(Some(let_stmt(ty, expr(IrExprKind::Lit(IrLit::Bool(*value))))))
            }
            NodeKind::EvaluateInt(e) => {
                let value = self.eval_int(e)?;
                let ty = self.lower_wire_type(scope, &out_wire)?;
                let lit = self.int_lit(&value)?;
                Ok(Some(let_stmt(ty, lit)))
            }
            NodeKind::IntBinary(op) => {
                let ty = self.lower_wire_type(scope, &out_wire)?;
                let op = match op {
                    mxx_ir_core::node::IntBinaryOp::Add => SpecBinOp::Add,
                    mxx_ir_core::node::IntBinaryOp::Subtract => SpecBinOp::Sub,
                    mxx_ir_core::node::IntBinaryOp::Multiply => SpecBinOp::Mul,
                    mxx_ir_core::node::IntBinaryOp::Divide => SpecBinOp::Div,
                    mxx_ir_core::node::IntBinaryOp::Remainder => SpecBinOp::Rem,
                };
                Ok(Some(let_stmt(
                    ty,
                    expr(IrExprKind::Binary {
                        op,
                        left: Box::new(var(&arg_var(0)?)),
                        right: Box::new(var(&arg_var(1)?)),
                    }),
                )))
            }
            NodeKind::IntCompare(op) => {
                let ty = self.lower_wire_type(scope, &out_wire)?;
                let op = match op {
                    mxx_ir_core::node::IntCompareOp::Equal => SpecBinOp::Eq,
                    mxx_ir_core::node::IntCompareOp::Less => SpecBinOp::Lt,
                    mxx_ir_core::node::IntCompareOp::LessEqual => SpecBinOp::Le,
                };
                Ok(Some(let_stmt(
                    ty,
                    expr(IrExprKind::Binary {
                        op,
                        left: Box::new(var(&arg_var(0)?)),
                        right: Box::new(var(&arg_var(1)?)),
                    }),
                )))
            }
            NodeKind::BoolToInt => {
                let ty = self.lower_wire_type(scope, &out_wire)?;
                Ok(Some(let_stmt(
                    ty,
                    expr(IrExprKind::If {
                        cond: Box::new(var(&arg_var(0)?)),
                        then_branch: IrBlock {
                            stmts: vec![],
                            expr: Some(Box::new(expr(IrExprKind::Lit(IrLit::Int(1))))),
                        },
                        else_branch: Some(Box::new(expr(IrExprKind::Block(IrBlock {
                            stmts: vec![],
                            expr: Some(Box::new(expr(IrExprKind::Lit(IrLit::Int(0))))),
                        })))),
                    }),
                )))
            }
            NodeKind::ConstantMatrix { value, .. } => {
                self.lower_constant_matrix(scope, node, &out_wire, value, mode).map(Some)
            }
            NodeKind::MatrixBinary(op) => {
                let matrix = self.matrix_type_of(scope, &out_wire, "MatrixBinary")?;
                self.ring_index(&matrix);
                let (helper, const_args) = match op {
                    mxx_ir_core::node::MatrixBinaryOp::Add => (
                        "mat_add",
                        vec![matrix.rows, matrix.columns, matrix.ring.ring_dimension() as usize, matrix.ring.crt_depth()],
                    ),
                    mxx_ir_core::node::MatrixBinaryOp::Subtract => (
                        "mat_sub",
                        vec![matrix.rows, matrix.columns, matrix.ring.ring_dimension() as usize, matrix.ring.crt_depth()],
                    ),
                    mxx_ir_core::node::MatrixBinaryOp::Multiply => {
                        let lhs = self.matrix_of_arg(scope, args, 0, "MatrixBinary::Multiply")?;
                        (
                            "mat_mul",
                            vec![
                                lhs.rows,
                                lhs.columns,
                                matrix.columns,
                                matrix.ring.ring_dimension() as usize,
                                matrix.ring.crt_depth(),
                            ],
                        )
                    }
                };
                let ring = self.ring_index(&matrix);
                let call = self.helper_call(
                    mode,
                    helper,
                    &const_args,
                    vec![var(&arg_var(0)?), var(&arg_var(1)?), self.ring_const_expr(ring)],
                );
                let ty = self.lower_wire_type(scope, &out_wire)?;
                Ok(Some(let_stmt(ty, call)))
            }
            NodeKind::MatrixMulSmallRhs => {
                // Execution fusion; semantics are ordinary matrix multiply
                // (mxx's own Lean export lowers it identically).
                let matrix = self.matrix_type_of(scope, &out_wire, "MatrixMulSmallRhs")?;
                let lhs = self.matrix_of_arg(scope, args, 0, "MatrixMulSmallRhs")?;
                let ring = self.ring_index(&matrix);
                let call = self.helper_call(
                    mode,
                    "mat_mul",
                    &[
                        lhs.rows,
                        lhs.columns,
                        matrix.columns,
                        matrix.ring.ring_dimension() as usize,
                        matrix.ring.crt_depth(),
                    ],
                    vec![var(&arg_var(0)?), var(&arg_var(1)?), self.ring_const_expr(ring)],
                );
                let ty = self.lower_wire_type(scope, &out_wire)?;
                Ok(Some(let_stmt(ty, call)))
            }
            NodeKind::MatrixNegate => {
                let matrix = self.matrix_type_of(scope, &out_wire, "MatrixNegate")?;
                let ring = self.ring_index(&matrix);
                let call = self.helper_call(
                    mode,
                    "mat_neg",
                    &[matrix.rows, matrix.columns, matrix.ring.ring_dimension() as usize, matrix.ring.crt_depth()],
                    vec![var(&arg_var(0)?), self.ring_const_expr(ring)],
                );
                let ty = self.lower_wire_type(scope, &out_wire)?;
                Ok(Some(let_stmt(ty, call)))
            }
            NodeKind::MatrixScale { scalar } => {
                let matrix = self.matrix_type_of(scope, &out_wire, "MatrixScale")?;
                let ring = self.ring_index(&matrix);
                let scalar = self.eval_int(scalar)?;
                // Reduce the (possibly negative or oversized) scalar into
                // each tower's canonical residue.
                let per_tower: Vec<IrExpr> = matrix
                    .ring
                    .crt_moduli()
                    .iter()
                    .map(|p| {
                        let p = BigInt::from(*p);
                        let r = ((scalar.clone() % &p) + &p) % &p;
                        self.int_lit(&r)
                    })
                    .collect::<Result<_, _>>()?;
                let s_table = expr(IrExprKind::FixedArray(per_tower));
                let call = self.helper_call(
                    mode,
                    "mat_scale",
                    &[matrix.rows, matrix.columns, matrix.ring.ring_dimension() as usize, matrix.ring.crt_depth()],
                    vec![var(&arg_var(0)?), s_table, self.ring_const_expr(ring)],
                );
                let ty = self.lower_wire_type(scope, &out_wire)?;
                Ok(Some(let_stmt(ty, call)))
            }
            NodeKind::Transpose => {
                let matrix = self.matrix_type_of(scope, &out_wire, "Transpose")?;
                // Output is C x R; input is R x C.
                let input = self.matrix_of_arg(scope, args, 0, "Transpose")?;
                let _ = matrix;
                let call = self.helper_call(
                    mode,
                    "mat_transpose",
                    &[input.rows, input.columns, input.ring.ring_dimension() as usize, input.ring.crt_depth()],
                    vec![var(&arg_var(0)?)],
                );
                let ty = self.lower_wire_type(scope, &out_wire)?;
                Ok(Some(let_stmt(ty, call)))
            }
            other => self.unsupported(
                scope,
                node,
                other,
                "node kind not covered by the Phase-0 lowerer (samplers, loops, \
                 families, gadget/trapdoor machinery, modulus switching, bytes/hash)",
            ),
        }
    }

    /// The concrete matrix type of a wire, or a loud error.
    fn matrix_type_of(
        &self,
        scope: &GraphScope,
        wire: &WireRef,
        kind_desc: &str,
    ) -> Result<ConcreteMatrixType, InteropError> {
        match self.concrete_type(scope, wire)? {
            ConcreteWireType::Matrix(matrix) => Ok(matrix.clone()),
            other => Err(InteropError::Unsupported {
                scope: format!("{:?}", scope.id()),
                node: wire.node.0,
                kind: kind_desc.into(),
                detail: format!("expected a matrix wire, found {other:?}"),
            }),
        }
    }

    /// The concrete matrix type of argument `i`.
    fn matrix_of_arg(
        &self,
        scope: &GraphScope,
        args: &[WireRef],
        i: usize,
        kind_desc: &str,
    ) -> Result<ConcreteMatrixType, InteropError> {
        let wire = args.get(i).ok_or_else(|| {
            InteropError::MissingWireType(format!("node argument {i} for {kind_desc}"))
        })?;
        self.matrix_type_of(scope, wire, kind_desc)
    }

    /// Lower a `ConstantMatrix` node.
    fn lower_constant_matrix(
        &mut self,
        scope: &GraphScope,
        node: NodeId,
        out_wire: &WireRef,
        value: &ConstantMatrix,
        mode: &HelperMode,
    ) -> Result<IrStmt, InteropError> {
        let kind_desc = format!("ConstantMatrix::{value:?}");
        let matrix = self.matrix_type_of(scope, out_wire, &kind_desc)?;
        let out_name = wire_var(out_wire);
        let ty = matrix_ir_type(&matrix);
        let let_stmt = |init: IrExpr| {
            stmt(IrStmtKind::Let {
                pattern: IrPattern::ident(out_name.clone()),
                ty: Some(ty.clone()),
                init: Some(init),
            })
        };

        match value {
            ConstantMatrix::Zero => {
                let zero = expr(IrExprKind::Lit(IrLit::Int(0)));
                let init = expr(IrExprKind::Repeat {
                    elem: Box::new(expr(IrExprKind::Repeat {
                        elem: Box::new(expr(IrExprKind::Repeat {
                            elem: Box::new(expr(IrExprKind::Repeat {
                                elem: Box::new(zero),
                                len: Box::new(expr(IrExprKind::Lit(IrLit::Int(matrix.ring.ring_dimension() as i128)))),
                            })),
                            len: Box::new(expr(IrExprKind::Lit(IrLit::Int(matrix.ring.crt_depth() as i128)))),
                        })),
                        len: Box::new(expr(IrExprKind::Lit(IrLit::Int(matrix.columns as i128)))),
                    })),
                    len: Box::new(expr(IrExprKind::Lit(IrLit::Int(matrix.rows as i128)))),
                });
                Ok(let_stmt(init))
            }
            ConstantMatrix::Identity if matrix.rows == matrix.columns => {
                let call = self.helper_call(
                    mode,
                    "mat_identity",
                    &[matrix.rows, matrix.ring.ring_dimension() as usize, matrix.ring.crt_depth()],
                    vec![],
                );
                Ok(let_stmt(call))
            }
            ConstantMatrix::Polynomial { coefficients } if matrix.rows == 1 && matrix.columns == 1 => {
                let n = matrix.ring.ring_dimension() as usize;
                if coefficients.len() > n {
                    return Err(InteropError::Unsupported {
                        scope: format!("{:?}", scope.id()),
                        node: node.0,
                        kind: kind_desc.clone(),
                        detail: "polynomial constant has more coefficients than the ring dimension".into(),
                    });
                }
                // Evaluate each coefficient, then reduce canonically into
                // every CRT tower; missing coefficients are zero.
                let mut coeffs = Vec::with_capacity(n);
                for c in coefficients {
                    coeffs.push(self.eval_int(c)?);
                }
                coeffs.resize(n, BigInt::from(0));
                let towers: Vec<IrExpr> = matrix
                    .ring
                    .crt_moduli()
                    .iter()
                    .map(|p| {
                        let p_big = BigInt::from(*p);
                        let tower: Vec<IrExpr> = coeffs
                            .iter()
                            .map(|c| {
                                let r = ((c.clone() % &p_big) + &p_big) % &p_big;
                                self.int_lit(&r)
                            })
                            .collect::<Result<_, _>>()?;
                        Ok(expr(IrExprKind::FixedArray(tower)))
                    })
                    .collect::<Result<_, InteropError>>()?;
                let poly = expr(IrExprKind::FixedArray(towers));
                let col = expr(IrExprKind::FixedArray(vec![poly]));
                let init = expr(IrExprKind::FixedArray(vec![col]));
                Ok(let_stmt(init))
            }
            _ => Err(InteropError::Unsupported {
                scope: format!("{:?}", scope.id()),
                node: node.0,
                kind: kind_desc,
                detail: "constant payload or shape not covered yet (gadget, unit vectors, \
                         power-of-base, rotation, non-scalar polynomials)"
                    .into(),
            }),
        }
    }
}

/// Lower a validated frozen mxx graph into a spec-IR module with one public
/// function computing the graph's named outputs from its root inputs.
///
/// The function's parameters are named `w_<node>_<port>` after the root
/// input nodes (matching mxx's Lean export wire names); its return type is
/// the single output's type, or a tuple of output types in
/// (`BTreeMap`) name order.
pub fn lower_validated_to_module(
    graph: &Graph,
    validated: &ValidatedGraph,
    options: &LowerOptions,
) -> Result<IrModule<IrFunction>, InteropError> {
    // Phase 0: root scope only.
    let non_root: Vec<String> = graph
        .scopes()
        .keys()
        .filter(|id| !matches!(id, mxx_ir_core::graph::FrozenGraphScopeId::Root))
        .map(|id| format!("{id:?}"))
        .collect();
    if !non_root.is_empty() {
        return Err(InteropError::NonRootScopes(non_root));
    }

    let scope = graph.root_scope();
    let vscope = validated.root_scope();
    let mut lowerer = Lowerer::new(validated);

    // ── Parameters from root input nodes (execution order) ─────────────
    // Graph inputs are `Input` nodes in the root scope; `scope.inputs()`
    // only lists captured wires of non-root scopes and is empty at root.
    let mut params = Vec::new();
    for handle in &vscope.execution_order {
        if let NodeKind::Input { name, artifact, .. } = handle.kind() {
            if artifact.is_some() {
                return Err(InteropError::ArtifactInput { name: name.clone() });
            }
            let node_id = scope
                .node_id(handle)
                .ok_or_else(|| InteropError::MissingWireType(format!("handle {handle:?}")))?;
            let wire = WireRef {
                node: node_id,
                port: mxx_ir_core::types::Port(0),
            };
            let ty = lowerer.lower_wire_type(scope, &wire)?;
            params.push(IrParam {
                name: wire_var(&wire),
                ty,
            });
        }
    }

    // ── Body ────────────────────────────────────────────────────────────
    let mut stmts = Vec::new();
    for handle in &vscope.execution_order {
        let node_id = scope
            .node_id(handle)
            .ok_or_else(|| InteropError::MissingWireType(format!("handle {handle:?}")))?;
        let args = scope
            .arguments(handle)
            .ok_or_else(|| InteropError::MissingWireType(format!("arguments of {node_id:?}")))?;
        if let Some(s) = lowerer.lower_node(scope, node_id, handle.kind(), &args, &options.helpers)? {
            stmts.push(s);
        }
    }

    // ── Outputs (BTreeMap: deterministic name order) ────────────────────
    let mut output_exprs = Vec::new();
    let mut output_types = Vec::new();
    for (name, root) in graph.outputs() {
        let ty = lowerer
            .lower_wire_type(scope, &root.value)
            .map_err(|_| InteropError::MissingOutput { name: name.clone() })?;
        output_exprs.push(var(&wire_var(&root.value)));
        output_types.push(ty);
    }
    if output_exprs.is_empty() {
        return Err(InteropError::MissingOutput {
            name: "<graph has no named outputs>".into(),
        });
    }
    let (ret_ty, ret_expr) = if output_exprs.len() == 1 {
        (output_types.remove(0), output_exprs.remove(0))
    } else {
        (
            IrType::Tuple(output_types),
            expr(IrExprKind::Tuple(output_exprs)),
        )
    };

    let function = IrFunction {
        name: options.function_name.clone(),
        module_path: vec![],
        generics: vec![],
        receiver: None,
        params,
        return_type: Some(ret_ty),
        where_clause: vec![],
        body: IrBlock {
            stmts,
            expr: Some(Box::new(ret_expr)),
        },
        external_kind: volar_compiler::ExternalKind::Normal,
        no_inline: false,
    };

    // ── Ring constants ──────────────────────────────────────────────────
    let consts: Vec<IrConst> = lowerer
        .rings
        .iter()
        .enumerate()
        .map(|(i, key)| IrConst {
            name: format!("MXX_RING_{i}"),
            module_path: vec![],
            ty: fixed_array(u64_ty(), key.moduli.len()),
            value: expr(IrExprKind::FixedArray(
                key.moduli
                    .iter()
                    .map(|p| expr(IrExprKind::Lit(IrLit::Int(*p as i128))))
                    .collect(),
            )),
        })
        .collect();

    // ── Helpers (inline mode) ───────────────────────────────────────────
    // Helpers call each other (mat_* → poly_*), and the lowerer only tracks
    // direct calls; rather than maintaining a fragile transitive closure,
    // inline mode includes every helper. They are small and unused ones are
    // dead-code-eliminated by rustc (they carry no side effects or state).
    let mut functions = Vec::new();
    if options.helpers == HelperMode::Inline {
        functions.extend(crate::rt::helper_functions()?);
    }
    let _ = &lowerer.used_helpers;
    functions.push(function);

    Ok(IrModule {
        name: options.module_name.clone(),
        consts,
        functions,
        ..Default::default()
    })
}
