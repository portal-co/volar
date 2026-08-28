// @reliability: normal
// @ai: assisted
//! Lower `IrModule` (spec/compiler IR) to any `LirTarget`.
//!
//! Works best on monomorphized IR — run `monomorphize_module` first for any
//! module that uses generic type/length parameters.
//!
//! Handles: primitive types, arrays, structs, binary/unary ops, variables,
//! literals, if/else, let-bindings, field access, array indexing,
//! struct construction, BoundedLoop, ArrayGenerate, RawMap, RawZip,
//! `.clone()` (identity), crypto method calls (→ extern), call_extern.

pub mod mono;
pub mod structs;

use std::sync::LazyLock;

use std::collections::BTreeMap;
use volar_compiler::ir::{
    ArrayKind, ArrayLength, ExternalKind, IrAnyFunction, IrBlock, IrCfgFunction, IrCfgJump,
    IrCfgModule, IrCfgTerminator, IrExpr, IrExprKind, IrFunction, IrLit, IrModule, IrPattern,
    IrStmt, IrStmtKind, IrType, MethodKind, PrimitiveType, ReentryHint, SpecBinOp, SpecUnaryOp,
    StdMethod, StructKind,
};
use volar_lir::{BranchTarget, IcmpPred, LirTarget, LirType};

use mono::{
    FunctionInstanceKey, MonoEnv, MonoPlan, mono_len, mono_type, normalized_args, type_args_to_len,
    typenum_usize,
};
use structs::{
    EnumRegistry, StructRegistry, flatten_count, flatten_scalar_types, primitive_to_lir,
    struct_field_scalar_offset, struct_field_scalar_width,
};
use volar_compiler::ir::IrEnum;

pub use mono::{MonoError, plan_flat_module};

// Unwrap a single-element Vec into a scalar, panicking if the vec has != 1 element.
fn into_scalar<V: Clone>(vals: Vec<V>, context: &str) -> V {
    vals.into_iter()
        .next()
        .unwrap_or_else(|| panic!("expected scalar at {context}, got 0 values"))
}

/// Resolve `N::USIZE` / typenum markers via const_params, type_params, or `U{n}`.
fn resolve_usize_param(name: &str, env: &MonoEnv) -> Option<usize> {
    if let Some(&n) = env.const_params.get(name) {
        return Some(n);
    }
    if let Some(concrete) = env.type_params.get(name) {
        match mono_len(&type_args_to_len(Some(concrete), env), env) {
            ArrayLength::Const(n) => return Some(n),
            ArrayLength::TypeNum(tn) => return Some(tn.to_usize()),
            _ => {}
        }
    }
    typenum_usize(name)
}

// ============================================================================
// Type mapping
// ============================================================================

/// Convert an `IrType` to `LirType` without a struct registry (primitives only),
/// applying `env` substitutions on the fly.
fn ir_type_to_lir_prim(ty: &IrType, env: &MonoEnv) -> LirType {
    let ty = &mono_type(ty, env);
    match ty {
        IrType::Primitive(p) => primitive_to_lir(*p),
        IrType::Unit => LirType::Bool,
        IrType::Reference { elem, .. } => ir_type_to_lir_prim(elem, env),
        IrType::Array { elem, len, .. } => {
            let n = const_len(len, env);
            LirType::Arr(Box::new(ir_type_to_lir_prim(elem, env)), n)
        }
        other => unimplemented!("ir_type_to_lir_prim: unsupported type {:?}", other),
    }
}

fn const_len(len: &ArrayLength, env: &MonoEnv) -> usize {
    match mono_len(len, env) {
        ArrayLength::Const(n) => n,
        ArrayLength::TypeNum(tn) => tn.to_usize(),
        ArrayLength::TypeParam(name) => {
            panic!("unsubstituted TypeParam length '{name}' — add it to MonoEnv")
        }
        ArrayLength::Projection { .. } => unimplemented!("Projection length in lowering"),
    }
}

// ============================================================================
// External-function dispatch info
// ============================================================================

/// Metadata for a function annotated `#[oracle]`, `#[action]`, or `#[rng]`.
/// Built once per module and consulted at each call site during lowering.
struct ExternalFnInfo {
    kind: ExternalKind,
    /// LIR types of the declared parameters (for arg-type passing to `LirTarget`).
    param_tys: Vec<LirType>,
    /// LIR return type (single element; multi-output uses multiple entries).
    return_type: Option<LirType>,
}

/// Signature metadata for any function in the module (normal or external).
/// Used to infer return types at call sites.
struct FuncSigInfo {
    /// LIR types of the declared parameters.
    #[allow(dead_code)]
    param_tys: Vec<LirType>,
    /// LIR return type.
    return_type: Option<LirType>,
}

// ============================================================================
// Lowering context
// ============================================================================

struct LowerCtx<'t, T: LirTarget<P>, P: Clone = ()> {
    target: &'t mut T,
    /// Variable name → flat scalar SSA values.
    env: BTreeMap<String, Vec<T::Value>>,
    /// Variable name → IrType (for field/index type inference).
    env_types: BTreeMap<String, IrType>,
    /// The block we are currently emitting into.
    current_block: T::Block,
    /// Struct registry (IDs, field names, field types).
    registry: &'t StructRegistry,
    /// Enum registry (variant discriminants, payload widths).
    enum_registry: &'t EnumRegistry,
    /// Monomorphization environment: concrete lengths and hash suffix.
    mono: &'t MonoEnv,
    /// Original IrModule struct defs (for field type lookup).
    module_structs: &'t [volar_compiler::ir::IrStruct],
    /// Original IrModule enum defs (reserved for future variant lookup refinements).
    #[allow(dead_code)]
    module_enums: &'t [IrEnum],
    /// Functions annotated `#[oracle]`, `#[action]`, or `#[rng]`.
    /// Populated from the module at `lower_module_with_opts` time.
    external_fns: &'t BTreeMap<String, ExternalFnInfo>,
    /// Signatures of all functions in the module (for return-type inference
    /// at call sites).  Populated by `lower_module_with_opts`.
    func_sigs: &'t BTreeMap<String, FuncSigInfo>,
    /// IR-level return types for functions, for type inference on Call exprs.
    ir_func_ret_types: &'t BTreeMap<String, IrType>,
    /// Current concrete local function and the closed specialization plan.
    current_instance: Option<&'t FunctionInstanceKey>,
    mono_plan: Option<&'t MonoPlan<P>>,
    _p: std::marker::PhantomData<P>,
}

impl<'t, T: LirTarget<P>, P: Clone> LowerCtx<'t, T, P> {
    fn new(
        target: &'t mut T,
        entry: T::Block,
        params: Vec<(String, Vec<T::Value>, IrType)>,
        registry: &'t StructRegistry,
        enum_registry: &'t EnumRegistry,
        mono: &'t MonoEnv,
        module_structs: &'t [volar_compiler::ir::IrStruct],
        module_enums: &'t [IrEnum],
    ) -> Self {
        let mut env = BTreeMap::new();
        let mut env_types = BTreeMap::new();
        for (name, vals, ty) in params {
            env.insert(name.clone(), vals);
            env_types.insert(name, ty);
        }
        LowerCtx {
            target,
            env,
            env_types,
            current_block: entry,
            registry,
            enum_registry,
            mono,
            module_structs,
            module_enums,
            external_fns: &EMPTY_EXTERNAL_FNS,
            func_sigs: &EMPTY_FUNC_SIGS,
            ir_func_ret_types: &EMPTY_IR_RET_TYPES,
            current_instance: None,
            mono_plan: None,
            _p: std::marker::PhantomData,
        }
    }

    /// Infer the IrType of an expression using env_types and module struct defs.
    /// Returns `None` for unsupported or uninferable expressions.
    fn infer_type(&self, expr: &IrExpr<P>) -> Option<IrType> {
        match &expr.kind {
            IrExprKind::Var(name) => self.env_types.get(name).cloned(),

            IrExprKind::Field { base, field } => {
                let base_ty = self.infer_type(base)?;
                // Handle tuple field access (e.g., `.0`, `.1`).
                if let IrType::Tuple(elems) = &base_ty {
                    let idx: usize = field.parse().ok()?;
                    return elems.get(idx).cloned();
                }
                let (struct_kind, type_args) = match &base_ty {
                    IrType::Struct { kind, type_args } => (kind, type_args.as_slice()),
                    IrType::Reference { elem, .. } => match elem.as_ref() {
                        IrType::Struct { kind, type_args } => (kind, type_args.as_slice()),
                        _ => return None,
                    },
                    _ => return None,
                };
                let ir_struct = self
                    .module_structs
                    .iter()
                    .find(|s| s.kind == *struct_kind)?;
                let field_def = ir_struct.fields.iter().find(|f| &f.name == field)?;
                // Substitute the struct's own generics from the use-site type args.
                let mut local = MonoEnv::new(self.mono.hash_suffix.clone());
                for (parameter, argument) in ir_struct.generics.iter().zip(type_args.iter()) {
                    match parameter.kind {
                        volar_compiler::ir::IrGenericParamKind::Type => {
                            local
                                .type_params
                                .insert(parameter.name.clone(), mono_type(argument, self.mono));
                        }
                        volar_compiler::ir::IrGenericParamKind::Const => {
                            if let ArrayLength::Const(n) =
                                mono_len(&type_args_to_len(Some(argument), self.mono), self.mono)
                            {
                                local.const_params.insert(parameter.name.clone(), n);
                            }
                        }
                        volar_compiler::ir::IrGenericParamKind::Lifetime => {}
                    }
                }
                Some(mono_type(&field_def.ty, &local))
            }

            IrExprKind::Index { base, .. } => {
                let base_ty = self.infer_type(base)?;
                match &base_ty {
                    IrType::Array { elem, .. } => Some(*elem.clone()),
                    // Reference<Array<T>> (including slices): element type is T
                    IrType::Reference { elem, .. } => match elem.as_ref() {
                        IrType::Array { elem: inner, .. } => Some(*inner.clone()),
                        _ => None,
                    },
                    // Box<Array<T>> (see `volar_compiler::ir::box_type`'s
                    // own doc, and `is_slice_ref`/`slice_ref_elem`'s Box
                    // arm below): element type is T, same as a reference.
                    _ => match volar_compiler::ir::as_box_type(&base_ty) {
                        Some(IrType::Array { elem: inner, .. }) => Some(*inner.clone()),
                        _ => None,
                    },
                }
            }

            IrExprKind::MethodCall {
                receiver,
                method: MethodKind::Known(StdMethod::Clone | StdMethod::Deref),
                ..
            } => self.infer_type(receiver),

            // User-defined method: resolve through the monomorphization plan
            // (the receiver's type is the method's receiver type, not the
            // result type).
            IrExprKind::MethodCall {
                receiver,
                method: MethodKind::Other(name),
                type_args,
                args,
                ..
            } => {
                if let (Some(plan), Some(caller)) = (self.mono_plan, self.current_instance) {
                    let generic_key = normalized_args(type_args, self.mono);
                    let arg_types_key: String = std::iter::once(receiver.as_ref())
                        .chain(args.iter())
                        .map(|a| {
                            self.infer_type(a)
                                .map(|ty| {
                                    crate::mono::canonical_type(&crate::mono::mono_type(
                                        &ty, self.mono,
                                    ))
                                })
                                .unwrap_or_default()
                        })
                        .collect::<Vec<_>>()
                        .join("|");
                    // Keys match the planner's canonical arg-types string
                    // (no generic prefix; see mono.rs calls insert).
                    let args_key = arg_types_key;
                    if let Some(callee) = plan.local_call_deduce(caller, name, &args_key) {
                        return self
                            .ir_func_ret_types
                            .get(plan.emitted_name(callee))
                            .cloned();
                    }
                }
                self.ir_func_ret_types.get(name).cloned()
            }

            IrExprKind::Unary {
                op: SpecUnaryOp::Ref | SpecUnaryOp::RefMut,
                expr: inner,
            } => self.infer_type(inner),

            // Operator overload on a struct with a declared `Mul` impl
            // (e.g. `let q = vope * delta;`): the result is the impl method's
            // return type — resolve through the plan just like a method call,
            // with the same `mul__<StructKind>` dispatch key as lowering.
            IrExprKind::Binary {
                op: SpecBinOp::Mul,
                left,
                right,
            } => {
                let is_mul_impl = self
                    .infer_type(left)
                    .map(|ty| match &ty {
                        IrType::Struct { kind, .. } => self
                            .mono_plan
                            .map(|p| p.mul_impl_structs.contains_key(&kind.to_string()))
                            .unwrap_or(false),
                        _ => false,
                    })
                    .unwrap_or(false);
                if !is_mul_impl {
                    return None;
                }
                let callee = match self.infer_type(left).as_ref() {
                    Some(IrType::Struct { kind, .. }) => {
                        format!("mul__{}", crate::structs::kind_name(kind))
                    }
                    _ => return None,
                };
                if let (Some(plan), Some(caller)) = (self.mono_plan, self.current_instance) {
                    let arg_types_key: String = std::iter::once(left.as_ref())
                        .chain(std::iter::once(right.as_ref()))
                        .map(|a| {
                            self.infer_type(a)
                                .map(|ty| {
                                    crate::mono::canonical_type(&crate::mono::mono_type(
                                        &ty, self.mono,
                                    ))
                                })
                                .unwrap_or_default()
                        })
                        .collect::<Vec<_>>()
                        .join("|");
                    if let Some(found) = plan.local_call_deduce(caller, &callee, &arg_types_key) {
                        return self
                            .ir_func_ret_types
                            .get(plan.emitted_name(found))
                            .cloned();
                    }
                }
                self.ir_func_ret_types.get(&callee).cloned()
            }

            IrExprKind::Unary {
                op: SpecUnaryOp::Deref,
                expr: inner,
            } => {
                let ty = self.infer_type(inner)?;
                match ty {
                    IrType::Reference { elem, .. } => Some(*elem),
                    other => Some(other),
                }
            }

            // Call: resolve the planned local specialization before falling back
            // to an unmangled source-name lookup. This preserves the concrete
            // return layout for `let value = helper::<N>(...)`.
            IrExprKind::Call { func, .. } => {
                let (name, type_args): (String, &[IrType]) = match &func.kind {
                    IrExprKind::Path {
                        segments,
                        type_args,
                    } => (segments.join("_"), type_args.as_slice()),
                    // Local calls without turbofish parse as `Var`.
                    IrExprKind::Var(name) => (name.clone(), &[]),
                    _ => return None,
                };
                let func_kind_segments_last_is_default = matches!(
                    &func.kind,
                    IrExprKind::Path { segments, .. }
                        if segments.last().map(String::as_str) == Some("default")
                );
                if let (Some(plan), Some(caller)) = (self.mono_plan, self.current_instance) {
                    // Mirror the planning-side key: canonical argument types
                    // with no generic prefix (see mono.rs calls.insert). Args
                    // are not available here, so the key is empty and deduce
                    // falls back to the unique (caller, callee) entry.
                    let args = String::new();
                    if let Some(callee) = plan.local_call_deduce(caller, &name, &args) {
                        return self
                            .ir_func_ret_types
                            .get(plan.emitted_name(callee))
                            .cloned();
                    }
                }
                // `Array::<E, L>::default()`: the result type is the array
                // itself — recoverable from the turbofish arguments.
                if type_args.len() == 2
                    && func_kind_segments_last_is_default
                    && type_args
                        .first()
                        .map(|a| matches!(a, IrType::Array { .. }))
                        .unwrap_or(false)
                {
                    if let IrType::Array { elem, kind, .. } = &type_args[0] {
                        return Some(IrType::Array {
                            kind: *kind,
                            elem: Box::new(elem.as_ref().clone()),
                            len: ArrayLength::TypeParam(match &type_args[1] {
                                IrType::TypeParam(n) => n.clone(),
                                other => match crate::mono::type_args_to_len(
                                    Some(other),
                                    self.mono,
                                ) {
                                    volar_compiler::ir::ArrayLength::TypeParam(n) => n,
                                    volar_compiler::ir::ArrayLength::Const(n) => n.to_string(),
                                    _ => "_unresolved".to_owned(),
                                },
                            }),
                        });
                    }
                }
                self.ir_func_ret_types.get(&name).cloned()
            }

            // Default value: the type is carried by the expression itself
            // (e.g. `Array::<_, K2::Output>::default()`).
            IrExprKind::DefaultValue { ty } => ty.as_deref().cloned(),

            // If-expression: both branches share a type; infer from the then
            // branch, falling back to the else branch.
            IrExprKind::If {
                then_branch,
                else_branch,
                ..
            } => then_branch
                .expr
                .as_deref()
                .and_then(|e| self.infer_type(e))
                .or_else(|| {
                    else_branch
                        .as_deref()
                        .and_then(|e| self.infer_type(e))
                }),

            // Literal: infer primitive type from the literal variant.
            IrExprKind::Lit(lit) => match lit {
                IrLit::Bool(_) => Some(IrType::Primitive(PrimitiveType::Bool)),
                IrLit::Int(_) => Some(IrType::Primitive(PrimitiveType::U64)),
                IrLit::Unit => Some(IrType::Unit),
                _ => None,
            },

            // FixedArray: infer element type from the first element + count.
            IrExprKind::FixedArray(elems) => {
                if elems.is_empty() {
                    return None;
                }
                let elem_ty = self.infer_type(&elems[0])?;
                Some(IrType::Array {
                    kind: ArrayKind::FixedArray,
                    elem: Box::new(elem_ty),
                    len: ArrayLength::Const(elems.len()),
                })
            }

            // Struct literal: the struct type is the literal's kind + args.
            IrExprKind::StructExpr {
                kind, type_args, ..
            } => Some(IrType::Struct {
                kind: kind.clone(),
                type_args: type_args
                    .iter()
                    .map(|a| crate::mono::mono_type(a, &self.mono))
                    .collect(),
            }),

            // Parsed `[value; N]`: preserve the concrete or const-generic
            // length so later indexed assignments can update the flattened
            // local array.
            IrExprKind::Repeat { elem, len } => {
                let elem_ty = self.infer_type(elem)?;
                let len = match &len.kind {
                    IrExprKind::Lit(IrLit::Int(value)) => ArrayLength::Const(*value as usize),
                    IrExprKind::Var(name) => self
                        .mono
                        .const_params
                        .get(name)
                        .copied()
                        .map(ArrayLength::Const)
                        .unwrap_or_else(|| ArrayLength::TypeParam(name.clone())),
                    _ => return None,
                };
                Some(IrType::Array {
                    kind: ArrayKind::FixedArray,
                    elem: Box::new(elem_ty),
                    len,
                })
            }

            _ => None,
        }
    }
}

/// Extract the `StructKind` from a type (stripping references).
thread_local! {
    /// Debug aid: the instance currently being lowered (set by
    /// `lower_function_instance`).
    pub static CURRENT_INSTANCE_DEBUG: std::cell::RefCell<String> =
        const { std::cell::RefCell::new(String::new()) };
}

static EMPTY_EXTERNAL_FNS: LazyLock<BTreeMap<String, ExternalFnInfo>> =
    LazyLock::new(BTreeMap::new);
static EMPTY_FUNC_SIGS: LazyLock<BTreeMap<String, FuncSigInfo>> = LazyLock::new(BTreeMap::new);
static EMPTY_IR_RET_TYPES: LazyLock<BTreeMap<String, IrType>> = LazyLock::new(BTreeMap::new);
static EMPTY_ENUMS: LazyLock<Vec<IrEnum>> = LazyLock::new(Vec::new);
static EMPTY_ENUM_REGISTRY: LazyLock<EnumRegistry> = LazyLock::new(EnumRegistry::empty);

// ============================================================================
// Public API
// ============================================================================

#[derive(Clone, Debug)]
pub struct MonoRoot {
    /// Local source-function name to specialize.
    pub function: String,
    /// Concrete substitutions for this root instance.
    pub env: MonoEnv,
}

impl MonoRoot {
    pub fn new(function: impl Into<String>, env: MonoEnv) -> Self {
        Self {
            function: function.into(),
            env,
        }
    }
}

#[derive(Clone, Debug)]
pub struct MonoPlanOptions {
    /// Explicit entry instances. Empty selects every non-generic normal function.
    pub roots: Vec<MonoRoot>,
    /// Bound recursive specialization expansion before reporting an error.
    pub max_instances: usize,
    /// When true, unknown / unregistered nominal types become opaque `U64`
    /// placeholders instead of panicking (useful for full-spec widen).
    pub lenient: bool,
    /// For CFG modules: whether to emit flat auxiliary function bodies.
    pub include_auxiliary: bool,
}

impl Default for MonoPlanOptions {
    fn default() -> Self {
        Self {
            roots: Vec::new(),
            max_instances: 4_096,
            lenient: false,
            include_auxiliary: true,
        }
    }
}

/// Build one [`MonoRoot`] (all sharing `env`) for every `Normal` function in
/// `module` whose name starts with any of `prefixes`, plus one more for
/// every name in `extra_helpers` that's actually present in `module` (for
/// non-entry-point functions an entry point calls directly, e.g. a spec's
/// own `vole_and_prover_step`/`vole_and_verifier_check`/`derive_and_q` —
/// rooting every such helper unconditionally would force specializations
/// a given circuit never actually calls).
///
/// A configurable prefix LIST rather than a single hardcoded pair: this
/// codebase's own woven functions alone already use three different
/// prefixes across roles (`vole_prove_`/`vole_verify_`/`vole_qsim_`) —
/// hardcoding just the first two (as this crate's own earlier example
/// helpers did) silently roots zero QSim functions, which then either
/// produces an empty `roots` list (if QSim is the only role woven) or
/// silently drops QSim's own functions from the plan entirely (if woven
/// alongside another role) — no error, just a quietly incomplete lowering.
/// A future weaver (net-style, or a wholly different naming convention) is
/// one more prefix, not a new hardcoded helper.
///
/// Panics if the result is empty — an empty root set is never useful (see
/// [`lower_module_monomorphized`], which would simply lower nothing) and
/// usually means a prefix typo or the wrong module, better caught here than
/// as silent no-op output downstream.
pub fn roots_by_name_prefix<P: Clone>(
    module: &IrModule<IrFunction<P>, P>,
    prefixes: &[&str],
    extra_helpers: &[&str],
    env: mono::MonoEnv,
) -> Vec<MonoRoot> {
    let mut roots: Vec<MonoRoot> = module
        .functions
        .iter()
        .filter(|f| f.external_kind == ExternalKind::Normal)
        .filter(|f| prefixes.iter().any(|p| f.name.starts_with(p)))
        .map(|f| MonoRoot::new(f.name.clone(), env.clone()))
        .collect();
    for &helper in extra_helpers {
        if module.functions.iter().any(|f| f.name == helper)
            && !roots.iter().any(|r| r.function == helper)
        {
            roots.push(MonoRoot::new(helper, env.clone()));
        }
    }
    assert!(
        !roots.is_empty(),
        "roots_by_name_prefix: no function in module matched any of {prefixes:?} — \
         wrong prefix list, wrong module, or genuinely nothing to root"
    );
    roots
}

/// Lower a closed set of concrete local function instances to `target`.
///
/// Each root owns a separate [`MonoEnv`]. Direct calls to local generic
/// functions discover and emit their own concrete instances; calls to opaque
/// functions retain the target's existing external-call ABI.
pub fn lower_module_monomorphized<T: LirTarget<P>, P: Clone>(
    module: &IrModule<IrFunction<P>, P>,
    target: &mut T,
    options: MonoPlanOptions,
) -> Result<(), MonoError> {
    let plan = mono::plan_flat_module(module, &options.roots, options.max_instances)?;
    lower_planned_module(module, target, &plan, options.lenient);
    Ok(())
}

/// Lower all non-generic normal functions, discovering their concrete local
/// callees. Generic roots must be supplied through [`lower_module_monomorphized`].
pub fn lower_module<T: LirTarget<P>, P: Clone>(
    module: &IrModule<IrFunction<P>, P>,
    target: &mut T,
) {
    lower_module_monomorphized(module, target, MonoPlanOptions::default())
        .unwrap_or_else(|err| panic!("LIR monomorphization failed: {err}"));
}

/// Compatibility entry point for callers that historically provided one
/// environment for a whole module.
///
/// Only **non-generic** normal functions become roots (each cloning `env`).
/// Generic callees are discovered from explicit turbofish / inferred bindings
/// and receive their own environments. Rooting every generic definition with a
/// shared env incorrectly treats unbound const params (e.g. `encrypt_branch`'s
/// `L`) as concrete layouts.
pub fn lower_module_with_opts<T: LirTarget<P>, P: Clone>(
    module: &IrModule<IrFunction<P>, P>,
    target: &mut T,
    env: &MonoEnv,
) {
    let roots = module
        .functions
        .iter()
        .filter(|f| f.external_kind == ExternalKind::Normal && f.generics.is_empty())
        .map(|f| MonoRoot::new(f.name.clone(), env.clone()))
        .collect();
    lower_module_monomorphized(
        module,
        target,
        MonoPlanOptions {
            roots,
            ..Default::default()
        },
    )
    .unwrap_or_else(|err| panic!("LIR monomorphization failed: {err}"));
}

/// Lower the closed specialization set reachable from `seeds`.
///
/// Each seed becomes a [`MonoRoot`] with `env` (so generic seeds such as
/// `vole_and_prover_step` can be specialized). Callees are discovered by the
/// planner; orphan generic definitions are not forced as roots.
pub fn lower_module_seeded<T: LirTarget>(
    module: &IrModule<IrFunction>,
    target: &mut T,
    env: &MonoEnv,
    seeds: &[&str],
) {
    let roots = seeds
        .iter()
        .map(|seed| MonoRoot::new((*seed).to_owned(), env.clone()))
        .collect();
    lower_module_monomorphized(
        module,
        target,
        MonoPlanOptions {
            roots,
            ..Default::default()
        },
    )
    .unwrap_or_else(|err| panic!("LIR monomorphization failed: {err}"));
}

fn lower_planned_module<T: LirTarget<P>, P: Clone>(
    module: &IrModule<IrFunction<P>, P>,
    target: &mut T,
    plan: &MonoPlan<P>,
    lenient: bool,
) {
    // Non-generic structs first; concrete generic nominals are registered
    // per planned instance via `ensure_type_nominals` (no module-wide merge).
    // Enums register before structs so struct field types naming an enum
    // (which the parser may emit as a bare TypeParam when the path crosses
    // modules) resolve through the registry during field mapping.
    let empty = MonoEnv::new("");
    let mut registry = structs::StructRegistry::new();
    registry.lenient = lenient;
    let enum_registry = structs::build_enum_registry(&module.enums, &mut registry, target, &empty);
    let mut registry =
        structs::build_struct_registry_with_lenient(module, target, &empty, lenient, registry);

    for (key, env) in &plan.instances {
        // Impl methods live in `module.impls`, not `module.functions`; only
        // their parameters' nominals matter here (the instance's own
        // lowering uses the materialized definition when present).
        let func = plan
            .materialized
            .get(key)
            .cloned()
            .or_else(|| {
                module
                    .functions
                    .iter()
                    .find(|func| func.name == key.source_name)
                    .cloned()
            })
            .or_else(|| {
                module.impls.iter().find_map(|ir_impl| {
                    ir_impl.items.iter().find_map(|item| match item {
                        volar_compiler::ir::IrImplItem::Method(m)
                            if m.name == key.source_name =>
                        {
                            Some(m.clone())
                        }
                        _ => None,
                    })
                })
            })
            .expect("planned source function exists");
        for parameter in &func.params {
            structs::ensure_type_nominals(
                &parameter.ty,
                &mut registry,
                target,
                env,
                &module.structs,
                &module.enums,
            );
            structs::register_tuples_in_type(&parameter.ty, &mut registry, target, env);
        }
        if let Some(return_type) = &func.return_type {
            structs::ensure_type_nominals(
                return_type,
                &mut registry,
                target,
                env,
                &module.structs,
                &module.enums,
            );
            structs::register_tuples_in_type(return_type, &mut registry, target, env);
        }
        // Register generic module structs under this instance env when every
        // declared generic is bound (covers Vope/Delta/Q for woven VOLE).
        for ir_struct in &module.structs {
            if ir_struct.generics.is_empty() {
                continue;
            }
            let mut args: Vec<IrType> = Vec::new();
            let mut complete = true;
            for parameter in &ir_struct.generics {
                match parameter.kind {
                    volar_compiler::ir::IrGenericParamKind::Type => {
                        if let Some(ty) = env.type_params.get(&parameter.name) {
                            args.push(ty.clone());
                        } else if let Some(&n) = env.const_params.get(&parameter.name) {
                            // `K: ArraySize` often bound via `with_len("K", …)`.
                            args.push(IrType::TypeParam(n.to_string()));
                        } else {
                            complete = false;
                            break;
                        }
                    }
                    volar_compiler::ir::IrGenericParamKind::Const => {
                        if let Some(&n) = env.const_params.get(&parameter.name) {
                            args.push(IrType::TypeParam(n.to_string()));
                        } else {
                            complete = false;
                            break;
                        }
                    }
                    volar_compiler::ir::IrGenericParamKind::Lifetime => {}
                }
            }
            if complete && !args.is_empty() {
                structs::ensure_struct_instance(
                    ir_struct,
                    &args,
                    &mut registry,
                    target,
                    env,
                    &module.structs,
                    &module.enums,
                );
            }
        }
    }

    let mut external_fns = BTreeMap::new();
    for func in &module.functions {
        if !matches!(
            func.external_kind,
            ExternalKind::Oracle | ExternalKind::Action | ExternalKind::Rng
        ) {
            continue;
        }
        let env = MonoEnv::new("");
        external_fns.insert(
            func.name.clone(),
            ExternalFnInfo {
                kind: func.external_kind,
                param_tys: func
                    .params
                    .iter()
                    .map(|param| registry.ir_type_to_lir(&param.ty, &env))
                    .collect(),
                return_type: func
                    .return_type
                    .as_ref()
                    .map(|ty| registry.ir_type_to_lir(ty, &env)),
            },
        );
    }

    let mut func_sigs = BTreeMap::new();
    let mut ir_func_ret_types = BTreeMap::new();
    for (key, env) in &plan.instances {
        let func = plan
            .materialized
            .get(key)
            .unwrap_or_else(|| {
                module
                    .functions
                    .iter()
                    .find(|func| func.name == key.source_name)
                    .unwrap_or_else(|| {
                        // Impl methods live outside `module.functions`.
                        module
                            .impls
                            .iter()
                            .find_map(|ir_impl| {
                                ir_impl.items.iter().find_map(|item| match item {
                                    volar_compiler::ir::IrImplItem::Method(m)
                                        if m.name == key.source_name =>
                                    {
                                        Some(m)
                                    }
                                    _ => None,
                                })
                            })
                            .expect("planned source function exists")
                    })
            });
        let emitted = plan.emitted_name(key).to_owned();
        func_sigs.insert(
            emitted.clone(),
            FuncSigInfo {
                param_tys: func
                    .params
                    .iter()
                    .map(|param| registry.ir_type_to_lir(&param.ty, env))
                    .collect(),
                return_type: func
                    .return_type
                    .as_ref()
                    .map(|ty| registry.ir_type_to_lir(ty, env)),
            },
        );
        if let Some(return_type) = &func.return_type {
            ir_func_ret_types.insert(emitted, mono_type(return_type, env));
        }
    }

    for (key, env) in &plan.instances {
        // Materialized method instances lower from their derived definition
        // (receiver promoted to a typed `self` param).
        let func = plan
            .materialized
            .get(key)
            .unwrap_or_else(|| {
                module
                    .functions
                    .iter()
                    .find(|func| func.name == key.source_name)
                    .expect("planned source function exists")
            });
        lower_function_instance(
            func,
            plan.emitted_name(key),
            key,
            plan,
            target,
            &registry,
            &enum_registry,
            env,
            &module.structs,
            &module.enums,
            &external_fns,
            &func_sigs,
            &ir_func_ret_types,
        );
    }
}

/// Internal: lower a concrete planned instance with full module context.
fn lower_function_instance<T: LirTarget<P>, P: Clone>(
    func: &IrFunction<P>,
    emitted_name: &str,
    instance: &FunctionInstanceKey,
    plan: &MonoPlan<P>,
    target: &mut T,
    registry: &StructRegistry,
    enum_registry: &EnumRegistry,
    env: &MonoEnv,
    module_structs: &[volar_compiler::ir::IrStruct],
    module_enums: &[IrEnum],
    external_fns: &BTreeMap<String, ExternalFnInfo>,
    func_sigs: &BTreeMap<String, FuncSigInfo>,
    ir_func_ret_types: &BTreeMap<String, IrType>,
) {
    CURRENT_INSTANCE_DEBUG.with(|c| *c.borrow_mut() = emitted_name.to_owned());
    let param_lir_tys: Vec<_> = func
        .params
        .iter()
        .map(|param| {
            let lir = registry.ir_type_to_lir(&param.ty, env);
            if std::env::var("VOLAR_PARAM_DEBUG").is_ok() {
                eprintln!(
                    "[param-debug] {} param {} ty={:?} lir={:?} width={}",
                    emitted_name,
                    param.name,
                    mono_type(&param.ty, env),
                    lir,
                    flatten_count(&lir, registry)
                );
            }
            lir
        })
        .collect();
    let ret_ty = func
        .return_type
        .as_ref()
        .map(|ty| registry.ir_type_to_lir(ty, env));
    let (entry, param_val_groups) = target.begin_function(emitted_name, &param_lir_tys, ret_ty);
    target.switch_to_block(entry.clone());
    let named_params = func
        .params
        .iter()
        .zip(param_val_groups)
        .map(|(param, values)| (param.name.clone(), values, mono_type(&param.ty, env)))
        .collect();
    let mut ctx = LowerCtx::new(
        target,
        entry,
        named_params,
        registry,
        enum_registry,
        env,
        module_structs,
        module_enums,
    );
    ctx.external_fns = external_fns;
    ctx.func_sigs = func_sigs;
    ctx.ir_func_ret_types = ir_func_ret_types;
    ctx.current_instance = Some(instance);
    ctx.mono_plan = Some(plan);
    let tail_vals = lower_block(&func.body, &mut ctx);
    ctx.target.ret(&tail_vals);
    ctx.target.end_function();
}

/// Internal: lower a function with full module context (external-fn dispatch).
fn lower_function_in_module<T: LirTarget<P>, P: Clone>(
    func: &IrFunction<P>,
    target: &mut T,
    registry: &StructRegistry,
    enum_registry: &EnumRegistry,
    env: &MonoEnv,
    module_structs: &[volar_compiler::ir::IrStruct],
    module_enums: &[IrEnum],
    external_fns: &BTreeMap<String, ExternalFnInfo>,
    func_sigs: &BTreeMap<String, FuncSigInfo>,
    ir_func_ret_types: &BTreeMap<String, IrType>,
) {
    let param_lir_tys: Vec<LirType> = func
        .params
        .iter()
        .map(|p| registry.ir_type_to_lir(&p.ty, env))
        .collect();
    let ret_ty = func
        .return_type
        .as_ref()
        .map(|t| registry.ir_type_to_lir(t, env));

    let (entry, param_val_groups) = target.begin_function(&func.name, &param_lir_tys, ret_ty);
    target.switch_to_block(entry.clone());

    let named_params: Vec<(String, Vec<T::Value>, IrType)> = func
        .params
        .iter()
        .zip(param_val_groups.into_iter())
        .map(|(p, vals)| (p.name.clone(), vals, p.ty.clone()))
        .collect();

    let mut ctx = LowerCtx::new(
        target,
        entry,
        named_params,
        registry,
        enum_registry,
        env,
        module_structs,
        module_enums,
    );
    ctx.external_fns = external_fns;
    ctx.func_sigs = func_sigs;
    ctx.ir_func_ret_types = ir_func_ret_types;

    let tail_vals = lower_block(&func.body, &mut ctx);
    ctx.target.ret(&tail_vals);
    ctx.target.end_function();
}

/// Lower a single `IrFunction` to `target` (no struct support).
///
/// Use `lower_module_with_opts` for modules that use structs or arrays.
pub fn lower_function<T: LirTarget>(func: &IrFunction, target: &mut T) {
    static EMPTY_ENV: std::sync::LazyLock<MonoEnv> = std::sync::LazyLock::new(|| MonoEnv::new(""));
    let empty_registry = StructRegistry::empty();
    lower_function_with_registry(func, target, &empty_registry, &EMPTY_ENV, &[]);
}

pub fn lower_function_with_registry<T: LirTarget>(
    func: &IrFunction,
    target: &mut T,
    registry: &StructRegistry,
    env: &MonoEnv,
    module_structs: &[volar_compiler::ir::IrStruct],
) {
    let param_lir_tys: Vec<LirType> = func
        .params
        .iter()
        .map(|p| registry.ir_type_to_lir(&p.ty, env))
        .collect();
    let ret_ty = func
        .return_type
        .as_ref()
        .map(|t| registry.ir_type_to_lir(t, env));

    let (entry, param_val_groups) = target.begin_function(&func.name, &param_lir_tys, ret_ty);
    target.switch_to_block(entry.clone());

    let named_params: Vec<(String, Vec<T::Value>, IrType)> = func
        .params
        .iter()
        .zip(param_val_groups.into_iter())
        .map(|(p, vals)| (p.name.clone(), vals, p.ty.clone()))
        .collect();

    let mut ctx = LowerCtx::new(
        target,
        entry,
        named_params,
        registry,
        &EMPTY_ENUM_REGISTRY,
        env,
        module_structs,
        &EMPTY_ENUMS,
    );

    let tail_vals = lower_block(&func.body, &mut ctx);
    ctx.target.ret(&tail_vals);
    ctx.target.end_function();
}

// ============================================================================
// Block and statement lowering
// ============================================================================

/// Lower a block, returning the flat scalar list produced by its trailing
/// expression (or an empty vec for unit-typed blocks).
fn lower_block<T: LirTarget<P>, P: Clone>(
    block: &IrBlock<P>,
    ctx: &mut LowerCtx<T, P>,
) -> Vec<T::Value> {
    for stmt in &block.stmts {
        // `continue` inside an unrolled loop body: stop this block's lowering.
        // The unroller re-invokes lower_block for the next iteration, so the
        // remaining statements of THIS iteration are skipped — exactly the
        // Rust semantics — while later iterations still execute. Blocks
        // lowered outside any enclosing unrolled loop never contain
        // `continue` (validated by external-source checks upstream).
        if matches!(
            stmt.kind,
            IrStmtKind::Expr(IrExpr {
                kind: IrExprKind::Continue,
                ..
            }) | IrStmtKind::Semi(IrExpr {
                kind: IrExprKind::Continue,
                ..
            })
        ) {
            return Vec::new();
        }
        ctx.target.set_prov(stmt.prov.clone());
        ctx.target.set_side(stmt.side);
        lower_stmt(stmt, ctx);
    }
    block
        .expr
        .as_deref()
        .map(|e| lower_expr(e, ctx))
        .unwrap_or_default()
}

fn lower_stmt<T: LirTarget<P>, P: Clone>(stmt: &IrStmt<P>, ctx: &mut LowerCtx<T, P>) {
    match &stmt.kind {
        IrStmtKind::Let { pattern, ty, init } => {
            if let Some(init_expr) = init {
                // When the init expression is a literal and we have a type
                // annotation, pass the type hint so the literal gets the
                // correct narrow type (e.g. U8 instead of U64).
                let vals = match &init_expr.kind {
                    IrExprKind::Lit(lit) => {
                        vec![lower_lit(lit, ctx, ty.as_ref())]
                    }
                    _ => lower_expr(init_expr, ctx),
                };
                // For tuple patterns, split by element widths derived from the type.
                if let IrPattern::Tuple(sub_pats) = pattern {
                    let tuple_ty = ty.clone().or_else(|| ctx.infer_type(init_expr));
                    bind_tuple_pattern(sub_pats, vals, tuple_ty.as_ref(), ctx);
                    return;
                }
                if let IrPattern::Ident { name, .. } = pattern {
                    let ir_ty = ty.clone().or_else(|| ctx.infer_type(init_expr));
                    if let Some(ir_ty) = ir_ty {
                        ctx.env_types.insert(name.clone(), ir_ty);
                    }
                }
                bind_pattern(pattern, vals, None, ctx);
            }
        }
        IrStmtKind::Semi(expr) | IrStmtKind::Expr(expr) => {
            lower_expr(expr, ctx);
        }
        _ => panic!("lower_stmt: unhandled IrStmt variant — add lowering for this variant"),
    }
}

/// Split flat scalars across a tuple sub-pattern list using element type widths.
fn bind_tuple_pattern<T: LirTarget<P>, P: Clone>(
    sub_pats: &[IrPattern],
    vals: Vec<T::Value>,
    tuple_ty: Option<&IrType>,
    ctx: &mut LowerCtx<T, P>,
) {
    let elem_types: Option<&Vec<IrType>> = match tuple_ty {
        Some(IrType::Tuple(elems)) => Some(elems),
        _ => None,
    };
    let mut offset = 0;
    for (i, sub_pat) in sub_pats.iter().enumerate() {
        let width = if let Some(elems) = elem_types {
            if let Some(elem_ty) = elems.get(i) {
                flatten_count(
                    &ctx.registry.ir_type_to_lir(elem_ty, ctx.mono),
                    ctx.registry,
                )
            } else {
                1
            }
        } else {
            // No type info: give each sub-pattern 1 scalar.
            1
        };
        let slice = if offset + width <= vals.len() {
            vals[offset..offset + width].to_vec()
        } else {
            vals[offset..].to_vec()
        };
        let elem_ty_hint = elem_types.and_then(|elems| elems.get(i));
        bind_pattern(sub_pat, slice, elem_ty_hint, ctx);
        offset += width;
    }
}

fn bind_pattern<T: LirTarget<P>, P: Clone>(
    pattern: &IrPattern,
    vals: Vec<T::Value>,
    ty_hint: Option<&IrType>,
    ctx: &mut LowerCtx<T, P>,
) {
    match pattern {
        IrPattern::Ident { name, .. } => {
            if let Some(ty) = ty_hint {
                ctx.env_types.insert(name.clone(), ty.clone());
            }
            ctx.env.insert(name.clone(), vals);
        }
        IrPattern::Wild => {}
        IrPattern::Tuple(sub_pats) => {
            bind_tuple_pattern(sub_pats, vals, ty_hint, ctx);
        }
        IrPattern::TupleStruct { kind, elems } => {
            // Enum variant pattern: the vals ARE the payload (caller strips tag first).
            use volar_compiler::ir::StructKind;
            let variant_name = match kind {
                StructKind::Custom(n) => n.as_str(),
                StructKind::GenericArray => "Array",
            };
            let payload_tys: Vec<LirType> = ctx
                .enum_registry
                .find_variant(variant_name)
                .map(|(_, v)| v.payload_lir_tys.clone())
                .unwrap_or_default();
            let mut offset = 0;
            for (sub_pat, lir_ty) in elems.iter().zip(payload_tys.iter()) {
                let w = flatten_count(lir_ty, ctx.registry);
                let slice = vals[offset..(offset + w).min(vals.len())].to_vec();
                bind_pattern(sub_pat, slice, None, ctx);
                offset += w;
            }
        }
        other => unimplemented!("bind_pattern: unsupported pattern {:?}", other),
    }
}

// ============================================================================
// Expression lowering
// ============================================================================

/// Lower an expression, returning the flat scalar list for its value.
/// Scalar-typed expressions return a single-element vec.
/// Aggregate-typed expressions return multiple scalars (array = N elems, struct = all fields).
fn lower_expr<T: LirTarget<P>, P: Clone>(
    expr: &IrExpr<P>,
    ctx: &mut LowerCtx<T, P>,
) -> Vec<T::Value> {
    match &expr.kind {
        IrExprKind::Lit(lit) => vec![lower_lit(lit, ctx, None)],

        IrExprKind::Var(name) => {
            if let Some(values) = ctx.env.get(name.as_str()) {
                return values.clone();
            }
            // Const-generic names in parsed repeat lengths and type-level
            // arithmetic are expression nodes, not runtime locals.
            if let Some(&value) = ctx.mono.const_params.get(name.as_str()) {
                return vec![ctx.target.iconst(LirType::U64, value as i64)];
            }
            // Module-level constants (e.g. TFHE's Q4) are compile-time words.
            if let Some(&value) = ctx.mono.consts.get(name.as_str()) {
                return vec![ctx.target.iconst(LirType::U64, value as i64)];
            }
            // See `digit_var_as_literal`'s own doc: an all-digit `Var` name
            // is never a real variable reference, always a smuggled literal.
            if let Some(value) = volar_compiler::ir::digit_var_as_literal(name) {
                return vec![ctx.target.iconst(LirType::U64, value as i64)];
            }
            panic!("undefined variable: {name}")
        }

        IrExprKind::Binary { op, left, right } => {
            // Operator overloads on user structs stay as Binary in IR (the
            // parser has no type info to resolve `a * b`), but element-wise
            // lowering is wrong when the operands are struct instances with
            // a declared operator impl (e.g. `Vope * Delta` where the Vope
            // is 48 scalars and Delta 16). Route those through the impl's
            // method via the monomorphization plan.
            if let Some(plan) = ctx.mono_plan.as_ref() {
                let left_is_struct_mul = ctx
                    .infer_type(left)
                    .map(|ty| match &ty {
                        IrType::Struct { kind, .. } => {
                            plan.mul_impl_structs.contains_key(&kind.to_string())
                        }
                        _ => false,
                    })
                    .unwrap_or(false);
                if left_is_struct_mul {
                    // Dispatch by receiver struct: several `Mul` impls share
                    // the method name `mul` in the source module, so the
                    // callee is keyed as `mul__<StructKind>` (mirroring the
                    // planner) to keep impls distinct.
                    let callee = match ctx.infer_type(left).as_ref() {
                        Some(IrType::Struct { kind, .. }) => {
                            format!("mul__{}", crate::structs::kind_name(kind))
                        }
                        _ => unreachable!("checked struct receiver above"),
                    };
                    return lower_method_extern(
                        left,
                        &callee,
                        &[],
                        std::slice::from_ref(right.as_ref()),
                        ctx,
                    );
                }
            }
            let lv = lower_expr(left, ctx);
            let rv = lower_expr(right, ctx);
            if lv.len() == 1 && rv.len() == 1 {
                vec![lower_binop(
                    *op,
                    lv.into_iter().next().unwrap(),
                    rv.into_iter().next().unwrap(),
                    ctx,
                )]
            } else if lv.len() == rv.len() && !lv.is_empty() {
                // Element-wise operation over aggregate types (e.g. Vope addition).
                lv.into_iter()
                    .zip(rv)
                    .map(|(l, r)| lower_binop(*op, l, r, ctx))
                    .collect()
            } else {
                panic!(
                    "Binary {:?}: operand widths {} vs {} — cannot apply element-wise",
                    op,
                    lv.len(),
                    rv.len()
                )
            }
        }

        IrExprKind::Unary { op, expr: inner } => {
            // Ref / RefMut / Deref are transparent in value-semantics LIR and must
            // pass through all flat scalars of the inner expression (e.g. `&arr`
            // must keep 64 scalars, not squeeze to 1).
            match op {
                SpecUnaryOp::Ref | SpecUnaryOp::RefMut | SpecUnaryOp::Deref => {
                    lower_expr(inner, ctx)
                }
                _ => {
                    let v = into_scalar(lower_expr(inner, ctx), "unary operand");
                    vec![lower_unop(*op, v, ctx)]
                }
            }
        }

        IrExprKind::Block(b) => lower_block(b, ctx),

        IrExprKind::If {
            cond,
            then_branch,
            else_branch,
        } => lower_if(cond, then_branch, else_branch.as_deref(), ctx),

        // ---- Match (enum discriminant or primitive switch) ------------------
        IrExprKind::Match {
            expr: scrutinee,
            arms,
        } => lower_match(scrutinee, arms, ctx),

        // ---- Try (`?` operator) ---------------------------------------------
        IrExprKind::Try(inner) => lower_try(inner, ctx),

        IrExprKind::Cast { expr: inner, ty } => {
            let v = into_scalar(lower_expr(inner, ctx), "cast operand");
            let dst = ir_type_to_lir_prim(ty, ctx.mono);
            vec![ctx.target.zext(v, dst)]
        }

        IrExprKind::Return(val) => {
            let ret_vals = val
                .as_deref()
                .map(|e| lower_expr(e, ctx))
                .unwrap_or_default();
            ctx.target.ret(&ret_vals);
            vec![] // unreachable placeholder
        }

        // ---- Phase 2: field access ------------------------------------------
        IrExprKind::Field { base, field } => lower_field(base, field, ctx),

        // ---- Phase 2: array index -------------------------------------------
        IrExprKind::Index { base, index } => lower_index(base, index, ctx),

        // ---- Phase 2: struct construction -----------------------------------
        IrExprKind::StructExpr {
            kind,
            type_args,
            fields,
            ..
        } => lower_struct_expr(kind, type_args, fields, ctx),

        // ---- Tuple construction ---------------------------------------------
        IrExprKind::Tuple(elems) => {
            // Flatten all tuple elements into a single scalar list.
            elems.iter().flat_map(|e| lower_expr(e, ctx)).collect()
        }

        // ---- Phase 2: fixed-size array literal ------------------------------
        IrExprKind::FixedArray(elems) => lower_fixed_array(elems, ctx),
        IrExprKind::Array(elems) => lower_fixed_array(elems, ctx),
        IrExprKind::Repeat { elem, len } => lower_repeat_array(elem, len, ctx),

        // ---- Phase 2: array generation via closure --------------------------
        IrExprKind::ArrayGenerate {
            elem_ty,
            len,
            index_var,
            body,
        } => lower_array_generate(elem_ty.as_deref(), len, index_var, body, ctx),

        // ---- Phase 2: element-wise map (RawMap) -----------------------------
        IrExprKind::RawMap {
            receiver,
            elem_var,
            body,
        } => lower_raw_map(receiver, elem_var, body, ctx),

        // ---- Phase 2: element-wise zip (RawZip) -----------------------------
        IrExprKind::RawZip {
            left,
            right,
            left_var,
            right_var,
            body,
        } => lower_raw_zip(left, right, left_var, right_var, body, ctx),

        // ---- Phase 2: bounded loop ------------------------------------------
        IrExprKind::IterLoop {
            pattern,
            collection,
            body,
        } => {
            // Recognize `(start..end).rev()` — a reversed bounded loop — and
            // unroll it descending. Other iterator shapes are not supported.
            let reversed_range = match &collection.kind {
                IrExprKind::MethodCall {
                    receiver,
                    method: MethodKind::Known(StdMethod::Rev),
                    args,
                    ..
                } if args.is_empty() => Some(receiver.as_ref()),
                _ => None,
            };
            if let Some(IrExpr {
                kind:
                    IrExprKind::Range {
                        start: Some(start),
                        end: Some(end),
                        ..
                    },
                ..
            }) = reversed_range
            {
                // `Range` itself never carries `inclusive`; `.rev()` of an
                // inclusive range is not expressible in Rust source, so this
                // is always exclusive.
                lower_bounded_loop_descending(pattern, start, end, body, ctx);
                return Vec::new();
            }
            unimplemented!("lower_expr: unsupported iterator loop shape")
        }

        IrExprKind::BoundedLoop {
            var,
            start,
            end,
            inclusive,
            body,
        } => {
            lower_bounded_loop(var, start, end, *inclusive, body, ctx);
            vec![] // loops are ()-typed
        }

        // ---- Phase 2: method calls ------------------------------------------
        IrExprKind::MethodCall {
            receiver,
            method,
            type_args,
            args,
        } => lower_method_call(receiver, method, type_args, args, ctx),

        // ---- Phase 2: free function calls -----------------------------------
        IrExprKind::Call { func, args } => lower_call(func, args, ctx),

        // ---- Assignment (storage writes, etc.) ------------------------------
        IrExprKind::Assign { left, right } => {
            lower_assign(left, right, ctx);
            vec![]
        }

        // ---- Compound assignment (x op= rhs) --------------------------------
        IrExprKind::AssignOp { op, left, right } => {
            let lv = into_scalar(lower_expr(left, ctx), "assignop lhs");
            let rv = into_scalar(lower_expr(right, ctx), "assignop rhs");
            let result = lower_binop(*op, lv, rv, ctx);
            if let IrExprKind::Var(name) = &left.kind {
                ctx.env.insert(name.clone(), vec![result]);
            } else {
                unimplemented!("AssignOp on non-variable lhs");
            }
            vec![]
        }

        // ---- TypenumUsize and LengthOf — resolve to concrete usize const ------
        IrExprKind::TypenumUsize { ty } => {
            // `T::USIZE` — resolve T as a const / type-level size.
            let n = match ty.as_ref() {
                IrType::TypeParam(name) => resolve_usize_param(name, ctx.mono)
                    .unwrap_or_else(|| panic!("TypenumUsize: unresolved TypeParam '{name}'")),
                IrType::Struct {
                    kind: StructKind::Custom(name),
                    type_args,
                } if type_args.is_empty() => typenum_usize(name)
                    .unwrap_or_else(|| panic!("TypenumUsize: unresolved typenum '{name}'")),
                other => panic!("TypenumUsize: unexpected type {:?}", other),
            };
            vec![ctx.target.iconst(LirType::U64, n as i64)]
        }

        IrExprKind::LengthOf(len) => {
            let n = const_len(len, ctx.mono);
            vec![ctx.target.iconst(LirType::U64, n as i64)]
        }

        // ---- Default / zero value -------------------------------------------
        IrExprKind::DefaultValue { ty } => {
            let lir_ty = ty
                .as_ref()
                .map(|t| ctx.registry.ir_type_to_lir(t, ctx.mono))
                .unwrap_or(LirType::Bool);
            flatten_scalar_types(&lir_ty, ctx.registry)
                .into_iter()
                .map(|sty| ctx.target.iconst(sty, 0))
                .collect()
        }

        // ---- Fold (unrolled accumulation over array) -------------------------
        IrExprKind::RawFold {
            receiver,
            init,
            acc_var,
            elem_var,
            body,
        } => lower_raw_fold(receiver, init, acc_var, elem_var, body, ctx),

        // Iterator pipelines: lower the shapes the spec uses. The chain is
        // normalized to a fold-like unrolled loop over the source collection.
        IrExprKind::IterPipeline(chain) => {
            lower_iter_chain(chain, ctx)
        }

        // ---- Path: may be a unit enum variant or a type-level size constant --
        IrExprKind::Path { segments, .. } => {
            // `T::USIZE` pattern — access const generic usize from MonoEnv.
            if segments.len() == 2 && segments[1] == "USIZE" {
                if let Some(n) = resolve_usize_param(&segments[0], ctx.mono) {
                    return vec![ctx.target.iconst(LirType::U64, n as i64)];
                }
            }

            // Check for unit enum variant.
            let joined = segments.join("_");
            if let Some((entry, variant)) = ctx.enum_registry.find_variant(&joined) {
                let tag = ctx.target.iconst(LirType::U8, variant.discriminant as i64);
                let mut scalars: Vec<T::Value> = vec![tag];
                let max_w = entry.max_payload_width;
                while scalars.len() < 1 + max_w {
                    scalars.push(ctx.target.iconst(LirType::U8, 0));
                }
                return scalars;
            }
            // Also try the last segment alone (for unqualified names like `None`).
            if let Some(last) = segments.last() {
                if let Some((entry, variant)) = ctx.enum_registry.find_variant(last) {
                    let tag = ctx.target.iconst(LirType::U8, variant.discriminant as i64);
                    let mut scalars: Vec<T::Value> = vec![tag];
                    let max_w = entry.max_payload_width;
                    while scalars.len() < 1 + max_w {
                        scalars.push(ctx.target.iconst(LirType::U8, 0));
                    }
                    return scalars;
                }
            }
            unimplemented!("lower_expr: unresolved Path {:?}", segments)
        }

        other => unimplemented!(
            "lower_expr: unsupported expr {:?} in instance {:?}",
            std::mem::discriminant(other),
            ctx.current_instance.as_ref().map(|c| c.source_name.clone())
        ),
    }
}

// ============================================================================
// Literal lowering
// ============================================================================

fn lower_lit<T: LirTarget<P>, P: Clone>(
    lit: &IrLit,
    ctx: &mut LowerCtx<T, P>,
    hint: Option<&IrType>,
) -> T::Value {
    match lit {
        IrLit::Int(n) => {
            let lir_ty = hint
                .and_then(|ty| match ty {
                    IrType::Primitive(p) => Some(primitive_to_lir(*p)),
                    _ => None,
                })
                .unwrap_or(LirType::U64);
            ctx.target.iconst(lir_ty, *n as i64)
        }
        IrLit::Bool(b) => ctx.target.iconst(LirType::Bool, *b as i64),
        IrLit::Float(_) => unimplemented!("float literals not supported in LIR"),
        other => unimplemented!("lower_lit: unsupported literal {:?}", other),
    }
}

// ============================================================================
// Binary / unary ops
// ============================================================================

fn lower_binop<T: LirTarget<P>, P: Clone>(
    op: SpecBinOp,
    lv: T::Value,
    rv: T::Value,
    ctx: &mut LowerCtx<T, P>,
) -> T::Value {
    match op {
        SpecBinOp::Add => ctx.target.add(lv, rv),
        SpecBinOp::Sub => ctx.target.sub(lv, rv),
        SpecBinOp::Mul => ctx.target.mul(lv, rv),
        SpecBinOp::Div => ctx.target.udiv(lv, rv),
        SpecBinOp::Rem => {
            // a % b = a - (a / b) * b (unsigned); LirTarget has udiv.
            let q = ctx.target.udiv(lv.clone(), rv.clone());
            let prod = ctx.target.mul(q, rv);
            ctx.target.sub(lv, prod)
        }
        SpecBinOp::BitAnd => ctx.target.and(lv, rv),
        SpecBinOp::BitOr => ctx.target.or(lv, rv),
        SpecBinOp::BitXor => ctx.target.xor(lv, rv),
        SpecBinOp::Shl => ctx.target.shl(lv, rv),
        SpecBinOp::Shr => ctx.target.lshr(lv, rv),
        SpecBinOp::Eq => ctx.target.icmp(IcmpPred::Eq, lv, rv),
        SpecBinOp::Ne => ctx.target.icmp(IcmpPred::Ne, lv, rv),
        SpecBinOp::Lt => ctx.target.icmp(IcmpPred::Ult, lv, rv),
        SpecBinOp::Le => ctx.target.icmp(IcmpPred::Ule, lv, rv),
        SpecBinOp::Gt => ctx.target.icmp(IcmpPred::Ugt, lv, rv),
        SpecBinOp::Ge => ctx.target.icmp(IcmpPred::Uge, lv, rv),
        SpecBinOp::And => ctx.target.and(lv, rv),
        SpecBinOp::Or => ctx.target.or(lv, rv),
    }
}

fn lower_unop<T: LirTarget<P>, P: Clone>(
    op: SpecUnaryOp,
    v: T::Value,
    ctx: &mut LowerCtx<T, P>,
) -> T::Value {
    match op {
        SpecUnaryOp::Not => ctx.target.not(v),
        SpecUnaryOp::Neg => {
            let ty = ctx.target.value_scalar_type(&v);
            let zero = ctx.target.iconst(ty, 0);
            ctx.target.sub(zero, v)
        }
        // References / dereferences are transparent in value-semantics LIR.
        SpecUnaryOp::Deref | SpecUnaryOp::Ref | SpecUnaryOp::RefMut => v,
    }
}

// ============================================================================
// If/else lowering
// ============================================================================

fn lower_if<T: LirTarget<P>, P: Clone>(
    cond_expr: &IrExpr<P>,
    then_branch: &IrBlock<P>,
    else_branch: Option<&IrExpr<P>>,
    ctx: &mut LowerCtx<T, P>,
) -> Vec<T::Value> {
    let cond_val = into_scalar(lower_expr(cond_expr, ctx), "if condition");

    let then_block = ctx.target.create_block();
    let else_block = ctx.target.create_block();

    // Branch to then/else — join block created lazily below.
    ctx.target.branch(
        cond_val,
        then_block.clone(),
        BranchTarget::args([]),
        else_block.clone(),
        BranchTarget::args([]),
    );

    // Lower the then-branch first so we discover N (the result scalar count).
    ctx.target.switch_to_block(then_block.clone());
    ctx.current_block = then_block;
    let then_vals = lower_block(then_branch, ctx);
    let n = then_vals.len();

    // Recover the scalar LirType of each then-value to use as join-block param types.
    let scalar_tys: Vec<LirType> = then_vals
        .iter()
        .map(|v| ctx.target.value_scalar_type(v))
        .collect();

    // Create the join block now that we know N.
    let join_block = ctx.target.create_block();
    let join_params: Vec<T::Value> = scalar_tys
        .iter()
        .map(|ty| ctx.target.add_block_param(join_block.clone(), ty.clone()))
        .collect();

    ctx.target
        .jump(join_block.clone(), BranchTarget::args(then_vals.clone()));

    // Lower the else-branch.
    ctx.target.switch_to_block(else_block.clone());
    ctx.current_block = else_block;
    let else_vals: Vec<T::Value> = if let Some(else_expr) = else_branch {
        lower_expr(else_expr, ctx)
    } else {
        // No else clause — produce N zeroed scalars matching the then-branch shape.
        scalar_tys
            .iter()
            .map(|ty| ctx.target.iconst(ty.clone(), 0))
            .collect()
    };
    assert_eq!(
        else_vals.len(),
        n,
        "if/else branches produce different numbers of scalars ({n} vs {})",
        else_vals.len()
    );
    ctx.target
        .jump(join_block.clone(), BranchTarget::args(else_vals.clone()));

    ctx.target.switch_to_block(join_block.clone());
    ctx.current_block = join_block;
    join_params
}

// ============================================================================
// Match lowering
// ============================================================================

fn lower_match<T: LirTarget<P>, P: Clone>(
    scrutinee: &IrExpr<P>,
    arms: &[volar_compiler::ir::IrMatchArm<P>],
    ctx: &mut LowerCtx<T, P>,
) -> Vec<T::Value> {
    let scrutinee_vals = lower_expr(scrutinee, ctx);
    let scrutinee_ty = ctx.infer_type(scrutinee);

    // Determine whether this is an enum match or primitive match.
    let is_enum = scrutinee_ty.as_ref().map_or(false, |ty| {
        let base_ty = match ty {
            IrType::Reference { elem, .. } => elem.as_ref(),
            other => other,
        };
        matches!(base_ty, IrType::Struct { kind, .. } if ctx.enum_registry.is_enum(kind))
    });

    if is_enum && !scrutinee_vals.is_empty() {
        let tag = scrutinee_vals[0].clone();
        let payload = scrutinee_vals[1..].to_vec();
        lower_enum_match(tag, payload, arms, ctx)
    } else {
        // Primitive match: compare scrutinee scalar against literal patterns.
        lower_primitive_match(&scrutinee_vals, arms, ctx)
    }
}

/// Lower a match over a primitive value (integer or bool literals).
fn lower_primitive_match<T: LirTarget<P>, P: Clone>(
    scrutinee_vals: &[T::Value],
    arms: &[volar_compiler::ir::IrMatchArm<P>],
    ctx: &mut LowerCtx<T, P>,
) -> Vec<T::Value> {
    // Emit an if-else chain: for each arm check scrutinee == literal, else fallthrough.
    // The last arm must be `Wild` (catch-all).
    let scrutinee = scrutinee_vals
        .first()
        .cloned()
        .unwrap_or_else(|| ctx.target.iconst(LirType::Bool, 0));

    // Emit an if-else chain via the recursive arm handler.

    // Iterative if-else construction using a pre-built list.
    let mut remaining = arms.iter().peekable();
    lower_match_arm_chain(&scrutinee, &mut remaining, ctx)
}

fn lower_match_arm_chain<'a, T: LirTarget<P>, P: Clone>(
    scrutinee: &T::Value,
    arms: &mut std::iter::Peekable<std::slice::Iter<'a, volar_compiler::ir::IrMatchArm<P>>>,
    ctx: &mut LowerCtx<T, P>,
) -> Vec<T::Value> {
    use volar_compiler::ir::{IrLit, IrPattern};
    let Some(arm) = arms.next() else {
        return vec![];
    };
    match &arm.pattern {
        IrPattern::Wild | IrPattern::Ident { .. } => {
            // Catch-all: bind if ident, then lower body.
            if let IrPattern::Ident { name, .. } = &arm.pattern {
                ctx.env.insert(name.clone(), vec![scrutinee.clone()]);
            }
            lower_expr(&arm.body, ctx)
        }
        IrPattern::Lit(IrLit::Bool(b)) => {
            let expected = ctx.target.iconst(LirType::Bool, *b as i64);
            let cond = ctx.target.icmp(IcmpPred::Eq, scrutinee.clone(), expected);
            let then_block = ctx.target.create_block();
            let else_block = ctx.target.create_block();
            ctx.target.branch(
                cond,
                then_block.clone(),
                BranchTarget::args([]),
                else_block.clone(),
                BranchTarget::args([]),
            );

            ctx.target.switch_to_block(then_block.clone());
            ctx.current_block = then_block;
            let then_vals = lower_expr(&arm.body, ctx);
            let n = then_vals.len();
            let scalar_tys: Vec<LirType> = then_vals
                .iter()
                .map(|v| ctx.target.value_scalar_type(v))
                .collect();
            let join_block = ctx.target.create_block();
            let join_params: Vec<T::Value> = scalar_tys
                .iter()
                .map(|ty| ctx.target.add_block_param(join_block.clone(), ty.clone()))
                .collect();
            ctx.target
                .jump(join_block.clone(), BranchTarget::args(then_vals.clone()));

            ctx.target.switch_to_block(else_block.clone());
            ctx.current_block = else_block;
            let else_vals = lower_match_arm_chain(scrutinee, arms, ctx);
            let else_vals = if else_vals.len() == n {
                else_vals
            } else {
                scalar_tys
                    .iter()
                    .map(|ty| ctx.target.iconst(ty.clone(), 0))
                    .collect()
            };
            ctx.target
                .jump(join_block.clone(), BranchTarget::args(else_vals.clone()));

            ctx.target.switch_to_block(join_block.clone());
            ctx.current_block = join_block;
            join_params
        }
        IrPattern::Lit(IrLit::Int(v)) => {
            let val = *v as i64;
            let expected = ctx.target.iconst(LirType::U64, val);
            let cond = ctx.target.icmp(IcmpPred::Eq, scrutinee.clone(), expected);
            let then_block = ctx.target.create_block();
            let else_block = ctx.target.create_block();
            ctx.target.branch(
                cond,
                then_block.clone(),
                BranchTarget::args([]),
                else_block.clone(),
                BranchTarget::args([]),
            );

            ctx.target.switch_to_block(then_block.clone());
            ctx.current_block = then_block;
            let then_vals = lower_expr(&arm.body, ctx);
            let n = then_vals.len();
            let scalar_tys: Vec<LirType> = then_vals
                .iter()
                .map(|v| ctx.target.value_scalar_type(v))
                .collect();
            let join_block = ctx.target.create_block();
            let join_params: Vec<T::Value> = scalar_tys
                .iter()
                .map(|ty| ctx.target.add_block_param(join_block.clone(), ty.clone()))
                .collect();
            ctx.target
                .jump(join_block.clone(), BranchTarget::args(then_vals.clone()));

            ctx.target.switch_to_block(else_block.clone());
            ctx.current_block = else_block;
            let else_vals = lower_match_arm_chain(scrutinee, arms, ctx);
            let else_vals = if else_vals.len() == n {
                else_vals
            } else {
                scalar_tys
                    .iter()
                    .map(|ty| ctx.target.iconst(ty.clone(), 0))
                    .collect()
            };
            ctx.target
                .jump(join_block.clone(), BranchTarget::args(else_vals.clone()));

            ctx.target.switch_to_block(join_block.clone());
            ctx.current_block = join_block;
            join_params
        }
        other => unimplemented!("lower_primitive_match: unsupported arm pattern {:?}", other),
    }
}

/// Lower a match over an enum value: flat scalars `[tag, payload...]`.
///
/// Emits an if-else chain: for each variant arm check `tag == discriminant`,
/// bind the payload slice to the pattern, lower the body.  A wild/ident arm
/// (or absence of one) provides the fallthrough.
fn lower_enum_match<T: LirTarget<P>, P: Clone>(
    tag: T::Value,
    payload: Vec<T::Value>,
    arms: &[volar_compiler::ir::IrMatchArm<P>],
    ctx: &mut LowerCtx<T, P>,
) -> Vec<T::Value> {
    use volar_compiler::ir::IrPattern;

    let join_block = ctx.target.create_block();
    let mut join_params: Option<Vec<T::Value>> = None;
    let mut scalar_tys: Vec<LirType> = vec![];

    // Separate variant arms from the catch-all.
    let mut variant_arms: Vec<&volar_compiler::ir::IrMatchArm<P>> = vec![];
    let mut catch_all: Option<&volar_compiler::ir::IrMatchArm<P>> = None;
    for arm in arms {
        match &arm.pattern {
            IrPattern::Wild | IrPattern::Ident { .. } => catch_all = Some(arm),
            _ => variant_arms.push(arm),
        }
    }

    for arm in &variant_arms {
        // Resolve discriminant and payload binding from the pattern.
        let (disc, bound_payload) = enum_arm_disc_and_payload(arm, &payload, ctx);

        let disc_val = ctx.target.iconst(LirType::U8, disc as i64);
        let cond = ctx.target.icmp(IcmpPred::Eq, tag.clone(), disc_val);
        let then_block = ctx.target.create_block();
        let else_block = ctx.target.create_block();
        ctx.target.branch(
            cond,
            then_block.clone(),
            BranchTarget::args([]),
            else_block.clone(),
            BranchTarget::args([]),
        );

        ctx.target.switch_to_block(then_block.clone());
        ctx.current_block = then_block;
        // Bind pattern variables to their payload slices.
        bind_enum_arm_pattern(arm, &bound_payload, ctx);
        let then_vals = lower_expr(&arm.body, ctx);

        if join_params.is_none() {
            scalar_tys = then_vals
                .iter()
                .map(|v| ctx.target.value_scalar_type(v))
                .collect();
            join_params = Some(
                scalar_tys
                    .iter()
                    .map(|ty| ctx.target.add_block_param(join_block.clone(), ty.clone()))
                    .collect(),
            );
        }
        ctx.target
            .jump(join_block.clone(), BranchTarget::args(then_vals.clone()));

        ctx.target.switch_to_block(else_block.clone());
        ctx.current_block = else_block;
    }

    // Emit catch-all / wildcard body.
    let catch_vals = if let Some(arm) = catch_all {
        if let IrPattern::Ident { name, .. } = &arm.pattern {
            // Bind the whole flattened enum value (tag + payload) to the ident.
            let mut all = vec![tag.clone()];
            all.extend(payload.iter().cloned());
            ctx.env.insert(name.clone(), all);
        }
        lower_expr(&arm.body, ctx)
    } else {
        scalar_tys
            .iter()
            .map(|ty| ctx.target.iconst(ty.clone(), 0))
            .collect()
    };

    let Some(jp) = join_params else {
        // No variant arms — only catch-all.
        return catch_vals;
    };
    ctx.target
        .jump(join_block.clone(), BranchTarget::args(catch_vals.clone()));
    ctx.target.switch_to_block(join_block.clone());
    ctx.current_block = join_block;
    jp
}

/// Return the discriminant of a variant arm and the relevant payload slice.
fn enum_arm_disc_and_payload<T: LirTarget<P>, P: Clone>(
    arm: &volar_compiler::ir::IrMatchArm<P>,
    payload: &[T::Value],
    ctx: &LowerCtx<T, P>,
) -> (u64, Vec<T::Value>) {
    use volar_compiler::ir::{IrPattern, StructKind};
    let variant_name = match &arm.pattern {
        IrPattern::TupleStruct { kind, .. } | IrPattern::Struct { kind, .. } => match kind {
            StructKind::Custom(n) => n.as_str(),
            StructKind::GenericArray => "Array",
        },
        IrPattern::Ident { name, .. } => name.as_str(),
        _ => return (0, payload.to_vec()),
    };
    let (disc, pay_width) = ctx
        .enum_registry
        .find_variant(variant_name)
        .map(|(_, v)| (v.discriminant, v.payload_lir_tys.len()))
        .unwrap_or((0, payload.len()));
    let slice = payload[..pay_width.min(payload.len())].to_vec();
    (disc, slice)
}

/// Bind the sub-patterns of an enum arm to slices of the payload.
fn bind_enum_arm_pattern<T: LirTarget<P>, P: Clone>(
    arm: &volar_compiler::ir::IrMatchArm<P>,
    payload: &[T::Value],
    ctx: &mut LowerCtx<T, P>,
) {
    use volar_compiler::ir::{IrPattern, StructKind};
    match &arm.pattern {
        IrPattern::TupleStruct { kind, elems } => {
            let variant_name = match kind {
                StructKind::Custom(n) => n.as_str(),
                StructKind::GenericArray => "Array",
            };
            let payload_tys: Vec<LirType> = ctx
                .enum_registry
                .find_variant(variant_name)
                .map(|(_, v)| v.payload_lir_tys.clone())
                .unwrap_or_default();
            let mut offset = 0;
            for (sub_pat, lir_ty) in elems.iter().zip(payload_tys.iter()) {
                let w = flatten_count(lir_ty, ctx.registry);
                let slice = payload[offset..(offset + w).min(payload.len())].to_vec();
                bind_pattern(sub_pat, slice, None, ctx);
                offset += w;
            }
        }
        IrPattern::Struct { kind, fields, .. } => {
            let variant_name = match kind {
                StructKind::Custom(n) => n.as_str(),
                StructKind::GenericArray => "Array",
            };
            let variant_entry = ctx.enum_registry.find_variant(variant_name);
            if let Some((_, v)) = variant_entry {
                let mut offset = 0;
                for (field_name, sub_pat) in fields {
                    let field_idx = v
                        .field_names
                        .as_ref()
                        .and_then(|names| names.iter().position(|n| n == field_name))
                        .unwrap_or(0);
                    let w = v
                        .payload_lir_tys
                        .get(field_idx)
                        .map(|t| flatten_count(t, ctx.registry))
                        .unwrap_or(1);
                    let slice = payload[offset..(offset + w).min(payload.len())].to_vec();
                    bind_pattern(sub_pat, slice, None, ctx);
                    offset += w;
                }
            }
        }
        IrPattern::Wild => {}
        IrPattern::Ident { name, .. } => {
            ctx.env.insert(name.clone(), payload.to_vec());
        }
        _ => {}
    }
}

/// Lower `expr?` — early-return on Err/None (tag ≠ 0), continue with Ok/Some payload.
fn lower_try<T: LirTarget<P>, P: Clone>(
    inner: &IrExpr<P>,
    ctx: &mut LowerCtx<T, P>,
) -> Vec<T::Value> {
    let vals = lower_expr(inner, ctx);
    if vals.is_empty() {
        return vals;
    }
    let tag = vals[0].clone();
    let ok_disc = ctx.target.iconst(LirType::U8, 0);
    let is_ok = ctx.target.icmp(IcmpPred::Eq, tag, ok_disc);
    let ok_block = ctx.target.create_block();
    let err_block = ctx.target.create_block();
    ctx.target.branch(
        is_ok,
        ok_block.clone(),
        BranchTarget::args([]),
        err_block.clone(),
        BranchTarget::args([]),
    );
    // Err path: propagate error via early return.
    ctx.target.switch_to_block(err_block.clone());
    ctx.current_block = err_block;
    ctx.target.ret(&vals);
    // Ok path: unwrap the payload (drop tag).
    ctx.target.switch_to_block(ok_block.clone());
    ctx.current_block = ok_block;
    vals[1..].to_vec()
}

// ============================================================================
// Phase 2: field access
// ============================================================================

fn lower_field<T: LirTarget<P>, P: Clone>(
    base: &IrExpr<P>,
    field: &str,
    ctx: &mut LowerCtx<T, P>,
) -> Vec<T::Value> {
    let base_ir_ty = ctx
        .infer_type(base)
        .unwrap_or_else(|| panic!("could not infer type for field access .{field}"));

    // Handle tuple field access (`.0`, `.1`, ...).
    if let IrType::Tuple(ref elems) = base_ir_ty {
        let idx: usize = field
            .parse()
            .unwrap_or_else(|_| panic!("tuple field access with non-numeric field `.{field}`"));
        assert!(
            idx < elems.len(),
            "tuple index {idx} out of bounds (tuple has {} elements)",
            elems.len()
        );

        // Compute scalar offset = sum of flattened widths of elements 0..idx.
        let offset: usize = elems[..idx]
            .iter()
            .map(|ty| flatten_count(&ctx.registry.ir_type_to_lir(ty, ctx.mono), ctx.registry))
            .sum();
        let width = flatten_count(
            &ctx.registry.ir_type_to_lir(&elems[idx], ctx.mono),
            ctx.registry,
        );

        let base_vals = lower_expr(base, ctx);
        return base_vals[offset..offset + width].to_vec();
    }

    // Peel any reference layers before the struct lookup.
    let peeled = match &base_ir_ty {
        IrType::Reference { elem, .. } => elem.as_ref().clone(),
        other => other.clone(),
    };
    let (struct_kind, type_args) = match &peeled {
        IrType::Struct { kind, type_args } => (kind, type_args.as_slice()),
        other => panic!("field .{field} on non-struct type {other:?}"),
    };

    let mono_args: Vec<IrType> = type_args.iter().map(|a| mono_type(a, ctx.mono)).collect();
    let struct_id = ctx
        .registry
        .id_for_instance(struct_kind, &mono_args)
        .unwrap_or_else(|| {
            panic!(
                "struct '{}' not in registry for field .{field}",
                structs::nominal_instance_name(struct_kind, &mono_args)
            )
        });

    let field_idx = ctx.registry.field_index(struct_id, field);
    let offset = struct_field_scalar_offset(ctx.registry, struct_id, field_idx);
    let width = struct_field_scalar_width(ctx.registry, struct_id, field_idx);

    let base_vals = lower_expr(base, ctx);
    base_vals[offset..offset + width].to_vec()
}

// ============================================================================
// Phase 2: struct construction
// ============================================================================

fn lower_struct_expr<T: LirTarget<P>, P: Clone>(
    kind: &StructKind,
    type_args: &[IrType],
    fields: &[(String, IrExpr<P>)],
    ctx: &mut LowerCtx<T, P>,
) -> Vec<T::Value> {
    let mut mono_args: Vec<IrType> = type_args.iter().map(|a| mono_type(a, ctx.mono)).collect();
    // `Wrap { value: x }` parses with empty type_args; infer from field exprs.
    if mono_args.is_empty() {
        if let Some(ir_struct) = ctx.module_structs.iter().find(|s| s.kind == *kind) {
            if !ir_struct.generics.is_empty() {
                let mut local = MonoEnv::new(ctx.mono.hash_suffix.clone());
                for field_def in &ir_struct.fields {
                    let Some((_, expr)) = fields.iter().find(|(n, _)| n == &field_def.name) else {
                        continue;
                    };
                    let Some(concrete) = ctx.infer_type(expr) else {
                        continue;
                    };
                    if let IrType::TypeParam(name) = &field_def.ty {
                        local.type_params.insert(name.clone(), concrete);
                    }
                }
                mono_args = ir_struct
                    .generics
                    .iter()
                    .filter_map(|parameter| match parameter.kind {
                        volar_compiler::ir::IrGenericParamKind::Type => {
                            local.type_params.get(&parameter.name).cloned()
                        }
                        volar_compiler::ir::IrGenericParamKind::Const => local
                            .const_params
                            .get(&parameter.name)
                            .map(|&n| IrType::TypeParam(n.to_string())),
                        volar_compiler::ir::IrGenericParamKind::Lifetime => None,
                    })
                    .collect();
            }
        }
    }
    let struct_id = ctx
        .registry
        .id_for_instance(kind, &mono_args)
        .unwrap_or_else(|| {
            panic!(
                "struct '{}' not in registry",
                structs::nominal_instance_name(kind, &mono_args)
            )
        });

    let field_map: BTreeMap<&str, &IrExpr<P>> =
        fields.iter().map(|(n, e)| (n.as_str(), e)).collect();

    let decl_names: Vec<String> = ctx.registry.field_names(struct_id).to_vec();
    // Concatenate flat scalar lists for all fields in declaration order.
    decl_names
        .iter()
        .flat_map(|name| {
            let expr = field_map
                .get(name.as_str())
                .copied()
                .or_else(|| fields.iter().find(|(n, _)| n == name).map(|(_, e)| e))
                .unwrap_or_else(|| panic!("StructExpr missing field '{name}'"));
            lower_expr(expr, ctx)
        })
        .collect()
}

// ============================================================================
// Phase 2: fixed-size array literal
// ============================================================================

fn lower_fixed_array<T: LirTarget<P>, P: Clone>(
    elems: &[IrExpr<P>],
    ctx: &mut LowerCtx<T, P>,
) -> Vec<T::Value> {
    // Concatenate the flat scalar lists of all elements directly — no arr_new.
    elems.iter().flat_map(|e| lower_expr(e, ctx)).collect()
}

/// Lower a fixed-size `[value; N]` literal by concretely unrolling it.
///
/// LIR arrays are flattened values, so this must duplicate the element's
/// lowered scalar values rather than emit a target-specific aggregate literal.
fn lower_repeat_array<T: LirTarget<P>, P: Clone>(
    elem: &IrExpr<P>,
    len: &IrExpr<P>,
    ctx: &mut LowerCtx<T, P>,
) -> Vec<T::Value> {
    let n = resolve_const_expr_len(len, ctx.mono);
    let values = lower_expr(elem, ctx);
    (0..n).flat_map(|_| values.iter().cloned()).collect()
}

/// Recognizes `0`, `false`, and a bare `T::default()` call — the only
/// element-expression shapes `Box::new([elem; N])`'s own lowering currently
/// accepts (see its doc for why: relying on the heap allocation's own
/// zero-init instead of N individual stores).
fn is_zero_default_expr<P: Clone>(expr: &IrExpr<P>) -> bool {
    match &expr.kind {
        IrExprKind::Lit(IrLit::Bool(false)) | IrExprKind::Lit(IrLit::Int(0)) => true,
        IrExprKind::Call { func, args } if args.is_empty() => {
            matches!(&func.kind, IrExprKind::Path { segments, .. } if segments.last().map(String::as_str) == Some("default"))
        }
        // A tuple of recognized-default elements (e.g. `_and_pool`'s own
        // `(Vope::default(), Array::default())` element type) is itself
        // recognized-default — `calloc`'s zero-init is still correct field
        // by field.
        IrExprKind::Tuple(elems) => elems.iter().all(is_zero_default_expr),
        _ => false,
    }
}

/// Resolve an expression-typed array length (an `IrExprKind::Repeat`'s own
/// `len` field, e.g. from `[elem; N]`) to a concrete `usize` — shared by
/// [`lower_repeat_array`] and `Box::new([elem; N])`'s own lowering.
fn resolve_const_expr_len<P: Clone>(len: &IrExpr<P>, mono: &MonoEnv) -> usize {
    match &len.kind {
        IrExprKind::Lit(IrLit::Int(value)) => *value as usize,
        IrExprKind::Var(name) => mono
            .const_params
            .get(name)
            .or_else(|| mono.consts.get(name.as_str()))
            .copied()
            .unwrap_or_else(|| panic!("array length '{name}' is not concrete")),
        IrExprKind::Path { segments, .. } if segments.len() == 2 && segments[1] == "USIZE" => mono
            .const_params
            .get(&segments[0])
            .or_else(|| mono.consts.get(segments[0].as_str()))
            .copied()
            .unwrap_or_else(|| panic!("array length '{}' is not concrete", segments[0])),
        _ => panic!("array length must be a concrete integer"),
    }
}

// ============================================================================
// Phase 2: ArrayGenerate (from_fn / closure-based array fill)
// ============================================================================

fn lower_array_generate<T: LirTarget<P>, P: Clone>(
    _elem_ty: Option<&IrType>,
    len: &ArrayLength,
    index_var: &str,
    body: &IrExpr<P>,
    ctx: &mut LowerCtx<T, P>,
) -> Vec<T::Value> {
    let n = const_len(len, ctx.mono);

    // Unroll: evaluate body once per index, concatenate flat scalar lists.
    let result: Vec<T::Value> = (0..n)
        .flat_map(|k| {
            let idx_val = ctx.target.iconst(LirType::U64, k as i64);
            ctx.env.insert(index_var.to_owned(), vec![idx_val]);
            ctx.env_types.insert(
                index_var.to_owned(),
                IrType::Primitive(PrimitiveType::Usize),
            );
            lower_expr(body, ctx)
        })
        .collect();

    ctx.env.remove(index_var);
    ctx.env_types.remove(index_var);
    result
}

// ============================================================================
// Phase 2: RawMap (element-wise unary array op)
// ============================================================================

fn lower_raw_map<T: LirTarget<P>, P: Clone>(
    receiver: &IrExpr<P>,
    elem_var: &IrPattern,
    body: &IrExpr<P>,
    ctx: &mut LowerCtx<T, P>,
) -> Vec<T::Value> {
    let recv_ty = ctx
        .infer_type(receiver)
        .unwrap_or_else(|| panic!("RawMap: could not infer receiver type"));
    let (elem_ir_ty, n) = array_elem_and_len(&recv_ty, ctx.mono);
    let elem_lir_ty = ctx.registry.ir_type_to_lir(&elem_ir_ty, ctx.mono);
    let elem_width = flatten_count(&elem_lir_ty, ctx.registry);

    let recv_vals = lower_expr(receiver, ctx); // n * elem_width scalars

    (0..n)
        .flat_map(|k| {
            let elem_vals = recv_vals[k * elem_width..(k + 1) * elem_width].to_vec();
            bind_map_pattern(elem_var, elem_vals, &elem_ir_ty, ctx);
            lower_expr(body, ctx)
        })
        .collect()
}

// ============================================================================
// Phase 2: RawZip (element-wise binary array op)
// ============================================================================

fn lower_raw_zip<T: LirTarget<P>, P: Clone>(
    left: &IrExpr<P>,
    right: &IrExpr<P>,
    left_var: &IrPattern,
    right_var: &IrPattern,
    body: &IrExpr<P>,
    ctx: &mut LowerCtx<T, P>,
) -> Vec<T::Value> {
    let left_ty = ctx
        .infer_type(left)
        .unwrap_or_else(|| panic!("RawZip: could not infer left type"));
    let (elem_ir_ty, n) = array_elem_and_len(&left_ty, ctx.mono);
    let elem_lir_ty = ctx.registry.ir_type_to_lir(&elem_ir_ty, ctx.mono);
    let elem_width = flatten_count(&elem_lir_ty, ctx.registry);

    let left_vals = lower_expr(left, ctx);
    let right_vals = lower_expr(right, ctx);

    (0..n)
        .flat_map(|k| {
            let lv = left_vals[k * elem_width..(k + 1) * elem_width].to_vec();
            let rv = right_vals[k * elem_width..(k + 1) * elem_width].to_vec();
            bind_map_pattern(left_var, lv, &elem_ir_ty, ctx);
            bind_map_pattern(right_var, rv, &elem_ir_ty, ctx);
            lower_expr(body, ctx)
        })
        .collect()
}

fn bind_map_pattern<T: LirTarget<P>, P: Clone>(
    pattern: &IrPattern,
    vals: Vec<T::Value>,
    ir_ty: &IrType,
    ctx: &mut LowerCtx<T, P>,
) {
    match pattern {
        IrPattern::Ident { name, .. } => {
            ctx.env.insert(name.clone(), vals);
            ctx.env_types.insert(name.clone(), ir_ty.clone());
        }
        IrPattern::Wild => {}
        other => unimplemented!("bind_map_pattern: {:?}", other),
    }
}


// ============================================================================
// IterPipeline (unrolled iterator chain)
// ============================================================================

/// Bind one pipeline element to its pattern. Tuple patterns (e.g. `(i, b)`
/// after `enumerate()`) split into (index, elem); identifiers bind the elem.
fn bind_chain_elem_pattern<T: LirTarget<P>, P: Clone>(
    pattern: &IrPattern,
    index: Option<usize>,
    elem_vals: &[T::Value],
    elem_ty: &IrType,
    ctx: &mut LowerCtx<T, P>,
) {
    match pattern {
        IrPattern::Tuple(sub_pats) if sub_pats.len() == 2 => {
            if let Some(i) = index {
                bind_map_pattern(&sub_pats[0],
                    vec![ctx.target.iconst(LirType::U64, i as i64)],
                    &IrType::Primitive(PrimitiveType::U64), ctx);
            } else {
                unimplemented!("IterChain tuple pattern without enumerate index");
            }
            bind_map_pattern(&sub_pats[1], elem_vals.to_vec(), elem_ty, ctx);
        }
        IrPattern::Ident { .. } | IrPattern::Wild => {
            bind_map_pattern(pattern, elem_vals.to_vec(), elem_ty, ctx);
        }
        other => unimplemented!("IterChain elem pattern {other:?}"),
    }
}

/// Lower an iterator chain by unrolling it into per-element computations.
///
/// Supported shapes (everything the VOLE/FAEST spec bodies use):
/// - sources: `expr.iter()` / `.into_iter()` over a flat array, and ranges;
/// - steps: `map`, `enumerate` (in any relative order);
/// - terminals: `fold(init, |acc, elem| ..)` and `.collect()`.
fn lower_iter_chain<T: LirTarget<P>, P: Clone>(
    chain: &volar_compiler::ir::IrIterChain<P>,
    ctx: &mut LowerCtx<T, P>,
) -> Vec<T::Value> {
    use volar_compiler::ir::{IterChainSource, IterStep, IterTerminal};

    // ---- Gather (index, elem) pairs from the source ----
    let mut elems: Vec<(Option<usize>, Vec<T::Value>, IrType)> = match &chain.source {
        IterChainSource::Method { collection, .. } => {
            let coll_ty = ctx
                .infer_type(collection)
                .unwrap_or_else(|| panic!("IterChain: could not infer collection type"));
            let (elem_ir_ty, n) = array_elem_and_len(&coll_ty, ctx.mono);
            let elem_lir_ty = ctx.registry.ir_type_to_lir(&elem_ir_ty, ctx.mono);
            let elem_width = flatten_count(&elem_lir_ty, ctx.registry);
            let recv_vals = lower_expr(collection, ctx);
            (0..n)
                .map(|k| {
                    (
                        None,
                        recv_vals[k * elem_width..(k + 1) * elem_width].to_vec(),
                        elem_ir_ty.clone(),
                    )
                })
                .collect()
        }
        IterChainSource::Range { start, end, .. } => {
            let s = concrete_usize_expr(start, ctx)
                .expect("IterChain range start must be concrete");
            let e = concrete_usize_expr(end, ctx)
                .expect("IterChain range end must be concrete");
            (s..e)
                .map(|i| {
                    (
                        Some(i),
                        vec![ctx.target.iconst(LirType::U64, i as i64)],
                        IrType::Primitive(PrimitiveType::U64),
                    )
                })
                .collect()
        }
        other => unimplemented!("IterChain source {:?}", std::mem::discriminant(other)),
    };

    // ---- Apply intermediate steps ----
    for step in &chain.steps {
        match step {
            IterStep::Enumerate => {
                for (k, (idx, _, _)) in elems.iter_mut().enumerate() {
                    if idx.is_none() {
                        *idx = Some(k);
                    }
                }
            }
            IterStep::Map { var, body } => {
                let mut next = Vec::with_capacity(elems.len());
                for (k, (idx, vals, ty)) in elems.into_iter().enumerate() {
                    let body_ty_hint = ctx.infer_type(body);
                    bind_map_pattern(var, vals.clone(), &ty, ctx);
                    let out = lower_expr(body, ctx);
                    let out_ty = body_ty_hint.unwrap_or_else(|| ty.clone());
                    if let IrPattern::Ident { name, .. } = var {
                        ctx.env.remove(name);
                        ctx.env_types.remove(name);
                    }
                    next.push((idx.or(Some(k)), out, out_ty));
                }
                elems = next;
            }
            other => unimplemented!("IterChain step {:?}", std::mem::discriminant(other)),
        }
    }

    // ---- Terminal ----
    match &chain.terminal {
        IterTerminal::Fold {
            init,
            acc_var,
            elem_var,
            body,
        } => {
            let init_ty = ctx.infer_type(init);
            let acc_ty = init_ty.unwrap_or_else(|| {
                elems.first().map(|(_, _, ty)| ty.clone()).unwrap_or(IrType::Primitive(PrimitiveType::U64))
            });
            let mut acc_vals = lower_expr(init, ctx);
            for (idx, elem_vals, elem_ty) in &elems {
                bind_chain_elem_pattern(elem_var, *idx, elem_vals, elem_ty, ctx);
                bind_map_pattern(acc_var, acc_vals.clone(), &acc_ty, ctx);
                acc_vals = lower_expr(body, ctx);
            }
            if let IrPattern::Ident { name, .. } = elem_var {
                ctx.env.remove(name);
                ctx.env_types.remove(name);
            }
            if let IrPattern::Ident { name, .. } = acc_var {
                ctx.env.remove(name);
                ctx.env_types.remove(name);
            }
            acc_vals
        }
        IterTerminal::Collect | IterTerminal::CollectTyped(_) => {
            let mut out: Vec<T::Value> = Vec::new();
            for (_, vals, _) in &elems {
                out.extend(vals.iter().cloned());
            }
            out
        }
        other => unimplemented!("IterChain terminal {:?}", std::mem::discriminant(other)),
    }
}
// ============================================================================
// RawFold (unrolled accumulation over a flat array)
// ============================================================================

fn lower_raw_fold<T: LirTarget<P>, P: Clone>(
    receiver: &IrExpr<P>,
    init: &IrExpr<P>,
    acc_var: &IrPattern,
    elem_var: &IrPattern,
    body: &IrExpr<P>,
    ctx: &mut LowerCtx<T, P>,
) -> Vec<T::Value> {
    let recv_ty = ctx
        .infer_type(receiver)
        .unwrap_or_else(|| panic!("RawFold: could not infer receiver type"));
    let (elem_ir_ty, n) = array_elem_and_len(&recv_ty, ctx.mono);
    let elem_lir_ty = ctx.registry.ir_type_to_lir(&elem_ir_ty, ctx.mono);
    let elem_width = flatten_count(&elem_lir_ty, ctx.registry);

    let recv_vals = lower_expr(receiver, ctx);
    let init_ty = ctx.infer_type(init);
    let mut acc_vals = lower_expr(init, ctx);

    for k in 0..n {
        let elem_vals = recv_vals[k * elem_width..(k + 1) * elem_width].to_vec();
        bind_map_pattern(elem_var, elem_vals, &elem_ir_ty, ctx);
        let acc_ty = init_ty.as_ref().unwrap_or(&elem_ir_ty);
        bind_map_pattern(acc_var, acc_vals, acc_ty, ctx);
        acc_vals = lower_expr(body, ctx);
    }

    // Clean up loop variables from env.
    if let IrPattern::Ident { name, .. } = elem_var {
        ctx.env.remove(name);
        ctx.env_types.remove(name);
    }
    if let IrPattern::Ident { name, .. } = acc_var {
        ctx.env.remove(name);
        ctx.env_types.remove(name);
    }
    acc_vals
}

fn array_elem_and_len(ty: &IrType, env: &MonoEnv) -> (IrType, usize) {
    match ty {
        IrType::Array { elem, len, .. } => (*elem.clone(), const_len(len, env)),
        IrType::Reference { elem, .. } => array_elem_and_len(elem, env),
        other => panic!("expected array type, got {:?}", other),
    }
}

// ============================================================================
// Slice-reference helpers
// ============================================================================

/// Check if an IrType should use real pointer-indexed access (`ptr_index_load`/
/// `ptr_index_store`) rather than the O(n) select-mux-tree fallback: either a
/// reference to a slice (`&[T]`/`&mut [T]`), or a `Box<[T; N]>` (see
/// `volar_compiler::ir::box_type`'s own doc) — both lower to `LirType::Ptr`,
/// so both need the same real-pointer indexing rather than the mux tree,
/// which is what a plain unwrapped (stack-flattened) `IrType::Array` gets.
/// This matters a lot in practice: a `Box`ed pool with thousands of slots
/// indexed via the mux-tree fallback would emit a `select` chain over every
/// slot on every single read/write — exactly the code-volume blowup pooling
/// exists to avoid.
fn is_slice_ref(ty: &IrType) -> bool {
    matches!(ty,
        IrType::Reference { elem, .. }
            if matches!(elem.as_ref(), IrType::Array { kind: ArrayKind::Slice, .. })
    ) || matches!(
        volar_compiler::ir::as_box_type(ty),
        Some(IrType::Array { .. })
    )
}

/// Extract the element type from a `Reference<Slice<T>>` or `Box<[T; N]>`.
fn lower_bounded_loop<T: LirTarget<P>, P: Clone>(
    var: &str,
    start: &IrExpr<P>,
    end: &IrExpr<P>,
    inclusive: bool,
    body: &IrBlock<P>,
    ctx: &mut LowerCtx<T, P>,
) {
    // Const-generic spec loops have a concrete trip count after planning.
    // Unroll them so updates to flattened local aggregates remain ordinary SSA
    // bindings. A CFG back-edge can only carry block parameters; the old path
    // carried `i` but silently discarded assignments to e.g. `result[i]`.
    if let (Some(start), Some(end)) = (
        concrete_usize_expr(start, ctx),
        concrete_usize_expr(end, ctx),
    ) {
        let end = end
            .checked_add(usize::from(inclusive))
            .expect("loop bound overflow");
        let prior_values = ctx.env.remove(var);
        let prior_type = ctx.env_types.remove(var);
        for index in start..end {
            let value = ctx.target.iconst(LirType::U64, index as i64);
            ctx.env.insert(var.to_owned(), vec![value]);
            ctx.env_types
                .insert(var.to_owned(), IrType::Primitive(PrimitiveType::Usize));
            lower_block(body, ctx);
        }
        if let Some(values) = prior_values {
            ctx.env.insert(var.to_owned(), values);
        } else {
            ctx.env.remove(var);
        }
        if let Some(ty) = prior_type {
            ctx.env_types.insert(var.to_owned(), ty);
        } else {
            ctx.env_types.remove(var);
        }
        return;
    }

    // Strategy: loop_header(counter: U64, limit: U64)
    //
    // before:
    //   start_val = lower(start)
    //   end_val   = lower(end)
    //   if inclusive: limit = end_val + 1, else limit = end_val
    //   jump loop_header(start_val, limit)
    //
    // loop_header(counter, limit):
    //   cmp = counter < limit
    //   branch cmp → body_block, done_block
    //
    // body_block:
    //   env[var] = counter
    //   lower(body)
    //   next = counter + 1
    //   jump loop_header(next, limit)
    //
    // done_block:
    //   (unit)

    let loop_header = ctx.target.create_block();
    let body_block = ctx.target.create_block();
    let done_block = ctx.target.create_block();

    let counter = ctx
        .target
        .add_block_param(loop_header.clone(), LirType::U64);
    let limit = ctx
        .target
        .add_block_param(loop_header.clone(), LirType::U64);

    // Before block: compute start and end.
    let start_val = into_scalar(lower_expr(start, ctx), "loop start");
    let end_val = into_scalar(lower_expr(end, ctx), "loop end");
    let limit_val = if inclusive {
        let one = ctx.target.iconst(LirType::U64, 1);
        ctx.target.add(end_val, one)
    } else {
        end_val
    };
    // Ensure values are U64 (the loop uses U64 counters).
    let start_u64 = ctx.target.zext(start_val, LirType::U64);
    ctx.target.jump(
        loop_header.clone(),
        BranchTarget::args(vec![start_u64, limit_val.clone()]),
    );

    // Loop header.
    ctx.target.switch_to_block(loop_header.clone());
    ctx.current_block = loop_header.clone();
    let cmp = ctx
        .target
        .icmp(IcmpPred::Ult, counter.clone(), limit.clone());
    ctx.target.branch(
        cmp,
        body_block.clone(),
        BranchTarget::args([]),
        done_block.clone(),
        BranchTarget::args([]),
    );

    // Body block.
    ctx.target.switch_to_block(body_block.clone());
    ctx.current_block = body_block;
    ctx.env.insert(var.to_owned(), vec![counter.clone()]);
    ctx.env_types
        .insert(var.to_owned(), IrType::Primitive(PrimitiveType::Usize));
    lower_block(body, ctx);
    let one = ctx.target.iconst(LirType::U64, 1);
    let next = ctx.target.add(counter, one);
    ctx.target.jump(
        loop_header,
        BranchTarget::args(vec![next, limit_val])
            .with_reentry(ReentryHint::bounded_loop_ascending()),
    );

    // Done block.
    ctx.target.switch_to_block(done_block.clone());
    ctx.current_block = done_block;

    // Clean up loop variable from env.
    ctx.env.remove(var);
    ctx.env_types.remove(var);
}

/// Panics if `ty` is neither (see [`is_slice_ref`]).
fn slice_ref_elem(ty: &IrType) -> IrType {
    if let Some(IrType::Array { elem: inner, .. }) = volar_compiler::ir::as_box_type(ty) {
        return *inner.clone();
    }
    match ty {
        IrType::Reference { elem, .. } => match elem.as_ref() {
            IrType::Array { elem: inner, .. } => *inner.clone(),
            _ => panic!("slice_ref_elem: not a slice"),
        },
        _ => panic!("slice_ref_elem: not a reference or Box"),
    }
}

// ============================================================================
// Phase 2: array index (runtime mux tree)
// ============================================================================

fn lower_index<T: LirTarget<P>, P: Clone>(
    base: &IrExpr<P>,
    index: &IrExpr<P>,
    ctx: &mut LowerCtx<T, P>,
) -> Vec<T::Value> {
    let base_ir_ty = ctx.infer_type(base).unwrap_or_else(|| {
        panic!(
            "Index: could not infer base type, base {}, caller={:?}",
            match &base.kind {
                IrExprKind::Var(n) => format!("var '{n}'"),
                IrExprKind::Field { base: fb, field, .. } => {
                    if let IrExprKind::Var(vn) = &fb.kind {
                        format!("var '{vn}.{field}'")
                    } else {
                        format!("field .{field}")
                    }
                }
                other => format!("kind {:?}", std::mem::discriminant(other)),
            },
            ctx.current_instance.map(|k| k.source_name.clone()),
        )
    });

    // Pointer-based indexing: Reference<Slice<T>> → Ptr(T) in LIR.
    // Use ptr_index_load instead of the flat mux tree.
    if is_slice_ref(&base_ir_ty) {
        let elem_ir_ty = slice_ref_elem(&base_ir_ty);
        let elem_lir_ty = ctx.registry.ir_type_to_lir(&elem_ir_ty, ctx.mono);
        let ptr_vals = lower_expr(base, ctx);
        let ptr = ptr_vals
            .into_iter()
            .next()
            .expect("pointer should be a single scalar");
        let idx_val = into_scalar(lower_expr(index, ctx), "array index");
        return ctx.target.ptr_index_load(ptr, idx_val, &elem_lir_ty);
    }

    let (elem_ir_ty, n) = array_elem_and_len(&base_ir_ty, ctx.mono);
    let elem_lir_ty = ctx.registry.ir_type_to_lir(&elem_ir_ty, ctx.mono);
    let elem_width = flatten_count(&elem_lir_ty, ctx.registry);

    let arr_vals = lower_expr(base, ctx); // n * elem_width scalars
    let idx_val = into_scalar(lower_expr(index, ctx), "array index");

    // Build a select mux tree for each scalar position within the element.
    // result[j] = select chain over arr_vals[0*ew+j], arr_vals[1*ew+j], ...
    (0..elem_width)
        .map(|j| {
            let mut result = arr_vals[j].clone(); // element 0's j-th scalar
            for k in 1..n {
                let k_val = ctx.target.iconst(LirType::U64, k as i64);
                let cond = ctx.target.icmp(IcmpPred::Eq, idx_val.clone(), k_val);
                result = ctx
                    .target
                    .select(cond, arr_vals[k * elem_width + j].clone(), result);
            }
            result
        })
        .collect()
}

// ============================================================================
// Phase 2: assignment (storage writes, variable updates)
// ============================================================================

fn lower_assign<T: LirTarget<P>, P: Clone>(
    left: &IrExpr<P>,
    right: &IrExpr<P>,
    ctx: &mut LowerCtx<T, P>,
) {
    match &left.kind {
        // Assignment to an indexed location: base[index] = rhs
        IrExprKind::Index { base, index } => {
            let base_ir_ty = ctx.infer_type(base).unwrap_or_else(|| {
                panic!(
                    "Assign: could not infer indexed base type, base kind {:?}",
                    std::mem::discriminant(&base.kind)
                )
            });

            if is_slice_ref(&base_ir_ty) {
                // Pointer-based store: ptr[idx] = pack(rhs_vals)
                let elem_ir_ty = slice_ref_elem(&base_ir_ty);
                let elem_lir_ty = ctx.registry.ir_type_to_lir(&elem_ir_ty, ctx.mono);
                let ptr_vals = lower_expr(base, ctx);
                let ptr = ptr_vals
                    .into_iter()
                    .next()
                    .expect("pointer should be a single scalar");
                let idx_val = into_scalar(lower_expr(index, ctx), "assign index");
                let rhs_vals = lower_expr(right, ctx);
                ctx.target
                    .ptr_index_store(ptr, idx_val, &rhs_vals, &elem_lir_ty);
            } else {
                // In-memory array: update env with new values.
                // For flat-scalar arrays this replaces the slice at the right index.
                let (elem_ir_ty, _n) = array_elem_and_len(&base_ir_ty, ctx.mono);
                let elem_lir_ty = ctx.registry.ir_type_to_lir(&elem_ir_ty, ctx.mono);
                let elem_width = flatten_count(&elem_lir_ty, ctx.registry);

                // Peel nested index chains (`result[j][i] = …`): collect the
                // index expressions outermost-first, derive each subscripted
                // array's length from the base type, and fold into a single
                // linear element position over the flattened aggregate
                // (outer index major, matching flatten order).
                let mut chain: Vec<&IrExpr<P>> = Vec::new(); // index exprs, outermost first
                let mut chain_bases_last = base;
                {
                    // The outermost index is the arm's own `index`; peel the
                    // remaining nested Index layers from `base`.
                    chain.push(index);
                    let mut cursor = base;
                    while let IrExprKind::Index { base: inner, index } = &cursor.kind {
                        chain.insert(0, index);
                        cursor = inner;
                    }
                    chain_bases_last = cursor;
                }
                // Array lengths per depth: base type is Array<L0, len0>; the
                // element subscripted by chain[0] is L0, whose length feeds
                // the multiplier for chain[0], etc.
                let mut lens: Vec<Option<usize>> = Vec::new();
                {
                    let mut ty = Some(&base_ir_ty);
                    for _ in 0..chain.len() {
                        match ty {
                            Some(IrType::Array { elem, len, .. }) => {
                                lens.push(mono::array_len_const(&mono_len(len, ctx.mono)));
                                ty = Some(elem.as_ref());
                            }
                            _ => {
                                lens.push(None);
                                ty = None;
                            }
                        }
                    }
                }
                // pos = Σ_{d} idx[d] * Π_{e>d} len[e]
                if chain.is_empty() {
                    panic!("Assign: empty index chain for base type {base_ir_ty:?}");
                }
                let mut pos = into_scalar(lower_expr(chain[0], ctx), "assign index");
                for (d, index_expr) in chain.iter().enumerate().skip(1) {
                    // product of lens[d+1..]
                    let mut factor: Option<usize> = Some(1usize);
                    for l in lens[d..].iter() {
                        match (factor, l) {
                            (Some(f), Some(v)) => factor = Some(f * v),
                            _ => {
                                factor = None;
                                break;
                            }
                        }
                    }
                    let idx_v = into_scalar(lower_expr(index_expr, ctx), "assign index");
                    let scaled = match factor {
                        Some(f) if f > 1 => {
                            let f_val = ctx.target.iconst(LirType::U64, f as i64);
                            ctx.target.mul(pos.clone(), f_val)
                        }
                        _ => pos.clone(),
                    };
                    pos = ctx.target.add(scaled, idx_v);
                }
                let idx_val = pos;
                let rhs_vals = lower_expr(right, ctx);

                // The innermost base after peeling must be a variable.
                let final_base = chain_bases_last;
                // We need the variable name to update env.
                if let IrExprKind::Var(name) = &final_base.kind {
                    let mut arr_vals = ctx
                        .env
                        .get(name)
                        .cloned()
                        .unwrap_or_else(|| panic!("undefined variable: {name}"));
                    if arr_vals.is_empty() {
                        panic!("Assign: {name} has empty env (elem_width={elem_width})");
                    }
                    // For each element position, conditionally update using select.
                    let n = arr_vals.len() / elem_width;
                    for k in 0..n {
                        let k_val = ctx.target.iconst(LirType::U64, k as i64);
                        let cond = ctx.target.icmp(IcmpPred::Eq, idx_val.clone(), k_val);
                        for j in 0..elem_width {
                            if j >= rhs_vals.len() {
                                panic!(
                                    "Assign: rhs empty (rhs_len={} arr_len={elem_width} n={n} base={})",
                                    rhs_vals.len(),
                                    if let IrExprKind::Var(nm) = &base.kind {
                                        nm.as_str()
                                    } else {
                                        "?"
                                    }
                                );
                            }
                            let old = arr_vals[k * elem_width + j].clone();
                            let new = rhs_vals[j].clone();
                            arr_vals[k * elem_width + j] =
                                ctx.target.select(cond.clone(), new, old);
                        }
                    }
                    ctx.env.insert(name.clone(), arr_vals);
                } else {
                    unimplemented!("assign to non-variable indexed base");
                }
            }
        }
        // Simple variable assignment: `x = rhs`
        IrExprKind::Var(name) => {
            let rhs_vals = lower_expr(right, ctx);
            let inferred_ty = ctx.infer_type(right);
            if let Some(ty) = inferred_ty {
                ctx.env_types.insert(name.clone(), ty);
            }
            ctx.env.insert(name.clone(), rhs_vals);
        }
        // Field assignment `struct_var.field = rhs`: update the flattened
        // scalars at the field's offset using the same select pattern as
        // indexed assignment (unconditional here — no runtime index).
        IrExprKind::Field { base, field } => {
            let base_ir_ty = ctx.infer_type(base).unwrap_or_else(|| {
                panic!(
                    "Assign: could not infer base type for field .{field} on base kind {:?}",
                    std::mem::discriminant(&base.kind)
                )
            });
            let base_ir_ty = match base_ir_ty {
                IrType::Reference { elem, .. } => *elem,
                other => other,
            };
            let IrType::Struct { kind, type_args } = &base_ir_ty else {
                unimplemented!("lower_assign: field assign on non-struct {base_ir_ty:?}");
            };
            let ir_struct = ctx
                .module_structs
                .iter()
                .find(|s| s.kind == *kind)
                .unwrap_or_else(|| panic!("Field assign: struct {kind:?} not found"));
            let field_def = ir_struct
                .fields
                .iter()
                .find(|f| f.name == *field)
                .unwrap_or_else(|| panic!("Field assign: no field {field} in {kind:?}"));

            // Field offset in flattened scalars: widths of preceding fields.
            let mut offset = 0usize;
            for f in &ir_struct.fields {
                if f.name == *field {
                    break;
                }
                let ty = crate::mono::mono_type(&f.ty, ctx.mono);
                let lir = ctx.registry.ir_type_to_lir(&ty, ctx.mono);
                offset += flatten_count(&lir, ctx.registry);
            }
            let field_lir = ctx
                .registry
                .ir_type_to_lir(&crate::mono::mono_type(&field_def.ty, ctx.mono), ctx.mono);
            let field_width = flatten_count(&field_lir, ctx.registry);
            let rhs_vals = lower_expr(right, ctx);

            let IrExprKind::Var(name) = &base.kind else {
                unimplemented!("lower_assign: field assign on non-variable base");
            };
            let mut arr_vals = ctx
                .env
                .get(name)
                .cloned()
                .unwrap_or_else(|| panic!("undefined variable: {name}"));
            for j in 0..field_width {
                arr_vals[offset + j] = rhs_vals[j].clone();
            }
            ctx.env.insert(name.clone(), arr_vals);
        }
        // Field assignment is not supported in value-semantics LIR.
        _other => unimplemented!("lower_assign: unsupported lhs"),
    }
}

// ============================================================================
// Phase 2: BoundedLoop
// ============================================================================

fn concrete_usize_expr<T: LirTarget<P>, P: Clone>(
    expr: &IrExpr<P>,
    ctx: &LowerCtx<T, P>,
) -> Option<usize> {
    match &expr.kind {
        IrExprKind::Lit(IrLit::Int(value)) => usize::try_from(*value).ok(),
        IrExprKind::Var(name) => ctx
            .mono
            .const_params
            .get(name)
            .or_else(|| ctx.mono.consts.get(name.as_str()))
            .copied(),
        IrExprKind::Path { segments, .. } if segments.len() == 2 && segments[1] == "USIZE" => ctx
            .mono
            .const_params
            .get(&segments[0])
            .or_else(|| ctx.mono.consts.get(segments[0].as_str()))
            .copied(),
        // Const arithmetic on parameters (`2 * BIG_N`, `BIG_N / 2`): fold
        // recursively through this resolver.
        IrExprKind::Binary { op, left, right } => {
            let l = concrete_usize_expr(left, ctx)?;
            let r = concrete_usize_expr(right, ctx)?;
            Some(match op {
                SpecBinOp::Add => l.wrapping_add(r),
                SpecBinOp::Sub => l.wrapping_sub(r),
                SpecBinOp::Mul => l.wrapping_mul(r),
                SpecBinOp::Div if r != 0 => l / r,
                SpecBinOp::Rem if r != 0 => l % r,
                SpecBinOp::BitAnd => l & r,
                SpecBinOp::BitOr => l | r,
                SpecBinOp::BitXor => l ^ r,
                SpecBinOp::Shl if r < 64 => l << r,
                SpecBinOp::Shr if r < 64 => l >> r,
                _ => return None,
            })
        }
        _ => None,
    }
}

fn lower_bounded_loop_descending<T: LirTarget<P>, P: Clone>(
    pattern: &IrPattern,
    start: &IrExpr<P>,
    end: &IrExpr<P>,
    body: &IrBlock<P>,
    ctx: &mut LowerCtx<T, P>,
) {
    let Some(start) = concrete_usize_expr(start, ctx) else {
        unimplemented!("lower_expr: reversed iterator loop with non-concrete start");
    };
    let Some(end) = concrete_usize_expr(end, ctx) else {
        unimplemented!("lower_expr: reversed iterator loop with non-concrete end");
    };
    let Some(name) = (match pattern {
        IrPattern::Ident { name, .. } => Some(name.clone()),
        _ => None,
    }) else {
        unimplemented!("lower_expr: reversed iterator loop with non-ident pattern");
    };
    let prior_values = ctx.env.remove(&name);
    let prior_type = ctx.env_types.remove(&name);
    let mut indices = Vec::new();
    let mut k = end;
    while k > start {
        k -= 1;
        indices.push(k);
    }
    for index in indices {
        let value = ctx.target.iconst(LirType::U64, index as i64);
        ctx.env.insert(name.clone(), vec![value]);
        ctx.env_types
            .insert(name.clone(), IrType::Primitive(PrimitiveType::Usize));
        lower_block(body, ctx);
    }
    if let Some(values) = prior_values {
        ctx.env.insert(name.clone(), values);
    } else {
        ctx.env.remove(&name);
    }
    if let Some(ty) = prior_type {
        ctx.env_types.insert(name.clone(), ty);
    } else {
        ctx.env_types.remove(&name);
    }
}

fn lower_method_call<T: LirTarget<P>, P: Clone>(
    receiver: &IrExpr<P>,
    method: &MethodKind,
    type_args: &[IrType],
    args: &[IrExpr<P>],
    ctx: &mut LowerCtx<T, P>,
) -> Vec<T::Value> {
    match method {
        // `.clone()` and `.deref()` are transparent in value semantics.
        MethodKind::Known(StdMethod::Clone | StdMethod::Deref) => lower_expr(receiver, ctx),

        // Integer wrapping operations are the target's native modular
        // operations. Emitting an extern here incorrectly requires a runtime
        // symbol such as `wrapping_add` for parsed spec helpers.
        MethodKind::Known(StdMethod::WrappingAdd) => {
            let rhs = into_scalar(lower_expr(&args[0], ctx), "wrapping_add rhs");
            let lhs = into_scalar(lower_expr(receiver, ctx), "wrapping_add receiver");
            vec![ctx.target.add(lhs, rhs)]
        }
        MethodKind::Known(StdMethod::WrappingSub) => {
            let rhs = into_scalar(lower_expr(&args[0], ctx), "wrapping_sub rhs");
            let lhs = into_scalar(lower_expr(receiver, ctx), "wrapping_sub receiver");
            vec![ctx.target.sub(lhs, rhs)]
        }
        MethodKind::Known(StdMethod::WrappingMul) => {
            let rhs = into_scalar(lower_expr(&args[0], ctx), "wrapping_mul rhs");
            let lhs = into_scalar(lower_expr(receiver, ctx), "wrapping_mul receiver");
            vec![ctx.target.mul(lhs, rhs)]
        }
        MethodKind::Known(StdMethod::WrappingNeg) => {
            let lhs = into_scalar(lower_expr(receiver, ctx), "wrapping_neg receiver");
            let zero = ctx.target.iconst(LirType::U64, 0);
            vec![ctx.target.sub(zero, lhs)]
        }
        MethodKind::Known(StdMethod::SaturatingAdd) => {
            let rhs = into_scalar(lower_expr(&args[0], ctx), "saturating_add rhs");
            let lhs = into_scalar(lower_expr(receiver, ctx), "saturating_add receiver");
            let sum = ctx.target.add(lhs.clone(), rhs.clone());
            // Overflow when sum < lhs (unsigned wraparound).
            let wrapped = ctx.target.icmp(IcmpPred::Ult, sum.clone(), lhs);
            let max = ctx.target.iconst(LirType::U64, u64::MAX as i64);
            vec![ctx.target.select(wrapped, max, sum)]
        }
        MethodKind::Known(StdMethod::SaturatingSub) => {
            let rhs = into_scalar(lower_expr(&args[0], ctx), "saturating_sub rhs");
            let lhs = into_scalar(lower_expr(receiver, ctx), "saturating_sub receiver");
            let diff = ctx.target.sub(lhs.clone(), rhs.clone());
            let negative = ctx.target.icmp(IcmpPred::Ult, lhs, rhs);
            let zero = ctx.target.iconst(LirType::U64, 0);
            vec![ctx.target.select(negative, zero, diff)]
        }
        MethodKind::Known(StdMethod::TrailingZeros) => {
            // Unrolled select chain over the 64 bit positions:
            // result = lowest set bit index, or 64 when the value is zero.
            let val = into_scalar(lower_expr(receiver, ctx), "trailing_zeros receiver");
            let mut tz = ctx.target.iconst(LirType::U64, 64);
            for k in 0..64u32 {
                let k_val = ctx.target.iconst(LirType::U64, k as i64);
                let one = ctx.target.iconst(LirType::U64, 1);
                let one_cmp = ctx.target.iconst(LirType::U64, 1);
                let shifted = ctx.target.lshr(val.clone(), k_val);
                let bit = ctx.target.and(shifted, one);
                let is_set = ctx.target.icmp(IcmpPred::Eq, bit, one_cmp);
                let k_val2 = ctx.target.iconst(LirType::U64, k as i64);
                let take_new = ctx.target.icmp(IcmpPred::Ugt, tz.clone(), k_val2);
                let take = ctx.target.and(is_set, take_new);
                let k_val3 = ctx.target.iconst(LirType::U64, k as i64);
                tz = ctx.target.select(take, k_val3, tz);
            }
            vec![tz]
        }

        // Reference methods — transparent.
        MethodKind::Known(StdMethod::AsRef | StdMethod::AsSlice) => lower_expr(receiver, ctx),

        // Other methods → extern call.
        MethodKind::Known(m) => lower_method_extern(receiver, m.as_str(), type_args, args, ctx),
        MethodKind::Other(name) => {
            // Guardrails: ident-char validity + misrouting check.
            debug_assert!(
                name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_'),
                "LIR lower: method name is not a valid identifier: {name:?}"
            );
            debug_assert!(
                StdMethod::try_from_str(name.as_str()).is_none(),
                "LIR lower: {name:?} is a well-known StdMethod but was constructed as \
                 MethodKind::Other; use MethodKind::from_str(\"{name}\") or \
                 MethodKind::Known(StdMethod::…) instead"
            );
            lower_method_extern(receiver, name, type_args, args, ctx)
        }

        other => unimplemented!("lower_method_call: {:?}", other),
    }
}

fn lower_method_extern<T: LirTarget<P>, P: Clone>(
    receiver: &IrExpr<P>,
    method_name: &str,
    type_args: &[IrType],
    args: &[IrExpr<P>],
    ctx: &mut LowerCtx<T, P>,
) -> Vec<T::Value> {
    // A non-std method is a user-defined impl fn: resolve it through the
    // monomorphization plan exactly like a free-function call, with the
    // receiver as the first argument.
    if let (Some(plan), Some(caller)) = (ctx.mono_plan, ctx.current_instance) {
        let generic_key = crate::mono::normalized_args(type_args, ctx.mono);
        let arg_types_key: String = std::iter::once(receiver)
            .chain(args.iter())
            .map(|a| {
                ctx.infer_type(a)
                    .map(|ty| crate::mono::canonical_type(&crate::mono::mono_type(&ty, ctx.mono)))
                    .unwrap_or_default()
            })
            .collect::<Vec<_>>()
            .join("|");
        // Keys match the planner's canonical arg-types string (no generic
        // prefix; see mono.rs calls insert).
        let args_key = arg_types_key;
        if let Some(callee) = plan.local_call_deduce(caller, method_name, &args_key) {
            let emitted_name = plan.emitted_name(callee);
            let ret_ty = ctx
                .func_sigs
                .get(emitted_name)
                .and_then(|sig| sig.return_type.clone());
            let mut arg_tys = Vec::new();
            let mut flat_args = Vec::new();
            for a in std::iter::once(receiver).chain(args.iter()) {
                let a_ty = ctx
                    .infer_type(a)
                    .map(|ty| ctx.registry.ir_type_to_lir(&ty, ctx.mono))
                    .unwrap_or(LirType::U64);
                arg_tys.push(a_ty);
                flat_args.extend(lower_expr(a, ctx));
            }
            return ctx
                .target
                .call_extern(emitted_name, &arg_tys, &flat_args, ret_ty);
        }
    }

    let extern_name = if ctx.mono.hash_suffix.is_empty() {
        method_name.to_owned()
    } else {
        format!("{}_{}", method_name, ctx.mono.hash_suffix)
    };

    // Collect ABI types and flat scalar values for receiver + args.
    let recv_ir_ty = ctx.infer_type(receiver);
    let recv_lir_ty = recv_ir_ty
        .as_ref()
        .map(|t| ctx.registry.ir_type_to_lir(t, ctx.mono))
        .unwrap_or(LirType::U64);

    let mut arg_tys: Vec<LirType> = vec![recv_lir_ty];
    let mut flat_args: Vec<T::Value> = lower_expr(receiver, ctx);

    for a in args {
        let a_ir_ty = ctx.infer_type(a);
        let a_lir_ty = a_ir_ty
            .as_ref()
            .map(|t| ctx.registry.ir_type_to_lir(t, ctx.mono))
            .unwrap_or(LirType::U64);
        arg_tys.push(a_lir_ty);
        flat_args.extend(lower_expr(a, ctx));
    }

    let ret_lir_ty = recv_ir_ty.map(|t| ctx.registry.ir_type_to_lir(&t, ctx.mono));
    ctx.target
        .call_extern(&extern_name, &arg_tys, &flat_args, ret_lir_ty)
}

// ============================================================================
// Phase 2: free function calls
// ============================================================================

fn lower_call<T: LirTarget<P>, P: Clone>(
    func: &IrExpr<P>,
    args: &[IrExpr<P>],
    ctx: &mut LowerCtx<T, P>,
) -> Vec<T::Value> {
    // Handle Array::from_fn(|i| body) — convert on the fly to ArrayGenerate.
    if let IrExprKind::Path {
        segments,
        type_args,
    } = &func.kind
    {
        if segments.len() >= 2
            && (segments[segments.len() - 2] == "Array"
                || segments[segments.len() - 2] == "GenericArray")
            && segments[segments.len() - 1] == "from_fn"
        {
            if let Some(IrExprKind::Closure { params, body, .. }) = args.first().map(|a| &a.kind) {
                if let Some(volar_compiler::ir::IrClosureParam {
                    pattern: IrPattern::Ident { name: idx, .. },
                    ..
                }) = params.first()
                {
                    // Derive element type and length from the call's type_args.
                    // type_args = [elem_type, length_type] for Array::<T, N>::from_fn
                    let elem_ty = type_args.first().map(|t| mono_type(t, ctx.mono));
                    let len = type_args_to_len(type_args.get(1), ctx.mono);
                    return lower_array_generate(elem_ty.as_ref(), &len, idx, body, ctx);
                }
            }
            // Fallback if closure form is unexpected.
            panic!("Array::from_fn with unexpected args — expected a single closure argument");
        }

        // `Box::<[T; N]>::new(core::array::from_fn(|_| elem))` (see
        // `volar_compiler::ir::box_new_array_expr`'s own doc, and
        // `HeapAllocExt`'s doc on why this must NOT unroll into N individual
        // stores): heap-allocate `N` slots of the array type carried on the
        // Path's own `type_args` (the producer is expected to supply this
        // explicitly — see `box_new_array_expr` — rather than relying on
        // inference), and return the resulting pointer as a single scalar
        // value, not the flattened N*width scalars a plain unwrapped array
        // would produce.
        //
        // Relies on the allocation being zero-initialized (`calloc`, see
        // `HeapAllocExt::heap_alloc`'s own doc) matching the generator
        // closure's own real value — true for every current caller
        // (`T::default()`/`0`/`false` element expressions), asserted below
        // rather than assumed, so a future non-default element expression
        // fails loudly instead of silently allocating uninitialized-looking
        // (but not really, just wrong) storage.
        if segments.len() == 2 && segments[0] == "Box" && segments[1] == "new" {
            let inner = args.first().expect("Box::new expects exactly one argument");
            let IrExprKind::Call {
                func: inner_func,
                args: inner_args,
            } = &inner.kind
            else {
                unimplemented!(
                    "lower_call: Box::new(_) is currently only supported for a \
                     `core::array::from_fn(|_| _)` argument — see box_new_array_expr"
                );
            };
            let IrExprKind::Path {
                segments: inner_segments,
                ..
            } = &inner_func.kind
            else {
                unimplemented!(
                    "lower_call: Box::new(_)'s argument must be a core::array::from_fn call"
                );
            };
            if inner_segments.last().map(String::as_str) != Some("from_fn") {
                unimplemented!(
                    "lower_call: Box::new(_) is currently only supported for a \
                     `core::array::from_fn(|_| _)` argument, got a call to {inner_segments:?}"
                );
            }
            let Some(IrExprKind::Closure { body, .. }) = inner_args.first().map(|a| &a.kind) else {
                unimplemented!(
                    "lower_call: Box::new(core::array::from_fn(_))'s argument must be a closure"
                );
            };
            if !is_zero_default_expr(body) {
                unimplemented!(
                    "lower_call: Box::new(core::array::from_fn(|_| elem))'s elem must be a \
                     recognized zero/Default::default()-shaped expression (relies on the heap \
                     allocation's own zero-init matching elem's real value — see \
                     HeapAllocExt::heap_alloc's doc); got a different shape, which needs real \
                     per-slot initialization, and must NOT be unrolled into N individual stores \
                     (defeats pooling's whole purpose)"
                );
            }
            let array_ir_ty = type_args
                .first()
                .map(|t| mono_type(t, ctx.mono))
                .unwrap_or_else(|| panic!("Box::new: missing array type_arg on the Box::new path — see box_new_array_expr"));
            let (elem_ir_ty, count) = array_elem_and_len(&array_ir_ty, ctx.mono);
            let elem_lir_ty = ctx.registry.ir_type_to_lir(&elem_ir_ty, ctx.mono);
            let ptr = ctx
                .target
                .heap_alloc_ext()
                .expect("Box::new requires a LirTarget with HeapAllocExt support")
                .heap_alloc(elem_lir_ty, count);
            return vec![ptr];
        }
    }

    let (func_name, type_args) = match &func.kind {
        IrExprKind::Path {
            segments,
            type_args,
        } => (segments.join("_"), type_args.as_slice()),
        IrExprKind::Var(name) => (name.clone(), &[] as &[IrType]),
        _other => unimplemented!("lower_call: non-path func"),
    };

    // ---- Enum variant construction (Tuple variant) -------------------------
    // Check if this call is `EnumKind::VariantName(args...)` — a tuple variant constructor.
    // Both "Some" and "Option_Some" (joined path) are recognized.
    if let Some((entry, variant)) = ctx.enum_registry.find_variant(&func_name) {
        let tag = ctx.target.iconst(LirType::U8, variant.discriminant as i64);
        let mut scalars: Vec<T::Value> = vec![tag];
        for a in args {
            scalars.extend(lower_expr(a, ctx));
        }
        let max_w = entry.max_payload_width;
        while scalars.len() < 1 + max_w {
            scalars.push(ctx.target.iconst(LirType::U8, 0));
        }
        return scalars;
    }

    // ---- Dispatch oracle / action / rng ------------------------------------
    if let Some(info) = ctx.external_fns.get(&func_name) {
        return match info.kind {
            ExternalKind::Oracle => {
                // Oracle: all call-site args are the actual function args.
                // Use declared parameter types from the function signature.
                let arg_tys: Vec<LirType> = info.param_tys.clone();
                let mut flat_args: Vec<T::Value> = Vec::new();
                for a in args {
                    flat_args.extend(lower_expr(a, ctx));
                }
                let ret_tys: Vec<LirType> = info.return_type.iter().cloned().collect();
                ctx.target
                    .oracle(&func_name, &arg_tys, &flat_args, &ret_tys)
            }
            ExternalKind::Action => {
                // Action ABI (encoded in the function declaration):
                //   params[0]                 = guard (Bool)
                //   params[1 .. 1+n_fb]       = fallback values (one per return element)
                //   params[1+n_fb ..]          = real arguments
                //
                // The call-site args match the declaration 1:1.
                let n_declared = info.param_tys.len();

                // Determine n_fb from the IR-level return type.
                let n_fb = match ctx.ir_func_ret_types.get(&func_name) {
                    Some(IrType::Tuple(elems)) => elems.len(),
                    Some(_) => 1,
                    None => 0,
                };

                assert!(
                    args.len() == n_declared,
                    "action '{func_name}': call has {} args but declaration has {n_declared} params",
                    args.len()
                );

                // Guard (first arg).
                let guard_vals = lower_expr(&args[0], ctx);
                let guard = guard_vals
                    .into_iter()
                    .next()
                    .expect("action guard must be a scalar");

                // Fallbacks (next n_fb args).
                let mut flat_fallbacks: Vec<T::Value> = Vec::new();
                for a in &args[1..1 + n_fb] {
                    flat_fallbacks.extend(lower_expr(a, ctx));
                }

                // Real arguments (remaining args).
                // Use declared parameter types from the function signature
                // rather than inferring from expressions — infer_type cannot
                // handle complex expressions like RawMap.
                let arg_tys: Vec<LirType> = info.param_tys[1 + n_fb..].to_vec();
                let mut flat_args: Vec<T::Value> = Vec::new();
                for a in args[1 + n_fb..].iter() {
                    flat_args.extend(lower_expr(a, ctx));
                }

                let ret_tys: Vec<LirType> = info.return_type.iter().cloned().collect();
                ctx.target.action(
                    &func_name,
                    guard,
                    &arg_tys,
                    &flat_args,
                    &flat_fallbacks,
                    &ret_tys,
                )
            }
            ExternalKind::Rng => {
                // Rng: no arguments, return type from declaration.
                let ret_ty = info.return_type.clone().unwrap_or(LirType::U64);
                vec![ctx.target.rng(ret_ty)]
            }
            _ => unreachable!(),
        };
    }

    // ---- Planned local call -------------------------------------------------
    // The plan owns specialization resolution, so a source-name collision
    // cannot accidentally call a differently-instantiated local function.
    if let (Some(plan), Some(caller)) = (ctx.mono_plan, ctx.current_instance) {
        // Keys match the planner's canonical arg-types string (no generic
        // prefix; see mono.rs calls insert).
        let args_key: String = args
            .iter()
            .map(|a| {
                ctx.infer_type(a)
                    .map(|ty| mono::canonical_type(&crate::mono::mono_type(&ty, ctx.mono)))
                    .unwrap_or_default()
            })
            .collect::<Vec<_>>()
            .join("|");
        if let Some(callee) = plan.local_call_deduce(caller, &func_name, &args_key) {
            let emitted_name = plan.emitted_name(callee);
            let ret_ty = ctx
                .func_sigs
                .get(emitted_name)
                .and_then(|sig| sig.return_type.clone());
            let mut arg_tys = Vec::new();
            let mut flat_args = Vec::new();
            for arg in args {
                let arg_ty = ctx
                    .infer_type(arg)
                    .map(|ty| ctx.registry.ir_type_to_lir(&ty, ctx.mono))
                    .unwrap_or(LirType::U64);
                arg_tys.push(arg_ty);
                flat_args.extend(lower_expr(arg, ctx));
            }
            return ctx
                .target
                .call_extern(emitted_name, &arg_tys, &flat_args, ret_ty);
        }
    }

    // ---- Default: call_extern ----------------------------------------------
    // Look up the called function's signature for return-type inference.
    // Without this, intra-module calls would discard their return value.
    let ret_ty = ctx
        .func_sigs
        .get(&func_name)
        .and_then(|sig| sig.return_type.clone());

    let mut arg_tys: Vec<LirType> = Vec::new();
    let mut flat_args: Vec<T::Value> = Vec::new();
    for a in args {
        let a_ir_ty = ctx.infer_type(a);
        let a_lir_ty = a_ir_ty
            .as_ref()
            .map(|t| ctx.registry.ir_type_to_lir(t, ctx.mono))
            .unwrap_or(LirType::U64);
        arg_tys.push(a_lir_ty);
        flat_args.extend(lower_expr(a, ctx));
    }

    ctx.target
        .call_extern(&func_name, &arg_tys, &flat_args, ret_ty)
}

// ============================================================================
// CFG Module lowering — IrCfgModule → LirTarget (direct block map)
// ============================================================================

/// Lower all functions in an `IrCfgModule` to `target` using an empty `MonoEnv`.
///
/// Auxiliary functions (flat bodies from linked specs) are lowered through the
/// normal `lower_function_in_module` path.  CFG-structured circuit functions
/// map each `IrCfgBlock` directly to a LIR block, using the target's native
/// `create_block`/`jump`/`branch`/`ret`.
pub fn lower_cfg_module<T: LirTarget>(module: &IrCfgModule, target: &mut T) {
    lower_cfg_module_monomorphized(module, target, MonoPlanOptions::default())
        .unwrap_or_else(|err| panic!("LIR CFG monomorphization failed: {err}"));
}

/// Plan-API CFG lowering.
///
/// Validates that flat auxiliaries are a finite, bindable specialization set
/// under [`plan_flat_module`], then emits via [`lower_cfg_module_with_opts`]
/// using the first root's environment (or empty). Multi-instance CFG body
/// emission shares that planner for callee discovery; CFG functions themselves
/// remain single-env in this pass (weaver CFG entry points are non-generic).
pub fn lower_cfg_module_monomorphized<T: LirTarget>(
    module: &IrCfgModule,
    target: &mut T,
    options: MonoPlanOptions,
) -> Result<(), MonoError> {
    let flat: IrModule<IrFunction> = IrModule {
        name: module.name.clone(),
        structs: module.structs.clone(),
        enums: module.enums.clone(),
        traits: module.traits.clone(),
        impls: module.impls.clone(),
        functions: module
            .functions
            .iter()
            .filter_map(|f| {
                if let IrAnyFunction::Flat(f) = f {
                    Some(f.clone())
                } else {
                    None
                }
            })
            .collect(),
        type_aliases: module.type_aliases.clone(),
        consts: module.consts.clone(),
    };

    let cfg_env = options
        .roots
        .first()
        .map(|r| r.env.clone())
        .unwrap_or_else(|| MonoEnv::new(""));

    // Ensure auxiliary specializations are plannable before emission.
    let flat_roots: Vec<MonoRoot> = if options.roots.is_empty() {
        Vec::new()
    } else {
        options
            .roots
            .iter()
            .filter(|r| flat.functions.iter().any(|f| f.name == r.function))
            .cloned()
            .collect()
    };
    let _plan = mono::plan_flat_module(&flat, &flat_roots, options.max_instances)?;

    lower_cfg_module_with_opts(module, target, &cfg_env, options.include_auxiliary);
    Ok(())
}

/// Like `lower_cfg_module` but with a `MonoEnv` and control over auxiliary
/// function lowering.
///
/// When `include_auxiliary` is `false`, only CFG functions are lowered.
/// This is useful for backends (like C) that cannot handle the Rust-specific
/// constructs typically found in linked spec auxiliary functions.
pub fn lower_cfg_module_with_opts<T: LirTarget>(
    module: &IrCfgModule,
    target: &mut T,
    env: &MonoEnv,
    include_auxiliary: bool,
) {
    // Build struct registry from the CFG module's structs.
    // We need a temporary flat IrModule to reuse `build_struct_registry`.
    let flat: IrModule<IrFunction> = IrModule {
        name: module.name.clone(),
        structs: module.structs.clone(),
        enums: module.enums.clone(),
        traits: module.traits.clone(),
        impls: module.impls.clone(),
        functions: module
            .functions
            .iter()
            .filter_map(|f| {
                if let IrAnyFunction::Flat(f) = f {
                    Some(f.clone())
                } else {
                    None
                }
            })
            .collect(),
        type_aliases: module.type_aliases.clone(),
        consts: module.consts.clone(),
    };
    let mut registry = structs::build_struct_registry(&flat, target, env);
    let enum_registry = structs::build_enum_registry(&module.enums, &mut registry, target, env);
    // When skipping auxiliary functions, enable lenient mode so that external
    // types (e.g. TFHE scheme types not defined in the module) are treated
    // as opaque rather than causing a panic.
    if !include_auxiliary {
        registry.lenient = true;
    }

    // Pre-register synthetic structs for any tuple types appearing in function
    // signatures.  This must happen before `ir_type_to_lir` encounters tuples.
    for func in &module.functions {
        let (params, return_type) = match func {
            IrAnyFunction::Flat(f) => (&f.params, &f.return_type),
            IrAnyFunction::Cfg(f) => (&f.params, &f.return_type),
        };
        for p in params {
            structs::register_tuples_in_type(&p.ty, &mut registry, target, env);
        }
        if let Some(rt) = return_type {
            structs::register_tuples_in_type(rt, &mut registry, target, env);
        }
    }

    // Build external-fn and func-sigs tables from both flat and CFG functions.
    let mut external_fns: BTreeMap<String, ExternalFnInfo> = BTreeMap::new();
    let mut func_sigs: BTreeMap<String, FuncSigInfo> = BTreeMap::new();
    // IR-level return types for type inference at call sites.
    let mut ir_func_ret_types: BTreeMap<String, IrType> = BTreeMap::new();

    // Always register metadata (external_fns, func_sigs, ir_func_ret_types)
    // from all functions — even when `include_auxiliary` is false for flat bodies.
    // The CFG functions call action/oracle/rng functions defined as flat entries,
    // so we need their signatures and external-kind info for lowering.
    // `include_auxiliary` only controls whether we *lower flat function bodies*.
    for func in &module.functions {
        let (name, params, return_type, external_kind) = match func {
            IrAnyFunction::Flat(f) => (&f.name, &f.params, &f.return_type, f.external_kind),
            IrAnyFunction::Cfg(f) => (&f.name, &f.params, &f.return_type, f.external_kind),
        };
        let param_tys: Vec<LirType> = params
            .iter()
            .map(|p| registry.ir_type_to_lir(&p.ty, env))
            .collect();
        let ret_lir = return_type
            .as_ref()
            .map(|t| registry.ir_type_to_lir(t, env));
        func_sigs.insert(
            name.clone(),
            FuncSigInfo {
                param_tys: param_tys.clone(),
                return_type: ret_lir.clone(),
            },
        );
        if let Some(rt) = return_type {
            ir_func_ret_types.insert(name.clone(), rt.clone());
        }
        if matches!(
            external_kind,
            ExternalKind::Oracle | ExternalKind::Action | ExternalKind::Rng
        ) {
            external_fns.insert(
                name.clone(),
                ExternalFnInfo {
                    kind: external_kind,
                    param_tys,
                    return_type: ret_lir,
                },
            );
        }
    }

    // Lower flat functions (auxiliary spec functions).
    if include_auxiliary {
        for func in &module.functions {
            if let IrAnyFunction::Flat(func) = func {
                if func.external_kind != ExternalKind::Normal {
                    continue;
                }
                lower_function_in_module(
                    func,
                    target,
                    &registry,
                    &enum_registry,
                    env,
                    &module.structs,
                    &module.enums,
                    &external_fns,
                    &func_sigs,
                    &ir_func_ret_types,
                );
            }
        }
    }

    // Lower CFG functions.
    for func in &module.functions {
        if let IrAnyFunction::Cfg(func) = func {
            if func.external_kind != ExternalKind::Normal {
                continue;
            }
            lower_cfg_function(
                func,
                target,
                &registry,
                &enum_registry,
                env,
                &module.structs,
                &module.enums,
                &external_fns,
                &func_sigs,
                &ir_func_ret_types,
            );
        }
    }
}

/// Lower a single `IrCfgFunction` by mapping each CFG block to a LIR block.
fn lower_cfg_function<T: LirTarget<P>, P: Clone>(
    func: &IrCfgFunction<P>,
    target: &mut T,
    registry: &StructRegistry,
    enum_registry: &EnumRegistry,
    env: &MonoEnv,
    module_structs: &[volar_compiler::ir::IrStruct],
    module_enums: &[IrEnum],
    external_fns: &BTreeMap<String, ExternalFnInfo>,
    func_sigs: &BTreeMap<String, FuncSigInfo>,
    ir_func_ret_types: &BTreeMap<String, IrType>,
) {
    let blocks = &func.body.blocks;

    // Compute LIR param types and return type.
    let param_lir_tys: Vec<LirType> = func
        .params
        .iter()
        .map(|p| registry.ir_type_to_lir(&p.ty, env))
        .collect();
    let ret_ty = func
        .return_type
        .as_ref()
        .map(|t| registry.ir_type_to_lir(t, env));

    // Begin the function — gets the entry block (block 0) and its parameter values.
    let (entry, param_val_groups) = target.begin_function(&func.name, &param_lir_tys, ret_ty);

    // Create LIR blocks for each CFG block.  Block 0 = entry.
    let mut lir_blocks: Vec<T::Block> = Vec::new();
    lir_blocks.push(entry.clone());
    for _ in 1..blocks.len() {
        lir_blocks.push(target.create_block());
    }

    // Add block parameters for blocks 1.. (block 0 uses function params).
    // Collect the resulting LIR values for use when binding params at block start.
    let mut block_param_vals: Vec<Vec<Vec<T::Value>>> = Vec::new();
    block_param_vals.push(Vec::new()); // block 0 — no block params, uses fn params
    for (bidx, blk) in blocks.iter().enumerate().skip(1) {
        let mut param_groups = Vec::new();
        for param in &blk.params {
            let lir_ty = registry.ir_type_to_lir(&param.ty, env);
            // add_block_param returns a single LIR value; for aggregate types
            // we need to flatten (same as begin_function).
            let scalar_tys = structs::flatten_scalar_types(&lir_ty, registry);
            let mut vals = Vec::new();
            for sty in &scalar_tys {
                vals.push(target.add_block_param(lir_blocks[bidx].clone(), sty.clone()));
            }
            param_groups.push(vals);
        }
        block_param_vals.push(param_groups);
    }

    // Now emit each block.
    for (bidx, blk) in blocks.iter().enumerate() {
        target.switch_to_block(lir_blocks[bidx].clone());

        // Set up LowerCtx with the correct params in env.
        let named_params: Vec<(String, Vec<T::Value>, IrType)> = if bidx == 0 {
            // Block 0: use function params.
            func.params
                .iter()
                .zip(param_val_groups.iter())
                .map(|(p, vals)| (p.name.clone(), vals.clone(), p.ty.clone()))
                .collect()
        } else {
            // Blocks 1..: use block params.
            blk.params
                .iter()
                .zip(block_param_vals[bidx].iter())
                .map(|(p, vals)| (p.name.clone(), vals.clone(), p.ty.clone()))
                .collect()
        };

        let mut ctx = LowerCtx::new(
            target,
            lir_blocks[bidx].clone(),
            named_params,
            registry,
            enum_registry,
            env,
            module_structs,
            module_enums,
        );
        ctx.external_fns = external_fns;
        ctx.func_sigs = func_sigs;
        ctx.ir_func_ret_types = ir_func_ret_types;

        // Lower statements.
        for stmt in &blk.stmts {
            lower_stmt(stmt, &mut ctx);
        }

        // Emit terminator.
        lower_cfg_terminator(&blk.terminator, &lir_blocks, &mut ctx, registry);
    }

    target.end_function();
}

/// Emit LIR instructions for a CFG terminator.
fn lower_cfg_terminator<T: LirTarget<P>, P: Clone>(
    term: &IrCfgTerminator<P>,
    lir_blocks: &[T::Block],
    ctx: &mut LowerCtx<T, P>,
    _registry: &StructRegistry,
) {
    match term {
        IrCfgTerminator::Return(None) => {
            ctx.target.ret(&[]);
        }
        IrCfgTerminator::Return(Some(expr)) => {
            let vals = lower_expr(expr, ctx);
            ctx.target.ret(&vals);
        }
        IrCfgTerminator::Goto(jump) => {
            let args = lower_jump_args(jump, ctx);
            ctx.target.jump(
                lir_blocks[jump.target].clone(),
                branch_target_from_jump::<T, P>(jump, args),
            );
        }
        IrCfgTerminator::CondGoto { cond, then_, else_ } => {
            let cond_vals = lower_expr(cond, ctx);
            let cond_val = into_scalar(cond_vals, "CondGoto condition");
            let then_args = lower_jump_args(then_, ctx);
            let else_args = lower_jump_args(else_, ctx);
            ctx.target.branch(
                cond_val,
                lir_blocks[then_.target].clone(),
                branch_target_from_jump::<T, P>(then_, then_args),
                lir_blocks[else_.target].clone(),
                branch_target_from_jump::<T, P>(else_, else_args),
            );
        }
        _ => panic!(
            "lower_cfg_terminator: unhandled IrCfgTerminator variant — add lowering for this variant"
        ),
    }
}

fn branch_target_from_jump<T: LirTarget<P>, P: Clone>(
    jump: &IrCfgJump<P>,
    args: Vec<T::Value>,
) -> BranchTarget<T::Value> {
    match &jump.reentry {
        Some(h) => BranchTarget::args(args).with_reentry(h.clone()),
        None => BranchTarget::args(args),
    }
}

/// Lower the argument expressions for a CFG jump, flattening to scalar values.
fn lower_jump_args<T: LirTarget<P>, P: Clone>(
    jump: &IrCfgJump<P>,
    ctx: &mut LowerCtx<T, P>,
) -> Vec<T::Value> {
    let mut all = Vec::new();
    for arg in &jump.args {
        all.extend(lower_expr(arg, ctx));
    }
    all
}
