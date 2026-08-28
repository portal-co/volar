// @reliability: normal
// @ai: assisted
#![allow(dead_code)]
//! Monomorphization support for IR→LIR lowering.
//!
//! `MonoEnv` carries substitutions for const/length parameters and type parameters.
//! Lowering functions (`lower_module_with_opts`, etc.) accept a `&MonoEnv` and
//! apply these substitutions on the fly — no separate pre-lowering pass needed.
//! Trait-dispatch parameters (e.g. `D: Digest`) use `MonoEnv::hash_suffix`.

use std::collections::{BTreeMap, VecDeque};

use crate::structs::kind_name;
use volar_compiler::ir::{
    ArrayKind, ArrayLength, IrAnyFunction, IrCfgBlock, IrCfgBody, IrCfgFunction, IrCfgJump,
    IrCfgModule, IrCfgTerminator, IrEnum, IrEnumVariant, IrEnumVariantData, IrExpr, IrExprKind,
    IrField, IrFunction, IrImpl, IrImplItem, IrLit, IrModule, IrParam, IrPattern, IrStmt,
    IrStmtKind, IrStruct, IrType, IrTypeAlias, MethodKind, SpecBinOp, SpecUnaryOp, StructKind,
    TypeNumConst,
};

// ============================================================================
// Array length helpers
// ============================================================================

/// Parse a typenum-style unsigned marker (`U0`…`U64`, and any `U{digits}` such
/// as `U3` used for VOLE polynomial degree).
pub(crate) fn typenum_usize(name: &str) -> Option<usize> {
    if let Some(tn) = TypeNumConst::from_str(name) {
        return Some(tn.to_usize());
    }
    let rest = name.strip_prefix('U')?;
    if rest.is_empty() || !rest.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    rest.parse().ok()
}

/// Convert the second type argument of a generic array type (e.g. `Array<T, N>`)
/// to an `ArrayLength`, resolving const params via `env` where possible.
pub(crate) fn type_args_to_len(len_ty: Option<&IrType>, env: &MonoEnv) -> ArrayLength {
    match len_ty {
        Some(IrType::TypeParam(name)) => {
            // The parser represents an integer const argument such as `::<2>`
            // as a TypeParam. Recognize it before looking for an in-scope
            // caller substitution.
            if let Ok(value) = name.parse::<usize>() {
                return ArrayLength::Const(value);
            }
            // Check const_params first (most common: `N` → 16).
            if let Some(&n) = env.const_params.get(name.as_str()) {
                return ArrayLength::Const(n);
            }
            // Check type_params in case this is aliased to a concrete size.
            if let Some(concrete) = env.type_params.get(name.as_str()) {
                return type_args_to_len(Some(concrete), env);
            }
            if let Some(n) = typenum_usize(name) {
                return ArrayLength::Const(n);
            }
            ArrayLength::TypeParam(name.clone())
        }
        Some(IrType::Struct {
            kind: StructKind::Custom(name),
            type_args,
        }) if type_args.is_empty() => {
            // Typenum constant used as a type argument (e.g. `U1`, `U3`, `U16`).
            if let Some(n) = typenum_usize(name) {
                return ArrayLength::Const(n);
            }
            // Fall back to const_params (e.g. env has with_len("U1", 1)).
            if let Some(&n) = env.const_params.get(name.as_str()) {
                return ArrayLength::Const(n);
            }
            ArrayLength::TypeParam(name.clone()) // unresolved
        }
        Some(IrType::Primitive(volar_compiler::ir::PrimitiveType::Usize)) => ArrayLength::Const(1),
        None => ArrayLength::Const(0),
        _ => ArrayLength::Const(0), // unknown — default to 0; lowering will panic with a clear message
    }
}

// ============================================================================
// MonoEnv
// ============================================================================

/// Monomorphization environment: concrete values for type/const parameters.
#[derive(Clone, Debug, PartialEq)]
pub struct MonoEnv {
    /// Substitutions for array-length type parameters.
    /// e.g., `"N" → 16` for `Array<u8, N>` → `Array<u8, 16>`.
    pub const_params: BTreeMap<String, usize>,
    /// Substitutions for type parameters.
    /// e.g., `"T" → IrType::Primitive(U8)` for `fn foo<T>(x: T)`.
    pub type_params: BTreeMap<String, IrType>,
    /// Substitutions for associated type projections.
    /// Key: `(base_param_name, assoc_name)` e.g., `("B", "OutputSize")`.
    pub projections: BTreeMap<(String, String), IrType>,
    /// Concrete name suffix for the `D: Digest` trait parameter.
    /// e.g., `"sha256"`. Appended to crypto extern function names.
    pub hash_suffix: String,
    /// Module-level `const NAME: usize = v;` values (name → value), used to
    /// resolve named constants in array lengths during monomorphization
    /// (e.g. FAEST's `LAMBDA_BYTES`). Populated from `IrModule.consts` by
    /// the planning entry points; left empty for hand-built envs.
    pub consts: BTreeMap<String, usize>,
    /// Struct kind names with a declared `Mul` operator impl (see
    /// `MonoPlan::mul_impl_structs`); needed at planning time to recognize
    /// operator-overload call sites inside function bodies. Populated by
    /// `plan_flat_module`; empty for hand-built envs.
    pub mul_impl_structs: BTreeMap<String, ()>,
}

impl MonoEnv {
    pub fn new(hash_suffix: impl Into<String>) -> Self {
        MonoEnv {
            const_params: BTreeMap::new(),
            type_params: BTreeMap::new(),
            projections: BTreeMap::new(),
            hash_suffix: hash_suffix.into(),
            consts: BTreeMap::new(),
            mul_impl_structs: BTreeMap::new(),
        }
    }

    pub fn with_len(mut self, param: impl Into<String>, len: usize) -> Self {
        self.const_params.insert(param.into(), len);
        self
    }

    pub fn with_type(mut self, param: impl Into<String>, ty: IrType) -> Self {
        self.type_params.insert(param.into(), ty);
        self
    }

    /// Bind `base_param::assoc_name` to a concrete type.
    /// e.g., `.with_projection("B", "OutputSize", Array<u8, 32>)`
    pub fn with_projection(
        mut self,
        base: impl Into<String>,
        assoc: impl Into<String>,
        ty: IrType,
    ) -> Self {
        self.projections.insert((base.into(), assoc.into()), ty);
        self
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MonoError {
    message: String,
}

impl MonoError {
    fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }
}

impl core::fmt::Display for MonoError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_str(&self.message)
    }
}
impl std::error::Error for MonoError {}

/// Concrete identity of one source function instantiation.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionInstanceKey {
    pub source_name: String,
    pub canonical_env: String,
}

/// A closed, deterministic collection of emitted local function instances.
#[derive(Clone, Debug)]
pub struct MonoPlan<P: Clone = ()> {
    pub instances: BTreeMap<FunctionInstanceKey, MonoEnv>,
    pub emitted_names: BTreeMap<FunctionInstanceKey, String>,
    pub calls: BTreeMap<(FunctionInstanceKey, String, String), FunctionInstanceKey>,
    /// Impl methods planned with their receiver materialized as an explicit
    /// first parameter: the parser records only the receiver's ref kind, so
    /// the planner derives a per-instance function whose `self` param carries
    /// the concrete receiver type from the call site (keyed by instance).
    pub materialized: BTreeMap<FunctionInstanceKey, IrFunction<P>>,
    /// Struct kind names whose module declares a `Mul` operator impl
    /// (`impl Mul<Rhs> for Struct`). The parser cannot resolve `a * b`
    /// without type info, so such sites stay as `Binary Mul`; lowering
    /// routes them through the impl's `mul` method via the plan.
    pub mul_impl_structs: std::collections::BTreeMap<String, ()>,
}

impl<P: Clone> MonoPlan<P> {
    pub fn emitted_name(&self, key: &FunctionInstanceKey) -> &str {
        self.emitted_names
            .get(key)
            .expect("planned instance has an emitted name")
    }
    pub fn local_call(
        &self,
        caller: &FunctionInstanceKey,
        callee: &str,
        args: &str,
    ) -> Option<&FunctionInstanceKey> {
        self.calls
            .get(&(caller.clone(), callee.to_owned(), args.to_owned()))
    }

    /// Resolve a local call whose lower-time `args` key may be coarser than
    /// the planning-time key. Planning infers omitted turbofish generics
    /// (so two sites differing only in an impl-level receiver parameter get
    /// distinct keys like `"1"` and `"2"`), while lowering only sees the
    /// written type arguments (`""`). When the exact key misses, fall back
    /// to the unique entry for `(caller, callee)` — ambiguous multi-entry
    /// cases require the exact key.
    pub fn local_call_deduce(
        &self,
        caller: &FunctionInstanceKey,
        callee: &str,
        args: &str,
    ) -> Option<&FunctionInstanceKey> {
        if let Some(found) = self.local_call(caller, callee, args) {
            return Some(found);
        }
        let lower = (caller.clone(), callee.to_owned(), String::new());
        let upper = (caller.clone(), callee.to_owned(), char::MAX.to_string());
        let mut matches = self
            .calls
            .range(lower..=upper)
            .filter(|((c, _n, _a), _)| c == caller && _n == callee);
        let first = matches.next()?;
        if matches.next().is_some() {
            return None; // ambiguous — require exact match
        }
        Some(first.1)
    }
}

impl MonoEnv {
    /// Canonical deterministic identity used in function-instance keys.
    pub(crate) fn canonical(&self) -> String {
        let mut out = String::new();
        for (name, value) in &self.const_params {
            out.push_str(&format!("L:{name}={value};"));
        }
        for (name, value) in &self.type_params {
            out.push_str(&format!("T:{name}={};", canonical_type(value)));
        }
        for ((base, assoc), value) in &self.projections {
            out.push_str(&format!("P:{base}::{assoc}={};", canonical_type(value)));
        }
        out.push_str("H:");
        out.push_str(&self.hash_suffix);
        out
    }
}

pub(crate) fn canonical_type(ty: &IrType) -> String {
    format!("{ty:?}")
}
pub(crate) fn normalized_args(type_args: &[IrType], env: &MonoEnv) -> String {
    type_args
        .iter()
        .map(|arg| canonical_type(&mono_type(arg, env)))
        .collect::<Vec<_>>()
        .join("|")
}
fn instance_key(name: &str, env: &MonoEnv) -> FunctionInstanceKey {
    FunctionInstanceKey {
        source_name: name.to_owned(),
        canonical_env: env.canonical(),
    }
}
fn mangle(name: &str, env: &MonoEnv, generic: bool) -> String {
    if !generic {
        return name.to_owned();
    }
    let suffix: String = env
        .canonical()
        .bytes()
        .map(|byte| match byte {
            b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' => byte as char,
            _ => '_',
        })
        .collect();
    format!("{name}__mono_{suffix}")
}

/// Plan all direct local specializations reachable from `roots`.
/// Generic local calls require explicit type arguments; full type inference
/// remains outside this backend-local monomorphizer.
pub fn plan_flat_module<P: Clone>(
    module: &IrModule<IrFunction<P>, P>,
    roots: &[crate::MonoRoot],
    max_instances: usize,
) -> Result<MonoPlan<P>, MonoError> {
    let mut definitions: BTreeMap<String, &IrFunction<P>> = module
        .functions
        .iter()
        .map(|function| (function.name.clone(), function))
        .collect();
    // Impl methods are full functions too: register them so method calls
    // (`vope.mul_generalized(..)`) plan and specialize like free calls.
    for ir_impl in &module.impls {
        for item in &ir_impl.items {
            if let volar_compiler::ir::IrImplItem::Method(method) = item {
                definitions.entry(method.name.clone()).or_insert(method);
            }
        }
    }
    // Operator impls are additionally keyed by receiver struct
    // (`mul__Vope`, `mul__Galois`, …): several impls share the method name
    // `mul` in the source, and the dispatch key keeps their instances
    // distinct end to end.
    for ir_impl in &module.impls {
        let Some(volar_compiler::ir::IrTraitRef {
            kind: volar_compiler::ir::TraitKind::Math(volar_compiler::ir::MathTrait::Mul),
            ..
        }) = &ir_impl.trait_
        else {
            continue;
        };
        let volar_compiler::ir::IrType::Struct { kind, .. } = &ir_impl.self_ty else {
            continue;
        };
        let dispatched = format!("mul__{}", kind_name(kind));
        for item in &ir_impl.items {
            if let volar_compiler::ir::IrImplItem::Method(method) = item {
                if method.name == "mul" {
                    definitions.insert(dispatched.clone(), method);
                }
            }
        }
    }
    let struct_table: StructTable = module
        .structs
        .iter()
        .map(|s| (s.kind.to_string(), s.clone()))
        .collect();
    // Named module-level constants (e.g. FAEST's LAMBDA_BYTES) participate
    // in array-length resolution for every planned instance.
    let module_consts: BTreeMap<String, usize> = module
        .consts
        .iter()
        .filter_map(|c| const_eval_expr(&c.value).map(|v| (c.name.clone(), v)))
        .collect();
    let selected: Vec<(String, MonoEnv)> = if roots.is_empty() {
        module
            .functions
            .iter()
            .filter(|function| {
                function.external_kind == volar_compiler::ir::ExternalKind::Normal
                    && function.generics.is_empty()
            })
            .map(|function| (function.name.clone(), MonoEnv::new("")))
            .collect()
    } else {
        roots
            .iter()
            .map(|root| (root.function.clone(), root.env.clone()))
            .collect()
    };
    let mul_impl_structs: std::collections::BTreeMap<String, ()> = module
        .impls
        .iter()
        .filter_map(|ir_impl| {
            match (&ir_impl.trait_, &ir_impl.self_ty) {
                (
                    Some(volar_compiler::ir::IrTraitRef {
                        kind: volar_compiler::ir::TraitKind::Math(
                            volar_compiler::ir::MathTrait::Mul,
                        ),
                        ..
                    }),
                    IrType::Struct { kind, .. },
                ) => Some((kind.to_string(), ())),
                _ => None,
            }
        })
        .collect();
    // For each impl with an associated-type declaration (`type Output = ...`),
    // map method name -> the declared type so `Self::Output` in the method's
    // signature resolves per call site (e.g. `Mul::mul` returning
    // `Self::Output = Q<N, O>`).
    //
    // Several impls declare methods with the SAME source name (`mul` on
    // Vope, on Galois, …), so operator impls are keyed by their dispatched
    // name (`mul__<StructKind>`) — a plain method-name key would bleed one
    // impl's associated type into another's instance. Non-operator impls
    // keep the plain method-name key (first impl wins).
    let mut impl_assoc_dispatched: BTreeMap<String, Vec<(String, IrType)>> =
        BTreeMap::new();
    let mut impl_assoc_plain: BTreeMap<String, Vec<(String, IrType)>> = BTreeMap::new();
    for ir_impl in &module.impls {
        let is_mul_op = matches!(
            &ir_impl.trait_,
            Some(volar_compiler::ir::IrTraitRef {
                kind: volar_compiler::ir::TraitKind::Math(volar_compiler::ir::MathTrait::Mul),
                ..
            })
        ) && matches!(&ir_impl.self_ty, IrType::Struct { .. });
        let dispatched = if is_mul_op {
            match &ir_impl.self_ty {
                IrType::Struct { kind, .. } => {
                    Some(format!("mul__{}", kind_name(kind)))
                }
                _ => None,
            }
        } else {
            None
        };
        let assoc: Vec<(String, IrType)> = ir_impl
            .items
            .iter()
            .filter_map(|item| match item {
                volar_compiler::ir::IrImplItem::AssociatedType { name, ty } => {
                    Some((name.to_string(), ty.clone()))
                }
                _ => None,
            })
            .collect();
        if assoc.is_empty() {
            continue;
        }
        for item in &ir_impl.items {
            if let volar_compiler::ir::IrImplItem::Method(method) = item {
                if let Some(key) = &dispatched {
                    if method.name == "mul" {
                        impl_assoc_dispatched
                            .entry(key.clone())
                            .or_default()
                            .extend(assoc.iter().cloned());
                    }
                } else {
                    impl_assoc_plain
                        .entry(method.name.clone())
                        .or_insert_with(|| assoc.clone());
                }
            }
        }
    }
    let mut instances = BTreeMap::new();
    let mut calls = BTreeMap::new();
    let mut materialized: BTreeMap<FunctionInstanceKey, IrFunction<P>> = BTreeMap::new();
    let mut queue = VecDeque::new();
    for (name, env) in selected {
        let Some(definition) = definitions.get(&name) else {
            return Err(MonoError::new(format!(
                "monomorphization root '{name}' is not a local function"
            )));
        };
        if definition.external_kind == volar_compiler::ir::ExternalKind::Normal {
            let mut env = env;
            env.consts.extend(module_consts.clone());
            env.mul_impl_structs = mul_impl_structs.clone();
            queue.push_back((instance_key(&name, &env), env));
        }
    }
    while let Some((key, mut env)) = queue.pop_front() {
        if instances.contains_key(&key) {
            continue;
        }
        if instances.len() >= max_instances {
            return Err(MonoError::new(format!(
                "non-finite local specialization: exceeded {max_instances} instances while planning '{}'",
                key.source_name
            )));
        }
        // A materialized method instance (receiver promoted to a param) uses
        // its derived definition; free functions use the module definition.
        let definition = materialized
            .get(&key)
            .map(|derived| derived as &IrFunction<P>)
            .or_else(|| definitions.get(&key.source_name).copied())
            .expect("queued source definition exists");
        instances.insert(key.clone(), env.clone());
        for (callee, type_args, arg_tys, expected) in
            direct_calls(definition, &env, &definitions, &struct_table)
        {
            let Some(callee_def) = definitions.get(&callee) else {
                continue;
            };
            if callee_def.external_kind != volar_compiler::ir::ExternalKind::Normal {
                continue;
            }
            let mut derived_for_binding: Option<std::rc::Rc<IrFunction<P>>> = None;
            let mut derived_held: Option<std::rc::Rc<IrFunction<P>>> = None;
            // Impl method with a receiver: the parser records the receiver's
            // ref kind but not its type, so `params` excludes `self`. For
            // this call site, derive a definition with `self` promoted to an
            // explicit parameter typed from the receiver's concrete type —
            // that type participates in unification (binding impl-level
            // parameters like `K`) and becomes the instance's first C param.
            let (callee_def, arg_tys): (&IrFunction<P>, Vec<Option<IrType>>) =
                match (&callee_def.receiver, callee_def.params.len()) {
                    (Some(receiver_kind), n) if arg_tys.len() == n + 1 => {
                        let Some(recv_ty) = arg_tys[0].clone() else {
                            continue;
                        };
                        // Value receivers hold the type directly; reference
                        // receivers wrap it (flattening is transparent).
                        let self_ty = match receiver_kind {
                            volar_compiler::ir::IrReceiver::Ref
                            | volar_compiler::ir::IrReceiver::RefMut => {
                                // The inferred receiver type may already be a
                                // reference (e.g. `&self` on a struct param);
                                // wrap only when it is a bare value.
                                match &recv_ty {
                                    r @ IrType::Reference { .. } => r.clone(),
                                    v => IrType::Reference {
                                        mutable: matches!(
                                            receiver_kind,
                                            volar_compiler::ir::IrReceiver::RefMut
                                        ),
                                        elem: Box::new(v.clone()),
                                    },
                                }
                            }
                            volar_compiler::ir::IrReceiver::Value => recv_ty.clone(),
                        };
                        // Distinguish instances by the receiver's lane
                        // count: it is an impl-level parameter with no name
                        // inside the method, so record it under a reserved
                        // key that flows into the instance's canonical env.
                        if let Some(recv_k) = trailing_numeric_slot(&recv_ty) {
                            env.const_params.insert("#recv_k".to_owned(), recv_k);
                        }
                        let mut derived = (*callee_def).clone();
                        derived.receiver = None;
                        derived.params.insert(
                            0,
                            volar_compiler::ir::IrParam {
                                name: "self".to_owned(),
                                ty: self_ty.clone(),
                            },
                        );
                        // The parser erases the impl's own generics from the
                        // method (they live only on the impl). Re-attach the
                        // names appearing in the method's signature as its
                        // own generics so bind_call_args unifies them from
                        // the concrete self/arg types instead of skipping
                        // inference entirely (fn mul has no declared
                        // generics, which would early-return an ambient-only
                        // env and leave `U` unsubstituted).
                        {
                            let mut names: std::collections::BTreeSet<String> =
                                Default::default();
                            for param in &derived.params {
                                collect_type_params(&param.ty, &mut names);
                            }
                            if let Some(ret) = &derived.return_type {
                                collect_type_params(ret, &mut names);
                            }
                            let declared: Vec<String> =
                                derived.generics.iter().map(|g| g.name.clone()).collect();
                            for name in &declared {
                                names.remove(name);
                            }
                            for name in names {
                                // Numeric/typenum spellings are const bindings;
                                // everything else is a genuine type param.
                                let kind = if name.parse::<usize>().is_ok()
                                    || typenum_usize(&name).is_some()
                                {
                                    volar_compiler::ir::IrGenericParamKind::Const
                                } else {
                                    volar_compiler::ir::IrGenericParamKind::Type
                                };
                                derived.generics.push(volar_compiler::ir::IrGenericParam {
                                    name,
                                    kind,
                                    const_ty: None,
                                    bounds: Vec::new(),
                                    default: None,
                                });
                            }
                        }
                        // Rewrite `Generic::Output` projections in the derived
                        // signature. The parser erases the impl's own generic
                        // (`K`) from the method, so an associated-type result
                        // like `Vope<N, T, K2::Output>` (the sum of receiver
                        // and argument lane counts) cannot be resolved by
                        // unification. Recover it arithmetically: the callee
                        // generic appearing in the other param's last struct
                        // type-arg slot binds to that argument's numeric
                        // lane count; the receiver supplies its own.
                        let other_arg_ty = arg_tys.get(1).cloned().flatten();
                        if let (Some(other_ty), Some(ret_ty)) =
                            (other_arg_ty, derived.return_type.as_ref())
                        {
                            let recv_k = trailing_numeric_slot(&self_ty);
                            let other_k = trailing_numeric_slot(&other_ty);
                            if std::env::var("VOLAR_KEY_DEBUG").is_ok() {
                                eprintln!(
                                    "[key-debug] derive probe: recv_k={recv_k:?} other_k={other_k:?} other_arg={other_ty:?}"
                                );
                            }
                            if let (Some(recv_k), Some(other_k)) = (recv_k, other_k) {
                                // Rewrite every `G::Output` projection whose
                                // generic `G` occupies the other param's
                                // trailing slot: with the impl-level lane
                                // count erased from the method, the
                                // associated type resolves arithmetically to
                                // `receiver_k + other_k`.
                                let other_param_generic = callee_def
                                    .params
                                    .first()
                                    .and_then(|p| trailing_generic_name(&p.ty));
                                if let Some(g) = other_param_generic {
                                    let sum = recv_k + other_k;
                                    if let Some(ret) = derived.return_type.as_mut() {
                                        substitute_projection_output(ret, &g, sum);
                                    }
                                    for param in derived.params.iter_mut() {
                                        substitute_projection_output(
                                            &mut param.ty,
                                            &g,
                                            sum,
                                        );
                                    }
                                    substitute_projection_output_in_block(
                                        &mut derived.body,
                                        &g,
                                        sum,
                                    );
                                }
                            }
                        }
                        // Resolve `Self::Output` in the derived signature:
                        // the parser keeps the projection, but the impl's
                        // `type Output = ...` declaration gives the concrete
                        // shape once its own generics are bound by
                        // unification at bind_call_args time. Substitute the
                        // declaration now; unresolved impl generics inside it
                        // (e.g. `O`) are bound later by unification against
                        // the call's expected type or args.
                        let assocs = impl_assoc_dispatched
                            .get(&callee)
                            .or_else(|| impl_assoc_plain.get(callee_def.name.as_str()));
                        if let Some(assocs) = assocs {
                            for (assoc_name, assoc_ty) in assocs {
                                let projection_ty = IrType::Projection {
                                    base: Box::new(IrType::TypeParam("Self".to_owned())),
                                    trait_path: None,
                                    trait_args: Vec::new(),
                                    assoc: volar_compiler::ir::AssociatedType::from_str(
                                        assoc_name,
                                    ),
                                };
                                if let Some(ret) = derived.return_type.as_mut() {
                                    substitute_type_for_projection(
                                        ret,
                                        &projection_ty,
                                        assoc_ty,
                                    );
                                }
                                for param in derived.params.iter_mut() {
                                    substitute_type_for_projection(
                                        &mut param.ty,
                                        &projection_ty,
                                        assoc_ty,
                                    );
                                }
                                substitute_type_for_projection_in_block(
                                    &mut derived.body,
                                    &projection_ty,
                                    assoc_ty,
                                );
                            }
                        }
                        let derived = std::rc::Rc::new(derived);
                        derived_held = Some(derived);
                        // Keep the full arg list: the derived `self` param
                        // pairs with the receiver argument, restoring the
                        // 1:1 param/arg alignment lost by the parser.
                        (derived_held.as_deref().unwrap(), arg_tys)
                    }
                    _ => (callee_def, arg_tys),
                };
            let mut callee_env = bind_call_args(
                callee_def,
                &type_args,
                &arg_tys,
                &env,
                expected.as_ref(),
            )?;
            if let Some(derived) = derived_held.as_ref() {
                // Default unbound return-position generics: a field-mul
                // output (`O` in `T: Mul<U, Output = O>`) has the same layout
                // as the operand element type. Bind any still-unbound generic
                // that appears ONLY in the return type to the receiver's
                // element generic concrete (`T`), else to the first bound
                // type param.
                if let Some(ret) = &derived.return_type {
                    let mut ret_names: std::collections::BTreeSet<String> =
                        Default::default();
                    collect_type_params(ret, &mut ret_names);
                    let mut param_names: std::collections::BTreeSet<String> =
                        Default::default();
                    for param in &derived.params {
                        collect_type_params(&param.ty, &mut param_names);
                    }
                    let t_concrete = param_names
                        .iter()
                        .find_map(|n| callee_env.type_params.get(n).cloned());
                    for name in ret_names {
                        if param_names.contains(&name) {
                            continue;
                        }
                        if !callee_env.type_params.contains_key(&name) {
                            if let Some(concrete) = &t_concrete {
                                callee_env.type_params.insert(name, concrete.clone());
                            }
                        }
                    }
                }
                materialized.insert(instance_key(&callee, &callee_env), (**derived).clone());
            }
            derived_for_binding = None;
            let args = normalized_args(
                &if type_args.is_empty() {
                    // Prefer inferred concrete args for the call key when turbofish
                    // was omitted.
                    callee_def
                        .generics
                        .iter()
                        .filter_map(|parameter| match parameter.kind {
                            volar_compiler::ir::IrGenericParamKind::Const => callee_env
                                .const_params
                                .get(&parameter.name)
                                .map(|&n| IrType::TypeParam(n.to_string())),
                            volar_compiler::ir::IrGenericParamKind::Type => {
                                callee_env.type_params.get(&parameter.name).cloned()
                            }
                            volar_compiler::ir::IrGenericParamKind::Lifetime => None,
                        })
                        .collect::<Vec<_>>()
                } else {
                    type_args.clone()
                },
                &env,
            );
            let callee_key = instance_key(&callee, &callee_env);
            // The plain generic-args key can collide when two sites differ
            // only in impl-level receiver parameters (e.g. `self: Vope<..,K>`
            // with K=1 vs K=2). Append the canonical argument types — the
            // same string the lowering side derives from `infer_type` — so
            // each static call site maps to its own instance.
            let arg_types_key: String = arg_tys
                .iter()
                .map(|ty| {
                    ty.as_ref()
                        .map(|t| canonical_type(&mono_type(t, &env)))
                        .unwrap_or_default()
                })
                .collect::<Vec<_>>()
                .join("|");
            // Key on the canonical arg-types string alone. The generic-args
            // prefix mismatches the lowering side whenever the turbofish is
            // omitted (lowering computes an empty generic key), and identical
            // arg types already imply identical instantiation — the receiver's
            // impl-level generics show up in the receiver arg's type.
            let args = arg_types_key;
            calls.insert((key.clone(), callee.clone(), args), callee_key.clone());
            if !instances.contains_key(&callee_key) {
                queue.push_back((callee_key, callee_env));
            }
        }
    }
    // First specialization of each source name keeps the bare name for C ABI /
    // harness friendliness (mirrors nominal struct emission).
    let mut claimed_bare: BTreeMap<String, ()> = BTreeMap::new();
    let emitted_names = instances
        .iter()
        .map(|(key, env)| {
            let generic = definitions
                .get(&key.source_name)
                .expect("planned source exists")
                .generics
                .len()
                > 0;
            let emitted = if !generic {
                key.source_name.clone()
            } else if claimed_bare.insert(key.source_name.clone(), ()).is_none() {
                key.source_name.clone()
            } else {
                mangle(&key.source_name, env, true)
            };
            (key.clone(), emitted)
        })
        .collect();
    let materialized = materialized
        .into_iter()
        .map(|(k, v)| (k, v.clone()))
        .collect();
    Ok(MonoPlan {
        instances,
        emitted_names,
        materialized,
        calls,
        mul_impl_structs,
    })
}

fn bind_call_args<P: Clone>(
    function: &IrFunction<P>,
    type_args: &[IrType],
    arg_tys: &[Option<IrType>],
    caller_env: &MonoEnv,
    expected: Option<&IrType>,
) -> Result<MonoEnv, MonoError> {
    if function.generics.is_empty() {
        if !type_args.is_empty() {
            return Err(MonoError::new(format!(
                "non-generic local function '{}' was called with type arguments",
                function.name
            )));
        }
        // Keep the caller's layout bindings. An empty env would re-plan the same
        // non-generic body without `N`/`U1` and panic during type conversion.
        return Ok(caller_env.clone());
    }
    // Seed with ambient caller bindings that this function does not shadow
    // (typenum defaults, VOLE `U1`/`U0`, hash suffix, etc.).
    let mut env = ambient_callee_env(function, caller_env);
    // Impl-level parameters (bound by the receiver's `impl`, e.g. `K` in
    // `impl Vope<N, T, K> { fn m<K2>(...) }`) are per-call-site: strip any
    // ambient binding so unification below binds them from the actual
    // argument types instead of silently inheriting the caller's value.
    {
        let mut impl_level: std::collections::BTreeSet<String> = Default::default();
        for param in &function.params {
            collect_type_params(&param.ty, &mut impl_level);
        }
        if let Some(ret) = &function.return_type {
            collect_type_params(ret, &mut impl_level);
        }
        for name in function.generics.iter().map(|g| &g.name) {
            impl_level.remove(name);
        }
        for name in &impl_level {
            env.const_params.remove(name);
            env.type_params.remove(name);
        }
    }
    // Explicit turbofish bindings first.
    if !type_args.is_empty() {
        if type_args.len() != function.generics.len() {
            return Err(MonoError::new(format!(
                "generic local call '{}' requires {} explicit type arguments; found {}",
                function.name,
                function.generics.len(),
                type_args.len()
            )));
        }
        for (parameter, argument) in function.generics.iter().zip(type_args) {
            bind_one_generic(&mut env, function, parameter, argument, caller_env)?;
        }
        return Ok(env);
    }
    // Restricted inference: unify callee parameter types with call-site arg types.
    for (param, arg_ty) in function.params.iter().zip(arg_tys.iter()) {
        let Some(arg_ty) = arg_ty else {
            continue;
        };
        let arg_ty = mono_type(arg_ty, caller_env);
        unify_into(&mut env, &param.ty, &arg_ty, function, caller_env)?;
    }
    // Bidirectional hint: unify the callee's return type against the
    // expected type from the call site (a `let` annotation) to bind
    // generics that appear only in the return position (e.g.
    // `lift_bit<N, T>(bit: T) -> Array<T, N>` assigned to `Array<T, N>`).
    if let Some(expected) = expected {
        if let Some(ret) = &function.return_type {
            unify_into(&mut env, ret, expected, function, caller_env)?;
        }
    }
    // Every layout-relevant generic must be concrete after inference.
    for parameter in &function.generics {
        match parameter.kind {
            volar_compiler::ir::IrGenericParamKind::Const => {
                if !env.const_params.contains_key(&parameter.name) {
                    // Fallback: same-name ambient const binding from the
                    // caller. Spec code threads const params (e.g. BIG_N)
                    // through helper chains under one name; zero-arg helpers
                    // like `and_test_poly::<BIG_N>()` carry no inference
                    // evidence of their own, so the caller's binding is the
                    // only available (and intended) source. This is
                    // context-driven resolution — an unbound name anywhere
                    // in the chain still errors, keeping the root discipline.
                    if let Some(value) = caller_env.const_params.get(&parameter.name) {
                        env.const_params.insert(parameter.name.clone(), *value);
                    } else {
                        return Err(MonoError::new(format!(
                            "generic local call '{}': const parameter '{}' could not be inferred",
                            function.name, parameter.name
                        )));
                    }
                }
            }
            volar_compiler::ir::IrGenericParamKind::Type => {
                match env.type_params.get(&parameter.name) {
                    Some(ty) if is_concrete_type(ty) => {}
                    // Cross-module nominal shadowing its own generic slot
                    // (`BigVoleProver`): the registry resolves the layout.
                    Some(IrType::TypeParam(self_name))
                        if self_name == &parameter.name => {}
                    Some(ty) => {
                        return Err(MonoError::new(format!(
                            "generic local call '{}': type parameter '{}' inferred non-concrete ({ty:?})",
                            function.name, parameter.name
                        )));
                    }
                    None => {
                        // Cross-module struct/enum names parse as bare
                        // TypeParams. When a generic slot's name is a
                        // nominal type name (multi-char, uppercase-initial
                        // — e.g. `BigVoleProver`), it self-describes its
                        // layout: leave it unbound here; mono_type keeps the
                        // TypeParam spelling and ensure_type_nominals
                        // resolves the registry entry.
                        let name = parameter.name.as_str();
                        let looks_nominal = name.len() > 2
                            && name.chars().next().map(char::is_uppercase).unwrap_or(false);
                        if !looks_nominal {
                            return Err(MonoError::new(format!(
                                "generic local call '{}': type parameter '{}' could not be inferred",
                                function.name, parameter.name
                            )));
                        }
                    }
                }
            }
            volar_compiler::ir::IrGenericParamKind::Lifetime => {}
        }
    }
    Ok(env)
}

/// Caller bindings whose names are not parameters of `function`.
fn ambient_callee_env<P: Clone>(function: &IrFunction<P>, caller_env: &MonoEnv) -> MonoEnv {
    let shadows = |name: &str| function.generics.iter().any(|g| g.name == name);
    let mut env = MonoEnv::new(caller_env.hash_suffix.clone());
    env.consts = caller_env.consts.clone();
    for (name, value) in &caller_env.const_params {
        if !shadows(name) {
            env.const_params.insert(name.clone(), *value);
        }
    }
    for (name, value) in &caller_env.type_params {
        if !shadows(name) {
            env.type_params.insert(name.clone(), value.clone());
        }
    }
    for (key, value) in &caller_env.projections {
        if !shadows(&key.0) {
            env.projections.insert(key.clone(), value.clone());
        }
    }
    env
}

fn bind_one_generic<P: Clone>(
    env: &mut MonoEnv,
    function: &IrFunction<P>,
    parameter: &volar_compiler::ir::IrGenericParam,
    argument: &IrType,
    caller_env: &MonoEnv,
) -> Result<(), MonoError> {
    let argument = mono_type(argument, caller_env);
    match parameter.kind {
        volar_compiler::ir::IrGenericParamKind::Type => {
            if !is_concrete_type(&argument) {
                return Err(MonoError::new(format!(
                    "generic local call '{}': type parameter '{}' is unresolved ({argument:?})",
                    function.name, parameter.name
                )));
            }
            // ArraySize-style type params often arrive as `TypeParam("16")`.
            if let IrType::TypeParam(name) = &argument {
                if let Ok(n) = name.parse::<usize>() {
                    env.const_params.insert(parameter.name.clone(), n);
                } else if let Some(n) = typenum_usize(name) {
                    env.const_params.insert(parameter.name.clone(), n);
                }
            }
            env.type_params.insert(parameter.name.clone(), argument);
        }
        volar_compiler::ir::IrGenericParamKind::Const => {
            match mono_len(&type_args_to_len(Some(&argument), caller_env), caller_env) {
                ArrayLength::Const(value) => {
                    env.const_params.insert(parameter.name.clone(), value);
                }
                ArrayLength::TypeNum(value) => {
                    env.const_params
                        .insert(parameter.name.clone(), value.to_usize());
                }
                other => {
                    return Err(MonoError::new(format!(
                        "generic local call '{}': const parameter '{}' is unresolved ({other:?})",
                        function.name, parameter.name
                    )));
                }
            }
        }
        volar_compiler::ir::IrGenericParamKind::Lifetime => {}
    }
    Ok(())
}

/// Extract a concrete `usize` from a monomorphized type parameter shape:
/// `TypeParam("2")` / numeric-literal names, or typenum marker names
/// (`U4`, …). `mono_type` rewrites bound const generics to numeric strings,
/// so this is the only form that reaches unification for call-site args.
fn concrete_usize(ty: &IrType) -> Option<usize> {
    if let IrType::TypeParam(name) = ty {
        if let Ok(n) = name.parse::<usize>() {
            return Some(n);
        }
        return typenum_usize(name);
    }
    None
}

fn is_concrete_type(ty: &IrType) -> bool {
    match ty {
        IrType::TypeParam(name) => name.parse::<usize>().is_ok() || typenum_usize(name).is_some(),
        IrType::Primitive(_) | IrType::Unit | IrType::Never => true,
        IrType::Array { elem, len, .. } => {
            is_concrete_type(elem) && matches!(len, ArrayLength::Const(_) | ArrayLength::TypeNum(_))
        }
        IrType::Struct { type_args, .. } => type_args.iter().all(is_concrete_type),
        IrType::Reference { elem, .. } | IrType::Vector { elem } => is_concrete_type(elem),
        IrType::Tuple(elems) => elems.iter().all(is_concrete_type),
        _ => false,
    }
}

fn unify_into<P: Clone>(
    env: &mut MonoEnv,
    pattern: &IrType,
    concrete: &IrType,
    function: &IrFunction<P>,
    caller_env: &MonoEnv,
) -> Result<(), MonoError> {
    let pattern = mono_type(pattern, env);
    let concrete = mono_type(concrete, caller_env);
    match (&pattern, &concrete) {
        (IrType::TypeParam(name), concrete) => {
            // Cross-module struct/enum names parse as bare TypeParams; when a
            // param type and its call-site argument are the SAME unresolved
            // name (e.g. `big_vole: &BigVoleProver` passed straight through),
            // there is nothing to learn — the registry resolves the nominal
            // later. Skip instead of erroring.
            if let IrType::TypeParam(cname) = concrete {
                if cname == name {
                    return Ok(());
                }
            }
            if !is_concrete_type(concrete) {
                return Err(MonoError::new(format!(
                    "generic local call '{}': cannot bind '{}' to non-concrete {concrete:?}",
                    function.name, name
                )));
            }
            if let Some(existing) = env.type_params.get(name) {
                if existing != concrete {
                    return Err(MonoError::new(format!(
                        "generic local call '{}': contradictory binding for '{}'",
                        function.name, name
                    )));
                }
            } else if function
                .generics
                .iter()
                .any(|g| g.name == *name && g.kind == volar_compiler::ir::IrGenericParamKind::Type)
            {
                if let IrType::TypeParam(nname) = concrete {
                    if let Ok(n) = nname.parse::<usize>() {
                        env.const_params.insert(name.clone(), n);
                    } else if let Some(n) = typenum_usize(nname) {
                        env.const_params.insert(name.clone(), n);
                    }
                }
                env.type_params.insert(name.clone(), concrete.clone());
            } else if function
                .generics
                .iter()
                .any(|g| g.name == *name && g.kind == volar_compiler::ir::IrGenericParamKind::Const)
            {
                // Const-generic parameter of the callee appearing as a plain
                // type parameter (e.g. `LweCiphertext<N_LWE>` field/param
                // types): bind it from the concrete numeric argument so
                // call sites without turbofish still specialize. TFHE's
                // `lwe_add`/`rlwe_add` are the motivating shapes.
                if let Some(n) = concrete_usize(concrete) {
                    if let Some(existing) = env.const_params.get(name) {
                        if *existing != n {
                            return Err(MonoError::new(format!(
                                "generic local call '{}': contradictory const binding for '{}'",
                                function.name, name
                            )));
                        }
                    } else {
                        env.const_params.insert(name.clone(), n);
                    }
                }
            } else if !function.generics.iter().any(|g| g.name == *name) {
                // Impl-level parameter: a name bound by the receiver's
                // `impl` block, not the method's own generics (e.g. `K` in
                // `impl Vope<N, T, K> { fn mul_generalized<K2>(...) }`).
                // Bind numeric concretes as consts; bind other concrete
                // types as type params. Projections (`K2::Output`) are
                // skipped: binding them creates self-referential
                // substitutions that never resolve.
                if let IrType::TypeParam(nname) = concrete {
                    if let Ok(n) = nname.parse::<usize>() {
                        env.const_params.insert(name.clone(), n);
                    } else if let Some(n) = typenum_usize(nname) {
                        env.const_params.insert(name.clone(), n);
                    }
                } else if !matches!(concrete, IrType::Projection { .. }) {
                    env.type_params.insert(name.clone(), concrete.clone());
                }
            }
            Ok(())
        }
        (
            IrType::Array {
                elem: p_elem,
                len: p_len,
                ..
            },
            IrType::Array {
                elem: c_elem,
                len: c_len,
                ..
            },
        ) => {
            unify_into(env, p_elem, c_elem, function, caller_env)?;
            if let (ArrayLength::TypeParam(name), ArrayLength::Const(n)) = (p_len, c_len) {
                if function.generics.iter().any(|g| {
                    g.name == *name && g.kind == volar_compiler::ir::IrGenericParamKind::Const
                }) {
                    if let Some(existing) = env.const_params.get(name) {
                        if existing != n {
                            return Err(MonoError::new(format!(
                                "generic local call '{}': contradictory const binding for '{}'",
                                function.name, name
                            )));
                        }
                    } else {
                        env.const_params.insert(name.clone(), *n);
                    }
                } else if function.generics.iter().any(|g| {
                    g.name == *name && g.kind == volar_compiler::ir::IrGenericParamKind::Type
                }) {
                    // `N: ArraySize` (hybrid-array) params appear as type
                    // params used in array-length position; bind them as
                    // concrete numeric type params so the layout check
                    // below sees them as resolved.
                    let concrete = IrType::TypeParam(n.to_string());
                    if let Some(existing) = env.type_params.get(name) {
                        if existing != &concrete {
                            return Err(MonoError::new(format!(
                                "generic local call '{}': contradictory binding for '{}'",
                                function.name, name
                            )));
                        }
                    } else {
                        env.type_params.insert(name.clone(), concrete);
                    }
                }
            }
            Ok(())
        }
        (
            IrType::Struct {
                kind: p_kind,
                type_args: p_args,
            },
            IrType::Struct {
                kind: c_kind,
                type_args: c_args,
            },
        ) if p_kind == c_kind && p_args.len() == c_args.len() => {

            for (p, c) in p_args.iter().zip(c_args.iter()) {
                unify_into(env, p, c, function, caller_env)?;
            }
            Ok(())
        }
        (IrType::Reference { elem: p_elem, .. }, IrType::Reference { elem: c_elem, .. }) => {
            unify_into(env, p_elem, c_elem, function, caller_env)
        }
        (IrType::Reference { elem: p_elem, .. }, concrete) => {
            // Value-semantics flattening: a `&T` argument carries T's layout,
            // so a reference on either side unifies against the pointee.
            unify_into(env, p_elem, concrete, function, caller_env)
        }
        (pattern, IrType::Reference { elem: c_elem, .. }) => {
            unify_into(env, pattern, c_elem, function, caller_env)
        }
        (IrType::Tuple(p_elems), IrType::Tuple(c_elems)) if p_elems.len() == c_elems.len() => {
            for (p, c) in p_elems.iter().zip(c_elems.iter()) {
                unify_into(env, p, c, function, caller_env)?;
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

/// Struct definitions by rendered kind name, for field-type inference.
pub(crate) type StructTable = BTreeMap<String, IrStruct>;

/// Substitute struct-generic names with concrete type arguments.
fn substitute_type_args(ty: &IrType, generics: &[String], args: &[IrType]) -> IrType {
    match ty {
        IrType::TypeParam(name) => {
            if let Some(pos) = generics.iter().position(|g| g == name) {
                args.get(pos).cloned().unwrap_or_else(|| ty.clone())
            } else {
                ty.clone()
            }
        }
        IrType::Array { kind, elem, len } => IrType::Array {
            kind: *kind,
            elem: Box::new(substitute_type_args(elem, generics, args)),
            len: len.clone(),
        },
        IrType::Struct { kind, type_args } => IrType::Struct {
            kind: kind.clone(),
            type_args: type_args
                .iter()
                .map(|a| substitute_type_args(a, generics, args))
                .collect(),
        },
        IrType::Reference { mutable, elem } => IrType::Reference {
            mutable: *mutable,
            elem: Box::new(substitute_type_args(elem, generics, args)),
        },
        IrType::Tuple(elems) => IrType::Tuple(
            elems
                .iter()
                .map(|e| substitute_type_args(e, generics, args))
                .collect(),
        ),
        other => other.clone(),
    }
}

fn infer_expr_type<P: Clone>(
    expr: &IrExpr<P>,
    env: &MonoEnv,
    vars: Option<&VarTypes>,
    structs: Option<&StructTable>,
) -> Option<IrType> {
    match &expr.kind {
        IrExprKind::Var(name) => vars.and_then(|m| m.get(name).cloned()),
        IrExprKind::Field { base, field, .. } => {
            let base_ty = infer_expr_type(base, env, vars, structs)?;
            // Strip one reference level (`&bk.ksk` → base is the ref).
            let base_ty = match base_ty {
                IrType::Reference { elem, .. } => *elem,
                other => other,
            };
            let IrType::Struct { kind, type_args } = base_ty else {
                return None;
            };
            let def = structs?.get(&kind.to_string())?;
            let generic_names: Vec<String> = def.generics.iter().map(|g| g.name.clone()).collect();
            let field = def.fields.iter().find(|f| &f.name == field)?;
            Some(substitute_type_args(&field.ty, &generic_names, &type_args))
        }
        IrExprKind::Lit(IrLit::Int(_)) => {
            Some(IrType::Primitive(volar_compiler::ir::PrimitiveType::U64))
        }
        IrExprKind::Lit(IrLit::Bool(_)) => {
            Some(IrType::Primitive(volar_compiler::ir::PrimitiveType::Bool))
        }
        IrExprKind::FixedArray(elems) => {
            let elem_ty = elems
                .first()
                .and_then(|e| infer_expr_type(e, env, vars, structs))?;
            Some(IrType::Array {
                kind: ArrayKind::FixedArray,
                elem: Box::new(elem_ty),
                len: ArrayLength::Const(elems.len()),
            })
        }
        IrExprKind::Array(elems) => {
            let elem_ty = elems
                .first()
                .and_then(|e| infer_expr_type(e, env, vars, structs))?;
            Some(IrType::Array {
                kind: ArrayKind::FixedArray,
                elem: Box::new(elem_ty),
                len: ArrayLength::Const(elems.len()),
            })
        }
        IrExprKind::StructExpr {
            kind, type_args, ..
        } => Some(IrType::Struct {
            kind: kind.clone(),
            type_args: type_args.iter().map(|a| mono_type(a, env)).collect(),
        }),
        IrExprKind::Cast { ty, .. } => Some(mono_type(ty, env)),
        // `.clone()` / `.deref()` preserve the receiver's type — resolving
        // through it keeps call-arg inference alive for arguments like
        // `delta.clone()` passed to operator-impl methods.
        IrExprKind::MethodCall { receiver, .. } => infer_expr_type(receiver, env, vars, structs),
        IrExprKind::Unary {
            op: SpecUnaryOp::Ref | SpecUnaryOp::RefMut,
            expr,
        } => infer_expr_type(expr, env, vars, structs).map(|elem| IrType::Reference {
            mutable: false,
            elem: Box::new(elem),
        }),
        IrExprKind::Unary {
            op: SpecUnaryOp::Deref,
            expr,
        } => match infer_expr_type(expr, env, vars, structs)? {
            IrType::Reference { elem, .. } => Some(*elem),
            other => Some(other),
        },
        _ => None,
    }
}

/// Local variable types for call-site argument inference: name → declared/
/// inferred type. Seeded from function parameters, extended by `let`
/// bindings. Scoped nesting (blocks, loops) pushes/pops entries; shadowing
/// by a later binding simply overwrites.
pub(crate) type VarTypes = std::collections::BTreeMap<String, IrType>;

/// Collect every generic-parameter name appearing in a type (used to detect
/// impl-level parameters during call binding).
fn collect_type_params(ty: &IrType, out: &mut std::collections::BTreeSet<String>) {
    match ty {
        IrType::TypeParam(name) => {
            out.insert(name.clone());
        }
        IrType::Array { elem, len, .. } => {
            collect_type_params(elem, out);
            if let ArrayLength::TypeParam(name) = len {
                out.insert(name.clone());
            }
        }
        IrType::Struct { type_args, .. } => {
            for a in type_args {
                collect_type_params(a, out);
            }
        }
        IrType::Reference { elem, .. } | IrType::Vector { elem } => collect_type_params(elem, out),
        IrType::Tuple(elems) => {
            for e in elems {
                collect_type_params(e, out);
            }
        }
        _ => {}
    }
}

/// The trailing generic name in a struct-typed parameter (`Vope<N, T, K2>`
/// → `"K2"`), used to identify which callee generic occupies the argument's
/// lane-count slot.
fn trailing_generic_name(ty: &IrType) -> Option<String> {
    match ty {
        IrType::Reference { elem, .. } => trailing_generic_name(elem),
        IrType::Struct { type_args, .. } => match type_args.last()? {
            IrType::TypeParam(name) => Some(name.clone()),
            _ => None,
        },
        _ => None,
    }
}

/// The numeric value of a struct type's trailing type argument
/// (`Vope<.., U2>` / `Vope<.., TypeParam("2")>` → `2`).
fn trailing_numeric_slot(ty: &IrType) -> Option<usize> {
    match ty {
        IrType::Reference { elem, .. } => trailing_numeric_slot(elem),
        IrType::Struct { type_args, .. } => {
            let last = type_args.last()?;
            match last {
                IrType::TypeParam(name) => {
                    name.parse::<usize>().ok().or_else(|| typenum_usize(name))
                }
                _ => None,
            }
        }
        _ => None,
    }
}

/// Replace `Projection{ base: TypeParam(g), assoc: Output }` lengths in a
/// type with the concrete sum value (as `TypeParam(n)`).

/// Replace every occurrence of `projection` (e.g. `Self::Output`) in a type
/// position with `replacement` (the impl's associated-type declaration).
fn substitute_type_for_projection(ty: &mut IrType, projection: &IrType, replacement: &IrType) {
    if ty == projection {
        *ty = replacement.clone();
        return;
    }
    match ty {
        IrType::Struct { type_args, .. } => {
            for a in type_args.iter_mut() {
                substitute_type_for_projection(a, projection, replacement);
            }
        }
        IrType::Reference { elem, .. } | IrType::Vector { elem } => {
            substitute_type_for_projection(elem, projection, replacement)
        }
        IrType::Array { elem, len, .. } => {
            substitute_type_for_projection(elem, projection, replacement);
            // A projection can also appear in an array-length position
            // (`Array<_, Self::Output>`); leave those for the later
            // projection-rewrite pass, which handles length positions.
        }
        _ => {}
    }
}

fn substitute_type_for_projection_in_block<P: Clone>(
    block: &mut volar_compiler::ir::IrBlock<P>,
    projection: &IrType,
    replacement: &IrType,
) {
    for stmt in &mut block.stmts {
        substitute_type_for_projection_in_stmt(stmt, projection, replacement);
    }
    if let Some(expr) = &mut block.expr {
        substitute_type_for_projection_in_expr(expr, projection, replacement);
    }
}

fn substitute_type_for_projection_in_stmt<P: Clone>(
    stmt: &mut volar_compiler::ir::IrStmt<P>,
    projection: &IrType,
    replacement: &IrType,
) {
    match &mut stmt.kind {
        IrStmtKind::Let { ty, init, .. } => {
            if let Some(ty) = ty {
                substitute_type_for_projection(ty, projection, replacement);
            }
            if let Some(init) = init {
                substitute_type_for_projection_in_expr(init, projection, replacement);
            }
        }
        IrStmtKind::Expr(expr) | IrStmtKind::Semi(expr) => {
            substitute_type_for_projection_in_expr(expr, projection, replacement)
        }
        _ => {}
    }
}

fn substitute_type_for_projection_in_expr<P: Clone>(
    expr: &mut IrExpr<P>,
    projection: &IrType,
    replacement: &IrType,
) {
    match &mut expr.kind {
        IrExprKind::Call { func, args } => {
            substitute_type_for_projection_in_expr(func, projection, replacement);
            for a in args {
                substitute_type_for_projection_in_expr(a, projection, replacement);
            }
        }
        IrExprKind::MethodCall {
            receiver,
            type_args,
            args,
            ..
        } => {
            substitute_type_for_projection_in_expr(receiver, projection, replacement);
            for a in type_args.iter_mut() {
                substitute_type_for_projection(a, projection, replacement);
            }
            for a in args {
                substitute_type_for_projection_in_expr(a, projection, replacement);
            }
        }
        IrExprKind::StructExpr { type_args, fields, .. } => {
            for a in type_args.iter_mut() {
                substitute_type_for_projection(a, projection, replacement);
            }
            for (_, v) in fields {
                substitute_type_for_projection_in_expr(v, projection, replacement);
            }
        }
        IrExprKind::Block(b) => {
            substitute_type_for_projection_in_block(b, projection, replacement)
        }
        IrExprKind::If {
            cond,
            then_branch,
            else_branch,
        } => {
            substitute_type_for_projection_in_expr(cond, projection, replacement);
            substitute_type_for_projection_in_block(then_branch, projection, replacement);
            if let Some(e) = else_branch {
                substitute_type_for_projection_in_expr(e, projection, replacement);
            }
        }
        IrExprKind::Unary { expr, .. } | IrExprKind::Field { base: expr, .. } => {
            substitute_type_for_projection_in_expr(expr, projection, replacement)
        }
        IrExprKind::Binary { left, right, .. }
        | IrExprKind::Assign { left, right }
        | IrExprKind::AssignOp { left, right, .. } => {
            substitute_type_for_projection_in_expr(left, projection, replacement);
            substitute_type_for_projection_in_expr(right, projection, replacement);
        }
        IrExprKind::Index { base, index } => {
            substitute_type_for_projection_in_expr(base, projection, replacement);
            substitute_type_for_projection_in_expr(index, projection, replacement);
        }
        IrExprKind::Tuple(values) | IrExprKind::Array(values) | IrExprKind::FixedArray(values) => {
            for v in values {
                substitute_type_for_projection_in_expr(v, projection, replacement);
            }
        }
        IrExprKind::DefaultValue { ty } => {
            if let Some(ty) = ty {
                substitute_type_for_projection(ty, projection, replacement);
            }
        }
        IrExprKind::Closure { ret_type, body, .. } => {
            if let Some(rt) = ret_type {
                substitute_type_for_projection(rt, projection, replacement);
            }
            substitute_type_for_projection_in_expr(body, projection, replacement);
        }
        _ => {}
    }
}

fn substitute_projection_output(ty: &mut IrType, generic: &str, value: usize) {
    match ty {
        IrType::Projection { base, assoc, .. } => {
            if matches!(base.as_ref(), IrType::TypeParam(n) if n == generic)
                && assoc.to_string() == "Output"
            {
                *ty = IrType::TypeParam(value.to_string());
            }
        }
        IrType::Struct { type_args, .. } => {
            for a in type_args.iter_mut() {
                substitute_projection_output(a, generic, value);
            }
        }
        IrType::Reference { elem, .. } | IrType::Vector { elem } => {
            substitute_projection_output(elem, generic, value)
        }
        IrType::Array { elem, len, .. } => {
            substitute_projection_output(elem, generic, value);
            // Array-length positions carry their own projection shape
            // (`Array::<_, K2::Output>` in method bodies).
            if let volar_compiler::ir::ArrayLength::Projection { r#type, field, .. } = len {
                if matches!(r#type.as_ref(), IrType::TypeParam(n) if n == generic)
                    && field.to_string() == "Output"
                {
                    *len = volar_compiler::ir::ArrayLength::Const(value);
                }
            }
        }
        _ => {}
    }
}

fn substitute_projection_output_in_block<P: Clone>(
    block: &mut volar_compiler::ir::IrBlock<P>,
    generic: &str,
    value: usize,
) {
    for stmt in block.stmts.iter_mut() {
        substitute_projection_output_in_stmt(stmt, generic, value);
    }
    if let Some(expr) = block.expr.as_mut() {
        substitute_projection_output_in_expr(expr, generic, value);
    }
}

fn substitute_projection_output_in_stmt<P: Clone>(
    stmt: &mut volar_compiler::ir::IrStmt<P>,
    generic: &str,
    value: usize,
) {
    use volar_compiler::ir::IrStmtKind;
    match &mut stmt.kind {
        IrStmtKind::Let { ty, init, .. } => {
            if let Some(ty) = ty {
                substitute_projection_output(ty, generic, value);
            }
            if let Some(init) = init {
                substitute_projection_output_in_expr(init, generic, value);
            }
        }
        IrStmtKind::Semi(e) | IrStmtKind::Expr(e) => {
            substitute_projection_output_in_expr(e, generic, value)
        }
        _ => {}
    }
}

fn substitute_projection_output_in_expr<P: Clone>(
    expr: &mut volar_compiler::ir::IrExpr<P>,
    generic: &str,
    value: usize,
) {
    use volar_compiler::ir::IrExprKind;
    match &mut expr.kind {
        IrExprKind::Call { func, args } => {
            substitute_projection_output_in_expr(func, generic, value);
            for a in args.iter_mut() {
                substitute_projection_output_in_expr(a, generic, value);
            }
        }
        IrExprKind::MethodCall {
            receiver,
            args,
            type_args,
            ..
        } => {
            substitute_projection_output_in_expr(receiver, generic, value);
            for a in args.iter_mut() {
                substitute_projection_output_in_expr(a, generic, value);
            }
            for a in type_args.iter_mut() {
                substitute_projection_output(a, generic, value);
            }
        }
        IrExprKind::Binary { left, right, .. }
        | IrExprKind::Assign { left, right }
        | IrExprKind::AssignOp { left, right, .. } => {
            substitute_projection_output_in_expr(left, generic, value);
            substitute_projection_output_in_expr(right, generic, value);
        }
        IrExprKind::Unary { expr, .. } | IrExprKind::Try(expr) => {
            substitute_projection_output_in_expr(expr, generic, value)
        }
        IrExprKind::Field { base, .. } | IrExprKind::Index { base, .. } => {
            substitute_projection_output_in_expr(base, generic, value)
        }
        IrExprKind::StructExpr { type_args, fields, .. } => {
            for a in type_args.iter_mut() {
                substitute_projection_output(a, generic, value);
            }
            let _ = fields;
        }
        IrExprKind::Repeat { elem, len, .. } => {
            substitute_projection_output_in_expr(elem, generic, value);
            substitute_projection_output_in_expr(len, generic, value);
        }
        IrExprKind::FixedArray(elems) | IrExprKind::Tuple(elems) | IrExprKind::Array(elems) => {
            for e in elems.iter_mut() {
                substitute_projection_output_in_expr(e, generic, value);
            }
        }
        IrExprKind::Block(b) => substitute_projection_output_in_block(b, generic, value),
        IrExprKind::If {
            cond,
            then_branch,
            else_branch,
            ..
        } => {
            substitute_projection_output_in_expr(cond, generic, value);
            substitute_projection_output_in_block(then_branch, generic, value);
            if let Some(e) = else_branch {
                substitute_projection_output_in_expr(e, generic, value);
            }
        }
        IrExprKind::BoundedLoop {
            start,
            end,
            body,
            ..
        } => {
            substitute_projection_output_in_expr(start, generic, value);
            substitute_projection_output_in_expr(end, generic, value);
            substitute_projection_output_in_block(body, generic, value);
        }
        IrExprKind::Cast { expr, .. } => substitute_projection_output_in_expr(expr, generic, value),
        IrExprKind::Path { type_args, .. } => {
            for a in type_args.iter_mut() {
                substitute_projection_output(a, generic, value);
            }
        }
        IrExprKind::DefaultValue { ty } => {
            if let Some(t) = ty.as_mut() {
                substitute_projection_output(t, generic, value);
            }
        }
        IrExprKind::Closure { ret_type, body, .. } => {
            if let Some(rt) = ret_type {
                substitute_projection_output(rt, generic, value);
            }
            substitute_projection_output_in_expr(body, generic, value);
        }
        IrExprKind::WhileLoop { body, .. } => {
            substitute_projection_output_in_block(body, generic, value)
        }
        IrExprKind::IterLoop { collection, body, .. } => {
            substitute_projection_output_in_expr(collection, generic, value);
            substitute_projection_output_in_block(body, generic, value);
        }
        IrExprKind::ArrayGenerate { elem_ty, len, body, .. } => {
            if let Some(t) = elem_ty {
                substitute_projection_output(t, generic, value);
            }
            if let volar_compiler::ir::ArrayLength::Projection { r#type, field, .. } = len {
                if matches!(r#type.as_ref(), IrType::TypeParam(n) if n == generic)
                    && field.to_string() == "Output"
                {
                    *len = volar_compiler::ir::ArrayLength::Const(value);
                }
            }
            substitute_projection_output_in_expr(body, generic, value);
        }
        IrExprKind::RawMap { receiver, body, .. } | IrExprKind::RawFold { receiver, body, .. } => {
            substitute_projection_output_in_expr(receiver, generic, value);
            substitute_projection_output_in_expr(body, generic, value);
        }
        IrExprKind::RawZip { left, right, body, .. } => {
            substitute_projection_output_in_expr(left, generic, value);
            substitute_projection_output_in_expr(right, generic, value);
            substitute_projection_output_in_expr(body, generic, value);
        }
        IrExprKind::BoundedLoop { body, .. } => {
            substitute_projection_output_in_block(body, generic, value)
        }
        IrExprKind::Match { .. } => {}
        _ => {}
    }
}

/// Constant-fold a module const initializer down to a `usize`.
/// Handles integer literals and the arithmetic/shift/logic binary ops the
/// spec sources use in const expressions (e.g. `Q4 = 1 << 30`).
fn const_eval_expr(expr: &IrExpr) -> Option<usize> {
    match &expr.kind {
        IrExprKind::Lit(IrLit::Int(v)) => Some(*v as usize),
        IrExprKind::Lit(IrLit::Bool(b)) => Some(*b as usize),
        IrExprKind::Unary {
            op: SpecUnaryOp::Neg,
            expr,
        } => const_eval_expr(expr).map(|v| v.wrapping_neg()),
        IrExprKind::Unary {
            op: SpecUnaryOp::Not,
            expr,
        } => const_eval_expr(expr).map(|v| !v),
        IrExprKind::Binary { op, left, right } => {
            let l = const_eval_expr(left)?;
            let r = const_eval_expr(right)?;
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

/// Collect the identifier names a pattern binds (ignoring subpatterns beyond
/// tuple/struct nesting, which is all spec sources emit).
fn pattern_idents(pattern: &IrPattern, out: &mut Vec<String>) {
    match pattern {
        IrPattern::Ident { name, .. } => out.push(name.clone()),
        IrPattern::Tuple(elems) => {
            for e in elems {
                pattern_idents(e, out);
            }
        }
        IrPattern::Struct { fields, .. } => {
            for (_, p) in fields {
                pattern_idents(p, out);
            }
        }
        IrPattern::TupleStruct { elems, .. } | IrPattern::Slice(elems) => {
            for p in elems {
                pattern_idents(p, out);
            }
        }
        IrPattern::Ref { pat, .. } => pattern_idents(pat, out),
        _ => {}
    }
}

fn direct_calls<P: Clone>(
    function: &IrFunction<P>,
    env: &MonoEnv,
    defs: &BTreeMap<String, &IrFunction<P>>,
    structs: &StructTable,
) -> Vec<(String, Vec<IrType>, Vec<Option<IrType>>, Option<IrType>)> {
    let mut var_types: VarTypes = function
        .params
        .iter()
        .map(|p| (p.name.clone(), p.ty.clone()))
        .collect();
    let block = &function.body;
    let mut calls = Vec::new();
    for stmt in &block.stmts {
        collect_stmt_calls(stmt, env, defs, structs, &mut var_types, &mut calls);
    }
    if let Some(expr) = &block.expr {
        collect_expr_calls(expr, env, defs, structs, &mut var_types, &mut calls);
    }
    calls
}
/// Infer the result type of a call expression whose callee is a known local
/// function: bind the call-site generics, then monomorphize the callee's
/// declared return type. Returns `None` when the callee is not local or
/// binding fails (a later plan step will surface the real error).
fn call_result_type<P: Clone>(
    expr: &IrExpr<P>,
    env: &MonoEnv,
    defs: &BTreeMap<String, &IrFunction<P>>,
    var_types: &VarTypes,
    structs: &StructTable,
) -> Option<IrType> {
    let IrExprKind::Call { func, args } = &expr.kind else {
        return None;
    };
    let name = match &func.kind {
        IrExprKind::Path { segments, .. } => segments.join("_"),
        IrExprKind::Var(name) => name.clone(),
        _ => return None,
    };
    let callee = *defs.get(&name)?;
    let type_args = match &func.kind {
        IrExprKind::Path { type_args, .. } => type_args.clone(),
        _ => Vec::new(),
    };
    let arg_tys: Vec<Option<IrType>> = args
        .iter()
        .map(|a| infer_expr_type(a, env, Some(var_types), Some(structs)))
        .collect();
    let callee_env = bind_call_args(callee, &type_args, &arg_tys, env, None).ok()?;
    callee
        .return_type
        .as_ref()
        .map(|ret| mono_type(ret, &callee_env))
}
fn collect_stmt_calls<P: Clone>(
    stmt: &IrStmt<P>,
    env: &MonoEnv,
    defs: &BTreeMap<String, &IrFunction<P>>,
    structs: &StructTable,
    var_types: &mut VarTypes,
    calls: &mut Vec<(String, Vec<IrType>, Vec<Option<IrType>>, Option<IrType>)>,
) {
    match &stmt.kind {
        IrStmtKind::Let { pattern, ty, init } => {
            if let Some(expr) = init {
                let expected = ty.clone();
                // If init is a direct call, record it with the expected type.
                if let IrExprKind::Call { func, args } = &expr.kind {
                    let callee = match &func.kind {
                        IrExprKind::Path {
                            segments,
                            type_args,
                        } => Some((segments.join("_"), type_args.clone())),
                        IrExprKind::Var(name) => Some((name.clone(), Vec::new())),
                        _ => None,
                    };
                    if let Some((name, type_args)) = callee {
                        let arg_tys: Vec<Option<IrType>> = args
                            .iter()
                            .map(|a| infer_expr_type(a, env, Some(var_types), Some(structs)))
                            .collect();
                        calls.push((name, type_args, arg_tys, expected.clone()));
                        // Record the call exactly once: collect_expr_calls
                        // below must not re-walk the top-level Call (it
                        // would push a duplicate without the expected type).
                        for arg in args {
                            collect_expr_calls(arg, env, defs, structs, var_types, calls);
                        }
                        // Bind the pattern to the declared type, else the
                        // inferred init type — enough for call-argument
                        // inference downstream.
                        let binding = ty
                            .clone()
                            .or_else(|| call_result_type(expr, env, defs, var_types, structs));
                        if let Some(binding) = binding {
                            let mut idents = Vec::new();
                            pattern_idents(pattern, &mut idents);
                            for name in idents {
                                var_types.insert(name, binding.clone());
                            }
                        }
                        return;
                    }
                }
                collect_expr_calls(expr, env, defs, structs, var_types, calls);
                // Bind the pattern to the declared type, else the inferred
                // init type — enough for call-argument inference downstream.
                let binding = ty
                    .clone()
                    .or_else(|| infer_expr_type(expr, env, Some(var_types), Some(structs)))
                    .or_else(|| call_result_type(expr, env, defs, var_types, structs));
                if let Some(binding) = binding {
                    let mut idents = Vec::new();
                    pattern_idents(pattern, &mut idents);
                    for name in idents {
                        var_types.insert(name, binding.clone());
                    }
                }
            }
        }
        IrStmtKind::Semi(expr) | IrStmtKind::Expr(expr) => {
            collect_expr_calls(expr, env, defs, structs, var_types, calls)
        }
        _ => {}
    }
}
fn collect_expr_calls<P: Clone>(
    expr: &IrExpr<P>,
    env: &MonoEnv,
    defs: &BTreeMap<String, &IrFunction<P>>,
    structs: &StructTable,
    var_types: &mut VarTypes,
    calls: &mut Vec<(String, Vec<IrType>, Vec<Option<IrType>>, Option<IrType>)>,
) {
    use IrExprKind::*;
    match &expr.kind {
        Call { func, args } => {
            let callee = match &func.kind {
                Path {
                    segments,
                    type_args,
                } => Some((segments.join("_"), type_args.clone())),
                Var(name) => Some((name.clone(), Vec::new())),
                _ => None,
            };
            if let Some((name, type_args)) = callee {
                let arg_tys = args
                    .iter()
                    .map(|a| infer_expr_type(a, env, Some(var_types), Some(structs)))
                    .collect();
                calls.push((name, type_args, arg_tys, None));
            }
            for arg in args {
                collect_expr_calls(arg, env, defs, structs, var_types, calls);
            }
        }
        MethodCall {
            receiver,
            method: MethodKind::Other(name),
            type_args,
            args,
        } => {
            // User-defined impl method: callee is the method name with the
            // receiver as the first argument (mirrors the lowering side).
            let arg_tys: Vec<Option<IrType>> = std::iter::once(receiver)
                .map(|r| infer_expr_type(r, env, Some(var_types), Some(structs)))
                .chain(
                    args.iter()
                        .map(|a| infer_expr_type(a, env, Some(var_types), Some(structs))),
                )
                .collect();
            calls.push((name.clone(), type_args.clone(), arg_tys, None));
            collect_expr_calls(receiver, env, defs, structs, var_types, calls);
            for arg in args {
                collect_expr_calls(arg, env, defs, structs, var_types, calls);
            }
        }
        Binary {
            op: SpecBinOp::Mul,
            left,
            right,
        } => {
            // Operator-overload sites: `struct * rhs` with a declared `Mul`
            // impl lowers via the impl's `mul` method, so plan that call.
            let left_ty = infer_expr_type(left, env, Some(var_types), Some(structs));
            let is_mul_impl = left_ty
                .as_ref()
                .map(|ty| match ty {
                    IrType::Struct { kind, .. } => {
                        env.mul_impl_structs.contains_key(&kind.to_string())
                    }
                    _ => false,
                })
                .unwrap_or(false);
            if is_mul_impl {
                let arg_tys: Vec<Option<IrType>> = std::iter::once(left)
                    .chain(std::iter::once(right))
                    .map(|a| infer_expr_type(a, env, Some(var_types), Some(structs)))
                    .collect();
                // Dispatch by receiver struct: several `Mul` impls share the
                // method name `mul` in the source module, so the callee is
                // keyed as `mul__<StructKind>` to keep impls distinct.
                if let Some(IrType::Struct { kind, .. }) = &left_ty {
                    let callee = format!("mul__{}", kind_name(kind));
                    calls.push((callee, Vec::new(), arg_tys, None));
                }
            }
            collect_expr_calls(left, env, defs, structs, var_types, calls);
            collect_expr_calls(right, env, defs, structs, var_types, calls);
        }
        Binary { left, right, .. }
        | Assign { left, right }
        | AssignOp { left, right, .. }
        | RawZip { left, right, .. } => {
            collect_expr_calls(left, env, defs, structs, var_types, calls);
            collect_expr_calls(right, env, defs, structs, var_types, calls);
        }
        Unary { expr, .. }
        | Field { base: expr, .. }
        | Try(expr)
        | Cast { expr, .. }
        | RawMap { receiver: expr, .. }
        | RawFold { receiver: expr, .. } => {
            collect_expr_calls(expr, env, defs, structs, var_types, calls)
        }
        Index { base, index } => {
            collect_expr_calls(base, env, defs, structs, var_types, calls);
            collect_expr_calls(index, env, defs, structs, var_types, calls);
        }
        Block(block) | BoundedLoop { body: block, .. } => {
            for stmt in &block.stmts {
                collect_stmt_calls(stmt, env, defs, structs, var_types, calls);
            }
            if let Some(expr) = &block.expr {
                collect_expr_calls(expr, env, defs, structs, var_types, calls);
            }
        }
        If {
            cond,
            then_branch,
            else_branch,
        } => {
            collect_expr_calls(cond, env, defs, structs, var_types, calls);
            for stmt in &then_branch.stmts {
                collect_stmt_calls(stmt, env, defs, structs, var_types, calls);
            }
            if let Some(expr) = &then_branch.expr {
                collect_expr_calls(expr, env, defs, structs, var_types, calls);
            }
            if let Some(expr) = else_branch {
                collect_expr_calls(expr, env, defs, structs, var_types, calls);
            }
        }
        Tuple(values) | Array(values) | FixedArray(values) => {
            for value in values {
                collect_expr_calls(value, env, defs, structs, var_types, calls);
            }
        }
        StructExpr { fields, .. } => {
            for (_, value) in fields {
                collect_expr_calls(value, env, defs, structs, var_types, calls);
            }
        }
        Return(Some(expr)) => collect_expr_calls(expr, env, defs, structs, var_types, calls),
        _ => {}
    }
}

// ============================================================================
// Module entry point
// ============================================================================

/// Monomorphize an entire `IrModule`: substitutes type/length parameters
/// in all parts of the module (structs, enums, impls, type aliases, functions).
///
/// No longer needed as a separate pass — `lower_module_with_opts` accepts a
/// `MonoEnv` and applies substitutions on the fly.  Kept for completeness.
pub(crate) fn monomorphize_module(
    module: &IrModule<IrFunction>,
    env: &MonoEnv,
) -> IrModule<IrFunction> {
    IrModule {
        name: module.name.clone(),
        structs: module
            .structs
            .iter()
            .map(|s| monomorphize_struct(s, env))
            .collect(),
        enums: module
            .enums
            .iter()
            .map(|e| monomorphize_enum(e, env))
            .collect(),
        traits: module.traits.clone(),
        impls: module
            .impls
            .iter()
            .map(|i| monomorphize_impl(i, env))
            .collect(),
        functions: module
            .functions
            .iter()
            .map(|f| monomorphize_function(f, env))
            .collect(),
        type_aliases: module
            .type_aliases
            .iter()
            .map(|a| monomorphize_type_alias(a, env))
            .collect(),
        consts: module.consts.clone(),
    }
}

/// Monomorphize an `IrCfgModule`: substitutes type/length parameters
/// in all struct definitions, enums, type aliases, impls, and function
/// signatures/bodies (both CFG and flat).
///
/// No longer needed as a separate pass — `lower_cfg_module_with_opts` accepts a
/// `MonoEnv` and applies substitutions on the fly.  Kept for completeness.
pub(crate) fn monomorphize_cfg_module(module: &IrCfgModule, env: &MonoEnv) -> IrCfgModule {
    IrModule {
        name: module.name.clone(),
        structs: module
            .structs
            .iter()
            .map(|s| monomorphize_struct(s, env))
            .collect(),
        enums: module
            .enums
            .iter()
            .map(|e| monomorphize_enum(e, env))
            .collect(),
        traits: module.traits.clone(),
        impls: module
            .impls
            .iter()
            .map(|i| monomorphize_impl(i, env))
            .collect(),
        functions: module
            .functions
            .iter()
            .map(|f| match f {
                IrAnyFunction::Cfg(f) => IrAnyFunction::Cfg(monomorphize_cfg_function(f, env)),
                IrAnyFunction::Flat(f) => IrAnyFunction::Flat(monomorphize_function(f, env)),
            })
            .collect(),
        type_aliases: module
            .type_aliases
            .iter()
            .map(|a| monomorphize_type_alias(a, env))
            .collect(),
        consts: module.consts.clone(),
    }
}

// ============================================================================
// Struct monomorphization
// ============================================================================

fn monomorphize_struct(s: &IrStruct, env: &MonoEnv) -> IrStruct {
    IrStruct {
        kind: s.kind.clone(),
        module_path: s.module_path.clone(),
        // Remove generic params that are being substituted.
        generics: s
            .generics
            .iter()
            .filter(|g| !env.const_params.contains_key(&g.name))
            .cloned()
            .collect(),
        fields: s
            .fields
            .iter()
            .map(|f| IrField {
                name: f.name.clone(),
                ty: mono_type(&f.ty, env),
                public: f.public,
            })
            .collect(),
        is_tuple: s.is_tuple,
        native_volar_type: s.native_volar_type,
        derives: s.derives.clone(),
    }
}

// ============================================================================
// Function monomorphization
// ============================================================================

pub fn monomorphize_function(func: &IrFunction, env: &MonoEnv) -> IrFunction {
    IrFunction {
        no_inline: false,
        name: func.name.clone(),
        module_path: func.module_path.clone(),
        generics: func
            .generics
            .iter()
            .filter(|g| !env.const_params.contains_key(&g.name))
            .cloned()
            .collect(),
        receiver: func.receiver,
        params: func
            .params
            .iter()
            .map(|p| IrParam {
                name: p.name.clone(),
                ty: mono_type(&p.ty, env),
            })
            .collect(),
        return_type: func.return_type.as_ref().map(|t| mono_type(t, env)),
        where_clause: func.where_clause.clone(),
        body: mono_block(&func.body, env),
        external_kind: func.external_kind,
    }
}

// ============================================================================
// Enum monomorphization
// ============================================================================

fn monomorphize_enum(e: &IrEnum, env: &MonoEnv) -> IrEnum {
    IrEnum {
        kind: e.kind.clone(),
        generics: e
            .generics
            .iter()
            .filter(|g| !env.const_params.contains_key(&g.name))
            .cloned()
            .collect(),
        variants: e
            .variants
            .iter()
            .map(|v| IrEnumVariant {
                name: v.name.clone(),
                fields: match &v.fields {
                    IrEnumVariantData::Unit => IrEnumVariantData::Unit,
                    IrEnumVariantData::Tuple(tys) => {
                        IrEnumVariantData::Tuple(tys.iter().map(|t| mono_type(t, env)).collect())
                    }
                    IrEnumVariantData::Struct(fields) => IrEnumVariantData::Struct(
                        fields
                            .iter()
                            .map(|f| IrField {
                                name: f.name.clone(),
                                ty: mono_type(&f.ty, env),
                                public: f.public,
                            })
                            .collect(),
                    ),
                },
            })
            .collect(),
        derives: e.derives.clone(),
    }
}

// ============================================================================
// Impl monomorphization
// ============================================================================

fn monomorphize_impl(imp: &IrImpl, env: &MonoEnv) -> IrImpl {
    IrImpl {
        generics: imp
            .generics
            .iter()
            .filter(|g| !env.const_params.contains_key(&g.name))
            .cloned()
            .collect(),
        trait_: imp.trait_.clone(),
        self_ty: mono_type(&imp.self_ty, env),
        where_clause: imp.where_clause.clone(),
        items: imp
            .items
            .iter()
            .map(|item| match item {
                IrImplItem::Method(f) => IrImplItem::Method(monomorphize_function(f, env)),
                IrImplItem::AssociatedType { name, ty } => IrImplItem::AssociatedType {
                    name: name.clone(),
                    ty: mono_type(ty, env),
                },
            })
            .collect(),
    }
}

// ============================================================================
// Type alias monomorphization
// ============================================================================

fn monomorphize_type_alias(alias: &IrTypeAlias, env: &MonoEnv) -> IrTypeAlias {
    IrTypeAlias {
        name: alias.name.clone(),
        module_path: alias.module_path.clone(),
        generics: alias
            .generics
            .iter()
            .filter(|g| !env.const_params.contains_key(&g.name))
            .cloned()
            .collect(),
        target: mono_type(&alias.target, env),
    }
}

// ============================================================================
// CFG function monomorphization
// ============================================================================

fn monomorphize_cfg_function(func: &IrCfgFunction, env: &MonoEnv) -> IrCfgFunction {
    IrCfgFunction {
        name: func.name.clone(),
        generics: func
            .generics
            .iter()
            .filter(|g| !env.const_params.contains_key(&g.name))
            .cloned()
            .collect(),
        receiver: func.receiver,
        params: func
            .params
            .iter()
            .map(|p| IrParam {
                name: p.name.clone(),
                ty: mono_type(&p.ty, env),
            })
            .collect(),
        return_type: func.return_type.as_ref().map(|t| mono_type(t, env)),
        where_clause: func.where_clause.clone(),
        external_kind: func.external_kind,
        body: mono_cfg_body(&func.body, env),
    }
}

fn mono_cfg_body(body: &IrCfgBody, env: &MonoEnv) -> IrCfgBody {
    IrCfgBody {
        blocks: body.blocks.iter().map(|b| mono_cfg_block(b, env)).collect(),
    }
}

fn mono_cfg_block(block: &IrCfgBlock, env: &MonoEnv) -> IrCfgBlock {
    IrCfgBlock {
        params: block
            .params
            .iter()
            .map(|p| IrParam {
                name: p.name.clone(),
                ty: mono_type(&p.ty, env),
            })
            .collect(),
        stmts: block.stmts.iter().map(|s| mono_stmt(s, env)).collect(),
        terminator: mono_cfg_terminator(&block.terminator, env),
    }
}

fn mono_cfg_terminator(term: &IrCfgTerminator, env: &MonoEnv) -> IrCfgTerminator {
    match term {
        IrCfgTerminator::Return(val) => {
            IrCfgTerminator::Return(val.as_ref().map(|e| mono_expr(e, env)))
        }
        IrCfgTerminator::Goto(jump) => IrCfgTerminator::Goto(mono_cfg_jump(jump, env)),
        IrCfgTerminator::CondGoto { cond, then_, else_ } => IrCfgTerminator::CondGoto {
            cond: mono_expr(cond, env),
            then_: mono_cfg_jump(then_, env),
            else_: mono_cfg_jump(else_, env),
        },
        _ => panic!(
            "mono_cfg_terminator: unhandled IrCfgTerminator variant — add monomorphization for this variant"
        ),
    }
}

fn mono_cfg_jump(jump: &IrCfgJump, env: &MonoEnv) -> IrCfgJump {
    IrCfgJump {
        target: jump.target,
        args: jump.args.iter().map(|a| mono_expr(a, env)).collect(),
        reentry: jump.reentry.clone(),
    }
}

// ============================================================================
// Type monomorphization
// ============================================================================

pub fn mono_type(ty: &IrType, env: &MonoEnv) -> IrType {
    match ty {
        IrType::TypeParam(name) => {
            if let Some(concrete) = env.type_params.get(name) {
                mono_type(concrete, env) // recurse in case the substituted type itself has params
            } else if let Some(&n) = env.const_params.get(name) {
                // Const-generic names often appear as `TypeParam("N")` in
                // struct type_args (`Vope<N, T, U1>`); bind via `with_len`.
                IrType::TypeParam(n.to_string())
            } else if let Ok(n) = name.parse::<usize>() {
                IrType::TypeParam(n.to_string())
            } else if let Some(n) = typenum_usize(name) {
                // Default typenum args (`Vope<N, T, U1>` / `U3`) survive into
                // callee envs that only bind N/T — resolve them globally.
                IrType::TypeParam(n.to_string())
            } else {
                ty.clone()
            }
        }
        IrType::Array { kind, elem, len } => IrType::Array {
            kind: *kind,
            elem: Box::new(mono_type(elem, env)),
            len: mono_len(len, env),
        },
        IrType::Struct { kind, type_args } => {
            // hybrid_array::Array<T, N> is parsed as Struct { kind: Custom("Array") | GenericArray }
            // but must be lowered as IrType::Array.  Convert it here so the LIR pipeline sees
            // a concrete array length rather than a struct it can't register.
            let is_generic_array = matches!(kind, StructKind::GenericArray)
                || matches!(kind, StructKind::Custom(n) if n == "Array");

            if is_generic_array && !type_args.is_empty() {
                let elem = Box::new(mono_type(&type_args[0], env));
                let len = type_args_to_len(type_args.get(1), env);
                return IrType::Array {
                    kind: ArrayKind::GenericArray,
                    elem,
                    len,
                };
            }

            // Typenum / const-generic markers used as type arguments (`U1`, `U3`).
            if type_args.is_empty() {
                if let StructKind::Custom(name) = kind {
                    if let Some(&n) = env.const_params.get(name) {
                        return IrType::TypeParam(n.to_string());
                    }
                    if let Some(n) = typenum_usize(name) {
                        return IrType::TypeParam(n.to_string());
                    }
                }
            }

            IrType::Struct {
                kind: kind.clone(),
                type_args: type_args.iter().map(|a| mono_type(a, env)).collect(),
            }
        }
        IrType::Reference { mutable, elem } => IrType::Reference {
            mutable: *mutable,
            elem: Box::new(mono_type(elem, env)),
        },
        IrType::Tuple(elems) => IrType::Tuple(elems.iter().map(|e| mono_type(e, env)).collect()),
        IrType::Vector { elem } => IrType::Vector {
            elem: Box::new(mono_type(elem, env)),
        },
        IrType::Projection { base, assoc, .. } => {
            if let IrType::TypeParam(name) = base.as_ref() {
                let assoc_str = assoc.to_string();
                if let Some(concrete) = env.projections.get(&(name.clone(), assoc_str)) {
                    return mono_type(concrete, env);
                }
            }
            // Recurse into base in case it contains substitutable params.
            IrType::Projection {
                base: Box::new(mono_type(base, env)),
                trait_path: {
                    // preserve the trait_path field
                    match ty {
                        IrType::Projection { trait_path, .. } => trait_path.clone(),
                        _ => unreachable!(),
                    }
                },
                trait_args: match ty {
                    IrType::Projection { trait_args, .. } => {
                        trait_args.iter().map(|a| mono_type(a, env)).collect()
                    }
                    _ => unreachable!(),
                },
                assoc: assoc.clone(),
            }
        }

        // Primitive, Unit, Never, Infer, Existential, FnPtr, Param
        other => other.clone(),
    }
}

/// The concrete length when fully resolved (`Const`); `None` otherwise.
pub(crate) fn array_len_const(len: &ArrayLength) -> Option<usize> {
    match len {
        ArrayLength::Const(n) => Some(*n),
        _ => None,
    }
}

pub(crate) fn mono_len(len: &ArrayLength, env: &MonoEnv) -> ArrayLength {
    match len {
        ArrayLength::TypeParam(name) => {
            if let Some(&n) = env.const_params.get(name) {
                return ArrayLength::Const(n);
            }
            // `N: ArraySize` is often a Type generic bound via turbofish/inference
            // into `type_params` as `TypeParam("16")` rather than `const_params`.
            if let Some(concrete) = env.type_params.get(name) {
                return mono_len(&type_args_to_len(Some(concrete), env), env);
            }
            if let Ok(n) = name.parse::<usize>() {
                return ArrayLength::Const(n);
            }
            if let Some(n) = typenum_usize(name) {
                return ArrayLength::Const(n);
            }
            if let Some(&n) = env.consts.get(name) {
                return ArrayLength::Const(n);
            }
            len.clone()
        }
        // Projection might reference a const param indirectly; leave for now.
        other => other.clone(),
    }
}

// ============================================================================
// Expression / block monomorphization
// ============================================================================

fn mono_block(block: &volar_compiler::ir::IrBlock, env: &MonoEnv) -> volar_compiler::ir::IrBlock {
    volar_compiler::ir::IrBlock {
        stmts: block.stmts.iter().map(|s| mono_stmt(s, env)).collect(),
        expr: block.expr.as_ref().map(|e| Box::new(mono_expr(e, env))),
    }
}

fn mono_stmt(stmt: &IrStmt, env: &MonoEnv) -> IrStmt {
    let kind = match &stmt.kind {
        IrStmtKind::Let { pattern, ty, init } => IrStmtKind::Let {
            pattern: pattern.clone(),
            ty: ty.as_ref().map(|t| mono_type(t, env)),
            init: init.as_ref().map(|e| mono_expr(e, env)),
        },
        IrStmtKind::Semi(e) => IrStmtKind::Semi(mono_expr(e, env)),
        IrStmtKind::Expr(e) => IrStmtKind::Expr(mono_expr(e, env)),
        _ => panic!("mono_stmt: unhandled IrStmt variant — add monomorphization for this variant"),
    };
    IrStmt::new(kind, stmt.prov.clone(), stmt.side)
}

fn mono_expr(expr: &IrExpr, env: &MonoEnv) -> IrExpr {
    use IrExprKind::*;
    let kind = match &expr.kind {
        Lit(_) | Var(_) | Continue => expr.kind.clone(),

        Path {
            segments,
            type_args,
        } => Path {
            segments: segments.clone(),
            type_args: type_args.iter().map(|a| mono_type(a, env)).collect(),
        },

        Binary { op, left, right } => Binary {
            op: *op,
            left: Box::new(mono_expr(left, env)),
            right: Box::new(mono_expr(right, env)),
        },

        Unary { op, expr: inner } => Unary {
            op: *op,
            expr: Box::new(mono_expr(inner, env)),
        },

        MethodCall {
            receiver,
            method,
            type_args,
            args,
        } => MethodCall {
            receiver: Box::new(mono_expr(receiver, env)),
            method: method.clone(),
            type_args: type_args.iter().map(|a| mono_type(a, env)).collect(),
            args: args.iter().map(|a| mono_expr(a, env)).collect(),
        },

        Call { func, args } => Call {
            func: Box::new(mono_expr(func, env)),
            args: args.iter().map(|a| mono_expr(a, env)).collect(),
        },

        Field { base, field } => Field {
            base: Box::new(mono_expr(base, env)),
            field: field.clone(),
        },

        Index { base, index } => Index {
            base: Box::new(mono_expr(base, env)),
            index: Box::new(mono_expr(index, env)),
        },

        StructExpr {
            kind,
            type_args,
            fields,
            rest,
        } => StructExpr {
            kind: kind.clone(),
            type_args: type_args.iter().map(|a| mono_type(a, env)).collect(),
            fields: fields
                .iter()
                .map(|(name, e)| (name.clone(), mono_expr(e, env)))
                .collect(),
            rest: rest.as_ref().map(|r| Box::new(mono_expr(r, env))),
        },

        Tuple(elems) => Tuple(elems.iter().map(|e| mono_expr(e, env)).collect()),
        Array(elems) => Array(elems.iter().map(|e| mono_expr(e, env)).collect()),
        FixedArray(elems) => FixedArray(elems.iter().map(|e| mono_expr(e, env)).collect()),

        Repeat { elem, len } => Repeat {
            elem: Box::new(mono_expr(elem, env)),
            len: Box::new(mono_expr(len, env)),
        },

        ArrayGenerate {
            elem_ty,
            len,
            index_var,
            body,
        } => ArrayGenerate {
            elem_ty: elem_ty.as_ref().map(|t| Box::new(mono_type(t, env))),
            len: mono_len(len, env),
            index_var: index_var.clone(),
            body: Box::new(mono_expr(body, env)),
        },

        DefaultValue { ty } => DefaultValue {
            ty: ty.as_ref().map(|t| Box::new(mono_type(t, env))),
        },

        LengthOf(len) => LengthOf(mono_len(len, env)),

        BoundedLoop {
            var,
            start,
            end,
            inclusive,
            body,
        } => BoundedLoop {
            var: var.clone(),
            start: Box::new(mono_expr(start, env)),
            end: Box::new(mono_expr(end, env)),
            inclusive: *inclusive,
            body: mono_block(body, env),
        },

        IterLoop {
            pattern,
            collection,
            body,
        } => IterLoop {
            pattern: pattern.clone(),
            collection: Box::new(mono_expr(collection, env)),
            body: mono_block(body, env),
        },

        Block(b) => Block(mono_block(b, env)),

        If {
            cond,
            then_branch,
            else_branch,
        } => If {
            cond: Box::new(mono_expr(cond, env)),
            then_branch: mono_block(then_branch, env),
            else_branch: else_branch.as_ref().map(|e| Box::new(mono_expr(e, env))),
        },

        Match { expr: inner, arms } => Match {
            expr: Box::new(mono_expr(inner, env)),
            arms: arms
                .iter()
                .map(|arm| volar_compiler::ir::IrMatchArm {
                    pattern: arm.pattern.clone(),
                    guard: arm.guard.as_ref().map(|g| mono_expr(g, env)),
                    body: mono_expr(&arm.body, env),
                })
                .collect(),
        },

        Closure {
            params,
            ret_type,
            body,
        } => Closure {
            params: params
                .iter()
                .map(|p| volar_compiler::ir::IrClosureParam {
                    pattern: p.pattern.clone(),
                    ty: p.ty.as_ref().map(|t| mono_type(t, env)),
                })
                .collect(),
            ret_type: ret_type.as_ref().map(|t| Box::new(mono_type(t, env))),
            body: Box::new(mono_expr(body, env)),
        },

        Cast { expr: inner, ty } => Cast {
            expr: Box::new(mono_expr(inner, env)),
            ty: Box::new(mono_type(ty, env)),
        },

        Return(val) => Return(val.as_ref().map(|e| Box::new(mono_expr(e, env)))),
        Break(val) => Break(val.as_ref().map(|e| Box::new(mono_expr(e, env)))),

        Assign { left, right } => Assign {
            left: Box::new(mono_expr(left, env)),
            right: Box::new(mono_expr(right, env)),
        },

        AssignOp { op, left, right } => AssignOp {
            op: *op,
            left: Box::new(mono_expr(left, env)),
            right: Box::new(mono_expr(right, env)),
        },

        RawMap {
            receiver,
            elem_var,
            body,
        } => RawMap {
            receiver: Box::new(mono_expr(receiver, env)),
            elem_var: elem_var.clone(),
            body: Box::new(mono_expr(body, env)),
        },

        RawZip {
            left,
            right,
            left_var,
            right_var,
            body,
        } => RawZip {
            left: Box::new(mono_expr(left, env)),
            right: Box::new(mono_expr(right, env)),
            left_var: left_var.clone(),
            right_var: right_var.clone(),
            body: Box::new(mono_expr(body, env)),
        },

        RawFold {
            receiver,
            init,
            acc_var,
            elem_var,
            body,
        } => RawFold {
            receiver: Box::new(mono_expr(receiver, env)),
            init: Box::new(mono_expr(init, env)),
            acc_var: acc_var.clone(),
            elem_var: elem_var.clone(),
            body: Box::new(mono_expr(body, env)),
        },

        IterPipeline(_) | Range { .. } => expr.kind.clone(),

        other => other.clone(),
    };
    IrExpr::new(kind, expr.prov.clone(), expr.side)
}
