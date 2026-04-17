// @reliability: normal
//! @ai: assisted
//! IR-to-protected pass: weaves a boolean circuit into garbled-circuit or
//! VOLE-ZK spec calls.
//!
//! Takes a single-block `BIrBlocks` circuit and produces an `IrModule` where
//! each gate is replaced by a call to the corresponding `volar-spec` operation.
//! The output can be rendered to Rust via the existing printer machinery.
//!
//! ## Provenance
//!
//! All weaving passes preserve **provenance** — per-statement annotations that
//! track where each output statement originated.  Input circuits carry
//! `BIrBlocks<P>` and produce `IrModule<Q>` where `P: Into<Q>`.  When `P = ()`,
//! this is zero-cost.  See `docs/provenance.md` for the full design.
//!
//! ## Submodules
//!
//! - [`garble`] — garbled-circuit weaving (evaluator, garbler, GarbledCircuit, EvalSetup).
//! - [`vole`]   — VOLE proving and verifying (Quicksilver-style ZK).
//!
//! ## Shared helpers (private)
//!
//! [`expand_ors`], [`build_return`], and the expression/type building functions
//! (`var`, `clone_expr`, `ref_expr`, `ref_to`, `array_default`, `array_from_fn`,
//! `base_index`) are defined here and used by both submodules.
#![no_std]
extern crate alloc;

use alloc::{
    boxed::Box,
    collections::BTreeMap,
    string::String,
    vec,
    vec::Vec,
};

use volar_compiler::ir::{
    IrClosureParam, IrExpr, IrPattern, IrType, MethodKind, PrimitiveType,
    SpecUnaryOp,
};
use volar_ir::{
    boolar::{BIrBlock, BIrStmt, BIrTerminator},
    ir::{IRBlockTargetId, IRVarId},
};

pub mod garble;
pub mod vole;

// Re-export the most commonly used public items from each submodule.
pub use garble::{
    print_weaved_module, weave_eval_from_setup, weave_eval_from_setup_bounded,
    weave_evaluator, weave_evaluator_bounded, weave_garbler, weave_garbler_bounded,
    weave_into_gc, weave_into_gc_bounded, LoweringMode,
};
#[cfg(feature = "linking")]
pub use garble::garble_linked_spec;

pub use vole::{
    print_weaved_vole_module, weave_vole_prover, weave_vole_prover_bounded,
    weave_vole_verifier, weave_vole_verifier_bounded,
    weave_vole_prover_ir, weave_vole_verifier_ir,
    weave_vole_prover_ir_with_mode, weave_vole_verifier_ir_with_mode,
    StorageSizes, StorageMode, MemoryTrace, MemoryTraceEntry,
};

// ============================================================================
// Shared: Or-gate lowering
// ============================================================================

/// Expand `Or(a, b)` gates inline using De Morgan: `Not(And(Not(a), Not(b)))`.
///
/// Returns a flat list of `(result_var_id, stmt, provenance)` triples with no
/// `Or` gates.  Synthetic statements (the De Morgan expansion) inherit the
/// provenance of the original `Or` gate.
pub(crate) fn expand_ors<P: Clone + Default>(block: &BIrBlock<P>) -> Vec<(IRVarId, BIrStmt, P)> {
    let num_params = block.params as u32;
    let num_stmts = block.stmts.len() as u32;
    let mut next_synthetic = num_params + num_stmts;
    let mut out: Vec<(IRVarId, BIrStmt, P)> = Vec::new();

    for (i, stmt) in block.stmts.iter().enumerate() {
        let result_id = IRVarId(num_params + i as u32);
        let prov = block.stmt_provs.get(i).cloned().unwrap_or_default();
        match stmt {
            BIrStmt::Or(a, b) => {
                let not_a = IRVarId(next_synthetic);
                next_synthetic += 1;
                let not_b = IRVarId(next_synthetic);
                next_synthetic += 1;
                let and_ab = IRVarId(next_synthetic);
                next_synthetic += 1;
                out.push((not_a.clone(), BIrStmt::Not(a.clone()), prov.clone()));
                out.push((not_b.clone(), BIrStmt::Not(b.clone()), prov.clone()));
                out.push((and_ab.clone(), BIrStmt::And(not_a, not_b), prov.clone()));
                out.push((result_id, BIrStmt::Not(and_ab), prov));
            }
            other => out.push((result_id, other.clone(), prov)),
        }
    }
    out
}

// ============================================================================
// Shared: Return expression builder
// ============================================================================

/// Build the return expression and type from a circuit terminator.
///
/// For single-output circuits returns `(Var(wire_N), T)`.
/// For multi-output returns `(Tuple([...]), Tuple([T; n]))`.
pub(crate) fn build_return<P: Clone + Default, Q: Clone + Default>(
    block: &BIrBlock<P>,
    var_names: &BTreeMap<u32, String>,
    elem_ty: IrType,
) -> (IrExpr<Q>, IrType) {
    match &block.terminator {
        BIrTerminator::Jmp(target) => {
            assert!(
                matches!(target.block, IRBlockTargetId::Return),
                "build_return: expected Return terminator"
            );
            let args = &target.args;
            if args.len() == 1 {
                let expr = var(var_names[&args[0].0].as_str());
                (expr, elem_ty)
            } else {
                let exprs: Vec<IrExpr<Q>> = args
                    .iter()
                    .map(|id| var(var_names[&id.0].as_str()))
                    .collect();
                let tys: Vec<IrType> = args.iter().map(|_| elem_ty.clone()).collect();
                (IrExpr::Tuple(exprs), IrType::Tuple(tys))
            }
        }
        _ => panic!("build_return: circuit must have a Jmp(Return) terminator"),
    }
}

// ============================================================================
// Shared: Expression helpers
// ============================================================================

/// Variable reference by name.
pub(crate) fn var<P: Clone + Default>(name: &str) -> IrExpr<P> {
    IrExpr::Var(name.into())
}

/// `expr.clone()`
pub(crate) fn clone_expr<P: Clone + Default>(expr: IrExpr<P>) -> IrExpr<P> {
    IrExpr::MethodCall {
        receiver: Box::new(expr),
        method: MethodKind::Std("clone".into()),
        type_args: vec![],
        args: vec![],
    }
}

/// `&expr`
pub(crate) fn ref_expr<P: Clone + Default>(expr: IrExpr<P>) -> IrExpr<P> {
    IrExpr::Unary {
        op: SpecUnaryOp::Ref,
        expr: Box::new(expr),
    }
}

/// `&T` reference type.
pub(crate) fn ref_to(ty: IrType) -> IrType {
    IrType::Reference {
        mutable: false,
        elem: Box::new(ty),
    }
}

/// `Array::<u8, N>::default()`
pub(crate) fn array_default<P: Clone + Default>() -> IrExpr<P> {
    IrExpr::Call {
        func: Box::new(IrExpr::Path {
            segments: vec!["Array".into(), "default".into()],
            type_args: vec![
                IrType::Primitive(PrimitiveType::U8),
                IrType::TypeParam("N".into()),
            ],
        }),
        args: vec![],
    }
}

/// `Array::<u8, N>::from_fn(|{idx}| {body})`
pub(crate) fn array_from_fn<P: Clone + Default>(idx: &str, body: IrExpr<P>) -> IrExpr<P> {
    IrExpr::Call {
        func: Box::new(IrExpr::Path {
            segments: vec!["Array".into(), "from_fn".into()],
            type_args: vec![
                IrType::Primitive(PrimitiveType::U8),
                IrType::TypeParam("N".into()),
            ],
        }),
        args: vec![IrExpr::Closure {
            params: vec![IrClosureParam {
                pattern: IrPattern::ident(idx),
                ty: None,
            }],
            ret_type: None,
            body: Box::new(body),
        }],
    }
}

/// `wire.base[idx]`
pub(crate) fn base_index<P: Clone + Default>(wire_name: &str, idx_name: &str) -> IrExpr<P> {
    IrExpr::Index {
        base: Box::new(IrExpr::Field {
            base: Box::new(var(wire_name)),
            field: "base".into(),
        }),
        index: Box::new(var(idx_name)),
    }
}

// ============================================================================
// Shared test fixtures
// ============================================================================

#[cfg(test)]
pub(crate) mod tests_common {
    extern crate std;
    use alloc::{format, vec};
    use std::{fs, path::Path, process::Command, string::String};

    use volar_ir::{
        boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator},
        ir::{IRBlockId, IRBlockTargetId, IRVarId},
    };

    /// Workspace root derived from `CARGO_MANIFEST_DIR` at compile time.
    pub fn workspace_root() -> String {
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .ancestors()
            .nth(3)
            .expect("could not find workspace root")
            .to_string_lossy()
            .into_owned()
    }

    /// Two-input circuit: Xor(0,1)->wire2, And(0,2)->wire3, Return wire3.
    pub fn build_xor_and_circuit() -> BIrBlocks {
        BIrBlocks(vec![BIrBlock {
            params: 2,
            stmts: vec![
                BIrStmt::Xor(IRVarId(0), IRVarId(1)),
                BIrStmt::And(IRVarId(0), IRVarId(2)),
            ],
            stmt_provs: vec![(), ()],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: vec![IRVarId(3)],
            }),
        }])
    }

    /// Two-input AND circuit: And(0,1)->wire2, Return wire2.
    pub fn build_and_circuit() -> BIrBlocks {
        BIrBlocks(vec![BIrBlock {
            params: 2,
            stmts: vec![BIrStmt::And(IRVarId(0), IRVarId(1))],
            stmt_provs: vec![()],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: vec![IRVarId(2)],
            }),
        }])
    }

    /// Single-bit self-loop: params=1, stmts=[One], CondJmp → Return or Block(0).
    pub fn build_simple_loop() -> BIrBlocks {
        BIrBlocks(vec![BIrBlock {
            params: 1,
            stmts: vec![BIrStmt::One],
            stmt_provs: vec![()],
            terminator: BIrTerminator::CondJmp {
                val: IRVarId(0),
                then_target: BIrTarget {
                    block: IRBlockTargetId::Return,
                    args: vec![IRVarId(0)],
                },
                else_target: BIrTarget {
                    block: IRBlockTargetId::Block(IRBlockId(0)),
                    args: vec![IRVarId(1)],
                },
            },
        }])
    }

    /// Generate a temp Cargo project, put `code` in src/lib.rs,
    /// and run `cargo check` to verify compilation.
    pub fn run_compile_check(code: &str, test_name: &str) {
        let root = workspace_root();
        let tmpdir = std::env::temp_dir().join(format!("volar_weaver_{}", test_name));
        let srcdir = tmpdir.join("src");
        fs::create_dir_all(&srcdir).unwrap();

        let cargo_toml = std::format!(
            "[package]\n\
             name = \"weave-check-{name}\"\n\
             version = \"0.1.0\"\n\
             edition = \"2024\"\n\
             \n\
             [lib]\n\
             path = \"src/lib.rs\"\n\
             \n\
             [dependencies]\n\
             volar-spec = {{ path = \"{root}/crates/spec/volar-spec\" }}\n\
             volar-primitives = {{ path = \"{root}/crates/spec/volar-primitives\" }}\n\
             volar-common = {{ path = \"{root}/crates/spec/volar-common\" }}\n\
             hybrid-array = \"0.4.8\"\n\
             digest = {{ version = \"0.11.2\", default-features = false }}\n\
             cipher = {{ version = \"0.5.1\", default-features = false }}\n\
             rand = {{ version = \"0.9.2\", default-features = false }}\n\
             typenum = {{ version = \"1.17\", default-features = false }}\n\
             elliptic-curve = {{ version = \"0.13.8\", features = [\"arithmetic\"], default-features = false }}\n",
            name = test_name,
            root = root,
        );

        fs::write(tmpdir.join("Cargo.toml"), &cargo_toml).unwrap();
        fs::write(srcdir.join("lib.rs"), code).unwrap();

        let output = Command::new("cargo")
            .args(["check", "--quiet"])
            .current_dir(&tmpdir)
            .env(
                "CARGO_TARGET_DIR",
                String::from(tmpdir.join("target").to_str().unwrap()),
            )
            .output()
            .expect("failed to run cargo check");

        let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
        let _ = fs::remove_dir_all(&tmpdir);

        if !output.status.success() {
            panic!(
                "Generated code failed to compile (test: {})\n--- code ---\n{}\n--- stderr ---\n{}",
                test_name, code, stderr
            );
        }
    }
}
