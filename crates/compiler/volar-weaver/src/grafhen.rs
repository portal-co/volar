// @reliability: experimental
// @experimental-status: design
// @experimental-since: (not yet reviewed)
// @ai: unreviewed
//! GRAFHEN weaving pass.
//!
//! Lowers a boolean circuit (`BIrBlocks`) into an `IrModule` that evaluates
//! the circuit **homomorphically** over GRAFHEN ciphertexts.
//!
//! **WARNING: IND-CPA BROKEN.** The generated code relies on GRAFHEN, which
//! is broken by ePrint 2026/700. Use only inside a ZK proof for correctness,
//! never as a confidentiality primitive. See `docs/grafhen.md`.
//!
//! ## Generated function signature
//!
//! ```text
//! fn <name>_grafhen<R: WordReducer<WBOUND>>(
//!     pk: &GrafhenPublic<R, WBOUND>,
//!     input_0: &GrafhenWord<WBOUND>,
//!     input_1: &GrafhenWord<WBOUND>,
//!     ...
//! ) -> GrafhenWord<WBOUND>
//! ```
//!
//! `WBOUND` is a concrete literal baked in at weave time via the `word_bound`
//! parameter (e.g. `42`). The same literal appears in all type positions, so
//! the caller instantiates by choosing a concrete `R`.
//!
//! ## Gate lowering
//!
//! | Gate | Generated call |
//! |------|---------------|
//! | Zero | `GrafhenWord::identity()` |
//! | One  | `pk.enc_one.clone()` |
//! | XOR  | `grafhen_xor(&a, &b)` |
//! | NOT  | `grafhen_not(&a, pk)` |
//! | AND  | `grafhen_and(&a, &b, pk)` |
//! | OR   | expanded via De Morgan: `NOT(AND(NOT(a), NOT(b)))` |

use alloc::{
    boxed::Box,
    format,
    string::String,
    vec,
    vec::Vec,
};

use volar_compiler::{
    ir::{
        ExternalKind, IrBlock, IrExpr, IrFunction, IrGenericParam, IrGenericParamKind,
        IrModule, IrParam, IrPattern, IrStmt, IrTraitBound, IrType,
        StructKind, TraitKind,
    },
    linkage::LinkageSystem,
};
use volar_ir::boolar::{BIrBlocks, BIrStmt};

use crate::{
    build_return, clone_expr, expand_ors, ref_expr, var, NoProvenance, ProvenanceHandler,
};

// ============================================================================
// Type helpers
// ============================================================================

/// `GrafhenWord<WBOUND>` where `WBOUND` is a concrete literal string.
fn grafhen_word_type(word_bound: usize) -> IrType {
    IrType::Struct {
        kind: StructKind::Custom("GrafhenWord".into()),
        type_args: vec![IrType::TypeParam(format!("{}", word_bound))],
    }
}

/// `GrafhenPublic<R, WBOUND>`.
fn grafhen_public_type(word_bound: usize) -> IrType {
    IrType::Struct {
        kind: StructKind::Custom("GrafhenPublic".into()),
        type_args: vec![
            IrType::TypeParam("R".into()),
            IrType::TypeParam(format!("{}", word_bound)),
        ],
    }
}

/// Generic params: `<R: WordReducer<WBOUND>>` with WBOUND as a literal.
fn generic_params(word_bound: usize) -> Vec<IrGenericParam> {
    vec![IrGenericParam {
        name: "R".into(),
        kind: IrGenericParamKind::Type,
        bounds: vec![IrTraitBound {
            trait_kind: TraitKind::Custom("WordReducer".into()),
            type_args: vec![IrType::TypeParam(format!("{}", word_bound))],
            assoc_bindings: vec![],
        }],
        default: None,
    }]
}

// ============================================================================
// Weaver
// ============================================================================

/// Weave a boolean circuit into a GRAFHEN homomorphic evaluator.
///
/// `word_bound` is baked into all `GrafhenWord<WBOUND>` and `GrafhenPublic<R,
/// WBOUND>` type positions as a concrete literal.
///
/// # Panics
/// Panics if `circuit` does not satisfy `is_circuit()`.
pub fn weave_grafhen<P: Clone + Default>(
    circuit: &BIrBlocks<P>,
    name: &str,
    word_bound: usize,
    linkage: Option<&LinkageSystem>,
) -> IrModule {
    weave_grafhen_with_handler(circuit, name, word_bound, linkage, &NoProvenance)
}

/// Weave a boolean circuit into a GRAFHEN homomorphic evaluator,
/// using `handler` to map input provenance into the output IR.
///
/// # Panics
/// Panics if `circuit` does not satisfy `is_circuit()`.
pub fn weave_grafhen_with_handler<P, H>(
    circuit: &BIrBlocks<P>,
    name: &str,
    word_bound: usize,
    linkage: Option<&LinkageSystem>,
    handler: &H,
) -> IrModule<H::Output>
where
    P: Clone + Default,
    H: ProvenanceHandler<P>,
{
    assert!(
        circuit.is_circuit(),
        "weave_grafhen: circuit must satisfy is_circuit() (single block with Return terminator)"
    );

    let block = &circuit.0[0];
    let num_params = block.params as usize;
    let expanded = expand_ors(block);

    // Build initial var_name map for circuit inputs.
    let mut var_names = alloc::collections::BTreeMap::<u32, String>::new();
    for i in 0..num_params {
        var_names.insert(i as u32, format!("input_{}", i));
    }

    // Function parameters: pk first, then one GrafhenWord per circuit input.
    let word_ty = grafhen_word_type(word_bound);
    let pk_ty = grafhen_public_type(word_bound);

    let mut params: Vec<IrParam> = Vec::new();
    params.push(IrParam {
        name: "pk".into(),
        ty: IrType::Reference {
            mutable: false,
            elem: Box::new(pk_ty),
        },
    });
    for i in 0..num_params {
        params.push(IrParam {
            name: format!("input_{}", i),
            ty: IrType::Reference {
                mutable: false,
                elem: Box::new(word_ty.clone()),
            },
        });
    }

    // Lower each gate.
    let mut stmts: Vec<IrStmt<H::Output>> = Vec::new();
    let mut stmt_provs: Vec<H::Output> = Vec::new();

    for (result_id, stmt, prov) in &expanded {
        let let_name = format!("wire_{}", result_id.0);
        let q = handler.map(prov);

        let init_expr: IrExpr<H::Output> = match stmt {
            // Zero: identity word.
            BIrStmt::Zero => IrExpr::Call {
                func: Box::new(IrExpr::Path {
                    segments: vec!["GrafhenWord".into(), "identity".into()],
                    type_args: vec![],
                }),
                args: vec![],
            },

            // One: clone pk.enc_one.
            BIrStmt::One => clone_expr(IrExpr::Field {
                base: Box::new(var("pk")),
                field: "enc_one".into(),
            }),

            // XOR: grafhen_xor(&a, &b).
            BIrStmt::Xor(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                IrExpr::Call {
                    func: Box::new(IrExpr::Path {
                        segments: vec!["grafhen_xor".into()],
                        type_args: vec![],
                    }),
                    args: vec![
                        ref_expr(clone_expr(var(&name_a))),
                        ref_expr(clone_expr(var(&name_b))),
                    ],
                }
            }

            // NOT: grafhen_not(&a, pk).
            BIrStmt::Not(a) => {
                let name_a = var_names[&a.0].clone();
                IrExpr::Call {
                    func: Box::new(IrExpr::Path {
                        segments: vec!["grafhen_not".into()],
                        type_args: vec![],
                    }),
                    args: vec![
                        ref_expr(clone_expr(var(&name_a))),
                        var("pk"),
                    ],
                }
            }

            // AND: grafhen_and(&a, &b, pk).
            BIrStmt::And(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                IrExpr::Call {
                    func: Box::new(IrExpr::Path {
                        segments: vec!["grafhen_and".into()],
                        type_args: vec![],
                    }),
                    args: vec![
                        ref_expr(clone_expr(var(&name_a))),
                        ref_expr(clone_expr(var(&name_b))),
                        var("pk"),
                    ],
                }
            }

            BIrStmt::Or(..) => unreachable!("Or gates must be expanded before weaving"),
        };

        stmts.push(IrStmt::Let {
            pattern: IrPattern::ident(&let_name),
            ty: None,
            init: Some(init_expr),
        });
        stmt_provs.push(q);
        var_names.insert(result_id.0, let_name);
    }

    let (ret_expr, ret_type) = build_return(block, &var_names, word_ty);

    let func = IrFunction {
        name: format!("{}_grafhen", name),
        generics: generic_params(word_bound),
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause: vec![],
        body: IrBlock {
            stmts,
            stmt_provs,
            expr: Some(Box::new(ret_expr)),
        },
        external_kind: ExternalKind::Normal,
    };

    let mut module = IrModule {
        name: "weaved_grafhen".into(),
        functions: vec![func],
        structs: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],
    };
    if let Some(ls) = linkage {
        ls.apply(&mut module);
    }
    module
}

// ============================================================================
// Printer
// ============================================================================

/// Render a weaved GRAFHEN `IrModule` to Rust source.
pub fn print_grafhen_module(module: &IrModule, self_contained: bool) -> String {
    use volar_compiler::printer::{DisplayRust, ModuleWriter};
    use alloc::fmt::Write as _;

    let mut out = String::new();
    let _ = write!(out, "{}", DisplayRust(ModuleWriter { module }));

    let preamble: &str = if self_contained {
        concat!(
            "#![allow(unused_variables, dead_code, unused_mut, unused_imports, non_snake_case, unused_parens)]\n",
            "use volar_spec::grafhen::{GrafhenWord, GrafhenPublic, WordReducer, NoReduction,\n",
            "    grafhen_zero, grafhen_xor, grafhen_not, grafhen_and};\n",
            "\n",
        )
    } else {
        concat!(
            "#![allow(unused_variables, dead_code, unused_mut, unused_imports, non_snake_case, unused_parens)]\n",
            "use volar_spec::grafhen::{GrafhenWord, GrafhenPublic, WordReducer,\n",
            "    grafhen_zero, grafhen_xor, grafhen_not, grafhen_and};\n",
            "\n",
        )
    };

    let mut full = String::with_capacity(preamble.len() + out.len());
    full.push_str(preamble);
    full.push_str(&out);
    full
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    extern crate std;
    use std::string::String;

    use super::*;
    use crate::tests_common::{build_xor_and_circuit, run_compile_check, workspace_root};

    #[test]
    fn test_weave_grafhen_xor_and_compiles() {
        let circuit = build_xor_and_circuit();
        let module = weave_grafhen(&circuit, "test_circuit", 64, None);
        let code = print_grafhen_module(&module, false);
        run_compile_check_grafhen(&code, "grafhen_xor_and");
    }

    /// Like `run_compile_check` but with the GRAFHEN spec dependency.
    fn run_compile_check_grafhen(code: &str, test_name: &str) {
        use std::{fs, process::Command};
        let root = workspace_root();
        let tmpdir = std::env::temp_dir().join(std::format!("volar_weaver_{}", test_name));
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
                "Generated GRAFHEN code failed to compile (test: {})\n--- code ---\n{}\n--- stderr ---\n{}",
                test_name, code, stderr
            );
        }
    }
}
