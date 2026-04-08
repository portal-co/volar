// @reliability: normal
//! @ai: assisted
//! IR-to-protected pass: weaves a boolean circuit into garbled-circuit spec calls.
//!
//! Takes a single-block `BIrBlocks` circuit and produces an `IrModule` where each
//! gate is replaced by a call to the corresponding `volar-spec` garble operation.
//! The output can be rendered to Rust or TypeScript via the existing printer machinery.
//!
//! Two variants are generated:
//! - **Evaluator** (`weave_evaluator`): evaluates the garbled circuit given precomputed tables.
//! - **Garbler** (`weave_garbler`): generates the AND-gate tables from wire labels.
#![no_std]
extern crate alloc;

use alloc::{
    collections::BTreeMap,
    format,
    string::String,
    vec,
    vec::Vec,
};

use volar_compiler::ir::{
    IrBlock, IrClosureParam, IrExpr, IrFunction, IrGenericParam,
    IrGenericParamKind, IrModule, IrParam, IrPattern, IrStmt, IrTraitBound, IrType, MethodKind,
    SpecBinOp, SpecUnaryOp, StructKind, TraitKind,
};
use volar_ir::{
    boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTerminator},
    ir::{IRBlockTargetId, IRVarId},
};

// ============================================================================
// Type helpers
// ============================================================================

fn eval_type() -> IrType {
    IrType::Struct {
        kind: StructKind::Custom("Eval".into()),
        type_args: vec![IrType::TypeParam("N".into())],
    }
}

fn garble_type() -> IrType {
    IrType::Struct {
        kind: StructKind::Custom("Garble".into()),
        type_args: vec![IrType::TypeParam("N".into())],
    }
}

fn garble_table_type() -> IrType {
    IrType::Struct {
        kind: StructKind::Custom("GarbleTable".into()),
        type_args: vec![IrType::TypeParam("N".into())],
    }
}

fn ref_to(ty: IrType) -> IrType {
    IrType::Reference {
        mutable: false,
        elem: alloc::boxed::Box::new(ty),
    }
}

/// Generic params `<N: ArraySize, D: Digest>`.
fn generic_params() -> Vec<IrGenericParam> {
    vec![
        IrGenericParam {
            name: "N".into(),
            kind: IrGenericParamKind::Type,
            bounds: vec![IrTraitBound {
                trait_kind: TraitKind::Custom("ArraySize".into()),
                type_args: vec![],
                assoc_bindings: vec![],
            }],
            default: None,
        },
        IrGenericParam {
            name: "D".into(),
            kind: IrGenericParamKind::Type,
            bounds: vec![IrTraitBound {
                trait_kind: TraitKind::Custom("Digest".into()),
                type_args: vec![],
                assoc_bindings: vec![],
            }],
            default: None,
        },
    ]
}

// ============================================================================
// Expression helpers
// ============================================================================

/// `expr.clone()`
fn clone_expr(expr: IrExpr) -> IrExpr {
    IrExpr::MethodCall {
        receiver: alloc::boxed::Box::new(expr),
        method: MethodKind::Std("clone".into()),
        type_args: vec![],
        args: vec![],
    }
}

/// `&expr`
fn ref_expr(expr: IrExpr) -> IrExpr {
    IrExpr::Unary {
        op: SpecUnaryOp::Ref,
        expr: alloc::boxed::Box::new(expr),
    }
}

/// Variable reference by name.
fn var(name: &str) -> IrExpr {
    IrExpr::Var(name.into())
}

/// `Array::<u8, N>::default()`
fn array_default() -> IrExpr {
    IrExpr::Call {
        func: alloc::boxed::Box::new(IrExpr::Path {
            segments: vec!["Array".into(), "default".into()],
            type_args: vec![
                IrType::Primitive(volar_compiler::ir::PrimitiveType::U8),
                IrType::TypeParam("N".into()),
            ],
        }),
        args: vec![],
    }
}

/// `Array::<u8, N>::from_fn(|{idx}| {body})`
fn array_from_fn(idx: &str, body: IrExpr) -> IrExpr {
    IrExpr::Call {
        func: alloc::boxed::Box::new(IrExpr::Path {
            segments: vec!["Array".into(), "from_fn".into()],
            type_args: vec![
                IrType::Primitive(volar_compiler::ir::PrimitiveType::U8),
                IrType::TypeParam("N".into()),
            ],
        }),
        args: vec![IrExpr::Closure {
            params: vec![IrClosureParam {
                pattern: IrPattern::ident(idx),
                ty: None,
            }],
            ret_type: None,
            body: alloc::boxed::Box::new(body),
        }],
    }
}

/// `wire.base[idx]`
fn base_index(wire_name: &str, idx_name: &str) -> IrExpr {
    IrExpr::Index {
        base: alloc::boxed::Box::new(IrExpr::Field {
            base: alloc::boxed::Box::new(var(wire_name)),
            field: "base".into(),
        }),
        index: alloc::boxed::Box::new(var(idx_name)),
    }
}

// ============================================================================
// Or-gate lowering
// ============================================================================

/// Expand `Or(a, b)` gates inline using De Morgan: `Not(And(Not(a), Not(b)))`.
///
/// Returns a flat list of `(result_var_id, stmt)` pairs with no `Or` gates.
/// Synthetic var IDs start above the existing SSA range.
fn expand_ors(block: &BIrBlock) -> Vec<(IRVarId, BIrStmt)> {
    let num_params = block.params as u32;
    let num_stmts = block.stmts.len() as u32;
    // Existing SSA range: 0..num_params (params) + num_params..num_params+num_stmts (stmts).
    let mut next_synthetic = num_params + num_stmts;
    let mut out: Vec<(IRVarId, BIrStmt)> = Vec::new();

    for (i, stmt) in block.stmts.iter().enumerate() {
        let result_id = IRVarId(num_params + i as u32);
        match stmt {
            BIrStmt::Or(a, b) => {
                let not_a = IRVarId(next_synthetic);
                next_synthetic += 1;
                let not_b = IRVarId(next_synthetic);
                next_synthetic += 1;
                let and_ab = IRVarId(next_synthetic);
                next_synthetic += 1;
                out.push((not_a.clone(), BIrStmt::Not(a.clone())));
                out.push((not_b.clone(), BIrStmt::Not(b.clone())));
                out.push((and_ab.clone(), BIrStmt::And(not_a, not_b)));
                // result = Not(And(Not(a), Not(b))) = De Morgan's law for Or
                out.push((result_id, BIrStmt::Not(and_ab)));
            }
            other => out.push((result_id, other.clone())),
        }
    }
    out
}

// ============================================================================
// Evaluator weaving pass
// ============================================================================

/// Weave a single-block boolean circuit into a garbled-circuit **evaluator** `IrModule`.
///
/// The generated function signature is:
/// ```text
/// fn <name><N: ArraySize, D: Digest>(
///     one_wire: &Eval<N>,           // label for constant 1
///     and_table_0: &GarbleTable<N>, // one per AND gate, in circuit order
///     ...
///     input_0: &Eval<N>,            // one per block parameter, in order
///     ...
/// ) -> Eval<N>
/// ```
///
/// # Panics
/// Panics if `circuit` does not satisfy `is_circuit()`.
pub fn weave_evaluator(circuit: &BIrBlocks, name: &str) -> IrModule {
    assert!(
        circuit.is_circuit(),
        "weave_evaluator: circuit must satisfy is_circuit() (single block with Return terminator)"
    );

    let block = &circuit.0[0];
    let num_params = block.params as usize;

    // Expand Or gates.
    let expanded = expand_ors(block);

    // Count AND gates to determine table parameter count.
    let and_count = expanded
        .iter()
        .filter(|(_, s)| matches!(s, BIrStmt::And(..)))
        .count();

    // Map from SSA var ID to the Rust variable name used in the generated function.
    let mut var_names: BTreeMap<u32, String> = BTreeMap::new();
    for i in 0..num_params {
        var_names.insert(i as u32, format!("input_{}", i));
    }

    // Build parameter list: one_wire, then and_table_k, then input_i.
    let mut params: Vec<IrParam> = Vec::new();
    params.push(IrParam {
        name: "one_wire".into(),
        ty: ref_to(eval_type()),
    });
    for k in 0..and_count {
        params.push(IrParam {
            name: format!("and_table_{}", k),
            ty: ref_to(garble_table_type()),
        });
    }
    for i in 0..num_params {
        params.push(IrParam {
            name: format!("input_{}", i),
            ty: ref_to(eval_type()),
        });
    }

    // Build the function body.
    let mut stmts: Vec<IrStmt> = Vec::new();
    let mut and_counter: usize = 0;

    for (result_id, stmt) in &expanded {
        let let_name = format!("wire_{}", result_id.0);

        let init_expr = match stmt {
            BIrStmt::Zero => {
                // Eval { target: Array::<u8, N>::default() }
                IrExpr::StructExpr {
                    kind: StructKind::Custom("Eval".into()),
                    type_args: vec![],
                    fields: vec![("target".into(), array_default())],
                    rest: None,
                }
            }

            BIrStmt::One => {
                // one_wire.clone()
                clone_expr(var("one_wire"))
            }

            BIrStmt::Xor(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                // wire_a.clone() ^ wire_b.clone()
                IrExpr::Binary {
                    op: SpecBinOp::BitXor,
                    left: alloc::boxed::Box::new(clone_expr(var(&name_a))),
                    right: alloc::boxed::Box::new(clone_expr(var(&name_b))),
                }
            }

            BIrStmt::And(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                let table_name = format!("and_table_{}", and_counter);
                and_counter += 1;
                // wire_a.clone().and_via_table::<D>(&wire_b.clone(), and_table_k)
                IrExpr::MethodCall {
                    receiver: alloc::boxed::Box::new(clone_expr(var(&name_a))),
                    method: MethodKind::Std("and_via_table".into()),
                    type_args: vec![IrType::TypeParam("D".into())],
                    args: vec![
                        ref_expr(clone_expr(var(&name_b))),
                        var(&table_name),
                    ],
                }
            }

            BIrStmt::Not(a) => {
                let name_a = var_names[&a.0].clone();
                // wire_a.clone() ^ one_wire.clone()  (free-XOR NOT)
                IrExpr::Binary {
                    op: SpecBinOp::BitXor,
                    left: alloc::boxed::Box::new(clone_expr(var(&name_a))),
                    right: alloc::boxed::Box::new(clone_expr(var("one_wire"))),
                }
            }

            BIrStmt::Or(..) => unreachable!("Or gates must be expanded before weaving"),
        };

        stmts.push(IrStmt::Let {
            pattern: IrPattern::ident(&let_name),
            ty: None,
            init: Some(init_expr),
        });
        var_names.insert(result_id.0, let_name);
    }

    // Return expression from terminator.
    let (ret_expr, ret_type) = build_return(block, &var_names, eval_type());

    let func = IrFunction {
        name: name.into(),
        generics: generic_params(),
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause: vec![],
        body: IrBlock {
            stmts,
            expr: Some(alloc::boxed::Box::new(ret_expr)),
        },
    };

    IrModule {
        name: "weaved".into(),
        functions: vec![func],
        structs: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],
    }
}

// ============================================================================
// Garbler weaving pass
// ============================================================================

/// Weave a single-block boolean circuit into a garbled-circuit **garbler** `IrModule`.
///
/// The generated function signature is:
/// ```text
/// fn <name>_garble<N: ArraySize, D: Digest>(
///     secret: &GlobalSecret<N>,
///     input_0: &Garble<N>,   // garbler's false-label for input bit 0
///     ...
/// ) -> (Vec<GarbleTable<N>>, Garble<N>)
/// ```
///
/// The returned tuple is (all AND tables in circuit order, output wire false-label).
///
/// # Panics
/// Panics if `circuit` does not satisfy `is_circuit()`.
pub fn weave_garbler(circuit: &BIrBlocks, name: &str) -> IrModule {
    assert!(
        circuit.is_circuit(),
        "weave_garbler: circuit must satisfy is_circuit() (single block with Return terminator)"
    );

    let block = &circuit.0[0];
    let num_params = block.params as usize;
    let expanded = expand_ors(block);

    let mut var_names: BTreeMap<u32, String> = BTreeMap::new();
    for i in 0..num_params {
        var_names.insert(i as u32, format!("input_{}", i));
    }

    // Parameters: secret, then input_i.
    let mut params: Vec<IrParam> = Vec::new();
    params.push(IrParam {
        name: "secret".into(),
        ty: ref_to(IrType::Struct {
            kind: StructKind::Custom("GlobalSecret".into()),
            type_args: vec![IrType::TypeParam("N".into())],
        }),
    });
    for i in 0..num_params {
        params.push(IrParam {
            name: format!("input_{}", i),
            ty: ref_to(garble_type()),
        });
    }

    // Return type: (Vec<GarbleTable<N>>, Garble<N>)
    let ret_type = IrType::Tuple(vec![
        IrType::Vector {
            elem: alloc::boxed::Box::new(garble_table_type()),
        },
        garble_type(),
    ]);

    let mut stmts: Vec<IrStmt> = Vec::new();
    let mut table_counter: usize = 0;

    // `let mut tables: Vec<GarbleTable<N>> = Vec::new();`
    stmts.push(IrStmt::Let {
        pattern: IrPattern::Ident {
            mutable: true,
            name: "tables".into(),
            subpat: None,
        },
        ty: Some(IrType::Vector {
            elem: alloc::boxed::Box::new(garble_table_type()),
        }),
        init: Some(IrExpr::Call {
            func: alloc::boxed::Box::new(IrExpr::Path {
                segments: vec!["Vec".into(), "new".into()],
                type_args: vec![],
            }),
            args: vec![],
        }),
    });

    for (result_id, stmt) in &expanded {
        let let_name = format!("wire_{}", result_id.0);

        let garble_expr = match stmt {
            BIrStmt::Zero | BIrStmt::One => {
                // Garble { base: Array::<u8, N>::default() }
                garble_struct(array_default())
            }

            BIrStmt::Xor(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                // Garble { base: Array::from_fn(|j| wire_a.base[j] ^ wire_b.base[j]) }
                garble_struct(array_from_fn(
                    "j",
                    IrExpr::Binary {
                        op: SpecBinOp::BitXor,
                        left: alloc::boxed::Box::new(base_index(&name_a, "j")),
                        right: alloc::boxed::Box::new(base_index(&name_b, "j")),
                    },
                ))
            }

            BIrStmt::Not(a) => {
                let name_a = var_names[&a.0].clone();
                // NOT: flip the false-label by XOR-ing with the global secret.
                // Garble { base: Array::from_fn(|j| wire_a.base[j] ^ secret.secret()[j]) }
                garble_struct(array_from_fn(
                    "j",
                    IrExpr::Binary {
                        op: SpecBinOp::BitXor,
                        left: alloc::boxed::Box::new(base_index(&name_a, "j")),
                        right: alloc::boxed::Box::new(IrExpr::Index {
                            base: alloc::boxed::Box::new(IrExpr::MethodCall {
                                receiver: alloc::boxed::Box::new(var("secret")),
                                method: MethodKind::Std("secret".into()),
                                type_args: vec![],
                                args: vec![],
                            }),
                            index: alloc::boxed::Box::new(var("j")),
                        }),
                    },
                ))
            }

            BIrStmt::And(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                let table_var = format!("table_{}", table_counter);
                table_counter += 1;

                // `let table_k = secret.gen_and_table::<D>(&wire_a, &wire_b);`
                stmts.push(IrStmt::Let {
                    pattern: IrPattern::ident(&table_var),
                    ty: None,
                    init: Some(IrExpr::MethodCall {
                        receiver: alloc::boxed::Box::new(var("secret")),
                        method: MethodKind::Std("gen_and_table".into()),
                        type_args: vec![IrType::TypeParam("D".into())],
                        args: vec![
                            ref_expr(clone_expr(var(&name_a))),
                            ref_expr(clone_expr(var(&name_b))),
                        ],
                    }),
                });

                // `tables.push(table_k);`
                stmts.push(IrStmt::Semi(IrExpr::MethodCall {
                    receiver: alloc::boxed::Box::new(var("tables")),
                    method: MethodKind::Std("push".into()),
                    type_args: vec![],
                    args: vec![var(&table_var)],
                }));

                // Result wire false-label for AND: placeholder (all-zeros).
                // TODO: compute the proper AND result garble once garble.rs exposes
                // a method to derive the result wire label from the gate table.
                garble_struct(array_default())
            }

            BIrStmt::Or(..) => unreachable!("Or gates must be expanded before weaving"),
        };

        stmts.push(IrStmt::Let {
            pattern: IrPattern::ident(&let_name),
            ty: None,
            init: Some(garble_expr),
        });
        var_names.insert(result_id.0, let_name);
    }

    // Return (tables, output_garble).
    let (output_garble_expr, _) = build_return(block, &var_names, garble_type());
    let ret_expr = IrExpr::Tuple(vec![var("tables"), output_garble_expr]);

    let func = IrFunction {
        name: format!("{}_garble", name),
        generics: generic_params(),
        receiver: None,
        params,
        return_type: Some(ret_type),
        where_clause: vec![],
        body: IrBlock {
            stmts,
            expr: Some(alloc::boxed::Box::new(ret_expr)),
        },
    };

    IrModule {
        name: "weaved_garbler".into(),
        functions: vec![func],
        structs: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],
    }
}

// ============================================================================
// Shared helpers
// ============================================================================

/// `Garble { base: {base_expr} }`
fn garble_struct(base_expr: IrExpr) -> IrExpr {
    IrExpr::StructExpr {
        kind: StructKind::Custom("Garble".into()),
        type_args: vec![],
        fields: vec![("base".into(), base_expr)],
        rest: None,
    }
}

/// Build the return expression and type from a circuit terminator.
///
/// For single-output circuits returns `(Var(wire_N), T)`.
/// For multi-output returns `(Tuple([...]), Tuple([T; n]))`.
fn build_return(
    block: &BIrBlock,
    var_names: &BTreeMap<u32, String>,
    elem_ty: IrType,
) -> (IrExpr, IrType) {
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
                let exprs: Vec<IrExpr> = args
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
// Weaver-specific printer helper
// ============================================================================

/// Render a weaved `IrModule` to Rust source with a garble-specific preamble.
///
/// Generates a self-contained Rust source file that imports all types needed by
/// the weaved evaluator/garbler functions.
pub fn print_weaved_module(module: &IrModule) -> String {
    use volar_compiler::printer::{DisplayRust, ModuleWriter};
    use alloc::fmt::Write as _;

    let mut out = String::new();
    // Write the module body (no preamble).
    let _ = write!(out, "{}", DisplayRust(ModuleWriter { module }));

    // Prepend garble-specific preamble.
    let preamble = concat!(
        "#![allow(unused_variables, dead_code, unused_mut, unused_imports, non_snake_case, unused_parens)]\n",
        "extern crate alloc;\n",
        "use alloc::vec::Vec;\n",
        "use alloc::vec;\n",
        "use core::ops::{BitXor};\n",
        "use hybrid_array::{Array, ArraySize};\n",
        "use digest::Digest;\n",
        "use volar_spec::garble::{Eval, Garble, GarbleTable, GlobalSecret};\n",
        "\n",
    );
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
    use std::{fs, path::Path, process::Command, string::String};

    use super::*;
    use volar_ir::{
        boolar::{BIrBlock, BIrBlocks, BIrStmt, BIrTarget, BIrTerminator},
        ir::{IRBlockTargetId, IRVarId},
    };

    /// Workspace root derived from `CARGO_MANIFEST_DIR` at compile time.
    fn workspace_root() -> String {
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .ancestors()
            .nth(3) // volar-weaver -> compiler -> crates -> workspace root
            .expect("could not find workspace root")
            .to_string_lossy()
            .into_owned()
    }

    /// Two-input circuit: Xor(0,1)->wire2, And(0,2)->wire3, Return wire3.
    fn build_xor_and_circuit() -> BIrBlocks {
        BIrBlocks(vec![BIrBlock {
            params: 2,
            stmts: vec![
                BIrStmt::Xor(IRVarId(0), IRVarId(1)),
                BIrStmt::And(IRVarId(0), IRVarId(2)),
            ],
            terminator: BIrTerminator::Jmp(BIrTarget {
                block: IRBlockTargetId::Return,
                args: vec![IRVarId(3)],
            }),
        }])
    }

    /// Generate a temp Cargo project, put `code` in src/lib.rs,
    /// and run `cargo check` to verify compilation.
    fn run_compile_check(code: &str, test_name: &str) {
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
            // Use a per-test target dir to avoid locking conflicts.
            .env(
                "CARGO_TARGET_DIR",
                String::from(tmpdir.join("target").to_str().unwrap()),
            )
            .output()
            .expect("failed to run cargo check");

        // Capture output before cleanup so we can report it on failure.
        let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
        let _ = fs::remove_dir_all(&tmpdir);

        if !output.status.success() {
            panic!(
                "Generated code failed to compile (test: {})\n--- code ---\n{}\n--- stderr ---\n{}",
                test_name, code, stderr
            );
        }
    }

    #[test]
    fn test_weave_evaluator_compiles() {
        let circuit = build_xor_and_circuit();
        let module = weave_evaluator(&circuit, "test_circuit");
        let code = print_weaved_module(&module);
        run_compile_check(&code, "evaluator");
    }

    #[test]
    fn test_weave_garbler_compiles() {
        let circuit = build_xor_and_circuit();
        let module = weave_garbler(&circuit, "test_circuit");
        let code = print_weaved_module(&module);
        run_compile_check(&code, "garbler");
    }
}
