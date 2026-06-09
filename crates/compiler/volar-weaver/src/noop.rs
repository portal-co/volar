// @reliability: normal
//! @ai: assisted
//! No-op (cleartext) weaving pass.
//!
//! Lowers a boolean circuit (`BIrBlocks`) or Volar field-level IR (`IRBlocks`)
//! into a plain Rust bool evaluator with no ZK, no garbling, and no VOLE.
//! Output is a normal `IrModule<IrFunction>` that the existing Rust printer
//! can render.

use alloc::{
    boxed::Box,
    collections::BTreeMap,
    format,
    string::String,
    vec,
    vec::Vec,
};

use volar_compiler::{
    ir::{
        ExternalKind, IrBlock, IrExpr, IrFunction, IrLit, IrModule, IrParam,
        IrPattern, IrStmt, IrType, PrimitiveType, SpecBinOp, SpecUnaryOp,
    },
    linkage::LinkageSystem,
};
use volar_ir::boolar::{BIrBlocks, BIrStmt};
use volar_ir::ir::{
    IRBlocks, IRBlockTargetId, IRTerminator,
    IRTypes as CirTypes, IRVarId as CirVar,
    Stmt,
};

use crate::{build_return, expand_ors, var};

// ============================================================================
// Boolar (BIrBlocks) → cleartext bool evaluator
// ============================================================================

/// Weave a single-block boolean circuit into a cleartext bool evaluator.
///
/// The generated function signature is:
/// ```text
/// fn noop_<name>(in_0: bool, in_1: bool, ...) -> bool
/// ```
/// or a tuple for multi-output circuits.
///
/// # Panics
/// Panics if `circuit` does not satisfy `is_circuit()`.
pub fn weave_noop(
    circuit: &BIrBlocks,
    name: &str,
    linkage: Option<&LinkageSystem>,
) -> IrModule<IrFunction> {
    assert!(
        circuit.is_circuit(),
        "weave_noop: circuit must satisfy is_circuit() (single block with Return terminator)"
    );

    let block = &circuit.blocks[0];
    let num_params = block.params as usize;
    let expanded = expand_ors(block);

    let mut var_names = BTreeMap::<u32, String>::new();
    for i in 0..num_params {
        var_names.insert(i as u32, format!("in_{}", i));
    }

    let mut params: Vec<IrParam> = Vec::new();
    for i in 0..num_params {
        params.push(IrParam {
            name: format!("in_{}", i),
            ty: IrType::Primitive(PrimitiveType::Bool),
        });
    }

    let mut stmts: Vec<IrStmt> = Vec::new();
    let mut stmt_provs: Vec<()> = Vec::new();

    for (result_id, stmt, _prov) in &expanded {
        let let_name = format!("wire_{}", result_id.0);

        let init_expr = match stmt {
            BIrStmt::Zero => IrExpr::Lit(IrLit::Bool(false)),

            BIrStmt::One => IrExpr::Lit(IrLit::Bool(true)),

            BIrStmt::Xor(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                IrExpr::Binary {
                    op: SpecBinOp::BitXor,
                    left: Box::new(var(&name_a)),
                    right: Box::new(var(&name_b)),
                }
            }

            BIrStmt::And(a, b) => {
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                IrExpr::Binary {
                    op: SpecBinOp::BitAnd,
                    left: Box::new(var(&name_a)),
                    right: Box::new(var(&name_b)),
                }
            }

            BIrStmt::Or(a, b) => {
                // expand_ors should have removed these, but handle defensively
                let name_a = var_names[&a.0].clone();
                let name_b = var_names[&b.0].clone();
                IrExpr::Binary {
                    op: SpecBinOp::BitOr,
                    left: Box::new(var(&name_a)),
                    right: Box::new(var(&name_b)),
                }
            }

            BIrStmt::Not(a) => {
                let name_a = var_names[&a.0].clone();
                IrExpr::Unary {
                    op: SpecUnaryOp::Not,
                    expr: Box::new(var(&name_a)),
                }
            }

            BIrStmt::OracleCall { .. }
            | BIrStmt::OracleBit { .. }
            | BIrStmt::ActionCall { .. }
            | BIrStmt::ActionBit { .. }
            | BIrStmt::Rng { .. }
            | BIrStmt::StorageRead { .. }
            | BIrStmt::StorageWrite { .. } => {
                panic!(
                    "noop weaver: {:?} not yet supported — only pure boolean gates (Zero, One, And, Xor, Not, Or) are handled",
                    core::mem::discriminant(stmt)
                )
            }
            _ => panic!("noop weaver: unhandled BIrStmt variant — add support for this variant"),
        };

        stmts.push(IrStmt::Let {
            pattern: IrPattern::ident(&let_name),
            ty: None,
            init: Some(init_expr),
        });
        stmt_provs.push(());
        var_names.insert(result_id.0, let_name);
    }

    let (ret_expr, ret_type) =
        build_return(block, &var_names, IrType::Primitive(PrimitiveType::Bool));

    let func = IrFunction {
        name: format!("noop_{}", name),
        module_path: vec![],
        generics: vec![],
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
        name: "weaved_noop".into(),
        functions: vec![func],
        structs: vec![],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],
        consts: vec![],
    };
    if let Some(ls) = linkage {
        ls.apply(&mut module);
    }
    module
}

// ============================================================================
// Volar field-level IR (IRBlocks) → cleartext evaluator
// ============================================================================

/// Weave a single-block Volar field-level IR circuit into a cleartext evaluator.
///
/// Only `Stmt::Poly`, `Stmt::Const`, `Stmt::Transmute`, `Stmt::StorageRead`,
/// and `Stmt::StorageWrite` are lowered; all other variants emit a weave-time
/// panic with a descriptive message.  The function always compiles.
///
/// # Panics
/// Panics if `circuit` does not satisfy `is_circuit()`, or if an unsupported
/// `Stmt` variant is encountered.
pub fn weave_noop_ir(
    circuit: &IRBlocks,
    types: &CirTypes,
    name: &str,
    linkage: Option<&LinkageSystem>,
) -> IrModule<IrFunction> {
    assert!(
        circuit.is_circuit(),
        "weave_noop_ir: circuit must satisfy is_circuit() (single block with Return terminator)"
    );

    let block = &circuit.blocks[0];
    let num_params = block.params.len();

    // Build a mapping from IRVarId → name string.
    let mut var_names = BTreeMap::<u32, String>::new();
    for i in 0..num_params {
        var_names.insert(i as u32, format!("w_{}", i));
    }

    // All params are `bool` for the cleartext evaluator.
    let mut params: Vec<IrParam> = Vec::new();
    for i in 0..num_params {
        params.push(IrParam {
            name: format!("w_{}", i),
            ty: IrType::Primitive(PrimitiveType::Bool),
        });
    }

    let mut stmts: Vec<IrStmt> = Vec::new();
    let mut stmt_provs: Vec<()> = Vec::new();

    for (i, stmt) in block.stmts.iter().enumerate() {
        let result_id = CirVar(num_params as u32 + i as u32);
        let let_name = format!("w_{}", result_id.0);

        let init_expr = lower_ir_stmt(stmt, &var_names, types);

        stmts.push(IrStmt::Let {
            pattern: IrPattern::ident(&let_name),
            ty: None,
            init: Some(init_expr),
        });
        stmt_provs.push(());
        var_names.insert(result_id.0, let_name);
    }

    // Build the return expression from the terminator.
    let ret_args = match &block.terminator {
        IRTerminator::Jmp { func: IRBlockTargetId::Return, args } => args,
        _ => panic!("weave_noop_ir: expected Jmp(Return) terminator"),
    };

    let (ret_expr, ret_type) = if ret_args.len() == 1 {
        let expr = var(var_names[&ret_args[0].0].as_str());
        (expr, IrType::Primitive(PrimitiveType::Bool))
    } else {
        let exprs: Vec<IrExpr> = ret_args
            .iter()
            .map(|id| var(var_names[&id.0].as_str()))
            .collect();
        let tys: Vec<IrType> = ret_args
            .iter()
            .map(|_| IrType::Primitive(PrimitiveType::Bool))
            .collect();
        (IrExpr::Tuple(exprs), IrType::Tuple(tys))
    };

    let func = IrFunction {
        name: format!("noop_ir_{}", name),
        module_path: vec![],
        generics: vec![],
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
        name: "weaved_noop_ir".into(),
        functions: vec![func],
        structs: vec![],
        enums: vec![],
        traits: vec![],
        impls: vec![],
        type_aliases: vec![],
        consts: vec![],
    };
    if let Some(ls) = linkage {
        ls.apply(&mut module);
    }
    module
}

/// Lower a single `IRStmt` (Volar field-level) to a cleartext `IrExpr<()>`.
///
/// Only pure boolean/arithmetic statements are handled; unsupported variants
/// cause a weave-time panic.
fn lower_ir_stmt(
    stmt: &Stmt<CirVar>,
    var_names: &BTreeMap<u32, String>,
    types: &CirTypes,
) -> IrExpr {
    match stmt {
        // ---- Constant -------------------------------------------------------
        Stmt::Const(c, ty) => {
            let is_bit = types.is_bit(*ty);
            if is_bit {
                IrExpr::Lit(IrLit::Bool(c.lo != 0))
            } else {
                // For wider constants, emit as a u64 integer literal cast; for
                // the noop evaluator a raw integer literal is good enough.
                IrExpr::Lit(IrLit::Int(c.lo as i128))
            }
        }

        // ---- Polynomial (covers XOR, AND, NOT, linear combinations) ---------
        Stmt::Poly { ty, coeffs, constant } => {
            let is_bit = types.is_bit(*ty);
            // The constant term (degree-0 part).
            let const_val: u8 = constant.lo as u8 & 1;
            // Accumulate each monomial into an expression.
            // Start with the constant term.
            let mut acc: Option<IrExpr> =
                if const_val != 0 || coeffs.is_empty() {
                    Some(if is_bit {
                        IrExpr::Lit(IrLit::Bool(const_val != 0))
                    } else {
                        IrExpr::Lit(IrLit::Int(constant.lo as i128))
                    })
                } else {
                    None
                };

            for (monomial, &coeff) in coeffs {
                if coeff == 0 || monomial.is_empty() {
                    continue;
                }
                // Build the product of vars in the monomial (AND in GF(2)).
                let mut product: Option<IrExpr> = None;
                for v in monomial {
                    let vname = var_names[&v.0].clone();
                    let term = var::<()>(&vname);
                    product = Some(match product {
                        None => term,
                        Some(p) => IrExpr::Binary {
                            op: SpecBinOp::BitAnd,
                            left: Box::new(p),
                            right: Box::new(term),
                        },
                    });
                }
                if let Some(p) = product {
                    // Multiply by coeff; in GF(2) coeff is always 0 or 1, so
                    // if coeff is odd we include the monomial, otherwise skip.
                    if coeff & 1 != 0 {
                        acc = Some(match acc {
                            None => p,
                            Some(a) => IrExpr::Binary {
                                op: if is_bit { SpecBinOp::BitXor } else { SpecBinOp::Add },
                                left: Box::new(a),
                                right: Box::new(p),
                            },
                        });
                    }
                }
            }

            // If acc is still None (all zero), emit false/0.
            acc.unwrap_or_else(|| {
                if is_bit {
                    IrExpr::Lit(IrLit::Bool(false))
                } else {
                    IrExpr::Lit(IrLit::Int(0))
                }
            })
        }

        // ---- Transmute (reinterpret bits) -----------------------------------
        Stmt::Transmute { src, .. } => {
            // Cleartext: transmute is identity — just copy the value.
            var::<()>(&var_names[&src.0])
        }

        // ---- Storage --------------------------------------------------------
        Stmt::StorageRead { .. } => {
            panic!("noop weaver: StorageRead not yet supported in weave_noop_ir")
        }
        Stmt::StorageWrite { .. } => {
            // StorageWrite produces a dummy zero bit (matches BIrStmt::StorageWrite semantics).
            IrExpr::Lit(IrLit::Bool(false))
        }

        // ---- Unsupported ----------------------------------------------------
        other => {
            panic!(
                "noop weaver: unsupported IRStmt variant in weave_noop_ir: {:?}",
                other
            )
        }
    }
}

// ============================================================================
// Printer
// ============================================================================

/// Render a no-op weaved `IrModule` to Rust source with a minimal bool-friendly preamble.
///
/// Unlike [`garble::print_weaved_module`], this preamble does not import
/// garble-specific types (`Eval`, `Garble`, …) and does not require
/// `ArraySize`/`Digest` bounds.  The generated code only uses `core::ops::Not`,
/// `core::ops::BitAnd`, `core::ops::BitOr`, and `core::ops::BitXor` — all
/// implemented by `bool` in the standard library.
pub fn print_noop_module(module: &IrModule<IrFunction>) -> String {
    use volar_compiler::printer::{DisplayRust, ModuleWriter};
    use alloc::fmt::Write as _;

    let mut body = String::new();
    let _ = write!(body, "{}", DisplayRust(ModuleWriter { module, emit_async: false }));

    let preamble = concat!(
        "#![allow(unused_variables, dead_code, unused_mut, unused_imports, non_snake_case, unused_parens)]\n",
        "use core::ops::{BitAnd, BitOr, BitXor, Not};\n",
        "\n",
    );

    let mut full = String::with_capacity(preamble.len() + body.len());
    full.push_str(preamble);
    full.push_str(&body);
    full
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    extern crate std;
    use alloc::collections::BTreeMap;
    use std::string::String;

    use super::*;
    use crate::tests_common::{
        build_and_circuit, build_xor_and_circuit, run_compile_check,
    };

    #[test]
    fn test_weave_noop_compiles() {
        let circuit = build_xor_and_circuit();
        let module = weave_noop(&circuit, "test", None);
        let code = print_noop_module(&module);
        run_compile_check(&code, "noop_xor_and");
    }

    #[test]
    fn test_weave_noop_and_circuit() {
        let circuit = build_and_circuit();
        let module = weave_noop(&circuit, "and2", None);
        let code = print_noop_module(&module);
        run_compile_check(&code, "noop_and_circuit");
    }

    #[test]
    fn test_weave_noop_is_cleartext() {
        let circuit = build_xor_and_circuit();
        let module = weave_noop(&circuit, "test", None);
        let code = print_noop_module(&module);
        assert!(
            code.contains('^') || code.contains('&'),
            "Expected ^ or & in generated noop code:\n{}",
            code
        );
        assert!(
            !code.contains("Vope"),
            "Generated noop code must not contain Vope:\n{}",
            code
        );
        assert!(
            !code.contains("vole_and_prover_step"),
            "Generated noop code must not contain vole_and_prover_step:\n{}",
            code
        );
    }

    #[test]
    fn test_weave_noop_ir_compiles() {
        use volar_ir::ir::{
            IRBlocks, IRBlock as CirBlock, IRBlockTargetId, IRTerminator,
            IRTypes as CirTypes, IRVarId as CirVar,
            IRType as CircuitIrType, PrimType,
            Stmt, Constant as CirConst,
        };

        // Build a small Poly (AND) circuit: params = [Bit, Bit], stmt = Poly(w0 & w1), return w2.
        let mut types = CirTypes::new();
        let bit = types.intern(CircuitIrType::Primitive(PrimType::Bit));
        let mut coeffs = BTreeMap::new();
        coeffs.insert(alloc::vec![CirVar(0), CirVar(1)], 1u8);

        let block = CirBlock {
            params: alloc::vec![bit, bit],
            stmts: alloc::vec![
                Stmt::Poly {
                    ty: bit,
                    coeffs,
                    constant: CirConst { hi: 0, lo: 0 },
                },
            ],
            stmt_provs: alloc::vec![()],
            terminator: IRTerminator::Jmp {
                func: IRBlockTargetId::Return,
                args: alloc::vec![CirVar(2)],
            },
        };

        let circuit = IRBlocks::new(alloc::vec![block]);
        let module = weave_noop_ir(&circuit, &types, "and_ir", None);
        let code = print_noop_module(&module);
        run_compile_check(&code, "noop_ir_and");
    }
}
